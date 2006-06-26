/*
 * props.c :  routines dealing with properties in the working copy
 *
 * ====================================================================
 * Copyright (c) 2000-2004 CollabNet.  All rights reserved.
 *
 * This software is licensed as described in the file COPYING, which
 * you should have received as part of this distribution.  The terms
 * are also available at http://subversion.tigris.org/license-1.html.
 * If newer versions of this license are posted there, you may use a
 * newer version instead, at your option.
 *
 * This software consists of voluntary contributions made by many
 * individuals.  For exact contribution history, see the revision
 * history and logs, available at http://subversion.tigris.org/.
 * ====================================================================
 */



#include <stdio.h>       /* temporary, for printf() */
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <apr_pools.h>
#include <apr_hash.h>
#include <apr_tables.h>
#include <apr_file_io.h>
#include <apr_strings.h>
#include <apr_general.h>
#include "svn_types.h"
#include "svn_string.h"
#include "svn_pools.h"
#include "svn_path.h"
#include "svn_xml.h"
#include "svn_error.h"
#include "svn_props.h"
#include "svn_io.h"
#include "svn_hash.h"
#include "svn_wc.h"
#include "svn_utf.h"

#include "wc.h"
#include "log.h"
#include "adm_files.h"
#include "entries.h"
#include "props.h"
#include "translate.h"
#include "questions.h"

#include "svn_private_config.h"

/*---------------------------------------------------------------------*/

/*** Deducing local changes to properties ***/

/*---------------------------------------------------------------------*/

/*** Detecting a property conflict ***/


/* Given two propchange objects, return TRUE iff they conflict.  If
   there's a conflict, DESCRIPTION will contain an english description
   of the problem. */

/* For note, here's the table being implemented:

              |  update set     |    update delete   |
  ------------|-----------------|--------------------|
  user set    | conflict iff    |      conflict      |
              |  vals differ    |                    |
  ------------|-----------------|--------------------|
  user delete |   conflict      |      merge         |
              |                 |    (no problem)    |
  ----------------------------------------------------

*/
svn_boolean_t
svn_wc__conflicting_propchanges_p (const svn_string_t **description,
                                   const svn_prop_t *local,
                                   const svn_prop_t *update,
                                   apr_pool_t *pool)
{
  /* We're assuming that whoever called this routine has already
     deduced that local and change2 affect the same property name.
     (After all, if they affect different property names, how can they
     possibly conflict?)  But still, let's make this routine
     `complete' by checking anyway. */
  if (strcmp(local->name, update->name) != 0)
    return FALSE;  /* no conflict */

  /* If one change wants to delete a property and the other wants to
     set it, this is a conflict.  This check covers two bases of our
     chi-square. */
  if ((local->value != NULL) && (update->value == NULL))
    {
      *description =
        svn_string_createf
        (pool,
         _("Property '%s' locally changed to '%s', but update deletes it\n"),
         local->name, local->value->data);
      return TRUE;  /* conflict */
    }
  if ((local->value == NULL) && (update->value != NULL))
    {
      *description =
        svn_string_createf
        (pool,
         _("Property '%s' locally deleted, but update sets it to '%s'\n"),
         local->name, update->value->data);
      return TRUE;  /* conflict */
    }

  /* If both changes delete the same property, there's no conflict.
     It's an implicit merge.  :)  */
  if ((local->value == NULL) && (update->value == NULL))
    return FALSE;  /* no conflict */

  /* If both changes set the property, it's a conflict iff the values
     are different */
  else if (! svn_string_compare (local->value, update->value))
    {
      *description =
        svn_string_createf
        (pool, _("Property '%s' locally changed to '%s', "
                 "but update sets it to '%s'\n"),
         local->name, local->value->data, update->value->data);
      return TRUE;  /* conflict */
    }

  /* values are the same, so another implicit merge. */
  return FALSE;  /* no conflict */
}



/*---------------------------------------------------------------------*/

/*** Reading/writing property hashes from disk ***/

/* The real functionality here is part of libsvn_subr, in hashdump.c.
   But these are convenience routines for use in libsvn_wc. */



/* If PROPFILE_PATH exists (and is a file), assume it's full of
   properties and load this file into HASH.  Otherwise, leave HASH
   untouched.  */
svn_error_t *
svn_wc__load_prop_file (const char *propfile_path,
                        apr_hash_t *hash,
                        apr_pool_t *pool)
{
  svn_error_t *err;

  apr_file_t *propfile = NULL;

  err = svn_io_file_open (&propfile, propfile_path,
                          APR_READ | APR_BUFFERED, APR_OS_DEFAULT,
                          pool);

  if (err && (APR_STATUS_IS_ENOENT (err->apr_err)
              || APR_STATUS_IS_ENOTDIR (err->apr_err)))
    {
      svn_error_clear (err);
      return SVN_NO_ERROR;
    }

  SVN_ERR (err);

  SVN_ERR_W (svn_hash_read (hash, propfile, pool),
             apr_psprintf (pool, _("Can't parse '%s'"),
                           svn_path_local_style (propfile_path, pool)));

  SVN_ERR (svn_io_file_close (propfile, pool));

  return SVN_NO_ERROR;
}



/* Given a HASH full of property name/values, write them to a file
   located at PROPFILE_PATH.  */
svn_error_t *
svn_wc__save_prop_file (const char *propfile_path,
                        apr_hash_t *hash,
                        apr_pool_t *pool)
{
  apr_file_t *prop_tmp;
  

  SVN_ERR (svn_io_file_open (&prop_tmp, propfile_path,
                             (APR_WRITE | APR_CREATE | APR_TRUNCATE
                              | APR_BUFFERED), 
                             APR_OS_DEFAULT, pool));

  if (apr_hash_count (hash) != 0)
    SVN_ERR_W (svn_hash_write (hash, prop_tmp, pool),
               apr_psprintf (pool, 
                             _("Can't write property hash to '%s'"),
                             svn_path_local_style (propfile_path, pool)));

  SVN_ERR (svn_io_file_close (prop_tmp, pool));

  return SVN_NO_ERROR;
}


/*---------------------------------------------------------------------*/

/*** Misc ***/

/* Assuming FP is a filehandle already open for appending, write
   CONFLICT_DESCRIPTION to file. */
static svn_error_t *
append_prop_conflict (apr_file_t *fp,
                      const svn_string_t *conflict_description,
                      apr_pool_t *pool)
{
  /* TODO:  someday, perhaps prefix each conflict_description with a
     timestamp or something? */
  apr_size_t written;
  const char *conflict_description_native =
    svn_utf_cstring_from_utf8_fuzzy (conflict_description->data, pool);

  SVN_ERR (svn_io_file_write_full (fp, conflict_description_native,
                                   strlen (conflict_description_native),
                                   &written, pool));

  return SVN_NO_ERROR;
}


/* ### not used outside this file. make it static? */
svn_error_t *
svn_wc__get_existing_prop_reject_file (const char **reject_file,
                                       svn_wc_adm_access_t *adm_access,
                                       const char *name,
                                       apr_pool_t *pool)
{
  apr_hash_t *entries;
  const svn_wc_entry_t *the_entry;

  SVN_ERR (svn_wc_entries_read (&entries, adm_access, FALSE, pool));
  the_entry = apr_hash_get (entries, name, APR_HASH_KEY_STRING);

  if (! the_entry)
    return svn_error_createf
      (SVN_ERR_ENTRY_NOT_FOUND, NULL,
       _("Can't find entry '%s' in '%s'"),
       name,
       svn_path_local_style (svn_wc_adm_access_path (adm_access), pool));

  *reject_file = the_entry->prejfile 
                 ? apr_pstrdup (pool, the_entry->prejfile)
                 : NULL;
  return SVN_NO_ERROR;
}



/*---------------------------------------------------------------------*/

/*** Merging propchanges into the working copy ***/


svn_error_t *
svn_wc_merge_props (svn_wc_notify_state_t *state,
                    const char *path,
                    svn_wc_adm_access_t *adm_access,
                    apr_hash_t *baseprops,
                    const apr_array_header_t *propchanges,
                    svn_boolean_t base_merge,
                    svn_boolean_t dry_run,
                    apr_pool_t *pool)
{
  const svn_wc_entry_t *entry;
  const char *parent, *base_name;
  svn_stringbuf_t *log_accum;
  apr_file_t *log_fp = NULL;

  SVN_ERR (svn_wc_entry (&entry, path, adm_access, FALSE, pool));
  if (entry == NULL)
    return svn_error_createf (SVN_ERR_UNVERSIONED_RESOURCE, NULL,
                              _("'%s' is not under version control"),
                              svn_path_local_style (path, pool));

  /* Notice that we're not using svn_path_split_if_file(), because
     that looks at the actual working file.  Its existence shouldn't
     matter, so we're looking at entry->kind instead. */
  switch (entry->kind)
    {
    case svn_node_dir:
      parent = path;
      base_name = NULL;
      break;
    case svn_node_file:
      svn_path_split (path, &parent, &base_name, pool);
      break;
    default:
      return SVN_NO_ERROR; /* ### svn_node_none or svn_node_unknown */
    }

  if (! dry_run)
    {
      SVN_ERR (svn_wc__open_adm_file (&log_fp, parent, SVN_WC__ADM_LOG,
                                      (APR_WRITE | APR_CREATE), /* not excl */
                                      pool));
      log_accum = svn_stringbuf_create ("", pool);
    }
  
  /* Note that while this routine does the "real" work, it's only
     prepping tempfiles and writing log commands.  */
  SVN_ERR (svn_wc__merge_props (state, adm_access, base_name, baseprops,
                                propchanges, base_merge, dry_run,
                                pool, &log_accum));

  if (! dry_run)
    {
      SVN_ERR_W (svn_io_file_write_full (log_fp, log_accum->data, 
                                         log_accum->len, NULL, pool),
                 apr_psprintf (pool, _("Error writing log for '%s'"),
                               svn_path_local_style (path, pool)));

      SVN_ERR (svn_wc__close_adm_file (log_fp, parent, SVN_WC__ADM_LOG,
                                       1, /* sync */ pool));
      SVN_ERR (svn_wc__run_log (adm_access, NULL, pool));
    }

  return SVN_NO_ERROR;
}



svn_error_t *
svn_wc__merge_props (svn_wc_notify_state_t *state,
                     svn_wc_adm_access_t *adm_access,
                     const char *name,
                     apr_hash_t *server_baseprops,
                     const apr_array_header_t *propchanges,
                     svn_boolean_t base_merge,
                     svn_boolean_t dry_run,
                     apr_pool_t *pool,
                     svn_stringbuf_t **entry_accum)
{
  int i, slash;
  svn_boolean_t is_dir;

  const char *entryname;
  const char *full_path;
  apr_hash_t *working_props;   /* all `working' properties */
  apr_hash_t *base_props;    /* all `pristine' properties */
  
  /* Zillions of pathnames to compute!  yeargh!  */
  const char *base_propfile_path, *local_propfile_path;
  const char *base_prop_tmp_path, *local_prop_tmp_path;
  const char *tmp_props, *real_props;      
  const char *access_path = svn_wc_adm_access_path (adm_access);
  int access_len = strlen (access_path);
  apr_file_t *reject_fp = NULL;           /* the real conflicts file */
  const char *reject_path = NULL;
  apr_file_t *reject_tmp_fp = NULL;       /* the temporary conflicts file */
  const char *reject_tmp_path = NULL;

  /* Empty path and paths ending in / don't need an extra slash removed */
  if (access_len == 0 || access_path[access_len - 1] == '/')
    slash = 0;
  else
    slash = 1;

  if (name == NULL)
    {
      /* We must be merging props on the directory PATH  */
      entryname = SVN_WC_ENTRY_THIS_DIR;
      full_path = access_path;
      is_dir = TRUE;
    }
  else
    {
      /* We must be merging props on the file PATH/NAME */
      entryname = name;
      full_path = svn_path_join (access_path, name, pool);
      is_dir = FALSE;
    }

  /* Load the base & working property files into hashes */
  working_props = apr_hash_make (pool);
  base_props = apr_hash_make (pool);
  SVN_ERR (svn_wc__prop_path (&local_propfile_path, full_path, adm_access,
                              FALSE, pool)); 
  SVN_ERR (svn_wc__prop_base_path (&base_propfile_path, full_path, adm_access,
                                   FALSE, pool));
  SVN_ERR (svn_wc__load_prop_file (base_propfile_path, base_props, pool));
  SVN_ERR (svn_wc__load_prop_file (local_propfile_path, working_props, pool));
  
  if (state)
    {
      /* Start out assuming no conflicts.  Don't bother to examine
         propchanges->nelts yet; even if we knew there were
         propchanges, we wouldn't yet know if they are "normal" props,
         as opposed wc or entry props.  */ 
      if (propchanges->nelts > 0)
        *state = svn_wc_notify_state_changed;
      else
        *state = svn_wc_notify_state_unchanged;
    }

  /* Looping over the array of incoming propchanges we want to apply: */
  for (i = 0; i < propchanges->nelts; i++)
    {
      const char *propname;
      svn_string_t *conflict_description;
      const svn_prop_t *incoming_change;
      const svn_string_t *from_val, *to_val, *working_val;
      svn_boolean_t is_normal, conflict = FALSE;

      /* For the incoming propchange, figure out the TO and FROM values. */
      incoming_change = &APR_ARRAY_IDX (propchanges, i, svn_prop_t);
      propname = incoming_change->name;
      is_normal = svn_wc_is_normal_prop (propname);
      to_val = incoming_change->value 
                ? svn_string_dup (incoming_change->value, pool) : NULL;
      from_val = apr_hash_get (server_baseprops, propname, APR_HASH_KEY_STRING);
                
      working_val = apr_hash_get (working_props, propname, APR_HASH_KEY_STRING);

      /* Apply the update_change to the pristine hash while we're
         here.  We may or may not write this hash back out later on. */
      apr_hash_set (base_props, propname, APR_HASH_KEY_STRING, to_val);
      
      /* We already know that state is at least `modified', so mark
         that, but remember that we may later upgrade to `merged' or
         even `conflicted'. */
      if (state && is_normal)
        *state = svn_wc_notify_state_changed;

      if (! from_val)  /* adding a new property */
        {
          if (working_val) 
            {
              /* the property already exists in working_props... */
              
              if (svn_string_compare (working_val, to_val))
                {
                  /* The value we want is already there, so it's a
                     'clean merge'. */
                  if (state && is_normal
                      && (*state != svn_wc_notify_state_conflicted))
                    *state = svn_wc_notify_state_merged;
                }

              else
                {
                  conflict = TRUE;
                  conflict_description = 
                    svn_string_createf 
                    (pool, 
                     _("Trying to add new property '%s' with value '%s',\n"
                       "but property already exists with value '%s'."),
                     propname, to_val->data, working_val->data);
                }
            }
          
          else  /* property dosen't yet exist in working_props...  */
            {
              /* so just set it */
              apr_hash_set (working_props, propname,
                            APR_HASH_KEY_STRING, to_val);
            }
        }
      
      else  /* changing or deleting an existing property */
        {
          if (! working_val)  /* ... but it's not in the working_props */
            {
              if (to_val)
                {
                  conflict = TRUE;
                  conflict_description = 
                    svn_string_createf 
                    (pool, 
                     _("Trying to change property '%s' from '%s' to '%s',\n"
                       "but the property does not exist."),
                     propname, from_val->data, to_val->data);
                }
              else
                {
                  /* The property to be deleted doesn't exist, so it's
                     a 'clean merge'. */
                  if (state && is_normal
                      && (*state != svn_wc_notify_state_conflicted))
                    *state = svn_wc_notify_state_merged;
                }
            }
          
          else  /* property already exists */
            {
              if (svn_string_compare (working_val, from_val))
                {
                  /* property has the expected 'from' value, so it's
                     fine to set it to the 'to' value.  */
                  apr_hash_set (working_props, propname,
                                APR_HASH_KEY_STRING, to_val);
                }

              else if (!to_val && !svn_string_compare (from_val, working_val))
                {
                  conflict = TRUE;
                  conflict_description =
                    svn_string_createf
                    (pool, _("Trying to delete property '%s' but value"
                             " has been modified from '%s' to '%s'."),
                     propname, from_val->data, working_val->data);
                }

              else if (svn_string_compare (working_val, to_val))
                {
                  /* The value we want is already there, so it's a
                     'clean merge'. */
                  if (state && is_normal
                      && (*state != svn_wc_notify_state_conflicted))
                    *state = svn_wc_notify_state_merged;
                }
              else /* property has some random value */
                {
                  conflict = TRUE;
                  conflict_description = 
                    svn_string_createf 
                    (pool, 
                     _("Trying to change property '%s' from '%s' to '%s',\n"
                       "but property already exists with value '%s'."),
                     propname, from_val->data, to_val->data, working_val->data);
                }
            }
        }

      /* merging logic complete, now we need to possibly log conflict
         data to tmpfiles.  */
      
      if (conflict)
        {
          if (state && is_normal)
            *state = svn_wc_notify_state_conflicted;
          
          if (dry_run)
            continue;   /* skip to next incoming change */
          
          if (! reject_tmp_fp)
            {
              /* This is the very first prop conflict found on this item. */
              const char *tmppath, *tmpname;
              
              /* Get path to temporary local prop file, and reserve a
                 .prej filename based on it. */
              SVN_ERR (svn_wc__prop_path (&tmppath, full_path, adm_access,
                                          TRUE, pool));              
              SVN_ERR (svn_io_open_unique_file (&reject_tmp_fp,
                                                &reject_tmp_path,
                                                tmppath, SVN_WC__PROP_REJ_EXT,
                                                FALSE, pool));
              
              /* reject_tmp_path is an absolute path at this point,
                 but that's no good for us.  We need to convert this
                 path to a *relative* path to use in the logfile. */
              tmpname = svn_path_basename (reject_tmp_path, pool);
              
              if (is_dir)  /* Dealing with directory "path" */                
                reject_tmp_path = svn_wc__adm_path ("", TRUE, /* use tmp */
                                                    pool, tmpname, NULL);
              else /* Dealing with file "path/name" */
                reject_tmp_path = svn_wc__adm_path ("", TRUE, pool,
                                                    SVN_WC__ADM_PROPS,
                                                    tmpname, NULL);
            }
          
          /* Append the conflict to the open tmp/PROPS/---.prej file */
          SVN_ERR (append_prop_conflict (reject_tmp_fp, conflict_description,
                                         pool));
        }

    }  /* foreach propchange ... */

  /* Finished applying all incoming propchanges to our hashes! */
  
  if (dry_run)
    return SVN_NO_ERROR;

  /* Write our property hashes into temporary files.  Notice that the
     paths computed are ABSOLUTE pathnames, which is what our disk
     routines require.*/
  SVN_ERR (svn_wc__prop_path (&local_prop_tmp_path, full_path,
                              adm_access, TRUE, pool));
  
  /* Write the merged working prop hash to path/.svn/tmp/props/name or
     path/.svn/tmp/dir-props */
  SVN_ERR (svn_wc__save_prop_file (local_prop_tmp_path, working_props, pool));
  
  /* Compute pathnames for the "mv" log entries.  Notice that these
     paths are RELATIVE pathnames (each beginning with ".svn/"), so
     that each .svn subdir remains separable when executing run_log().  */
  tmp_props = apr_pstrdup (pool, local_prop_tmp_path + access_len + slash);
  real_props = apr_pstrdup (pool, local_propfile_path + access_len + slash);
  
  /* Write log entry to move working tmp copy to real working area. */
  svn_xml_make_open_tag (entry_accum,
                         pool,
                         svn_xml_self_closing,
                         SVN_WC__LOG_MV,
                         SVN_WC__LOG_ATTR_NAME,
                         tmp_props,
                         SVN_WC__LOG_ATTR_DEST,
                         real_props,
                         NULL);

  /* Make props readonly */
  svn_xml_make_open_tag (entry_accum,
                         pool,
                         svn_xml_self_closing,
                         SVN_WC__LOG_READONLY,
                         SVN_WC__LOG_ATTR_NAME,
                         real_props,
                         NULL);

  /* Repeat the above steps for the base properties if required. */
  if (base_merge)
    {
      const char *tmp_prop_base, *real_prop_base;

      SVN_ERR (svn_wc__prop_base_path (&base_prop_tmp_path, full_path,
                                       adm_access, TRUE, pool));
      SVN_ERR (svn_wc__save_prop_file (base_prop_tmp_path, base_props, pool));

      tmp_prop_base = apr_pstrdup (pool,
                                   base_prop_tmp_path + access_len + slash);
      real_prop_base = apr_pstrdup (pool,
                                    base_propfile_path + access_len + slash);

      svn_xml_make_open_tag (entry_accum,
                             pool,
                             svn_xml_self_closing,
                             SVN_WC__LOG_MV,
                             SVN_WC__LOG_ATTR_NAME,
                             tmp_prop_base,
                             SVN_WC__LOG_ATTR_DEST,
                             real_prop_base,
                             NULL);

      svn_xml_make_open_tag (entry_accum,
                             pool,
                             svn_xml_self_closing,
                             SVN_WC__LOG_READONLY,
                             SVN_WC__LOG_ATTR_NAME,
                             real_prop_base,
                             NULL);
    }

  if (reject_tmp_fp)
    {
      /* There's a .prej file sitting in .svn/tmp/ somewhere.  Deal
         with the conflicts.  */

      /* First, _close_ this temporary conflicts file.  We've been
         appending to it all along. */
      SVN_ERR (svn_io_file_close (reject_tmp_fp, pool));
                                  
      /* Now try to get the name of a pre-existing .prej file from the
         entries file */
      SVN_ERR (svn_wc__get_existing_prop_reject_file (&reject_path,
                                                      adm_access,
                                                      entryname,
                                                      pool));

      if (! reject_path)
        {
          /* Reserve a new .prej file *above* the .svn/ directory by
             opening and closing it. */
          const char *reserved_path;
          const char *full_reject_path;

          full_reject_path = svn_path_join 
            (access_path, is_dir ? SVN_WC__THIS_DIR_PREJ : name, pool);

          SVN_ERR (svn_io_open_unique_file (&reject_fp, &reserved_path,
                                            full_reject_path,
                                            SVN_WC__PROP_REJ_EXT,
                                            FALSE, pool));

          SVN_ERR (svn_io_file_close (reject_fp, pool));
          
          /* This file will be overwritten when the log is run; that's
             ok, because at least now we have a reservation on
             disk. */

          /* Now just get the name of the reserved file.  This is the
             "relative" path we will use in the log entry. */
          reject_path = svn_path_basename (reserved_path, pool);
        }

      /* We've now guaranteed that some kind of .prej file exists
         above the .svn/ dir.  We write log entries to append our
         conflicts to it. */      
      svn_xml_make_open_tag (entry_accum,
                             pool,
                             svn_xml_self_closing,
                             SVN_WC__LOG_APPEND,
                             SVN_WC__LOG_ATTR_NAME,
                             reject_tmp_path,
                             SVN_WC__LOG_ATTR_DEST,
                             reject_path,
                             NULL);

      /* And of course, delete the temporary reject file. */
      svn_xml_make_open_tag (entry_accum,
                             pool,
                             svn_xml_self_closing,
                             SVN_WC__LOG_RM,
                             SVN_WC__LOG_ATTR_NAME,
                             reject_tmp_path,
                             NULL);
      
      /* Mark entry as "conflicted" with a particular .prej file. */
      svn_xml_make_open_tag (entry_accum,
                             pool,
                             svn_xml_self_closing,
                             SVN_WC__LOG_MODIFY_ENTRY,
                             SVN_WC__LOG_ATTR_NAME,
                             entryname,
                             SVN_WC__ENTRY_ATTR_PREJFILE,
                             reject_path,
                             NULL);      

    } /* if (reject_tmp_fp) */
  

  return SVN_NO_ERROR;
}




/* DEPRECATED: This function only exists as the implementation of the
   deprecated svn_wc_merge_prop_diffs().  */
static svn_error_t *
svn_wc__merge_prop_diffs (svn_wc_notify_state_t *state,
                          svn_wc_adm_access_t *adm_access,
                          const char *name,
                          const apr_array_header_t *propchanges,
                          svn_boolean_t base_merge,
                          svn_boolean_t dry_run,
                          apr_pool_t *pool,
                          svn_stringbuf_t **entry_accum)
{
  int i;
  svn_boolean_t is_dir;
  
  /* Zillions of pathnames to compute!  yeargh!  */
  const char *base_propfile_path, *local_propfile_path;
  const char *base_prop_tmp_path, *local_prop_tmp_path;
  const char *tmp_prop_base, *real_prop_base;
  const char *tmp_props, *real_props;

  const char *access_path = svn_wc_adm_access_path (adm_access);
  int access_len = strlen (access_path);
  int slash;

  const char *entryname;
  const char *full_path;
  
  apr_array_header_t *local_propchanges; /* propchanges that the user
                                            has made since last update */
  apr_hash_t *localhash;   /* all `working' properties */
  apr_hash_t *basehash;    /* all `pristine' properties */

  /* For writing conflicts to a .prej file */
  apr_file_t *reject_fp = NULL;           /* the real conflicts file */
  const char *reject_path = NULL;

  apr_file_t *reject_tmp_fp = NULL;       /* the temporary conflicts file */
  const char *reject_tmp_path = NULL;

  /* Empty path and paths ending in / don't need an extra slash removed */
  if (access_len == 0 || access_path[access_len - 1] == '/')
    slash = 0;
  else
    slash = 1;

  if (name == NULL)
    {
      /* We must be merging props on the directory PATH  */
      entryname = SVN_WC_ENTRY_THIS_DIR;
      full_path = access_path;
      is_dir = TRUE;
    }
  else
    {
      /* We must be merging props on the file PATH/NAME */
      entryname = name;
      full_path = svn_path_join (access_path, name, pool);
      is_dir = FALSE;
    }

  /* Get paths to the local and pristine property files. */
  SVN_ERR (svn_wc__prop_path (&local_propfile_path, full_path, adm_access,
                              FALSE, pool));
  
  SVN_ERR (svn_wc__prop_base_path (&base_propfile_path, full_path, adm_access,
                                   FALSE, pool));

  /* Load the base & working property files into hashes */
  localhash = apr_hash_make (pool);
  basehash = apr_hash_make (pool);
  
  SVN_ERR (svn_wc__load_prop_file (base_propfile_path, basehash, pool));
  
  SVN_ERR (svn_wc__load_prop_file (local_propfile_path, localhash, pool));
  
  /* Deduce any local propchanges the user has made since the last
     update.  */
  SVN_ERR (svn_prop_diffs (&local_propchanges, localhash, basehash, pool));
  
  if (state)
    {
      /* Start out assuming no conflicts.  Don't bother to examine
         propchanges->nelts yet; even if we knew there were
         propchanges, we wouldn't yet know if they are "normal" props,
         as opposed wc or entry props.  */ 
      if (local_propchanges->nelts > 0)
        *state = svn_wc_notify_state_changed;
      else
        *state = svn_wc_notify_state_unchanged;
    }

  /* Looping over the array of `update' propchanges we want to apply: */
  for (i = 0; i < propchanges->nelts; i++)
    {
      int j;
      int found_match = 0;          
      const svn_string_t *conflict_description;
      const svn_prop_t *update_change;
      const svn_prop_t *local_change = NULL;
      const svn_string_t *value;
      svn_boolean_t is_normal;

      update_change = &APR_ARRAY_IDX (propchanges, i, svn_prop_t);
      is_normal = svn_wc_is_normal_prop (update_change->name);
      value = update_change->value 
              ? svn_string_dup (update_change->value, pool) 
              : NULL;

      /* Apply the update_change to the pristine hash, no questions
         asked. */
      apr_hash_set (basehash, update_change->name, APR_HASH_KEY_STRING, value);
      
      /* We already know that state is at least `modified', so mark
         that, but remember that we may later upgrade to `merged' or
         even `conflicted'. */
      if (state && is_normal)
        *state = svn_wc_notify_state_changed;

      /* Now, does the update_change conflict with some local change?  */
      
      /* First check if the property name even exists in our list
         of local changes... */
      for (j = 0; j < local_propchanges->nelts; j++)

        {
          local_change = &APR_ARRAY_IDX (local_propchanges, j, svn_prop_t);
          if (strcmp (local_change->name, update_change->name) == 0)
            {
              found_match = 1;
              break;
            }
        }
      
      if (found_match)
        {
          svn_boolean_t conflict
            = svn_wc__conflicting_propchanges_p (&conflict_description,
                                                 local_change,
                                                 update_change,
                                                 pool);

          /* Now see if the two changes actually conflict */
          if (conflict)
            {
              /* Found one!  Reflect the conflict in the notification state. */
              if (state && is_normal)
                *state = svn_wc_notify_state_conflicted;

              if (dry_run)
                continue;

              if (! reject_tmp_fp)
                {
                  /* This is the very first prop conflict found on this
                     node. */
                  const char *tmppath;
                  const char *tmpname;

                  /* Get path to /temporary/ local prop file */
                  SVN_ERR (svn_wc__prop_path (&tmppath, full_path, adm_access,
                                              TRUE, pool));

                  /* Reserve a .prej file based on it.  */
                  SVN_ERR (svn_io_open_unique_file (&reject_tmp_fp,
                                                    &reject_tmp_path,
                                                    tmppath,
                                                    SVN_WC__PROP_REJ_EXT,
                                                    FALSE,
                                                    pool));

                  /* reject_tmp_path is an absolute path at this point,
                     but that's no good for us.  We need to convert this
                     path to a *relative* path to use in the logfile. */
                  tmpname = svn_path_basename (reject_tmp_path, pool);

                  if (is_dir)
                    {
                      /* Dealing with directory "path" */
                      reject_tmp_path = 
                        svn_wc__adm_path ("",
                                          TRUE, /* use tmp */
                                          pool,
                                          tmpname,
                                          NULL);
                    }
                  else
                    {
                      /* Dealing with file "path/name" */
                      reject_tmp_path = 
                        svn_wc__adm_path ("",
                                          TRUE, 
                                          pool,
                                          SVN_WC__ADM_PROPS,
                                          tmpname,
                                          NULL);
                    }               
                }

              /* Append the conflict to the open tmp/PROPS/---.prej file */
              SVN_ERR (append_prop_conflict (reject_tmp_fp,
                                             conflict_description,
                                             pool));

              continue;  /* skip to the next update_change */
            }
          else  /* not a conflict */
            {
              /* Reflect the merge in the notification state, but
                 don't override any previous conflicted state. */
              if (state && is_normal
                  && (*state != svn_wc_notify_state_conflicted))
                *state = svn_wc_notify_state_merged;
            }
        }
      
      /* Every time we reach this point, it's not a conflict, so we
         can safely apply the update_change to our working property hash. */
      apr_hash_set (localhash,
                    update_change->name, APR_HASH_KEY_STRING,
                    value);
    }
  
  
  if (dry_run)
    return SVN_NO_ERROR;

  /* Done merging property changes into both pristine and working
  hashes.  Now we write them to temporary files.  Notice that the
  paths computed are ABSOLUTE pathnames, which is what our disk
  routines require.*/

  SVN_ERR (svn_wc__prop_path (&local_prop_tmp_path, full_path, adm_access, TRUE,
                              pool));
  
  /* Write the merged local prop hash to path/.svn/tmp/props/name or
     path/.svn/tmp/dir-props */
  SVN_ERR (svn_wc__save_prop_file (local_prop_tmp_path, localhash, pool));
  
  /* Compute pathnames for the "mv" log entries.  Notice that these
     paths are RELATIVE pathnames (each beginning with ".svn/"), so
     that each .svn subdir remains separable when executing run_log().  */
  tmp_props = apr_pstrdup (pool, local_prop_tmp_path + access_len + slash);
  real_props = apr_pstrdup (pool, local_propfile_path + access_len + slash);
  
  /* Write log entry to move working tmp copy to real working area. */
  svn_xml_make_open_tag (entry_accum,
                         pool,
                         svn_xml_self_closing,
                         SVN_WC__LOG_MV,
                         SVN_WC__LOG_ATTR_NAME,
                         tmp_props,
                         SVN_WC__LOG_ATTR_DEST,
                         real_props,
                         NULL);

  /* Make props readonly */
  svn_xml_make_open_tag (entry_accum,
                         pool,
                         svn_xml_self_closing,
                         SVN_WC__LOG_READONLY,
                         SVN_WC__LOG_ATTR_NAME,
                         real_props,
                         NULL);

  /* Repeat the above steps for the base properties if required */
  if (base_merge)
    {
      SVN_ERR (svn_wc__prop_base_path (&base_prop_tmp_path, full_path,
                                       adm_access, TRUE, pool));
      SVN_ERR (svn_wc__save_prop_file (base_prop_tmp_path, basehash, pool));

      tmp_prop_base = apr_pstrdup (pool,
                                   base_prop_tmp_path + access_len + slash);
      real_prop_base = apr_pstrdup (pool,
                                    base_propfile_path + access_len + slash);

      svn_xml_make_open_tag (entry_accum,
                             pool,
                             svn_xml_self_closing,
                             SVN_WC__LOG_MV,
                             SVN_WC__LOG_ATTR_NAME,
                             tmp_prop_base,
                             SVN_WC__LOG_ATTR_DEST,
                             real_prop_base,
                             NULL);
      svn_xml_make_open_tag (entry_accum,
                             pool,
                             svn_xml_self_closing,
                             SVN_WC__LOG_READONLY,
                             SVN_WC__LOG_ATTR_NAME,
                             real_prop_base,
                             NULL);
    }


  if (reject_tmp_fp)
    {
      /* There's a .prej file sitting in .svn/tmp/ somewhere.  Deal
         with the conflicts.  */

      /* First, _close_ this temporary conflicts file.  We've been
         appending to it all along. */
      SVN_ERR (svn_io_file_close (reject_tmp_fp, pool));
                                  
      /* Now try to get the name of a pre-existing .prej file from the
         entries file */
      SVN_ERR (svn_wc__get_existing_prop_reject_file (&reject_path,
                                                      adm_access,
                                                      entryname,
                                                      pool));

      if (! reject_path)
        {
          /* Reserve a new .prej file *above* the .svn/ directory by
             opening and closing it. */
          const char *reserved_path;
          const char *full_reject_path;

          full_reject_path
            = svn_path_join (access_path,
                             is_dir ? SVN_WC__THIS_DIR_PREJ : name,
                             pool);

          SVN_ERR (svn_io_open_unique_file (&reject_fp,
                                            &reserved_path,
                                            full_reject_path,
                                            SVN_WC__PROP_REJ_EXT,
                                            FALSE,
                                            pool));

          SVN_ERR (svn_io_file_close (reject_fp, pool));
          
          /* This file will be overwritten when the log is run; that's
             ok, because at least now we have a reservation on
             disk. */

          /* Now just get the name of the reserved file.  This is the
             "relative" path we will use in the log entry. */
          reject_path = svn_path_basename (reserved_path, pool);
        }

      /* We've now guaranteed that some kind of .prej file exists
         above the .svn/ dir.  We write log entries to append our
         conflicts to it. */      
      svn_xml_make_open_tag (entry_accum,
                             pool,
                             svn_xml_self_closing,
                             SVN_WC__LOG_APPEND,
                             SVN_WC__LOG_ATTR_NAME,
                             reject_tmp_path,
                             SVN_WC__LOG_ATTR_DEST,
                             reject_path,
                             NULL);

      /* And of course, delete the temporary reject file. */
      svn_xml_make_open_tag (entry_accum,
                             pool,
                             svn_xml_self_closing,
                             SVN_WC__LOG_RM,
                             SVN_WC__LOG_ATTR_NAME,
                             reject_tmp_path,
                             NULL);
      
      /* Mark entry as "conflicted" with a particular .prej file. */
      svn_xml_make_open_tag (entry_accum,
                             pool,
                             svn_xml_self_closing,
                             SVN_WC__LOG_MODIFY_ENTRY,
                             SVN_WC__LOG_ATTR_NAME,
                             entryname,
                             SVN_WC__ENTRY_ATTR_PREJFILE,
                             reject_path,
                             NULL);      

    } /* if (reject_tmp_fp) */
  
  /* At this point, we need to write log entries that bump revision
     number and set new entry timestamps.  The caller of this function
     should (hopefully) add those commands to the log accumulator. */

  return SVN_NO_ERROR;
}


/* This is DEPRECATED, use svn_wc_merge_props() instead. */
svn_error_t *
svn_wc_merge_prop_diffs (svn_wc_notify_state_t *state,
                         const char *path,
                         svn_wc_adm_access_t *adm_access,
                         const apr_array_header_t *propchanges,
                         svn_boolean_t base_merge,
                         svn_boolean_t dry_run,
                         apr_pool_t *pool)
{
  const svn_wc_entry_t *entry;
  const char *parent, *base_name;
  svn_stringbuf_t *log_accum;
  apr_file_t *log_fp = NULL;

  SVN_ERR (svn_wc_entry (&entry, path, adm_access, FALSE, pool));
  if (entry == NULL)
    return svn_error_createf (SVN_ERR_UNVERSIONED_RESOURCE, NULL,
                              _("'%s' is not under version control"),
                              svn_path_local_style (path, pool));

  /* Notice that we're not using svn_path_split_if_file(), because
     that looks at the actual working file.  Its existence shouldn't
     matter, so we're looking at entry->kind instead. */
  switch (entry->kind)
    {
    case svn_node_dir:
      parent = path;
      base_name = NULL;
      break;
    case svn_node_file:
      svn_path_split (path, &parent, &base_name, pool);
      break;
    default:
      return SVN_NO_ERROR; /* ### svn_node_none or svn_node_unknown */
    }

  if (! dry_run)
    {
      SVN_ERR (svn_wc__open_adm_file (&log_fp, parent, SVN_WC__ADM_LOG,
                                      (APR_WRITE | APR_CREATE), /* not excl */
                                      pool));
      log_accum = svn_stringbuf_create ("", pool);
    }
  
  /* Note that while this routine does the "real" work, it's only
     prepping tempfiles and writing log commands.  */
  SVN_ERR (svn_wc__merge_prop_diffs (state, adm_access, base_name,
                                     propchanges, base_merge, dry_run,
                                     pool, &log_accum));

  if (! dry_run)
    {
      SVN_ERR_W (svn_io_file_write_full (log_fp, log_accum->data, 
                                         log_accum->len, NULL, pool),
                 apr_psprintf (pool, _("Error writing log for '%s'"),
                               svn_path_local_style (path, pool)));

      SVN_ERR (svn_wc__close_adm_file (log_fp, parent, SVN_WC__ADM_LOG,
                                       1, /* sync */ pool));
      SVN_ERR (svn_wc__run_log (adm_access, NULL, pool));
    }

  return SVN_NO_ERROR;
}



/*------------------------------------------------------------------*/

/*** Private 'wc prop' functions ***/

/* A clone of svn_wc_prop_list, for the most part, except that it
   returns 'wc' props instead of normal props.  */
static svn_error_t *
wcprop_list (apr_hash_t **props,
             const char *path,
             svn_wc_adm_access_t *adm_access,
             apr_pool_t *pool)
{
  svn_node_kind_t kind, pkind;
  const char *prop_path;
  
  *props = apr_hash_make (pool);

  /* Check validity of PATH */
  SVN_ERR( svn_io_check_path (path, &kind, pool) );
  
#if 0
  if (kind == svn_node_none)
    return svn_error_createf (SVN_ERR_BAD_FILENAME, NULL,
                              _("'%s' does not exist"),
                              svn_path_local_style (path, pool));
  
  if (kind == svn_node_unknown)
    return svn_error_createf (SVN_ERR_NODE_UNKNOWN_KIND, NULL,
                              _("Unknown node kind: '%s'"),
                              svn_path_local_style (path, pool));
#endif

  /* Construct a path to the relevant property file */
  SVN_ERR( svn_wc__wcprop_path (&prop_path, path, adm_access, FALSE, pool) );

  /* Does the property file exist? */
  SVN_ERR( svn_io_check_path (prop_path, &pkind, pool) );
  
  if (pkind == svn_node_none)
    /* No property file exists.  Just go home, with an empty hash. */
    return SVN_NO_ERROR;
  
  /* else... */

  SVN_ERR( svn_wc__load_prop_file (prop_path, *props, pool) );

  return SVN_NO_ERROR;
}


svn_error_t *
svn_wc__wcprop_get (const svn_string_t **value,
                    const char *name,
                    const char *path,
                    svn_wc_adm_access_t *adm_access,
                    apr_pool_t *pool)
{
  svn_error_t *err;
  apr_hash_t *prophash;

  err = wcprop_list (&prophash, path, adm_access, pool);
  if (err)
    return
      svn_error_quick_wrap
      (err, _("Failed to load properties from disk"));

  *value = apr_hash_get (prophash, name, APR_HASH_KEY_STRING);
  return SVN_NO_ERROR;
}


svn_error_t *
svn_wc__wcprop_set (const char *name,
                    const svn_string_t *value,
                    const char *path,
                    svn_wc_adm_access_t *adm_access,
                    apr_pool_t *pool)
{
  svn_error_t *err;
  apr_hash_t *prophash;
  apr_file_t *fp = NULL;

  err = wcprop_list (&prophash, path, adm_access, pool);
  if (err)
    return
      svn_error_quick_wrap
      (err, _("Failed to load properties from disk"));

  /* Now we have all the properties in our hash.  Simply merge the new
     property into it. */
  apr_hash_set (prophash, name, APR_HASH_KEY_STRING, value);

  /* Open the propfile for writing. */
  SVN_ERR (svn_wc__open_props (&fp, 
                               path, /* open in PATH */
                               (APR_WRITE | APR_CREATE | APR_BUFFERED),
                               0, /* not base props */
                               1, /* we DO want wcprops */
                               pool));
  /* Write. */
  SVN_ERR_W (svn_hash_write (prophash, fp, pool),
             apr_psprintf (pool,
                           _("Cannot write property hash for '%s'"),
                           svn_path_local_style (path, pool)));
  
  /* Close file, and doing an atomic "move". */
  SVN_ERR (svn_wc__close_props (fp, path, 0, 1,
                                1, /* sync! */
                                pool));

  return SVN_NO_ERROR;
}




/*------------------------------------------------------------------*/

/*** Public Functions ***/


svn_error_t *
svn_wc_prop_list (apr_hash_t **props,
                  const char *path,
                  svn_wc_adm_access_t *adm_access,
                  apr_pool_t *pool)
{
  const char *prop_path;

  *props = apr_hash_make (pool);

  /* Construct a path to the relevant property file */
  SVN_ERR (svn_wc__prop_path (&prop_path, path, adm_access, FALSE, pool));

  /* svn_wc__load_prop_file checks if the prop file exists */
  return svn_wc__load_prop_file (prop_path, *props, pool);
}





svn_error_t *
svn_wc_prop_get (const svn_string_t **value,
                 const char *name,
                 const char *path,
                 svn_wc_adm_access_t *adm_access,
                 apr_pool_t *pool)
{
  svn_error_t *err;
  apr_hash_t *prophash;

  enum svn_prop_kind kind = svn_property_kind (NULL, name);

  if (kind == svn_prop_wc_kind)
    {
      return svn_wc__wcprop_get (value, name, path, adm_access, pool);
    }
  if (kind == svn_prop_entry_kind)
    {
      return svn_error_createf   /* we don't do entry properties here */
        (SVN_ERR_BAD_PROP_KIND, NULL,
         _("Property '%s' is an entry property"), name);
    }
  else  /* regular prop */
    {
      err = svn_wc_prop_list (&prophash, path, adm_access, pool);
      if (err)
        return
          svn_error_quick_wrap
          (err, _("Failed to load properties from disk"));
      
      *value = apr_hash_get (prophash, name, APR_HASH_KEY_STRING);

      return SVN_NO_ERROR;
    }
}


/* The special Subversion properties are not valid for all node kinds.
   Return an error if NAME is an invalid Subversion property for PATH which
   is of kind NODE_KIND. */
static svn_error_t *
validate_prop_against_node_kind (const char *name,
                                 const char *path,
                                 svn_node_kind_t node_kind,
                                 apr_pool_t *pool)
{

  const char *file_prohibit[] = { SVN_PROP_IGNORE,
                                  SVN_PROP_EXTERNALS,
                                  NULL };
  const char *dir_prohibit[] = { SVN_PROP_EXECUTABLE,
                                 SVN_PROP_KEYWORDS,
                                 SVN_PROP_EOL_STYLE,
                                 SVN_PROP_MIME_TYPE,
                                 SVN_PROP_NEEDS_LOCK,
                                 NULL };
  const char **node_kind_prohibit;

  switch (node_kind)
    {
    case svn_node_dir:
      node_kind_prohibit = dir_prohibit;
      while (*node_kind_prohibit)
        if (strcmp (name, *node_kind_prohibit++) == 0)
          return svn_error_createf (SVN_ERR_ILLEGAL_TARGET, NULL,
                                    _("Cannot set '%s' on a directory ('%s')"),
                                    name, svn_path_local_style (path, pool));
      break;
    case svn_node_file:
      node_kind_prohibit = file_prohibit;
      while (*node_kind_prohibit)
        if (strcmp (name, *node_kind_prohibit++) == 0)
          return svn_error_createf (SVN_ERR_ILLEGAL_TARGET, NULL,
                                    _("Cannot set '%s' on a file ('%s')"),
                                    name,
                                    svn_path_local_style (path, pool));
      break;
    default:
      return svn_error_createf (SVN_ERR_NODE_UNEXPECTED_KIND, NULL,
                                _("'%s' is not a file or directory"),
                                svn_path_local_style (path, pool));
    }

  return SVN_NO_ERROR;
}                             


static svn_error_t *
validate_eol_prop_against_file (const char *path, 
                                svn_wc_adm_access_t *adm_access,
                                apr_pool_t *pool)
{
  apr_file_t *fp;
  svn_stream_t *read_stream, *write_stream;
  svn_error_t *err;
  const svn_string_t *mime_type;

  /* See if this file has been determined to be binary. */
  SVN_ERR (svn_wc_prop_get (&mime_type, SVN_PROP_MIME_TYPE, path, adm_access,
                            pool));
  if (mime_type && svn_mime_type_is_binary (mime_type->data))
    return svn_error_createf 
      (SVN_ERR_ILLEGAL_TARGET, NULL,
       _("File '%s' has binary mime type property"),
       svn_path_local_style (path, pool));

  /* Open PATH. */
  SVN_ERR (svn_io_file_open (&fp, path, 
                             (APR_READ | APR_BINARY | APR_BUFFERED),
                             0, pool));

  /* Get a READ_STREAM from the file we just opened. */
  read_stream = svn_stream_from_aprfile (fp, pool);

  /* Now, make an empty WRITE_STREAM. */
  write_stream = svn_stream_empty (pool);

  /* Do a newline translation.  Of course, all we really care about
     here is whether or not the function fails on inconsistent line
     endings.  The function is "translating" to an empty stream.  This
     is sneeeeeeeeeeeaky. */
  err = svn_subst_translate_stream3 (read_stream, write_stream,
                                     "", FALSE, NULL, FALSE, pool);
  if (err && err->apr_err == SVN_ERR_IO_INCONSISTENT_EOL)
    return svn_error_createf (SVN_ERR_ILLEGAL_TARGET, err,
                              _("File '%s' has inconsistent newlines"),
                              svn_path_local_style (path, pool));
  else if (err)
    return err;

  SVN_ERR (svn_io_file_close (fp, pool));
  return SVN_NO_ERROR;
}


svn_error_t *
svn_wc_prop_set2 (const char *name,
                  const svn_string_t *value,
                  const char *path,
                  svn_wc_adm_access_t *adm_access,
                  svn_boolean_t skip_checks,
                  apr_pool_t *pool)
{
  svn_error_t *err;
  apr_hash_t *prophash;
  apr_file_t *fp = NULL;
  apr_hash_t *old_keywords;
  svn_stringbuf_t *new_value = NULL;
  svn_node_kind_t kind;
  enum svn_prop_kind prop_kind = svn_property_kind (NULL, name);

  SVN_ERR (svn_io_check_path (path, &kind, pool));

  if (prop_kind == svn_prop_wc_kind)
    return svn_wc__wcprop_set (name, value, path, adm_access, pool);
  else if (prop_kind == svn_prop_entry_kind)
    return svn_error_createf   /* we don't do entry properties here */
      (SVN_ERR_BAD_PROP_KIND, NULL,
       _("Property '%s' is an entry property"), name);

  /* Else, handle a regular property: */

  /* Setting an inappropriate property is not allowed (unless
     overridden by 'skip_checks', in some circumstances).  Deleting an
     inappropriate property is allowed, however, since older clients
     allowed (and other clients possibly still allow) setting it in
     the first place. */
  if (value)
    {
      SVN_ERR (validate_prop_against_node_kind (name, path, kind, pool));
      if (!skip_checks && (strcmp (name, SVN_PROP_EOL_STYLE) == 0))
        {
          new_value = svn_stringbuf_create_from_string (value, pool);
          svn_stringbuf_strip_whitespace (new_value);
          SVN_ERR (validate_eol_prop_against_file (path, adm_access, pool));
        }
      else if (!skip_checks && (strcmp (name, SVN_PROP_MIME_TYPE) == 0))
        {
          new_value = svn_stringbuf_create_from_string (value, pool);
          svn_stringbuf_strip_whitespace (new_value);
          SVN_ERR (svn_mime_type_validate (new_value->data, pool));
        }
      else if (strcmp (name, SVN_PROP_IGNORE) == 0
               || strcmp (name, SVN_PROP_EXTERNALS) == 0)
        {
          /* Make sure that the last line ends in a newline */
          if (value->data[value->len - 1] != '\n')
            {
              new_value = svn_stringbuf_create_from_string (value, pool);
              svn_stringbuf_appendbytes (new_value, "\n", 1);
            }

          /* Make sure this is a valid externals property.  Do not
             allow 'skip_checks' to override, as there is no circumstance in
             which this is proper (because there is no circumstance in
             which Subversion can handle it). */
          if (strcmp (name, SVN_PROP_EXTERNALS) == 0)
            {
              /* We don't allow "." nor ".." as target directories in
                 an svn:externals line.  As it happens, our parse code
                 checks for this, so all we have to is invoke it --
                 we're not interested in the parsed result, only in
                 whether or the parsing errored. */
              SVN_ERR (svn_wc_parse_externals_description2
                       (NULL, path, value->data, pool));
            }
        }
      else if (strcmp (name, SVN_PROP_KEYWORDS) == 0)
        {
          new_value = svn_stringbuf_create_from_string (value, pool);
          svn_stringbuf_strip_whitespace (new_value);
        }
    }

  if (kind == svn_node_file && strcmp (name, SVN_PROP_EXECUTABLE) == 0)
    {
      /* If the svn:executable property was set, then chmod +x.
         If the svn:executable property was deleted (NULL value passed
         in), then chmod -x. */
      if (value == NULL)
        {
          SVN_ERR (svn_io_set_file_executable (path, FALSE, TRUE, pool));
        }
      else
        {
          /* Since we only check if the property exists or not, force the
             property value to a specific value */
          static const svn_string_t executable_value =
            {
              SVN_PROP_EXECUTABLE_VALUE,
              sizeof (SVN_PROP_EXECUTABLE_VALUE) - 1
            };

          value = &executable_value;
          SVN_ERR (svn_io_set_file_executable (path, TRUE, TRUE, pool));
        }
    }

  if (kind == svn_node_file && strcmp (name, SVN_PROP_NEEDS_LOCK) == 0)
    {
      /* If the svn:needs-lock property was set to NULL, set the file
         to read-write */
      if (value == NULL)
        {
          SVN_ERR (svn_io_set_file_read_write_carefully (path, TRUE, 
                                                         FALSE, pool));
        }
      else
        {
          /* Since we only check if the property exists or not, force the
             property value to a specific value */
          static const svn_string_t needs_lock_value =
            {
              SVN_PROP_NEEDS_LOCK_VALUE,
              sizeof (SVN_PROP_NEEDS_LOCK_VALUE) - 1
            };

          value = &needs_lock_value;
          /* And we'll set the file to read-only at commit time. */
        }
    }

  err = svn_wc_prop_list (&prophash, path, adm_access, pool);
  if (err)
    return
      svn_error_quick_wrap
      (err, _("Failed to load properties from disk"));

  /* If we're changing this file's list of expanded keywords, then
   * we'll need to invalidate its text timestamp, since keyword
   * expansion affects the comparison of working file to text base.
   *
   * Here we retrieve the old list of expanded keywords; after the
   * property is set, we'll grab the new list and see if it differs
   * from the old one.
   */
  if (kind == svn_node_file && strcmp (name, SVN_PROP_KEYWORDS) == 0)
    SVN_ERR (svn_wc__get_keywords (&old_keywords, path, adm_access, NULL,
                                   pool));

  /* Now we have all the properties in our hash.  Simply merge the new
     property into it. */
  if (new_value)
    value = svn_string_create_from_buf (new_value, pool);
  apr_hash_set (prophash, name, APR_HASH_KEY_STRING, value);
  
  /* Open the propfile for writing. */
  SVN_ERR (svn_wc__open_props (&fp, 
                               path, /* open in PATH */
                               (APR_WRITE | APR_CREATE | APR_BUFFERED),
                               0, /* not base props */
                               0, /* not wcprops */
                               pool));
  /* Write. */
  SVN_ERR_W (svn_hash_write (prophash, fp, pool),
             apr_psprintf (pool, 
                           _("Cannot write property hash for '%s'"),
                           svn_path_local_style (path, pool)));
  
  /* Close file, and doing an atomic "move". */
  SVN_ERR (svn_wc__close_props (fp, path, 0, 0,
                                1, /* sync! */
                                pool));

  if (kind == svn_node_file && strcmp (name, SVN_PROP_KEYWORDS) == 0)
    {
      apr_hash_t *new_keywords;
      SVN_ERR (svn_wc__get_keywords (&new_keywords, path, adm_access, NULL,
                                     pool));

      if (svn_subst_keywords_differ2 (old_keywords, new_keywords, FALSE, pool))
        {
          const char *base_name;
          svn_wc_entry_t tmp_entry;

          /* If we changed the keywords or newlines, void the entry
             timestamp for this file, so svn_wc_text_modified_p() does
             a real (albeit slow) check later on. */
          svn_path_split (path, NULL, &base_name, pool);
          tmp_entry.kind = svn_node_file;
          tmp_entry.text_time = 0;
          SVN_ERR (svn_wc__entry_modify (adm_access, base_name, &tmp_entry,
                                         SVN_WC__ENTRY_MODIFY_TEXT_TIME,
                                         TRUE, pool));
        }
    }

  return SVN_NO_ERROR;
}


svn_error_t *
svn_wc_prop_set (const char *name,
                 const svn_string_t *value,
                 const char *path,
                 svn_wc_adm_access_t *adm_access,
                 apr_pool_t *pool)
{
  return svn_wc_prop_set2 (name, value, path, adm_access, FALSE, pool);
}



svn_boolean_t
svn_wc_is_normal_prop (const char *name)
{
  enum svn_prop_kind kind = svn_property_kind (NULL, name);
  return (kind == svn_prop_regular_kind);
}


svn_boolean_t
svn_wc_is_wc_prop (const char *name)
{
  enum svn_prop_kind kind = svn_property_kind (NULL, name);
  return (kind == svn_prop_wc_kind);
}


svn_boolean_t
svn_wc_is_entry_prop (const char *name)
{
  enum svn_prop_kind kind = svn_property_kind (NULL, name);
  return (kind == svn_prop_entry_kind);
}


/* Helper to optimize svn_wc_props_modified_p().

   If PATH_TO_PROP_FILE is nonexistent, or is of size 4 bytes ("END"),
   then set EMPTY_P to true.   Otherwise set EMPTY_P to false, which
   means that the file must contain real properties.  */
static svn_error_t *
empty_props_p (svn_boolean_t *empty_p,
               const char *path_to_prop_file,
               apr_pool_t *pool)
{
  svn_error_t *err;
  apr_finfo_t finfo;

  err = svn_io_stat (&finfo, path_to_prop_file, APR_FINFO_MIN | APR_FINFO_TYPE,
                     pool);
  if (err)
    {
      if (! APR_STATUS_IS_ENOENT (err->apr_err)
          && ! APR_STATUS_IS_ENOTDIR (err->apr_err))
        return err;

      /* nonexistent */
      svn_error_clear (err);
      *empty_p = TRUE;
    }
  else
    {


      /* If we remove props from a propfile, eventually the file will
         contain nothing but "END\n" */
      if (finfo.filetype == APR_REG && (finfo.size == 4 || finfo.size == 0))
        *empty_p = TRUE;

      else
        *empty_p = FALSE;

      /* If the size is < 4, then something is corrupt.
         If the size is between 4 and 16, then something is corrupt,
         because 16 is the -smallest- the file can possibly be if it
         contained only one property.  So long as we say it is "not
         empty", we will discover such corruption later when we try
         to read the properties from the file. */
    }

  return SVN_NO_ERROR;
}


/* Simple wrapper around empty_props_p, and inversed. */
svn_error_t *
svn_wc__has_props (svn_boolean_t *has_props,
                   const char *path,
                   svn_wc_adm_access_t *adm_access,
                   apr_pool_t *pool)
{
  svn_boolean_t is_empty;
  const char *prop_path;

  SVN_ERR (svn_wc__prop_path (&prop_path, path, adm_access, FALSE, pool));
  SVN_ERR (empty_props_p (&is_empty, prop_path, pool));

  if (is_empty)
    *has_props = FALSE;
  else
    *has_props = TRUE;

  return SVN_NO_ERROR;
}


svn_error_t *
svn_wc_props_modified_p (svn_boolean_t *modified_p,
                         const char *path,
                         svn_wc_adm_access_t *adm_access,
                         apr_pool_t *pool)
{
  svn_boolean_t bempty, wempty;
  const char *prop_path;
  const char *prop_base_path;
  svn_boolean_t different_filesizes, equal_timestamps;
  const svn_wc_entry_t *entry;
  apr_pool_t *subpool = svn_pool_create (pool);

  /* First, get the paths of the working and 'base' prop files. */
  SVN_ERR (svn_wc__prop_path (&prop_path, path, adm_access, FALSE, subpool));
  SVN_ERR (svn_wc__prop_base_path (&prop_base_path, path, adm_access, FALSE,
                                   subpool));

  /* Decide if either path is "empty" of properties. */
  SVN_ERR (empty_props_p (&wempty, prop_path, subpool));
  SVN_ERR (empty_props_p (&bempty, prop_base_path, subpool));

  /* If something is scheduled for replacement, we do *not* want to
     pay attention to any base-props;  they might be residual from the
     old deleted file. */
  SVN_ERR (svn_wc_entry (&entry, path, adm_access, TRUE, subpool));  
  if (entry && (entry->schedule == svn_wc_schedule_replace))
    {
      *modified_p = wempty ? FALSE : TRUE;
      goto cleanup;        
    }

  /* Easy out:  if the base file is empty, we know the answer
     immediately. */
  if (bempty)
    {
      if (! wempty)
        {
          /* base is empty, but working is not */
          *modified_p = TRUE;
          goto cleanup;
        }
      else
        {
          /* base and working are both empty */
          *modified_p = FALSE;
          goto cleanup;
        }
    }

  /* OK, so the base file is non-empty.  One more easy out: */
  if (wempty)
    {
      /* base exists, working is empty */
      *modified_p = TRUE;
      goto cleanup;
    }

  /* At this point, we know both files exists.  Therefore we have no
     choice but to start checking their contents. */
  
  /* There are at least three tests we can try in succession. */
  
  /* Easy-answer attempt #1:  (### this stat's the files again) */
  
  /* Check if the local and prop-base file have *definitely* different
     filesizes. */
  SVN_ERR (svn_io_filesizes_different_p (&different_filesizes,
                                         prop_path,
                                         prop_base_path,
                                         subpool));
  if (different_filesizes) 
    {
      *modified_p = TRUE;
      goto cleanup;
    }
  
  /* Easy-answer attempt #2:  (### this stat's the files again) */
      
  /* See if the local file's prop timestamp is the same as the one
     recorded in the administrative directory.  */
  SVN_ERR (svn_wc__timestamps_equal_p (&equal_timestamps, path, adm_access,
                                       svn_wc__prop_time, subpool));
  if (equal_timestamps)
    {
      *modified_p = FALSE;
      goto cleanup;
    }
  
  /* Last ditch attempt:  */
  
  /* If we get here, then we know that the filesizes are the same,
     but the timestamps are different.  That's still not enough
     evidence to make a correct decision;  we need to look at the
     files' contents directly.

     However, doing a byte-for-byte comparison won't work.  The two
     properties files may have the *exact* same name/value pairs, but
     arranged in a different order.  (Our hashdump format makes no
     guarantees about ordering.)

     Therefore, rather than use contents_identical_p(), we use
     svn_prop_diffs(). */
  {
    apr_array_header_t *local_propchanges;
    apr_hash_t *localprops = apr_hash_make (subpool);
    apr_hash_t *baseprops = apr_hash_make (subpool);

    /* ### Amazingly, this stats the files again! */
    SVN_ERR (svn_wc__load_prop_file (prop_path, localprops, subpool));
    SVN_ERR (svn_wc__load_prop_file (prop_base_path,
                                     baseprops,
                                     subpool));
    SVN_ERR (svn_prop_diffs (&local_propchanges, localprops, 
                             baseprops, subpool));
                                         
    if (local_propchanges->nelts > 0)
      *modified_p = TRUE;
    else
      *modified_p = FALSE;

    /* If it turns out that there are no differences then we might be able
       to "repair" the prop-time in the entries file and avoid the
       expensive file contents comparison next time.

       ### Unlike the text-time in svn_wc_text_modified_p the only
       ### "legitimate" way to produce a prop-time variation with no
       ### corresponding property variation, is by using the Subversion
       ### property interface.  Perhaps those functions should detect the
       ### change that restores the pristine values and reset the
       ### prop-time?  This code would still be needed, to handle someone
       ### or something manually changing the timestamp on the
       ### prop-base. */
    if (! *modified_p && svn_wc_adm_locked (adm_access))
      {
        svn_wc_entry_t tmp;
        SVN_ERR (svn_io_file_affected_time (&tmp.prop_time, prop_path, pool));
        SVN_ERR (svn_wc__entry_modify (adm_access,
                                       (entry->kind == svn_node_dir
                                        ? SVN_WC_ENTRY_THIS_DIR
                                        : svn_path_basename (path, pool)),
                                       &tmp, SVN_WC__ENTRY_MODIFY_PROP_TIME,
                                       TRUE, pool));
      }
  }
 
 cleanup:
  svn_pool_destroy (subpool);
  
  return SVN_NO_ERROR;
}



svn_error_t *
svn_wc_get_prop_diffs (apr_array_header_t **propchanges,
                       apr_hash_t **original_props,
                       const char *path,
                       svn_wc_adm_access_t *adm_access,
                       apr_pool_t *pool)
{
  const char *prop_path, *prop_base_path;
  apr_array_header_t *local_propchanges;
  apr_hash_t *localprops = apr_hash_make (pool);
  apr_hash_t *baseprops = apr_hash_make (pool);


  SVN_ERR (svn_wc__prop_path (&prop_path, path, adm_access, FALSE, pool));
  SVN_ERR (svn_wc__prop_base_path (&prop_base_path, path, adm_access, FALSE,
                                   pool));

  SVN_ERR (svn_wc__load_prop_file (prop_path, localprops, pool));
  SVN_ERR (svn_wc__load_prop_file (prop_base_path, baseprops, pool));

  if (original_props != NULL)
    *original_props = baseprops;

  /* At this point, if either of the propfiles are non-existent, then
     the corresponding hash is simply empty. */
  
  if (propchanges != NULL)
    {
      SVN_ERR (svn_prop_diffs (&local_propchanges, localprops, 
                               baseprops, pool));      
      *propchanges = local_propchanges;
    }

  return SVN_NO_ERROR;
}



/** Externals **/

svn_error_t *
svn_wc_parse_externals_description2 (apr_array_header_t **externals_p,
                                     const char *parent_directory,
                                     const char *desc,
                                     apr_pool_t *pool)
{
  apr_array_header_t *lines = svn_cstring_split (desc, "\n\r", TRUE, pool);
  int i;
  
  if (externals_p)
    *externals_p = apr_array_make (pool, 1, sizeof (svn_wc_external_item_t *));

  for (i = 0; i < lines->nelts; i++)
    {
      const char *line = APR_ARRAY_IDX (lines, i, const char *);
      apr_array_header_t *line_parts;
      svn_wc_external_item_t *item;

      if ((! line) || (line[0] == '#'))
        continue;

      /* else proceed */

      line_parts = svn_cstring_split (line, " \t", TRUE, pool);

      item = apr_palloc (pool, sizeof (*item));

      if (line_parts->nelts < 2)
        goto parse_error;

      else if (line_parts->nelts == 2)
        {
          /* No "-r REV" given. */
          item->target_dir = APR_ARRAY_IDX (line_parts, 0, const char *);
          item->url = APR_ARRAY_IDX (line_parts, 1, const char *);
          item->revision.kind = svn_opt_revision_head;
        }
      else if ((line_parts->nelts == 3) || (line_parts->nelts == 4))
        {
          /* We're dealing with one of these two forms:
           * 
           *    TARGET_DIR  -rN  URL
           *    TARGET_DIR  -r N  URL
           * 
           * Handle either way.
           */

          const char *r_part_1 = NULL, *r_part_2 = NULL;

          item->target_dir = APR_ARRAY_IDX (line_parts, 0, const char *);
          item->revision.kind = svn_opt_revision_number;

          if (line_parts->nelts == 3)
            {
              r_part_1 = APR_ARRAY_IDX (line_parts, 1, const char *);
              item->url = APR_ARRAY_IDX (line_parts, 2, const char *);
            }
          else  /* nelts == 4 */
            {
              r_part_1 = APR_ARRAY_IDX (line_parts, 1, const char *);
              r_part_2 = APR_ARRAY_IDX (line_parts, 2, const char *);
              item->url = APR_ARRAY_IDX (line_parts, 3, const char *);
            }

          if ((! r_part_1) || (r_part_1[0] != '-') || (r_part_1[1] != 'r'))
            goto parse_error;

          if (! r_part_2)  /* "-rN" */
            {
              if (strlen (r_part_1) < 3)
                goto parse_error;
              else
                item->revision.value.number = SVN_STR_TO_REV (r_part_1 + 2);
            }
          else             /* "-r N" */
            {
              if (strlen (r_part_2) < 1)
                goto parse_error;
              else
                item->revision.value.number = SVN_STR_TO_REV (r_part_2);
            }
        }
      else    /* too many items on line */
        goto parse_error;

      if (0)
        {
        parse_error:
          return svn_error_createf
            (SVN_ERR_CLIENT_INVALID_EXTERNALS_DESCRIPTION, NULL,
             _("Error parsing %s property on '%s': '%s'"),
             SVN_PROP_EXTERNALS,
             svn_path_local_style (parent_directory, pool),
             line);
        }

      item->target_dir = svn_path_canonicalize
        (svn_path_internal_style (item->target_dir, pool), pool);
      {
        if (item->target_dir[0] == '\0' || item->target_dir[0] == '/'
            || svn_path_is_backpath_present (item->target_dir))
          return svn_error_createf
            (SVN_ERR_CLIENT_INVALID_EXTERNALS_DESCRIPTION, NULL,
             _("Invalid %s property on '%s': "
               "target involves '.' or '..' or is an absolute path"),
             SVN_PROP_EXTERNALS,
             svn_path_local_style (parent_directory, pool));
      }

      item->url = svn_path_canonicalize (item->url, pool);

      if (externals_p)
        APR_ARRAY_PUSH (*externals_p, svn_wc_external_item_t *) = item;
    }

  return SVN_NO_ERROR;
}


svn_error_t *
svn_wc_parse_externals_description (apr_hash_t **externals_p,
                                    const char *parent_directory,
                                    const char *desc,
                                    apr_pool_t *pool)
{
  apr_array_header_t *list;
  int i;

  SVN_ERR (svn_wc_parse_externals_description2 (externals_p ? &list : NULL,
                                                parent_directory, desc, pool));

  /* Store all of the items into the hash if that was requested. */
  if (externals_p)
    {
      *externals_p = apr_hash_make (pool);
      for (i = 0; i < list->nelts; i++)
        {
          svn_wc_external_item_t *item;
          item = APR_ARRAY_IDX (list, i, svn_wc_external_item_t *);

          apr_hash_set (*externals_p, item->target_dir,
                        APR_HASH_KEY_STRING, item);
        }
    }
  return SVN_NO_ERROR;
}


svn_boolean_t
svn_wc__has_magic_property (apr_array_header_t *properties)
{
  int i;

  for (i = 0; i < properties->nelts; i++)
    {
      svn_prop_t *property = &APR_ARRAY_IDX (properties, i, svn_prop_t);

      if (strcmp (property->name, SVN_PROP_EXECUTABLE) == 0
          || strcmp (property->name, SVN_PROP_KEYWORDS) == 0
          || strcmp (property->name, SVN_PROP_EOL_STYLE) == 0
          || strcmp (property->name, SVN_PROP_SPECIAL) == 0
          || strcmp (property->name, SVN_PROP_NEEDS_LOCK) == 0)
        return TRUE;
    }
  return FALSE;
}