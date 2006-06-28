{**********************************************************************************************************************}
{                                                                                                                      }
{ delphisvn: Subversion plugin for Borland Delphi                                                                      }
{                                                                                                                      }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); you may not use     }
{ this file except in compliance with the License. You may obtain a copy of the License at http://www.mozilla.org/MPL/ }
{                                                                                                                      }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either      }
{ express or implied. See the License for the specific language governing rights and limitations under the License.    }
{                                                                                                                      }
{ The Original Code is svn_client.pas.                                                                                 }
{                                                                                                                      }
{ The Initial Developer of the Original Code is Ondrej Kelle.                                                          }
{ Portions created by Ondrej Kelle are Copyright Ondrej Kelle. All rights reserved.                                    }
{                                                                                                                      }
{ Contributors:                                                                                                        }
{   Ondrej Kelle (tondrej)                                                                                             }
{                                                                                                                      }
{**********************************************************************************************************************}
{                                                                                                                      }
{ This unit contains import declarations for libsvn_client.dll, Subversion 1.3.2 DLL.                                  }
{                                                                                                                      }
{**********************************************************************************************************************}

unit svn_client;

interface

{$MINENUMSIZE 4}

uses
  Windows, SysUtils,
  apr;

//----- svn_ctype.h ----------------------------------------------------------------------------------------------------

const
  SVN_CTYPE_ASCII_MINUS            =  45; // ASCII value of '-'
  SVN_CTYPE_ASCII_DOT              =  46; // ASCII value of '.'
  SVN_CTYPE_ASCII_COLON            =  58; // ASCII value of ':'
  SVN_CTYPE_ASCII_UNDERSCORE       =  95; // ASCII value of '_'
  SVN_CTYPE_ASCII_TAB              =   9; // ASCII value of a tab
  SVN_CTYPE_ASCII_LINEFEED         =  10; // ASCII value of a line feed
  SVN_CTYPE_ASCII_CARRIAGERETURN   =  13;
  SVN_CTYPE_ASCII_DELETE           = 127;

function svn_ctype_iscntrl(C: Char): Boolean;
function svn_ctype_isspace(C: Char): Boolean;
function svn_ctype_isdigit(C: Char): Boolean;
function svn_ctype_isupper(C: Char): Boolean;
function svn_ctype_islower(C: Char): Boolean;
function svn_ctype_ispunct(C: Char): Boolean;
function svn_ctype_isascii(C: Char): Boolean;
function svn_ctype_isalpha(C: Char): Boolean;
function svn_ctype_isalnum(C: Char): Boolean;
function svn_ctype_isxdigit(C: Char): Boolean;
function svn_ctype_isgraph(C: Char): Boolean;
function svn_ctype_isprint(C: Char): Boolean;
function svn_ctype_isutf8lead(C: Char): Boolean;
function svn_ctype_isutf8cont(C: Char): Boolean;
function svn_ctype_isutf8mbc(C: Char): Boolean;
function svn_ctype_isutf8(C: Char): Boolean;

//----- svn_ctype.h ----------------------------------------------------------------------------------------------------

//----- svn_dav.h ------------------------------------------------------------------------------------------------------

const
  SVN_SVNDIFF_MIME_TYPE = 'application/vnd.svn-svndiff';
  SVN_DAV_DELTA_BASE_HEADER = 'X-SVN-VR-Base';
  SVN_DAV_OPTIONS_HEADER = 'X-SVN-Options';
  SVN_DAV_OPTION_NO_MERGE_RESPONSE = 'no-merge-response';
  SVN_DAV_OPTION_LOCK_BREAK = 'lock-break';
  SVN_DAV_OPTION_LOCK_STEAL = 'lock-steal';
  SVN_DAV_OPTION_RELEASE_LOCKS = 'release-locks';
  SVN_DAV_OPTION_KEEP_LOCKS = 'keep-locks';
  SVN_DAV_VERSION_NAME_HEADER = 'X-SVN-Version-Name';
  SVN_DAV_CREATIONDATE_HEADER = 'X-SVN-Creation-Date';
  SVN_DAV_LOCK_OWNER_HEADER = 'X-SVN-Lock-Owner';
  SVN_DAV_BASE_FULLTEXT_MD5_HEADER = 'X-SVN-Base-Fulltext-MD5';
  SVN_DAV_RESULT_FULLTEXT_MD5_HEADER = 'X-SVN-Result-Fulltext-MD5';
  SVN_DAV_ERROR_NAMESPACE = 'svn:';
  SVN_DAV_ERROR_TAG = 'error';
  SVN_DAV_PROP_NS_SVN = 'http://subversion.tigris.org/xmlns/svn/';
  SVN_DAV_PROP_NS_CUSTOM = 'http://subversion.tigris.org/xmlns/custom/';
  SVN_DAV_PROP_NS_DAV = 'http://subversion.tigris.org/xmlns/dav/';

//----- svn_dav.h ------------------------------------------------------------------------------------------------------

//----- svn_types.h ----------------------------------------------------------------------------------------------------

type
  PSvnError = ^TSvnError;
  TSvnError = record
    apr_err: TAprStatus;
    message: PChar;
    child: PSvnError;
    pool: PAprPool;
    afile: PChar;
    line: Longint;
  end;
  TSvnNodeKind = (svnNodeNone, svnNodeFile, svnNodeDir, svnNodeUnknown);
  PSvnRevNum = ^TSvnRevNum;
  TSvnRevNum = Longint;
  TSvnFileSize = Int64;
  TSvnBoolean = LongBool;
  TSvnRecurseKind = (svnRecursive, svnNonrecursive);

  PPSvnDirEnt = ^PSvnDirEnt;
  PSvnDirEnt = ^TSvnDirEnt;
  TSvnDirEnt = record
    kind: TSvnNodeKind;
    size: TSvnFileSize;
    has_props: TSvnBoolean;
    created_rev: TSvnRevNum;
    time: TAprTime;
    last_author: PChar;
  end;

  PSvnCommitInfo = ^TSvnCommitInfo;
  TSvnCommitInfo = record
    revision: TSvnRevNum;
    date: PChar;
    author: PChar;
    post_commit_err: PChar;
  end;

  PSvnLogChangedPath = ^TSvnLogChangedPath;
  TSvnLogChangedPath = record
    action: Char;
    copyfrom_path: PChar;
    copyfrom_rev: TSvnRevNum;
  end;

  TSvnLogMessageReceiver = function(baton: Pointer; changed_paths: PAprHash; revision: TSvnRevNum;
    author, date, message: PChar; pool: PAprPool): PSvnError; cdecl;
  TSvnCommitCallback = function(new_revision: TSvnRevNum; date, author: PChar; baton: Pointer): PSvnError; cdecl;
  TSvnCancelFunc = function(cancel_baton: Pointer): PSvnError; cdecl;

  PSvnLock = ^TSvnLock;
  TSvnLock = record
    path: PChar;
    token: PChar;
    owner: PChar;
    comment: PChar;
    is_dav_comment: TSvnBoolean;
    creation_date: TAprTime;
    expiration_date: TAprTime;
  end;

const
  SVN_INVALID_REVNUM: TSvnRevNum = -1;
  SVN_INVALID_FILESIZE: TSvnFileSize = -1;

  SVN_KEYWORD_MAX_LEN = 255;
  SVN_KEYWORD_REVISION_LONG = 'LastChangedRevision';
  SVN_KEYWORD_REVISION_SHORT = 'Rev';
  SVN_KEYWORD_REVISION_MEDIUM = 'Revision';
  SVN_KEYWORD_DATE_LONG = 'LastChangedDate';
  SVN_KEYWORD_DATE_SHORT = 'Date';
  SVN_KEYWORD_AUTHOR_LONG = 'LastChangedBy';
  SVN_KEYWORD_AUTHOR_SHORT = 'Author';
  SVN_KEYWORD_URL_LONG = 'HeadURL';
  SVN_KEYWORD_URL_SHORT = 'URL';
  SVN_KEYWORD_ID = 'Id';

  SVN_STREAM_CHUNK_SIZE = 102400;

var
  svn_create_commit_info: function(pool: PAprPool): PSvnCommitInfo; cdecl;
  svn_log_changed_path_dup: function(changed_path: PSvnLogChangedPath; pool: PAprPool): PSvnLogChangedPath; cdecl;
  svn_mime_type_validate: function(mime_type: PChar; pool: PAprPool): PSvnError; cdecl;
  svn_mime_type_is_binary: function(mime_type: PChar): TSvnBoolean; cdecl;
  svn_lock_create: function(pool: PAprPool): PSvnLock; cdecl;
  svn_lock_dup: function(lock: PSvnLock; pool: PAprPool): PSvnLock; cdecl;

//----- svn_types.h ----------------------------------------------------------------------------------------------------

//----- svn_nls.h ------------------------------------------------------------------------------------------------------

var
  svn_nls_init: function: PSvnError; cdecl;

//----- svn_nls.h ------------------------------------------------------------------------------------------------------

//----- svn_version.h --------------------------------------------------------------------------------------------------

type
  PSvnVersion = ^TSvnVersion;
  TSvnVersion = record
    major: Integer;
    minor: Integer;
    patch: Integer;
    tag: PChar;
  end;
  TVersionQueryFunc = function: PSvnVersion; cdecl;
  PSvnVersionChecklist = ^TSvnVersionChecklist;
  TSvnVersionChecklist = record
    alabel: PChar;
    version_query: TVersionQueryFunc;
  end;

const
  SVN_VER_MAJOR = 1;
  SVN_VER_MINOR = 3;
  SVN_VER_PATCH = 2;
  SVN_VER_TAG = ' (r19776)';
  SVN_VER_NUMTAG = '';
  SVN_VER_REVISION = 19776;

  SVN_VER_NUM = '1.3.2';
  SVN_VER_NUMBER = SVN_VER_NUM + SVN_VER_NUMTAG;
  SVN_VERSION = SVN_VER_NUM + SVN_VER_TAG;

var
  svn_ver_compatible: function(my_version, lib_version: PSvnVersion): TSvnBoolean; cdecl;
  svn_ver_equal: function(my_version, lib_version: PSvnVersion): TSvnBoolean; cdecl;
  svn_ver_check_list: function(my_version: PSvnVersion; checklist: PSvnVersionChecklist): PSvnError; cdecl;
  svn_subr_version: function: PSvnVersion; cdecl;

//----- svn_version.h --------------------------------------------------------------------------------------------------

//----- svn_time.h -----------------------------------------------------------------------------------------------------

var
  svn_time_to_cstring: function(when: TAprTime; pool: PAprPool): PChar; cdecl;
  svn_time_from_cstring: function(out when: TAprTime; data: PChar; pool: PAprPool): PSvnError; cdecl;
  svn_time_to_human_cstring: function(when: TAprTime; pool: PAprPool): PChar; cdecl;
  svn_parse_date: function(out matched: TSvnBoolean; out result: TAprTime; text: PChar; now: TAprTime;
    pool: PAprPool): PSvnError; cdecl;
  svn_sleep_for_timestamps: procedure; cdecl;

//----- svn_time.h -----------------------------------------------------------------------------------------------------

//----- svn_sorts.h ----------------------------------------------------------------------------------------------------

type
  PSvnSortItem = ^TSvnSortItem;
  TSvnSortItem = record
    key: Pointer;
    klen: TAprSize;
    value: Pointer;
  end;
  TSvnSortHashCompare = function(a, b: PSvnSortItem): Integer; cdecl;

var
  svn_sort_compare_items_as_paths: function(a, b: PSvnSortItem): Integer; cdecl;
  svn_sort_compare_items_lexically: function(a, b: PSvnSortItem): Integer; cdecl;
  svn_sort_compare_revisions: function(a, b: Pointer): Integer; cdecl;
  svn_sort_compare_paths: function(a, b: Pointer): Integer; cdecl;
  svn_sort__hash: function(ht: PAprHash; comparison_func: TSvnSortHashCompare; pool: PAprPool): PAprArrayHeader; cdecl;

//----- svn_sorts.h ----------------------------------------------------------------------------------------------------

//----- svn_pools.h ----------------------------------------------------------------------------------------------------

const
  SVN_ALLOCATOR_RECOMMENDED_MAX_FREE = 4096 * 1024;

var
  svn_pool_create_ex: function(parent_pool: PAprPool; allocator: PAprAllocator): PAprPool; cdecl;

//----- svn_pools.h ----------------------------------------------------------------------------------------------------

//----- svn_md5.h ------------------------------------------------------------------------------------------------------

var
  svn_md5_empty_string_digest: function: PByte; cdecl;
  svn_md5_digest_to_cstring_display: function(digest: PByte; pool: PAprPool): PChar; cdecl;
  svn_md5_digest_to_cstring: function(digest: PByte; pool: PAprPool): PChar; cdecl;
  svn_md5_digests_match: function(d1, d2: PByte): TSvnBoolean; cdecl;

//----- svn_md5.h ------------------------------------------------------------------------------------------------------

//----- svn_string.h ---------------------------------------------------------------------------------------------------

type
  PPSvnString = ^PSvnString;
  PSvnString = ^TSvnString;
  TSvnString = record
    data: PChar;
    len: TAprSize;
  end;
  PSvnStringBuf = ^TSvnStringBuf;
  TSvnStringBuf = record
    pool: PAprPool;
    data: PChar;
    len: TAprSize;
    blocksize: TAprSize;
  end;

var
  svn_string_create: function(cstring: PChar; pool: PAprPool): PSvnString; cdecl;
  svn_string_ncreate: function(bytes: PChar; size: TAprSize; pool: PAprPool): PSvnString; cdecl;
  svn_string_create_from_buf: function(strbuf: PSvnStringBuf; pool: PAprPool): PSvnString; cdecl;
  svn_string_createf: function(pool: PAprPool; fmt: PChar; const args: array of const): PSvnString; cdecl;
  svn_string_createv: function(pool: PAprPool; fmt: PChar; const ap: array of const): PSvnString; cdecl;
  svn_string_isempty: function(str: PSvnString): TSvnBoolean; cdecl;
  svn_string_dup: function(original_string: PSvnString; pool: PAprPool): PSvnString; cdecl;
  svn_string_compare: function(str1, str2: PSvnString): TSvnBoolean; cdecl;
  svn_string_first_non_whitespace: function(str: PSvnString): TAprSize; cdecl;
  svn_string_find_char_backward: function(str: PSvnString; ch: Char): TAprSize; cdecl;
  
  svn_stringbuf_create: function(cstring: PChar; pool: PAprPool): PSvnStringBuf; cdecl;
  svn_stringbuf_ncreate: function(bytes: PChar; size: TAprSize; pool: PAprPool): PSvnStringBuf; cdecl;
  svn_stringbuf_create_from_string: function(str: PSvnString; pool: PAprPool): PSvnStringBuf; cdecl;
  svn_stringbuf_createf: function(pool: PAprPool; fmt: PChar; const args: array of const): PSvnStringBuf; cdecl;
  svn_stringbuf_createv: function(pool: PAprPool; fmt: PChar; const ap: array of const): PSvnStringBuf; cdecl;
  svn_stringbuf_ensure: procedure(str: PSvnStringBuf; minimum_size: TAprSize); cdecl;
  svn_stringbuf_set: procedure(str: PSvnStringBuf; value: PChar); cdecl;
  svn_stringbuf_setempty: procedure(str: PSvnStringBuf); cdecl;
  svn_stringbuf_isempty: function(str: PSvnStringBuf): TSvnBoolean; cdecl;
  svn_stringbuf_chop: procedure(str: PSvnStringBuf; bytes: TAprSize); cdecl;
  svn_stringbuf_fillchar: procedure(str: PSvnStringBuf; c: Byte); cdecl;
  svn_stringbuf_appendbytes: procedure(targetstr: PSvnStringBuf; bytes: PChar; count: TAprSize); cdecl;
  svn_stringbuf_appendstr: procedure(targetstr, appendstr: PSvnStringBuf); cdecl;
  svn_stringbuf_appendcstr: procedure(targetstr: PSvnStringBuf; cstr: PChar); cdecl;
  svn_stringbuf_dup: function(original_string: PSvnStringBuf; pool: PAprPool): PSvnStringBuf; cdecl;
  svn_stringbuf_compare: function(str1, str2: PSvnStringBuf): TSvnBoolean; cdecl;
  svn_stringbuf_first_non_whitespace: function(str: PSvnStringBuf): TAprSize; cdecl;
  svn_stringbuf_strip_whitespace: procedure(str: PSvnStringBuf); cdecl;
  svn_stringbuf_find_char_backward: function(str: PSvnStringBuf; ch: Char): TAprSize; cdecl;
  svn_string_compare_stringbuf: function(str1, str2: PSvnStringBuf): TSvnBoolean; cdecl;

  svn_cstring_split: function(input, sep_chars: PChar; chop_whitespace: TSvnBoolean; pool: PAprPool): PAprArrayHeader;
    cdecl;
  svn_cstring_split_append: procedure(arr: PAprArrayHeader; input, sep_chars: PChar; chop_whitespace: TSvnBoolean;
    pool: PAprPool); cdecl;
  svn_cstring_match_glob_list: function(str: PChar; list: PAprArrayHeader): TSvnBoolean; cdecl;
  svn_cstring_count_newlines: function(msg: PChar): Integer; cdecl;
  svn_cstring_join: function(strings: PAprArrayHeader; separator: PChar; pool: PAprPool): PChar; cdecl;

//----- svn_string.h ---------------------------------------------------------------------------------------------------

//----- svn_xml.h ------------------------------------------------------------------------------------------------------

const
  SVN_XML_NAMESPACE = 'svn:';

type
  TSvnXMLOpenTagStyle = (svnXMLNormal = 1, svnXMLProtectPCData, svnXMLSelfClosing);
  PSvnXMLParser = ^TSvnXMLParser;
  TSvnXMLParser = THandle;
  TSvnXMLStartElem = procedure(baton: Pointer; name: PChar; atts: PPChar); cdecl;
  TSvnXMLEndElem = procedure(baton: Pointer; name: PChar); cdecl;
  TSvnXMLCharData = procedure(baton: Pointer; data: PChar; len: TAprSize); cdecl;

var
  svn_xml_is_xml_safe: function(data: PChar; len: TAprSize): TSvnBoolean; cdecl;
  svn_xml_escape_cdata_stringbuf: procedure(outstr: PSvnStringBuf; str: PSvnStringBuf; pool: PAprPool); cdecl;
  svn_xml_escape_cdata_string: procedure(out outstr: PSvnStringBuf; str: PSvnString; pool: PAprPool); cdecl;
  svn_xml_escape_cdata_cstring: procedure(out outstr: PSvnStringBuf; str: PChar; pool: PAprPool); cdecl;
  svn_xml_escape_attr_stringbuf: procedure(out outstr: PSvnStringBuf; str: PSvnStringBuf; pool: PAprPool); cdecl;
  svn_xml_escape_attr_string: procedure(out outstr: PSvnStringBuf; str: PSvnString; pool: PAprPool); cdecl;
  svn_xml_escape_attr_cstring: procedure(out outstr: PSvnStringBuf; str: PChar; pool: PAprPool); cdecl;
  svn_xml_fuzzy_escape: function(str: PChar; pool: PAprPool): PChar; cdecl;
  svn_xml_make_parser: function(baton: Pointer; start_handler: TSvnXMLStartElem; end_handler: TSvnXMLEndElem;
    data_handler: TSvnXMLCharData; pool: PAprPool): PSvnXMLParser; cdecl;
  svn_xml_free_parser: procedure(svn_parser: PSvnXMLParser); cdecl;
  svn_xml_parse: function(parser: PSvnXMLParser; buf: PChar; len: TAprSize; is_final: TSvnBoolean): PSvnError; cdecl;
  svn_xml_signal_bailout: procedure(error: PSvnError; svn_parser: PSvnXMLParser); cdecl;
  svn_xml_get_attr_value: function(name: PChar; atts: PPChar): PChar; cdecl;
  svn_xml_ap_to_hash: function(ap: array of const; pool: PAprPool): PAprHash; cdecl;
  svn_xml_make_att_hash: function(atts: PPChar; pool: PAprPool): PAprHash; cdecl;
  svn_xml_hash_atts_preserving: procedure(atts: PPChar; ht: PAprHash; pool: PAprPool); cdecl;
  svn_xml_hash_atts_overlaying: procedure(atts: PPChar; ht: PAprHash; pool: PAprPool); cdecl;
  svn_xml_make_header: procedure(out str: PSvnStringBuf; pool: PAprPool); cdecl;
  svn_xml_make_open_tag: procedure(out str: PSvnStringBuf; pool: PAprPool; style: TSvnXMLOpenTagStyle;
    tagname: PChar; const args: array of const); cdecl;
  svn_xml_make_open_tag_v: procedure(out str: PSvnStringBuf; pool: PAprPool; style: TSvnXMLOpenTagStyle; tagname: PChar;
    const ap: array of const); cdecl;
  svn_xml_make_open_tag_hash: procedure(out str: PSvnStringBuf; pool: PAprPool; style: TSvnXMLOpenTagStyle;
    tagname: PChar; attributes: PAprHash); cdecl;
  svn_xml_make_close_tag: procedure(out str: PSvnStringBuf; pool: PAprPool; tagname: PChar); cdecl;

//----- svn_xml.h ------------------------------------------------------------------------------------------------------

//----- svn_cmdline.h --------------------------------------------------------------------------------------------------

var
  svn_cmdline_init: function(progname: PChar; error_stream: THandle): Integer; cdecl;
  svn_cmdline_cstring_from_utf8: function(out dest: PChar; src: PChar; pool: PAprPool): PSvnError; cdecl;
  svn_cmdline_cstring_from_utf8_fuzzy: function(src: PChar; pool: PAprPool): PChar; cdecl;
  svn_cmdline_cstring_to_utf8: function(out dest: PChar; src: PChar; pool: PAprPool): PSvnError; cdecl;
  svn_cmdline_path_local_style_from_utf8: function(out dest: PChar; src: PChar; pool: PAprPool): PSvnError; cdecl;
  svn_cmdline_printf: function(pool: PAprPool; fmt: PChar; const args: array of const): PSvnError; cdecl;
  svn_cmdline_fprintf: function(stream: THandle; pool: PAprPool; fmt: PChar; const args: array of const): PSvnError;
    cdecl;
  svn_cmdline_fputs: function(str: PChar; stream: THandle; pool: PAprPool): PSvnError; cdecl;
  svn_cmdline_fflush: function(stream: THandle): PSvnError; cdecl;
  svn_cmdline_output_encoding: function(pool: PAprPool): PChar; cdecl;
  svn_cmdline_handle_exit_error: function(error: PSvnError; pool: PAprPool; prefix: PChar): Integer; cdecl;

//----- svn_cmdline.h --------------------------------------------------------------------------------------------------

//----- svn_utf.h ------------------------------------------------------------------------------------------------------

var
  svn_utf_initialize: procedure(pool: PAprPool); cdecl;
  svn_utf_stringbuf_to_utf8: function(out dest: PSvnStringBuf; src: PSvnStringBuf; pool: PAprPool): PSvnError; cdecl;
  svn_utf_string_to_utf8: function(out dest: PSvnString; src: PSvnString; pool: PAprPool): PSvnError; cdecl;
  svn_utf_cstring_to_utf8: function(dest: PPChar; src: PChar; pool: PAprPool): PSvnError; cdecl;
  svn_utf_cstring_to_utf8_ex: function(dest: PPChar; src, frompage, convset_key: PChar; pool: PAprPool): PSvnError;
    cdecl;
  svn_utf_stringbuf_from_utf8: function(out dest: PSvnStringBuf; src: PSvnStringBuf; pool: PAprPool): PSvnError;
    cdecl;
  svn_utf_string_from_utf8: function(out dest: PSvnString; src: PSvnString; pool: PAprPool): PSvnError; cdecl;
  svn_utf_cstring_from_utf8: function(out dest: PChar; src: PChar; pool: PAprPool): PSvnError; cdecl;
  svn_utf_cstring_from_utf8_ex: function(out dest: PChar; src, topage, convset_key: PChar; pool: PAprPool): PSvnError;
    cdecl;
  svn_utf_cstring_from_utf8_fuzzy: function(src: PChar; pool: PAprPool): PChar; cdecl;
  svn_utf_cstring_from_utf8_stringbuf: function(out dest: PChar; src: PSvnStringBuf; pool: PAprPool): PSvnError;
    cdecl;
  svn_utf_cstring_from_utf8_string: function(out dest: PChar; src: PChar; pool: PAprPool): PSvnError; cdecl;

//----- svn_utf.h ------------------------------------------------------------------------------------------------------

//----- svn_props.h ----------------------------------------------------------------------------------------------------

type
  PSvnProp = ^TSvnProp;
  TSvnProp = record
    name: PChar;
    value: PSvnString;
  end;
  TSvnPropKind = (svnPropEntryKind, svnPropWCKind, svnPropRegularKind);

var
  svn_prop_dup: function(prop: PSvnProp; pool: PAprPool): PSvnProp; cdecl;
  svn_prop_array_dup: function(arr: PAprArrayHeader; pool: PAprPool): PAprArrayHeader; cdecl;
  svn_property_kind: function(prefix_len: PInteger; prop_name: PChar): TSvnPropKind; cdecl;
  svn_prop_is_svn_prop: function(prop_name: PChar): TSvnBoolean; cdecl;
  svn_prop_needs_translation: function(propname: PChar): TSvnBoolean; cdecl;
  svn_categorize_props: function(proplist: PAprArrayHeader; out entry_props, wc_props, regular_props: PAprArrayHeader;
    pool: PAprPool): PSvnError; cdecl;
  svn_prop_diffs: function(out propdiffs: PAprArrayHeader; target_props, source_props: PAprHash;
    pool: PAprPool): PSvnError; cdecl;

const
  SVN_PROP_PREFIX = 'svn:';
  SVN_PROP_MIME_TYPE = SVN_PROP_PREFIX + 'mime-type';
  SVN_PROP_IGNORE = SVN_PROP_PREFIX = 'ignore';
  SVN_PROP_EOL_STYLE = SVN_PROP_PREFIX + 'eol-style';
  SVN_PROP_KEYWORDS = SVN_PROP_PREFIX + 'keywords';
  SVN_PROP_EXECUTABLE = SVN_PROP_PREFIX + 'executable';
  SVN_PROP_EXECUTABLE_VALUE = '*';
  SVN_PROP_NEEDS_LOCK = SVN_PROP_PREFIX = 'needs-lock';
  SVN_PROP_NEEDS_LOCK_VALUE = '*';
  SVN_PROP_SPECIAL = SVN_PROP_PREFIX + 'special';
  SVN_PROP_SPECIAL_VALUE = '*';
  SVN_PROP_EXTERNALS = SVN_PROP_PREFIX + 'externals';

  SVN_PROP_WC_PREFIX = SVN_PROP_PREFIX + 'wc:';
  SVN_PROP_ENTRY_PREFIX = SVN_PROP_PREFIX + 'entry:';
  SVN_PROP_ENTRY_COMMITTED_REV = SVN_PROP_ENTRY_PREFIX + 'committed-rev';
  SVN_PROP_ENTRY_COMMITTED_DATE = SVN_PROP_ENTRY_PREFIX + 'committed-date';
  SVN_PROP_ENTRY_LAST_AUTHOR = SVN_PROP_ENTRY_PREFIX + 'last-author';
  SVN_PROP_ENTRY_UUID = SVN_PROP_ENTRY_PREFIX + 'uuid';
  SVN_PROP_ENTRY_LOCK_TOKEN = SVN_PROP_ENTRY_PREFIX + 'lock-token';
  SVN_PROP_CUSTOM_PREFIX = SVN_PROP_PREFIX + 'custom:';

  SVN_PROP_REVISION_AUTHOR = SVN_PROP_PREFIX + 'author';
  SVN_PROP_REVISION_LOG = SVN_PROP_PREFIX + 'log';
  SVN_PROP_REVISION_DATE = SVN_PROP_PREFIX + 'date';
  SVN_PROP_REVISION_ORIG_DATE = SVN_PROP_PREFIX + 'original-date';
  SVN_PROP_REVISION_AUTOVERSIONED = SVN_PROP_PREFIX + 'autoversioned';
  SVN_PROP_REVISION_ALL_PROPS: array[0..4] of string = (SVN_PROP_REVISION_AUTHOR, SVN_PROP_REVISION_LOG,
    SVN_PROP_REVISION_DATE, SVN_PROP_REVISION_AUTOVERSIONED, SVN_PROP_REVISION_ORIG_DATE);

//----- svn_props.h ----------------------------------------------------------------------------------------------------

//----- svn_error_codes.h ----------------------------------------------------------------------------------------------

const
  SVN_ERR_CATEGORY_SIZE                           = 5000;
  SVN_ERR_BAD_CATEGORY_START                      = APR_OS_START_USERERR + 1 * SVN_ERR_CATEGORY_SIZE;
  SVN_ERR_XML_CATEGORY_START                      = APR_OS_START_USERERR + 2 * SVN_ERR_CATEGORY_SIZE;
  SVN_ERR_IO_CATEGORY_START                       = APR_OS_START_USERERR + 3 * SVN_ERR_CATEGORY_SIZE;
  SVN_ERR_STREAM_CATEGORY_START                   = APR_OS_START_USERERR + 4 * SVN_ERR_CATEGORY_SIZE;
  SVN_ERR_NODE_CATEGORY_START                     = APR_OS_START_USERERR + 5 * SVN_ERR_CATEGORY_SIZE;
  SVN_ERR_ENTRY_CATEGORY_START                    = APR_OS_START_USERERR + 6 * SVN_ERR_CATEGORY_SIZE;
  SVN_ERR_WC_CATEGORY_START                       = APR_OS_START_USERERR + 7 * SVN_ERR_CATEGORY_SIZE;
  SVN_ERR_FS_CATEGORY_START                       = APR_OS_START_USERERR + 8 * SVN_ERR_CATEGORY_SIZE;
  SVN_ERR_REPOS_CATEGORY_START                    = APR_OS_START_USERERR + 9 * SVN_ERR_CATEGORY_SIZE;
  SVN_ERR_RA_CATEGORY_START                       = APR_OS_START_USERERR + 10 * SVN_ERR_CATEGORY_SIZE;
  SVN_ERR_RA_DAV_CATEGORY_START                   = APR_OS_START_USERERR + 11 * SVN_ERR_CATEGORY_SIZE;
  SVN_ERR_RA_LOCAL_CATEGORY_START                 = APR_OS_START_USERERR + 12 * SVN_ERR_CATEGORY_SIZE;
  SVN_ERR_SVNDIFF_CATEGORY_START                  = APR_OS_START_USERERR + 13 * SVN_ERR_CATEGORY_SIZE;
  SVN_ERR_APMOD_CATEGORY_START                    = APR_OS_START_USERERR + 14 * SVN_ERR_CATEGORY_SIZE;
  SVN_ERR_CLIENT_CATEGORY_START                   = APR_OS_START_USERERR + 15 * SVN_ERR_CATEGORY_SIZE;
  SVN_ERR_MISC_CATEGORY_START                     = APR_OS_START_USERERR + 16 * SVN_ERR_CATEGORY_SIZE;
  SVN_ERR_CL_CATEGORY_START                       = APR_OS_START_USERERR + 17 * SVN_ERR_CATEGORY_SIZE;
  SVN_ERR_RA_SVN_CATEGORY_START                   = APR_OS_START_USERERR + 18 * SVN_ERR_CATEGORY_SIZE;
  SVN_ERR_AUTHN_CATEGORY_START                    = APR_OS_START_USERERR + 19 * SVN_ERR_CATEGORY_SIZE;
  SVN_ERR_AUTHZ_CATEGORY_START                    = APR_OS_START_USERERR + 20 * SVN_ERR_CATEGORY_SIZE;

  SVN_ERR_BAD_CONTAINING_POOL                     = SVN_ERR_BAD_CATEGORY_START + 0;
  SVN_ERR_BAD_FILENAME                            = SVN_ERR_BAD_CATEGORY_START + 1;
  SVN_ERR_BAD_URL                                 = SVN_ERR_BAD_CATEGORY_START + 2;
  SVN_ERR_BAD_DATE                                = SVN_ERR_BAD_CATEGORY_START + 3;
  SVN_ERR_BAD_MIME_TYPE                           = SVN_ERR_BAD_CATEGORY_START + 4;
  SVN_ERR_BAD_VERSION_FILE_FORMAT                 = SVN_ERR_BAD_CATEGORY_START + 6;

  SVN_ERR_XML_ATTRIB_NOT_FOUND                    = SVN_ERR_XML_CATEGORY_START + 0;
  SVN_ERR_XML_MISSING_ANCESTRY                    = SVN_ERR_XML_CATEGORY_START + 1;
  SVN_ERR_XML_UNKNOWN_ENCODING                    = SVN_ERR_XML_CATEGORY_START + 2;
  SVN_ERR_XML_MALFORMED                           = SVN_ERR_XML_CATEGORY_START + 3;
  SVN_ERR_XML_UNESCAPABLE_DATA                    = SVN_ERR_XML_CATEGORY_START + 4;

  SVN_ERR_IO_INCONSISTENT_EOL                     = SVN_ERR_IO_CATEGORY_START + 0;
  SVN_ERR_IO_UNKNOWN_EOL                          = SVN_ERR_IO_CATEGORY_START + 1;
  SVN_ERR_IO_CORRUPT_EOL                          = SVN_ERR_IO_CATEGORY_START + 2;
  SVN_ERR_IO_UNIQUE_NAMES_EXHAUSTED               = SVN_ERR_IO_CATEGORY_START + 3;
  SVN_ERR_IO_PIPE_FRAME_ERROR                     = SVN_ERR_IO_CATEGORY_START + 4;
  SVN_ERR_IO_PIPE_READ_ERROR                      = SVN_ERR_IO_CATEGORY_START + 5;
  SVN_ERR_IO_WRITE_ERROR                          = SVN_ERR_IO_CATEGORY_START + 6;

  SVN_ERR_STREAM_UNEXPECTED_EOF                   = SVN_ERR_STREAM_CATEGORY_START + 0;
  SVN_ERR_STREAM_MALFORMED_DATA                   = SVN_ERR_STREAM_CATEGORY_START + 1;
  SVN_ERR_STREAM_UNRECOGNIZED_DATA                = SVN_ERR_STREAM_CATEGORY_START + 2;

  SVN_ERR_NODE_UNKNOWN_KIND                       = SVN_ERR_NODE_CATEGORY_START + 0;
  SVN_ERR_NODE_UNEXPECTED_KIND                    = SVN_ERR_NODE_CATEGORY_START + 1;

  SVN_ERR_ENTRY_NOT_FOUND                         = SVN_ERR_ENTRY_CATEGORY_START + 0;
  SVN_ERR_ENTRY_EXISTS                            = SVN_ERR_ENTRY_CATEGORY_START + 2;
  SVN_ERR_ENTRY_MISSING_REVISION                  = SVN_ERR_ENTRY_CATEGORY_START + 3;
  SVN_ERR_ENTRY_MISSING_URL                       = SVN_ERR_ENTRY_CATEGORY_START + 4;
  SVN_ERR_ENTRY_ATTRIBUTE_INVALID                 = SVN_ERR_ENTRY_CATEGORY_START + 5;

  SVN_ERR_WC_OBSTRUCTED_UPDATE                    = SVN_ERR_WC_CATEGORY_START + 0;
  SVN_ERR_WC_UNWIND_MISMATCH                      = SVN_ERR_WC_CATEGORY_START + 1;
  SVN_ERR_WC_UNWIND_EMPTY                         = SVN_ERR_WC_CATEGORY_START + 2;
  SVN_ERR_WC_UNWIND_NOT_EMPTY                     = SVN_ERR_WC_CATEGORY_START + 3;
  SVN_ERR_WC_LOCKED                               = SVN_ERR_WC_CATEGORY_START + 4;
  SVN_ERR_WC_NOT_LOCKED                           = SVN_ERR_WC_CATEGORY_START + 5;
  SVN_ERR_WC_INVALID_LOCK                         = SVN_ERR_WC_CATEGORY_START + 6;
  SVN_ERR_WC_NOT_DIRECTORY                        = SVN_ERR_WC_CATEGORY_START + 7;
  SVN_ERR_WC_NOT_FILE                             = SVN_ERR_WC_CATEGORY_START + 8;
  SVN_ERR_WC_BAD_ADM_LOG                          = SVN_ERR_WC_CATEGORY_START + 9;
  SVN_ERR_WC_PATH_NOT_FOUND                       = SVN_ERR_WC_CATEGORY_START + 10;
  SVN_ERR_WC_NOT_UP_TO_DATE                       = SVN_ERR_WC_CATEGORY_START + 11;
  SVN_ERR_WC_LEFT_LOCAL_MOD                       = SVN_ERR_WC_CATEGORY_START + 12;
  SVN_ERR_WC_SCHEDULE_CONFLICT                    = SVN_ERR_WC_CATEGORY_START + 13;
  SVN_ERR_WC_PATH_FOUND                           = SVN_ERR_WC_CATEGORY_START + 14;
  SVN_ERR_WC_FOUND_CONFLICT                       = SVN_ERR_WC_CATEGORY_START + 15;
  SVN_ERR_WC_CORRUPT                              = SVN_ERR_WC_CATEGORY_START + 16;
  SVN_ERR_WC_CORRUPT_TEXT_BASE                    = SVN_ERR_WC_CATEGORY_START + 17;
  SVN_ERR_WC_NODE_KIND_CHANGE                     = SVN_ERR_WC_CATEGORY_START + 18;
  SVN_ERR_WC_INVALID_OP_ON_CWD                    = SVN_ERR_WC_CATEGORY_START + 19;
  SVN_ERR_WC_BAD_ADM_LOG_START                    = SVN_ERR_WC_CATEGORY_START + 20;
  SVN_ERR_WC_UNSUPPORTED_FORMAT                   = SVN_ERR_WC_CATEGORY_START + 21;
  SVN_ERR_WC_BAD_PATH                             = SVN_ERR_WC_CATEGORY_START + 22;
  SVN_ERR_WC_INVALID_SCHEDULE                     = SVN_ERR_WC_CATEGORY_START + 23;
  SVN_ERR_WC_INVALID_RELOCATION                   = SVN_ERR_WC_CATEGORY_START + 24;
  SVN_ERR_WC_INVALID_SWITCH                       = SVN_ERR_WC_CATEGORY_START + 25;

  SVN_ERR_FS_GENERAL                              = SVN_ERR_FS_CATEGORY_START + 0;
  SVN_ERR_FS_CLEANUP                              = SVN_ERR_FS_CATEGORY_START + 1;
  SVN_ERR_FS_ALREADY_OPEN                         = SVN_ERR_FS_CATEGORY_START + 2;
  SVN_ERR_FS_NOT_OPEN                             = SVN_ERR_FS_CATEGORY_START + 3;
  SVN_ERR_FS_CORRUPT                              = SVN_ERR_FS_CATEGORY_START + 4;
  SVN_ERR_FS_PATH_SYNTAX                          = SVN_ERR_FS_CATEGORY_START + 5;
  SVN_ERR_FS_NO_SUCH_REVISION                     = SVN_ERR_FS_CATEGORY_START + 6;
  SVN_ERR_FS_NO_SUCH_TRANSACTION                  = SVN_ERR_FS_CATEGORY_START + 7;
  SVN_ERR_FS_NO_SUCH_ENTRY                        = SVN_ERR_FS_CATEGORY_START + 8;
  SVN_ERR_FS_NO_SUCH_REPRESENTATION               = SVN_ERR_FS_CATEGORY_START + 9;
  SVN_ERR_FS_NO_SUCH_STRING                       = SVN_ERR_FS_CATEGORY_START + 10;
  SVN_ERR_FS_NO_SUCH_COPY                         = SVN_ERR_FS_CATEGORY_START + 11;
  SVN_ERR_FS_TRANSACTION_NOT_MUTABLE              = SVN_ERR_FS_CATEGORY_START + 12;
  SVN_ERR_FS_NOT_FOUND                            = SVN_ERR_FS_CATEGORY_START + 13;
  SVN_ERR_FS_ID_NOT_FOUND                         = SVN_ERR_FS_CATEGORY_START + 14;
  SVN_ERR_FS_NOT_ID                               = SVN_ERR_FS_CATEGORY_START + 15;
  SVN_ERR_FS_NOT_DIRECTORY                        = SVN_ERR_FS_CATEGORY_START + 16;
  SVN_ERR_FS_NOT_FILE                             = SVN_ERR_FS_CATEGORY_START + 17;
  SVN_ERR_FS_NOT_SINGLE_PATH_COMPONENT            = SVN_ERR_FS_CATEGORY_START + 18;
  SVN_ERR_FS_NOT_MUTABLE                          = SVN_ERR_FS_CATEGORY_START + 19;
  SVN_ERR_FS_ALREADY_EXISTS                       = SVN_ERR_FS_CATEGORY_START + 20;
  SVN_ERR_FS_ROOT_DIR                             = SVN_ERR_FS_CATEGORY_START + 21;
  SVN_ERR_FS_NOT_TXN_ROOT                         = SVN_ERR_FS_CATEGORY_START + 22;
  SVN_ERR_FS_NOT_REVISION_ROOT                    = SVN_ERR_FS_CATEGORY_START + 23;
  SVN_ERR_FS_CONFLICT                             = SVN_ERR_FS_CATEGORY_START + 24;
  SVN_ERR_FS_REP_CHANGED                          = SVN_ERR_FS_CATEGORY_START + 25;
  SVN_ERR_FS_REP_NOT_MUTABLE                      = SVN_ERR_FS_CATEGORY_START + 26;
  SVN_ERR_FS_MALFORMED_SKEL                       = SVN_ERR_FS_CATEGORY_START + 27;
  SVN_ERR_FS_TXN_OUT_OF_DATE                      = SVN_ERR_FS_CATEGORY_START + 28;
  SVN_ERR_FS_BERKELEY_DB                          = SVN_ERR_FS_CATEGORY_START + 29;
  SVN_ERR_FS_BERKELEY_DB_DEADLOCK                 = SVN_ERR_FS_CATEGORY_START + 30;
  SVN_ERR_FS_TRANSACTION_DEAD                     = SVN_ERR_FS_CATEGORY_START + 31;
  SVN_ERR_FS_TRANSACTION_NOT_DEAD                 = SVN_ERR_FS_CATEGORY_START + 32;
  SVN_ERR_FS_UNKNOWN_FS_TYPE                      = SVN_ERR_FS_CATEGORY_START + 33;
  SVN_ERR_FS_NO_USER                              = SVN_ERR_FS_CATEGORY_START + 34;
  SVN_ERR_FS_PATH_ALREADY_LOCKED                  = SVN_ERR_FS_CATEGORY_START + 35;
  SVN_ERR_FS_PATH_NOT_LOCKED                      = SVN_ERR_FS_CATEGORY_START + 36;
  SVN_ERR_FS_BAD_LOCK_TOKEN                       = SVN_ERR_FS_CATEGORY_START + 37;
  SVN_ERR_FS_NO_LOCK_TOKEN                        = SVN_ERR_FS_CATEGORY_START + 38;
  SVN_ERR_FS_LOCK_OWNER_MISMATCH                  = SVN_ERR_FS_CATEGORY_START + 39;
  SVN_ERR_FS_NO_SUCH_LOCK                         = SVN_ERR_FS_CATEGORY_START + 40;
  SVN_ERR_FS_LOCK_EXPIRED                         = SVN_ERR_FS_CATEGORY_START + 41;
  SVN_ERR_FS_OUT_OF_DATE                          = SVN_ERR_FS_CATEGORY_START + 42;
  SVN_ERR_FS_UNSUPPORTED_FORMAT                   = SVN_ERR_FS_CATEGORY_START + 43;

  SVN_ERR_REPOS_LOCKED                            = SVN_ERR_REPOS_CATEGORY_START + 0;
  SVN_ERR_REPOS_HOOK_FAILURE                      = SVN_ERR_REPOS_CATEGORY_START + 1;
  SVN_ERR_REPOS_BAD_ARGS                          = SVN_ERR_REPOS_CATEGORY_START + 2;
  SVN_ERR_REPOS_NO_DATA_FOR_REPORT                = SVN_ERR_REPOS_CATEGORY_START + 3;
  SVN_ERR_REPOS_BAD_REVISION_REPORT               = SVN_ERR_REPOS_CATEGORY_START + 4;
  SVN_ERR_REPOS_UNSUPPORTED_VERSION               = SVN_ERR_REPOS_CATEGORY_START + 5;
  SVN_ERR_REPOS_DISABLED_FEATURE                  = SVN_ERR_REPOS_CATEGORY_START + 6;
  SVN_ERR_REPOS_POST_COMMIT_HOOK_FAILED           = SVN_ERR_REPOS_CATEGORY_START + 7;
  SVN_ERR_REPOS_POST_LOCK_HOOK_FAILED             = SVN_ERR_REPOS_CATEGORY_START + 8;
  SVN_ERR_REPOS_POST_UNLOCK_HOOK_FAILED           = SVN_ERR_REPOS_CATEGORY_START + 9;

  SVN_ERR_RA_ILLEGAL_URL                          = SVN_ERR_RA_CATEGORY_START + 0;
  SVN_ERR_RA_NOT_AUTHORIZED                       = SVN_ERR_RA_CATEGORY_START + 1;
  SVN_ERR_RA_UNKNOWN_AUTH                         = SVN_ERR_RA_CATEGORY_START + 2;
  SVN_ERR_RA_NOT_IMPLEMENTED                      = SVN_ERR_RA_CATEGORY_START + 3;
  SVN_ERR_RA_OUT_OF_DATE                          = SVN_ERR_RA_CATEGORY_START + 4;
  SVN_ERR_RA_NO_REPOS_UUID                        = SVN_ERR_RA_CATEGORY_START + 5;
  SVN_ERR_RA_UNSUPPORTED_ABI_VERSION              = SVN_ERR_RA_CATEGORY_START + 6;
  SVN_ERR_RA_NOT_LOCKED                           = SVN_ERR_RA_CATEGORY_START + 7;

  SVN_ERR_RA_DAV_SOCK_INIT                        = SVN_ERR_RA_DAV_CATEGORY_START + 0;
  SVN_ERR_RA_DAV_CREATING_REQUEST                 = SVN_ERR_RA_DAV_CATEGORY_START + 1;
  SVN_ERR_RA_DAV_REQUEST_FAILED                   = SVN_ERR_RA_DAV_CATEGORY_START + 2;
  SVN_ERR_RA_DAV_OPTIONS_REQ_FAILED               = SVN_ERR_RA_DAV_CATEGORY_START + 3;
  SVN_ERR_RA_DAV_PROPS_NOT_FOUND                  = SVN_ERR_RA_DAV_CATEGORY_START + 4;
  SVN_ERR_RA_DAV_ALREADY_EXISTS                   = SVN_ERR_RA_DAV_CATEGORY_START + 5;
  SVN_ERR_RA_DAV_INVALID_CONFIG_VALUE             = SVN_ERR_RA_DAV_CATEGORY_START + 6;
  SVN_ERR_RA_DAV_PATH_NOT_FOUND                   = SVN_ERR_RA_DAV_CATEGORY_START + 7;
  SVN_ERR_RA_DAV_PROPPATCH_FAILED                 = SVN_ERR_RA_DAV_CATEGORY_START + 8;
  SVN_ERR_RA_DAV_MALFORMED_DATA                   = SVN_ERR_RA_DAV_CATEGORY_START + 9;
  SVN_ERR_RA_DAV_RESPONSE_HEADER_BADNESS          = SVN_ERR_RA_DAV_CATEGORY_START + 10;

  SVN_ERR_RA_LOCAL_REPOS_NOT_FOUND                = SVN_ERR_RA_LOCAL_CATEGORY_START + 0;
  SVN_ERR_RA_LOCAL_REPOS_OPEN_FAILED              = SVN_ERR_RA_LOCAL_CATEGORY_START + 1;

  SVN_ERR_RA_SVN_CMD_ERR                          = SVN_ERR_RA_SVN_CATEGORY_START + 0;
  SVN_ERR_RA_SVN_UNKNOWN_CMD                      = SVN_ERR_RA_SVN_CATEGORY_START + 1;
  SVN_ERR_RA_SVN_CONNECTION_CLOSED                = SVN_ERR_RA_SVN_CATEGORY_START + 2;
  SVN_ERR_RA_SVN_IO_ERROR                         = SVN_ERR_RA_SVN_CATEGORY_START + 3;
  SVN_ERR_RA_SVN_MALFORMED_DATA                   = SVN_ERR_RA_SVN_CATEGORY_START + 4;
  SVN_ERR_RA_SVN_REPOS_NOT_FOUND                  = SVN_ERR_RA_SVN_CATEGORY_START + 5;
  SVN_ERR_RA_SVN_BAD_VERSION                      = SVN_ERR_RA_SVN_CATEGORY_START + 6;

  SVN_ERR_AUTHN_CREDS_UNAVAILABLE                 = SVN_ERR_AUTHN_CATEGORY_START + 0;
  SVN_ERR_AUTHN_NO_PROVIDER                       = SVN_ERR_AUTHN_CATEGORY_START + 1;
  SVN_ERR_AUTHN_PROVIDERS_EXHAUSTED               = SVN_ERR_AUTHN_CATEGORY_START + 2;
  SVN_ERR_AUTHN_CREDS_NOT_SAVED                   = SVN_ERR_AUTHN_CATEGORY_START + 3;

  SVN_ERR_AUTHZ_ROOT_UNREADABLE                   = SVN_ERR_AUTHZ_CATEGORY_START + 0;
  SVN_ERR_AUTHZ_UNREADABLE                        = SVN_ERR_AUTHZ_CATEGORY_START + 1;
  SVN_ERR_AUTHZ_PARTIALLY_READABLE                = SVN_ERR_AUTHZ_CATEGORY_START + 2;
  SVN_ERR_AUTHZ_INVALID_CONFIG                    = SVN_ERR_AUTHZ_CATEGORY_START + 3;
  SVN_ERR_AUTHZ_UNWRITABLE                        = SVN_ERR_AUTHZ_CATEGORY_START + 4;

  SVN_ERR_SVNDIFF_INVALID_HEADER                  = SVN_ERR_SVNDIFF_CATEGORY_START + 0;
  SVN_ERR_SVNDIFF_CORRUPT_WINDOW                  = SVN_ERR_SVNDIFF_CATEGORY_START + 1;
  SVN_ERR_SVNDIFF_BACKWARD_VIEW                   = SVN_ERR_SVNDIFF_CATEGORY_START + 2;
  SVN_ERR_SVNDIFF_INVALID_OPS                     = SVN_ERR_SVNDIFF_CATEGORY_START + 3;
  SVN_ERR_SVNDIFF_UNEXPECTED_END                  = SVN_ERR_SVNDIFF_CATEGORY_START + 4;

  SVN_ERR_APMOD_MISSING_PATH_TO_FS                = SVN_ERR_APMOD_CATEGORY_START + 0;
  SVN_ERR_APMOD_MALFORMED_URI                     = SVN_ERR_APMOD_CATEGORY_START + 1;
  SVN_ERR_APMOD_ACTIVITY_NOT_FOUND                = SVN_ERR_APMOD_CATEGORY_START + 2;
  SVN_ERR_APMOD_BAD_BASELINE                      = SVN_ERR_APMOD_CATEGORY_START + 3;
  SVN_ERR_APMOD_CONNECTION_ABORTED                = SVN_ERR_APMOD_CATEGORY_START + 4;

  SVN_ERR_CLIENT_VERSIONED_PATH_REQUIRED          = SVN_ERR_CLIENT_CATEGORY_START + 0;
  SVN_ERR_CLIENT_RA_ACCESS_REQUIRED               = SVN_ERR_CLIENT_CATEGORY_START + 1;
  SVN_ERR_CLIENT_BAD_REVISION                     = SVN_ERR_CLIENT_CATEGORY_START + 2;
  SVN_ERR_CLIENT_DUPLICATE_COMMIT_URL             = SVN_ERR_CLIENT_CATEGORY_START + 3;
  SVN_ERR_CLIENT_IS_BINARY_FILE                   = SVN_ERR_CLIENT_CATEGORY_START + 4;
  SVN_ERR_CLIENT_INVALID_EXTERNALS_DESCRIPTION    = SVN_ERR_CLIENT_CATEGORY_START + 5;
  SVN_ERR_CLIENT_MODIFIED                         = SVN_ERR_CLIENT_CATEGORY_START + 6;
  SVN_ERR_CLIENT_IS_DIRECTORY                     = SVN_ERR_CLIENT_CATEGORY_START + 7;
  SVN_ERR_CLIENT_REVISION_RANGE                   = SVN_ERR_CLIENT_CATEGORY_START + 8;
  SVN_ERR_CLIENT_INVALID_RELOCATION               = SVN_ERR_CLIENT_CATEGORY_START + 9;
  SVN_ERR_CLIENT_REVISION_AUTHOR_CONTAINS_NEWLINE = SVN_ERR_CLIENT_CATEGORY_START + 10;
  SVN_ERR_CLIENT_PROPERTY_NAME                    = SVN_ERR_CLIENT_CATEGORY_START + 11;
  SVN_ERR_CLIENT_UNRELATED_RESOURCES              = SVN_ERR_CLIENT_CATEGORY_START + 12;
  SVN_ERR_CLIENT_MISSING_LOCK_TOKEN               = SVN_ERR_CLIENT_CATEGORY_START + 13;

  SVN_ERR_BASE                                    = SVN_ERR_MISC_CATEGORY_START + 0;
  SVN_ERR_PLUGIN_LOAD_FAILURE                     = SVN_ERR_MISC_CATEGORY_START + 1;
  SVN_ERR_MALFORMED_FILE                          = SVN_ERR_MISC_CATEGORY_START + 2;
  SVN_ERR_INCOMPLETE_DATA                         = SVN_ERR_MISC_CATEGORY_START + 3;
  SVN_ERR_INCORRECT_PARAMS                        = SVN_ERR_MISC_CATEGORY_START + 4;
  SVN_ERR_UNVERSIONED_RESOURCE                    = SVN_ERR_MISC_CATEGORY_START + 5;
  SVN_ERR_TEST_FAILED                             = SVN_ERR_MISC_CATEGORY_START + 6;
  SVN_ERR_UNSUPPORTED_FEATURE                     = SVN_ERR_MISC_CATEGORY_START + 7;
  SVN_ERR_BAD_PROP_KIND                           = SVN_ERR_MISC_CATEGORY_START + 8;
  SVN_ERR_ILLEGAL_TARGET                          = SVN_ERR_MISC_CATEGORY_START + 9;
  SVN_ERR_DELTA_MD5_CHECKSUM_ABSENT               = SVN_ERR_MISC_CATEGORY_START + 10;
  SVN_ERR_DIR_NOT_EMPTY                           = SVN_ERR_MISC_CATEGORY_START + 11;
  SVN_ERR_EXTERNAL_PROGRAM                        = SVN_ERR_MISC_CATEGORY_START + 12;
  SVN_ERR_SWIG_PY_EXCEPTION_SET                   = SVN_ERR_MISC_CATEGORY_START + 13;
  SVN_ERR_CHECKSUM_MISMATCH                       = SVN_ERR_MISC_CATEGORY_START + 14;
  SVN_ERR_CANCELLED                               = SVN_ERR_MISC_CATEGORY_START + 15;
  SVN_ERR_INVALID_DIFF_OPTION                     = SVN_ERR_MISC_CATEGORY_START + 16;
  SVN_ERR_PROPERTY_NOT_FOUND                      = SVN_ERR_MISC_CATEGORY_START + 17;
  SVN_ERR_NO_AUTH_FILE_PATH                       = SVN_ERR_MISC_CATEGORY_START + 18;
  SVN_ERR_VERSION_MISMATCH                        = SVN_ERR_MISC_CATEGORY_START + 19;

  SVN_ERR_CL_ARG_PARSING_ERROR                    = SVN_ERR_CL_CATEGORY_START + 0;
  SVN_ERR_CL_INSUFFICIENT_ARGS                    = SVN_ERR_CL_CATEGORY_START + 1;
  SVN_ERR_CL_MUTUALLY_EXCLUSIVE_ARGS              = SVN_ERR_CL_CATEGORY_START + 2;
  SVN_ERR_CL_ADM_DIR_RESERVED                     = SVN_ERR_CL_CATEGORY_START + 3;
  SVN_ERR_CL_LOG_MESSAGE_IS_VERSIONED_FILE        = SVN_ERR_CL_CATEGORY_START + 4;
  SVN_ERR_CL_LOG_MESSAGE_IS_PATHNAME              = SVN_ERR_CL_CATEGORY_START + 5;
  SVN_ERR_CL_COMMIT_IN_ADDED_DIR                  = SVN_ERR_CL_CATEGORY_START + 6;
  SVN_ERR_CL_NO_EXTERNAL_EDITOR                   = SVN_ERR_CL_CATEGORY_START + 7;
  SVN_ERR_CL_BAD_LOG_MESSAGE                      = SVN_ERR_CL_CATEGORY_START + 8;
  SVN_ERR_CL_UNNECESSARY_LOG_MESSAGE              = SVN_ERR_CL_CATEGORY_START + 9;
  
//----- svn_error_codes.h ----------------------------------------------------------------------------------------------

//----- svn_error.h ----------------------------------------------------------------------------------------------------

const
  SVN_NO_ERROR   = 0;

var
  svn_error__locate: procedure(afile: PChar; line: Longint); cdecl;
  svn_strerror: function(statcode: TAprStatus; buf: PChar; bufsize: TAprSize): PChar; cdecl;
  svn_error_create: function(apr_err: TAprStatus; child: PSvnError; message: PChar): PSvnError; cdecl;
  svn_error_createf: function(apr_err: TAprStatus; child: PSvnError; fmt: PChar;
    const args: array of const): PSvnError; cdecl;
  svn_error_wrap_apr: function(status: TAprStatus; fmt: PChar; const args: array of const): PSvnError; cdecl;
  svn_error_quick_wrap: function(child: PSvnError; new_msg: PChar): PSvnError; cdecl;
  svn_error_compose: procedure(chain: PSvnError; new_err: pSvnError); cdecl;
  svn_error_dup: function(err: PSvnError): PSvnError; cdecl;
  svn_error_clear: procedure(error: PSvnError); cdecl;
  svn_handle_error2: procedure(error: PSvnError; stream: THandle; fatal: TSvnBoolean; prefix: PChar); cdecl;
  svn_handle_error: procedure(error: PSvnError; stream: THandle; fatal: TSvnBoolean); cdecl;
  svn_handle_warning2: procedure(stream: THandle; error: PSvnError; prefix: PChar); cdecl;
  svn_handle_warning: procedure(stream: THandle; error: PSvnError); cdecl;

function SvnIsLockError(err: TSvnError): Boolean;
function SvnIsUnlockError(err: TSvnError): Boolean;

//----- svn_error.h ----------------------------------------------------------------------------------------------------

//----- svn_path.h -----------------------------------------------------------------------------------------------------

var
  svn_path_internal_style: function(path: PChar; pool: PAprPool): PChar; cdecl;
  svn_path_local_style: function(path: PChar; pool: PAprPool): PChar; cdecl;
  svn_path_join: function(base, component: PChar; pool: PAprPool): PChar; cdecl;
  svn_path_join_many: function(pool: PAprPool; const base: array of PChar): PChar; cdecl;
  svn_path_basename: function(path: PChar; pool: PAprPool): PChar; cdecl;
  svn_path_dirname: function(path: PChar; pool: PAprPool): PChar; cdecl;
  svn_path_component_count: function(path: PChar): TAprSize; cdecl;
  svn_path_add_component: procedure(path: PSvnStringBuf; component: PChar); cdecl;
  svn_path_remove_component: procedure(path: PSvnStringBuf); cdecl;
  svn_path_remove_components: procedure(path: PSvnStringBuf; n: TAprSize); cdecl;
  svn_path_split: procedure(path: PChar; out dirpath, base_name: PChar; pool: PAprPool); cdecl;
  svn_path_is_empty: function(path: PChar): LongBool; cdecl;
  svn_path_canonicalize: function(path: PChar; pool: PAprPool): PChar; cdecl;
  svn_path_compare_paths: function(path1, path2: PChar): Integer; cdecl;
  svn_path_get_longest_ancestor: function(path1, path2: PChar; pool: PAprPool): PChar; cdecl;
  svn_path_get_absolute: function(out pabsolute: PChar; relative: PChar; pool: PAprPool): PSvnError; cdecl;
  svn_path_split_if_file: function(path: PChar; out pdirectory, pfile: PChar; pool: PAprPool): PSvnError; cdecl;
  svn_path_condense_targets: function(out pcommon: PChar; out pcondensed_targets: PAprArrayHeader;
    targets: PAprArrayHeader; remove_redundancies: TSvnBoolean; pool: PAprPool): PSvnError; cdecl;
  svn_path_remove_redundancies: function(out pcondensed_targets: PAprArrayHeader; targets: PAprArrayHeader;
    pool: PAprPool): PSvnError; cdecl;
  svn_path_decompose: function(path: PChar; pool: PAprPool): PAprArrayHeader; cdecl;
  svn_path_is_single_path_component: function(name: PChar): TSvnBoolean; cdecl;
  svn_path_is_backpath_present: function(path: PChar): TSvnBoolean; cdecl;
  svn_path_is_child: function(path1, path2: PChar; pool: PAprPool): PChar; cdecl;
  svn_path_is_ancestor: function(path1, path2: PChar): TSvnBoolean; cdecl;
  svn_path_check_valid: function(path: PChar; pool: PAprPool): PSvnError; cdecl;
  svn_path_is_url: function(path: PChar): TSvnBoolean; cdecl;
  svn_path_is_uri_safe: function(path: PChar): TSvnBoolean; cdecl;
  svn_path_uri_encode: function(path: PChar; pool: PAprPool): PChar; cdecl;
  svn_path_uri_decode: function(path: PChar; pool: PAprPool): PChar; cdecl;
  svn_path_url_add_component: function(url, component: PChar; pool: PAprPool): PChar; cdecl;
  svn_path_uri_from_iri: function(iri: PChar; pool: PAprPool): PChar; cdecl;
  svn_path_uri_autoescape: function(uri: PChar; pool: PAprPool): PChar; cdecl;
  svn_path_cstring_from_utf8: function(out path_apr: PChar; path_utf8: PChar; pool: PAprPool): PSvnError; cdecl;
  svn_path_cstring_to_utf8: function(out path_utf8: PChar; path_apr: PChar; pool: PAprPool): PSvnError; cdecl;

//----- svn_path.h -----------------------------------------------------------------------------------------------------

//----- svn_config.h ---------------------------------------------------------------------------------------------------

const
  SVN_CONFIG_CATEGORY_SERVERS = 'servers';
  SVN_CONFIG_SECTION_GROUPS = 'groups';
  SVN_CONFIG_SECTION_GLOBAL = 'global';
  SVN_CONFIG_OPTION_HTTP_PROXY_HOST = 'http-proxy-host';
  SVN_CONFIG_OPTION_HTTP_PROXY_PORT = 'http-proxy-port';
  SVN_CONFIG_OPTION_HTTP_PROXY_USERNAME = 'http-proxy-username';
  SVN_CONFIG_OPTION_HTTP_PROXY_PASSWORD = 'http-proxy-password';
  SVN_CONFIG_OPTION_HTTP_PROXY_EXCEPTIONS = 'http-proxy-exceptions';
  SVN_CONFIG_OPTION_HTTP_TIMEOUT = 'http-timeout';
  SVN_CONFIG_OPTION_HTTP_COMPRESSION = 'http-compression';
  SVN_CONFIG_OPTION_NEON_DEBUG_MASK = 'neon-debug-mask';
  SVN_CONFIG_OPTION_SSL_AUTHORITY_FILES = 'ssl-authority-files';
  SVN_CONFIG_OPTION_SSL_TRUST_DEFAULT_CA = 'ssl-trust-default-ca';
  SVN_CONFIG_OPTION_SSL_CLIENT_CERT_FILE = 'ssl-client-cert-file';
  SVN_CONFIG_OPTION_SSL_CLIENT_CERT_PASSWORD = 'ssl-client-cert-password';

  SVN_CONFIG_CATEGORY_CONFIG = 'config';
  SVN_CONFIG_SECTION_AUTH = 'auth';
  SVN_CONFIG_OPTION_STORE_PASSWORDS = 'store-passwords';
  SVN_CONFIG_OPTION_STORE_AUTH_CREDS = 'store-auth-creds';
  SVN_CONFIG_SECTION_HELPERS = 'helpers';
  SVN_CONFIG_OPTION_EDITOR_CMD = 'editor-cmd';
  SVN_CONFIG_OPTION_DIFF_CMD = 'diff-cmd';
  SVN_CONFIG_OPTION_DIFF3_CMD = 'diff3-cmd';
  SVN_CONFIG_OPTION_DIFF3_HAS_PROGRAM_ARG = 'diff3-has-program-arg';
  SVN_CONFIG_SECTION_MISCELLANY = 'miscellany';
  SVN_CONFIG_OPTION_GLOBAL_IGNORES = 'global-ignores';
  SVN_CONFIG_OPTION_LOG_ENCODING = 'log-encoding';
  SVN_CONFIG_OPTION_USE_COMMIT_TIMES = 'use-commit-times';
  SVN_CONFIG_OPTION_TEMPLATE_ROOT = 'template-root';
  SVN_CONFIG_OPTION_ENABLE_AUTO_PROPS = 'enable-auto-props';
  SVN_CONFIG_OPTION_NO_UNLOCK = 'no-unlock';
  SVN_CONFIG_SECTION_TUNNELS = 'tunnels';
  SVN_CONFIG_SECTION_AUTO_PROPS = 'auto-props';

  SVN_CONFIG_SECTION_GENERAL = 'general';
  SVN_CONFIG_OPTION_ANON_ACCESS = 'anon-access';
  SVN_CONFIG_OPTION_AUTH_ACCESS = 'auth-access';
  SVN_CONFIG_OPTION_PASSWORD_DB = 'password-db';
  SVN_CONFIG_OPTION_REALM = 'realm';
  SVN_CONFIG_OPTION_AUTHZ_DB = 'authz-db';

  SVN_CONFIG_SECTION_USERS = 'users';

  SVN_CONFIG_DEFAULT_GLOBAL_IGNORES = '*.o *.lo *.la #*# .*.rej *.rej .*~ *~ .#* .DS_Store';

  SVN_CONFIG_TRUE = 'true';
  SVN_CONFIG_FALSE = 'false';

  SVN_CONFIG_REALMSTRING_KEY = 'svn:realmstring';

type
  PSvnConfig = ^TSvnConfig;
  TSvnConfig = THandle;
  TSvnConfigSectionEnumerator = function(name: PChar; baton: Pointer): TSvnBoolean; cdecl;
  TSvnConfigSectionEnumerator2 = function(name: PChar; baton: Pointer; pool: PAprPool): TSvnBoolean; cdecl;
  TSvnConfigEnumerator = function(name, value: PChar; baton: Pointer): TSvnBoolean; cdecl;
  TSvnConfigEnumerator2 = function(name, value: PChar; baton: Pointer; pool: PAprPool): TSvnBoolean; cdecl;

var
  svn_config_get_config: function(out cfg_hash: PAprHash; config_dir: PChar; pool: PAprPool): PSvnError; cdecl;
  svn_config_read: function(out cfgp: PSvnConfig; afile: PChar; must_exist: TSvnBoolean; pool: PAprPool): PSvnError;
    cdecl;
  svn_config_merge: function(cfg: PSvnConfig; afile: PChar; must_exist: TSvnBoolean): PSvnError; cdecl;
  svn_config_get: procedure(cfg: PSvnConfig; out valuep: PChar; section, option, default_value: PChar); cdecl;
  svn_config_set: procedure(cfg: PSvnConfig; section, option, value: PChar); cdecl;
  svn_config_get_bool: function(cfg: PSvnConfig; out valuep: TSvnBoolean; section, option: PChar;
    default_value: TSvnBoolean): PSvnError; cdecl;
  svn_config_set_bool: procedure(cfg: PSvnConfig; section, option: PChar; value: TSvnBoolean); cdecl;
  svn_config_enumerate_sections: function(cfg: PSvnConfig; callback: TSvnConfigSectionEnumerator;
    baton: Pointer): Integer; cdecl;
  svn_config_enumerate_sections2: function(cfg: PSvnConfig; callback: TSvnConfigSectionEnumerator2; baton: Pointer;
    pool: PAprPool): Integer; cdecl;
  svn_config_enumerate: function(cfg: PSvnConfig; section: PChar; callback: TSvnConfigEnumerator;
    baton: Pointer): Integer; cdecl;
  svn_config_enumerate2: function(cfg: PSvnConfig; section: PChar; callback: TSvnConfigEnumerator2; baton: Pointer;
    pool: PAprPool): Integer; cdecl;
  svn_config_find_group: function(cfg: PSvnConfig; key, master_section: PChar; pool: PAprPool): PChar; cdecl;
  svn_config_get_server_setting: function(cfg: PSvnConfig; server_group, option_name, default_value: PChar): PChar;
    cdecl;
  svn_config_get_server_setting_int: function(cfg: PSvnConfig; server_group, option_name: PChar; default_value: Int64;
    out result_value: Int64; pool: PAprPool): PSvnError; cdecl;
  svn_config_ensure: function(config_dir: PChar; pool: PAprPool): PSvnError; cdecl;
  svn_config_read_auth_data: function(out hash: PAprHash; cred_kind, realmstring, config_dir: PChar;
    pool: PAprPool): PSvnError; cdecl;
  svn_config_write_auth_data: function(hash: PAprHash; cred_kind, realmstring, config_dir: PChar;
    pool: PAprPool): PSvnError; stdcall;

//----- svn_config.h ---------------------------------------------------------------------------------------------------

//----- svn_io.h -------------------------------------------------------------------------------------------------------

type
  PSvnIODirEnt = ^TSvnIODirEnt;
  TSvnIODirEnt = record
    kind: TSvnNodeKind;
    special: TSvnBoolean;
  end;
  PPSvnStream = ^PSvnStream;
  PSvnStream = ^TSvnStream;
  TSvnStream = THandle;
  TSvnReadFunc = function(baton: Pointer; buffer: PChar; var len: TAprSize): PSvnError; cdecl;
  TSvnWriteFunc = function(baton: Pointer; data: PChar; var len: TAprSize): PSvnError; cdecl;
  TSvnCloseFunc = function(baton: Pointer): PSvnError; cdecl;
  TSvnIOWalkFunc = function(baton: Pointer; path: PChar; finfo: PAprFInfo; pool: PAprPool): PSvnError; cdecl;

var
  svn_io_check_path: function(path: PChar; out kind: TSvnNodeKind; pool: PAprPool): PSvnError; cdecl;
  svn_io_check_special_path: function(path: PChar; out kind: TSvnNodeKind; out is_special: TSvnBoolean;
    pool: PAprPool): PSvnError; cdecl;
  svn_io_check_resolved_path: function(path: PChar; out kind: TSvnNodeKind; pool: PAprPool): PSvnError; cdecl;
  svn_io_open_unique_file: function(out f: PAprFile; out unique_name_p: PChar; path, suffix: PChar;
    delete_on_close: TSvnBoolean; pool: PAprPool): PSvnError; cdecl;
  svn_io_create_unique_link: function(out unique_name_p: PChar; path, dest, suffix: PChar; pool: PAprPool): PSvnError;
    cdecl;
  svn_io_read_link: function(out dest: PSvnString; path: PChar; pool: PAprPool): PSvnError; cdecl;
  svn_io_temp_dir: function(out dir: PChar; pool: PAprPool): PSvnError; cdecl;
  svn_io_copy_file: function(src, dst: PChar; copy_perms: TSvnBoolean; pool: PAprPool): PSvnError; cdecl;
  svn_io_copy_link: function(src, dst: PChar; pool: PAprPool): PSvnError; cdecl;
  svn_io_copy_dir_recursively: function(src, dst_parent, dst_basename: PChar; copy_perms: TSvnBoolean;
    cancel_func: TSvnCancelFunc; cancel_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
  svn_io_make_dir_recursively: function(path: PChar; pool: PAprPool): PSvnError; cdecl;
  svn_io_dir_empty: function(out is_empty_p: TSvnBoolean; path: PChar; pool: PAprPool): PSvnError; cdecl;
  svn_io_append_file: function(src, dst: PChar; pool: PAprPool): PSvnError; cdecl;
  svn_io_set_file_read_only: function(path: PChar; ignore_enoent: TSvnBoolean; pool: PAprPool): PSvnError; cdecl;
  svn_io_set_file_read_write: function(path: PChar; ignore_enoent: TSvnBoolean; pool: PAprPool): PSvnError; cdecl;
  svn_io_set_file_read_write_carefully: function(path: PChar; enable_write, ignore_enoent: TSvnBoolean;
    pool: PAprPool): PSvnError; cdecl;
  svn_io_set_file_executable: function(path: PChar; executable, ignore_enoent: TSvnBoolean; pool: PAprPool): PSvnError;
    cdecl;
  svn_io_is_file_executable: function(out executable: TSvnBoolean; path: PChar; pool: PAprPool): PSvnError; cdecl;
  svn_io_read_length_line: function(afile: PAprFile; buf: PChar; out limit: TAprSize; pool: PAprPool): PSvnError;
    cdecl;
  svn_io_file_affected_time: function(out apr_time: TAprTime; path: PChar; pool: PAprPool): PSvnError; cdecl;
  svn_io_set_file_affected_time: function(apr_time: TAprTime; path: PChar; pool: PAprPool): PSvnError; cdecl;
  svn_io_filesizes_different_p: function(out different_p: TSvnBoolean; file1, file2: PChar; pool: PAprPool): PSvnError;
    cdecl;
  svn_io_file_checksum: function(digest: PByte; afile: PChar; pool: PAprPool): PSvnError; cdecl;
  svn_io_files_contents_same_p: function(out same: TSvnBoolean; file1, file2: PChar; pool: PAprPool): PSvnError;
    cdecl;
  svn_io_file_create: function(afile, contents: PChar; pool: PAprPool): PSvnError; cdecl;
  svn_io_file_lock: function(lock_file: PChar; exclusive: TSvnBoolean; pool: PAprPool): PSvnError; cdecl;
  svn_io_file_lock2: function(lock_file: PChar; exclusive, nonblocking: TSvnBoolean; pool: PAprPool): PSvnError;
    cdecl;
  svn_io_file_flush_to_disk: function(afile: PAprFile; pool: PAprPool): PSvnError; cdecl;
  svn_io_dir_file_copy: function(src_path, dest_path, afile: PChar; pool: PAprPool): PSvnError; cdecl;

  svn_stream_create: function(baton: Pointer; pool: PAprPool): PSvnStream;
  svn_stream_set_baton: procedure(stream: PSvnStream; baton: Pointer); cdecl;
  svn_stream_set_read: procedure(stream: PSvnStream; read_fn: TSvnReadFunc); cdecl;
  svn_stream_set_write: procedure(stream: PSvnStream; write_fn: TSvnWriteFunc); cdecl;
  svn_stream_set_close: procedure(stream: PSvnStream; close_fn: TSvnCloseFunc); cdecl;
  svn_stream_empty: function(pool: PAprPool): PSvnStream; cdecl;
  svn_stream_from_aprfile: function(afile: PAprFile; pool: PAprPool): PSvnStream; cdecl;
  svn_stream_for_stdout: function(out stdout: PSvnStream; pool: PAprPool): PSvnError; cdecl;
  svn_stream_from_stringbuf: function(str: PSvnStringBuf; pool: PAprPool): PSvnStream; cdecl;
  svn_stream_compressed: function(stream: PSvnStream; pool: PAprPool): PSvnStream; cdecl;
  svn_stream_read: function(stream: PSvnStream; buffer: PChar; var len: TAprSize): PSvnError; cdecl;
  svn_stream_write: function(stream: PSvnStream; data: PChar; var len: TAprSize): PSvnError; cdecl;
  svn_stream_close: function(stream: PSvnStream): PSvnError; cdecl;
  svn_stream_printf: function(stream: PSvnStream; pool: PAprPool; fmt: PChar; const args: array of const): PSvnError;
    cdecl;
  svn_stream_printf_from_utf8: function(stream: PSvnStream; encoding: PChar; pool: PAprPool; fmt: PChar;
    const args: array of const): PSvnError; cdecl;
  svn_stream_readline: function(stream: PSvnStream; out stringbuf: PSvnStringBuf; eol: PChar; out eof: TSvnBoolean;
    pool: PAprPool): PSvnError; cdecl;
  svn_stream_copy: function(sfrom, sto: PSvnStream; pool: PAprPool): PSvnError; cdecl;
  svn_stringbuf_from_file: function(out result: PSvnStringBuf; filename: PChar; pool: PAprPool): PSvnError; cdecl;
  svn_stringbuf_from_aprfile: function(out result: PSvnStringBuf; afile: PAprFile; pool: PAprPool): PSvnError; cdecl;
  svn_io_remove_file: function(path: PChar; pool: PAprPool): PSvnError; cdecl;
  svn_io_remove_dir: function(path: PChar; pool: PAprPool): PSvnError; cdecl;
  svn_io_get_dirents2: function(out dirents: PAprHash; path: PChar; pool: PAprPool): PSvnError; cdecl;
  svn_io_get_dirents: function(out dirents: PAprHash; path: PChar; pool: PAprPool): PSvnError; cdecl;
  svn_io_dir_walk: function(dirname: PChar; wanted: Integer; walk_func: TSvnIOWalkFunc; walk_baton: Pointer;
    pool: PAprPool): PSvnError; cdecl;
  svn_io_start_cmd: function(out cmd_proc: TAprProc; path, cmd: PChar; args: PPChar; inherit: TSvnBoolean;
    infile, outfile, errfile: PAprFile; pool: PAprPool): PSvnError; cdecl;
  svn_io_wait_for_cmd: function(cmd_proc: PAprProc; cmd: PChar; exitcode, exitwhy: PInteger; pool: PAprPool): PSvnError;
    cdecl;
  svn_io_run_cmd: function(path, cmd: PChar; args: PPChar; exitcode, exitwhy: PInteger; inherit: TSvnBoolean;
    infile, outfile, errfile: PAprFile; pool: PAprPool): PSvnError; cdecl;
  svn_io_run_diff: function(dir: PChar; user_args: PPChar; num_user_args: Integer; label1, label2, ffrom, fto: PChar;
    exitcode: PInteger; outfile, errfile: PAprFile; diff_cmd: PChar; pool: PAprPool): PSvnError; cdecl;
  svn_io_run_diff3: function(dir, mine, older, yours, mine_label, older_label, yours_label: PChar; merged: PAprFile;
    exitcode: PInteger; diff3_cmd: PChar; pool: PAprPool): PSvnError; cdecl;
  svn_io_detect_mimetype: function(out mimetype: PChar; afile: PChar; pool: PAprPool): PSvnError; cdecl;
  svn_io_file_open: function(out new_file: PAprFile; fname: PChar; flag: Integer; perm: TAprFilePerms;
    pool: PAprPool): PSvnError; cdecl;
  svn_io_file_close: function(afile: PAprFile; pool: PAprPool): PSvnError; cdecl;
  svn_io_file_getc: function(out ch: Char; afile: PAprFile; pool: PAprPool): PSvnError; cdecl;
  svn_io_file_info_get: function(out finfo: TAprFInfo; wanted: Integer; afile: PAprFile; pool: PAprPool): PSvnError;
    cdecl;
  svn_io_file_read: function(afile: PAprFile; buf: Pointer; var nbytes: TAprSize; pool: PAprPool): PSvnError; cdecl;
  svn_io_file_read_full: function(afile: PAprFile; buf: Pointer; nbytes: TAprSize; out bytes_read: TAprSize;
    pool: PAprPool): PSvnError; cdecl;
  svn_io_file_seek: function(afile: PAprFile; where: TAprSeekWhere; offset: PAprOff; pool: PAprPool): PSvnError;
    cdecl;
  svn_io_file_write: function(afile: PAprFile; buf: Pointer; var nbytes: TAprSize; pool: PAprPool): PSvnError; cdecl;
  svn_io_file_write_full: function(afile: PAprFile; buf: Pointer; nbytes: TAprSize; bytes_written: PAprSize;
    pool: PAprPool): PSvnError; cdecl;
  svn_io_stat: function(out finfo: TAprFInfo; fname: PChar; wanted: Integer; pool: PAprPool): PSvnError; cdecl;
  svn_io_file_rename: function(from_path, to_path: PChar; pool: PAprPool): PSvnError; cdecl;
  svn_io_file_move: function(from_path, to_path: PChar; pool: PAprPool): PSvnError; cdecl;
  svn_io_dir_make: function(path: PChar; perm: TAprFilePerms; pool: PAprPool): PSvnError; cdecl;
  svn_io_dir_make_hidden: function(path: PChar; perm: TAprFilePerms; pool: PAprPool): PSvnError; cdecl;
  svn_io_dir_make_sgid: function(path: PChar; perm: TAprFilePerms; pool: PAprPool): PSvnError; cdecl;
  svn_io_dir_open: function(out new_dir: PAprDir; dirname: PChar; pool: PAprPool): PSvnError; cdecl;
  svn_io_dir_remove_nonrecursive: function(dirname: PChar; pool: PAprPool): PSvnError; cdecl;
  svn_io_dir_read: function(finfo: PAprFInfo; wanted: Integer; thedir: PAprDir; pool: PAprPool): PSvnError; cdecl;
  svn_io_read_version_file: function(out version: Integer; path: PChar; pool: PAprPool): PSvnError; cdecl;
  svn_io_write_version_file: function(path: PChar; version: Integer; pool: PAprPool): PSvnError; cdecl;

//----- svn_io.h -------------------------------------------------------------------------------------------------------

//----- svn_quoprint.h -------------------------------------------------------------------------------------------------

var
  svn_quoprint_encode: function(output: PSvnStream; pool: PAprPool): PSvnStream; cdecl;
  svn_quoprint_decode: function(output: PSvnStream; pool: PAprPool): PSvnStream; cdecl;
  svn_quoprint_encode_string: function(str: PSvnStringBuf; pool: PAprPool): PSvnStringBuf; cdecl;
  svn_quoprint_decode_string: function(str: PSvnStringBuf; pool: PAprPool): PSvnStringBuf; cdecl;

//----- svn_quoprint.h -------------------------------------------------------------------------------------------------

//----- svn_hash.h -----------------------------------------------------------------------------------------------------

const
  SVN_KEYLINE_MAXLEN = 100;
  SVN_HASH_TERMINATOR = 'END';

type
  TSvnHashDiffKeyStatus = (svnHashDiffKeyBoth, svnHashDiffKeyA, svnHashDiffKeyB);
  TSvnHashDiffFunc = function(key: Pointer; klen: TAprSize; status: TSvnHashDiffKeyStatus; baton: Pointer): PSvnError;
    cdecl;

var
  svn_hash_read2: function(hash: PAprHash; stream: PSvnStream; terminator: PChar; pool: PAprPool): PSvnError; cdecl;
  svn_hash_write2: function(hash: PAprHash; stream: PSvnStream; terminator: PChar; pool: PAprPool): PSvnError; cdecl;
  svn_hash_read_incremental: function(hash: PAprHash; stream: PSvnStream; terminator: PChar; pool: PAprPool): PSvnError;
    cdecl;
  svn_hash_write_incremental: function(hash, oldhash: PAprHash; stream: PSvnStream; terminator: PChar;
    pool: PAprPool): PSvnError; cdecl;
  svn_hash_read: function(hash: PAprHash; srcfile: PAprFile; pool: PAprPool): PSvnError; cdecl;
  svn_hash_write: function(hash: PAprHash; destfile: PAprFile; pool: PAprPool): PSvnError; cdecl;
  svn_hash_diff: function(hash_a, hash_b: PAprHash; diff_func: TSvnHashDiffFunc; diff_func_baton: Pointer;
    pool: PAprPool): PSvnError; cdecl;

//----- svn_hash.h -----------------------------------------------------------------------------------------------------

//----- svn_io.h -------------------------------------------------------------------------------------------------------

type
  TSvnSubstEOLStyle = (svnSubstEOLStyleUnknown, svnSubstEOLStyleNone, svnSubstEOLStyleNative, svnSubstEOLStyleFixed);
  PSvnSubstKeywords = ^TSvnSubstKeywords;
  TSvnSubstKeywords = record
    revision: PSvnString;
    date: PSvnString;
    author: PSvnString;
    url: PSvnString;
    id: PSvnString;
  end;

var
  svn_subst_eol_style_from_value: procedure(out style: TSvnSubstEOLStyle; out eol: PChar; value: PChar); cdecl;
  svn_subst_build_keywords2: function(out kw: PAprHash; keywords_string, rev, url: PChar; date: TAprTime;
    author: PChar; pool: PAprPool): PSvnError; cdecl;
  svn_subst_build_keywords: function(out kw: TSvnSubstKeywords; keywords_string, rev, url: PChar; date: TAprTime;
    author: PChar; pool: PAprPool): PSvnError; cdecl;
  svn_subst_keywords_differ2: function(a, b: PAprHash; compare_values: TSvnBoolean; pool: PAprPool): TSvnBoolean; cdecl;
  svn_subst_keywords_differ: function(a, b: PSvnSubstKeywords; compare_values: TSvnBoolean): TSvnBoolean; cdecl;
  svn_subst_translate_stream3: function(src, dst: PSvnStream; eol_str: PChar; repair: TSvnBoolean; keywords: PAprHash;
    expand: TSvnBoolean; pool: PAprPool): PSvnError; cdecl;
  svn_subst_translate_stream2: function(src, dst: PSvnStream; eol_str: PChar; repair: TSvnBoolean;
    keywords: PSvnSubstKeywords; expand: TSvnBoolean; pool: PAprPool): PSvnError; cdecl;
  svn_subst_translate_stream: function(src, dst: PSvnStream; eol_str: PChar; repair: TSvnBoolean;
    keywords: PSvnSubstKeywords; expand: TSvnBoolean): PSvnError; cdecl;
  svn_subst_copy_and_translate3: function(src, dst, eol_str: PChar; repair: TSvnBoolean; keywords: PAprHash;
    expand, special: TSvnBoolean; pool: PAprPool): PSvnError; cdecl;
  svn_subst_copy_and_translate2: function(src, dst, eol_str: PChar; repair: TSvnBoolean; keywords: PSvnSubstKeywords;
    expand, special: TSvnBoolean; pool: PAprPool): PSvnError; cdecl;
  svn_subst_copy_and_translate: function(src, dst, eol_str: PChar; repair: TSvnBoolean; keywords: PSvnSubstKeywords;
    expand: TSvnBoolean; pool: PAprPool): PSvnError; cdecl;
  svn_subst_translate_cstring2: function(src: PChar; out dst: PChar; eol_str: PChar; repair: TSvnBoolean;
    keywords: PAprHash; expand: TSvnBoolean; pool: PAprPool): PSvnError; cdecl;
  svn_subst_translate_cstring: function(src: PChar; out dst: PChar; eol_str: PChar; repair: TSvnBoolean;
    keywords: PSvnSubstKeywords; expand: TSvnBoolean; pool: PAprPool): PSvnError; cdecl;
  svn_subst_translate_string: function(out new_value: PSvnString; value: PSvnString; encoding: PChar;
    pool: PAprPool): PSvnError; cdecl;
  svn_subst_detranslate_string: function(out new_value: PSvnString; value: PSvnString; for_stdout: TSvnBoolean;
    pool: PAprPool): PSvnError; cdecl;

//----- svn_io.h -------------------------------------------------------------------------------------------------------

//----- svn_diff.h -----------------------------------------------------------------------------------------------------

type
  PSvnDiff = ^TSvnDiff;
  TSvnDiff = THandle;
  TSvnDiffDataSource = (svnDiffDatasourceOriginal, svnDiffDatasourceModified, svnDiffDatasourceLatest,
    svnDiffDatasourceAncestor);
  PSvnDiffFns = ^TSvnDiffFns;
  TSvnDiffFns = record
    datasource_open: function(diff_baton: Pointer; datasource: TSvnDiffDataSource): PSvnError; cdecl;
    datasource_close: function(diff_baton: Pointer; datasource: TSvnDiffDataSource): PSvnError; cdecl;
    datasource_get_next_token: function(hash: PCardinal; out token: Pointer; diff_baton: Pointer;
      datasource: TSvnDiffDataSource): PSvnError; cdecl;
    token_compare: function(diff_baton, ltoken, rtoken: Pointer; out compare: Integer): PSvnError; cdecl;
    token_discard: procedure(diff_baton, token: Pointer); cdecl;
    token_discard_all: procedure(diff_baton: Pointer); cdecl;
  end;
  PSvnDiffOutputFns = ^TSvnDiffOutputFns;
  TSvnDiffOutputFns = record
    output_common: function(output_baton: Pointer; original_start, original_length, modified_start, modified_length,
      latest_start, latest_length: TAprOff): PSvnError; cdecl;
    output_diff_modified: function(output_baton: Pointer; original_start, original_length, modified_start,
      modified_length, latest_start, latest_length: TAprOff): PSvnError; cdecl;
    output_diff_latest: function(output_baton: Pointer; original_start, original_length, modified_start,
      modified_length, latest_start, latest_length: TAprOff): PSvnError; cdecl;
    output_diff_common: function(output_baton: Pointer; original_start, original_length, modified_start,
      modified_length, latest_start, latest_length: TAprOff): PSvnError; cdecl;
    output_conflict: function(output_baton: Pointer; original_start, original_length, modified_start, modified_length,
      latest_start, latest_length: TAprOff; resolved_diff: PSvnDiff): PSvnError; cdecl;
  end;

var
  svn_diff_version: function: PSvnVersion; cdecl;
  svn_diff_diff: function(out diff: PSvnDiff; diff_baton: Pointer; diff_fns: PSvnDiffFns; pool: PAprPool): PSvnError;
    cdecl;
  svn_diff_diff3: function(out diff: PSvnDiff; diff_baton: Pointer; diff_fns: PSvnDiffFns; pool: PAprPool): PSvnError;
    cdecl;
  svn_diff_diff4: function(out diff: PSvnDiff; diff_baton: Pointer; diff_fns: PSvnDiffFns; pool: PAprPool): PSvnError;
    cdecl;
  svn_diff_contains_conflicts: function(diff: PSvnDiff): TSvnBoolean; cdecl;
  svn_diff_contains_diffs: function(diff: PSvnDiff): TSvnBoolean; cdecl;
  svn_diff_output: function(diff: PSvnDiff; output_baton: Pointer; output_fns: PSvnDiffOutputFns): PSvnError; cdecl;
  svn_diff_file_diff: function(out diff: PSvnDiff; original, modified: PChar; pool: PAprPool): PSvnError; cdecl;
  svn_diff_file_diff3: function(out diff: PSvnDiff; original, modified, latest: PChar; pool: PAprPool): PSvnError;
    cdecl;
  svn_diff_file_diff4: function(out diff: PSvnDiff; original, modified, latest, ancestor: PChar;
    pool: PAprPool): PSvnError; cdecl;
  svn_diff_file_output_unified2: function(output_stream: PSvnStream; diff: PSvnDiff; original_path, modified_path,
    original_header, modified_header, header_encoding: PChar; pool: PAprPool): PSvnError; cdecl;
  svn_diff_file_output_unified: function(output_stream: PSvnStream; diff: PSvnDiff; original_path, modified_path,
    original_header, modified_header: PChar; pool: PAprPool): PSvnError; cdecl;
  svn_diff_file_output_merge: function(output_stream: PSvnStream; diff: PSvnDiff; original_path, modified_path,
    latest_path, conflict_original, conflict_modified, conflict_latest, conflict_separator: PChar;
    display_original_in_conflict, display_resolved_conflicts: TSvnBoolean; pool: PAprPool): PSvnError; cdecl;

//----- svn_diff.h -----------------------------------------------------------------------------------------------------

//----- svn_base64.h ---------------------------------------------------------------------------------------------------

var
  svn_base64_encode: function(output: PSvnStream; pool: PAprPool): PSvnStream; cdecl;
  svn_base64_decode: function(output: PSvnStream; pool: PAprPool): PSvnStream; cdecl;
  svn_base64_encode_string: function(str: PSvnString; pool: PAprPool): PSvnString; cdecl;
  svn_base64_decode_string: function(str: PSvnString; pool: PAprPool): PSvnString; cdecl;
  svn_base64_from_md5: function(digest: PByte; pool: PAprPool): PSvnStringBuf; cdecl;

//----- svn_base64.h ---------------------------------------------------------------------------------------------------

//----- svn_delta.h ----------------------------------------------------------------------------------------------------

type
  TSvnDeltaAction = (svnTxDeltaSource, svnTxDeltaTarget, svnTxDeltaNew);
  PSvnTxDeltaOp = ^TSvnTxDeltaOp;
  TSvnTxDeltaOp = record
    action_code: TSvnDeltaAction;
    offset: TAprSize;
    length: TAprSize;
  end;
  PSvnTxDeltaWindow = ^TSvnTxDeltaWindow;
  TSvnTxDeltaWindow = record
    sview_offset: TSvnFileSize;
    sview_len: TAprSize;
    tview_len: TAprSize;
    num_ops: Integer;
    src_ops: Integer;
    ops: PSvnTxDeltaOp;
    new_data: PSvnString;
  end;
  TSvnTxDeltaWindowHandler = function(window: PSvnTxDeltaWindow; baton: Pointer): PSvnError; cdecl;
  PSvnTxDeltaStream = ^TSvnTxDeltaStream;
  TSvnTxDeltaStream = THandle;

  PSvnDeltaEditor = ^TSvnDeltaEditor;
  TSvnDeltaEditor = record
    set_target_revision: function(edit_baton: Pointer; target_revision: TSvnRevNum; pool: PAprPool): PSvnError; cdecl;
    open_root: function(edit_baton: Pointer; base_revision: TSvnRevNum; dir_pool: PAprPool;
      out root_baton: Pointer): PSvnError; cdecl;
    delete_entry: function(path: PChar; revision: TSvnRevNum; parent_baton: Pointer; pool: PAprPool): PSvnError;
      cdecl;
    add_directory: function(path: PChar; parent_baton: Pointer; copyfrom_path: PChar; copyfrom_revision: TSvnRevNum;
      dir_pool: PAprPool; out child_baton: Pointer): PSvnError; cdecl;
    open_directory: function(path: PChar; parent_baton: Pointer; base_revision: TSvnRevNum; dir_pool: PAprPool;
      out child_baton: Pointer): PSvnError; cdecl;
    change_dir_prop: function(dir_baton: Pointer; name: PChar; value: PSvnString; pool: PAprPool): PSvnError; cdecl;
    close_directory: function(dir_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
    absent_directory: function(path: PChar; parent_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
    add_file: function(path: PChar; parent_baton: Pointer; copy_path: PChar; copy_revision: TSvnRevNum;
      file_pool: PAprPool; out file_baton: Pointer): PSvnError; cdecl;
    open_file: function(path: PChar; parent_baton: Pointer; base_revision: TSvnRevNum; file_pool: PAprPool;
      out file_baton: Pointer): PSvnError; cdecl;
    apply_text_delta: function(file_baton: Pointer; base_checksum: PChar; pool: PAprPool;
      out handler: TSvnTxDeltaWindowHandler; out handler_baton: Pointer): PSvnError; cdecl;
    change_file_prop: function(file_baton: Pointer; name: PChar; value: PSvnString; pool: PAprPool): PSvnError; cdecl;
    close_file: function(file_baton: Pointer; text_checksum: PChar; pool: PAprPool): PSvnError; cdecl;
    absent_file: function(path: PChar; parent_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
    close_edit: function(edit_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
    abort_edit: function(edit_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
  end;

  TSvnDeltaPathDriverCbFunc = function(out dir_baton: Pointer; parent_baton, callback_baton: Pointer;
    path: PChar; pool: PAprPool): PSvnError; cdecl;

var
  svn_delta_version: function: PSvnVersion; cdecl;
  svn_txdelta_window_dup: function(window: PSvnTxDeltaWindow; pool: PAprPool): PSvnTxDeltaWindow; cdecl;
  svn_txdelta_next_window: function(out window: PSvnTxDeltaWindow; stream: PSvnTxDeltaStream;
    pool: PAprPool): PSvnError; cdecl;
  svn_txdelta_md5_digest: function(stream: PSvnTxDeltaStream): PByte; cdecl;
  svn_txdelta: procedure(out stream: PSvnTxDeltaStream; source, target: PSvnStream; pool: PAprPool); cdecl;
  svn_txdelta_target_push: function(handler: TSvnTxDeltaWindowHandler; handler_baton: Pointer; source: PSvnStream;
    pool: PAprPool): PSvnStream; cdecl;
  svn_txdelta_send_string: function(str: PSvnString; handler: TSvnTxDeltaWindowHandler; handler_baton: Pointer;
    pool: PAprPool): PSvnError; cdecl;
  svn_txdelta_send_stream: function(stream: PSvnStream; handler: TSvnTxDeltaWindowHandler; handler_baton: Pointer;
    digest: PByte; pool: PAprPool): PSvnError; cdecl;
  svn_txdelta_send_txstream: function(txstream: PSvnTxDeltaStream; handler: TSvnTxDeltaWindowHandler;
    handler_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
  svn_txdelta_apply: procedure(source, target: PSvnStream; result_digest: PByte; error_info: PChar; pool: PAprPool;
    out handler: TSvnTxDeltaWindowHandler; out handler_baton: Pointer); cdecl;
  svn_txdelta_to_svndiff: procedure(output: PSvnStream; pool: PAprPool; out handler: TSvnTxDeltaWindowHandler;
    out handler_baton: Pointer); cdecl;
  svn_txdelta_parse_svndiff: function(handler: TSvnTxDeltaWindowHandler; handler_baton: Pointer;
    error_on_early_close: TSvnBoolean; pool: PAprPool): PSvnStream; cdecl;
  svn_txdelta_read_svndiff_window: function(out window: PSvnTxDeltaWindow; stream: PSvnStream; svndiff_version: Integer;
    pool: PAprPool): PSvnError; cdecl;
  svn_txdelta_skip_svndiff_window: function(afile: PAprFile; svndiff_version: Integer; pool: PAprPool): PSvnError;
    cdecl;

  svn_delta_default_editor: function(pool: PAprPool): PSvnDeltaEditor; cdecl;
  svn_delta_noop_window_handler: function(window: PSvnTxDeltaWindow; baton: Pointer): PSvnError; cdecl;
  svn_delta_get_cancellation_editor: function(cancel_func: TSvnCancelFunc; cancel_baton: Pointer;
    wrapped_editor: PSvnDeltaEditor; wrapped_baton: Pointer; out editor: PSvnDeltaEditor; out edit_baton: Pointer;
    pool: PAprPool): PSvnError; cdecl;
  svn_delta_path_driver: function(editor: PSvnDeltaEditor; edit_baton: Pointer; revision: TSvnRevNum;
    paths: PAprArrayHeader; callback_func: TSvnDeltaPathDriverCbFunc; callback_baton: Pointer;
    pool: PAprPool): PSvnError; cdecl;

//----- svn_delta.h ----------------------------------------------------------------------------------------------------

//----- svn_fs.h -------------------------------------------------------------------------------------------------------

const
  SVN_FS_CONFIG_BDB_TXN_NOSYNC = 'bdb-txn-nosync';
  SVN_FS_CONFIG_BDB_LOG_AUTOREMOVE = 'bdb-log-autoremove';
  SVN_FS_CONFIG_FS_TYPE = 'fs-type';
  SVN_FS_TYPE_BDB = 'bdb';
  SVN_FS_TYPE_FSFS = 'fsfs';
  SVN_FS_TXN_CHECK_OOD = $00001;
  SVN_FS_TXN_CHECK_LOCKS = $00002;

type
  PSvnFS = ^TSvnFS;
  TSvnFS = THandle;
  TSvnFSWarningCallback = procedure(baton: Pointer; err: PSvnError); cdecl;
  TSvnBerkeleyErrorCallback = procedure(errpfs, msg: PChar); cdecl;
  PSvnFSAccess = ^TSvnFSAccess;
  TSvnFSAccess = THandle;
  PSvnFSID = ^TSvnFSID;
  TSvnFSID = THandle;
  PSvnFSTxn = ^TSvnFSTxn;
  TSvnFSTxn = THandle;
  PSvnFSRoot = ^TSvnFSRoot;
  TSvnFSRoot = THandle;
  TSvnFSPathChangeKind = (svnFSPathChangeModify, svnFSPathChangeAdd, svnFSPathChangeDelete, svnFSPathChangeReplace,
    svnFSPathChangeReset);
  PSvnFSPathChange = ^TSvnFSPathChange;
  TSvnFSPathChange = record
    node_rev_id: PSvnFSID;
    change_kind: TSvnFSPathChangeKind;
    text_mod: TSvnBoolean;
    prop_mod: TSvnBoolean;
  end;
  PSvnFSHistory = ^TSvnFSHistory;
  TSvnFSHistory = THandle;
  PSvnFSDirent = ^TSvnFSDirent;
  TSvnFSDirent = record
    name: PChar;
    id: PSvnFSID;
    kind: TSvnNodeKind;
  end;
  TSvnFSGetLocksCallback = function(baton: Pointer; lock: PSvnLock; pool: PAprPool): PSvnError; cdecl;

var
  svn_fs_version: function: PSvnVersion; cdecl;
  svn_fs_initialize: function(pool: PAprPool): PSvnError; cdecl;
  svn_fs_set_warning_func: procedure(fs: PSvnFS; warning: TSvnFSWarningCallback; warning_baton: Pointer); cdecl;
  svn_fs_create: function(out fs_p: PSvnFS; path: PChar; fs_config: PAprHash; pool: PAprPool): PSvnError; cdecl;
  svn_fs_open: function(out fs_p: PSvnFS; path: PChar; config: PAprHash; pool: PAprPool): PSvnError; cdecl;
  svn_fs_type: function(out fs_type: PChar; path: PChar; pool: PAprPool): PSvnError; cdecl;
  svn_fs_path: function(fs: PSvnFS; pool: PAprPool): PChar; cdecl;
  svn_fs_delete_fs: function(path: PChar; pool: PAprPool): PSvnError; cdecl;
  svn_fs_hotcopy: function(src_path, dest_path: PChar; clean: TSvnBoolean; pool: PAprPool): PSvnError; cdecl;
  svn_fs_set_berkeley_errcall: function(fs: PSvnFS; handler: TSvnBerkeleyErrorCallback): PSvnError; cdecl;
  svn_fs_berkeley_recover: function(path: PChar; pool: PAprPool): PSvnError; cdecl;
  svn_fs_berkeley_logfiles: function(out logfiles: PAprArrayHeader; path: PChar; only_unused: TSvnBoolean;
    pool: PAprPool): PSvnError; cdecl;
  svn_fs_new: function(fs_config: PAprHash; pool: PAprPool): PSvnFS; cdecl;
  svn_fs_create_berkeley: function(fs: PSvnFS; path: PChar): PSvnError; cdecl;
  svn_fs_open_berkeley: function(fs: PSvnFS; path: PChar): PSvnError; cdecl;
  svn_fs_berkeley_path: function(fs: PSvnFS; pool: PAprPool): PChar; cdecl;
  svn_fs_delete_berkeley: function(path: PChar; pool: PAprPool): PSvnError; cdecl;
  svn_fs_hotcopy_berkeley: function(src_path, dest_path: PChar; clean_logs: TSvnBoolean; pool: PAprPool): PSvnError;
    cdecl;
  svn_fs_create_access: function(out access_ctx: PSvnFSAccess; username: PChar; pool: PAprPool): PSvnError; cdecl;
  svn_fs_set_access: function(fs: PSvnFS; access_ctx: PSvnFSAccess): PSvnError; cdecl;
  svn_fs_get_access: function(out access_ctx: PSvnFSAccess; fs: PSvnFS): PSvnError; cdecl;
  svn_fs_access_get_username: function(out username: PChar; access_ctx: PSvnFSAccess): PSvnError; cdecl;
  svn_fs_access_add_lock_token: function(access_ctx: PSvnFSAccess; token: PChar): PSvnError; cdecl;
  svn_fs_compare_ids: function(a, b: PSvnFSID): Integer; cdecl;
  svn_fs_check_related: function(id1, id2: PSvnFSID): TSvnBoolean; cdecl;
  svn_fs_parse_id: function(data: PChar; len: TAprSize; pool: PAprPool): PSvnFSID; cdecl;
  svn_fs_unparse_id: function(id: PSvnFSID; pool: PAprPool): PSvnString; cdecl;
  svn_fs_begin_txn2: function(out txn_p: PSvnFSTxn; fs: PSvnFS; rev: TSvnRevNum; flags: Cardinal;
    pool: PAprPool): PSvnError; cdecl;
  svn_fs_begin_txn: function(out txn_p: PSvnFSTxn; fs: PSvnFS; rev: TSvnRevNum; pool: PAprPool): PSvnError; cdecl;
  svn_fs_commit_txn: function(conflict_p: PPChar; out new_rev: TSvnRevNum; txn: PSvnFSTxn; pool: PAprPool): PSvnError;
    cdecl;
  svn_fs_abort_txn: function(txn: PSvnFSTxn; pool: PAprPool): PSvnError; cdecl;
  svn_fs_purge_txn: function(fs: PSvnFS; txn_id: PChar; pool: PAprPool): PSvnError; cdecl;
  svn_fs_txn_name: function(out name_p: PChar; txn: PSvnFSTxn; pool: PAprPool): PSvnError; cdecl;
  svn_fs_txn_base_revision: function(txn: PSvnFSTxn): TSvnRevNum; cdecl;
  svn_fs_open_txn: function(out txn: PSvnFSTxn; fs: PSvnFS; name: PChar; pool: PAprPool): PSvnError; cdecl;
  svn_fs_list_transactions: function(out names_p: PAprArrayHeader; fs: PSvnFS; pool: PAprPool): PSvnError; cdecl;
  svn_fs_txn_prop: function(out value_p: PSvnString; txn: PSvnFSTxn; propname: PChar; pool: PAprPool): PSvnError; cdecl;
  svn_fs_txn_proplist: function(out table_p: PAprHash; txn: PSvnFSTxn; pool: PAprPool): PSvnError; cdecl;
  svn_fs_change_txn_prop: function(txn: PSvnFSTxn; name: PChar; value: PSvnString; pool: PAprPool): PSvnError; cdecl;
  svn_fs_revision_root: function(out root_p: PSvnFSRoot; fs: PSvnFS; rev: TSvnRevNum; pool: PAprPool): PSvnError; cdecl;
  svn_fs_txn_root: function(out root_p: PSvnFSRoot; txn: PSvnFSTxn; pool: PAprPool): PSvnError; cdecl;
  svn_fs_close_root: procedure(root: PSvnFSRoot); cdecl;
  svn_fs_root_fs: function(root: PSvnFSRoot): PSvnFS; cdecl;
  svn_fs_is_txn_root: function(root: PSvnFSRoot): TSvnBoolean; cdecl;
  svn_fs_is_revision_root: function(root: PSvnFSRoot): TSvnBoolean; cdecl;
  svn_fs_txn_root_name: function(root: PSvnFSRoot; pool: PAprPool): PChar; cdecl;
  svn_fs_revision_root_revision: function(root: PSvnFSRoot): TSvnRevNum; cdecl;
  svn_fs_paths_changed: function(out changed_paths_p: PAprHash; root: PSvnFSRoot; pool: PAprPool): PSvnError; cdecl;
  svn_fs_check_path: function(out kind_p: TSvnNodeKind; root: PSvnFSRoot; path: PChar; pool: PAprPool): PSvnError;
    cdecl;
  svn_fs_node_history: function(out history_p: PSvnFSHistory; root: PSvnFSRoot; path: PChar; pool: PAprPool): PSvnError;
    cdecl;
  svn_fs_history_prev: function(out prev_history_p: PSvnFSHistory; history: PSvnFSHistory; cross_copies: TSvnBoolean;
    pool: PAprPool): PSvnError; cdecl;
  svn_fs_history_location: function(out path: PChar; out revision: TSvnRevNum; history: PSvnFSHistory;
    pool: PAprPool): PSvnError; cdecl;
  svn_fs_is_dir: function(out is_dir: TSvnBoolean; root: PSvnFSRoot; path: PChar; pool: PAprPool): PSvnError; cdecl;
  svn_fs_is_file: function(out is_file: TSvnBoolean; root: PSvnFSRoot; path: PChar; pool: PAprPool): PSvnError; cdecl;
  svn_fs_node_id: function(out id_p: PSvnFSID; root: PSvnFSRoot; path: PChar; pool: PAprPool): PSvnError; cdecl;
  svn_fs_node_created_rev: function(out revision: TSvnRevNum; root: PSvnFSRoot; path: PChar; pool: PAprPool): PSvnError;
    cdecl;
  svn_fs_node_created_path: function(out created_path: PChar; root: PSvnFSRoot; path: PChar; pool: PAprPool): PSvnError;
    cdecl;
  svn_fs_node_prop: function(out value_p: PSvnString; root: PSvnFSRoot; path, propname: PChar;
    pool: PAprPool): PSvnError; cdecl;
  svn_fs_node_proplist: function(out table_p: PAprHash; root: PSvnFSRoot; path: PChar; pool: PAprPool): PSvnError;
    cdecl;
  svn_fs_change_node_prop: function(root: PSvnFSRoot; path, name: PChar; value: PSvnString; pool: PAprPool): PSvnError;
    cdecl;
  svn_fs_props_changed: function(out changed_p: TSvnBoolean; root1: PSvnFSRoot; path1: PChar; root2: PSvnFSRoot;
    path2: PChar; pool: PAprPool): PSvnError; cdecl;
  svn_fs_copied_from: function(out rev_p: TSvnRevNum; out path_p: PChar; root: PSvnFSRoot; path: PChar;
    pool: PAprPool): PSvnError; cdecl;
  svn_fs_closest_copy: function(out root_p: PSvnFSRoot; out path_p: PChar; root: PSvnFSRoot; path: PChar;
    pool: PAprPool): PSvnError; cdecl;
  svn_fs_merge: function(conflict_p: PPChar; source_root: PSvnFSRoot; source_path: PChar; target_root: PSvnFSRoot;
    target_path: PChar; ancestor_root: PSvnFSRoot; ancestor_path: PChar; pool: PAprPool): PSvnError; cdecl;
  svn_fs_dir_entries: function(out entries_p: PAprHash; root: PSvnFSRoot; path: PChar; pool: PAprPool): PSvnError;
    cdecl;
  svn_fs_make_dir: function(root: PSvnFSRoot; path: PChar; pool: PAprPool): PSvnError; cdecl;
  svn_fs_delete: function(root: PSvnFSRoot; path: PChar; pool: PAprPool): PSvnError; cdecl;
  svn_fs_copy: function(from_root: PSvnFSRoot; from_path: PChar; to_root: PSvnFSRoot; to_path: PChar;
    pool: PAprPool): PSvnError; cdecl;
  svn_fs_revision_link: function(from_root, to_root: PSvnFSRoot; path: PChar; pool: PAprPool): PSvnError; cdecl;
  svn_fs_file_length: function(out length_p: TSvnFileSize; root: PSvnFSRoot; path: PChar; pool: PAprPool): PSvnError;
    cdecl;
  svn_fs_file_md5_checksum: function(digest: PByte; root: PSvnFSRoot; path: PChar; pool: PAprPool): PSvnError; cdecl;
  svn_fs_file_contents: function(out contents: PSvnStream; root: PSvnFSRoot; path: PChar; pool: PAprPool): PSvnError;
    cdecl;
  svn_fs_make_file: function(root: PSvnFSRoot; path: PChar; pool: PAprPool): PSvnError; cdecl;
  svn_fs_apply_textdelta: function(out contents_p: TSvnTxDeltaWindowHandler; out contents_baton_p: Pointer;
    root: PSvnFSRoot; path, base_checksum, result_checksum: PChar; pool: PAprPool): PSvnError; cdecl;
  svn_fs_apply_text: function(out contents_p: PSvnStream; root: PSvnFSRoot; path, result_checksum: PChar;
    pool: PAprPool): PSvnError; cdecl;
  svn_fs_contents_changed: function(out changed_p: TSvnBoolean; root1: PSvnFSRoot; path1: PChar; root2: PSvnFSRoot;
    path2: PChar; pool: PAprPool): PSvnError; cdecl;
  svn_fs_youngest_rev: function(out youngest_p: TSvnRevNum; fs: PSvnFS; pool: PAprPool): PSvnError; cdecl;
  svn_fs_deltify_revision: function(fs: PSvnFS; revision: TSvnRevNum; pool: PAprPool): PSvnError; cdecl;
  svn_fs_revision_prop: function(out value_p: PSvnString; fs: PSvnFS; rev: TSvnRevNum; propname: PChar;
    pool: PAprPool): PSvnError; cdecl;
  svn_fs_revision_proplist: function(out table_p: PAprHash; fs: PSvnFS; rev: TSvnRevNum; pool: PAprPool): PSvnError;
    cdecl;
  svn_fs_change_rev_prop: function(fs: PSvnFS; rev: TSvnRevNum; name: PChar; value: PSvnString;
    pool: PAprPool): PSvnError; cdecl;
  svn_fs_get_file_delta_stream: function(out stream_p: PSvnTxDeltaStream; source_root: PSvnFSRoot; source_path: PChar;
    target_root: PSvnFSRoot; target_path: PChar; pool: PAprPool): PSvnError; cdecl;
  svn_fs_get_uuid: function(fs: PSvnFS; out uuid: PChar; pool: PAprPool): PSvnError; cdecl;
  svn_fs_set_uuid: function(fs: PSvnFS; uuid: PChar; pool: PAprPool): PSvnError; cdecl;
  svn_fs_lock: function(out lock: PSvnLock; fs: PSvnFS; path, token, comment: PChar; is_dav_comment: TSvnBoolean;
    expiration_date: TAprTime; current_rev: TSvnRevNum; steal_lock: TSvnBoolean; pool: PAprPool): PSvnError; cdecl;
  svn_fs_generate_lock_token: function(out token: PChar; fs: PSvnFS; pool: PAprPool): PSvnError; cdecl;
  svn_fs_unlock: function(fs: PSvnFS; path, token: PChar; break_lock: TSvnBoolean; pool: PAprPool): PSvnError; cdecl;
  svn_fs_get_lock: function(out lock: PSvnLock; fs: PSvnFS; path: PChar; pool: PAprPool): PSvnError; cdecl;
  svn_fs_get_locks: function(fs: PSvnFS; path: PChar; get_locks_func: TSvnFSGetLocksCallback; get_locks_baton: Pointer;
    pool: PAprPool): PSvnError; cdecl;
  svn_fs_print_modules: function(output: PSvnStringBuf; pool: PAprPool): PSvnError; cdecl;

//----- svn_fs.h -------------------------------------------------------------------------------------------------------

//----- svn_repos.h ----------------------------------------------------------------------------------------------------

const
  SVN_REPOS_DUMPFILE_MAGIC_HEADER = 'SVN-fs-dump-format-version';
  SVN_REPOS_DUMPFILE_FORMAT_VERSION = 3;
  SVN_REPOS_DUMPFILE_UUID = 'UUID';
  SVN_REPOS_DUMPFILE_CONTENT_LENGTH = 'Content-length';
  SVN_REPOS_DUMPFILE_REVISION_NUMBER = 'Revision-number';
  SVN_REPOS_DUMPFILE_NODE_PATH = 'Node-path';
  SVN_REPOS_DUMPFILE_NODE_KIND = 'Node-kind';
  SVN_REPOS_DUMPFILE_NODE_ACTION = 'Node-action';
  SVN_REPOS_DUMPFILE_NODE_COPYFROM_PATH = 'Node-copyfrom-path';
  SVN_REPOS_DUMPFILE_NODE_COPYFROM_REV = 'Node-copyfrom-rev';
  SVN_REPOS_DUMPFILE_TEXT_COPY_SOURCE_CHECKSUM = 'Text-copy-source-md5';
  SVN_REPOS_DUMPFILE_TEXT_CONTENT_CHECKSUM = 'Text-content-md5';
  SVN_REPOS_DUMPFILE_PROP_CONTENT_LENGTH = 'Prop-content-length';
  SVN_REPOS_DUMPFILE_TEXT_CONTENT_LENGTH = 'Text-content-length';
  SVN_REPOS_DUMPFILE_PROP_DELTA = 'Prop-delta';
  SVN_REPOS_DUMPFILE_TEXT_DELTA = 'Text-delta';

type
  TSvnReposAuthzFunc = function(out allowed: TSvnBoolean; root: PSvnFSRoot; path: PChar; baton: Pointer;
    pool: PAprPool): PSvnError; cdecl;
  TSvnReposAuthzAccess = (svnAuthzNone = 0, svnAuthzRead = 1, svnAuthzWrite = 2, svnAuthzRecursive = 4);
  TSvnReposAuthzCallback = function(required: TSvnReposAuthzAccess; out allowed: TSvnBoolean; root: PSvnFSRoot;
    path: PChar; baton: Pointer; pool: PAprPool): PSvnError; cdecl;
  TSvnReposFileRevHandler = function(baton: Pointer; path: PChar; rev: TSvnRevNum; rev_props: PAprHash;
    out delta_handler: TSvnTxDeltaWindowHandler; out delta_baton: Pointer; prop_diffs: PAprArrayHeader;
    pool: PAprPool): PSvnError; cdecl;
  PSvnRepos = ^TSvnRepos;
  TSvnRepos = THandle;
  TSvnReposRecoverCallback = function(baton: Pointer): PSvnError; cdecl;
  TSvnReposHistoryFunc = function(baton: Pointer; path: PChar; revision: TSvnRevNum; pool: PAprPool): PSvnError; cdecl;
  PSvnReposNode = ^TSvnReposNode;
  TSvnReposNode = record
    kind: TSvnNodeKind;
    action: Char;
    text_mod: TSvnBoolean;
    prop_mod: TSvnBoolean;
    name: PChar;
    copyfrom_rev: TSvnRevNum;
    copyfrom_path: PChar;
    sibling: PSvnReposNode;
    child: PSvnReposNode;
    parent: PSvnReposNode;
  end;
  TSvnNodeAction = (svnNodeActionChange, svnNodeActionAdd, svnNodeActionDelete, svnNodeActionReplace);
  TSvnReposLoadUuid = (svnReposLoadUuidDefault, svnReposLoadUuidIgnore, svnReposLoadUuidForce);
  PSvnReposParseFns2 = ^TSvnReposParseFns2;
  TSvnReposParseFns2 = record
    new_revision_record: function(out revision_baton: Pointer; headers: PAprHash; parse_baton: Pointer;
      pool: PAprPool): PSvnError; cdecl;
    uuid_record: function(uuid: PChar; parse_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
    new_node_record: function(out node_baton: Pointer; headers: PAprHash; revision_baton: Pointer;
      pool: PAprPool): PSvnError; cdecl;
    set_revision_property: function(revision_baton: Pointer; name: PChar; value: PSvnString): PSvnError; cdecl;
    set_node_property: function(node_baton: Pointer; name: PChar; value: PSvnString): PSvnError; cdecl;
    delete_node_property: function(node_baton: Pointer;  name: PChar): PSvnError; cdecl;
    remove_node_props: function(node_baton: Pointer): PSvnError; cdecl;
    set_fulltext: function(stream: PPSvnStream): PSvnError; cdecl;
    apply_textdelta: function(out handler: TSvnTxDeltaWindowHandler; out handler_baton: Pointer;
      node_baton: Pointer): PSvnError; cdecl;
    close_node: function(node_baton: Pointer): PSvnError; cdecl;
    close_revision: function(revision_baton: Pointer): PSvnError; cdecl;
  end;
  PSvnReposParseFns = ^TSvnReposParseFns;
  TSvnReposParseFns = record
    new_revision_record: function(out revision_baton: Pointer; headers: PAprHash; parse_baton: Pointer;
      pool: PAprPool): PSvnError; cdecl;
    uuid_record: function(uuid: PChar; parse_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
    new_node_record: function(out node_baton: Pointer; headers: PAprHash; revision_baton: Pointer;
      pool: PAprPool): PSvnError; cdecl;
    set_revision_property: function(revision_baton: Pointer; name: PChar; value: PSvnString): PSvnError; cdecl;
    set_node_property: function(node_baton: Pointer; name: PChar; value: PSvnString): PSvnError; cdecl;
    remove_node_props: function(node_baton: Pointer): PSvnError; cdecl;
    set_fulltext: function(out stream: PSvnStream; node_baton: Pointer): PSvnError; cdecl;
    close_node: function(node_baton: Pointer): PSvnError; cdecl;
    close_revision: function(revision_baton: Pointer): PSvnError; cdecl;
  end;
  PSvnAuthz = ^TSvnAuthz;
  TSvnAuthz = THandle;

var
  svn_repos_version: function: PSvnVersion; cdecl;
  svn_repos_find_root_path: function(path: PChar; pool: PAprPool): PChar; cdecl;
  svn_repos_open: function(out repos_p: PSvnRepos; path: PChar; pool: PAprPool): PSvnError; cdecl;
  svn_repos_create: function(out repos_p: PSvnRepos; path, unused_1, unused_2: PChar; config, fs_config: PAprHash;
    pool: PAprPool): PSvnError; cdecl;
  svn_repos_delete: function(path: PChar; pool: PAprPool): PSvnError; cdecl;
  svn_repos_fs: function(repos: PSvnRepos): PSvnFS; cdecl;
  svn_repos_hotcopy: function(src_path, dst_path: PChar; clean_logs: TSvnBoolean; pool: PAprPool): PSvnError; cdecl;
  svn_repos_recover: function(path: PChar; pool: PAprPool): PSvnError; cdecl;
  svn_repos_recover2: function(path: PChar; nonblocking: TSvnBoolean; start_callback: TSvnReposRecoverCallback;
    start_callback_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
  svn_repos_db_logfiles: function(out logfiles: PAprArrayHeader; path: PChar; only_unused: TSvnBoolean;
    pool: PAprPool): PSvnError; cdecl;
  svn_repos_path: function(repos: PSvnRepos; pool: PAprPool): PChar; cdecl;
  svn_repos_db_env: function(repos: PSvnRepos; pool: PAprPool): PChar; cdecl;
  svn_repos_conf_dir: function(repos: PSvnRepos; pool: PAprPool): PChar; cdecl;
  svn_repos_svnserve_conf: function(repos: PSvnRepos; pool: PAprPool): PChar; cdecl;
  svn_repos_lock_dir: function(repos: PSvnRepos; pool: PAprPool): PChar; cdecl;
  svn_repos_db_lockfile: function(repos: PSvnRepos; pool: PAprPool): PChar; cdecl;
  svn_repos_db_logs_lockfile: function(repos: PSvnRepos; pool: PAprPool): PChar; cdecl;
  svn_repos_hook_dir: function(repos: PSvnRepos; pool: PAprPool): PChar; cdecl;
  svn_repos_start_commit_hook: function(repos: PSvnRepos; pool: PAprPool): PChar; cdecl;
  svn_repos_pre_commit_hook: function(repos: PSvnRepos; pool: PAprPool): PChar; cdecl;
  svn_repos_post_commit_hook: function(repos: PSvnRepos; pool: PAprPool): PChar; cdecl;
  svn_repos_pre_revprop_change_hook: function(repos: PSvnRepos; pool: PAprPool): PChar; cdecl;
  svn_repos_post_revprop_change_hook: function(repos: PSvnRepos; pool: PAprPool): PChar; cdecl;
  svn_repos_pre_lock_hook: function(repos: PSvnRepos; pool: PAprPool): PChar; cdecl;
  svn_repos_post_lock_hook: function(repos: PSvnRepos; pool: PAprPool): PChar; cdecl;
  svn_repos_pre_unlock_hook: function(repos: PSvnRepos; pool: PAprPool): PChar; cdecl;
  svn_repos_post_unlock_hook: function(repos: PSvnRepos; pool: PAprPool): PChar; cdecl;
  svn_repos_begin_report: function(out report_baton: Pointer; revnum: TSvnRevNum; username: PChar; repos: PSvnRepos;
    fs_base, target, tgt_path: PChar; text_deltas, recurse, ignore_ancestry: TSvnBoolean; editor: PSvnDeltaEditor;
    edit_baton: Pointer; authz_read_func: TSvnReposAuthzFunc; authz_read_baton: Pointer; pool: PAprPool): PSvnError;
    cdecl;
  svn_repos_set_path2: function(report_baton: Pointer; path: PChar; revision: TSvnRevNum; start_empty: TSvnBoolean;
    lock_token: PChar; pool: PAprPool): PSvnError; cdecl;
  svn_repos_set_path: function(report_baton: Pointer; path: PChar; revision: TSvnRevNum; start_empty: TSvnBoolean;
    pool: PAprPool): PSvnError; cdecl;
  svn_repos_link_path2: function(report_baton: Pointer; path, link_path: PChar; revision: TSvnRevNum;
    start_empty: TSvnBoolean; lock_token: PChar; pool: PAprPool): PSvnError; cdecl;
  svn_repos_link_path: function(report_baton: Pointer; path, link_path: PChar; revision: TSvnRevNum;
    start_empty: TSvnBoolean; pool: PAprPool): PSvnError; cdecl;
  svn_repos_delete_path: function(report_baton: Pointer; path: PChar; pool: PAprPool): PSvnError; cdecl;
  svn_repos_finish_report: function(report_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
  svn_repos_abort_report: function(report_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
  svn_repos_dir_delta: function(src_root: PSvnFSRoot; src_parent_dir, src_entry: PChar; tgt_root: PSvnFSRoot;
    tgt_path: PChar; editor: PSvnDeltaEditor; edit_baton: Pointer; authz_read_func: TSvnReposAuthzFunc;
    authz_read_baton: Pointer; text_deltas, recurse, entry_props, ignore_ancestry: TSvnBoolean;
    pool: PAprPool): PSvnError; cdecl;
  svn_repos_replay: function(root: PSvnFSRoot; editor: PSvnDeltaEditor; edit_baton: Pointer;
    pool: PAprPool): PSvnError; cdecl;
  svn_repos_get_commit_editor3: function(out editor: PSvnDeltaEditor; out edit_baton: Pointer; repos: PSvnRepos;
    txn: PSvnFSTxn; repos_url, base_path, user, log_msg: PChar; callback: TSvnCommitCallback; callback_baton: Pointer;
    authz_callback: TSvnReposAuthzCallback; authz_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
  svn_repos_get_commit_editor2: function(out editor: PSvnDeltaEditor; out edit_baton: Pointer; repos: PSvnRepos;
    txn: PSvnFSTxn; repos_url, base_path, user, log_msg: PChar; callback: TSvnCommitCallback; callback_baton: Pointer;
    pool: PAprPool): PSvnError; cdecl;
  svn_repos_get_commit_editor: function(out editor: PSvnDeltaEditor; out edit_baton: Pointer; repos: PSvnRepos;
    repos_url, base_path, user, log_msg: PChar; callback: TSvnCommitCallback; callback_baton: Pointer;
    pool: PAprPool): PSvnError; cdecl;
  svn_repos_dated_revision: function(out revision: TSvnRevNum; repos: PSvnRepos; tm: TAprTime;
    pool: PAprPool): PSvnError; cdecl;
  svn_repos_get_committed_info: function(out committed_rev: TSvnRevNum; out committed_date, last_author: PChar;
    root: PSvnFSRoot; path: PChar; pool: PAprPool): PSvnError; cdecl;
  svn_repos_stat: function(out dirent: PSvnDirEnt; root: PSvnFSRoot; path: PChar; pool: PAprPool): PSvnError; cdecl;
  svn_repos_history2: function(fs: PSvnFS; path: PChar; history_func: TSvnReposHistoryFunc; history_baton: Pointer;
    authz_read_func: TSvnReposAuthzFunc; authz_read_baton: Pointer; rev_start, rev_end: TSvnRevNum;
    cross_copies: TSvnBoolean; pool: PAprPool): PSvnError; cdecl;
  svn_repos_history: function(fs: PSvnFS; path: PChar; history_func: TSvnReposHistoryFunc; history_baton: Pointer;
    rev_start, rev_end: TSvnRevNum; cross_copies: TSvnBoolean; pool: PAprPool): PSvnError; cdecl;
  svn_repos_trace_node_locations: function(fs: PSvnFS; out locations: PAprHash; fs_path: PChar;
    peg_revision: TSvnRevNum; location_revisions: PAprArrayHeader; authz_read_func: TSvnReposAuthzFunc;
    authz_read_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
  svn_repos_get_logs3: function(repos: PSvnRepos; paths: PAprArrayHeader; rev_start, rev_end: TSvnRevNum;
    limit: Integer; discover_changed_paths, strict_node_history: TSvnBoolean; authz_read_func: TSvnReposAuthzFunc;
    authz_read_baton: Pointer; receiver: TSvnLogMessageReceiver; receiver_baton: Pointer; pool: PAprPool): PSvnError;
    cdecl;
  svn_repos_get_logs2: function(repos: PSvnRepos; paths: PAprArrayHeader; rev_start, rev_end: TSvnRevNum;
    discover_changed_paths, strict_node_history: TSvnBoolean; authz_read_func: TSvnReposAuthzFunc;
    authz_read_baton: Pointer; receiver: TSvnLogMessageReceiver; receiver_baton: Pointer; pool: PAprPool): PSvnError;
    cdecl;
  svn_repos_get_logs: function(repos: PSvnRepos; paths: PAprArrayHeader; rev_start, rev_end: TSvnRevNum;
    discover_changed_paths, strict_node_history: TSvnBoolean; receiver: TSvnLogMessageReceiver; receiver_baton: Pointer;
    pool: PAprPool): PSvnError; cdecl;
  svn_repos_get_file_revs: function(repos: PSvnRepos; path: PChar; rev_start, rev_end: TSvnRevNum;
    authz_read_func: TSvnReposAuthzFunc; authz_read_baton: Pointer; handler: TSvnReposFileRevHandler;
    handler_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
  svn_repos_fs_commit_txn: function(out conflict_p: PChar; repos: PSvnRepos; out new_rev: TSvnRevNum; txn: PSvnFSTxn;
    pool: PAprPool): PSvnError; cdecl;
  svn_repos_fs_begin_txn_for_commit: function(out txn_p: PSvnFSTxn; repos: PSvnRepos; rev: TSvnRevNum;
    author, log_msg: PChar; pool: PAprPool): PSvnError; cdecl;
  svn_repos_fs_begin_txn_for_update: function(out txn_p: PSvnFSTxn; repos: PSvnRepos; rev: TSvnRevNum; author: PChar;
    pool: PAprPool): PSvnError; cdecl;
  svn_repos_fs_lock: function(out lock: PSvnLock; repos: PSvnRepos; path, token, comment: PChar;
    is_dav_comment: TSvnBoolean; expiration_date: TAprTime; current_rev: TSvnRevNum; steal_lock: TSvnBoolean;
    pool: PAprPool): PSvnError; cdecl;
  svn_repos_fs_unlock: function(repos: PSvnRepos; path, token: PChar; break_lock: TSvnBoolean;
    pool: PAprPool): PSvnError; cdecl;
  svn_repos_fs_get_locks: function(out locks: PAprHash; repos: PSvnRepos; path: PChar;
    authz_read_func: TSvnReposAuthzFunc; authz_read_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
  svn_repos_fs_change_rev_prop2: function(repos: PSvnRepos; rev: TSvnRevNum; author, name: PChar; new_value: PSvnString;
    authz_read_func: TSvnReposAuthzFunc; authz_read_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
  svn_repos_fs_change_rev_prop: function(repos: PSvnRepos; rev: TSvnRevNum; author, name: PChar; new_value: PSvnString;
    pool: PAprPool): PSvnError; cdecl;
  svn_repos_fs_revision_prop: function(out value_p: PSvnString; repos: PSvnRepos; rev: TSvnRevNum; propname: PChar;
    authz_read_func: TSvnReposAuthzFunc; authz_read_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
  svn_repos_fs_revision_proplist: function(out table_p: PAprHash; repos: PSvnRepos; rev: TSvnRevNum;
    authz_read_func: TSvnReposAuthzFunc; authz_read_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
  svn_repos_fs_change_node_prop: function(root: PSvnFSRoot; path, name: PChar; value: PSvnString;
    pool: PAprPool): PSvnError; cdecl;
  svn_repos_fs_change_txn_prop: function(txn: PSvnFSTxn; name: PChar; value: PSvnString; pool: PAprPool): PSvnError;
    cdecl;
  svn_repos_node_editor: function(out editor: PSvnDeltaEditor; out edit_baton: Pointer; repos: PSvnRepos;
    base_root, root: PSvnFSRoot; node_pool, pool: PAprPool): PSvnError; cdecl;
  svn_repos_node_from_baton: function(edit_baton: Pointer): PSvnReposNode; cdecl;
  svn_repos_dump_fs2: function(repos: PSvnRepos; dumpstream, feedback_stream: PSvnStream;
    start_rev, end_rev: TSvnRevNum; incremental, use_deltas: TSvnBoolean; cancel_func: TSvnCancelFunc;
    cancel_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
  svn_repos_dump_fs: function(repos: PSvnRepos; dumpstream, feedback_stream: PSvnStream; start_rev, end_rev: TSvnRevNum;
    incremental: TSvnBoolean; cancel_func: TSvnCancelFunc; cancel_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
  svn_repos_load_fs2: function(repos: PSvnRepos; dumpstream, feedback_stream: PSvnStream;
    uuid_action: TSvnReposLoadUuid; parent_dir: pChar; use_pre_commit_hook, use_post_commit_hook: TSvnBoolean;
    cancel_func: TSvnCancelFunc; cancel_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
  svn_repos_load_fs: function(repos: PSvnRepos; dumpstream, feedback_stream: PSvnStream; uuid_action: TSvnReposLoadUuid;
    parent_dir: PChar; cancel_func: TSvnCancelFunc; cancel_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
  svn_repos_parse_dumpstream2: function(stream: PSvnStream; parse_fns: PSvnReposParseFns2; parse_baton: Pointer;
    cancel_func: TSvnCancelFunc; cancel_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
  svn_repos_get_fs_build_parser2: function(out parser: PSvnReposParseFns2; out parse_baton: Pointer; repos: PSvnRepos;
    use_history: TSvnBoolean; uuid_action: TSvnReposLoadUuid; outstream: PSvnStream; parent_dir: PChar;
    pool: PAprPool): PSvnError; cdecl;
  svn_repos_parse_dumpstream: function(stream: PSvnStream; parse_fns: PSvnReposParseFns; parse_baton: Pointer;
    cancel_func: TSvnCancelFunc; cancel_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
  svn_repos_get_fs_build_parser: function(out parser: PSvnReposParseFns; out parse_baton: Pointer; repos: PSvnRepos;
    use_history: TSvnBoolean; uuid_action: TSvnReposLoadUuid; outstream: PSvnStream; parent_dir: PChar;
    pool: PAprPool): PSvnError; cdecl;
  svn_repos_authz_read: function(out authz_p: PSvnAuthz; afile: PChar; must_exist: TSvnBoolean;
    pool: PAprPool): PSvnError; cdecl;
  svn_repos_authz_check_access: function(authz: PSvnAuthz; repos_name, path, user: PChar;
    required_access: TSvnReposAuthzAccess; out access_granted: TSvnBoolean; pool: PAprPool): PSvnError; cdecl;

//----- svn_repos.h ----------------------------------------------------------------------------------------------------

//----- svn_opt.h ------------------------------------------------------------------------------------------------------

const
  SVN_OPT_MAX_ALIASES = 3;
  SVN_OPT_MAX_OPTIONS = 50;
  SVN_OPT_FIRST_LONGOPT_ID = 256;

type
  TSvnOptSubcommand = function(os: PAprGetOpt; baton: Pointer; pool: PAprPool): PSvnError; cdecl;
  PSvnOptSubcommandDesc = ^TSvnOptSubcommandDesc;
  TSvnOptSubcommandDesc = record
    name: PChar;
    cmd_func: TSvnOptSubcommand;
    aliases: array[0..SVN_OPT_MAX_ALIASES - 1] of PChar;
    help: PChar;
    valid_options: array[0..SVN_OPT_MAX_OPTIONS - 1] of Integer;
  end;
  TSvnOptRevisionKind = (svnOptRevisionUnspecified, svnOptRevisionNumber, svnOptRevisionDate,
    svnOptRevisionCommitted, svnOptRevisionPrevious, svnOptRevisionBase, svnOptRevisionWorking,
    svnOptRevisionHead);
  PSvnOptRevisionValue = ^TSvnOptRevisionValue;
  TSvnOptRevisionValue = record
    case Boolean of
      False: (number: TSvnRevNum);
      True: (date: TAprTime);
  end;
  PSvnOptRevision = ^TSvnOptRevision;
  TSvnOptRevision = record
    Kind: TSvnOptRevisionKind;
    Value: TSvnOptRevisionValue;
  end;

var
  svn_opt_get_canonical_subcommand: function(table: PSvnOptSubcommandDesc; cmd_name: PChar): PSvnOptSubcommandDesc;
    cdecl;
  svn_opt_get_option_from_code: function(code: Integer; option_table: PAprGetOptOption): PAprGetOptOption; cdecl;
  svn_opt_subcommand_takes_option: function(command: PSvnOptSubcommandDesc; option_code: Integer): TSvnBoolean; cdecl;
  svn_opt_print_generic_help: procedure(header: PChar; cmd_table: PSvnOptSubcommandDesc; opt_table: PAprGetOptOption;
    footer: PChar; pool: PAprPool; stream: THandle); cdecl;
  svn_opt_format_option: procedure(str: PPChar; opt: PAprGetOptOption; doc: TSvnBoolean; pool: PAprPool); cdecl;
  svn_opt_subcommand_help: procedure(subcommand: PChar; table: PSvnOptSubcommandDesc; options_table: PAprGetOptOption;
    pool: PAprPool); cdecl;
  svn_opt_parse_revision: function(start_revision, end_revision: PSvnOptRevision; arg: PChar; pool: PAprPool): Integer;
    cdecl;
  svn_opt_args_to_target_array2: function(out targets_p: PAprArrayHeader; os: PAprGetOpt;
    known_targets: PAprArrayHeader; pool: PAprPool): PSvnError; cdecl;
  svn_opt_args_to_target_array: function(out targets_p: PAprArrayHeader; os: PAprGetOpt; known_targets: PAprArrayHeader;
    start_revision, end_revision: PSvnOptRevision; extract_revisions: TSvnBoolean; pool: PAprPool): PSvnError; cdecl;
  svn_opt_push_implicit_dot_target: procedure(targets: PAprArrayHeader; pool: PAprPool); cdecl;
  svn_opt_parse_num_args: function(out args_p: PAprArrayHeader; os: PAprGetOpt; num_args: Integer;
    pool: PAprPool): PSvnError; cdecl;
  svn_opt_parse_all_args: function(out args_p: PAprArrayHeader; os: PAprGetOpt; pool: PAprPool): PSvnError; cdecl;
  svn_opt_parse_path: function(out rev: TSvnOptRevision; out truepath: PChar; path: PChar; pool: PAprPool): PSvnError;
    cdecl;
  svn_opt_print_help: function(os: PAprGetOpt; pgm_name: PChar; print_version, quiet: TSvnBoolean;
    version_footer, header: PChar; cmd_table: PSvnOptSubcommandDesc; option_table: PAprGetOptOption; footer: PChar;
    pool: PAprPool): PSvnError; cdecl;

//----- svn_opt.h ------------------------------------------------------------------------------------------------------

//----- svn_auth.h -----------------------------------------------------------------------------------------------------

const
  SVN_AUTH_CRED_SIMPLE = 'svn.simple';
  SVN_AUTH_CRED_USERNAME = 'svn.username';
  SVN_AUTH_CRED_SSL_CLIENT_CERT = 'svn.ssl.client-cert';
  SVN_AUTH_CRED_SSL_CLIENT_CERT_PW = 'svn.ssl.client-passphrase';
  SVN_AUTH_CRED_SSL_SERVER_TRUST = 'svn.ssl.server';

  SVN_AUTH_SSL_NOTYETVALID = $00000001;
  SVN_AUTH_SSL_EXPIRED     = $00000002;
  SVN_AUTH_SSL_CNMISMATCH  = $00000004;
  SVN_AUTH_SSL_UNKNOWNCA   = $00000008;
  SVN_AUTH_SSL_OTHER       = $40000000;

  SVN_AUTH_PARAM_PREFIX = 'svn:auth:';
  SVN_AUTH_PARAM_DEFAULT_USERNAME                 = SVN_AUTH_PARAM_PREFIX + 'username';
  SVN_AUTH_PARAM_DEFAULT_PASSWORD                 = SVN_AUTH_PARAM_PREFIX + 'password';
  SVN_AUTH_PARAM_NON_INTERACTIVE                  = SVN_AUTH_PARAM_PREFIX + 'non-interactive';
  SVN_AUTH_PARAM_DONT_STORE_PASSWORDS             = SVN_AUTH_PARAM_PREFIX + 'dont-store-passwords';
  SVN_AUTH_PARAM_NO_AUTH_CACHE                    = SVN_AUTH_PARAM_PREFIX + 'no-auth-cache';
  SVN_AUTH_PARAM_SSL_SERVER_FAILURES              = SVN_AUTH_PARAM_PREFIX + 'ssl:failures';
  SVN_AUTH_PARAM_SSL_SERVER_CERT_INFO             = SVN_AUTH_PARAM_PREFIX + 'ssl:cert-info';
  SVN_AUTH_PARAM_CONFIG                           = SVN_AUTH_PARAM_PREFIX + 'config';
  SVN_AUTH_PARAM_SERVER_GROUP                     = SVN_AUTH_PARAM_PREFIX + 'server-group';
  SVN_AUTH_PARAM_CONFIG_DIR                       = SVN_AUTH_PARAM_PREFIX + 'config-dir';

  SVN_RA_ABI_VERSION = 2;

type
  PSvnAuthBaton = ^TSvnAuthBaton;
  TSvnAuthBaton = THandle;
  PSvnAuthIterState = ^TSvnAuthIterState;
  TSvnAuthIterState = THandle;

  PSvnAuthProvider = ^TSvnAuthProvider;
  TSvnAuthProvider = record
    cred_kind: PChar;
    first_credentials: function(out credentials, iter_baton: Pointer; provider_baton: Pointer; parameters: PAprHash;
      realmstring: PChar; pool: PAprPool): PSvnError; cdecl;
    next_credentials: function(out credentials: Pointer; iter_baton, provider_baton: Pointer; parameters: PAprHash;
      realmstring: PChar; pool: PAprPool): PSvnError; cdecl;
    save_credentials: function(out saved: TSvnBoolean; credentials, provider_baton: Pointer; parameters: PAprHash;
      realmstring: PChar; pool: PAprPool): PSvnError; cdecl;
  end;
  PPSvnAuthProviderObject = ^PSvnAuthProviderObject;
  PSvnAuthProviderObject = ^TSvnAuthProviderObject;
  TSvnAuthProviderObject = record
    vtable: PSvnAuthProvider;
    provider_baton: Pointer;
  end;
  PSvnAuthCredSimple = ^TSvnAuthCredSimple;
  TSvnAuthCredSimple = record
    username: PChar;
    password: PChar;
    may_save: TSvnBoolean;
  end;
  PSvnAuthCredUsername = ^TSvnAuthCredUsername;
  TSvnAuthCredUsername = record
    username: PChar;
    may_save: TSvnBoolean;
  end;
  PSvnAuthCredSSLClientCert = ^TSvnAuthCredSSLClientCert;
  TSvnAuthCredSSLClientCert = record
    cert_file: PChar;
    may_save: TSvnBoolean;
  end;
  PSvnAuthCredSSLClientCertPw = ^TSvnAuthCredSSLClientCertPw;
  TSvnAuthCredSSLClientCertPw = record
    password: PChar;
    may_save: TSvnBoolean;
  end;
  PSvnAuthSSLServerCertInfo = ^TSvnAuthSSLServerCertInfo;
  TSvnAuthSSLServerCertInfo = record
    hostname: PChar;
    fingerprint: PChar;
    valid_from: PChar;
    valid_until: PChar;
    issuer_dname: PChar;
    ascii_cert: PChar;
  end;
  PSvnAuthCredSSLServerTrust = ^TSvnAuthCredSSLServerTrust;
  TSvnAuthCredSSLServerTrust = record
    may_save: TSvnBoolean;
    accepted_failures: Cardinal;
  end;
  
  TSvnAuthSimplePromptFunc = function(out cred: PSvnAuthCredSimple; baton: Pointer; realm, username: PChar;
    may_save: TSvnBoolean; pool: PAprPool): PSvnError; cdecl;
  TSvnAuthUsernamePromptFunc = function(out cred: PSvnAuthCredUsername; baton: Pointer; realm: PChar;
    may_save: TSvnBoolean; pool: PAprPool): PSvnError; cdecl;
  TSvnAuthSSLServerTrustPromptFunc = function(out cred: PSvnAuthCredSSLServerTrust; baton: Pointer; realm: PChar;
    failures: Cardinal; cert_info: PSvnAuthSSLServerCertInfo; may_save: TSvnBoolean; pool: PAprPool): PSvnError;
    cdecl;
  TSvnAuthSSLClientCertPromptFunc = function(out cred: PSvnAuthCredSSLClientCert; baton: Pointer; realm: PChar;
    may_save: TSvnBoolean; pool: PAprPool): PSvnError; cdecl;
  TSvnAuthSSLClientCertPwPromptFunc = function(out cred: PSvnAuthCredSSLClientCertPw; baton: Pointer; realm: PChar;
    may_save: TSvnBoolean; pool: PAprPool): PSvnError; cdecl;

var
  svn_auth_ssl_server_cert_info_dup: function(info: PSvnAuthSSLServerCertInfo;
    pool: PAprPool): PSvnAuthSSLServerCertInfo; cdecl;
  svn_auth_open: procedure(out auth_baton: PSvnAuthBaton; providers: PAprArrayHeader; pool: PAprPool); cdecl;
  svn_auth_set_parameter: procedure(auth_baton: PSvnAuthBaton; name: PChar; value: Pointer); cdecl;
  svn_auth_get_parameter: function(auth_baton: PSvnAuthBaton; name: PChar): Pointer; cdecl;
  svn_auth_first_credentials: function(out credentials: Pointer; out state: PSvnAuthIterState;
    cred_kind, realmstring: PChar; auth_baton: PSvnAuthBaton; pool: PAprPool): PSvnError; cdecl;
  svn_auth_next_credentials: function(out credentials: Pointer; state: PSvnAuthIterState; pool: PAprPool): PSvnError;
    cdecl;
  svn_auth_save_credentials: function(state: PSvnAuthIterState; pool: PAprPool): PSvnError; cdecl;

//----- svn_auth.h -----------------------------------------------------------------------------------------------------

//----- svn_ra.h -------------------------------------------------------------------------------------------------------

type
  TSvnRaGetWCPropFunc = function(baton: Pointer; relpath, name: PChar; value: PPSvnString; pool: PAprPool): PSvnError;
    cdecl;
  TSvnRaSetWCPropFunc = function(baton: Pointer; path, name: PChar; value: PSvnString; pool: PAprPool): PSvnError;
    cdecl;
  TSvnRaPushWCPropFunc = function(baton: Pointer; path, name: PChar; value: PSvnString; pool: PAprPool): PSvnError;
    cdecl;
  TSvnRaInvalidateWCPropsFunc = function(baton: Pointer; path, name: PChar; pool: PAprPool): PSvnError; cdecl;
  TSvnRaGetLatestRevNumFunc = function(session_baton: Pointer; out latest_revnum: TSvnRevNum): PSvnError; cdecl;
  TSvnRaFileRevHandler = function(baton: Pointer; path: PChar; rev: TSvnRevNum; rev_props: PAprHash;
    out delta_handler: TSvnTxDeltaWindowHandler; out delta_baton: Pointer; prop_diffs: PAprArrayHeader;
    pool: PAprPool): PSvnError; cdecl;
  TSvnRaLockCallback = function(baton: Pointer; path: PChar; do_lock: TSvnBoolean; lock: PSvnLock; ra_err: PSvnError;
    pool: PAprPool): PSvnError; cdecl;
  TSvnRaProgressNotifyFunc = procedure(progress, total: TAprOff; baton: Pointer; pool: PAprPool); cdecl;

  PSvnRaReporter2 = ^TSvnRaReporter2;
  TSvnRaReporter2 = record
    set_path: function(report_baton: Pointer; path: PChar; revision: TSvnRevNum; start_empty: TSvnBoolean;
      lock_token: PChar; pool: PAprPool): PSvnError; cdecl;
    delete_path: function(report_baton: Pointer; path: PChar; pool: PAprPool): PSvnError; cdecl;
    link_path: function(report_baton: Pointer; path, url: PChar; revision: TSvnRevNum; start_empty: TSvnBoolean;
      lock_token: PChar; pool: PAprPool): PSvnError; cdecl;
    finish_report: function(report_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
    abort_report: function(report_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
  end;

  PSvnRaReporter = ^TSvnRaReporter;
  TSvnRaReporter = record
    set_path: function(report_baton: Pointer; path: PChar; revision: TSvnRevNum; start_empty: TSvnBoolean;
      pool: PAprPool): PSvnError; cdecl;
    delete_path: function(report_baton: Pointer; path: PChar; pool: PAprPool): PSvnError; cdecl;
    link_path: function(report_baton: Pointer; path, url: PChar; revision: TSvnRevNum; start_empty: TSvnBoolean;
      pool: PAprPool): PSvnError; cdecl;
    finish_report: function(report_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
    abort_report: function(report_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
  end;

  PSvnRaCallbacks2 = ^TSvnRaCallbacks2;
  TSvnRaCallbacks2 = record
    open_tmp_file: function(out fp: PAprFile; callback_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
    auth_baton: PSvnAuthBaton;
    get_wc_prop: TSvnRaGetWCPropFunc;
    set_wc_prop: TSvnRaSetWCPropFunc;
    push_wc_prop: TSvnRaPushWCPropFunc;
    invalidate_wc_props: TSvnRaInvalidateWCPropsFunc;
    progress_func: TSvnRaProgressNotifyFunc;
    progress_baton: Pointer;
  end;

  PSvnRaCallbacks = ^TSvnRaCallbacks;
  TSvnRaCallbacks = record
    open_tmp_file: function(out fp: PAprFile; callback_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
    auth_baton: PSvnAuthBaton;
    get_wc_prop: TSvnRaGetWCPropFunc;
    set_wc_prop: TSvnRaSetWCPropFunc;
    push_wc_prop: TSvnRaPushWCPropFunc;
    invalidate_wc_props: TSvnRaInvalidateWCPropsFunc;
  end;

  PSvnRaSession = ^TSvnRaSession;
  TSvnRaSession = THandle;

  PSvnRaPlugin = ^TSvnRaPlugin;
  TSvnRaPlugin = record
    name: PChar;
    description: PChar;
    open: function(out session_baton: Pointer; repos_URL: PChar; callbacks: PSvnRaCallbacks; callback_baton: Pointer;
      config: PAprHash; pool: PAprPool): PSvnError; cdecl;
    get_latest_revnum: function(session_baton: Pointer; out latest_revnum: TSvnRevNum; pool: PAprPool): PSvnError;
      cdecl;
    get_dated_revision: function(session_baton: Pointer; out revision: TSvnRevNum; tm: TAprTime;
      pool: PAprPool): PSvnError; cdecl;
    change_rev_prop: function(session_baton: Pointer; rev: TSvnRevNum; name: PChar; value: PSvnString;
      pool: PAprPool): PSvnError; cdecl;
    rev_proplist: function(session_baton: Pointer; rev: TSvnRevNum; out props: PAprHash; pool: PAprPool): PSvnError;
      cdecl;
    rev_prop: function(session_baton: Pointer; rev: TSvnRevNum; name: PChar; out value: PSvnString;
      pool: PAprPool): PSvnError; cdecl;
    get_commit_editor: function(session_baton: Pointer; out editor: PSvnDeltaEditor; out edit_baton: Pointer;
      log_msg: PChar; callback: TSvnCommitCallback; callback_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
    get_file: function(session_baton: Pointer; path: PChar; revision: TSvnRevNum; stream: PSvnStream;
      fetched_rev: TSvnRevNum; out props: PAprHash; pool: PAprPool): PSvnError; cdecl;
    get_dir: function(session_baton: Pointer; path: PChar; revision: TSvnRevNum; out dirents: PAprHash;
      out fetched_rev: TSvnRevNum; out props: PAprHash; pool: PAprPool): PSvnError; cdecl;
    do_update: function(session_baton: Pointer; out reporter: PSvnRaReporter; out report_baton: Pointer;
      revision_to_update_to: TSvnRevNum; update_target: PChar; recurse: TSvnBoolean; update_editor: TSvnDeltaEditor;
      update_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
    do_switch: function(session_baton: Pointer; out reporter: PSvnRaReporter; out report_baton: Pointer;
      revision_to_switch_to: TSvnRevNum; switch_target: PChar; recurse: TSvnBoolean; switch_url: PChar;
      switch_editor: PSvnDeltaEditor; switch_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
    do_status: function(session_baton: Pointer; out reporter: PSvnRaReporter; out report_baton: Pointer;
      status_target: PChar; revision: TSvnRevNum; recurse: TSvnBoolean; status_editor: PSvnDeltaEditor;
      status_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
    do_diff: function(session_baton: Pointer; out reporter: PSvnRaReporter; out report_baton: Pointer;
      revision: TSvnRevNum; diff_target: PChar; recurse, ignore_ancestry: TSvnBoolean; versus_url: PChar;
      diff_editor: PSvnDeltaEditor; diff_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
    get_log: function(session_baton: Pointer; paths: PAprArrayHeader; revstart, revend: TSvnRevNum;
      discover_changed_paths, strict_node_history: TSvnBoolean; receiver: TSvnLogMessageReceiver;
      receiver_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
    check_path: function(session_baton: Pointer; path: PChar; revision: TSvnRevNum; out kind: TSvnNodeKind;
      pool: PAprPool): PSvnError; cdecl;
    get_uuid: function(session_baton: Pointer; out uuid: PChar; pool: PAprPool): PSvnError; cdecl;
    get_repos_root: function(session_baton: Pointer; out url: PChar; pool: PAprPool): PSvnError; cdecl;
    get_locations: function(session_baton: Pointer; out locations: PAprHash; path: PChar; peg_revision: TSvnRevNum;
      location_revisions: PAprArrayHeader; pool: PAprPool): PSvnError; cdecl;
    get_file_revs: function(session_baton: Pointer; path: PChar; revstart, revend: TSvnRevNum;
      handler: TSvnRaFileRevHandler; handler_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
    get_version: function: PSvnVersion; cdecl;
  end;

  TSvnRaInitFunc = function(abi_version: Integer; pool: PAprPool; hash: PAprHash): PSvnError; cdecl;

var
  svn_ra_version: function: PSvnVersion; cdecl;
  svn_ra_initialize: function(pool: PAprPool): PSvnError; cdecl;
  svn_ra_create_callbacks: function(out callbacks: PSvnRaCallbacks2; pool: PAprPool): PSvnError; cdecl;
  svn_ra_open2: function(out session_p: PSvnRaSession; repos_URL: PChar; callbacks: PSvnRaCallbacks2;
    callback_baton: Pointer; config: PAprHash; pool: PAprPool): PSvnError; cdecl;
  svn_ra_open: function(out session_p: PSvnRaSession; repos_URL: PChar; callbacks: PSvnRaCallbacks;
    callback_baton: Pointer; config: PAprHash; pool: PAprPool): PSvnError; cdecl;
  svn_ra_get_latest_revnum: function(session: PSvnRaSession; out latest_revnum: TSvnRevNum; pool: PAprPool): PSvnError;
    cdecl;
  svn_ra_get_dated_revision: function(session: PSvnRaSession; out revision: TSvnRevNum; tm: TAprTime;
    pool: PAprPool): PSvnError; cdecl;
  svn_ra_change_rev_prop: function(session: PSvnRaSession; rev: TSvnRevNum; name: PChar; value: PSvnString;
    pool: PAprPool): PSvnError; cdecl;
  svn_ra_rev_proplist: function(session: PSvnRaSession; rev: TSvnRevNum; out props: PAprHash;
    pool: PAprPool): PSvnError; cdecl;
  svn_ra_rev_prop: function(session: PSvnRaSession; rev: TSvnRevNum; name: PChar; out value: PSvnString;
    pool: PAprPool): PSvnError; cdecl;
  svn_ra_get_commit_editor: function(session: PSvnRaSession; out editor: PSvnDeltaEditor; out edit_baton: Pointer;
    log_msg: PChar; callback: TSvnCommitCallback; callback_baton: Pointer; lock_tokens: PAprHash;
    keep_locks: TSvnBoolean; pool: PAprPool): PSvnError; cdecl;
  svn_ra_get_file: function(session: PSvnRaSession; path: PChar; revision: TSvnRevNum; stream: PSvnStream;
    out fetched_rev: TSvnRevNum; out props: PAprHash; pool: PAprPool): PSvnError; cdecl;
  svn_ra_get_dir: function(session: PSvnRaSession; path: PChar; revision: TSvnRevNum; out dirents: PAprHash;
    out fetched_rev: TSvnRevNum; out props: PAprHash; pool: PAprPool): PSvnError; cdecl;
  svn_ra_do_update: function(session: PSvnRaSession; out reporter: PSvnRaReporter2; out report_baton: Pointer;
    revision_to_update_to: TSvnRevNum; update_target: PChar; recurse: TSvnBoolean; update_editor: PSvnDeltaEditor;
    update_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
  svn_ra_do_switch: function(session: PSvnRaSession; out reporter: PSvnRaReporter2; out report_baton: Pointer;
    revision_to_switch_to: TSvnRevNum; switch_target: PChar; recurse: TSvnBoolean; switch_url: PChar;
    switch_editor: PSvnDeltaEditor; switch_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
  svn_ra_do_status: function(session: PSvnRaSession; out reporter: PSvnRaReporter2; out report_baton: Pointer;
    status_target: PChar; revision: TSvnRevNum; recurse: TSvnBoolean; status_editor: PSvnDeltaEditor;
    status_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
  svn_ra_do_diff: function(session: PSvnRaSession; out reporter: PSvnRaReporter2; out report_baton: Pointer;
    revision: TSvnRevNum; diff_target: PChar; recurse, ignore_ancestry: TSvnBoolean; versus_url: PChar;
    diff_editor: PSvnDeltaEditor; diff_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
  svn_ra_get_log: function(session: PSvnRaSession; paths: PAprArrayHeader; revstart, revend: TSvnRevNum; limit: Integer;
    discover_changed_paths, strict_node_history: TSvnBoolean; receiver: TSvnLogMessageReceiver; receiver_baton: Pointer;
    pool: PAprPool): PSvnError; cdecl;
  svn_ra_check_path: function(session: PSvnRaSession; path: PChar; revision: TSvnRevNum; out kind: TSvnNodeKind;
    pool: PAprPool): PSvnError; cdecl;
  svn_ra_stat: function(session: PSvnRaSession; path: PChar; revision: TSvnRevNum; dirent: PPSvnDirEnt;
    pool: PAprPool): PSvnError; cdecl;
  svn_ra_get_uuid: function(session: PSvnRaSession; out uuid: PChar; pool: PAprPool): PSvnError; cdecl;
  svn_ra_get_repos_root: function(session: PSvnRaSession; out url: PChar; pool: PAprPool): PSvnError; cdecl;
  svn_ra_get_locations: function(session: PSvnRaSession; out locations: PAprHash; path: PChar; peg_revision: TSvnRevNum;
    location_revisions: PAprArrayHeader; pool: PAprPool): PSvnError; cdecl;
  svn_ra_get_file_revs: function(session: PSvnRaSession; path: PChar; revstart, revend: TSvnRevNum;
    handler: TSvnRaFileRevHandler; handler_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
  svn_ra_lock: function(session: PSvnRaSession; path_revs: PAprHash; comment: PChar; steal_lock: TSvnBoolean;
    lock_func: TSvnRaLockCallback; lock_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
  svn_ra_unlock: function(session: PSvnRaSession; path_tokens: PAprHash; break_lock: TSvnBoolean;
    lock_func: TSvnRaLockCallback; lock_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
  svn_ra_get_lock: function(session: PSvnRaSession; out lock: PSvnLock; path: PChar; pool: PAprPool): PSvnError;
    cdecl;
  svn_ra_get_locks: function(session: PSvnRaSession; out locks: PAprHash; path: PChar; pool: PAprPool): PSvnError;
    cdecl;
  svn_ra_print_modules: function(out output: TSvnStringBuf; pool: PAprPool): PSvnError; cdecl;
  svn_ra_print_ra_libraries: function(out descriptions: PSvnStringBuf; ra_baton: Pointer; pool: PAprPool): PSvnError;
    cdecl;

  svn_ra_dav_init: function(abi_version: Integer; pool: PAprPool; hash: PAprHash): PSvnError; cdecl;
  svn_ra_local_init: function(abi_version: Integer; pool: PAprPool; hash: PAprHash): PSvnError; cdecl;
  svn_ra_svn_init: function(abi_version: Integer; pool: PAprPool; hash: PAprHash): PSvnError; cdecl;
  svn_ra_init_ra_libs: function(out ra_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
  svn_ra_get_ra_library: function(out lib: PSvnRaPlugin; ra_baton: Pointer; url: PChar; pool: PAprPool): PSvnError;
    cdecl;

//----- svn_ra.h -------------------------------------------------------------------------------------------------------

//----- svn_wc.h -------------------------------------------------------------------------------------------------------

const
  SVN_WC_ADM_DIR_NAME = '.svn';
  SVN_WC_ENTRY_THIS_DIR = '';

type
  PSvnWCAdmAccess = ^TSvnWCAdmAccess;
  TSvnWCAdmAccess = THandle;
  PSvnWCTraversalInfo = ^TSvnWCTraversalInfo;
  TSvnWCTraversalInfo = THandle;
  PSvnWCExternalItem = ^TSvnWCExternalItem;
  TSvnWCExternalItem = record
    target_dir: PChar;
    url: PChar;
    revision: TSvnOptRevision;
  end;
  TSvnWCNotifyAction = (svnWcNotifyAdd, svnWcNotifyCopy, svnWcNotifyDelete, svnWcNotifyRestore,
    svnWcNotifyRevert, svnWcNotifyFailedRevert, svnWcNotifyResolved, svnWcNotifySkip, svnWcNotifyUpdateDelete,
    svnWcNotifyUpdateAdd, svnWcNotifyUpdateUpdate, svnWcNotifyUpdateCompleted, svnWcNotifyUpdateExternal,
    svnWcNotifyStatusCompleted, svnWcNotifyStatusExternal, svnWcNotifyCommitModified, svnWcNotifyCommitAdded,
    svnWcNotifyCommitDeleted, svnWcNotifyCommitReplaced, svnWcNotifyCommitPostfixTxdelta, svnWcNotifyBlameRevision,
    svnWcNotifyLocked, svnWcNotifyUnlocked, svnWcNotifyFailedLock, svnWcNotifyFailedUnlock);
  PSvnWCNotifyState = ^TSvnWCNotifyState;
  TSvnWCNotifyState = (svnWcNotifyStateInapplicable, svnWcNotifyStateUnknown, svnWcNotifyStateUnchanged,
    svnWcNotifyStateMissing, svnWcNotifyStateObstructed, svnWcNotifyStateChanged, svnWcNotifyStateMerged,
    svnWcNotifyStateConflicted);
  TSvnWCNotifyLockState = (svnWcNotifyLockStateInapplicable, svnWcNotifyLockStateUnknown, svnWcNotifyLockStateUnchanged,
    svnWcNotifyLockStateLocked, svnWcNotifyLockStateUnlocked);
  PSvnWCNotify = ^TSvnWCNotify;
  TSvnWCNotify = record
    path: PChar;
    action: TSvnWCNotifyAction;
    kind: TSvnNodeKind;
    mime_type: PChar;
    lock: PSvnLock;
    err: PSvnError;
    content_state: TSvnWCNotifyState;
    prop_state: TSvnWCNotifyState;
    lock_state: TSvnWCNotifyLockState;
    revision: TSvnRevNum;
  end;
  TSvnWCNotifyFunc2 = procedure(baton: Pointer; notify: PSvnWCNotify; pool: PAprPool); cdecl;
  TSvnWCNotifyFunc = procedure(baton: Pointer; path: PChar; action: TSvnWCNotifyAction; kind: TSvnNodeKind;
    mime_type: PChar; content_state, prop_state: TSvnWCNotifyState; revision: TSvnRevNum); cdecl;
  PSvnWCDiffCallbacks2 = ^TSvnWCDiffCallbacks2;
  TSvnWCDiffCallbacks2 = record
    file_changed: function(adm_access: PSvnWCAdmAccess; contentstate, propstate: PSvnWCNotifyState;
      path, tmpfile1, tmpfile2: PChar; rev1, rev2: TSvnRevNum; mimetype1, mimetype2: PChar;
      propchanges: PAprArrayHeader; originalprops: PAprHash; diff_baton: Pointer): PSvnError; cdecl;
    file_added: function(adm_access: PSvnWCAdmAccess; contentstate, propstate: PSvnWCNotifyState;
      path, tmpfile1, tmpfile2: PChar; rev1, rev2: TSvnRevNum; mimetype1, mimetype2: PChar;
      propchanges: PAprArrayHeader; originalprops: PAprHash; diff_baton: Pointer): PSvnError; cdecl;
    file_deleted: function(adm_access: PSvnWCAdmAccess; state: PSvnWCNotifyState;
      path, tmpfile1, tmpfile2, mimetype1, mimetype2: PChar; originalprops: PAprHash; diff_baton: Pointer): PSvnError;
      cdecl;
    dir_added: function(adm_access: PSvnWCAdmAccess; state: PSvnWCNotifyState; path: PChar; rev: TSvnRevNum;
      diff_baton: Pointer): PSvnError; cdecl;
    dir_deleted: function(adm_access: PSvnWCAdmAccess; state: PSvnWCNotifyState; path: PChar;
      diff_baton: Pointer): PSvnError; cdecl;
    dir_props_changed: function(adm_access: PSvnWCAdmAccess; state: PSvnWCNotifyState; path: PChar;
      propchanges: PAprArrayHeader; original_props: PAprHash; diff_baton: Pointer): PSvnError; cdecl;
  end;
  PSvnWCDiffCallbacks = ^TSvnWCDiffCallbacks;
  TSvnWCDiffCallbacks = record
    file_changed: function(adm_access: PSvnWCAdmAccess; state: PSvnWCNotifyState; path, tmpfile1, tmpfile2: PChar;
      rev1, rev2: TSvnRevNum; mimetype1, mimetype2: PChar; diff_baton: Pointer): PSvnError; cdecl;
    file_added: function(adm_access: PSvnWCAdmAccess; state: PSvnWCNotifyState; path, tmpfile1, tmpfile2: PChar;
      rev1, rev2: TSvnRevNum; mimetype1, mimetype2: PChar; diff_baton: Pointer): PSvnError; cdecl;
    file_deleted: function(adm_access: PSvnWCAdmAccess; state: PSvnWCNotifyState;
      path, tmpfile1, tmpfile2, mimetype1, mimetype2: PChar; diff_baton: Pointer): PSvnError; cdecl;
    dir_added: function(adm_access: PSvnWCAdmAccess; state: PSvnWCNotifyState; path: PChar; rev: TSvnRevNum;
      diff_baton: Pointer): PSvnError; cdecl;
    dir_deleted: function(adm_access: PSvnWCAdmAccess; state: PSvnWCNotifyState; path: PChar;
      diff_baton: Pointer): PSvnError; cdecl;
    props_changed: function(adm_access: PSvnWCAdmAccess; state: PSvnWCNotifyState; path: PChar;
      propchanges: PAprArrayHeader; original_props: PAprHash; diff_baton: Pointer): PSvnError; cdecl;
  end;
  TSvnWCSchedule = (svnWcScheduleNormal, svnWcScheduleAdd, svnWcScheduleDelete, svnWcScheduleReplace);
  PSvnWCEntry = ^TSvnWCEntry;
  TSvnWCEntry = record
    name: PChar;
    revision: TSvnRevNum;
    url: PChar;
    repos: PChar;
    uuid: PChar;
    kind: TSvnNodeKind;
    schedule: TSvnWCSchedule;
    copied: TSvnBoolean;
    deleted: TSvnBoolean;
    absent: TSvnBoolean;
    incomplete: TSvnBoolean;
    copyfrom_url: PChar;
    copyfrom_rev: TSvnRevNum;
    conflict_old: PChar;
    conflict_new: PChar;
    conflict_wrk: PChar;
    prejfile: PChar;
    text_time: TAprTime;
    prop_time: TAprTime;
    checksum: PChar;
    cmt_rev: TSvnRevNum;
    cmt_date: TAprTime;
    cmt_author: PChar;
    lock_token: PChar;
    lock_owner: PChar;
    lock_comment: PChar;
    lock_creation_date: TAprTime;
  end;
  PSvnWCEntryCallbacks = ^TSvnWCEntryCallbacks;
  TSvnWCEntryCallbacks = record
    found_entry: function(path: PChar; entry: PSvnWCEntry; walk_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
  end;
  TSvnWCStatusKind = (svnWcStatusNone = 1, svnWcStatusUnversioned, svnWcStatusNormal, svnWcStatusAdded,
    svnWcStatusMissing, svnWcStatusDeleted, svnWcStatusReplaced, svnWcStatusModified, svnWcStatusMerged,
    svnWcStatusConflicted, svnWcStatusIgnored, svnWcStatusObstructed, svnWcStatusExternal,
    svnWcStatusIncomplete);
  PSvnWCStatus2 = ^TSvnWCStatus2;
  TSvnWCStatus2 = record
    entry: PSvnWCEntry;
    text_status: TSvnWCStatusKind;
    prop_status: TSvnWCStatusKind;
    locked: TSvnBoolean;
    copied: TSvnBoolean;
    switched: TSvnBoolean;
    repos_text_status: TSvnWCStatusKind;
    repos_prop_status: TSvnWCStatusKind;
    repos_lock: PSvnLock;
    url: PChar;
    ood_last_cmt_rev: TSvnRevNum;
    ood_last_cmt_date: TAprTime;
    ood_kind: TSvnNodeKind;
    ood_last_cmt_author: PChar;
  end;
  PSvnWCStatus = ^TSvnWCStatus;
  TSvnWCStatus = record
    entry: PSvnWCEntry;
    text_status: TSvnWCStatusKind;
    prop_status: TSvnWCStatusKind;
    locked: TSvnBoolean;
    copied: TSvnBoolean;
    switched: TSvnBoolean;
    repos_text_status: TSvnWCStatusKind;
    repos_prop_status: TSvnWCStatusKind;
  end;
  TSvnWCStatusFunc2 = procedure(baton: Pointer; path: PChar; status: PSvnWCStatus2); cdecl;
  TSvnWCStatusFunc = procedure(baton: Pointer; path: PChar; status: PSvnWCStatus); cdecl;
  TSvnWCMergeOutcome = (svnWcMergeUnchanged, svnWcMergeMerged, svnWcMergeConflict, svnWcMergeNoMerge);
  TSvnWCRelocationValidator = function(baton: Pointer; uuid, url: PChar): PSvnError; cdecl;

var
  svn_wc_version: function: PSvnVersion; cdecl;
  svn_wc_adm_open3: function(out adm_access: PSvnWCAdmAccess; associated: PSvnWCAdmAccess; path: PChar;
    write_lock: TSvnBoolean; depth: Integer; cancel_func: TSvnCancelFunc; cancel_baton: Pointer;
    pool: PAprPool): PSvnError; cdecl;
  svn_wc_adm_open2: function(out adm_access: PSvnWCAdmAccess; associated: PSvnWCAdmAccess; path: PChar;
    write_lock: TSvnBoolean; depth: Integer; pool: PAprPool): PSvnError; cdecl;
  svn_wc_adm_open: function(out adm_access: PSvnWCAdmAccess; associated: PSvnWCAdmAccess; path: PChar;
    write_lock, tree_lock: TSvnBoolean; pool: PAprPool): PSvnError; cdecl;
  svn_wc_adm_probe_open3: function(out adm_access: PSvnWCAdmAccess; associated: PSvnWCAdmAccess; path: PChar;
    write_lock: TSvnBoolean; depth: Integer; cancel_func: TSvnCancelFunc; cancel_baton: Pointer;
    pool: PAprPool): PSvnError; cdecl;
  svn_wc_adm_probe_open2: function(out adm_access: PSvnWCAdmAccess; associated: PSvnWCAdmAccess; path: PChar;
    write_lock: TSvnBoolean; depth: Integer; pool: PAprPool): PSvnError; cdecl;
  svn_wc_adm_probe_open: function(out adm_access: PSvnWCAdmAccess; associated: PSvnWCAdmAccess; path: PChar;
    write_lock, tree_lock: TSvnBoolean; pool: PAprPool): PSvnError; cdecl;
  svn_wc_adm_open_anchor: function(out anchor_access, target_access: PSvnWCAdmAccess; out target: PChar;
    path: PChar; write_lock: TSvnBoolean; depth: Integer; cancel_func: TSvnCancelFunc; cancel_baton: Pointer;
    pool: PAprPool): PSvnError; cdecl;
  svn_wc_adm_retrieve: function(out adm_access: PSvnWCAdmAccess; associated: PSvnWCAdmAccess; path: PChar;
    pool: PAprPool): PSvnError; cdecl;
  svn_wc_adm_probe_retrieve: function(adm_access: PSvnWCAdmAccess; associated: PSvnWCAdmAccess; path: PChar;
    pool: PAprPool): PSvnError; cdecl;
  svn_wc_adm_probe_try3: function(adm_access: PSvnWCAdmAccess; associated: PSvnWCAdmAccess; path: PChar;
    write_lock: TSvnBoolean; depth: Integer; cancel_func: TSvnCancelFunc; cancel_baton: Pointer;
    pool: PAprPool): PSvnError; cdecl;
  svn_wc_adm_probe_try2: function(out adm_access: PSvnWCAdmAccess; associated: PSvnWCAdmAccess; path: PChar;
    write_lock: TSvnBoolean; depth: Integer; pool: PAprPool): PSvnError; cdecl;
  svn_wc_adm_probe_try: function(out adm_access: PSvnWCAdmAccess; associated: PSvnWCAdmAccess; path: PChar;
    write_lock, tree_lock: TSvnBoolean; pool: PAprPool): PSvnError; cdecl;
  svn_wc_adm_close: function(adm_access: PSvnWCAdmAccess): PSvnError; cdecl;
  svn_wc_adm_access_path: function(adm_access: PSvnWCAdmAccess): PChar; cdecl;
  svn_wc_adm_access_pool: function(adm_access: PSvnWCAdmAccess): PAprPool; cdecl;
  svn_wc_adm_locked: function(adm_access: PSvnWCAdmAccess): TSvnBoolean; cdecl;
  svn_wc_locked: function(out locked: TSvnBoolean; path: PChar; pool: PAprPool): PSvnError; cdecl;
  svn_wc_is_adm_dir: function(name: PChar; pool: PAprPool): TSvnBoolean; cdecl;
  svn_wc_get_adm_dir: function(pool: PAprPool): PChar; cdecl;
  svn_wc_set_adm_dir: function(name: PChar; pool: PAprPool): PSvnError; cdecl;
  svn_wc_init_traversal_info: function(pool: PAprPool): PSvnWCTraversalInfo; cdecl;
  svn_wc_edited_externals: procedure(out externals_old, externals_new: PAprHash; traversal_info: PSvnWCTraversalInfo);
    cdecl;
  svn_wc_external_item_dup: function(item: PSvnWCExternalItem; pool: PAprPool): PSvnWCExternalItem; cdecl;
  svn_wc_parse_externals_description2: function(out externals_p: PAprArrayHeader; parent_directory, desc: PChar;
    pool: PAprPool): PSvnError; cdecl;
  svn_wc_parse_externals_description: function(out externals_p: PAprHash; parent_directory, desc: PChar;
    pool: PAprPool): PSvnError; cdecl;
  svn_wc_create_notify: function(path: PChar; action: TSvnWCNotifyAction; pool: PAprPool): PSvnWCNotify; cdecl;
  svn_wc_dup_notify: function(notify: PSvnWCNotify; pool: PAprPool): PSvnWCNotify; cdecl;
  svn_wc_check_wc: function(path: PChar; out wc_format: Integer; pool: PAprPool): PSvnError; cdecl;
  svn_wc_has_binary_prop: function(out has_binary_prop: TSvnBoolean; path: PChar; adm_access: PSvnWCAdmAccess;
    pool: PAprPool): PSvnError; cdecl;
  svn_wc_text_modified_p: function(out modified_p: TSvnBoolean; filename: PChar; force_comparison: TSvnBoolean;
    adm_access: PSvnWCAdmAccess; pool: PAprPool): PSvnError; cdecl;
  svn_wc_props_modified_p: function(out modified_p: TSvnBoolean; path: PChar; adm_access: PSvnWCAdmAccess;
    pool: PAprPool): PSvnError; cdecl;
  svn_wc_entry: function(out entry: PSvnWCEntry; path: PChar; adm_access: PSvnWCAdmAccess; show_hidden: TSvnBoolean;
    pool: PAprPool): PSvnError; cdecl;
  svn_wc_entries_read: function(out entries: PAprHash; adm_access: PSvnWCAdmAccess; show_hidden: TSvnBoolean;
    pool: PAprPool): PSvnError; cdecl;
  svn_wc_entry_dup: function(entry: PSvnWCEntry; pool: PAprPool): PSvnWCEntry; cdecl;
  svn_wc_conflicted_p: function(out text_conflicted_p, prop_conflicted_p: TSvnBoolean; dir_path: PChar;
    entry: PSvnWCEntry; pool: PAprPool): PSvnError; cdecl;
  svn_wc_get_ancestry: function(url: PPChar; rev: PSvnRevNum; path: PChar; adm_access: PSvnWCAdmAccess;
    pool: PAprPool): PSvnError; cdecl;
  svn_wc_walk_entries2: function(path: PChar; adm_access: PSvnWCAdmAccess; walk_callbacks: PSvnWCEntryCallbacks;
    walk_baton: Pointer; show_hidden: TSvnBoolean; cancel_func: TSvnCancelFunc; cancel_baton: Pointer;
    pool: PAprPool): PSvnError; cdecl;
  svn_wc_walk_entries: function(path: PChar; adm_access: PSvnWCAdmAccess; walk_callbacks: PSvnWCEntryCallbacks;
    walk_baton: Pointer; show_hidden: TSvnBoolean; pool: PAprPool): PSvnError; cdecl;
  svn_wc_mark_missing_deleted: function(path: PChar; parent: PSvnWCAdmAccess; pool: PAprPool): PSvnError; cdecl;
  svn_wc_ensure_adm2: function(path, uuid, url, repos: PChar; revision: TSvnRevNum; pool: PAprPool): PSvnError; cdecl;
  svn_wc_ensure_adm: function(path, uuid, url: PChar; revision: TSvnRevNum; pool: PAprPool): PSvnError; cdecl;
  svn_wc_maybe_set_repos_root: function(adm_access: PSvnWCAdmAccess; path, repos: PChar; pool: PAprPool): PSvnError;
    cdecl;
  svn_wc_dup_status2: function(orig_stat: PSvnWCStatus2; pool: PAprPool): PSvnWCStatus2; cdecl;
  svn_wc_dup_status: function(orig_stat: PSvnWCStatus; pool: PAprPool): PSvnWCStatus; cdecl;
  svn_wc_status2: function(out status: PSvnWCStatus2; path: PChar; adm_access: PSvnWCAdmAccess;
    pool: PAprPool): PSvnError; cdecl;
  svn_wc_status: function(out status: PSvnWCStatus; path: PChar; adm_access: PSvnWCAdmAccess;
    pool: PAprPool): PSvnError; cdecl;
  svn_wc_get_status_editor2: function(out editor: PSvnDeltaEditor; out edit_baton, set_locks_baton: Pointer;
    out edit_revision: TSvnRevNum; anchor: PSvnWCAdmAccess; target: PChar; config: PAprHash;
    recurse, get_all, no_ignore: TSvnBoolean; status_func: TSvnWCStatusFunc2; status_baton: Pointer;
    cancel_func: TSvnCancelFunc; cancel_baton: Pointer; traversal_info: PSvnWCTraversalInfo;
    pool: PAprPool): PSvnError; cdecl;
  svn_wc_get_status_editor: function(out editor: PSvnDeltaEditor; out edit_baton: Pointer;
    out edit_revision: TSvnRevNum; anchor: PSvnWCAdmAccess; target: PChar; config: PAprHash;
    recurse, get_all, no_ignore: TSvnBoolean; status_func: TSvnWCStatusFunc; status_baton: Pointer;
    cancel_func: TSvnCancelFunc; cancel_baton: Pointer; traversal_info: PSvnWCTraversalInfo; pool: PAprPool): PSvnError;
    cdecl;
  svn_wc_status_set_repos_locks: function(set_locks_baton: Pointer; locks: PAprHash; repos_root: PChar;
    pool: PAprPool): PSvnError; cdecl;
  svn_wc_copy2: function(src: PChar; dst_parent: PSvnWCAdmAccess; dst_basename: PChar; cancel_func: TSvnCancelFunc;
    cancel_baton: Pointer; notify_func: TSvnWCNotifyFunc2; notify_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
  svn_wc_copy: function(src: PChar; dst_parent: PSvnWCAdmAccess; dst_basename: PChar; cancel_func: TSvnCancelFunc;
    cancel_baton: Pointer; notify_func: TSvnWCNotifyFunc; notify_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
  svn_wc_delete2: function(path: PChar; adm_access: PSvnWCAdmAccess; cancel_func: TSvnCancelFunc; cancel_baton: Pointer;
    notify_func: TSvnWCNotifyFunc2; notify_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
  svn_wc_delete: function(path: PChar; adm_access: PSvnWCAdmAccess; cancel_func: TSvnCancelFunc; cancel_baton: Pointer;
    notify_func: TSvnWCNotifyFunc; notify_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
  svn_wc_add2: function(path: PChar; parent_access: PSvnWCAdmAccess; copyfrom_url: PChar; copyfrom_rev: TSvnRevNum;
    cancel_func: TSvnCancelFunc; cancel_baton: Pointer; notify_func: TSvnWCNotifyFunc2; notify_baton: Pointer;
    pool: PAprPool): PSvnError; cdecl;
  svn_wc_add: function(path: PChar; parent_access: PSvnWCAdmAccess; copyfrom_url: PChar; copyfrom_rev: TSvnRevNum;
    cancel_func: TSvnCancelFunc; cancel_baton: Pointer; notify_func: TSvnWCNotifyFunc; notify_baton: Pointer;
    pool: PAprPool): PSvnError; cdecl;
  svn_wc_add_repos_file: function(dst_path: PChar; adm_access: PSvnWCAdmAccess; new_text_path: PChar;
    new_props: PAprHash; copyfrom_url: PChar; copyfrom_rev: TSvnRevNum; pool: PAprPool): PSvnError; cdecl;
  svn_wc_remove_from_revision_control: function(adm_access: PSvnWCAdmAccess; name: PChar;
    destroy_wf, instant_error: TSvnBoolean; cancel_func: TSvnCancelFunc; cancel_baton: Pointer;
    pool: PAprPool): PSvnError; cdecl;
  svn_wc_resolved_conflict2: function(path: PChar; adm_access: PSvnWCAdmAccess;
    resolve_text, resolve_props, recurse: TSvnBoolean; notify_func: TSvnWCNotifyFunc2; notify_baton: Pointer;
    cancel_func: TSvnCancelFunc; cancel_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
  svn_wc_resolved_conflict: function(path: PChar; adm_access: PSvnWCAdmAccess;
    resolve_text, resolve_props, recurse: TSvnBoolean; notify_func: TSvnWCNotifyFunc; notify_baton: Pointer;
    pool: PAprPool): PSvnError; cdecl;
  svn_wc_process_committed2: function(path: PChar; adm_access: PSvnWCAdmAccess; recurse: TSvnBoolean;
    new_revnum: TSvnRevNum; rev_date, rev_author: PChar; wcprop_changes: PAprArrayHeader; remove_lock: TSvnBoolean;
    pool: PAprPool): PSvnError; cdecl;
  svn_wc_process_committed: function(path: PChar; adm_access: PSvnWCAdmAccess; recurse: TSvnBoolean;
    new_revnum: TSvnRevNum; rev_date, rev_author: PChar; wcprop_changes: PAprArrayHeader; pool: PAprPool): PSvnError;
    cdecl;
  svn_wc_crawl_revisions2: function(path: PChar; adm_access: PSvnWCAdmAccess; reporter: PSvnRaReporter2;
    report_baton: Pointer; restore_files, recurse, use_commit_times: TSvnBoolean; notify_func: TSvnWCNotifyFunc2;
    notify_baton: Pointer; traversal_info: PSvnWCTraversalInfo; pool: PAprPool): PSvnError; cdecl;
  svn_wc_crawl_revisions: function(path: PChar; adm_access: PSvnWCAdmAccess; reporter: PSvnRaReporter;
    report_baton: Pointer; restore_files, recurse, use_commit_times: TSvnBoolean; notify_func: TSvnWCNotifyFunc;
    notify_baton: Pointer; traversal_info: PSvnWCTraversalInfo; pool: PAprPool): PSvnError; cdecl;
  svn_wc_is_wc_root: function(out wc_root: TSvnBoolean; path: PChar; adm_access: PSvnWCAdmAccess;
    pool: PAprPool): PSvnError; cdecl;
  svn_wc_get_actual_target: function(path: PChar; out anchor, target: PChar; pool: PAprPool): PSvnError; cdecl;
  svn_wc_get_update_editor2: function(out target_revision: TSvnRevNum; anchor: PSvnWCAdmAccess; target: PChar;
    use_commit_times, recurse: TSvnBoolean; notify_func: TSvnWCNotifyFunc2; notify_baton: Pointer;
    cancel_func: TSvnCancelFunc; cancel_baton: Pointer; diff3_cmd: PChar; out editor: PSvnDeltaEditor;
    out edit_baton: Pointer; ti: PSvnWCTraversalInfo; pool: PAprPool): PSvnError; cdecl;
  svn_wc_get_update_editor: function(out target_revision: TSvnRevNum; anchor: PSvnWCAdmAccess; target: PChar;
    use_commit_times, recurse: TSvnBoolean; notify_func: TSvnWCNotifyFunc; notify_baton: Pointer;
    cancel_func: TSvnCancelFunc; cancel_baton: Pointer; diff3_cmd: PChar; out editor: PSvnDeltaEditor;
    out edit_baton: Pointer; ti: PSvnWCTraversalInfo; pool: PAprPool): PSvnError; cdecl;
  svn_wc_get_switch_editor2: function(out target_revision: TSvnRevNum; anchor: PSvnWCAdmAccess;
    target, switch_url: PChar; use_commit_times, recurse: TSvnBoolean; notify_func: TSvnWCNotifyFunc2;
    notify_baton: Pointer; cancel_func: TSvnCancelFunc; cancel_baton: Pointer; diff3_cmd: PChar;
    out editor: PSvnDeltaEditor; out edit_baton: Pointer; ti: PSvnWCTraversalInfo; pool: PAprPool): PSvnError; cdecl;
  svn_wc_get_switch_editor: function(out target_revision: TSvnRevNum; anchor: PSvnWCAdmAccess;
    target, switch_url: PChar; use_commit_times, recurse: TSvnBoolean; notify_func: TSvnWCNotifyFunc;
    notify_baton: Pointer; cancel_func: TSvnCancelFunc; cancel_baton: Pointer; diff3_cmd: PChar;
    out editor: PSvnDeltaEditor; out edit_baton: Pointer; ti: PSvnWCTraversalInfo; pool: PAprPool): PSvnError; cdecl;
  svn_wc_prop_list: function(out props: PAprHash; path: PChar; adm_access: PSvnWCAdmAccess; pool: PAprPool): PSvnError;
    cdecl;
  svn_wc_prop_get: function(out value: PSvnString; name, path: PChar; adm_access: PSvnWCAdmAccess;
    pool: PAprPool): PSvnError; cdecl;
  svn_wc_prop_set2: function(name: PChar; value: PSvnString; path: PChar; adm_access: PSvnWCAdmAccess;
    skip_checks: TSvnBoolean; pool: PAprPool): PSvnError; cdecl;
  svn_wc_prop_set: function(name: PChar; value: PSvnString; path: PChar; adm_access: PSvnWCAdmAccess;
    pool: PAprPool): PSvnError; cdecl;
  svn_wc_is_normal_prop: function(name: PChar): TSvnBoolean; cdecl;
  svn_wc_is_wc_prop: function(name: PChar): TSvnBoolean; cdecl;
  svn_wc_is_entry_prop: function(name: PChar): TSvnBoolean; cdecl;
  svn_wc_get_diff_editor3: function(anchor: PSvnWCAdmAccess; target: PChar; callbacks: PSvnWCDiffCallbacks;
    callback_baton: Pointer; recurse, ignore_ancestry, use_text_base, reverse_order: TSvnBoolean;
    cancel_func: TSvnCancelFunc; cancel_baton: Pointer; out editor: PSvnDeltaEditor; out edit_baton: Pointer;
    pool: PAprPool): PSvnError; cdecl;
  svn_wc_get_diff_editor2: function(anchor: PSvnWCAdmAccess; target: PChar; callbacks: PSvnWCDiffCallbacks;
    callback_baton: Pointer; recurse, ignore_ancestry, use_text_base, reverse_order: TSvnBoolean;
    cancel_func: TSvnCancelFunc; cancel_baton: Pointer; out editor: PSvnDeltaEditor; edit_baton: Pointer;
    pool: PAprPool): PSvnError; cdecl;
  svn_wc_get_diff_editor: function(anchor: PSvnWCAdmAccess; target: PChar; callbacks: PSvnWCDiffCallbacks;
    callback_baton: Pointer; recurse, use_text_base, reverse_order: TSvnBoolean; cancel_func: TSvnCancelFunc;
    cancel_baton: Pointer; out editor: PSvnDeltaEditor; out edit_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
  svn_wc_diff3: function(anchor: PSvnWCAdmAccess; target: PChar; callbacks: PSvnWCDiffCallbacks2;
    callback_baton: Pointer; recurse, ignore_ancestry: TSvnBoolean; pool: PAprPool): PSvnError; cdecl;
  svn_wc_diff2: function(anchor: PSvnWCAdmAccess; target: PChar; callbacks: PSvnWCDiffCallbacks;
    callback_baton: Pointer; recurse, ignore_ancestry: TSvnBoolean; pool: PAprPool): PSvnError; cdecl;
  svn_wc_diff: function(anchor: PSvnWCAdmAccess; target: PChar; callbacks: PSvnWCDiffCallbacks; callback_baton: Pointer;
    recurse: TSvnBoolean; pool: PAprPool): PSvnError; cdecl;
  svn_wc_get_prop_diffs: function(out propchanges: PAprArrayHeader; out original_props: PAprHash; path: PChar;
    adm_access: PSvnWCAdmAccess; pool: PAprPool): PSvnError; cdecl;
  svn_wc_merge: function(left, right, merge_target: PChar; adm_access: PSvnWCAdmAccess;
    left_label, right_label, target_label: PChar; dry_run: TSvnBoolean; out merge_outcome: TSvnWCMergeOutcome;
    diff3_cmd: PChar; pool: PAprPool): PSvnError; cdecl;
  svn_wc_merge_props: function(state: PSvnWCNotifyState; path: PChar; adm_access: PSvnWCAdmAccess; baseprops: PAprHash;
    propchanges: PAprArrayHeader; base_merge, dry_run: TSvnBoolean; pool: PAprPool): PSvnError; cdecl;
  svn_wc_merge_prop_diffs: function(state: PSvnWCNotifyState; path: PChar; adm_access: PSvnWCAdmAccess;
    propchanges: PAprArrayHeader; base_merge, dry_run: TSvnBoolean; pool: PAprPool): PSvnError; cdecl;
  svn_wc_get_pristine_copy_path: function(path: PChar; out pristine_path: PChar; pool: PAprPool): PSvnError; cdecl;
  svn_wc_cleanup2: function(path, diff3_cmd: PChar; cancel_func: TSvnCancelFunc; cancel_baton: Pointer;
    pool: PAprPool): PSvnError; cdecl;
  svn_wc_cleanup: function(path: PChar; optional_adm_access: PSvnWCAdmAccess; diff3_cmd: PChar;
    cancel_func: TSvnCancelFunc; cancel_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
  svn_wc_relocate: function(path: PChar; adm_access: PSvnWCAdmAccess; loc_from, loc_to: PChar; recurse: TSvnBoolean;
    validator: TSvnWCRelocationValidator; validator_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
  svn_wc_revert2: function(path: PChar; parent_access: PSvnWCAdmAccess; recursive, use_commit_times: TSvnBoolean;
    cancel_func: TSvnCancelFunc; cancel_baton: Pointer; notify_func: TSvnWCNotifyFunc2; notify_baton: Pointer;
    pool: PAprPool): PSvnError; cdecl;
  svn_wc_revert: function(path: PChar; parent_access: PSvnWCAdmAccess; recursive, use_commit_times: TSvnBoolean;
    cancel_func: TSvnCancelFunc; cancel_baton: Pointer; notify_func: TSvnWCNotifyFunc; notify_baton: Pointer;
    pool: Pointer): PSvnError; cdecl;
  svn_wc_create_tmp_file: function(out fp: PAprFile; path: PChar; delete_on_close: TSvnBoolean;
    pool: PAprPool): PSvnError; cdecl;
  svn_wc_translated_file: function(out xlated_p: PChar; vfile: PChar; adm_access: PSvnWCAdmAccess;
    force_repair: TSvnBoolean; pool: PAprPool): PSvnError; cdecl;
  svn_wc_transmit_text_deltas: function(path: PChar; adm_access: PSvnWCAdmAccess; fulltext: TSvnBoolean;
    editor: PSvnDeltaEditor; file_baton: Pointer; tempfile: PPChar; pool: PAprPool): PSvnError; cdecl;
  svn_wc_transmit_prop_deltas: function(path: PChar; adm_access: PSvnWCAdmAccess; entry: PSvnWCEntry;
    editor: PSvnDeltaEditor; baton: Pointer; tempfile: PPChar; pool: PAprPool): PSvnError; cdecl;
  svn_wc_get_default_ignores: function(out patterns: PAprArrayHeader; config: PAprHash; pool: PAprPool): PSvnError;
    cdecl;
  svn_wc_get_ignores: function(out patterns: PAprArrayHeader; config: PAprHash; adm_access: PSvnWCAdmAccess;
    pool: PAprPool): PSvnError; cdecl;
  svn_wc_add_lock: function(path: PChar; lock: PSvnLock; adm_access: PSvnWCAdmAccess; pool: PAprPool): PSvnError;
    cdecl;
  svn_wc_remove_lock: function(path: PChar; adm_access: PSvnWCAdmAccess; pool: PAprPool): PSvnError; cdecl;

//----- svn_wc.h -------------------------------------------------------------------------------------------------------

//----- svn_client.h ---------------------------------------------------------------------------------------------------

const
  SVN_CLIENT_COMMIT_ITEM_ADD         = $01;
  SVN_CLIENT_COMMIT_ITEM_DELETE      = $02;
  SVN_CLIENT_COMMIT_ITEM_TEXT_MODS   = $04;
  SVN_CLIENT_COMMIT_ITEM_PROP_MODS   = $08;
  SVN_CLIENT_COMMIT_ITEM_IS_COPY     = $10;
  SVN_CLIENT_COMMIT_ITEM_LOCK_TOKEN  = $20;

  SVN_CLIENT_AUTH_USERNAME = 'username';
  SVN_CLIENT_AUTH_PASSWORD = 'password';

type
  PPSvnClientPropListItem = ^PSvnClientPropListItem;
  PSvnClientPropListItem = ^TSvnClientPropListItem;
  TSvnClientPropListItem = record
    node_name: PSvnStringBuf;
    prop_hash: PAprHash;
  end;
  PSvnClientCommitInfo = ^TSvnClientCommitInfo;
  TSvnClientCommitInfo = record
    revision: TSvnRevNum;
    date: PChar;
    author: PChar;
  end;
  PSvnClientCommitItem2 = ^TSvnClientCommitItem2;
  TSvnClientCommitItem2 = record
    path: PChar;
    kind: TSvnNodeKind;
    url: PChar;
    revision: TSvnRevNum;
    copyfrom_url: PChar;
    copyfrom_rev: TSvnRevNum;
    state_flags: Byte;
    wcprop_changes: PAprArrayHeader;
  end;
  PSvnClientCommitItem = ^TSvnClientCommitItem;
  TSvnClientCommitItem = record
    path: PChar;
    kind: TSvnNodeKind;
    url: PChar;
    revision: TSvnRevNum;
    copyfrom_url: PChar;
    state_flags: Byte;
    wcprop_changes: PAprArrayHeader;
  end;
  TSvnClientGetCommitLog2 = function(out log_msg, tmp_file: PChar; commit_items: PAprArrayHeader; baton: Pointer;
    pool: PAprPool): PSvnError; cdecl;
  TSvnClientGetCommitLog = function(out log_msg, tmp_file: PChar; commit_items: PAprArrayHeader; baton: Pointer;
    pool: PAprPool): PSvnError; cdecl;
  TSvnClientBlameReceiver = function(baton: Pointer; line_no: Int64; revision: TSvnRevNum;
    author, date, line: PChar; pool: PAprPool): PSvnError; cdecl;
  PSvnClientCtx = ^TSvnClientCtx;
  TSvnClientCtx = record
    auth_baton: PSvnAuthBaton;
    notify_func: TSvnWCNotifyFunc;
    notify_baton: Pointer;
    log_msg_func: TSvnClientGetCommitLog;
    log_msg_baton: Pointer;
    config: PAprHash;
    cancel_func: TSvnCancelFunc;
    cancel_baton: Pointer;
    notify_func2: TSvnWCNotifyFunc2;
    notify_baton2: Pointer;
    log_msg_func2: TSvnClientGetCommitLog2;
    log_msg_baton2: Pointer;
    progress_func: TSvnRaProgressNotifyFunc;
    progress_baton: Pointer;
  end;
  PSvnInfo = ^TSvnInfo;
  TSvnInfo = record
    URL: PChar;
    rev: TSvnRevNum;
    kind: TSvnNodeKind;
    repos_root_URL: PChar;
    repos_UUID: PChar;
    last_changed_rev: TSvnRevNum;
    last_changed_date: TAprTime;
    last_changed_author: PChar;
    lock: PSvnLock;
    has_wc_info: TSvnBoolean;
    schedule: TSvnWCSchedule;
    copyfrom_url: PChar;
    copyfrom_rev: TSvnRevNum;
    text_time: TAprTime;
    prop_time: TAprTime;
    checksum: PChar;
    conflict_old: PChar;
    conflict_new: PChar;
    conflict_wrk: PChar;
    prejfile: PChar;
  end;
  TSvnInfoReceiver = function(baton: Pointer; path: PChar; const info: TSvnInfo; pool: PAprPool): PSvnError; cdecl;

var
  svn_client_version: function: PSvnVersion; cdecl;
  svn_client_get_simple_prompt_provider: procedure(out provider: PSvnAuthProviderObject;
    prompt_func: TSvnAuthSimplePromptFunc; prompt_baton: Pointer; retry_limit: Integer; pool: PAprPool); cdecl;
  svn_client_get_username_prompt_provider: procedure(out provider: PSvnAuthProviderObject;
    prompt_func: TSvnAuthUsernamePromptFunc; prompt_baton: Pointer; retry_limit: Integer; pool: PAprPool); cdecl;
  svn_client_get_simple_provider: procedure(out provider: PSvnAuthProviderObject; pool: PAprPool); cdecl;
  svn_client_get_windows_simple_provider: procedure(out provider: PSvnAuthProviderObject; pool: PAprPool); cdecl;
  svn_client_get_username_provider: procedure(out provider: PSvnAuthProviderObject; pool: PAprPool); cdecl;
  svn_client_get_ssl_server_trust_file_provider: procedure(out provider: PSvnAuthProviderObject; pool: PAprPool);
    cdecl;
  svn_client_get_ssl_client_cert_file_provider: procedure(out provider: PSvnAuthProviderObject; pool: PAprPool);
    cdecl;
  svn_client_get_ssl_client_cert_pw_file_provider: procedure(out provider: PSvnAuthProviderObject; pool: PAprPool);
    cdecl;
  svn_client_get_ssl_server_trust_prompt_provider: procedure(out provider: PSvnAuthProviderObject;
    prompt_func: TSvnAuthSSLServerTrustPromptFunc; prompt_baton: Pointer; pool: PAprPool); cdecl;
  svn_client_get_ssl_client_cert_prompt_provider: procedure(out provider: PSvnAuthProviderObject;
    prompt_func: TSvnAuthSSLClientCertPromptFunc; prompt_baton: Pointer; retry_limit: Integer; pool: PAprPool); cdecl;
  svn_client_get_ssl_client_cert_pw_prompt_provider: procedure(out provider: PSvnAuthProviderObject;
    prompt_func: TSvnAuthSSLClientCertPwPromptFunc; prompt_baton: Pointer; retry_limit: Integer; pool: PAprPool);
    cdecl;
  svn_client_proplist_item_dup: function(item: PSvnClientPropListItem; pool: PAprPool): PSvnClientPropListItem; cdecl;
  svn_client_commit_item2_dup: function(item: PSvnClientCommitItem2; pool: PAprPool): PSvnClientCommitItem2; cdecl;
  svn_client_create_context: function(out ctx: PSvnClientCtx; pool: PAprPool): PSvnError; cdecl;
  svn_client_checkout2: function(out result_rev: TSvnRevNum; URL, path: PChar; peg_revision, revision: PSvnOptRevision;
    recurse, ignore_externals: TSvnBoolean; ctx: PSvnClientCtx; pool: PAprPool): PSvnError; cdecl;
  svn_client_checkout: function(out result_rev: TSvnRevNum; URL, path: PChar; revision: PSvnOptRevision;
    recurse: TSvnBoolean; ctx: PSvnClientCtx; pool: PAprPool): PSvnError; cdecl;
  svn_client_update2: function(result_revs: PPAprArrayHeader; paths: PAprArrayHeader; revision: PSvnOptRevision;
    recurse, ignore_externals: TSvnBoolean; ctx: PSvnClientCtx; pool: PAprPool): PSvnError; cdecl;
  svn_client_update: function(out result_rev: TSvnRevNum; path: PChar; revision: PSvnOptRevision; recurse: TSvnBoolean;
    ctx: PSvnClientCtx; pool: PAprPool): PSvnError; cdecl;
  svn_client_switch: function(out result_rev: TSvnRevNum; path, url: PChar; revision: PSvnOptRevision;
    recurse: TSvnBoolean; ctx: PSvnClientCtx; pool: PAprPool): PSvnError; cdecl;
  svn_client_add3: function(path: PChar; recursive, force, no_ignore: TSvnBoolean; ctx: PSvnClientCtx;
    pool: PAprPool): PSvnError; cdecl;
  svn_client_add2: function(path: PChar; recursive, force: TSvnBoolean; ctx: PSvnClientCtx; pool: PAprPool): PSvnError;
    cdecl;
  svn_client_add: function(path: PChar; recursive: TSvnBoolean; ctx: PSvnClientCtx; pool: PAprPool): PSvnError; cdecl;
  svn_client_mkdir2: function(out commit_info_p: PSvnCommitInfo; paths: PAprArrayHeader; ctx: PSvnClientCtx;
    pool: PAprPool): PSvnError; cdecl;
  svn_client_mkdir: function(out commit_info_p: PSvnCommitInfo; paths: PAprArrayHeader; ctx: PSvnClientCtx;
    pool: PAprPool): PSvnError; cdecl;
  svn_client_delete2: function(out commit_info_p: PSvnCommitInfo; paths: PAprArrayHeader; force: TSvnBoolean;
    ctx: PSvnClientCtx; pool: PAprPool): PSvnError; cdecl;
  svn_client_delete: function(out commit_info_p: PSvnCommitInfo; paths: PAprArrayHeader; force: TSvnBoolean;
    ctx: PSvnClientCtx; pool: PAprPool): PSvnError; cdecl;
  svn_client_import2: function(out commit_info_p: PSvnCommitInfo; path, url: PChar;
    nonrecursive, no_ignore: TSvnBoolean; ctx: PSvnClientCtx; pool: PAprPool): PSvnError; cdecl;
  svn_client_import: function(out commit_info_p: PSvnCommitInfo; path, url: PChar; nonrecursive: TSvnBoolean;
    ctx: PSvnClientCtx; pool: PAprPool): PSvnError; cdecl;
  svn_client_commit3: function(out commit_info_p: PSvnCommitInfo; targets: PAprArrayHeader;
    recurse, keep_locks: TSvnBoolean; ctx: PSvnClientCtx; pool: PAprPool): PSvnError; cdecl;
  svn_client_commit2: function(out commit_info_p: PSvnCommitInfo; targets: PAprArrayHeader;
    recurse, keep_locks: TSvnBoolean; ctx: PSvnClientCtx; pool: PAprPool): PSvnError; cdecl;
  svn_client_commit: function(out commit_info_p: PSvnCommitInfo; targets: PAprArrayHeader;
    nonrecursive: TSvnBoolean; ctx: PSvnClientCtx; pool: PAprPool): PSvnError; cdecl;
  svn_client_status2: function(result_rev: PSvnRevNum; path: PChar; revision: PSvnOptRevision;
    status_func: TSvnWCStatusFunc2; status_baton: Pointer;
    recurse, get_all, update, no_ignore, ignore_externals: TSvnBoolean; ctx: PSvnClientCtx;
    pool: PAprPool): PSvnError; cdecl;
  svn_client_status: function(out result_rev: TSvnRevNum; path: PChar; revision: PSvnOptRevision;
    status_func: TSvnWCStatusFunc; status_baton: Pointer; recurse, get_all, update, no_ignore: TSvnBoolean;
    ctx: PSvnClientCtx; pool: PAprPool): PSvnError; cdecl;
  svn_client_log2: function(targets: PAprArrayHeader; rev_start, rev_end: PSvnOptRevision; limit: Integer;
    discover_changed_paths, strict_node_history: TSvnBoolean; receiver: TSvnLogMessageReceiver; receiver_baton: Pointer;
    ctx: PSvnClientCtx; pool: PAprPool): PSvnError; cdecl;
  svn_client_log: function(targets: PAprArrayHeader; rev_start, rev_end: PSvnOptRevision;
    discover_changed_paths, strict_node_history: TSvnBoolean; receiver: TSvnLogMessageReceiver; receiver_baton: Pointer;
    ctx: PSvnClientCtx; pool: PAprPool): PSvnError; cdecl;
  svn_client_blame2: function(path_or_url: PChar; peg_revision, rev_start, rev_end: PSvnOptRevision;
    receiver: TSvnClientBlameReceiver; receiver_baton: Pointer; ctx: PSvnClientCtx; pool: PAprPool): PSvnError; cdecl;
  svn_client_blame: function(path_or_url: PChar; rev_start, rev_end: PSvnOptRevision; receiver: TSvnClientBlameReceiver;
    receiver_baton: Pointer; ctx: PSvnClientCtx; pool: PAprPool): PSvnError; cdecl;
  svn_client_diff3: function(diff_options: PAprArrayHeader; path1: PChar; revision1: PSvnOptRevision; path2: PChar;
    revision2: PSvnOptRevision; recurse, ignore_ancestry, no_diff_deleted, ignore_content_type: TSvnBoolean;
    header_encoding: PChar; outfile, errfile: PAprFile; ctx: PSvnClientCtx; pool: PAprPool): PSvnError; cdecl;
  svn_client_diff2: function(diff_options: PAprArrayHeader; path1: PChar; revision1: PSvnOptRevision; path2: PChar;
    revision2: PSvnOptRevision; recurse, ignore_ancestry, no_diff_deleted, ignore_content_type: TSvnBoolean;
    outfile, errfile: PAprFile; ctx: PSvnClientCtx; pool: PAprPool): PSvnError; cdecl;
  svn_client_diff: function(diff_options: PAprArrayHeader; path1: PChar; revision1: PSvnOptRevision; path2: PChar;
    revision2: PSvnOptRevision; recurse, ignore_ancestry, no_diff_deleted: TSvnBoolean; outfile, errfile: PAprFile;
    ctx: PSvnClientCtx; pool: PAprPool): PSvnError; cdecl;
  svn_client_diff_peg3: function(diff_options: PAprArrayHeader; path: PChar;
    peg_revision, start_revision, end_revision: PSvnOptRevision;
    recurse, ignore_ancestry, no_diff_deleted, ignore_content_type: TSvnBoolean; header_encoding: PChar;
    outfile, errfile: PAprFile; ctx: PSvnClientCtx; pool: PAprPool): PSvnError; cdecl;
  svn_client_diff_peg2: function(diff_options: PAprArrayHeader; path: PChar;
    peg_revision, start_revision, end_revision: PSvnOptRevision;
    recurse, ignore_ancestry, no_diff_deleted, ignore_content_type: TSvnBoolean; outfile, errfile: PAprFile;
    ctx: PSvnClientCtx; pool: PAprPool): PSvnError; cdecl;
  svn_client_diff_peg: function(diff_options: PAprArrayHeader; path: PChar;
    peg_revision, start_revision, end_revision: PSvnOptRevision; recurse, ignore_ancestry, no_diff_deleted: TSvnBoolean;
    outfile, errfile: PAprFile; ctx: PSvnClientCtx; pool: PAprPool): PSvnError; cdecl;
  svn_client_merge: function(source1: PChar; revision1: PSvnOptRevision; source2: PChar; revision2: PSvnOptRevision;
    target_wcpath: PChar; recurse, ignore_ancestry, force, dry_run: TSvnBoolean; ctx: PSvnClientCtx;
    pool: PAprPool): PSvnError; cdecl;
  svn_client_merge_peg: function(source: PChar; revision1, revision2, peg_revision: PSvnOptRevision;
    target_wcpath: PChar; recurse, ignore_ancestry, force, dry_run: TSvnBoolean; ctx: PSvnClientCtx;
    pool: PAprPool): PSvnError; cdecl;
  svn_client_cleanup: function(dir: PChar; ctx: PSvnClientCtx; pool: PAprPool): PSvnError; cdecl;
  svn_client_relocate: function(dir, url_from, url_to: PChar; recurse: TSvnBoolean; ctx: PSvnClientCtx;
    pool: PAprPool): PSvnError; cdecl;
  svn_client_revert: function(paths: PAprArrayHeader; recursive: TSvnBoolean; ctx: PSvnClientCtx;
    pool: PAprPool): PSvnError; cdecl;
  svn_client_resolved: function(path: PChar; recursive: TSvnBoolean; ctx: PSvnClientCtx; pool: PAprPool): PSvnError;
    cdecl;
  svn_client_copy2: function(out commit_info_p: PSvnCommitInfo; src_path: PChar; src_revision: PSvnOptRevision;
    dst_path: PChar; ctx: PSvnClientCtx; pool: PAprPool): PSvnError; cdecl;
  svn_client_copy: function(out commit_info_p: PSvnClientCommitInfo; src_path: PChar; src_revision: PSvnOptRevision;
    dst_path: PChar; ctx: PSvnClientCtx; pool: PAprPool): PSvnError; cdecl;
  svn_client_move3: function(out commit_info_p: PSvnCommitInfo; src_path, dst_path: PChar; force: TSvnBoolean;
    ctx: PSvnClientCtx; pool: PAprPool): PSvnError; cdecl;
  svn_client_move2: function(out commit_info_p: PSvnClientCommitInfo; src_path, dst_path: PChar; force: TSvnBoolean;
    ctx: PSvnClientCtx; pool: PAprPool): PSvnError; cdecl;
  svn_client_move: function(out commit_info_p: PSvnClientCommitInfo; src_path: PChar; src_revision: PSvnOptRevision;
    dst_path: PChar; force: TSvnBoolean; ctx: PSvnClientCtx; pool: PAprPool): PSvnError; cdecl;
  svn_client_propset2: function(propname: PChar; propval: PSvnString; target: PChar; recurse, skip_checks: TSvnBoolean;
    ctx: PSvnClientCtx; pool: PAprPool): PSvnError; cdecl;
  svn_client_propset: function(propname: PChar; propval: PSvnString; target: PChar; recurse: TSvnBoolean;
    pool: PAprPool): PSvnError; cdecl;
  svn_client_revprop_set: function(propname: PChar; propval: PSvnString; URL: PChar; revision: PSvnOptRevision;
    out set_rev: TSvnRevNum; force: TSvnBoolean; ctx: PSvnClientCtx; pool: PAprPOol): PSvnError; cdecl;
  svn_client_propget2: function(out props: PAprHash; propname, target: PChar; peg_revision, revision: PSvnOptRevision;
    recurse: TSvnBoolean; ctx: PSvnClientCtx; pool: PAprPool): PSvnError; cdecl;
  svn_client_propget: function(out props: PAprHash; propname, target: PChar; revision: PSvnOptRevision;
    recurse: TSvnBoolean; ctx: PSvnClientCtx; pool: PAprPool): PSvnError; cdecl;
  svn_client_revprop_get: function(propname: PChar; out propval: PSvnString; URL: PChar; revision: PSvnOptRevision;
    out set_rev: TSvnRevNum; ctx: PSvnClientCtx; pool: PAprPool): PSvnError; cdecl;
  svn_client_proplist2: function(out props: PAprArrayHeader; target: PChar; peg_revision, revision: PSvnOptRevision;
    recurse: TSvnBoolean; ctx: PSvnClientCtx; pool: PAprPool): PSvnError; cdecl;
  svn_client_proplist: function(out props: PAprArrayHeader; target: PChar; revision: PSvnOptRevision;
    recurse: TSvnBoolean; ctx: PSvnClientCtx; pool: PAprPool): PSvnError; cdecl;
  svn_client_revprop_list: function(out props: PAprHash; URL: PChar; revision: PSvnOptRevision; out set_rev: TSvnRevNum;
    ctx: PSvnClientCtx; pool: PAprPool): PSvnError; cdecl;
  svn_client_export3: function(out result_rev: TSvnRevNum; dir_from, dir_to: PChar;
    peg_revision, revision: PSvnOptRevision; overwrite, ignore_externals, recurse: TSvnBoolean; native_eol: PChar;
    ctx: PSvnClientCtx; pool: PAprPool): PSvnError; cdecl;
  svn_client_export2: function(out result_rev: TSvnRevNum; dir_from, dir_to: PChar; revision: PSvnOptRevision;
    force: TSvnBoolean; native_eol: PChar; ctx: PSvnClientCtx; pool: PAprPool): PSvnError; cdecl;
  svn_client_export: function(out result_rev: TSvnRevNum; dir_from, dir_to: PChar; revision: PSvnOptRevision;
    force: TSvnBoolean; ctx: PSvnClientCtx; pool: PAprPool): PSvnError; cdecl;
  svn_client_ls3: function(out dirents, locks: PAprHash; path_or_url: PChar; peg_revision, revision: PSvnOptRevision;
    recurse: TSvnBoolean; ctx: PSvnClientCtx; pool: PAprPool): PSvnError; cdecl;
  svn_client_ls2: function(out dirents: PAprHash; path_or_url: PChar; peg_revision, revision: PSvnOptRevision;
    recurse: TSvnBoolean; ctx: PSvnClientCtx; pool: PAprPool): PSvnError; cdecl;
  svn_client_ls: function(out dirents: PAprHash; path_or_url: PChar; revision: PSvnOptRevision; recurse: TSvnBoolean;
    ctx: PSvnClientCtx; pool: PAprPool): PSvnError; cdecl;
  svn_client_cat2: function(strm_out: PSvnStream; path_or_url: PChar; peg_revision, revision: PSvnOptRevision;
    ctx: PSvnClientCtx; pool: PAprPool): PSvnError; cdecl;
  svn_client_cat: function(strm_out: PSvnStream; path_or_url: PChar; revision: PSvnOptRevision; ctx: PSvnClientCtx;
    pool: PAprPool): PSvnError; cdecl;
  svn_client_lock: function(targets: PAprArrayHeader; comment: PChar; steal_lock: TSvnBoolean; ctx: PSvnClientCtx;
    pool: PAprPool): PSvnError; cdecl;
  svn_client_unlock: function(targets: PAprArrayHeader; break_lock: TSvnBoolean; ctx: PSvnClientCtx;
    pool: PAprPool): PSvnError; cdecl;
  svn_info_dup: function(info: PSvnInfo; pool: PAprPool): PSvnInfo; cdecl;
  svn_client_info: function(path_or_url: PChar; peg_revision, revision: PSvnOptRevision; receiver: TSvnInfoReceiver;
    receiver_baton: Pointer; recurse: TSvnBoolean; ctx: PSvnClientCtx; pool: PAprPool): PSvnError; cdecl;
  svn_client_url_from_path: function(out url: PChar; path_or_url: PChar; pool: PAprPool): PSvnError; cdecl;
  svn_client_uuid_from_url: function(out uuid: PChar; url: PChar; ctx: PSvnClientCtx; pool: PAprPool): PSvnError;
    cdecl;
  svn_client_uuid_from_path: function(out uuid: PChar; path: PChar; adm_access: PSvnWCAdmAccess; ctx: PSvnClientCtx;
    pool: PAprPool): PSvnError; cdecl;
  svn_client_open_ra_session: function(out session: PSvnRaSession; url: PChar; ctx: PSvnClientCtx;
    pool: PAprPool): PSvnError; cdecl;

//----- svn_client.h ---------------------------------------------------------------------------------------------------

type
  TErrorData = record
    Code: Integer; // error code
    SPos: Integer; // substring start pos within Message
    SLen: Integer; // substring length
  end;
  ESvnError = class(EAprError)
  private
    FErrors: array of TErrorData;

    function GetCount: Integer;
    function GetErrorCodes(Index: Integer): Integer;
    function GetMessages(Index: Integer): string;
  public
    constructor Create(Error: PSvnError);
    destructor Destroy; override;

    property Count: Integer read GetCount;
    property ErrorCodes[Index: Integer]: Integer read GetErrorCodes;
    property Messages[Index: Integer]: string read GetMessages;
  end;

function SvnClientLibLoaded: Boolean;
function LoadSvnClientLib(const FileName: string = ''): Boolean;
procedure FreeSvnClientLib;

function GetSvnErrorMessage(Status: TAprStatus): string;
procedure SvnCheck(Error: PSvnError);
procedure RaiseSvnError(Error: PSvnError);

implementation

// svn_ctype.h

const
  SVN_CTYPE_CNTRL    = $0001; // Control character
  SVN_CTYPE_SPACE    = $0002; // Whitespace
  SVN_CTYPE_DIGIT    = $0004; // Decimal digit
  SVN_CTYPE_UPPER    = $0008; // Uppercase letter
  SVN_CTYPE_LOWER    = $0010; // Lowercase letter
  SVN_CTYPE_PUNCT    = $0020; // Punctuation mark
  SVN_CTYPE_XALPHA   = $0040; // Hexadecimal digits A to F
  SVN_CTYPE_ASCII    = $0080; // ASCII subset
  SVN_CTYPE_UTF8LEAD = $0100; // UTF-8 multibyte lead byte
  SVN_CTYPE_UTF8CONT = $0200; // UTF-8 multibyte non-lead byte

  SVN_CTYPE_UTF8MBC  = SVN_CTYPE_UTF8LEAD or SVN_CTYPE_UTF8CONT;
  SVN_CTYPE_UTF8     = SVN_CTYPE_ASCII or SVN_CTYPE_UTF8MBC;

  SVN_CTYPE_ALPHA    = SVN_CTYPE_LOWER or SVN_CTYPE_UPPER;
  SVN_CTYPE_ALNUM    = SVN_CTYPE_ALPHA or SVN_CTYPE_DIGIT;
  SVN_CTYPE_XDIGIT   = SVN_CTYPE_DIGIT or SVN_CTYPE_XALPHA;
  SVN_CTYPE_GRAPH    = SVN_CTYPE_PUNCT or SVN_CTYPE_ALNUM;
  SVN_CTYPE_PRINT    = SVN_CTYPE_GRAPH or SVN_CTYPE_SPACE;

  CTypeTable: array[Char] of Word = (
    SVN_CTYPE_ASCII or SVN_CTYPE_CNTRL, // nul
    SVN_CTYPE_ASCII or SVN_CTYPE_CNTRL, // soh
    SVN_CTYPE_ASCII or SVN_CTYPE_CNTRL, // stx
    SVN_CTYPE_ASCII or SVN_CTYPE_CNTRL, // etx
    SVN_CTYPE_ASCII or SVN_CTYPE_CNTRL, // eot
    SVN_CTYPE_ASCII or SVN_CTYPE_CNTRL, // enq
    SVN_CTYPE_ASCII or SVN_CTYPE_CNTRL, // ack
    SVN_CTYPE_ASCII or SVN_CTYPE_CNTRL, // bel
    SVN_CTYPE_ASCII or SVN_CTYPE_CNTRL, // bs
    SVN_CTYPE_ASCII or SVN_CTYPE_CNTRL or SVN_CTYPE_SPACE, // ht
    SVN_CTYPE_ASCII or SVN_CTYPE_CNTRL or SVN_CTYPE_SPACE, // nl
    SVN_CTYPE_ASCII or SVN_CTYPE_CNTRL or SVN_CTYPE_SPACE, // vt
    SVN_CTYPE_ASCII or SVN_CTYPE_CNTRL or SVN_CTYPE_SPACE, // np
    SVN_CTYPE_ASCII or SVN_CTYPE_CNTRL or SVN_CTYPE_SPACE, // cr
    SVN_CTYPE_ASCII or SVN_CTYPE_CNTRL, // so
    SVN_CTYPE_ASCII or SVN_CTYPE_CNTRL, // si
    SVN_CTYPE_ASCII or SVN_CTYPE_CNTRL, // dle
    SVN_CTYPE_ASCII or SVN_CTYPE_CNTRL, // dc1
    SVN_CTYPE_ASCII or SVN_CTYPE_CNTRL, // dc2
    SVN_CTYPE_ASCII or SVN_CTYPE_CNTRL, // dc3
    SVN_CTYPE_ASCII or SVN_CTYPE_CNTRL, // dc4
    SVN_CTYPE_ASCII or SVN_CTYPE_CNTRL, // nak
    SVN_CTYPE_ASCII or SVN_CTYPE_CNTRL, // syn
    SVN_CTYPE_ASCII or SVN_CTYPE_CNTRL, // etb
    SVN_CTYPE_ASCII or SVN_CTYPE_CNTRL, // can
    SVN_CTYPE_ASCII or SVN_CTYPE_CNTRL, // em
    SVN_CTYPE_ASCII or SVN_CTYPE_CNTRL, // sub
    SVN_CTYPE_ASCII or SVN_CTYPE_CNTRL, // esc
    SVN_CTYPE_ASCII or SVN_CTYPE_CNTRL, // fs
    SVN_CTYPE_ASCII or SVN_CTYPE_CNTRL, // gs
    SVN_CTYPE_ASCII or SVN_CTYPE_CNTRL, // rs
    SVN_CTYPE_ASCII or SVN_CTYPE_CNTRL, // us
    SVN_CTYPE_ASCII or SVN_CTYPE_SPACE, // sp
    SVN_CTYPE_ASCII or SVN_CTYPE_PUNCT, // !
    SVN_CTYPE_ASCII or SVN_CTYPE_PUNCT, // "
    SVN_CTYPE_ASCII or SVN_CTYPE_PUNCT, // #
    SVN_CTYPE_ASCII or SVN_CTYPE_PUNCT, // $
    SVN_CTYPE_ASCII or SVN_CTYPE_PUNCT, // %
    SVN_CTYPE_ASCII or SVN_CTYPE_PUNCT, // &
    SVN_CTYPE_ASCII or SVN_CTYPE_PUNCT, // '
    SVN_CTYPE_ASCII or SVN_CTYPE_PUNCT, // (
    SVN_CTYPE_ASCII or SVN_CTYPE_PUNCT, // )
    SVN_CTYPE_ASCII or SVN_CTYPE_PUNCT, // *
    SVN_CTYPE_ASCII or SVN_CTYPE_PUNCT, // +
    SVN_CTYPE_ASCII or SVN_CTYPE_PUNCT, // ,
    SVN_CTYPE_ASCII or SVN_CTYPE_PUNCT, // -
    SVN_CTYPE_ASCII or SVN_CTYPE_PUNCT, // .
    SVN_CTYPE_ASCII or SVN_CTYPE_PUNCT, // /
    SVN_CTYPE_ASCII or SVN_CTYPE_DIGIT, // 0
    SVN_CTYPE_ASCII or SVN_CTYPE_DIGIT, // 1
    SVN_CTYPE_ASCII or SVN_CTYPE_DIGIT, // 2
    SVN_CTYPE_ASCII or SVN_CTYPE_DIGIT, // 3
    SVN_CTYPE_ASCII or SVN_CTYPE_DIGIT, // 4
    SVN_CTYPE_ASCII or SVN_CTYPE_DIGIT, // 5
    SVN_CTYPE_ASCII or SVN_CTYPE_DIGIT, // 6
    SVN_CTYPE_ASCII or SVN_CTYPE_DIGIT, // 7
    SVN_CTYPE_ASCII or SVN_CTYPE_DIGIT, // 8
    SVN_CTYPE_ASCII or SVN_CTYPE_DIGIT, // 9
    SVN_CTYPE_ASCII or SVN_CTYPE_PUNCT, // :
    SVN_CTYPE_ASCII or SVN_CTYPE_PUNCT, // ;
    SVN_CTYPE_ASCII or SVN_CTYPE_PUNCT, // <
    SVN_CTYPE_ASCII or SVN_CTYPE_PUNCT, // =
    SVN_CTYPE_ASCII or SVN_CTYPE_PUNCT, // >
    SVN_CTYPE_ASCII or SVN_CTYPE_PUNCT, // ?
    SVN_CTYPE_ASCII or SVN_CTYPE_PUNCT, // @
    SVN_CTYPE_ASCII or SVN_CTYPE_UPPER or SVN_CTYPE_XALPHA, // A
    SVN_CTYPE_ASCII or SVN_CTYPE_UPPER or SVN_CTYPE_XALPHA, // B
    SVN_CTYPE_ASCII or SVN_CTYPE_UPPER or SVN_CTYPE_XALPHA, // C
    SVN_CTYPE_ASCII or SVN_CTYPE_UPPER or SVN_CTYPE_XALPHA, // D
    SVN_CTYPE_ASCII or SVN_CTYPE_UPPER or SVN_CTYPE_XALPHA, // E
    SVN_CTYPE_ASCII or SVN_CTYPE_UPPER or SVN_CTYPE_XALPHA, // F
    SVN_CTYPE_ASCII or SVN_CTYPE_UPPER, // G
    SVN_CTYPE_ASCII or SVN_CTYPE_UPPER, // H
    SVN_CTYPE_ASCII or SVN_CTYPE_UPPER, // I
    SVN_CTYPE_ASCII or SVN_CTYPE_UPPER, // J
    SVN_CTYPE_ASCII or SVN_CTYPE_UPPER, // K
    SVN_CTYPE_ASCII or SVN_CTYPE_UPPER, // L
    SVN_CTYPE_ASCII or SVN_CTYPE_UPPER, // M
    SVN_CTYPE_ASCII or SVN_CTYPE_UPPER, // N
    SVN_CTYPE_ASCII or SVN_CTYPE_UPPER, // O
    SVN_CTYPE_ASCII or SVN_CTYPE_UPPER, // P
    SVN_CTYPE_ASCII or SVN_CTYPE_UPPER, // Q
    SVN_CTYPE_ASCII or SVN_CTYPE_UPPER, // R
    SVN_CTYPE_ASCII or SVN_CTYPE_UPPER, // S
    SVN_CTYPE_ASCII or SVN_CTYPE_UPPER, // T
    SVN_CTYPE_ASCII or SVN_CTYPE_UPPER, // U
    SVN_CTYPE_ASCII or SVN_CTYPE_UPPER, // V
    SVN_CTYPE_ASCII or SVN_CTYPE_UPPER, // W
    SVN_CTYPE_ASCII or SVN_CTYPE_UPPER, // X
    SVN_CTYPE_ASCII or SVN_CTYPE_UPPER, // Y
    SVN_CTYPE_ASCII or SVN_CTYPE_UPPER, // Z
    SVN_CTYPE_ASCII or SVN_CTYPE_PUNCT, // [
    SVN_CTYPE_ASCII or SVN_CTYPE_PUNCT, // \
    SVN_CTYPE_ASCII or SVN_CTYPE_PUNCT, // ]
    SVN_CTYPE_ASCII or SVN_CTYPE_PUNCT, // ^
    SVN_CTYPE_ASCII or SVN_CTYPE_PUNCT, // _
    SVN_CTYPE_ASCII or SVN_CTYPE_PUNCT, // `
    SVN_CTYPE_ASCII or SVN_CTYPE_LOWER or SVN_CTYPE_XALPHA, // a
    SVN_CTYPE_ASCII or SVN_CTYPE_LOWER or SVN_CTYPE_XALPHA, // b
    SVN_CTYPE_ASCII or SVN_CTYPE_LOWER or SVN_CTYPE_XALPHA, // c
    SVN_CTYPE_ASCII or SVN_CTYPE_LOWER or SVN_CTYPE_XALPHA, // d
    SVN_CTYPE_ASCII or SVN_CTYPE_LOWER or SVN_CTYPE_XALPHA, // e
    SVN_CTYPE_ASCII or SVN_CTYPE_LOWER or SVN_CTYPE_XALPHA, // f
    SVN_CTYPE_ASCII or SVN_CTYPE_LOWER, // g
    SVN_CTYPE_ASCII or SVN_CTYPE_LOWER, // h
    SVN_CTYPE_ASCII or SVN_CTYPE_LOWER, // i
    SVN_CTYPE_ASCII or SVN_CTYPE_LOWER, // j
    SVN_CTYPE_ASCII or SVN_CTYPE_LOWER, // k
    SVN_CTYPE_ASCII or SVN_CTYPE_LOWER, // l
    SVN_CTYPE_ASCII or SVN_CTYPE_LOWER, // m
    SVN_CTYPE_ASCII or SVN_CTYPE_LOWER, // n
    SVN_CTYPE_ASCII or SVN_CTYPE_LOWER, // o
    SVN_CTYPE_ASCII or SVN_CTYPE_LOWER, // p
    SVN_CTYPE_ASCII or SVN_CTYPE_LOWER, // q
    SVN_CTYPE_ASCII or SVN_CTYPE_LOWER, // r
    SVN_CTYPE_ASCII or SVN_CTYPE_LOWER, // s
    SVN_CTYPE_ASCII or SVN_CTYPE_LOWER, // t
    SVN_CTYPE_ASCII or SVN_CTYPE_LOWER, // u
    SVN_CTYPE_ASCII or SVN_CTYPE_LOWER, // v
    SVN_CTYPE_ASCII or SVN_CTYPE_LOWER, // w
    SVN_CTYPE_ASCII or SVN_CTYPE_LOWER, // x
    SVN_CTYPE_ASCII or SVN_CTYPE_LOWER, // y
    SVN_CTYPE_ASCII or SVN_CTYPE_LOWER, // z
    SVN_CTYPE_ASCII or SVN_CTYPE_PUNCT, // {
    SVN_CTYPE_ASCII or SVN_CTYPE_PUNCT, // |
    SVN_CTYPE_ASCII or SVN_CTYPE_PUNCT, // }
    SVN_CTYPE_ASCII or SVN_CTYPE_PUNCT, // ~
    SVN_CTYPE_ASCII or SVN_CTYPE_CNTRL, // del
    SVN_CTYPE_UTF8CONT, // x80
    SVN_CTYPE_UTF8CONT, // x81
    SVN_CTYPE_UTF8CONT, // x82
    SVN_CTYPE_UTF8CONT, // x83
    SVN_CTYPE_UTF8CONT, // x84
    SVN_CTYPE_UTF8CONT, // x85
    SVN_CTYPE_UTF8CONT, // x86
    SVN_CTYPE_UTF8CONT, // x87
    SVN_CTYPE_UTF8CONT, // x88
    SVN_CTYPE_UTF8CONT, // x89
    SVN_CTYPE_UTF8CONT, // x8a
    SVN_CTYPE_UTF8CONT, // x8b
    SVN_CTYPE_UTF8CONT, // x8c
    SVN_CTYPE_UTF8CONT, // x8d
    SVN_CTYPE_UTF8CONT, // x8e
    SVN_CTYPE_UTF8CONT, // x8f
    SVN_CTYPE_UTF8CONT, // x90
    SVN_CTYPE_UTF8CONT, // x91
    SVN_CTYPE_UTF8CONT, // x92
    SVN_CTYPE_UTF8CONT, // x93
    SVN_CTYPE_UTF8CONT, // x94
    SVN_CTYPE_UTF8CONT, // x95
    SVN_CTYPE_UTF8CONT, // x96
    SVN_CTYPE_UTF8CONT, // x97
    SVN_CTYPE_UTF8CONT, // x98
    SVN_CTYPE_UTF8CONT, // x99
    SVN_CTYPE_UTF8CONT, // x9a
    SVN_CTYPE_UTF8CONT, // x9b
    SVN_CTYPE_UTF8CONT, // x9c
    SVN_CTYPE_UTF8CONT, // x9d
    SVN_CTYPE_UTF8CONT, // x9e
    SVN_CTYPE_UTF8CONT, // x9f
    SVN_CTYPE_UTF8CONT, // xa0
    SVN_CTYPE_UTF8CONT, // xa1
    SVN_CTYPE_UTF8CONT, // xa2
    SVN_CTYPE_UTF8CONT, // xa3
    SVN_CTYPE_UTF8CONT, // xa4
    SVN_CTYPE_UTF8CONT, // xa5
    SVN_CTYPE_UTF8CONT, // xa6
    SVN_CTYPE_UTF8CONT, // xa7
    SVN_CTYPE_UTF8CONT, // xa8
    SVN_CTYPE_UTF8CONT, // xa9
    SVN_CTYPE_UTF8CONT, // xaa
    SVN_CTYPE_UTF8CONT, // xab
    SVN_CTYPE_UTF8CONT, // xac
    SVN_CTYPE_UTF8CONT, // xad
    SVN_CTYPE_UTF8CONT, // xae
    SVN_CTYPE_UTF8CONT, // xaf
    SVN_CTYPE_UTF8CONT, // xb0
    SVN_CTYPE_UTF8CONT, // xb1
    SVN_CTYPE_UTF8CONT, // xb2
    SVN_CTYPE_UTF8CONT, // xb3
    SVN_CTYPE_UTF8CONT, // xb4
    SVN_CTYPE_UTF8CONT, // xb5
    SVN_CTYPE_UTF8CONT, // xb6
    SVN_CTYPE_UTF8CONT, // xb7
    SVN_CTYPE_UTF8CONT, // xb8
    SVN_CTYPE_UTF8CONT, // xb9
    SVN_CTYPE_UTF8CONT, // xba
    SVN_CTYPE_UTF8CONT, // xbb
    SVN_CTYPE_UTF8CONT, // xbc
    SVN_CTYPE_UTF8CONT, // xbd
    SVN_CTYPE_UTF8CONT, // xbe
    SVN_CTYPE_UTF8CONT, // xbf
    0, // xc0
    SVN_CTYPE_UTF8LEAD, // xc1
    SVN_CTYPE_UTF8LEAD, // xc2
    SVN_CTYPE_UTF8LEAD, // xc3
    SVN_CTYPE_UTF8LEAD, // xc4
    SVN_CTYPE_UTF8LEAD, // xc5
    SVN_CTYPE_UTF8LEAD, // xc6
    SVN_CTYPE_UTF8LEAD, // xc7
    SVN_CTYPE_UTF8LEAD, // xc8
    SVN_CTYPE_UTF8LEAD, // xc9
    SVN_CTYPE_UTF8LEAD, // xca
    SVN_CTYPE_UTF8LEAD, // xcb
    SVN_CTYPE_UTF8LEAD, // xcc
    SVN_CTYPE_UTF8LEAD, // xcd
    SVN_CTYPE_UTF8LEAD, // xce
    SVN_CTYPE_UTF8LEAD, // xcf
    SVN_CTYPE_UTF8LEAD, // xd0
    SVN_CTYPE_UTF8LEAD, // xd1
    SVN_CTYPE_UTF8LEAD, // xd2
    SVN_CTYPE_UTF8LEAD, // xd3
    SVN_CTYPE_UTF8LEAD, // xd4
    SVN_CTYPE_UTF8LEAD, // xd5
    SVN_CTYPE_UTF8LEAD, // xd6
    SVN_CTYPE_UTF8LEAD, // xd7
    SVN_CTYPE_UTF8LEAD, // xd8
    SVN_CTYPE_UTF8LEAD, // xd9
    SVN_CTYPE_UTF8LEAD, // xda
    SVN_CTYPE_UTF8LEAD, // xdb
    SVN_CTYPE_UTF8LEAD, // xdc
    SVN_CTYPE_UTF8LEAD, // xdd
    SVN_CTYPE_UTF8LEAD, // xde
    SVN_CTYPE_UTF8LEAD, // xdf
    0, // xe0
    SVN_CTYPE_UTF8LEAD, // xe1
    SVN_CTYPE_UTF8LEAD, // xe2
    SVN_CTYPE_UTF8LEAD, // xe3
    SVN_CTYPE_UTF8LEAD, // xe4
    SVN_CTYPE_UTF8LEAD, // xe5
    SVN_CTYPE_UTF8LEAD, // xe6
    SVN_CTYPE_UTF8LEAD, // xe7
    SVN_CTYPE_UTF8LEAD, // xe8
    SVN_CTYPE_UTF8LEAD, // xe9
    SVN_CTYPE_UTF8LEAD, // xea
    SVN_CTYPE_UTF8LEAD, // xeb
    SVN_CTYPE_UTF8LEAD, // xec
    SVN_CTYPE_UTF8LEAD, // xed
    SVN_CTYPE_UTF8LEAD, // xee
    SVN_CTYPE_UTF8LEAD, // xef
    0, // xf0
    SVN_CTYPE_UTF8LEAD, // xf1
    SVN_CTYPE_UTF8LEAD, // xf2
    SVN_CTYPE_UTF8LEAD, // xf3
    SVN_CTYPE_UTF8LEAD, // xf4
    SVN_CTYPE_UTF8LEAD, // xf5
    SVN_CTYPE_UTF8LEAD, // xf6
    SVN_CTYPE_UTF8LEAD, // xf7
    0, // xf8
    SVN_CTYPE_UTF8LEAD, // xf9
    SVN_CTYPE_UTF8LEAD, // xfa
    SVN_CTYPE_UTF8LEAD, // xfb
    0, // xfc
    SVN_CTYPE_UTF8LEAD, // xfd
    0, // xfe
    0 // xff
  );

function svn_ctype_iscntrl(C: Char): Boolean;
begin
  Result := CTypeTable[C] and SVN_CTYPE_CNTRL <> 0;
end;

function svn_ctype_isspace(C: Char): Boolean;
begin
  Result := CTypeTable[C] and SVN_CTYPE_SPACE <> 0;
end;

function svn_ctype_isdigit(C: Char): Boolean;
begin
  Result := CTypeTable[C] and SVN_CTYPE_DIGIT <> 0;
end;

function svn_ctype_isupper(C: Char): Boolean;
begin
  Result := CTypeTable[C] and SVN_CTYPE_UPPER <> 0;
end;

function svn_ctype_islower(C: Char): Boolean;
begin
  Result := CTypeTable[C] and SVN_CTYPE_LOWER <> 0;
end;

function svn_ctype_ispunct(C: Char): Boolean;
begin
  Result := CTypeTable[C] and SVN_CTYPE_PUNCT <> 0;
end;

function svn_ctype_isascii(C: Char): Boolean;
begin
  Result := CTypeTable[C] and SVN_CTYPE_ASCII <> 0;
end;

function svn_ctype_isalpha(C: Char): Boolean;
begin
  Result := CTypeTable[C] and SVN_CTYPE_ALPHA <> 0;
end;

function svn_ctype_isalnum(C: Char): Boolean;
begin
  Result := CTypeTable[C] and SVN_CTYPE_ALNUM <> 0;
end;

function svn_ctype_isxdigit(C: Char): Boolean;
begin
  Result := CTypeTable[C] and SVN_CTYPE_XDIGIT <> 0;
end;

function svn_ctype_isgraph(C: Char): Boolean;
begin
  Result := CTypeTable[C] and SVN_CTYPE_GRAPH <> 0;
end;

function svn_ctype_isprint(C: Char): Boolean;
begin
  Result := CTypeTable[C] and SVN_CTYPE_PRINT <> 0;
end;

function svn_ctype_isutf8lead(C: Char): Boolean;
begin
  Result := CTypeTable[C] and SVN_CTYPE_UTF8LEAD <> 0;
end;

function svn_ctype_isutf8cont(C: Char): Boolean;
begin
  Result := CTypeTable[C] and SVN_CTYPE_UTF8CONT <> 0;
end;

function svn_ctype_isutf8mbc(C: Char): Boolean;
begin
  Result := CTypeTable[C] and SVN_CTYPE_UTF8MBC <> 0;
end;

function svn_ctype_isutf8(C: Char): Boolean;
begin
  Result := CTypeTable[C] and SVN_CTYPE_UTF8 <> 0;
end;

//----------------------------------------------------------------------------------------------------------------------

function SvnIsLockError(err: TSvnError): Boolean;

begin
  Result := (err.apr_err = SVN_ERR_FS_PATH_ALREADY_LOCKED) or (err.apr_err = SVN_ERR_FS_OUT_OF_DATE);
end;

//----------------------------------------------------------------------------------------------------------------------

function SvnIsUnlockError(err: TSvnError): Boolean;

begin
  Result := (err.apr_err = SVN_ERR_FS_PATH_NOT_LOCKED) or
    (err.apr_err = SVN_ERR_FS_BAD_LOCK_TOKEN) or
    (err.apr_err = SVN_ERR_FS_LOCK_OWNER_MISMATCH) or
    (err.apr_err = SVN_ERR_FS_NO_SUCH_LOCK) or
    (err.apr_err = SVN_ERR_RA_NOT_LOCKED) or
    (err.apr_err = SVN_ERR_FS_LOCK_EXPIRED);
end;

//----------------------------------------------------------------------------------------------------------------------

var
  SvnClientLib: THandle = INVALID_HANDLE_VALUE;

//----------------------------------------------------------------------------------------------------------------------

function SvnClientLibLoaded: Boolean;

begin
  Result := SvnClientLib <> INVALID_HANDLE_VALUE;
end;

//----------------------------------------------------------------------------------------------------------------------

function LoadSvnClientLib(const FileName: string = ''): Boolean;

var
  LibFileName: string;

begin
  Result := not SvnClientLibLoaded;

  if Result then
  begin
    if FileName = '' then
      LibFileName := 'libsvn_client.dll'
    else
      LibFileName := FileName;

    SvnClientLib := LoadLibrary(PChar(LibFileName));
    Result := SvnClientLib <> 0;
    if not Result then
      SvnClientLib := INVALID_HANDLE_VALUE
    else
    begin
      // svn_types.h
      @svn_create_commit_info := GetProcAddress(SvnClientLib, 'svn_create_commit_info');
      @svn_log_changed_path_dup := GetProcAddress(SvnClientLib, 'svn_log_changed_path_dup');
      @svn_mime_type_validate := GetProcAddress(SvnClientLib, 'svn_mime_type_validate');
      @svn_mime_type_is_binary := GetProcAddress(SvnClientLib, 'svn_mime_type_is_binary');
      @svn_lock_create := GetProcAddress(SvnClientLib, 'svn_lock_create');
      @svn_lock_dup := GetProcAddress(SvnClientLib, 'svn_lock_dup');
      // svn_nls.h
      @svn_nls_init := GetProcAddress(SvnClientLib, 'svn_nls_init');
      // svn_version.h
      @svn_ver_compatible := GetProcAddress(SvnClientLib, 'svn_ver_compatible');
      @svn_ver_equal := GetProcAddress(SvnClientLib, 'svn_ver_equal');
      @svn_ver_check_list := GetProcAddress(SvnClientLib, 'svn_ver_check_list');
      @svn_subr_version := GetProcAddress(SvnClientLib, 'svn_subr_version');
      // svn_time.h
      @svn_time_to_cstring := GetProcAddress(SvnClientLib, 'svn_time_to_cstring');
      @svn_time_from_cstring := GetProcAddress(SvnClientLib, 'svn_time_from_cstring');
      @svn_time_to_human_cstring := GetProcAddress(SvnClientLib, 'svn_time_to_human_cstring');
      @svn_parse_date := GetProcAddress(SvnClientLib, 'svn_parse_date');
      @svn_sleep_for_timestamps := GetProcAddress(SvnClientLib, 'svn_sleep_for_timestamps');
      // svn_pools.h
      @svn_pool_create_ex := GetProcAddress(SvnClientLib, 'svn_pool_create_ex');
      // svn_sorts.h
      @svn_sort_compare_items_as_paths := GetProcAddress(SvnClientLib, 'svn_sort_compare_items_as_paths');
      @svn_sort_compare_items_lexically := GetProcAddress(SvnClientLib, 'svn_sort_compare_items_lexically');
      @svn_sort_compare_revisions := GetProcAddress(SvnClientLib, 'svn_sort_compare_revisions');
      @svn_sort_compare_paths := GetProcAddress(SvnClientLib, 'svn_sort_compare_paths');
      @svn_sort__hash := GetProcAddress(SvnClientLib, 'svn_sort__hash');
      // svn_md5.h
      @svn_md5_empty_string_digest := GetProcAddress(SvnClientLib, 'svn_md5_empty_string_digest');
      @svn_md5_digest_to_cstring_display := GetProcAddress(SvnClientLib, 'svn_md5_digest_to_cstring_display');
      @svn_md5_digest_to_cstring := GetProcAddress(SvnClientLib, 'svn_md5_digest_to_cstring');
      @svn_md5_digests_match := GetProcAddress(SvnClientLib, 'svn_md5_digests_match');
      // svn_string.h
      @svn_string_create := GetProcAddress(SvnClientLib, 'svn_string_create');
      @svn_string_ncreate := GetProcAddress(SvnClientLib, 'svn_string_ncreate');
      @svn_string_create_from_buf := GetProcAddress(SvnClientLib, 'svn_string_create_from_buf');
      @svn_string_createf := GetProcAddress(SvnClientLib, 'svn_string_createf');
      @svn_string_createv := GetProcAddress(SvnClientLib, 'svn_string_createv');
      @svn_string_isempty := GetProcAddress(SvnClientLib, 'svn_string_isempty');
      @svn_string_dup := GetProcAddress(SvnClientLib, 'svn_string_dup');
      @svn_string_compare := GetProcAddress(SvnClientLib, 'svn_string_compare');
      @svn_string_first_non_whitespace := GetProcAddress(SvnClientLib, 'svn_string_first_non_whitespace');
      @svn_string_find_char_backward := GetProcAddress(SvnClientLib, 'svn_string_find_char_backward');
      @svn_stringbuf_create := GetProcAddress(SvnClientLib, 'svn_stringbuf_create');
      @svn_stringbuf_ncreate := GetProcAddress(SvnClientLib, 'svn_stringbuf_ncreate');
      @svn_stringbuf_create_from_string := GetProcAddress(SvnClientLib, 'svn_stringbuf_create_from_string');
      @svn_stringbuf_createf := GetProcAddress(SvnClientLib, 'svn_stringbuf_createf');
      @svn_stringbuf_createv := GetProcAddress(SvnClientLib, 'svn_stringbuf_createv');
      @svn_stringbuf_ensure := GetProcAddress(SvnClientLib, 'svn_stringbuf_ensure');
      @svn_stringbuf_set := GetProcAddress(SvnClientLib, 'svn_stringbuf_set');
      @svn_stringbuf_setempty := GetProcAddress(SvnClientLib, 'svn_stringbuf_setempty');
      @svn_stringbuf_isempty := GetProcAddress(SvnClientLib, 'svn_stringbuf_isempty');
      @svn_stringbuf_chop := GetProcAddress(SvnClientLib, 'svn_stringbuf_chop');
      @svn_stringbuf_fillchar := GetProcAddress(SvnClientLib, 'svn_stringbuf_fillchar');
      @svn_stringbuf_appendbytes := GetProcAddress(SvnClientLib, 'svn_stringbuf_appendbytes');
      @svn_stringbuf_appendstr := GetProcAddress(SvnClientLib, 'svn_stringbuf_appendstr');
      @svn_stringbuf_appendcstr := GetProcAddress(SvnClientLib, 'svn_stringbuf_appendcstr');
      @svn_stringbuf_dup := GetProcAddress(SvnClientLib, 'svn_stringbuf_dup');
      @svn_stringbuf_compare := GetProcAddress(SvnClientLib, 'svn_stringbuf_compare');
      @svn_stringbuf_first_non_whitespace := GetProcAddress(SvnClientLib, 'svn_stringbuf_first_non_whitespace');
      @svn_stringbuf_strip_whitespace := GetProcAddress(SvnClientLib, 'svn_stringbuf_strip_whitespace');
      @svn_stringbuf_find_char_backward := GetProcAddress(SvnClientLib, 'svn_stringbuf_find_char_backward');
      @svn_string_compare_stringbuf := GetProcAddress(SvnClientLib, 'svn_string_compare_stringbuf');
      @svn_cstring_split := GetProcAddress(SvnClientLib, 'svn_cstring_split');
      @svn_cstring_split_append := GetProcAddress(SvnClientLib, 'svn_cstring_split_append');
      @svn_cstring_match_glob_list := GetProcAddress(SvnClientLib, 'svn_cstring_match_glob_list');
      @svn_cstring_count_newlines := GetProcAddress(SvnClientLib, 'svn_cstring_count_newlines');
      @svn_cstring_join := GetProcAddress(SvnClientLib, 'svn_cstring_join');
      // svn_xml.h
      @svn_xml_is_xml_safe := GetProcAddress(SvnClientLib, 'svn_xml_is_xml_safe');
      @svn_xml_escape_cdata_stringbuf := GetProcAddress(SvnClientLib, 'svn_xml_escape_cdata_stringbuf');
      @svn_xml_escape_cdata_string := GetProcAddress(SvnClientLib, 'svn_xml_escape_cdata_string');
      @svn_xml_escape_cdata_cstring := GetProcAddress(SvnClientLib, 'svn_xml_escape_cdata_cstring');
      @svn_xml_escape_attr_stringbuf := GetProcAddress(SvnClientLib, 'svn_xml_escape_attr_stringbuf');
      @svn_xml_escape_attr_string := GetProcAddress(SvnClientLib, 'svn_xml_escape_attr_string');
      @svn_xml_escape_attr_cstring := GetProcAddress(SvnClientLib, 'svn_xml_escape_attr_cstring');
      @svn_xml_fuzzy_escape := GetProcAddress(SvnClientLib, 'svn_xml_fuzzy_escape');
      @svn_xml_make_parser := GetProcAddress(SvnClientLib, 'svn_xml_make_parser');
      @svn_xml_free_parser := GetProcAddress(SvnClientLib, 'svn_xml_free_parser');
      @svn_xml_parse := GetProcAddress(SvnClientLib, 'svn_xml_parse');
      @svn_xml_signal_bailout := GetProcAddress(SvnClientLib, 'svn_xml_signal_bailout');
      @svn_xml_get_attr_value := GetProcAddress(SvnClientLib, 'svn_xml_get_attr_value');
      @svn_xml_ap_to_hash := GetProcAddress(SvnClientLib, 'svn_xml_ap_to_hash');
      @svn_xml_make_att_hash := GetProcAddress(SvnClientLib, 'svn_xml_make_att_hash');
      @svn_xml_hash_atts_preserving := GetProcAddress(SvnClientLib, 'svn_xml_hash_atts_preserving');
      @svn_xml_hash_atts_overlaying := GetProcAddress(SvnClientLib, 'svn_xml_hash_atts_overlaying');
      @svn_xml_make_header := GetProcAddress(SvnClientLib, 'svn_xml_make_header');
      @svn_xml_make_open_tag := GetProcAddress(SvnClientLib, 'svn_xml_make_open_tag');
      @svn_xml_make_open_tag_v := GetProcAddress(SvnClientLib, 'svn_xml_make_open_tag_v');
      @svn_xml_make_open_tag_hash := GetProcAddress(SvnClientLib, 'svn_xml_make_open_tag_hash');
      @svn_xml_make_close_tag := GetProcAddress(SvnClientLib, 'svn_xml_make_close_tag');
      // svn_cmdline.h
      @svn_cmdline_init := GetProcAddress(SvnClientLib, 'svn_cmdline_init');
      @svn_cmdline_cstring_from_utf8 := GetProcAddress(SvnClientLib, 'svn_cmdline_cstring_from_utf8');
      @svn_cmdline_cstring_from_utf8_fuzzy := GetProcAddress(SvnClientLib, 'svn_cmdline_cstring_from_utf8_fuzzy');
      @svn_cmdline_cstring_to_utf8 := GetProcAddress(SvnClientLib, 'svn_cmdline_cstring_to_utf8');
      @svn_cmdline_path_local_style_from_utf8 := GetProcAddress(SvnClientLib, 'svn_cmdline_path_local_style_from_utf8');
      @svn_cmdline_printf := GetProcAddress(SvnClientLib, 'svn_cmdline_printf');
      @svn_cmdline_fprintf := GetProcAddress(SvnClientLib, 'svn_cmdline_fprintf');
      @svn_cmdline_fputs := GetProcAddress(SvnClientLib, 'svn_cmdline_fputs');
      @svn_cmdline_fflush := GetProcAddress(SvnClientLib, 'svn_cmdline_fflush');
      @svn_cmdline_output_encoding := GetProcAddress(SvnClientLib, 'svn_cmdline_output_encoding');
      @svn_cmdline_handle_exit_error := GetProcAddress(SvnClientLib, 'svn_cmdline_handle_exit_error');
      // svn_utf.h
      @svn_utf_initialize := GetProcAddress(SvnClientLib, 'svn_utf_initialize');
      @svn_utf_stringbuf_to_utf8 := GetProcAddress(SvnClientLib, 'svn_utf_stringbuf_to_utf8');
      @svn_utf_string_to_utf8 := GetProcAddress(SvnClientLib, 'svn_utf_string_to_utf8');
      @svn_utf_cstring_to_utf8 := GetProcAddress(SvnClientLib, 'svn_utf_cstring_to_utf8');
      @svn_utf_cstring_to_utf8_ex := GetProcAddress(SvnClientLib, 'svn_utf_cstring_to_utf8_ex');
      @svn_utf_stringbuf_from_utf8 := GetProcAddress(SvnClientLib, 'svn_utf_stringbuf_from_utf8');
      @svn_utf_string_from_utf8 := GetProcAddress(SvnClientLib, 'svn_utf_string_from_utf8');
      @svn_utf_cstring_from_utf8 := GetProcAddress(SvnClientLib, 'svn_utf_cstring_from_utf8');
      @svn_utf_cstring_from_utf8_ex := GetProcAddress(SvnClientLib, 'svn_utf_cstring_from_utf8_ex');
      @svn_utf_cstring_from_utf8_fuzzy := GetProcAddress(SvnClientLib, 'svn_utf_cstring_from_utf8_fuzzy');
      @svn_utf_cstring_from_utf8_stringbuf := GetProcAddress(SvnClientLib, 'svn_utf_cstring_from_utf8_stringbuf');
      @svn_utf_cstring_from_utf8_string := GetProcAddress(SvnClientLib, 'svn_utf_cstring_from_utf8_string');
      // svn_props.h
      @svn_prop_dup := GetProcAddress(SvnClientLib, 'svn_prop_dup');
      @svn_prop_array_dup := GetProcAddress(SvnClientLib, 'svn_prop_array_dup');
      @svn_property_kind := GetProcAddress(SvnClientLib, 'svn_property_kind');
      @svn_prop_is_svn_prop := GetProcAddress(SvnClientLib, 'svn_prop_is_svn_prop');
      @svn_prop_needs_translation := GetProcAddress(SvnClientLib, 'svn_prop_needs_translation');
      @svn_categorize_props := GetProcAddress(SvnClientLib, 'svn_categorize_props');
      @svn_prop_diffs := GetProcAddress(SvnClientLib, 'svn_prop_diffs');
      // svn_error.h
      @svn_error__locate := GetProcAddress(SvnClientLib, 'svn_error__locate');
      @svn_strerror := GetProcAddress(SvnClientLib, 'svn_strerror');
      @svn_error_create := GetProcAddress(SvnClientLib, 'svn_error_create');
      @svn_error_createf := GetProcAddress(SvnClientLib, 'svn_error_createf');
      @svn_error_wrap_apr := GetProcAddress(SvnClientLib, 'svn_error_wrap_apr');
      @svn_error_quick_wrap := GetProcAddress(SvnClientLib, 'svn_error_quick_wrap');
      @svn_error_compose := GetProcAddress(SvnClientLib, 'svn_error_compose');
      @svn_error_dup := GetProcAddress(SvnClientLib, 'svn_error_dup');
      @svn_error_clear := GetProcAddress(SvnClientLib, 'svn_error_clear');
      @svn_handle_error2 := GetProcAddress(SvnClientLib, 'svn_handle_error2');
      @svn_handle_error := GetProcAddress(SvnClientLib, 'svn_handle_error');
      @svn_handle_warning2 := GetProcAddress(SvnClientLib, 'svn_handle_warning2');
      @svn_handle_warning := GetProcAddress(SvnClientLib, 'svn_handle_warning');
      // svn_path.h
      @svn_path_internal_style := GetProcAddress(SvnClientLib, 'svn_path_internal_style');
      @svn_path_local_style := GetProcAddress(SvnClientLib, 'svn_path_local_style');
      @svn_path_join := GetProcAddress(SvnClientLib, 'svn_path_join');
      @svn_path_join_many := GetProcAddress(SvnClientLib, 'svn_path_join_many');
      @svn_path_basename := GetProcAddress(SvnClientLib, 'svn_path_basename');
      @svn_path_dirname := GetProcAddress(SvnClientLib, 'svn_path_dirname');
      @svn_path_component_count := GetProcAddress(SvnClientLib, 'svn_path_component_count');
      @svn_path_add_component := GetProcAddress(SvnClientLib, 'svn_path_add_component');
      @svn_path_remove_component := GetProcAddress(SvnClientLib, 'svn_path_remove_component');
      @svn_path_remove_components := GetProcAddress(SvnClientLib, 'svn_path_remove_components');
      @svn_path_split := GetProcAddress(SvnClientLib, 'svn_path_split');
      @svn_path_is_empty := GetProcAddress(SvnClientLib, 'svn_path_is_empty');
      @svn_path_canonicalize := GetProcAddress(SvnClientLib, 'svn_path_canonicalize');
      @svn_path_compare_paths := GetProcAddress(SvnClientLib, 'svn_path_compare_paths');
      @svn_path_get_longest_ancestor := GetProcAddress(SvnClientLib, 'svn_path_get_longest_ancestor');
      @svn_path_get_absolute := GetProcAddress(SvnClientLib, 'svn_path_get_absolute');
      @svn_path_split_if_file := GetProcAddress(SvnClientLib, 'svn_path_split_if_file');
      @svn_path_condense_targets := GetProcAddress(SvnClientLib, 'svn_path_condense_targets');
      @svn_path_remove_redundancies := GetProcAddress(SvnClientLib, 'svn_path_remove_redundancies');
      @svn_path_decompose := GetProcAddress(SvnClientLib, 'svn_path_decompose');
      @svn_path_is_single_path_component := GetProcAddress(SvnClientLib, 'svn_path_is_single_path_component');
      @svn_path_is_backpath_present := GetProcAddress(SvnClientLib, 'svn_path_is_backpath_present');
      @svn_path_is_child := GetProcAddress(SvnClientLib, 'svn_path_is_child');
      @svn_path_is_ancestor := GetProcAddress(SvnClientLib, 'svn_path_is_ancestor');
      @svn_path_check_valid := GetProcAddress(SvnClientLib, 'svn_path_check_valid');
      @svn_path_is_url := GetProcAddress(SvnClientLib, 'svn_path_is_url');
      @svn_path_is_uri_safe := GetProcAddress(SvnClientLib, 'svn_path_is_uri_safe');
      @svn_path_uri_encode := GetProcAddress(SvnClientLib, 'svn_path_uri_encode');
      @svn_path_uri_decode := GetProcAddress(SvnClientLib, 'svn_path_uri_decode');
      @svn_path_url_add_component := GetProcAddress(SvnClientLib, 'svn_path_url_add_component');
      @svn_path_uri_from_iri := GetProcAddress(SvnClientLib, 'svn_path_uri_from_iri');
      @svn_path_uri_autoescape := GetProcAddress(SvnClientLib, 'svn_path_uri_autoescape');
      @svn_path_cstring_from_utf8 := GetProcAddress(SvnClientLib, 'svn_path_cstring_from_utf8');
      @svn_path_cstring_to_utf8 := GetProcAddress(SvnClientLib, 'svn_path_cstring_to_utf8');
      // svn_config.h
      @svn_config_get_config := GetProcAddress(SvnClientLib, 'svn_config_get_config');
      @svn_config_read := GetProcAddress(SvnClientLib, 'svn_config_read');
      @svn_config_merge := GetProcAddress(SvnClientLib, 'svn_config_merge');
      @svn_config_get := GetProcAddress(SvnClientLib, 'svn_config_get');
      @svn_config_set := GetProcAddress(SvnClientLib, 'svn_config_set');
      @svn_config_get_bool := GetProcAddress(SvnClientLib, 'svn_config_get_bool');
      @svn_config_set_bool := GetProcAddress(SvnClientLib, 'svn_config_set_bool');
      @svn_config_enumerate_sections := GetProcAddress(SvnClientLib, 'svn_config_enumerate_sections');
      @svn_config_enumerate_sections2 := GetProcAddress(SvnClientLib, 'svn_config_enumerate_sections2');
      @svn_config_enumerate := GetProcAddress(SvnClientLib, 'svn_config_enumerate');
      @svn_config_enumerate2 := GetProcAddress(SvnClientLib, 'svn_config_enumerate2');
      @svn_config_find_group := GetProcAddress(SvnClientLib, 'svn_config_find_group');
      @svn_config_get_server_setting := GetProcAddress(SvnClientLib, 'svn_config_get_server_setting');
      @svn_config_get_server_setting_int := GetProcAddress(SvnClientLib, 'svn_config_get_server_setting_int');
      @svn_config_ensure := GetProcAddress(SvnClientLib, 'svn_config_ensure');
      @svn_config_read_auth_data := GetProcAddress(SvnClientLib, 'svn_config_read_auth_data');
      @svn_config_write_auth_data := GetProcAddress(SvnClientLib, 'svn_config_write_auth_data');
      // svn_io.h
      @svn_io_check_path := GetProcAddress(SvnClientLib, 'svn_io_check_path');
      @svn_io_check_special_path := GetProcAddress(SvnClientLib, 'svn_io_check_special_path');
      @svn_io_check_resolved_path := GetProcAddress(SvnClientLib, 'svn_io_check_resolved_path');
      @svn_io_open_unique_file := GetProcAddress(SvnClientLib, 'svn_io_open_unique_file');
      @svn_io_create_unique_link := GetProcAddress(SvnClientLib, 'svn_io_create_unique_link');
      @svn_io_read_link := GetProcAddress(SvnClientLib, 'svn_io_read_link');
      @svn_io_temp_dir := GetProcAddress(SvnClientLib, 'svn_io_temp_dir');
      @svn_io_copy_file := GetProcAddress(SvnClientLib, 'svn_io_copy_file');
      @svn_io_copy_link := GetProcAddress(SvnClientLib, 'svn_io_copy_link');
      @svn_io_copy_dir_recursively := GetProcAddress(SvnClientLib, 'svn_io_copy_dir_recursively');
      @svn_io_make_dir_recursively := GetProcAddress(SvnClientLib, 'svn_io_make_dir_recursively');
      @svn_io_dir_empty := GetProcAddress(SvnClientLib, 'svn_io_dir_empty');
      @svn_io_append_file := GetProcAddress(SvnClientLib, 'svn_io_append_file');
      @svn_io_set_file_read_only := GetProcAddress(SvnClientLib, 'svn_io_set_file_read_only');
      @svn_io_set_file_read_write := GetProcAddress(SvnClientLib, 'svn_io_set_file_read_write');
      @svn_io_set_file_read_write_carefully := GetProcAddress(SvnClientLib, 'svn_io_set_file_read_write_carefully');
      @svn_io_set_file_executable := GetProcAddress(SvnClientLib, 'svn_io_set_file_executable');
      @svn_io_is_file_executable := GetProcAddress(SvnClientLib, 'svn_io_is_file_executable');
      @svn_io_read_length_line := GetProcAddress(SvnClientLib, 'svn_io_read_length_line');
      @svn_io_file_affected_time := GetProcAddress(SvnClientLib, 'svn_io_file_affected_time');
      @svn_io_set_file_affected_time := GetProcAddress(SvnClientLib, 'svn_io_set_file_affected_time');
      @svn_io_filesizes_different_p := GetProcAddress(SvnClientLib, 'svn_io_filesizes_different_p');
      @svn_io_file_checksum := GetProcAddress(SvnClientLib, 'svn_io_file_checksum');
      @svn_io_files_contents_same_p := GetProcAddress(SvnClientLib, 'svn_io_files_contents_same_p');
      @svn_io_file_create := GetProcAddress(SvnClientLib, 'svn_io_file_create');
      @svn_io_file_lock := GetProcAddress(SvnClientLib, 'svn_io_file_lock');
      @svn_io_file_lock2 := GetProcAddress(SvnClientLib, 'svn_io_file_lock2');
      @svn_io_file_flush_to_disk := GetProcAddress(SvnClientLib, 'svn_io_file_flush_to_disk');
      @svn_io_dir_file_copy := GetProcAddress(SvnClientLib, 'svn_io_dir_file_copy');
      @svn_stream_create := GetProcAddress(SvnClientLib, 'svn_stream_create');
      @svn_stream_set_baton := GetProcAddress(SvnClientLib, 'svn_stream_set_baton');
      @svn_stream_set_read := GetProcAddress(SvnClientLib, 'svn_stream_set_read');
      @svn_stream_set_write := GetProcAddress(SvnClientLib, 'svn_stream_set_write');
      @svn_stream_set_close := GetProcAddress(SvnClientLib, 'svn_stream_set_close');
      @svn_stream_empty := GetProcAddress(SvnClientLib, 'svn_stream_empty');
      @svn_stream_from_aprfile := GetProcAddress(SvnClientLib, 'svn_stream_from_aprfile');
      @svn_stream_for_stdout := GetProcAddress(SvnClientLib, 'svn_stream_for_stdout');
      @svn_stream_from_stringbuf := GetProcAddress(SvnClientLib, 'svn_stream_from_stringbuf');
      @svn_stream_compressed := GetProcAddress(SvnClientLib, 'svn_stream_compressed');
      @svn_stream_read := GetProcAddress(SvnClientLib, 'svn_stream_read');
      @svn_stream_write := GetProcAddress(SvnClientLib, 'svn_stream_write');
      @svn_stream_close := GetProcAddress(SvnClientLib, 'svn_stream_close');
      @svn_stream_printf := GetProcAddress(SvnClientLib, 'svn_stream_printf');
      @svn_stream_printf_from_utf8 := GetProcAddress(SvnClientLib, 'svn_stream_printf_from_utf8');
      @svn_stream_readline := GetProcAddress(SvnClientLib, 'svn_stream_readline');
      @svn_stream_copy := GetProcAddress(SvnClientLib, 'svn_stream_copy');
      @svn_stringbuf_from_file := GetProcAddress(SvnClientLib, 'svn_stringbuf_from_file');
      @svn_stringbuf_from_aprfile := GetProcAddress(SvnClientLib, 'svn_stringbuf_from_aprfile');
      @svn_io_remove_file := GetProcAddress(SvnClientLib, 'svn_io_remove_file');
      @svn_io_remove_dir := GetProcAddress(SvnClientLib, 'svn_io_remove_dir');
      @svn_io_get_dirents2 := GetProcAddress(SvnClientLib, 'svn_io_get_dirents2');
      @svn_io_get_dirents := GetProcAddress(SvnClientLib, 'svn_io_get_dirents');
      @svn_io_dir_walk := GetProcAddress(SvnClientLib, 'svn_io_dir_walk');
      @svn_io_start_cmd := GetProcAddress(SvnClientLib, 'svn_io_start_cmd');
      @svn_io_wait_for_cmd := GetProcAddress(SvnClientLib, 'svn_io_wait_for_cmd');
      @svn_io_run_cmd := GetProcAddress(SvnClientLib, 'svn_io_run_cmd');
      @svn_io_run_diff := GetProcAddress(SvnClientLib, 'svn_io_run_diff');
      @svn_io_run_diff3 := GetProcAddress(SvnClientLib, 'svn_io_run_diff3');
      @svn_io_detect_mimetype := GetProcAddress(SvnClientLib, 'svn_io_detect_mimetype');
      @svn_io_file_open := GetProcAddress(SvnClientLib, 'svn_io_file_open');
      @svn_io_file_close := GetProcAddress(SvnClientLib, 'svn_io_file_close');
      @svn_io_file_getc := GetProcAddress(SvnClientLib, 'svn_io_file_getc');
      @svn_io_file_info_get := GetProcAddress(SvnClientLib, 'svn_io_file_info_get');
      @svn_io_file_read := GetProcAddress(SvnClientLib, 'svn_io_file_read');
      @svn_io_file_read_full := GetProcAddress(SvnClientLib, 'svn_io_file_read_full');
      @svn_io_file_seek := GetProcAddress(SvnClientLib, 'svn_io_file_seek');
      @svn_io_file_write := GetProcAddress(SvnClientLib, 'svn_io_file_write');
      @svn_io_file_write_full := GetProcAddress(SvnClientLib, 'svn_io_file_write_full');
      @svn_io_stat := GetProcAddress(SvnClientLib, 'svn_io_stat');
      @svn_io_file_rename := GetProcAddress(SvnClientLib, 'svn_io_file_rename');
      @svn_io_file_move := GetProcAddress(SvnClientLib, 'svn_io_file_move');
      @svn_io_dir_make := GetProcAddress(SvnClientLib, 'svn_io_dir_make');
      @svn_io_dir_make_hidden := GetProcAddress(SvnClientLib, 'svn_io_dir_make_hidden');
      @svn_io_dir_make_sgid := GetProcAddress(SvnClientLib, 'svn_io_dir_make_sgid');
      @svn_io_dir_open := GetProcAddress(SvnClientLib, 'svn_io_dir_open');
      @svn_io_dir_remove_nonrecursive := GetProcAddress(SvnClientLib, 'svn_io_dir_remove_nonrecursive');
      @svn_io_dir_read := GetProcAddress(SvnClientLib, 'svn_io_dir_read');
      @svn_io_read_version_file := GetProcAddress(SvnClientLib, 'svn_io_read_version_file');
      @svn_io_write_version_file := GetProcAddress(SvnClientLib, 'svn_io_write_version_file');
      // svn_quoprint.h
      @svn_quoprint_encode := GetProcAddress(SvnClientLib, 'svn_quoprint_encode');
      @svn_quoprint_decode := GetProcAddress(SvnClientLib, 'svn_quoprint_decode');
      @svn_quoprint_encode_string := GetProcAddress(SvnClientLib, 'svn_quoprint_encode_string');
      @svn_quoprint_decode_string := GetProcAddress(SvnClientLib, 'svn_quoprint_decode_string');
      // svn_hash.h
      @svn_hash_read2 := GetProcAddress(SvnClientLib, 'svn_hash_read2');
      @svn_hash_write2 := GetProcAddress(SvnClientLib, 'svn_hash_write2');
      @svn_hash_read_incremental := GetProcAddress(SvnClientLib, 'svn_hash_read_incremental');
      @svn_hash_write_incremental := GetProcAddress(SvnClientLib, 'svn_hash_write_incremental');
      @svn_hash_read := GetProcAddress(SvnClientLib, 'svn_hash_read');
      @svn_hash_write := GetProcAddress(SvnClientLib, 'svn_hash_write');
      @svn_hash_diff := GetProcAddress(SvnClientLib, 'svn_hash_diff');
      // svn_subst.h
      @svn_subst_eol_style_from_value := GetProcAddress(SvnClientLib, 'svn_subst_eol_style_from_value');
      @svn_subst_build_keywords2 := GetProcAddress(SvnClientLib, 'svn_subst_build_keywords2');
      @svn_subst_build_keywords := GetProcAddress(SvnClientLib, 'svn_subst_build_keywords');
      @svn_subst_keywords_differ2 := GetProcAddress(SvnClientLib, 'svn_subst_keywords_differ2');
      @svn_subst_keywords_differ := GetProcAddress(SvnClientLib, 'svn_subst_keywords_differ');
      @svn_subst_translate_stream3 := GetProcAddress(SvnClientLib, 'svn_subst_translate_stream3');
      @svn_subst_translate_stream2 := GetProcAddress(SvnClientLib, 'svn_subst_translate_stream2');
      @svn_subst_translate_stream := GetProcAddress(SvnClientLib, 'svn_subst_translate_stream');
      @svn_subst_copy_and_translate3 := GetProcAddress(SvnClientLib, 'svn_subst_copy_and_translate3');
      @svn_subst_copy_and_translate2 := GetProcAddress(SvnClientLib, 'svn_subst_copy_and_translate2');
      @svn_subst_copy_and_translate := GetProcAddress(SvnClientLib, 'svn_subst_copy_and_translate');
      @svn_subst_translate_cstring2 := GetProcAddress(SvnClientLib, 'svn_subst_translate_cstring2');
      @svn_subst_translate_cstring := GetProcAddress(SvnClientLib, 'svn_subst_translate_cstring');
      @svn_subst_translate_string := GetProcAddress(SvnClientLib, 'svn_subst_translate_string');
      @svn_subst_detranslate_string := GetProcAddress(SvnClientLib, 'svn_subst_detranslate_string');
      // svn_diff.h
      @svn_diff_version := GetProcAddress(SvnClientLib, 'svn_diff_version');
      @svn_diff_diff := GetProcAddress(SvnClientLib, 'svn_diff_diff');
      @svn_diff_diff3 := GetProcAddress(SvnClientLib, 'svn_diff_diff3');
      @svn_diff_diff4 := GetProcAddress(SvnClientLib, 'svn_diff_diff4');
      @svn_diff_contains_conflicts := GetProcAddress(SvnClientLib, 'svn_diff_contains_conflicts');
      @svn_diff_contains_diffs := GetProcAddress(SvnClientLib, 'svn_diff_contains_diffs');
      @svn_diff_output := GetProcAddress(SvnClientLib, 'svn_diff_output');
      @svn_diff_file_diff := GetProcAddress(SvnClientLib, 'svn_diff_file_diff');
      @svn_diff_file_diff3 := GetProcAddress(SvnClientLib, 'svn_diff_file_diff3');
      @svn_diff_file_diff4 := GetProcAddress(SvnClientLib, 'svn_diff_file_diff4');
      @svn_diff_file_output_unified2 := GetProcAddress(SvnClientLib, 'svn_diff_file_output_unified2');
      @svn_diff_file_output_unified := GetProcAddress(SvnClientLib, 'svn_diff_file_output_unified');
      @svn_diff_file_output_merge := GetProcAddress(SvnClientLib, 'svn_diff_file_output_merge');
      // svn_base64.h
      @svn_base64_encode := GetProcAddress(SvnClientLib, 'svn_base64_encode');
      @svn_base64_decode := GetProcAddress(SvnClientLib, 'svn_base64_decode');
      @svn_base64_encode_string := GetProcAddress(SvnClientLib, 'svn_base64_encode_string');
      @svn_base64_decode_string := GetProcAddress(SvnClientLib, 'svn_base64_decode_string');
      @svn_base64_from_md5 := GetProcAddress(SvnClientLib, 'svn_base64_from_md5');
      // svn_delta.h
      @svn_delta_version := GetProcAddress(SvnClientLib, 'svn_delta_version');
      @svn_txdelta_window_dup := GetProcAddress(SvnClientLib, 'svn_txdelta_window_dup');
      @svn_txdelta_next_window := GetProcAddress(SvnClientLib, 'svn_txdelta_next_window');
      @svn_txdelta_md5_digest := GetProcAddress(SvnClientLib, 'svn_txdelta_md5_digest');
      @svn_txdelta := GetProcAddress(SvnClientLib, 'svn_txdelta');
      @svn_txdelta_target_push := GetProcAddress(SvnClientLib, 'svn_txdelta_target_push');
      @svn_txdelta_send_string := GetProcAddress(SvnClientLib, 'svn_txdelta_send_string');
      @svn_txdelta_send_stream := GetProcAddress(SvnClientLib, 'svn_txdelta_send_stream');
      @svn_txdelta_send_txstream := GetProcAddress(SvnClientLib, 'svn_txdelta_send_txstream');
      @svn_txdelta_apply := GetProcAddress(SvnClientLib, 'svn_txdelta_apply');
      @svn_txdelta_to_svndiff := GetProcAddress(SvnClientLib, 'svn_txdelta_to_svndiff');
      @svn_txdelta_parse_svndiff := GetProcAddress(SvnClientLib, 'svn_txdelta_parse_svndiff');
      @svn_txdelta_read_svndiff_window := GetProcAddress(SvnClientLib, 'svn_txdelta_read_svndiff_window');
      @svn_txdelta_skip_svndiff_window := GetProcAddress(SvnClientLib, 'svn_txdelta_skip_svndiff_window');
      @svn_delta_default_editor := GetProcAddress(SvnClientLib, 'svn_delta_default_editor');
      @svn_delta_noop_window_handler := GetProcAddress(SvnClientLib, 'svn_delta_noop_window_handler');
      @svn_delta_get_cancellation_editor := GetProcAddress(SvnClientLib, 'svn_delta_get_cancellation_editor');
      @svn_delta_path_driver := GetProcAddress(SvnClientLib, 'svn_delta_path_driver');
      // svn_fs.h
      @svn_fs_version := GetProcAddress(SvnClientLib, 'svn_fs_version');
      @svn_fs_initialize := GetProcAddress(SvnClientLib, 'svn_fs_initialize');
      @svn_fs_set_warning_func := GetProcAddress(SvnClientLib, 'svn_fs_set_warning_func');
      @svn_fs_create := GetProcAddress(SvnClientLib, 'svn_fs_create');
      @svn_fs_open := GetProcAddress(SvnClientLib, 'svn_fs_open');
      @svn_fs_type := GetProcAddress(SvnClientLib, 'svn_fs_type');
      @svn_fs_path := GetProcAddress(SvnClientLib, 'svn_fs_path');
      @svn_fs_delete_fs := GetProcAddress(SvnClientLib, 'svn_fs_delete_fs');
      @svn_fs_hotcopy := GetProcAddress(SvnClientLib, 'svn_fs_hotcopy');
      @svn_fs_set_berkeley_errcall := GetProcAddress(SvnClientLib, 'svn_fs_set_berkeley_errcall');
      @svn_fs_berkeley_recover := GetProcAddress(SvnClientLib, 'svn_fs_berkeley_recover');
      @svn_fs_berkeley_logfiles := GetProcAddress(SvnClientLib, 'svn_fs_berkeley_logfiles');
      @svn_fs_new := GetProcAddress(SvnClientLib, 'svn_fs_new');
      @svn_fs_create_berkeley := GetProcAddress(SvnClientLib, 'svn_fs_create_berkeley');
      @svn_fs_open_berkeley := GetProcAddress(SvnClientLib, 'svn_fs_open_berkeley');
      @svn_fs_berkeley_path := GetProcAddress(SvnClientLib, 'svn_fs_berkeley_path');
      @svn_fs_delete_berkeley := GetProcAddress(SvnClientLib, 'svn_fs_delete_berkeley');
      @svn_fs_hotcopy_berkeley := GetProcAddress(SvnClientLib, 'svn_fs_hotcopy_berkeley');
      @svn_fs_create_access := GetProcAddress(SvnClientLib, 'svn_fs_create_access');
      @svn_fs_set_access := GetProcAddress(SvnClientLib, 'svn_fs_set_access');
      @svn_fs_get_access := GetProcAddress(SvnClientLib, 'svn_fs_get_access');
      @svn_fs_access_get_username := GetProcAddress(SvnClientLib, 'svn_fs_access_get_username');
      @svn_fs_access_add_lock_token := GetProcAddress(SvnClientLib, 'svn_fs_access_add_lock_token');
      @svn_fs_compare_ids := GetProcAddress(SvnClientLib, 'svn_fs_compare_ids');
      @svn_fs_check_related := GetProcAddress(SvnClientLib, 'svn_fs_check_related');
      @svn_fs_parse_id := GetProcAddress(SvnClientLib, 'svn_fs_parse_id');
      @svn_fs_unparse_id := GetProcAddress(SvnClientLib, 'svn_fs_unparse_id');
      @svn_fs_begin_txn2 := GetProcAddress(SvnClientLib, 'svn_fs_begin_txn2');
      @svn_fs_begin_txn := GetProcAddress(SvnClientLib, 'svn_fs_begin_txn');
      @svn_fs_commit_txn := GetProcAddress(SvnClientLib, 'svn_fs_commit_txn');
      @svn_fs_abort_txn := GetProcAddress(SvnClientLib, 'svn_fs_abort_txn');
      @svn_fs_purge_txn := GetProcAddress(SvnClientLib, 'svn_fs_purge_txn');
      @svn_fs_txn_name := GetProcAddress(SvnClientLib, 'svn_fs_txn_name');
      @svn_fs_txn_base_revision := GetProcAddress(SvnClientLib, 'svn_fs_txn_base_revision');
      @svn_fs_open_txn := GetProcAddress(SvnClientLib, 'svn_fs_open_txn');
      @svn_fs_list_transactions := GetProcAddress(SvnClientLib, 'svn_fs_list_transactions');
      @svn_fs_txn_prop := GetProcAddress(SvnClientLib, 'svn_fs_txn_prop');
      @svn_fs_txn_proplist := GetProcAddress(SvnClientLib, 'svn_fs_txn_proplist');
      @svn_fs_change_txn_prop := GetProcAddress(SvnClientLib, 'svn_fs_change_txn_prop');
      @svn_fs_revision_root := GetProcAddress(SvnClientLib, 'svn_fs_revision_root');
      @svn_fs_txn_root := GetProcAddress(SvnClientLib, 'svn_fs_txn_root');
      @svn_fs_close_root := GetProcAddress(SvnClientLib, 'svn_fs_close_root');
      @svn_fs_root_fs := GetProcAddress(SvnClientLib, 'svn_fs_root_fs');
      @svn_fs_is_txn_root := GetProcAddress(SvnClientLib, 'svn_fs_is_txn_root');
      @svn_fs_is_revision_root := GetProcAddress(SvnClientLib, 'svn_fs_is_revision_root');
      @svn_fs_txn_root_name := GetProcAddress(SvnClientLib, 'svn_fs_txn_root_name');
      @svn_fs_revision_root_revision := GetProcAddress(SvnClientLib, 'svn_fs_revision_root_revision');
      @svn_fs_paths_changed := GetProcAddress(SvnClientLib, 'svn_fs_paths_changed');
      @svn_fs_check_path := GetProcAddress(SvnClientLib, 'svn_fs_check_path');
      @svn_fs_node_history := GetProcAddress(SvnClientLib, 'svn_fs_node_history');
      @svn_fs_history_prev := GetProcAddress(SvnClientLib, 'svn_fs_history_prev');
      @svn_fs_history_location := GetProcAddress(SvnClientLib, 'svn_fs_history_location');
      @svn_fs_is_dir := GetProcAddress(SvnClientLib, 'svn_fs_is_dir');
      @svn_fs_is_file := GetProcAddress(SvnClientLib, 'svn_fs_is_file');
      @svn_fs_node_id := GetProcAddress(SvnClientLib, 'svn_fs_node_id');
      @svn_fs_node_created_rev := GetProcAddress(SvnClientLib, 'svn_fs_node_created_rev');
      @svn_fs_node_created_path := GetProcAddress(SvnClientLib, 'svn_fs_node_created_path');
      @svn_fs_node_prop := GetProcAddress(SvnClientLib, 'svn_fs_node_prop');
      @svn_fs_node_proplist := GetProcAddress(SvnClientLib, 'svn_fs_node_proplist');
      @svn_fs_change_node_prop := GetProcAddress(SvnClientLib, 'svn_fs_change_node_prop');
      @svn_fs_props_changed := GetProcAddress(SvnClientLib, 'svn_fs_props_changed');
      @svn_fs_copied_from := GetProcAddress(SvnClientLib, 'svn_fs_copied_from');
      @svn_fs_closest_copy := GetProcAddress(SvnClientLib, 'svn_fs_closest_copy');
      @svn_fs_merge := GetProcAddress(SvnClientLib, 'svn_fs_merge');
      @svn_fs_dir_entries := GetProcAddress(SvnClientLib, 'svn_fs_dir_entries');
      @svn_fs_make_dir := GetProcAddress(SvnClientLib, 'svn_fs_make_dir');
      @svn_fs_delete := GetProcAddress(SvnClientLib, 'svn_fs_delete');
      @svn_fs_copy := GetProcAddress(SvnClientLib, 'svn_fs_copy');
      @svn_fs_revision_link := GetProcAddress(SvnClientLib, 'svn_fs_revision_link');
      @svn_fs_file_length := GetProcAddress(SvnClientLib, 'svn_fs_file_length');
      @svn_fs_file_md5_checksum := GetProcAddress(SvnClientLib, 'svn_fs_file_md5_checksum');
      @svn_fs_file_contents := GetProcAddress(SvnClientLib, 'svn_fs_file_contents');
      @svn_fs_make_file := GetProcAddress(SvnClientLib, 'svn_fs_make_file');
      @svn_fs_apply_textdelta := GetProcAddress(SvnClientLib, 'svn_fs_apply_textdelta');
      @svn_fs_apply_text := GetProcAddress(SvnClientLib, 'svn_fs_apply_text');
      @svn_fs_contents_changed := GetProcAddress(SvnClientLib, 'svn_fs_contents_changed');
      @svn_fs_youngest_rev := GetProcAddress(SvnClientLib, 'svn_fs_youngest_rev');
      @svn_fs_deltify_revision := GetProcAddress(SvnClientLib, 'svn_fs_deltify_revision');
      @svn_fs_revision_prop := GetProcAddress(SvnClientLib, 'svn_fs_revision_prop');
      @svn_fs_revision_proplist := GetProcAddress(SvnClientLib, 'svn_fs_revision_proplist');
      @svn_fs_change_rev_prop := GetProcAddress(SvnClientLib, 'svn_fs_change_rev_prop');
      @svn_fs_get_file_delta_stream := GetProcAddress(SvnClientLib, 'svn_fs_get_file_delta_stream');
      @svn_fs_get_uuid := GetProcAddress(SvnClientLib, 'svn_fs_get_uuid');
      @svn_fs_set_uuid := GetProcAddress(SvnClientLib, 'svn_fs_set_uuid');
      @svn_fs_lock := GetProcAddress(SvnClientLib, 'svn_fs_lock');
      @svn_fs_generate_lock_token := GetProcAddress(SvnClientLib, 'svn_fs_generate_lock_token');
      @svn_fs_unlock := GetProcAddress(SvnClientLib, 'svn_fs_unlock');
      @svn_fs_get_lock := GetProcAddress(SvnClientLib, 'svn_fs_get_lock');
      @svn_fs_get_locks := GetProcAddress(SvnClientLib, 'svn_fs_get_locks');
      @svn_fs_print_modules := GetProcAddress(SvnClientLib, 'svn_fs_print_modules');
      // svn_repos.h
      @svn_repos_version := GetProcAddress(SvnClientLib, 'svn_repos_version');
      @svn_repos_find_root_path := GetProcAddress(SvnClientLib, 'svn_repos_find_root_path');
      @svn_repos_open := GetProcAddress(SvnClientLib, 'svn_repos_open');
      @svn_repos_create := GetProcAddress(SvnClientLib, 'svn_repos_create');
      @svn_repos_delete := GetProcAddress(SvnClientLib, 'svn_repos_delete');
      @svn_repos_fs := GetProcAddress(SvnClientLib, 'svn_repos_fs');
      @svn_repos_hotcopy := GetProcAddress(SvnClientLib, 'svn_repos_hotcopy');
      @svn_repos_recover := GetProcAddress(SvnClientLib, 'svn_repos_recover');
      @svn_repos_recover2 := GetProcAddress(SvnClientLib, 'svn_repos_recover2');
      @svn_repos_db_logfiles := GetProcAddress(SvnClientLib, 'svn_repos_db_logfiles');
      @svn_repos_path := GetProcAddress(SvnClientLib, 'svn_repos_path');
      @svn_repos_db_env := GetProcAddress(SvnClientLib, 'svn_repos_db_env');
      @svn_repos_conf_dir := GetProcAddress(SvnClientLib, 'svn_repos_conf_dir');
      @svn_repos_svnserve_conf := GetProcAddress(SvnClientLib, 'svn_repos_svnserve_conf');
      @svn_repos_lock_dir := GetProcAddress(SvnClientLib, 'svn_repos_lock_dir');
      @svn_repos_db_lockfile := GetProcAddress(SvnClientLib, 'svn_repos_db_lockfile');
      @svn_repos_db_logs_lockfile := GetProcAddress(SvnClientLib, 'svn_repos_db_logs_lockfile');
      @svn_repos_hook_dir := GetProcAddress(SvnClientLib, 'svn_repos_hook_dir');
      @svn_repos_start_commit_hook := GetProcAddress(SvnClientLib, 'svn_repos_start_commit_hook');
      @svn_repos_pre_commit_hook := GetProcAddress(SvnClientLib, 'svn_repos_pre_commit_hook');
      @svn_repos_post_commit_hook := GetProcAddress(SvnClientLib, 'svn_repos_post_commit_hook');
      @svn_repos_pre_revprop_change_hook := GetProcAddress(SvnClientLib, 'svn_repos_pre_revprop_change_hook');
      @svn_repos_post_revprop_change_hook := GetProcAddress(SvnClientLib, 'svn_repos_post_revprop_change_hook');
      @svn_repos_pre_lock_hook := GetProcAddress(SvnClientLib, 'svn_repos_pre_lock_hook');
      @svn_repos_post_lock_hook := GetProcAddress(SvnClientLib, 'svn_repos_post_lock_hook');
      @svn_repos_pre_unlock_hook := GetProcAddress(SvnClientLib, 'svn_repos_pre_unlock_hook');
      @svn_repos_post_unlock_hook := GetProcAddress(SvnClientLib, 'svn_repos_post_unlock_hook');
      @svn_repos_begin_report := GetProcAddress(SvnClientLib, 'svn_repos_begin_report');
      @svn_repos_set_path2 := GetProcAddress(SvnClientLib, 'svn_repos_set_path2');
      @svn_repos_set_path := GetProcAddress(SvnClientLib, 'svn_repos_set_path');
      @svn_repos_link_path2 := GetProcAddress(SvnClientLib, 'svn_repos_link_path2');
      @svn_repos_link_path := GetProcAddress(SvnClientLib, 'svn_repos_link_path');
      @svn_repos_delete_path := GetProcAddress(SvnClientLib, 'svn_repos_delete_path');
      @svn_repos_finish_report := GetProcAddress(SvnClientLib, 'svn_repos_finish_report');
      @svn_repos_abort_report := GetProcAddress(SvnClientLib, 'svn_repos_abort_report');
      @svn_repos_dir_delta := GetProcAddress(SvnClientLib, 'svn_repos_dir_delta');
      @svn_repos_replay := GetProcAddress(SvnClientLib, 'svn_repos_replay');
      @svn_repos_get_commit_editor3 := GetProcAddress(SvnClientLib, 'svn_repos_get_commit_editor3');
      @svn_repos_get_commit_editor2 := GetProcAddress(SvnClientLib, 'svn_repos_get_commit_editor2');
      @svn_repos_get_commit_editor := GetProcAddress(SvnClientLib, 'svn_repos_get_commit_editor');
      @svn_repos_dated_revision := GetProcAddress(SvnClientLib, 'svn_repos_dated_revision');
      @svn_repos_get_committed_info := GetProcAddress(SvnClientLib, 'svn_repos_get_committed_info');
      @svn_repos_stat := GetProcAddress(SvnClientLib, 'svn_repos_stat');
      @svn_repos_history2 := GetProcAddress(SvnClientLib, 'svn_repos_history2');
      @svn_repos_history := GetProcAddress(SvnClientLib, 'svn_repos_history');
      @svn_repos_trace_node_locations := GetProcAddress(SvnClientLib, 'svn_repos_trace_node_locations');
      @svn_repos_get_logs3 := GetProcAddress(SvnClientLib, 'svn_repos_get_logs3');
      @svn_repos_get_logs2 := GetProcAddress(SvnClientLib, 'svn_repos_get_logs2');
      @svn_repos_get_logs := GetProcAddress(SvnClientLib, 'svn_repos_get_logs');
      @svn_repos_get_file_revs := GetProcAddress(SvnClientLib, 'svn_repos_get_file_revs');
      @svn_repos_fs_commit_txn := GetProcAddress(SvnClientLib, 'svn_repos_fs_commit_txn');
      @svn_repos_fs_begin_txn_for_commit := GetProcAddress(SvnClientLib, 'svn_repos_fs_begin_txn_for_commit');
      @svn_repos_fs_begin_txn_for_update := GetProcAddress(SvnClientLib, 'svn_repos_fs_begin_txn_for_update');
      @svn_repos_fs_lock := GetProcAddress(SvnClientLib, 'svn_repos_fs_lock');
      @svn_repos_fs_unlock := GetProcAddress(SvnClientLib, 'svn_repos_fs_unlock');
      @svn_repos_fs_get_locks := GetProcAddress(SvnClientLib, 'svn_repos_fs_get_locks');
      @svn_repos_fs_change_rev_prop2 := GetProcAddress(SvnClientLib, 'svn_repos_fs_change_rev_prop2');
      @svn_repos_fs_change_rev_prop := GetProcAddress(SvnClientLib, 'svn_repos_fs_change_rev_prop');
      @svn_repos_fs_revision_prop := GetProcAddress(SvnClientLib, 'svn_repos_fs_revision_prop');
      @svn_repos_fs_revision_proplist := GetProcAddress(SvnClientLib, 'svn_repos_fs_revision_proplist');
      @svn_repos_fs_change_node_prop := GetProcAddress(SvnClientLib, 'svn_repos_fs_change_node_prop');
      @svn_repos_fs_change_txn_prop := GetProcAddress(SvnClientLib, 'svn_repos_fs_change_txn_prop');
      @svn_repos_node_editor := GetProcAddress(SvnClientLib, 'svn_repos_node_editor');
      @svn_repos_node_from_baton := GetProcAddress(SvnClientLib, 'svn_repos_node_from_baton');
      @svn_repos_dump_fs2 := GetProcAddress(SvnClientLib, 'svn_repos_dump_fs2');
      @svn_repos_dump_fs := GetProcAddress(SvnClientLib, 'svn_repos_dump_fs');
      @svn_repos_load_fs2 := GetProcAddress(SvnClientLib, 'svn_repos_load_fs2');
      @svn_repos_load_fs := GetProcAddress(SvnClientLib, 'svn_repos_load_fs');
      @svn_repos_parse_dumpstream2 := GetProcAddress(SvnClientLib, 'svn_repos_parse_dumpstream2');
      @svn_repos_get_fs_build_parser2 := GetProcAddress(SvnClientLib, 'svn_repos_get_fs_build_parser2');
      @svn_repos_parse_dumpstream := GetProcAddress(SvnClientLib, 'svn_repos_parse_dumpstream');
      @svn_repos_get_fs_build_parser := GetProcAddress(SvnClientLib, 'svn_repos_get_fs_build_parser');
      @svn_repos_authz_read := GetProcAddress(SvnClientLib, 'svn_repos_authz_read');
      @svn_repos_authz_check_access := GetProcAddress(SvnClientLib, 'svn_repos_authz_check_access');
      // svn_opt.h
      @svn_opt_get_canonical_subcommand := GetProcAddress(SvnClientLib, 'svn_opt_get_canonical_subcommand');
      @svn_opt_get_option_from_code := GetProcAddress(SvnClientLib, 'svn_opt_get_option_from_code');
      @svn_opt_subcommand_takes_option := GetProcAddress(SvnClientLib, 'svn_opt_subcommand_takes_option');
      @svn_opt_print_generic_help := GetProcAddress(SvnClientLib, 'svn_opt_print_generic_help');
      @svn_opt_format_option := GetProcAddress(SvnClientLib, 'svn_opt_format_option');
      @svn_opt_subcommand_help := GetProcAddress(SvnClientLib, 'svn_opt_subcommand_help');
      @svn_opt_parse_revision := GetProcAddress(SvnClientLib, 'svn_opt_parse_revision');
      @svn_opt_args_to_target_array2 := GetProcAddress(SvnClientLib, 'svn_opt_args_to_target_array2');
      @svn_opt_args_to_target_array := GetProcAddress(SvnClientLib, 'svn_opt_args_to_target_array');
      @svn_opt_push_implicit_dot_target := GetProcAddress(SvnClientLib, 'svn_opt_push_implicit_dot_target');
      @svn_opt_parse_num_args := GetProcAddress(SvnClientLib, 'svn_opt_parse_num_args');
      @svn_opt_parse_all_args := GetProcAddress(SvnClientLib, 'svn_opt_parse_all_args');
      @svn_opt_parse_path := GetProcAddress(SvnClientLib, 'svn_opt_parse_path');
      @svn_opt_print_help := GetProcAddress(SvnClientLib, 'svn_opt_print_help');
      // svn_auth.h
      @svn_auth_ssl_server_cert_info_dup := GetProcAddress(SvnClientLib, 'svn_auth_ssl_server_cert_info_dup');
      @svn_auth_open := GetProcAddress(SvnClientLib, 'svn_auth_open');
      @svn_auth_set_parameter := GetProcAddress(SvnClientLib, 'svn_auth_set_parameter');
      @svn_auth_get_parameter := GetProcAddress(SvnClientLib, 'svn_auth_get_parameter');
      @svn_auth_first_credentials := GetProcAddress(SvnClientLib, 'svn_auth_first_credentials');
      @svn_auth_next_credentials := GetProcAddress(SvnClientLib, 'svn_auth_next_credentials');
      @svn_auth_save_credentials := GetProcAddress(SvnClientLib, 'svn_auth_save_credentials');
      // svn_ra.h
      @svn_ra_version := GetProcAddress(SvnClientLib, 'svn_ra_version');
      @svn_ra_initialize := GetProcAddress(SvnClientLib, 'svn_ra_initialize');
      @svn_ra_create_callbacks := GetProcAddress(SvnClientLib, 'svn_ra_create_callbacks');
      @svn_ra_open2 := GetProcAddress(SvnClientLib, 'svn_ra_open2');
      @svn_ra_open := GetProcAddress(SvnClientLib, 'svn_ra_open');
      @svn_ra_get_latest_revnum := GetProcAddress(SvnClientLib, 'svn_ra_get_latest_revnum');
      @svn_ra_get_dated_revision := GetProcAddress(SvnClientLib, 'svn_ra_get_dated_revision');
      @svn_ra_change_rev_prop := GetProcAddress(SvnClientLib, 'svn_ra_change_rev_prop');
      @svn_ra_rev_proplist := GetProcAddress(SvnClientLib, 'svn_ra_rev_proplist');
      @svn_ra_rev_prop := GetProcAddress(SvnClientLib, 'svn_ra_rev_prop');
      @svn_ra_get_commit_editor := GetProcAddress(SvnClientLib, 'svn_ra_get_commit_editor');
      @svn_ra_get_file := GetProcAddress(SvnClientLib, 'svn_ra_get_file');
      @svn_ra_get_dir := GetProcAddress(SvnClientLib, 'svn_ra_get_dir');
      @svn_ra_do_update := GetProcAddress(SvnClientLib, 'svn_ra_do_update');
      @svn_ra_do_switch := GetProcAddress(SvnClientLib, 'svn_ra_do_switch');
      @svn_ra_do_status := GetProcAddress(SvnClientLib, 'svn_ra_do_status');
      @svn_ra_do_diff := GetProcAddress(SvnClientLib, 'svn_ra_do_diff');
      @svn_ra_get_log := GetProcAddress(SvnClientLib, 'svn_ra_get_log');
      @svn_ra_check_path := GetProcAddress(SvnClientLib, 'svn_ra_check_path');
      @svn_ra_stat := GetProcAddress(SvnClientLib, 'svn_ra_stat');
      @svn_ra_get_uuid := GetProcAddress(SvnClientLib, 'svn_ra_get_uuid');
      @svn_ra_get_repos_root := GetProcAddress(SvnClientLib, 'svn_ra_get_repos_root');
      @svn_ra_get_locations := GetProcAddress(SvnClientLib, 'svn_ra_get_locations');
      @svn_ra_get_file_revs := GetProcAddress(SvnClientLib, 'svn_ra_get_file_revs');
      @svn_ra_lock := GetProcAddress(SvnClientLib, 'svn_ra_lock');
      @svn_ra_unlock := GetProcAddress(SvnClientLib, 'svn_ra_unlock');
      @svn_ra_get_lock := GetProcAddress(SvnClientLib, 'svn_ra_get_lock');
      @svn_ra_get_locks := GetProcAddress(SvnClientLib, 'svn_ra_get_locks');
      @svn_ra_print_modules := GetProcAddress(SvnClientLib, 'svn_ra_print_modules');
      @svn_ra_print_ra_libraries := GetProcAddress(SvnClientLib, 'svn_ra_print_ra_libraries');
      @svn_ra_dav_init := GetProcAddress(SvnClientLib, 'svn_ra_dav_init');
      @svn_ra_local_init := GetProcAddress(SvnClientLib, 'svn_ra_local_init');
      @svn_ra_svn_init := GetProcAddress(SvnClientLib, 'svn_ra_svn_init');
      @svn_ra_init_ra_libs := GetProcAddress(SvnClientLib, 'svn_ra_init_ra_libs');
      @svn_ra_get_ra_library := GetProcAddress(SvnClientLib, 'svn_ra_get_ra_library');
      // svn_wc.h
      @svn_wc_version := GetProcAddress(SvnClientLib, 'svn_wc_version');
      @svn_wc_adm_open3 := GetProcAddress(SvnClientLib, 'svn_wc_adm_open3');
      @svn_wc_adm_open2 := GetProcAddress(SvnClientLib, 'svn_wc_adm_open2');
      @svn_wc_adm_open := GetProcAddress(SvnClientLib, 'svn_wc_adm_open');
      @svn_wc_adm_probe_open3 := GetProcAddress(SvnClientLib, 'svn_wc_adm_probe_open3');
      @svn_wc_adm_probe_open2 := GetProcAddress(SvnClientLib, 'svn_wc_adm_probe_open2');
      @svn_wc_adm_probe_open := GetProcAddress(SvnClientLib, 'svn_wc_adm_probe_open');
      @svn_wc_adm_open_anchor := GetProcAddress(SvnClientLib, 'svn_wc_adm_open_anchor');
      @svn_wc_adm_retrieve := GetProcAddress(SvnClientLib, 'svn_wc_adm_retrieve');
      @svn_wc_adm_probe_retrieve := GetProcAddress(SvnClientLib, 'svn_wc_adm_probe_retrieve');
      @svn_wc_adm_probe_try3 := GetProcAddress(SvnClientLib, 'svn_wc_adm_probe_try3');
      @svn_wc_adm_probe_try2 := GetProcAddress(SvnClientLib, 'svn_wc_adm_probe_try2');
      @svn_wc_adm_probe_try := GetProcAddress(SvnClientLib, 'svn_wc_adm_probe_try');
      @svn_wc_adm_close := GetProcAddress(SvnClientLib, 'svn_wc_adm_close');
      @svn_wc_adm_access_path := GetProcAddress(SvnClientLib, 'svn_wc_adm_access_path');
      @svn_wc_adm_access_pool := GetProcAddress(SvnClientLib, 'svn_wc_adm_access_pool');
      @svn_wc_adm_locked := GetProcAddress(SvnClientLib, 'svn_wc_adm_locked');
      @svn_wc_locked := GetProcAddress(SvnClientLib, 'svn_wc_locked');
      @svn_wc_is_adm_dir := GetProcAddress(SvnClientLib, 'svn_wc_is_adm_dir');
      @svn_wc_get_adm_dir := GetProcAddress(SvnClientLib, 'svn_wc_get_adm_dir');
      @svn_wc_set_adm_dir := GetProcAddress(SvnClientLib, 'svn_wc_set_adm_dir');
      @svn_wc_init_traversal_info := GetProcAddress(SvnClientLib, 'svn_wc_init_traversal_info');
      @svn_wc_edited_externals := GetProcAddress(SvnClientLib, 'svn_wc_edited_externals');
      @svn_wc_external_item_dup := GetProcAddress(SvnClientLib, 'svn_wc_external_item_dup');
      @svn_wc_parse_externals_description2 := GetProcAddress(SvnClientLib, 'svn_wc_parse_externals_description2');
      @svn_wc_parse_externals_description := GetProcAddress(SvnClientLib, 'svn_wc_parse_externals_description');
      @svn_wc_create_notify := GetProcAddress(SvnClientLib, 'svn_wc_create_notify');
      @svn_wc_dup_notify := GetProcAddress(SvnClientLib, 'svn_wc_dup_notify');
      @svn_wc_check_wc := GetProcAddress(SvnClientLib, 'svn_wc_check_wc');
      @svn_wc_has_binary_prop := GetProcAddress(SvnClientLib, 'svn_wc_has_binary_prop');
      @svn_wc_text_modified_p := GetProcAddress(SvnClientLib, 'svn_wc_text_modified_p');
      @svn_wc_props_modified_p := GetProcAddress(SvnClientLib, 'svn_wc_props_modified_p');
      @svn_wc_entry := GetProcAddress(SvnClientLib, 'svn_wc_entry');
      @svn_wc_entries_read := GetProcAddress(SvnClientLib, 'svn_wc_entries_read');
      @svn_wc_entry_dup := GetProcAddress(SvnClientLib, 'svn_wc_entry_dup');
      @svn_wc_conflicted_p := GetProcAddress(SvnClientLib, 'svn_wc_conflicted_p');
      @svn_wc_get_ancestry := GetProcAddress(SvnClientLib, 'svn_wc_get_ancestry');
      @svn_wc_walk_entries2 := GetProcAddress(SvnClientLib, 'svn_wc_walk_entries2');
      @svn_wc_walk_entries := GetProcAddress(SvnClientLib, 'svn_wc_walk_entries');
      @svn_wc_mark_missing_deleted := GetProcAddress(SvnClientLib, 'svn_wc_mark_missing_deleted');
      @svn_wc_ensure_adm2 := GetProcAddress(SvnClientLib, 'svn_wc_ensure_adm2');
      @svn_wc_ensure_adm := GetProcAddress(SvnClientLib, 'svn_wc_ensure_adm');
      @svn_wc_maybe_set_repos_root := GetProcAddress(SvnClientLib, 'svn_wc_maybe_set_repos_root');
      @svn_wc_dup_status2 := GetProcAddress(SvnClientLib, 'svn_wc_dup_status2');
      @svn_wc_dup_status := GetProcAddress(SvnClientLib, 'svn_wc_dup_status');
      @svn_wc_status2 := GetProcAddress(SvnClientLib, 'svn_wc_status2');
      @svn_wc_status := GetProcAddress(SvnClientLib, 'svn_wc_status');
      @svn_wc_get_status_editor2 := GetProcAddress(SvnClientLib, 'svn_wc_get_status_editor2');
      @svn_wc_get_status_editor := GetProcAddress(SvnClientLib, 'svn_wc_get_status_editor');
      @svn_wc_status_set_repos_locks := GetProcAddress(SvnClientLib, 'svn_wc_status_set_repos_locks');
      @svn_wc_copy2 := GetProcAddress(SvnClientLib, 'svn_wc_copy2');
      @svn_wc_copy := GetProcAddress(SvnClientLib, 'svn_wc_copy');
      @svn_wc_delete2 := GetProcAddress(SvnClientLib, 'svn_wc_delete2');
      @svn_wc_delete := GetProcAddress(SvnClientLib, 'svn_wc_delete');
      @svn_wc_add2 := GetProcAddress(SvnClientLib, 'svn_wc_add2');
      @svn_wc_add := GetProcAddress(SvnClientLib, 'svn_wc_add');
      @svn_wc_add_repos_file := GetProcAddress(SvnClientLib, 'svn_wc_add_repos_file');
      @svn_wc_remove_from_revision_control := GetProcAddress(SvnClientLib, 'svn_wc_remove_from_revision_control');
      @svn_wc_resolved_conflict2 := GetProcAddress(SvnClientLib, 'svn_wc_resolved_conflict2');
      @svn_wc_resolved_conflict := GetProcAddress(SvnClientLib, 'svn_wc_resolved_conflict');
      @svn_wc_process_committed2 := GetProcAddress(SvnClientLib, 'svn_wc_process_committed2');
      @svn_wc_process_committed := GetProcAddress(SvnClientLib, 'svn_wc_process_committed');
      @svn_wc_crawl_revisions2 := GetProcAddress(SvnClientLib, 'svn_wc_crawl_revisions2');
      @svn_wc_crawl_revisions := GetProcAddress(SvnClientLib, 'svn_wc_crawl_revisions');
      @svn_wc_is_wc_root := GetProcAddress(SvnClientLib, 'svn_wc_is_wc_root');
      @svn_wc_get_actual_target := GetProcAddress(SvnClientLib, 'svn_wc_get_actual_target');
      @svn_wc_get_update_editor2 := GetProcAddress(SvnClientLib, 'svn_wc_get_update_editor2');
      @svn_wc_get_update_editor := GetProcAddress(SvnClientLib, 'svn_wc_get_update_editor');
      @svn_wc_get_switch_editor2 := GetProcAddress(SvnClientLib, 'svn_wc_get_switch_editor2');
      @svn_wc_get_switch_editor := GetProcAddress(SvnClientLib, 'svn_wc_get_switch_editor');
      @svn_wc_prop_list := GetProcAddress(SvnClientLib, 'svn_wc_prop_list');
      @svn_wc_prop_get := GetProcAddress(SvnClientLib, 'svn_wc_prop_get');
      @svn_wc_prop_set2 := GetProcAddress(SvnClientLib, 'svn_wc_prop_set2');
      @svn_wc_prop_set := GetProcAddress(SvnClientLib, 'svn_wc_prop_set');
      @svn_wc_is_normal_prop := GetProcAddress(SvnClientLib, 'svn_wc_is_normal_prop');
      @svn_wc_is_wc_prop := GetProcAddress(SvnClientLib, 'svn_wc_is_wc_prop');
      @svn_wc_is_entry_prop := GetProcAddress(SvnClientLib, 'svn_wc_is_entry_prop');
      @svn_wc_get_diff_editor3 := GetProcAddress(SvnClientLib, 'svn_wc_get_diff_editor3');
      @svn_wc_get_diff_editor2 := GetProcAddress(SvnClientLib, 'svn_wc_get_diff_editor2');
      @svn_wc_get_diff_editor := GetProcAddress(SvnClientLib, 'svn_wc_get_diff_editor');
      @svn_wc_diff3 := GetProcAddress(SvnClientLib, 'svn_wc_diff3');
      @svn_wc_diff2 := GetProcAddress(SvnClientLib, 'svn_wc_diff2');
      @svn_wc_diff := GetProcAddress(SvnClientLib, 'svn_wc_diff');
      @svn_wc_get_prop_diffs := GetProcAddress(SvnClientLib, 'svn_wc_get_prop_diffs');
      @svn_wc_merge := GetProcAddress(SvnClientLib, 'svn_wc_merge');
      @svn_wc_merge_props := GetProcAddress(SvnClientLib, 'svn_wc_merge_props');
      @svn_wc_merge_prop_diffs := GetProcAddress(SvnClientLib, 'svn_wc_merge_prop_diffs');
      @svn_wc_get_pristine_copy_path := GetProcAddress(SvnClientLib, 'svn_wc_get_pristine_copy_path');
      @svn_wc_cleanup2 := GetProcAddress(SvnClientLib, 'svn_wc_cleanup2');
      @svn_wc_cleanup := GetProcAddress(SvnClientLib, 'svn_wc_cleanup');
      @svn_wc_relocate := GetProcAddress(SvnClientLib, 'svn_wc_relocate');
      @svn_wc_revert2 := GetProcAddress(SvnClientLib, 'svn_wc_revert2');
      @svn_wc_revert := GetProcAddress(SvnClientLib, 'svn_wc_revert');
      @svn_wc_create_tmp_file := GetProcAddress(SvnClientLib, 'svn_wc_create_tmp_file');
      @svn_wc_translated_file := GetProcAddress(SvnClientLib, 'svn_wc_translated_file');
      @svn_wc_transmit_text_deltas := GetProcAddress(SvnClientLib, 'svn_wc_transmit_text_deltas');
      @svn_wc_transmit_prop_deltas := GetProcAddress(SvnClientLib, 'svn_wc_transmit_prop_deltas');
      @svn_wc_get_default_ignores := GetProcAddress(SvnClientLib, 'svn_wc_get_default_ignores');
      @svn_wc_get_ignores := GetProcAddress(SvnClientLib, 'svn_wc_get_ignores');
      @svn_wc_add_lock := GetProcAddress(SvnClientLib, 'svn_wc_add_lock');
      @svn_wc_remove_lock := GetProcAddress(SvnClientLib, 'svn_wc_remove_lock');
      // svn_client.h
      @svn_client_version := GetProcAddress(SvnClientLib, 'svn_client_version');
      @svn_client_get_simple_prompt_provider := GetProcAddress(SvnClientLib, 'svn_client_get_simple_prompt_provider');
      @svn_client_get_username_prompt_provider := GetProcAddress(SvnClientLib,
        'svn_client_get_username_prompt_provider');
      @svn_client_get_simple_provider := GetProcAddress(SvnClientLib, 'svn_client_get_simple_provider');
      @svn_client_get_windows_simple_provider := GetProcAddress(SvnClientLib, 'svn_client_get_windows_simple_provider');
      @svn_client_get_username_provider := GetProcAddress(SvnClientLib, 'svn_client_get_username_provider');
      @svn_client_get_ssl_server_trust_file_provider := GetProcAddress(SvnClientLib,
        'svn_client_get_ssl_server_trust_file_provider');
      @svn_client_get_ssl_client_cert_file_provider := GetProcAddress(SvnClientLib,
        'svn_client_get_ssl_client_cert_file_provider');
      @svn_client_get_ssl_client_cert_pw_file_provider := GetProcAddress(SvnClientLib,
        'svn_client_get_ssl_client_cert_pw_file_provider');
      @svn_client_get_ssl_server_trust_prompt_provider := GetProcAddress(SvnClientLib,
        'svn_client_get_ssl_server_trust_prompt_provider');
      @svn_client_get_ssl_client_cert_prompt_provider := GetProcAddress(SvnClientLib,
        'svn_client_get_ssl_client_cert_prompt_provider');
      @svn_client_get_ssl_client_cert_pw_prompt_provider := GetProcAddress(SvnClientLib,
        'svn_client_get_ssl_client_cert_pw_prompt_provider');
      @svn_client_proplist_item_dup := GetProcAddress(SvnClientLib, 'svn_client_proplist_item_dup');
      @svn_client_commit_item2_dup := GetProcAddress(SvnClientLib, 'svn_client_commit_item2_dup');
      @svn_client_create_context := GetProcAddress(SvnClientLib, 'svn_client_create_context');
      @svn_client_checkout2 := GetProcAddress(SvnClientLib, 'svn_client_checkout2');
      @svn_client_checkout := GetProcAddress(SvnClientLib, 'svn_client_checkout');
      @svn_client_update2 := GetProcAddress(SvnClientLib, 'svn_client_update2');
      @svn_client_update := GetProcAddress(SvnClientLib, 'svn_client_update');
      @svn_client_switch := GetProcAddress(SvnClientLib, 'svn_client_switch');
      @svn_client_add3 := GetProcAddress(SvnClientLib, 'svn_client_add3');
      @svn_client_add2 := GetProcAddress(SvnClientLib, 'svn_client_add2');
      @svn_client_add := GetProcAddress(SvnClientLib, 'svn_client_add');
      @svn_client_mkdir2 := GetProcAddress(SvnClientLib, 'svn_client_mkdir2');
      @svn_client_mkdir := GetProcAddress(SvnClientLib, 'svn_client_mkdir');
      @svn_client_delete2 := GetProcAddress(SvnClientLib, 'svn_client_delete2');
      @svn_client_delete := GetProcAddress(SvnClientLib, 'svn_client_delete');
      @svn_client_import2 := GetProcAddress(SvnClientLib, 'svn_client_import2');
      @svn_client_import := GetProcAddress(SvnClientLib, 'svn_client_import');
      @svn_client_commit3 := GetProcAddress(SvnClientLib, 'svn_client_commit3');
      @svn_client_commit2 := GetProcAddress(SvnClientLib, 'svn_client_commit2');
      @svn_client_commit := GetProcAddress(SvnClientLib, 'svn_client_commit');
      @svn_client_status2 := GetProcAddress(SvnClientLib, 'svn_client_status2');
      @svn_client_status := GetProcAddress(SvnClientLib, 'svn_client_status');
      @svn_client_log2 := GetProcAddress(SvnClientLib, 'svn_client_log2');
      @svn_client_log := GetProcAddress(SvnClientLib, 'svn_client_log');
      @svn_client_blame2 := GetProcAddress(SvnClientLib, 'svn_client_blame2');
      @svn_client_blame := GetProcAddress(SvnClientLib, 'svn_client_blame');
      @svn_client_diff3 := GetProcAddress(SvnClientLib, 'svn_client_diff3');
      @svn_client_diff2 := GetProcAddress(SvnClientLib, 'svn_client_diff2');
      @svn_client_diff := GetProcAddress(SvnClientLib, 'svn_client_diff');
      @svn_client_diff_peg3 := GetProcAddress(SvnClientLib, 'svn_client_diff_peg3');
      @svn_client_diff_peg2 := GetProcAddress(SvnClientLib, 'svn_client_diff_peg2');
      @svn_client_diff_peg := GetProcAddress(SvnClientLib, 'svn_client_diff_peg');
      @svn_client_merge := GetProcAddress(SvnClientLib, 'svn_client_merge');
      @svn_client_merge_peg := GetProcAddress(SvnClientLib, 'svn_client_merge_peg');
      @svn_client_cleanup := GetProcAddress(SvnClientLib, 'svn_client_cleanup');
      @svn_client_relocate := GetProcAddress(SvnClientLib, 'svn_client_relocate');
      @svn_client_revert := GetProcAddress(SvnClientLib, 'svn_client_revert');
      @svn_client_resolved := GetProcAddress(SvnClientLib, 'svn_client_resolved');
      @svn_client_copy2 := GetProcAddress(SvnClientLib, 'svn_client_copy2');
      @svn_client_copy := GetProcAddress(SvnClientLib, 'svn_client_copy');
      @svn_client_move3 := GetProcAddress(SvnClientLib, 'svn_client_move3');
      @svn_client_move2 := GetProcAddress(SvnClientLib, 'svn_client_move2');
      @svn_client_move := GetProcAddress(SvnClientLib, 'svn_client_move');
      @svn_client_propset2 := GetProcAddress(SvnClientLib, 'svn_client_propset2');
      @svn_client_propset := GetProcAddress(SvnClientLib, 'svn_client_propset');
      @svn_client_revprop_set := GetProcAddress(SvnClientLib, 'svn_client_revprop_set');
      @svn_client_propget2 := GetProcAddress(SvnClientLib, 'svn_client_propget2');
      @svn_client_propget := GetProcAddress(SvnClientLib, 'svn_client_propget');
      @svn_client_revprop_get := GetProcAddress(SvnClientLib, 'svn_client_revprop_get');
      @svn_client_proplist2 := GetProcAddress(SvnClientLib, 'svn_client_proplist2');
      @svn_client_proplist := GetProcAddress(SvnClientLib, 'svn_client_proplist');
      @svn_client_revprop_list := GetProcAddress(SvnClientLib, 'svn_client_revprop_list');
      @svn_client_export3 := GetProcAddress(SvnClientLib, 'svn_client_export3');
      @svn_client_export2 := GetProcAddress(SvnClientLib, 'svn_client_export2');
      @svn_client_export := GetProcAddress(SvnClientLib, 'svn_client_export');
      @svn_client_ls3 := GetProcAddress(SvnClientLib, 'svn_client_ls3');
      @svn_client_ls2 := GetProcAddress(SvnClientLib, 'svn_client_ls2');
      @svn_client_ls := GetProcAddress(SvnClientLib, 'svn_client_ls');
      @svn_client_cat2 := GetProcAddress(SvnClientLib, 'svn_client_cat2');
      @svn_client_cat := GetProcAddress(SvnClientLib, 'svn_client_cat');
      @svn_client_lock := GetProcAddress(SvnClientLib, 'svn_client_lock');
      @svn_client_unlock := GetProcAddress(SvnClientLib, 'svn_client_unlock');
      @svn_info_dup := GetProcAddress(SvnClientLib, 'svn_info_dup');
      @svn_client_info := GetProcAddress(SvnClientLib, 'svn_client_info');
      @svn_client_url_from_path := GetProcAddress(SvnClientLib, 'svn_client_url_from_path');
      @svn_client_uuid_from_url := GetProcAddress(SvnClientLib, 'svn_client_uuid_from_url');
      @svn_client_uuid_from_path := GetProcAddress(SvnClientLib, 'svn_client_uuid_from_path');
      @svn_client_open_ra_session := GetProcAddress(SvnClientLib, 'svn_client_open_ra_session');
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure FreeSvnClientLib;

begin
  if SvnClientLibLoaded then
    FreeLibrary(SvnClientLib);
  SvnClientLib := INVALID_HANDLE_VALUE;
  @svn_create_commit_info := nil;
  @svn_log_changed_path_dup := nil;
  @svn_mime_type_validate := nil;
  @svn_mime_type_is_binary := nil;
  @svn_lock_create := nil;
  @svn_lock_dup := nil;
  @svn_nls_init := nil;
  @svn_ver_compatible := nil;
  @svn_ver_equal := nil;
  @svn_ver_check_list := nil;
  @svn_subr_version := nil;
  @svn_time_to_cstring := nil;
  @svn_time_from_cstring := nil;
  @svn_time_to_human_cstring := nil;
  @svn_parse_date := nil;
  @svn_sleep_for_timestamps := nil;
  @svn_pool_create_ex := nil;
  @svn_sort_compare_items_as_paths := nil;
  @svn_sort_compare_items_lexically := nil;
  @svn_sort_compare_revisions := nil;
  @svn_sort_compare_paths := nil;
  @svn_sort__hash := nil;
  @svn_md5_empty_string_digest := nil;
  @svn_md5_digest_to_cstring_display := nil;
  @svn_md5_digest_to_cstring := nil;
  @svn_md5_digests_match := nil;
  @svn_string_create := nil;
  @svn_string_ncreate := nil;
  @svn_string_create_from_buf := nil;
  @svn_string_createf := nil;
  @svn_string_createv := nil;
  @svn_string_isempty := nil;
  @svn_string_dup := nil;
  @svn_string_compare := nil;
  @svn_string_first_non_whitespace := nil;
  @svn_string_find_char_backward := nil;
  @svn_stringbuf_create := nil;
  @svn_stringbuf_ncreate := nil;
  @svn_stringbuf_create_from_string := nil;
  @svn_stringbuf_createf := nil;
  @svn_stringbuf_createv := nil;
  @svn_stringbuf_ensure := nil;
  @svn_stringbuf_set := nil;
  @svn_stringbuf_setempty := nil;
  @svn_stringbuf_isempty := nil;
  @svn_stringbuf_chop := nil;
  @svn_stringbuf_fillchar := nil;
  @svn_stringbuf_appendbytes := nil;
  @svn_stringbuf_appendstr := nil;
  @svn_stringbuf_appendcstr := nil;
  @svn_stringbuf_dup := nil;
  @svn_stringbuf_compare := nil;
  @svn_stringbuf_first_non_whitespace := nil;
  @svn_stringbuf_strip_whitespace := nil;
  @svn_stringbuf_find_char_backward := nil;
  @svn_string_compare_stringbuf := nil;
  @svn_cstring_split := nil;
  @svn_cstring_split_append := nil;
  @svn_cstring_match_glob_list := nil;
  @svn_cstring_count_newlines := nil;
  @svn_cstring_join := nil;
  @svn_xml_is_xml_safe := nil;
  @svn_xml_escape_cdata_stringbuf := nil;
  @svn_xml_escape_cdata_string := nil;
  @svn_xml_escape_cdata_cstring := nil;
  @svn_xml_escape_attr_stringbuf := nil;
  @svn_xml_escape_attr_string := nil;
  @svn_xml_escape_attr_cstring := nil;
  @svn_xml_fuzzy_escape := nil;
  @svn_xml_make_parser := nil;
  @svn_xml_free_parser := nil;
  @svn_xml_parse := nil;
  @svn_xml_signal_bailout := nil;
  @svn_xml_get_attr_value := nil;
  @svn_xml_ap_to_hash := nil;
  @svn_xml_make_att_hash := nil;
  @svn_xml_hash_atts_preserving := nil;
  @svn_xml_hash_atts_overlaying := nil;
  @svn_xml_make_header := nil;
  @svn_xml_make_open_tag := nil;
  @svn_xml_make_open_tag_v := nil;
  @svn_xml_make_open_tag_hash := nil;
  @svn_xml_make_close_tag := nil;
  @svn_cmdline_init := nil;
  @svn_cmdline_cstring_from_utf8 := nil;
  @svn_cmdline_cstring_from_utf8_fuzzy := nil;
  @svn_cmdline_cstring_to_utf8 := nil;
  @svn_cmdline_path_local_style_from_utf8 := nil;
  @svn_cmdline_printf := nil;
  @svn_cmdline_fprintf := nil;
  @svn_cmdline_fputs := nil;
  @svn_cmdline_fflush := nil;
  @svn_cmdline_output_encoding := nil;
  @svn_cmdline_handle_exit_error := nil;
  @svn_utf_initialize := nil;
  @svn_utf_stringbuf_to_utf8 := nil;
  @svn_utf_string_to_utf8 := nil;
  @svn_utf_cstring_to_utf8 := nil;
  @svn_utf_cstring_to_utf8_ex := nil;
  @svn_utf_stringbuf_from_utf8 := nil;
  @svn_utf_string_from_utf8 := nil;
  @svn_utf_cstring_from_utf8 := nil;
  @svn_utf_cstring_from_utf8_ex := nil;
  @svn_utf_cstring_from_utf8_fuzzy := nil;
  @svn_utf_cstring_from_utf8_stringbuf := nil;
  @svn_utf_cstring_from_utf8_string := nil;
  @svn_prop_dup := nil;
  @svn_prop_array_dup := nil;
  @svn_property_kind := nil;
  @svn_prop_is_svn_prop := nil;
  @svn_prop_needs_translation := nil;
  @svn_categorize_props := nil;
  @svn_prop_diffs := nil;
  @svn_error__locate := nil;
  @svn_strerror := nil;
  @svn_error_create := nil;
  @svn_error_createf := nil;
  @svn_error_wrap_apr := nil;
  @svn_error_quick_wrap := nil;
  @svn_error_compose := nil;
  @svn_error_dup := nil;
  @svn_error_clear := nil;
  @svn_handle_error2 := nil;
  @svn_handle_error := nil;
  @svn_handle_warning2 := nil;
  @svn_handle_warning := nil;
  @svn_path_internal_style := nil;
  @svn_path_local_style := nil;
  @svn_path_join := nil;
  @svn_path_join_many := nil;
  @svn_path_basename := nil;
  @svn_path_dirname := nil;
  @svn_path_component_count := nil;
  @svn_path_add_component := nil;
  @svn_path_remove_component := nil;
  @svn_path_remove_components := nil;
  @svn_path_split := nil;
  @svn_path_is_empty := nil;
  @svn_path_canonicalize := nil;
  @svn_path_compare_paths := nil;
  @svn_path_get_longest_ancestor := nil;
  @svn_path_get_absolute := nil;
  @svn_path_split_if_file := nil;
  @svn_path_condense_targets := nil;
  @svn_path_remove_redundancies := nil;
  @svn_path_decompose := nil;
  @svn_path_is_single_path_component := nil;
  @svn_path_is_backpath_present := nil;
  @svn_path_is_child := nil;
  @svn_path_is_ancestor := nil;
  @svn_path_check_valid := nil;
  @svn_path_is_url := nil;
  @svn_path_is_uri_safe := nil;
  @svn_path_uri_encode := nil;
  @svn_path_uri_decode := nil;
  @svn_path_url_add_component := nil;
  @svn_path_uri_from_iri := nil;
  @svn_path_uri_autoescape := nil;
  @svn_path_cstring_from_utf8 := nil;
  @svn_path_cstring_to_utf8 := nil;
  @svn_config_get_config := nil;
  @svn_config_read := nil;
  @svn_config_merge := nil;
  @svn_config_get := nil;
  @svn_config_set := nil;
  @svn_config_get_bool := nil;
  @svn_config_set_bool := nil;
  @svn_config_enumerate_sections := nil;
  @svn_config_enumerate_sections2 := nil;
  @svn_config_enumerate := nil;
  @svn_config_enumerate2 := nil;
  @svn_config_find_group := nil;
  @svn_config_get_server_setting := nil;
  @svn_config_get_server_setting_int := nil;
  @svn_config_ensure := nil;
  @svn_config_read_auth_data := nil;
  @svn_config_write_auth_data := nil;
  @svn_io_check_path := nil;
  @svn_io_check_special_path := nil;
  @svn_io_check_resolved_path := nil;
  @svn_io_open_unique_file := nil;
  @svn_io_create_unique_link := nil;
  @svn_io_read_link := nil;
  @svn_io_temp_dir := nil;
  @svn_io_copy_file := nil;
  @svn_io_copy_link := nil;
  @svn_io_copy_dir_recursively := nil;
  @svn_io_make_dir_recursively := nil;
  @svn_io_dir_empty := nil;
  @svn_io_append_file := nil;
  @svn_io_set_file_read_only := nil;
  @svn_io_set_file_read_write := nil;
  @svn_io_set_file_read_write_carefully := nil;
  @svn_io_set_file_executable := nil;
  @svn_io_is_file_executable := nil;
  @svn_io_read_length_line := nil;
  @svn_io_file_affected_time := nil;
  @svn_io_set_file_affected_time := nil;
  @svn_io_filesizes_different_p := nil;
  @svn_io_file_checksum := nil;
  @svn_io_files_contents_same_p := nil;
  @svn_io_file_create := nil;
  @svn_io_file_lock := nil;
  @svn_io_file_lock2 := nil;
  @svn_io_file_flush_to_disk := nil;
  @svn_io_dir_file_copy := nil;
  @svn_stream_create := nil;
  @svn_stream_set_baton := nil;
  @svn_stream_set_read := nil;
  @svn_stream_set_write := nil;
  @svn_stream_set_close := nil;
  @svn_stream_empty := nil;
  @svn_stream_from_aprfile := nil;
  @svn_stream_for_stdout := nil;
  @svn_stream_from_stringbuf := nil;
  @svn_stream_compressed := nil;
  @svn_stream_read := nil;
  @svn_stream_write := nil;
  @svn_stream_close := nil;
  @svn_stream_printf := nil;
  @svn_stream_printf_from_utf8 := nil;
  @svn_stream_readline := nil;
  @svn_stream_copy := nil;
  @svn_stringbuf_from_file := nil;
  @svn_stringbuf_from_aprfile := nil;
  @svn_io_remove_file := nil;
  @svn_io_remove_dir := nil;
  @svn_io_get_dirents2 := nil;
  @svn_io_get_dirents := nil;
  @svn_io_dir_walk := nil;
  @svn_io_start_cmd := nil;
  @svn_io_wait_for_cmd := nil;
  @svn_io_run_cmd := nil;
  @svn_io_run_diff := nil;
  @svn_io_run_diff3 := nil;
  @svn_io_detect_mimetype := nil;
  @svn_io_file_open := nil;
  @svn_io_file_close := nil;
  @svn_io_file_getc := nil;
  @svn_io_file_info_get := nil;
  @svn_io_file_read := nil;
  @svn_io_file_read_full := nil;
  @svn_io_file_seek := nil;
  @svn_io_file_write := nil;
  @svn_io_file_write_full := nil;
  @svn_io_stat := nil;
  @svn_io_file_rename := nil;
  @svn_io_file_move := nil;
  @svn_io_dir_make := nil;
  @svn_io_dir_make_hidden := nil;
  @svn_io_dir_make_sgid := nil;
  @svn_io_dir_open := nil;
  @svn_io_dir_remove_nonrecursive := nil;
  @svn_io_dir_read := nil;
  @svn_io_read_version_file := nil;
  @svn_io_write_version_file := nil;
  @svn_quoprint_encode := nil;
  @svn_quoprint_decode := nil;
  @svn_quoprint_encode_string := nil;
  @svn_quoprint_decode_string := nil;
  @svn_hash_read2 := nil;
  @svn_hash_write2 := nil;
  @svn_hash_read_incremental := nil;
  @svn_hash_write_incremental := nil;
  @svn_hash_read := nil;
  @svn_hash_write := nil;
  @svn_hash_diff := nil;
  @svn_subst_eol_style_from_value := nil;
  @svn_subst_build_keywords2 := nil;
  @svn_subst_build_keywords := nil;
  @svn_subst_keywords_differ2 := nil;
  @svn_subst_keywords_differ := nil;
  @svn_subst_translate_stream3 := nil;
  @svn_subst_translate_stream2 := nil;
  @svn_subst_translate_stream := nil;
  @svn_subst_copy_and_translate3 := nil;
  @svn_subst_copy_and_translate2 := nil;
  @svn_subst_copy_and_translate := nil;
  @svn_subst_translate_cstring2 := nil;
  @svn_subst_translate_cstring := nil;
  @svn_subst_translate_string := nil;
  @svn_subst_detranslate_string := nil;
  @svn_diff_version := nil;
  @svn_diff_diff := nil;
  @svn_diff_diff3 := nil;
  @svn_diff_diff4 := nil;
  @svn_diff_contains_conflicts := nil;
  @svn_diff_contains_diffs := nil;
  @svn_diff_output := nil;
  @svn_diff_file_diff := nil;
  @svn_diff_file_diff3 := nil;
  @svn_diff_file_diff4 := nil;
  @svn_diff_file_output_unified2 := nil;
  @svn_diff_file_output_unified := nil;
  @svn_diff_file_output_merge := nil;
  @svn_base64_encode := nil;
  @svn_base64_decode := nil;
  @svn_base64_encode_string := nil;
  @svn_base64_decode_string := nil;
  @svn_base64_from_md5 := nil;
  @svn_delta_version := nil;
  @svn_txdelta_window_dup := nil;
  @svn_txdelta_next_window := nil;
  @svn_txdelta_md5_digest := nil;
  @svn_txdelta := nil;
  @svn_txdelta_target_push := nil;
  @svn_txdelta_send_string := nil;
  @svn_txdelta_send_stream := nil;
  @svn_txdelta_send_txstream := nil;
  @svn_txdelta_apply := nil;
  @svn_txdelta_to_svndiff := nil;
  @svn_txdelta_parse_svndiff := nil;
  @svn_txdelta_read_svndiff_window := nil;
  @svn_txdelta_skip_svndiff_window := nil;
  @svn_delta_default_editor := nil;
  @svn_delta_noop_window_handler := nil;
  @svn_delta_get_cancellation_editor := nil;
  @svn_delta_path_driver := nil;
  @svn_fs_version := nil;
  @svn_fs_initialize := nil;
  @svn_fs_set_warning_func := nil;
  @svn_fs_create := nil;
  @svn_fs_open := nil;
  @svn_fs_type := nil;
  @svn_fs_path := nil;
  @svn_fs_delete_fs := nil;
  @svn_fs_hotcopy := nil;
  @svn_fs_set_berkeley_errcall := nil;
  @svn_fs_berkeley_recover := nil;
  @svn_fs_berkeley_logfiles := nil;
  @svn_fs_new := nil;
  @svn_fs_create_berkeley := nil;
  @svn_fs_open_berkeley := nil;
  @svn_fs_berkeley_path := nil;
  @svn_fs_delete_berkeley := nil;
  @svn_fs_hotcopy_berkeley := nil;
  @svn_fs_create_access := nil;
  @svn_fs_set_access := nil;
  @svn_fs_get_access := nil;
  @svn_fs_access_get_username := nil;
  @svn_fs_access_add_lock_token := nil;
  @svn_fs_compare_ids := nil;
  @svn_fs_check_related := nil;
  @svn_fs_parse_id := nil;
  @svn_fs_unparse_id := nil;
  @svn_fs_begin_txn2 := nil;
  @svn_fs_begin_txn := nil;
  @svn_fs_commit_txn := nil;
  @svn_fs_abort_txn := nil;
  @svn_fs_purge_txn := nil;
  @svn_fs_txn_name := nil;
  @svn_fs_txn_base_revision := nil;
  @svn_fs_open_txn := nil;
  @svn_fs_list_transactions := nil;
  @svn_fs_txn_prop := nil;
  @svn_fs_txn_proplist := nil;
  @svn_fs_change_txn_prop := nil;
  @svn_fs_revision_root := nil;
  @svn_fs_txn_root := nil;
  @svn_fs_close_root := nil;
  @svn_fs_root_fs := nil;
  @svn_fs_is_txn_root := nil;
  @svn_fs_is_revision_root := nil;
  @svn_fs_txn_root_name := nil;
  @svn_fs_revision_root_revision := nil;
  @svn_fs_paths_changed := nil;
  @svn_fs_check_path := nil;
  @svn_fs_node_history := nil;
  @svn_fs_history_prev := nil;
  @svn_fs_history_location := nil;
  @svn_fs_is_dir := nil;
  @svn_fs_is_file := nil;
  @svn_fs_node_id := nil;
  @svn_fs_node_created_rev := nil;
  @svn_fs_node_created_path := nil;
  @svn_fs_node_prop := nil;
  @svn_fs_node_proplist := nil;
  @svn_fs_change_node_prop := nil;
  @svn_fs_props_changed := nil;
  @svn_fs_copied_from := nil;
  @svn_fs_closest_copy := nil;
  @svn_fs_merge := nil;
  @svn_fs_dir_entries := nil;
  @svn_fs_make_dir := nil;
  @svn_fs_delete := nil;
  @svn_fs_copy := nil;
  @svn_fs_revision_link := nil;
  @svn_fs_file_length := nil;
  @svn_fs_file_md5_checksum := nil;
  @svn_fs_file_contents := nil;
  @svn_fs_make_file := nil;
  @svn_fs_apply_textdelta := nil;
  @svn_fs_apply_text := nil;
  @svn_fs_contents_changed := nil;
  @svn_fs_youngest_rev := nil;
  @svn_fs_deltify_revision := nil;
  @svn_fs_revision_prop := nil;
  @svn_fs_revision_proplist := nil;
  @svn_fs_change_rev_prop := nil;
  @svn_fs_get_file_delta_stream := nil;
  @svn_fs_get_uuid := nil;
  @svn_fs_set_uuid := nil;
  @svn_fs_lock := nil;
  @svn_fs_generate_lock_token := nil;
  @svn_fs_unlock := nil;
  @svn_fs_get_lock := nil;
  @svn_fs_get_locks := nil;
  @svn_fs_print_modules := nil;
  @svn_repos_version := nil;
  @svn_repos_find_root_path := nil;
  @svn_repos_open := nil;
  @svn_repos_create := nil;
  @svn_repos_delete := nil;
  @svn_repos_fs := nil;
  @svn_repos_hotcopy := nil;
  @svn_repos_recover := nil;
  @svn_repos_recover2 := nil;
  @svn_repos_db_logfiles := nil;
  @svn_repos_path := nil;
  @svn_repos_db_env := nil;
  @svn_repos_conf_dir := nil;
  @svn_repos_svnserve_conf := nil;
  @svn_repos_lock_dir := nil;
  @svn_repos_db_lockfile := nil;
  @svn_repos_db_logs_lockfile := nil;
  @svn_repos_hook_dir := nil;
  @svn_repos_start_commit_hook := nil;
  @svn_repos_pre_commit_hook := nil;
  @svn_repos_post_commit_hook := nil;
  @svn_repos_pre_revprop_change_hook := nil;
  @svn_repos_post_revprop_change_hook := nil;
  @svn_repos_pre_lock_hook := nil;
  @svn_repos_post_lock_hook := nil;
  @svn_repos_pre_unlock_hook := nil;
  @svn_repos_post_unlock_hook := nil;
  @svn_repos_begin_report := nil;
  @svn_repos_set_path2 := nil;
  @svn_repos_set_path := nil;
  @svn_repos_link_path2 := nil;
  @svn_repos_link_path := nil;
  @svn_repos_delete_path := nil;
  @svn_repos_finish_report := nil;
  @svn_repos_abort_report := nil;
  @svn_repos_dir_delta := nil;
  @svn_repos_replay := nil;
  @svn_repos_get_commit_editor3 := nil;
  @svn_repos_get_commit_editor2 := nil;
  @svn_repos_get_commit_editor := nil;
  @svn_repos_dated_revision := nil;
  @svn_repos_get_committed_info := nil;
  @svn_repos_stat := nil;
  @svn_repos_history2 := nil;
  @svn_repos_history := nil;
  @svn_repos_trace_node_locations := nil;
  @svn_repos_get_logs3 := nil;
  @svn_repos_get_logs2 := nil;
  @svn_repos_get_logs := nil;
  @svn_repos_get_file_revs := nil;
  @svn_repos_fs_commit_txn := nil;
  @svn_repos_fs_begin_txn_for_commit := nil;
  @svn_repos_fs_begin_txn_for_update := nil;
  @svn_repos_fs_lock := nil;
  @svn_repos_fs_unlock := nil;
  @svn_repos_fs_get_locks := nil;
  @svn_repos_fs_change_rev_prop2 := nil;
  @svn_repos_fs_change_rev_prop := nil;
  @svn_repos_fs_revision_prop := nil;
  @svn_repos_fs_revision_proplist := nil;
  @svn_repos_fs_change_node_prop := nil;
  @svn_repos_fs_change_txn_prop := nil;
  @svn_repos_node_editor := nil;
  @svn_repos_node_from_baton := nil;
  @svn_repos_dump_fs2 := nil;
  @svn_repos_dump_fs := nil;
  @svn_repos_load_fs2 := nil;
  @svn_repos_load_fs := nil;
  @svn_repos_parse_dumpstream2 := nil;
  @svn_repos_get_fs_build_parser2 := nil;
  @svn_repos_parse_dumpstream := nil;
  @svn_repos_get_fs_build_parser := nil;
  @svn_repos_authz_read := nil;
  @svn_repos_authz_check_access := nil;
  @svn_opt_get_canonical_subcommand := nil;
  @svn_opt_get_option_from_code := nil;
  @svn_opt_subcommand_takes_option := nil;
  @svn_opt_print_generic_help := nil;
  @svn_opt_format_option := nil;
  @svn_opt_subcommand_help := nil;
  @svn_opt_parse_revision := nil;
  @svn_opt_args_to_target_array2 := nil;
  @svn_opt_args_to_target_array := nil;
  @svn_opt_push_implicit_dot_target := nil;
  @svn_opt_parse_num_args := nil;
  @svn_opt_parse_all_args := nil;
  @svn_opt_parse_path := nil;
  @svn_opt_print_help := nil;
  @svn_auth_ssl_server_cert_info_dup := nil;
  @svn_auth_open := nil;
  @svn_auth_set_parameter := nil;
  @svn_auth_get_parameter := nil;
  @svn_auth_first_credentials := nil;
  @svn_auth_next_credentials := nil;
  @svn_auth_save_credentials := nil;
  @svn_ra_version := nil;
  @svn_ra_initialize := nil;
  @svn_ra_create_callbacks := nil;
  @svn_ra_open2 := nil;
  @svn_ra_open := nil;
  @svn_ra_get_latest_revnum := nil;
  @svn_ra_get_dated_revision := nil;
  @svn_ra_change_rev_prop := nil;
  @svn_ra_rev_proplist := nil;
  @svn_ra_rev_prop := nil;
  @svn_ra_get_commit_editor := nil;
  @svn_ra_get_file := nil;
  @svn_ra_get_dir := nil;
  @svn_ra_do_update := nil;
  @svn_ra_do_switch := nil;
  @svn_ra_do_status := nil;
  @svn_ra_do_diff := nil;
  @svn_ra_get_log := nil;
  @svn_ra_check_path := nil;
  @svn_ra_stat := nil;
  @svn_ra_get_uuid := nil;
  @svn_ra_get_repos_root := nil;
  @svn_ra_get_locations := nil;
  @svn_ra_get_file_revs := nil;
  @svn_ra_lock := nil;
  @svn_ra_unlock := nil;
  @svn_ra_get_lock := nil;
  @svn_ra_get_locks := nil;
  @svn_ra_print_modules := nil;
  @svn_ra_print_ra_libraries := nil;
  @svn_ra_dav_init := nil;
  @svn_ra_local_init := nil;
  @svn_ra_svn_init := nil;
  @svn_ra_init_ra_libs := nil;
  @svn_ra_get_ra_library := nil;
  @svn_wc_version := nil;
  @svn_wc_adm_open3 := nil;
  @svn_wc_adm_open2 := nil;
  @svn_wc_adm_open := nil;
  @svn_wc_adm_probe_open3 := nil;
  @svn_wc_adm_probe_open2 := nil;
  @svn_wc_adm_probe_open := nil;
  @svn_wc_adm_open_anchor := nil;
  @svn_wc_adm_retrieve := nil;
  @svn_wc_adm_probe_retrieve := nil;
  @svn_wc_adm_probe_try3 := nil;
  @svn_wc_adm_probe_try2 := nil;
  @svn_wc_adm_probe_try := nil;
  @svn_wc_adm_close := nil;
  @svn_wc_adm_access_path := nil;
  @svn_wc_adm_access_pool := nil;
  @svn_wc_adm_locked := nil;
  @svn_wc_locked := nil;
  @svn_wc_is_adm_dir := nil;
  @svn_wc_get_adm_dir := nil;
  @svn_wc_set_adm_dir := nil;
  @svn_wc_init_traversal_info := nil;
  @svn_wc_edited_externals := nil;
  @svn_wc_external_item_dup := nil;
  @svn_wc_parse_externals_description2 := nil;
  @svn_wc_parse_externals_description := nil;
  @svn_wc_create_notify := nil;
  @svn_wc_dup_notify := nil;
  @svn_wc_check_wc := nil;
  @svn_wc_has_binary_prop := nil;
  @svn_wc_text_modified_p := nil;
  @svn_wc_props_modified_p := nil;
  @svn_wc_entry := nil;
  @svn_wc_entries_read := nil;
  @svn_wc_entry_dup := nil;
  @svn_wc_conflicted_p := nil;
  @svn_wc_get_ancestry := nil;
  @svn_wc_walk_entries2 := nil;
  @svn_wc_walk_entries := nil;
  @svn_wc_mark_missing_deleted := nil;
  @svn_wc_ensure_adm2 := nil;
  @svn_wc_ensure_adm := nil;
  @svn_wc_maybe_set_repos_root := nil;
  @svn_wc_dup_status2 := nil;
  @svn_wc_dup_status := nil;
  @svn_wc_status2 := nil;
  @svn_wc_status := nil;
  @svn_wc_get_status_editor2 := nil;
  @svn_wc_get_status_editor := nil;
  @svn_wc_status_set_repos_locks := nil;
  @svn_wc_copy2 := nil;
  @svn_wc_copy := nil;
  @svn_wc_delete2 := nil;
  @svn_wc_delete := nil;
  @svn_wc_add2 := nil;
  @svn_wc_add := nil;
  @svn_wc_add_repos_file := nil;
  @svn_wc_remove_from_revision_control := nil;
  @svn_wc_resolved_conflict2 := nil;
  @svn_wc_resolved_conflict := nil;
  @svn_wc_process_committed2 := nil;
  @svn_wc_process_committed := nil;
  @svn_wc_crawl_revisions2 := nil;
  @svn_wc_crawl_revisions := nil;
  @svn_wc_is_wc_root := nil;
  @svn_wc_get_actual_target := nil;
  @svn_wc_get_update_editor2 := nil;
  @svn_wc_get_update_editor := nil;
  @svn_wc_get_switch_editor2 := nil;
  @svn_wc_get_switch_editor := nil;
  @svn_wc_prop_list := nil;
  @svn_wc_prop_get := nil;
  @svn_wc_prop_set2 := nil;
  @svn_wc_prop_set := nil;
  @svn_wc_is_normal_prop := nil;
  @svn_wc_is_wc_prop := nil;
  @svn_wc_is_entry_prop := nil;
  @svn_wc_get_diff_editor3 := nil;
  @svn_wc_get_diff_editor2 := nil;
  @svn_wc_get_diff_editor := nil;
  @svn_wc_diff3 := nil;
  @svn_wc_diff2 := nil;
  @svn_wc_diff := nil;
  @svn_wc_get_prop_diffs := nil;
  @svn_wc_merge := nil;
  @svn_wc_merge_props := nil;
  @svn_wc_merge_prop_diffs := nil;
  @svn_wc_get_pristine_copy_path := nil;
  @svn_wc_cleanup2 := nil;
  @svn_wc_cleanup := nil;
  @svn_wc_relocate := nil;
  @svn_wc_revert2 := nil;
  @svn_wc_revert := nil;
  @svn_wc_create_tmp_file := nil;
  @svn_wc_translated_file := nil;
  @svn_wc_transmit_text_deltas := nil;
  @svn_wc_transmit_prop_deltas := nil;
  @svn_wc_get_default_ignores := nil;
  @svn_wc_get_ignores := nil;
  @svn_wc_add_lock := nil;
  @svn_wc_remove_lock := nil;
  @svn_client_version := nil;
  @svn_client_get_simple_prompt_provider := nil;
  @svn_client_get_username_prompt_provider := nil;
  @svn_client_get_simple_provider := nil;
  @svn_client_get_windows_simple_provider := nil;
  @svn_client_get_username_provider := nil;
  @svn_client_get_ssl_server_trust_file_provider := nil;
  @svn_client_get_ssl_client_cert_file_provider := nil;
  @svn_client_get_ssl_client_cert_pw_file_provider := nil;
  @svn_client_get_ssl_server_trust_prompt_provider := nil;
  @svn_client_get_ssl_client_cert_prompt_provider := nil;
  @svn_client_get_ssl_client_cert_pw_prompt_provider := nil;
  @svn_client_proplist_item_dup := nil;
  @svn_client_commit_item2_dup := nil;
  @svn_client_create_context := nil;
  @svn_client_checkout2 := nil;
  @svn_client_checkout := nil;
  @svn_client_update2 := nil;
  @svn_client_update := nil;
  @svn_client_switch := nil;
  @svn_client_add3 := nil;
  @svn_client_add2 := nil;
  @svn_client_add := nil;
  @svn_client_mkdir2 := nil;
  @svn_client_mkdir := nil;
  @svn_client_delete2 := nil;
  @svn_client_delete := nil;
  @svn_client_import2 := nil;
  @svn_client_import := nil;
  @svn_client_commit3 := nil;
  @svn_client_commit2 := nil;
  @svn_client_commit := nil;
  @svn_client_status2 := nil;
  @svn_client_status := nil;
  @svn_client_log2 := nil;
  @svn_client_log := nil;
  @svn_client_blame2 := nil;
  @svn_client_blame := nil;
  @svn_client_diff3 := nil;
  @svn_client_diff2 := nil;
  @svn_client_diff := nil;
  @svn_client_diff_peg3 := nil;
  @svn_client_diff_peg2 := nil;
  @svn_client_diff_peg := nil;
  @svn_client_merge := nil;
  @svn_client_merge_peg := nil;
  @svn_client_cleanup := nil;
  @svn_client_relocate := nil;
  @svn_client_revert := nil;
  @svn_client_resolved := nil;
  @svn_client_copy2 := nil;
  @svn_client_copy := nil;
  @svn_client_move3 := nil;
  @svn_client_move2 := nil;
  @svn_client_move := nil;
  @svn_client_propset2 := nil;
  @svn_client_propset := nil;
  @svn_client_revprop_set := nil;
  @svn_client_propget2 := nil;
  @svn_client_propget := nil;
  @svn_client_revprop_get := nil;
  @svn_client_proplist2 := nil;
  @svn_client_proplist := nil;
  @svn_client_revprop_list := nil;
  @svn_client_export3 := nil;
  @svn_client_export2 := nil;
  @svn_client_export := nil;
  @svn_client_ls3 := nil;
  @svn_client_ls2 := nil;
  @svn_client_ls := nil;
  @svn_client_cat2 := nil;
  @svn_client_cat := nil;
  @svn_client_lock := nil;
  @svn_client_unlock := nil;
  @svn_info_dup := nil;
  @svn_client_info := nil;
  @svn_client_url_from_path := nil;
  @svn_client_uuid_from_url := nil;
  @svn_client_uuid_from_path := nil;
  @svn_client_open_ra_session := nil;
end;

//----------------------------------------------------------------------------------------------------------------------

{ ESvnError private }

//----------------------------------------------------------------------------------------------------------------------

function ESvnError.GetCount: Integer;

begin
  Result := Length(FErrors);
end;

//----------------------------------------------------------------------------------------------------------------------

function ESvnError.GetErrorCodes(Index: Integer): Integer;

begin
  if (Index >= Low(FErrors)) and (Index <= High(FErrors)) then
    Result := FErrors[Index].Code
  else
    Result := -1;
end;

//----------------------------------------------------------------------------------------------------------------------

function ESvnError.GetMessages(Index: Integer): string;

begin
  if (Index >= Low(FErrors)) and (Index <= High(FErrors)) then
    Result := Copy(Message, FErrors[Index].SPos, FErrors[Index].SLen)
  else
    Result := '';
end;

//----------------------------------------------------------------------------------------------------------------------

{ ESvnError public }

//----------------------------------------------------------------------------------------------------------------------

constructor ESvnError.Create(Error: PSvnError);

var
  E: PSvnError;
  CurPos: Integer;
  S: string;

begin
  if Assigned(Error) then
    inherited Create(Error^.apr_err)
  else
  begin
    inherited Create(APR_SUCCESS);
    Exit;
  end;

  Message := '';
  CurPos := 1;
  E := Error;
  while Assigned(E) do
  begin
    SetLength(FErrors, Length(FErrors) + 1);

    if Assigned(E^.message) then
      S := E^.message
    else
      S := GetSvnErrorMessage(E^.apr_err);

    with FErrors[High(FErrors)] do
    begin
      Code := E^.apr_err;
      SPos := CurPos;
      SLen := Length(S);
    end;

    if Message <> '' then
    begin
      Message := Message + #13#10;
      Inc(CurPos, 2);
    end;

    Message := Message + S;
    Inc(CurPos, Length(S));

    E := E^.child;
  end;

  svn_error_clear(Error);
end;

//----------------------------------------------------------------------------------------------------------------------

destructor ESvnError.Destroy;

begin
  FErrors := nil;
  inherited Destroy;
end;

//----------------------------------------------------------------------------------------------------------------------

function GetSvnErrorMessage(Status: TAprStatus): string;

begin
  Result := '';
  if Status = SVN_NO_ERROR then
    Exit;
  SetLength(Result, 255);
  svn_strerror(Status, PChar(Result), Length(Result));
  SetLength(Result, StrLen(PChar(Result)));
end;

//----------------------------------------------------------------------------------------------------------------------

procedure SvnCheck(Error: PSvnError);

begin
  if Assigned(Error) then
    RaiseSvnError(Error);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure RaiseSvnError(Error: PSvnError);

begin
  raise ESvnError.Create(Error);
end;

//----------------------------------------------------------------------------------------------------------------------

initialization

finalization
  FreeSvnClientLib;

//----------------------------------------------------------------------------------------------------------------------

end.