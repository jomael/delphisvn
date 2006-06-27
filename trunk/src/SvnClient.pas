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
{ The Original Code is SvnClient.pas.                                                                                  }
{                                                                                                                      }
{ The Initial Developer of the Original Code is Ondrej Kelle.                                                          }
{ Portions created by Ondrej Kelle are Copyright Ondrej Kelle. All rights reserved.                                    }
{                                                                                                                      }
{ Contributors:                                                                                                        }
{   Ondrej Kelle (tondrej)                                                                                             }
{                                                                                                                      }
{**********************************************************************************************************************}
{                                                                                                                      }
{ This unit contains helper routines and classes encapsulating Subversion API calls.                                   }
{                                                                                                                      }
{**********************************************************************************************************************}

unit SvnClient;

interface

uses
  Windows, Classes, SysUtils, Contnrs,
  apr, svn_client;

const
  DefaultPropValDelimiter = ';';

type
  TSvnClient = class;
  TSvnItem = class;

  TSvnHistoryItem = class
  private
    FAuthor: string;
    FFile: string;
    FLogMessage: string;
    FOwner: TSvnItem;
    FRevision: Integer;
    FTime: TDateTime;
  public
    function GetFile: string;

    property Author: string read FAuthor;
    property LogMessage: string read FLogMessage;
    property Owner: TSvnItem read FOwner;
    property Revision: Integer read FRevision;
    property Time: TDateTime read FTime;
  end;

  TSvnItem = class
  private
    FAbsent: Boolean;
    FBaseRevision: Integer;
    FCheckSum: string;
    FCommitAuthor: string;
    FCommittedRevision: Integer;
    FCommitTime: TDateTime;
    FConflictNewFile: string;
    FConflictOldFile: string;
    FConflictWorkingFile: string;
    FCopied: Boolean;
    FCopiedFromRevision: Integer;
    FCopiedFromURL: string;
    FDeleted: Boolean;
    FFileAttr: Cardinal;
    FHistory: TList;
    FIncomplete: Boolean;
    FItems: TList;
    FKind: TSvnNodeKind;
    FLargeImageIndex: Integer;
    FLastCommitAuthor: string;
    FLastCommitRevision: Integer;
    FLastCommitTime: TDateTime;
    FLockComment: string;
    FLockDAVComment: Boolean;
    FLocked: Boolean;
    FLockExpirationTime: TDateTime;
    FLockOwner: string;
    FLockPath: string;
    FLockTime: TDateTime;
    FLockToken: string;
    FParent: TSvnItem;
    FPathName: string;
    FPropRejectFile: string;
    FProps: TStringList;
    FPropStatus: TSvnWCStatusKind;
    FPropTime: TDateTime;
    FPropValDelimiter: Char;
    FReloadStack: TStack;
    FReloadUnversionedList: TList;
    FRemotePropStatus: TSvnWCStatusKind;
    FRemoteTextStatus: TSvnWCStatusKind;
    FRepository: string;
    FSchedule: TSvnWCSchedule;
    FSmallImageIndex: Integer;
    FSvnClient: TSvnClient;
    FSvnPathName: string;
    FSwitched: Boolean;
    FTag: Integer;
    FTextStatus: TSvnWCStatusKind;
    FTextTime: TDateTime;
    FURL: string;
    FUUID: string;

    FOnDestroy: TNotifyEvent;

    procedure ClearHistory;
    procedure ClearItems;
    function GetCount: Integer;
    function GetFileAttr: Cardinal;
    function GetHistoryCount: Integer;
    function GetHistoryItems(Index: Integer): TSvnHistoryItem;
    function GetIsDirectory: Boolean;
    function GetItems(Index: Integer): TSvnItem;
    function GetPropCount: Integer;
    function GetPropNames(Index: Integer): string;
    function GetPropValueFromIndex(Index: Integer): string;
    function GetPropValues(const Name: string): string;
    procedure LoadStatus(const Status: TSvnWCStatus2);
    procedure ReloadHistory;
    procedure ReloadProps;
    procedure SetPropValues(const Name, Value: string);
    procedure SortItems(Recurse: Boolean);
  protected
    procedure DoDestroy; virtual;
    procedure DoWCStatus(Path: PChar; const Status: TSvnWCStatus2);
  public
    constructor Create(ASvnClient: TSvnClient; AParent: TSvnItem; const APathName: string; Recurse: Boolean = False;
      Update: Boolean = False); overload;
    constructor Create(ASvnClient: TSvnClient; AParent: TSvnItem; const ASvnPathName: string;
      const Status: TSvnWCStatus2); overload;
    destructor Destroy; override;

    function Add(Item: TSvnItem): Integer;
    procedure Clear;
    function IndexOf(Item: TSvnItem): Integer; overload;
    function IndexOf(const PathName: string; SvnPath: Boolean = False): Integer; overload;
    procedure Reload(Recurse: Boolean = False; Update: Boolean = False);
    procedure Remove(Item: TSvnItem);

    property Absent: Boolean read FAbsent;
    property BaseRevision: Integer read FBaseRevision;
    property CheckSum: string read FCheckSum;
    property CommitAuthor: string read FCommitAuthor;
    property CommittedRevision: Integer read FCommittedRevision;
    property CommitTime: TDateTime read FCommitTime;
    property ConflictNewFile: string read FConflictNewFile;
    property ConflictOldFile: string read FConflictOldFile;
    property ConflictWorkingFile: string read FConflictWorkingFile;
    property Copied: Boolean read FCopied;
    property CopiedFromRevision: Integer read FCopiedFromRevision;
    property CopiedFromURL: string read FCopiedFromURL;
    property Count: Integer read GetCount;
    property Deleted: Boolean read FDeleted;
    property FileAttr: Cardinal read GetFileAttr;
    property HistoryCount: Integer read GetHistoryCount;
    property HistoryItems[Index: Integer]: TSvnHistoryItem read GetHistoryItems;
    property Incomplete: Boolean read FIncomplete;
    property Items[Index: Integer]: TSvnItem read GetItems; default;
    property IsDirectory: Boolean read GetIsDirectory;
    property Kind: TSvnNodeKind read FKind;
    property LargeImageIndex: Integer read FLargeImageIndex write FLargeImageIndex;
    property LastCommitAuthor: string read FLastCommitAuthor;
    property LastCommitRevision: Integer read FLastCommitRevision;
    property LastCommitTime: TDateTime read FLastCommitTime;
    property LockComment: string read FLockComment;
    property LockDAVComment: Boolean read FLockDAVComment;
    property LockExpirationTime: TDateTime read FLockExpirationTime;
    property Locked: Boolean read FLocked;
    property LockOwner: string read FLockOwner;
    property LockPath: string read FLockPath;
    property LockTime: TDateTime read FLockTime;
    property LockToken: string read FLockToken;
    property Parent: TSvnItem read FParent;
    property PathName: string read FPathName;
    property PropCount: Integer read GetPropCount;
    property PropNames[Index: Integer]: string read GetPropNames;
    property PropRejectFile: string read FPropRejectFile;
    property PropStatus: TSvnWCStatusKind read FPropStatus;
    property PropTime: TDateTime read FPropTime;
    property PropValDelimiter: Char read FPropValDelimiter write FPropValDelimiter default ';';
    property PropValueFromIndex[Index: Integer]: string read GetPropValueFromIndex;
    property PropValues[const Name: string]: string read GetPropValues write SetPropValues;
    property RemotePropStatus: TSvnWCStatusKind read FRemotePropStatus;
    property RemoteTextStatus: TSvnWCStatusKind read FRemoteTextStatus;
    property Repository: string read FRepository;
    property Schedule: TSvnWCSchedule read FSchedule;
    property SmallImageIndex: Integer read FSmallImageIndex write FSmallImageIndex;
    property SvnClient: TSvnClient read FSvnClient;
    property SvnPathName: string read FSvnPathName;
    property Switched: Boolean read FSwitched;
    property TextTime: TDateTime read FTextTime;
    property Tag: Integer read FTag write FTag;
    property TextStatus: TSvnWCStatusKind read FTextStatus;
    property URL: string read FURL;
    property UUID: string read FUUID;

    property OnDestroy: TNotifyEvent read FOnDestroy write FOnDestroy;
  end;

  TSvnItemArray = array of TSvnItem;

  TSSLServerTrustFailures = set of (sslCertNotYetValid, sslCertExpired, sslCertHostNameMismatch,
    sslCertAuthorityUnknown, sslCertOther);

  TLoginPromptEvent = procedure(Sender: TObject; const Realm: string; var UserName, Password: string;
    var Cancel: Boolean) of object;
  TUserNamePromptEvent = procedure(Sender: TObject; const Realm: string; var UserName: string;
    var Cancel: Boolean) of object;
  TSSLServerTrustPrompt = procedure(Sender: TObject; const Realm: string; const CertInfo: TSvnAuthSSLServerCertInfo;
    Failures: TSSLServerTrustFailures; var Cancel: Boolean) of object;
  TSSLClientCertPrompt = procedure(Sender: TObject; const Realm: string; var CertFileName: string;
    var Cancel: Boolean) of object;
  TSSLClientPasswordPrompt = procedure(Sender: TObject; const Realm: string; var Password: string;
    var Cancel: Boolean) of object;

  TSvnNotifyCallback = procedure(Sender: TObject; const Path, MimeType: string; Action: TSvnWCNotifyAction;
    Kind: TSvnNodeKind; ContentState, PropState: TSvnWCNotifyState; Revision: TSvnRevNum; var Cancel: Boolean)
    of object;
  TSvnStatusCallback = procedure(Sender: TObject; Item: TSvnItem; var Cancel: Boolean) of object;

  TSvnClient = class
  private
    FAllocator: PAprAllocator;
    FCancelled: Boolean;
    FCommitLogMessage: string;
    FConfigDir: string;
    FCtx: PSvnClientCtx;
    FPassword: string;
    FPool: PAprPool; // main pool
    FPoolUtf8: PAprPool; // pool for UTF-8 routines
    FUserName: string;

    FNotifyCallback: TSvnNotifyCallback;
    FStatusCallback: TSvnStatusCallback;

    FOnLoginPrompt: TLoginPromptEvent;
    FOnSSLClientCertPrompt: TSSLClientCertPrompt;
    FOnSSLClientPasswordPrompt: TSSLClientPasswordPrompt;
    FOnSSLServerTrustPrompt: TSSLServerTrustPrompt;
    FOnUserNamePrompt: TUserNamePromptEvent;

    function GetInitialized: Boolean;
  protected
    function DoLoginPrompt(const Realm: string; var UserName, Password: string): Boolean; virtual;
    function DoNotify(const Path, MimeType: string; Action: TSvnWCNotifyAction; Kind: TSvnNodeKind;
      ContentState, PropState: TSvnWCNotifyState; Revision: TSvnRevNum): Boolean; virtual;
    function DoSSLClientCertPrompt(const Realm: string; var CertFileName: string): Boolean; virtual;
    function DoSSLClientPasswordPrompt(const Realm: string; var Password: string): Boolean; virtual;
    function DoSSLServerTrustPrompt(const Realm: string; const CertInfo: TSvnAuthSSLServerCertInfo;
      Failures: TSSLServerTrustFailures): Boolean; virtual;
    function DoUserNamePrompt(const Realm: string; var UserName: string): Boolean; virtual;
    function DoWCStatus(Path: PChar; const Status: TSvnWCStatus2): Boolean;

    property Cancelled: Boolean read FCancelled;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Cleanup(const PathName: string; SubPool: PAprPool = nil);
    function Commit(PathNames: TStrings; const LogMessage: string; Callback: TSvnNotifyCallback = nil;
      Recurse: Boolean = True; KeepLocks: Boolean = False; SubPool: PAprPool = nil): Boolean;
    procedure Finalize;
    function GetModifications(const PathName: string; Callback: TSvnStatusCallback = nil;
      Recurse: Boolean = True; Update: Boolean = False; IgnoreExternals: Boolean = False;
      SubPool: PAprPool = nil): TSvnRevNum;
    procedure GetProps(Props: PAprArrayHeader; Strings: TStrings; SubPool: PAprPool = nil;
      Delimiter: Char = DefaultPropValDelimiter);
    procedure Initialize(const AConfigDir: string = '');
    function IsPathVersioned(const PathName: string): Boolean;
    function NativePathToSvnPath(const NativePath: string; SubPool: PAprPool = nil): string;
    function PathNamesToAprArray(PathNames: TStrings; SubPool: PAprPool = nil): PAprArrayHeader;
    procedure Revert(PathNames: TStrings; Callback: TSvnNotifyCallback = nil; Recurse: Boolean = True;
      SubPool: PAprPool = nil);
    function SvnPathToNativePath(const SvnPath: string; SubPool: PAprPool = nil): string;
    procedure Update(PathNames: TStrings; Callback: TSvnNotifyCallback = nil; Recurse: Boolean = True;
      IgnoreExternals: Boolean = False; SubPool: PAprPool = nil);

    property Allocator: PAprAllocator read FAllocator;
    property ConfigDir: string read FConfigDir;
    property Ctx: PSvnClientCtx read FCtx;
    property Initialized: Boolean read GetInitialized;
    property Password: string read FPassword write FPassword;
    property Pool: PAprPool read FPool;
    property UserName: string read FUserName write FUserName;

    property OnLoginPrompt: TLoginPromptEvent read FOnLoginPrompt write FOnLoginPrompt;
    property OnSSLClientCertPrompt: TSSLClientCertPrompt read FOnSSLClientCertPrompt write FOnSSLClientCertPrompt;
    property OnSSLClientPasswordPrompt: TSSLClientPasswordPrompt read FOnSSLClientPasswordPrompt
      write FOnSSLClientPasswordPrompt;
    property OnSSLServerTrustPrompt: TSSLServerTrustPrompt read FOnSSLServerTrustPrompt write FOnSSLServerTrustPrompt;
    property OnUserNamePrompt: TUserNamePromptEvent read FOnUserNamePrompt write FOnUserNamePrompt;
  end;

resourcestring
  SNodeKindNone = 'None';
  SNodeKindFile = 'File';
  SNodeKindDir = 'Directory';
  SNodeKindUnknown = 'Unknown';

  SWcStatusNone = '';
  SWcStatusUnversioned = 'Unversioned';
  SWcStatusNormal = 'Normal';
  SWcStatusAdded = 'Added';
  SWcStatusMissing = 'Missing';
  SWcStatusDeleted = 'Deleted';
  SWcStatusReplaced = 'Replaced';
  SWcStatusModified = 'Modified';
  SWcStatusMerged = 'Merged';
  SWcStatusConflicted = 'Conflicted';
  SWcStatusIgnored = 'Ignored';
  SWcStatusObstructed = 'Obstructed';
  SWcStatusExternal = 'External';
  SWcStatusIncomplete = 'Incomplete';

  SWcNotifyAdd = 'Added';
  SWcNotifyCopy = 'Copied';
  SWcNotifyDelete = 'Deleted';
  SWcNotifyRestore = 'Restored';
  SWcNotifyRevert = 'Reverted';
  SWcNotifyFailedRevert = 'Revert Failed';
  SWcNotifyResolved = 'Resolved';
  SWcNotifySkip = 'Skipped';
  SWcNotifyUpdateDelete = 'Deleted';
  SWcNotifyUpdateAdd = 'Added';
  SWcNotifyUpdateUpdate = 'Updated';
  SWcNotifyUpdateCompleted = 'Completed';
  SWcNotifyUpdateExternal = 'External Updated';
  SWcNotifyStatusCompleted = 'Completed';
  SWcNotifyStatusExternal = 'External';
  SWcNotifyCommitModified = 'Modified';
  SWcNotifyCommitAdded = 'Added';
  SWcNotifyCommitDeleted = 'Deleted';
  SWcNotifyCommitReplaced = 'Replaced';
  SWcNotifyCommitPostfixTxdelta = 'File Sent';
  SWcNotifyBlameRevision = 'Blame Revision';
  SWcNotifyLocked = 'Locked';
  SWcNotifyUnlocked = 'Unlocked';
  SWcNotifyFailedLock = 'Lock Failed';
  SWcNotifyFailedUnlock = 'Unlock Failed';

const
  SvnLineBreak = #10;
  SvnPathDelim = '/';
  NodeKindStrings: array[TSvnNodeKind] of string = (SNodeKindNone, SNodeKindFile, SNodeKindDir, SNodeKindUnknown);

function AprTimeToDateTime(AprTime: TAprTime): TDateTime;
function TzToUTCDateTime(Value: TDateTime): TDateTime;
function UTCToTzDateTime(Value: TDateTime): TDateTime;

function FileAttrStr(Attr: Cardinal): string;
function NotifyActionStr(Action: TSvnWCNotifyAction): string;
function StatusKindStr(Status: TSvnWCStatusKind): string;
function SvnExcludeTrailingPathDelimiter(const S: string): string;
function SvnExtractFileDrive(const SvnFileName: string): string;
function SvnExtractFileName(const SvnFileName: string): string;
function SvnExtractFilePath(const SvnFileName: string): string;
function SvnIncludeTrailingPathDelimiter(const S: string): string;

implementation

uses
  RTLConsts, ActiveX, ComObj, ShlObj, TypInfo, WinSock;

{ helper routines }

//----------------------------------------------------------------------------------------------------------------------

function TzToUTCDateTime(Value: TDateTime): TDateTime;

var
  TZ: TTimeZoneInformation;

begin
  Result := Value;

  case GetTimeZoneInformation(TZ) of
    TIME_ZONE_ID_DAYLIGHT:
      Result := Result + (TZ.Bias + TZ.DaylightBias) / MinsPerDay;
    TIME_ZONE_ID_STANDARD:
      Result := Result + (TZ.Bias + TZ.StandardBias) / MinsPerDay;
    TIME_ZONE_ID_UNKNOWN:
      Result := Result + TZ.Bias / MinsPerDay;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function UTCToTzDateTime(Value: TDateTime): TDateTime;

var
  TZ: TTimeZoneInformation;

begin
  Result := Value;

  case GetTimeZoneInformation(TZ) of
    TIME_ZONE_ID_DAYLIGHT:
      Result := Result - (TZ.Bias + TZ.DaylightBias) / MinsPerDay;
    TIME_ZONE_ID_STANDARD:
      Result := Result - (TZ.Bias + TZ.StandardBias) / MinsPerDay;
    TIME_ZONE_ID_UNKNOWN:
      Result := Result - TZ.Bias / MinsPerDay;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function AprTimeToDateTime(AprTime: TAprTime): TDateTime;

begin
  if AprTime = 0 then
    Result := 0
  else
    Result := UTCToTzDateTime(UnixDateDelta + AprTime / SecsPerDay / 1000000);
end;

//----------------------------------------------------------------------------------------------------------------------

function CompareSvnPathNames(Item1, Item2: Pointer): Integer;

begin
  Result := AnsiCompareText(TSvnItem(Item1).SvnPathName, TSvnItem(Item2).SvnPathName);
end;

//----------------------------------------------------------------------------------------------------------------------

function CompareNativePathNames(P1, P2: Pointer): Integer;

var
  Item1: TSvnItem absolute P1;
  Item2: TSvnItem absolute P2;

begin
  Result := 0;
  if Assigned(P1) and Assigned(P2) then
  begin
    Result := Ord(Item2.IsDirectory) - Ord(Item1.IsDirectory);
    if Result = 0 then
      Result := AnsiCompareText(Item1.PathName, Item2.PathName);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function GetAppDataDir: string;

var
  Malloc: IMalloc;
  P: PItemIDList;

begin
  Result := '';

  OleCheck(SHGetMalloc(Malloc));

  if Succeeded(SHGetSpecialFolderLocation(0, CSIDL_APPDATA, P)) then
  begin
    SetLength(Result, MAX_PATH);
    if SHGetPathFromIDList(P, PChar(Result)) then
      SetLength(Result, StrLen(PChar(Result)));
    Malloc.Free(P);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function LogMessage(baton: Pointer; changed_paths: PAprHash; revision: TSvnRevNum; author, date, message: PChar;
  pool: PAprPool): PSvnError; cdecl;

var
  Item: TSvnHistoryItem;
  Time: TAprTime;

begin
  Result := nil;

  Item := TSvnHistoryItem.Create;
  try
    Item.FOwner := baton;
    Item.FRevision := revision;
    Item.FAuthor := author;
    Item.FLogMessage := message;
    if Assigned(date) and (date^ <> #0) then
    begin
      SvnCheck(svn_time_from_cstring(Time, date, pool));
      Item.FTime := AprTimeToDateTime(Time);
    end
    else
      Item.FTime := 0;
      
    TSvnItem(baton).FHistory.Add(Item);
    apr_pool_clear(pool);
  except
    Item.Free;
    raise;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function NotifyActionStr(Action: TSvnWCNotifyAction): string;

const
  NotifyActionStrings: array[TSvnWcNotifyAction] of string = (SWcNotifyAdd, SWcNotifyCopy, SWcNotifyDelete,
    SWcNotifyRestore, SWcNotifyRevert, SWcNotifyFailedRevert, SWcNotifyResolved, SWcNotifySkip, SWcNotifyUpdateDelete,
    SWcNotifyUpdateAdd, SWcNotifyUpdateUpdate, SWcNotifyUpdateCompleted, SWcNotifyUpdateExternal,
    SWcNotifyStatusCompleted, SWcNotifyStatusExternal, SWcNotifyCommitModified, SWcNotifyCommitAdded,
    SWcNotifyCommitDeleted, SWcNotifyCommitReplaced, SWcNotifyCommitPostfixTxdelta, SWcNotifyBlameRevision,
    SWcNotifyLocked, SWcNotifyUnlocked, SWcNotifyFailedLock, SWcNotifyFailedUnlock);

begin
  Result := NotifyActionStrings[Action];
end;

//----------------------------------------------------------------------------------------------------------------------

function SimplePrompt(out cred: PSvnAuthCredSimple; baton: Pointer; realm, username: PChar; may_save: TSvnBoolean;
  pool: PAprPool): PSvnError; cdecl;

var
  SRealm, SUserName, SPassword: string;

begin
  Result := nil;
  cred := nil;
  if Assigned(realm) then
    SetString(SRealm, realm, StrLen(realm))
  else
    SRealm := '';
  if Assigned(username) then
    SetString(SUserName, username, StrLen(username))
  else
    SUserName := '';
  SPassword := '';

  if not TSvnClient(baton).DoLoginPrompt(SRealm, SUserName, SPassword) then // not cancelled
  begin
    cred := apr_pcalloc(pool, SizeOf(TSvnAuthCredSimple));
    if SUserName <> '' then
      cred^.username := apr_pstrdup(pool, PChar(SUserName));
    if SPassword <> '' then
      cred^.password := apr_pstrdup(pool, PChar(SPassword));
    cred^.may_save := may_save;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function SSLClientCertPrompt(out cred: PSvnAuthCredSSLClientCert; baton: Pointer; realm: PChar; may_save: TSvnBoolean;
  pool: PAprPool): PSvnError; cdecl;

var
  SRealm, SCertFileName: string;

begin
  Result := nil;
  cred := nil;
  if Assigned(realm) then
    SetString(SRealm, realm, StrLen(realm))
  else
    SRealm := '';
  SCertFileName := '';
  if not TSvnClient(baton).DoSSLClientCertPrompt(SRealm, SCertFileName) then
  begin
    cred := apr_pcalloc(pool, SizeOf(TSvnAuthCredSSLClientCert));
    if SCertFileName <> '' then
      cred^.cert_file := apr_pstrdup(pool, PChar(SCertFileName));
    cred^.may_save := may_save;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function SSLClientPasswordPrompt(out cred: PSvnAuthCredSSLClientCertPw; baton: Pointer; realm: PChar;
  may_save: TSvnBoolean; pool: PAprPool): PSvnError; cdecl;

var
  SRealm, SPassword: string;

begin
  Result := nil;
  cred := nil;
  if Assigned(realm) then
    SetString(SRealm, realm, StrLen(realm))
  else
    SRealm := '';
  SPassword := '';
  if not TSvnClient(baton).DoSSLClientPasswordPrompt(SRealm, SPassword) then
  begin
    cred := apr_pcalloc(pool, SizeOf(TSvnAuthCredSSLClientCertPw));
    if SPassword <> '' then
      cred^.password := apr_pstrdup(pool, PChar(SPassword));
    cred^.may_save := may_save;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function SSLServerTrustPrompt(out cred: PSvnAuthCredSSLServerTrust; baton: Pointer; realm: PChar; failures: Cardinal;
  cert_info: PSvnAuthSSLServerCertInfo; may_save: TSvnBoolean; pool: PAprPool): PSvnError; cdecl;

var
  SRealm: string;
  Failed: TSSLServerTrustFailures;

begin
  Result := nil;
  cred := nil;
  if Assigned(realm) then
    SetString(SRealm, realm, StrLen(realm))
  else
    SRealm := '';

  Failed := [];
  if failures and SVN_AUTH_SSL_NOTYETVALID <> 0 then
    Include(Failed, sslCertNotYetValid);
  if failures and SVN_AUTH_SSL_EXPIRED <> 0 then
    Include(Failed, sslCertExpired);
  if failures and SVN_AUTH_SSL_CNMISMATCH <> 0 then
    Include(Failed, sslCertHostNameMismatch);
  if failures and SVN_AUTH_SSL_UNKNOWNCA <> 0 then
    Include(Failed, sslCertAuthorityUnknown);
  if failures and SVN_AUTH_SSL_OTHER <> 0 then
    Include(Failed, sslCertOther);

  if not TSvnClient(baton).DoSSLServerTrustPrompt(SRealm, cert_info^, Failed) then
  begin
    cred := apr_pcalloc(pool, SizeOf(TSvnAuthCredSSLServerTrust));
    cred^.may_save := False;
    cred^.accepted_failures := failures;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function FileAttrStr(Attr: Cardinal): string;

begin
  Result := '';
  if Attr and faReadOnly <> 0 then
    Result := Result + 'R';
  if Attr and faHidden <> 0 then
    Result := Result + 'H';
  if Attr and faSysFile <> 0 then
    Result := Result + 'S';
  if Attr and faArchive <> 0 then
    Result := Result + 'A';
end;

//----------------------------------------------------------------------------------------------------------------------

function StatusKindStr(Status: TSvnWCStatusKind): string;

begin
  case Status of
    svnWcStatusNone:
      Result := SWcStatusNone;
    svnWcStatusUnversioned:
      Result := SWcStatusUnversioned;
    svnWcStatusNormal:
      Result := SWcStatusNormal;
    svnWcStatusAdded:
      Result := SWcStatusAdded;
    svnWcStatusMissing:
      Result := SWcStatusMissing;
    svnWcStatusDeleted:
      Result := SWcStatusDeleted;
    svnWcStatusReplaced:
      Result := SWcStatusReplaced;
    svnWcStatusModified:
      Result := SWcStatusModified;
    svnWcStatusMerged:
      Result := SWcStatusMerged;
    svnWcStatusConflicted:
      Result := SWcStatusConflicted;
    svnWcStatusIgnored:
      Result := SWcStatusIgnored;
    svnWcStatusObstructed:
      Result := SWcStatusObstructed;
    svnWcStatusExternal:
      Result := SWcStatusExternal;
    svnWcStatusIncomplete:
      Result := SWcStatusIncomplete;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function SvnContextCancel(cancel_baton: Pointer): PSvnError; cdecl;

begin
  if TSvnClient(cancel_baton).Cancelled then
    Result := svn_error_create(SVN_ERR_CANCELLED, nil, 'Cancelled by user')
  else
    Result := nil;
end;

//----------------------------------------------------------------------------------------------------------------------

function SvnContextLogMessage(out log_msg, tmp_file: PChar; commit_items: PAprArrayHeader; baton: Pointer;
  pool: PAprPool): PSvnError; cdecl;

begin
  Result := nil;
  log_msg := apr_pstrdup(pool, PChar(TSvnClient(baton).FCommitLogMessage));
end;

//----------------------------------------------------------------------------------------------------------------------

procedure SvnContextNotify(baton: Pointer; path: PChar; action: TSvnWCNotifyAction; kind: TSvnNodeKind;
  mime_type: PChar; content_state, prop_state: TSvnWCNotifyState; revision: TSvnRevNum); cdecl;

begin
  TSvnClient(baton).DoNotify(path, mime_type, action, kind, content_state, prop_state, revision);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure SvnContextProgress(progress, total: TAprOff; baton: Pointer; pool: PAprPool); cdecl;

begin
  OutputDebugString(PChar(Format('SvnContextProgress(%d, %d) %.2f%%', [progress, total, 100 * progress / total])));
end;

//----------------------------------------------------------------------------------------------------------------------

function SvnExcludeTrailingPathDelimiter(const S: string): string;

var
  L: Integer;

begin
  Result := S;
  if Result = '' then
    Exit;

  L := Length(Result);
  if IsDelimiter(SvnPathDelim, Result, L) then
    SetLength(Result, L - 1);
end;

//----------------------------------------------------------------------------------------------------------------------

function SvnExtractFileDrive(const SvnFileName: string): string;

var
  I, J: Integer;

begin
  if (Length(SvnFileName) >= 2) and (SvnFileName[2] = DriveDelim) then
    Result := Copy(SvnFileName, 1, 2)
  else if (Length(SvnFileName) >= 2) and (SvnFileName[1] = SvnPathDelim) and
    (SvnFileName[2] = SvnPathDelim) then
  begin
    J := 0;
    I := 3;
    While (I < Length(SvnFileName)) and (J < 2) do
    begin
      if SvnFileName[I] = SvnPathDelim then Inc(J);
      if J < 2 then Inc(I);
    end;
    if SvnFileName[I] = SvnPathDelim then Dec(I);
    Result := Copy(SvnFileName, 1, I);
  end else Result := '';
end;

//----------------------------------------------------------------------------------------------------------------------

function SvnExtractFileName(const SvnFileName: string): string;

var
  I: Integer;

begin
  I := LastDelimiter(DriveDelim + SvnPathDelim, SvnFileName);
  Result := Copy(SvnFileName, I + 1, MaxInt);
end;

//----------------------------------------------------------------------------------------------------------------------

function SvnExtractFilePath(const SvnFileName: string): string;

var
  I: Integer;

begin
  I := LastDelimiter(SvnPathDelim + DriveDelim, SvnFileName);
  Result := Copy(SvnFileName, 1, I);
end;

//----------------------------------------------------------------------------------------------------------------------

function SvnIncludeTrailingPathDelimiter(const S: string): string;

var
  L: Integer;

begin
  Result := S;
  if Result = '' then
    Exit;

  L := Length(Result);
  if not IsDelimiter(SvnPathDelim, Result, L) then
    Result := Result + SvnPathDelim;
end;

//----------------------------------------------------------------------------------------------------------------------

function UserNamePrompt(out cred: PSvnAuthCredUsername; baton: Pointer; realm: PChar; may_save: TSvnBoolean;
  pool: PAprPool): PSvnError; cdecl;

var
  SRealm, SUserName: string;

begin
  Result := nil;
  cred := nil;
  if Assigned(realm) then
    SetString(SRealm, realm, StrLen(realm))
  else
    SRealm := '';
  SUserName := '';

  if not TSvnClient(baton).DoUserNamePrompt(SRealm, SUserName) then
  begin
    cred := apr_pcalloc(pool, SizeOf(TSvnAuthCredUserName));
    if SUserName <> '' then
      cred^.username := apr_pstrdup(pool, PChar(SUserName));
    cred^.may_save := may_save;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure WCStatus(baton: Pointer; path: PChar; status: PSvnWCStatus2); cdecl;

begin
  if Assigned(status) then
    TSvnItem(baton).DoWCStatus(path, status^);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure WCStatus2(baton: Pointer; path: PChar; status: PSvnWCStatus2); cdecl;

begin
  if Assigned(status) then
    TSvnClient(baton).DoWCStatus(path, status^);
end;

//----------------------------------------------------------------------------------------------------------------------

function DummyInfoReceiver(baton: Pointer; path: PChar; const info: TSvnInfo; pool: PAprPool): PSvnError; cdecl;

begin
  Result := nil;
end;

//----------------------------------------------------------------------------------------------------------------------

{ TSvnHistoryItem public }

//----------------------------------------------------------------------------------------------------------------------

function TSvnHistoryItem.GetFile: string;

var
  SubPool: PAprPool;
  PegRevision, Revision: TSvnOptRevision;
  Buffer: PSvnStringBuf;
  Stream: PSvnStream;

begin
  Result := '';

  if FFile = '' then
  begin
    AprCheck(apr_pool_create_ex(SubPool, FOwner.SvnClient.Pool, nil, FOwner.SvnClient.Allocator));
    try
      FillChar(PegRevision, SizeOf(TSvnOptRevision), 0);
      PegRevision.Kind := svnOptRevisionUnspecified;;
      FillChar(Revision, SizeOf(TSvnOptRevision), 0);
      Revision.Kind := svnOptRevisionNumber;
      Revision.Value.number := FRevision;
      Buffer := svn_stringbuf_create('', SubPool);
      Stream := svn_stream_from_stringbuf(Buffer, SubPool);
      SvnCheck(svn_client_cat2(Stream, PChar(FOwner.SvnPathName), @PegRevision, @Revision, FOwner.SvnClient.Ctx,
        SubPool));
      SetString(FFile, Buffer.data, Buffer.len);
    finally
      apr_pool_destroy(SubPool);
    end;
  end;

  Result := FFile;
end;

//----------------------------------------------------------------------------------------------------------------------

{ TSvnItem private }

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnItem.ClearHistory;

var
  I: Integer;

begin
  if Assigned(FHistory) then
  begin
    for I := 0 to FHistory.Count - 1 do
      TObject(FHistory[I]).Free;
    FreeAndNil(FHistory);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnItem.ClearItems;

var
  I: Integer;

begin
  if Assigned(FItems) then
  begin
    for I := FItems.Count - 1 downto 0 do
      TObject(FItems[I]).Free;
    FreeAndNil(FItems);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnItem.GetCount: Integer;

begin
  if not Assigned(FItems) then
    Reload;
  Result := FItems.Count;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnItem.GetFileAttr: Cardinal;

begin
  if FFileAttr = 0 then
    FFileAttr := GetFileAttributes(PChar(FPathName));
  Result := FFileAttr;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnItem.GetHistoryCount: Integer;

begin
  if not Assigned(FHistory) then
    ReloadHistory;
  Result := FHistory.Count;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnItem.GetHistoryItems(Index: Integer): TSvnHistoryItem;

begin
  if not Assigned(FHistory) then
    ReloadHistory;
  Result := FHistory[Index];
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnItem.GetIsDirectory: Boolean;

begin
  case FKind of
    svnNodeNone:
      Result := GetFileAttr and FILE_ATTRIBUTE_DIRECTORY <> 0;
    svnNodeDir:
      Result := True;
    else
      Result := False;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnItem.GetItems(Index: Integer): TSvnItem;

begin
  if not Assigned(FItems) then
    Reload;
  Result := FItems[Index];
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnItem.GetPropCount: Integer;

begin
  if not Assigned(FProps) then
    ReloadProps;
  Result := FProps.Count;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnItem.GetPropNames(Index: Integer): string;

begin
  if not Assigned(FProps) then
    ReloadProps;
  Result := FProps.Names[Index];
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnItem.GetPropValueFromIndex(Index: Integer): string;

begin
  if not Assigned(FProps) then
    ReloadProps;
  Result := FProps.ValueFromIndex[Index];
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnItem.GetPropValues(const Name: string): string;

begin
  if not Assigned(FProps) then
    ReloadProps;
  Result := FProps.Values[Name];
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnItem.LoadStatus(const Status: TSvnWCStatus2);

begin
  if Assigned(Status.entry) then
  begin
    FBaseRevision := Status.entry^.revision;
    FURL := Status.entry^.url;
    FRepository := Status.entry^.repos;
    FUUID := Status.entry^.uuid;
    FKind := Status.entry^.kind;
    FSchedule := Status.entry^.schedule;
    FCopied := Status.entry^.copied;
    FDeleted := Status.entry^.deleted;
    FAbsent := Status.entry^.absent;
    FIncomplete := Status.entry^.incomplete;
    FCopiedFromURL := Status.entry^.copyfrom_url;
    FCopiedFromRevision := Status.entry^.copyfrom_rev;
    FConflictOldFile := Status.entry^.conflict_old;
    FConflictNewFile := Status.entry^.conflict_new;
    FConflictWorkingFile := Status.entry^.conflict_wrk;
    FPropRejectFile := Status.entry^.prejfile;
    FTextTime := AprTimeToDateTime(Status.entry^.text_time);
    FPropTime := AprTimeToDateTime(Status.entry^.prop_time);
    FCheckSum := Status.entry^.checksum;
    FCommittedRevision := Status.entry^.cmt_rev;
    FCommitAuthor := Status.entry^.cmt_author;
    FCommitTime := AprTimeToDateTime(Status.entry^.cmt_date);
    FLockToken := Status.entry^.lock_token;
    FLockOwner := Status.entry^.lock_owner;
    FLockComment := Status.entry^.lock_comment;
    FLockTime := AprTimeToDateTime(Status.entry^.lock_creation_date);
  end
  else
  begin
    FBaseRevision := -1;
    FURL := '';
    FRepository := '';
    FUUID := '';
    FKind := svnNodeNone;
    FSchedule := svnWcScheduleNormal;
    FCopied := False;
    FDeleted := False;
    FAbsent := False;
    FIncomplete := False;
    FCopiedFromURL := '';
    FCopiedFromRevision := -1;
    FConflictOldFile := '';
    FConflictNewFile := '';
    FConflictWorkingFile := '';
    FPropRejectFile := '';
    FTextTime := 0;
    FPropTime := 0;
    FCheckSum := '';
    FCommittedRevision := -1;
    FCommitAuthor := '';
    FCommitTime := 0;
    FLockToken := '';
    FLockOwner := '';
    FLockComment := '';
    FLockTime := 0;
  end;
  FTextStatus := Status.text_status;
  FPropStatus := Status.prop_status;
  FLocked := Status.locked;
  FCopied := Status.copied;
  FSwitched := Status.switched;
  FRemoteTextStatus := Status.repos_text_status;
  FRemotePropStatus := Status.repos_prop_status;
  if Assigned(Status.repos_lock) then
  begin
    FLockPath := Status.repos_lock^.path;
    FLockToken := Status.repos_lock^.token;
    FLockOwner := Status.repos_lock^.owner;
    FLockComment := Status.repos_lock^.comment;
    FLockDAVComment := Status.repos_lock^.is_dav_comment;
    FLockTime := AprTimeToDateTime(Status.repos_lock^.creation_date);
    FLockExpirationTime := AprTimeToDateTime(Status.repos_lock^.expiration_date);
  end
  else
  begin
    FLockPath := '';
    FLockToken := '';
    FLockOwner := '';
    FLockComment := '';
    FLockDAVComment := False;
    FLockTime := 0;
    FLockExpirationTime := 0;
  end;
  FURL := Status.url;

  FLastCommitRevision := Status.ood_last_cmt_rev;
  FLastCommitAuthor := Status.ood_last_cmt_author;
  FLastCommitTime := AprTimeToDateTime(Status.ood_last_cmt_date);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnItem.ReloadHistory;

var
  SubPool: PAprPool;
  StartRevision, EndRevision: TSvnOptRevision;
  Targets: PAprArrayHeader;
  Error: PSvnError;

begin
  ClearHistory;

  FHistory := TList.Create;
  try
    if FTextStatus in [svnWcStatusNone, svnWcStatusUnversioned] then
      Exit;

    AprCheck(apr_pool_create_ex(SubPool, FSvnClient.Pool, nil, FSvnClient.Allocator));
    try
      FillChar(StartRevision, SizeOf(TSvnOptRevision), 0);
      StartRevision.Kind := svnOptRevisionHead;
      FillChar(EndRevision, SizeOf(TSvnOptRevision), 0);
      EndRevision.Kind := svnOptRevisionNumber;
      EndRevision.Value.number := 0;

      Targets := apr_array_make(SubPool, 1, SizeOf(PChar));
      PPChar(apr_array_push(Targets))^ := PChar(FSvnPathName);
      Error := svn_client_log2(Targets, @StartRevision, @EndRevision, 0, False, False, LogMessage, Self, FSvnClient.Ctx,
        SubPool);
      if Assigned(Error) then
      begin
        if Error^.apr_err = APR_OS_START_SYSERR + WSAHOST_NOT_FOUND then
          svn_error_clear(Error)
        else
          RaiseSvnError(Error);
      end;
    finally
      apr_pool_destroy(SubPool);
    end;
  except
    ClearHistory;
    raise;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnItem.ReloadProps;

var
  SubPool: PAprPool;
  PegRevision, Revision: TSvnOptRevision;
  TruePath: PChar;
  Props: PAprArrayHeader;

begin
  FreeAndNil(FProps);

  FProps := TStringList.Create;
  try
    AprCheck(apr_pool_create_ex(SubPool, FSvnClient.Pool, nil, FSvnClient.Allocator));
    try
      FillChar(PegRevision, SizeOf(TSvnOptRevision), 0);
      PegRevision.Kind := svnOptRevisionUnspecified;
      FillChar(Revision, SizeOf(TSvnOptRevision), 0);
      Revision.Kind := svnOptRevisionUnspecified;
      AprCheck(apr_filepath_merge(TruePath, '', PChar(FPathName), APR_FILEPATH_TRUENAME, SubPool));
      SvnCheck(svn_client_proplist2(Props, TruePath, @PegRevision, @Revision, False, FSvnClient.Ctx,
        SubPool));

      FSvnClient.GetProps(Props, FProps, SubPool, FPropValDelimiter);
    finally
      apr_pool_destroy(SubPool);
    end;
  except
    FreeAndNil(FProps);
    raise;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnItem.Remove(Item: TSvnItem);

begin
  if Assigned(FItems) then
    FItems.Remove(Item);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnItem.SetPropValues(const Name, Value: string);

var
  SubPool: PAprPool;
  TruePath: PChar;
  SValue: string;
  SvnValue: PSvnString;

begin
  AprCheck(apr_pool_create_ex(SubPool, FSvnClient.Pool, nil, FSvnClient.Allocator));
  try
    AprCheck(apr_filepath_merge(TruePath, '', PChar(FPathName), APR_FILEPATH_TRUENAME, SubPool));
    SValue := Value;
    if Pos(';', SValue) <> 0 then
    begin
      if SValue[Length(SValue)] <> ';' then
        SValue := SValue + ';';
      SValue := StringReplace(SValue, ';', SvnLineBreak, [rfReplaceAll, rfIgnoreCase]);
    end;

    SvnValue := svn_string_create(PChar(Value), SubPool);
    if svn_prop_needs_translation(PChar(Name)) then
      SvnCheck(svn_subst_translate_string(SvnValue, SvnValue, nil, SubPool));

    SvnCheck(svn_client_propset2(PChar(Name), SvnValue, TruePath, False, False, FSvnClient.Ctx, SubPool));
  finally
    apr_pool_destroy(SubPool);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnItem.SortItems(Recurse: Boolean);

var
  I: Integer;

begin
  if not Assigned(FItems) then
    Exit;

  for I := 0 to FItems.Count - 1 do
    if TSvnItem(FItems[I]).Kind = svnNodeDir then
      TSvnItem(FItems[I]).SortItems(Recurse);
  FItems.Sort(CompareNativePathNames);
end;

//----------------------------------------------------------------------------------------------------------------------

{ TSvnItem protected }

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnItem.DoDestroy;

begin
  if Assigned(FOnDestroy) then
    FOnDestroy(Self);
end;

//----------------------------------------------------------------------------------------------------------------------
// svn recursive status lists items in the following order:
// 1.  unversioned items within current directory, if any
// 2.  current directory itself
// 3.  versioned items within current directory, however:
//     if the item is a directory, descend and continue with step 1 (unversioned items first).
//
// for example:
//   V://unversioned1.txt
//   V://unversioned1.txt
//   V:/
//   V://Subdir/unversioned1.txt
//   V://Subdir/unversioned2.txt
//   V://Subdir
//   V://Subdir/versioned1.txt
//   V://Subdir/versioned2.txt
//   V://versioned1.txt
//   V://versioned2.txt

procedure TSvnItem.DoWCStatus(Path: PChar; const Status: TSvnWCStatus2);

var
  Parent, Child: TSvnItem;
  ParentPath, ChildsParentPath: string;

  procedure AddUnversionedItems(Parent: TSvnItem);
  var
    I: Integer;
    Child: TSvnItem;
  begin
    for I := FReloadUnversionedList.Count - 1 downto 0 do
    begin
      Child := FReloadUnversionedList[I];
      FReloadUnversionedList.Delete(I);
      Child.FParent := Parent;
      Parent.Add(Child);
    end;
  end;

begin
  if Status.text_status = svnWcStatusUnversioned then
    FReloadUnversionedList.Add(TSvnItem.Create(FSvnClient, nil, Path, Status))
  else
  begin
    Parent := FReloadStack.Peek;
    if AnsiStrIComp(Path, PChar(Parent.SvnPathName)) = 0 then
    begin
      LoadStatus(Status);
      if FKind = svnNodeDir then
        AddUnversionedItems(Self);
    end
    else
    begin
      ChildsParentPath := SvnExtractFilePath(Path);
      ParentPath := Parent.SvnPathName + SvnPathDelim;
      while not AnsiSameText(ChildsParentPath, ParentPath) do
      begin
        FReloadStack.Pop;
        Parent := FReloadStack.Peek;
        ParentPath := Parent.SvnPathName + SvnPathDelim;
      end;

      Child := TSvnItem.Create(FSvnClient, Parent, Path, Status);

      if Child.Kind = svnNodeDir then
      begin
        AddUnversionedItems(Child);
        FReloadStack.Push(Child);
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

{ TSvnItem public }

//----------------------------------------------------------------------------------------------------------------------

constructor TSvnItem.Create(ASvnClient: TSvnClient; AParent: TSvnItem; const APathName: string;
  Recurse: Boolean = False; Update: Boolean = False);

begin
  inherited Create;
  FSvnClient := ASvnClient;
  FParent := AParent;
  if Assigned(FParent) then
    FParent.Add(Self);
  FItems := nil;
  FHistory := nil;
  FProps := nil;
  FPathName := APathName;
  FFileAttr := 0;
  FSmallImageIndex := -1;
  FLargeImageIndex := -1;
  FPropValDelimiter := ';';
  Reload(Recurse, Update);
end;

//----------------------------------------------------------------------------------------------------------------------

constructor TSvnItem.Create(ASvnClient: TSvnClient; AParent: TSvnItem; const ASvnPathName: string;
  const Status: TSvnWCStatus2);

begin
  inherited Create;
  FSvnClient := ASvnClient;
  FSvnPathName := ASvnPathName;
  FParent := AParent;
  FItems := nil;
  FHistory := nil;
  FProps := nil;
  FFileAttr := 0;
  FPathName := FSvnClient.SvnPathToNativePath(FSvnPathName);
  FSmallImageIndex := -1;
  FLargeImageIndex := -1;
  FPropValDelimiter := ';';
  LoadStatus(Status);
  if Assigned(FParent) then
    FParent.Add(Self);
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TSvnItem.Destroy;

begin
  if Assigned(FParent) then
    FParent.Remove(Self);
  Clear;
  DoDestroy;
  inherited Destroy;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnItem.Add(Item: TSvnItem): Integer;

begin
  if not Assigned(FItems) then
    FItems := TList.Create;
  Result := FItems.Add(Item);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnItem.Clear;

begin
  FBaseRevision := -1;
  FURL := '';
  FRepository := '';
  FUUID := '';
  FKind := svnNodeNone;
  FSchedule := svnWcScheduleNormal;
  FCopied := False;
  FDeleted := False;
  FAbsent := False;
  FIncomplete := False;
  FCopiedFromURL := '';
  FCopiedFromRevision := -1;
  FConflictOldFile := '';
  FConflictNewFile := '';
  FConflictWorkingFile := '';
  FPropRejectFile := '';
  FTextTime := 0;
  FPropTime := 0;
  FCheckSum := '';
  FCommittedRevision := -1;
  FCommitAuthor := '';
  FCommitTime := 0;
  FLockToken := '';
  FLockOwner := '';
  FLockComment := '';
  FLockTime := 0;
  FTextStatus := svnWcStatusNone;
  FPropStatus := svnWcStatusNone;
  FLocked := False;
  FCopied := False;
  FSwitched := False;
  FRemoteTextStatus := svnWcStatusNone;
  FRemotePropStatus := svnWcStatusNone;
  FLockPath := '';
  FLockToken := '';
  FLockOwner := '';
  FLockComment := '';
  FLockDAVComment := False;
  FLockTime := 0;
  FLockExpirationTime := 0;
  FURL := '';
  FLastCommitRevision := -1;
  FLastCommitAuthor := '';
  FLastCommitTime := 0;

  ClearItems;
  ClearHistory;
  FreeAndNil(FProps);
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnItem.IndexOf(Item: TSvnItem): Integer;

begin
  if Assigned(FItems) then
    Result := FItems.IndexOf(Item)
  else
    Result := -1;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnItem.IndexOf(const PathName: string; SvnPath: Boolean = False): Integer;

var
  I: Integer;
  SPathName1, SPathName2: string;

begin
  Result := -1;

  if not Assigned(FItems) then
    FItems := TList.Create;

  for I := 0 to FItems.Count - 1 do
  begin
    if SvnPath then
    begin
      SPathName1 := SvnExcludeTrailingPathDelimiter(PathName);
      SPathName2 := SvnExcludeTrailingPathDelimiter(TSvnItem(FItems[I]).SvnPathName);
    end
    else
    begin
      SPathName1 := ExcludeTrailingPathDelimiter(PathName);
      SPathName2 := ExcludeTrailingPathDelimiter(TSvnItem(FItems[I]).PathName);
    end;
    
    if AnsiSameText(SPathName1, SPathName2) then
    begin
      Result := I;
      Break;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnItem.Reload(Recurse: Boolean = False; Update: Boolean = False);

var
  SubPool: PAprPool;
  Revision: TSvnOptRevision;

begin
  Clear;
  FItems := TList.Create;

  FReloadUnversionedList := nil;
  FReloadStack := TStack.Create;
  try
    FReloadUnversionedList := TList.Create;
    AprCheck(apr_pool_create_ex(SubPool, FSvnClient.Pool, nil, FSvnClient.Allocator));
    try
      FSvnPathName := FSvnClient.NativePathToSvnPath(FPathName, SubPool);
      FillChar(Revision, SizeOf(TSvnOptRevision), 0);
      Revision.Kind := svnOptRevisionHead;
      FReloadStack.Push(Self);
      SvnCheck(svn_client_status2(nil, PChar(FSvnPathName), @Revision, WCStatus, Self, Recurse, True, Update, False,
        False, FSvnClient.Ctx, SubPool));
    finally
      apr_pool_destroy(SubPool);
    end;

    if FKind = svnNodeDir then
      SortItems(Recurse);
  finally
    FreeAndNil(FReloadStack);
    FreeAndNil(FReloadUnversionedList);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

{ TSvnClient private }

//----------------------------------------------------------------------------------------------------------------------

function TSvnClient.GetInitialized: Boolean;

begin
  Result := Assigned(FAllocator);
end;

//----------------------------------------------------------------------------------------------------------------------

{ TSvnClient protected }

//----------------------------------------------------------------------------------------------------------------------

function TSvnClient.DoLoginPrompt(const Realm: string; var UserName, Password: string): Boolean;

begin
  Result := not Assigned(FOnLoginPrompt);
  if not Result then
    FOnLoginPrompt(Self, Realm, UserName, Password, Result);
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnClient.DoNotify(const Path, MimeType: string; Action: TSvnWCNotifyAction; Kind: TSvnNodeKind;
  ContentState, PropState: TSvnWCNotifyState; Revision: TSvnRevNum): Boolean;

var
  SPath: string;

begin
  Result := False;
  if Assigned(FNotifyCallback) then
  begin
    SPath := Path;
    if SPath = SvnPathDelim then
      Insert(ExtractFileDrive(GetCurrentDir), SPath, 1)
    else if SPath = SvnExtractFileDrive(SPath) then
      SPath := SvnIncludeTrailingPathDelimiter(SPath);
         
    FNotifyCallback(Self, SPath, MimeType, Action, Kind, ContentState, PropState, Revision, Result);
    FCancelled := Result;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnClient.DoSSLClientCertPrompt(const Realm: string; var CertFileName: string): Boolean;

begin
  Result := not Assigned(FOnSSLClientCertPrompt);
  if not Result then
    FOnSSLClientCertPrompt(Self, Realm, CertFileName, Result);
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnClient.DoSSLClientPasswordPrompt(const Realm: string; var Password: string): Boolean;

begin
  Result := not Assigned(FOnSSLClientPasswordPrompt);
  if not Result then
     FOnSSLClientPasswordPrompt(Self, Realm, Password, Result);
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnClient.DoSSLServerTrustPrompt(const Realm: string; const CertInfo: TSvnAuthSSLServerCertInfo;
  Failures: TSSLServerTrustFailures): Boolean;

begin
  Result := not Assigned(FOnSSLServerTrustPrompt);
  if not Result then
    FOnSSLServerTrustPrompt(Self, Realm, CertInfo, Failures, Result);
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnClient.DoUserNamePrompt(const Realm: string; var UserName: string): Boolean;

begin
  Result := not Assigned(FOnUserNamePrompt);
  if not Result then
    FOnUserNamePrompt(Self, Realm, UserName, Result);
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnClient.DoWCStatus(Path: PChar; const Status: TSvnWCStatus2): Boolean;

var
  Item: TSvnItem;

begin
  Result := False;
  if Assigned(FStatusCallback) then
  begin
    Item := TSvnItem.Create(Self, nil, Path, Status);
    try
      FStatusCallback(Self, Item, Result);
    except
      Item.Free;
      raise;
    end;
    FCancelled := Result;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

{ TSvnClient public }

//----------------------------------------------------------------------------------------------------------------------

constructor TSvnClient.Create;

begin
  inherited Create;
  FAllocator := nil;
  FPool := nil;
  FCtx := nil;
  FUserName := '';
  FPassword := '';
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TSvnClient.Destroy;

begin
  Finalize;
  inherited Destroy;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnClient.Cleanup(const PathName: string; SubPool: PAprPool);

var
  NewPool: Boolean;

begin
  if not Initialized then
    Initialize;

  NewPool := not Assigned(SubPool);
  if NewPool then
    AprCheck(apr_pool_create_ex(SubPool, FPool, nil, FAllocator));
  try
    SvnCheck(svn_client_cleanup(PChar(NativePathToSvnPath(PathName)), FCtx, SubPool));
  finally
    if NewPool then
      apr_pool_destroy(SubPool);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnClient.Commit(PathNames: TStrings; const LogMessage: string; Callback: TSvnNotifyCallback = nil;
  Recurse: Boolean = True; KeepLocks: Boolean = False; SubPool: PAprPool = nil): Boolean;

var
  NewPool: Boolean;
  Targets: PAprArrayHeader;
  CommitInfo: PSvnCommitInfo;

begin
  Result := False;

  if not Assigned(PathNames) or (PathNames.Count = 0) then
    Exit;

  if not Initialized then
    Initialize;

  NewPool := not Assigned(SubPool);
  if NewPool then
    AprCheck(apr_pool_create_ex(SubPool, FPool, nil, FAllocator));
  try
    Targets := PathNamesToAprArray(PathNames, SubPool);
    CommitInfo := nil;
    FCommitLogMessage := LogMessage;
    FNotifyCallback := Callback;
    SvnCheck(svn_client_commit3(CommitInfo, Targets, Recurse, KeepLocks, FCtx, SubPool));
    if Assigned(CommitInfo) and Assigned(CommitInfo^.post_commit_err) and (CommitInfo^.post_commit_err^ <> #0) then
      raise Exception.Create(CommitInfo^.post_commit_err);

    Result := Assigned(CommitInfo) and (CommitInfo^.revision <> SVN_INVALID_REVNUM);
  finally
    FCommitLogMessage := '';
    FNotifyCallback := nil;
    if NewPool then
      apr_pool_destroy(SubPool);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnClient.Finalize;

begin
  if Assigned(FPool) then
  begin
    apr_pool_destroy(FPool);
    FPool := nil;
  end;
  if Assigned(FPoolUtf8) then
  begin
    apr_pool_destroy(FPoolUtf8);
    FPoolUtf8 := nil;
  end;
  if Assigned(FAllocator) then
  begin
    apr_allocator_destroy(FAllocator);
    FAllocator := nil;
  end;
  apr_terminate2;
  FreeSvnClientLib;
  FreeAprLib;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnClient.GetModifications(const PathName: string; Callback: TSvnStatusCallback = nil;
  Recurse: Boolean = True; Update: Boolean = False; IgnoreExternals: Boolean = False;
  SubPool: PAprPool = nil): TSvnRevNum;

var
  NewPool: Boolean;
  Revision: TSvnOptRevision;

begin
  Result := -1;
  if not Initialized then
    Initialize;

  NewPool := not Assigned(SubPool);
  if NewPool then
    AprCheck(apr_pool_create_ex(SubPool, FPool, nil, FAllocator));
  try
    FillChar(Revision, SizeOf(TSvnOptRevision), 0);
    Revision.Kind := svnOptRevisionHead;
    FCancelled := False;
    FStatusCallback := Callback;
    SvnCheck(svn_client_status2(@Result, PChar(NativePathToSvnPath(PathName)), @Revision, WCStatus2, Self, Recurse,
      False, Update, False, IgnoreExternals, FCtx, SubPool));
  finally
    FStatusCallback := nil;
    if NewPool then
      apr_pool_destroy(SubPool);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnClient.GetProps(Props: PAprArrayHeader; Strings: TStrings; SubPool: PAprPool = nil;
  Delimiter: Char = DefaultPropValDelimiter);

const
  CRLF = #13#10;

var
  NewPool: Boolean;
  P: PSvnClientPropListItem;
  I, J: Integer;
  H: PAprHashIndex;
  PName: PChar;
  PValue: PSvnString;
  Name, Value: string;

begin
  Strings.BeginUpdate;
  try
    Strings.Clear;
    if not Assigned(Props) then
      Exit;

    NewPool := not Assigned(SubPool);
    if NewPool then
      AprCheck(apr_pool_create_ex(SubPool, FPool, nil, FAllocator));
    try
      P := PPSvnClientPropListItem(Props^.elts)^;
      for I := 0 to Props^.nelts - 1 do
      begin
        H := apr_hash_first(SubPool, P^.prop_hash);
        while Assigned(H) do
        begin
          apr_hash_this(H, @PName, 0, @PValue);
          // special Subversion properties, stored as UTF8
          if svn_prop_needs_translation(PName) or // svn:
            (StrLComp(PName, 'bugtraq:', 8) = 0) or (StrLComp(PName, 'tsvn:', 5) = 0) then // TortoiseSVN
            SvnCheck(svn_subst_detranslate_string(PValue, PValue, False, SubPool));

          SetString(Name, PName, StrLen(PName));
          SetString(Value, PValue^.data, StrLen(PValue^.data));

          Value := StringReplace(Value, CRLF, Delimiter, [rfReplaceAll, rfIgnoreCase]);
          if (Value <> '') and (Value[Length(Value)] = Delimiter) then
            Delete(Value, Length(Value), 1);

          J := Strings.IndexOfName(Name);
          if J = -1 then
            Strings.Add(Format('%s=%s', [Name, Value]))
          else
            Strings.ValueFromIndex[J] := Strings.ValueFromIndex[J] + Delimiter + Value;

          H := apr_hash_next(H);
        end;

        Inc(P);
      end;
    finally
      if NewPool then
        apr_pool_destroy(SubPool);
    end;
  finally
    Strings.EndUpdate;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnClient.Initialize(const AConfigDir: string = '');

var
  Providers: PAprArrayHeader;
  Provider: PSvnAuthProviderObject;
  Auth: PSvnAuthBaton;
  S: string;
  P: PChar;

begin
  if not AprLibLoaded then
  begin
    if not LoadAprLib then
      RaiseLastOSError;
    AprCheck(apr_initialize);
  end;
  if not SvnClientLibLoaded then
  begin
    if not LoadSvnClientLib then
      RaiseLastOSError;
    FPoolUtf8 := svn_pool_create_ex(nil, nil);
    svn_utf_initialize(FPoolUtf8);
    SvnCheck(svn_nls_init);
  end;

  AprCheck(apr_allocator_create(FAllocator));
  try
    apr_allocator_max_free_set(Allocator, SVN_ALLOCATOR_RECOMMENDED_MAX_FREE);
    AprCheck(apr_pool_create_ex(FPool, nil, nil, FAllocator));
    try
      SvnCheck(svn_ra_initialize(Pool));
      SvnCheck(svn_client_create_context(FCtx, FPool));

      FCtx^.notify_func := SvnContextNotify;
      FCtx^.notify_baton := Self;
      FCtx^.progress_func := SvnContextProgress;
      FCtx^.progress_baton := Self;
      FCtx^.cancel_func := SvnContextCancel;
      FCtx^.cancel_baton := Self;
      FCtx^.log_msg_func := SvnContextLogMessage;
      FCtx^.log_msg_baton := Self;

      if AConfigDir = '' then
        S := IncludeTrailingPathDelimiter(GetAppDataDir) + 'Subversion'
      else
        S := ExcludeTrailingPathDelimiter(AConfigDir);
      SvnCheck(svn_utf_cstring_to_utf8(@P, PChar(S), FPool));
      P := svn_path_canonicalize(P, FPool);
      SvnCheck(svn_config_ensure(P, FPool));
      SetString(FConfigDir, P, StrLen(P));
      SvnCheck(svn_config_get_config(FCtx^.config, PChar(FConfigDir), FPool));

      Provider := nil;
      Providers := apr_array_make(FPool, 11, SizeOf(PSvnAuthProviderObject));

      svn_client_get_windows_simple_provider(Provider, FPool);
      if Assigned(Provider) then
        PPSvnAuthProviderObject(apr_array_push(Providers))^ := Provider;
      svn_client_get_simple_provider(Provider, FPool);
      if Assigned(Provider) then
        PPSvnAuthProviderObject(apr_array_push(Providers))^ := Provider;
      svn_client_get_username_provider(Provider, FPool);
      if Assigned(Provider) then
        PPSvnAuthProviderObject(apr_array_push(Providers))^ := Provider;
      svn_client_get_ssl_server_trust_file_provider(Provider, FPool);
      if Assigned(Provider) then
        PPSvnAuthProviderObject(apr_array_push(Providers))^ := Provider;
      svn_client_get_ssl_client_cert_file_provider(Provider, FPool);
      if Assigned(Provider) then
        PPSvnAuthProviderObject(apr_array_push(Providers))^ := Provider;
      svn_client_get_ssl_client_cert_pw_file_provider(Provider, FPool);
      if Assigned(Provider) then
        PPSvnAuthProviderObject(apr_array_push(Providers))^ := Provider;
      svn_client_get_simple_prompt_provider(Provider, SimplePrompt, Self, 1, FPool);
      if Assigned(Provider) then
        PPSvnAuthProviderObject(apr_array_push(Providers))^ := Provider;
      svn_client_get_username_prompt_provider(Provider, UserNamePrompt, Self, 1, FPool);
      if Assigned(Provider) then
        PPSvnAuthProviderObject(apr_array_push(Providers))^ := Provider;
      svn_client_get_ssl_server_trust_prompt_provider(Provider, SSLServerTrustPrompt, Self, FPool);
      if Assigned(Provider) then
        PPSvnAuthProviderObject(apr_array_push(Providers))^ := Provider;
      svn_client_get_ssl_client_cert_prompt_provider(Provider, SSLClientCertPrompt, Self, 0, FPool);
      if Assigned(Provider) then
        PPSvnAuthProviderObject(apr_array_push(Providers))^ := Provider;
      svn_client_get_ssl_client_cert_pw_prompt_provider(Provider, SSLClientPasswordPrompt, Self, 0, FPool);
      if Assigned(Provider) then
        PPSvnAuthProviderObject(apr_array_push(Providers))^ := Provider;

      svn_auth_open(Auth, Providers, FPool);
      Ctx^.auth_baton := Auth;
      svn_auth_set_parameter(Auth, SVN_AUTH_PARAM_CONFIG_DIR, PChar(FConfigDir));
      if FUserName <> '' then
        svn_auth_set_parameter(Auth, SVN_AUTH_PARAM_DEFAULT_USERNAME, PChar(FUserName));
      if FPassword <> '' then
        svn_auth_set_parameter(Auth, SVN_AUTH_PARAM_DEFAULT_PASSWORD, PChar(FPassword));
      svn_auth_set_parameter(Auth, SVN_AUTH_PARAM_DONT_STORE_PASSWORDS, PChar(''));
      svn_auth_set_parameter(Auth, SVN_AUTH_PARAM_NO_AUTH_CACHE, PChar(''));
    except
      apr_pool_destroy(FPool);
      FPool := nil;
      raise;
    end;
  except
    apr_pool_destroy(FPoolUtf8);
    apr_allocator_destroy(FAllocator);
    FAllocator := nil;
    raise;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnClient.IsPathVersioned(const PathName: string): Boolean;

var
  SubPool: PAprPool;
  TruePath: PChar;
  Revision, PegRevision: TSvnOptRevision;
  SvnError: PSvnError;

begin
  Result := False;
  if (PathName = '') or (GetFileAttributes(PChar(PathName)) = Cardinal(-1)) then // path does not exist
    Exit;

  if not Initialized then
    Initialize;

  AprCheck(apr_pool_create_ex(SubPool, FPool, nil, FAllocator));
  try
    FillChar(PegRevision, SizeOf(TSvnOptRevision), 0);
    PegRevision.Kind := svnOptRevisionUnspecified;
    FillChar(Revision, SizeOf(TSvnOptRevision), 0);
    Revision.Kind := svnOptRevisionUnspecified;
    AprCheck(apr_filepath_merge(TruePath, '', PChar(PathName), APR_FILEPATH_TRUENAME, SubPool));
    SvnError := svn_client_info(TruePath, @PegRevision, @Revision, DummyInfoReceiver, nil, False, Ctx, SubPool);
    Result := not Assigned(SvnError);
    if not Result then
    begin
      case SvnError^.apr_err of
        SVN_ERR_WC_NOT_DIRECTORY, SVN_ERR_UNVERSIONED_RESOURCE:
          svn_error_clear(SvnError);
        else
          RaiseSvnError(SvnError);
      end;
    end;
  finally
    apr_pool_destroy(SubPool);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnClient.NativePathToSvnPath(const NativePath: string; SubPool: PAprPool = nil): string;

var
  NewPool: Boolean;
  SvnPath: PChar;

begin
  NewPool := not Assigned(SubPool);
  if NewPool then
    AprCheck(apr_pool_create_ex(SubPool, FPool, nil, FAllocator));
  try
    AprCheck(apr_filepath_merge(SvnPath, '', PChar(NativePath), APR_FILEPATH_TRUENAME, SubPool));
    Result := SvnPath;
  finally
    if NewPool then
      apr_pool_destroy(SubPool);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnClient.PathNamesToAprArray(PathNames: TStrings; SubPool: PAprPool): PAprArrayHeader;

var
  NewPool: Boolean;
  I: Integer;
  CurrentDrive, S: string;
  P: PChar;

begin
  Result := nil;
  
  if not Assigned(PathNames) or (PathNames.Count = 0) then
    Exit;

  if not Initialized then
    Initialize;

  CurrentDrive := ExtractFileDrive(GetCurrentDir);

  NewPool := not Assigned(SubPool);
  if NewPool then
    AprCheck(apr_pool_create_ex(SubPool, FPool, nil, FAllocator));
  try
    Result := apr_array_make(SubPool, PathNames.Count, SizeOf(PChar));

    for I := 0 to PathNames.Count - 1 do
    begin
      // Work around an apparent Subversion glitch:
      // Svn update reproducibly fails with error SVN_ERR_WC_LOCKED "Attempted to lock an already-locked dir"
      // in two cases:
      // 1. A directory with trailing path delimiter, e.g. D:\Temp\Test\ (D:/Temp/Test/ in SVN notation)
      //    Workaround: always exclude the trailing path delimiter

      S := ExcludeTrailingPathDelimiter(PathNames[I]);

      // 2. Root directory which is the same as the current directory drive, e.g. 'D:\' or 'D:'
      //    when current directory is anywhere on drive D:
      //    Workaround: use '/'

      if S = CurrentDrive then
        P := SvnPathDelim
      else
        AprCheck(apr_filepath_merge(P, '', PChar(S), APR_FILEPATH_TRUENAME, SubPool));
        
      PPChar(apr_array_push(Result))^ := P;
    end;
  finally
    if NewPool then
      apr_pool_destroy(SubPool);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnClient.Revert(PathNames: TStrings; Callback: TSvnNotifyCallback = nil; Recurse: Boolean = True;
  SubPool: PAprPool = nil);

var
  NewPool: Boolean;
  Paths: PAprArrayHeader;

begin
  if not Assigned(PathNames) or (PathNames.Count = 0) then
    Exit;

  if not Initialized then
    Initialize;

  NewPool := not Assigned(SubPool);
  if NewPool then
    AprCheck(apr_pool_create_ex(SubPool, FPool, nil, FAllocator));
  try
    Paths := PathNamesToAprArray(PathNames, SubPool);
    FNotifyCallback := Callback;
    SvnCheck(svn_client_revert(Paths, Recurse, FCtx, SubPool));
  finally
    FNotifyCallback := nil;
    if NewPool then
      apr_pool_destroy(SubPool);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnClient.SvnPathToNativePath(const SvnPath: string; SubPool: PAprPool = nil): string;

var
  NewPool: Boolean;
  NativePath: PChar;

begin
  NewPool := not Assigned(SubPool);
  if NewPool then
    AprCheck(apr_pool_create_ex(SubPool, FPool, nil, FAllocator));
  try
    AprCheck(apr_filepath_merge(NativePath, '', PChar(SvnPath), APR_FILEPATH_NATIVE, SubPool));
    Result := NativePath;
  finally
    if NewPool then
      apr_pool_destroy(SubPool);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnClient.Update(PathNames: TStrings; Callback: TSvnNotifyCallback = nil; Recurse: Boolean = True;
  IgnoreExternals: Boolean = False; SubPool: PAprPool = nil);

var
  NewPool: Boolean;
  Targets: PAprArrayHeader;
  Revision: TSvnOptRevision;

begin
  if not Assigned(PathNames) or (PathNames.Count = 0) then
    Exit;

  if not Initialized then
    Initialize;

  NewPool := not Assigned(SubPool);
  if NewPool then
    AprCheck(apr_pool_create_ex(SubPool, FPool, nil, FAllocator));
  try
    Targets := PathNamesToAprArray(PathNames, SubPool);
    FillChar(Revision, SizeOf(TSvnOptRevision), 0);
    Revision.Kind := svnOptRevisionHead;
    FCancelled := False;
    FNotifyCallback := Callback;
    SvnCheck(svn_client_update2(nil, Targets, @Revision, Recurse, IgnoreExternals, FCtx, SubPool));
  finally
    FNotifyCallback := nil;
    if NewPool then
      apr_pool_destroy(SubPool);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

end.