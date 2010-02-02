{**********************************************************************************************************************}
{                                                                                                                      }
{ delphisvn: Subversion plugin for Delphi                                                                              }
{                                                                                                                      }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); you may not use     }
{ this file except in compliance with the License. You may obtain a copy of the License at http://www.mozilla.org/MPL/ }
{                                                                                                                      }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either      }
{ express or implied. See the License for the specific language governing rights and limitations under the License.    }
{                                                                                                                      }
{ The Original Code is TestSvnClient.pas.                                                                              }
{                                                                                                                      }
{ The Initial Developer of the Original Code is Uwe Schuster.                                                          }
{ Portions created by Uwe Schuster are Copyright Uwe Schuster. All rights reserved.                                    }
{                                                                                                                      }
{ Contributors:                                                                                                        }
{   Uwe Schuster (uschuster)                                                                                           }
{                                                                                                                      }
{**********************************************************************************************************************}

unit TestSvnClient;

{$I jedi.inc}

interface

uses
  TestFramework, Windows, Classes, Contnrs, SysUtils, SyncObjs,
  JclSysUtils, JclFileUtils,
  svn_client, SvnClient;

type
  TUTF8Tests = class(TTestCase)
  private
    const
      {$IFDEF COMPILER12_UP}
      TestFileWithInternationalCharacters = 'Windm'#252'hlen'#35774#35745#35828#26126#20070;
      {$ELSE ~COMPILER12_UP}
      TestFileWithInternationalCharacters = 'Windm'#252'hlen';
      {$ENDIF ~COMPILER12_UP}
      TestFile = TestFileWithInternationalCharacters + '.txt';
      TestPath = TestFileWithInternationalCharacters + 'Path';
      TestUser = 'M'#252'ller';
      MoveLogMessage = 'Move ' + TestFile;
    var
      FRepoCreated: Boolean;
      FRepoPath: string;
      FSvnAdminExe: string;
      FSvnClient: TSvnClient;
      FSvnExe: string;
      FWorkingCopyCreated: Boolean;
      FWorkingCopyPath: string;
    function CreateFile(const AFileName: string): Boolean;
    function CreateRepo(var AErrorStr: string): Boolean;
    function SvnActionAddFile(const AFileName: string): Boolean;
    function SvnActionCheckOut: Boolean;
    function SvnActionCommit(const APath, ALogMessage: string): Boolean;
    function SvnActionCopy(const ASourceFile, ADestFile: string): Boolean;
    function SvnActionDelete(const AFileName: string): Boolean;
    function SvnActionListFiles(const APathName: string; AFiles: TStringList): Boolean;
    function SvnActionLog(const AFileName: string; ALogMessages: TStringList): Boolean;
    function SvnActionMove(const ASourceFile, ADestFile: string): Boolean;
    function SvnActionRevert(const AFileName: string): Boolean;
    function SvnModifyFileRepoRevisionUser(ARevision: Integer; const ANewUserName: string): Boolean;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Test1AddCommitFile;
    procedure Test2CopyCommitFile;
    procedure Test3DeleteCommitFile;
    procedure Test4MoveCommitFile;
    procedure Test5ModifyRevertFile;
    procedure Test6ListFiles;
    procedure Test7CheckLogMessage;
    procedure Test8AddPath;
    procedure Test9ListFilesBySvnList;
  published
    procedure Test10CheckLogUser;
    procedure Test11CheckBlameUser;
    procedure Test12PathTranslation;
  end;

implementation

{$IFNDEF COMPILER12_UP}
function UTF8ToString(const AStr: string): string; {$IFDEF COMPILER9_UP} inline;{$ENDIF}
begin
  Result := UTF8Decode(AStr);
end;
{$ENDIF ~COMPILER12_UP}

function TUTF8Tests.CreateFile(const AFileName: string): Boolean;
var
  Handle: Integer;
begin
  Handle := FileCreate(AFileName);
  if Handle > 0 then
    FileClose(Handle)
  else
  if not FileExists(AFileName) then
    raise Exception.CreateFmt('Could not create file %s', [AFileName]);
  Result := True;
end;

function TUTF8Tests.CreateRepo(var AErrorStr: string): Boolean;
var
  OutputStr: string;
begin
  Result := Execute(Format('%s create %s', [FSvnAdminExe, FRepoPath]), OutputStr) = 0;
  if not Result then
    AErrorStr := OutputStr;
end;

procedure TUTF8Tests.SetUp;
var
  ErrorStr: string;
begin
  //Settings
  FRepoCreated := False;
  FRepoPath := 'c:\delphisvntest\repo';
  FSvnAdminExe := 'c:\tools\svn\svnadmin.exe';
  FSvnExe := 'c:\tools\svn\svn.exe';
  FWorkingCopyCreated := False;
  FWorkingCopyPath := 'c:\delphisvntest\wc';
  //Checks
  if DirectoryExists(FRepoPath) then
    raise Exception.CreateFmt('Repo path %s must not exist', [FRepoPath]);
  if DirectoryExists(FWorkingCopyPath) then
    raise Exception.CreateFmt('Working copy path %s must not exist', [FRepoPath]);
  if not FileExists(FSvnAdminExe) then
    raise Exception.CreateFmt('%s not found', [FSvnAdminExe]);
  //Actual Setup
  FRepoCreated := ForceDirectories(FRepoPath);
  FWorkingCopyCreated := ForceDirectories(FWorkingCopyPath);
  if not CreateRepo(ErrorStr) then
    raise Exception.Create('Repo creation failed' + #13#10 + ErrorStr);
  FSvnClient := TSvnClient.Create;
  FSvnClient.Initialize;
  if not FSvnClient.Initialized then
    raise Exception.Create('SvnClient initialization failed');
  if not SvnActionCheckOut then
    raise Exception.Create('Checkout failed');
end;

function TUTF8Tests.SvnActionAddFile(const AFileName: string): Boolean;
begin
  FSvnClient.Add(AFileName);
  Result := True;
end;

function TUTF8Tests.SvnActionCheckOut: Boolean;
var
  TranslatedRepoPath: string;
begin
  TranslatedRepoPath := FSvnClient.NativePathToSvnPath(FRepoPath);
  FSvnClient.Checkout('file:///' + TranslatedRepoPath, FWorkingCopyPath);
  Result := True;
end;

function TUTF8Tests.SvnActionCommit(const APath, ALogMessage: string): Boolean;
var
  Paths: TStringList;
begin
  Paths := TStringList.Create;
  try
    Paths.Add(APath);
    FSvnClient.Commit(Paths, ALogMessage);
    Result := True;
  finally
    Paths.Free;
  end;
end;

function TUTF8Tests.SvnActionCopy(const ASourceFile, ADestFile: string): Boolean;
var
  Paths: TStringList;
  TranslatedSourceFile, TranslatedDestFile: string;
begin
  TranslatedSourceFile := FSvnClient.NativePathToSvnPath(ASourceFile);
  TranslatedDestFile := FSvnClient.NativePathToSvnPath(ADestFile);
  Paths := TStringList.Create;
  try
    Paths.Add(TranslatedSourceFile);
    FSvnClient.Copy(Paths, TranslatedDestFile);
    Result := True;
  finally
    Paths.Free;
  end;
end;

function TUTF8Tests.SvnActionDelete(const AFileName: string): Boolean;
var
  Paths: TStringList;
  TranslatedFile: string;
begin
  TranslatedFile := FSvnClient.NativePathToSvnPath(AFileName);
  Paths := TStringList.Create;
  try
    Paths.Add(TranslatedFile);
    FSvnClient.Delete(Paths);
    Result := True;
  finally
    Paths.Free;
  end;
end;

function TUTF8Tests.SvnActionListFiles(const APathName: string; AFiles: TStringList): Boolean;
var
  TranslatedPath: string;
begin
  TranslatedPath := FSvnClient.NativePathToSvnPath(APathName);
  FSvnClient.List(TranslatedPath, svnDepthFiles, False, AFiles);
  Result := True;
end;

function TUTF8Tests.SvnActionLog(const AFileName: string; ALogMessages: TStringList): Boolean;
var
  I: Integer;
  SvnItem: TSvnItem;
  TranslatedPath: string;
begin
  TranslatedPath := FSvnClient.NativePathToSvnPath(AFileName);
  SvnItem := TSvnItem.Create(FSvnClient, nil, TranslatedPath, True);
  try
    for I := 0 to SvnItem.HistoryCount - 1 do
      ALogMessages.Add(SvnItem.HistoryItems[I].LogMessage);
  finally
    SvnItem.Free;
  end;
  Result := True;
end;

function TUTF8Tests.SvnActionMove(const ASourceFile, ADestFile: string): Boolean;
var
  Paths: TStringList;
  TranslatedSourceFile, TranslatedDestFile: string;
begin
  TranslatedSourceFile := FSvnClient.NativePathToSvnPath(ASourceFile);
  TranslatedDestFile := FSvnClient.NativePathToSvnPath(ADestFile);
  Paths := TStringList.Create;
  try
    Paths.Add(TranslatedSourceFile);
    FSvnClient.Move(Paths, TranslatedDestFile);
    Result := True;
  finally
    Paths.Free;
  end;
end;

function TUTF8Tests.SvnActionRevert(const AFileName: string): Boolean;
var
  Paths: TStringList;
  TranslatedFile: string;
begin
  TranslatedFile := FSvnClient.NativePathToSvnPath(AFileName);
  Paths := TStringList.Create;
  try
    Paths.Add(TranslatedFile);
    FSvnClient.Revert(Paths);
    Result := True;
  finally
    Paths.Free;
  end;
end;

{
A revision property file looks like this:
K 10
svn:author
V 4
Test
K 8

The third line is is amount of bytes in the forth line and the forth line is the
user name as UTF-8
}
function TUTF8Tests.SvnModifyFileRepoRevisionUser(ARevision: Integer; const ANewUserName: string): Boolean;
var
  RevisionPropertyFile: string;
  FileStr: AnsiString;
  NewStr, FileStrDecoded: string;
  FS: TFileStream;
  I, L, NullACnt: Integer;
  AddedStr: Boolean;
begin
  Result := False;
  RevisionPropertyFile := PathAddSeparator(FRepoPath) + 'db\revprops\0\' + IntToStr(ARevision);
  if FileExists(RevisionPropertyFile) then
  begin
    FS := TFileStream.Create(RevisionPropertyFile, fmOpenReadWrite);
    try
      SetLength(FileStr, FS.Size);
      FS.Read(PAnsiString(FileStr)^, FS.Size);
      FileStrDecoded := UTF8ToString(FileStr);
      NullACnt := 0;
      L := Length(FileStr);
      NewStr := '';
      AddedStr := False;
      for I := 1 to L do
      begin
        if FileStr[I] = #10 then
          Inc(NullACnt);
        if not (NullACnt in [2, 3]) then
          NewStr := NewStr + FileStrDecoded[I]
        else
        if (NullACnt = 2) and not AddedStr then
        begin
          NewStr := NewStr + #10 + Format('V %d', [Length(UTF8Encode(ANewUserName))]) + #10 + ANewUserName;
          AddedStr := True;
        end;
      end;
      FS.Size := 0;
      FileStr := UTF8Encode(NewStr);
      FS.Write(PAnsiString(FileStr)^, Length(FileStr));
    finally
      FS.Free;
    end;
  end;
end;

procedure TUTF8Tests.TearDown;
begin
  FSvnClient.Free;
  if FRepoCreated then
    DelTree(FRepoPath);
  if FWorkingCopyCreated then
    DelTree(FWorkingCopyPath);
end;

procedure TUTF8Tests.Test10CheckLogUser;
var
  SvnItem: TSvnItem;
  TranslatedPath: string;
  FirstRevisionUser: string;
begin
  Test1AddCommitFile;

  SvnModifyFileRepoRevisionUser(1, TestUser);

  TranslatedPath := FSvnClient.NativePathToSvnPath(FWorkingCopyPath);
  FirstRevisionUser := '';
  SvnItem := TSvnItem.Create(FSvnClient, nil, TranslatedPath, True);
  try
    if SvnItem.HistoryCount > 0 then
      FirstRevisionUser := SvnItem.HistoryItems[0].Author;
  finally
    SvnItem.Free;
  end;
  CheckEqualsString(TestUser, FirstRevisionUser, Format('Author of the first revision is not %s', [TestUser]));
end;

procedure TUTF8Tests.Test11CheckBlameUser;
var
  FileName, TranslatedFileName: string;
  TSL: TStringList;
  SvnItem: TSvnItem;
  FirstLineUser: string;
begin
  FileName := PathAddSeparator(FWorkingCopyPath) + TestFile;

  TSL := TStringList.Create;
  try
    TSL.Add('This is a test file with an umlaut in the name ' + TestUser + '.');
    TSL.SaveToFile(PathAddSeparator(FWorkingCopyPath) + TestFile);
  finally
    TSL.Free;
  end;

  SvnActionAddFile(FileName);
  SvnActionCommit(FWorkingCopyPath, FileName);

  SvnModifyFileRepoRevisionUser(1, TestUser);  

  FirstLineUser := '';
  TranslatedFileName := FSvnClient.NativePathToSvnPath(FileName);
  SvnItem := TSvnItem.Create(FSvnClient, nil, TranslatedFileName, True);
  try
    if (SvnItem.HistoryCount > 0) and (SvnItem.HistoryItems[0].BlameCount > 0) then
      FirstLineUser := SvnItem.HistoryItems[0].BlameItems[0].Author;
  finally
    SvnItem.Free;
  end;
  CheckEqualsString(TestUser, FirstLineUser, Format('Author of the first line is not %s', [TestUser]));  
end;

procedure TUTF8Tests.Test12PathTranslation;
var
  InStr, OutStr, NativeStr, SVNStr: string;
begin
  NativeStr := 'C:\' + TestPath + '\' + TestFile;
  SVNStr := 'C:/' + TestPath + '/' + TestFile;

  InStr := NativeStr;
  OutStr := FSvnClient.NativePathToSvnPath(InStr);
  CheckEqualsString(OutStr, SVNStr);

  InStr := SVNStr;
  OutStr := FSvnClient.SvnPathToNativePath(InStr);
  CheckEqualsString(OutStr, NativeStr);
end;

procedure TUTF8Tests.Test1AddCommitFile;
var
  FileName, TranslatedRepoPath, OutputStr: string;
begin
  FileName := PathAddSeparator(FWorkingCopyPath) + TestFile;
  CreateFile(FileName);
  SvnActionAddFile(FileName);
  SvnActionCommit(FWorkingCopyPath, FileName);

  TranslatedRepoPath := FSvnClient.NativePathToSvnPath(FRepoPath);
  Execute(Format('%s list --xml file:///' + TranslatedRepoPath, [FSvnExe]), OutputStr);
  CheckTrue(Pos('<name>' + TestFile + '</name>', UTF8ToString(OutputStr)) > 0,
    TestFile + ' not found in svn list result');
end;

procedure TUTF8Tests.Test2CopyCommitFile;
begin
  Test1AddCommitFile;
  
  SvnActionCopy(PathAddSeparator(FWorkingCopyPath) + TestFile,
    PathAddSeparator(FWorkingCopyPath) + 'Foo.txt');
  SvnActionCommit(FWorkingCopyPath, 'LogMessage');
end;

procedure TUTF8Tests.Test3DeleteCommitFile;
begin
  Test2CopyCommitFile;

  SvnActionDelete(PathAddSeparator(FWorkingCopyPath) + TestFile);
  SvnActionCommit(FWorkingCopyPath, 'LogMessage');  
end;

procedure TUTF8Tests.Test4MoveCommitFile;
begin
  Test3DeleteCommitFile;

  SvnActionMove(PathAddSeparator(FWorkingCopyPath) + 'Foo.txt',
    PathAddSeparator(FWorkingCopyPath) + TestFile);
  SvnActionCommit(FWorkingCopyPath, MoveLogMessage);
end;

procedure TUTF8Tests.Test5ModifyRevertFile;
var
  TSL: TStringList;
begin
  Test4MoveCommitFile;

  TSL := TStringList.Create;
  try
    TSL.Add('This is a test file with an umlaut in the name ' + TestUser + '.');
    TSL.SaveToFile(PathAddSeparator(FWorkingCopyPath) + TestFile);
  finally
    TSL.Free;
  end;

  SvnActionRevert(PathAddSeparator(FWorkingCopyPath) + TestFile);
end;

procedure TUTF8Tests.Test6ListFiles;
var
  Files: TStringList;
  I: Integer;
  CheckStr: string;
  FoundFile: Boolean;
begin
  Test5ModifyRevertFile;

  Files := TStringList.Create;
  try
    SvnActionListFiles(PathAddSeparator(FWorkingCopyPath), Files);
    FoundFile := False;
    CheckStr := Format('%s=//%s', [TestFile, TestFile]);
    for I := 0 to Files.Count - 1 do
      if Files[I] = CheckStr then
      begin
        FoundFile := True;
        Break;
      end;
  finally
    Files.Free;
  end;
  CheckTrue(FoundFile, Format('%s not found in SVN file list', [TestFile]));
end;

procedure TUTF8Tests.Test7CheckLogMessage;
var
  LogMessages: TStringList;
  FoundMessage: Boolean;
begin
  Test6ListFiles;

  LogMessages := TStringList.Create;
  try
    SvnActionLog(PathAddSeparator(FWorkingCopyPath) + TestFile, LogMessages);
    FoundMessage := (LogMessages.Count > 0) and (LogMessages[0] = MoveLogMessage);
  finally
    LogMessages.Free;
  end;
  CheckTrue(FoundMessage, Format('First log message is not %s', [MoveLogMessage]));
end;

procedure TUTF8Tests.Test8AddPath;
begin
  Test7CheckLogMessage;

  ForceDirectories(PathAddSeparator(FWorkingCopyPath) + TestPath);
  SvnActionAddFile(PathAddSeparator(FWorkingCopyPath) + TestPath);
  SvnActionCommit(FWorkingCopyPath, TestFile);
end;

procedure TUTF8Tests.Test9ListFilesBySvnList;
var
  SvnList: TSvnList;
  Path: string;
  I: Integer;
  FoundPath: Boolean;
begin
  Test8AddPath;

  Path := FSvnClient.NativePathToSvnPath(PathAddSeparator(FWorkingCopyPath));
  SvnList := TSvnList.Create(FSvnClient);
  try
    SvnList.LoadList(Path, svnDepthImmediates, False);
    FoundPath := False;
    for I := 0 to SvnList.Count - 1 do
      if (SvnList[I].Kind = svnNodeDir) and (SvnList[I].Path = TestPath) then
      begin
        FoundPath := True;
        Break;
      end;
  finally
    SvnList.Free;
  end;
  CheckTrue(FoundPath, Format('%s not found in SVN file list', [TestPath]));
end;

initialization
  RegisterTest(TUTF8Tests.Suite);
  
end.