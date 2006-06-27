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
{ The Original Code is SvnIDEClient.pas.                                                                               }
{                                                                                                                      }
{ The Initial Developer of the Original Code is Ondrej Kelle.                                                          }
{ Portions created by Ondrej Kelle are Copyright Ondrej Kelle. All rights reserved.                                    }
{                                                                                                                      }
{ Contributors:                                                                                                        }
{   Ondrej Kelle (tondrej)                                                                                             }
{                                                                                                                      }
{**********************************************************************************************************************}
{                                                                                                                      }
{ This unit contains TSvnIDEClient, a utility data module class for Delphi IDE Subversion plugin (to be used as a      }
{ single global instance).                                                                                             }
{                                                                                                                      }
{**********************************************************************************************************************}

unit SvnIDEClient;

interface

uses
  Windows, Classes, SysUtils, Forms, Dialogs, Controls, Menus, ActnList, ActnMenus, ImgList,
  ToolsAPI, FileHistoryAPI, EditorViewSupport,
  svn_client, SvnClient;

type
  TSvnIDESettings = class
  private
    FDirectories: string;
    FDirHistory: TStrings;
    FModified: Boolean;

    procedure SetDirectories(const Value: string);
  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadSettings;
    procedure SaveSettings;

    property Directories: string read FDirectories write SetDirectories;
    property DirHistory: TStrings read FDirHistory;
    property Modified: Boolean read FModified;
  end;

  TSvnIDEClient = class(TDataModule)
    ActionCancel: TAction;
    ActionCheckModifications: TAction;
    ActionCleanup: TAction;
    ActionCommit: TAction;
    ActionOptions: TAction;
    ActionRevert: TAction;
    Actions: TActionList;
    ActionUpdate: TAction;
    ImageList: TImageList;

    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);

    procedure ActionCancelExecute(Sender: TObject);
    procedure ActionCancelUpdate(Sender: TObject);
    procedure ActionCheckModificationsExecute(Sender: TObject);
    procedure ActionCheckModificationsUpdate(Sender: TObject);
    procedure ActionCleanupExecute(Sender: TObject);
    procedure ActionCleanupUpdate(Sender: TObject);
    procedure ActionCommitExecute(Sender: TObject);
    procedure ActionCommitUpdate(Sender: TObject);
    procedure ActionOptionsExecute(Sender: TObject);
    procedure ActionRevertExecute(Sender: TObject);
    procedure ActionRevertUpdate(Sender: TObject);
    procedure ActionUpdateExecute(Sender: TObject);
    procedure ActionUpdateUpdate(Sender: TObject);
  private
    FEditorView: Pointer;
    FEditorViewIntf: ICustomEditorFrameView;
    FHistoryProviderIndex: Integer;
    FMenuItem: TMenuItem;
    FSettings: TSvnIDESettings;
    FSvnClient: TSvnClient;
    FSyncData: Pointer;

    procedure Finalize;
    procedure GetDirectories(Directories: TStrings);
    procedure Initialize;
    procedure SvnClientLoginPrompt(Sender: TObject; const Realm: string; var UserName, Password: string;
      var Cancel, Save: Boolean);
    procedure SvnClientSSLClientCertPrompt(Sender: TObject; const Realm: string; var CertFileName: string;
      var Cancel, Save: Boolean);
    procedure SvnClientSSLClientPasswordPrompt(Sender: TObject; const Realm: string; var Password: string;
      var Cancel, Save: Boolean);
    procedure SvnClientSSLServerTrustPrompt(Sender: TObject; const Realm: string;
      const CertInfo: TSvnAuthSSLServerCertInfo; Failures: TSSLServerTrustFailures; var Cancel, Save: Boolean);
    procedure SvnClientUserNamePrompt(Sender: TObject; const Realm: string; var UserName: string;
      var Cancel, Save: Boolean);

    procedure SyncLoginPrompt;
    procedure SyncSSLClientCertPrompt;
    procedure SyncSSLClientPasswordPrompt;
    procedure SyncSSLServerTrustPrompt;
    procedure SyncUserNamePrompt;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function FindSvnHistoryNode(Tree: TObject; Revision: Integer): Pointer;
    function GetEditWindow: TCustomForm;
    function SelectEditorView(const TabCaption: string): Boolean;
    function ShowDiff(const FileName: string; FromRevision, ToRevision: Integer): Boolean;
    procedure ShowEditor(const FileName: string);

    property SvnClient: TSvnClient read FSvnClient;
  end;

var
  SvnIDEModule: TSvnIDEClient = nil;

resourcestring
  SConfirmCleanup = 'Clean up working copy directories?';
  SConfirmRevert = 'Revert local changes?';

procedure Register;

implementation

uses
  Registry, ActnMan, StdCtrls, Tabs,
  DeskUtil,
  SvnImages, SvnClientLoginPrompt, SvnClientSSLClientCertPrompt, SvnClientSSLServerTrustPrompt, SvnLogMessagePrompt,
  SvnIDEHistory, SvnEditorView, SvnOptionsDialog, SvnToolForm;

{$R *.dfm}

type
  THackActionMainMenuBar = class(TActionMainMenuBar);

  PHistoryNodeData = ^THistoryNodeData;
  THistoryNodeData = record
    History: IOTAFileHistory;
    Index: Integer;
  end;

  PSyncLoginPrompt = ^TSyncLoginPrompt;
  TSyncLoginPrompt = record
    SvnClient: TSvnClient;
    Realm: string;
    UserName: string;
    Password: string;
    Cancel: Boolean;
    Save: Boolean;
  end;

  PSyncSSLClientCertPrompt = ^TSyncSSLClientCertPrompt;
  TSyncSSLClientCertPrompt = record
    SvnClient: TSvnClient;
    Realm: string;
    CertFileName: string;
    Cancel: Boolean;
    Save: Boolean;
  end;

  PSyncSSLClientPasswordPrompt = ^TSyncSSLClientPasswordPrompt;
  TSyncSSLClientPasswordPrompt = record
    SvnClient: TSvnClient;
    Realm: string;
    Password: string;
    Cancel: Boolean;
    Save: Boolean;
  end;

  PSyncSSLServerTrustPrompt = ^TSyncSSLServerTrustPrompt;
  TSyncSSLServerTrustPrompt = record
    SvnClient: TSvnClient;
    Realm: string;
    CertInfo: TSvnAuthSSLServerCertInfo;
    Failures: TSSLServerTrustFailures;
    Cancel: Boolean;
    Save: Boolean;
  end;

//----------------------------------------------------------------------------------------------------------------------

function ExpandRootMacro(const S: string): string; external 'coreide100.bpl' index 1158;

// vclide seems to contain a different version of Virtual TreeView; hence these imports as a workaround
function BaseVirtualTreeGetFirst(Self: TObject): Pointer; external 'vclide100.bpl' index 3704;
function BaseVirtualTreeGetNext(Self: TObject; Node: Pointer): Pointer; external 'vclide100.bpl' index 3683;
function BaseVirtualTreeGetNodeData(Self: TObject; Node: Pointer): Pointer; external 'vclide100.bpl' index 3671;
function BaseVirtualTreeScrollIntoView(Self: TObject; Node: Pointer; Center, Horizontally: Boolean): Boolean;
  external 'vclide100.bpl' index 3628;
procedure BaseVirtualTreeSetSelected(Self: TObject; Node: Pointer; Value: Boolean);
  external 'vclide100.bpl' index 3990;

//----------------------------------------------------------------------------------------------------------------------

function FindChildControl(Parent: TWinControl; const ClassName: string): TControl;

var
  I: Integer;

begin
  Result := nil;

  for I := 0 to Parent.ControlCount - 1 do
    if Parent.Controls[I].ClassNameIs(ClassName) then
    begin
      Result := Parent.Controls[I];
      Break;
    end
    else if Parent.Controls[I] is TWinControl then
    begin
      Result := FindChildControl(TWinControl(Parent.Controls[I]), ClassName);
      if Assigned(Result) then
        Break;
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

{ TSvnIDESettings private }

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnIDESettings.SetDirectories(const Value: string);

var
  I: Integer;

begin
  if not SameText(FDirectories, Value) then
  begin
    I := FDirHistory.IndexOf(FDirectories);
    if I = -1 then
      FDirHistory.Insert(0, FDirectories)
    else
      FDirHistory.Move(I, 0);

    FDirectories := Value;

    I := FDirHistory.IndexOf(Value);
    if I <> -1 then
      FDirHistory.Delete(I);
    
    FModified := True;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

{ TSvnIDESettings public }

//----------------------------------------------------------------------------------------------------------------------

constructor TSvnIDESettings.Create;

begin
  inherited Create;
  FDirectories := '';
  FDirHistory := TStringList.Create;
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TSvnIDESettings.Destroy;

begin
  if FModified then
    SaveSettings;
  FDirHistory.Free;
  inherited Destroy;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnIDESettings.LoadSettings;

var
  Services: IOTAServices;
  Registry: TRegistry;
  SKey: string;
  Count, I: Integer;

begin
  FDirectories := '';
  FDirHistory.Clear;
  if not Assigned(BorlandIDEServices) or not BorlandIDEServices.GetService(IOTAServices, Services) then
    Exit;

  Registry := TRegistry.Create(KEY_READ);
  try
    Registry.RootKey := HKEY_CURRENT_USER;
    SKey := Format('%s\Subversion', [Services.GetBaseRegistryKey]);
    if Registry.OpenKeyReadOnly(SKey) then
      FDirectories := Registry.ReadString('Directories');

    SKey := Format('%s\Subversion\hlDirectories', [Services.GetBaseRegistryKey]);
    if Registry.OpenKeyReadOnly(SKey) then
    begin
      Count := Registry.ReadInteger('Count');
      for I := 0 to Count - 1 do
        FDirHistory.Add(Registry.ReadString(Format('Item%d', [I])));
    end;
  finally
    Registry.Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnIDESettings.SaveSettings;

var
  Services: IOTAServices;
  Registry: TRegistry;
  SKey: string;
  I: Integer;

begin
  if not Assigned(BorlandIDEServices) or not BorlandIDEServices.GetService(IOTAServices, Services) then
    Exit;

  Registry := TRegistry.Create;
  try
    Registry.RootKey := HKEY_CURRENT_USER;
    SKey := Format('%s\Subversion', [Services.GetBaseRegistryKey]);
    if Registry.OpenKey(SKey, True) then
      Registry.WriteString('Directories', FDirectories);
    SKey := Format('%s\Subversion\hlDirectories', [Services.GetBaseRegistryKey]);
    if Registry.OpenKey(SKey, True) then
    begin
      Registry.WriteInteger('Count', FDirHistory.Count);
      for I := 0 to FDirHistory.Count - 1 do
        Registry.WriteString(Format('Item%d', [I]), FDirHistory[I]);
      I := FDirHistory.Count;
      while Registry.ValueExists(Format('Item%d', [I])) do
      begin
        Registry.DeleteValue(Format('Item%d', [I]));
        Inc(I);
      end;
    end;
    
    FModified := False;
  finally
    Registry.Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

{ TSvnIDEClient private }

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnIDEClient.Finalize;

var
  FileHistoryManager: IOTAFileHistoryManager;
  MainMenuBar: THackActionMainMenuBar;
  I: Integer;
  Item: TActionClientItem;
  S: string;

  procedure TraverseItem(Item: TActionClientItem);
  var
    I: Integer;
  begin
    for I := Item.Items.Count - 1 downto 0 do
      TraverseItem(Item.Items[I]);
    Item.Free;
  end;

begin
  if (FHistoryProviderIndex <> -1) and Assigned(BorlandIDEServices) and
    BorlandIDEServices.GetService(IOTAFileHistoryManager, FileHistoryManager) then
    FileHistoryManager.UnregisterHistoryProvider(FHistoryProviderIndex);
  FHistoryProviderIndex := -1;
  if Assigned(FEditorView) then
    UnregisterEditorView(FEditorView);
  FEditorView := nil;
  FEditorViewIntf := nil;

  FreeAndNil(FSvnClient);
  FreeAndNil(FMenuItem);
  FreeAndNil(SvnImageModule);

  MainMenuBar := THackActionMainMenuBar(Application.MainForm.FindComponent('MenuBar'));
  if not Assigned(MainMenuBar) then
    Exit;
  Item := nil;
  for I := 0 to MainMenuBar.ItemCount - 1 do
  begin
    S := StringReplace(MainMenuBar.Items[I].Caption, '&', '', [rfReplaceAll]);
    if AnsiSameText(S, 'Subversion') then
    begin
      Item := MainMenuBar.Items[I];
      Break;
    end;
  end;
  if Assigned(Item) then
    TraverseItem(Item);
    
  FreeAndNil(FormSvnTools);
  if Assigned(UnregisterFieldAddress) then
    UnregisterFieldAddress(@FormSvnTools);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnIDEClient.GetDirectories(Directories: TStrings);

var
  StringList: TStringList;
  I: Integer;
  S: string;

begin
  StringList := TStringList.Create;
  try
    StringList.Delimiter := ';';
    StringList.StrictDelimiter := True;
    StringList.DelimitedText := FSettings.Directories;
    for I := StringList.Count - 1 downto 0 do
    begin
      S := ExpandRootMacro(StringList[I]);
      if DirectoryExists(S) then
        StringList[I] := S
      else
        StringList.Delete(I);
    end;

    Directories.BeginUpdate;
    try
      Directories.Assign(StringList);
    finally
      Directories.EndUpdate;
    end;
  finally
    StringList.Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnIDEClient.Initialize;

var
  FileHistoryManager: IOTAFileHistoryManager;
  NTAServices: INTAServices;
  MenuItem: TMenuItem;
  I: Integer;

begin
  FSettings.LoadSettings;

  SvnImageModule := TSvnImageModule.Create(Self);
  FSvnClient := TSvnClient.Create;
  FSvnClient.OnLoginPrompt := SvnClientLoginPrompt;
  FSvnClient.OnUserNamePrompt := SvnClientUserNamePrompt;
  FSvnClient.OnSSLServerTrustPrompt := SvnClientSSLServerTrustPrompt;
  FSvnClient.OnSSLClientCertPrompt := SvnClientSSLClientCertPrompt;
  FSvnClient.OnSSLClientPasswordPrompt := SvnClientSSLClientPasswordPrompt;
  FSvnClient.Initialize;

  if Assigned(BorlandIDEServices) then
  begin
    if BorlandIDEServices.GetService(IOTAFileHistoryManager, FileHistoryManager) then
      FHistoryProviderIndex := FileHistoryManager.RegisterHistoryProvider(TSvnFileHistoryProvider.Create);
    FEditorViewIntf := TSvnEditorView.Create;
    FEditorView := RegisterEditorView(FEditorViewIntf);

    if BorlandIDEServices.GetService(INTAServices, NTAServices) then
    begin
      FMenuItem := TMenuItem.Create(Application.MainForm);
      FMenuItem.Name := 'SubversionMenu';
      FMenuItem.Caption := 'Subversion';

      for I := 0 to Actions.ActionCount - 1 do
        if Actions[I] <> ActionOptions then
        begin
          MenuItem := nil;
          try
            MenuItem := TMenuItem.Create(Self);
            MenuItem.Name := 'Menu' + Actions[I].Name;
            MenuItem.Action := Actions[I];
            FMenuItem.Add(MenuItem);
          except
            FreeAndNil(MenuItem);
            raise;
          end;
        end;

      MenuItem := nil;
      try
        MenuItem := TMenuItem.Create(Self);
        MenuItem.Name := 'MenuSeparator';
        MenuItem.Caption := '-';
        FMenuItem.Add(MenuItem);
      except
        FreeAndNil(MenuItem);
        raise;
      end;

      MenuItem := nil;
      try
        MenuItem := TMenuItem.Create(Self);
        MenuItem.Name := 'Menu' + ActionOptions.Name;
        MenuItem.Action := ActionOptions;
        FMenuItem.Add(MenuItem);
      except
        FreeAndNil(MenuItem);
        raise;
      end;

      NTAServices.AddActionMenu('ToolsMenu', nil, FMenuItem);
    end;
  end;

  RegisterFieldAddress(SvnToolFormSection, @FormSvnTools);
  RegisterDesktopFormClass(TFormSvnTools, SvnToolFormSection, SvnToolFormSection);

  FormSvnTools := TFormSvnTools.Create(Application);
  FormSvnTools.Name := SvnToolFormSection;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnIDEClient.SvnClientLoginPrompt(Sender: TObject; const Realm: string; var UserName, Password: string;
  var Cancel, Save: Boolean);

begin
  FSyncData := AllocMem(SizeOf(TSyncLoginPrompt));
  try
    PSyncLoginPrompt(FSyncData)^.SvnClient := Sender as TSvnClient;
    PSyncLoginPrompt(FSyncData)^.Realm := Realm;
    PSyncLoginPrompt(FSyncData)^.UserName := UserName;
    PSyncLoginPrompt(FSyncData)^.Password := Password;
    PSyncLoginPrompt(FSyncData)^.Cancel := Cancel;
    PSyncLoginPrompt(FSyncData)^.Save := Save;
    TThread.Synchronize(nil, SyncLoginPrompt);
    UserName := PSyncLoginPrompt(FSyncData)^.UserName;
    Password := PSyncLoginPrompt(FSyncData)^.Password;
    Cancel := PSyncLoginPrompt(FSyncData)^.Cancel;
    Save := PSyncLoginPrompt(FSyncData)^.Save;
  finally
    System.Finalize(TSyncLoginPrompt(FSyncData^));
    FreeMem(FSyncData);
    FSyncData := nil;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnIDEClient.SvnClientSSLClientCertPrompt(Sender: TObject; const Realm: string; var CertFileName: string;
  var Cancel, Save: Boolean);

begin
  FSyncData := AllocMem(SizeOf(TSyncSSLClientCertPrompt));
  try
    PSyncSSLClientCertPrompt(FSyncData)^.SvnClient := Sender as TSvnClient;
    PSyncSSLClientCertPrompt(FSyncData)^.Realm := Realm;
    PSyncSSLClientCertPrompt(FSyncData)^.CertFileName := CertFileName;
    PSyncSSLClientCertPrompt(FSyncData)^.Cancel := Cancel;
    PSyncSSLClientCertPrompt(FSyncData)^.Save := Save;
    TThread.Synchronize(nil, SyncSSLClientCertPrompt);
    CertFileName := PSyncSSLClientCertPrompt(FSyncData)^.CertFileName;
    Cancel := PSyncSSLClientCertPrompt(FSyncData)^.Cancel;
    Save := PSyncSSLClientCertPrompt(FSyncData)^.Save;
  finally
    System.Finalize(TSyncSSLClientCertPrompt(FSyncData^));
    FreeMem(FSyncData);
    FSyncData := nil;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnIDEClient.SvnClientSSLClientPasswordPrompt(Sender: TObject; const Realm: string; var Password: string;
  var Cancel, Save: Boolean);

begin
  FSyncData := AllocMem(SizeOf(TSyncSSLClientPasswordPrompt));
  try
    PSyncSSLClientPasswordPrompt(FSyncData)^.SvnClient := Sender as TSvnClient;
    PSyncSSLClientPasswordPrompt(FSyncData)^.Realm := Realm;
    PSyncSSLClientPasswordPrompt(FSyncData)^.Password := Password;
    PSyncSSLClientPasswordPrompt(FSyncData)^.Cancel := Cancel;
    PSyncSSLClientPasswordPrompt(FSyncData)^.Save := Save;
    TThread.Synchronize(nil, SyncSSLClientPasswordPrompt);
    Password := PSyncSSLClientPasswordPrompt(FSyncData)^.Password;
    Cancel := PSyncSSLClientPasswordPrompt(FSyncData)^.Cancel;
    Save := PSyncSSLClientPasswordPrompt(FSyncData)^.Save;
  finally
    System.Finalize(TSyncSSLClientPasswordPrompt(FSyncData^));
    FreeMem(FSyncData);
    FSyncData := nil;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnIDEClient.SvnClientSSLServerTrustPrompt(Sender: TObject; const Realm: string;
  const CertInfo: TSvnAuthSSLServerCertInfo; Failures: TSSLServerTrustFailures; var Cancel, Save: Boolean);

begin
  FSyncData := AllocMem(SizeOf(TSyncSSLServerTrustPrompt));
  try
    PSyncSSLServerTrustPrompt(FSyncData)^.SvnClient := Sender as TSvnClient;
    PSyncSSLServerTrustPrompt(FSyncData)^.Realm := Realm;
    PSyncSSLServerTrustPrompt(FSyncData)^.CertInfo := CertInfo;
    PSyncSSLServerTrustPrompt(FSyncData)^.Failures := Failures;
    PSyncSSLServerTrustPrompt(FSyncData)^.Cancel := Cancel;
    PSyncSSLServerTrustPrompt(FSyncData)^.Save := Save;
    TThread.Synchronize(nil, SyncSSLServerTrustPrompt);
    Cancel := PSyncSSLServerTrustPrompt(FSyncData)^.Cancel;
    Save := PSyncSSLServerTrustPrompt(FSyncData)^.Save;
  finally
    System.Finalize(TSyncSSLServerTrustPrompt(FSyncData^));
    FreeMem(FSyncData);
    FSyncData := nil;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnIDEClient.SvnClientUserNamePrompt(Sender: TObject; const Realm: string; var UserName: string;
  var Cancel, Save: Boolean);

begin
  FSyncData := AllocMem(SizeOf(TSyncLoginPrompt));
  try
    PSyncLoginPrompt(FSyncData)^.SvnClient := Sender as TSvnClient;
    PSyncLoginPrompt(FSyncData)^.Realm := Realm;
    PSyncLoginPrompt(FSyncData)^.UserName := UserName;
    PSyncLoginPrompt(FSyncData)^.Password := '';
    PSyncLoginPrompt(FSyncData)^.Cancel := Cancel;
    PSyncLoginPrompt(FSyncData)^.Save := Save;
    TThread.Synchronize(nil, SyncUserNamePrompt);
    UserName := PSyncLoginPrompt(FSyncData)^.UserName;
    Cancel := PSyncLoginPrompt(FSyncData)^.Cancel;
    Save := PSyncLoginPrompt(FSyncData)^.Save;
  finally
    System.Finalize(TSyncLoginPrompt(FSyncData^));
    FreeMem(FSyncData);
    FSyncData := nil;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnIDEClient.SyncLoginPrompt;

begin
  with PSyncLoginPrompt(FSyncData)^ do
    Cancel := ShowSvnClientLoginPrompt(SvnClient, Realm, UserName, Password, Save) <> mrOK;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnIDEClient.SyncSSLClientCertPrompt;

begin
  with PSyncSSLClientCertPrompt(FSyncData)^ do
    Cancel := ShowSvnClientSSLClientCertPrompt(SvnClient, Realm, CertFileName, Save) <> mrOK;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnIDEClient.SyncSSLClientPasswordPrompt;

begin
  with PSyncSSLClientPasswordPrompt(FSyncData)^ do
    Cancel := ShowSvnClientSSLClientPasswordPrompt(SvnClient, Realm, Password, Save) <> mrOK;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnIDEClient.SyncSSLServerTrustPrompt;

begin
  with PSyncSSLServerTrustPrompt(FSyncData)^ do
    Cancel := ShowSvnClientSSLServerTrustPrompt(SvnClient, Realm, CertInfo, Failures, Save) <> mrOK;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnIDEClient.SyncUserNamePrompt;

begin
  with PSyncLoginPrompt(FSyncData)^ do
    Cancel := ShowSvnClientLoginPrompt(SvnClient, Realm, UserName, Password, Save, [lpoUserName]) <> mrOK;
end;

//----------------------------------------------------------------------------------------------------------------------

{ TSvnIDEClient public }

//----------------------------------------------------------------------------------------------------------------------

constructor TSvnIDEClient.Create(AOwner: TComponent);

begin
  inherited Create(AOwner);
  FSvnClient := nil;
  FHistoryProviderIndex := -1;
  FEditorView := nil;
  FMenuItem := nil;
  FSettings := TSvnIDESettings.Create;
  FSyncData := nil;
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TSvnIDEClient.Destroy;

begin
  FSettings.Free;
  inherited Destroy;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnIDEClient.FindSvnHistoryNode(Tree: TObject; Revision: Integer): Pointer;

var
  Node: Pointer;
  Data: PHistoryNodeData;
  SvnFileHistory: ISvnFileHistory;

begin
  Result := nil;

  Node := BaseVirtualTreeGetFirst(Tree);
  // -1 = top node ('File' in the from treeview, 'Buffer' in the to treeview)
  if Revision = -1 then
  begin
    Result := Node;
    Exit;
  end;

  while Assigned(Node) do
  begin
    Data := BaseVirtualTreeGetNodeData(Tree, Node);
    if Assigned(Data) and Assigned(Data^.History) and
      Succeeded(Data^.History.QueryInterface(ISvnFileHistory, SvnFileHistory)) and Assigned(SvnFileHistory.Item) and
      (SvnFileHistory.Item.HistoryItems[Data^.Index].Revision = Revision) then
    begin
      Result := Node;
      Break;
    end;

    Node := BaseVirtualTreeGetNext(Tree, Node);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnIDEClient.GetEditWindow: TCustomForm;

var
  EditorServices: IOTAEditorServices;
  EditView: IOTAEditView;
  NTAEditWindow: INTAEditWindow;

begin
  Result := nil;
  if not BorlandIDEServices.GetService(IOTAEditorServices, EditorServices) then
    Exit;
  EditView := EditorServices.TopView;
  if not Assigned(EditView) then
    Exit;
  NTAEditWindow := EditView.GetEditWindow;
  if not Assigned(NTAEditWindow) then
    Exit;
  Result := NTAEditWindow.Form;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnIDEClient.SelectEditorView(const TabCaption: string): Boolean;

var
  Form: TCustomForm;
  ViewBar: TTabSet;
  TabIndex: Integer;

begin
  Result := False;
  Form := GetEditWindow;
  if not Assigned(Form) then
    Exit;
  ViewBar := TTabSet(Form.FindComponent('ViewBar'));
  if not Assigned(ViewBar) then
    Exit;
  TabIndex := ViewBar.Tabs.IndexOf(TabCaption);
  if TabIndex = -1 then
    Exit;
  ViewBar.TabIndex := TabIndex;
  Result := True;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnIDEClient.ShowDiff(const FileName: string; FromRevision, ToRevision: Integer): Boolean;

var
  ModuleServices: IOTAModuleServices;
  Module: IOTAModule;
  SourceEditor: IOTASourceEditor;
  I: Integer;
  Form: TCustomForm;
  HistoryFrame: TCustomFrame;
  TabSet1: TTabSet;
  Index: Integer;
  FileSelector: TComboBox;
  FromTree, ToTree: TObject;
  Node: Pointer;

begin
  Result := False;

  // open and show the file in source code editor
  if not BorlandIDEServices.GetService(IOTAModuleServices, ModuleServices) then
    Exit;
  Module := ModuleServices.FindModule(FileName);
  if not Assigned(Module) then
    Module := ModuleServices.OpenModule(FileName);
  if not Assigned(Module) then
    Exit;
  SourceEditor := nil;
  for I := 0 to Module.ModuleFileCount - 1 do
    if Succeeded(Module.ModuleFileEditors[I].QueryInterface(IOTASourceEditor, SourceEditor)) then
      Break;
  if Assigned(SourceEditor) then
  begin
    SourceEditor.Show;
    SourceEditor.SwitchToView('Borland.FileHistoryView');
  end
  else if Assigned(Module.CurrentEditor) then
  begin
    Module.CurrentEditor.Show;
    // switch to 'History' tab
    if not SelectEditorView('History') then
      Exit;
  end;

  // find the history frame
  Form := GetEditWindow;
  if not Assigned(Form) then
    Exit;
  HistoryFrame := TCustomFrame(FindChildControl(Form, 'TFileHistoryFrame'));
  if not Assigned(HistoryFrame) then
    Exit;

  // switch to 'Diff' tab
  TabSet1 := TTabSet(HistoryFrame.FindComponent('TabSet1'));
  if not Assigned(TabSet1) then
    Exit;
  Index := TabSet1.Tabs.IndexOf('Diff');
  if Index = -1 then
    Exit;
  TabSet1.TabIndex := Index;

  // select file
  if Module.ModuleFileCount > 1 then
  begin
    FileSelector := TComboBox(HistoryFrame.FindComponent('FileSelector'));
    if not Assigned(FileSelector) then
      Exit;
    Index := FileSelector.Items.IndexOf(ExtractFileName(FileName));
    if Index = -1 then
      Exit;
    FileSelector.ItemIndex := Index;
    if Assigned(FileSelector.OnClick) then
      FileSelector.OnClick(FileSelector);
  end;

  // find the treeviews
  FromTree := HistoryFrame.FindComponent('DiffFrom');
  if not Assigned(FromTree) then
    Exit;
  ToTree := HistoryFrame.FindComponent('DiffTo');
  if not Assigned(ToTree) then
    Exit;

  // select and show from node
  Node := FindSvnHistoryNode(FromTree, FromRevision);
  if not Assigned(Node) then
    Exit;
  BaseVirtualTreeSetSelected(FromTree, Node, True);
  BaseVirtualTreeScrollIntoView(FromTree, Node, True, False);

  // select and show to node
  Node := FindSvnHistoryNode(ToTree, ToRevision);
  if not Assigned(Node) then
    Exit;
  BaseVirtualTreeSetSelected(ToTree, Node, True);
  BaseVirtualTreeScrollIntoView(ToTree, Node, True, False);

  Result := True;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnIDEClient.ShowEditor(const FileName: string);

var
  ModuleServices: IOTAModuleServices;
  Module: IOTAModule;
  Editor: IOTAEditor;
  I: Integer;

begin
  Module := nil;
  if BorlandIDEServices.GetService(IOTAModuleServices, ModuleServices) then
  begin
    Module := ModuleServices.FindModule(FileName);
    if not Assigned(Module) then
      Module := ModuleServices.OpenModule(FileName);
  end;
  if not Assigned(Module) then
    Exit;

  Editor := nil;
  for I := 0 to Module.ModuleFileCount - 1 do
    if AnsiSameText(FileName, Module.ModuleFileEditors[I].FileName) then
    begin
      Editor := Module.ModuleFileEditors[I];
      Break;
    end;
  if Assigned(Editor) then
    Editor.Show
  else if Assigned(Module.CurrentEditor) then
    Module.CurrentEditor.Show;
end;

//----------------------------------------------------------------------------------------------------------------------

{ TSvnIDEClient event handlers }

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnIDEClient.DataModuleCreate(Sender: TObject);

begin
  SvnIDEModule := Self;
  Initialize;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnIDEClient.DataModuleDestroy(Sender: TObject);

begin
  Finalize;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnIDEClient.ActionCancelExecute(Sender: TObject);

begin
  if Assigned(FormSvnTools) and FormSvnTools.IsBusy then
    FormSvnTools.Cancel;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnIDEClient.ActionCancelUpdate(Sender: TObject);

begin
  (Sender as TAction).Enabled := Assigned(FormSvnTools) and FormSvnTools.IsBusy;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnIDEClient.ActionCheckModificationsExecute(Sender: TObject);

var
  Directories: TStringList;

begin
  Directories := TStringList.Create;
  try
    GetDirectories(Directories);
    if Directories.Count > 0 then
    begin
      if not Assigned(FormSvnTools) then
        FormSvnTools := TFormSvnTools.Create(Application);
      FormSvnTools.Show;
      FormSvnTools.StartSvnCheckModifications(FSvnClient, Directories);
    end;
  finally
    Directories.Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnIDEClient.ActionCheckModificationsUpdate(Sender: TObject);

begin
  (Sender as TAction).Enabled := not Assigned(FormSvnTools) or not FormSvnTools.IsBusy;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnIDEClient.ActionCleanupExecute(Sender: TObject);

var
  Directories: TStringList;

begin
  if MessageDlg(SConfirmCleanup, mtConfirmation, [mbYes, mbNo], 0, mbNo) <> mrYes then
    Exit;

  Directories := TStringList.Create;
  try
    GetDirectories(Directories);
    if Directories.Count > 0 then
    begin
      if not Assigned(FormSvnTools) then
        FormSvnTools := TFormSvnTools.Create(Application);
      FormSvnTools.Show;
      FormSvnTools.StartSvnCleanup(FSvnClient, Directories);
    end;
  finally
    Directories.Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnIDEClient.ActionCleanupUpdate(Sender: TObject);

begin
  (Sender as TAction).Enabled := not Assigned(FormSvnTools) or not FormSvnTools.IsBusy;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnIDEClient.ActionCommitExecute(Sender: TObject);

var
  LogMessage: string;
  Directories: TStringList;

begin
  LogMessage := '';
  if ShowSvnLogMessagePrompt('Enter your commit log message:', LogMessage) <> mrOK then
    Exit;

  Directories := TStringList.Create;
  try
    GetDirectories(Directories);
    if Directories.Count > 0 then
    begin
      if not Assigned(FormSvnTools) then
        FormSvnTools := TFormSvnTools.Create(Application);
      FormSvnTools.Show;
      FormSvnTools.StartSvnCommit(FSvnClient, Directories, LogMessage);
    end;
  finally
    Directories.Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnIDEClient.ActionCommitUpdate(Sender: TObject);

begin
  (Sender as TAction).Enabled := not Assigned(FormSvnTools) or not FormSvnTools.IsBusy;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnIDEClient.ActionOptionsExecute(Sender: TObject);

var
  Directories: string;
  History: TStrings;

begin
  FSettings.LoadSettings;

  Directories := FSettings.Directories;
  History := TStringList.Create;
  try
    History.Assign(FSettings.DirHistory);
    if ShowSvnOptionsDialog(Directories, History) = mrOK then
    begin
      FSettings.Directories := Directories;
      FSettings.SaveSettings;
    end;
  finally
    History.Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnIDEClient.ActionRevertExecute(Sender: TObject);

var
  Directories: TStringList;

begin
  if MessageDlg(SConfirmRevert, mtConfirmation, [mbYes, mbNo], 0, mbNo) <> mrYes then
    Exit;
    
  Directories := TStringList.Create;
  try
    GetDirectories(Directories);
    if Directories.Count > 0 then
    begin
      if not Assigned(FormSvnTools) then
        FormSvnTools := TFormSvnTools.Create(Application);
      FormSvnTools.Show;
      FormSvnTools.StartSvnRevert(FSvnClient, Directories);
    end;
  finally
    Directories.Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnIDEClient.ActionRevertUpdate(Sender: TObject);

begin
  (Sender as TAction).Enabled := not Assigned(FormSvnTools) or not FormSvnTools.IsBusy;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnIDEClient.ActionUpdateExecute(Sender: TObject);

var
  Directories: TStringList;

begin
  Directories := TStringList.Create;
  try
    GetDirectories(Directories);
    if Directories.Count > 0 then
    begin
      if not Assigned(FormSvnTools) then
        FormSvnTools := TFormSvnTools.Create(Application);
      FormSvnTools.Show;
      FormSvnTools.StartSvnUpdate(FSvnClient, Directories);
    end;
  finally
    Directories.Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnIDEClient.ActionUpdateUpdate(Sender: TObject);

begin
  (Sender as TAction).Enabled := not Assigned(FormSvnTools) or not FormSvnTools.IsBusy;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure Register;

begin
  TSvnIDEClient.Create(nil);
end;

//----------------------------------------------------------------------------------------------------------------------

initialization

finalization
  FreeAndNil(SvnIDEModule);

//----------------------------------------------------------------------------------------------------------------------

end.
