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
{   Uwe Schuster (uschuster)                                                                                           }
{                                                                                                                      }
{**********************************************************************************************************************}
{                                                                                                                      }
{ This unit contains TSvnIDEClient, a utility data module class for Delphi IDE Subversion plugin (to be used as a      }
{ single global instance).                                                                                             }
{                                                                                                                      }
{**********************************************************************************************************************}

unit SvnIDEClient;

interface

{$INCLUDE Compilers.inc}

uses
  Windows, Classes, SysUtils, Forms, Messages, Graphics, Dialogs, Controls, Menus, ActnList, ActnMenus, ImgList,
  ToolsAPI, FileHistoryAPI, EditorViewSupport, Dockform,
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
    procedure InsertBlamePanel(Form: TCustomForm);
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

    function FindFirstSvnHistoryNode(Tree: TObject): Pointer;
    function FindSvnHistoryNode(Tree: TObject; Revision: Integer): Pointer;
    function GetEditWindow: TCustomForm;
    function GetSvnHistoryNodeItem(Tree: TObject; Node: Pointer): TSvnHistoryItem;
    procedure SetupBlamePanel;
    function ShowBlame(const FileName: string): Boolean;
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
  Registry, ActnMan, StdCtrls, Tabs, ComCtrls, ExtCtrls,
  DeskUtil,
  SvnImages, SvnClientLoginPrompt, SvnClientSSLClientCertPrompt, SvnClientSSLServerTrustPrompt, SvnLogMessagePrompt,
  SvnIDEHistory, SvnEditorView, SvnOptionsDialog, SvnToolForm;

{$R *.dfm}

type
  THackActionMainMenuBar = class(TActionMainMenuBar);
  THackWinControl = class(TWinControl);

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

const
  {$IFDEF COMPILER_10}
  coreide = 'coreide100.bpl';
  vclide = 'vclide100.bpl';
  {$ENDIF}

  {$IFDEF COMPILER_9}
  coreide = 'coreide90.bpl';
  vclide = 'vclide90.bpl';
  {$ENDIF}

  SExpandRootMacro = '@Uiutils@ExpandRootMacro$qqrx17System@AnsiString';
  SEditControlGetLinesInWindow = '@Editorcontrol@TCustomEditControl@GetLinesInWindow$qqrv';
  SEditControlGetTopLine = '@Editorcontrol@TCustomEditControl@GetTopLine$qqrv';

  SBaseVirtualTreeGetFirst = '@Idevirtualtrees@TBaseVirtualTree@GetFirst$qqrv';
  SBaseVirtualTreeGetFirstSelected = '@Idevirtualtrees@TBaseVirtualTree@GetFirstSelected$qqrv';
  SBaseVirtualTreeGetNext = '@Idevirtualtrees@TBaseVirtualTree@GetNext$qqrp28Idevirtualtrees@TVirtualNode';
  SBaseVirtualTreeGetNodeData = '@Idevirtualtrees@TBaseVirtualTree@GetNodeData$qqrp28Idevirtualtrees@TVirtualNode';
  SBaseVirtualTreeScrollIntoView = '@Idevirtualtrees@TBaseVirtualTree@ScrollIntoView$qqrp28Idevirtualtrees@TVirtualNodeoo';
  SBaseVirtualTreeSetSelected = '@Idevirtualtrees@TBaseVirtualTree@SetSelected$qqrp28Idevirtualtrees@TVirtualNodeo';

function ExpandRootMacro(const S: string): string; external coreide name SExpandRootMacro;
function EditControlGetLinesInWindow(Self: TObject): Integer; external coreide name SEditControlGetLinesInWindow;
function EditControlGetTopLine(Self: TObject): Integer; external coreide name SEditControlGetTopLine;

// vclide seems to contain a different version of Virtual TreeView; hence these imports as a workaround
function BaseVirtualTreeGetFirst(Self: TObject): Pointer; external vclide name SBaseVirtualTreeGetFirst;
function BaseVirtualTreeGetFirstSelected(Self: TObject): Pointer; external vclide name SBaseVirtualTreeGetFirstSelected;
function BaseVirtualTreeGetNext(Self: TObject; Node: Pointer): Pointer; external vclide name SBaseVirtualTreeGetNext;
function BaseVirtualTreeGetNodeData(Self: TObject; Node: Pointer): Pointer; external vclide
  name SBaseVirtualTreeGetNodeData;
function BaseVirtualTreeScrollIntoView(Self: TObject; Node: Pointer; Center, Horizontally: Boolean): Boolean;
  external vclide name SBaseVirtualTreeScrollIntoView;
procedure BaseVirtualTreeSetSelected(Self: TObject; Node: Pointer; Value: Boolean);
  external vclide name SBaseVirtualTreeSetSelected;

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

const
  BlameColors: array[0..25] of TColor = (
    $00E0E1F5, //   0 220 120
    $00E0EAF5, //  20 220 120
    $00E0F5F5, //  40 220 120
    $00E0F5EA, //  60 220 120
    $00E1F5E0, //  80 220 120
    $00EAF5E0, // 100 220 120
    $00F5F5E0, // 120 220 120
    $00F5EAE0, // 140 220 120
    $00F5E0E0, // 160 220 120
    $00F5E0EA, // 180 220 120
    $00F5E0F5, // 200 220 120
    $00EAE0F5, // 220 220 120
    $00E0E0F5, // 240 220 120
    $00E0E6F5, //  10 220 120
    $00E0F0F5, //  30 220 120
    $00E0F5F0, //  50 220 120
    $00E0F5E6, //  70 220 120
    $00E6F5E0, //  90 220 120
    $00F0F5E0, // 110 220 120
    $00F5F0E0, // 130 220 120
    $00F5E6E0, // 150 220 120
    $00F5E0E6, // 170 220 120
    $00F5E0F0, // 190 220 120
    $00F0E0F5, // 210 220 120
    $00E6E0F5, // 230 220 120
    $00E0E0E0  // -10 220 120
  );

type
  TBlameControl = class(TCustomControl)
  private
    FAuthors: TStringList;
    FControl: TWinControl;
    FControlProc: TWndMethod;
    FLastItem: TSvnHistoryItem;
    FLastColor: Integer;
    FRevisionX: Integer;
    FTimeX: Integer;

    procedure BlameLoaded(Sender: TObject);
    procedure ControlProc(var Message: TMessage);
    function GetNextColor: TColor;

    procedure CMHintShow(var Message: TMessage); message CM_HINTSHOW;
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent; AControl: TWinControl); reintroduce;
    destructor Destroy; override;
  end;

//----------------------------------------------------------------------------------------------------------------------

{ TBlameControl private }

//----------------------------------------------------------------------------------------------------------------------

procedure TBlameControl.BlameLoaded(Sender: TObject);

begin
  Invalidate;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBlameControl.ControlProc(var Message: TMessage);

begin
  FControlProc(Message);
  case Message.Msg of
    WM_PAINT, WM_KEYDOWN, WM_VSCROLL:
      Invalidate;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBlameControl.GetNextColor: TColor;

begin
  Result := BlameColors[FLastColor];
  Inc(FLastColor);
  if FLastColor > High(BlameColors) then
    FLastColor := 0;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBlameControl.CMHintShow(var Message: TMessage);

var
  Frame: TWinControl;
  Tree: TObject;
  I, TopLine, LinesInWindow, LineHeight, Index: Integer;
  HistoryItem, BlameHistoryItem: TSvnHistoryItem;

begin
  TCMHintShow(Message).HintInfo^.ReshowTimeout := 500;
  TCMHintShow(Message).HintInfo^.HintStr := '';

  Frame := Parent.Parent;
  Tree := nil;
  for I := 0 to Frame.ControlCount - 1 do
    if SameText(Frame.Controls[I].Name, 'RevisionContentTree') then
    begin
      Tree := Frame.Controls[I];
      Break;
    end;
  if not Assigned(Tree) then
    Exit;

  HistoryItem := SvnIDEModule.GetSvnHistoryNodeItem(Tree, BaseVirtualTreeGetFirstSelected(Tree));
  if not Assigned(HistoryItem) then
    Exit;

  TopLine := EditControlGetTopLine(FControl);
  LinesInWindow := EditControlGetLinesInWindow(FControl);
  LineHeight := FControl.Height div LinesInWindow;
  Index := TopLine + TCMHintShow(Message).HintInfo^.CursorPos.Y div LineHeight - 1;
  if Index > HistoryItem.BlameCount - 1 then
    Exit;

  BlameHistoryItem := nil;
  if Assigned(HistoryItem.Owner) then
    for I := 0 to HistoryItem.Owner.HistoryCount - 1 do
      if HistoryItem.Owner.HistoryItems[I].Revision = HistoryItem.BlameItems[Index].Revision then
      begin
        BlameHistoryItem := HistoryItem.Owner.HistoryItems[I];
        Break;
      end;
  if Assigned(BlameHistoryItem) then
    TCMHintShow(Message).HintInfo^.HintStr := BlameHistoryItem.LogMessage;
end;

//----------------------------------------------------------------------------------------------------------------------

{ TBlameControl protected }

//----------------------------------------------------------------------------------------------------------------------

procedure TBlameControl.Paint;

var
  Frame: TWinControl;
  TabSet1: TTabSet;
  Tree: TObject;
  I, Index, W: Integer;
  EditorServices: IOTAEditorServices;
  EditOptions: IOTAEditOptions;
  HistoryItem: TSvnHistoryItem;
  TopLine, LinesInWindow, LineHeight, Y: Integer;
  BlameItem: TSvnBlameItem;
  R: TRect;

begin
  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := clWindow;
  Canvas.FillRect(ClientRect);

  if not Assigned(FControl) or not Assigned(Parent) or // our panel
    not Assigned(Parent.Parent) or // tab sheet
    not Assigned(Parent.Parent.Parent) then // page control
    Exit;

  // find tabset and check its index
  Frame := Parent.Parent.Parent.Parent;
  if not Assigned(Frame) then
    Exit;
  TabSet1 := TTabSet(Frame.FindComponent('TabSet1'));
  if not Assigned(TabSet1) or (TabSet1.TabIndex <> 0) then
    Exit;

  // find treeview
  Tree := Frame.FindComponent('RevisionContentTree');
  if not Assigned(Tree) then
    Exit;

  HistoryItem := SvnIDEModule.GetSvnHistoryNodeItem(Tree, BaseVirtualTreeGetFirstSelected(Tree));
  if not Assigned(HistoryItem) then
  begin
    FLastItem := nil;
    Exit;
  end;

  if not HistoryItem.HasBlameLoaded then
  begin
    Canvas.Font := Font;
    if HistoryItem.BlameError = '' then
    begin
      Canvas.Font.Color := clGrayText;
      Canvas.TextOut(8, 8, 'loading...');
      if not HistoryItem.IsLoadingBlame then
        HistoryItem.StartLoadingBlame(BlameLoaded);
      Exit;
    end
    else
    begin
      Canvas.Font.Color := clRed;
      Canvas.TextOut(8, 8, HistoryItem.BlameError);
      Exit;
    end;
  end;

  if BorlandIDEServices.GetService(IOTAEditorServices, EditorServices) then
  begin
    EditOptions := EditorServices.GetEditOptions(cDefEdPascal);
    if Assigned(EditOptions) then
    begin
      Canvas.Font.Name := EditOptions.FontName;
      Canvas.Font.Size := EditOptions.FontSize;
      Canvas.Font.Color := clWindowText;
    end;
  end;

  TopLine := EditControlGetTopLine(FControl);
  LinesInWindow := EditControlGetLinesInWindow(FControl);
  LineHeight := FControl.Height div LinesInWindow;

  if HistoryItem <> FLastItem then
  begin
    FLastColor := 0;
    FAuthors.Clear;
    FRevisionX := 0;
    FTimeX := 0;
    for I := 0 to HistoryItem.BlameCount - 1 do
    begin
      BlameItem := HistoryItem.BlameItems[I];
      if FAuthors.IndexOf(BlameItem.Author) = -1 then
      begin
        if FAuthors.Count = 0 then
          FAuthors.AddObject(BlameItem.Author, TObject(clWhite))
        else
          FAuthors.AddObject(BlameItem.Author, TObject(GetNextColor));
      end;
      W := Canvas.TextWidth(BlameItem.Author);
      if W + 8 > FRevisionX then
        FRevisionX := W + 8;
      W := Canvas.TextWidth(IntToStr(BlameItem.Revision));
      if FRevisionX + W + 8 > FTimeX then
        FTimeX := FRevisionX + W + 8;
    end;
    FLastItem := HistoryItem;
  end;

  Y := 0;
  for I := TopLine - 1 to TopLine + LinesInWindow - 1 do
  begin
    if I >= HistoryItem.BlameCount then
      Break;
    BlameItem := HistoryItem.BlameItems[I];

    if FAuthors.Find(BlameItem.Author, Index) then
      Canvas.Brush.Color := TColor(FAuthors.Objects[Index])
    else
      Canvas.Brush.Color := clWhite;
    R := Rect(0, Y, ClientWidth, Y + LineHeight);
    Canvas.FillRect(R);

    Canvas.TextOut(0, Y, BlameItem.Author);
    R := Rect(FRevisionX, Y, FTimeX - 8, Y + LineHeight);
    DrawText(Canvas.Handle, PChar(IntToStr(BlameItem.Revision)), -1, R, DT_RIGHT);
    Canvas.TextOut(FTimeX, Y, FormatDateTime('yyyy/mm/dd hh:nn:ss', BlameItem.Time));

    Inc(Y, LineHeight);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

{ TBlameControl public }

//----------------------------------------------------------------------------------------------------------------------

constructor TBlameControl.Create(AOwner: TComponent; AControl: TWinControl);

begin
  inherited Create(AOwner);
  FLastItem := nil;
  FAuthors := TStringList.Create;
  FAuthors.Sorted := True;
  FControl := AControl;
  FControlProc := FControl.WindowProc;
  FControl.WindowProc := ControlProc;
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TBlameControl.Destroy;

begin
  FAuthors.Free;
  FControl.WindowProc := FControlProc;
  FControlProc := nil;
  inherited Destroy;
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
    {$IFDEF COMPILER_10_UP}
    StringList.StrictDelimiter := True;
    {$ENDIF}
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

procedure TSvnIDEClient.InsertBlamePanel(Form: TCustomForm);

var
  HistoryFrame: TCustomFrame;
  RevisionContentTree: TObject;
  TabSet1, TabSheet1, EditControl: TWinControl;
  I: Integer;
  Panel: TPanel;
  BlameControl: TBlameControl;
  Splitter: TSplitter;

begin
  HistoryFrame := TCustomFrame(FindChildControl(Form, 'TFileHistoryFrame'));
  if not Assigned(HistoryFrame) then
    Exit;
  RevisionContentTree := HistoryFrame.FindComponent('RevisionContentTree');
  if not Assigned(RevisionContentTree) then
    Exit;

  TabSheet1 := TTabSheet(HistoryFrame.FindComponent('TabSheet1'));
  if not Assigned(TabSheet1) then
    Exit;

  // find edit control
  EditControl := nil;
  for I := 0 to TabSheet1.ControlCount - 1 do
    if TabSheet1.Controls[I].ClassNameIs('TEditControl') then
    begin
      EditControl := TabSheet1.Controls[I] as TWinControl;
      Break;
    end;

  // find panel
  Panel := nil;
  for I := 0 to TabSheet1.ControlCount - 1 do
    if SameText(TabSheet1.Controls[I].Name, 'SvnBlamePanel') then
    begin
      Panel := TPanel(TabSheet1.Controls[I]);
      Break;
    end;

  if Assigned(Panel) then
  begin
    TabSet1 := nil;
    for I := 0 to HistoryFrame.ControlCount - 1 do
      if HistoryFrame.Controls[I].ClassNameIs('TTabSet') then
      begin
        TabSet1 := TWinControl(HistoryFrame.Controls[I]);
        Break;
      end;
    if Assigned(TabSet1) and (TTabSet(TabSet1).TabIndex = 0) then
    begin
      if not Assigned(EditControl) then
        for I := 0 to Panel.ControlCount - 1 do
          if Panel.Controls[I].ClassNameIs('TEditControl') then
          begin
            EditControl := TWinControl(Panel.Controls[I]);
            Break;
          end;
      if Assigned(EditControl) then
        EditControl.Show;
    end;
  end
  else
  begin
    if not Assigned(EditControl) then
      Exit;

    EditControl.Align := alNone;
    Panel := TPanel.Create(HistoryFrame);
    Panel.Parent := TabSheet1;
    Panel.Name := 'SvnBlamePanel';
    Panel.Align := alClient;
    Panel.BevelInner := bvNone;
    Panel.BevelOuter := bvNone;
    Panel.Caption := '';
    BlameControl := TBlameControl.Create(HistoryFrame, EditControl);
    BlameControl.Parent := Panel;
    BlameControl.Name := 'SvnBlameControl';
    BlameControl.Width := 170;
    BlameControl.Align := alLeft;
    BlameControl.DoubleBuffered := True;
    BlameControl.ShowHint := True;
    Splitter := TSplitter.Create(EditControl.Parent);
    Splitter.Parent := Panel;
    Splitter.Name := 'SvnBlameSplitter';
    Splitter.Left := BlameControl.Width + 1;
    Splitter.Width := 3;
    Splitter.Align := alLeft;
    EditControl.Parent := Panel;
    EditControl.Align := alClient;
  end;
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

function TSvnIDEClient.FindFirstSvnHistoryNode(Tree: TObject): Pointer;

var
  Node: Pointer;
  Data: PHistoryNodeData;
  SvnFileHistory: ISvnFileHistory;

begin
  Result := nil;

  Node := BaseVirtualTreeGetFirst(Tree);

  while Assigned(Node) do
  begin
    Data := BaseVirtualTreeGetNodeData(Tree, Node);
    if Assigned(Data) and Assigned(Data^.History) and
      Succeeded(Data^.History.QueryInterface(ISvnFileHistory, SvnFileHistory)) and Assigned(SvnFileHistory.Item) then
    begin
      Result := Node;
      Break;
    end;

    Node := BaseVirtualTreeGetNext(Tree, Node);
  end;
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

function TSvnIDEClient.GetSvnHistoryNodeItem(Tree: TObject; Node: Pointer): TSvnHistoryItem;

var
  Data: PHistoryNodeData;
  SvnFileHistory: ISvnFileHistory;
  Item: TSvnItem;

begin
  Result := nil;

  if not Assigned(Node) then
    Exit;
  Data := BaseVirtualTreeGetNodeData(Tree, Node);
  if Assigned(Data) and Assigned(Data^.History) and
    Succeeded(Data^.History.QueryInterface(ISvnFileHistory, SvnFileHistory)) then
  begin
    Item := SvnFileHistory.Item;
    if Assigned(Item) and (Data^.Index >= 0) and (Data^.Index < Item.HistoryCount) then
      Result := Item.HistoryItems[Data^.Index];
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnIDEClient.SetupBlamePanel;

begin
  InsertBlamePanel(GetEditWindow);
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnIDEClient.ShowBlame(const FileName: string): Boolean;

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
  Tree: TObject;
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

  // switch to 'History' tab
  if not Assigned(SourceEditor) then
    Exit;
  SourceEditor.Show;
  SourceEditor.SwitchToView('Borland.FileHistoryView');

  // find the history frame
  Form := GetEditWindow;
  if not Assigned(Form) then
    Exit;
  HistoryFrame := TCustomFrame(FindChildControl(Form, 'TFileHistoryFrame'));
  if not Assigned(HistoryFrame) then
    Exit;

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

  // switch to 'Contents' tab
  TabSet1 := TTabSet(HistoryFrame.FindComponent('TabSet1'));
  if not Assigned(TabSet1) then
    Exit;
  Index := TabSet1.Tabs.IndexOf('Contents');
  if Index = -1 then
    Exit;
  TabSet1.TabIndex := Index;
  if Assigned(TabSet1.OnClick) then
    TabSet1.OnClick(TabSet1);

  // find the treeview
  Tree := HistoryFrame.FindComponent('RevisionContentTree');
  if not Assigned(Tree) then
    Exit;

  Node := FindFirstSvnHistoryNode(Tree);
  if not Assigned(Node) then
    Exit;

  // select and show node
  BaseVirtualTreeSetSelected(Tree, Node, True);
  BaseVirtualTreeScrollIntoView(Tree, Node, True, False);

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

  // switch to 'History' tab
  if not Assigned(SourceEditor) then
    Exit;
  SourceEditor.Show;
  SourceEditor.SwitchToView('Borland.FileHistoryView');

  // find the history frame
  Form := GetEditWindow;
  if not Assigned(Form) then
    Exit;
  HistoryFrame := TCustomFrame(FindChildControl(Form, 'TFileHistoryFrame'));
  if not Assigned(HistoryFrame) then
    Exit;

  // order is important; first select file, then switch tabs
  // otherwise selecting file reactivates first tab sheet

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

  // switch to 'Diff' tab
  TabSet1 := TTabSet(HistoryFrame.FindComponent('TabSet1'));
  if not Assigned(TabSet1) then
    Exit;
  Index := TabSet1.Tabs.IndexOf('Diff');
  if Index = -1 then
    Exit;
  TabSet1.TabIndex := Index;
  if Assigned(TabSet1.OnClick) then
    TabSet1.OnClick(TabSet1);

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
  if MessageDlg(SConfirmCleanup, mtConfirmation, [mbYes, mbNo], 0 {$IFDEF COMPILER_10_UP}, mbNo {$ENDIF}) <> mrYes then
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
  if MessageDlg(SConfirmRevert, mtConfirmation, [mbYes, mbNo], 0 {$IFDEF COMPILER_10_UP}, mbNo {$ENDIF}) <> mrYes then
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
