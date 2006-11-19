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
{ The Original Code is SvnEditorViewFrame.pas.                                                                         }
{                                                                                                                      }
{ The Initial Developer of the Original Code is Ondrej Kelle.                                                          }
{ Portions created by Ondrej Kelle are Copyright Ondrej Kelle. All rights reserved.                                    }
{                                                                                                                      }
{ Contributors:                                                                                                        }
{   Ondrej Kelle (tondrej)                                                                                             }
{                                                                                                                      }
{**********************************************************************************************************************}
{                                                                                                                      }
{ This unit contains the frame class used in the Subversion historyview.                                               }
{                                                                                                                      }
{**********************************************************************************************************************}

unit SvnHistoryViewFrame;

interface

uses
  Windows, Messages, SysUtils, Variants, Types, ActiveX, ComObj, Classes, Graphics, Controls, Forms, Dialogs, ComCtrls,
  StdCtrls, ExtCtrls, ToolWin, ImgList, ActnList,
  ToolsAPI,
  VirtualTrees,
  SynEdit, SynEditHighlighter,
  svn_client, SvnIDEHistory;

type
  TFrameSvnHistoryView = class(TFrame)
    ActionDiffNext: TAction;
    ActionDiffPrev: TAction;
    ActionList: TActionList;
    ActionRefresh: TAction;
    ActionRevert: TAction;
    ComboBoxFileSelector: TComboBox;
    ImageList: TImageList;
    PageControl: TPageControl;
    StatusBar: TStatusBar;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    ToolBar: TToolBar;
    ToolButtonDiffNext: TToolButton;
    ToolButtonDiffPrev: TToolButton;
    ToolButtonRefresh: TToolButton;
    ToolButtonRevert: TToolButton;
    ToolButtonSep1: TToolButton;
    ToolButtonSep2: TToolButton;

    procedure ActionRefreshExecute(Sender: TObject);
    procedure ActionRevertExecute(Sender: TObject);
    procedure ActionRevertUpdate(Sender: TObject);
    procedure ActionDiffNextExecute(Sender: TObject);
    procedure ActionDiffNextUpdate(Sender: TObject);
    procedure ActionDiffPrevExecute(Sender: TObject);
    procedure ActionDiffPrevUpdate(Sender: TObject);
    procedure ComboBoxFileSelectorClick(Sender: TObject);
    procedure PageControlChange(Sender: TObject);
  private
    FFileCount: Integer;
    FFileName: string;
    FHighlighter: TSynCustomHighlighter;

    procedure ClearLineInfos;
    procedure EditDiffGutterPaint(Sender: TObject; ALine, X, Y: Integer);
    procedure EditDiffSpecialLineColors(Sender: TObject; Line: Integer; var Special: Boolean; var FG, BG: TColor);
    function GetDiffCount: Integer;
    procedure HeaderControlSectionResize(HeaderControl: THeaderControl; Section: THeaderSection);
    procedure InitializeEdit(Edit: TSynEdit);
    procedure InitializeHighlighter;
    procedure LoadDiff(OriginalLines, ModifiedLines: TStrings; Diff: PSvnDiff);
    procedure ShowFile(Index: Integer);
    procedure Splitter3Moved(Sender: TObject);
    procedure TreeCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex;
      var Result: Integer);
    procedure TreeContentChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure TreeDiffChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure TreeDiffCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex;
      var Result: Integer);
    procedure TreeDiffGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var CellText: WideString);
    procedure TreeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure TreeGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
      Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
    procedure TreeGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
    procedure TreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var CellText: WideString);
    procedure TreeHeaderClick(Sender: TVTHeader; Column: TColumnIndex; Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
    procedure TreeInfoChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure UpdateStatusBar;
  public
    // Content tab
    EditContent: TSynEdit;
    LabelContent: TLabel;
    TreeContent: TVirtualStringTree;
    Splitter1: TSplitter;

    // Info tab
    HeaderControl: THeaderControl;
    LabelInfo: TLabel;
    ListBoxLabels: TListBox;
    MemoComments: TMemo;
    TreeInfo: TVirtualStringTree;
    Splitter2: TSplitter;
    Splitter3: TSplitter;

    // Diff tab
    EditDiff: TSynEdit;
    LabelDiffLeft: TLabel;
    LabelDiffRight: TLabel;
    PanelTrees: TPanel;
    PanelTreeLeft: TPanel;
    PanelTreeRight: TPanel;
    Splitter4: TSplitter;
    Splitter5: TSplitter;
    TreeDiffLeft: TVirtualStringTree;
    TreeDiffRight: TVirtualStringTree;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Display(const Module: IOTAModule); overload;
  end;

implementation

uses
  Registry, Math, StrUtils,
  SynHighlighterPas, SynHighlighterCpp, SynHighlighterCS, SynHighlighterHtml, SynHighlighterXML, SynHighlighterSQL,
  SynHighlighterIDL,
  SvnIDEClient, SvnHistoryManager;

{$R *.dfm}

const
  GutterMargin = 2;

type
  TDiffKind = (dkCommon, dkDeleted, dkInserted);
  PLineInfo = ^TLineInfo;
  TLineInfo = record
    LineNo: Integer;
    Kind: TDiffKind;
  end;

  PNodeData = ^TNodeData;
  TNodeData = record
    History: IOTAFileHistory;
    Index: Integer;
  end;

type
  TLocalFileHistory = class(TDispInterfacedObject, IOTAFileHistory)
  private
    FAuthor: string;
    FFileName: string;

    { IOTAFileHistory }
    function Get_Count: Integer; safecall;
    function GetAuthor(Index: Integer): WideString; safecall;
    function GetComment(Index: Integer): WideString; safecall;
    function GetContent(Index: Integer): IStream; safecall;
    function GetDate(Index: Integer): TDateTime; safecall;
    function GetIdent(Index: Integer): WideString; safecall;
    function GetHistoryStyle(Index: Integer): TOTAHistoryStyle; safecall;
    function GetLabelCount(Index: Integer): Integer; safecall;
    function GetLabels(Index, LabelIndex: Integer): WideString; safecall;
  public
    constructor Create(const AFileName: string);

    function SafeCallException(ExceptObject: TObject; ExceptAddr: Pointer): HResult; override;
  end;

type
  TBufferFileHistory = class(TDispInterfacedObject, IOTAFileHistory)
  private
    FFileName: string;
    FModuleFileName: string;

    { IOTAFileHistory }
    function Get_Count: Integer; safecall;
    function GetAuthor(Index: Integer): WideString; safecall;
    function GetComment(Index: Integer): WideString; safecall;
    function GetContent(Index: Integer): IStream; safecall;
    function GetDate(Index: Integer): TDateTime; safecall;
    function GetIdent(Index: Integer): WideString; safecall;
    function GetHistoryStyle(Index: Integer): TOTAHistoryStyle; safecall;
    function GetLabelCount(Index: Integer): Integer; safecall;
    function GetLabels(Index, LabelIndex: Integer): WideString; safecall;
  public
    constructor Create(const AModuleFileName, AFileName: string);

    function SafeCallException(ExceptObject: TObject; ExceptAddr: Pointer): HResult; override;
  end;

//----------------------------------------------------------------------------------------------------------------------

function CreateBufferStream(const Buffer: IOTAEditBuffer): TStream;

const
  BufSize = 1024;

var
  Reader: IOTAEditReader;
  ReaderPos, Read: Integer;
  Buf: array[0..BufSize] of Char;

begin
  Result := nil;
  if Buffer = nil then
    Exit;

  Reader := Buffer.CreateReader;
  Result := TMemoryStream.Create;
  try
    ReaderPos := 0;
    repeat
      Read := Reader.GetText(ReaderPos, @Buf, BufSize);
      Inc(ReaderPos, Read);
      if (Read < 0) or (Read > BufSize) then
        raise Exception.Create('Error reading from edit buffer');
      Result.Write(Buf, Read);
    until Read < BufSize;

    Result.Seek(0, soFromBeginning);
  except
    Result.Free;
    raise;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function GetAccountName(P: PSID): string;

var
  DomainNameLen, UserNameLen, Use: Cardinal;

begin
  Result := '';

  DomainNameLen := 0;
  UserNameLen := 0;
  LookupAccountSid(nil, P, nil, UserNameLen, nil, DomainNameLen, Use);
  SetLength(Result, UserNameLen);

  if LookupAccountSid(nil, P, PChar(Result), UserNameLen, nil, DomainNameLen, Use) then
    SetLength(Result, UserNameLen);
end;

//----------------------------------------------------------------------------------------------------------------------

function GetEditor(const ModuleFileName, FileName: string): IOTAEditor;

var
  ModuleServices: IOTAModuleServices;
  Module: IOTAModule;
  I, EditorIndex: Integer;

begin
  Result := nil;

  if not Supports(BorlandIDEServices, IOTAModuleServices, ModuleServices) then
    Exit;

  Module := ModuleServices.FindModule(FileName);
  if Assigned(Module) then
    Result := Module.CurrentEditor
  else
  begin
    Module := ModuleServices.FindModule(ModuleFileName);
    if not Assigned(Module) then
      Exit;

    EditorIndex := -1;
    for I := 0 to Module.ModuleFileCount - 1 do
      if AnsiSameText(Module.ModuleFileEditors[I].FileName, FileName) then
      begin
        EditorIndex := I;
        Break;
      end;

    if EditorIndex <> -1 then
      Result := Module.ModuleFileEditors[EditorIndex];
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function GetEditBuffer(const ModuleFileName, FileName: string): IOTAEditBuffer;

var
  Editor: IOTAEditor;

begin
  Editor := GetEditor(ModuleFileName, FileName);
  if Assigned(Editor) then
    Editor.QueryInterface(IOTAEditBuffer, Result)
  else
    Result := nil;
end;

//----------------------------------------------------------------------------------------------------------------------

function GetFileOwner(const FileName: string): string;

var
  SD: PSecurityDescriptor;
  Owner: Pointer;
  Defaulted: LongBool;
  Needed: Cardinal;

begin
  Needed := 0;
  GetFileSecurity(PChar(FileName), OWNER_SECURITY_INFORMATION, nil, 0, Needed);
  SD := AllocMem(Needed);
  try
    if GetFileSecurity(PChar(FileName), OWNER_SECURITY_INFORMATION, SD, Needed, Needed) and
      GetSecurityDescriptorOwner(SD, Owner, Defaulted) then
      Result := GetAccountName(Owner);
  finally
    FreeMem(SD);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function GetTempFileName(const Path, Prefix: string): string;

var
  SPath: string;

begin
  if Path = '' then
  begin
    SetLength(SPath, MAX_PATH);
    Win32Check(GetTempPath(MAX_PATH, PChar(SPath)) <> 0);
    SetLength(SPath, StrLen(PChar(SPath)));
  end
  else
    SPath := Path;
  SetLength(Result, MAX_PATH);
  Win32Check(Windows.GetTempFileName(PChar(SPath), PChar(Prefix), 0, PChar(Result)) <> 0);
  SetLength(Result, StrLen(PChar(Result)));
end;

//----------------------------------------------------------------------------------------------------------------------

function GetUserName: string;

var
  Count: Cardinal;

begin
  Result := '';
  Count := MAX_PATH + 1;
  SetLength(Result, Count);
  if Windows.GetUserName(PChar(Result), Count) then
    SetLength(Result, Count);
end;

//----------------------------------------------------------------------------------------------------------------------

function SaveStreamToFile(const Stream: IStream; const FileName: string): Boolean;

var
  Dst: IStream;
  Read, Written: Int64;

begin
  Result := False;
  if not Assigned(Stream) then
    Exit;

  Dst := TStreamAdapter.Create(TFileStream.Create(FileName, fmCreate), soOwned);
  Read := 0;
  Written := 0;
  OleCheck(Stream.CopyTo(Dst, High(Int64), Read, Written));
  Result := True;
end;

//----------------------------------------------------------------------------------------------------------------------

{ TLocalFileHistory private: IOTAFileHistory }

//----------------------------------------------------------------------------------------------------------------------

function TLocalFileHistory.Get_Count: Integer;

begin
  Result := 1;
end;

//----------------------------------------------------------------------------------------------------------------------

function TLocalFileHistory.GetAuthor(Index: Integer): WideString;

begin
  Result := FAuthor;
end;

//----------------------------------------------------------------------------------------------------------------------

function TLocalFileHistory.GetComment(Index: Integer): WideString;

begin
  Result := 'Local saved file';
end;

//----------------------------------------------------------------------------------------------------------------------

function TLocalFileHistory.GetContent(Index: Integer): IStream;

begin
  Result := TStreamAdapter.Create(TFileStream.Create(FFileName, fmOpenRead or fmShareDenyNone), soOwned);
end;

//----------------------------------------------------------------------------------------------------------------------

function TLocalFileHistory.GetDate(Index: Integer): TDateTime;

begin
  Result := FileDateToDateTime(FileAge(FFileName));
end;

//----------------------------------------------------------------------------------------------------------------------

function TLocalFileHistory.GetIdent(Index: Integer): WideString;

begin
  Result := 'File';
end;

//----------------------------------------------------------------------------------------------------------------------

function TLocalFileHistory.GetHistoryStyle(Index: Integer): TOTAHistoryStyle;

begin
  Result := hsFile;
end;

//----------------------------------------------------------------------------------------------------------------------

function TLocalFileHistory.GetLabelCount(Index: Integer): Integer;

begin
  Result := 1;
end;

//----------------------------------------------------------------------------------------------------------------------

function TLocalFileHistory.GetLabels(Index, LabelIndex: Integer): WideString;

begin
  if (Index = 0) and (LabelIndex = 0) then
    Result := 'Local File';
end;

//----------------------------------------------------------------------------------------------------------------------

{ TLocalFileHistory public }

//----------------------------------------------------------------------------------------------------------------------

constructor TLocalFileHistory.Create(const AFileName: string);

begin
  inherited Create;
  FFileName := AFileName;
  FAuthor := GetFileOwner(FFileName);
end;

//----------------------------------------------------------------------------------------------------------------------

function TLocalFileHistory.SafeCallException(ExceptObject: TObject; ExceptAddr: Pointer): HResult;

begin
  Result := HandleSafeCallException(ExceptObject, ExceptAddr, IOTAFileHistory, '', '');
end;

//----------------------------------------------------------------------------------------------------------------------

{ TBufferFileHistory private: IOTAFileHistory }

//----------------------------------------------------------------------------------------------------------------------

function TBufferFileHistory.Get_Count: Integer;

begin
  Result := 1;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBufferFileHistory.GetAuthor(Index: Integer): WideString;

begin
  Result := GetUserName;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBufferFileHistory.GetComment(Index: Integer): WideString;

begin
  Result := '';
end;

//----------------------------------------------------------------------------------------------------------------------

function TBufferFileHistory.GetContent(Index: Integer): IStream;

var
  Editor: IOTAEditor;
  Buffer: IOTAEditBuffer;
  FormEditor: IOTAFormEditor;
  NewPos: Int64;

begin
  Result := nil;

  Editor := GetEditor(FModuleFileName, FFileName);
  if not Assigned(Editor) then
    Exit;

  if Succeeded(Editor.QueryInterface(IOTAEditBuffer, Buffer)) then
    Result := TStreamAdapter.Create(CreateBufferStream(Buffer), soOwned)
  else if Succeeded(Editor.QueryInterface(IOTAFormEditor, FormEditor)) then
  begin
    Result := TStreamAdapter.Create(TMemoryStream.Create, soOwned);
    FormEditor.GetFormResource(Result);
    OleCheck(Result.Seek(0, STREAM_SEEK_SET, NewPos));
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBufferFileHistory.GetDate(Index: Integer): TDateTime;

var
  Buffer: IOTAEditBuffer;

begin
  Buffer := GetEditBuffer(FModuleFileName, FFileName);
  if Assigned(Buffer) then
    Result := Buffer.GetCurrentDate
  else
    Result := 0;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBufferFileHistory.GetIdent(Index: Integer): WideString;

begin
  Result := 'Buffer';
end;

//----------------------------------------------------------------------------------------------------------------------

function TBufferFileHistory.GetHistoryStyle(Index: Integer): TOTAHistoryStyle;

begin
  Result := hsBuffer;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBufferFileHistory.GetLabelCount(Index: Integer): Integer;

begin
  Result := 1;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBufferFileHistory.GetLabels(Index, LabelIndex: Integer): WideString;

begin
  Result := '';
end;

//----------------------------------------------------------------------------------------------------------------------

{ TBufferFileHistory public }

//----------------------------------------------------------------------------------------------------------------------

constructor TBufferFileHistory.Create(const AModuleFileName, AFileName: string);

begin
  inherited Create;
  FModuleFileName := AModuleFileName;
  FFileName := AFileName;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBufferFileHistory.SafeCallException(ExceptObject: TObject; ExceptAddr: Pointer): HResult;

begin
  Result := HandleSafeCallException(ExceptObject, ExceptAddr, IOTAFileHistory, '', '');
end;

//----------------------------------------------------------------------------------------------------------------------

{ TFrameHistory private }

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnHistoryView.ClearLineInfos;

var
  I: Integer;
  P: PLineInfo;

begin
  for I := 0 to EditDiff.Lines.Count - 1 do
  begin
    P := PLineInfo(EditDiff.Lines.Objects[I]);
    if Assigned(P) then
      FreeMem(P);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnHistoryView.EditDiffGutterPaint(Sender: TObject; ALine, X, Y: Integer);

const
  SignPenWidth = 4;

var
  P: PLineInfo;
  LineHeight, LineNoWidth, SignWidth: Integer;
  R: TRect;
  S: string;

begin
  P := PLineInfo(TSynEdit(Sender).Lines.Objects[ALine - 1]);
  if Assigned(P) then
    case P^.Kind of
      dkCommon:
        begin
          TSynEdit(Sender).Canvas.Font := TSynEdit(Sender).Gutter.Font;
          LineHeight := TSynEdit(Sender).LineHeight;
          LineNoWidth := TSynEdit(Sender).Canvas.TextWidth(StringOfChar('X', TSynEdit(Sender).Gutter.DigitCount));

          // draw line number
          R := Rect(0, 0, LineNoWidth, LineHeight);
          OffsetRect(R, X + GutterMargin, Y);
          S := IntToStr(P^.LineNo + 1);
          DrawText(TSynEdit(Sender).Canvas.Handle, PChar(S), Length(S), R, DT_SINGLELINE or DT_RIGHT);
        end;
      dkDeleted:
        begin
          LineHeight := TSynEdit(Sender).LineHeight;
          LineNoWidth := TSynEdit(Sender).Canvas.TextWidth(StringOfChar('X', TSynEdit(Sender).Gutter.DigitCount));

          // draw minus sign
          TSynEdit(Sender).Canvas.Brush.Color := $FF9C9C;
          SignWidth := 2 * ((LineHeight - 2 * GutterMargin) div 2);

          R := Rect(0, (SignWidth - SignPenWidth) div 2, SignWidth - 1, (SignWidth + SignPenWidth) div 2 - 1);
          OffsetRect(R, X + LineNoWidth + 2 * GutterMargin, Y + GutterMargin);
          TSynEdit(Sender).Canvas.FillRect(R);

          TSynEdit(Sender).Canvas.Brush.Color := TSynEdit(Sender).Gutter.Color;
        end;
      dkInserted:
        begin
          // draw line number
          TSynEdit(Sender).Canvas.Font := TSynEdit(Sender).Gutter.Font;
          LineHeight := TSynEdit(Sender).LineHeight;
          LineNoWidth := TSynEdit(Sender).Canvas.TextWidth(StringOfChar('X', TSynEdit(Sender).Gutter.DigitCount));

          R := Rect(0, 0, LineNoWidth, LineHeight);
          OffsetRect(R, X + GutterMargin, Y);
          S := IntToStr(P^.LineNo);
          DrawText(TSynEdit(Sender).Canvas.Handle, PChar(S), Length(S), R, DT_SINGLELINE or DT_RIGHT);

          // draw plus sign
          TSynEdit(Sender).Canvas.Brush.Color := $FF9C9C;
          SignWidth := 2 * ((LineHeight - 2 * GutterMargin) div 2);

          R := Rect(0, (SignWidth - SignPenWidth) div 2, SignWidth - 1, (SignWidth + SignPenWidth) div 2 - 1);
          OffsetRect(R, X + LineNoWidth + 2 * GutterMargin, Y + GutterMargin);
          TSynEdit(Sender).Canvas.FillRect(R);

          R := Rect((SignWidth - SignPenWidth) div 2, 0, (SignWidth + SignPenWidth) div 2 - 1, SignWidth - 1);
          OffsetRect(R, X + LineNoWidth + 2 * GutterMargin, Y + GutterMargin);
          TSynEdit(Sender).Canvas.FillRect(R);

          TSynEdit(Sender).Canvas.Brush.Color := TSynEdit(Sender).Gutter.Color;
        end;
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnHistoryView.EditDiffSpecialLineColors(Sender: TObject; Line: Integer; var Special: Boolean;
  var FG, BG: TColor);

const
  Colors: array[TDiffKind] of TColor = (clWindow, $FFC7C7, $CCFFCC);

var
  P: PLineInfo;

begin
  P := PLineInfo(TSynEdit(Sender).Lines.Objects[Line - 1]);
  if Assigned(P) and (P^.Kind <> dkCommon) then
  begin
    Special := True;
    BG := Colors[P^.Kind];
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TFrameSvnHistoryView.GetDiffCount: Integer;

var
  I: Integer;
  CurKind: TDiffKind;
  P: PLineInfo;

begin
  Result := 0;
  CurKind := dkCommon;

  for I := 0 to EditDiff.Lines.Count - 1 do
  begin
    P := PLineInfo(EditDiff.Lines.Objects[I]);
    if P^.Kind <> CurKind then
    begin
      if P^.Kind <> dkCommon then
        Inc(Result);
      CurKind := P^.Kind;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnHistoryView.HeaderControlSectionResize(HeaderControl: THeaderControl; Section: THeaderSection);

begin
  if Section.Index = 0 then
    ListBoxLabels.Width := Section.Right - Splitter3.Width div 2 - 1;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnHistoryView.InitializeEdit(Edit: TSynEdit);

var
  Services: IOTAServices;
  EditorServices: IOTAEditorServices;
  EditOptions: IOTAEditOptions;
  S: string;
  Reg: TRegistry;

begin
  if not Supports(BorlandIDEServices, IOTAEditorServices, EditorServices) then
    Exit;
  EditOptions := EditorServices.GetEditOptionsForFile(FFileName);
  if not Assigned(EditOptions) then
    Exit;

  Edit.Font.Name := EditOptions.FontName;
  Edit.Font.Size := EditOptions.FontSize;
  Edit.RightEdge := EditOptions.BufferOptions.RightMargin;
  if Supports(BorlandIDEServices, IOTAServices, Services) then
  begin
    S := IncludeTrailingBackslash(Services.GetBaseRegistryKey) + 'Editor\Highlight';
    Reg := TRegistry.Create(KEY_READ);
    try
      Reg.RootKey := HKEY_CURRENT_USER;
      if Reg.OpenKeyReadOnly(IncludeTrailingBackslash(S) + 'Right margin') and
        Reg.ValueExists('Foreground Color New') then
        Edit.RightEdgeColor := StringToColor(Reg.ReadString('Foreground Color New'));
    finally
      Reg.Free;
    end;
  end;
  Edit.Gutter.Font := Edit.Font;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnHistoryView.InitializeHighlighter;

var
  Services: IOTAServices;
  EditorServices: IOTAEditorServices;
  EditOptions: IOTAEditOptions;
  SID, SRegKey: string;

begin
  if not Supports(BorlandIDEServices, IOTAServices, Services) or
    not Supports(BorlandIDEServices, IOTAEditorServices, EditorServices) then
    Exit;

  SRegKey := IncludeTrailingBackslash(Services.GetBaseRegistryKey) + 'Editor\Highlight';
  EditOptions := EditorServices.GetEditOptionsForFile(FFileName);
  if not Assigned(EditOptions) then
    Exit;

  SID := EditOptions.IDString;
  if AnsiSameText(SID, cDefEdDefault) then
    FreeAndNil(FHighlighter)
  else if AnsiSameText(SID, cDefEdPascal) then
  begin
    if not (FHighlighter is TSynPasSyn) then
    begin
      FreeAndNil(FHighlighter);
      FHighlighter := TSynPasSyn.Create(Self);
      with TSynPasSyn(FHighlighter) do
      begin
        AsmAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Assembler', False);
        CharAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Character', False);
        CommentAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Comment', False);
        DirectiveAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Preprocessor', False);
        FloatAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Float', False);
        HexAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Hex', False);
        IdentifierAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Identifier', False);
        KeyAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Reserved word', False);
        NumberAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Number', False);
        SpaceAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Whitespace', False);
        StringAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'String', False);
        SymbolAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Symbol', False);
      end;
    end;
  end
  else if AnsiSameText(SID, cDefEdC) then
  begin
    if not (FHighlighter is TSynCppSyn) then
    begin
      FreeAndNil(FHighlighter);
      FHighlighter := TSynCppSyn.Create(Self);
      with TSynCppSyn(FHighlighter) do
      begin
        AsmAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Assembler', False);
        CommentAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Comment', False);
        DirecAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Preprocessor', False);
        IdentifierAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Identifier', False);
        InvalidAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Illegal Char', False);
        KeyAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Reserved word', False);
        NumberAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Number', False);
        FloatAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Float', False);
        HexAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Hex', False);
        OctalAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Octal', False);
        SpaceAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Whitespace', False);
        StringAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'String', False);
        CharAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Character', False);
        SymbolAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Symbol', False);
      end;
    end;
  end
  else if AnsiSameText(SID, cDefEdCSharp) then
  begin
    if not (FHighlighter is TSynCSSyn) then
    begin
      FreeAndNil(FHighlighter);
      FHighlighter := TSynCSSyn.Create(Self);
      with TSynCSSyn(FHighlighter) do
      begin
        AsmAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Assembler', False);
        CommentAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Comment', False);
        DirecAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Preprocessor', False);
        IdentifierAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Identifier', False);
        InvalidAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Illegal Char', False);
        KeyAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Reserved word', False);
        NumberAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Number', False);
        SpaceAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Whitespace', False);
        StringAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'String', False);
        SymbolAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Symbol', False);
      end;
    end;
  end
  else if AnsiSameText(SID, cDefEdHTML) then
  begin
    if not (FHighlighter is TSynHTMLSyn) then
    begin
      FreeAndNil(FHighlighter);
      FHighlighter := TSynHTMLSyn.Create(Self);
      with TSynHTMLSyn(FHighlighter) do
      begin
        // AndAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, '', False);
        CommentAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Comment', False);
        IdentifierAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Attribute Names', False);
        KeyAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Reserved word', False);
        SpaceAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Whitespace', False);
        SymbolAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Symbol', False);
        TextAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Plain text', False);
        // UndefKeyAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, '', False);
        ValueAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Attribute Values', False);
      end;
    end;
  end
  else if AnsiSameText(SID, cDefEdXML) then
  begin
    if not (FHighlighter is TSynXMLSyn) then
    begin
      FreeAndNil(FHighlighter);
      FHighlighter := TSynXMLSyn.Create(Self);
      with TSynXMLSyn(FHighlighter) do
      begin
        ElementAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Reserved word', False);
        AttributeAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Attribute Names', False);
        NamespaceAttributeAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Attribute Names', False);
        AttributeValueAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Attribute Values', False);
        NamespaceAttributeValueAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Attribute Values', False);
        TextAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Plain text', False);
        CDATAAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'String', False);
        EntityRefAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'String', False);
        ProcessingInstructionAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Preprocessor', False);
        CommentAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Comment', False);
        // DocTypeAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, '', False);
        SpaceAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Whitespace', False);
        SymbolAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Symbol', False);
      end;
    end;
  end
  else if AnsiSameText(SID, cDefEdSQL) then
  begin
    if not (FHighlighter is TSynSQLSyn) then
    begin
      FreeAndNil(FHighlighter);
      FHighlighter := TSynSQLSyn.Create(Self);
      with TSynSQLSyn(FHighlighter) do
      begin
        CommentAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Comment', False);
        ConditionalCommentAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Preprocessor', False);
        // DataTypeAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, '', False);
        // DefaultPackageAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, '', False);
        // DelimitedIdentifierAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, '', False);
        ExceptionAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Reserved word', False);
        FunctionAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Reserved word', False);
        IdentifierAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Identifier', False);
        KeyAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Reserved word', False);
        NumberAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Number', False);
        // PLSQLAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, '', False);
        SpaceAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Whitespace', False);
        // SQLPlusAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, '', False);
        StringAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'String', False);
        SymbolAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Symbol', False);
        // TableNameAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, '', False);
        // VariableAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, '', False);
      end;
    end;
  end
  else if AnsiSameText(SID, cDefEdIDL) then
  begin
    if not (FHighlighter is TSynIdlSyn) then
    begin
      FreeAndNil(FHighlighter);
      FHighlighter := TSynIdlSyn.Create(Self);
      with TSynIdlSyn(FHighlighter) do
      begin
        CommentAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Comment', False);
        // DatatypeAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, '', False);
        IdentifierAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Identifier', False);
        KeyAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Reserved word', False);
        NumberAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Number', False);
        PreprocessorAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Preprocessor', False);
        SpaceAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Whitespace', False);
        StringAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'String', False);
        SymbolAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Symbol', False);
      end;
    end;
  end
  else
    FreeAndNil(FHighlighter);

  EditContent.Highlighter := FHighlighter;
  EditDiff.Highlighter := FHighlighter;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnHistoryView.LoadDiff(OriginalLines, ModifiedLines: TStrings; Diff: PSvnDiff);

var
  I: Integer;
  P: PLineInfo;

begin
  EditDiff.Lines.BeginUpdate;
  try
    ClearLineInfos;
    EditDiff.Clear;

    if not Assigned(OriginalLines) or not Assigned(ModifiedLines) or not Assigned(Diff) then
      Exit;

    if ModifiedLines.Count > 9999 then
      EditDiff.Gutter.DigitCount := Trunc(Log10(ModifiedLines.Count)) + 1
    else
      EditDiff.Gutter.DigitCount := 4;
    EditDiff.Gutter.Width := EditDiff.Canvas.TextWidth(StringOfChar('X', EditDiff.Gutter.DigitCount)) +
      EditDiff.LineHeight;

    while Assigned(Diff) do
    begin
      case Diff^._type of
        svnDifftypeCommon:
          begin
            for I := Diff^.modified_start to Diff^.modified_start + Diff^.modified_length - 1 do
            begin
              P := AllocMem(SizeOf(TLineInfo));
              try
                P^.LineNo := I;
                P^.Kind := dkCommon;
                EditDiff.Lines.AddObject(ModifiedLines[I], TObject(P));
              except
                FreeMem(P);
                raise;
              end;
            end;
          end;
        svnDiffTypeDiffModified:
          begin
            for I := Diff^.modified_start to Diff^.modified_start + Diff^.modified_length - 1 do
            begin
              P := AllocMem(SizeOf(TLineInfo));
              try
                P^.LineNo := I;
                P^.Kind := dkInserted;
                EditDiff.Lines.AddObject(ModifiedLines[I], TObject(P));
              except
                FreeMem(P);
                raise;
              end;
            end;

            for I := Diff^.original_start to Diff^.original_start + Diff^.original_length - 1 do
            begin
              P := AllocMem(SizeOf(TLineInfo));
              try
                P^.LineNo := I;
                P^.Kind := dkDeleted;
                EditDiff.Lines.AddObject(OriginalLines[I], TObject(P));
              except
                FreeMem(P);
                raise;
              end;
            end;
          end;
        svnDiffTypeDiffLatest:
          begin

          end;
        svnDiffTypeDiffCommon:
          begin

          end;
        svnDiffTypeConflict:
          begin

          end;
      end;

      Diff := Diff^.next;
    end;
  finally
    EditDiff.Lines.EndUpdate;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnHistoryView.ShowFile(Index: Integer);

var
  FileName: string;
  I, J: Integer;
  FileHistory: IOTAFileHistory;
  Node: PVirtualNode;
  Data: PNodeData;

begin
  FileName := ExtractFilePath(FFileName) + ComboBoxFileSelector.Items[Index];

  TreeContent.BeginUpdate;
  try
    TreeContent.Clear;

    TreeInfo.BeginUpdate;
    try
      TreeInfo.Clear;

      TreeDiffLeft.BeginUpdate;
      try
        TreeDiffLeft.Clear;

        TreeDiffRight.BeginUpdate;
        try
          TreeDiffRight.Clear;

          InitializeEdit(EditContent);
          InitializeEdit(EditDiff);
          InitializeHighlighter;

          Node := TreeContent.AddChild(nil);
          Data := TreeContent.GetNodeData(Node);
          Data^.History := TLocalFileHistory.Create(FileName);
          Data^.Index := 0;

          Node := TreeInfo.AddChild(nil);
          Data := TreeInfo.GetNodeData(Node);
          Data^.History := TLocalFileHistory.Create(FileName);
          Data^.Index := 0;

          Node := TreeDiffLeft.AddChild(nil);
          Data := TreeDiffLeft.GetNodeData(Node);
          Data^.History := TLocalFileHistory.Create(FileName);
          Data^.Index := 0;

          Node := TreeDiffRight.AddChild(nil);
          Data := TreeDiffRight.GetNodeData(Node);
          Data^.History := TBufferFileHistory.Create(FFileName, FileName);
          Data^.Index := 0;

          Node := TreeDiffRight.AddChild(nil);
          Data := TreeDiffRight.GetNodeData(Node);
          Data^.History := TLocalFileHistory.Create(FileName);
          Data^.Index := 0;

          for I := 0 to FileHistoryManager.Count - 1 do
          begin
            FileHistory := FileHistoryManager.FileHistoryProvider[I].GetFileHistory(FileName);
            if Assigned(FileHistory) then
            begin
              for J := 0 to FileHistory.Count - 1 do
              begin
                Node := TreeContent.AddChild(nil);
                Data := TreeContent.GetNodeData(Node);
                if Assigned(Data) then
                begin
                  Data^.History := FileHistory;
                  Data^.Index := J;
                end;

                Node := TreeInfo.AddChild(nil);
                Data := TreeInfo.GetNodeData(Node);
                if Assigned(Data) then
                begin
                  Data^.History := FileHistory;
                  Data^.Index := J;
                end;

                Node := TreeDiffLeft.AddChild(nil);
                Data := TreeDiffLeft.GetNodeData(Node);
                if Assigned(Data) then
                begin
                  Data^.History := FileHistory;
                  Data^.Index := J;
                end;

                Node := TreeDiffRight.AddChild(nil);
                Data := TreeDiffRight.GetNodeData(Node);
                if Assigned(Data) then
                begin
                  Data^.History := FileHistory;
                  Data^.Index := J;
                end;
              end;
            end;
          end;
        finally
          TreeDiffRight.EndUpdate;
        end;
      finally
        TreeDiffLeft.EndUpdate;
      end;
    finally
      TreeInfo.EndUpdate;
    end;
  finally
    TreeContent.EndUpdate;
  end;

  PageControlChange(PageControl);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnHistoryView.Splitter3Moved(Sender: TObject);

begin
  HeaderControl.Sections[0].Width := Splitter3.Left + Splitter3.Width div 2 + 1;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnHistoryView.TreeCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode;
  Column: TColumnIndex; var Result: Integer);

var
  Data1, Data2: PNodeData;

begin
  Result := 0;
  Data1 := Sender.GetNodeData(Node1);
  Data2 := Sender.GetNodeData(Node2);
  if not Assigned(Data1) or not Assigned(Data2) or not Assigned(Data1^.History) or not Assigned(Data2^.History) then
    Exit;

  case Column of
    1:
      Result := AnsiCompareStr(Data1^.History.Ident[Data1^.Index], Data2^.History.Ident[Data2^.Index]);
    2:
      Result := AnsiCompareStr(Data1^.History.Labels[Data1^.Index, 0], Data2^.History.Labels[Data2^.Index, 0]);
    3:
      if Data1^.History.Date[Data1^.Index] < Data2^.History.Date[Data2^.Index] then
        Result := -1
      else if Data1^.History.Date[Data1^.Index] > Data2^.History.Date[Data2^.Index] then
        Result := 1;
    4:
      Result := AnsiCompareStr(Data1^.History.Author[Data1^.Index], Data2^.History.Author[Data2^.Index]);
    5:
      Result := AnsiCompareStr(Data1^.History.Comment[Data1^.Index], Data2^.History.Comment[Data2^.Index]);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnHistoryView.TreeContentChange(Sender: TBaseVirtualTree; Node: PVirtualNode);

var
  Data: PNodeData;
  Src, Dst: IStream;
  Stream: TMemoryStream;
  Read, Written: Int64;

begin
  UpdateStatusBar;
  Data := Sender.GetNodeData(Node);
  if not Assigned(Data) or not Assigned(Data^.History) then
    Exit;

  Src := Data^.History.Content[Data^.Index];
  if Assigned(Src) then
  begin
    Screen.Cursor := crHourGlass;
    Stream := TMemoryStream.Create;
    try
      Dst := TStreamAdapter.Create(Stream, soReference);
      try
        Read := 0;
        Written := 0;
        OleCheck(Src.CopyTo(Dst, High(Int64), Read, Written));
      finally
        Dst := nil;
      end;

      Stream.Seek(0, soFromBeginning);
      EditContent.Lines.LoadFromStream(Stream);
      if EditContent.Lines.Count > 9999 then
        EditContent.Gutter.DigitCount := Trunc(Log10(EditContent.Lines.Count)) + 1
      else
        EditContent.Gutter.DigitCount := 4;
    finally
      Screen.Cursor := crDefault;
      Stream.Free;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnHistoryView.TreeDiffChange(Sender: TBaseVirtualTree; Node: PVirtualNode);

var
  SelectedLeft, SelectedRight: TNodeArray;
  DataLeft, DataRight: PNodeData;
  LeftFileName, RightFileName: string;
  DiffOptions: TSvnDiffFileOptions;
  Diff: PSvnDiff;
  LinesLeft, LinesRight: TStringList;

begin
  EditDiff.Lines.BeginUpdate;
  try
    EditDiff.Clear;

    SelectedLeft := TreeDiffLeft.GetSortedSelection(True);
    SelectedRight := TreeDiffRight.GetSortedSelection(True);
    if (Length(SelectedLeft) = 0) or (Length(SelectedRight) = 0) then
      Exit;

    DataLeft := TreeDiffLeft.GetNodeData(SelectedLeft[0]);
    DataRight := TreeDiffRight.GetNodeData(SelectedRight[0]);
    if not Assigned(DataLeft) or not Assigned(DataLeft^.History) or not Assigned(DataRight) or
      not Assigned(DataRight^.History) then
      Exit;

    DiffOptions.ignore_space := svnIgnoreSpaceAll;
    DiffOptions.ignore_eol_style := True;

    LinesLeft := nil;
    LinesRight := nil;
    try
      LeftFileName := GetTempFileName('', 'SVN');
      RightFileName := GetTempFileName('', 'SVN');
      try
        if SaveStreamToFile(DataLeft^.History.Content[DataLeft^.Index], LeftFileName) and
          SaveStreamToFile(DataRight^.History.Content[DataRight^.Index], RightFileName) then
        begin
          if Assigned(@svn_diff_file_diff_2) then
            SvnCheck(svn_diff_file_diff_2(Diff, PChar(RightFileName), PChar(LeftFileName), @DiffOptions,
              SvnIDEModule.SvnClient.Pool))
          else if Assigned(@svn_diff_file_diff) then
            SvnCheck(svn_diff_file_diff(Diff, PChar(RightFileName), PChar(LeftFileName), SvnIDEModule.SvnClient.Pool));

          LinesLeft := TStringList.Create;
          LinesLeft.LoadFromFile(LeftFileName);

          LinesRight := TStringList.Create;
          LinesRight.LoadFromFile(RightFileName);
        end;
      finally
        DeleteFile(LeftFileName);
        DeleteFile(RightFileName);
      end;

      LoadDiff(LinesRight, LinesLeft, Diff);
      EditDiff.SetFocus;
      UpdateStatusBar;
    finally
      LinesLeft.Free;
      LinesRight.Free;
    end;
  finally
    EditDiff.Lines.EndUpdate;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnHistoryView.TreeDiffCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode;
  Column: TColumnIndex; var Result: Integer);

var
  Data1, Data2: PNodeData;

begin
  Result := 0;
  Data1 := Sender.GetNodeData(Node1);
  Data2 := Sender.GetNodeData(Node2);
  if not Assigned(Data1) or not Assigned(Data2) or not Assigned(Data1^.History) or not Assigned(Data2^.History) then
    Exit;

  case Column of
    1:
      Result := AnsiCompareStr(Data1^.History.Ident[Data1^.Index], Data2^.History.Ident[Data2^.Index]);
    2:
      if Data1^.History.Date[Data1^.Index] < Data2^.History.Date[Data2^.Index] then
        Result := -1
      else if Data1^.History.Date[Data1^.Index] > Data2^.History.Date[Data2^.Index] then
        Result := 1;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnHistoryView.TreeDiffGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: WideString);

var
  Data: PNodeData;
  D: TDateTime;

begin
  CellText := '';
  Data := Sender.GetNodeData(Node);
  if not Assigned(Data) or not Assigned(Data^.History) then
    Exit;

  case TextType of
    ttNormal:
      case Column of
        1:
          CellText := Data^.History.Ident[Data^.Index];
        2:
          begin
            D := Data^.History.Date[Data^.Index];
            if D <> 0 then
              CellText := DateTimeToStr(D);
          end;
      end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnHistoryView.TreeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);

var
  Data: PNodeData;

begin
  Data := Sender.GetNodeData(Node);
  if Assigned(Data) then
    Data^.History := nil;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnHistoryView.TreeGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);

var
  Data: PNodeData;

begin
  ImageIndex := -1;

  case Column of
    0:
      case Kind of
        ikNormal, ikSelected:
          begin
            Data := Sender.GetNodeData(Node);
            if not Assigned(Data) or not Assigned(Data^.History) then
              Exit;
              
            case Data^.History.HistoryStyle[Data^.Index] of
              hsBuffer:
                ImageIndex := 3;
              hsFile:
                ImageIndex := 4;
              hsLocalFile:
                ImageIndex :=  5;
              hsRemoteRevision:
                ImageIndex := 6;
              hsActiveRevision:
                ImageIndex := 7;
            end;
          end;
      end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnHistoryView.TreeGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);

begin
  NodeDataSize := SizeOf(TNodeData);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnHistoryView.TreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: WideString);

var
  Data: PNodeData;
  D: TDateTime;

begin
  CellText := '';
  Data := Sender.GetNodeData(Node);
  if not Assigned(Data) or not Assigned(Data^.History) then
    Exit;

  case TextType of
    ttNormal:
      case Column of
        1:
          CellText := Data^.History.Ident[Data^.Index];
        2:
          CellText := Data^.History.Labels[Data^.Index, 0];
        3:
          begin
            D := Data^.History.Date[Data^.Index];
            if D <> 0 then
              CellText := DateTimeToStr(D);
          end;
        4:
          CellText := Data^.History.Author[Data^.Index];
        5:
          CellText := Data^.History.Comment[Data^.Index];
      end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnHistoryView.TreeHeaderClick(Sender: TVTHeader; Column: TColumnIndex; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
  
begin
  if (Button = mbLeft) and (Shift = []) then
  begin
    if Column = Sender.SortColumn then
      case Sender.SortDirection of
        sdAscending:
          Sender.SortDirection := sdDescending;
        sdDescending:
          Sender.SortDirection := sdAscending;
      end
    else
    begin
      Sender.SortColumn := Column;
      Sender.SortDirection := sdAscending;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnHistoryView.TreeInfoChange(Sender: TBaseVirtualTree; Node: PVirtualNode);

var
  SelectedNodes: TNodeArray;
  Data: PNodeData;
  I: Integer;
  S: string;

begin
  UpdateStatusBar;

  ListBoxLabels.Items.BeginUpdate;
  try
    ListBoxLabels.Clear;

    MemoComments.Lines.BeginUpdate;
    try
      MemoComments.Clear;

      SelectedNodes := TreeInfo.GetSortedSelection(True);
      if Length(SelectedNodes) = 0 then
        Exit;

      Data := TreeInfo.GetNodeData(SelectedNodes[0]);
      if not Assigned(Data) or not Assigned(Data^.History) then
        Exit;

      S := Data^.History.Comment[Data^.Index];
      I := Pos(#10, S);
      while I <> 0 do
      begin
        if (I = 1) or (S[I - 1] <> #13) then
        begin
          Insert(#13, S, I);
          Inc(I);
        end;

        I := PosEx(#10, S, I + 1);
      end;

      MemoComments.Text := S;
    finally
      MemoComments.Lines.EndUpdate;
    end;

    for I := 0 to Data^.History.LabelCount[Data^.Index] - 1 do
      ListBoxLabels.Items.Add(Data^.History.Labels[Data^.Index, I]);
  finally
    ListBoxLabels.Items.EndUpdate;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnHistoryView.UpdateStatusBar;

var
  SelectedNodes: TNodeArray;
  Data1, Data2: PNodeData;
  I, DiffCount: Integer;

begin
  StatusBar.Panels[0].Text := '';
  StatusBar.Panels[1].Text := '';

  case PageControl.ActivePageIndex of
    0:
      begin
        SelectedNodes := TreeContent.GetSortedSelection(True);
        if Length(SelectedNodes) = 0 then
          Exit;
        Data1 := TreeContent.GetNodeData(SelectedNodes[0]);
        if not Assigned(Data1) then
          Exit;
        StatusBar.Panels[0].Text := FormatDateTime(LongDateFormat + ', ' + LongTimeFormat,
          Data1^.History.Date[Data1^.Index]);
        StatusBar.Panels[1].Text := ExtractFileName(FFileName);
        if StatusBar.Panels.Count = 3 then
          StatusBar.Panels.Delete(2);
      end;
    1:
      begin
        SelectedNodes := TreeInfo.GetSortedSelection(True);
        if Length(SelectedNodes) = 0 then
          Exit;
        Data1 := TreeInfo.GetNodeData(SelectedNodes[0]);
        if not Assigned(Data1) then
          Exit;
        StatusBar.Panels[0].Text := FormatDateTime(LongDateFormat + ', ' + LongTimeFormat,
          Data1^.History.Date[Data1^.Index]);
        StatusBar.Panels[1].Text := ExtractFileName(FFileName);
        if StatusBar.Panels.Count = 3 then
          StatusBar.Panels.Delete(2);
      end;
    2:
      begin
        DiffCount := GetDiffCount;
        if DiffCount = 0 then
          StatusBar.Panels[0].Text := 'No differences found'
        else
          StatusBar.Panels[0].Text := Format('%d differences found', [DiffCount]);
        SelectedNodes := TreeDiffLeft.GetSortedSelection(True);
        if Length(SelectedNodes) = 0 then
          Data1 := nil
        else
          Data1 := TreeDiffLeft.GetNodeData(SelectedNodes[0]);
        SelectedNodes := TreeDiffRight.GetSortedSelection(True);
        if Length(SelectedNodes) = 0 then
          Data2 := nil
        else
          Data2 := TreeDiffRight.GetNodeData(SelectedNodes[0]);
        if Assigned(Data1) and Assigned(Data1^.History) and Assigned(Data2) and Assigned(Data2^.History) then
          StatusBar.Panels[1].Text := Format('Diff from %s to %s', [Data1^.History.Ident[Data1^.Index],
            Data2^.History.Ident[Data2^.Index]])
        else
          StatusBar.Panels[1].Text := '';
        if StatusBar.Panels.Count = 2 then
          StatusBar.Panels.Add;
        StatusBar.Panels[2].Text := ExtractFileName(FFileName);
      end;
  end;

  for I := 0 to StatusBar.Panels.Count - 2 do
    StatusBar.Panels[I].Width := StatusBar.Canvas.TextWidth(StatusBar.Panels[I].Text) + 12;
end;

//----------------------------------------------------------------------------------------------------------------------

{ TFrameHistory public }

//----------------------------------------------------------------------------------------------------------------------

constructor TFrameSvnHistoryView.Create(AOwner: TComponent);

var
  Column: TVirtualTreeColumn;
  Section: THeaderSection;

begin
  inherited Create(AOwner);

  FHighlighter := nil;

  // Content tab
  LabelContent := TLabel.Create(Self);
  LabelContent.Parent := TabSheet1;
  LabelContent.Caption := 'Revision content';
  LabelContent.Align := alTop;

  TreeContent := TVirtualStringTree.Create(Self);
  TreeContent.Parent := TabSheet1;
  TreeContent.Height := 240;
  TreeContent.Align := alTop;
  TreeContent.Images := ImageList;
  TreeContent.TreeOptions.SelectionOptions := [toFullRowSelect];
  TreeContent.TreeOptions.AutoOptions := [toAutoSort];
  TreeContent.TreeOptions.PaintOptions := [toHideFocusRect, toShowHorzGridLines, toShowVertGridLines];
  TreeContent.Header.Options := [hoShowSortGlyphs, hoVisible, hoColumnResize];

  Column := TreeContent.Header.Columns.Add;
  Column.Width := 26;
  Column.MinWidth := 26;
  Column.MaxWidth := 26;
  Column.Options := [coFixed, coVisible];

  Column := TreeContent.Header.Columns.Add;
  Column.Width := 150;
  Column.Text := 'Rev.';
  Column.Options := [coAllowClick, coEnabled, coResizable, coVisible];

  Column := TreeContent.Header.Columns.Add;
  Column.Width := 150;
  Column.Text := 'Label';
  Column.Options := [coAllowClick, coEnabled, coResizable, coVisible];

  Column := TreeContent.Header.Columns.Add;
  Column.Width := 190;
  Column.Text := 'Date';
  Column.Options := [coAllowClick, coEnabled, coResizable, coVisible];

  Column := TreeContent.Header.Columns.Add;
  Column.Width := 190;
  Column.Text := 'Author';
  Column.Options := [coAllowClick, coEnabled, coResizable, coVisible];

  TreeContent.OnGetNodeDataSize := TreeGetNodeDataSize;
  TreeContent.OnFreeNode := TreeFreeNode;
  TreeContent.OnGetText := TreeGetText;
  TreeContent.OnCompareNodes := TreeCompareNodes;
  TreeContent.OnGetImageIndex := TreeGetImageIndex;
  TreeContent.OnChange := TreeContentChange;

  TreeContent.Header.SortColumn := 3;
  TreeContent.Header.SortDirection := sdDescending;

  TreeContent.OnHeaderClick := TreeHeaderClick;

  Splitter1 := TSplitter.Create(Self);
  Splitter1.Parent := TabSheet1;
  Splitter1.Top := TreeContent.Height + 1;
  Splitter1.Align := alTop;

  EditContent := TSynEdit.Create(Self);
  EditContent.Parent := TabSheet1;
  EditContent.Align := alClient;
  EditContent.ReadOnly := True;
  EditContent.Gutter.ShowLineNumbers := True;

  // Info tab
  LabelInfo := TLabel.Create(Self);
  LabelInfo.Parent := TabSheet2;
  LabelInfo.Caption := 'Revision info';
  LabelInfo.Align := alTop;

  TreeInfo := TVirtualStringTree.Create(Self);
  TreeInfo.Parent := TabSheet2;
  TreeInfo.Height := 240;
  TreeInfo.Align := alTop;
  TreeInfo.Images := ImageList;
  TreeInfo.TreeOptions.SelectionOptions := [toFullRowSelect];
  TreeInfo.TreeOptions.AutoOptions := [toAutoSort];
  TreeInfo.TreeOptions.PaintOptions := [toHideFocusRect, toShowHorzGridLines, toShowVertGridLines];
  TreeInfo.Header.Options := [hoShowSortGlyphs, hoVisible, hoColumnResize];

  Column := TreeInfo.Header.Columns.Add;
  Column.Width := 26;
  Column.MinWidth := 26;
  Column.MaxWidth := 26;
  Column.Options := [coFixed, coVisible];

  Column := TreeInfo.Header.Columns.Add;
  Column.Width := 150;
  Column.Text := 'Rev.';
  Column.Options := [coAllowClick, coEnabled, coResizable, coVisible];

  Column := TreeInfo.Header.Columns.Add;
  Column.Width := 150;
  Column.Text := 'Label';
  Column.Options := [coAllowClick, coEnabled, coResizable, coVisible];

  Column := TreeInfo.Header.Columns.Add;
  Column.Width := 190;
  Column.Text := 'Date';
  Column.Options := [coAllowClick, coEnabled, coResizable, coVisible];

  Column := TreeInfo.Header.Columns.Add;
  Column.Width := 190;
  Column.Text := 'Author';
  Column.Options := [coAllowClick, coEnabled, coResizable, coVisible];

  Column := TreeInfo.Header.Columns.Add;
  Column.Width := 190;
  Column.Text := 'Comment';
  Column.Options := [coAllowClick, coEnabled, coResizable, coVisible];

  TreeInfo.OnGetNodeDataSize := TreeGetNodeDataSize;
  TreeInfo.OnFreeNode := TreeFreeNode;
  TreeInfo.OnGetText := TreeGetText;
  TreeInfo.OnCompareNodes := TreeCompareNodes;
  TreeInfo.OnGetImageIndex := TreeGetImageIndex;
  TreeInfo.OnChange := TreeInfoChange;

  TreeInfo.Header.SortColumn := 3;
  TreeInfo.Header.SortDirection := sdDescending;

  TreeInfo.OnHeaderClick := TreeHeaderClick;

  Splitter2 := TSplitter.Create(Self);
  Splitter2.Parent := TabSheet2;
  Splitter2.Top := TreeInfo.Height + 1;
  Splitter2.Align := alTop;

  HeaderControl := THeaderControl.Create(Self);
  HeaderControl.Parent := TabSheet2;
  HeaderControl.Top := Splitter2.Top + Splitter2.Height + 1;
  HeaderControl.Align := alTop;
  HeaderControl.OnSectionResize := HeaderControlSectionResize;

  Section := HeaderControl.Sections.Add;
  Section.Style := hsText;
  Section.Text := 'Label';
  Section.Width := 190;

  Section := HeaderControl.Sections.Add;
  Section.Style := hsText;
  Section.Text := 'Comments';
  Section.Width := 10000;

  ListBoxLabels := TListBox.Create(Self);
  ListBoxLabels.Parent := TabSheet2;
  ListBoxLabels.Width := 190;
  ListBoxLabels.Align := alLeft;

  Splitter3 := TSplitter.Create(Self);
  Splitter3.Parent := TabSheet2;
  Splitter3.Left := ListBoxLabels.Width + 1;
  Splitter3.Align := alLeft;
  Splitter3.ResizeStyle := rsUpdate;
  Splitter3.OnMoved := Splitter3Moved;
  Splitter3Moved(Splitter3);

  MemoComments := TMemo.Create(Self);
  MemoComments.Parent := TabSheet2;
  MemoComments.Left := Splitter3.Left + Splitter3.Width + 1;
  MemoComments.Align := alClient;

  // Diff tab
  PanelTrees := TPanel.Create(Self);
  PanelTrees.Parent := TabSheet3;
  PanelTrees.BevelOuter := bvNone;
  PanelTrees.Caption := '';
  PanelTrees.Height := 240;
  PanelTrees.Align := alTop;

  PanelTreeLeft := TPanel.Create(Self);
  PanelTreeLeft.Parent := PanelTrees;
  PanelTreeLeft.BevelOuter := bvNone;
  PanelTreeLeft.Caption := '';
  PanelTreeLeft.Width := (Width - 3) div 2;
  PanelTreeLeft.Align := alLeft;

  Splitter4 := TSplitter.Create(Self);
  Splitter4.Parent := PanelTrees;
  Splitter4.Left := PanelTreeLeft.Width + 1;
  Splitter4.Align := alLeft;

  PanelTreeRight := TPanel.Create(Self);
  PanelTreeRight.Parent := PanelTrees;
  PanelTreeRight.BevelOuter := bvNone;
  PanelTreeRight.Caption := '';
  PanelTreeRight.Left := Splitter4.Left + Splitter4.Width + 1;
  PanelTreeRight.Align := alClient;

  LabelDiffLeft := TLabel.Create(Self);
  LabelDiffLeft.Parent := PanelTreeLeft;
  LabelDiffLeft.Caption := 'Differences From:';
  LabelDiffLeft.Align := alTop;

  LabelDiffRight := TLabel.Create(Self);
  LabelDiffRight.Parent := PanelTreeRight;
  LabelDiffRight.Caption := 'To:';
  LabelDiffRight.Align := alTop;

  TreeDiffLeft := TVirtualStringTree.Create(Self);
  TreeDiffLeft.Parent := PanelTreeLeft;
  TreeDiffLeft.Height := 240;
  TreeDiffLeft.Top := LabelDiffLeft.Height + 1;
  TreeDiffLeft.Align := alClient;
  TreeDiffLeft.Images := ImageList;
  TreeDiffLeft.TreeOptions.SelectionOptions := [toFullRowSelect];
  TreeDiffLeft.TreeOptions.AutoOptions := [toAutoSort];
  TreeDiffLeft.TreeOptions.PaintOptions := [toHideFocusRect, toShowHorzGridLines, toShowVertGridLines];
  TreeDiffLeft.Header.Options := [hoShowSortGlyphs, hoVisible, hoColumnResize];

  Column := TreeDiffLeft.Header.Columns.Add;
  Column.Width := 26;
  Column.MinWidth := 26;
  Column.MaxWidth := 26;
  Column.Options := [coFixed, coVisible];

  Column := TreeDiffLeft.Header.Columns.Add;
  Column.Width := 150;
  Column.Text := 'Rev.';
  Column.Options := [coAllowClick, coEnabled, coResizable, coVisible];

  Column := TreeDiffLeft.Header.Columns.Add;
  Column.Width := 190;
  Column.Text := 'Date';
  Column.Options := [coAllowClick, coEnabled, coResizable, coVisible];

  TreeDiffLeft.OnGetNodeDataSize := TreeGetNodeDataSize;
  TreeDiffLeft.OnFreeNode := TreeFreeNode;
  TreeDiffLeft.OnGetText := TreeDiffGetText;
  TreeDiffLeft.OnCompareNodes := TreeDiffCompareNodes;
  TreeDiffLeft.OnGetImageIndex := TreeGetImageIndex;
  TreeDiffLeft.OnChange := TreeDiffChange;

  TreeDiffLeft.Header.SortColumn := 3;
  TreeDiffLeft.Header.SortDirection := sdDescending;

  TreeDiffLeft.OnHeaderClick := TreeHeaderClick;

  TreeDiffRight := TVirtualStringTree.Create(Self);
  TreeDiffRight.Parent := PanelTreeRight;
  TreeDiffRight.Height := 240;
  TreeDiffRight.Top := LabelDiffRight.Height + 1;
  TreeDiffRight.Align := alClient;
  TreeDiffRight.Images := ImageList;
  TreeDiffRight.TreeOptions.SelectionOptions := [toFullRowSelect];
  TreeDiffRight.TreeOptions.AutoOptions := [toAutoSort];
  TreeDiffRight.TreeOptions.PaintOptions := [toHideFocusRect, toShowHorzGridLines, toShowVertGridLines];
  TreeDiffRight.Header.Options := [hoShowSortGlyphs, hoVisible, hoColumnResize];

  Column := TreeDiffRight.Header.Columns.Add;
  Column.Width := 26;
  Column.MinWidth := 26;
  Column.MaxWidth := 26;
  Column.Options := [coFixed, coVisible];

  Column := TreeDiffRight.Header.Columns.Add;
  Column.Width := 150;
  Column.Text := 'Rev.';
  Column.Options := [coAllowClick, coEnabled, coResizable, coVisible];

  Column := TreeDiffRight.Header.Columns.Add;
  Column.Width := 190;
  Column.Text := 'Date';
  Column.Options := [coAllowClick, coEnabled, coResizable, coVisible];

  TreeDiffRight.OnGetNodeDataSize := TreeGetNodeDataSize;
  TreeDiffRight.OnFreeNode := TreeFreeNode;
  TreeDiffRight.OnGetText := TreeDiffGetText;
  TreeDiffRight.OnCompareNodes := TreeDiffCompareNodes;
  TreeDiffRight.OnGetImageIndex := TreeGetImageIndex;
  TreeDiffRight.OnChange := TreeDiffChange;

  TreeDiffRight.Header.SortColumn := 3;
  TreeDiffRight.Header.SortDirection := sdDescending;

  TreeDiffRight.OnHeaderClick := TreeHeaderClick;

  Splitter5 := TSplitter.Create(Self);
  Splitter5.Parent := TabSheet3;
  Splitter5.Top := PanelTrees.Height + 1;
  Splitter5.Align := alTop;

  EditDiff := TSynEdit.Create(Self);
  EditDiff.Parent := TabSheet3;
  EditDiff.Top := Splitter5.Top + Splitter5.Height + 1;
  EditDiff.Align := alClient;
  EditDiff.ReadOnly := True;
  EditDiff.Gutter.ShowLineNumbers := False;
  EditDiff.OnGutterPaint := EditDiffGutterPaint;
  EditDiff.OnSpecialLineColors := EditDiffSpecialLineColors;
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TFrameSvnHistoryView.Destroy;

begin
  TreeContent.Clear;
  TreeInfo.Clear;
  TreeDiffLeft.Clear;
  TreeDiffRight.Clear;
  ClearLineInfos;
  inherited Destroy;
end;

//----------------------------------------------------------------------------------------------------------------------
// when opening a multi-file module (e.g. .pas with .dfm), Display may be called twice, first with count 1 then 2 

procedure TFrameSvnHistoryView.Display(const Module: IOTAModule);

var
  I: Integer;

begin
  if AnsiSameText(FFileName, Module.FileName) and (FFileCount = Module.ModuleFileCount) then
    Exit;

  FFileName := Module.FileName;
  FFileCount := Module.ModuleFileCount;
  ComboBoxFileSelector.Visible := FFileCount > 1;
  ComboBoxFileSelector.Items.BeginUpdate;
  try
    ComboBoxFileSelector.Clear;
    for I := 0 to Module.ModuleFileCount - 1 do
      ComboBoxFileSelector.Items.Add(ExtractFileName(Module.ModuleFileEditors[I].FileName));

    ComboBoxFileSelector.ItemIndex := 0;
    ShowFile(0);
  finally
    ComboBoxFileSelector.Items.EndUpdate;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

{ TFrameHistory event handlers }

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnHistoryView.ActionRefreshExecute(Sender: TObject);

var
  ModuleServices: IOTAModuleServices;
  Module: IOTAModule;

begin
  if not Supports(BorlandIDEServices, IOTAModuleServices, ModuleServices) then
    raise Exception.Create('Error obtaining IOTAModuleServices');
  Module := ModuleServices.FindModule(FFileName);
  if not Assigned(Module) then
    raise Exception.CreateFmt('Error obtaining IOTAModule for %s', [FFileName]);
    
  FFileCount := 0;
  FFileName := '';
  Display(Module);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnHistoryView.ActionRevertExecute(Sender: TObject);

var
  Tree: TVirtualStringTree;
  SelectedNodes: TNodeArray;
  Data: PNodeData;
  FileName: string;
  ActionServices: IOTAActionServices;

begin
  case PageControl.ActivePageIndex of
    0:
      Tree := TreeContent;
    1:
      Tree := TreeInfo;
    else
      Exit;
  end;

  SelectedNodes := Tree.GetSortedSelection(True);
  if Length(SelectedNodes) = 0 then
    Exit;
  if not Supports(BorlandIDEServices, IOTAActionServices, ActionServices) then
    Exit;

  Data := Tree.GetNodeData(SelectedNodes[0]);
  if Assigned(Data) and Assigned(Data^.History) and
    (MessageDlg(Format('Reverting to %s will result in permanently losing the contents of the editor buffer.'#13#10#13#10 +
    'This revert will not be applied to your remote repository.'#13#10#13#10 +
    'Would you like to continue with the revert operation?', [Data^.History.Ident[Data^.Index]]), mtConfirmation,
    [mbYes, mbNo], 0) = mrYes) then
  begin
    FileName := ExtractFilePath(FFileName) + ComboBoxFileSelector.Items[ComboBoxFileSelector.ItemIndex];
    if not SaveStreamToFile(Data^.History.Content[Data^.Index], FileName) then
      raise Exception.CreateFmt('Error reverting to %s', [Data^.History.Ident[Data^.Index]]);

    ActionServices.ReloadFile(FileName);
    ShowFile(ComboBoxFileSelector.ItemIndex);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnHistoryView.ActionRevertUpdate(Sender: TObject);

var
  Tree: TVirtualStringTree;
  SelectedNodes: TNodeArray;
  Data: PNodeData;

begin
  Data := nil;
  case PageControl.ActivePageIndex of
    0:
      Tree := TreeContent;
    1:
      Tree := TreeInfo;
    else
      Tree := nil;
  end;

  if Assigned(Tree) then
  begin
    SelectedNodes := Tree.GetSortedSelection(True);
    if Length(SelectedNodes) > 0 then
      Data := Tree.GetNodeData(SelectedNodes[0]);
  end;

  TAction(Sender).Enabled :=  Assigned(Data) and Assigned(Data^.History);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnHistoryView.ActionDiffNextExecute(Sender: TObject);

var
  NewBlock: Boolean;
  CurPos, NextPos, I: Integer;
  CurKind: TDiffKind;

begin
  NewBlock := False;
  NextPos := -1;
  CurPos := EditDiff.CaretY - 1;
  CurKind := PLineInfo(EditDiff.Lines.Objects[CurPos])^.Kind;
  for I := CurPos to EditDiff.Lines.Count - 1 do
    with PLineInfo(EditDiff.Lines.Objects[I])^ do
    begin
      if not NewBlock then
        NewBlock := Kind <> CurKind;
      if NewBlock and (Kind <> dkCommon) then
      begin
        NextPos := I;
        Break;
      end;
    end;

  if NextPos = -1 then
    NextPos := EditDiff.Lines.Count - 1;

  EditDiff.CaretX := 1;
  EditDiff.CaretY := NextPos + 1;
  EditDiff.TopLine := NextPos;
  EditDiff.SetFocus;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnHistoryView.ActionDiffNextUpdate(Sender: TObject);

begin
  (Sender as TAction).Enabled := (PageControl.ActivePageIndex = 2) and (TreeDiffLeft.SelectedCount <> 0) and
    (TreeDiffRight.SelectedCount <> 0);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnHistoryView.ActionDiffPrevExecute(Sender: TObject);

var
  NewBlock: Boolean;
  CurPos, NextPos, I: Integer;
  CurKind: TDiffKind;

begin
  NewBlock := False;
  NextPos := -1;
  CurPos := EditDiff.CaretY - 1;
  CurKind := PLineInfo(EditDiff.Lines.Objects[CurPos])^.Kind;
  for I := CurPos - 1 downto 0 do
    with PLineInfo(EditDiff.Lines.Objects[I])^ do
    begin
      if not NewBlock then
        NewBlock := Kind <> CurKind;
      if NewBlock and (Kind <> dkCommon) then
      begin
        NextPos := I;
        Break;
      end;
    end;

  if NextPos = -1 then
    NextPos := 0
  else
  begin
    CurKind := PLineInfo(EditDiff.Lines.Objects[NextPos])^.Kind;
    while (NextPos > 0) and (PLineInfo(EditDiff.Lines.Objects[NextPos - 1])^.Kind = CurKind) do
      Dec(NextPos);
  end;

  EditDiff.CaretX := 1;
  EditDiff.CaretY := NextPos + 1;
  EditDiff.TopLine := NextPos;
  EditDiff.SetFocus;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnHistoryView.ActionDiffPrevUpdate(Sender: TObject);

begin
  (Sender as TAction).Enabled := (PageControl.ActivePageIndex = 2) and (TreeDiffLeft.SelectedCount <> 0) and
    (TreeDiffRight.SelectedCount <> 0);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnHistoryView.ComboBoxFileSelectorClick(Sender: TObject);

begin
  ShowFile((Sender as TComboBox).ItemIndex);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnHistoryView.PageControlChange(Sender: TObject);

var
  Node: PVirtualNode;

begin
  case TPageControl(Sender).ActivePageIndex of
    0:
      if TreeContent.SelectedCount = 0 then
      begin
        Node := TreeContent.GetFirst;
        TreeContent.Selected[Node] := True;
      end
      else
        UpdateStatusBar;
    1:
      if TreeInfo.SelectedCount = 0 then
      begin
        Node := TreeInfo.GetFirst;
        TreeInfo.Selected[Node] := True;
      end
      else
        UpdateStatusBar;
    2:
      UpdateStatusBar;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

end.
