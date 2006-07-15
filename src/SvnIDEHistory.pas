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
{ The Original Code is SvnIDEHistory.pas.                                                                              }
{                                                                                                                      }
{ The Initial Developer of the Original Code is Ondrej Kelle.                                                          }
{ Portions created by Ondrej Kelle are Copyright Ondrej Kelle. All rights reserved.                                    }
{                                                                                                                      }
{ Contributors:                                                                                                        }
{   Ondrej Kelle (tondrej)                                                                                             }
{                                                                                                                      }
{**********************************************************************************************************************}
{                                                                                                                      }
{ This unit contains Subversion history provider, implementing OpenTools FileHistory API interfaces.                   }
{                                                                                                                      }
{**********************************************************************************************************************}

unit SvnIDEHistory;

interface

uses
  Classes, SysUtils, Types, ActiveX, ComObj, Controls,
  ToolsAPI, FileHistoryAPI,
  SvnClient;

type
  TDispInterfacedObject = class(TInterfacedObject, IDispatch)
  private
    { IDispatch }
    function GetTypeInfoCount(out Count: Integer): HResult; stdcall;
    function GetTypeInfo(Index, LocaleID: Integer; out TypeInfo): HResult; stdcall;
    function GetIDsOfNames(const IID: TGUID; Names: Pointer;
      NameCount, LocaleID: Integer; DispIDs: Pointer): HResult; stdcall;
    function Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer;
      Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer): HResult; stdcall;
  end;

  TSvnFileHistoryProvider = class(TDispInterfacedObject, IOTAFileHistoryProvider)
  private
    FClient: TSvnClient;
    FItems: TStringList;

    procedure ClearItems;

    { IOTAFileHistoryProvider }
    function Get_Ident: WideString; safecall;
    function Get_Name: WideString; safecall;
    function GetFileHistory(const AFileName: WideString): IOTAFileHistory; safecall;
  public
    constructor Create;
    destructor Destroy; override;

    function SafeCallException(ExceptObject: TObject; ExceptAddr: Pointer): HResult; override;
  end;

  ISvnFileHistory = interface
    ['{BA13BDFA-7E77-409E-9FEB-E282BD0F809D}']
    function GetItem: TSvnItem; safecall;
    property Item: TSvnItem read GetItem;
  end;

implementation

uses
  apr, svn_client,
  SvnIDEClient;

type
  TSvnFileHistory = class(TDispInterfacedObject, IOTAFileHistory, ISvnFileHistory)
  private
    FItem: TSvnItem;
    FItems: TList;

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

    { ISvnFileHistory }
    function GetItem: TSvnItem; safecall;
  public
    constructor Create(AItem: TSvnItem);
    destructor Destroy; override;

    function SafeCallException(ExceptObject: TObject; ExceptAddr: Pointer): HResult; override;
  end;

//----------------------------------------------------------------------------------------------------------------------

{ TDispInterfacedObject private: IDispatch }

//----------------------------------------------------------------------------------------------------------------------

function TDispInterfacedObject.GetIDsOfNames(const IID: TGUID; Names: Pointer; NameCount, LocaleID: Integer;
  DispIDs: Pointer): HResult;

begin
  Result := E_NOTIMPL;
end;

//----------------------------------------------------------------------------------------------------------------------

function TDispInterfacedObject.GetTypeInfo(Index, LocaleID: Integer; out TypeInfo): HResult;

begin
  Result := E_NOTIMPL;
end;

//----------------------------------------------------------------------------------------------------------------------

function TDispInterfacedObject.GetTypeInfoCount(out Count: Integer): HResult;

begin
  Result := S_OK;
  Count := 0;
end;

//----------------------------------------------------------------------------------------------------------------------

function TDispInterfacedObject.Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer; Flags: Word; var Params;
  VarResult, ExcepInfo, ArgErr: Pointer): HResult;
  
begin
  Result := E_NOTIMPL;
end;

//----------------------------------------------------------------------------------------------------------------------

{ TSvnFileHistory private: IOTAFileHistory }

//----------------------------------------------------------------------------------------------------------------------

function TSvnFileHistory.Get_Count: Integer;

begin
  Result := FItems.Count;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnFileHistory.GetAuthor(Index: Integer): WideString;

begin
  Result := TSvnHistoryItem(FItems[Index]).Author;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnFileHistory.GetComment(Index: Integer): WideString;

begin
  Result := TSvnHistoryItem(FItems[Index]).LogMessage;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnFileHistory.GetContent(Index: Integer): IStream;

begin
  Result := TStreamAdapter.Create(TStringStream.Create(TSvnHistoryItem(FItems[Index]).GetFile), soOwned);
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnFileHistory.GetDate(Index: Integer): TDateTime;

begin
  Result := TzToUTCDateTime(TSvnHistoryItem(FItems[Index]).Time);
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnFileHistory.GetHistoryStyle(Index: Integer): TOTAHistoryStyle;

var
  Item: TSvnHistoryItem;

begin
  Item := FItems[Index];
  if Item.Revision = Item.Owner.CommittedRevision then
    Result := hsActiveRevision
  else
    Result := hsRemoteRevision;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnFileHistory.GetIdent(Index: Integer): WideString;

begin
  Result := IntToStr(TSvnHistoryItem(FItems[Index]).Revision);
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnFileHistory.GetLabelCount(Index: Integer): Integer;

begin
  Result := 1;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnFileHistory.GetLabels(Index, LabelIndex: Integer): WideString;

begin
  case LabelIndex of
    0:
      Result := GetIdent(Index);
    else
      Result := '';
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

{ TSvnFileHistory private: ISvnFileHistory }

//----------------------------------------------------------------------------------------------------------------------

function TSvnFileHistory.GetItem: TSvnItem;

begin
  Result := FItem;
end;

//----------------------------------------------------------------------------------------------------------------------

{ TSvnFileHistory public }

//----------------------------------------------------------------------------------------------------------------------

constructor TSvnFileHistory.Create(AItem: TSvnItem);

var
  I: Integer;

begin
  inherited Create;
  FItems := TList.Create;
  FItem := AItem;
  if Assigned(FItem) then
    for I := 0 to FItem.HistoryCount - 1 do
      FItems.Add(FItem.HistoryItems[I]);
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TSvnFileHistory.Destroy;

begin
  FItems.Free;
  inherited Destroy;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnFileHistory.SafeCallException(ExceptObject: TObject; ExceptAddr: Pointer): HResult;

begin
  Result := HandleSafeCallException(ExceptObject, ExceptAddr, IOTAFileHistory, '', '');
end;

//----------------------------------------------------------------------------------------------------------------------

{ TSvnFileHistoryProvider private }

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnFileHistoryProvider.ClearItems;

var
  I: Integer;

begin
  for I := 0 to FItems.Count - 1 do
    FItems.Objects[I].Free;
  FItems.Clear;
end;

//----------------------------------------------------------------------------------------------------------------------

{ TSvnFileHistoryProvider private: IOTAFileHistoryProvider }

//----------------------------------------------------------------------------------------------------------------------

function TSvnFileHistoryProvider.GetFileHistory(const AFileName: WideString): IOTAFileHistory;

var
  Index: Integer;
  Item: TSvnItem;

begin
  Result := nil;

  if not FClient.IsPathVersioned(AFileName) then
    Exit;

  if FItems.Find(AFileName, Index) then
    Item := TSvnItem(FItems.Objects[Index])
  else
  begin
    Item := TSvnItem.Create(FClient, nil, AFileName);
    try
      FItems.AddObject(Item.PathName, Item);
    except
      Item.Free;
      raise;
    end;
  end;

  SvnIDEModule.SetupBlamePanel;

  Result := TSvnFileHistory.Create(Item);
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnFileHistoryProvider.Get_Ident: WideString;

begin
  Result := 'TOndrej.SubversionFileHistoryProvider';
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnFileHistoryProvider.Get_Name: WideString;

begin
  Result := 'Subversion history provider';
end;

//----------------------------------------------------------------------------------------------------------------------

{ TSvnFileHistoryProvider public }

//----------------------------------------------------------------------------------------------------------------------

constructor TSvnFileHistoryProvider.Create;

begin
  inherited Create;
  FClient := SvnIDEModule.SvnClient;
  FItems := TStringList.Create;
  FItems.CaseSensitive := False;
  FItems.Duplicates := dupError;
  FItems.Sorted := True;
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TSvnFileHistoryProvider.Destroy;

begin
  ClearItems;
  FItems.Free;
  FClient := nil;
  inherited Destroy;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnFileHistoryProvider.SafeCallException(ExceptObject: TObject; ExceptAddr: Pointer): HResult;

begin
  Result := HandleSafeCallException(ExceptObject, ExceptAddr, IOTAFileHistoryProvider, '', '');
end;

//----------------------------------------------------------------------------------------------------------------------

end.
