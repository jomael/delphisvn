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
{ This unit contains the history manager.                                                                              }
{                                                                                                                      }
{**********************************************************************************************************************}

unit SvnHistoryManager;

interface

uses
  Classes, SysUtils, SvnIDEHistory;

type
  TFileHistoryManager = class(TInterfacedObject, IOTAFileHistoryManager)
  private
    FNotifiers: TInterfaceList;
    FProviders: TInterfaceList;

    { IOTAFileHistoryManager }
    function AddNotifier(const ANotifier: IOTAFileHistoryNotifier): Integer;
    procedure AddTemporaryLabel(const ALabelName: WideString; const AFiles: TOTAFileNameArray);
    function Get_Count: Integer;
    function GetFileHistoryProvider(Index: Integer): IOTAFileHistoryProvider;
    function RegisterHistoryProvider(const HistoryProvider: IOTAFileHistoryProvider): Integer;
    procedure RemoveNotifier(Index: Integer);
    procedure RevertTemporaryLabel(const ALabelName: WideString);
    procedure UnregisterHistoryProvider(Index: Integer);
    procedure UpdateProviders;
  public
    constructor Create;
    destructor Destroy; override;
  end;

var
  FileHistoryManager: IOTAFileHistoryManager = nil;

implementation

uses
  RTLConsts;

//----------------------------------------------------------------------------------------------------------------------

{ TFileHistoryManager private: IOTAFileHistoryManager }

//----------------------------------------------------------------------------------------------------------------------

function TFileHistoryManager.AddNotifier(const ANotifier: IOTAFileHistoryNotifier): Integer;

var
  I: Integer;

begin
  for I := 0 to FNotifiers.Count - 1 do
    if not Assigned(FNotifiers[I]) then
    begin
      FNotifiers[I] := ANotifier;
      Result := I;
      Exit;
    end;

  Result := FNotifiers.Add(ANotifier);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFileHistoryManager.AddTemporaryLabel(const ALabelName: WideString; const AFiles: TOTAFileNameArray);

begin

end;

//----------------------------------------------------------------------------------------------------------------------

function TFileHistoryManager.Get_Count: Integer;

var
  I: Integer;

begin
  Result := 0;

  for I := 0 to FProviders.Count - 1 do
    if Assigned(FProviders[I]) then
      Inc(Result);
end;

//----------------------------------------------------------------------------------------------------------------------

function TFileHistoryManager.GetFileHistoryProvider(Index: Integer): IOTAFileHistoryProvider;

var
  I, TmpIndex: Integer;

begin
  Result := nil;
  if (Index < 0) or (Index > Get_Count - 1) then
    TList.Error(SListIndexError, Index);

  TmpIndex := -1;
  for I := 0 to FProviders.Count - 1 do
    if Assigned(FProviders[I]) then
    begin
      Inc(TmpIndex);
      if (TmpIndex = Index) then
      begin
        Result := FProviders[I] as IOTAFileHistoryProvider;
        Break;
      end;
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TFileHistoryManager.RegisterHistoryProvider(const HistoryProvider: IOTAFileHistoryProvider): Integer;

var
  I: Integer;

begin
  Result := -1;

  for I := 0 to FProviders.Count - 1 do
    if not Assigned(FProviders[I]) then
    begin
      FProviders[I] := HistoryProvider;
      Result := I;
      Break;
    end;
  if Result = -1 then
    Result := FProviders.Add(HistoryProvider);

  for I := 0 to FNotifiers.Count - 1 do
    if Assigned(FNotifiers[I]) then
      (FNotifiers[I] as IOTAFileHistoryNotifier).ProvidersUpdated;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFileHistoryManager.RemoveNotifier(Index: Integer);

begin
  FNotifiers[Index] := nil;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFileHistoryManager.RevertTemporaryLabel(const ALabelName: WideString);

begin

end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFileHistoryManager.UnregisterHistoryProvider(Index: Integer);

var
  I: Integer;

begin
  FProviders[Index] := nil;

  for I := 0 to FNotifiers.Count - 1 do
    if Assigned(FNotifiers[I]) then
      (FNotifiers[I] as IOTAFileHistoryNotifier).ProvidersUpdated;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFileHistoryManager.UpdateProviders;

begin

end;

//----------------------------------------------------------------------------------------------------------------------

{ TFileHistoryManager public }

//----------------------------------------------------------------------------------------------------------------------

constructor TFileHistoryManager.Create;

begin
  inherited Create;
  FNotifiers := TInterfaceList.Create;
  FProviders := TInterfaceList.Create;
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TFileHistoryManager.Destroy;

begin
  FProviders.Free;
  FNotifiers.Free;
  inherited Destroy;
end;

//----------------------------------------------------------------------------------------------------------------------

initialization
  FileHistoryManager := TFileHistoryManager.Create;

finalization
  FileHistoryManager := nil;

//----------------------------------------------------------------------------------------------------------------------

end.
