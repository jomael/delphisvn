{**********************************************************************************************************************}
{                                                                                                                      }
{ delphisvn: Subversion plugin for CodeGear Delphi                                                                     }
{                                                                                                                      }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); you may not use     }
{ this file except in compliance with the License. You may obtain a copy of the License at http://www.mozilla.org/MPL/ }
{                                                                                                                      }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either      }
{ express or implied. See the License for the specific language governing rights and limitations under the License.    }
{                                                                                                                      }
{ The Original Code is SvnEditorView.pas.                                                                              }
{                                                                                                                      }
{ The Initial Developer of the Original Code is Ondrej Kelle.                                                          }
{ Portions created by Ondrej Kelle are Copyright Ondrej Kelle. All rights reserved.                                    }
{                                                                                                                      }
{ Contributors:                                                                                                        }
{   Ondrej Kelle (tondrej)                                                                                             }
{                                                                                                                      }
{**********************************************************************************************************************}
{                                                                                                                      }
{ This unit contains TSvnHistoryView, a class which implements ICustomEditorView and ICustomEditorFrameView interfaces }
{ to provide a new editor view displaying Subversion history information about the file.                               }
{                                                                                                                      }
{**********************************************************************************************************************}

unit SvnHistoryView;

interface

uses
  Classes, SysUtils, Forms,
  ToolsAPI, DesignIntf, EditorViewSupport;

type
  TSvnHistoryView = class(TInterfacedObject, ICustomEditorView, ICustomEditorFrameView)
  private
    { ICustomEditorView }
    function GetCaption: string;
    function GetPriority: Integer;
    function GetStyle: TEditorViewStyle;
    procedure Display(const AContext: IInterface; AViewObject: TObject);
    function EditAction(const AContext: IInterface; Action: TEditAction; AViewObject: TObject): Boolean;
    function GetEditState(const AContext: IInterface; AViewObject: TObject): TEditState;
    function Handles(const AContext: IInterface): Boolean;

    { ICustomEditorFrameView }
    function GetFrameClass: TCustomFrameClass;
  end;

implementation

uses
  SvnClient, SvnIDEClient, SvnHistoryViewFrame;

//----------------------------------------------------------------------------------------------------------------------

{ TSvnHistoryView private: ICustomEditorView}

//----------------------------------------------------------------------------------------------------------------------

function TSvnHistoryView.GetCaption: string;

begin
  Result := 'History';
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnHistoryView.GetPriority: Integer;

begin
  Result := NormalViewPriority;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnHistoryView.GetStyle: TEditorViewStyle;

begin
  Result := [evsDesigntime];
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnHistoryView.Display(const AContext: IInterface; AViewObject: TObject);

var
  Module: IOTAModule;

begin
  if ContextToModule(AContext, Module) and Assigned(Module) and (AViewObject is TFrameSvnHistoryView) then
    TFrameSvnHistoryView(AViewObject).Display(Module);
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnHistoryView.EditAction(const AContext: IInterface; Action: TEditAction; AViewObject: TObject): Boolean;

begin
  Result := True;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnHistoryView.GetEditState(const AContext: IInterface; AViewObject: TObject): TEditState;

begin
  Result := [];
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnHistoryView.Handles(const AContext: IInterface): Boolean;

var
  Module: IOTAModule;

begin
  Result := False;

  if ContextToModule(AContext, Module) and Assigned(Module) then
    Result := SvnIDEModule.SvnClient.IsPathVersioned(Module.FileName);
end;

//----------------------------------------------------------------------------------------------------------------------

{ TSvnHistoryView private: ICustomEditorFrameView }

//----------------------------------------------------------------------------------------------------------------------

function TSvnHistoryView.GetFrameClass: TCustomFrameClass;

begin
  Result := TFrameSvnHistoryView;
end;

//----------------------------------------------------------------------------------------------------------------------

end.
