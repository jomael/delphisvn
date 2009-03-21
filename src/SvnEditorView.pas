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
{ This unit contains TSvnEditorView, a class which implements ICustomEditorView and ICustomEditorFrameView interfaces  }
{ to provide a new editor view displaying Subversion information about the file.                                       }
{                                                                                                                      }
{**********************************************************************************************************************}

unit SvnEditorView;

interface

uses
  Classes, SysUtils, Forms,
  ToolsAPI, DesignIntf, EditorViewSupport;

{$INCLUDE Compilers.inc}

type
  TSvnEditorView = class(TInterfacedObject, ICustomEditorView, ICustomEditorFrameView)
  private
    { ICustomEditorView }
    function GetCaption: string;
    function GetPriority: Integer;
    function GetStyle: TEditorViewStyle;
    procedure Display(const AContext: IInterface; AViewObject: TObject);
    function EditAction(const AContext: IInterface; Action: TEditAction; AViewObject: TObject): Boolean;
    function GetEditState(const AContext: IInterface; AViewObject: TObject): TEditState;
    function Handles(const AContext: IInterface): Boolean;
    {$IFDEF COMPILER_9_UP}
    function GetCanCloneView: Boolean;
    function GetViewIdentifier: string;
    procedure Hide(const AContext: IInterface; AViewObject: TObject);
    procedure ViewClosed(const AContext: IInterface; AViewObject: TObject);
    {$ENDIF}

    { ICustomEditorFrameView }
    function GetFrameClass: TCustomFrameClass;
  end;

implementation

uses
  SvnClient, SvnIDEClient, SvnEditorViewFrame;

//----------------------------------------------------------------------------------------------------------------------

{ TSvnEditorView private: IOTACustomEditorView }

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnEditorView.Display(const AContext: IInterface; AViewObject: TObject);

var
  Module: IOTAModule;
  Items: TSvnItemArray;
  I: Integer;

begin
  if ContextToModule(AContext, Module) and Assigned(Module) then
  begin
    if AViewObject is TFrameSvnEditorView then
    begin
      SetLength(Items, Module.ModuleFileCount);
      for I := 0 to Module.ModuleFileCount - 1 do
        Items[I] := TSvnItem.Create(SvnIDEModule.SvnClient, nil, Module.ModuleFileEditors[I].FileName);
      TFrameSvnEditorView(AViewObject).Display(Items);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnEditorView.EditAction(const AContext: IInterface; Action: TEditAction; AViewObject: TObject): Boolean;

begin
  Result := True;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnEditorView.GetCaption: string;

begin
  Result := 'Subversion';
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnEditorView.GetEditState(const AContext: IInterface; AViewObject: TObject): TEditState;

begin
  Result := [];
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnEditorView.GetPriority: Integer;

begin
  Result := NormalViewPriority;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnEditorView.GetStyle: TEditorViewStyle;

begin
  Result := [evsDesigntime];
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnEditorView.Handles(const AContext: IInterface): Boolean;

var
  Module: IOTAModule;

begin
  Result := False;

  if ContextToModule(AContext, Module) and Assigned(Module) then
    Result := SvnIDEModule.SvnClient.IsPathVersioned(Module.FileName);
end;

{$IFDEF COMPILER_9_UP}
//----------------------------------------------------------------------------------------------------------------------

function TSvnEditorView.GetCanCloneView: Boolean;

begin
  Result := False;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnEditorView.GetViewIdentifier: string;

begin
  Result := 'TOndrej.SubversionView';
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnEditorView.Hide(const AContext: IInterface; AViewObject: TObject);

begin
  if AViewObject is TFrameSvnEditorView then
    TFrameSvnEditorView(AViewObject).FreeItems;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnEditorView.ViewClosed(const AContext: IInterface; AViewObject: TObject);

begin
  if AViewObject is TFrameSvnEditorView then
    TFrameSvnEditorView(AViewObject).Clear;
end;
{$ENDIF}

//----------------------------------------------------------------------------------------------------------------------

{ TSvnEditorView private: IOTACustomEditorFrameView }

//----------------------------------------------------------------------------------------------------------------------

function TSvnEditorView.GetFrameClass: TCustomFrameClass;

begin
  Result := TFrameSvnEditorView;
end;

//----------------------------------------------------------------------------------------------------------------------

end.
