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
{ The Original Code is SvnOptionsDialog.pas.                                                                           }
{                                                                                                                      }
{ The Initial Developer of the Original Code is Ondrej Kelle.                                                          }
{ Portions created by Ondrej Kelle are Copyright Ondrej Kelle. All rights reserved.                                    }
{                                                                                                                      }
{ Contributors:                                                                                                        }
{   Ondrej Kelle (tondrej)                                                                                             }
{                                                                                                                      }
{**********************************************************************************************************************}
{                                                                                                                      }
{ This unit contains a modal dialog which provides user interface to configure Subversion plugin options.              }
{                                                                                                                      }
{**********************************************************************************************************************}

unit SvnOptionsDialog;

interface

{$include Compilers.inc}

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls;

type
  TFormSvnOptions = class(TForm)
    ButtonBrowse: TButton;
    ButtonCancel: TButton;
    ButtonOK: TButton;
    ComboBoxDirs: TComboBox;
    LabelDirs: TLabel;

    procedure ButtonBrowseClick(Sender: TObject);
    procedure ComboBoxDirsChange(Sender: TObject);
  private
  public
  end;

function ShowSvnOptionsDialog(var Directories: string; History: TStrings): TModalResult;

implementation

uses
  FileCtrl,
  SvnImages;

{$R *.dfm}

//----------------------------------------------------------------------------------------------------------------------

function ShowSvnOptionsDialog(var Directories: string; History: TStrings): TModalResult;

var
  Form: TFormSvnOptions;

begin
  Form := TFormSvnOptions.Create(nil);
  try
    Form.Icon := SvnImageModule.Icon;
    Form.ComboBoxDirs.Items.Assign(History);
    Form.ComboBoxDirs.Text := Directories;
    Result := Form.ShowModal;
    if Result = mrOK then
      Directories := Form.ComboBoxDirs.Text;
  finally
    Form.Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

{ TFormSvnOptions event handlers }

//----------------------------------------------------------------------------------------------------------------------

procedure TFormSvnOptions.ButtonBrowseClick(Sender: TObject);

var
  Lib: THandle;
  TOrderedListEditDlg: TCustomFormClass;
  Dlg: TCustomForm;
  Label1: TLabel;
  OKButton, CancelButton, HelpButton: TButton;
  ListBox: TListBox;

begin
  {$IFDEF COMPILER_10}
  Lib := GetModuleHandle('coreide100.bpl');
  {$ENDIF}
  {$IFDEF COMPILER_9}
  Lib := GetModuleHandle('coreide90.bpl');
  {$ENDIF}
  {$IFNDEF COMPILER_9_UP}
  Lib := 0;
  {$ENDIF}
  if Lib = 0 then
    Exit;
  TOrderedListEditDlg := GetProcAddress(Lib, '@Orderedlisteditdialog@TOrderedListEditDlg@');
  if not Assigned(TOrderedListEditDlg) then
    Exit;

  Dlg := TOrderedListEditDlg.Create(nil);
  try
    Dlg.Caption := 'Subversion Directories';

    Label1 := Dlg.FindComponent('Label1') as TLabel;
    if Assigned(Label1) then
      Label1.Caption := 'Working copy directories:';

    ListBox := Dlg.FindComponent('CreationList') as TListBox;
    if not Assigned(ListBox) then
      Exit;
    ListBox.Items.Delimiter := ';';
    {$IFDEF COMPILER_10_UP}
    ListBox.Items.StrictDelimiter := True;
    {$ENDIF}
    ListBox.Items.DelimitedText := ComboBoxDirs.Text;
    ListBox.Style := lbOwnerDrawFixed;

    // No help is available; hide help button and move OK/Cancel buttons to the right
    HelpButton := Dlg.FindComponent('HelpButton') as TButton;
    CancelButton := Dlg.FindComponent('CancelButton') as TButton;
    OKButton := Dlg.FindComponent('OKButton') as TButton;
    if Assigned(HelpButton) and Assigned(OKButton) and Assigned(CancelButton) then
    begin
      HelpButton.Visible := False;
      OKButton.Left := CancelButton.Left;
      CancelButton.Left := HelpButton.Left;
    end;

    if Dlg.ShowModal = mrOK then
    begin
      ComboBoxDirs.Text := ListBox.Items.DelimitedText;
      ComboBoxDirsChange(ComboBoxDirs);
    end;
  finally
    Dlg.Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFormSvnOptions.ComboBoxDirsChange(Sender: TObject);

begin
  ButtonOK.Enabled := True;
end;

//----------------------------------------------------------------------------------------------------------------------

end.
