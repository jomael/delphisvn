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
{ The Original Code is DelphiSvnTests.dpr.                                                                             }
{                                                                                                                      }
{ The Initial Developer of the Original Code is Uwe Schuster.                                                          }
{ Portions created by Uwe Schuster are Copyright Uwe Schuster. All rights reserved.                                    }
{                                                                                                                      }
{ Contributors:                                                                                                        }
{   Uwe Schuster (uschuster)                                                                                           }
{                                                                                                                      }
{**********************************************************************************************************************}

program DelphiSvnTests;

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  Forms,
  TestFramework,
  GUITestRunner,
  TextTestRunner,
  TestSvnClient in 'TestSvnClient.pas';

{$R *.RES}

begin
  Application.Initialize;
  if IsConsole then
    TextTestRunner.RunRegisteredTests
  else
    GUITestRunner.RunRegisteredTests;
end.

