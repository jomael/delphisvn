inherited FormSvnTools: TFormSvnTools
  Left = 365
  Top = 193
  Caption = 'Subversion'
  ClientHeight = 340
  ClientWidth = 516
  ExplicitLeft = 365
  ExplicitTop = 193
  ExplicitWidth = 524
  ExplicitHeight = 367
  PixelsPerInch = 96
  TextHeight = 13
  inherited Splitter1: TSplitter
    Width = 516
    ExplicitWidth = 516
  end
  inherited ToolBar1: TToolBar
    Width = 516
    ButtonWidth = 27
    Images = SvnIDEClient.ImageList
    List = True
    ExplicitWidth = 516
    object ToolButtonCancel: TToolButton
      Left = 4
      Top = 0
      Action = SvnIDEClient.ActionCancel
      AutoSize = True
    end
    object ToolButtonUpdate: TToolButton
      Left = 31
      Top = 0
      Action = SvnIDEClient.ActionUpdate
      AutoSize = True
    end
    object ToolButtonCheckModifications: TToolButton
      Left = 58
      Top = 0
      Action = SvnIDEClient.ActionCheckModifications
      AutoSize = True
    end
    object ToolButtonCommit: TToolButton
      Left = 85
      Top = 0
      Action = SvnIDEClient.ActionCommit
      AutoSize = True
    end
    object ToolButtonRevert: TToolButton
      Left = 112
      Top = 0
      Action = SvnIDEClient.ActionRevert
      AutoSize = True
    end
    object ToolButtonCleanup: TToolButton
      Left = 139
      Top = 0
      Action = SvnIDEClient.ActionCleanup
      AutoSize = True
    end
    object ToolButtonSeparator1: TToolButton
      Left = 166
      Top = 0
      Width = 8
      Caption = 'ToolButtonSeparator1'
      ImageIndex = 3
      Style = tbsSeparator
    end
    object ToolButtonOptions: TToolButton
      Left = 174
      Top = 0
      Action = SvnIDEClient.ActionOptions
      AutoSize = True
    end
  end
  inherited DockActionList: TActionList
    object ActionShowDiff: TAction
      Caption = 'Show &Diff'
      Hint = 'Show differences from Base revision'
      Visible = False
      OnExecute = ActionShowDiffExecute
      OnUpdate = ActionShowDiffUpdate
    end
    object ActionOpen: TAction
      Caption = '&Open'
      Hint = 'Open selected file in the editor'
      OnExecute = ActionOpenExecute
      OnUpdate = ActionOpenUpdate
    end
  end
  inherited PopupMenu1: TPopupActionBar
    OnPopup = PopupMenu1Popup
    object MenuOpen: TMenuItem [0]
      Action = ActionOpen
      Default = True
    end
    object MenuShowDiff: TMenuItem [1]
      Action = ActionShowDiff
    end
    object MenuSeparator1: TMenuItem [2]
      Caption = '-'
      Visible = False
    end
    object MenuStayOnTop: TMenuItem
      Action = StayOnTopCmd
    end
    object MenuDockable: TMenuItem
      Action = DockableCmd
    end
  end
end