object B2ConfigFrm: TB2ConfigFrm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'CloudFiles Accounts Manager'
  ClientHeight = 404
  ClientWidth = 807
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 273
    Top = 0
    Height = 363
    ExplicitLeft = 288
    ExplicitTop = 120
    ExplicitHeight = 100
  end
  object Panel1: TPanel
    Left = 276
    Top = 0
    Width = 531
    Height = 363
    Align = alClient
    BevelOuter = bvNone
    Caption = 'Please select Entry in the Tree.'
    TabOrder = 0
    ExplicitLeft = 303
    ExplicitTop = 26
    ExplicitWidth = 435
    ExplicitHeight = 241
  end
  object Panel2: TPanel
    Left = 0
    Top = 363
    Width = 807
    Height = 41
    Align = alBottom
    BevelOuter = bvLowered
    TabOrder = 1
    ExplicitTop = 368
    DesignSize = (
      807
      41)
    object btnNewAccount: TButton
      Left = 8
      Top = 8
      Width = 89
      Height = 25
      Caption = 'New Account'
      TabOrder = 0
      OnClick = btnNewAccountClick
    end
    object btnDeleteAccount: TButton
      Left = 103
      Top = 8
      Width = 89
      Height = 25
      Caption = 'Delete Account'
      TabOrder = 1
      OnClick = btnDeleteAccountClick
    end
    object btnApply: TButton
      Left = 198
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Apply'
      TabOrder = 2
    end
    object ctl_OK: TButton
      Left = 719
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = 'OK'
      ModalResult = 1
      TabOrder = 3
    end
  end
  object Panel3: TPanel
    Left = 0
    Top = 0
    Width = 273
    Height = 363
    Align = alLeft
    BevelOuter = bvLowered
    Caption = 'Panel3'
    TabOrder = 2
    ExplicitTop = 2
    ExplicitHeight = 337
    object TV: TTreeView
      Left = 1
      Top = 17
      Width = 271
      Height = 345
      Align = alClient
      BevelOuter = bvNone
      Indent = 19
      PopupMenu = PopupMenu1
      TabOrder = 0
      OnChange = TVChange
      OnEdited = TVEdited
      Items.NodeData = {
        0301000000340000000000000000000000FFFFFFFFFFFFFFFF00000000000000
        0000000000010B4D00790020004100630063006F0075006E0074007300}
      ExplicitLeft = -1
      ExplicitTop = 20
    end
    object Panel4: TPanel
      Left = 1
      Top = 1
      Width = 271
      Height = 16
      Align = alTop
      BevelOuter = bvLowered
      Caption = 'Select Entry:'
      TabOrder = 1
    end
  end
  object PopupMenu1: TPopupMenu
    Left = 120
    Top = 120
    object acEditAccountName1: TMenuItem
      Action = acEditAccountName
    end
  end
  object ActionList1: TActionList
    Left = 128
    Top = 72
    object acEditAccountName: TAction
      Caption = 'Edit'
      OnExecute = acEditAccountNameExecute
    end
  end
end
