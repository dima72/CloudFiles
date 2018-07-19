object UploadDialogFrm: TUploadDialogFrm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Cloud Selection'
  ClientHeight = 124
  ClientWidth = 486
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnShow = FormShow
  DesignSize = (
    486
    124)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 24
    Top = 64
    Width = 20
    Height = 13
    Caption = 'File:'
  end
  object lblFileName: TLabel
    Left = 88
    Top = 64
    Width = 53
    Height = 13
    Caption = 'lblFileName'
  end
  object Label2: TLabel
    Left = 24
    Top = 83
    Width = 52
    Height = 13
    Caption = 'Upload To:'
  end
  object Button1: TButton
    Left = 387
    Top = 78
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Next >>'
    ModalResult = 1
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 486
    Height = 49
    Align = alTop
    Caption = 
      'Please select Cloud account where you want to send the file to a' +
      'nd  press '#39'Next >>'#39' button.'
    Color = clWhite
    ParentBackground = False
    TabOrder = 1
  end
  object ctl_AccountName: TComboBox
    Left = 88
    Top = 83
    Width = 145
    Height = 21
    Style = csDropDownList
    TabOrder = 2
  end
end
