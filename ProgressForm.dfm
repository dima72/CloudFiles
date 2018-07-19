object ProgressFrm: TProgressFrm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Progress'
  ClientHeight = 125
  ClientWidth = 481
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 96
  TextHeight = 13
  object LblFileName: TLabel
    Left = 40
    Top = 64
    Width = 56
    Height = 13
    Caption = 'LblFileName'
  end
  object LblComplete: TLabel
    Left = 40
    Top = 83
    Width = 58
    Height = 13
    Caption = 'LblComplete'
  end
  object ProgressBar1: TProgressBar
    Left = 32
    Top = 24
    Width = 417
    Height = 17
    TabOrder = 0
  end
  object Cancel: TButton
    Left = 368
    Top = 78
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 1
    OnClick = CancelClick
  end
end
