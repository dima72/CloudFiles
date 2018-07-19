object LoginFrm: TLoginFrm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Login'
  ClientHeight = 167
  ClientWidth = 330
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 23
    Top = 13
    Width = 230
    Height = 13
    Caption = 'Please enter your Backblaze login and password'
  end
  object Label2: TLabel
    Left = 32
    Top = 32
    Width = 86
    Height = 13
    Caption = 'Login( e.g. email )'
  end
  object Label3: TLabel
    Left = 32
    Top = 80
    Width = 46
    Height = 13
    Caption = 'Password'
  end
  object ctl_Login: TEdit
    Left = 32
    Top = 51
    Width = 169
    Height = 21
    TabOrder = 0
    Text = 'Abhay'
  end
  object ctl_Password: TEdit
    Left = 32
    Top = 99
    Width = 169
    Height = 21
    PasswordChar = '*'
    TabOrder = 1
  end
  object Button1: TButton
    Left = 224
    Top = 128
    Width = 75
    Height = 25
    Caption = 'OK'
    TabOrder = 2
    OnClick = Button1Click
  end
end
