object CloudProfileFrm: TCloudProfileFrm
  Left = 0
  Top = 0
  Width = 619
  Height = 359
  TabOrder = 0
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 619
    Height = 359
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'Account Details'
      object PageControl2: TPageControl
        Left = 0
        Top = 41
        Width = 611
        Height = 290
        ActivePage = BackblazeSheet
        Align = alClient
        Style = tsFlatButtons
        TabOrder = 0
        object HubicSheet: TTabSheet
          Caption = 'HubicSheet'
          ImageIndex = 1
          TabVisible = False
          object Label14: TLabel
            Left = 19
            Top = 21
            Width = 44
            Height = 13
            Caption = 'Provider:'
          end
          object Label13: TLabel
            Left = 69
            Top = 21
            Width = 26
            Height = 13
            Cursor = crHandPoint
            Caption = 'Hubic'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlue
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
            OnClick = Label13Click
          end
          object Label15: TLabel
            Left = 102
            Top = 21
            Width = 183
            Height = 13
            Caption = 'You can sign up to use 25GB for free! '
          end
          object Label11: TLabel
            Left = 19
            Top = 87
            Width = 336
            Height = 13
            Caption = 
              'if not authenticated OAuth page will be presented in Internet br' +
              'owser'
          end
          object ctl_HubicTestBtn: TButton
            Left = 19
            Top = 56
            Width = 118
            Height = 25
            Caption = 'Test Connection'
            TabOrder = 0
            OnClick = ctl_HubicTestBtnClick
          end
        end
        object BackblazeSheet: TTabSheet
          Caption = 'BackblazeSheet'
          TabVisible = False
          object Label1: TLabel
            Left = 8
            Top = 28
            Width = 62
            Height = 13
            Caption = 'Bucket Name'
          end
          object Label2: TLabel
            Left = 8
            Top = 74
            Width = 37
            Height = 13
            Caption = 'AppKey'
          end
          object Label3: TLabel
            Left = 128
            Top = 74
            Width = 50
            Height = 13
            Caption = 'AppSecret'
          end
          object Label9: TLabel
            Left = 61
            Top = 9
            Width = 47
            Height = 13
            Cursor = crHandPoint
            Caption = 'Backblaze'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlue
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
            OnClick = Label9Click
          end
          object Label10: TLabel
            Left = 8
            Top = 9
            Width = 44
            Height = 13
            Caption = 'Provider:'
          end
          object ctl_BucketName: TEdit
            Left = 8
            Top = 47
            Width = 161
            Height = 21
            TabOrder = 0
            Text = ' auslogics'
            OnChange = ctl_BucketNameChange
          end
          object ctl_AppKey: TEdit
            Left = 8
            Top = 93
            Width = 89
            Height = 21
            TabOrder = 1
            Text = '532880bb7475'
            OnChange = ctl_BucketNameChange
          end
          object ctl_AppSecret: TEdit
            Left = 128
            Top = 93
            Width = 257
            Height = 21
            TabOrder = 2
            Text = '0019cf303b98bed6541d0d75bf292eaadc557380b1'
            OnChange = ctl_BucketNameChange
          end
          object ctl_CheckBucket: TButton
            Left = 175
            Top = 43
            Width = 98
            Height = 25
            Caption = 'Test Connection'
            TabOrder = 3
            OnClick = ctl_CheckBucketClick
          end
        end
        object CloudFilesSheet: TTabSheet
          Caption = 'CloudFilesSheet'
          ImageIndex = 2
          TabVisible = False
          object Label4: TLabel
            Left = 13
            Top = 35
            Width = 104
            Height = 13
            Caption = 'You should SignUp at '
          end
          object Label5: TLabel
            Left = 123
            Top = 35
            Width = 48
            Height = 13
            Cursor = crHandPoint
            Caption = 'CloudFiles'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlue
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
            OnClick = Label5Click
          end
          object Label6: TLabel
            Left = 16
            Top = 52
            Width = 88
            Height = 13
            Caption = 'Login( your email )'
          end
          object Label7: TLabel
            Left = 16
            Top = 91
            Width = 46
            Height = 13
            Caption = 'Password'
          end
          object Label8: TLabel
            Left = 177
            Top = 35
            Width = 41
            Height = 13
            Caption = 'website.'
          end
          object Label16: TLabel
            Left = 13
            Top = 3
            Width = 342
            Height = 26
            Caption = 
              'CloudFiles Pro is experimental cloud storage allowing you to sta' +
              'rt using cloud backup immediately '
            WordWrap = True
          end
          object ctl_CloudfilesLogin: TEdit
            Left = 16
            Top = 71
            Width = 161
            Height = 21
            TabOrder = 0
          end
          object ctl_CloudFilesPassword: TEdit
            Left = 16
            Top = 108
            Width = 161
            Height = 21
            PasswordChar = '*'
            TabOrder = 1
          end
          object ctl_CloudFilesCheck: TButton
            Left = 207
            Top = 108
            Width = 98
            Height = 25
            Caption = 'Test Connection'
            TabOrder = 2
            OnClick = ctl_CloudFilesCheckClick
          end
        end
      end
      object Panel1: TPanel
        Left = 0
        Top = 0
        Width = 611
        Height = 41
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 1
        object Label12: TLabel
          Left = 15
          Top = 14
          Width = 54
          Height = 13
          Caption = 'Cloud Type'
        end
        object Label17: TLabel
          Left = 197
          Top = 14
          Width = 60
          Height = 13
          Caption = 'Storage Size'
        end
        object Label18: TLabel
          Left = 340
          Top = 14
          Width = 13
          Height = 13
          Caption = 'GB'
        end
        object ctl_ActiveStorageComboBox: TComboBox
          Left = 83
          Top = 11
          Width = 101
          Height = 21
          Style = csDropDownList
          TabOrder = 0
          OnChange = ctl_ActiveStorageComboBoxChange
          Items.Strings = (
            'Hubic'
            'Backblaze')
        end
        object ctl_StorageSizeEdit: TEdit
          Left = 263
          Top = 11
          Width = 55
          Height = 21
          NumbersOnly = True
          TabOrder = 1
          Text = '0'
          OnChange = ctl_BucketNameChange
        end
        object ctl_StorageSize: TUpDown
          Left = 318
          Top = 11
          Width = 16
          Height = 21
          Associate = ctl_StorageSizeEdit
          TabOrder = 2
          OnChanging = ctl_StorageSizeChanging
        end
      end
    end
    object FilesTabSheet: TTabSheet
      Caption = 'Files'
      ImageIndex = 1
      OnShow = FilesTabSheetShow
      object Panel2: TPanel
        Left = 0
        Top = 0
        Width = 611
        Height = 33
        Align = alTop
        BevelOuter = bvLowered
        Caption = 'Uploaded files'
        TabOrder = 0
      end
      object FilesLV: TListView
        Left = 0
        Top = 33
        Width = 611
        Height = 298
        Align = alClient
        Columns = <
          item
            AutoSize = True
            Caption = 'FileName'
          end
          item
            AutoSize = True
            Caption = 'Size'
          end
          item
            AutoSize = True
            Caption = 'Date'
          end>
        ReadOnly = True
        RowSelect = True
        TabOrder = 1
        ViewStyle = vsReport
      end
    end
  end
end
