object F_Settings: TF_Settings
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsDialog
  Caption = 'Nastaven'#237
  ClientHeight = 321
  ClientWidth = 641
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label4: TLabel
    Left = 28
    Top = 137
    Width = 125
    Height = 13
    Caption = #381#225'dost o tra'#357'ov'#253' souhlas:'
  end
  object B_Apply: TButton
    Left = 558
    Top = 287
    Width = 75
    Height = 25
    Caption = 'Pou'#382#237't'
    Default = True
    TabOrder = 0
    OnClick = B_ApplyClick
  end
  object B_Storno: TButton
    Left = 477
    Top = 287
    Width = 75
    Height = 25
    Caption = 'Storno'
    TabOrder = 1
    OnClick = B_StornoClick
  end
  object PC_Main: TPageControl
    Left = 8
    Top = 8
    Width = 629
    Height = 273
    ActivePage = TS_Rights
    TabOrder = 2
    object TS_Server: TTabSheet
      Caption = 'P'#345'ipojen'#237
      object Label1: TLabel
        Left = 168
        Top = 51
        Width = 81
        Height = 13
        Caption = 'Server (IP/DNS):'
      end
      object Label2: TLabel
        Left = 168
        Top = 81
        Width = 24
        Height = 13
        Caption = 'Port:'
      end
      object E_Host: TEdit
        Left = 278
        Top = 51
        Width = 179
        Height = 21
        TabOrder = 0
        Text = '[E_Host]'
      end
      object CHB_Autoconnect: TCheckBox
        Left = 168
        Top = 120
        Width = 257
        Height = 17
        Caption = 'P'#345'ipojit se k serveru po startu'
        TabOrder = 1
      end
      object SE_Port: TSpinEdit
        Left = 278
        Top = 81
        Width = 179
        Height = 22
        MaxValue = 65535
        MinValue = 1
        TabOrder = 2
        Value = 1
      end
      object Button1: TButton
        Left = 334
        Top = 161
        Width = 123
        Height = 25
        Action = F_Main.A_Connect
        TabOrder = 3
      end
      object Button2: TButton
        Left = 334
        Top = 192
        Width = 123
        Height = 25
        Action = F_Main.A_Disconnect
        TabOrder = 4
      end
    end
    object TS_Rights: TTabSheet
      Caption = 'Autorizace'
      ImageIndex = 6
      object GB_Auth: TGroupBox
        Left = 184
        Top = 16
        Width = 257
        Height = 209
        Caption = ' P'#345#237'stup k syst'#233'mu '
        TabOrder = 0
        object Label14: TLabel
          Left = 16
          Top = 32
          Width = 90
          Height = 13
          Caption = 'U'#382'ivatelsk'#233' jm'#233'no:'
        end
        object Label15: TLabel
          Left = 16
          Top = 80
          Width = 30
          Height = 13
          Caption = 'Heslo:'
        end
        object CHB_RememberAuth: TCheckBox
          Left = 16
          Top = 159
          Width = 185
          Height = 17
          Caption = 'Ulo'#382'it u'#382'ivatelsk'#233' jm'#233'no a heslo'
          TabOrder = 0
          OnClick = CHB_RememberAuthClick
        end
        object E_username: TEdit
          Left = 16
          Top = 51
          Width = 193
          Height = 21
          TabOrder = 1
          Text = 'E_username'
          OnKeyPress = E_usernameKeyPress
        end
        object E_Password: TEdit
          Left = 16
          Top = 99
          Width = 193
          Height = 21
          PasswordChar = '*'
          TabOrder = 2
          Text = 'Edit1'
          OnChange = E_PasswordChange
          OnKeyPress = E_usernameKeyPress
        end
        object CHB_ShowPassword: TCheckBox
          Left = 16
          Top = 136
          Width = 97
          Height = 17
          Caption = 'Zobrazit heslo'
          TabOrder = 3
          OnClick = CHB_ShowPasswordClick
        end
      end
    end
    object TS_display: TTabSheet
      Caption = 'Zobrazen'#237
      ImageIndex = 2
      object CHB_StayOnTop: TCheckBox
        Left = 248
        Top = 24
        Width = 129
        Height = 17
        Caption = 'Okno v'#382'dy na pop'#345'ed'#237
        TabOrder = 0
      end
    end
  end
end
