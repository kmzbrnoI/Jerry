object F_NewLoko: TF_NewLoko
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  Caption = #381#225'dost o novou lokomotivu'
  ClientHeight = 398
  ClientWidth = 216
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 247
    Width = 97
    Height = 13
    Caption = 'Pozn'#225'mka k '#382#225'dosti:'
  end
  object Label2: TLabel
    Left = 8
    Top = 335
    Width = 63
    Height = 13
    Caption = 'Stav '#382#225'dosti:'
  end
  object LB_Stanice: TListBox
    Left = 8
    Top = 48
    Width = 200
    Height = 193
    ItemHeight = 13
    TabOrder = 0
    OnDblClick = LB_StaniceDblClick
  end
  object StaticText1: TStaticText
    Left = 8
    Top = 8
    Width = 200
    Height = 34
    AutoSize = False
    Caption = 'Vyberte stanici, kterou chcete '#382#225'dat o lokomotivu:'
    TabOrder = 1
  end
  object E_Note: TEdit
    Left = 8
    Top = 266
    Width = 201
    Height = 21
    TabOrder = 2
    Text = 'E_Note'
  end
  object B_Apply: TButton
    Left = 8
    Top = 304
    Width = 97
    Height = 25
    Caption = 'Vytvo'#345'it '#382#225'dost'
    Default = True
    TabOrder = 3
    OnClick = B_ApplyClick
  end
  object B_Cancel: TButton
    Left = 111
    Top = 304
    Width = 98
    Height = 25
    Caption = 'Zru'#353'it '#382#225'dost'
    Enabled = False
    TabOrder = 4
    OnClick = B_CancelClick
  end
  object ST_Stav: TStaticText
    Left = 8
    Top = 354
    Width = 200
    Height = 31
    Alignment = taCenter
    AutoSize = False
    Caption = 'ST_Stav'
    Color = clBtnFace
    Font.Charset = ANSI_CHARSET
    Font.Color = clRed
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    TabOrder = 5
    Transparent = False
  end
end
