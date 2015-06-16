object F_DigiReg: TF_DigiReg
  Left = 599
  Top = 400
  BorderStyle = bsNone
  Caption = 'Regul'#225'tor'
  ClientHeight = 166
  ClientWidth = 417
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label5: TLabel
    Left = 336
    Top = 56
    Width = 33
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Adresa'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Label6: TLabel
    Left = 336
    Top = 71
    Width = 43
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'P'#345'evzato'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Label7: TLabel
    Left = 336
    Top = 87
    Width = 34
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Stupe'#328
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object L_address: TLabel
    Left = 386
    Top = 56
    Width = 24
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Alignment = taRightJustify
    Caption = '9999'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object L_mine: TLabel
    Left = 398
    Top = 71
    Width = 12
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Alignment = taRightJustify
    Caption = 'ne'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object L_stupen: TLabel
    Left = 398
    Top = 87
    Width = 12
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Alignment = taRightJustify
    Caption = '28'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object S_Status: TShape
    Left = 25
    Top = 110
    Width = 48
    Height = 18
    Brush.Color = clGray
    OnMouseUp = S_StatusMouseUp
  end
  object L_ComStatus: TLabel
    Left = 232
    Top = 43
    Width = 63
    Height = 13
    Caption = 'L_ComStatus'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentColor = False
    ParentFont = False
  end
  object Label1: TLabel
    Left = 336
    Top = 104
    Width = 41
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Rychlost'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object L_speed: TLabel
    Left = 404
    Top = 104
    Width = 6
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Alignment = taRightJustify
    Caption = '0'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Label2: TLabel
    Left = 346
    Top = 118
    Width = 31
    Height = 13
    Caption = '(km/h)'
  end
  object CHB_svetla: TCheckBox
    Left = 147
    Top = 42
    Width = 80
    Height = 21
    Hint = '0'
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'F0 (sv'#283'tla)'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
    OnClick = CHB_svetlaClick
  end
  object CHB_f1: TCheckBox
    Tag = 1
    Left = 147
    Top = 60
    Width = 34
    Height = 17
    Hint = '1'
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'F1'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 3
    OnClick = CHB_svetlaClick
  end
  object CHB_f2: TCheckBox
    Tag = 2
    Left = 147
    Top = 76
    Width = 32
    Height = 17
    Hint = '2'
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'F2'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 4
    OnClick = CHB_svetlaClick
  end
  object CHB_f4: TCheckBox
    Tag = 4
    Left = 147
    Top = 108
    Width = 34
    Height = 17
    Hint = '4'
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'F4'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 6
    OnClick = CHB_svetlaClick
  end
  object CHB_f3: TCheckBox
    Tag = 3
    Left = 147
    Top = 92
    Width = 34
    Height = 17
    Hint = '3'
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'F3'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 5
    OnClick = CHB_svetlaClick
  end
  object CHB_f5: TCheckBox
    Tag = 5
    Left = 147
    Top = 124
    Width = 32
    Height = 17
    Hint = '5'
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'F5'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 7
    OnClick = CHB_svetlaClick
  end
  object CHB_f6: TCheckBox
    Tag = 6
    Left = 147
    Top = 140
    Width = 34
    Height = 17
    Hint = '6'
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'F6'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 8
    OnClick = CHB_svetlaClick
  end
  object CHB_f8: TCheckBox
    Tag = 8
    Left = 230
    Top = 76
    Width = 32
    Height = 17
    Hint = '8'
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'F8'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 10
    OnClick = CHB_svetlaClick
  end
  object CHB_f7: TCheckBox
    Tag = 7
    Left = 230
    Top = 60
    Width = 32
    Height = 17
    Hint = '7'
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'F7'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 9
    OnClick = CHB_svetlaClick
  end
  object RG_Smer: TRadioGroup
    Left = 16
    Top = 48
    Width = 65
    Height = 57
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = ' Sm'#283'r '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Items.Strings = (
      'vp'#345'ed'
      'vzad')
    ParentFont = False
    TabOrder = 1
    OnClick = RG_SmerClick
  end
  object B_PrevzitLoko: TButton
    Left = 336
    Top = 138
    Width = 75
    Height = 24
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'P'#345'evz'#237't loko'
    Enabled = False
    TabOrder = 19
    OnClick = B_PrevzitLokoClick
  end
  object B_STOP: TButton
    Left = 96
    Top = 54
    Width = 39
    Height = 30
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'STOP'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 15
    OnClick = B_STOPClick
  end
  object CHB_Total: TCheckBox
    Left = 17
    Top = 129
    Width = 119
    Height = 17
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Tot'#225'ln'#237' ru'#269'n'#237' '#345#237'zen'#237
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 17
    OnClick = CHB_TotalClick
  end
  object CHB_f9: TCheckBox
    Tag = 9
    Left = 230
    Top = 92
    Width = 32
    Height = 17
    Hint = '5'
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'F9'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 11
    OnClick = CHB_svetlaClick
  end
  object CHB_f10: TCheckBox
    Tag = 10
    Left = 230
    Top = 108
    Width = 40
    Height = 17
    Hint = '6'
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'F10'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 12
    OnClick = CHB_svetlaClick
  end
  object CHB_f12: TCheckBox
    Tag = 12
    Left = 230
    Top = 140
    Width = 40
    Height = 16
    Hint = '8'
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'F12'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 14
    OnClick = CHB_svetlaClick
  end
  object CHB_f11: TCheckBox
    Tag = 11
    Left = 230
    Top = 124
    Width = 40
    Height = 17
    Hint = '7'
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'F11'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 13
    OnClick = CHB_svetlaClick
  end
  object B_Idle: TButton
    Left = 96
    Top = 89
    Width = 39
    Height = 30
    Caption = 'idle'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 16
    OnClick = B_IdleClick
  end
  object TB_reg: TTrackBar
    Left = 7
    Top = 7
    Width = 405
    Height = 27
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    DoubleBuffered = False
    Max = 28
    ParentDoubleBuffered = False
    ParentShowHint = False
    PageSize = 1
    ShowHint = False
    TabOrder = 0
  end
  object CHB_Multitrack: TCheckBox
    Left = 17
    Top = 144
    Width = 81
    Height = 17
    Caption = 'Multitrakce'
    TabOrder = 18
  end
  object T_Speed: TTimer
    Interval = 100
    OnTimer = T_SpeedTimer
    Left = 16
    Top = 16
  end
end
