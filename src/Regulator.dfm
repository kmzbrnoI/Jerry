object F_DigiReg: TF_DigiReg
  Left = 599
  Top = 400
  BorderStyle = bsNone
  Caption = 'Regul'#225'tor'
  ClientHeight = 189
  ClientWidth = 410
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  TextHeight = 13
  object L_stupen: TLabel
    Left = 93
    Top = 164
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
    Left = 17
    Top = 100
    Width = 48
    Height = 18
    Brush.Color = clGray
    OnMouseUp = S_StatusMouseUp
  end
  object L_speed: TLabel
    Left = 21
    Top = 164
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
    Left = 32
    Top = 164
    Width = 25
    Height = 13
    Caption = 'km/h'
  end
  object PC_Funkce: TPageControl
    Left = 120
    Top = 39
    Width = 282
    Height = 144
    ActivePage = TS_func_0_13
    TabOrder = 7
    object TS_func_0_13: TTabSheet
      Caption = 'F1-F14'
    end
    object TS_func_14_28: TTabSheet
      Caption = 'F15-F28'
      ImageIndex = 1
    end
  end
  object RG_Smer: TRadioGroup
    Left = 8
    Top = 35
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
    Left = 78
    Top = 105
    Width = 39
    Height = 30
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'P'#345'evz'#237't'
    Enabled = False
    TabOrder = 4
    OnClick = B_PrevzitLokoClick
  end
  object B_STOP: TButton
    Left = 78
    Top = 39
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
    TabOrder = 2
    OnClick = B_STOPClick
  end
  object CHB_Total: TCheckBox
    Left = 5
    Top = 141
    Width = 85
    Height = 17
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Ru'#269'n'#237' '#345#237'zen'#237
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 6
    OnClick = CHB_TotalClick
  end
  object B_Idle: TButton
    Left = 78
    Top = 72
    Width = 39
    Height = 30
    Caption = 'idle'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 3
    OnClick = B_IdleClick
  end
  object TB_reg: TTrackBar
    Left = 3
    Top = 6
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
    Left = 5
    Top = 125
    Width = 72
    Height = 17
    Caption = 'Multitrakce'
    TabOrder = 5
  end
  object T_Speed: TTimer
    Interval = 100
    OnTimer = T_SpeedTimer
    Left = 48
    Top = 5
  end
  object T_Mom_Release: TTimer
    Interval = 200
    OnTimer = T_Mom_ReleaseTimer
    Left = 96
    Top = 5
  end
end
