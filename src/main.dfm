object F_Main: TF_Main
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Jerry'
  ClientHeight = 229
  ClientWidth = 415
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Visible = True
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object SB_Main: TStatusBar
    Left = 0
    Top = 210
    Width = 415
    Height = 19
    Panels = <
      item
        Text = 'Odpojeno od serveru'
        Width = 120
      end
      item
        Width = 250
      end>
  end
  object PC_Main: TPageControl
    Left = 0
    Top = 0
    Width = 416
    Height = 211
    MultiLine = True
    OwnerDraw = True
    TabOrder = 1
    OnChange = PC_MainChange
    OnDragDrop = PageControlDragDrop
    OnDragOver = PageControlDragOver
    OnDrawTab = PageControlCloseButtonDrawTab
    OnMouseDown = PageControlCloseButtonMouseDown
    OnMouseLeave = PageControlCloseButtonMouseLeave
    OnMouseMove = PageControlCloseButtonMouseMove
    OnMouseUp = PageControlCloseButtonMouseUp
  end
  object BitBtn1: TBitBtn
    Left = 367
    Top = 0
    Width = 25
    Height = 25
    DoubleBuffered = True
    Glyph.Data = {
      36030000424D3603000000000000360000002800000010000000100000000100
      1800000000000003000000000000000000000000000000000000FFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFBEBEBEBBBBBBBBBBBBECECECFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00000000
      0000000000AAAAAAFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFF000000000000000000AAAAAAFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00000000
      0000000000AAAAAAFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFF000000000000000000AAAAAAFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00000000
      0000000000AAAAAAFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFBEBEBEBBBBBB
      BBBBBBBBBBBBBBBBBBBBBBBB0000000000000000007D7D7DBBBBBBBBBBBBBBBB
      BBBBBBBBBBBBBBECECEC00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000AAAAAA000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000AAAAAA09090900000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000B3B3B3FFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFF000000000000000000AAAAAAFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00000000
      0000000000AAAAAAFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFF000000000000000000AAAAAAFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00000000
      0000000000AAAAAAFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFF000000000000000000AAAAAAFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF09090900
      0000000000B3B3B3FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
    ParentDoubleBuffered = False
    TabOrder = 2
    OnClick = BitBtn1Click
  end
  object BitBtn2: TBitBtn
    Left = 391
    Top = 0
    Width = 25
    Height = 25
    DoubleBuffered = True
    Glyph.Data = {
      36030000424D3603000000000000360000002800000010000000100000000100
      1800000000000003000000000000000000000000000000000000FFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFDFBF9C88A
      56C67D24BC6F27EDDED3FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFBF6F2C58140E9B244F2C34CEDB73ABF742BFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFCF9F6CF9048EAB54BEDBC
      4FEAB851F0C04CC77C25FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFCF8F2D99C53E4AA47E2A643E2A844DC9D3EC06E24D29B66FFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFDFAF5E2AF5BDCA042D48F35D69238D08A
      33B76221CF8D46FCF8F5FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFDFAE6
      B65ACF8D3CC87C2DC97F2FC27328B86423DAA053FCF9F6FFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFCEBAB9EBB75CAB4E08B26121B3601FB76426E4B0
      5CFCF8F3FFFFFFFFFFFFFFFFFFFFFFFFEEE9EAC5B5B6CFC0C1BAA4A5C7B7C4F0
      EFF1EBBA59A24304AB5B20EBBC61FDFBF7FFFFFFFFFFFFFFFFFFFFFFFFA89091
      AF9195DFD1D5DDD1D3DED1D3E0D4D3D8D0DCE6E2E3E1AE52ECB95AFDF9F3FFFF
      FFFFFFFFFFFFFFFFFFFFC9B8BAD6C4C6FFFFFFF3F0F0EFEBEDF5F1F3D5C8C8D1
      BFBFB19EB9DBC3B4FEFCF7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFAD9194FFFFFF
      C6B5B6EAE6E6F9F7F8F3EDEEF9F8F7E2DAD9C7B8BEFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFE0D3D3D9C8CAC5B4B6A89194FFFFFFFFFFFFFDFCFDFA
      FAF9CAB7BBFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFA28187CBBBBE
      FFFFFFFFFFFFB59CA2F5F1F1FFFFFFFFFFFFC3AFB4FFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFE7DFE1FFFFFFFFFFFFFFFFFFB69EA4F3EFEFFFFFFFD9
      CCCFF3EFF0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FBFAFAC0A9B0FFFFFFFFFFFFF2F2F1C4B1B4FFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFDFD4D6CFBBC0E6DEDEC7B3B8D8CBCEFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
    ParentDoubleBuffered = False
    TabOrder = 3
    OnClick = BitBtn2Click
  end
  object AL_Main: TActionList
    Left = 8
    Top = 16
    object A_Connect: TAction
      Caption = 'P'#345'ipojit k serveru'
      OnExecute = A_ConnectExecute
    end
    object A_Disconnect: TAction
      Caption = 'Odpojit od serveru'
      Enabled = False
      OnExecute = A_DisconnectExecute
    end
    object A_Debug: TAction
      Caption = 'Debug'
      ShortCut = 16499
      OnExecute = A_DebugExecute
    end
    object A_About: TAction
      Caption = 'About'
      ShortCut = 16496
      OnExecute = A_AboutExecute
    end
  end
  object AE_Main: TApplicationEvents
    OnMessage = AE_MainMessage
    Left = 72
    Top = 16
  end
end
