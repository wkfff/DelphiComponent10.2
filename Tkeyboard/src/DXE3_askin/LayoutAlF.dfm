object LayoutAlForm: TLayoutAlForm
  Left = 259
  Top = 245
  Width = 639
  Height = 387
  BorderIcons = [biSystemMenu]
  Caption = 'Keyboard Layout Editor'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object LayoutPanel: TsPanel
    Left = 0
    Top = 40
    Width = 623
    Height = 309
    Align = alClient
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    TabOrder = 0
    SkinData.SkinSection = 'PANEL'
  end
  object TopPanel: TsPanel
    Left = 0
    Top = 0
    Width = 623
    Height = 40
    Align = alTop
    BevelInner = bvRaised
    BevelOuter = bvLowered
    ParentBackground = False
    TabOrder = 1
    SkinData.SkinSection = 'PANEL'
    object ShiftRightBtn: TsSpeedButton
      Left = 450
      Top = 12
      Width = 18
      Height = 18
      Action = ShiftRightAction
      Glyph.Data = {
        C6000000424DC60000000000000076000000280000000A0000000A0000000100
        0400000000005000000000000000000000001000000010000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFF00
        0000FFFFFFFFFF000000FFFFFF0FFF000000FFFFFF00FF000000F00000000F00
        0000F00000000F000000FFFFFF00FF000000FFFFFF0FFF000000FFFFFFFFFF00
        0000FFFFFFFFFF000000}
      SkinData.SkinSection = 'SPEEDBUTTON'
    end
    object ShiftLeftBtn: TsSpeedButton
      Left = 414
      Top = 12
      Width = 18
      Height = 18
      Action = ShiftLeftAction
      Glyph.Data = {
        C6000000424DC60000000000000076000000280000000A0000000A0000000100
        0400000000005000000000000000000000001000000010000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFF0FFFFFFFFFFFFFF00FFFFFFFFFFFFF00000000FFF
        FFFFF00000000FFFFFFFFF00FFFFFFFFFFFFFFF0FFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFF}
      SkinData.SkinSection = 'SPEEDBUTTON'
    end
    object ShiftUpBtn: TsSpeedButton
      Left = 432
      Top = 2
      Width = 18
      Height = 18
      Action = ShiftUpAction
      Glyph.Data = {
        C6000000424DC60000000000000076000000280000000A0000000A0000000100
        0400000000005000000000000000000000001000000010000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFF00
        0000FFFF00FFFF000000FFFF00FFFF000000FFFF00FFFF000000FFFF00FFFF00
        0000FFFF00FFFF000000FF000000FF000000FFF0000FFF000000FFFF00FFFF00
        0000FFFFFFFFFF000000}
      SkinData.SkinSection = 'SPEEDBUTTON'
    end
    object ShiftDownBtn: TsSpeedButton
      Left = 432
      Top = 20
      Width = 18
      Height = 18
      Action = ShiftDownAction
      Glyph.Data = {
        C6000000424DC60000000000000076000000280000000A0000000A0000000100
        0400000000005000000000000000000000001000000010000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFF00
        0000FFFF00FFFF000000FFF0000FFF000000FF000000FF000000FFFF00FFFF00
        0000FFFF00FFFF000000FFFF00FFFF000000FFFF00FFFF000000FFFF00FFFF00
        0000FFFFFFFFFF000000}
      SkinData.SkinSection = 'SPEEDBUTTON'
    end
    object RightPanel: TsPanel
      Left = 541
      Top = 2
      Width = 80
      Height = 36
      Align = alRight
      BevelOuter = bvNone
      ParentBackground = False
      TabOrder = 3
      SkinData.SkinSection = 'PANEL'
      object CloseBtn: TsButton
        Left = 2
        Top = 6
        Width = 76
        Height = 25
        Caption = '&Close'
        ModalResult = 1
        TabOrder = 0
        WordWrap = False
        SkinData.SkinSection = 'BUTTON'
      end
    end
    object NewKeyBtn: TsButton
      Left = 8
      Top = 8
      Width = 75
      Height = 24
      Action = NewKeyAction
      TabOrder = 0
      WordWrap = False
      SkinData.SkinSection = 'BUTTON'
    end
    object SaveLayoutBtn: TsButton
      Left = 168
      Top = 8
      Width = 75
      Height = 24
      Action = SaveLayoutAction
      TabOrder = 1
      WordWrap = False
      SkinData.SkinSection = 'BUTTON'
    end
    object LoadLayoutBtn: TsButton
      Left = 88
      Top = 8
      Width = 75
      Height = 24
      Action = LoadLayoutAction
      TabOrder = 2
      WordWrap = False
      SkinData.SkinSection = 'BUTTON'
    end
    object ClearBtn: TsButton
      Left = 248
      Top = 8
      Width = 75
      Height = 24
      Action = ClearAllBtn
      TabOrder = 4
      WordWrap = False
      SkinData.SkinSection = 'BUTTON'
    end
    object KeyFontBtn: TsButton
      Left = 328
      Top = 8
      Width = 75
      Height = 24
      Action = SetFontAction
      TabOrder = 5
      WordWrap = False
      SkinData.SkinSection = 'BUTTON'
    end
  end
  object ActionList: TActionList
    Left = 576
    Top = 288
    object NewKeyAction: TAction
      Caption = '&New Key'
      OnExecute = NewKeyActionExecute
    end
    object DeleteKeyAction: TAction
      Caption = '&Delete Key'
      OnExecute = DeleteKeyActionExecute
    end
    object PropertiesAction: TAction
      Caption = '&Properties'
      OnExecute = PropertiesActionExecute
    end
    object SaveLayoutAction: TAction
      Caption = '&Save Layout'
      OnExecute = SaveLayoutActionExecute
    end
    object LoadLayoutAction: TAction
      Caption = '&Load Layout'
      OnExecute = LoadLayoutActionExecute
    end
    object ClearAllBtn: TAction
      Caption = '&Clear All'
      OnExecute = ClearAllBtnExecute
    end
    object ShiftUpAction: TAction
      OnExecute = ShiftUpActionExecute
    end
    object ShiftDownAction: TAction
      OnExecute = ShiftDownActionExecute
    end
    object ShiftLeftAction: TAction
      OnExecute = ShiftLeftActionExecute
    end
    object ShiftRightAction: TAction
      OnExecute = ShiftRightActionExecute
    end
    object SetFontAction: TAction
      Caption = 'Set Key &Font'
      OnExecute = SetFontActionExecute
    end
  end
  object PopupMenu: TPopupMenu
    Left = 548
    Top = 288
    object DeleteKey1: TMenuItem
      Action = DeleteKeyAction
    end
    object Properties1: TMenuItem
      Action = PropertiesAction
    end
  end
  object SaveDialog: TSaveDialog
    DefaultExt = 'kly'
    Filter = 'Keyboard Layout (*.kly)|*.kly'
    InitialDir = 'c:\'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Title = 'Save Keyboard Layout'
    Left = 520
    Top = 288
  end
  object OpenDialog: TOpenDialog
    DefaultExt = 'kly'
    Filter = 'Keyboard Layout (*.kly)|*.kly'
    InitialDir = 'c:\'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Title = 'Load Keyboard Layout'
    Left = 492
    Top = 288
  end
  object FontDialog: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Options = [fdEffects, fdForceFontExist]
    Left = 464
    Top = 288
  end
end
