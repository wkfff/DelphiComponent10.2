object KeyAlPropertiesForm: TKeyAlPropertiesForm
  Left = 370
  Top = 216
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Key Properties'
  ClientHeight = 381
  ClientWidth = 395
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object Label7: TLabel
    Left = 8
    Top = 272
    Width = 49
    Height = 13
    Caption = 'Key Width'
  end
  object Label8: TLabel
    Left = 76
    Top = 272
    Width = 52
    Height = 13
    Caption = 'Key Height'
  end
  object FontLabel: TLabel
    Left = 232
    Top = 289
    Width = 47
    Height = 13
    Caption = 'FontLabel'
  end
  object Label9: TLabel
    Left = 12
    Top = 325
    Width = 45
    Height = 13
    Caption = 'Img index'
  end
  object OKBtn: TButton
    Left = 232
    Top = 344
    Width = 75
    Height = 25
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 6
  end
  object CancelBtn: TButton
    Left = 312
    Top = 344
    Width = 75
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 7
  end
  object KeyTypeRadioGroup: TRadioGroup
    Left = 8
    Top = 8
    Width = 245
    Height = 254
    Caption = ' Key Type '
    Columns = 2
    ItemIndex = 0
    Items.Strings = (
      'Normal'
      'Shift'
      'Caps Lock'
      'Escape'
      'Alt Gr'
      'Enter'
      'Tabulator'
      'Backspace'
      'Insert'
      'Delete'
      'Home'
      'End'
      'Page Up'
      'Page Down'
      'Left'
      'Right'
      'Up'
      'Down'
      'Function [ F1 - F12 ]')
    TabOrder = 0
    OnClick = KeyTypeRadioGroupClick
  end
  object KeyCaptionsGroupBox: TGroupBox
    Left = 264
    Top = 8
    Width = 121
    Height = 121
    Caption = ' Key Captions '
    TabOrder = 1
    object Label1: TLabel
      Left = 12
      Top = 24
      Width = 36
      Height = 13
      Caption = 'Normal:'
    end
    object Label2: TLabel
      Left = 12
      Top = 58
      Width = 24
      Height = 13
      Caption = 'Shift:'
    end
    object Label3: TLabel
      Left = 12
      Top = 92
      Width = 29
      Height = 13
      Caption = 'Alt Gr:'
    end
    object NormalCaptionEdit: TEdit
      Left = 60
      Top = 20
      Width = 49
      Height = 21
      TabOrder = 0
    end
    object ShiftCaptionEdit: TEdit
      Left = 60
      Top = 54
      Width = 49
      Height = 21
      TabOrder = 1
    end
    object AltGrCaptionEdit: TEdit
      Left = 60
      Top = 86
      Width = 49
      Height = 21
      TabOrder = 2
    end
  end
  object KeyValuesGroupBox: TGroupBox
    Left = 264
    Top = 144
    Width = 121
    Height = 118
    Caption = ' Key Values '
    TabOrder = 2
    object Label4: TLabel
      Left = 12
      Top = 24
      Width = 36
      Height = 13
      Caption = 'Normal:'
    end
    object Label5: TLabel
      Left = 12
      Top = 58
      Width = 24
      Height = 13
      Caption = 'Shift:'
    end
    object Label6: TLabel
      Left = 12
      Top = 92
      Width = 29
      Height = 13
      Caption = 'Alt Gr:'
    end
    object NormalValueEdit: TEdit
      Left = 60
      Top = 20
      Width = 49
      Height = 21
      TabOrder = 0
    end
    object ShiftValueEdit: TEdit
      Left = 60
      Top = 54
      Width = 49
      Height = 21
      TabOrder = 1
    end
    object AltGrValueEdit: TEdit
      Left = 60
      Top = 88
      Width = 49
      Height = 21
      TabOrder = 2
    end
  end
  object KeyWidthComboBox: TComboBox
    Left = 8
    Top = 288
    Width = 53
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    ItemIndex = 1
    TabOrder = 3
    Text = '1'
    Items.Strings = (
      '1/2'
      '1'
      '1 1/2'
      '2'
      '2 1/2'
      '3'
      '3 1/2'
      '4'
      '4 1/2'
      '5'
      '5 1/2'
      '6'
      '6 1/2'
      '7'
      '7 1/2'
      '8'
      '8 1/2'
      '9'
      '9 1/2'
      '10')
  end
  object KeyHeightComboBox: TComboBox
    Left = 76
    Top = 288
    Width = 53
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    ItemIndex = 1
    TabOrder = 4
    Text = '1'
    Items.Strings = (
      '1/2'
      '1'
      '1 1/2'
      '2'
      '2 1/2'
      '3'
      '3 1/2'
      '4'
      '4 1/2'
      '5'
      '5 1/2'
      '6'
      '6 1/2'
      '7'
      '7 1/2'
      '8'
      '8 1/2'
      '9'
      '9 1/2'
      '10')
  end
  object FontBtn: TButton
    Left = 148
    Top = 284
    Width = 75
    Height = 25
    Caption = '&Font'
    TabOrder = 5
    OnClick = FontBtnClick
  end
  object SpinEdit1: TSpinEdit
    Left = 12
    Top = 344
    Width = 49
    Height = 22
    MaxValue = 1000
    MinValue = -1
    TabOrder = 8
    Value = 0
  end
  object FontDialog: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Options = [fdEffects, fdForceFontExist]
    Left = 336
    Top = 268
  end
end
