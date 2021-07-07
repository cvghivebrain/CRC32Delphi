object Form1: TForm1
  Left = 192
  Top = 124
  BorderStyle = bsDialog
  Caption = 'CRC32/SHA-1/MD5 Delphi Test'
  ClientHeight = 89
  ClientWidth = 801
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object edFile: TEdit
    Left = 8
    Top = 8
    Width = 449
    Height = 21
    Enabled = False
    TabOrder = 0
  end
  object edString: TEdit
    Left = 8
    Top = 48
    Width = 449
    Height = 21
    TabOrder = 1
  end
  object btnFile: TButton
    Left = 464
    Top = 8
    Width = 105
    Height = 33
    Caption = 'CRC32 File...'
    TabOrder = 2
    OnClick = btnFileClick
  end
  object btnString: TButton
    Left = 464
    Top = 48
    Width = 105
    Height = 33
    Caption = 'CRC32 String'
    TabOrder = 3
    OnClick = btnStringClick
  end
  object btnString2: TButton
    Left = 575
    Top = 48
    Width = 106
    Height = 33
    Caption = 'SHA-1 String'
    TabOrder = 4
    OnClick = btnString2Click
  end
  object btnFile2: TButton
    Left = 576
    Top = 8
    Width = 105
    Height = 33
    Caption = 'SHA-1 File...'
    TabOrder = 5
    OnClick = btnFile2Click
  end
  object btnFile3: TButton
    Left = 688
    Top = 8
    Width = 105
    Height = 33
    Caption = 'MD5 File...'
    TabOrder = 6
    OnClick = btnFile3Click
  end
  object btnString3: TButton
    Left = 688
    Top = 48
    Width = 105
    Height = 33
    Caption = 'MD5 String'
    TabOrder = 7
    OnClick = btnString3Click
  end
  object dlgFile: TOpenDialog
    Left = 544
    Top = 16
  end
end
