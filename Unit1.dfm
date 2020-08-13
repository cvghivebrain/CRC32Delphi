object Form1: TForm1
  Left = 192
  Top = 124
  BorderStyle = bsDialog
  Caption = 'CRC32 Delphi Test'
  ClientHeight = 89
  ClientWidth = 577
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
    Height = 33
    Enabled = False
    TabOrder = 0
  end
  object edString: TEdit
    Left = 8
    Top = 48
    Width = 449
    Height = 33
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
  object dlgFile: TOpenDialog
    Left = 544
    Top = 16
  end
end
