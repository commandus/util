object Form1: TForm1
  Left = 182
  Top = 130
  Width = 696
  Height = 480
  Caption = 'LinkChecker Demo (IEDownload)'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object StringGrid1: TStringGrid
    Left = 15
    Top = 9
    Width = 647
    Height = 358
    ColCount = 3
    DefaultRowHeight = 15
    FixedCols = 0
    TabOrder = 0
    ColWidths = (
      258
      263
      118)
  end
  object Button1: TButton
    Left = 274
    Top = 400
    Width = 103
    Height = 25
    Caption = 'Check Links'
    TabOrder = 1
    OnClick = Button1Click
  end
  object RadioButton1: TRadioButton
    Left = 80
    Top = 392
    Width = 113
    Height = 17
    Caption = 'Synchronous'
    TabOrder = 2
    OnClick = RadioButton1Click
  end
  object RadioButton2: TRadioButton
    Left = 80
    Top = 411
    Width = 113
    Height = 17
    Caption = 'Asynchronous'
    Checked = True
    TabOrder = 3
    TabStop = True
    OnClick = RadioButton2Click
  end
  object IEDownload1: TIEDownload
    TimeOut = 60000
    Codepage = Ansi
    Method = Get
    Options = [Asynchronous, AsyncStorage, GetNewestVersion, NoWriteCache, PullData]
    UrlEncode = []
    Security.InheritHandle = False
    Range.RangeBegin = 0
    Range.RangeEnd = 0
    AdditionalHeader.Strings = (
      'Content-Type: application/x-www-form-urlencoded')
    UserAgent = 'Mozilla/4.0 (compatible; MSIE 5.0; Win32)'
    OnProgress = IEDownload1Progress
    OnResponse = IEDownload1Response
    OnComplete = IEDownload1Complete
    Left = 652
    Top = 400
  end
end
