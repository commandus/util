object Form1: TForm1
  Left = 309
  Top = 202
  Width = 327
  Height = 423
  Caption = 'Download Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object memo1: TMemo
    Left = 16
    Top = 16
    Width = 233
    Height = 113
    TabOrder = 0
  end
  object memo2: TMemo
    Left = 16
    Top = 144
    Width = 233
    Height = 177
    TabOrder = 1
  end
  object Button1: TButton
    Left = 74
    Top = 348
    Width = 115
    Height = 25
    Caption = 'Start Download'
    TabOrder = 2
    OnClick = Button1Click
  end
  object IEDownload1: TIEDownload
    TimeOut = 0
    Codepage = Ansi
    Method = Get
    Options = [Asynchronous, AsyncStorage, GetNewestVersion, NoWriteCache, PullData]
    UrlEncode = []
    Security.InheritHandle = False
    Range.RangeBegin = 0
    Range.RangeEnd = 0
    AdditionalHeader.Strings = (
      'Content-Type: application/x-www-form-urlencoded')
    UserAgent = 'Mozilla/4.0 (compatible; MSIE 6.0; Win32)'
    OnProgress = IEDownload1Progress
    OnComplete = IEDownload1Complete
    OnData = IEDownload1Data
    Left = 216
    Top = 336
  end
end
