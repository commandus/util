unit
  dbchart;
(*##*)
(*******************************************************************************
*                                                                             *
*   D  B  C  H  A  R  T  interbase/firebird chart drawing routines             *
*                                                                             *
*   Copyright © 2004 Andrei Ivanov. All rights reserved.                       *
*                                                                             *
*   Conditional defines:                                                       *
*   Next AStyle parameter values allowed:                                     *                                    *
*   Line                                                                       *
*   Bar                                                                       *
*   HBar                                                                       *
*   Area                                                                      *
*   Point                                                                      *
*   Pie                                                                       *
*   FastLine                                                                   *
*                                                                             *
*                                                                              *
*                                                                             *
*   Last Revision: Nov 14 2004                                                 *
*   Last fixes   :                                                            *
*   Lines        : 260                                                         *
*   History      : see CHANGES.TXT file                                       *
*   Printed      : ---                                                         *
*                                                                             *
********************************************************************************)
(*##*)


interface
uses
  Types, SysUtils, Classes, Graphics,
  IBHeader, IBDatabase, IBSQL,
  Chart, TeeFunci, TeeProcs, TeEngine, Series,
  util1, GifImage;

procedure DrawChart(AResourceForm: TComponent; const AImgAlias, ASelAlias: String; AParameters: TStrings;
  const ATitle, ABottomAxisLabel, ALeftAxisLabel: String; const AStyle: String; AChart: TChart);

procedure DrawBitmap(AResourceForm: TComponent; const AImgAlias, ASelAlias: String; AParameters: TStrings;
  const ATitle, ABottomAxisLabel, ALeftAxisLabel: String; const AStyle: String; var ABitmap: TBitmap);

procedure DrawGif(AResourceForm: TComponent; const AImgAlias, ASelAlias: String; AParameters: TStrings;
  const ATitle, ABottomAxisLabel, ALeftAxisLabel: String; const AStyle: String; var AGif: TGifImage);

function ReadComponent(strm: TStream; ABinary: Boolean; ARoot: TComponent): TComponent;

procedure WriteComponent(strm: TStream; Component: TComponent; ABinary: Boolean);

implementation

procedure WriteComponent(strm: TStream; Component: TComponent; ABinary: Boolean);
var
  BinStream:TMemoryStream;
  sof: TStreamOriginalFormat;
begin
  BinStream := TMemoryStream.Create;
  if ABinary
  then sof:= sofBinary
  else sof:= sofText;
  try
    BinStream.WriteComponent(Component);
    BinStream.Seek(0, soFromBeginning);
    ObjectBinaryToText(BinStream, Strm, sof);
  finally
    BinStream.Free
  end;
end;

function ReadComponent(strm: TStream; ABinary: Boolean; ARoot: TComponent): TComponent;
var
  BinStream: TMemoryStream;
begin
  if ABinary
  then Result:= Strm.ReadComponent(ARoot)
  else begin
    try
      BinStream := TMemoryStream.Create;
      try
        ObjectTextToBinary(Strm, BinStream);
        BinStream.Seek(0, soFromBeginning);
        Result:= BinStream.ReadComponent(ARoot);
      finally
        BinStream.Free;
      end;
    finally
    end;
  end;
end;

function ComponentToString(Component: TComponent): string;
var
  StrStream: TStringStream;
begin
  StrStream:= TStringStream.Create('');
  WriteComponent(StrStream, Component, False);
  try
    Result:= StrStream.DataString;
  finally
    StrStream.Free;
  end;
end;

function StringToComponent(Value: string; ARoot: TComponent): TComponent;
var
  StrStream: TStringStream;
begin
  StrStream:= TStringStream.Create(Value);
  try
    Result:= ReadComponent(StrStream, False, ARoot);
  finally
    StrStream.Free;
  end;
end;

procedure DrawChart(AResourceForm: TComponent; const AImgAlias, ASelAlias: String; AParameters: TStrings;
  const ATitle, ABottomAxisLabel, ALeftAxisLabel: String; const AStyle: String; AChart: TChart);
const
  MYCOLORLIST: array [0..7] of TColor = (clLime, clGreen, clYellow, clRed, clNavy, clGreen, clOlive, clSilver);
var
  i, idx: Integer;
  cl: TColor;
  sql: TIBSQL;
  sercnt: Integer;
  cs: TChartSeries;
  sqlt, pname, pval, savesql: String;
  // fldtype: Integer;
  // isTime, isNum: Boolean;
  chrt: TChart;
begin
  TComponent(chrt):= AResourceForm.FindComponent(AImgAlias);
  if (not Assigned(chrt)) or (not (chrt is TChart))
  then Exit;

  TComponent(sql):= AResourceForm.FindComponent(ASelAlias);
  if (not Assigned(sql)) or (not (sql is TIBSQL))
  then Exit;

  AChart.Assign(chrt);
  AChart.Width:= TChart(chrt).Width;
  AChart.Height:= TChart(chrt).Height;

  AChart.Title.Text.Text:= ATitle;
  AChart.BottomAxis.Title.Caption:= ABottomAxisLabel;
  AChart.LeftAxis.Title.Caption:= ALeftAxisLabel;

  if not sql.Database.Connected
  then sql.Database.Connected:= True;

  if not sql.Transaction.InTransaction
  then sql.Transaction.StartTransaction;

  sql.Close;
  
  sqlt:= sql.SQL.Text;
  savesql:= sqlt;

  for idx:= 0 to AParameters.Count - 1 do begin
    pname:= AParameters.Names[idx];
    pval:= AParameters.ValueFromIndex[idx];
    while util1.ReplaceStr(sqlt, false, ':' + pname, pval) do;
  end;
  sql.SQL.Text:= sqlt;

  sql.Prepare;

  sql.ExecQuery;

  AChart.RemoveAllSeries;

  // fldtype:= sql.Fields[0].SQLType;
  // isTime:= (fldtype = SQL_TIMESTAMP) or (fldtype = SQL_TYPE_TIME) or (fldtype = SQL_TYPE_DATE);
  // isNum:= (not isTime) and ((fldtype = SQL_DOUBLE) or (fldtype = SQL_FLOAT) or (fldtype = SQL_LONG) or (fldtype = SQL_SHORT) or (fldtype = SQL_D_FLOAT) or (fldtype = SQL_INT64));

//  if isTime then ChartTest.See BottomAxis. IsDateTime:= True;

  sercnt:= sql.Current.Count - 1;
  for i:= 0 to sercnt - 1 do begin
    cs:= Nil;
    if ANSICompareText(AStyle, 'line') = 0 then cs:= TLineSeries.Create(AChart);
    if ANSICompareText(AStyle, 'bar') = 0 then cs:= TBarSeries.Create(AChart);
    if ANSICompareText(AStyle, 'hbar') = 0 then cs:= THorizBarSeries.Create(AChart);
    if ANSICompareText(AStyle, 'area') = 0 then cs:= TAreaSeries.Create(AChart);
    if ANSICompareText(AStyle, 'point') = 0 then cs:= TPointSeries.Create(AChart);
    if ANSICompareText(AStyle, 'pie') = 0 then cs:= TPieSeries.Create(AChart);
    if cs = Nil
    then Exit;

    cs.SeriesColor:= MYCOLORLIST[i mod 8];
    cs.Title:= sql.Fields[i + 1].Name;
    AChart.AddSeries(cs);
  end;

  // ChartTest.BottomAxis.ExactDateTime:= True;

  i:= 0;
  while not sql.Eof do begin
    for idx:= 0 to sercnt - 1 do begin
      if Odd(idx)
      then cl:= clRed // ;clLime
      else cl:= clLime;// $40F040;
      AChart.Series[idx].AddXY(i, sql.Fields[idx + 1].AsDouble, sql.Fields[0].AsTrimString, cl);
    end;
    sql.Next;
    Inc(i);
  end;
  sql.Close;
  sql.SQL.Text:= savesql;

  // sql.Database.Connected:= False;
end;

procedure DrawBitmap(AResourceForm: TComponent; const AImgAlias, ASelAlias: String; AParameters: TStrings;
  const ATitle, ABottomAxisLabel, ALeftAxisLabel: String; const AStyle: String; var ABitmap: TBitmap);
var
  chart: TChart;
  r: TRect;
begin
  chart:= TChart.Create(Nil);
  DrawChart(AResourceForm, AImgAlias, ASelAlias, AParameters,
    ATitle, ABottomAxisLabel, ALeftAxisLabel, AStyle, Chart);
  with r do begin
    Left:= 0;
    Top:= 0;
    Right:= Chart.Width - 1;
    Bottom:= Chart.Height - 1;
  end;

  if not Assigned(ABitmap)
  then ABitmap:= TBitmap.Create;

  with ABitmap do begin
    PixelFormat:= pf4bit;
    Width:= Chart.Width;
    Height:= Chart.Height;
    Chart.Draw(Canvas, r);
  end;
  chart.Free;
end;

procedure DrawGif(AResourceForm: TComponent; const AImgAlias, ASelAlias: String; AParameters: TStrings;
  const ATitle, ABottomAxisLabel, ALeftAxisLabel: String; const AStyle: String; var AGif: TGifImage);
var
  chart: TChart;
  r: TRect;
  bmp: TBitmap;
begin
  bmp:= Nil;
  DrawBitmap(AResourceForm, AImgAlias, ASelAlias, AParameters,
    ATitle, ABottomAxisLabel, ALeftAxisLabel, AStyle, bmp);

  if not Assigned(AGif)
  then AGif:= GifImage.TGIFImage.Create;
  AGif.Assign(bmp);
  bmp.Free;
end;

end.
