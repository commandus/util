unit LinkChecker_U;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Grids, IEDownload, ComCtrls, StdCtrls;

type
  TForm1 = class(TForm)
    IEDownload1: TIEDownload;
    StringGrid1: TStringGrid;
    Button1: TButton;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    function IEDownload1Response(Sender: TBSCB; dwResponseCode: Cardinal;
      szResponseHeaders, szRequestHeaders: PWideChar;
      out szAdditionalRequestHeaders: PWideChar): HRESULT;
    procedure IEDownload1Progress(Sender: TBSCB; ulProgress, ulProgressMax,
      ulStatusCode: Cardinal; szStatusText: PWideChar; ElapsedTime, Speed,
      EstimatedTime: string);
    procedure IEDownload1Complete(Sender: TBSCB; Stream: TStream;
      Result: HRESULT);
    procedure FormDestroy(Sender: TObject);
    procedure RadioButton1Click(Sender: TObject);
    procedure RadioButton2Click(Sender: TObject);
  private
    { Private declarations }
    procedure ClearGrid;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  Links: TStringlist;

implementation

{$R *.DFM}

procedure TForm1.FormCreate(Sender: TObject);
begin
  Links := TStringlist.create;
  Links.LoadFromFile('links.txt');
end;

procedure TForm1.FormShow(Sender: TObject);
var
  x: Integer;
begin
  Stringgrid1.Cells[0, 0] := 'Url';
  Stringgrid1.Cells[1, 0] := 'Status';
  Stringgrid1.Cells[2, 0] := 'Result';
  for x := 0 to links.count - 1 do
  begin
    Stringgrid1.RowCount := x + 2;
    Stringgrid1.Cells[0, x + 1] := links[x];
  end;
end;

procedure TForm1.ClearGrid;
var
  X, y: Integer;
begin
  for x := 1 to Links.Count do
    for y := 1 to 2 do
      Stringgrid1.Cells[y, x] := '';
end;


procedure TForm1.Button1Click(Sender: TObject);
var
  x: Integer;
begin
  ClearGrid;
  for x := 0 to links.count - 1 do IEDownload1.Go(Links[x]);
end;



function TForm1.IEDownload1Response(Sender: TBSCB;
  dwResponseCode: Cardinal; szResponseHeaders, szRequestHeaders: PWideChar;
  out szAdditionalRequestHeaders: PWideChar): HRESULT;
begin
// We only need the responseheaders, so stop further downloading...
  Result := E_ABORT;
end;



procedure TForm1.IEDownload1Progress(Sender: TBSCB; ulProgress,
  ulProgressMax, ulStatusCode: Cardinal; szStatusText: PWideChar;
  ElapsedTime, Speed, EstimatedTime: string);
begin
  StringGrid1.Cells[1, Links.IndexOf(Sender.Url) + 1] := BindstatusText(ulStatusCode);
  StringGrid1.Repaint; // For Synchronous download only
  end;

procedure TForm1.IEDownload1Complete(Sender: TBSCB; Stream: TStream;
  Result: HRESULT);
begin
  if Sender.Responsecode = 200 then
    StringGrid1.Cells[1, Links.IndexOf(Sender.Url) + 1] := 'Finished...'
  else if Sender.ResponseCode > 400 then
      StringGrid1.Cells[1, Links.IndexOf(Sender.Url) + 1] := ResponseCodeText(Sender.ResponseCode)
  else
      StringGrid1.Cells[1, Links.IndexOf(Sender.Url) + 1] := ErrorText(result);

  StringGrid1.Cells[2, Links.IndexOf(Sender.Url) + 1] := ResponseCodeText(Sender.ResponseCode);
  StringGrid1.Repaint; // For Synchronous download only
  end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  links.free;
end;

procedure TForm1.RadioButton1Click(Sender: TObject);
begin
//Download Synchronous
Iedownload1.Options:=[GetNewestVersion, NoWriteCache, PullData];
end;

procedure TForm1.RadioButton2Click(Sender: TObject);
begin
//Download ASynchronous
Iedownload1.Options:=[Asynchronous, AsyncStorage, GetNewestVersion, NoWriteCache, PullData];
end;

end.

