unit Downloaddemo_U;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  urlmon, IEDownload, StdCtrls, wininet;

type
  TForm1 = class(TForm)
    Button1: TButton;
    memo1: TMemo;
    memo2: TMemo;
    IEDownload1: TIEDownload;
    procedure Button1Click(Sender: TObject);
    procedure IEDownload1Progress(Sender: TBSCB; ulProgress, ulProgressMax,
      ulStatusCode: Cardinal; szStatusText: PWideChar; ElapsedTime, Speed,
      EstimatedTime: string);
    procedure IEDownload1Data(Sender: TBSCB; var Buffer: PByte;
      var BufLength: Cardinal);
    procedure IEDownload1Complete(Sender: TBSCB; Stream: TStream;
      Result: HRESULT);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.Button1Click(Sender: TObject);
begin
  IEDownload1.go('http://localhost');
end;

procedure TForm1.IEDownload1Progress(Sender: TBSCB; ulProgress,
  ulProgressMax, ulStatusCode: Cardinal; szStatusText: PWideChar;
  ElapsedTime, Speed, EstimatedTime: string);
var
  s: string;
begin
  s := BindstatusText(ulStatusCode);
  if ulStatusCode = BINDSTATUS_DOWNLOADINGDATA then
    s := S + ' (' + InttoStr(ulProgress) + '/' + InttoStr(ulProgressMax) + ')';
  memo1.lines.add(s);
end;

procedure TForm1.IEDownload1Data(Sender: TBSCB; var Buffer: PByte;
  var BufLength: Cardinal);
begin
  memo2.lines.add(pchar(buffer));
end;

procedure TForm1.IEDownload1Complete(Sender: TBSCB; Stream: TStream;
  Result: HRESULT);
begin
  if (Result = S_OK) then
    memo1.lines.add('Download complete...')
  else
    memo1.lines.add(Errortext(Result) + ' ' + ResponseCodeText(Sender.ResponseCode));
end;

end.

