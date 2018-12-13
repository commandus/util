program Quantize;

uses
  Forms,
  Main in 'Main.pas' {FormMain};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'TGIFImage quantization demo';
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
