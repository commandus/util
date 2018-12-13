program bmp2gif;

uses
  Forms,
  main in 'main.pas' {FormMain};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'BMP to GIF converter';
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
