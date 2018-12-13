program gif2bmp;

uses
  Forms,
  main in 'main.pas' {FormMain};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'GIF to BMP converter';
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
