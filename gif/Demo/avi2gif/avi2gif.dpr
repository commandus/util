program avi2gif;

uses
  Forms,
  main in 'main.pas' {FormMain};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'AVI to GIF converter';
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
