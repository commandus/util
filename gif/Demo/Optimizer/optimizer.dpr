program Optimizer;

uses
  Forms,
  main in 'main.pas' {FormMain};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'GIF Optimizer';
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
