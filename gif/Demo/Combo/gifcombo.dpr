program gifcombo;

uses
  Forms,
  main in 'main.pas' {FormMain};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'GIF Combo Box Demo';
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
