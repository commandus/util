program reduce;

uses
  Forms,
  main in 'main.pas' {FormMain};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Color reduction';
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
