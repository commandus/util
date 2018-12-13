program resource;

uses
  Forms,
  main in 'main.pas' {FormDemo};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Load GIF from resource demo';
  Application.CreateForm(TFormDemo, FormDemo);
  Application.Run;
end.
