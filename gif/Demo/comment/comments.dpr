program comments;

uses
  Forms,
  main in 'main.pas' {Form1};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'GIF Comment Viewer';
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
