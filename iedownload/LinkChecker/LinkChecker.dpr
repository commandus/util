program LinkChecker;

uses
  Forms,
  LinkChecker_U in 'LinkChecker_U.pas' {Form1};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
