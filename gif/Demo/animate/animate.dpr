program animate;

uses
  Forms,
  main in 'main.pas' {FormAnimate};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Animated GIF builder';
  Application.CreateForm(TFormAnimate, FormAnimate);
  Application.Run;
end.
