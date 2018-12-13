program gifexplore;

uses
  Forms,
  main in 'main.pas' {FormExplorer},
  about in 'about.pas' {FormAbout},
  speed in 'speed.pas' {FormAnimationSpeed};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'GIF Explorer';
  Application.CreateForm(TFormExplorer, FormExplorer);
  Application.Run;
end.
