library counter;

uses
  HTTPApp,
  ISAPIApp,
  classes,
  main in 'main.pas' {WebModuleCounter: TWebModule};

{$R *.RES}

exports
  GetExtensionVersion,
  HttpExtensionProc,
  TerminateExtension;

begin
  Application.Initialize;
  Application.CreateForm(TWebModuleCounter, WebModuleCounter);
  Application.Run;
end.
