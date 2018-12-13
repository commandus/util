program ThreadPaint;

uses
  Forms,
  main in 'main.pas' {FormMain},
  single in 'single.pas' {FormSingle},
  multiple in 'multiple.pas' {FormMultiple},
  native in 'native.pas' {FormNative};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Threaded Paint Demo';
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
