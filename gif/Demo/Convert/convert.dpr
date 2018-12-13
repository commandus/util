program convert;

uses
  Forms,
  main in 'main.pas' {FormConvert};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Image Converter';
  Application.CreateForm(TFormConvert, FormConvert);
  Application.Run;
end.
