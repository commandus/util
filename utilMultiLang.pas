unit
  utilMultiLang;

interface
uses
  SysUtils, Forms, IniFiles, StdCtrls;

function FormLangLoadIni(AForm: TForm; const ALangIniFileName: string): Integer;

implementation

const
  RES_CAPTION = 'Caption';
  RES_HINT = 'Hint';
  RES_TEXT = 'Text';

function FormLangLoadIni(AForm: TForm; const ALangIniFileName: string): Integer;
var
  i: Integer;
  LangIniFile: TIniFile;
begin
  with AForm do begin
    if (not (FileExists(ALangIniFileName))) then Exit;
    LangIniFile:= TIniFile.Create(ALangIniFileName);
    // читаем заголовок окна
    Caption:= LangIniFile.ReadString(AForm.Name, RES_CAPTION, Caption);
    Hint:= LangIniFile.ReadString(AForm.Name, RES_HINT, Hint);;
    for i:= 0 to ComponentCount - 1 do with Components[i] do begin
      if (ClassType = TButton) then
        (Components[i] as TButton).Caption := LangIniFile.ReadString(LangSection,
        name+Components[i-1].name, (Components[i-1] as TButton).Caption);

      // аналогично для других типов:
      if Components[i-1].ClassType = TLabel then
        (Components[i-1] as TLabel).Caption := LangIniFile.ReadString(LangSection,
        name+Components[i-1].name, (Components[i-1] as TLabel).Caption);
      if Components[i-1].ClassType = TEdit then
        (Components[i-1] as TEdit).Text := LangIniFile.ReadString(LangSection,
        name+Components[i-1].name, (Components[i-1] as TEdit).Text);
    // ...
    // ...
    // ...
    end;
    LangIniFile.Free; // освобождаем ресурс
  end;
end;


end.
