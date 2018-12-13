unit
  HtmlDialog;

interface

uses
  SysUtils, Windows, Classes, UrlMon, Dialogs, ActiveX;

type
  TShowHTMLDialogFn = function(const hwndParent: HWND; const pmk: IMoniker;
    const pvarArgIn: Variant; const pchOptions: POleStr;
    var pvarArgOut: Variant): HResult stdcall;

var
  ShowHTMLDialog: TShowHTMLDialogFn;

implementation

var
  hInstHTML: THandle;

procedure ShowAboutDialog;
var
  Url: string;
  Moniker: IMoniker;
  v1, v2: Variant;
  F: string;
begin
  // <HTML id=dlgAbout STYLE="width: 25.9em; height: 18em">

  f:= 'dialogHeight: 180px; dialogWidth: 405px; dialogTop: px; dialogLeft: px;' +
    ' center: 1; dialoghide: 0; edge: raised; help: 0; resizable: 0; scroll: 0;' +
    ' status: 0; unadorned: 0;';
  Url:= 'res://' + 'a.dll' + '/about';
  {
  ResURLStr:=StringToOleStr('res://'+StrPas(ModuleName)+'/RT_HTML/HTML_RESOURCE');
  OleCheck(CreateURLMoniker(Nil, ResURLStr, pmk));
  SysFreeString(ResURLStr);

  OleCheck(ShowHTMLDialog( Handle , pmk, InParam, nil, OutParam));

  if VarType(OutParam)=varOleStr then
    MessageDlg('Dialog returned '+OutParam,mtInformation,[mbOK],0)
  else
    MessageDlg('Dialog canceled!',mtInformation,[mbOK],0);
  InParam:=Unassigned;
  OutParam:=Unassigned;

  }
  CreateUrlMoniker(nil, StringToOleStr(Url), Moniker);
  ShowHTMLDialog(0, Moniker, v1, StringToOleStr(F), v2);
  {Release moniker}
end;

initialization
  // RT_HTML
  hInstHTML:= LoadLibrary('MSHTML.DLL');
  if hInstHTML = 0
  then MessageDlg('Unable to load library MSHTML.DLL) - Nothing will work!', mtError, [mbOK], 0);
  if hInstHTML > 0
  then @ShowHTMLDialog:= GetProcAddress(hInstHTML,'ShowHTMLDialog')
  else @ShowHTMLDialog:= Nil;

finalization
  FreeLibrary(hInstHTML);

end.
