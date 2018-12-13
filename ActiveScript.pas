unit
  ActiveScript;

interface
uses
  Windows, ActiveX, Classes,
  MSScriptControl_TLB;
type
  IActiveScriptError = interface(IUnknown)
  // Получить описание ошибки
    function GetExceptionInfo(out pexcepinfo: EXCEPINFO): HResult; stdcall;
  // Получить позицию ошибки в тексте // контекст (см. ParseSriptText) // номер строки // номер символа в строке
    function GetSourcePosition(out pdwSourceContext: DWORD; out pulLineNumber: ULONG; out plCharacterPosition: Integer):
      HResult;
      stdcall;
  // Строка кода, вызвавшая ошибку
    function GetSourceLineText(out pbstrSourceLine: WideString): HResult; stdcall;
  end;

  IActiveScriptSite = interface(IUnknown)
    // Запрос языка носителя
    function GetLCID(out plcid: LCID): HResult; stdcall;
    // Запрос прикладного объекта // имя объекта // запрашиваемая информация // интерфейс объекта // инфомация о типе объекта
    function GetItemInfo(pstrName: WideString; dwReturnMask: DWORD;
      out ppiunkItem: IUnknown; out ppti: ITypeInfo): HResult; stdcall;
    // Запрос версии сценария
    function GetDocVersionString(out pbstrVersion: WideString): HResult; stdcall;
    // Уведомление о завершении // возвращаемое значение // информация об ошибке
    function OnScriptTerminate(var pvarResult: OleVariant; var pexcepinfo: EXCEPINFO): HResult; stdcall;
    // Уведомление об изменении состояния // новое состояние
    function OnStateChange(ssScriptState: TOleEnum): HResult; stdcall;
    // Уведомление об ошибке
    function OnScriptError(const pscripterror: IActiveScriptError): HResult; stdcall;
    // Начало исполнения
    function OnEnterScript: HResult; stdcall;
    // Окончание исполнения
    function OnLeaveScript: HResult; stdcall;
  end;

  IActiveScriptSiteWindow = interface(IUnknown)
    function GetWindow(out phwnd: HWND): HResult; stdcall;
    function EnableModeless(fEnable: BOOL): HResult; stdcall;
  end;

  TActiveScriptSite = class(TComponent, IActiveScriptSite, IActiveScriptSiteWindow)
  private
  { Private declarations }
    FWindowHandle: HWND;
  protected
  {IActiveScriptSite}
    function GetLCID(out plcid: LCID): HResult; stdcall;
    function GetItemInfo(pstrName: WideString; dwReturnMask: DWORD; out ppiunkItem: IUnknown; out ppti: ITypeInfo): HResult; stdcall;
    function GetDocVersionString(out pbstrVersion: WideString): HResult; stdcall;
    function OnScriptTerminate(var pvarResult: OleVariant; var pexcepinfo: EXCEPINFO): HResult; stdcall;
    function OnStateChange(ssScriptState: TOleEnum): HResult; stdcall;
    function OnScriptError(const pscripterror: IActiveScriptError): HResult; stdcall;
    function OnEnterScript: HResult; stdcall;
    function OnLeaveScript: HResult; stdcall;
  protected
  { IActiveSriptSiteWindow}
    function GetWindow(out phwnd: HWND): HResult; stdcall;
    function EnableModeless(fEnable: BOOL): HResult; stdcall;
  public
  end;

implementation

function TActiveScriptSite.GetLCID(out plcid: LCID): HResult;
begin
  plcid:= GetSystemDefaultLCID;
  Result:= S_OK;
end;

function TActiveScriptSite.GetWindow(out phwnd: HWND): HResult;
begin
  phwnd:= FWindowHandle;
  Result:= S_OK;
end;

function TActiveScriptSite.GetDocVersionString(out pbstrVersion: WideString): HResult;
begin
  Result:= E_NOTIMPL;
end;

function TActiveScriptSite.GetItemInfo(pstrName: WideString; dwReturnMask: DWORD;
  out ppiunkItem: IUnknown; out ppti: ITypeInfo): HResult;
begin
  Result:= E_NOTIMPL;
end;

end.

