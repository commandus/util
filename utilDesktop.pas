unit
  utilDesktop;

interface
uses
  Windows, SysUtils;

function IsActiveDesktopEnable: Boolean;

implementation

uses
   ComObj, ShlObj, ActiveX;

 // Check if Active Desktop is enabled (2)
function IsActiveDesktopEnable: Boolean;
const
  CLSID_ActiveDesktop: TGUID = '{75048700-EF1F-11D0-9888-006097DEACF9}';
var
  ActiveDesk: IActiveDesktop;
  ComponentsOpt: TComponentsOpt;
  hr: HRESULT;
  dwReserved: DWORD;
 begin
  ZeroMemory(@ComponentsOpt, SizeOf(TComponentsOpt));
  ComponentsOpt.dwSize:= SizeOf(TComponentsOpt);
  hr:= CoCreateInstance(CLSID_ActiveDesktop, nil, CLSCTX_INPROC_SERVER,
    CLSID_ActiveDesktop, ActiveDesk);
  if SUCCEEDED(hr) then begin
    hr:= ActiveDesk.GetDesktopItemOptions(ComponentsOpt, dwReserved);
    // ActiveDesk._Release;
  end;
  Result := ComponentsOpt.fActiveDesktop;
 end;

end.
