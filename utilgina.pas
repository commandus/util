unit
  utilgina;
(*##*)
(*******************************************************************************
*                                                                             *
*   U  T  I  L  G  I  N  A                                                     *
*                                                                             *
*   Copyright © 2007 Andrei Ivanov. All rights reserved.                       *
*   Routines to load GINA dll                                                 *
*   Conditional defines:                                                       *
*                                                                             *
*   Revisions    : Mar 31 2007                                                 *
*   Last revision: Mar 31 2007                                                *
*   Lines        : 144                                                         *
*   History      :                                                            *
*   Printed      : ---                                                         *
*                                                                             *
********************************************************************************)
(*##*)

interface
uses
  Windows, Registry,
  winWlx;

const
  REG_WINLOGON = '\Software\Microsoft\Windows NT\CurrentVersion\Winlogon';
  REG_VALUE = 'GinaDLL';
  REG_DEF_VALUE = 'MSGina.dll';

// return current GINA DLL full or relative file path
function GetGINA: string;

// if AGINAFileName = '' removes value from registry
function SetGINA(AGINAFileName: string): Boolean;

// return False if fails
function LoadDefaultGINAFuncs(var AValue: TGINAFuncsTable): HMODULE;

implementation

// return current GINA DLL full or relative file path

function GetGINA: string;
var
  rg: TRegistry;
begin
  Result:= REG_DEF_VALUE;
  rg:= TRegistry.Create;
  if Assigned(rg) then begin
    with rg do begin
      RootKey:= HKEY_LOCAL_MACHINE;
      if OpenKey(REG_WINLOGON, False) then begin
        try
          Result:= rg.ReadString(REG_VALUE);
        except
        end;
      end;
      Free;
    end;
  end;
  // expand file path is not necessary because PATH must
end;

// if AGINAFileName = '' removes value from registry

function SetGINA(AGINAFileName: string): Boolean;
var
  rg: TRegistry;
begin
  Result:= False;
  rg:= TRegistry.Create;
  if Assigned(rg) then begin
    with rg do begin
      RootKey:= HKEY_LOCAL_MACHINE;
      if OpenKey(REG_WINLOGON, False) then begin
        try
          if Length(AGINAFileName) > 0
            then rg.WriteString(REG_VALUE, AGINAFileName)
          else rg.DeleteValue(REG_VALUE);
          Result:= True;
        except
        end;
      end;
      Free;
    end;
  end;
end;

// return Nil if fails

function LoadDefaultGINAFuncs(var AValue: TGINAFuncsTable): HMODULE;
var
  h: HMODULE;
begin
  Result:= 0;
  FillChar(AValue, SizeOf(TGINAFuncsTable), #0);
  h:= LoadLibrary(REG_DEF_VALUE);
  if h = 0 then Exit;
  with AValue do begin
    ActivateUserShell:= GetProcAddress(h, 'WlxActivateUserShell');
    if not Assigned(ActivateUserShell) then begin
      FreeLibrary(h);
      Exit;
    end;
    DisplayLockedNotice:= GetProcAddress(h, 'WlxDisplayLockedNotice');
    if not Assigned(DisplayLockedNotice) then begin
      FreeLibrary(h);
      Exit;
    end;
    DisplaySASNotice:= GetProcAddress(h, 'WlxDisplaySASNotice');
    if not Assigned(DisplaySASNotice) then begin
      FreeLibrary(h);
      Exit;
    end;
    DisplayStatusMessage:= GetProcAddress(h, 'WlxDisplayStatusMessage');
    if not Assigned(DisplayStatusMessage) then begin
      FreeLibrary(h);
      Exit;
    end;
    GetConsoleSwitchCredentials:= GetProcAddress(h, 'WlxGetConsoleSwitchCredentials');
    if not Assigned(GetConsoleSwitchCredentials) then begin
      FreeLibrary(h);
      Exit;
    end;
    GetStatusMessage:= GetProcAddress(h, 'WlxGetStatusMessage');
    if not Assigned(GetStatusMessage) then begin
      FreeLibrary(h);
      Exit;
    end;
    Initialize:= GetProcAddress(h, 'WlxInitialize');
    if not Assigned(Initialize) then begin
      FreeLibrary(h);
      Exit;
    end;
    IsLockOk:= GetProcAddress(h, 'WlxIsLockOk');
    if not Assigned(IsLockOk) then begin
      FreeLibrary(h);
      Exit;
    end;
    IsLogoffOk:= GetProcAddress(h, 'WlxIsLogoffOk');
    if not Assigned(IslogoffOk) then begin
      FreeLibrary(h);
      Exit;
    end;
    LoggedOnSAS:= GetProcAddress(h, 'WlxLoggedOnSAS');
    if not Assigned(LoggedOnSAS) then begin
      FreeLibrary(h);
      Exit;
    end;
    LoggedOutSAS:= GetProcAddress(h, 'WlxLoggedOutSAS');
    if not Assigned(LoggedOutSAS) then begin
      FreeLibrary(h);
      Exit;
    end;
    Logoff:= GetProcAddress(h, 'WlxLogoff');
    if not Assigned(Logoff) then begin
      FreeLibrary(h);
      Exit;
    end;
    Negotiate:= GetProcAddress(h, 'WlxNegotiate');
    if not Assigned(Negotiate) then begin
      FreeLibrary(h);
      Exit;
    end;
    NetworkProviderLoad:= GetProcAddress(h, 'WlxNetworkProviderLoad');
    if not Assigned(NetworkProviderLoad) then begin
      FreeLibrary(h);
      Exit;
    end;
    RemoveStatusMessage:= GetProcAddress(h, 'WlxRemoveStatusMessage');
    if not Assigned(RemoveStatusMessage) then begin
      FreeLibrary(h);
      Exit;
    end;
    ScreenSaverNotify:= GetProcAddress(h, 'WlxScreenSaverNotify');
    if not Assigned(ScreensaverNotify) then begin
      FreeLibrary(h);
      Exit;
    end;
    Shutdown:= GetProcAddress(h, 'WlxShutdown');
    if not Assigned(Shutdown) then begin
      FreeLibrary(h);
      Exit;
    end;
    StartApplication:= GetProcAddress(h, 'WlxStartApplication');
    if not Assigned(StartApplication) then begin
      FreeLibrary(h);
      Exit;
    end;
    WkstaLockedSAS:= GetProcAddress(h, 'WlxWkstaLockedSAS');
    if not Assigned(WkstaLockedSAS) then begin
      FreeLibrary(h);
      Exit;
    end;
    Result:= h;
  end;
end;

end.

