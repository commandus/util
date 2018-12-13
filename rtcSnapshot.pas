unit
  rtcSnapshot;
(*##*)
(*******************************************************************************
*                                                                             *
*   r  t  c  S  n  a  p  s  h  o  t                                            *
*                                                                             *
*   Routines for making snapshot of top-level window of application            *
*                                                                             *
*   Copyright (c) 2007, Andrei Ivanov, RealThinComponents                      *
*   No components                                                             *
*   Conditional defines:                                                       *
*                                                                             *
*   Last Revision: Oct 31 2007                                                 *
*   Last fix:                                                                 *
*   Lines:         525                                                         *
*   History:                                                                  *
*   Notes                                                                      *
*                                                                             *
********************************************************************************)
(*##*)

interface
uses
  Windows, Classes, Graphics, Messages,
  Dialogs;

type
  // arrays ara used to return result (process lists, window handles list)
  TStringDynArray = array of String;
  TWindowDynArray = array of THandle;
  THandleDynArray = array of record
    pid: THandle;
    MainModuleHandle: THandle;
  end;

{ GetProcessHandles()
  Parameters:
    AFileNames: array of name (can be incomplete name not full path) of process to list
    AShowModules: if True, add all DLL modules linked with a process
  Returns:
    AHandles: array of process handles found
}
procedure GetProcessHandles(var AHandles: THandleDynArray; const AFileNames: TStringDynArray; const AShowModules: Boolean = False);

{ GetProcessList
  Wrapper for GetProcessHandles (returns String list instead of array of module handles)
  Parameters:
    AFileNames: array of name (can be incomplete name not full path) of process to list
    AShowModules: if True, add all DLL modules linked with a process
  Returns:
    AResult: string list with full file name of process executable. Objects[] contain a handle to the module
  Depends on GetProcessHandles()
}
procedure GetProcessList(const AFileNames: TStringDynArray; AResult: TStrings; const AShowModules: Boolean = False);

{ GetProcessWindowHandles
  Parameters:
    AFileNames: array of name (can be incomplete name not full path) of process to list
  Returns:
    pw: window handle array
}
procedure GetProcessWindowHandles(const AFileNames: TStringDynArray;  var pw: TWindowDynArray);

{ GetProcessWindowList

  Wrapper for GetProcessWindowHandles (returns String list instead of array of window handles)
  Parameters:
    AFileNames: array of name (can be incomplete name not full path) of process to list
  Returns:
    AResult: List of window caption. Objects[] refer to widnow handle
}
procedure GetProcessWindowList(const AFileNames: TStringDynArray; AResult: TStrings);

// get bitmap of window or application (by file name)
function CopyWindowDC(AHWnd: HWND; AResultDC: HDC): Boolean;
function CopyAppDC(const AApplication: String; AResultDC: HDC): Boolean;

function GetWindowBitmap(AHWnd: HWND): TBitmap;
function GetAppBitmap(const AApplication: String): TBitmap;

implementation

uses
  PSAPI, // WinNT
  TlHelp32; //Win9x

const
  // this is not limit, just initial buffer size
  MAX_PROCESS_LIST = 2048; // twice more than MS recommends
type
  // PrintWindow() is not declared in Windows.pas
  TPrintWindowFunc = function (AHwnd: HWND; AHDC: HDC; AFlags: Cardinal): BOOL; stdcall;

const
  DLL_USER32 = 'user32.dll';        // this module usually is loaded by apps so first check is it loaded
  FUNC_PRINTWINDOW = 'PrintWindow'; // this function is presented in WindowsXP/2003

var
  HDll: HINST; // USER32.DLL module instance handle
  HPrintWindow: TPrintWindowFunc; // PrintWindow() inroduced in Windows XP/2003

// check Windows platform
function GetWin32Platform: Integer;
var
  VersionInformation: TOSVersionInfo;
begin
  if Windows.GetVersionEx(VersionInformation)
    then Result:= VersionInformation.dwPlatformId
    else Result:= -1;
end;

{ GetProcessHandles()
  Parameters:
    AFileNames: array of name (can be incomplete name not full path) of process to list
    AShowModules: if True, add all DLL modules linked with a process
  Returns:
    AHandles: array of process handles found

}
procedure GetProcessHandles(var AHandles: THandleDynArray; const AFileNames: TStringDynArray; const AShowModules: Boolean = False);
var
  pe: TProcessEntry32;
  ph,  //process handle
  snap: THandle; // snapshot handle
  mh: array of HModule; // module handles
  procs: array of DWord; // process desriptors array
  cnt, count, // number of processes
  cm, cmcnt: Cardinal; // number of modules
  i, m: Integer;
  isWin32: Boolean;

  procedure AddModule(AId: Integer);
  var
    ModName: array[0..MAX_PATH] of Char; // Module name
    c, cnt: Integer;
    fn: String;
    v: Boolean;
  begin
    if isWin32
      then Move(pe.szExeFile, ModName, Length(pe.szExeFile) + 1)
      else GetModuleFileNameEx(ph, mh[AId], ModName, SizeOf(ModName));
    cnt:= Length(AFileNames);
    v:= cnt = 0;
    if not v then begin
      // compare file name
      for c:= 0 to cnt - 1 do begin
        fn:= AFileNames[c]; // SysUtils.Trim
        if (Length(fn) = 0) or (Pos(fn, ModName) > 0) then begin
          v:= True;
          Break;
        end;
      end;
    end;
    if v then begin
      cnt:= System.Length(AHandles);
      System.SetLength(AHandles, cnt + 1);
      if isWin32 then begin
        with AHandles[cnt] do begin
          pid:= pe.th32ProcessID;
          MainModuleHandle:= pe.th32ModuleID;
        end;
      end else begin
        with AHandles[cnt] do begin
          pid:= procs[i];
          MainModuleHandle:= mh[AId];
        end;
      end;
    end;
  end;

begin
  isWin32:= GetWin32Platform = VER_PLATFORM_WIN32_WINDOWS;
  if (isWin32) then  begin // Win9x
    snap:= CreateToolhelp32Snapshot(th32cs_snapprocess, 0);
    if Integer(snap) = -1
      then Exit;
    pe.dwSize:= SizeOf(pe);
    if Process32First(snap, pe) then begin
      repeat
        AddModule(0);
      until not Process32Next(snap, pe);
    end;
  end else begin // WinNT/2000/XP VER_PLATFORM_WIN32_NT
    // get process list of first processes
    cnt:= 50; // at my machine 35 processes
    repeat
      SetLength(procs, cnt);
      if not EnumProcesses(@procs[0], cnt * SizeOf(DWord), Count) or (Count = 0) then Exit;
      if (Count < cnt * SizeOf(DWord)) then Break; // enough
      cnt:= cnt * 2; // extend array to keep all processes
    until (cnt > MAX_PROCESS_LIST);
    cnt:= Count div SizeOf(DWord);
    for i:= 0 to cnt - 1 do begin
      ph:= OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ,
        False, procs[i]);
      if (ph > 0) then begin
        // each process have at least 1 module..
        cmcnt:= 1;
        SetLength(mh, cmcnt);
        mh[0]:= 0;
        if not EnumProcessModules(ph, @mh[0], cmcnt * SizeOf(HModule), cm) then Continue;
        if AShowModules then begin
          if (cm > cmcnt * SizeOf(cm)) then begin
            // or more DLLs..
            cmcnt:= cm div SizeOf(HModule);
            if (cmcnt <= 0) then cmcnt:= 1; // just in case
            SetLength(mh, cmcnt);
            if not EnumProcessModules(ph, @mh[0], cm, cm) then Continue;
          end;
          for m:= 0 to cmcnt - 1 do begin
            AddModule(m);
          end;
        end else begin
          AddModule(0);
        end;
        CloseHandle(ph);
      end;
    end;
  end;
end;

{ GetProcessList
  Wrapper for GetProcessHandles (returns String list instead of array of module handles)
  Parameters:
    AFileNames: array of name (can be incomplete name not full path) of process to list
    AShowModules: if True, add all DLL modules linked with a process
  Returns:
    AResult: string list with full file name of process executable. Objects[] contain a handle to the module
  Depends on GetProcessHandles()
}
procedure GetProcessList(const AFileNames: TStringDynArray; AResult: TStrings; const AShowModules: Boolean = False);
var
  handles: THandleDynArray;
  c, bytes, cnt: Integer;
  ModName: array[0..MAX_PATH] of Char; // Module name
  isWin32: Boolean;
  ph: THandle;
begin
  if not Assigned(AResult)
    then Exit;// raise SysUtils.Exception.Create('GetProccessList() wrong parameter');
  try
    AResult.BeginUpdate;
    AResult.Clear;
    GetProcessHandles(handles, AFileNames, AShowModules);
    cnt:= Length(handles);
    isWin32:= GetWin32Platform = VER_PLATFORM_WIN32_WINDOWS;
    ModName[0]:= #0;
    for c:= 0 to cnt - 1 do begin
      ph:= OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ,
        False, handles[c].pid);
      if isWin32
        then bytes:= GetModuleFileName(ph, ModName, SizeOf(ModName))
        else bytes:= GetModuleFileNameEx(ph, handles[c].MainModuleHandle, ModName, SizeOf(ModName));
      if bytes > 0
        then AResult.AddObject(ModName, TObject(handles[c].pid));
      CloseHandle(ph);
    end;
  finally
    AResult.EndUpdate;
  end;
end;

type
  { TProcessWindows
    Purpose: record is used in EnumWindows callback
    pids: array of process and DLL modules associated with [top-level] window
      First element of array is a process itself, others are DLL
    handles: array of top-level window(first array element) and subcontrols windows
  }
  TProcessWindows = record
    pids: THandleDynArray;
    handles: TWindowDynArray;
  end;

{ Callback enumerates through windows active in the system. }
function EnumWindowsProc(AHw: HWnd; var AParam: TProcessWindows): Boolean; stdcall;
var
  // tid,
  pid: THandle;
  cnt: Cardinal;
  p, pcnt: Integer;
begin
  { Return true by default which indicates not to stop enumerating through the windows }
  Result:= True;
  // GetWindowText(AHw, WinName, 144); // Obtain the current window text
  // GetClassName(AHw, CName, 144); // Obtain the class name of the window
  GetWindowThreadProcessId(AHw, pid); // tid:=
  pcnt:= Length(AParam.pids);
  for p:= 0 to pcnt - 1 do begin
    if (pid = AParam.pids[p].pid) then begin
      // found window
      // add window handle to the end of list
      cnt:= Length(AParam.handles);
      SetLength(AParam.handles, cnt + 1);
      AParam.handles[cnt]:= AHw;
    end;
  end;
end;

{ GetProcessWindowHandles
  Parameters:
    AFileNames: array of name (can be incomplete name not full path) of process to list
  Returns:
    pw: window handle array
}
procedure GetProcessWindowHandles(const AFileNames: TStringDynArray; var pw: TWindowDynArray);
var
  sw: TProcessWindows;
  // hs: THandleDynArray;
  cnt: Integer;
begin
  SetLength(sw.pids, 0);
  GetProcessHandles(sw.pids, AFileNames);
  cnt:= Length(sw.pids);
  if cnt <= 0 then Exit;
  SetLength(sw.handles, 0);
  if EnumWindows(@EnumWindowsProc, Integer(@sw))
    then pw:= sw.handles;
end;

{ GetProcessWindowList

  Wrapper for GetProcessWindowHandles (returns String list instead of array of window handles)
  Parameters:
    AFileNames: array of name (can be incomplete name not full path) of process to list
  Returns:
    AResult: List of window caption. Objects[] refer to widnow handle
}
procedure GetProcessWindowList(const AFileNames: TStringDynArray; AResult: TStrings);
var
  pw: TWindowDynArray;
  c, cnt: Integer;
  cap: array[0..144] of Char;
begin
  if not Assigned(AResult)
    then Exit;// raise SysUtils.Exception.Create('GetProccessList() wrong parameter');
  try
    AResult.BeginUpdate;
    AResult.Clear;
    GetProcessWindowHandles(AFileNames, pw);
    cnt:= Length(pw);
    for c:= 0 to cnt - 1 do begin
      GetWindowText(pw[c], cap, 144); // Obtain the current window text
      AResult.AddObject(cap, TObject(pw[c]))
    end;
  finally
    AResult.EndUpdate;
  end;
end;

{ PaintTopWindowToDC() demonstrates how to paint subcontrols of top-level windows
  so it is commented in case if it 'll be useful
type
  PPaintData = ^TPaintData;
  TPaintData = record
    Parent: HWND;
    DC: HDC;
  end;

procedure PaintTopWindowToDC(Wnd: HWND; Bmp: TBitmap);
var
  R: TRect;
  PD: TPaintData;
  hr: Integer;

  function EnumWnd(Wnd: HWND; P: PPaintData): bool; stdcall;
  var
    SDC: integer;
    R: TRect;
    PD: TPaintData;
  begin
    try
      GetWindowRect(Wnd, R);
      Windows.ScreenToClient(P.Parent, R.TopLeft);
      SDC:= SaveDC(P.DC);
      try
        SetViewportOrgEx(P.DC, R.Left, R.Top, nil);
        SendMessage(Wnd, WM_ERASEBKGND, integer(P.DC), 0);
        SendMessage(Wnd, WM_PRINT, integer(P.DC), PRF_CHILDREN or PRF_CLIENT or PRF_ERASEBKGND or PRF_NONCLIENT or PRF_OWNED);
SendMessage(Wnd, WM_DRAWITEM, integer(PD.DC), 0);
SendMessage(Wnd, WM_SETTEXT, 0, Integer(PChar('AA')));
        PD.Parent:= Wnd;
        PD.DC:= P.DC;
        EnumChildWindows(Wnd, Addr(EnumWnd), integer(@PD));
      finally
        RestoreDC(P.DC, SDC);
      end;
      Result:= true;
    except
      Result:= false;
    end;
  end;

begin
  GetClientRect(Wnd, R);
  Bmp.Width:= R.Right;
  Bmp.Height:= R.Bottom;
  PD.Parent:= Wnd;
  Bmp.Canvas.Lock;
  try
    PD.DC:= Bmp.Canvas.Handle;
hr:=     Windows.SendMessage(Wnd, WM_ERASEBKGND, integer(PD.DC), 0);
hr:=     SendMessage(Wnd, WM_PRINT, integer(PD.DC), PRF_CHILDREN or PRF_CLIENT or PRF_ERASEBKGND or PRF_NONCLIENT or PRF_OWNED);
hr:= SendMessage(Wnd, WM_PAINT, 0, 0);

hr:= SendMessage(Wnd, WM_SETTEXT, 0, Integer(PChar('AA')));
    EnumChildWindows(Wnd, Addr(EnumWnd), integer(@PD));
  finally
    Bmp.Canvas.Unlock;
  end;
end;
}

function CopyWindowDC(AHWnd: HWND; AResultDC: HDC): Boolean;
var
  Width, Height, SaveIndex: Integer;
  pwi: TWindowInfo; // Win98+
  Rect: TRect;
  srcdc: HDC;
  // Ofs, EdgeFlags, BorderFlags: Integer; // border
  // xy: TPoint;
begin
  Result:= False;
  // get window properties
  if not GetWindowInfo(AHWnd, pwi) then Exit;
  // get windows size
  if not GetWindowRect(AHWnd, Rect) then Exit;
  Width:= Rect.Right - Rect.Left;
  Height:= Rect.Bottom - Rect.Top;
  {
  Result.Canvas.Brush := Brush;
  Result.Canvas.FillRect(ClientRect);
  }
  { border
  if GetWindowLong(AHWnd, GWL_STYLE) and WS_BORDER <> 0 then
    Ofs := -1  // Don't draw form border
  else
    Ofs := 0;  // There is no border
  }
  // paint
  SaveIndex:= SaveDC(AResultDC);
  try
    {
    // make a border
    GetWindowOrgEx(AResultDC, xy);
    SetWindowOrgEx(AResultDC, xy.X - Ofs, xy.Y - Ofs, nil);
    IntersectClipRect(AResultDC, 0, 0, Width, Height);
    BorderFlags := 0;
    EdgeFlags := 0;
    if GetWindowLong(AHWnd, GWL_EXSTYLE) and WS_EX_CLIENTEDGE <> 0 then
    begin
      EdgeFlags := EDGE_SUNKEN;
      BorderFlags := BF_RECT or BF_ADJUST
    end else
    if GetWindowLong(AHWnd, GWL_STYLE) and WS_BORDER <> 0 then
    begin
      EdgeFlags := BDR_OUTER;
      BorderFlags := BF_RECT or BF_ADJUST or BF_MONO;
    end;
    if BorderFlags <> 0 then
    begin
      SetRect(Rect, 0, 0, Width, Height);
      DrawEdge(AResultDC, Rect, EdgeFlags, BorderFlags);
      SetWindowOrgEx(AResultDC, xy.X - Rect.Left, xy.Y - Rect.Top, nil);
      IntersectClipRect(AResultDC, 0, 0, Rect.Right - Rect.Left, Rect.Bottom - Rect.Top);
    end;
    }
    // PrintWindow() is not a part of Win9X, NT, 2000
    if Assigned(HPrintWindow) and (AHWnd <> Windows.GetDesktopWindow)then begin
      // Host's Windows version is XP or 2003
      HPrintWindow(AHWnd, AResultDC, 0);
    end else begin
      // it 'll works within process and (sometiems with MS apps like notepad)
      // hr:= SendMessage(AHWnd, WM_ERASEBKGND, AResultDC, 0);
      // hr:= SendMessage(AHWnd, WM_PRINT, AResultDC, PRF_CHILDREN or PRF_CLIENT or PRF_ERASEBKGND or PRF_NONCLIENT);
      // if hook in use
      // hr:= SendMessage(AHWnd, WM_USER, 0, 130869856); // hooking test
      srcdc:= GetWindowDC(AHWnd);
      if (srcdc <> 0) then begin
        StretchBlt(AResultDC, 0, 0, Width, Height,
          srcdc, 0, 0, Width, Height, SRCCOPY); // -1 if no border
      end;
    end;
  finally
    RestoreDC(AResultDC, SaveIndex);
    // end paint
    // ReleaseDC(AHWnd, srcdc);
  end;
end;

function GetWindowBitmap(AHWnd: HWND): TBitmap;
var
  dc: HDC;
  r: TRect;
begin
  Result:= TBitmap.Create;
  if not GetWindowRect(AHWnd, r) then Exit;
  Result.Width:= r.Right - r.Left;
  Result.Height:= r.Bottom - r.Top;
  dc:= Result.Canvas.Handle;
  Result.Canvas.Lock;
  try
    if CopyWindowDC(AHWnd, dc) then ;
  finally
    Result.Canvas.Unlock;
  end;
end;

function CopyAppDc(const AApplication: String; AResultDC: HDC): Boolean;
var
  n: TStringDynArray;
  pw: TWindowDynArray;
  h: HWND;
begin
  Result:= False;
  SetLength(n, 1);
  if Length(AApplication) = 0
  then h:= GetDesktopWindow
  else begin
    n[0]:= AApplication;
    rtcSnapshot.GetProcessWindowHandles(n, pw);
    if Length(pw) <= 0
      then Exit;
    h:= pw[0];
  end;
  CopyWindowDC(h, AResultDC);
  Result:= True;
end;

function GetAppBitmap(const AApplication: String): TBitmap;
var
  n: TStringDynArray;
  pw: TWindowDynArray;
  h: HWND;
begin
  Result:= Nil;
  SetLength(n, 1);
  if Length(AApplication) = 0
  then h:= GetDesktopWindow
  else begin
    n[0]:= AApplication;
    rtcSnapshot.GetProcessWindowHandles(n, pw);
    if Length(pw) <= 0
      then Exit;
    h:= pw[0];
  end;
  Result:= rtcSnapshot.GetWindowBitmap(h);
end;

// try to load PrintWindow from User32.DLL
procedure LoadFuncs;
begin
  hdll:= GetModuleHandle(DLL_USER32); // first check is it loaded
  if (hdll = 0) then
    // it is impossible!
    hdll:= LoadLibrary(DLL_USER32);
  if (hdll = 0) then
    HPrintWindow:= Nil // it is not a Windows!
  else
    HPrintWindow:= GetProcAddress(hdll, FUNC_PRINTWINDOW);
  // uncomment this for experiments with WM_PRINT
  // HPrintWindow:= Nil;    
end;

initialization
  LoadFuncs;

end.
