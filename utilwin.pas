unit
  utilwin;
(*##*)
(*******************************************************************************
*                                                                             *
*   U  T  I  L  W  I  N   Ensen's windows routines for Delphi                  *
*                                                                             *
*   Copyright (c) 2001, 2003 Andrei Ivanov. All rights reserved.               *
*   Microsoft Windows routines                                                *
*   Conditional defines:                                                       *
*                                                                             *
*   Last Revision: Apr 04 2000                                                 *
*   Last fix     : Nov 12 2003                                                *
*   Lines        :                                                             *
*   History      :                                                            *
*   Printed      : ---                                                         *
*                                                                             *
********************************************************************************)
(*##*)
{ ini file routines }
interface

uses
  Classes, SysUtils, Windows, ShlObj;

function IsFileExtAssociatesWithCmd(AExtension, AFileType, ACmd: String;
  var AFileDescription: String; var AFileIcon: String;
  var ACmdDescription, ACmdProgram, ACmdParamString,
  ADDEApplication, ADDETopic, ADDEItem: String): Boolean;

// Parameters:    AExtension        .gif
//                AFileType         MyAppFileType
//                AFileDescription  '' == AFileType
//                AFileIconIndex    <0- do not assign icon
//
//                ACmd              '' =='open'
//                ACmdDescription,
//                ACmdProgram       '' - clear command ACmd.
//                ACmdParamString
//
// return AExtDescription, if AExtension (like .png) exists
function InstallFileExt(AExtension: String; AFileType: String; AFileDescription: String; AFileIconIndex: Integer;
  ACmd, ACmdDescription, ACmdProgram, ACmdParamString: String;
  ADDEApplication, ADDETopic, ADDEItem: String;
  AOverrideFileDescription, ADefaultCmd: Boolean): Boolean;

function DeInstallFileExt(AExtension, AFileType: String): Boolean;

{ FileExtIcon
  Parameters:
    AExtension - .ext file extension
  Return
    Result     - icon
  Note:
    Deprecared
    Use UrlFuncs.FileExtIcon instead

}
function FileExtIcon(const AExtension: String): String;

{ GetMozillaBrowserPath
  Purpose:
    Read HKEY_CLASSES_ROOT\http\shell\open\command default value
      "E:\Program Files\Internet Explorer\iexplore.exe" -nohome
      E:\PROGRA~1\MOZILLA.ORG\MOZILLA\MOZILLA.EXE -url "%1"
      "E:\Program Files\Opera7\opera.exe"
    Extract file name (skip partameters)
  Return
    default browser file name
  Usage
    ShellExecute(HWND_DESKTOP, 'open', GetMozillaBrowserPath(), 'http://www.commandus.com/'), NULL, SW_SHOWNORMAL);
}
function GetMozillaBrowserPath: String;

{ GetCDROMDrivesCount
  Parameters
    ADrives - return list of drive letters such 'DG' where D: and G: are cd-rom drives
  Return
    count of cd-rom drives found
}
function GetCDROMDrivesCount(var ADrives: String): Integer;

{ GetCDROMMedia
  Parameters
    APath - '' or 'D:\' or '.'
      '' - try to find out cd-rom in the current directory drive, if not found,
        try to find out other media in ALL other cd-rom drives
  Return
    AVolume - cd-rom disk media volume name
    ASerial - 8 bytes long media serial number
    AFileSystem - 'CDFS'
    Result - cd-rom disk drive root directory i.e. 'D:\' if found, '' if not found any disks
}
function GetCDROMMedia(const APath: String; var AVolume: String; var ASerial: DWORD; var AFileSystem: String): String;

function GetRunOnStartup(Title: String; AOnce: Boolean; const AMachine: Boolean = False): Boolean;

function SetRunOnStartup(ATitle, ACommandLine: String; ARun, AOnce: Boolean; const AMachine: Boolean = False): Boolean;

implementation

uses
  Messages, Registry, jclShell;

function IsFileExtAssociatesWithCmd(AExtension, AFileType, ACmd: String;
  var AFileDescription: String; var AFileIcon: String;
  var ACmdDescription, ACmdProgram, ACmdParamString,
  ADDEApplication, ADDETopic, ADDEItem: String): Boolean;
var
  Reg: TRegistry;
  S: String;
begin
  Result:= False;
  AFileDescription:= '';
  AFileIcon:= '';
  ACmdDescription:= '';
  ACmdProgram:= '';
  ACmdParamString:= '';
  ADDEApplication:= '';
  ADDETopic:= '';
  ADDEItem:= '';
  Reg:= TRegistry.Create;
  try
  with Reg do begin
    RootKey:= HKEY_CLASSES_ROOT;
    if KeyExists(AExtension) then begin
      // read file type description
      if OpenKeyReadOnly(AExtension) then begin
        S:= ReadString('');
        // is file type link exists
        if (CompareText(AFileType, S) = 0) then begin
          // read file type description
          if OpenKeyReadOnly('\' + AFileType) then begin
            AFileDescription:= ReadString('');
            // file icon
            OpenKeyReadOnly('DefaultIcon');
            AFileIcon:= ReadString('');
            // command. Default command: open
            if Length(ACmd) = 0
            then ACmd:= 'open';
            // default command
            if OpenKeyReadOnly('\' + AFileType + '\Shell\')
            then S:= ReadString('');
            if OpenKeyReadOnly('\' + AFileType + '\Shell\'+ACmd) then begin
              Result:= True;
              // command description
              if OpenKeyReadOnly('\' + AFileType + '\Shell\'+ACmd)
              then ACmdDescription:= ReadString('');
              // command string
              if OpenKeyReadOnly('\' + AFileType + '\Shell\'+ACmd+'\command')
              then ACmdProgram:= ReadString('');
              // ddeexec
              if OpenKey('\' + AFileType + '\Shell\'+ACmd+'\ddeexec', False) then begin
                if OpenKeyReadOnly('\' + AFileType + '\Shell\'+ACmd+'\ddeexec')
                then ADDEItem:= ReadString('');
                if OpenKeyReadOnly('\' + AFileType + '\Shell\'+ACmd+'\ddeexec\Application')
                then ADDEApplication:= ReadString('');
                if OpenKeyReadOnly('\' + AFileType + '\Shell\'+ACmd+'\ddeexec\Topic')
                then ADDETopic:= ReadString('');
              end;
            end;
          end;
        end;
      end;
    end;
  end;
  except
  end;
  Reg.Free;
end;

function InstallFileExt(AExtension: String; AFileType: String; AFileDescription: String; AFileIconIndex: Integer;
  ACmd, ACmdDescription, ACmdProgram, ACmdParamString: String;
  ADDEApplication, ADDETopic, ADDEItem: String;
  AOverrideFileDescription, ADefaultCmd: Boolean): Boolean;
var
  Reg: TRegistry;
  S: String;
  i: Integer;
  SL: TStrings;
begin
  Reg:= TRegistry.Create;
  try
  with Reg do begin
    // 1. associate file extension and file type
    // write file extension (like .png) link to description (default value)
    RootKey:= HKEY_CLASSES_ROOT;
    if KeyExists(AExtension) then begin
      // read description's link
      OpenKey(AExtension, False);
      S:= ReadString('');
      // is description's link exists, use it
      if (Length(S) > 0) and (not AOverrideFileDescription)
      then AFileType:= S
      else begin
        // create new link
        WriteString('', AFileType);
        CloseKey;
      end;
    end else begin
      // create new key
      OpenKey(AExtension, True);
      // create new link to description
      WriteString('', AFileType);
      CloseKey;
    end;

    // 2. file type description
    if Length(AFileDescription) = 0
    then AFileDescription:= AFileType;
    // write file description: default (description), default icon, shell open command
    OpenKey('\' + AFileType, True);
    S:= ReadString('');
    // create or replace file description
    if AOverrideFileDescription or (Length(S) = 0) then begin
      WriteString('', AFileDescription);
    end;
    CloseKey;
    // create or replace file icon
    OpenKey('\' + AFileType+'\DefaultIcon', True);
    S:= ReadString('');
    if (AFileIconIndex >= 0) and ((Length(S)=0) or AOverrideFileDescription) then begin
      WriteString('', ACmdProgram + ','+IntToStr(AFileIconIndex));
    end;
    CloseKey;
    if Length(ACmdProgram) = 0 then begin
      // NT requires delete all
      if OpenKey('\' + AFileType + '\Shell\'+ACmd+'\command', False) then begin
        SL:= TStringList.Create;
        GetValueNames(SL);
        for i:= 0 to SL.Count - 1 do begin
          DeleteKey(SL[i]);
        end;
        CloseKey;
      end;
      {
      DeleteKey('\' + AFileType + '\Shell\'+ACmd+'ddeexec\Application');
      DeleteKey('\' + AFileType + '\Shell\'+ACmd+'ddeexec\Topic');
      DeleteKey('\' + AFileType + '\Shell\'+ACmd+'ddeexec');
      DeleteKey('\' + AFileType + '\Shell\'+ACmd+'command');
      DeleteKey('\' + AFileType + '\Shell\'+ACmd);
      }
    end else begin
      // create or replace command. Default command: open
      if Length(ACmd) = 0
      then ACmd:= 'open';
      // default command
      OpenKey('\' + AFileType + '\Shell\', True);
      S:= ReadString('');
      if ADefaultCmd or (Length(S) = 0) then begin
        WriteString('', ACmd);
      end;
      CloseKey;
      // command description
      OpenKey('\' + AFileType + '\Shell\'+ACmd, True);
      WriteString('', ACmdDescription);
      CloseKey;
      // command string
      OpenKey('\' + AFileType + '\Shell\'+ACmd+'\command', True);
      WriteString('', '"' + ACmdProgram + '" "' + ACmdParamString + '"');
      CloseKey;
    end;
    // ddeexec
    if Length(ADDEApplication) = 0 then begin
      // NT requires delete all
      if OpenKey('\' + AFileType + '\Shell\'+ACmd+'\ddeexec', False) then begin
        SL:= TStringList.Create;
        GetValueNames(SL);
        for i:= 0 to SL.Count - 1 do begin
          DeleteKey(SL[i]);
        end;
        CloseKey;
      end;
    end else begin
      OpenKey('\' + AFileType + '\Shell\'+ACmd+'\ddeexec', True);
      WriteString('', ADDEItem);
      CloseKey;
      OpenKey('\' + AFileType + '\Shell\'+ACmd+'\ddeexec\Application', True);
      WriteString('', ADDEApplication);
      CloseKey;
      OpenKey('\' + AFileType + '\Shell\'+ACmd+'\ddeexec\Topic', True);
      WriteString('', ADDETopic);
      CloseKey;
      //
    end;
  end;
  // Finally, we want the Windows Explorer to realize we added
  // our file type by using the SHChangeNotify API.
  ShlObj.SHChangeNotify(SHCNE_ASSOCCHANGED, SHCNF_IDLIST, nil, nil);
  Result:= True;
  except
    Result:= False;
  end;
  Reg.Free;
end;

function DeInstallFileExt(AExtension, AFileType: String): Boolean;
var
  Reg: TRegistry;
begin
  Reg := TRegistry.Create;
  try
    with Reg do begin
      RootKey := HKEY_CLASSES_ROOT;
      DeleteKey(AExtension);
      DeleteKey('\' + AFileType);
     end;
   Result := True;
  except
   Result := False;
  end;
  Reg.Free;
end;

function FileExtIcon(const AExtension: String): String;
var
  Reg: TRegistry;
  FileType: String;
begin
  Result:= '';
  Reg:= TRegistry.Create;
  try
  with Reg do begin
    RootKey:= HKEY_CLASSES_ROOT;
    if KeyExists(AExtension) then begin
      // read file type description
      if OpenKeyReadOnly(AExtension) then begin
        // is file type link exists
        FileType:= ReadString('');
        // read file type description
        if OpenKeyReadOnly('\' + FileType) then begin
          // file icon
          if OpenKeyReadOnly('DefaultIcon')
          then Result:= ReadString('');
        end;
      end;
    end;
  end;
  except
  end;
  Reg.Free;
end;

{ GetMozillaBrowserPath
  Purpose:
    Read HKEY_CLASSES_ROOT\http\shell\open\command default value
      "E:\Program Files\Internet Explorer\iexplore.exe" -nohome
      E:\PROGRA~1\MOZILLA.ORG\MOZILLA\MOZILLA.EXE -url "%1"
      "E:\Program Files\Opera7\opera.exe"
    Extract file name (skip partameters)
  Return
    default browser file name
  Usage
    ShellExecute(HWND_DESKTOP, 'open', GetMozillaBrowserPath(), 'http://www.commandus.com/', Nil, SW_SHOWNORMAL);
  Based on
    http://jenyay.wallst.ru/index.php?id=browser
}
function GetMozillaBrowserPath: String;
var
  Rg: TRegistry;
	p: Integer;
begin
  Result:= '';
  Rg:= TRegistry.Create;
  with Rg do begin
    RootKey:= HKEY_CLASSES_ROOT;
    if OpenKeyReadOnly('http\shell\open\command') then begin
      Result:= ReadString('');
    end;
    Free;
  end;
  p:= Pos('.exe', LowerCase(Result));
  if p > 0 then begin
    Delete(Result, p + 4, MaxInt);
  end;
  if (Length(Result) > 0) and (Result[1] in ['"'])
  then Delete(Result, 1, 1);
end;

function GetCDROMDrivesCount(var ADrives: String): Integer;
var
  I, DriveType: Integer;
  volume, fs: String;
  maxcomplen, sn, flags: DWORD;
begin
  Result:= 0;
  ADrives:= '';
  for i:= 0 to 25 do begin
    DriveType:= Windows.GetDriveType(PChar(Chr(i+65) +':\'));
{
Windows.GetDriveType
0	The drive type cannot be determined.
1	The root directory does not exist.
DRIVE_REMOVABLE	The drive can be removed from the drive.
DRIVE_FIXED	The disk cannot be removed from the drive.
DRIVE_REMOTE	The drive is a remote (network) drive.
DRIVE_CDROM	The drive is a CD-ROM drive.
DRIVE_RAMDISK	The drive is a RAM disk.
}
    if (DriveType = DRIVE_CDROM) then begin
      SetLength(volume, 255);
      SetLength(fs, 255);
      if GetVolumeInformation(PChar(Chr(i+65) +':\'), PChar(@(volume[1])), Length(volume),
        @sn, maxcomplen, flags, PChar(@(fs[1])), Length(fs)) then begin
        Inc(Result);
        ADrives:= ADrives + Chr(i + 65);
      end;
    end;
  end;
end;

function GetCDROMMedia(const APath: String; var AVolume: String; var ASerial: DWORD; var AFileSystem: String): String;
var
  DriveType: Integer;
  drives, path: String;
  maxcomplen, flags: DWORD;
  curdrive: String[1];
begin
  Result:= '';
  AVolume:= '';
  ASerial:= 0;
  AFileSystem:= '';
  {
  Windows.GetDriveType
  0	The drive type cannot be determined.
  1	The root directory does not exist.
  DRIVE_REMOVABLE	The drive can be removed from the drive.
  DRIVE_FIXED	The disk cannot be removed from the drive.
  DRIVE_REMOTE	The drive is a remote (network) drive.
  DRIVE_CDROM	The drive is a CD-ROM drive.
  DRIVE_RAMDISK	The drive is a RAM disk.
  }

  if Length(APath) = 0 then begin
    // if no path is specified
    if GetCDROMDrivesCount(drives) = 0
    then Exit;
    // get current drive letter (CDROM drive if program is started from cdrom)
    curDrive:= Uppercase(Copy(GetCurrentDir, 1, 1));
    if Pos(curDrive, drives) > 0 then begin
      path:= curDrive + ':\';
    end else begin
      // get the first CDROM drive
      path:= Copy(drives, 1, 1) + ':\';
      Delete(drives, 1, 1);
    end;
  end else path:= APath;
  repeat
    DriveType:= Windows.GetDriveType(PChar(Path));
    if (DriveType = DRIVE_CDROM) then begin
      SetLength(AVolume, 255);
      SetLength(AFileSystem, 255);
      if GetVolumeInformation(PChar(Path), PChar(@(AVolume[1])), Length(AVolume),
        @ASerial, maxcomplen, flags, PChar(@(AFileSystem[1])), Length(AFileSystem)) then begin
        Result:= Path;
        AVolume:= PChar(AVolume);
        AFileSystem:= PChar(AFileSystem);
        Break;
      end;
    end else begin
      // let try other cdroms
      if Length(drives) = 0
      then Break;
      path:= Copy(drives, 1, 1) + ':\';
      Delete(drives, 1, 1);
    end;
  until False;
end;

const
  RC_RunKey = '\Software\Microsoft\Windows\CurrentVersion\Run';
  RC_RunOnceKey = '\Software\Microsoft\Windows\CurrentVersion\RunOnce';

function GetRunOnStartup(Title: String; AOnce: Boolean; const AMachine: Boolean = False): Boolean;
begin
  with TRegistry.Create do begin
    if AMachine
    then RootKey:= HKEY_LOCAL_MACHINE
    else RootKey:= HKEY_CURRENT_USER;
    if AOnce
    then OpenKey(RC_RunOnceKey, False)
    else OpenKey(RC_RunKey, False);
    Result:= ValueExists(Title);
    Free;
  end;
end;

function SetRunOnStartup(ATitle, ACommandLine: String; ARun, AOnce: Boolean; const AMachine: Boolean = False): Boolean;
begin
  Result:= False;
  // Bug: entry are allowed to have the same Title but may get both deleted here
  with TRegistry.Create do begin
    if AMachine
    then RootKey:= HKEY_LOCAL_MACHINE
    else RootKey:= HKEY_CURRENT_USER;
    OpenKey(RC_RunOnceKey, True);
    if AOnce then begin
      Result:= True;
      WriteString(ATitle, ACommandLine);
    end else begin
      if ValueExists(ATitle)
      then DeleteValue(ATitle);
    end;
    OpenKey(RC_RunKey, True);
    if ARun then begin
      WriteString(ATitle, ACommandLine);
      Result:= True;
    end else begin
      if ValueExists(ATitle)
      then DeleteValue(ATitle);
    end;
    Free;
  end;
end;

end.
