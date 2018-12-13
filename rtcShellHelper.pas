unit
  rtcShellHelper;
(*##*)
(*******************************************************************************
*                                                                             *
*   R  T  C  S  h  e  l  l  H  e  l  p  e  r                                   *
*                                                                             *
*   Folder list component based on TCustomListView                             *
*                                                                             *
*   Copyright (c) 2007, Andrei Ivanov, RealThinComponents                      *
*                                                                             *
*   Conditional defines:                                                       *
*                                                                             *
*   Last Revision: Nov 19 2007                                                 *
*   First release: Nov 19 2007                                                *
*   Lines:         2286                                                        *
*   History:       Nov 19 2007                                                *
*   Notes:                                                                     *
*  TListView with a vsReport style causes AVE when you run with a XP manifest *
*  resource. The VCL D6 has a bug and you must patch sources comctrls.pas      *
*                                                                             *
********************************************************************************)
(*##*)
{$R-}
{$WARNINGS OFF} // deprecated symbols, Move(ptr^..), @
{$IFNDEF VER90}{$IFNDEF VER91}{$IFNDEF VER92}
   {$DEFINE DELPHI3UP}
{$ENDIF}{$ENDIF}{$ENDIF}
interface
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls,
  ShellAPI, ComCtrls, ShlObj, ActiveX;
{
  @html(<b>)
  Folder List
  @html(</b>)
  @html(<br><br>)
  Introducing the @html(<b>) Shell helper functions @html(</b>) component:
  @html(<br>)
  Shell helper functions.
}


const
  WM_CHANGENOTIFY = WM_USER + 2;
  {* constants used by SHChangeNotifyRegister  *}
{ These flags are missed in the MS Windows header file. }
  SHCNRF_InterruptLevel = 1;         // Interrupt level notifications from the file system.
  SHCNF_ACCEPT_INTERRUPTS = SHCNRF_InterruptLevel;
  SHCNRF_ShellLevel     = 2;         // Shell-level notifications from the shell.
  SHCNF_ACCEPT_NON_INTERRUPTS = SHCNRF_ShellLevel;
  SHCNRF_RecursiveInterrupt = $1000; // Interrupt events on the whole subtree. This flag must be combined with the SHCNRF_InterruptLevel flag. When using this flag, notifications must also be made recursive by setting the fRecursive member of the corresponding SHChangeNotifyEntry structure referenced by pfsne to TRUE.
  SHCNRF_NewDelivery    = $8000;     // Messages received use shared memory.
  SHCNF_NO_PROXY = SHCNRF_NewDelivery;

type
  // @exclude
  NOTIFYREGISTER = record
    pidlPath: PItemIDList;
    bWatchSubtree: Boolean;
  end;
  // @exclude
  PNOTIFYREGISTER = ^NOTIFYREGISTER;

  { @Abstract(Folder path info including icon, selected icon and identifier) }
  TPathInfo = record
    Path: String;
    Text: String;
    IconIndex: UINT;
    SelectedIndex: UINT;
    Pid: PItemIdList;
  end;

  { @Abstract(Display mode of file name: As is, uppercase or lowercase) }
  TDisplayMode = (dmAsis, dmUppercase, dmLowercase);
  TFilePathList = record
    Normal: String;
    ForParsing: String;
    InFolder: String;
  end;

  { @Abstract(Node type: each node in file system is folder of ordinal or special file) }
  TNodeType = (ntFolder, ntFile);
  { @Abstract(Folder type: Folder of ftMyComputer is 'usual' JFS file. Other files can stored in different types of storage, like IE cache, PocketPC sync etc.) }
  TFolderType = (ftNetworkNeighborhood, ftRecycleBin, ftMyComputer, ftDesktop,
    ftFTPList, ftDbList, ftLDAPList, ftHTTPList,
    ftLocalNode, ftNetNode, ftFtpNode, ftDbNode, ftLdapNode, ftHTTPNode, ftUnknown);
  // SFolderTypes = set of TFolderType;
  { @Abstract(Options controls which files in storage can be show. Options also controls order of VirtualFolders.
    Option foFiles allow to show files in the tree, otherwise directories will be displayed only) }
  TFolderOption = (foMyComputer, foNetworkNeighborhood, foRecycleBin, foFTP, foDatabase, foLDAP, foHTTP,
    foVirtualFirst, foFiles);
  { @Abstract(Options is a set. If set is empty, TrtcFolderTree will be emtpy) }
  SFolderOption = set of TFolderOption;

  { @Abstract(File system events filter. See also: @Link(TNotifyFilters)) }
  TNotifyFilter = (nfFileNameChange, nfDirNameChange, nfAttributeChange,
    nfSizeChange, nfWriteChange, nfSecurityChange);
  { @Abstract(Set of @Link(TNotifyFilter)) }
  TNotifyFilters = set of TNotifyFilter;

  { @Abstract(Callback of this type is called when Windows closes the session.
    The WM_QUERYENDSESSION message is sent when the user chooses to end the session or when an application calls the ExitWindows function.
    You can close network connection and free resources before Windows close a session.
    if CanEndSession is set to False, it inform Windows that user must finish his job and
    close application 'normally'
  )}
  TEndSessionQueryEvent = procedure(Sender: TObject; var CanEndSession: Boolean) of object;

  // Nov 11 2007
  TRemoteFileInfo = record
    FileName: String;
    FileSize: Integer;
    FileCreationTime: TDateTime;
    FileLastAccessTime: TDateTime;
    FileNodeType: TNodeType;
    FileType: TFolderType;
    FileInfo: TSHFileInfo;
  end;
  TRemoteFileInfos = array of TRemoteFileInfo;

  { @Abstract(Callback function return a list of files within specified AURL. List contains an Objects of 0 or 1.
    if Integer(Objects[i]= 0, it is a file, otherwise it is a folder.
    Function must be a part of object such TForm.
    Parameters: AFolderType of type @Link(TFolderType) can be ftFTP, ftHTTP.
    AUrl specify location of FTP directory.
    Result is returned in parameter AFolderList.)}
  TOnExtStorageFolderList = procedure(ASender: TObject; AFolderType: TFolderType; const AURL: String; AFolderList: TStrings) of object;
  { @Abstract(Callback function send a notification to update directory
  async) }
  TOnExtStorageAsyncFolderList = procedure(ASender: TObject; const AURL: String) of object;

  { @Abstract(Delete file(s) callback function type) }
  TOnExtDeleteUrl = function (ASender: TObject; const AURL: String): Boolean of object;
  { @Abstract(Move file(s) callback function type) }
  TOnExtMoveUrl = function (ASender: TObject; const ASrcUrl, ADestUrl: String): Boolean of object;
  { @Abstract(Copy file(s) callback function type) }
  TOnExtCopyUrl = function (ASender: TObject; const ASrcUrl, ADestUrl: String): Boolean of object;

  // @exclude
  function  CustomSortProc(Node1, Node2: TTreeNode; ParamSort: Integer): Integer; stdcall;

type
  // Windows 2000 or higher
  // @exclude
  TSHChangeNotifyRegister=  function (hWnd: HWND; dwFlags: Integer; wEventMask: Cardinal;
    uMsg: UINT; cItems: Integer; lpItems: PNOTIFYREGISTER): HWND; stdcall;
  // @exclude
  TSHChangeNotifyDeregister = function (hWnd: HWND): Boolean; stdcall;

var
  SHChangeNotifyRegister: TSHChangeNotifyRegister;
  SHChangeNotifyDeRegister: TSHChangeNotifyDeregister;

const
  VirtualFolders = [ftNetworkNeighborhood, ftRecycleBin, ftMyComputer,
    ftFTPList, ftDbList, ftLDAPList, ftHTTPList];  // ftNetNode

type
  {@Abstract(Thread monitors changes in specified directory constructed by
    @Link(TCustomShellChangeNotifier) object.)
  }
  TShellChangeThread = class(TThread)
  private
    FMutex,
    FWaitHandle: Integer;
    FChangeEvent: TThreadMethod;
    FDirectory: string;
    FWatchSubTree: Boolean;
    FWaitChanged : Boolean;
    FNotifyOptionFlags: DWORD;
  protected
    procedure Execute; override;
  public
    constructor Create(ChangeEvent: TThreadMethod); virtual;
    destructor Destroy; override;
    {@Abstract(Set monitoring directory and notify options)}
    procedure SetDirectoryOptions(Directory: String; WatchSubTree: Boolean;
      NotifyOptionFlags: DWORD);
    {@Abstract(callback action on file system changes event)}
    property ChangeEvent : TThreadMethod read FChangeEvent write FChangeEvent;
  end;

  {@Abstract(Monitors changes in specified directory. Abstract class.
   Use @Link(TShellChangeNotifier) or other derived class) }
  TCustomShellChangeNotifier = class(TComponent)
  private
    FFilters: TNotifyFilters;
    FWatchSubTree: Boolean;
    FRoot : String;
    FThread: TShellChangeThread;
    FOnChange: TThreadMethod;
    procedure SetRoot(const Value: String);
    procedure SetWatchSubTree(const Value: Boolean);
    procedure SetFilters(const Value: TNotifyFilters);
    procedure SetOnChange(const Value: TThreadMethod);
  protected
    procedure Change;
    procedure Start;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    {@Abstract(File system events filter. See also: @Link(TNotifyFilters) @Link(TNotifyFilter)) }
    property NotifyFilters: TNotifyFilters read FFilters write SetFilters;
    {@Abstract(Directory which is monitored)}
    property Root: String read FRoot write SetRoot;
    {@Abstract(True- allow monitors subtree)}
    property WatchSubTree: Boolean read FWatchSubTree write SetWatchSubTree;
    {@Abstract(File system events filter. See also: @Link(TNotifyFilters) @Link(TNotifyFilter)) }
    property OnChange: TThreadMethod read FOnChange write SetOnChange;
  end;

  {@Abstract(Monitors changes in specified directory) }
  TShellChangeNotifier = class(TCustomShellChangeNotifier)
  published
    property NotifyFilters;
    property Root;
    property WatchSubTree;
    property OnChange;
  end;

type
  // @exclude
  PFolderNode = ^TFolderNode;
  { @Abstract(Folder Node has a path and text to display. Node of type ntFolder can has a list of sub-tree) }
  TFolderNode = packed record
    FN_Path: String;
    FN_Text: String;
    FN_Has_Parent: Boolean;
    FN_ShellFolder: IShellFolder;
    FN_Type: TFolderType;
    FN_NodeType: TNodeType;
    FN_PidLen: Word;
    FN_PidList: PItemIdList;
  end;

{@Abstract(callback procedure informs a folder is attached to the list view)}
  TAddFolderEvent = procedure(Sender: TObject; AFolder: PFolderNode;
    var CanAdd: Boolean) of object;
{@Abstract(callback function returns image index in AImageIndex parameter for index of list item)}
  TGetImageIndexEvent = procedure(Sender: TObject; Index: Integer;
     var ImageIndex: Integer) of object;

{@Abstract(GetShellImage return image index at the system icons set)}
function GetShellImage(PIDL: PItemIDList; ALarge, AOpen: Boolean): Integer;
{@Abstract(GetShellImageListItem return image index at the system icons set)}
function GetShellImageListItem(AListItem: TListItem; Large, Open: Boolean): Integer;
{@Abstract(InvokeContextMenu show popup menu at the screen position
  associated with a file identified by AFN_PidList popup menu)}
procedure InvokeContextMenu(Owner: TWinControl; AFolder: IShellFolder;
  AFN_PidList: PItemIdList; X, Y: Integer);
{@Abstract(GetDefaultFileImageIndex return image index for a file at the system icons set.
  Returned icon can be used to display remote files.)}
function GetDefaultFileImageIndex(ALarge, AOpen: Boolean): Integer;
{@Abstract(GetDefaultFolderImageIndex return image index for a folder at the system icons set.
  Returned icon can be used to display remote files.)}
function GetDefaultFolderImageIndex(ALarge, AOpen: Boolean): Integer;

{@Abstract(GetFileAttributesString return a string represents
JFS file attributes with characters:
@html(<li>)G - Cut
@html(<li>)L - Is link
@html(<li>)R - Read Only
@html(<li>)S - Shared
@html(<li>)Y - not a file system file
@html(<li>)A - system ancestor
@html(<li>)M - Removable
@html(<li>)V - Validate
@html(<br>)For more information how to interpret file attributes please refer to MSDN
)}
function GetFileAttributesString(AFileAttributes: Integer): String;

// ------------------------------ helper functions -----------------------------

{ @Abstract(Delete a file/folder specified by FQFN and move deleted fiel to Recycle Bin basket )}
function SendFileToRecycleBin(const AFileName: string): Boolean;
{ @Abstract(Move/Rename file(s) specified by FQFN to the destination file/folder)}
function MoveFileToDest(const AFileName: string; const ADest: String): Boolean;
{ @Abstract(Copy file(s) specified by FQFN to the destination file/folder)}
function CopyFileToDest(const AFileName: string; const ADest: String; AOverride: Boolean): Boolean;
{ @Abstract(return ntFolder or ntFile of list item (if ListView is assigned))}
function GetListViewNodeType(AListItem: TListItem): TNodeType;
{ @Abstract(return list item path name (if ListView is assigned))}
function GetListViewNodeName(AListItem: TListItem): String;
{ @Abstract(return file size of list item (if ListView is assigned))}
function GetListViewNodeSize(AListItem: TListItem): Int64;
{ @Abstract(return type description  (if ListView is assigned))}
function GetListViewNodeTypeDesc(AListItem: TListItem): String;
{ @Abstract(return attributes of list item (if ListView is assigned))}
function GetListViewNodeAttrs(AListItem: TListItem): Integer;
{ @Abstract(return modified or created date and time of list item (if ListView is assigned))}
function GetListViewNodeDate(AListItem: TListItem): TDateTime;

{ @Abstract(Change tree view node state. Example: SetNodeState(DropNode, TVIS_BOLD);)}
procedure SetNodeState(ANode: TTreeNode; AFlags: Integer);

function FindChildNode(AParentNode: TTreeNode; const AStr: String): TTreeNode;

type
  IShellCommandVerb = interface
    ['{7D2A7245-2376-4D33-8008-A130935A2E8B}']
    procedure ExecuteCommand(Verb: string; var Handled: boolean);
    procedure CommandCompleted(Verb: string; Succeeded: boolean);
  end;

function GetLastErrorDescription(AError: DWORD): WideString;

{ @Abstract(Return file name from URL in PC DOS format }
function Url2LocalFileName(const AUrl: String): String;

{ @Abstract(Return URL-like address of given file name }
function LocalFileName2Url(const AProtocol, AHost, AFileName: String): String;

implementation

uses
  CommCtrl, Forms,
  StrUtils,
  UrlFuncs;

const
  Shell32DLL = 'shell32.dll';

var
  CS : TRTLCriticalSection;
  ICM2: IContextMenu2 = nil;

// function SHChangeNotifyRegister; external Shell32DLL name 'SHChangeNotifyRegister'; // index 2;
// function SHChangeNotifyDeregister; external Shell32DLL name 'SHChangeNotifyDeregister'; // index 4;

function CustomSortProc(Node1, Node2: TTreeNode; ParamSort: Integer): Integer; stdcall;
var
  foldernode1, foldernode2: PFolderNode;
begin
  // If the first node is a special folder type, put it before the other
  // node in sort order. Otherwise, go by the node text, case insensitive.
  foldernode1:= Node1.data;
  foldernode2:= Node2.data;
  if (foldernode1.FN_Type in VirtualFolders) then begin // ^
    Result:= -1;
    Exit;
  end;
  if (foldernode1.FN_NodeType > foldernode2.FN_NodeType) then begin // ^
    Result:= 1;
    Exit;
  end;
  if (foldernode1.FN_NodeType < foldernode2.FN_NodeType) then begin // ^
    Result:= -1;
    Exit;
  end;
  Result:= StrIComp(PChar(Node1.Text),  PChar(Node2.Text));
end;

{ TCustomShellChangeNotifier }
{ @Abstract(If file system changes occurs, list view updated automatically if AutoRefresh = True) }
procedure TCustomShellChangeNotifier.Change;

  function NotifyOptionFlags: DWORD;
  begin
    Result := 0;
    if nfFileNameChange in FFilters then
      Result := Result or FILE_NOTIFY_CHANGE_FILE_NAME;
    if nfDirNameChange in FFilters then
      Result := Result or FILE_NOTIFY_CHANGE_DIR_NAME;
    if nfSizeChange in FFilters then
      Result := Result or FILE_NOTIFY_CHANGE_SIZE;
    if nfAttributeChange in FFilters then
      Result := Result or FILE_NOTIFY_CHANGE_ATTRIBUTES;
    if nfWriteChange in FFilters then
      Result := Result or FILE_NOTIFY_CHANGE_LAST_WRITE;
    if nfSecurityChange in FFilters then
      Result := Result or FILE_NOTIFY_CHANGE_SECURITY;
  end;

begin
  if Assigned(FThread) then
  begin
    FThread.SetDirectoryOptions(Root, LongBool(FWatchSubTree),
      NotifyOptionFlags);
  end;
end;

constructor TCustomShellChangeNotifier.Create(AOwner: TComponent);
begin
  inherited;
  FRoot := 'C:\';      { Do not localize }
  FWatchSubTree := True;
  FFilters := [nfFilenameChange, nfDirNameChange];
  Start;
end;

destructor TCustomShellChangeNotifier.Destroy;
var
  Temp : TShellChangeThread;
begin
  if Assigned(FThread) then
  begin
    Temp := FThread;
    FThread := nil;
    Temp.Terminate;
    ReleaseMutex(Temp.FMutex);
  end;
  inherited;
end;

procedure TCustomShellChangeNotifier.SetRoot(const Value: String);
begin
  if not SameText(FRoot, Value) then begin
    FRoot := Value;
    Change;
  end;
end;

procedure TCustomShellChangeNotifier.SetFilters(const Value: TNotifyFilters);
begin
  FFilters := Value;
  Change;
end;

procedure TCustomShellChangeNotifier.SetOnChange(const Value: TThreadMethod);
begin
  FOnChange:= Value;
  if Assigned(FThread) then
    FThread.ChangeEvent := FOnChange
  else
    Start;
end;

procedure TCustomShellChangeNotifier.SetWatchSubTree(const Value: Boolean);
begin
  FWatchSubTree := Value;
  Change;
end;

procedure TCustomShellChangeNotifier.Start;

  function NotifyOptionFlags: DWORD;
  begin
    Result := 0;
    if nfFileNameChange in FFilters then
      Result := Result or FILE_NOTIFY_CHANGE_FILE_NAME;
    if nfDirNameChange in FFilters then
      Result := Result or FILE_NOTIFY_CHANGE_DIR_NAME;
    if nfSizeChange in FFilters then
      Result := Result or FILE_NOTIFY_CHANGE_SIZE;
    if nfAttributeChange in FFilters then
      Result := Result or FILE_NOTIFY_CHANGE_ATTRIBUTES;
    if nfWriteChange in FFilters then
      Result := Result or FILE_NOTIFY_CHANGE_LAST_WRITE;
    if nfSecurityChange in FFilters then
      Result := Result or FILE_NOTIFY_CHANGE_SECURITY;
  end;

begin
  if Assigned(FOnChange) then
  begin
    FThread := TShellChangeThread.Create(FOnChange);
    FThread.SetDirectoryOptions(FRoot,
      LongBool(FWatchSubTree), NotifyOptionFlags);
    FThread.Resume;
  end;
end;

{ TShellChangeThread }

constructor TShellChangeThread.Create(ChangeEvent: TThreadMethod);
begin
  FreeOnTerminate := True;
  FChangeEvent := ChangeEvent;
  FMutex := CreateMutex(nil, True, nil);
  //Mutex is used to wake up the thread as it waits for any change notifications.
  WaitForSingleObject(FMutex, INFINITE); //Grab the mutex.
  FWaitChanged := false;
  inherited Create(True);
end;

destructor TShellChangeThread.Destroy;
begin
  if FWaitHandle <> ERROR_INVALID_HANDLE then
    FindCloseChangeNotification(FWaitHandle);
  CloseHandle(FMutex);
  inherited Destroy;
end;

procedure TShellChangeThread.Execute;
var
  Obj: DWORD;
  Handles: array[0..1] of DWORD;
begin
  EnterCriticalSection(CS);
  FWaitHandle := FindFirstChangeNotification(PChar(FDirectory),
     LongBool(FWatchSubTree), FNotifyOptionFlags);
  LeaveCriticalSection(CS);
  if FWaitHandle = ERROR_INVALID_HANDLE then Exit;
  while not Terminated do begin
    Handles[0] := FWaitHandle;
    Handles[1] := FMutex;
    Obj := WaitForMultipleObjects(2, @Handles, False, INFINITE);
    case Obj of
      WAIT_OBJECT_0:
        begin
          Synchronize(FChangeEvent);
          FindNextChangeNotification(FWaitHandle);
        end;
      WAIT_OBJECT_0 + 1:
        ReleaseMutex(FMutex);
      WAIT_FAILED:
        Exit;
    end;
    EnterCriticalSection(CS);
    if FWaitChanged then
    begin
      FWaitHandle := FindFirstChangeNotification(PChar(FDirectory),
         LongBool(FWatchSubTree), FNotifyOptionFlags);
      FWaitChanged := false;
    end;
    LeaveCriticalSection(CS);
  end;
end;

procedure TShellChangeThread.SetDirectoryOptions(Directory: String;
  WatchSubTree: Boolean; NotifyOptionFlags: DWORD);
begin
  EnterCriticalSection(CS);
  FDirectory := Directory;
  FWatchSubTree := WatchSubTree;
  FNotifyOptionFlags := NotifyOptionFlags;

  // Release the current notification handle
  FindCloseChangeNotification(FWaitHandle);
  FWaitChanged := true;
  LeaveCriticalSection(CS);
end;

{ GetShellImage }
function GetShellImage(PIDL: PItemIDList; ALarge, AOpen: Boolean): Integer;
var
  FileInfo: TSHFileInfo;
  Flags: Integer;
begin
  Flags:= SHGFI_PIDL or SHGFI_SYSICONINDEX;
  if AOpen then Flags := Flags or SHGFI_OPENICON;
  if ALarge then Flags := Flags or SHGFI_LARGEICON
  else Flags := Flags or SHGFI_SMALLICON;
  SHGetFileInfo(PChar(PIDL),
                0,
                FileInfo,
                SizeOf(FileInfo),
                Flags);
  Result := FileInfo.iIcon;
end;

// GetDefaultFileImageIndex return image index for a file at the system icons set.
// Returned icon can be used to display remote files.)}
function GetDefaultFileImageIndex(ALarge, AOpen: Boolean): Integer;
var
  FileInfo: TSHFileInfo;
  Flags: Integer;
  ff: THandle;
  fd: TWin32FindData;
  fn: String;
  wd: array[0..MAX_PATH] of Char;
begin
  Result:= 4;
  Flags:= SHGFI_SYSICONINDEX;
  if AOpen then Flags := Flags or SHGFI_OPENICON;
  if ALarge then Flags := Flags or SHGFI_LARGEICON;

  if Windows.GetWindowsDirectory(wd, SizeOf(wd)) > 0 then begin
    fn:= wd + '\*.ini';
    FillChar(fd, SizeOf(fd), 0);
    ff:= Windows.FindFirstFile(PChar(@(fn[1])), fd);
    if ff <> INVALID_HANDLE_VALUE then begin
      fn:= PChar(@wd) + '\' + fd.cFileName;
      if SHGetFileInfo(PChar(fn), 0, FileInfo, SizeOf(FileInfo), Flags) <> 0 then begin
        Result:= FileInfo.iIcon;
      end;
      Windows.FindClose(ff);
    end;
  end;
end;

// GetDefaultFolderImageIndex return image index for a folder at the system icons set.
// Returned icon can be used to display remote files.)}
function GetDefaultFolderImageIndex(ALarge, AOpen: Boolean): Integer;
var
  FileInfo: TSHFileInfo;
  Flags: Integer;
  wd: array[0..MAX_PATH] of Char;
begin
  Result:= 5;
  Flags:= SHGFI_SYSICONINDEX;
  if AOpen then Flags := Flags or SHGFI_OPENICON;
  if ALarge then Flags := Flags or SHGFI_LARGEICON;
  if Windows.GetWindowsDirectory(wd, SizeOf(wd)) > 0 then begin
    if SHGetFileInfo(wd, 0, FileInfo, SizeOf(FileInfo), Flags) <> 0 then begin
      Result:= FileInfo.iIcon;
    end;
  end;
end;

function GetShellImageListItem(AListItem: TListItem; Large, Open: Boolean): Integer;
begin
  Result:= AListItem.ImageIndex;
end;

procedure InvokeContextMenu(Owner: TWinControl; AFolder: IShellFolder;
  AFN_PidList: PItemIdList; X, Y: Integer);
var
  CM: IContextMenu;
  Menu: HMenu;
  ICI: TCMInvokeCommandInfo;
  P: TPoint;
  Command: LongBool;
  ICmd: integer;
  ZVerb: array[0..255] of char;
  Verb: string;
  Handled: boolean;
  SCV: IShellCommandVerb;
  HR: HResult;
begin
  if AFolder = nil then Exit;
  AFolder.GetUIObjectOf(Owner.Handle, 1, AFN_PidList, IID_IContextMenu, nil, CM);
  if CM = nil then Exit;
  P.X := X;
  P.Y := Y;

  Windows.ClientToScreen(Owner.Handle, P);
  Menu := CreatePopupMenu;
  try
    CM.QueryContextMenu(Menu, 0, 1, $7FFF, CMF_EXPLORE or CMF_CANRENAME);
    CM.QueryInterface(IID_IContextMenu2, ICM2); //To handle submenus.
    try
      Command := TrackPopupMenu(Menu, TPM_LEFTALIGN or TPM_LEFTBUTTON or TPM_RIGHTBUTTON or
        TPM_RETURNCMD, P.X, P.Y, 0, Owner.Handle, nil);
    finally
      ICM2 := nil;
    end;

    if Command then
    begin
      ICmd := LongInt(Command) - 1;
      HR := CM.GetCommandString(ICmd, GCS_VERBA, nil, ZVerb, SizeOf(ZVerb));
      Verb := StrPas(ZVerb);
      Handled := False;
      if Supports(Owner, IShellCommandVerb, SCV) then
      begin
        HR := 0;
        SCV.ExecuteCommand(Verb, Handled);
      end;

      if not Handled then
      begin
        FillChar(ICI, SizeOf(ICI), #0);
        with ICI do
        begin
          cbSize := SizeOf(ICI);
          hWND := Owner.Handle;
          lpVerb := MakeIntResource(ICmd);
          nShow := SW_SHOWNORMAL;
        end;
        HR := CM.InvokeCommand(ICI);
      end;

      if Assigned(SCV) then
        SCV.CommandCompleted(Verb, HR = S_OK);
    end;
  finally
    DestroyMenu(Menu);
  end;
end;

function GetFileAttributesString(AFileAttributes: Integer): String;
var
  Flags: LongWord;
begin
  Result:= '';
  if (SFGAO_GHOSTED and AFileAttributes) <> 0 then Result:= Result + 'G'; // Cut
  if (SFGAO_LINK and AFileAttributes) <> 0 then Result:= Result + 'L'; // IsLink
  if (SFGAO_READONLY and AFileAttributes) <> 0 then Result:= Result + 'R'; // ReadOnly
  if (SFGAO_SHARE and AFileAttributes) <> 0 then Result:= Result + 'S'; // Shared
  if (SFGAO_FILESYSTEM and AFileAttributes) = 0 then Result:= Result + 'Y'; // FyleSystem
  if (SFGAO_FILESYSANCESTOR and AFileAttributes) <> 0 then Result:= Result + 'A'; // SystemAncestor
  if (SFGAO_REMOVABLE and AFileAttributes) <> 0 then Result:= Result + 'M'; // Removable
  if (SFGAO_VALIDATE and AFileAttributes) <> 0 then Result:= Result + 'V'; // Validate
end;

function GetLastErrorDescription(AError: DWORD): WideString;
begin
  if (AError = 0)
  then Result:= ''
  else begin
    SetLength(Result, 1024);
    SetLength(Result, FormatMessageW(FORMAT_MESSAGE_FROM_SYSTEM, Nil,
      AError, LOCALE_USER_DEFAULT, PWideChar(Result), Length(Result), Nil));
  end;
end;


var
  Shell32DLLHandle: HMODULE;

procedure LoadNotifyFuncs;
begin
  Shell32DLLHandle:= LoadLibrary(Shell32DLL);
  if Shell32DLLHandle <> 0 then begin
    SHChangeNotifyRegister:= GetProcAddress(Shell32DLLHandle, 'SHChangeNotifyRegister'); // index 2;
    SHChangeNotifyDeRegister:= GetProcAddress(Shell32DLLHandle, 'SHChangeNotifyDeregister'); // index 2;
  end;
end;

procedure UnLoadNotifyFuncs;
begin
  SHChangeNotifyRegister:= Nil;
  SHChangeNotifyRegister:= Nil;
  if Shell32DLLHandle <> 0 then
    FreeModule(Shell32DLLHandle);
end;

// ------------------------------ helper functions -----------------------------

function GetListViewNodeType(AListItem: TListItem): TNodeType;
begin
  Result:= TNodeType(AListItem.Data);
end;

function GetListViewNodeName(AListItem: TListItem): String;
begin
  if AListItem.SubItems.Count < 5 then
    Result:= ''
  else
    Result:= AListItem.SubItems[4];
end;

function GetListViewNodeSize(AListItem: TListItem): Int64;
begin
  if AListItem.SubItems.Count < 1 then
    Result:= -1
  else begin
    if TNodeType(AListItem.Data) = ntFolder then
      Result:= 0
    else
      Result:= StrToIntDef(AListItem.SubItems[0], 0);
  end;
end;

function GetListViewNodeTypeDesc(AListItem: TListItem): String;
begin
  if AListItem.SubItems.Count < 2 then
    Result:= ''
  else
    Result:= AListItem.SubItems[1];
end;

function GetListViewNodeAttrs(AListItem: TListItem): Integer;
begin
  if AListItem.SubItems.Count < 4 then
    Result:= -1
  else begin
    Result:= Integer(AListItem.SubItems.Objects[3]); // StrToIntDef(AListItem.SubItems[3], 0);
  end;
end;

function GetListViewNodeDate(AListItem: TListItem): TDateTime;
begin
  if AListItem.SubItems.Count < 3 then
    Result:= 0
  else begin
    Result:= StrToDateTimeDef(AListItem.SubItems[2], 0);
  end;
end;

// Example: SetNodeState(DropNode, TVIS_BOLD);
//
procedure SetNodeState(ANode: TTreeNode; AFlags: Integer);
var
  tvi: TTVItem;
begin
  FillChar(tvi, Sizeof(TTVItem), 0);
  with tvi do begin
    hItem:= Anode.ItemID;
    mask:= TVIF_STATE;
    stateMask:= TVIS_FOCUSED or TVIS_SELECTED or TVIS_CUT or TVIS_DROPHILITED or
      TVIS_BOLD or TVIS_EXPANDED or TVIS_EXPANDEDONCE or TVIS_EXPANDPARTIAL;
    State:= AFlags;
  end;
  TreeView_SetItem(ANode.Handle, tvi);
end;

function SendFileToRecycleBin(const AFileName: string): Boolean;
var
  FileOp: TSHFileOpStruct;
  fn: ShortString;
  len: Integer;
begin
  FillChar(FileOp, SizeOf(FileOp), 0);
  fn:= AFileName;
  len:= Length(AFileName);
  fn[len + 1]:= #0; // it is used as a buffer to hold multiple file names.
  fn[len + 2]:= #0; // Each file name must be terminated by a single NULL character. An additional NULL character must be appended to the end of the final name to indicate the end of pFrom.
  with FileOp do begin
    Wnd:= Forms.Application.Handle;
    wFunc:= FO_DELETE;
    pFrom:= PChar(@fn[1]);
    fFlags:= FOF_ALLOWUNDO or FOF_NOERRORUI or FOF_SILENT or FOF_NOCONFIRMATION;
  end;
  Result:= (SHFileOperation(FileOp) = 0) and (not FileOp.fAnyOperationsAborted);
end;

function MoveFileToDest(const AFileName: string; const ADest: String): Boolean;
var
  dst: String;
  len: Integer;
begin
  dst:= ADest;
  if DirectoryExists(dst) then begin
    len:= Length(dst);
    if (len > 0) and (dst[len] <> '\') then
      // dst:= dst + '\';
  end;
  Result:= Windows.MoveFile(PChar(AFileName), PChar(dst));
  if not Result then begin
    //
    raise Exception.Create(GetLastErrorDescription(GetLastError));
  end;
end;

function CopyFileToDest(const AFileName: string; const ADest: String; AOverride: Boolean): Boolean;
var
  dfn: String;
begin
  if DirectoryExists(ADest) then
    dfn:= ADest + '\' + ExtractFileName(AFileName)
  else
    dfn:= ADest;

  Result:= Windows.CopyFile(PChar(AFileName), PChar(dfn), not AOverride);
end;

function FindChildNode(AParentNode: TTreeNode; const AStr: String): TTreeNode;
var
  n: TTreeNode;
begin
  Result:= Nil;
  n:= AParentNode.GetFirstChild;
  while (n <> nil) do begin
    if (n.Text = AStr) then begin
      Result:= n;
      Exit;
    end;
    n:= AParentNode.GetNextChild(n);
  end;
end;

// Return file name from URL in PC DOS format
function Url2LocalFileName(const AUrl: String): String;
var
  user, host, password, IPaddress, bookmark: String;
  port: Integer;
begin
  urlFuncs.ParseUrl(AUrl, Result, user, password, host, IPaddress, Result, bookmark, port, 'http', 80);
  Result:= Strutils.AnsiReplaceStr(Result, '/', '\');
  if Pos('\', Result) = 1 then
    System.Delete(Result, 1, 1);
end;

// Return URL-like address of given file name
function LocalFileName2Url(const AProtocol, AHost, AFileName: String): String;
begin
  Result:= Strutils.AnsiReplaceStr(AFileName, '\', '/');
  if Length(Result) > 0 then
    Result:= AProtocol + '://' + AHost + '/' + Result
  else
    Result:= AProtocol + '://' + AHost;
end;

initialization
  LoadNotifyFuncs;

finalization
  UnLoadNotifyFuncs;

end.
