unit
  RTCFolderTree;
(*##*)
(*******************************************************************************
*                                                                             *
*   R  T  C  F  o  l  d  e  r  T  r  e  e                                      *
*                                                                             *
*   Folder tree component based on TCustomTreeView                             *
*                                                                             *
*   Copyright (c) 2007, Andrei Ivanov, RealThinComponents                      *
*                                                                             *
*   Conditional defines:                                                       *
*                                                                             *
*   Last Revision: Nov 01 2007                                                 *
*   First release: Jul 29 2000                                                *
*   Lines:         3677                                                        *
*   History:       Jul 29 2001                                                *
*   Notes:                                                                     *
*     file system call ShellAPI.SHGetFileInfo                                 *
*     called from:                                                             *
*     CreateJFSFolderNode FGetSpecialFolder RetrieveSysImageList              *
*  TListView with a vsReport style causes AVE when you run with a XP manifest  *
*  resource. The VCL D6 has a bug and you must patch sources comctrls.pas     *
*  UpdateColumn method.                                                        *
*                                                                             *
*   MessageProc calls DefWindowProc, Dialog uses ShBrowseForFolder             *
*   to inform about filesystem changes                                        *
*                                                                              *
*   Dialog calls SysUtils.FileGetAttr instead SHGetFileInfo                   *
********************************************************************************)
(*##*)

{$R-}
{$WARNINGS OFF} // deprecated symbols, Move(ptr^..), @
{$IFNDEF VER90}{$IFNDEF VER91}{$IFNDEF VER92}
   {$DEFINE DELPHI3UP}
{$ENDIF}{$ENDIF}{$ENDIF}
{
  @html(<b>)
  Folder Tree
  @html(</b>)
  @html(<br><br>)
  Introducing the @html(<b>) @Link(TrtcFolderTree) @html(</b>) component:
  @html(<br>)
  Folder tree component based on TCustomTreeView.
}

interface

uses
  Messages, SysUtils, Windows, Classes, Graphics, Controls,
  ShellAPI, // if Explorer is not running?
  ComCtrls, ShlObj,
  Contnrs, // TStack
  ActiveX,
  Forms, // Application handle is required for some Shell operations
{$IFDEF DELPHI3UP}
  ComObj,
{$ELSE}
  FileCtrl, OLE2,
{$ENDIF}
  ImgList, CommCtrl,
  rtcShellHelper, rtcListViewAdaptor;

const
  COMPONENT_PALETTE = 'Samples'; // 'RTCControls'
  MAX_HISTORY_SIZE = 8;

type
  // Nov 14 2007
  { @Abstract(Internal state of folder tree)
  }
  TFolderTreeState = (fsUpdatingListView, fsWaitExpandDir);
  { @Abstract(Internal states of folder tree)
  }
  TFolderTreeStates = set of TFolderTreeState;

  { @Abstract(callback function return a list of external storages. For instance, if AFolderType = ftFTPList,
    callback return a list of 'ftp://site' strings.
    Function must be a part of object such TForm.
    Parameters: AFolderType of type @Link(TFolderType) can be ftFTPList, ftHTTPList.
    Result is returned in parameter ASiteList.
    )
  }
  TOnListExtStorageSites = procedure(AFolderType: TFolderType; ASiteList: TStrings) of object;

  { @Abstract(Folder Tree visual component shows folders and files stored in JFS and other storages)

    For specific storages like FTP first need to implement @Link(TOnListExtStorageSites)
    callback function which lists of all available FTP storages and implement
    @Link(TOnExtStorageFolderList) callback function. Both callbacks must be a
    method of object like TForm.
    @html(<br><br>)

    Properties to check first:
    @html(<br>)
    @Link(TrtcFolderTree.Active) -
    @html(<br>)
    @Link(TrtcFolderTree.FolderOptions) -
    @html(<br><br>)

    Methods to check first:
    @html(<br>)
    @Link(TRtcClient.Connect) - Connect to Server
    @html(<br>)
    @Link(TRtcDataClient.Request), @Link(TRtcHttpClient.WriteHeader), @Link(TRtcHttpClient.Write) - Write (send) Request to Server
    @html(<br>)
    @Link(TRtcDataClient.Response), @Link(TRtcConnection.Read) - Read Server's Response
    @html(<br>)
    @Link(TRtcConnection.Disconnect) - Disconnect from Server
    @html(<br><br>)

    Events to check first:
    @html(<br>)
    @Link(TRtcConnection.OnConnect) - Connected to Server
    @html(<br>)
    @html(<br><br>)

    Check @Link(TRtcClient) and @Link(TRtcConnection) for more info.
    }

  THistoryStack = class(TStack)
  protected
    procedure PushItem(AItem: Pointer); override;
  end;

  TrtcFolderTree = class(TCustomTreeView)
  private
    { Private declarations }
    FDefaultFolderImageIndex: Integer;
    FDefaultFileImageIndex: Integer;
    FDefaultHostImageIndex: Integer;
    FFileMask: String;  // like '*.htm;*.html'
    fDesktopPath: String;
    fNetHoodPath: String;
    SysImageList: TImageList;
    FMessageHandle: hWnd;
    FDisplayMode: TDisplayMode;
    FFolderOptions: SFolderOption;
    AllocInterface: IMalloc;
    NotifyRegister: PNotifyRegister;
    NotifyHandle: hwnd;
    FEndSessionQuery: TEndSessionQueryEvent;
    Folder_List: TList;
    FActive: Boolean;
    FWndDestroying: Boolean;
    FStates: TFolderTreeStates;
    // used to transfer settings data (DestroyWnd ->CreateWnd) while window recreated
    FFolderTreeInfoMemStream: TMemoryStream;
    // Get ftp, ldap and other external (network) storage sites list callback
    FOnListExternalStorageSites: TOnListExtStorageSites;
    FOnExternalStorageFolderList: TOnExtStorageFolderList;
    FOnExternalAsyncStorage: TOnExtStorageAsyncFolderList;
    // delete a file(s) by url or FQFN on external storage
    FOnExtDeleteUrl: TOnExtDeleteUrl;
    // move/rename file(s) by url or FQFN on external storage
    FOnExtMoveUrl: TOnExtMoveUrl;
    // copy file(s) by url or FQFN on external storage
    FOnExtCopyUrl: TOnExtCopyUrl;
    // associated list view control to show files in the selected folder
    // FFilesView: TCustomListView;
    FFilesViewAdaptor: TrtcListViewAdaptor;
    FOnDirectoryChanged: TNotifyEvent;
    FHistory: THistoryStack;
    procedure SetFileMask(AValue: String);
    function FGetNetHood: TPathInfo;
    function FGetSpecialFolder(NFolder: Integer): TPathInfo;
    function CreateJFSFolderNode(APidList: PItemIdList; AParent: TTreeNode;
      AVirtualOk: Boolean): TTreeNode;

    function CreateExtStorageNode(AFolderType: TFolderType; APathInfo: TPathInfo; AParent: TTreeNode): TTreeNode;
    procedure RetrieveSysImageList;

    procedure BuildDesktop;
    procedure RebuildDesktop;

    procedure ShowMyComputer(AShow: Boolean);
    procedure ShowNetHood(AShow: Boolean);
    procedure ShowRecycleBin(AShow: Boolean);
    procedure ShowStorageSites(AFolderType: TFolderType; AShow: Boolean);
    procedure SetDisplayMode(AValue: TDisplayMode);
    procedure SetFolderOptions (AValue: SFolderOption);

    function  GetLongFilePath(const Pid: PItemIdList;
      ShellFolder: IShellFolder): TFilePathList;
    function  GetPathName(const Pid: PItemIdList; Flags: DWORD;
      ShellFolder: IShellFolder): String;
    // AttachFolders unconditionally attaches child folders to the node passed in Node.
    function  AttachFolders(var Node: TTreeNode): Boolean;
    // JFS
    function  AttachJFSFolders(var Node: TTreeNode): Boolean;
    // AttachStorageFolders() call appropriate methods for nodes of JFS, FTP and other nodes
    function  AttachStorageFolders(AFolderType: TFolderType; var Node: TTreeNode): Boolean;
    function  FindPath(const APath: String; AExact: Boolean): TTreeNode;
    procedure MessageProc(var Message: TMessage);
    function  ParsePidl (Pidl: PITEMIDLIST): String;
    procedure FreeChildren(Node: TTreeNode);
    // onDeletion & onAddition called if parent control is docked to dock
    procedure FreeNode(Sender: TObject; Node: TTreeNode);
    procedure DriveChanged(Sender: TObject; FirstDriveLetter: Char; force: Boolean;
      AInserted: Boolean);
    procedure FolderTreeExpanding(Sender: TObject; Node: TTreeNode;
      var AllowExpansion: Boolean);
    procedure TrtcFolderTreeClick(var Message: TMessage); message WM_LBUTTONUP;
    procedure TrtcFolderTreeDblClick(var Message: TMessage); message WM_LBUTTONDBLCLK;
    procedure CheckPath;
    function  GetDirectory: String;
    procedure SetDirectory(const AValue: String);
    function  GetDesktopNode: TTreeNode;
    function  GetMyComputerNode: TTreeNode;
    function  GetNetHoodNode: TTreeNode;
    function  GetRecyledNode: TTreeNode;
    function  GetSpecialNode(nodetype: TFolderType): TTreeNode;
    procedure RegisterChangeNotify;
    procedure DeregisterChangeNotify;
    procedure SetActive(Value: Boolean);
    procedure Activate;
    procedure Deactivate;
    function ListPath(var APath: String; AList: TStrings): Integer;
    procedure ListViewClick(Sender: TObject);
    procedure SetFilesList(AValue: TCustomListView);
    { @Abstract(Return protocol used for transfer URL. Extact host name and DOS/Windows file name }
    class function ExtractExtProtocol(const AUrl: String; var AHost, AFileName: String): String;
    // drag'n'drop
    procedure FDragOverEvent(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure FDragDropEvent(Sender, Source: TObject; X, Y: Integer);
    procedure SetOnExtAsyncStorage(AValue: TOnExtStorageAsyncFolderList);
    procedure SetOnExtDeleteUrl(AValue: TOnExtDeleteUrl);
    procedure SetOnExtMoveUrl(AValue: TOnExtMoveUrl);
    procedure SetOnExtCopyUrl(AValue: TOnExtCopyUrl);
  protected
    { @Abstract(@Abstract(method invoked by the WM_QUERYENDSESSION message is sent
    when the user chooses to end the session or when an application calls the ExitWindows function.
    You can close network connection and free resources before Windows close a session.
    if CanEndSession is set to False, it inform Windows that user must finish his job and
    close application 'normally') }
    procedure QueryEndSession(var AMsg: TMessage);
    // @exclude
    procedure WndProc(var AMsg: TMessage); override;
    // @exclude
    procedure CreateWnd; override;
    // @exclude
    procedure DestroyWnd; override;
    {
    function CustomDrawItem(Node: TTreeNode; State: TCustomDrawState;
      Stage: TCustomDrawStage; var PaintImages: Boolean): Boolean; override;
    }
    { @Abstract(EmbeddedOnCustomDrawItem. Protected method draw node at FolderTree
      Default method used to draw FTP nodes with icon associated with file extension at Client computer.
    )}
    procedure EmbeddedOnCustomDrawItem(Sender: TCustomTreeView; Node: TTreeNode;
      State: TCustomDrawState; var DefaultDraw: Boolean);
  public
    { Flush all buffered data.
      @html(<br>)
      ...
      @html(<br>)
    }
    { @Abstract(After component is created, set @Link(TrtcFolderTree.FolderOptions)
      first to view folders only or folders and files.
      You do not need to call Create if component is not created dynamically.
    )}
    constructor Create(AOwner: TComponent); override;
    { @Abstract(You do not need to call Destroy or Free destructors if component is not created dynamically.
    )}
    destructor Destroy; override;
    { @Abstract(Open folder tree at the node specified by path. Returns node if
      file path exists. Otherwise returns Nil.
    )}
    function GoToDirectory(const ADir: String; ASelect: Boolean): TTreeNode;

    { @Abstract(Returns file path of the node.
    )}
    function GetPath(Node: TTreeNode): String;
    { @Abstract(Returns interface of node)}
    function NodeShellFolder(Node: TTreeNode): IShellFolder;
    { @Abstract(Returns type of storage of folder or file. Folder of ftMyComputer is 'usual' JFS file.
      Other files can stored in different types of storage, like IE cache, PocketPC sync etc.)}
    function FolderType(Node: TTreeNode): TFolderType;
    { @Abstract(Returns is node a folder or a file) }
    function NodeType(Node: TTreeNode): TNodeType;
    { @Abstract(Returns  identifier of node) }
    function NodeItemIdList(Node: TTreeNode): PItemIdList;
    { @Abstract(Returns  letter of the first 'drive') }
    function GetFirstDriveLetter(UnitMask: LongInt): Char;
    { @Abstract(Return attached node. If AShow=False, remove node and return Nil.
      Parameter AUrl [desc=]http://HOST/c:\dir\file.ext) }
    // Nov 13 2007
    function AttachStorageSite(AFolderType: TFolderType;
      AUrl: String; AShow: Boolean): TTreeNode;
    procedure CreateJFSFolderListItem(APidList: PItemIdList;
      AParent: TTreeNode; AVirtualOk: Boolean;
      var NewNodeData: TFolderNode; var FileInfo: TSHFileInfo; var Attributes: Cardinal);
    function CreateExtStorageFolderNode(AFolderType: TFolderType;
      AParent: TTreeNode; const AURL, AName: String; ANodeType: TNodeType): TTreeNode;
    procedure FreePidl(var APidl: PItemIdList);
    function FileIndexOfMask(const AFileName: String): Integer;
      { @Abstract(DirUpdateFiles called from TrtcPFileBrowser to update view when event is received}
    function DirUpdateFiles(AFolderType: TFolderType; const ADir: String;
      AFiles: TRemoteFileInfos; AShow: Boolean): Boolean;
  // list view specific
    {
    // @Abstract(Find a file list item in associated list view by fully qualified file name or url)
    function FindListItem(const AUrl: String): TListItem;
    // @Abstract(get a url of file item in associated list view.
    //  Return '' if not item is selected of File List is not associated with)
    function FileInList(AItem: TListItem): String;
    // @Abstract(get a url of file item in associated list view.
    //  Return '' if not item is selected of File List is not associated with)
    function FileInLV(AItem: TLVItem): String;
    // @Abstract(get a url of file itme selected in associated list view )
    function SelectedFileInList: String;
    // @Abstract(get a list of url of files selected in associated list view )
    function SelectedFilesList(var AResult: TStrings): Integer;
    // @Abstract(Delete a file by fully qualified file name or url )
    function RemoveFileByUrl(const AUrl: String): Boolean;
    // @Abstract(Delete a file by one selected item)
    function RemoveFileListItem(AItem: TListItem): Boolean;
    //@Abstract(Delete a file by one selected item (False) or all selected items (MultiSelect=True))
    function RemoveFileListItems(AAllSelected: Boolean): Boolean;
    // @Abstract(Move/Rename a file)
    function MoveFile(const ASrcUrl, ADestUrl: String): Boolean;
    // @Abstract(Copy file(s))
    function CopyFile(const ASrcUrl, ADestUrl: String): Boolean;
    }
    { @Abstract(ValidateHosts check is passed hosts displayed.
      If there are hosts displayed but not in a list, FolderTree delete them.
      AFolderType must be ftHTTPNode) }
    procedure ValidateHosts(AFolderType: TFolderType; AHosts: TStrings);
    { @Abstract(Go to the parent directory. Return a new directory) }
    function GoParent(): String;
    { @Abstract(Go back. Return a new directory) }
    function GoBack(): String;
    { @Abstract(Go forward. Return a new directory) }
    function GoForward(): String;
    { @Abstract(Lists the nodes) }
    property Items;
    { @Abstract(Selected node. Returns Nil if no file or folder is selected) }
    property Selected;
    { @Abstract(Current directory. Path of selected folder or file) }
    property Directory: String read GetDirectory write SetDirectory;
  published
    {@exclude}
    property Active: Boolean read FActive write SetActive;
    {@exclude}
    property PopupMenu;
    {@exclude}
    property OnDblClick;
    {@exclude}
    property OnChange;
    {@exclude}
    property OnClick;
    {@exclude}
    property OnDragOver;
    {@exclude}
    property OnDragDrop;
    {@exclude}
    property ShowButtons;
    {@exclude}
    property ShowLines;
    {@exclude}
    property ShowRoot;
    {@exclude}
    property HideSelection;
    {@exclude}
    property Align;
    {@exclude}
    property BorderStyle;
    {@exclude}
    property Color;
    {@exclude}
    property Ctl3D;
    {@exclude}
    property DragCursor;
    {@exclude}
    property DragMode;
    {@exclude}
    property Enabled;
    {@exclude}
    property Font;
    {@exclude}
    property Indent;
    {@exclude}
    property OnEnter;
    {@exclude}
    property OnExit;
    {@exclude}
    property ParentCtl3D;
    {@exclude}
    property ParentFont;
    {@exclude}
    property ParentShowHint;
    {@exclude}
    property ShowHint;
    {@exclude}
    property TabOrder;
    {@exclude}
    property TabStop default True;
    {@exclude}
    property Visible;
    {@Abstract(FolderOptions. After FolderTree is created, component does not show any
    folders and files. First
    foMyComputer: show folders at the hard disks of PC,
    foNetworkNeighborhood: show machines at the Windows network
    foRecycleBin: show special folder Recycle Bin,
    foFTP: show list of available ftp sites (requires @Link(TrtcFolderTree.OnListExternalStorageSites)
    foDatabase: reserved
    foLDAP: reserved
    foHTTP: show list of RTC Hosts (requires @Link(TrtcFolderTree.OnListExternalStorageSites))
    foFiles: show files in the folder tree. If not set, shows folders only.
    foVirtualFirst: sort nodes)
    }
    property FolderOptions: SFolderOption read fFolderOptions write SetFolderOptions;
    {@Abstract(Display mode of the file name: As is, uppercase or lowercase)
    }
    property DisplayMode: TDisplayMode read fDisplayMode write SetDisplayMode default dmAsis;
    {@Abstract(image index of file icon) used by remote connections. This property is read only.)}
    property DefaultFileImageIndex: Integer read FDefaultFileImageIndex default 4;
    {@Abstract(image index of folder icon) used by remote connections. This property is read only.)}
    property DefaultFolderImageIndex: Integer read FDefaultFolderImageIndex default 0;
    {@Abstract(image index of host icon) used by remote connections. This property is read only.)}
    property DefaultHostImageIndex: Integer read FDefaultHostImageIndex default 8;
    {@Abstract(File mask is wildcard such '*.*'. Default wildcard is '*.* (show all files with any file extension).
     Masks are separated by semicolon ';'. For instance, wildcard '*.pas;*.dfm'  will
     show Delphi source files (PAS) and Delphi form files (DFM))}
    property FileMask: String read FFileMask write SetFileMask;
    {@Abstract(ControlListView property is a linked file list control.
     Can be unassigned (Nil) it mean no file list ia associated with folder tree.
     In this case use foFiles option to see files in the folder tree)}
     // read FFilesView
    property ControlListView: TCustomListView write SetFilesList;
    property FilesViewAdaptor: TrtcListViewAdaptor read FFilesViewAdaptor; // write FFilesViewAdaptor;

    {@exclude}
    property OnCustomDrawItem;
    { @Abstract(Assign OnEndSessionQuery to contol situation when WM_QUERYENDSESSION message is sent
    when the user chooses to end the session or when an application calls the ExitWindows function.
    For more information see @Link(TrtcFolderTree.QueryEndSession) and @Link(TEndSessionQueryEvent)
    ) }
    property OnEndSessionQuery: TEndSessionQueryEvent read FEndSessionQuery write FEndSessionQuery;
    { @Abstract(Assign OnEndSessionQuery to contol situation when WM_QUERYENDSESSION message is sent
    when the user chooses to end the session or when an application calls the ExitWindows function.
    See also: @Link(TrtcFolderTree.OnExternalStorageFolderList), @Link(TOnListExtStorageSites) and @Link(TEndSessionQueryEvent)
    ) }
    property OnListExternalStorageSites: TOnListExtStorageSites read FOnListExternalStorageSites write FOnListExternalStorageSites;
    { @Abstract(Return a list of files and subfolders within specified location (AURL). List contains an Objects[i] of 0 or 1.
    if Integer(Objects[i] = 0, it is a file, otherwise it is a folder.
    Assign this property to the method if you want add a new external storages like
    FTP, WebDAV or database.
    Parameters: AFolderType of type @Link(TFolderType) can be ftFTP, ftHTTP.
    AUrl specify location of FTP directory.
    Result is returned in parameter AFolderList.
    See also: @Link(TrtcFolderTree.OnListExternalStorageSites), @Link(TOnListExtStorageSites) and @Link(TEndSessionQueryEvent)
    )}
    property OnExternalStorageFolderList: TOnExtStorageFolderList read FOnExternalStorageFolderList write FOnExternalStorageFolderList;
    { @Abstract(Return a list of files and subfolders within specified location (AURL). List contains an Objects[i] of 0 or 1.
    if Integer(Objects[i] = 0, it is a file, otherwise it is a folder.
    Assign this property to the method if you want add a new external storages like
    FTP, WebDAV or database.
    Parameters: AFolderType of type @Link(TFolderType) can be ftFTP, ftHTTP.
    AUrl specify location of FTP directory.
    Result is returned in parameter AFolderList.
    See also: @Link(TrtcFolderTree.OnListExternalStorageSites), @Link(TOnListExtStorageSites) and @Link(TEndSessionQueryEvent)
    )}
    property OnExternalAsyncStorage: TOnExtStorageAsyncFolderList read FOnExternalAsyncStorage write SetOnExtAsyncStorage;
    property OnExternalDeleteUrl: TOnExtDeleteUrl read FOnExtDeleteUrl write SetOnExtDeleteUrl;
    property OnExternalMoveUrl: TOnExtMoveUrl read FOnExtMoveUrl write SetOnExtMoveUrl;
    property OnExternalCopyUrl: TOnExtCopyUrl read FOnExtCopyUrl write SetOnExtCopyUrl;
    property States: TFolderTreeStates read FStates write FStates;
    property OnDirectoryChanged: TNotifyEvent read FOnDirectoryChanged write FOnDirectoryChanged;
  end;

  { @Abstract(TFolderBrowseDialog helper dialog component is intended for select local folder from the dialog.)}
  TFolderBrowseDialog = class(TComponent)
  private
    FDirectory: String;
    FTitle: String;
    FBrowseInfo: TBrowseInfo;
  public
    {@exclude}
    function Execute(Form: TWinControl): Boolean;
  published
    { @Abstract(Current (selected) folder file path. Returns '' if not selected.)}
    property Directory: String read FDirectory write FDirectory;
    { @Abstract(Title of the dialog window)}
    property Title: String read FTitle write FTitle;
  end;

procedure Register;

implementation
uses
  Registry, StrUtils,
  UrlFuncs, MIMEHelper, rtcFolderView;

constructor TrtcFolderTree.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  RightClickSelect:= True;
  NotifyRegister:= Nil;
  NotifyHandle:= 0;
  FFileMask:= '*.*';
  Items.Clear;
  fFolderOptions:= [foMyComputer, foVirtualFirst];
  fDisplayMode:= dmAsis;
  self.ReadOnly:= True;
  Folder_List:= TList.Create;
  FFolderTreeInfoMemStream:= Nil;
  FWndDestroying:= False;
  FStates:= [];
  self.OnDeletion:= FreeNode;  // ?!!
  fEndSessionQuery:= Nil;
{$IFDEF DELPHI3UP}
  CoGetMalloc(MEMCTX_TASK, AllocInterface);
{$ELSE}
  SHGetMalloc(AllocInterface);
{$ENDIF}
  self.parent:= (AOwner as TWinControl);
  fMessageHandle:= AllocateHWnd(MessageProc);
  self.OnExpanding:= FolderTreeExpanding;

  // Get the system image list.
  RetrieveSysImageList;
  with SysImageList do begin
    masked:= False;
    shareimages:= True;
  end;
  Self.Images:= SysImageList;

  if Items.Count > 0
  then TopItem:= Items[0];

  FOnListExternalStorageSites:= Nil;
  FOnExternalStorageFolderList:= Nil;
  FOnExternalAsyncStorage:= Nil;
  FOnExtDeleteUrl:= Nil;
  FOnExtMoveUrl:= Nil;
  FOnExtCopyUrl:= Nil;

  FDefaultFolderImageIndex:= 0;
  FDefaultFileImageIndex:= 4;
  FDefaultHostImageIndex:= 0;

  OnCustomDrawItem:= EmbeddedOnCustomDrawItem;

  // associated list view control object
  FFilesViewAdaptor:= Nil;

  // drag and drop
  OnDragOver:= FDragOverEvent;
  OnDragDrop:= FDragDropEvent;
  DragMode:= dmAutomatic;
  HideSelection:= False;

  FOnDirectoryChanged:= Nil;
  FHistory:= THistoryStack.Create;
  // HotTrack:= True;
  // If designing, activate the component.
  if csDesigning in ComponentState then begin
    Activate;
  end;
end;

// SetDisplayMode sets fDisplayMode, then changes the case of text
// in all nodes of the tree to the case specified.
procedure TrtcFolderTree.SetDisplayMode(AValue: TDisplayMode);

  procedure SetCase(ANode: TTreeNode);
  var
    foldernode: PFolderNode;
    child: TTreeNode;
    i: Integer;
  begin
    if Assigned(ANode) then begin
      foldernode:= ANode.data;
      case fDisplayMode of
        dmAsis: ANode.Text:= foldernode.FN_Text; // ^
        dmUpperCase: ANode.Text:= Uppercase(foldernode.FN_Text); // ^
        dmLowerCase: ANode.Text:= Lowercase(foldernode.FN_Text); // ^
      end;
      for i:= 0 to ANode.Count - 1 do begin
        child:= ANode.item[i];
        SetCase(child);
      end;
    end;
  end;

begin
  fDisplayMode:= AValue;
  if Active
  then SetCase(GetDesktopNode);
end;

procedure TrtcFolderTree.SetFolderOptions(AValue: SFolderOption);
begin
  fFolderOptions:= AValue;
end;

// BuildDesktop is performed when the component is created.
procedure TrtcFolderTree.BuildDesktop;
var
  foldernode: PFolderNode;
  FilePath: TFilePathList;
  pathinfo: TPathInfo;
  Desktop: TTreeNode;
begin
// LockWindowUpdate(self.handle);
   try
     // Establish the node for the Desktop.
     pathinfo:= FGetSpecialFolder(CSIDL_DESKTOP);
     fDesktopPath:= pathinfo.Path;
     Desktop:= CreateJFSFolderNode(pathinfo.pid, Nil, True);  // Create a node for the Desktop
     foldernode:= Desktop.Data;
     FilePath:= GetLongFilePath(pathinfo.pid, foldernode.FN_ShellFolder); // ^
     with Desktop do begin
       Text:= FilePath.Normal;
       // Set FN_Path to the path name of the desktop, and indicate
       // it is the desktop.
       foldernode:= data;
       foldernode.FN_Path:= fDesktopPath; //^
       foldernode.FN_Text:= text; //^
       foldernode.FN_Type:= ftDesktop; //^
     end;
   finally
//     LockWindowUpdate(0);
   end;

  // If the MyComputer is desired, then add it to the tree.
  if foMyComputer in fFolderOptions then begin
    ShowMyComputer(True);
    FDefaultHostImageIndex:= GetMyComputerNode.ImageIndex;
  end;
  FDefaultFileImageIndex:= GetDefaultFileImageIndex(False, False);
  FDefaultFolderImageIndex:= GetDefaultFolderImageIndex(False, False);

  // Establish the nodes for the ftp sites, if so desired.
  ShowStorageSites(ftFTPList, foFTP in fFolderOptions);
  // Establish the nodes for the ldap sites, if so desired.
  ShowStorageSites(ftLDAPList, foLDAP in fFolderOptions);
  // Establish the nodes for the ldap sites, if so desired.
  ShowStorageSites(ftHTTPList, foHTTP in fFolderOptions);
  // Establish the node for the Recycle Bin, if so desired.
  ShowRecycleBin(foRecycleBin in fFolderOptions);
  // Establish the node for the NetHood if so desired.
  ShowNetHood(foNetworkNeighborhood in fFolderOptions);

  // Expand the Desktop node.
  GetDesktopNode.Expand(False);
end;

// BuildDesktop is performed when the component is created.
procedure TrtcFolderTree.RebuildDesktop;
var
  P: String;
begin
  // Delete the top node. That will take care of deleting all the others.
  if Assigned(Selected) and (Selected.Data <> Nil)
  then P:= PFolderNode(Selected.Data).FN_Path; // ^
  if Items.Count > 0
  then Items[0].Delete;
  // build desktop again
  BuildDesktop;
  if Length(P) > 0
  then Directory:= P;
end;

procedure TrtcFolderTree.ShowRecycleBin(AShow: Boolean);
var
   pathinfo: TPathInfo;
   foldernode: PFolderNode;
   FilePath: TFilePathList;
   BitBucket: TTreeNode;
begin
  // If the Recycle Bin is desired, then add it to the tree.
  if AShow then begin
    BitBucket:= GetRecyledNode;
    if Assigned(BitBucket)
    then Exit;

    pathinfo:= FGetSpecialFolder(CSIDL_BITBUCKET);
    BitBucket:= CreateJFSFolderNode(pathinfo.pid, GetDesktopNode, True);  // Create a node for the BitBucket
    foldernode:= GetDesktopNode.Data;
    FilePath:= GetLongFilePath(pathinfo.pid, foldernode.FN_ShellFolder); // ^
    with BitBucket do begin
      text:= FilePath.Normal;
      // Set FN_Path null, and indicate it is the BitBucket.
      foldernode:= data;
      foldernode.FN_Path:= ''; // ^
      foldernode.FN_Text:= text;   // ^
      foldernode.FN_Type:= ftRecycleBin; // ^
    end;
  end
  // If the Recycle Bin is being cancelled, delete it.
  else begin
    BitBucket:= GetRecyledNode;
    if assigned(BitBucket)
    then BitBucket.Delete;
  end;
end;

// CreateExtStorageNode
function TrtcFolderTree.CreateExtStorageNode(AFolderType: TFolderType;
  APathInfo: TPathInfo; AParent: TTreeNode): TTreeNode;
var
  NewNodeData: PFolderNode;
begin
  New(NewNodeData);
  // Create an absolute PidList for this node.
  {
  if Assigned(AParent) then begin
    parentpidlen:= TFolderNode(AParent.Data^).FN_PidLen;
    parentpidbuf:= TFolderNode(AParent.Data^).FN_PidList;
  end else begin
    parentpidlen:= 0;
    parentpidbuf:= Nil;
  end;
  // ParentFolder:= ParentNodeData^.FN_ShellFolder;
  }
  NewNodeData.FN_Path:= APathInfo.Path;         // ^
  NewNodeData.FN_Text:= APathInfo.Text;         // ^
  NewNodeData.FN_Has_Parent:= (AParent <> Nil); // ^
  NewNodeData.FN_Type:= AFolderType;            // ^
  NewNodeData.FN_NodeType:= ntFolder;           // ^
  NewNodeData.FN_PidList:= Nil;                 // ^
  NewNodeData.FN_PidLen:= 0;                    // ^

  if Assigned(AParent)
  then if (foVirtualFirst in FolderOptions)
    then Result:= Items.AddChildObjectFirst(aParent, APathInfo.Text, NewNodeData)
    else Result:= Items.AddChildObject(aParent, APathInfo.Text, NewNodeData)
  else Result:= Items.AddObjectFirst(Selected, APathInfo.Text, NewNodeData);
  with Result do begin
    HasChildren:= True;
    ImageIndex:= FDefaultFolderImageIndex;
    SelectedIndex:= FDefaultFolderImageIndex;
  end;
end;

{
function TrtcFolderTree.CustomDrawItem(Node: TTreeNode; State: TCustomDrawState;
  Stage: TCustomDrawStage; var PaintImages: Boolean): Boolean;
}
procedure TrtcFolderTree.EmbeddedOnCustomDrawItem(Sender: TCustomTreeView; Node: TTreeNode;
  State: TCustomDrawState; var DefaultDraw: Boolean);
var
  NodeRect: TRect;
  iconfn: String;
  hico, hicobig: HICON;
  lpiIcon: Integer;

  procedure FixCanvasFont;
  begin
    Canvas.Font.Color:= not Canvas.Font.Color;
    Canvas.Font:= TTreeView(Sender).Font;
  end;

begin
  // add icon for
  if (PFolderNode(Node.Data).FN_Type in [ftFtpNode, ftLdapNode, ftDbNode, ftHTTPNode]) and (PFolderNode(Node.Data).FN_NodeType <> ntFolder) then begin // ^
    FileExtIcon(ExtractFileExt(PFolderNode(Node.Data).FN_Text), iconfn, lpiIcon); // ^
    NodeRect:= Node.DisplayRect(True);

    // Canvas.TextOut(NodeRect.Left, NodeRect.Top, Node.Text);
    with NodeRect do begin
      Dec(Left, Bottom - Top + 4);
      Right:= Left + Bottom - Top - 4;
    end;
    if (Length(iconfn) > 0) then begin
      if (ShellAPI.ExtractIconEx(PAnsiChar(iconfn), lpiIcon, hicobig, hico, 1) > 1) and (hico <> 0)
      then Windows.DrawIconEx(Canvas.Handle, NodeRect.Left, NodeRect.Top, hico, 16, 16, 0, 0, DI_NORMAL)
      else Images.Draw(Canvas, NodeRect.Left, NodeRect.Top, Node.OverlayIndex)
    end else begin
      Images.Draw(Canvas, NodeRect.Left, NodeRect.Top, Node.OverlayIndex); // Images.Draw(Canvas, NodeRect.Left, NodeRect.Top, Node.OverlayIndex, dsTransparent, itImage);
    end;
  end;
  FixCanvasFont;
end;

function GetStorageName(AFolderType: TFolderType): String;
begin
  case AFolderType of
    // "http:" "//" host [ ":" port ] [ abs_path ]
    ftFTPNode, ftFTPList: Result:= 'FTP sites';
    ftLDAPNode, ftLDAPList: Result:= 'LDAP sites';
    ftDBNode, ftDBList: Result:= 'DB sites';
    ftHTTPNode, ftHTTPList: Result:= 'Hosts';
    else Result:= 'Sites';
  end;
end;

function TrtcFolderTree.AttachStorageSite(AFolderType: TFolderType;
  AUrl: String; AShow: Boolean): TTreeNode;
var
  p: Integer;
  desc: String;
  b: Boolean;
  StorageListNode: TTreeNode;
  foldernode: PFolderNode;

  protocol, host, user, password, IPaddress, fn, bookmark: String;
  // ldap address
  baseDN, attrs, scope, filter: String;
  port: Integer;
  pathinfo: TPathInfo;
begin
  Result:= Nil;
  // delete description if exists
  p:= Pos('=', AUrl);
  if p > 0 then begin
    desc:= Copy(AUrl, 1, p - 1);
    System.Delete(AUrl, 1, p);
  end else desc:= '';

  case AFolderType of
    // "http:" "//" host [ ":" port ] [ abs_path ]
    ftFTPNode, ftFTPList: b:= ParseFtpUrl(AUrl, protocol, user, password, host, IPaddress, fn, bookmark, port, 'ftp', 21);
    ftLDAPNode, ftLDAPList: b:= ParseLDAPUrl(AUrl, protocol, user, password, host, baseDN, attrs, scope, filter, port, 'ldap', 389);
    ftDBNode, ftDBList: b:= False;// b:= ParseDbUrl(s, protocol, user, password, host, baseDN, attrs, scope, filter, port, 'ldap', 389);
    ftHTTPNode, ftHTTPList: b:= ParseUrl(AUrl, protocol, user, password, host, ipAddress, fn, bookmark, port, 'http', 80);
    else b:= False;
  end;
  if not b then
    Exit;
  // check is parent folder exists, if not, create
  StorageListNode:= GetSpecialNode(AFolderType);
  if not Assigned(StorageListNode) then begin
    // create a parent storage node if not exists
    // get a name for parent storage node
    name:= GetStorageName(AFolderType);
    // create a storage node
    StorageListNode:= Self.Items.AddChild(GetDesktopNode, name);
    with StorageListNode do begin
      New(foldernode); //
      Data:= foldernode; // foldernode
      foldernode.FN_PidLen:= 0;          // ^
      foldernode.FN_PidList:= Nil;       // ^

      foldernode.FN_Type:= AFolderType;  // ^
      foldernode.FN_Path:= '';           // ^
      foldernode.FN_Text:= name;         // ^
      foldernode.FN_NodeType:= ntFolder; // ^
      foldernode.FN_Has_Parent:= False;  // ^
      ImageIndex:= FDefaultHostImageIndex;
      SelectedIndex:= FDefaultHostImageIndex;
    end;
  end;

  Result:= FindPath(AUrl, True);
  if AShow then begin
    if not Assigned(Result) then begin
      // add node
      with pathinfo do begin
      // if (Length(IPaddress) > 0) and (Length(host) = 0) then host:= IPaddress;
        Pid:= Nil;
        if Length(desc) = 0
        then Text:= host
        else Text:= host + ' ' + Desc;
        Path:= AUrl;

        case AFolderType of
          ftFTPNode, ftFTPList: Result:= CreateExtStorageNode(ftFTPNode, PathInfo, StorageListNode);
          ftLDAPNode, ftLDAPList: Result:= CreateExtStorageNode(ftLDAPNode, PathInfo, StorageListNode);
          ftDBNode, ftDBList: Result:= CreateExtStorageNode(ftDBNode, PathInfo, StorageListNode);
          ftHTTPNode, ftHTTPList: Result:= CreateExtStorageNode(ftHTTPNode, PathInfo, StorageListNode);
          else Result:= Nil;
        end;
        Result.SelectedIndex:= FDefaultHostImageIndex;
        Result.ImageIndex:= FDefaultHostImageIndex;
      end;
    end;
  end else begin
    if Assigned(Result) then begin
      // remove node
      Result.Free;
      Result:= Nil;
    end;
  end;
end;

{
// Find a file list item in associated list view by fully qualified file name or url
function TrtcFolderTree.FindListItem(const AUrl: String): TListItem;
var
  idx: Integer;
begin
  Result:= Nil;
  if Assigned(FFilesView) then begin
    if FFilesView is TListView then begin
      for idx:= 0 to TListView(FFilesView).Items.Count - 1 do begin
        if (AUrl = TListView(FFilesView).Items[idx].SubItems[4]) then begin
          Result:= TListView(FFilesView).Items[idx];
          Exit;
        end;
      end;
    end;
    if FFilesView is TrtcFolderView then begin
      for idx:= 0 to TrtcFolderView(FFilesView).Items.Count - 1 do begin
        if (AUrl = TrtcFolderView(FFilesView).Items[idx].SubItems[4]) then begin
          Result:= TrtcFolderView(FFilesView).Items[idx];
          Exit;
        end;
      end;
    end;
  end;
end;

// get a url of file item in associated list view
// Return '' if not item is selected of File List is not associated with
function TrtcFolderTree.FileInList(AItem: TListItem): String;
begin
  Result:= '';
  if Assigned(AItem) then begin
    if (AItem.SubItems.Count >= 5) then begin
      Result:= AItem.SubItems[4];
    end;
  end;
end;

// get a url of file item in associated list view.
// Return '' if not item is selected of File List is not associated with)
function TrtcFolderTree.FileInLV(AItem: TLVItem): String;
begin
  Result:= '';
  if Assigned(FFilesView) then begin
    if FFilesView is TListView then begin
      if (TListView(FFilesView).Items[AItem.iItem].SubItems.Count >= 5) then begin
        Result:= TListView(FFilesView).Items[AItem.iItem].SubItems[4];
      end;
    end;
    if FFilesView is TrtcFolderView then begin
      if (TListView(FFilesView).Items[AItem.iItem].SubItems.Count >= 5) then begin
        Result:= TListView(FFilesView).Items[AItem.iItem].SubItems[4];
      end;
    end;
  end;
end;

// get a url of file itme selected in associated list view
// Return '' if not item is selected of File List is not associated with
function TrtcFolderTree.SelectedFileInList: String;
begin
  Result:= '';
  if Assigned(FFilesView) then begin
    Result:= FileInList(FFilesView.Selected);
  end;
end;

// get a list of url of files selected in associated list view
// Return 0 if no items selected or no file list associated with
// AResult can be Nil
function TrtcFolderTree.SelectedFilesList(var AResult: TStrings): Integer;
var
  idx: Integer;
begin
  Result:= 0;
  AResult.BeginUpdate;
  if Assigned(AResult) then
    AResult.Clear;
  if Assigned(FFilesView) then begin
    if FFilesView is TListView then begin
      for idx:= 0 to TListView(FFilesView).Items.Count - 1 do begin
        if (TListView(FFilesView).Items[idx].Selected) and (TListView(FFilesView).Items[idx].SubItems.Count >= 5) then begin
          Inc(Result);
          if Assigned(AResult) then
            AResult.Add(TListView(FFilesView).Items[idx].SubItems[4]);
        end;
      end;
    end;
    if FFilesView is TrtcFolderView then begin
      for idx:= 0 to TrtcFolderView(FFilesView).Items.Count - 1 do begin
        if (TrtcFolderView(FFilesView).Items[idx].Selected) and (TrtcFolderView(FFilesView).Items[idx].SubItems.Count >= 5) then begin
          Inc(Result);
          if Assigned(AResult) then
            AResult.Add(TrtcFolderView(FFilesView).Items[idx].SubItems[4]);
        end;
      end;
    end;
  end;
  AResult.EndUpdate;
end;

// Delete a file by fully qualified file name or url
function TrtcFolderTree.RemoveFileByUrl(const AUrl: String): Boolean;
var
  protocol, user, password, host, IPaddress, fn, bookmark: String;
  port: Integer;
  n: TTreeNode;
  li: TListItem;
begin
  urlFuncs.ParseUrl(AUrl, protocol, user, password, host, IPaddress, fn, bookmark, port, 'http', 80);
  if (Length(host) = 0) then begin
    n:= FindPath(AUrl, True);
    Result:= SendFileToRecycleBin(AUrl);
  end else begin
    // remove remote
    if Assigned(FOnExtDeleteUrl) then
      Result:= FOnExtDeleteUrl(AUrl);
  end;
  if Result then begin
    // if AUrl is a mask, need to redraw entire listview
    if (Pos('*', AUrl) > 0) or (Pos('?', AUrl) > 0) then begin
      n:= Selected;
      if Assigned(n) then
        UpdateListView(n, True);
    end else begin
      // delete item
      if not Assigned(n) then begin
        n:= Selected;
      end;
      if Assigned(n) then begin
        // search an item in associated list view and remove if found
        li:= FindListItem(AUrl);
        if Assigned(li) then
          li.Free;
      end;
    end;
  end;
end;

// Delete a file by one selected item
function TrtcFolderTree.RemoveFileListItem(AItem: TListItem): Boolean;
var
  url, protocol, user, password, host, IPaddress, fn, bookmark: String;
  port: Integer;
begin
  Result:= False;
  if AItem.SubItems.Count < 5 then
    Exit;
  url:= AItem.SubItems[4];
  urlFuncs.ParseUrl(Url, protocol, user, password, host, IPaddress, fn, bookmark, port, 'http', 80);
  if (Length(host) = 0) then begin
    Result:= SendFileToRecycleBin(url);
    if Result then
      AItem.Free;
  end else begin
    Result:= RemoveFileByUrl(url); // item deleted in this function
  end;
  if not Result then begin
    //
    raise Exception.Create(GetLastErrorDescription(GetLastError));
  end;
end;
// Delete a file by one selected node (False) or all selected nodes (MultiSelect=True)
function TrtcFolderTree.RemoveFileListItems(AAllSelected: Boolean): Boolean;
var
  i: Integer;
//  n: TTreeNode;
  li: TListItem;
  r: Boolean;
begin
  Result:= False;
  if not Assigned(FFilesView) then
    Exit;
  if not AAllSelected then begin
    li:= FFilesView.Selected;
    if Assigned(li) then
      Result:= RemoveFileListItem(li);
  end else begin
    Result:= True;
    i:= 0;
    if FFilesView is TListView then begin
      while i < TListView(FFilesView).Items.Count do begin
        li:= TListView(FFilesView).Items[i];
        if li.Selected then begin
          r:= RemoveFileListItem(li);
          if not r then Inc(i);
          Result:= Result and r;
        end else
          Inc(i)
      end;
    end;
    if FFilesView is TrtcFolderView then begin
      while i < TrtcFolderView(FFilesView).Items.Count do begin
        li:= TrtcFolderView(FFilesView).Items[i];
        if li.Selected then begin
          r:= RemoveFileListItem(li);
          if not r then Inc(i);
          Result:= Result and r;
        end else
          Inc(i)
      end;
    end;
  end;
end;

function TrtcFolderTree.MoveFile(const ASrcUrl, ADestUrl: String): Boolean;
var
  protocol, user, password, host, IPaddress, fn, bookmark: String;
  port: Integer;
  n: TTreeNode;
//  li: TListItem;
begin
  urlFuncs.ParseUrl(ASrcUrl, protocol, user, password, host, IPaddress, fn, bookmark, port, 'http', 80);
  if (Length(host) = 0) then begin
    Result:= MoveFileToDest(ASrcUrl, ADestUrl);
  end else begin
    // remove remote
    if Assigned(FOnExtMoveUrl) then
      Result:= FOnExtMoveUrl(ASrcUrl, ADestUrl);
  end;
  if Result then begin
    n:= Selected;
    if Assigned(n) then
      UpdateListView(n, True);
  end;
end;

function TrtcFolderTree.CopyFile(const ASrcUrl, ADestUrl: String): Boolean;
var
  protocol, user, password, host, IPaddress, fn, bookmark: String;
  port: Integer;
  desthost, destfn, destbookmark: String;
  n: TTreeNode;
//  li: TListItem;
begin
  urlFuncs.ParseUrl(ASrcUrl, protocol, user, password, host, IPaddress, fn, bookmark, port, 'http', 80);
  urlFuncs.ParseUrl(ADestUrl, protocol, user, password, desthost, IPaddress, destfn, destbookmark, port, 'http', 80);
  if (Length(host) = 0) and (Length(desthost) = 0) then begin
    Result:= CopyFileToDest(ASrcUrl, ADestUrl, True);
  end else begin
    // copy remote
    if Assigned(FOnExtCopyUrl) then begin
      Result:= FOnExtCopyUrl(ASrcUrl, ADestUrl);
    end;
  end;
  if Result then begin
    n:= Selected;
    if Assigned(n) then
      UpdateListView(n, True);
  end;
end;
}
function FindChildNodeByText(AParentNode: TTreeNode; const AText: String): TTreeNode;
var
  ch: TTreeNode;
begin
  Result:= Nil;
  ch:= AParentNode.getFirstChild;
  while (ch <> nil) do begin
    if (SameText(AText, ch.Text)) then begin
      Result:= ch;
      Exit;
    end;
    ch:= AParentNode.GetNextChild(ch);
  end;
end;

// ValidateHosts check is passed hosts displayed.
// If there are hosts displayed but not in a list, FolderTree delete them.
// AFolderType must be ftHTTPNode) }
procedure TrtcFolderTree.ValidateHosts(AFolderType: TFolderType; AHosts: TStrings);
var
  p: String;
  h: Integer;
  ch, n: TTreeNode;
  SiteFolderType: TFolderType;
begin
  //
  case AFolderType of
    ftLDAPNode: begin
      p:= 'ldap://';
      SiteFolderType:= ftLDAPList;
      n:= GetSpecialNode(ftLDAPList)
    end;
    ftFTPNode: begin
      p:= 'ftp://';
      SiteFolderType:= ftFTPList;
      n:= GetSpecialNode(ftFTPList)
    end;
    ftHTTPNode: begin
      p:= 'http://';
      SiteFolderType:= ftHTTPList;
      n:= GetSpecialNode(ftHTTPList)
    end
  else
    Exit;
  end;

  if not Assigned(n) then
    Exit; // not in a list

  // remove hosts not in list
  ch:= n.getFirstChild;
  while (ch <> nil) do begin
    if (AHosts.IndexOf(ch.Text) < 0) then begin
      // remove host from the list
      ch.Free;
    end;
    ch:= n.GetNextChild(ch);
  end;
  // add hosts in list (not exists already)
  for h:= 0 to AHosts.Count - 1 do begin
    if not Assigned(FindChildNodeByText(n, AHosts[h])) then begin
      AttachStorageSite(SiteFolderType, p + AHosts[h], True)
    end;
  end;
  n:= Selected;
  if Assigned(FFilesViewAdaptor) and Assigned(n) then
    FFilesViewAdaptor.Dir[False]:= PFolderNode(n.Data).FN_Path; // FFilesViewAdaptor.UpdateListView(Selected, False);
end;

{ Return a parent directory }
function GetParentDirectoryPath(const AStr: String): String;
var
  s: String;
  p: Integer;
begin
  s:= AStr;
  p:= SysUtils.LastDelimiter('\/', s);
  if p > 0 then begin
    System.Delete(s, p + 1, MaxInt);
  end;
  if Length(s) <> 0 then
    Result:= s
  else
    Result:= AStr;
end;

{ Go to the parent directory. Return a new directory }
function TrtcFolderTree.GoParent(): String;
var
  s: String;
begin
  s:= GetParentDirectoryPath(Directory);
  if (s <> Directory) then begin
    Directory:= s;
    if Assigned(FOnDirectoryChanged) then
      FOnDirectoryChanged(Self);
  end;
end;

{ Go back. Return a new directory }
function TrtcFolderTree.GoBack(): String;
var
  s: String;
  n: PChar;
begin
  if FHistory.Count > 0 then begin
    n:= FHistory.Pop;
    if (n <> Directory) then begin
      Directory:= n;
      if Assigned(FOnDirectoryChanged) then
        FOnDirectoryChanged(Self);
    end;
    FreeMem(n);
  end;
  Result:= Directory;
end;

{ Go forward. Return a new directory }
function TrtcFolderTree.GoForward(): String;
begin
  Result:= Directory;
end;

// Establish the nodes for the ftp sites, if so desired.
procedure TrtcFolderTree.ShowStorageSites(AFolderType: TFolderType; AShow: Boolean);
var
  foldernode: PFolderNode;
  StorageNode, StorageListNode: TTreeNode;
  SiteList: TStrings;
  i: Integer;
  name: String;
begin
  // If the ftp|ldap|http sites is desired, then add them to the tree.
  if AShow then begin
    StorageNode:= GetSpecialNode(AFolderType);
    if Assigned(StorageNode)
    then Exit;

    name:= GetStorageName(AFolderType);
    // if not Assigned(FOnListExternalStorageSites) then Exit;
    // Create a node for the StorageList even FOnListExternalStorageSites is not assigned
    StorageListNode:= Self.Items.AddChild(GetDesktopNode, name);
    with StorageListNode do begin
      New(foldernode);
      Data:= foldernode;
      foldernode.FN_PidLen:= 0;          // ^
      foldernode.FN_PidList:= Nil;       // ^

      foldernode.FN_Type:= AFolderType;  // ^
      foldernode.FN_Path:= '';           // ^
      foldernode.FN_Text:= name;         // ^
      foldernode.FN_NodeType:= ntFolder; // ^
      foldernode.FN_Has_Parent:= False;  // ^
      ImageIndex:= FDefaultFolderImageIndex;
      SelectedIndex:= FDefaultFolderImageIndex;
    end;
    // SetNodeState(StorageListNode, TVIS_BOLD); // bold reflects on hint only.

    //StorageListNode:= GetDesktopNode;
    if Assigned(FOnListExternalStorageSites) then begin
      SiteList:= TStringList.Create;
      FOnListExternalStorageSites(AFolderType, SiteList);

      for i:= 0 to SiteList.Count - 1 do begin
        AttachStorageSite(AFolderType, SiteList[i], AShow);
      end;
      SiteList.Free;
    end;
  end else begin
    // if storage nodes cancelled
    StorageNode:= GetSpecialNode(AFolderType);
    if Assigned(StorageNode)
    then StorageNode.Delete;
  end;
end;

procedure TrtcFolderTree.ShowNetHood(AShow: Boolean);
var
  pathinfo: TPathInfo;
  foldernode: PFolderNode;
  NetHood: TTreeNode;
begin
  if AShow then begin
    NetHood:= GetNetHoodNode;
    if Assigned(NetHood)
    then Exit;
    pathinfo:= FGetNetHood;
    fNetHoodPath:= pathinfo.path;
    NetHood:= CreateJFSFolderNode(pathinfo.pid, GetDesktopNode, True);  // Create a node for the NetHood
    with NetHood do begin
      Text:= pathinfo.text;
      // Set FN_Path to \\, which is the root of Network
      // Neighborhood, and set FN_Type to shows it is the NetHood.
      foldernode:= data;
      foldernode.FN_PidLen:= 0;      // ^ 2003-04-14
      foldernode.FN_PidList:= Nil;   // ^ 2003-04-14
      foldernode.FN_Path:= '\\';     // ^
      foldernode.FN_Text:= text;     // ^
      foldernode.FN_Type:= ftNetworkNeighborhood; // ^
    end;
  end else begin
    // If the NetHood is being cancelled, delete it.
    NetHood:= GetNetHoodNode;
    if Assigned(NetHood)
    then NetHood.Delete;
  end;
end;

procedure TrtcFolderTree.ShowMyComputer(AShow: Boolean);
var
  foldernode: PFolderNode;
  FilePath: TFilePathList;
  pathinfo: TPathInfo;
  MyComputer: TTreeNode;
begin
  if AShow then begin
    MyComputer:= GetMyComputerNode;
    if Assigned(MyComputer)
    then Exit;
    try
      // Attach the MyComputer node to the Desktop node.
      pathinfo:= FGetSpecialFolder(CSIDL_DRIVES);
      MyComputer:= CreateJFSFolderNode(pathinfo.pid, GetDesktopNode, True);  // Create a node for the Desktop
      foldernode:= GetDesktopNode.Data;
      FilePath:= GetLongFilePath(pathinfo.pid, foldernode.FN_ShellFolder); // ^
      with MyComputer do begin
        Text:= FilePath.Normal;
        // Set FN_Path to a null String, and the type to ftMyComputer.
        foldernode:= data;
        foldernode.FN_Path:= '';   // ^
        foldernode.FN_Text:= text; // ^
        foldernode.FN_Type:= ftMyComputer; // ^
      end;
      // Expand the My Computer node.
      GetMyComputerNode.Expand(False);
    finally
    end;
  end else begin
    // If the MyComputer is being cancelled, delete it.
    MyComputer:= GetMyComputerNode;
    if Assigned(MyComputer)
    then MyComputer.Delete;
  end;
end;

procedure TrtcFolderTree.SetFileMask(AValue: String);
begin
  FFileMask:= AValue;
  if Active then begin
    RebuildDesktop;
  end;
end;

// return -1 if filename is not in mask
function TrtcFolderTree.FileIndexOfMask(const AFileName: String): Integer;
var
  i, p0, p1, len: Integer;
  fn, Mask: String[255];
begin
  Result:= -1;
  // get list of mask. Masks are separated by semicolon ";"
  i:= 0;
  p0:= 1;
  len:= Length(FFileMask) + 1;
  repeat
    p1:= PosEx(';', FFileMask, p0);
    if p1 <= 0 then p1:= len;
    fn:= Copy(FFileMask, p0, p1 - p0);
    p0:= p1 + 1;
    if Length(fn) = 0
    then Break;
    Mask:= ExtractFileExt(fn);
    if Length(Mask) = 0
    then Break;
    // compare extension
    if (Mask = '.*') or
      (ANSICompareText(Mask, ExtractFileExt(AFileName)) = 0) then begin
      Result:= i;
      Break;
    end;
    // ready to next
    Inc(i);
  until False;
end;

function  TrtcFolderTree.FGetNetHood: TPathInfo;
var
  FilePath: TFilePathList;
begin
  Result:= FGetSpecialFolder(CSIDL_NETWORK);
  FilePath:= GetLongFilePath(Result.pid, Nil);
  Result.Text:= FilePath.Normal;
end;

function TrtcFolderTree.FGetSpecialFolder(nFolder: Integer): TPathInfo;
var
  aPidl: PItemIDList;
  fLinkDir: String;
  FileInfo: ShellAPI.TSHFileInfo;
begin
  // Get the folder location (as a PItemIDList)
  if SUCCEEDED(SHGetSpecialFolderLocation(self.parent.handle, nFolder, aPidl)) then begin
    {    hIcon: HICON;  iIcon: Integer;  dwAttributes: DWORD; szDisplayName: array [0..MAX_PATH-1] of  AnsiChar;
         szTypeName: array [0..79] of AnsiChar;
    }
    Result.Pid:= aPidl;
    // Get the actual path of the directory from the PItemIDList
    SetLength(fLinkDir, MAX_PATH); // SHGetPathFromIDList assumes MAX_PATH buffer
    SHGetPathFromIDList(aPidl, PChar(fLinkDir)); // Do it
    SetLength(fLinkDir, StrLen(PChar(fLinkDir)));
    Result.Path:= fLinkDir;

    SHGetFileInfo(Pointer(aPidl), SFGAO_SHARE, FileInfo, Sizeof(FileInfo),
      SHGFI_PIDL or SHGFI_SYSICONINDEX);
    Result.IconIndex:= FileInfo.iIcon;
    DestroyIcon(FileInfo.hIcon);

    SHGetFileInfo(Pointer(aPidl), SFGAO_SHARE, FileInfo, sizeof(FileInfo),
      SHGFI_PIDL or SHGFI_SYSICONINDEX or SHGFI_OPENICON);
    DestroyIcon(FileInfo.hIcon);
    Result.SelectedIndex:= FileInfo.iIcon;
  end;
end;

// CreateJFSFolderNode creates a Folder_Node and inserts it under
// the "parent" node (if any), using the last of the path
// String as the Name, and setting the new node's text
// property to match.
function TrtcFolderTree.CreateJFSFolderNode(APidList: PItemIdList; AParent: TTreeNode;
  AVirtualOk: Boolean): TTreeNode;
var
   filename,
   path: String;
   NewNodeData,
   ParentNodeData,
   DesktopNodeData: PFolderNode;
   newnode: TTreeNode;
   FileInfo: TSHFileInfo;
   ImageIndex,
   SelectedIndex,
   Attributes: UINT;
   pidl: PItemIdList;
   parentfolder,
   ShellFolder: IShellFolder;
   FilePath: TFilePathList;
   virtualfolder: Boolean;
   pidlen,
   parentpidlen: Word;
   parentpidbuf,
   pidbuf: PItemIdList;
   itemid: TSHItemID;
   ext: String;
begin
  Result:= Nil;
  try
    // Get the Long File Path for the PidList passed.
    if Assigned(AParent) then begin
      ParentNodeData:= AParent.Data;
      FilePath:= GetLongFilePath(APidList, ParentNodeData.FN_ShellFolder); // ^
    end else FilePath:= GetLongFilePath(APidList, Nil);
    filename:= FilePath.InFolder;
    // skip zip files (it is folder, so do not get attribute - it is slow in Windows XP
    ext:= ExtractFileExt(FilePath.ForParsing);
    if CompareText(ext, '.zip') = 0
    then Exit;

    path:= FilePath.ForParsing;
    // Get the attributes we're interested in for this folder.
    Attributes:= SFGAO_FOLDER or SFGAO_SHARE or SFGAO_FILESYSTEM
      or SFGAO_LINK or SFGAO_HASSUBFOLDER;
    if Assigned(AParent) then begin
      ParentNodeData:= AParent.Data;
      ParentFolder:= ParentNodeData.FN_ShellFolder; // ^
      if (ParentFolder.GetAttributesOf(1, APidList, Attributes) <> NOERROR)
      then Attributes:= 0;
    end else begin
      ParentNodeData:= Nil;
      Attributes:= 0;
    end;

    VirtualFolder:= (Attributes and SFGAO_FILESYSTEM = 0);
    if Assigned(ParentNodeData)
    then if ParentNodeData.FN_Type in [ftNetworkNeighborhood, ftNetNode] // ^
      then virtualfolder:= False;

    if (not AVirtualOk) and (VirtualFolder)
    then Exit;

    // Dangerous to return Nil!
     if (not VirtualFolder) and (Attributes and SFGAO_FOLDER = 0) then begin
      if (Attributes and SFGAO_LINK <> 0)
      then Exit;
      if FileIndexOfMask(ext) < 0
      then Exit;
    end;

    // Create an absolute PidList for this node.
    if Assigned(AParent) then begin
      parentpidlen:= ParentNodeData.FN_PidLen; // ^
      parentpidbuf:= ParentNodeData.FN_PidList; // ^
    end else begin
      parentpidlen:= 0;
      parentpidbuf:= Nil;
    end;
    // Allocate a buffer large enough for the parent's Pid plus
    // the new one, plus the trailing nulls. Then concatenate the two.
    ItemId:= APidList.mkid; // ^
    pidlen:= ItemId.cb;
    GetMem(pidbuf, pidlen + parentpidlen + 2);
    // Copy the parent's pidlist to the buffer, followed by
    // the new pidlist.
    if parentpidlen <> 0 then begin
      System.Move(parentpidbuf^, pidbuf^, parentpidlen); // ^
      System.Move(APidList^, pidbuf.mkId.abID[parentpidlen - 2], pidlen); // ^
    end else System.Move(APidList^, pidbuf^, pidlen); // ^
    pidbuf.mkId.abID[parentpidlen + pidlen - 2]:= 0; // ^
    pidbuf.mkId.abID[parentpidlen + pidlen - 1]:= 0; // ^
    // Establish a pidlist to use when acquiring the icons. If this is a
    // virtual folder, use the PidList passed to us. Otherwise, get one
    // by parsing the complete path name using the Desktop IShellFolder.
    if virtualfolder
    then pidl:= APidList
    else pidl:= PidBuf;

    // get sys icon
    SHGetFileInfo(Pointer(PidL), SFGAO_SHARE, FileInfo, sizeof(FileInfo),
      SHGFI_PIDL or SHGFI_SYSICONINDEX
      //or SHGFI_ATTRIBUTES or SFGAO_COMPRESSED // to get FileInfo.dwAttributes. Disk A: and other removables rolls..
    );
    DestroyIcon(FileInfo.hIcon);
    ImageIndex:= FileInfo.iIcon;
    // get selected icon
    if (FileInfo.dwAttributes or SFGAO_FOLDER) = FileInfo.dwAttributes
    then SHGetFileInfo(Pointer(PidL), SFGAO_SHARE, FileInfo, sizeof(FileInfo),
      SHGFI_PIDL or SHGFI_SYSICONINDEX or SHGFI_OPENICON);
    DestroyIcon(FileInfo.hIcon);
    SelectedIndex:= FileInfo.iIcon;
    New(NewNodeData);
    NewNodeData.FN_PidLen:= parentpidlen + pidlen; // ^
    NewNodeData.FN_PidList:= pidbuf;               // ^
    NewNodeData.FN_Path:= path;                    // ^
    NewNodeData.FN_Text:= filename;                // ^
    NewNodeData.FN_Has_Parent:= (aParent <> Nil);  // ^
    //if (FileInfo.dwAttributes and SFGAO_FOLDER) > 0
    if (Attributes and SFGAO_FOLDER) > 0
    then NewNodeData.FN_NodeType:= ntFolder   // ^ The calling routine may change this.
    else NewNodeData.FN_NodeType:= ntFile;    // ^

    // If this node has no parent, it must be the Desktop.
    if Assigned(AParent) then begin
      ParentNodeData:= aParent.data;
      parentfolder:= ParentNodeData.FN_ShellFolder; // ^
      ParentFolder.BindToObject(APidList, Nil, IID_ISHELLFOLDER, Pointer(NewNodeData.FN_ShellFolder)); // ^
      case ParentNodeData.FN_Type of // ^
        ftNetworkNeighborhood: begin
          NewNodeData.FN_Type:= ftNetNode;       // ^
          NewNodeData.FN_Path:= '\\' + filename; // ^
          NewNodeData.FN_Text:= filename;        // ^
        end;
        ftRecycleBin, ftMyComputer, ftDesktop:begin
          NewNodeData.FN_Type:= ftLocalNode; // ^
        end;
        ftFTPList: begin
          NewNodeData.FN_Type:= ftFtpNode;        // ^
          NewNodeData.FN_Path:= 'ftp://' + filename;   // ^
          NewNodeData.FN_Text:= filename; // ^
        end;
        ftLDAPList: begin
          NewNodeData.FN_Type:= ftLdapNode; // ^
          NewNodeData.FN_Path:= 'ldap://' + filename;// ^
          NewNodeData.FN_Text:= filename;// ^
        end;
        ftDbList: begin
          NewNodeData.FN_Type:= ftDbNode;  // ^
          NewNodeData.FN_Path:= 'db://' + filename;// ^
          NewNodeData.FN_Text:= filename;// ^
        end;
        ftHTTPList: begin
          NewNodeData.FN_Type:= ftHttpNode;// ^
          NewNodeData.FN_Path:= 'http://' + filename;// ^
          NewNodeData.FN_Text:= filename;// ^
        end;
        ftLocalNode, ftNetNode, ftFtpNode, ftLdapNode: begin
          NewNodeData.FN_Type:= ParentNodeData.FN_Type;    // ^
        end;
        else begin
          // ftUnknown
        end;
      end;

      // Get the IShellFolder interface for the desktop,
      // so we can get folder attributes.
      DesktopNodeData:= GetDesktopNode.data;
      ShellFolder:= DesktopNodeData.FN_ShellFolder;// ^
    end else begin
      SHGetDesktopFolder(NewNodeData.FN_ShellFolder);// ^
    end;
    ShellFolder:= NewNodeData.FN_ShellFolder; // ^
    if Assigned(aParent)
    then if virtualfolder and (foVirtualFirst in FolderOptions)
      then NewNode:= self.Items.AddChildObjectFirst
        (aParent,FilePath.Normal,NewNodeData)
      else NewNode:= self.Items.AddChildObject(aParent,FilePath.Normal,NewNodeData)
    else NewNode:= self.Items.AddObjectFirst(self.selected,FilePath.Normal,NewNodeData);
    Folder_List.Add(NewNode.data);
    Result:= NewNode;
    NewNode.ImageIndex:= ImageIndex;
    NewNode.SelectedIndex:= SelectedIndex;
    if (Attributes or SFGAO_SHARE) = Attributes
      then NewNode.OverlayIndex:= 0
      else if (Attributes or SFGAO_LINK) = Attributes
        then NewNode.OverlayIndex:= 1
        else NewNode.OverlayIndex:= -1;
    if foFiles in FFolderOptions then
      NewNode.HasChildren:= ((Attributes or SFGAO_FOLDER) = Attributes)
    else
      NewNode.HasChildren:= ((Attributes or SFGAO_HASSUBFOLDER) = Attributes);
    case fDisplayMode of
      dmAsis: NewNode.text:= filename;
      dmLowercase: NewNode.text:= lowercase(filename);
      dmUppercase: NewNode.text:= uppercase(filename);
    end;
  finally
  end;
end;

// CreateJFSFolderListItem creates a Folder_Node and inserts it under
// the "parent" node (if any), using the last of the path
// String as the Name, and setting the new node's text
// property to match.
procedure TrtcFolderTree.CreateJFSFolderListItem(APidList: PItemIdList;
  AParent: TTreeNode; AVirtualOk: Boolean; var NewNodeData: TFolderNode; var FileInfo: TSHFileInfo;
  var Attributes: Cardinal);
var
   filename,
   path: String;
   ParentNodeData,
   DesktopNodeData: PFolderNode;
   pidl: PItemIdList;
   parentfolder,
   ShellFolder: IShellFolder;
   FilePath: TFilePathList;
   virtualfolder: Boolean;
   pidlen,
   parentpidlen: Word;
   parentpidbuf,
   pidbuf: PItemIdList;
   itemid: TSHItemID;
   ext: String;
begin
  try
    // Get the Long File Path for the PidList passed.
    if Assigned(AParent) then begin
      ParentNodeData:= AParent.Data;
      FilePath:= GetLongFilePath(APidList, ParentNodeData.FN_ShellFolder); // ^
    end else FilePath:= GetLongFilePath(APidList, Nil);
    filename:= FilePath.InFolder;
    {
    // skip zip files if you want
    ext:= ExtractFileExt(FilePath.ForParsing);
    if CompareText(ext, '.zip') = 0
    then Exit;
    }
    path:= FilePath.ForParsing;
    // Get the attributes we're interested in for this folder.
    Attributes:= SFGAO_FOLDER or SFGAO_SHARE or SFGAO_FILESYSTEM
      or SFGAO_LINK or SFGAO_HASSUBFOLDER;
    if Assigned(AParent) then begin
      ParentNodeData:= AParent.Data;
      ParentFolder:= ParentNodeData.FN_ShellFolder; // ^
      if (ParentFolder.GetAttributesOf(1, APidList, Attributes) <> NOERROR)
      then Attributes:= 0;
    end else begin
      ParentNodeData:= Nil;
      Attributes:= 0;
    end;

    VirtualFolder:= (Attributes and SFGAO_FILESYSTEM = 0);
    if Assigned(ParentNodeData)
    then if ParentNodeData.FN_Type in [ftNetworkNeighborhood, ftNetNode] // ^
      then virtualfolder:= False;

    if (not AVirtualOk) and (VirtualFolder)
    then Exit;

    // Dangerous to return Nil!
     if (not VirtualFolder) and (Attributes and SFGAO_FOLDER = 0) then begin
      {
      if (Attributes and SFGAO_LINK <> 0)  Nov 2007
      then Exit;
      }
      if FileIndexOfMask(ext) < 0
      then Exit;
    end;

    // Create an absolute PidList for this node.
    if Assigned(AParent) then begin
      parentpidlen:= ParentNodeData.FN_PidLen; // ^
      parentpidbuf:= ParentNodeData.FN_PidList; // ^
    end else begin
      parentpidlen:= 0;
      parentpidbuf:= Nil;
    end;
    // Allocate a buffer large enough for the parent's Pid plus
    // the new one, plus the trailing nulls. Then concatenate the two.
    ItemId:= APidList.mkid; // ^
    pidlen:= ItemId.cb;
    GetMem(pidbuf, pidlen + parentpidlen + 2);
    // Copy the parent's pidlist to the buffer, followed by
    // the new pidlist.
    if parentpidlen <> 0 then begin
      System.Move(parentpidbuf^, pidbuf^, parentpidlen); // ^
      System.Move(APidList^, pidbuf.mkId.abID[parentpidlen - 2], pidlen); // ^
    end else System.Move(APidList^, pidbuf^, pidlen); // ^
    pidbuf.mkId.abID[parentpidlen + pidlen - 2]:= 0; // ^
    pidbuf.mkId.abID[parentpidlen + pidlen - 1]:= 0; // ^
    // Establish a pidlist to use when acquiring the icons. If this is a
    // virtual folder, use the PidList passed to us. Otherwise, get one
    // by parsing the complete path name using the Desktop IShellFolder.
    if virtualfolder
    then pidl:= APidList
    else pidl:= PidBuf;

    // get sys icon and type name and file attributes
    SHGetFileInfo(Pointer(PidL), SFGAO_SHARE, FileInfo, sizeof(FileInfo),
      SHGFI_PIDL or SHGFI_SYSICONINDEX or SHGFI_OPENICON or SHGFI_TYPENAME or SHGFI_USEFILEATTRIBUTES
      //or SHGFI_ATTRIBUTES or SFGAO_COMPRESSED // to get FileInfo.dwAttributes. Disk A: and other removables rolls..
    );
    DestroyIcon(FileInfo.hIcon);

    NewNodeData.FN_PidLen:= parentpidlen + pidlen; // ^
    NewNodeData.FN_PidList:= pidbuf;               // ^
    NewNodeData.FN_Path:= path;                    // ^
    NewNodeData.FN_Text:= filename;                // ^
    NewNodeData.FN_Has_Parent:= (aParent <> Nil);  // ^
    //if (FileInfo.dwAttributes and SFGAO_FOLDER) > 0
    if (Attributes and SFGAO_FOLDER) > 0
    then NewNodeData.FN_NodeType:= ntFolder   // ^ The calling routine may change this.
    else NewNodeData.FN_NodeType:= ntFile;    // ^

    // If this node has no parent, it must be the Desktop.
    if Assigned(AParent) then begin
      ParentNodeData:= aParent.data;
      parentfolder:= ParentNodeData.FN_ShellFolder; // ^
      ParentFolder.BindToObject(APidList, Nil, IID_ISHELLFOLDER, Pointer(NewNodeData.FN_ShellFolder)); // ^
      case ParentNodeData.FN_Type of // ^
        ftNetworkNeighborhood: begin
          NewNodeData.FN_Type:= ftNetNode;       // ^
          NewNodeData.FN_Path:= '\\' + filename; // ^
          NewNodeData.FN_Text:= filename;        // ^
        end;
        ftRecycleBin, ftMyComputer, ftDesktop:begin
          NewNodeData.FN_Type:= ftLocalNode; // ^
        end;
        ftFTPList: begin
          NewNodeData.FN_Type:= ftFtpNode;        // ^
          NewNodeData.FN_Path:= 'ftp://' + filename;   // ^
          NewNodeData.FN_Text:= filename; // ^
        end;
        ftLDAPList: begin
          NewNodeData.FN_Type:= ftLdapNode; // ^
          NewNodeData.FN_Path:= 'ldap://' + filename;// ^
          NewNodeData.FN_Text:= filename;// ^
        end;
        ftDbList: begin
          NewNodeData.FN_Type:= ftDbNode;  // ^
          NewNodeData.FN_Path:= 'db://' + filename;// ^
          NewNodeData.FN_Text:= filename;// ^
        end;
        ftHTTPList: begin
          NewNodeData.FN_Type:= ftHttpNode;// ^
          NewNodeData.FN_Path:= 'http://' + filename;// ^
          NewNodeData.FN_Text:= filename;// ^
        end;
        ftLocalNode, ftNetNode, ftFtpNode, ftLdapNode: begin
          NewNodeData.FN_Type:= ParentNodeData.FN_Type;    // ^
        end;
        else begin
          // ftUnknown
        end;
      end;

      // Get the IShellFolder interface for the desktop,
      // so we can get folder attributes.
      DesktopNodeData:= GetDesktopNode.data;
      ShellFolder:= DesktopNodeData.FN_ShellFolder;// ^
    end else begin
      SHGetDesktopFolder(NewNodeData.FN_ShellFolder);// ^
    end;
    ShellFolder:= NewNodeData.FN_ShellFolder; // ^
  finally
  end;
end;

procedure TrtcFolderTree.RetrieveSysImageList;
var
  FileInfo: TSHFileInfo;
  DesktopInfo: TPathInfo;
begin
  DesktopInfo:= FGetSpecialFolder(CSIDL_DESKTOPDIRECTORY);
  SysImageList:= TImageList.Create(Self);
  SysImageList.Shareimages:= True;
  SysImageList.Handle:= SHGetFileInfo(Pointer(DesktopInfo.Pid), 0, FileInfo, sizeof(FileInfo),
    SHGFI_PIDL or SHGFI_SYSICONINDEX or SHGFI_SMALLICON);
  DestroyIcon(FileInfo.hIcon);     
end;

function TrtcFolderTree.GetLongFilePath(const Pid: PItemIdList;
  ShellFolder: IShellFolder): TFilePathList;
var
  sf: IShellfolder;
begin
  sf:= ShellFolder;
  if not(assigned(ShellFolder))
  then SHGetDesktopFolder(sf);

  Result.Normal:= GetPathName(Pid,SHGDN_NORMAL, sf);
  Result.ForParsing:= GetPathName(Pid,SHGDN_FORPARSING, sf);
  Result.InFolder:= GetPathName(Pid,SHGDN_INFOLDER, sf);
{$IFNDEF DELPHI3UP}
  if not(assigned(ShellFolder))
  then sf.Release;
{$ENDIF}
end;

function TrtcFolderTree.GetPathName(const Pid: PItemIdList; Flags: dword;
  ShellFolder: IShellFolder): String;
var
  StrReturn: TStrRet;
label
  Again;
begin
  Result:='';
Again:
  FillChar(StrReturn,SizeOf(StrReturn), #0);
  ShellFolder.GetDisplayNameOf(Pid,Flags, StrReturn);
  case StrReturn.uType of
    STRRET_WSTR: begin
      if Assigned(StrReturn.pOleStr)
      then Result:=WideCharToString(StrReturn.pOleStr)
      else if Flags=SHGDN_NORMAL then begin
        Flags:=SHGDN_FORPARSING;
        goto Again;
      end;
    end;
    STRRET_OFFSET: Result:=StrPas(PChar(Pid) + StrReturn.uOffset);
    STRRET_CSTR: Result:= Strpas(StrReturn.cStr);
  end;
end;

// GetDirectory is the read method for the Directory property.
// It calls GetPath using the currently selected node to build
// the current path.
function TrtcFolderTree.GetDirectory: String;
begin
  if csDesigning in ComponentState
  then Exit;
  Result:= GetPath(Selected);
end;

procedure TrtcFolderTree.SetDirectory(const AValue: String);
var
  node: TTreeNode;
  n: PChar;
begin
  if csDesigning in ComponentState
  then Exit;
  if Active then begin
    node:= GoToDirectory(AValue, True);
    if Assigned(node) then begin
      if PFolderNode(node.Data).FN_NodeType = ntFolder // ^
        then node.Expand(False);
      node.Selected:= True;
      node.Focused:= True;
    end;
    GetMem(n, Length(AValue) + 1);
    StrCopy(n, PChar(AValue));
    if (FHistory.Count = 0) or (PChar(FHistory.Peek) <> AValue) then
      FHistory.Push(n);
  end;
end;

// NodeShellFolder returns the IShellFolder interface at the indicated node.
function  TrtcFolderTree.NodeShellFolder(Node: TTreeNode): IShellFolder;
var
  foldernode: PFolderNode;
begin
  Result:= Nil;
  if csDesigning in ComponentState
  then Exit;
  if assigned(Node) then begin
    foldernode:= Node.data;
    Result:= foldernode.FN_ShellFolder; // ^
  end;
end;

// FolderType returns the folder type of the desired node.
function  TrtcFolderTree.FolderType (Node: TTreeNode): TFolderType;
begin
  Result:= ftUnknown;
  if csDesigning in ComponentState then Exit;
  if Assigned(Node) then begin
    Result:= PFolderNode(Node.Data).FN_Type; // ^
  end;
end;

// NodeType returns the folder type of the desired node.
function  TrtcFolderTree.NodeType (Node: TTreeNode): TNodeType;
begin
  Result:= ntFile;
  if Assigned(Node) then begin
    Result:= PFolderNode(Node.Data).FN_NodeType; // ^
  end;
end;

// NodeItemIdList returns the absolute PidList of the selected node.
function  TrtcFolderTree.NodeItemIdList(Node: TTreeNode): PItemIdList;
var
  foldernode: PFolderNode;
begin
  Result:= Nil;
  if csDesigning in ComponentState then Exit;
  if Assigned(Node) then begin
    foldernode:= Node.data;
    Result:= foldernode.FN_PidList; // ^
  end;
end;

// SetDirectory calls FindPath to get the node closest to the path
// passed to it, then selects the Node in the tree and returns it.
function TrtcFolderTree.GoToDirectory(const ADir: String; ASelect: Boolean): TTreeNode;
var
  Node: TTreeNode;
  foldernode: PFolderNode;
  inpath: String;
begin
  if Length(ADir) > 0
  then Node:= FindPath(ADir, False)
  else Node:= Nil;
  if ASelect then
    Self.Selected:= Node;
  Result:= Node;
  // Verify the path still exists. If not, perform the DblClick
  // routine to collapse the node, and return Nil.
  if Assigned(Node) then begin
    foldernode:= Node.data;
    inpath:= foldernode.FN_Path; // ^
    if (not (fsUpdatingListView in FStates)) then
      if Assigned(FFilesViewAdaptor) then
        FFilesViewAdaptor.Dir[False]:= ADir; // FFilesViewAdaptor.UpdateListView(node, False);
  end else inpath:= '';
  if length(inpath) > 3 then begin
    if FileGetAttr(inpath) = -1 then begin
      CheckPath;
      Result:= Nil;
    end;
  end;
end;

{ @Abstract(Return protocol used for transfer URL. Extact host name and DOS/Windows file name }
class function TrtcFolderTree.ExtractExtProtocol(const AUrl: String; var AHost, AFileName: String): String;
var
  user, password, IPaddress, bookmark: String;
  port: Integer;
begin
  urlFuncs.ParseUrl(AUrl, Result, user, password, AHost, IPaddress, AFileName, bookmark, port, 'http', 80);
  AFileName:= Strutils.AnsiReplaceStr(AFileName, '/', '\');
end;

// When the user tries to expand a node with no children, add its
// subfolders as children.
procedure TrtcFolderTree.FolderTreeExpanding(Sender: TObject; Node: TTreeNode;
  var AllowExpansion: Boolean);
begin
  if (csDesigning in ComponentState) then begin
    if Node.level > 1 then begin
      AllowExpansion:= False;
      Exit;
    end;
  end;
  // If Node already has children, we don't have to find them.
  try
    if Node.Count > 0 then begin
      AllowExpansion:= True;
      Exit;
    end;
//  foldernode:= Node.data;
//    LockWindowUpdate(self.handle);
    AllowExpansion:= AttachFolders(Node);
//    LockWindowUpdate(0);
  except
    AllowExpansion:= False;
  end;
end;

// AttachFolders unconditionally attaches child folders to the node passed in Node.
function TrtcFolderTree.AttachFolders(var Node: TTreeNode): Boolean;
var
  children: Boolean;
begin;
  // Screen.cursor:= crHourGlass;
  // LockWindowUpdate(self.handle);
  Result:= False;
  if not Assigned(Node) then
    Exit;
  case PFolderNode(Node.data).FN_Type of // ^
    ftFtpNode, ftDbNode, ftLdapNode, ftHTTPNode: begin
      children:= AttachStorageFolders(PFolderNode(Node.data).FN_Type, Node); // ^
    end;
    else begin
      children:= AttachJFSFolders(Node);
    end;
  end;
  Node.HasChildren:= children;
  // Alphabetize the nodes, unless the parent is MyComputer or
  // Network Neighborhood.
  if not (PFolderNode(Node.Data).FN_Type in [ftMyComputer, ftNetworkNeighborhood]) // ^
  then if children
    then Node.CustomSort(@CustomSortProc, 0);
  Result:= children;
  //  LockWindowUpdate(0);
  //  Screen.Cursor:= crDefault;
  if Assigned(FFilesViewAdaptor) then
    FFilesViewAdaptor.Dir[False]:= PFolderNode(node.Data).FN_Path; // FFilesViewAdaptor.UpdateListView(node, False);
end;

// return has children or not
function  TrtcFolderTree.AttachJFSFolders(var Node: TTreeNode): Boolean;
var
  ParentNodeData: PFolderNode;
  newnode: TTreeNode;
  path: String;
  EnumList: IEnumIDList;
  ParentFolder: IShellFolder;
  ItemIdList: PItemIdList;
  DummyResult: ULONG;
  Flags: Integer;
begin;
  Result:= False;
  path:= GetPath(Node);    // Get the complete path for this node.
  ParentNodeData:= Node.data;
  ParentFolder:= ParentNodeData.FN_ShellFolder; // ^
  if not Assigned(ParentFolder)
  then Exit;

  // If the node being expanded is a root, lop off the backslash.
  if length(path) <= 3
  then system.Delete(path,length(path),1);

  // Enumerate the folder's contents.
  Flags:= SHCONTF_FOLDERS + SHCONTF_INCLUDEHIDDEN;
  if foFiles in FFolderOptions then
    Flags:= Flags + SHCONTF_NONFOLDERS;
  if (ParentFolder.EnumObjects(self.parent.handle, Flags, EnumList) = NOERROR) then begin
    Result:= False;
    EnumList.Reset;
    DummyResult:= 1;
    while (EnumList.Next(1, ItemIdList, DummyResult) = NOERROR) do begin
      newnode:= CreateJFSFolderNode(ItemIdList, Node, False);
      FreePidl(ItemIdList);
      if Assigned(newnode) then begin
        Result:= True;
      end;
    end;
{$IFNDEF DELPHI3UP}
    EnumList.Release;
{$ENDIF}
  end;
end;

function TrtcFolderTree.CreateExtStorageFolderNode(AFolderType: TFolderType;
  AParent: TTreeNode; const AURL, AName: String; ANodeType: TNodeType): TTreeNode;
var
  NewNodeData: PFolderNode;
  // url address
  protocol, host, user, password, IPaddress, fn_baseDN, bookmark,
  attrs, scope, filter: String;
  port: Integer;
  p: Boolean;
begin
  Result:= Nil;
  // do not attach files to the folder tree if not required
  if (not (foFiles in FFolderOptions)) and(ANodeType = ntFile) then
    Exit;
  
  case AFolderType of
    ftFtpNode: begin
      p:= ParseFtpUrl(AURL, protocol, user, password, host, IPaddress, fn_baseDN, bookmark, port, 'ftp', 21)
    end;
    ftLdapNode: begin
      p:= ParseLdapUrl(AURL, protocol, user, password, host, fn_baseDN, attrs, scope, filter, port, 'ftp', 21)
    end;
    ftDbNode: begin
      p:= False;
    end;
    else begin // ftHTTPNode
      p:= ParseUrl(AURL, protocol, user, password, host, IPaddress, fn_baseDN, bookmark, port, 'ftp', 21)
    end;
  end; // case
  if not p or (Length(fn_baseDN) = 0)
  then Exit;

  if (ANodeType = ntFile) and (FileIndexOfMask(fn_baseDN) < 0)  // if file and file extension is on mask
  then Exit;

  New(NewNodeData);
  NewNodeData.FN_PidLen:= 0;    // ^
  NewNodeData.FN_PidList:= Nil; // ^
  NewNodeData.FN_Path:= AUrl;   // ^
  NewNodeData.FN_Text:= AName;  // ^
  NewNodeData.FN_Has_Parent:= (aParent <> Nil); // ^
  NewNodeData.FN_Type:= AFolderType; // ^

  NewNodeData.FN_NodeType:= ANodeType;

  {// get system icon
  if NewNodeData.FN_NodeType = ntFolder // ^
  then Flags:= SHGFI_SYSICONINDEX or SHGFI_OPENICON  // SHGFI_PIDL or
  else Flags:= SHGFI_SYSICONINDEX;
  // icon:= utilwin.FileExtIcon(ExtractFileExt(AName));
   SHGetFileInfo(PAnsiChar(AName), SFGAO_SHARE, FileInfo, Sizeof(FileInfo), Flags);
  }

  Result:= Self.Items.AddChildObject(aParent, NewNodeData.FN_Text, NewNodeData); // ^
  with Result do begin
    HasChildren:= NewNodeData.FN_NodeType = ntFolder; // ^

    OverlayIndex:= -1;
    { uncomment this if you want have overlay icons (one over one)
    OverlayIndex:= AParent.ImageIndex;
    if OverlayIndex < 0
    then OverlayIndex:= 0;
    }

    if NewNodeData.FN_NodeType = ntFolder // ^
    then ImageIndex:= FDefaultFolderImageIndex
    else begin
      // this node is not associated with Shell (located remotely)
      ImageIndex:= -1;
      // we must add icon from other resource (see EmbeddedOnCustomDrawItem)
      // and it 'll be located in other executables or library module not in Shell's icons
    end;
    // ImageIndex:= AParent.ImageIndex;
    SelectedIndex:= ImageIndex;
  end;
end;

// return has children or not
function TrtcFolderTree.AttachStorageFolders(AFolderType: TFolderType; var Node: TTreeNode): Boolean;
var
  FolderList: TStrings;
  newnode: TTreeNode;
  i: Integer;
begin
  Result:= True;
  if not Assigned(FOnExternalStorageFolderList) then begin
    if Assigned(FOnExternalAsyncStorage) then begin
      Include(FStates, fsWaitExpandDir);
      FOnExternalAsyncStorage(Self, PFolderNode(Node.Data).FN_Path);
    end;
    Result:= True;
    Exit;
  end;
  
  FolderList:= TStringList.Create;
  FOnExternalStorageFolderList(Self, AFolderType, PFolderNode(Node.Data).FN_Path, FolderList); // ^
  for i:= 0 to FolderList.Count - 1 do begin
    newnode:= CreateExtStorageFolderNode(AFolderType, Node, PFolderNode(Node.Data).FN_Path + '/' + FolderList[i],
      FolderList[i], TNodeType(FolderList.Objects[i])); // ^
    if Assigned(newnode) then begin
      Result:= True;
    end;
  end;
  FolderList.Free;
end;

// GetPath returns the path name for True folders, and a null String
// for virtual folders.
function TrtcFolderTree.GetPath(Node: TTreeNode): String;
var
  path: String;
  foldernode: PFolderNode;
  worknode: TTreeNode;
begin
  path:= '';
  if not Assigned(Node)
  then Exit;

  worknode:= Node;
  foldernode:= worknode.data;
  if foldernode.FN_Type in VirtualFolders // ^
  then Exit;

  path:= foldernode.FN_Path; // ^

  if Length(path) > 0                   // If path is null, it's a virtual folder.
    then if length(path) < 3           // If the Result is less than 3 bytes, its the root--add a "\".
      then path:= path + '\';
  Result:= path;
end;

{ ListPath
  Parameters
    APath - url

  Return
    AList - parsed list of directories and files
    Result
     -1 - unknown
      0 - JFS
      1 - ftp
      2 - ldap
      3 - http
}
function TrtcFolderTree.ListPath(var APath: String; AList: TStrings): Integer;
var
  // url address
  protocol, host, user, password, IPaddress, fn, bookmark,
    baseDN, attrs, scope, filter: String;
  p, port: Integer;
  tail, prot: String;
  b: Boolean;
begin
  AList.Clear;
  if IsFTPUrl(APath)
  then Result:= 1
  else if IsLdapUrl(APath)
    then Result:= 2
    else if IsHTTPUrl(APath)
    then Result:= 3
    else Result:= 0;
  if Result > 0 then begin
    case Result of
      1:begin
          prot:= 'ftp';
          b:= ParseFtpUrl(APath, protocol, user, password, host, IPaddress, fn, bookmark, port, prot, 21);
          if port = 21 then
            port:= -1; // for composeUrl to skip default port number
        end;
      2:begin
          prot:= 'ldap';
          b:= ParseLdapUrl(APath, protocol, user, password, host, baseDN, attrs, scope, filter, port, prot, 389);
          if port = 389 then
            port:= -1; // for composeUrl to skip default port number
        end;
      else begin // 3
          prot:= 'http';
          b:= ParseUrl(APath, protocol, user, password, host, IPaddress, fn, bookmark, port, prot, 80);
          if port = 80 then
            port:= -1; // for composeUrl to skip default port number
        end;
    end;
    if b then begin
      while Length(fn) > 0 do begin
        p:= LastDelimiter('/', fn);
        tail:= Copy(fn, p + 1, MaxInt);
        if tail = ''
        then tail:= fn;
        // AList.Insert(0, util1.ComposeUrl(prot, user, password, host, tail, '', Port));
        AList.Insert(0, tail);
        fn:= Copy(fn, 1, (Length(fn) - Length(tail) - 1));
      end;
      AList.Insert(0, ComposeUrl(prot, user, password, host, '', '', Port));
    end;
  end else begin
    // JFS
    Result:= 0;
     while Length(APath) > 0 do begin
       if ANSICompareText(APath, fDesktopPath) = 0
       then tail:= APath
       else begin
         tail:= ExtractFileName(APath);
         if tail = ''
         then tail:= APath;
       end;
       AList.Insert(0, tail);
       APath:= Copy(APath, 1, (Length(APath) - Length(tail) - 1));
     end;
  end;
end;

procedure TrtcFolderTree.ListViewClick(Sender: TObject);
var
  d: String;
begin
  // if Assigned(FFilesView) then ?!!
//  UpdateListView(Self.Selected);
  if Assigned(FFilesViewAdaptor) then begin
    d:= FFilesViewAdaptor.GetSelectedItemDir;
    if Length(d) > 0 then
      Directory:= d;
      if Assigned(FOnDirectoryChanged) then
        FOnDirectoryChanged(Self);
  end;
end;

// SetFilesList links control to show files in selected folder
procedure TrtcFolderTree.SetFilesList(AValue: TCustomListView);
begin
  if Assigned(FFilesViewAdaptor) then
    FFilesViewAdaptor.Free;

  FFilesViewAdaptor:= TrtcListViewAdaptor.Create;

  if Assigned(FFilesViewAdaptor) then begin
    if (AValue is TListView) then begin
      with TListView(AValue) do begin
        LargeImages:= SysImageList;
        SmallImages:= SysImageList;
        StateImages:= SysImageList;
        OnClick:= ListViewClick;
      end;
    end else begin
    end;
    FFilesViewAdaptor.FolderTree:= Self;
    FFilesViewAdaptor.OnExternalAsyncStorage:= OnExternalAsyncStorage;
    FFilesViewAdaptor.OnExternalStorageFolderList:= Nil;// OnExternalStorageFolderList;
    FFilesViewAdaptor.OnExternalDeleteUrl:= OnExternalDeleteUrl;
    FFilesViewAdaptor.OnExternalMoveUrl:= OnExternalMoveUrl;
    FFilesViewAdaptor.OnExternalCopyUrl:= OnExternalCopyUrl;

  end;
end;

{ ------------------------------ drag and drop ------------------------------- }
{ Source is origin control
  Sender is destination control
}
procedure TrtcFolderTree.FDragOverEvent(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
var
  DropNode: TTreeNode;
begin
  Accept:= False;
  if not ((Source is TCustomListView) or (Source is TCustomTreeView)) then begin
    Exit;
  end;
  if Sender is TCustomTreeView then begin // over treeview, so this check unessesary
    with TCustomTreeView(Sender) do begin
      DropNode:= GetNodeAt(X, Y);
      if Assigned(DropNode) then begin
        if (PFolderNode(DropNode.Data).FN_NodeType = ntFolder) then begin
          SetNodeState(DropNode, TVIS_BOLD); //
          Accept:= True;
        end;
      end;
    end;
  end;
end;

{ Source is origin control
  Sender is destination control
}
procedure TrtcFolderTree.FDragDropEvent(Sender, Source: TObject; X, Y: Integer);
var
  i: Integer;
  DragItem: TListItem;
  DropItem: TTreeNode;
  srcUrl, desturl: String;
begin
  if not ((Source is TCustomListView) or (Source is TCustomTreeView)) then begin
    Exit;
  end;
  if Sender is TCustomTreeView then begin // must be always
    if Source is TrtcFolderView then begin // must be always
      with TCustomTreeView(Sender) do begin
        DropItem:= GetNodeAt(X, Y);
        if Assigned(DropItem) then begin
          if PFolderNode(DropItem.Data).FN_NodeType = ntFolder then begin
            with TrtcFolderView(Source) do begin
              for i:= 0 to Items.Count - 1 do begin
                if Items[i].Selected then begin
                  if (Items[i].SubItems.Count < 5) then
                    Continue;
                  srcUrl:= Items[i].SubItems[4];
                  if Assigned(FFilesViewAdaptor) then
                    FFilesViewAdaptor.CopyFile(srcUrl, desturl);
                end;
              end;
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TrtcFolderTree.SetOnExtAsyncStorage(AValue: TOnExtStorageAsyncFolderList);
begin
  FOnExternalAsyncStorage:= AValue;
  if Assigned(FFilesViewAdaptor) then
    FFilesViewAdaptor.OnExternalAsyncStorage:= AValue;
end;

procedure TrtcFolderTree.SetOnExtDeleteUrl(AValue: TOnExtDeleteUrl);
begin
  FOnExtDeleteUrl:= AValue;
  if Assigned(FFilesViewAdaptor) then
    FFilesViewAdaptor.OnExternalDeleteUrl:= AValue;
end;

procedure TrtcFolderTree.SetOnExtMoveUrl(AValue: TOnExtMoveUrl);
begin
  FOnExtMoveUrl:= AValue;
  if Assigned(FFilesViewAdaptor) then
    FFilesViewAdaptor.OnExternalMoveUrl:= AValue;
end;

procedure TrtcFolderTree.SetOnExtCopyUrl(AValue: TOnExtCopyUrl);
begin
  FOnExtCopyUrl:= AValue;
  if Assigned(FFilesViewAdaptor) then
    FFilesViewAdaptor.OnExternalCopyUrl:= AValue;
end;


// FindPath tries to find the path passed in "path" in the tree,
// and returns the node closest to it. If "exact" is True, we're
// interested only in an exact match among nodes already showing,
// and return Nil if we don't get it; this is used by the routine
// that handles changes in the file system.
function TrtcFolderTree.FindPath(const APath: String; AExact: Boolean): TTreeNode;
var
  folder: TStringList;
  workpath,
  nodepath: String;
  Node,
  LastNode: TTreeNode;
  FolderNode: PFolderNode;
  wplen, depth: Integer;
  StartNode: TTreeNode;
  storage: Integer; // -1 - unknown, 0- jfs, 1- ftp 2- ldap 3- http
begin
   Result:= Nil;
   // Break "path" up into individual levels, which are stored in
   // the folder list in LIFO order. If the path starts with the
   // desktop folder, we'll pile the whole desktop folder path
   // onto the stack at once.
   workpath:= APath;
   wplen:= Length(workpath);
   if wplen < 3 then begin
     Self.Selected:= Nil;
     Exit;
   end;
   if workpath[wplen] in ['\', '/']    // Lop off any trailing backslash [and slash - Apr 2003]
   then System.Delete(workpath, wplen, 1);

   folder:= TStringList.Create;  // Moved here 12/14/1999
   storage:= ListPath(workpath, folder);
   try
     // Locate each successive element of the path, one level
     // at a time, except: the first element must be found
     // at DesktopNode, in a child of MyComputerNode, or
     // at Network Neighborhood.
     workpath:= folder[0];            // Set workpath to the first path node.
     Node:= GetDesktopNode;
     if Assigned(Node) then begin
       FolderNode:= Node.Data;
       if ANSICompareText(FolderNode.FN_Path, Workpath) = 0 // ^
       then                          // If the first path node is the desktop then continue processing...
       else begin
         if workpath = '\'
         then begin
           Node:= GetNetHoodNode;
           StartNode:= Node;
         end else begin
           case storage of
           0:begin
               StartNode:= GetMyComputerNode;
             end;
           1:begin
               StartNode:= GetSpecialNode(ftFTPList);
             end;
           2:begin
               StartNode:= GetSpecialNode(ftLDAPList);
             end;
           3:begin
               StartNode:= GetSpecialNode(ftHTTPList);
             end;
           else StartNode:= Nil;
           end;
           if Assigned(StartNode) then begin
             if StartNode.Count < 1
             then StartNode.Expand(False);
             Node:= StartNode.GetFirstChild;
           end;

         end;
         while Node <> Nil do begin
           FolderNode:= Node.data;                    // Search children of
           nodepath:= FolderNode.FN_Path;            // ^ MyComputerNode for a match.
           if Copy(nodepath, Length(nodepath),1) = '\'
           then system.delete(nodepath, Length(nodepath), 1);
           if AnsiCompareText(nodepath, workpath) = 0
           then Break
           else begin
             if Assigned(StartNode) then begin
               Node:= StartNode.GetNextChild(Node);
             end else node:= Nil;
           end;
         end;
       end;
     end;
     // If no match is found, give up.
     if Node = Nil
     then Self.Selected:= Nil

     // Starting with the children of the current Node, look for the
     // next level. Repeat until there is no match, or the list of
     // nodes is exhausted.
     else begin
       LastNode:= Node;
       depth:= 1;
       while depth < Folder.Count do begin
         if Node.Count <= 0
         then if (not AExact)                   // Unless exact match is required,
           then AttachFolders(LastNode);  // ensure children are found.
         Node:= LastNode.GetFirstChild;
         case storage of
           1,2,3: workpath:= workpath + '/' + Folder[depth];
           else workpath:= workpath + '\' + Folder[depth];
         end;
         while Node <> Nil do begin
           FolderNode:= Node.Data;          // Search children of
           if ANSICompareText(FolderNode.FN_Path, workpath) = 0  // ^ MyComputerNode for a match.
           then Break
           else begin
             if Assigned(StartNode) then begin
               Node:= StartNode.GetNextChild(Node);
             end;
           end;
         end;
         // If a matching node was found, set LastNode to that node,
         // then try the next level down, if any.
         // If an exact match is required, return Nil if we didn't
         // match at this level.
         if Node <> Nil then begin
           LastNode:= Node;
           Inc(depth);
         end else begin
           if AExact
           then LastNode:= Nil;
           Break;
         end;
       end;
       // Return the last matching node.
       Result:= LastNode;
     end;
   finally
     Folder.Free;
   end;
end;

// MessageProc is the message handling routine.
procedure TrtcFolderTree.MessageProc(var Message: TMessage);
type
  TPIDLLIST = record
    pidlist: array[1..2] of PITEMIDLIST;
end;
  PIDarray = ^TPIDLLIST;
var
  Path1, Path2: String;
  ptr: PIDarray;
  p1, p2: PITEMIDLIST;
  p: Integer;
  change: Integer;

  Node: TTreeNode;
  NodeExpanded: Boolean;
  AllowExpansion: Boolean;
  handled: Boolean;

  procedure RefillDir;
  begin
    NodeExpanded:= Node.expanded;
    // If the node is expanded, collapse it.
    if NodeExpanded then begin
    //               LockWindowUpdate(self.handle);
     Node.Collapse(True);
    //               LockWindowUpdate(0);
    end;

    // Free all the child nodes of the current node.
    FreeChildren(Node);
    // If this is the Desktop node, we've just freed all the nodes
    // for folders. FolderTreeExpanding won't reallocate them
    // because the node has other child nodes (My Computer, etc.).
    // So allocate nodes for the folders now.

    if Node = GetDesktopNode
      then AttachFolders(Node);

    // If the node was expanded before, expand it now. Otherwise,
    // just call the OnExpanding routine to attach child nodes
    // without showing.
    FolderTreeExpanding(self, Node, AllowExpansion);
    if NodeExpanded
    then Node.Expand(False);
  end;

begin
  handled:= False;

  if Message.msg = WM_CHANGENOTIFY then begin
    // Assign a pointer to the array of PIDLs sent with the message.
    ptr:= PIDarray(Message.wParam);
    // Parse the two PIDLs in the message.
    p1:= ptr.pidlist[1]; // ^
    try
      SetLength(Path1, MAX_PATH);
      Path1:= ParsePidl(p1);
      p:= Pos(#00, Path1);
      if p > 0
      then SetLength(Path1, p - 1);
    except
      Path1:= '';
    end;
    // Free the PIDLs passed to this routine.

    p2:= ptr.pidlist[2]; // ^
    try
      SetLength(Path2, MAX_PATH);
      Path2:= ParsePidl(p2);
      p:= Pos(#00, Path2);
      if p > 0
      then SetLength(Path2, p - 1);
    except
      Path2:= '';
    end;

    // Handle file system changes here.
    change:= Message.Lparam and (not SHCNE_INTERRUPT);

    case change of
      // SHCNE_CREATE: Node:= Nil;
      SHCNE_RENAMEITEM, SHCNE_DELETE: begin
          Node:= FindPath(Path1, True);
      end;
      // If a new directory is made, locate the node for its parent directory.
      SHCNE_MKDIR: Node:= FindPath(Path1, False);
      // If an existing directory was deleted or renamed, locate the
      // node by exact match.
      SHCNE_RENAMEFOLDER,
      SHCNE_RMDIR: begin
          Node:= FindPath(Path1, True);
          if Node <> Nil
          then Node:= Node.parent;
        end;
    end;

    case change of
      // file
      SHCNE_CREATE: begin
        Node:= FindPath(Path1, True);
        if not Assigned(Node) then begin
          Node:= FindPath(ExtractFilePath(Path1), True);
          if Assigned(Node) then begin
            RefillDir;
          end;
        end;
        Handled:= True;
      end;
      SHCNE_RENAMEITEM: begin
        if Assigned(Node) then begin
          PFolderNode(Node.Data).FN_Path:= Path2; // ^
          path2:= ExtractFileName(Path2);
          PFolderNode(Node.Data).FN_Text:= Path2; // ^
          case fDisplayMode of
            dmAsis: Node.Text:= Path2;
            dmLowercase: Node.Text:= lowercase(Path2);
            dmUppercase: Node.Text:= uppercase(Path2);
          end;
        end;

        Handled:= True;
      end;
      SHCNE_DELETE: begin
          if Assigned(Node) then begin
            Node.Delete;
            Handled:= True;
          end;
        end;
      // folder
      SHCNE_MKDIR,
      SHCNE_RENAMEFOLDER,
      SHCNE_RMDIR: begin
           if Node <> Nil then begin
             ReFillDir;
           end;
           handled:= True;
         end;

      SHCNE_DRIVEADD,
      SHCNE_MEDIAINSERTED:begin
           DriveChanged(self, path1[1], False, True);
           handled:= True;
         end;
      SHCNE_DRIVEREMOVED,
      SHCNE_MEDIAREMOVED: begin
           DriveChanged(self, path1[1], False, False);
           handled:= True;
         end;
     end;

    { //do not free Pidls not created by FolderTree (it is notification of Shell)
      // 01 Nov 2007 
    FreePidl(p1);
    FreePidl(p2);
    }
  end;

  if (Message.Msg = WM_PAINT) then begin
    Handled:= True;
    if csDesigning in ComponentState then begin
      // retrieves the cursor's position, in screen coordinates.
      {
      GetCursorPos(cursorpos);
      cursorpos.x:= cursorpos.x - parent.left;
      cursorpos.y:= cursorpos.y - parent.top;
      top:=  cursorpos.y;
      left:= cursorpos.x;
      }
      top:=  0;
      left:= 0;
    end;
  end;
  if not Handled then begin
// --Windows
// Specifies an application-defined callback function used with the SHBrowseForFolder
// function. The browse dialog box calls this function to notify it about events.
// The BFFCALLBACK type defines a pointer to this callback function.
    Message.Result:= DefWindowProc(fMessageHandle, Message.Msg,
      Message.wParam, Message.lParam);
  end;
end;

// The function ParsePidl returns the String corresponding
// to a PIDL.
function TrtcFolderTree.ParsePidl (Pidl: PITEMIDLIST): String;
begin
  SetLength(Result, MAX_PATH);
  if not SHGetPathFromIDList(Pidl, PChar(Result))
  then Result:= '';
end;

function TrtcFolderTree.GetFirstDriveLetter(unitmask: longint): Char;
var
  DriveLetter: ShortInt;
begin
  DriveLetter:= Ord('A');
  while (unitmask and 1) = 0  do begin
    unitmask:= unitmask shr 1;
    Inc(DriveLetter);
  end;
  Result:= Char(DriveLetter);
end;

destructor TrtcFolderTree.Destroy;
begin
  Deactivate;
  DeAllocateHWnd(fMessageHandle);
  SysImageList.Free;
{$IFNDEF DELPHI3UP}
  IUnknown(AllocInterface).Release;
{$ENDIF}
  Folder_List.Free;
  if Assigned(FFolderTreeInfoMemStream)
  then FFolderTreeInfoMemStream.Free;
  FHistory.Free;
  inherited;
end;

// FreeChildren frees the descendants of the Node passed to it, and
// sets HasChildren to True to permit reexpansion.
procedure TrtcFolderTree.FreeChildren(Node: TTreeNode);
var
  foldernode: PFolderNode;
  child: TTreeNode;
  i: Integer;
begin
  for i:= Node.Count - 1 downto 0 do begin
    child:= Node.item[i];
    foldernode:= Child.Data;
    if not (foldernode.FN_Type in VirtualFolders) // ^
    then Child.Delete;
  end;
  if PFolderNode(Node.Data).FN_NodeType = ntFolder // ^
  then Node.HasChildren:= True;             // Attach the plus sign button
end;

procedure TrtcFolderTree.FreeNode(Sender: TObject; Node: TTreeNode);
var
  foldernode: PFolderNode;
  path: String;
  text: String;
  pidbuf: pointer;
begin
  if FWndDestroying
  then Exit;

  foldernode:= Node.data;

  pidbuf:= foldernode.FN_PidList; // ^
  FreeMem(pidbuf);

  path:= foldernode.FN_Path;   // ^ Make sure these strings
  text:= foldernode.FN_Text;   // ^ are freed.

  Dispose(foldernode);
  Node.Data:= Nil;
end;

procedure TrtcFolderTree.DriveChanged(Sender: TObject; FirstDriveLetter: Char;
  Force: Boolean; AInserted: Boolean);
var
  Node: TTreeNode;
  MyComputer: TTreeNode;
  path: String;
  foldernode: PFolderNode;
  ShellFolder: IShellFolder;
  WCA: array[0..MAX_PATH] of WideChar;
  eaten: ULong;
  attrib: ULong;
  FilePathList: TFilePathList;
  Pidl: PItemIdList;
begin
  try
    MyComputer:= GetMyComputerNode;
    if not Assigned(MyComputer)
    then Exit;

    foldernode:= GetMyComputerNode.Data;
    ShellFolder:= foldernode.FN_ShellFolder; // ^

    // Find the node corresponding to the drive that has changed.
    if GetMyComputerNode.Count > 0
    then Node:= GetMyComputerNode.item[0]
    else Node:= Nil;
    while Node <> Nil do begin
      foldernode:= Node.data;
      if lowercase(foldernode.FN_Path[1]) = lowercase(FirstDriveLetter) // ^
      then Break;
      Node:= Node.GetNextSibling;
    end;

    path:= FirstDriveLetter + ':\';      // Set "path"
    if AInserted then begin
      StringToWideChar(path, WCA,SizeOf(WCA));
      ShellFolder.ParseDisplayName(self.parent.handle, Nil, @WCA,
        eaten, Pidl, attrib);
      FilePathList:= GetLongFilePath(Pidl, ShellFolder);
      if not Assigned(node) then begin
        // node:= AttachStorageSite(ftLocalNode, FilePathList.Normal, True)
        node:= CreateJFSFolderNode(Pidl, GetMyComputerNode, False);
        {
        if Assigned(node) then
          AttachFolders(node);
        }
      end else begin
        foldernode:= Node.data;
        if (Node.text <> FilePathList.Normal) or (force) then begin
  //        LockWindowUpdate(self.handle);
          FreeChildren(Node);                   // Free its children
          Node.Expand(False);
  //        LockWindowUpdate(0);
          Node.text:= FilePathList.Normal;
        end;
      end;
      FreePidl(Pidl);
    end else begin
      if Node <> Nil then begin
        Node.Free;
        if Assigned(FFilesViewAdaptor) and Assigned(Selected) then
          if FFilesViewAdaptor.FilesView is TrtcCustomFolderView then
            if Pos(Path, TrtcCustomFolderView(FFilesViewAdaptor.FilesView).Root) = 1 then
              // FFilesViewAdaptor.UpdateListView(Self.Selected, False);
              FFilesViewAdaptor.Dir[False]:= PFolderNode(Selected.Data).FN_Path
      end;
    end;
  except
  end;
  { // inform connected to the host viewers drive list changed
  if Assigned(FOnExternalAsyncStorage) then
    Include(FStates, fsWaitExpandDir);
    FOnExternalAsyncStorage('');
  }
end;

procedure TrtcFolderTree.FreePidl(var APidl: PItemIdList);
var
  allocator: Integer;
begin
  /// something wrong when on WM_CHANGENOTIFY try to Free pidls created by Shell.
  /// Nov 01 2007 added FreeAndNil(APidl). It does not affect, but a little bit accurate
  if (Assigned(APidl)) and Assigned(AllocInterface) then begin
    allocator:= AllocInterface.DidAlloc(APidl);
    // 1 memory block was allocated by this IMalloc instance.
    // 0 memory block was not allocated by this IMalloc instance.
    // -1 DidAlloc is unable to determine whether or not it allocated the memory block.
    if (allocator = 1)
    then begin
      AllocInterface.Free(APidl);
      APidl:= Nil;
    end;
  end;
end;

// When the user double-clicks on a node, and the path no longer
// exists, perform DriveChanged with force = True to collapse
// the tree at the drive level.
procedure TrtcFolderTree.TrtcFolderTreeDblClick(var Message: TMessage);
begin
  CheckPath;
  inherited;
end;

procedure TrtcFolderTree.TrtcFolderTreeClick(var Message: TMessage);
var
  n: TTreeNode;
begin
  n:= Selected;
  if Assigned(FFilesViewAdaptor) and (n <> Nil) then begin
    FFilesViewAdaptor.Dir[False]:= PFolderNode(n.Data).FN_Path;
  end;
  if Assigned(FOnDirectoryChanged) then
    FOnDirectoryChanged(Self);
end;

procedure TrtcFolderTree.CheckPath;
var
  Node: TTreeNode;
  foldernode: PFolderNode;
  path: String;
begin
  if Assigned(Selected) then begin;
    Node:= Selected;
    foldernode:= Node.Data;
    Path:= foldernode.FN_Path; // ^
    if (Length(Path) > 3) and (Path[1] = ':') then begin // was <> '\'
      if FileGetAttr(path) = -1
      then DriveChanged(self, path[1], True, True)
    end else Node.HasChildren:= True;
  end;
end;

function  TrtcFolderTree.GetDesktopNode: TTreeNode;
begin
  if items.Count > 0
  then Result:= items[0]
  else Result:= Nil;
end;

function  TrtcFolderTree.GetMyComputerNode: TTreeNode;
begin
  Result:= GetSpecialNode(ftMyComputer);
end;

function  TrtcFolderTree.GetNetHoodNode: TTreeNode;
begin
  Result:= GetSpecialNode(ftNetworkNeighborhood);
end;

function  TrtcFolderTree.GetRecyledNode: TTreeNode;
begin
  Result:= GetSpecialNode(ftRecycleBin);
end;

function TrtcFolderTree.GetSpecialNode(nodetype: TFolderType): TTreeNode;
var
  foldernode: PFolderNode;
  i: Integer;
begin
  Result:= Nil;
  for i:= 0 to Items.Count - 1 do begin
    foldernode:= items[i].data;
    if foldernode.FN_Type = nodetype then begin // ^
      Result:= items[i];
      Break;
    end;
  end;
end;

// This is our substitute window proc. Its function is to intercept
// the WM_QUERYENDSESSION message and act upon it.
procedure TrtcFolderTree.WndProc(var AMsg: TMessage);
begin
  if AMsg.Msg = WM_QUERYENDSESSION then QueryEndSession(AMsg);
  // Call the original message handler.
  inherited WndProc(AMsg);
end;

procedure TrtcFolderTree.QueryEndSession(var AMsg: TMessage);
var
  CanEndSession: Boolean;
begin
  CanEndSession:= True;
  if Assigned(fEndSessionQuery)
  then fEndSessionQuery(Self, CanEndSession);
  if CanEndSession then begin
    DeregisterChangeNotify;
    AMsg.Result:= 1;
  end else AMsg.Result:= 0;
end;

procedure TrtcFolderTree.RegisterChangeNotify;
begin
  // Don't trap change notifications while in design mode.
  if csDesigning in ComponentState
  then Exit;

  New(NotifyRegister);
  NotifyRegister.pidlPath:= Nil;  // ^
  NotifyRegister.bWatchSubtree:= True; // ^

  // Register a change notification handle.
  if Assigned(SHChangeNotifyRegister) then begin // works on Windows 2000+
    NotifyHandle:= SHChangeNotifyRegister(
      fMessageHandle,

      SHCNF_ACCEPT_INTERRUPTS + SHCNF_ACCEPT_NON_INTERRUPTS + SHCNRF_RecursiveInterrupt,

      // We're watching for the following events.
      SHCNE_RENAMEITEM +
      SHCNE_CREATE +
      SHCNE_DELETE +

      SHCNE_MEDIAINSERTED +
      SHCNE_MEDIAREMOVED +

      SHCNE_DRIVEADD +
      SHCNE_DRIVEREMOVED +

      SHCNE_MKDIR +
      SHCNE_RENAMEFOLDER +
      SHCNE_RMDIR,
      WM_CHANGENOTIFY, // WM_CHANGENOTIFY is the message number used for notifications
      1,               // There is one subtree being watched: Desktop (i.e., everything)
      NotifyRegister); // This parm indicates what's being watched.

    if NotifyHandle = 0
    then raise Exception.Create('Could not register SHChangeNotify');
  end else NotifyHandle:= 0;
end;

// This procedure unhooks the SHChangeNotifyRegister hook. It's needed
// both when the component is destroyed and if system shutdown is detected.
procedure TrtcFolderTree.DeregisterChangeNotify;
begin
  try
    if Assigned(NotifyRegister)
    then Dispose (NotifyRegister);
  finally
    NotifyRegister:= Nil;
  end;
  try
    if NotifyHandle <> 0 then begin
      if Assigned(SHChangeNotifyDeregister) then begin // works on Windows 2000+
        SHChangeNotifyDeregister(NotifyHandle);
      end;
    end
  finally
    NotifyHandle:= 0;
  end;
end;

procedure TrtcFolderTree.CreateWnd;
var
  I, c: Integer;
  b: Boolean;
begin
  inherited CreateWnd;
  if Assigned(FFolderTreeInfoMemStream) then begin
    // disable listview redraws
    // Perform(WM_SETREDRAW, 0, 0); // Apr 2003

    Items.BeginUpdate;
    try
      FFolderTreeInfoMemStream.Position:= 0;
      c:= FFolderTreeInfoMemStream.Size div SizeOf(Boolean);
      if c > Items.Count
      then c:= Items.Count;
      for i:= 0 to c - 1 do begin
        FFolderTreeInfoMemStream.Read(b, SizeOf(Boolean));
        Items[i].HasChildren:= b;
        // I dont know why IDE stops there at user breakpoint. sleep(100)
      end;
    finally
      Items.EndUpdate;
      // enable listview redraws
      // Perform(WM_SETREDRAW, 1, 0); // Apr 2003
    end;
    // destroy temporary memory buffers
    FFolderTreeInfoMemStream.Free;
    FFolderTreeInfoMemStream:= Nil;
  end;
  FWndDestroying:= False;
end;

// DirUpdateFiles called from TrtcPFileBrowser to update view when event is received
function TrtcFolderTree.DirUpdateFiles(AFolderType: TFolderType;
  const ADir: String; AFiles: TRemoteFileInfos; AShow: Boolean): Boolean;
var
  n, node, newnode: TTreeNode;
  i: Integer;
  foldernode: TFolderNode;
  fn: String;
  li: TListItem;
  ft: TFolderType;
  nofiles: Boolean;
  // fi: TSHFileInfo;
  // w: Word;
begin
  Result:= False;
  // add host node to the HOST nodes if not exists (or detach if AShow = False)
  // get site node type
  case AFolderType of
    ftDBNode, ftDBList: ft:= ftDBList;
    ftLDAPNode, ftLDAPList: ft:= ftLDAPList;
    ftFTPNode, ftFTPList: ft:= ftFTPList;
    ftHTTPNode, ftHTTPList: ft:= ftHTTPList;
    else ft:= ftDesktop;
  end;
  n:= AttachStorageSite(ft, ADir, AShow);
  // clear list view if nide is detached before exit
  if Assigned(FFilesViewAdaptor) then begin
    if (not AShow) and (Selected = node) then
      FFilesViewAdaptor.FilesView.Clear;

    if not Assigned(n) then begin
      if not AShow then begin
        // delete items in listview if exists
        li:= FFilesViewAdaptor.FindListItem(ADir);
        if Assigned(li) then
          li.Free;
      end;
      Exit;
    end;
  end;

  Include(FStates, fsUpdatingListView);
  node:= GoToDirectory(ADir, False);
  Exclude(FStates, fsUpdatingListView);
  nofiles:= not (foFiles in FFolderOptions);

  if not Assigned(node) then begin
    // get root node
    node:= n;
  end;
  if Assigned(FFilesViewAdaptor) then begin
    FFilesViewAdaptor.FilesView.Clear;
  end;

  if Assigned(AFiles) then begin
    // update tree view
    for i:= 0 to Length(AFiles) - 1 do begin
      // skip files, process folder (if not specified foFiles)
      if (AFiles[i].FileNodeType <> ntFolder) and (nofiles)
        then Continue;
      // update treeview
      fn:= PFolderNode(Node.Data).FN_Path + '/' + AFiles[i].FileName;
      // first check, is node already exists
      newnode:= FindChildNode(node, AFiles[i].FileName);
      // if not, create anew one
      if not Assigned(newnode) then begin
        // create an items in treview (if node is ntFile, node skipped (if nor foFilese is specified))
        newnode:= CreateExtStorageFolderNode(AFolderType, Node,
          fn,
          AFiles[i].FileName, AFiles[i].FileNodeType); // ^
      end;
      // AFiles return icon number that is associated at the remote PC.
      // ...
      // insert code to re-associate icon from remote to the local by scanning registry
      // for associated MIME type with a file extension
//      if newnode.ImageIndex < 0 then begin
//        // ExtractAssociatedIcon(HINSTANCE, PChar(@(AFiles[i].FileName[1])), w);
//        ShGetFileInfo(PChar(@(AFiles[i].FileName[1]), 0, fi, SizeOf(fi),
//          SHGFI_SMALLICON or SHGFI_SYSICONINDEX or SHGFI_TYPENAME);
//        newnode.ImageIndex:= fi.iIcon; // AFiles[i].FileInfo.iIcon;
//      end;
    end;

    if (Selected = node) then begin
      if fsWaitExpandDir in FStates then begin
        // when user click on '+' to expand subtree, it causes viewer
        // notify host that user want to expand node. Node is not expands until
        // host have send a subtree items (to thos place)
        node.Expand(False);
      end;
      for i:= 0 to Length(AFiles) - 1 do begin
//
//      CreateExtStorageListItem(AFolderType, Node, PFolderNode(Node.Data).FN_Path + '/' + AFiles[i].FileName,
//        AFiles[i].FileName, TObject(AFiles[i].FileNodeType),
//        foldernode, AFiles[i].FileInfo, AFiles[i].FileInfo.dwAttributes); // ^
//

        // it does not works because it extract icon from the local file
        // ExtractAssociatedIcon(HINSTANCE, , w);
        // it does not works because ShGetFileInfo wants existing local file
        // and it does not works with wilcards *.pas and non-existing files
        // ShGetFileInfo(PChar(@(AFiles[i].FileName[1])), SFGAO_SHARE, fi, SizeOf(fi), SHGFI_SYSICONINDEX);
        if AFiles[i].FileNodeType = ntFile then
          AFiles[i].FileInfo.iIcon:= FDefaultFileImageIndex
        else
          AFiles[i].FileInfo.iIcon:= FDefaultFolderImageIndex;

        // update linked list view if it is same directory
        if Assigned(FFilesViewAdaptor) then begin
          if FFilesViewAdaptor.FilesView is TListView then begin
            li:= TListView(FFilesViewAdaptor.FilesView).Items.Add;
          end;
          if FFilesViewAdaptor.FilesView is TrtcFolderView then begin
            li:= TrtcFolderView(FFilesViewAdaptor.FilesView).Items.Add;
          end;
          // file name
          li.Caption:= AFiles[i].FileName;
          // 0 size
          if AFiles[i].FileNodeType = ntFolder then
            li.SubItems.Add('')
          else
            li.SubItems.Add(IntToStr(AFiles[i].FileSize));
          // 1 type description
          li.SubItems.Add(AFiles[i].FileInfo.szTypeName);
          // 2 modified date
          if AFiles[i].FileNodeType = ntFolder then begin
            li.SubItems.AddObject(DateTimeToStr(AFiles[i].FileCreationTime), Nil); // sort by date stored in object pointer
          end else begin
            li.SubItems.AddObject(DateTimeToStr(AFiles[i].FileLastAccessTime), Nil); // sort by date stored in object pointer
          end;

          // 3 attrs
          li.SubItems.AddObject(GetFileAttributesString(AFiles[i].FileInfo.dwAttributes), TObject(AFiles[i].FileInfo.dwAttributes));
          // 4 path
          li.SubItems.Add(ADir + '/' + AFiles[i].FileName);
          li.ImageIndex:= AFiles[i].FileInfo.iIcon;
          // ntFolder, ntFile
          li.Data:= Pointer(AFiles[i].FileNodeType);
        end;
      end;
    end;
  end;
  if fsWaitExpandDir in FStates then begin
    Exclude(FStates, fsWaitExpandDir);
  end;

  // sort
  if Assigned(FFilesViewAdaptor) then begin
    if FFilesViewAdaptor.FilesView is TrtcFolderView then
      TrtcFolderView(FFilesViewAdaptor.FilesView).Sort;
  end;

  Result:= True;
end;

procedure TrtcFolderTree.DestroyWnd;
var
  I: Integer;
  b: Boolean;
begin
  FWndDestroying:= True;
  if (Items.Count > 0) then begin
    FFolderTreeInfoMemStream:= TMemoryStream.Create;
    try
      for i:= 0 to Items.Count - 1 do begin
        b:= Items[i].HasChildren;
        FFolderTreeInfoMemStream.Write(b, SizeOf(Boolean));
      end;
    finally
      Items.EndUpdate;
    end;
  end;
  inherited DestroyWnd;
end;

procedure TrtcFolderTree.SetActive(Value: Boolean);
begin
  if Value
  then Activate
  else Deactivate;
end;

procedure TrtcFolderTree.Activate;
begin
  if FActive
  then Exit;
  FActive:= True;
  // Register the change notification
  RegisterChangeNotify;
  RebuildDesktop;
end;

procedure TrtcFolderTree.Deactivate;
begin
  // Unregister the change notification
  DeRegisterChangeNotify;
  // Delete the top node. That will take care of deleting all the others.
  if self.items.Count > 0
  then self.items[0].Delete;

  FActive:= False;
end;

function BrowseCallback(Wnd: HWND; uMsg: UINT; lParam, lpData: LPARAM): Integer stdcall;
begin
  Result:= 0;
  if uMsg = BFFM_Initialized then with TFolderBrowseDialog(lpData) do begin // Pointer() to avoid compiler warnings
    if Length( Directory ) > 0 then begin
//--Windows
// BFFM_SETSELECTION Selects the specified folder. The lParam parameter is the PIDL of the folder
// to select if wParam is FALSE, or it is the path of the folder otherwise.
      SendMessage(Wnd, BFFM_SetSelection, 1, Longint(PChar(Directory)));
    end;
  end;
end;

function TFolderBrowseDialog.Execute(Form: TWinControl): Boolean;
var
  Buffer: array [ 0..MAX_PATH ] of Char;
  ItemIdList: PItemIDList;
begin
  Result:= False;
  with FBrowseInfo do begin
    hwndOwner:= Form.Handle;
    pidlRoot:= Nil;
    pszDisplayName:= Buffer;
    lpszTitle:= PChar( FTitle );
    ulFlags:= BIF_RETURNONLYFSDIRS;
    lpfn:= BrowseCallback;
    lParam:= Longint(Self);
  end;
  ItemIdList:= ShBrowseForFolder(FBrowseInfo);
  if ItemIDList = Nil
  then Exit;
  Result:= SHGetPathFromIDList(ItemIDList, Buffer);
  FDirectory:= Buffer;
end;

{ ------------------------- THistoryStack ------------------------------------ }

procedure THistoryStack.PushItem(AItem: Pointer);
begin
  if AtLeast(MAX_HISTORY_SIZE) then
    List.Delete(0);
  inherited PushItem(AItem);
end;

procedure Register;
begin
  RegisterComponents(COMPONENT_PALETTE, [TrtcFolderTree, TFolderBrowseDialog]);
end;

end.
