unit
  rtcListViewAdaptor;
(*##*)
(*******************************************************************************
*                                                                             *
*   R  T  C  L  i  s  t  V  i  e  w  A  d  a  p  t  o  r                       *
*                                                                             *
*   List view control adaptor for treeview                                     *
*                                                                             *
*   Copyright (c) 2007, Andrei Ivanov, RealThinComponents                      *
*                                                                             *
*   Conditional defines:                                                       *
*                                                                             *
*   Last Revision: Nov 26 2007                                                 *
*   First release: Nov 26 2007                                                *
*   Lines:         2286                                                        *
*   History:       Nov 26 2007                                                *
*   Notes:                                                                     *
*   ListView Adaptor publish methods to initiate list view to show files      *
*   and created automatically by FolderTree when ListView is attached toc the  *
*   tree. Can be used separately to display files from some kind of storages  *
********************************************************************************)
(*##*)

{$R-}
{$WARNINGS OFF} // deprecated symbols, Move(ptr^..), @
{$IFNDEF VER90}{$IFNDEF VER91}{$IFNDEF VER92}
   {$DEFINE DELPHI3UP}
{$ENDIF}{$ENDIF}{$ENDIF}
{
  @html(<b>)
  Folder View Adaptor
  @html(</b>)
  @html(<br><br>)
  Introducing the @html(<b>) @Link(TrtcListViewAdaptor) @html(</b>) class:
  @html(<br>)
  TrtcListViewAdaptor component is automatically created class based on TPersistent.
  This class introduce methods to display files in the List view and @Link(TrtcFolderView)
}

interface

uses
  Math, Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, CommCtrl, ShlObj, ActiveX, StdCtrls, ImgList,
  ShellAPI, rtcShellHelper;

{
  @html(<b>)
  Folder View
  @html(</b>)
  @html(<br><br>)
  Introducing the @html(<b>) @Link(TrtcFolderView)@html(</b>) component:
  @html(<br>)
  Folder view component based on TCustomListView and is descendant of
  @Link(TrtcCustomFolderView).
  @Link(TrtcFolderView) is a companion component for @Link(TrtcListViewAdaptor) assigned
  to the @Link(TrtcListViewAdaptor.ControlListView) property. TrtcListViewAdaptor.ControlListView
  can be assigned to TCustomListView descendant class such TListView.
  TrtcFolderView class designed for use with TrtcFolderView, it is
  not recommended to use TrtcFolderView alone. TrtcListViewAdaptor is responsible
  to fill out TrtcFolderView list view content. There is two reason why TrtcListViewAdaptor
  fill TrtcFolderView items:
  @html(<li>)you can assign TListView component or derived from TCustomListView class
  @html(<li>)TFolderTree already can functions to show files in the tree (see foFiles option in @Link(TrtcListViewAdaptor.FolderOptions))
}

type
  { @Abstract(Enumeration nodes callback function type) }
  TEnumNodeFunc = function(AFolderNode: TFolderNode;
    AFileInfo: TSHFileInfo; AAattrs: Cardinal): Boolean of object;

  { TrtcListViewAdaptor }
  TrtcListViewAdaptor = class(TPersistent)
  private
    FFilesView: TCustomListView;
    FFolderTree: TCustomTreeView;
    function GetDir(ATree: Boolean): String;
    procedure SetDir(ATree: Boolean; const AValue: String);
  protected
    FOnExternalStorageFolderList: TOnExtStorageFolderList;
    FOnExternalAsyncStorage: TOnExtStorageAsyncFolderList;

    // delete a file(s) by url or FQFN on external storage
    FOnExtDeleteUrl: TOnExtDeleteUrl;
    // move/rename file(s) by url or FQFN on external storage
    FOnExtMoveUrl: TOnExtMoveUrl;
    // copy file(s) by url or FQFN on external storage
    FOnExtCopyUrl: TOnExtCopyUrl;
    { @Abstract(if @Link(TrtcFolderTree.ControlListView) is assigned, UpdateListView
      updates list of files and folders in associated list view
    )}
    function UpdateListView(AFolderNode: TTreeNode; AForce: Boolean): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    { @Abstract(Find a file list item in associated list view by fully qualified file name or url)}
    function FindListItem(const AUrl: String): TListItem;
    { @Abstract(get a url of file item in associated list view.
      Return '' if not item is selected of File List is not associated with) }
    function FileInList(AItem: TListItem): String;
    { @Abstract(get a url of file item in associated list view.
      Return '' if not item is selected of File List is not associated with) }
    function FileInLV(AItem: TLVItem): String;
    { @Abstract(get a url of file itme selected in associated list view ) }
    function SelectedFileInList: String;
    { @Abstract(get a list of url of files selected in associated list view ) }
    function SelectedFilesList(var AResult: TStrings): Integer;
    { @Abstract(Delete a file by one selected item }
    function RemoveFileListItem(AItem: TListItem): Boolean;
    { @Abstract(Delete a file by one selected item (False) or all selected items (MultiSelect=True)) }
    function RemoveFileListItems(AAllSelected: Boolean): Boolean;
    { @Abstract(Move/Rename a file) }
    function MoveFile(const ASrcUrl, ADestUrl: String): Boolean;
    { @Abstract(Copy file(s)) }
    function CopyFile(const ASrcUrl, ADestUrl: String): Boolean;

    { @Abstract(if @Link(TrtcFolderTree.ControlListView) is assigned, AddItemToListView
      add a file to the list view
    )}
    function AddItemToListView(AFolderNode: TFolderNode;
      AFileInfo: TSHFileInfo; AAttrs: Cardinal): Boolean;

    { @Abstract(DirUpdateFiles called from TrtcPFileBrowser to update view when event is received}
    function DirUpdateFiles(AFolderType: TFolderType; const ADir: String;
      AFiles: TRemoteFileInfos; AShow: Boolean): Boolean;
    // GetPath returns the path name for True folders, and a null String
    // for virtual folders.
    function GetPath(Node: TTreeNode): String;
    // EnumFilesInFolder enumerate child folders to the node passed in Node.
    // Returns processed nodes count
    function  EnumFilesInFolder(var Node: TTreeNode; ACallBack: TEnumNodeFunc): Integer;
    // GetStorageFilesInFolder() call appropriate methods for nodes of JFS, FTP and other nodes
    function  EnumStorageFilesInFolder(AFolderType: TFolderType; var Node: TTreeNode): Integer;
    // JFS
    function  EnumJFSFilesInFolder(var Node: TTreeNode; ACallBack: TEnumNodeFunc): Integer;
    procedure CreateExtStorageListItem(AFolderType: TFolderType; AParent: TTreeNode;
      const AURL, AName: String; ANodeType: TNodeType;
      var NewNodeData: TFolderNode; var FileInfo: TSHFileInfo; Attributes: Cardinal);
    procedure SetOnExternalAsyncStorage(AValue: TOnExtStorageAsyncFolderList);
  public
    function GetSelectedItemDir: String;
    property Dir[ATree: Boolean]: String read GetDir write SetDir;
    property FilesView: TCustomListView read FFilesView write FFilesView;
    property FolderTree: TCustomTreeView read FFolderTree write FFolderTree;

    { @Abstract(Return a list of files and subfolders within specified location (AURL). List contains an Objects[i] of 0 or 1.
    if Integer(Objects[i] = 0, it is a file, otherwise it is a folder.
    Assign this property to the method if you want add a new external storages like
    FTP, WebDAV or database.
    Parameters: AFolderType of type @Link(TFolderType) can be ftFTP, ftHTTP.
    AUrl specify location of FTP directory.
    Result is returned in parameter AFolderList.
    See also: @Link(TrtcFolderTree.OnListExternalStorageSites), @Link(TOnListExtStorageSites) and @Link(TEndSessionQueryEvent)
    )}
    property OnExternalAsyncStorage: TOnExtStorageAsyncFolderList read FOnExternalAsyncStorage write SetOnExternalAsyncStorage;
    property OnExternalStorageFolderList: TOnExtStorageFolderList read FOnExternalStorageFolderList write FOnExternalStorageFolderList;
    property OnExternalDeleteUrl: TOnExtDeleteUrl read FOnExtDeleteUrl write FOnExtDeleteUrl;
    property OnExternalMoveUrl: TOnExtMoveUrl read FOnExtMoveUrl write FOnExtMoveUrl;
    property OnExternalCopyUrl: TOnExtCopyUrl read FOnExtCopyUrl write FOnExtCopyUrl;

  end;

implementation
uses
  StrUtils, urlFuncs, rtcFolderView, rtcFolderTree;

{ -------------------------------- TrtcListViewAdaptor ------------------------ }

constructor TrtcListViewAdaptor.Create;
begin
  inherited Create;
  FFilesView:= Nil;
  FFolderTree:= Nil;
  FOnExternalStorageFolderList:= Nil;
  FOnExternalAsyncStorage:= Nil;
  FOnExtDeleteUrl:= Nil;
  FOnExtMoveUrl:= Nil;
  FOnExtCopyUrl:= Nil;
end;

destructor TrtcListViewAdaptor.Destroy;
begin
  inherited Destroy;
end;

function TrtcListViewAdaptor.GetDir(ATree: Boolean): String;
begin
  Result:= '';
  if ATree then begin
    if Assigned(FFolderTree) then
      Result:= TrtcFolderTree(FFolderTree).Directory;
  end else begin
    if Assigned(FFilesView) then
      if FFilesView is TrtcCustomFolderView then
        Result:= TrtcCustomFolderView(FFilesView).Root;
  end;
end;

procedure TrtcListViewAdaptor.SetDir(ATree: Boolean; const AValue: String);
var
  n: TTreeNode;
begin
  if GetDir(False) <> AValue then begin
    if ATree then begin
      if Assigned(FFolderTree) then begin
        if FFolderTree is TrtcFolderTree then begin
          TrtcFolderTree(FFolderTree).Directory:= AValue;
          if Assigned(TrtcFolderTree(FFolderTree).OnDirectoryChanged) then
            TrtcFolderTree(FFolderTree).OnDirectoryChanged(Self);
        end;
      end;
    end else begin
      if Assigned(FFilesView) then begin
        if Assigned(FFolderTree) then
          n:= FFolderTree.Selected
        else
          n:= Nil;
        // UpdateListView(n, False);
        if FFilesView is TrtcCustomFolderView then begin
          UpdateListView(n, False);
          TrtcCustomFolderView(FFilesView).Root:= AValue;
        end;
      end;
    end;
  end;
end;

// Find a file list item in associated list view by fully qualified file name or url
function TrtcListViewAdaptor.FindListItem(const AUrl: String): TListItem;
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
function TrtcListViewAdaptor.FileInList(AItem: TListItem): String;
begin
  Result:= '';
  if Assigned(AItem) then begin
    if (AItem.SubItems.Count >= 5) then begin
      Result:= AItem.SubItems[4];
    end;
  end;
end;

// get a url of file item in associated list view.
// Return '' if not item is selected of File List is not associated with) }
function TrtcListViewAdaptor.FileInLV(AItem: TLVItem): String;
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
function TrtcListViewAdaptor.SelectedFileInList: String;
begin
  Result:= '';
  if Assigned(FFilesView) then begin
    Result:= FileInList(FFilesView.Selected);
  end;
end;

// get a list of url of files selected in associated list view
// Return 0 if no items selected or no file list associated with
// AResult can be Nil
function TrtcListViewAdaptor.SelectedFilesList(var AResult: TStrings): Integer;
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

// Delete a file by one selected item
function TrtcListViewAdaptor.RemoveFileListItem(AItem: TListItem): Boolean;
begin
//  Result:= False;
  AItem.Free;
  Result:= True;
end;
// Delete a file by one selected node (False) or all selected nodes (MultiSelect=True)
function TrtcListViewAdaptor.RemoveFileListItems(AAllSelected: Boolean): Boolean;
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

function TrtcListViewAdaptor.MoveFile(const ASrcUrl, ADestUrl: String): Boolean;
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
      Result:= FOnExtMoveUrl(Self, ASrcUrl, ADestUrl);
  end;
  if Result then begin
    if Assigned(FFolderTree) then
      n:= FFolderTree.Selected;
      if Assigned(n) then
        UpdateListView(n, True);
  end;
end;

function TrtcListViewAdaptor.CopyFile(const ASrcUrl, ADestUrl: String): Boolean;
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
      Result:= FOnExtCopyUrl(Self, ASrcUrl, ADestUrl);
    end;
  end;
  if Result then begin
    if Assigned(FFolderTree) then begin
      n:= FFolderTree.Selected;
      if Assigned(n) then
        UpdateListView(n, True);
    end;
  end;
end;

function TrtcListViewAdaptor.GetSelectedItemDir: String;
var
  li: TListItem;
begin
  li:= FFilesView.Selected;
  if not Assigned(li) then Exit;
  if TNodeType(li.Data) = ntFolder then begin
    //
    SetDir(False, li.SubItems[4]);
  end;
end;

function TrtcListViewAdaptor.AddItemToListView(AFolderNode: TFolderNode;
  AFileInfo: TSHFileInfo; AAttrs: Cardinal): Boolean;
var
  li: TListItem;
  f: THandle;
  sz, highsz: DWORD;
  tcr, tac, tlw: TFileTime;
  st: TSystemTime;
  dt: TDateTime;
  fn: String;
begin
  Result:= True;
  if (Length(AFolderNode.FN_Path) = 0) or (not Assigned(FFilesView)) then
    Exit;
  case AFolderNode.FN_Type of
    ftFtpNode, ftDbNode, ftLdapNode, ftHTTPNode: begin
      dt:= 0;
      sz:= 0;
    end;
    else begin

//      f:= CreateFile(PChar(AFolderNode.FN_Path), GENERIC_READ, FILE_SHARE_READ, Nil, FILE_Flag_Op FILE_FLAG_BACKUP_SEMANTICS,
//        FILE_ATTRIBUTE_NORMAL, 0);
      fn:= AFolderNode.FN_Path;
      if (AFolderNode.FN_NodeType = ntFolder) and (not StrUtils.ANSIEndsText('.zip', AFolderNode.FN_Path))  then
        fn:= fn + '\';
//      uncomment this if local files must be without
//      if (AFolderNode.FN_NodeType = ntFolder) then
//        AFileInfo.iIcon:=  FDefaultFolderImageIndex
//      else
//        AFileInfo.iIcon:= FDefaultFileImageIndex;

      f:= CreateFile(PChar(fn), 0, FILE_SHARE_READ, Nil, OPEN_EXISTING,
        FILE_ATTRIBUTE_NORMAL or FILE_FLAG_BACKUP_SEMANTICS, 0);

      if f = INVALID_HANDLE_VALUE then begin
        dt:= 0;
      end else begin
        if GetFileTime(f, @tcr, @tac, @tlw) then begin
          if AFolderNode.FN_NodeType = ntFolder then begin
            sz:= 0;  // no size for folder
            if FileTimeToSystemTime(tcr, st) then // has no access time, show creation time
              dt:= SystemTimeToDateTime(st)
            else dt:= 0;
          end else begin
            sz:= GetFileSize(f, @highsz); // get file size
            if FileTimeToSystemTime(tlw, st) then // show last access time
              dt:= SystemTimeToDateTime(st)
            else dt:= 0;
          end;
        end else dt:= 0;
        CloseHandle(f);
      end;
    end;
  end;

  if FFilesView is TListView then begin
    li:= TListView(FFilesView).Items.Add;
  end;
  if FFilesView is TrtcFolderView then begin
    li:= TrtcFolderView(FFilesView).Items.Add;
  end;

  // file name
  li.Caption:= AFolderNode.FN_Text;
  // 0 size
  if AFolderNode.FN_NodeType = ntFolder then
    li.SubItems.Add('')
  else
    li.SubItems.Add(IntToStr(sz));
  // 1 type description
  li.SubItems.Add(AFileInfo.szTypeName);
  // 2 modified date
  li.SubItems.Add(DateTimeToStr(dt)); // sort by date stored in object pointer
  // 3 attrs
  li.SubItems.AddObject(GetFileAttributesString(AAttrs), TObject(AAttrs));
  // 4 path
  li.SubItems.Add(AFolderNode.FN_Path);
  // icon
  li.ImageIndex:= AFileInfo.iIcon;
  // ntFolder, ntFile
  li.Data:= Pointer(AFolderNode.FN_NodeType);
end;

// if ControlListView is assigned, UpdateListView
// updates list of files and folders in associated list view
function TrtcListViewAdaptor.UpdateListView(AFolderNode: TTreeNode; AForce: Boolean): Boolean;
begin
  Result:= False;
  if not Assigned(FFilesView) then
    Exit;
  if FFilesView is TListView then begin
    TListView(FFilesView).Items.BeginUpdate;
    TListView(FFilesView).Clear;
    EnumFilesInFolder(AFolderNode, AddItemToListView);
    TListView(FFilesView).Items.EndUpdate;
  end;
  if FFilesView is TrtcFolderView then begin
    if (not AForce) and (TrtcFolderView(FFilesView).Root = GetDir(True)) then Exit;
    TrtcFolderView(FFilesView).Root:= GetDir(True);
    TrtcFolderView(FFilesView).Items.BeginUpdate;
    TrtcFolderView(FFilesView).Clear;
    EnumFilesInFolder(AFolderNode, AddItemToListView);
    TrtcFolderView(FFilesView).Items.EndUpdate;
    TrtcFolderView(FFilesView).Sort;
  end;
  Result:= True;
end;

// GetPath returns the path name for True folders, and a null String
// for virtual folders.
function TrtcListViewAdaptor.GetPath(Node: TTreeNode): String;
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

// EnumFilesInFolder enumerate child folders to the node passed in Node.
// Returns processed nodes count
function TrtcListViewAdaptor.EnumFilesInFolder(var Node: TTreeNode; ACallBack: TEnumNodeFunc): Integer;
begin;
  // LockWindowUpdate(self.handle);
  Result:= 0;
  if (not Assigned(Node)) or (not Assigned(ACallBack))
  then Exit;
  case PFolderNode(Node.data).FN_Type of // ^
    ftFtpNode, ftDbNode, ftLdapNode, ftHTTPNode,
    ftFtpList, ftDbList, ftLdapList, ftHTTPList: begin

      Result:= EnumStorageFilesInFolder(PFolderNode(Node.data).FN_Type, Node);
    end;
    else begin
      Result:= EnumJFSFilesInFolder(Node, ACallBack); // ^
    end;
  end;
  Node.HasChildren:= Result > 0;
  //  LockWindowUpdate(0);
end;

// JFS
function TrtcListViewAdaptor.EnumJFSFilesInFolder(var Node: TTreeNode; ACallBack: TEnumNodeFunc): Integer;
var
  ParentNodeData: PFolderNode;
  path: String;
  EnumList: IEnumIDList;
  ParentFolder: IShellFolder;
  ItemIdList: PItemIdList;
  DummyResult: ULONG;
  Flags: Integer;
  foldernode: TFolderNode;
  fileinfo: TSHFileInfo;
  attrs: Cardinal;
begin;
  Result:= 0;
  if (not Assigned(ACallBack)) then
    Exit;
  if not Assigned(FFolderTree) then
    Exit; // treeview window handle is required
  if not (FFolderTree is TrtcFolderTree) then
    Exit; // treeview window handle is required

  path:= GetPath(Node);    // Get the complete path for this node.
  ParentNodeData:= Node.data;
  ParentFolder:= ParentNodeData.FN_ShellFolder; // ^
  if not Assigned(ParentFolder)
  then Exit;

  // If the node being expanded is a root, lop off the backslash.
  if length(path) <= 3
  then System.Delete(path, Length(path), 1);

  // Enumerate the folder's contents.
  Flags:= SHCONTF_FOLDERS + SHCONTF_INCLUDEHIDDEN + SHCONTF_NONFOLDERS;
  if (ParentFolder.EnumObjects(FFolderTree.parent.handle, Flags, EnumList) = NOERROR) then begin
    Result:= 0;
    EnumList.Reset;
    DummyResult:= 1;
    while (EnumList.Next(1, ItemIdList, DummyResult) = NOERROR) do begin
      TrtcFolderTree(FFolderTree).CreateJFSFolderListItem(ItemIdList, Node, False,
        foldernode, fileinfo, attrs);
      TrtcFolderTree(FFolderTree).FreePidl(ItemIdList);
      if not ACallBack(foldernode, fileinfo, attrs) then
        Break;
      Inc(Result);
    end;
{$IFNDEF DELPHI3UP}
    EnumList.Release;
{$ENDIF}
  end;
end;

// GetStorageFilesInFolder() call appropriate methods for nodes of JFS, FTP and other nodes
function TrtcListViewAdaptor.EnumStorageFilesInFolder(AFolderType: TFolderType; var Node: TTreeNode): Integer;
var
  FolderList: TStrings;
  i: Integer;
  foldernode: TFolderNode;
  fileinfo: TSHFileInfo;
  attrs: Cardinal;
begin
  Result:= 0;
  if (not Assigned(FOnExternalStorageFolderList)) then begin
    if Assigned(FOnExternalAsyncStorage) then begin
      if Assigned(FFolderTree) then begin
        TrtcFolderTree(FFolderTree).States:= TrtcFolderTree(FFolderTree).States + [fsWaitExpandDir];
      end;
      FOnExternalAsyncStorage(Self, PFolderNode(Node.Data).FN_Path);
    end;
    Result:= 1;
    Exit;
  end;

  FolderList:= TStringList.Create;
  FOnExternalStorageFolderList(Self, AFolderType, PFolderNode(Node.Data).FN_Path, FolderList); // ^
  for i:= 0 to FolderList.Count - 1 do begin
    CreateExtStorageListItem(AFolderType, Node, PFolderNode(Node.Data).FN_Path + '/' + FolderList[i],
      FolderList[i], TNodeType(FolderList.Objects[i]),
      foldernode, fileinfo, attrs); // ^
    Inc(Result);
  end;
  FolderList.Free;
end;

procedure TrtcListViewAdaptor.SetOnExternalAsyncStorage(AValue: TOnExtStorageAsyncFolderList);
begin
  FOnExternalAsyncStorage:= AValue;
end;

procedure TrtcListViewAdaptor.CreateExtStorageListItem(AFolderType: TFolderType; AParent: TTreeNode;
      const AURL, AName: String; ANodeType: TNodeType;
      var NewNodeData: TFolderNode; var FileInfo: TSHFileInfo; Attributes: Cardinal);
var
  // url address
  protocol, host, user, password, IPaddress, fn_baseDN, bookmark,
  attrs, scope, filter: String;
  port: Integer;
  p: Boolean;
begin
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

  if (ANodeType = ntFile) and
    ((not Assigned(FFolderTree)) or
    (TrtcFolderTree(FFolderTree).FileIndexOfMask(fn_baseDN) < 0))  // if it is a file and mask file does not allow to show
  then Exit;

  NewNodeData.FN_PidLen:= 0;    // ^
  NewNodeData.FN_PidList:= Nil; // ^
  NewNodeData.FN_Path:= AUrl;   // ^
  NewNodeData.FN_Text:= AName;  // ^
  NewNodeData.FN_Has_Parent:= (aParent <> Nil); // ^
  NewNodeData.FN_Type:= AFolderType; // ^
  NewNodeData.FN_NodeType:= ANodeType;   // ^ The calling routine may change this.
end;

// DirUpdateFiles called from TrtcPFileBrowser to update view when event is received
function TrtcListViewAdaptor.DirUpdateFiles(AFolderType: TFolderType;
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
  if Assigned(FFolderTree) then
    if FFolderTree is TrtcFolderTree then
      n:= TrtcFolderTree(FFolderTree).AttachStorageSite(ft, ADir, AShow);
  // clear list view if nide is detached before exit
  if (not AShow) and (TrtcFolderTree(FFolderTree).Selected = node) then
    FFilesView.Clear;

  if not Assigned(n) then begin
    if not AShow then begin
      // delete items in listview if exists
      li:= FindListItem(ADir);
      if Assigned(li) then
        li.Free;
    end;
    Exit;
  end;

  if Assigned(FFolderTree) then begin
    TrtcFolderTree(FFolderTree).States:= TrtcFolderTree(FFolderTree).States + [fsUpdatingListView];
    node:= TrtcFolderTree(FFolderTree).GoToDirectory(ADir, False);
    TrtcFolderTree(FFolderTree).States:= TrtcFolderTree(FFolderTree).States - [fsUpdatingListView];
    nofiles:= not (foFiles in TrtcFolderTree(FFolderTree).FolderOptions);
  end;

  if not Assigned(node) then begin
    // get root node
    node:= n;
  end;
  if Assigned(FFilesView) then begin
     FFilesView.Clear;
  end;

  if Assigned(AFiles) and Assigned(FFolderTree) then begin
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
        newnode:= TrtcFolderTree(FFolderTree).CreateExtStorageFolderNode(AFolderType, Node,
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

    if Assigned(FFilesView) and (TrtcFolderTree(FFolderTree).Selected = node) then begin
      if fsWaitExpandDir in TrtcFolderTree(FFolderTree).States then begin
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
          AFiles[i].FileInfo.iIcon:= TrtcFolderTree(FFolderTree).DefaultFileImageIndex
        else
          AFiles[i].FileInfo.iIcon:= TrtcFolderTree(FFolderTree).DefaultFolderImageIndex;

        // update linked list view if it is same directory
        if FFilesView is TListView then begin
          li:= TListView(FFilesView).Items.Add;
        end;
        if FFilesView is TrtcFolderView then begin
          li:= TrtcFolderView(FFilesView).Items.Add;
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
  if Assigned(FFolderTree) then begin
    if fsWaitExpandDir in TrtcFolderTree(FFolderTree).States then begin
      TrtcFolderTree(FFolderTree).States:= TrtcFolderTree(FFolderTree).States + [fsWaitExpandDir];
    end;
  end;

  // sort
  if FFilesView is TrtcFolderView then
    TrtcFolderView(FFilesView).Sort;

  Result:= True;
end;


end.
