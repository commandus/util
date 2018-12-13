unit
  RTCFolderView;
(*##*)
(*******************************************************************************
*                                                                             *
*   R  T  C  F  o  l  d  e  r  V  i  e  w                                      *
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
{
  @html(<b>)
  Folder List
  @html(</b>)
  @html(<br><br>)
  Introducing the @html(<b>) @Link(TrtcFolderView) @html(</b>) component:
  @html(<br>)
  Folder List component based on TCustomListView.
}

interface

uses
  Math, Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, CommCtrl, ShlObj, ActiveX, StdCtrls, ImgList,
  rtcShellHelper, rtcListViewAdaptor;

{
  @html(<b>)
  Folder View
  @html(</b>)
  @html(<br><br>)
  Introducing the @html(<b>) @Link(TrtcFolderView)@html(</b>) component:
  @html(<br>)
  Folder view component based on TCustomListView and is descendant of
  @Link(TrtcCustomFolderView).
  @Link(TrtcFolderView) is a companion component for @Link(TrtcFolderTree) assigned
  to the @Link(TrtcFolderTree.ControlListView) property. TrtcFolderTree.ControlListView
  can be assigned to TCustomListView descendant class such TListView.
  TrtcFolderView class designed for use with TrtcFolderView, it is
  not recommended to use TrtcFolderView alone. TrtcFolderTree is responsible
  to fill out TrtcFolderView list view content. There is two reason why TrtcFolderTree
  fill TrtcFolderView items:
  @html(<li>)you can assign TListView component or derived from TCustomListView class
  @html(<li>)TFolderTree already can functions to show files in the tree (see foFiles option in @Link(TrtcFolderTree.FolderOptions))
}

type
{ TrtcCustomFolderView }
  TrtcCustomFolderView = class(TCustomListView) // , IDropSource, IDropTarget
  private
    FSortColumn: Integer;
    FSortColumnAsc: Boolean;
    FRoot: String;
    FAutoContext,
    FSorted,
    FUpdating: Boolean;
    // system icons
    FLargeImages,
    FSmallImages: Integer;
    // FFolderTree: TrtcFolderTree;
    FFilesViewAdaptor: TrtcListViewAdaptor;

    FNotifier: TShellChangeNotifier;
    procedure SetSorted(const Value: Boolean);
    procedure SetFolderTree(AValue: TCustomTreeView);
    // drag'n'drop
    procedure FDragOverEvent(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure FDragDropEvent(Sender, Source: TObject; X, Y: Integer);
    {
    // IDropTarget
    function DragEnter(const dataObj: IDataObject; grfKeyState: Longint;
      pt: TPoint; var dwEffect: Longint): HResult; stdcall;
    function DragOver(grfKeyState: Longint; pt: TPoint; var dwEffect: Longint): HResult; stdcall;
    function DragLeave: HResult; stdcall;
    function Drop(const dataObj: IDataObject; grfKeyState: Longint; pt: TPoint;
      var dwEffect: Longint): HResult; stdcall;
    // IDropSource
    function QueryContinueDrag(AEscapePressed: Bool;  //Status of escape key since previous call
      AKeyState: LongInt): HResult; stdcall;     //Current state of keyboard modifier keys
    function GiveFeedback(AEffect: Longint): HResult; stdcall;
    // IUnknown
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    }
  protected
    procedure CreateWnd; override;
    procedure DblClick; override;
    procedure DoContextPopup(MousePos: TPoint; var Handled: Boolean); override;
    procedure EditText;
    procedure Edit(const Item: TLVItem); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure ColClick(Column: TListColumn); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    { @Abstract( Special folder can be storage of objects not a file so they can have other attributes than files has) }
    procedure RootChanged;
    procedure SetRoot(const AValue: String);
    procedure WndProc(var Message: TMessage); override;
    procedure SetViewStyle(Value: TViewStyle); override;
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Sort;
    property Items;
    property Columns;
    { @Abstract( show auto context menu) }
    property AutoContextMenus: Boolean read FAutoContext write FAutoContext default True;
    { @Abstract(If file system changes occurs, list view updated automatically if AutoRefresh = True) }
    property Root: String read FRoot write SetRoot;
    { @Abstract(FolderTree is assigned to @Link(TrtcFolderTree) call ) }
    // read FFolderTree
    property FolderTree: TCustomTreeView write SetFolderTree;
    { @Abstract(Indicates is list view sorted or not. Default True ) }
    property Sorted: Boolean read FSorted write SetSorted;
    { @Abstract(Indicates which column is used for sort.
      Columns are: 1- Name, 2- Size, 3- Type, 4- Modified(Created), 5- Attributes.
      Note that directories allways are sorted first then files.) }
    property SortColumn: Integer read FSortColumn write FSortColumn;
    { @Abstract(Indicates is column sorted in ascending (True) or descending order ) }
    property SortColumnAsc: Boolean read FSortColumnAsc write FSortColumnAsc;
  end;

  { TrtcFolderView }
  TrtcFolderView = class(TrtcCustomFolderView)
  private
    property OnDragDrop;
    property OnDragOver;
  published
    property AutoContextMenus;
    property Root;
    //property FolderTree;
    property Sorted;
    // property OnAddFolder;
    property Align;
    property Anchors;
    property BorderStyle;
    property Color;
    property ColumnClick;
    property OnClick;
    property OnDblClick;
    property Ctl3D;
    property DragMode;
    property ReadOnly;
    property Enabled;
    property Font;
    property GridLines;
    property HideSelection;
    property HotTrack;
    property IconOptions;
    property AllocBy;
    property MultiSelect;
    property RowSelect;
    property OnChange;
    property OnChanging;
    property OnColumnClick;
    property OnContextPopup;
    property OnEnter;
    property OnExit;
    property OnInsert;

    property DragCursor;
    property OnStartDrag;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property ParentColor default False;
    property ParentFont;
    property ParentShowHint;
    property ShowHint;
    property PopupMenu;
    property ShowColumnHeaders;
    property TabOrder;
    property TabStop default True;
    property Visible;
    property ViewStyle;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnEditing;
  end;

implementation

uses
  ShellConsts, ShellAPI, ComObj, TypInfo, Menus, Consts,
  rtcFolderTree;

{ TrtcCustomFolderView }
var
  CompareFolder: PFolderNode = nil;
  ICM2: IContextMenu2 = nil;

constructor TrtcCustomFolderView.Create(AOwner: TComponent);
var
  FileInfo: TSHFileInfo;
begin
  inherited Create(AOwner);
  // DragKind:= dkDrag;
  DragMode:= dmAutomatic;
  OnDragOver:= FDragOverEvent;
  OnDragDrop:= FDragDropEvent;
  {
  // Allow window to accept drop events
  if Owner is TWinControl then begin
    // DragAcceptFiles(TWinControl(AOwner).Handle, True);
    // Parent:= TWinControl(Owner);
    // DragAcceptFiles(Handle, True);
  end;
  OleInitialize(nil);
  // OleCheck(RegisterDragDrop(Self.Parent.Handle, (Self.Parent as IDropTarget)));
  OleCheck(RegisterDragDrop(Handle, (Self as IDropTarget)));
  }

  OwnerData:= False;
  FSorted:= True;
  FAutoContext:= True;
  // FFolderTree:= nil;
  FFilesViewAdaptor:= Nil;
  FUpdating:= False;
  FSmallImages:= SHGetFileInfo('C:\', { Do not localize }
    0, FileInfo, SizeOf(FileInfo), SHGFI_SYSICONINDEX or SHGFI_SMALLICON);
  FLargeImages:= SHGetFileInfo('C:\', { Do not localize }
    0, FileInfo, SizeOf(FileInfo), SHGFI_SYSICONINDEX or SHGFI_LARGEICON);
  FRoot:= '';
  FSortColumn:= 0; // no sort
  FSortColumnAsc:= True;
  HideSelection:= False;
  MultiSelect:= True;
end;

destructor TrtcCustomFolderView.Destroy;
begin
  {
  // Finished accepting drops
  RevokeDragDrop(Handle);
  OleUninitialize;
  }
  inherited Destroy;
end;

procedure TrtcCustomFolderView.CreateWnd;
begin
  inherited CreateWnd;
  if HandleAllocated then begin
    if FSmallImages <> 0 then
      SendMessage(Handle, LVM_SETIMAGELIST, LVSIL_SMALL, FSmallImages);
    if FLargeImages <> 0 then
      SendMessage(Handle, LVM_SETIMAGELIST, LVSIL_NORMAL, FLargeImages);
  end;
  RootChanged;
end;

procedure TrtcCustomFolderView.RootChanged;
var
  c: Integer;
begin
  if FUpdating then Exit;
  FUpdating := True;
  try
    if ViewStyle = vsReport then begin
      if (Columns.Count < 5) then begin
        Columns.Clear;
        Columns.Add.Caption:= 'Name';
        Columns.Add.Caption:= 'Size';;
        Columns.Add.Caption:= 'Type';
        Columns.Add.Caption:= 'Modified';
        Columns.Add.Caption:= 'Attributes';
  //      Columns.Add.Caption:= 'Folder';
        for c:= 0 to Columns.Count - 1 do
          Columns[c].Width:= -1;
      end;
    end;
  finally
    FUpdating := False;
  end;
end;

function ListSortFunc(Item1, Item2: TListItem; lParam: Integer): Integer; stdcall;
const
  R: array[TNodeType] of Integer = (1, 0);
begin
  Result:= 0;
  if (Item1 = nil) or (Item2 = nil) then begin
    if Item1 = Nil then
      if Item2 <> Nil then Result:= -1;
    if Item2 = Nil then
      if Item1 <> Nil then Result:= 1;
    Exit;
  end;

  Result:= R[GetListViewNodeType(Item2)] - R[GetListViewNodeType(Item1)];
  if Result <> 0 then
    Exit;

  case lParam of
    0, 1: begin // Name
      Result:= ANSICompareText(GetListViewNodeName(Item1), GetListViewNodeName(Item2));
    end;
    2: begin // Size
      Result:= GetListViewNodeSize(Item1) - GetListViewNodeSize(Item2);
    end;
    3: begin // Type
      Result:= ANSICompareText(GetListViewNodeTypeDesc(Item1), GetListViewNodeTypeDesc(Item2));
    end;
    4: begin // Modified
      Result:= Math.Sign(GetListViewNodeDate(Item1) - GetListViewNodeDate(Item2));
    end;
    5: begin // Attributes
      Result:= GetListViewNodeAttrs(Item1) - GetListViewNodeAttrs(Item2);
    end;

    -1: begin // Name
      Result:= ANSICompareText(GetListViewNodeName(Item2), GetListViewNodeName(Item1));
    end;
    -2: begin // Size
      Result:= GetListViewNodeSize(Item2) - GetListViewNodeSize(Item1);
    end;
    -3: begin // Type
      Result:= ANSICompareText(GetListViewNodeTypeDesc(Item2), GetListViewNodeTypeDesc(Item1));
    end;
    -4: begin // Modified
      Result:= Sign(GetListViewNodeDate(Item2) - GetListViewNodeDate(Item1));
    end;
    -5: begin // Attributes
      Result:= GetListViewNodeAttrs(Item2) - GetListViewNodeAttrs(Item1);
    end;
  end;
end;

procedure TrtcCustomFolderView.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) then begin
    { // remove folder tree component
    if (AComponent = FFolderTree) then
      FFolderTree:= nil
    }
  end;
end;

procedure TrtcCustomFolderView.ColClick(Column: TListColumn);
begin
  inherited;
  if (FSortColumn = Column.Index + 1) then
    FSortColumnAsc:= not FSortColumnAsc
  else
    FSortColumn:= Column.Index + 1;
  Sort;  
end;

procedure TrtcCustomFolderView.DblClick;
var
  li: TListItem;
begin
  if (Selected <> nil) and Assigned(FFilesViewAdaptor) then begin
    li:= Self.Selected;
    if not Assigned(li) then Exit;
    if (TNodeType(li.Data) = ntFolder) and (li.SubItems.Count > 4) then begin
      FFilesViewAdaptor.Dir[True]:= li.SubItems[4];
    end else begin
      ShellExecute(Handle, nil, PChar(li.SubItems[4]), nil,
        PChar(ExtractFilePath(li.SubItems[4])), 0);
    end;
  end;
  inherited DblClick;
end;

procedure TrtcCustomFolderView.EditText;
begin
  if Selected <> nil then
    ListView_EditLabel(Handle, Selected.Index);
end;

procedure TrtcCustomFolderView.Edit(const Item: TLVItem);
var
  S: string;
  url, dest: String;
  p: Integer;
begin
  with Item do begin
    if iItem >= Items.Count then Exit;
    if not Assigned(FFilesViewAdaptor) then
      Exit;
    if (pszText <> nil) then begin
      s:= pszText;
      url:= FFilesViewAdaptor.FileInLV(Item);
      dest:= url;
      p:= LastDelimiter('\/', dest);
      if p > 0 then begin
        System.Delete(dest, p + 1, MaxInt);
      end;
      FFilesViewAdaptor.MoveFile(url, dest + s);

      ListView_RedrawItems(Handle, iItem, iItem);
    end;
  end;
end;

procedure TrtcCustomFolderView.SetRoot(const AValue: String);
begin
  if not SameText(AValue, FRoot) then begin
    RootChanged;
    FRoot:= AValue;
  end;
end;

procedure TrtcCustomFolderView.Sort;
begin
//
  if FSortColumnAsc then
    CustomSort(@ListSortFunc, FSortColumn)
  else
    CustomSort(@ListSortFunc, - FSortColumn)
end;

procedure TrtcCustomFolderView.SetSorted(const Value: Boolean);
begin
  if FSorted <> Value then begin
    FSorted:= Value;
  end;
end;

procedure TrtcCustomFolderView.DoContextPopup(MousePos: TPoint;
  var Handled: Boolean);
begin
  if FAutoContext and (Selected <> nil) then begin
    // InvokeContextMenu(Self, IShellFolder(Selected.SubItems.Objects[5]), MousePos.X, MousePos.Y);
    Handled:= True;
  end else
    inherited;
end;

procedure TrtcCustomFolderView.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
    case Key of
      VK_RETURN:
        if ssAlt in Shift then
        begin
          // DoContextMenuVerb(SelectedFolder, cmvProperties);
          Key := 0;
        end
        else if (Selected <> nil) then
          if TNodeType(Selected.Data) = ntFolder then begin
            // SetPathFromID(SelectedFolder.AbsoluteID);
          end;
      VK_BACK: ;
      VK_F5: Refresh;
    end;
end;

{ ------------------------------ drag and drop ------------------------------- }
{ Source is origin control
  Sender is destination control
}
procedure TrtcCustomFolderView.FDragOverEvent(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
var
  DropItem: TListItem;
begin
  Accept:= False;
  if not ((Source is TCustomListView) or (Source is TCustomTreeView)) then begin
    Exit;
  end;
  if Sender is TCustomListView then begin // over listview, so this check unessesary
    with TCustomListView(Sender) do begin
      DropItem:= GetItemAt(X, Y);
      if Assigned(DropItem) then begin
        if TNodeType(DropItem.Data) = ntFolder then begin
          Accept:= True;
        end;
      end;
    end;
  end;
end;

procedure TrtcCustomFolderView.FDragDropEvent(Sender, Source: TObject; X, Y: Integer);
var
  i: Integer;
//  DragItem,
  DropItem: TListItem;
  srcUrl, desturl: String;
begin
  if Sender is TCustomListView then begin
    with TCustomListView(Sender) do begin
      DropItem:= GetItemAt(X, Y);
      if Assigned(DropItem) then begin
        if TNodeType(DropItem.Data) = ntFolder then begin
          // each dragged item move to Drop item
          if Assigned(FFilesViewAdaptor) then begin
            if (DropItem.SubItems.Count < 5) then
              Exit;
            desturl:= DropItem.SubItems[4];
            for i:= 0 to Items.Count - 1 do begin
              if Items[i].Selected then begin
                if (Items[i].SubItems.Count < 5) then
                  Continue;
                srcUrl:= Items[i].SubItems[4];
                FFilesViewAdaptor.CopyFile(srcUrl, desturl);
              end;
            end;
          end;
        end;
      end;
    end;
  end;
end;


procedure TrtcCustomFolderView.SetFolderTree(AValue: TCustomTreeView);
begin
//  if Value = FFolderTree then Exit;
//  FFolderTree:= Value;
  if Assigned(FFilesViewAdaptor) then
    FFilesViewAdaptor.Free;
  if Assigned(AValue) then begin
    if (AValue is TrtcFolderTree) then
      FFilesViewAdaptor:= TrtcFolderTree(AValue).FilesViewAdaptor;
      FFilesViewAdaptor.FilesView:= Self;
  end;
end;


procedure TrtcCustomFolderView.WndProc(var Message: TMessage);
begin
  //to handle submenus of context menus.
  with Message do begin
    if ((Msg = WM_INITMENUPOPUP) or (Msg = WM_DRAWITEM) or (Msg = WM_MENUCHAR)
      or (Msg = WM_MEASUREITEM)) and Assigned(ICM2) then begin
      ICM2.HandleMenuMsg(Msg, wParam, lParam);
      Result:= 0;
    end;
    if (Msg = WM_DROPFILES) then begin
       Result:= 0;
    end; 
  end;
  inherited;
end;

procedure TrtcCustomFolderView.SetViewStyle(Value: TViewStyle);
begin
  inherited SetViewStyle(Value);
  RootChanged;
end;

procedure TrtcCustomFolderView.Resize;
begin
  inherited Resize;
  case ViewStyle of
      vsIcon,
      vsSmallIcon:
        if IconOptions.Arrangement = iaTop then
          Arrange(arAlignTop)
        else
          Arrange(arAlignLeft);
  end;
end;

{------------------------------- IDropTarget --------------------------------}
{
function TrtcCustomFolderView.DragEnter(const dataObj: IDataObject; grfKeyState: Longint;
  pt: TPoint; var dwEffect: Longint): HResult;
begin
  dwEffect:= DROPEFFECT_COPY;
  Result:= S_OK;
end;

function TrtcCustomFolderView.DragOver(grfKeyState: Longint; pt: TPoint;
   var dwEffect: Longint): HResult;
begin
  dwEffect := DROPEFFECT_COPY;
  Result := S_OK;
end;

function TrtcCustomFolderView.DragLeave: HResult;
begin
  Result := S_OK;
end;

function TrtcCustomFolderView._AddRef: Integer;
begin
   Result:= 1;
end;

function TrtcCustomFolderView._Release: Integer;
begin
  Result := 1;
end;

function TrtcCustomFolderView.Drop(const dataObj: IDataObject;  grfKeyState: Longint;
  pt: TPoint;  var dwEffect: Longint): HResult;
var
  aFmtEtc: TFORMATETC;
  aStgMed: TSTGMEDIUM;
  pData: PChar;
begin
  // Make certain the data rendering is available
  if (dataObj = nil) then
    raise Exception.Create('IDataObject-Pointer is not valid!');
  with aFmtEtc do begin
    cfFormat := CF_TEXT;
    ptd := nil;
    dwAspect := DVASPECT_CONTENT;
    lindex := -1;
    tymed := TYMED_HGLOBAL;
  end;
  // Get the data
  OleCheck(dataObj.GetData(aFmtEtc, aStgMed));
  try
    // Lock the global memory handle to get a pointer to the data
    pData := GlobalLock(aStgMed.hGlobal);
    // Replace Text
    // Memo1.Text := pData;
  finally
    // Finished with the pointer
    GlobalUnlock(aStgMed.hGlobal);
    // Free the memory
    ReleaseStgMedium(aStgMed);
  end;
  Result:= S_OK;
end;

// Status of escape key since previous call
function TrtcCustomFolderView.QueryContinueDrag(AEscapePressed: Bool;
  AKeyState: LongInt): HResult;
begin
end;

// Current state of keyboard modifier keys
function TrtcCustomFolderView.GiveFeedback(AEffect: Longint): HResult; stdcall;
begin
end;
}

end.
