unit main;
////////////////////////////////////////////////////////////////////////////////
//                                                                            //
// Project:	TGIFImage demo application.                                   //
// Description:	GIF file viewer.                                              //
// Copyright	(c) 1997-99 Anders Melander.                                  //
//		All rights reserved.                                          //
// Formatting:	2 space indent, 8 space tabs, 80 columns.                     //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////
//                                                                            //
// The Delphi 2.x version of this application requires that the TSplitter     //
// component is installed.                                                    //
// TSplitter is defined and implemented in the splitter.pas unit located in   //
// the same directory as this file.                                           //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////
//                                                                            //
// Due to incompatibilities between Delphi 2's TImageList and later version's //
// TImageList, the treeview will not display any glyphs when this demo is run //
// with Delphi 2.  To fix this problem, you must load a new set of glyphs     //
// into the image list.                                                       //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////
interface

uses
{$IFNDEF BCB}
{$IFNDEF VER100}
  // C++ Builder/ Delphi 3 has TImageList in controls unit
  ImgList,
{$ENDIF}
{$ENDIF}
{$IFDEF VER90}
  // Substitute for Delphi 3/4 TSplitter
  Splitter,
{$ELSE}
  // No TOpenPictureDialog in Delphi 2.x
  ExtDlgs,
{$ENDIF}
  GIFImage,
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, ExtCtrls, Buttons, Grids, StdCtrls, Menus;

type
  TVisibleElement = (
    veStatus,		// Status bar info
    veProperties,	// Property panel
    veSplitter,		// Horizontal splitter
    vePreview,		// Preview panel
      vePainter,	// Animation viewer in preview panel
      veFrame,		// Single frame viewer in preview panel
      vePaletteView,	// Palette viewer in preview panel
      vePaletteInfo,	// Color viewer in preview panel
      veTextView);	// Text viewer in preview panel

  TVisibleElements = set of TVisibleElement;

  TFormExplorer = class(TForm)
    StatusBar: TStatusBar;
    ImageListNodes: TImageList;
    PanelInfo: TPanel;
    ListViewInfo: TListView;
    NotebookPreview: TNotebook;
    MemoPreview: TMemo;
    MainMenu1: TMainMenu;
    MenuFile: TMenuItem;
    MenuFileOpen: TMenuItem;
    MenuFileClose: TMenuItem;
    N1: TMenuItem;
    Exit1: TMenuItem;
    MenuView: TMenuItem;
    MenuViewProperties: TMenuItem;
    MenuViewPreview: TMenuItem;
    MenuFileOpenPreview: TMenuItem;
    N2: TMenuItem;
    MenuViewWarnings: TMenuItem;
    ScrollBoxPreviewAnimate: TScrollBox;
    ImagePreviewAnimate: TImage;
    MenuAnimation: TMenuItem;
    MenuAnimationThreaded: TMenuItem;
    MenuAnimationBuffered: TMenuItem;
    MenuAnimationStretch: TMenuItem;
    N3: TMenuItem;
    MenuAnimationAnimate: TMenuItem;
    MenuAnimationTransparent: TMenuItem;
    N4: TMenuItem;
    MenuAnimationDither: TMenuItem;
    MenuAnimationLoop: TMenuItem;
    PaintBoxBackground: TPaintBox;
    MenuFileBackground: TMenuItem;
    MenuAnimationBackground: TMenuItem;
    ScrollBoxPreview: TScrollBox;
    ImagePreview: TImage;
    PaintBoxPreview: TPaintBox;
    PanelTree: TPanel;
    TreeViewGIF: TTreeView;
    ProgressBar: TProgressBar;
    MenuAnimationAutoDither: TMenuItem;
    PanelPaletteInfo: TPanel;
    Label1: TLabel;
    LabelPaletteIndex: TLabel;
    LabelPaletteGreen: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    LabelPaletteRed: TLabel;
    Label2: TLabel;
    LabelPaletteBlue: TLabel;
    Label5: TLabel;
    LabelPaletteHex: TLabel;
    MenuAbout: TMenuItem;
    SplitterVertical: TSplitter;
    SplitterPreview: TSplitter;
    MenuAnimationClearOnLoop: TMenuItem;
    MenuAnimationTile: TMenuItem;
    ImageBackground: TImage;
    MenuAnimationSpeed: TMenuItem;
    MenuAnimationSpeed50: TMenuItem;
    MenuAnimationSpeed100: TMenuItem;
    MenuAnimationSpeed200: TMenuItem;
    MenuAnimationSpeed400: TMenuItem;
    MenuAnimationSpeed800: TMenuItem;
    MenuAnimationSpeedX: TMenuItem;
    MenuFileOpenClipboard: TMenuItem;
    PaintBoxPalette: TPaintBox;
    MenuFileSave: TMenuItem;
    N5: TMenuItem;
    MenuFileSaveAs: TMenuItem;
    MenuOptimize: TMenuItem;
    SaveDialog: TSaveDialog;
    MenuOptimizeAll: TMenuItem;
    N6: TMenuItem;
    MenuOptimizeCrop: TMenuItem;
    MenuOptimizeMerge: TMenuItem;
    MenuOptimizeCleanup: TMenuItem;
    MenuOptimizeGlobalColorMap: TMenuItem;
    PopupMenuTree: TPopupMenu;
    PopupMenuDelete: TMenuItem;
    PopupMenuClear: TMenuItem;
    ColorDialog: TColorDialog;
    MenuOptimizeColorMaps: TMenuItem;
    N7: TMenuItem;
    procedure FileOpenClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TreeViewGIFChange(Sender: TObject; Node: TTreeNode);
    procedure TreeViewGIFChanging(Sender: TObject; Node: TTreeNode;
      var AllowChange: Boolean);
    procedure AnimatedPreviewPaint(Sender: TObject);
    procedure NotebookPreviewPageChanged(Sender: TObject);
    procedure FileCloseClick(Sender: TObject);
    procedure FileExitClick(Sender: TObject);
    procedure MenuViewPropertiesClick(Sender: TObject);
    procedure MenuViewPreviewClick(Sender: TObject);
    procedure MenuFileOpenPreviewClick(Sender: TObject);
    procedure MenuViewWarningsClick(Sender: TObject);
    procedure MenuAnimationClick(Sender: TObject);
    procedure PaintBoxBackgroundPaint(Sender: TObject);
    procedure MenuFileBackgroundClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure PaintBoxPreviewPaint(Sender: TObject);
    procedure OnProgress(Sender: TObject; Stage: TProgressStage;
      PercentDone: Byte; RedrawNow: Boolean; const R: TRect; const Msg: string);
    procedure OnAnimate(Sender: TObject);
    procedure PaintBoxPreviewMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure MenuAboutClick(Sender: TObject);
    procedure MenuAnimationSpeedClick(Sender: TObject);
    procedure MenuAnimationSpeedXClick(Sender: TObject);
    procedure PaintBoxPalettePaint(Sender: TObject);
    procedure PaintBoxPaletteMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure MenuFileClick(Sender: TObject);
    procedure MenuFileOpenClipboardClick(Sender: TObject);
    procedure MemoPreviewChange(Sender: TObject);
    procedure MenuFileSaveClick(Sender: TObject);
    procedure MenuFileSaveAsClick(Sender: TObject);
    procedure MenuOptimizeAllClick(Sender: TObject);
    procedure MenuOptimizeCropClick(Sender: TObject);
    procedure MenuOptimizeMergeClick(Sender: TObject);
    procedure MenuOptimizeCleanupClick(Sender: TObject);
    procedure MenuOptimizeGlobalColorMapClick(Sender: TObject);
    procedure PopupMenuDeleteClick(Sender: TObject);
    procedure PopupMenuClearClick(Sender: TObject);
    procedure PopupMenuTreePopup(Sender: TObject);
    procedure PaintBoxPaletteDblClick(Sender: TObject);
    procedure MenuOptimizeColorMapsClick(Sender: TObject);
  private
    { Private declarations }
    GIF			: TGIFImage;
    Filename		: string;
    PaletteXOffset	,
    PaletteYOffset	: integer;
    PaletteRowCount	,
    PaletteColCount	: integer;
    PaletteCellHeight	,
    PaletteCellWidth	: integer;
    CurrentElements	: TVisibleElements;
    CurrentNode		: TObject;
    PropertyHeight	: integer;
    function GetElements(Node: TObject): TVisibleElements;
    procedure ParseFile(Filename: string);
    procedure ParseImage(RootNode: TTreeNode; Index: integer);
    procedure ParseExtension(RootNode: TTreeNode; Image: TGIFSubImage;
      Index: integer);
    procedure OnWarning(Sender: TObject; Severity: TGIFSeverity; Message: string);
    procedure UpdateOptions;
    procedure ResizePaletteGrid(ColorMap: TGIFColorMap);
    procedure UpdateSinglePreview;
    procedure DoOptimize(Options: TGIFOptimizeOptions;
      ColorReduction: TColorReduction; DitherMode: TDitherMode;
      ReductionBits: integer;
      MakeColorMap: boolean);
    procedure DoSaveFile(Name: string);
    function MouseToColor(X, Y: integer; ColorMap: TGIFColorMap): integer;
  public
    { Public declarations }
    procedure DoOpenFile(Name: string);
    procedure DoCloseFile;
  end;

var
  FormExplorer: TFormExplorer;

implementation

{$R *.DFM}

uses
  ClipBrd,
  Speed,
  About;

const
  GlyphFile		= 0;
  GlyphExtensionLoop	= 1;
  GlyphExtensionApp	= 2;
  GlyphExtensionUnknown	= 3;
  GlyphImage		= 4;
  GlyphExtensionComment	= 5;
  GlyphExtensionText	= 6;
  Glyphheader		= 7;
  GlyphPalette		= 8;
  GlyphExtensionControl	= 9;
  GlyphExtension	= 10;
  GlyphUnused1		= 11;

{$IFDEF VER90}
const
{$ELSE}
resourcestring
{$ENDIF}
  // GIF Block names
  sHeader		= 'Header';
  sGlobalPalette	= 'Global Palette';
  sLocalPalette		= 'Local Palette';
  sImage		= 'Image %d';
  sExtension		= 'Extensions';
  sExtensionUnknown	= 'Unknown extension';
  sExtensionApp		= 'Application Extension';
  sExtensionText	= 'Text extension';
  sExtensionControl	= 'Graphic Control Extension';
  sExtensionComment	= 'Comment Extension';
  sExtensionLoop	= 'Netscape Loop Extension';
  // Open dialog captions
  sOpenFile		= 'Select GIF File';
  sOpenBackground	= 'Select Background';
  // Save dialog captions
  sSaveFile		= 'Save GIF file';
  // ***FIXME***
  // GIF Block properties needs to be resourced


procedure TFormExplorer.FormCreate(Sender: TObject);
begin
  // The default options will be used by TOpenPictureDialog:
  // Do not use buffering - This is safe since we have complete
  // control over the TImage's canvas
  include(GIFImageDefaultDrawOptions, goDirectDraw);

{$IFNDEF VER90}
  // Enable stuff that isn't supported by Delphi 2.x
  MenuFileOpenPreview.Enabled := True;
{$ENDIF}

{$IFNDEF VER100}
  // Set special properties for D2/4 TSplitter
  SplitterPreview.Beveled := False;
  SplitterVertical.Beveled := False;
  SplitterPreview.ResizeStyle := rsUpdate;
  SplitterVertical.ResizeStyle := rsUpdate;
{$ENDIF}

  // Initialize open path to application path
  FileName := Application.ExeName;

  // Currently visible UI element
  CurrentElements := [veProperties, veSplitter, vePreview];
  // Currently selected GIF object
  CurrentNode := nil;
  // Remember property view size
  PropertyHeight := ListViewInfo.Height;

  GIF := TGIFImage.Create;
  try
    // Make the preview TImage contain a TGIFImage
    ImagePreviewAnimate.Picture.Graphic := GIF;
  finally
    GIF.Free;
  end;

  GIF := (ImagePreviewAnimate.Picture.Graphic as TGIFImage);

  // Set event handlers
  GIF.OnPaint := OnAnimate;
  GIF.OnEndPaint := OnAnimate;
  GIF.OnProgress := OnProgress;
  GIF.OnWarning := OnWarning;
  GIF.DrawBackgroundColor := clWindow;

  // Open file specified on command line if any
  if (ParamCount > 0) then
    DoOpenFile(ParamStr(1))
  else
    DoCloseFile;
end;

procedure TFormExplorer.FormDestroy(Sender: TObject);
begin
  FormExplorer := nil;
  DoCloseFile;
  GIF := nil;
end;

// Find the visual elements used by a given TGIFImage class
function TFormExplorer.GetElements(Node: TObject): TVisibleElements;
begin
  // The GIF painter/animation preview is used by:
  // * TGIFImage
  // NotebookPreview is used by:
  // * TGIFImage
  // * TGIFSubImage
  // * TGIFColorMap
  // * TGIFCommentExtension
  // * TGIFUnknownAppExtension
  // PanelPaletteInfo is used by:
  // * TGIFSubImage
  // * TGIFColorMap
  // The statusbar panel #1 is used by:
  // * TGIFImage
  Result := [];
  if (Node = nil) then
    exit;

  if (MenuViewProperties.Checked) then
    Include(Result, veProperties);
  if not(MenuViewPreview.Checked) then
    Exit;

  if (Node is TGIFImage) then
    Result := Result + [vePainter, veStatus, vePreview]
  else if (Node is TGIFSubImage) then
    Result := Result + [vePreview, veFrame, vePaletteInfo]
  else if (Node is TGIFColorMap) then
    Result := Result + [vePreview, vePaletteView, vePaletteInfo]
  else if (Node is TGIFCommentExtension) then
    Result := Result + [vePreview, veTextView]
  else if (Node is TGIFTextExtension) then
    Result := Result + [vePreview, veTextView]
  else if (Node is TGIFUnknownAppExtension) then
    Result := Result + [vePreview, veTextView];

  if (vePreview in Result) and (veProperties in Result) then
    include(Result, veSplitter);
end;

procedure TFormExplorer.DoOpenFile(Name: string);
begin
  Screen.Cursor := crHourGlass;
  try
    // Zap old info
    DoCloseFile;
    // Remember filename
    Filename := ExpandFileName(Name);
    // Load file into memory
    GIF.LoadFromFile(Filename);

    // Load explorer tree
    ParseFile(ExtractFileName(Filename));

    // Update menu
    MenuFileSave.Enabled := True;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TFormExplorer.DoSaveFile(Name: string);
begin
  // Remember filename
  Filename := ExpandFileName(Name);
  // Save GIF as file
  GIF.SaveToFile(Filename);

  // Update menu
  MenuFileSave.Enabled := True;
end;

procedure TFormExplorer.DoCloseFile;
begin
  Caption := 'GIF Explorer';

  // Zap old info
  TreeViewGIF.Selected := nil;
  TreeViewGIF.Items.Clear;
  ListViewInfo.Items.Clear;
  // Hide preview panel
  NotebookPreview.Hide;
  // Update menu
  MenuFileSave.Enabled := False;
  MenuFileSaveAs.Enabled := False;
  MenuOptimize.Enabled := False;
end;

procedure TFormExplorer.MenuFileClick(Sender: TObject);
begin
  // Only enable load from clipboard if clipboard contains a GIF
  MenuFileOpenClipboard.Enabled := ClipBoard.HasFormat(CF_GIF);
end;

procedure TFormExplorer.MenuFileOpenClipboardClick(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  try
    // Zap old info
    DoCloseFile;
    // Dummy filename
    Filename := '[Clipboard]';

    // Load GIF from clipboard
    ClipBoard.Open;
    try
      // ClipBoard.GetAsHandle is not nescessary here
      // since TGIFImage.LoadFromClipboardFormat will
      // do it automatically if the AData paremeter is 0
      GIF.LoadFromClipboardFormat(CF_GIF, ClipBoard.GetAsHandle(CF_GIF), 0);
    finally
      ClipBoard.Close;
    end;
    // Set event handlers
    GIF.OnPaint := OnAnimate;
    GIF.OnEndPaint := OnAnimate;
    GIF.OnProgress := OnProgress;
    GIF.OnWarning := OnWarning;

    // Load explorer tree
    ParseFile(Filename);

    // Update menu
    MenuFileSave.Enabled := False;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TFormExplorer.FileOpenClick(Sender: TObject);
begin
  with TOpenDialog.Create(self) do
  try
    // Start in last used directory
    InitialDir := ExtractFilePath(FileName);
    // Set default file extension to GIF
    DefaultExt := GraphicExtension(TGIFImage);
    // Only use GIF file filter
    Filter :=  GraphicFilter(TGIFImage);
    Title := sOpenFile;
    // Only allow existing files
    Options := [ofReadOnly, ofHideReadOnly, ofPathMustExist, ofFileMustExist];
    // Get the file
    if (Execute) then
      DoOpenFile(Filename);
  finally
    Free;
  end;
end;

procedure TFormExplorer.MenuFileSaveClick(Sender: TObject);
begin
  DoSaveFile(FileName);
end;

procedure TFormExplorer.MenuFileSaveAsClick(Sender: TObject);
begin
  // Start in last used directory
  SaveDialog.FileName := Filename;
  // Set default file extension to GIF
  SaveDialog.DefaultExt := GraphicExtension(TGIFImage);
  // Only use GIF file filter
  SaveDialog.Filter :=  GraphicFilter(TGIFImage);
  SaveDialog.Title := sSaveFile;
  // Get the file name
  if (SaveDialog.Execute) then
    DoSaveFile(SaveDialog.FileName);
end;

{$IFNDEF VER90}
type
  // Dummy class used to clear TImage used by TOpenPictureDialog
  TDummyOpenPictureDialog = class(TOpenPictureDialog)
  public
    procedure DoShowHide;
  end;

procedure TDummyOpenPictureDialog.DoShowHide;
begin
  inherited DoShow;
  inherited DoClose;
end;
{$ENDIF}

procedure TFormExplorer.MenuFileOpenPreviewClick(Sender: TObject);
{$IFNDEF VER90}
var
  OpenPictureDialog	: TOpenPictureDialog;
{$ENDIF}
begin
{$IFNDEF VER90}
  OpenPictureDialog := TOpenPictureDialog.Create(self);
  try
    // Start in last used directory
    OpenPictureDialog.InitialDir := ExtractFilePath(FileName);
    // Set default file extension to GIF
    OpenPictureDialog.DefaultExt := GraphicExtension(TGIFImage);
    // Only use GIF file filter
    OpenPictureDialog.Filter :=  GraphicFilter(TGIFImage);
    OpenPictureDialog.Title := sOpenFile;
    // Only allow existing files
    OpenPictureDialog.Options := [ofReadOnly, ofHideReadOnly, ofPathMustExist, ofFileMustExist];
    // Use buffering since we have no control over the preview canvas.
    exclude(GIFImageDefaultDrawOptions, goDirectDraw);
    try
      // Get the file
      if (OpenPictureDialog.Execute) then
        DoOpenFile(OpenPictureDialog.Filename);
      // Trick to kill thread owned by OpenPictureDialog...
      // ...Not really nescessary since we delete the dialog a few lines down
{$IFNDEF VER90}
      // Trick to kill thread owned by OpenPictureDialog
      TDummyOpenPictureDialog(OpenPictureDialog).DoShowHide;
{$ENDIF}
    finally
      include(GIFImageDefaultDrawOptions, goDirectDraw);
    end;
  finally
    OpenPictureDialog.Free;
  end;
{$ENDIF}
end;

procedure TFormExplorer.MenuFileBackgroundClick(Sender: TObject);
var
{$IFDEF VER90}
  OpenDialog		: TOpenDialog;
{$ELSE}
  OpenDialog		: TOpenPictureDialog;
{$ENDIF}
begin
{$IFDEF VER90}
  OpenDialog := TOpenDialog.Create(self);
{$ELSE}
  OpenDialog := TOpenPictureDialog.Create(self);
{$ENDIF}
  try
    // Set default file extension to GIF
    OpenDialog.DefaultExt := GraphicExtension(TGIFImage);
    // Use all supported file filters
    OpenDialog.Filter :=  GraphicFilter(TGraphic);
    OpenDialog.Title := sOpenBackground;
    // Use buffering since we have no control over the preview canvas.
    exclude(GIFImageDefaultDrawOptions, goDirectDraw);
    try
      // Get the file
      if (OpenDialog.Execute) then
      begin
        // Prevent background from animating
        exclude(GIFImageDefaultDrawOptions, goAnimate);
        // Tile background
        include(GIFImageDefaultDrawOptions, goTile);
        try
          ImageBackground.Picture.LoadFromFile(OpenDialog.Filename);
        finally
          // Restore defaults
          include(GIFImageDefaultDrawOptions, goAnimate);
          exclude(GIFImageDefaultDrawOptions, goTile);
        end;
        if (ImagePreviewAnimate.Picture.Graphic <> nil) then
        begin
          // Stop the painter to get rid of the old background buffer
          TGIFImage(ImagePreviewAnimate.Picture.Graphic).StopDraw;
          // Redraw
          ImagePreviewAnimate.Invalidate;
        end;
      end;
{$IFNDEF VER90}
      // Trick to kill thread owned by OpenPictureDialog
      TDummyOpenPictureDialog(OpenDialog).DoShowHide;
{$ENDIF}
    finally
      include(GIFImageDefaultDrawOptions, goDirectDraw);
    end;
  finally
    OpenDialog.Free;
  end;
end;

procedure TFormExplorer.FileCloseClick(Sender: TObject);
begin
  DoCloseFile;
end;

procedure TFormExplorer.FileExitClick(Sender: TObject);
begin
  Close;
end;

procedure TFormExplorer.MenuViewPropertiesClick(Sender: TObject);
begin
  MenuViewProperties.Checked := not(MenuViewProperties.Checked);
  ListViewInfo.Visible := MenuViewProperties.Checked;
  // Update properties
  TreeViewGIFChange(TreeViewGIF, TreeViewGIF.Selected);
end;

procedure TFormExplorer.MenuViewPreviewClick(Sender: TObject);
begin
  MenuViewPreview.Checked := not(MenuViewPreview.Checked);
  if not(MenuViewPreview.Checked) then
    NotebookPreview.Hide
  else
    // Update properties
    TreeViewGIFChange(TreeViewGIF, TreeViewGIF.Selected);
end;

procedure TFormExplorer.MenuViewWarningsClick(Sender: TObject);
begin
  MenuViewWarnings.Checked := not(MenuViewWarnings.Checked);
end;

// Add GIF-image to tree view
procedure TFormExplorer.ParseFile(Filename: string);
var
  Node			,
  RootNode		: TTreeNode;
  i			: integer;
begin
  TreeViewGIF.Items.BeginUpdate;
  try
    // Add root-node for GIF file
    RootNode := TreeViewGIF.Items.AddObjectFirst(nil, Filename, GIF);
    RootNode.ImageIndex := GlyphFile;
    RootNode.SelectedIndex := GlyphFile;
    // Add node for header
    Node := TreeViewGIF.Items.AddChildObjectFirst(RootNode, sHeader, GIF.Header);
    Node.ImageIndex := GlyphHeader;
    Node.SelectedIndex := GlyphHeader;
    // Add node for global palette
    if (GIF.GlobalColorMap.Count > 0) then
    begin
      Node := TreeViewGIF.Items.AddChildObject(RootNode, sGlobalPalette, GIF.GlobalColorMap);
      Node.ImageIndex := GlyphPalette;
      Node.SelectedIndex := GlyphPalette;
    end;

    // Add nodes for sub-images
    for i := 0 to GIF.Images.Count-1 do
      ParseImage(RootNode, i);

    // Expand root node
    RootNode.Expand(False);
  finally
    TreeViewGIF.Items.EndUpdate;
  end;

  Caption := 'GIF Explorer: '+Filename;

  // Update menu
  MenuFileSaveAs.Enabled := True;
  MenuOptimize.Enabled := True;
end;

// Add sub-image to treeview
procedure TFormExplorer.ParseImage(RootNode: TTreeNode; Index: integer);
var
  Node			,
  SubNode		: TTreeNode;
  i			: integer;
begin
  // Add node for sub-image
  Node := TreeViewGIF.Items.AddChildObject(RootNode, Format(sImage, [Index]), GIF.Images[Index]);
  Node.ImageIndex := GlyphImage;
  Node.SelectedIndex := GlyphImage;

  // Add node for local palette
  if (GIF.Images[Index].ColorMap.Count > 0) then
  begin
    SubNode := TreeViewGIF.Items.AddChildObject(Node, sLocalPalette, GIF.Images[Index].ColorMap);
    SubNode.ImageIndex := GlyphPalette;
    SubNode.SelectedIndex := GlyphPalette;
  end;

  // Add node for extensions
  if (GIF.Images[Index].Extensions.Count > 0) then
  begin
    // Uncomment the following lines to add a container node for extensions
    (*
    Node := TreeViewGIF.Items.AddChildObject(Node, sExtension, nil);
    Node.ImageIndex := GlyphExtension;
    Node.SelectedIndex := GlyphExtension;
    *)

    for i := 0 to GIF.Images[Index].Extensions.Count-1 do
      ParseExtension(Node, GIF.Images[Index], i);
  end;
end;

// Add extension to treeview
procedure TFormExplorer.ParseExtension(RootNode: TTreeNode; Image: TGIFSubImage;
  Index: integer);
var
  Node			: TTreeNode;
  Name			: string;
  Glyph			: integer;
begin
  Glyph := GlyphExtensionUnknown;
  Name := sExtensionUnknown;
  // Determine extension type
  if (Image.Extensions[Index] is TGIFGraphicControlExtension) then
  begin
    Glyph := GlyphExtensionControl;
    Name := sExtensionControl;
  end else
  if (Image.Extensions[Index] is TGIFTextExtension) then
  begin
    Glyph := GlyphExtensionText;
    Name := sExtensionText;
  end else
  if (Image.Extensions[Index] is TGIFCommentExtension) then
  begin
    Glyph := GlyphExtensionComment;
    Name := sExtensionComment;
  end else
  if (Image.Extensions[Index] is TGIFAppExtNSLoop) then
  begin
    Glyph := GlyphExtensionLoop;
    Name := sExtensionLoop;
  end else
  if (Image.Extensions[Index] is TGIFApplicationExtension) then
  begin
    Glyph := GlyphExtensionApp;
    Name := sExtensionApp;
  end else
  if (Image.Extensions[Index] is TGIFUnknownAppExtension) then
  begin
  end;

  // Add node for extension
  Node := TreeViewGIF.Items.AddChildObject(RootNode, Name, Image.Extensions[Index]);
  Node.ImageIndex := Glyph;
  Node.SelectedIndex := Glyph;
end;

// Synchronize property panel with selected node in treeview
procedure TFormExplorer.TreeViewGIFChange(Sender: TObject;
  Node: TTreeNode);
var
  p			: PChar;
  Size			: integer;
  sHex, sText		: string;
  i, j			: integer;
  TempList		: TStringList;
  OldFocus		: TWinControl;
const
  VisibleChars: set of char =
    ['a'..'z','A'..'Z', '0'..'9', ' '..'@'];
  GIFVersion: array[boolean] of string =
    ('gif87a', 'gif89a');
  GIFDisposals: array[TDisposalMethod] of string =
    ('None specified', 'Do not dispose', 'Background', 'Previuos');

  function AddStringProperty(Item: string; Value: string): TListItem;
  begin
    Result := ListViewInfo.Items.Add;
    Result.Caption := Item;
    Result.SubItems.Add(Value);
  end;

  function AddFormattedProperty(Item, sFormat: string; Values: array of const): TListItem;
  begin
    Result := ListViewInfo.Items.Add;
    Result.Caption := Item;
    Result.SubItems.Add(format(sFormat, Values));
  end;

  function AddIntegerProperty(Item: string; Value: integer): TListItem;
  begin
    Result := ListViewInfo.Items.Add;
    Result.Caption := Item;
    Result.SubItems.Add(IntToStr(Value));
  end;

  function AddBooleanProperty(Item: string; Value: boolean): TListItem;
  begin
    Result := ListViewInfo.Items.Add;
    Result.Caption := Item;
    if (Value) then
      Result.SubItems.Add('Yes')
    else
      Result.SubItems.Add('No');
  end;

begin
  ListViewInfo.Items.BeginUpdate;
  try
    ListViewInfo.Items.Clear;
    if (Node = nil) or (Node.Data = nil) then
    begin
      CurrentNode := nil;
      CurrentElements := []
    end else
    begin
      CurrentNode := TObject(Node.Data);
      CurrentElements := GetElements(CurrentNode);
    end;
    // Ignore empty nodes
    if (CurrentElements = []) or (CurrentNode = nil) then
    begin
{$IFNDEF VER90}
      ListViewInfo.GridLines := False;
{$ENDIF}
      exit;
    end;

    (*
    ** TGIFImage
    *)
    if (CurrentNode is TGIFImage) then
    begin
      // Display PaintBox to make GIF paint itself
      if (vePainter in CurrentElements) then
      begin
        NotebookPreview.ActivePage := 'PagePreviewFile';
        AnimatedPreviewPaint(self);
      end;
      if (veProperties in CurrentElements) then
      begin
        AddStringProperty('Filename', ExtractFilename(Filename));
        AddIntegerProperty('Frames', TGIFImage(CurrentNode).Images.Count);
        AddBooleanProperty('Global palette', (TGIFImage(CurrentNode).GlobalColorMap.Count > 0));
        if (TGIFImage(CurrentNode).ColorResolution > 0) then
          AddFormattedProperty('Color resolution', '%d colors', [2 SHL TGIFImage(CurrentNode).ColorResolution]);
        if (TGIFImage(CurrentNode).BitsPerPixel > 0) then
          AddFormattedProperty('Bits per pixel', '%d bits', [TGIFImage(CurrentNode).BitsPerPixel]);
      end;
    end else
    (*
    ** TGIFSubImage
    *)
    if (CurrentNode is TGIFSubImage) then
    begin
      // Display single sub-image as TBitmap
      if (TGIFSubImage(CurrentNode).Empty) then
        Exclude(CurrentElements, vePreview);
      if (vePreview in CurrentElements) then
      begin
        PaintBoxPreview.Width := TGIFSubImage(CurrentNode).Image.Width + 8;
        PaintBoxPreview.Height := TGIFSubImage(CurrentNode).Image.Height + 8;
        NotebookPreview.ActivePage := 'PagePreviewImage';
      end;
      if (veProperties in CurrentElements) then
      begin
        AddIntegerProperty('Left', TGIFSubImage(CurrentNode).Left);
        AddIntegerProperty('Top', TGIFSubImage(CurrentNode).Top);
        AddIntegerProperty('Width', TGIFSubImage(CurrentNode).Width);
        AddIntegerProperty('Height', TGIFSubImage(CurrentNode).Height);
        AddBooleanProperty('Interlaced', TGIFSubImage(CurrentNode).Interlaced);
        AddIntegerProperty('Extensions', TGIFSubImage(CurrentNode).Extensions.Count);
        AddBooleanProperty('Local palette', (TGIFSubImage(CurrentNode).ColorMap.Count > 0));
        if (TGIFSubImage(CurrentNode).ColorResolution > 0) then
          AddFormattedProperty('Color resolution', '%d colors', [2 SHL TGIFSubImage(CurrentNode).ColorResolution]);
        if (TGIFSubImage(CurrentNode).BitsPerPixel > 0) then
          AddFormattedProperty('Bits per pixel', '%d bits', [TGIFSubImage(CurrentNode).BitsPerPixel]);
      end;
    end else
    (*
    ** TGIFColorMap
    *)
    if (CurrentNode is TGIFColorMap) then
    begin
      if (vePreview in CurrentElements) then
      begin
        NotebookPreview.ActivePage := 'PagePreviewPalette';
      end;
      if (veProperties in CurrentElements) then
      begin
        AddIntegerProperty('Count', TGIFColorMap(CurrentNode).Count);
        AddBooleanProperty('Optimized', TGIFColorMap(CurrentNode).Optimized);
        AddFormattedProperty('Bits per pixel', '%d bits', [TGIFColorMap(CurrentNode).BitsPerPixel]);
      end;
    end else
    (*
    ** TGIFHeader
    *)
    if (CurrentNode is TGIFHeader) then
    begin
      if (veProperties in CurrentElements) then
      begin
        AddStringProperty('Version', GIFVersion[(TGIFHeader(CurrentNode).Version = gv87a)]);
        AddIntegerProperty('Width', TGIFHeader(CurrentNode).Width);
        AddIntegerProperty('Height', TGIFHeader(CurrentNode).Height);
        AddIntegerProperty('Background color', TGIFHeader(CurrentNode).BackGroundColorIndex);
        AddIntegerProperty('Aspect ratio', TGIFHeader(CurrentNode).AspectRatio);
        if (TGIFHeader(CurrentNode).ColorResolution > 0) then
          AddFormattedProperty('Color resolution', '%d colors', [2 SHL TGIFHeader(CurrentNode).ColorResolution]);
        if (TGIFHeader(CurrentNode).BitsPerPixel > 0) then
          AddFormattedProperty('Bits per pixel', '%d bits', [TGIFHeader(CurrentNode).BitsPerPixel]);
      end;
    end else
    (*
    ** TGIFCommentExtension
    *)
    if (CurrentNode is TGIFCommentExtension) then
    begin
      if (vePreview in CurrentElements) then
      begin
        NotebookPreview.ActivePage := 'PagePreviewText';
        MemoPreview.Lines.Assign(TGIFCommentExtension(CurrentNode).Text);
        MemoPreview.Tag := integer(CurrentNode);
      end;
    end else
    (*
    ** TGIFTextExtension
    *)
    if (CurrentNode is TGIFTextExtension) then
    begin
      if (veProperties in CurrentElements) then
      begin
        AddIntegerProperty('Left', TGIFTextExtension(CurrentNode).Left);
        AddIntegerProperty('Top', TGIFTextExtension(CurrentNode).Top);
        AddIntegerProperty('Grid width', TGIFTextExtension(CurrentNode).GridWidth);
        AddIntegerProperty('Grid height', TGIFTextExtension(CurrentNode).GridHeight);
        AddIntegerProperty('Char width', TGIFTextExtension(CurrentNode).CharWidth);
        AddIntegerProperty('Char height', TGIFTextExtension(CurrentNode).CharHeight);
        AddIntegerProperty('Background color', TGIFTextExtension(CurrentNode).BackgroundColorIndex);
        AddIntegerProperty('Foreground color', TGIFTextExtension(CurrentNode).ForegroundColorIndex);
      end;
      if (vePreview in CurrentElements) then
      begin
        NotebookPreview.ActivePage := 'PagePreviewText';
        MemoPreview.Lines.Assign(TGIFTextExtension(CurrentNode).Text);
        MemoPreview.Tag := integer(CurrentNode);
      end;
    end else
    (*
    ** TGIFGraphicControlExtension
    *)
    if (CurrentNode is TGIFGraphicControlExtension) then
    begin
      if (veProperties in CurrentElements) then
      begin
        AddFormattedProperty('Delay', '%d mS', [TGIFGraphicControlExtension(CurrentNode).Delay*10]);
        AddBooleanProperty('Transparent', TGIFGraphicControlExtension(CurrentNode).Transparent);
        AddIntegerProperty('Transparent color', TGIFGraphicControlExtension(CurrentNode).TransparentColorIndex);
        AddBooleanProperty('Prompt', TGIFGraphicControlExtension(CurrentNode).UserInput);
        AddStringProperty('Disposal', GIFDisposals[TGIFGraphicControlExtension(CurrentNode).Disposal]);
      end;
    end else
    (*
    ** TGIFAppExtNSLoop
    *)
    if (CurrentNode is TGIFAppExtNSLoop) then
    begin
      if (veProperties in CurrentElements) then
      begin
        AddIntegerProperty('Loops', TGIFAppExtNSLoop(CurrentNode).Loops);
      end;
    end else
    (*
    ** TGIFAppExtNSLoop
    *)
    if (CurrentNode is TGIFUnknownAppExtension) then
    begin
      if (vePreview in CurrentElements) then
      begin
        NotebookPreview.ActivePage := 'PagePreviewText';
        TempList := TStringList.Create;
        try
          for i := 0 to TGIFUnknownAppExtension(CurrentNode).Blocks.Count-1 do
          begin
            sText := ' ';
            sHex := '';
            j := 0;
            p := PChar(TGIFBlock(TGIFUnknownAppExtension(CurrentNode).Blocks[i]).Data);
            Size := TGIFBlock(TGIFUnknownAppExtension(CurrentNode).Blocks[i]).Size;

            TempList.Add(format('Block: %.2d   Size: %.3d bytes', [i, Size]));
            TempList.Add('-----------------------------------------------------------------');

            while (Size > 0) do
            begin
              sHex := sHex + IntToHex(ord(p^), 2)+' ';
              if (p^ in VisibleChars) then
                sText := sText+p^
              else
                sText := sText+'.';
              inc(j);
              inc(p);
              dec(Size);

              if (j MOD 16 = 0) then
              begin
                TempList.Add(sHex+sText);
                sText := ' ';
                sHex := '';
              end;
            end;

            if (j MOD 16 <> 0) then
            begin
              while (j MOD 16 <> 0) do
              begin
                sHex := sHex + '   ';
                inc(j);
              end;
              TempList.Add(sHex+sText);
            end;

            TempList.Add('');
          end;
          MemoPreview.Lines.Assign(TempList);
        finally
          TempList.Free;
        end;
      end;
      if (veProperties in CurrentElements) then
      begin
        AddStringProperty('Identifier', TGIFUnknownAppExtension(CurrentNode).Identifier);
        AddStringProperty('Authentication', TGIFUnknownAppExtension(CurrentNode).Authentication);
        AddIntegerProperty('Data blocks', TGIFUnknownAppExtension(CurrentNode).Blocks.Count);
      end;
    end;

    NotebookPreview.Visible := (vePreview in CurrentElements);
    PanelPaletteInfo.Visible := (vePaletteInfo in CurrentElements);

    if (vePaletteView in CurrentElements) then
    begin
      // Workaround to force PaintBox to update
      PaintBoxPalette.Hide;
      PaintBoxPalette.Show;
      ResizePaletteGrid(TGIFColorMap(CurrentNode));
    end;

    if (veFrame in CurrentElements) then
    begin
      // Workaround to force PaintBox to update
      PaintBoxPreview.Hide;
      PaintBoxPreview.Show;
      UpdateSinglePreview;
    end;

    if (vePainter in CurrentElements) and (FormAnimationSpeed <> nil) then
    begin
      OldFocus := Screen.ActiveControl;
      FormAnimationSpeed.Show;
      if (OldFocus <> nil) then
        OldFocus.SetFocus;
    end;

    if (veSplitter in CurrentElements) then
    begin
      SplitterPreview.Top := ListViewInfo.Top + 1;
      SplitterPreview.Show;
    end;
{$IFNDEF VER90}
    ListViewInfo.GridLines := (veProperties in CurrentElements);
{$ENDIF}
  finally
    ListViewInfo.Items.EndUpdate;
  end;
end;

// Called when the user selects a new node in the tree
procedure TFormExplorer.TreeViewGIFChanging(Sender: TObject;
  Node: TTreeNode; var AllowChange: Boolean);
var
  OldElements		,
  NewElements		,
  RemovedElements	: TVisibleElements;
begin
  AllowChange := True;
  // Nothing to hide if current selection is empty
  if (CurrentNode = nil) then
    exit;
  // Nothing to hide if current selection is same type as new selection
  if (Node <> nil) and (TObject(Node.Data).ClassType = CurrentNode.ClassType) then
    exit;

  OldElements := CurrentElements;
  if (Node = nil) then
    NewElements := []
  else
    NewElements := GetElements(TObject(Node.Data));
  RemovedElements := OldElements - NewElements;

  // Zap paint threads
  if (vePainter in CurrentElements) then
  begin
    if (ImagePreviewAnimate.Picture.Graphic <> nil) then
      TGIFImage(ImagePreviewAnimate.Picture.Graphic).StopDraw;
    if (ImageBackground.Picture.Graphic <> nil) then
      TGIFImage(ImageBackground.Picture.Graphic).StopDraw;
  end;


  // Hide animation speed pop-up
  if (vePainter in RemovedElements) and (FormAnimationSpeed <> nil) then
    FormAnimationSpeed.Hide;
  // Clear status bar
  if (veStatus in RemovedElements) then
    StatusBar.Panels[1].Text := '';
  // Hide previews
  if (vePreview in RemovedElements) then
    NotebookPreview.Hide;
  // Hide palette/color info
  if (vePaletteInfo in RemovedElements) then
    PanelPaletteInfo.Hide;
  // Hide splitter
  if (veSplitter in RemovedElements) then
    SplitterPreview.Hide;

  if (veTextView in OldElements) then
    MemoPreview.Tag := 0;

  if ([veProperties, veSplitter, vePreview] * NewElements = [veProperties]) then
  begin
    if ([veProperties, veSplitter, vePreview] * OldElements <> [veProperties]) then
      PropertyHeight := ListViewInfo.Height;
    ListViewInfo.Align := alClient;
  end else
  begin
    ListViewInfo.Align := alTop;
    ListViewInfo.Height := PropertyHeight;
  end;

  CurrentElements := NewElements;
end;

// Repaint when any paint options change
procedure TFormExplorer.MenuAnimationClick(Sender: TObject);
begin
  TMenuItem(Sender).Checked := not(TMenuItem(Sender).Checked);
  UpdateOptions;
end;

procedure TFormExplorer.MenuAnimationSpeedClick(Sender: TObject);
begin
  TMenuItem(Sender).Checked := True;
  GIFImageDefaultAnimationSpeed := TMenuItem(Sender).Tag;
  if (FormAnimationSpeed <> nil) then
    FormAnimationSpeed.Value := GIFImageDefaultAnimationSpeed;
  GIF.AnimationSpeed := GIFImageDefaultAnimationSpeed;
end;

procedure TFormExplorer.MenuAnimationSpeedXClick(Sender: TObject);
var
  p			: TPoint;
begin
  TMenuItem(Sender).Checked := True;
  if (FormAnimationSpeed = nil) then
  begin
    FormAnimationSpeed := TFormAnimationSpeed.Create(self);
    p.x := ClientWidth - FormAnimationSpeed.Width;
    p.y := 0;
    p := ClientToScreen(p);
    FormAnimationSpeed.Top := p.y;
    FormAnimationSpeed.Left := p.x;
  end;
  FormAnimationSpeed.Show;
end;

// Update paint to reflect change in options
procedure TFormExplorer.UpdateOptions;
var
  DrawOptions		: TGIFDrawOptions;
begin
  MenuAnimationLoop.Enabled := MenuAnimationAnimate.Checked;
  MenuAnimationAutoDither.Enabled := MenuAnimationDither.Checked;
  MenuAnimationBuffered.Enabled := MenuAnimationThreaded.Checked;
  MenuAnimationTile.Enabled := not(MenuAnimationStretch.Checked);
  MenuAnimationStretch.Enabled := not(MenuAnimationTile.Checked);
  MenuAnimationTile.Checked := (MenuAnimationTile.Enabled and MenuAnimationTile.Checked);
  MenuAnimationStretch.Checked := (MenuAnimationStretch.Enabled and MenuAnimationStretch.Checked);

  if (ImagePreviewAnimate.Picture.Graphic = nil) or
    not(ImagePreviewAnimate.Picture.Graphic is TGIFImage) then
    exit;

  ImagePreviewAnimate.Stretch := (MenuAnimationStretch.Checked or MenuAnimationTile.Checked);
{$IFNDEF VER90}
  ImagePreviewAnimate.Transparent := MenuAnimationTransparent.Checked;
{$ENDIF}

  if (ImageBackground.Picture.Graphic <> nil) then
  begin
    if (MenuAnimationBackground.Checked) then
    begin
      // Tile background, but do not animate
      TGIFImage(ImageBackground.Picture.Graphic).DrawOptions :=
        TGIFImage(ImageBackground.Picture.Graphic).DrawOptions + [goTile] - [goAnimate];
      // Display background
      ImageBackground.Show;
    end else
    begin
      // Hide background
      ImageBackground.Hide;
      // Stop paint thread -
      // It shouldn't be running, but let's make sure it isn't
      GIF.StopDraw;
    end;
  end;
//  PaintBoxBackground.Visible := MenuAnimationBackground.Checked;

  if (MenuAnimationStretch.Checked) then
    ImagePreviewAnimate.Align := alClient
  else
    FormResize(self);

  // Zap old painter
  GIF.StopDraw;

  // Default paint options
  DrawOptions := [
    goLoop		// Loop animations
  ];

  if (MenuAnimationAnimate.Checked) then
    Include(DrawOptions, goAnimate);

  if (MenuAnimationTransparent.Checked) then
    Include(DrawOptions, goTransparent);

  if (MenuAnimationDither.Checked) then
    Include(DrawOptions, goDither);

  if (MenuAnimationAutoDither.Checked) then
    Include(DrawOptions, goAutoDither);

  if (MenuAnimationLoop.Checked) then
    Include(DrawOptions, goLoopContinously);

  if (MenuAnimationClearOnLoop.Checked) then
    Include(DrawOptions, goClearOnLoop);

  if (MenuAnimationThreaded.Checked) then
    Include(DrawOptions, goAsync);

  if (not MenuAnimationThreaded.Checked) or (not MenuAnimationBuffered.Checked) then
    Include(DrawOptions, goDirectDraw);

  if (MenuAnimationTile.Checked) then
    Include(DrawOptions, goTile);

  // Set new options
  GIF.DrawOptions := DrawOptions;

  // Force redraw
  ImagePreviewAnimate.Invalidate;
end;

// Realign controls on form resize
procedure TFormExplorer.FormResize(Sender: TObject);
begin
  if (veProperties in CurrentElements) then
    PropertyHeight := ListViewInfo.Height;

  if (veFrame in CurrentElements) then
  begin
    UpdateSinglePreview;
  end else

  if (vePaletteView in CurrentElements) then
  begin
    ResizePaletteGrid(TGIFColorMap(CurrentNode));
  end else

  if (vePainter in CurrentElements) then
  begin
    if (ImagePreviewAnimate.Picture.Graphic <> nil) and
      not(MenuAnimationStretch.Checked) then
    begin
      if (ScrollBoxPreviewAnimate.ClientHeight < GIF.Height) or
        (ScrollBoxPreviewAnimate.ClientWidth < GIF.Width) then
      begin
        if (ImagePreviewAnimate.Align <> alNone) then
          ImagePreviewAnimate.Align := alNone;
        if (ScrollBoxPreviewAnimate.ClientHeight < GIF.Height) then
          ImagePreviewAnimate.Height := GIF.Height
        else
          ImagePreviewAnimate.Height := ScrollBoxPreviewAnimate.ClientHeight;
        if (ScrollBoxPreviewAnimate.ClientWidth < GIF.Width) then
          ImagePreviewAnimate.Width := GIF.Width
        else
          ImagePreviewAnimate.Width := ScrollBoxPreviewAnimate.ClientWidth;
      end else
        if (ImagePreviewAnimate.Align <> alClient) then
          ImagePreviewAnimate.Align := alClient;
      // Stop paint thread and invalidate to force background buffer to refresh
      GIF.StopDraw;
      ImagePreviewAnimate.Invalidate;
    end;
  end;
end;

// Async paint of GIF-file
procedure TFormExplorer.AnimatedPreviewPaint(Sender: TObject);
begin
  UpdateOptions;
end;

// Paint background tiles
procedure TFormExplorer.PaintBoxBackgroundPaint(Sender: TObject);
var
  x, y			: integer;
begin
  for x := 0 to (TPaintBox(Sender).Width DIV ImageBackground.Width) do
    for y := 0 to (TPaintBox(Sender).Height DIV ImageBackground.Height) do
      TPaintBox(Sender).Canvas.Draw(x*ImageBackground.Width,
        y*ImageBackground.Height, ImageBackground.Picture.Graphic);
end;

// Preview notebook is changing page
procedure TFormExplorer.NotebookPreviewPageChanged(Sender: TObject);
begin
  {}
end;

procedure TFormExplorer.ResizePaletteGrid(ColorMap: TGIFColorMap);
begin
  if (ColorMap = nil) or not(TObject(ColorMap) is TGIFColorMap) then
    exit;
  // Display palette grid
  PaintBoxPalette.Tag := longInt(ColorMap);
  // Calculate grid dimensions
  PaletteRowCount := trunc(Sqrt(ColorMap.Count));
  PaletteColCount := (ColorMap.Count + PaletteRowCount - 1) DIV PaletteRowCount;
  // Calculate grid cell size
  PaletteCellHeight := PaintBoxPalette.Height DIV PaletteRowCount;
  PaletteCellWidth := PaintBoxPalette.Width DIV PaletteColCount;
  // Center grid
  PaletteXOffset := (PaintBoxPalette.Width - (PaletteColCount * PaletteCellWidth)) DIV 2;
  PaletteYOffset := (PaintBoxPalette.Height - (PaletteRowCount * PaletteCellHeight)) DIV 2;
end;

// Draw the palette grid
procedure TFormExplorer.PaintBoxPalettePaint(Sender: TObject);
var
  ColorIndex		,
  FirstIndex		: integer;
  MaxColor		: integer;
  r			: TRect;
  Frame			: TRect;
  x, y			: integer;
  OldPalette		,
  NewPalette		: HPALETTE;
begin
  // Should never happen - Just a precaution
  if (TPaintBox(Sender).Tag = 0) then
    raise Exception.Create('Internal error: TPaintBox.Tag = 0');

  MaxColor := TGIFColorMap(TPaintBox(Sender).Tag).Count;

  r := TPaintBox(Sender).Canvas.ClipRect;

  // Make sure that cells are large enough to display
  if (PaletteCellHeight < 2) or (PaletteCellWidth < 2) then
  begin
    TPaintBox(Sender).Canvas.Brush.Color := TPaintBox(Sender).Color;
    TPaintBox(Sender).Canvas.FillRect(r);
    exit;
  end;

  // Calculate start row and col
  y := ((r.Top - PaletteYOffset) DIV PaletteCellHeight);
  if (y < 0) then
    y := 0;
  r.Left := ((r.Left - PaletteXOffset) DIV PaletteCellWidth);
  if (r.Left < 0) then
    r.Left := 0;

  // Limit size
  if (r.Right > PaletteColCount * PaletteCellWidth + PaletteXOffset) then
    r.Right := PaletteColCount * PaletteCellWidth + PaletteXOffset;
  if (r.Bottom > PaletteRowCount * PaletteCellHeight + PaletteYOffset) then
    r.Bottom := PaletteRowCount * PaletteCellHeight + PaletteYOffset;

  // Calculate starting color index for this row
  FirstIndex := y * PaletteColCount + r.Left;

  // Convert row and col to coordinate
  y := y * PaletteCellHeight + PaletteYOffset;
  r.Left := r.Left * PaletteCellWidth + PaletteXOffset;

  NewPalette := TGIFColorMap(TPaintBox(Sender).Tag).ExportPalette;
  try
    OldPalette := SelectPalette(TPaintBox(Sender).Canvas.Handle, NewPalette, False);
    try
      RealizePalette(TPaintBox(Sender).Canvas.Handle);
      TPaintBox(Sender).Canvas.Brush.Style := bsSolid;

      while (y < r.Bottom) do
      begin
        x := r.Left;
        ColorIndex := FirstIndex;
        while (x < r.Right) do
        begin
          Frame.Left := x;
          Frame.Right := x + PaletteCellWidth;
          Frame.Top := y;
          Frame.Bottom := y + PaletteCellHeight;
          if (ColorIndex >= MaxColor) then
          begin
            // Draw empty cells special
            TPaintBox(Sender).Canvas.Brush.Color := TPaintBox(Sender).Color;
          end else
          begin
            if (PaletteCellHeight > 6) and (PaletteCellWidth > 3) then
              DrawEdge(TPaintBox(Sender).Canvas.Handle, Frame, BDR_SUNKENINNER, BF_ADJUST OR BF_RECT);
            TPaintBox(Sender).Canvas.Brush.Color := ($01000000 OR ColorIndex);
          end;
          TPaintBox(Sender).Canvas.FillRect(Frame);
          inc(ColorIndex);
          inc(x, PaletteCellWidth);
        end;
        inc(FirstIndex, PaletteColCount);
        inc(y, PaletteCellHeight);
      end;
    finally
      if (OldPalette <> 0) then
        SelectPalette(TPaintBox(Sender).Canvas.Handle, OldPalette, False);
    end;
  finally
    if (NewPalette <> 0) then
      DeleteObject(NewPalette);
  end;
end;

function TFormExplorer.MouseToColor(X, Y: integer; ColorMap: TGIFColorMap): integer;
var
  Row, Col		: integer;
begin
  if (PaletteCellHeight > 0) and (PaletteCellWidth > 0) and
    (X >= PaletteXOffset) and (Y >= PaletteYOffset) and
    (X <= PaletteXOffset + PaletteColCount * PaletteCellWidth) and
    (Y <= PaletteYOffset + PaletteRowCount * PaletteCellHeight) then
  begin
    // Convert pixel coordinates to cell coordinates
    Row := ((Y - PaletteYOffset) DIV PaletteCellHeight);
    Col := ((X - PaletteXOffset) DIV PaletteCellWidth);

    Result := Col + Row * PaletteColCount;
    if (Result < 0) or (Result >= ColorMap.Count) then
      Result := -1;
  end else
    Result := -1;
end;

// Modify a color
procedure TFormExplorer.PaintBoxPaletteDblClick(Sender: TObject);
var
  p			: TPoint;
  ColorIndex		: integer;
  i			: integer;
  Color			: TColor;
begin
  GetCursorPos(p);
  p := TPaintBox(Sender).ScreenToClient(p);

  ColorIndex := MouseToColor(p.X, p.Y, TGIFColorMap(TPaintBox(Sender).Tag));

  if (ColorIndex < 0) then
    exit;

  Color := TGIFColorMap(TPaintBox(Sender).Tag).Colors[ColorIndex];
  ColorDialog.CustomColors.Values['ColorA'] := IntToHex(Color, 6);
  ColorDialog.Color := Color;
  if (ColorDialog.Execute) then
  begin
    TGIFColorMap(TPaintBox(Sender).Tag).Colors[ColorIndex] := ColorDialog.Color;
    TPaintBox(Sender).Invalidate;
    // Zap the bitmaps of frames that uses this color map
    for i := 0 to GIF.Images.Count-1 do
      if (GIF.Images[i].ActiveColorMap = TGIFColorMap(TPaintBox(Sender).Tag)) then
        GIF.Images[i].HasBitmap := False;
  end;
end;

procedure TFormExplorer.PaintBoxPaletteMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  ColorIndex		: integer;
  Color			: TColor;
begin
  ColorIndex := MouseToColor(X, Y, TGIFColorMap(TPaintBox(Sender).Tag));

  if (ColorIndex < 0) then
  begin
    LabelPaletteIndex.Caption := '';
    LabelPaletteRed.Caption := '';
    LabelPaletteGreen.Caption := '';
    LabelPaletteBlue.Caption := '';
    LabelPaletteHex.Caption := '';
    exit;
  end;

  LabelPaletteIndex.Caption := IntToStr(ColorIndex);
  Color := TGIFColorMap(TPaintBox(Sender).Tag).Colors[ColorIndex];
  LabelPaletteRed.Caption := IntToStr(Color AND $000000FF);
  LabelPaletteGreen.Caption := IntToStr((Color AND $0000FF00) SHR 8);
  LabelPaletteBlue.Caption := IntToStr((Color AND $00FF0000) SHR 16);
  // Convert to HTML format
  Color := ((Color AND $000000FF) SHL 16) or
    (Color AND $0000FF00) or
    ((Color AND $00FF0000) SHR 16);
  LabelPaletteHex.Caption := IntToHex(Color, 6);

end;

// Show info about pixel color under cursor
procedure TFormExplorer.PaintBoxPreviewMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  ColorIndex		: integer;
  Color			: TColor;
begin
  if (CurrentNode = nil) or not(CurrentNode is TGIFSubImage) then
  begin
    X := -1;
    Y := -1;
  end else
  begin
    X := X - TGIFSubImage(CurrentNode).Left - 4;
    Y := Y - TGIFSubImage(CurrentNode).Top - 4;
  end;

  if (X < 0) or (X >= TGIFSubImage(CurrentNode).Width) or
    (Y < 0) or (Y >= TGIFSubImage(CurrentNode).Height) then
  begin
    LabelPaletteIndex.Caption := '';
    LabelPaletteRed.Caption := '';
    LabelPaletteGreen.Caption := '';
    LabelPaletteBlue.Caption := '';
    LabelPaletteHex.Caption := '';
    exit;
  end;

  ColorIndex := TGIFSubImage(CurrentNode).Pixels[X, Y];

  LabelPaletteIndex.Caption := IntToStr(ColorIndex);
  Color := TGIFSubImage(CurrentNode).ActiveColorMap.Colors[ColorIndex];
  LabelPaletteRed.Caption := IntToStr(Color AND $000000FF);
  LabelPaletteGreen.Caption := IntToStr((Color AND $0000FF00) SHR 8);
  LabelPaletteBlue.Caption := IntToStr((Color AND $00FF0000) SHR 16);
  // Convert to HTML format
  Color := ((Color AND $000000FF) SHL 16) or
    (Color AND $0000FF00) or
    ((Color AND $00FF0000) SHR 16);
  LabelPaletteHex.Caption := IntToHex(Color, 6);
end;

// GIF Warning event handler
// Called when the GIF component encounters a non-fatal error
procedure TFormExplorer.OnWarning(Sender: TObject; Severity: TGIFSeverity; Message: string);
const
  SevStr		: array[TGIFSeverity] of string
  			= ('Info', 'Warning', 'Error');
var
  Action		: word;
begin
  if not(MenuViewWarnings.Checked) then
    exit;
  Action := MessageDlg(
    'GIF Warning:'+#13+
    'Source:   '+Sender.ClassName+#13+
    'Severity: '+SevStr[Severity]+#13+
    'Message:  '+Message, mtWarning, [mbAbort, mbIgnore, mbAll], 0);
  if (Action = mrAll) then
    // Ignore all further warnings
    MenuViewWarnings.Checked := False
  else
    if (Action <> mrIgnore) then
      // mrAbort (and unknowns) raise exception to abort whatever we where doing
      raise Exception.Create('GIF Aborted');
end;

procedure TFormExplorer.UpdateSinglePreview;
begin
  if (CurrentNode = nil) or not(CurrentNode is TGIFSubImage) then
    exit;
  if (PaintBoxPreview.Width < ScrollBoxPreview.Width) then
    PaintBoxPreview.Left := (ScrollBoxPreview.Width - PaintBoxPreview.Width) DIV 2
  else
    PaintBoxPreview.Left := 0;
  if (PaintBoxPreview.Height < ScrollBoxPreview.Height) then
    PaintBoxPreview.Top := (ScrollBoxPreview.Height - PaintBoxPreview.Height) DIV 2
  else
    PaintBoxPreview.Top := 0;
end;

// Display bounding rects of a single frame
procedure TFormExplorer.PaintBoxPreviewPaint(Sender: TObject);
var
  ImageRect		,
  FrameRect		: TRect;
begin
  if (CurrentNode = nil) or not(CurrentNode is TGIFSubImage) then
    exit;

  // Clear canvas
  TPaintBox(Sender).Canvas.FillRect(TPaintBox(Sender).Canvas.ClipRect);

  ImageRect := TPaintBox(Sender).ClientRect;
  InflateRect(ImageRect, -4, -4);

  // Position frame
  FrameRect := TGIFSubImage(CurrentNode).BoundsRect;
  OffsetRect(FrameRect, ImageRect.Left, ImageRect.Top);

  // Draw frame
  TPaintBox(Sender).Canvas.Draw(FrameRect.Left, FrameRect.Top,
    TGIFSubImage(CurrentNode).Bitmap);

  // Draw image border
  InflateRect(ImageRect, 1, 1);
  TPaintBox(Sender).Canvas.DrawFocusRect(ImageRect);

  // Draw frame border
  InflateRect(FrameRect, 1, 1);
  if (not EqualRect(ImageRect, FrameRect)) then
    TPaintBox(Sender).Canvas.DrawFocusRect(FrameRect);
end;

// Update progress bar (on load and render)
procedure TFormExplorer.OnProgress(Sender: TObject; Stage: TProgressStage;
  PercentDone: Byte; RedrawNow: Boolean; const R: TRect; const Msg: string);
begin
  if (Stage = psEnding) then
  begin
    ProgressBar.Position := 0;
    StatusBar.Panels[0].Text := '';
  end else
  begin
    ProgressBar.Position := PercentDone;
    StatusBar.Panels[0].Text := Msg;
    ProgressBar.Update;
    StatusBar.Update;
  end;
end;

// Update status bar (on animate)
procedure TFormExplorer.OnAnimate(Sender: TObject);
begin
  if not(Sender is TGIFPainter) then
    exit;
  if (TGIFPainter(Sender).ActiveImage < 0) then
  begin
    StatusBar.Panels[1].Text := '';
  end else
  begin
    StatusBar.Panels[1].Text := ' Frame '+IntToStr(TGIFPainter(Sender).ActiveImage+1) +
      ' of '+IntToStr(TGIFPainter(Sender).Image.Images.Count);
    StatusBar.Update;
  end;
end;

// Display About box
procedure TFormExplorer.MenuAboutClick(Sender: TObject);
begin
  with TFormAbout.Create(self) do
    try
      ShowModal;
    finally
      Free;
    end;
end;

// Edit text preview
procedure TFormExplorer.MemoPreviewChange(Sender: TObject);
begin
   if (not(veTextView in CurrentElements)) or (TMemo(Sender).Tag = 0) then
     exit;
   if (TObject(TMemo(Sender).Tag) is TGIFCommentExtension) then
     TGIFCommentExtension(CurrentNode).Text.Assign(MemoPreview.Lines)
   else
   if (TObject(TMemo(Sender).Tag) is TGIFTextExtension) then
     TGIFTextExtension(CurrentNode).Text.Assign(MemoPreview.Lines)
end;

// Optimize GIF
procedure TFormExplorer.MenuOptimizeAllClick(Sender: TObject);
begin
  DoOptimize([ooCrop, ooMerge, ooCleanup, ooColorMap, ooReduceColors], rmNone, dmNearest, 8, False);
end;

procedure TFormExplorer.MenuOptimizeCropClick(Sender: TObject);
begin
  DoOptimize([ooCrop], rmNone, dmNearest, 8, False);
end;

procedure TFormExplorer.MenuOptimizeMergeClick(Sender: TObject);
begin
  DoOptimize([ooMerge], rmNone, dmNearest, 8, False);
end;

procedure TFormExplorer.MenuOptimizeCleanupClick(Sender: TObject);
begin
  DoOptimize([ooCleanup], rmNone, dmNearest, 8, False);
end;

procedure TFormExplorer.MenuOptimizeColorMapsClick(Sender: TObject);
begin
  DoOptimize([ooColorMap], rmNone, dmNearest, 8, False);
end;

procedure TFormExplorer.MenuOptimizeGlobalColorMapClick(Sender: TObject);
begin
  DoOptimize([], rmNone, dmNearest, 8, True);
end;

procedure TFormExplorer.DoOptimize(Options: TGIFOptimizeOptions;
  ColorReduction: TColorReduction; DitherMode: TDitherMode;
  ReductionBits: integer; MakeColorMap: boolean);
var
  SaveSave			: boolean;
begin
  if (GIF = nil) then
    exit;
  if (Options <> []) then
    GIF.Optimize(Options, ColorReduction, DitherMode, ReductionBits);
  if (MakeColorMap) then
    GIF.OptimizeColorMap;

  SaveSave := MenuFileSave.Enabled;

  DoCloseFile;
  ParseFile(ExtractFileName(Filename));

  MenuFileSave.Enabled := SaveSave;
end;

procedure TFormExplorer.PopupMenuTreePopup(Sender: TObject);
begin
  if (TreeViewGIF.Selected = nil) or (TreeViewGIF.Selected.Data = nil) then
  begin
    PopupMenuDelete.Enabled := False;
    PopupMenuClear.Enabled := False;
  end else
  begin
    PopupMenuDelete.Enabled := (TObject(TreeViewGIF.Selected.Data) is TGIFItem);
    PopupMenuClear.Enabled := (TObject(TreeViewGIF.Selected.Data) is TGIFColorMap);
  end;
end;

procedure TFormExplorer.PopupMenuDeleteClick(Sender: TObject);
begin
  if (TreeViewGIF.Selected = nil) or (TreeViewGIF.Selected.Data = nil) or
    not(TObject(TreeViewGIF.Selected.Data) is TGIFItem) then
    exit;
  TGIFItem(TreeViewGIF.Selected.Data).Free;
  TreeViewGIF.Selected.Free;
end;

procedure TFormExplorer.PopupMenuClearClick(Sender: TObject);
var
  i			: integer;
begin
  if (TreeViewGIF.Selected = nil) or (TreeViewGIF.Selected.Data = nil) or
    not(TObject(TreeViewGIF.Selected.Data) is TGIFColorMap) then
    exit;
  TGIFColorMap(TreeViewGIF.Selected.Data).Clear;
  TreeViewGIF.Selected.Free;
  // Zap the bitmaps of frames that used this color map
  for i := 0 to GIF.Images.Count-1 do
    if (GIF.Images[i].ActiveColorMap = TGIFColorMap(TreeViewGIF.Selected.Data)) then
      GIF.Images[i].HasBitmap := False;
end;

end.

