unit main;
////////////////////////////////////////////////////////////////////////////////
//                                                                            //
// Project:	TGIFImage demo application.                                   //
// Description:	Convert images to GIF format.                                 //
// Copyright	(c) 1997-99 Anders Melander.                                  //
//		All rights reserved.                                          //
// Formatting:	2 space indent, 8 space tabs, 80 columns.                     //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////
//                                                                            //
// Please note that when converting a TJPEGImage to a TGIFImage in 256 color  //
// display mode, TJPEGImage will automatically quantize the image to 256      //
// colors. This means that TGIFImage's color reduction facilities are not     //
// used, since they only apply to images with more than 256 colors.           //
// Likewise, the color reduction functions can only be tested on images with  //
// more than 256 colors.                                                      //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////
interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, Menus;

type
  TFormConvert = class(TForm)
    MainMenu: TMainMenu;
    MenuFile: TMenuItem;
    MenuFileOpen: TMenuItem;
    MenuFileSaveAs: TMenuItem;
    MenuFileSep: TMenuItem;
    MenuFileExit: TMenuItem;
    ImageView: TImage;
    SaveDialog: TSaveDialog;
    MenuView: TMenuItem;
    MenuViewRefresh: TMenuItem;
    MenuViewStretch: TMenuItem;
    MenuOptions: TMenuItem;
    MenuOptionsCompression: TMenuItem;
    MenuOptionsColorReduction: TMenuItem;
    MenuOptionsDithering: TMenuItem;
    MenuOptionsCompressionLZW: TMenuItem;
    MenuOptionsCompressionRLE: TMenuItem;
    MenuOptionsColorReductionNone: TMenuItem;
    MenuOptionsColorReductionOptimized: TMenuItem;
    MenuOptionsColorDitheringFloydSteinberg: TMenuItem;
    MenuOptionsColorDitheringNearest: TMenuItem;
    MenuOptionsColorReductionNetscape: TMenuItem;
    MenuEdit: TMenuItem;
    MenuEditPaste: TMenuItem;
    MenuEditCopy: TMenuItem;
    MenuEditCut: TMenuItem;
    MenuOptionsColorReductionWindows20: TMenuItem;
    MenuOptionsColorReductionWindows256: TMenuItem;
    MenuOptionsColorReductionGrayScale: TMenuItem;
    MenuOptionsColorReductionWindowsGray: TMenuItem;
    MenuOptionsColorReductionMonochrome: TMenuItem;
    MenuOptionsColorReductionOptimizedWindows: TMenuItem;
    MenuOptionsColorReductionOptimized16: TMenuItem;
    MenuOptionsColorReductionOptimized32: TMenuItem;
    MenuOptionsColorReductionOptimized64: TMenuItem;
    MenuOptionsColorReductionOptimized128: TMenuItem;
    MenuOptionsColorReductionOptimized256: TMenuItem;
    MenuOptionsColorReductionOptimized8: TMenuItem;
    procedure MenuFileOpenClick(Sender: TObject);
    procedure MenuViewRefreshClick(Sender: TObject);
    procedure MenuViewStretchClick(Sender: TObject);
    procedure MenuFileSaveAsClick(Sender: TObject);
    procedure MenuFileExitClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure MenuOptionsClick(Sender: TObject);
    procedure MenuEditCutClick(Sender: TObject);
    procedure MenuEditCopyClick(Sender: TObject);
    procedure MenuEditPasteClick(Sender: TObject);
    procedure MenuEditClick(Sender: TObject);
    procedure MenuOptionsColorReductionWindows256Click(Sender: TObject);
    procedure MenuOptionsColorReductionOptimizedBitsClick(Sender: TObject);
  private
    { Private declarations }
    OpenPath: string;
    procedure DoOpenFile(Filename: string);
  public
    { Public declarations }
  end;

var
  FormConvert: TFormConvert;

implementation

{$R *.DFM}

uses
  ClipBrd,
{$IFNDEF VER90}
{$IFNDEF BCB}
  // Sorry guys - No jpeg in Delphi 2 & C++ Builder (AFAIK)
  jpeg,
{$ENDIF}
  // No TOpenPictureDialog in Delphi 2
  ExtDlgs,
{$ENDIF}
  gifimage;

procedure TFormConvert.FormCreate(Sender: TObject);
begin
  // Do not use buffering.
  // This is safe since we have complete control over the TImage's canvas
  include(GIFImageDefaultDrawOptions, goDirectDraw);
end;

procedure TFormConvert.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  ImageView.Picture.Graphic := nil;
end;

procedure TFormConvert.DoOpenFile(Filename: string);
begin
  ImageView.Picture.LoadFromFile(Filename);
  // Remember path of the file for later save
  OpenPath := FileName;

  // Set default "save as" filename
  SaveDialog.FileName := ChangeFileExt(ExtractFilename(Filename), '.'+GraphicExtension(TGIFImage));
end;

procedure TFormConvert.MenuViewRefreshClick(Sender: TObject);
begin
  ImageView.Repaint;
end;

procedure TFormConvert.MenuViewStretchClick(Sender: TObject);
begin
  TMenuItem(Sender).Checked := not(TMenuItem(Sender).Checked);
  ImageView.Stretch := TMenuItem(Sender).Checked;
end;

procedure TFormConvert.MenuFileOpenClick(Sender: TObject);
var
{$IFDEF VER90}
  OpenDialog		: TOpenDialog;
{$ELSE}
  OpenDialog		: TOpenPictureDialog;
{$ENDIF}
begin
{$IFDEF VER90}
  // Delphi 2.x doesn't support TOpenPictureDialog
  OpenDialog := TOpenDialog.Create(self);
{$ELSE}
  OpenDialog := TOpenPictureDialog.Create(self);
{$ENDIF}
  try
    // Use buffering since we have no control over the preview canvas.
    exclude(GIFImageDefaultDrawOptions, goDirectDraw);
    // Set filter to all supported image formats
    OpenDialog.Filter := GraphicFilter(TGraphic);
    OpenDialog.InitialDir := ExtractFilePath(OpenPath);

    if (OpenDialog.Execute) then
      DoOpenFile(OpenDialog.Filename);
  finally
    OpenDialog.Free;
    include(GIFImageDefaultDrawOptions, goDirectDraw);
  end;
end;

procedure TFormConvert.MenuFileSaveAsClick(Sender: TObject);
var
  GIF			: TGIFImage;
begin
  // Set filter to GIF only
  SaveDialog.Filter := GraphicFilter(TGIFImage);
  SaveDialog.DefaultExt := GraphicExtension(TGIFImage);
  SaveDialog.InitialDir := ExtractFilePath(OpenPath);

  if not(SaveDialog.Execute) then
    exit;

  Screen.Cursor := crHourGlass;
  try
    GIF := TGifImage.Create;
    try
      // Set import color reduction options
      if (MenuOptionsColorReductionNetscape.Checked) then
        GIF.ColorReduction := rmNetscape
      else if (MenuOptionsColorReductionMonochrome.Checked) then
        GIF.ColorReduction := rmMonochrome
      else if (MenuOptionsColorReductionGrayScale.Checked) then
        GIF.ColorReduction := rmGrayScale
      else if (MenuOptionsColorReductionWindowsGray.Checked) then
        GIF.ColorReduction := rmWindowsGray
      else if (MenuOptionsColorReductionWindows20.Checked) then
        GIF.ColorReduction := rmWindows20
      else if (MenuOptionsColorReductionWindows256.Checked) then
        GIF.ColorReduction := rmWindows256
      else if (MenuOptionsColorReductionOptimized.Checked) then
        GIF.ColorReduction := rmQuantize
      else if (MenuOptionsColorReductionOptimizedWindows.Checked) then
        GIF.ColorReduction := rmQuantizeWindows
      else
        GIF.ColorReduction := rmNone;
      // Set import color mapping options
      if (MenuOptionsColorDitheringFloydSteinberg.Checked) then
        GIF.DitherMode := dmFloydSteinberg
      else
        GIF.DitherMode := dmNearest;
      // Set output compression options
      if (MenuOptionsCompressionLZW.Checked) then
        GIF.Compression := gcLZW
      else
        GIF.Compression := gcRLE;

      // Convert to GIF
      GIF.Assign(ImageView.Picture.Graphic);

      // Save to GIF
      GIF.SaveToFile(SaveDialog.Filename);

      // Display GIF
      ImageView.Picture.Assign(GIF);
    finally
      GIF.Free;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TFormConvert.MenuFileExitClick(Sender: TObject);
begin
  Close;
end;

procedure TFormConvert.MenuOptionsClick(Sender: TObject);
begin
  TMenuItem(Sender).Checked := True;
end;

procedure TFormConvert.MenuOptionsColorReductionOptimizedBitsClick(Sender: TObject);
begin
  GIFImageDefaultColorReductionBits := TMenuItem(Sender).Tag;
  TMenuItem(Sender).Checked := True;
end;

procedure TFormConvert.MenuOptionsColorReductionWindows256Click(
  Sender: TObject);
var
  DesktopDC		: HDC;
  BitsPerPixel		: integer;
begin
  DesktopDC := GetDC(0);
  try
    BitsPerPixel := GetDeviceCaps(DesktopDC, BITSPIXEL) * GetDeviceCaps(DesktopDC, PLANES);
  finally
    ReleaseDC(0, DesktopDC);
  end;
  if (BitsPerPixel > 8) then
    ShowMessage('Please note that the Windows Halftone color reduction method'+#13+
                'only works properly in 256 color display mode.'+#13+#13+
                'Since your display adapter is currently in '+IntToStr(1 SHL BitsPerPixel)+' color mode,'+#13+
                'the Windows Halftone Palette method will produce the same'+#13+
                'result as the Windows System Palette method.');

  MenuOptionsClick(Sender);
end;

procedure TFormConvert.MenuEditClick(Sender: TObject);
begin
  // Update menu items when edit menu is dropped down
  MenuEditCut.Enabled := (ImageView.Picture.Graphic <> nil);
  MenuEditCopy.Enabled := (ImageView.Picture.Graphic <> nil);
  MenuEditPaste.Enabled := ClipBoard.HasFormat(CF_PICTURE);
end;

procedure TFormConvert.MenuEditCutClick(Sender: TObject);
begin
  // Cut to clipboard
  Clipboard.Assign(ImageView.Picture);
  ImageView.Picture.Graphic := nil;
end;

procedure TFormConvert.MenuEditCopyClick(Sender: TObject);
begin
  // Copy to clipboard
  Clipboard.Assign(ImageView.Picture);
end;

procedure TFormConvert.MenuEditPasteClick(Sender: TObject);
begin
  // Paste from clipboard
  ImageView.Picture.Assign(Clipboard);
end;


end.


