unit main;
////////////////////////////////////////////////////////////////////////////////
//                                                                            //
// Project:	TGIFImage demo application.                                   //
// Description:	Demonstration of bitmap import with automatic color reduction.//
// Copyright	(c) 1997-99 Anders Melander.                                  //
//		All rights reserved.                                          //
// Formatting:	2 space indent, 8 space tabs, 80 columns.                     //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////
//                                                                            //
// To make this demo work with Delphi 2 or C++ builder, you will need to      //
// load a bitmap (with *many* colors preferably) into the ImageBitmap         //
// TImage component on the form.                                              //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////
interface

uses
  gifimage,
{$IFNDEF VER90}
{$IFNDEF BCB}
  // Sorry guys - No jpeg in Delphi 2 and C++ Builder 3
  jpeg,
{$ENDIF}
{$ENDIF}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls, Menus;

type
  TFormMain = class(TForm)
    ProgressBar: TProgressBar;
    PanelSource: TPanel;
    ImageBitmap: TImage;
    Label1: TLabel;
    Panel2: TPanel;
    ButtonConvert: TButton;
    PanelDestination: TPanel;
    Label2: TLabel;
    ImageGIF: TImage;
    MainMenu1: TMainMenu;
    MenuFile: TMenuItem;
    MenuFileExit: TMenuItem;
    MenuPalette: TMenuItem;
    MenuDithering: TMenuItem;
    MenuDitheringFloydSteinberg: TMenuItem;
    MenuDitheringNearest: TMenuItem;
    MenuPaletteNone: TMenuItem;
    MenuPaletteWindows20: TMenuItem;
    MenuPaletteWindows256: TMenuItem;
    MenuPaletteWindowsGray4: TMenuItem;
    MenuPaletteMonochrome: TMenuItem;
    MenuPaletteGrayScale256: TMenuItem;
    MenuPaletteNetscape216: TMenuItem;
    MenuPaletteOptimized: TMenuItem;
    MenuPaletteOptimized8: TMenuItem;
    MenuPaletteOptimized16: TMenuItem;
    MenuPaletteOptimized32: TMenuItem;
    MenuPaletteOptimized64: TMenuItem;
    MenuPaletteOptimized128: TMenuItem;
    MenuPaletteOptimized256: TMenuItem;
    MenuPaletteOptimizedWindows256: TMenuItem;
    MenuDitheringStucki: TMenuItem;
    MenuDitheringBurkes: TMenuItem;
    MenuDitheringSierra: TMenuItem;
    MenuDitheringJaJuNi: TMenuItem;
    MenuDitheringSteveArche: TMenuItem;
    procedure ButtonConvertClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure MenuDitheringClick(Sender: TObject);
    procedure MenuFileExitClick(Sender: TObject);
    procedure MenuPaletteOptimizedBitsClick(Sender: TObject);
    procedure MenuPaletteClick(Sender: TObject);
    procedure MenuPaletteHalftoneClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    procedure OnProgress(Sender: TObject; Stage: TProgressStage;
      PercentDone: Byte; RedrawNow: Boolean; const R: TRect; const Msg: string);
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.DFM}

procedure TFormMain.FormCreate(Sender: TObject);
begin
  // Disable GIF autodithering since it might interfere with the
  // input dithering.
  // Note: This is only done for test purposes. You don't need to
  // do this in your own applications.
  Exclude(GIFImageDefaultDrawOptions, goDither);
  // Make sure that we have a bitmap to work with
  if (ImageBitmap.Picture.Graphic = nil) then
  begin
    if (FileExists(ExtractFilePath(Application.ExeName)+'bitmap.jpg')) then
      try
        ImageBitmap.Picture.LoadFromFile(ExtractFilePath(Application.ExeName)+'bitmap.jpg')
      except
        // Ignore
      end;
    if (ImageBitmap.Picture.Graphic = nil) then
    begin
      ButtonConvert.Enabled := False;
      ShowMessage('No bitmap to convert!'+#13+#13+
        'You need to load a high color bitmap into the'+#13+
        'ImageBitmap TImage component before running this demo');
    end;
  end;
end;

procedure TFormMain.MenuFileExitClick(Sender: TObject);
begin
  Close;
end;

procedure TFormMain.ButtonConvertClick(Sender: TObject);
var
  GIF			: TGIFImage;
begin
  // Display warning for Delphi 2
{$ifdef VER90}
  if (PaletteDevice) then
    ShowMessage('Color reduction can not be used with Delphi 2 in 256 color mode!');
{$endif}
  ButtonConvert.Enabled := False;
  try
    // Clear previous GIF view
    ImageGIF.Picture.Assign(nil);

    // Set color reduction options
      // Set import color reduction options
      if (MenuPaletteNetscape216.Checked) then
        GIFImageDefaultColorReduction := rmNetscape
      else if (MenuPaletteMonochrome.Checked) then
        GIFImageDefaultColorReduction := rmMonochrome
      else if (MenuPaletteGrayScale256.Checked) then
        GIFImageDefaultColorReduction := rmGrayScale
      else if (MenuPaletteWindowsGray4.Checked) then
        GIFImageDefaultColorReduction := rmWindowsGray
      else if (MenuPaletteWindows20.Checked) then
        GIFImageDefaultColorReduction := rmWindows20
      else if (MenuPaletteWindows256.Checked) then
        GIFImageDefaultColorReduction := rmWindows256
      else if (MenuPaletteOptimized.Checked) then
        GIFImageDefaultColorReduction := rmQuantize
      else if (MenuPaletteOptimizedWindows256.Checked) then
        GIFImageDefaultColorReduction := rmQuantizeWindows
      else
        GIFImageDefaultColorReduction := rmNone;

    if (MenuDitheringFloydSteinberg.Checked) then
      GIFImageDefaultDitherMode := dmFloydSteinberg
    else if (MenuDitheringStucki.Checked) then
      GIFImageDefaultDitherMode := dmStucki
    else if (MenuDitheringSierra.Checked) then
      GIFImageDefaultDitherMode := dmSierra
    else if (MenuDitheringJaJuNi.Checked) then
      GIFImageDefaultDitherMode := dmJaJuNi
    else if (MenuDitheringSteveArche.Checked) then
      GIFImageDefaultDitherMode := dmSteveArche
    else if (MenuDitheringBurkes.Checked) then
      GIFImageDefaultDitherMode := dmBurkes
    else if (MenuDitheringNearest.Checked) then
      GIFImageDefaultDitherMode := dmNearest;

    // Import bitmap into a GIF
    GIF := TGIFImage.Create;
    try
      GIF.OnProgress := OnProgress;
      GIF.Assign(ImageBitmap.Picture.Graphic);
      // Display the GIF
      ImageGIF.Picture.Assign(GIF);
    finally
      GIF.Free;
    end;
  finally
    ButtonConvert.Enabled := True;
  end;
end;

procedure TFormMain.OnProgress(Sender: TObject; Stage: TProgressStage;
  PercentDone: Byte; RedrawNow: Boolean; const R: TRect; const Msg: string);
begin
  if (Stage= psEnding) then
    ProgressBar.Position := 0
  else
    ProgressBar.Position := PercentDone;
end;

procedure TFormMain.FormResize(Sender: TObject);
begin
  PanelSource.Width := ClientWidth DIV 2;
end;

procedure TFormMain.MenuDitheringClick(Sender: TObject);
begin
  TMenuItem(Sender).Checked := True;
end;

procedure TFormMain.MenuPaletteOptimizedBitsClick(Sender: TObject);
begin
  GIFImageDefaultColorReductionBits := TMenuItem(Sender).Tag;
  TMenuItem(Sender).Checked := True;
end;

procedure TFormMain.MenuPaletteClick(Sender: TObject);
begin
  TMenuItem(Sender).Checked := True;
end;

procedure TFormMain.MenuPaletteHalftoneClick(Sender: TObject);
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

  MenuPaletteClick(Sender);
end;

end.
