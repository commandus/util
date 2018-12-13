unit main;
////////////////////////////////////////////////////////////////////////////////
//                                                                            //
// Project:	TGIFImage demo application.                                   //
// Description:	Simple GIF Animation Builder.                                 //
// Copyright	(c) 1997-99 Anders Melander.                                  //
//		All rights reserved.                                          //
// Formatting:	2 space indent, 8 space tabs, 80 columns.                     //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////
interface

uses
  GIFImage,
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls;

type
  TFormAnimate = class(TForm)
    ScrollBoxSource: TScrollBox;
    Image1: TImage;
    Image2: TImage;
    Image3: TImage;
    Image4: TImage;
    Image5: TImage;
    Image6: TImage;
    Panel1: TPanel;
    Panel2: TPanel;
    ImageAnimate: TImage;
    Panel3: TPanel;
    ButtonAnimate: TButton;
    ButtonSave: TButton;
    SaveDialog: TSaveDialog;
    GroupBox1: TGroupBox;
    CheckBoxCrop: TCheckBox;
    CheckBoxMerge: TCheckBox;
    CheckBoxPalette: TCheckBox;
    procedure ButtonAnimateClick(Sender: TObject);
    procedure ButtonSaveClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ImageClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormAnimate: TFormAnimate;

implementation

{$R *.DFM}


procedure TFormAnimate.FormCreate(Sender: TObject);
begin
  // Do not use buffering.
  // This is safe since we have complete control over the TImage's canvas
  include(GIFImageDefaultDrawOptions, goDirectDraw);
end;

procedure TFormAnimate.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  // Zap GIF to stop display of animation (for safety)
  ImageAnimate.Picture.Graphic := nil;
end;

procedure TFormAnimate.ButtonAnimateClick(Sender: TObject);
var
  GIF			: TGIFImage;
  i			: integer;
  OptimizeOptions	: TGIFOptimizeOptions;

  function TransparentIndex(GIF: TGIFSubImage): byte;
  begin
    // Use the lower left pixel as the transparent color
    Result := GIF.Pixels[0, GIF.Height-1];
  end;

  function AddBitmap(GIF: TGIFImage; Source: TGraphic; Transparent: boolean): integer;
  var
    Ext			: TGIFGraphicControlExtension;
    LoopExt		: TGIFAppExtNSLoop;
  begin
    // Preview the image being added (for user feedback)
    ImageAnimate.Picture.Assign(Source);
    ImageAnimate.Update;

    // Add the source image to the animation
    Result := GIF.Add(Source);
    // Netscape Loop extension must be the first extension in the first frame!
    if (Result = 0) then
    begin
      LoopExt := TGIFAppExtNSLoop.Create(GIF.Images[Result]);
      LoopExt.Loops := 0; // Number of loops (0 = forever)
      GIF.Images[Result].Extensions.Add(LoopExt);
    end;
    // Add Graphic Control Extension
    Ext := TGIFGraphicControlExtension.Create(GIF.Images[Result]);
    Ext.Delay := 30; // Animation delay (30 = 300 mS)
    if (Transparent) then
    begin
      Ext.Transparent := True;
      Ext.TransparentColorIndex := TransparentIndex(GIF.Images[Result]);
    end;
    GIF.Images[Result].Extensions.Add(Ext);
  end;

begin
  Screen.Cursor := crHourGlass;
  try
    GIF := TGIFImage.Create;
    try
      // Add all the images in the scroll box to the animation
      for i := 0 to ScrollBoxSource.ControlCount-1 do
        // Add frame
        AddBitmap(GIF, // Destination GIF
          TImage(ScrollBoxSource.Controls[i]).Picture.Graphic, // Source bitmap
          (i > 0)); // First frame is not transparent

      // Optimize Color map...
      if (CheckBoxPalette.Checked) then
        GIF.OptimizeColorMap;

      // Optimize GIF frames...
      OptimizeOptions := [];
      if (CheckBoxMerge.Checked) then
        include(OptimizeOptions, ooMerge);
      if (CheckBoxCrop.Checked) then
        include(OptimizeOptions, ooCrop);
      if (OptimizeOptions <> []) then
        GIF.Optimize(OptimizeOptions, rmNone, dmNearest, 0);

      // Display the animation
      ImageAnimate.Picture.Assign(GIF);
    finally
      GIF.Free;
    end;
    ButtonSave.Enabled := True;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TFormAnimate.ButtonSaveClick(Sender: TObject);
begin
  // Set file extension and filter to GIF (or whatever is currently displayed)
  SaveDialog.DefaultExt := GraphicExtension(TGraphicClass(ImageAnimate.Picture.Graphic.ClassType));
  SaveDialog.Filter :=  GraphicFilter(TGraphicClass(ImageAnimate.Picture.Graphic.ClassType));
  // Prompt for filename
  if (SaveDialog.Execute) then
    ImageAnimate.Picture.SaveToFile(SaveDialog.Filename);
end;

procedure TFormAnimate.ImageClick(Sender: TObject);
begin
  // Preview a source image
  ImageAnimate.Picture.Assign(TImage(Sender).Picture);
end;

end.

