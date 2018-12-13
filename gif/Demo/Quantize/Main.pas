////////////////////////////////////////////////////////////////////////////////
//                                                                            //
// Project:	TGIFImage demo application.                                   //
// Description:	Color Quantization.                                           //
// Copyright	(c) 1997-99 Anders Melander.                                  //
//		All rights reserved.                                          //
// Formatting:	2 space indent, 8 space tabs, 80 columns.                     //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////
unit Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, GIFImage, ExtCtrls;

type
  TFormMain = class(TForm)
    Panel1: TPanel;
    Image1: TImage;
    Image2: TImage;
    Image3: TImage;
    Image4: TImage;
    Panel2: TPanel;
    Image5: TImage;
    Image6: TImage;
    Image7: TImage;
    Image8: TImage;
    Panel3: TPanel;
    ButtonNetscape: TButton;
    ButtonImage1: TButton;
    ButtonOptmized: TButton;
    procedure ButtonNetscapeClick(Sender: TObject);
    procedure ButtonImage1Click(Sender: TObject);
    procedure ButtonOptmizedClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.DFM}

procedure TFormMain.ButtonNetscapeClick(Sender: TObject);
var
  Bitmap		: TBitmap;
  NewGIF		: TGIFImage;
  NewBitmap		: TBitmap;
  i			: integer;
begin
  // Reduce to the Netscape 216 color palette
  NewGIF := TGIFImage.Create;
  try
    Bitmap := TBitmap.Create;
    try

      for i := 0 to Panel1.ControlCount-1 do
        if (Panel1.Controls[i] is TImage) and (TImage(Panel1.Controls[i]).Picture.Graphic <> nil) then
        begin
          // Convert source image to bitmap
          Bitmap.Assign(TImage(Panel1.Controls[i]).Picture.Graphic);

          // Reduce colors
          NewBitmap := ReduceColors(Bitmap, rmNetscape, dmFloydSteinberg, 0, 0);
          try
            // Convert reduced bitmap to GIF
            NewGIF.Assign(NewBitmap);
            NewGIF.SaveToFile(format('gif%d.gif', [i]));

            // Display result
            (Panel2.Controls[i] as TImage).Picture.Assign(NewGIF);
          finally
            NewBitmap.Free;
          end;
        end;
    finally
      Bitmap.Free;
    end;

  finally
    NewGIF.Free;
  end;
end;

procedure TFormMain.ButtonImage1Click(Sender: TObject);
var
  Bitmap		: TBitmap;
  NewGIF		: TGIFImage;
  NewBitmap		: TBitmap;
  i			: integer;
  Palette		: hPalette;
begin
  // Reduce to the palette used by the first image
  NewGIF := TGIFImage.Create;
  try
    Palette := 0;
    Bitmap := TBitmap.Create;
    try
      for i := 0 to Panel1.ControlCount-1 do
        if (Panel1.Controls[i] is TImage) and (TImage(Panel1.Controls[i]).Picture.Graphic <> nil) then
        begin
          // Convert source image to bitmap
          Bitmap.Assign(TImage(Panel1.Controls[i]).Picture.Graphic);

          // Use palette of first image (must be pf8bit or lower)
          if (Palette = 0) then
            Palette := CopyPalette(Bitmap.Palette);

          // Reduce colors
          NewBitmap := ReduceColors(Bitmap, rmPalette, dmFloydSteinberg, 0, Palette);
          try
            // Convert reduced bitmap to GIF
            NewGIF.Assign(NewBitmap);
            NewGIF.SaveToFile(format('gif%d.gif', [i]));

            // Display result
            (Panel2.Controls[i] as TImage).Picture.Assign(NewGIF);
          finally
            NewBitmap.Free;
          end;
        end;
    finally
      if (Palette <> 0) then
        DeleteObject(Palette);
      Bitmap.Free;
    end;

  finally
    NewGIF.Free;
  end;
end;

procedure TFormMain.ButtonOptmizedClick(Sender: TObject);
var
  List			: TList;
  i			: integer;
  Palette		: hPalette;
  Bitmap		: TBitmap;
  NewGIF		: TGIFImage;
  NewBitmap		: TBitmap;
begin
  // Reduce to an optimal palette based on all the images
  NewGIF := TGIFImage.Create;
  try
    List := TList.Create;
    try
      for i := 0 to Panel1.ControlCount-1 do
        if (Panel1.Controls[i] is TImage) and (TImage(Panel1.Controls[i]).Picture.Graphic <> nil) then
        begin
          Bitmap := TBitmap.Create;
          // Convert source image to bitmap
          Bitmap.Assign(TImage(Panel1.Controls[i]).Picture.Graphic);
          List.Add(Bitmap);
        end;
        // Create an optimized palette
        Palette := CreateOptimizedPaletteFromManyBitmaps(List, 256, 8, False);
        try
          for i := 0 to List.Count-1 do
          begin
            // Reduce colors
            NewBitmap := ReduceColors(TBitmap(List[i]), rmPalette, dmFloydSteinberg, 0, Palette);
            try
              // Convert reduced bitmap to GIF
              NewGIF.Assign(NewBitmap);
              NewGIF.SaveToFile(format('gif%d.gif', [i]));

              // Display result
              (Panel2.Controls[i] as TImage).Picture.Assign(NewGIF);
            finally
              NewBitmap.Free;
            end;
          end;
        finally
          DeleteObject(Palette);
        end;
    finally
      for i := 0 to List.Count-1 do
        TBitmap(List[i]).Free;
      List.Free;
    end;
  finally
    NewGIF.Free;
  end;
end;

end.
