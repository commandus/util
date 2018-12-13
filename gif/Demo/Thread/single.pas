unit single;
////////////////////////////////////////////////////////////////////////////////
//                                                                            //
// Project:	TGIFImage demo application.                                   //
// Description:	Displays a GIF without the TImage component.                  //
// Copyright	(c) 1997-99 Anders Melander.                                  //
//		All rights reserved.                                          //
// Formatting:	2 space indent, 8 space tabs, 80 columns.                     //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////
interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls;

type
  TFormSingle = class(TForm)
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
  private
    { Private declarations }
    Bitmap: TBitmap;
    procedure DoOnChange(Sender: TObject);
  public
    { Public declarations }
  end;

var
  FormSingle: TFormSingle;

implementation

{$R *.DFM}

uses
  main,
  gifimage;

procedure TFormSingle.FormCreate(Sender: TObject);
begin
  Bitmap := TBitmap.Create;

  Bitmap.Height := FormMain.ImageBackground.Height;
  Bitmap.Width := FormMain.ImageBackground.Width;
  Bitmap.OnChange := DoOnChange;

  TGIFImage(FormMain.ImageBackground.Picture.Graphic).Paint(Bitmap.Canvas, Bitmap.Canvas.ClipRect,
    [goAsync,goAnimate,goLoop,goLoopContinously,goDirectDraw,goDither,goAutoDither]);
end;

procedure TFormSingle.DoOnChange(Sender: TObject);
begin
  FormPaint(self);
end;

procedure TFormSingle.FormDestroy(Sender: TObject);
begin
  // Stop all running painters
  (FormMain.ImageBackground.Picture.Graphic as TGIFImage).PaintStop;

  Bitmap.Free;
end;

procedure TFormSingle.FormPaint(Sender: TObject);
var
  x, y			: integer;
begin
  if not(Assigned(FormMain.ImageBackground.Picture.Graphic)) then
    exit;
  // Loop to paint tiles
  x := 0;
  while (x < Width) do
  begin
    y := 0;
    while (y < Height) do
    begin
      Canvas.Draw(x, y, Bitmap);
      inc(y, FormMain.ImageBackground.Height);
    end;
    inc(x, FormMain.ImageBackground.Width);
  end;
end;

end.
