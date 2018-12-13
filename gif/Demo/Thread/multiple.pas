unit multiple;
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
  TFormMultiple = class(TForm)
    Memo1: TMemo;
    MemoWarning: TMemo;
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    LastWidth		,
    LastHeight		: integer;
  public
    { Public declarations }
  end;

var
  FormMultiple: TFormMultiple;

implementation

{$R *.DFM}

uses
  main,
  gifimage;

procedure TFormMultiple.FormDestroy(Sender: TObject);
begin
  // Stop all running painters
  (FormMain.ImageBackground.Picture.Graphic as TGIFImage).PaintStop;
end;

procedure TFormMultiple.FormResize(Sender: TObject);
var
  x, y			: integer;
  NewWidth, NewHeight	: integer;
begin
  if not(Assigned(FormMain.ImageBackground.Picture.Graphic)) then
    exit;

  NewWidth := (Width+FormMain.ImageBackground.Width-1) DIV FormMain.ImageBackground.Width;
  NewHeight := (Height+FormMain.ImageBackground.Height-1) DIV FormMain.ImageBackground.Height;

  // Only update when size differs a whole multiple of image widht/height
  if (NewWidth = LastWidth) and (NewHeight = LastHeight) then
    exit;

  LastWidth := NewWidth;
  LastHeight := NewHeight;

  // Stop all running painters
  (FormMain.ImageBackground.Picture.Graphic as TGIFImage).PaintStop;
  // Give windows a chance if we have a lot of threads
  GIFImageDefaultThreadPriority := tpIdle;
  // Loop to create painters
  x := 0;
  while (x < Width) do
  begin
    y := 0;
    while (y < Height) do
    begin
      // Start a paint thread to draw a chunk of the form canvas
      TGIFImage(FormMain.ImageBackground.Picture.Graphic).Paint(Canvas,
        Rect(x,y,x+FormMain.ImageBackground.Width,y+FormMain.ImageBackground.Height),
        [goAsync,goAnimate,goLoop,goLoopContinously,goDirectDraw,goDither,goAutoDither]);
      inc(y, FormMain.ImageBackground.Height);
    end;
    inc(x, FormMain.ImageBackground.Width);
  end;
end;

procedure TFormMultiple.FormCreate(Sender: TObject);
begin
  MemoWarning.Visible := (Win32Platform <> VER_PLATFORM_WIN32_NT); 
end;

end.
