unit native;
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
  TFormNative = class(TForm)
    ImageGIF: TImage;
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormNative: TFormNative;

implementation

{$R *.DFM}

uses
  main,
  gifimage;

procedure TFormNative.FormCreate(Sender: TObject);
begin
  ImageGIF.Picture.Assign(FormMain.ImageBackground.Picture);
  (ImageGIF.Picture.Graphic as TGIFImage).DrawOptions :=
    (ImageGIF.Picture.Graphic as TGIFImage).DrawOptions + [goTile, goDirectDraw];
end;

procedure TFormNative.FormDestroy(Sender: TObject);
begin
  // Stop all running painters
  ImageGIF.Picture.Graphic := nil;
end;

end.
