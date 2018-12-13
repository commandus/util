unit main;
////////////////////////////////////////////////////////////////////////////////
//                                                                            //
// Project:	TGIFImage demo application.                                   //
// Description:	Simple BMP to GIF converter.                                  //
// Copyright	(c) 1997-99 Anders Melander.                                  //
//		All rights reserved.                                          //
// Formatting:	2 space indent, 8 space tabs, 80 columns.                     //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////
interface

uses
  gifimage,
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls;

type
  TFormMain = class(TForm)
    ProgressBar: TProgressBar;
    PanelSource: TPanel;
    ImageBMP: TImage;
    Label1: TLabel;
    Panel2: TPanel;
    ButtonConvert: TButton;
    PanelDestination: TPanel;
    Label2: TLabel;
    ImageGIF: TImage;
    procedure ButtonConvertClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
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

procedure TFormMain.ButtonConvertClick(Sender: TObject);
var
  Bitmap		: TBitmap;
  GIF			: TGIFImage;
begin
  ButtonConvert.Enabled := False;
  try
    Bitmap := TBitmap.Create;
    try
      // Load the bitmap that will be converted
      Bitmap.LoadFromFile(ExtractFIlePath(Application.ExeName)+'test.bmp');
      // Display the bitmap
      ImageBMP.Picture.Assign(Bitmap);
      // Clear previous GIF view
      ImageGIF.Picture.Assign(nil);

      ShowMessage('This demo loads a bitmap from the file TEST.BMP,'+#13+
        'converts it to a GIF and saves it as TEST.GIF');

      GIF := TGIFImage.Create;
      try
        GIF.OnProgress := OnProgress;
        // Convert the bitmap to a GIF
        GIF.Assign(Bitmap);
        // Save the GIF
        GIF.SaveToFile('test.gif');
        // Display the GIF
        ImageGIF.Picture.Assign(GIF);
      finally
        GIF.Free;
      end;
    finally
      Bitmap.Free;
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

end.
