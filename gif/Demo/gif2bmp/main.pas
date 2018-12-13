unit main;
////////////////////////////////////////////////////////////////////////////////
//                                                                            //
// Project:	TGIFImage demo application.                                   //
// Description:	Simple GIF to BMP converter.                                  //
// Copyright	(c) 1997-99 Anders Melander.                                  //
//		All rights reserved.                                          //
// Formatting:	2 space indent, 8 space tabs, 80 columns.                     //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////
interface

uses
  GIFimage,
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls;

type
  TFormMain = class(TForm)
    ProgressBar: TProgressBar;
    PanelSource: TPanel;
    LabelGIF: TLabel;
    Panel2: TPanel;
    ButtonConvert: TButton;
    PanelDestination: TPanel;
    LabelBMP: TLabel;
    ImageBMP: TImage;
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
  GIF			: TGIFImage;
  Bitmap		: TBitmap;
begin
  ButtonConvert.Enabled := False;
  try
    GIF := TGIFImage.Create;
    try
      GIF.OnProgress := OnProgress;
      // Load the GIF that will be converted
      GIF.LoadFromFile(ExtractFIlePath(Application.ExeName)+'test.gif');
      // Display the GIF
      ImageGIF.Picture.Assign(GIF);
      // Clear previous BMP view
      ImageBMP.Picture.Assign(nil);

      ShowMessage('This demo loads a GIF from the file TEST.GIF,'+#13+
        'converts it to a bitmap and saves it as TEST.BMP');

      Bitmap := TBitmap.Create;
      try
        // Convert the GIF to a BMP
        Bitmap.Assign(GIF);
        // Save the BMP
        Bitmap.SaveToFile('test.bmp');
        // Display the BMP
        ImageBMP.Picture.Assign(Bitmap);
      finally
        Bitmap.Free;
      end;
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

end.
