unit main;
////////////////////////////////////////////////////////////////////////////////
//                                                                            //
// Project:	TGIFImage demo application.                                   //
// Description:	AVI to GIF converter.                                         //
// Copyright	(c) 1997-99 Anders Melander.                                  //
//		All rights reserved.                                          //
// Formatting:	2 space indent, 8 space tabs, 80 columns.                     //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

uses
  GIFImage,
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, Buttons;

type
  TFormMain = class(TForm)
    PanelAVI: TPanel;
    LabelAVI: TLabel;
    PanelGIF: TPanel;
    LabelGIF: TLabel;
    ImageGIF: TImage;
    PanelButton: TPanel;
    OpenDialog: TOpenDialog;
    ButtonOpen: TBitBtn;
    ButtonConvert: TBitBtn;
    ButtonSave: TBitBtn;
    SaveDialog: TSaveDialog;
    ProgressBar: TProgressBar;
    ButtonAbort: TBitBtn;
    procedure ButtonConvertClick(Sender: TObject);
    procedure ButtonOpenClick(Sender: TObject);
    procedure ButtonSaveClick(Sender: TObject);
    procedure ButtonAbortClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    Filename: string;
    DoAbort: boolean;
{$ifndef VER90}
    AnimateAVI: TAnimate;
{$endif}
    procedure OnProgress(Sender: TObject; Stage: TProgressStage;
      PercentDone: Byte; RedrawNow: Boolean; const R: TRect; const Msg: string);
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.DFM}

uses
  VfW;	// Video for Windows stuff

procedure TFormMain.FormCreate(Sender: TObject);
begin
  // Do not use buffering.
  // This is safe since we have complete control over the TImage's canvas
  include(GIFImageDefaultDrawOptions, goDirectDraw);

{$ifdef VER90}
  PanelAVI.Caption := 'No AVI preview in Delphi 2';
{$else}
  // Delphi 2 doesn't support TImage.OnProgress
  ImageGIF.OnProgress := OnProgress;
  // ...or TAnimate
  AnimateAVI := TAnimate.Create(self);
  AnimateAVI.Parent := PanelAVI;
  AnimateAVI.Align := alClient;
  AnimateAVI.Active := False;
  AnimateAVI.AutoSize := False;
  AnimateAVI.Transparent := True;
{$endif}
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  // Stop paint thread.
  // Very important when we are using the goDirectDraw option!
  ImageGIF.Picture.Graphic := nil;
end;

procedure TFormMain.ButtonOpenClick(Sender: TObject);
begin
  PanelAVI.Caption := ' ';
  if (OpenDialog.Execute) then
  begin
    Filename := OpenDialog.FileName;
{$ifndef VER90}
    try
      AnimateAVI.FileName := Filename;
      AnimateAVI.Active := True;
      AnimateAVI.Show;
    except
      AnimateAVI.Hide;
      AnimateAVI.Active := False;
      PanelAVI.Caption := 'Cannot preview AVI';
    end;
{$endif}
    ButtonConvert.Enabled := True;
    ButtonConvert.SetFocus;
    ButtonSave.Enabled := False;
    ImageGIF.Picture.Graphic := nil;
  end;
end;

procedure TFormMain.ButtonAbortClick(Sender: TObject);
begin
  DoAbort := True;
  ButtonAbort.Enabled := False;
end;

procedure TFormMain.ButtonConvertClick(Sender: TObject);
var
  pFile			: PAVIFile;
  pStream		: PAVISTREAM;
  StreamInfo		: TAVIStreamInfo;
  pGetFrame		: pointer;
  Bits			,
  BitmapInfo		: PBitmapInfo;
  BitmapFileHeader	: TBitmapFileHeader;
  Stream		: TMemoryStream;
  Bitmap		: TBitmap;
  Size			: integer;
  Frame			: integer;
  InfoSize		: longInt;
  GIF			: TGIFImage;
  Ext			: TGIFGraphicControlExtension;
  LoopExt		: TGIFAppExtNSLoop;
  Index			: integer;
  Delay			: integer;
begin
  DoAbort := False;
  ButtonAbort.Enabled := True;
  ButtonAbort.Show;
  ButtonAbort.SetFocus;
  ButtonConvert.Hide;
  ButtonOpen.Enabled := False;
  Screen.Cursor := crAppStart;
  try
    // Initialize the AVI file library
    AVIFileInit;
    try
      // Open AVI file for read
      if (AVIFileOpen(pFile, PChar(FileName), OF_READ OR OF_SHARE_DENY_WRITE, nil) <> AVIERR_OK) then
        raise Exception.Create('Failed to open AVI file');
      try
        // Open AVI data stream
        if (AVIFileGetStream(pFile, pStream, streamtypeVIDEO, 0) <> AVIERR_OK) then
          raise Exception.Create('Failed to open AVI stream');
        try
          // Get stream info
          if (AVIStreamInfo(pStream, StreamInfo, sizeof(StreamInfo)) <> AVIERR_OK) then
            raise Exception.Create('Failed to get AVI stream info');

          // Calculate delay/frame
          if (StreamInfo.dwRate > 0) then
            Delay := MulDiv(StreamInfo.dwScale, 100, StreamInfo.dwRate)
          else
            Delay := integer(StreamInfo.dwScale) DIV 100;

          // Get stream format size
          if (AVIStreamReadFormat(pStream, 0, nil, InfoSize) <> AVIERR_OK) then
            raise Exception.Create('Failed to get AVI stream format size');

          BitmapInfo := nil;
          if (InfoSize > 0) then
            GetMem(BitmapInfo, InfoSize);
          try

            // Get stream format
            if (AVIStreamReadFormat(pStream, 0, BitmapInfo, InfoSize) <> AVIERR_OK) then
              raise Exception.Create('Failed to get AVI stream format');

            // Begin reading from stream
            if (AVIStreamBeginStreaming(pStream, 0, 0, 1000) <> AVIERR_OK) then
              raise Exception.Create('Failed to start AVI stream');
            try
              // Begin reading frames (in any format)
              pGetFrame := AVIStreamGetFrameOpen(pStream, PBitmapInfoHeader(nil)^);
              if (pGetFrame = nil) then
                raise Exception.Create('Failed to start AVI decompression');
              try
                Stream := TMemoryStream.Create;
                try
                  Bitmap := TBitmap.Create;
                  try
                    GIF := TGIFImage.Create;
                    try
                      // Set color reduction options
                      GIF.ColorReduction := rmNetscape;
                      GIF.DitherMode := dmFloydSteinberg;

                      ProgressBar.Min := StreamInfo.dwStart;
                      ProgressBar.Max := StreamInfo.dwLength;
                      ProgressBar.Position := ProgressBar.Min;
                      try
                        for Frame := StreamInfo.dwStart to StreamInfo.dwLength-1 do
                        begin
                          ProgressBar.Position := Frame;
                          Application.ProcessMessages;
                          if (DoAbort) then
                            exit;
                          // Read a frame
                          Bits := AVIStreamGetFrame(pGetFrame, Frame);
                          if (Bits = nil) then
                            raise Exception.Create('Failed to decompress AVI frame');

                          // Check for palette change
                          (* I don't quite know how to handle this, so I have disabled it for now...
                          if ((StreamInfo.dwFlags AND AVISTREAMINFO_FORMATCHANGES) = AVISTREAMINFO_FORMATCHANGES) then
                          begin
                            // Get new stream info
                            if (AVIStreamInfo(pStream, StreamInfo, sizeof(StreamInfo)) <> AVIERR_OK) then
                              raise Exception.Create('Failed to get AVI stream info');
                          end;
                          *)

                          // Copy palette to DIB
                          Move(BitmapInfo^.bmiColors, Bits^.bmiColors, (BitmapInfo^.bmiHeader.biClrUsed * sizeof(TRGBQuad)));

                          // Init BMP file header
                          FillChar(BitmapFileHeader, sizeof(TBitmapFileHeader), 0);
                          with BitmapFileHeader do
                          begin
                            bfType := $4D42; // 'BM' = Windows BMP signature
                            bfSize := 0; // File size (not needed)
                            bfOffBits := sizeof(TBitmapFileHeader);
                            if (Bits^.bmiHeader.biBitCount > 8) then
                            begin
                              // Header but no palette
                              if ((Bits^.bmiHeader.biCompression and BI_BITFIELDS) = BI_BITFIELDS) then
                                Inc(bfOffBits, 12);
                            end else
                              // Header and palette
                              Inc(bfOffBits, sizeof(TRGBQuad) * (1 shl Bits^.bmiHeader.biBitCount));
                          end;
                          Size := BitmapFileHeader.bfOffBits + Bits.bmiHeader.biSizeImage;
                          // Write DIB as a BMP
                          Stream.Clear;
                          Stream.Write(BitmapFileheader, sizeof(BitmapFileheader));
                          Stream.Write(Bits^, Size+26); // ***FIXME*** Magic value found emperically
                          Stream.Position := 0;
                          // Load bitmap
                          Bitmap.LoadFromStream(Stream);
                          // Convert to GIF
                          Index := GIF.Add(Bitmap);
                          // Add Netscape Loop extension first!
                          if (Index = 0) then
                          begin
                            LoopExt := TGIFAppExtNSLoop.Create(GIF.Images[Index]);
                            LoopExt.Loops := 0; // Forever
                            GIF.Images[Index].Extensions.Add(LoopExt);
                          end;
                          // Add Graphic Control Extension (for delay)
                          Ext := TGIFGraphicControlExtension.Create(GIF.Images[Index]);
                          Ext.Delay := Delay;
                          GIF.Images[Index].Extensions.Add(Ext);

                          // Display frame
                          ImageGIF.Picture.Assign(Bitmap);
                          ImageGIF.Update;
                        end;
                        // Display GIF
                        ImageGIF.Picture.Assign(GIF);
                        ButtonSave.Enabled := True;
                        ButtonSave.SetFocus;
                      finally
                        ProgressBar.Position := ProgressBar.Min;
                      end;
                    finally
                      GIF.Free;
                    end;
                  finally
                    Bitmap.Free;
                  end;
                finally
                  Stream.Free;
                end;
              finally
                AVIStreamGetFrameClose(pGetFrame);
              end;
            finally
              AVIStreamEndStreaming(pStream);
            end;
          finally
            if (BitmapInfo <> nil) then
              FreeMem(BitmapInfo);
          end;
        finally
          AVIStreamRelease(pStream);
        end;
      finally
        AVIFileRelease(pFile);
      end;
    finally
      AVIFileExit;
    end;
  finally
    Screen.Cursor := crDefault;
    ButtonAbort.Hide;
    ButtonConvert.Show;
    ButtonOpen.Enabled := True;
  end;
end;

procedure TFormMain.ButtonSaveClick(Sender: TObject);
begin
  if (ImageGIF.Picture.Graphic <> nil) and (ImageGIF.Picture.Graphic is TGIFImage) then
  begin
    // Set default file extension to GIF
    SaveDialog.DefaultExt := GraphicExtension(TGIFImage);
    // Only use GIF file filter
    SaveDialog.Filter :=  GraphicFilter(TGIFImage);
    // Change extension of original filename to .GIF
    SaveDialog.Filename := ChangeFileExt(OpenDialog.Filename, '.'+GraphicExtension(TGIFImage));
    // Prompt for filename
    if (SaveDialog.Execute) then
    begin
      Screen.Cursor := crHourGlass;
      try
{$ifdef VER90}
        // Delphi 2 doesn't support TImage.OnProgress so we must use
        // TGIFImage.OnProgress instead
        TGIFImage(ImageGIF.Picture.Graphic).OnProgress := OnProgress;
{$endif}
        // Optimize GIF.
        // Note: This will stop the GIF animation!
        TGIFImage(ImageGIF.Picture.Graphic).Optimize([ooCrop, ooMerge, ooCleanup, ooColorMap], rmNone, dmNearest, 8);
        // Optimize multiple local color maps into a single global (saves space)
        TGIFImage(ImageGIF.Picture.Graphic).OptimizeColorMap;
        // Save GIF
        ImageGIF.Picture.SaveToFile(SaveDialog.FileName);
        // Restart animation
        ImageGIF.Invalidate;
      finally
        Screen.Cursor := crDefault;
      end;
      ButtonOpen.SetFocus;
    end;
  end;
end;

procedure TFormMain.OnProgress(Sender: TObject; Stage: TProgressStage;
  PercentDone: Byte; RedrawNow: Boolean; const R: TRect; const Msg: string);
begin
  if (Stage = psStarting) then
  begin
    ProgressBar.Position := 0;
    ProgressBar.Min := 0;
    ProgressBar.Max := 100;
  end else
  if (Stage = psEnding) then
  begin
    ProgressBar.Position := 0;
  end else
    ProgressBar.Position := PercentDone;
end;

end.
