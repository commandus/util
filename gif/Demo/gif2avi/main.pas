unit main;
////////////////////////////////////////////////////////////////////////////////
//                                                                            //
// Project:	TGIFImage demo application.                                   //
// Description:	GIF to AVI converter.                                         //
// Copyright	(c) 1997-99 Anders Melander.                                  //
//		All rights reserved.                                          //
// Formatting:	2 space indent, 8 space tabs, 80 columns.                     //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////
//                                                                            //
// Notes:                                                                     //
// * Does not handle transparent GIFs properly.                               //
// * Does not handle GIF frames that are smaller than the image.              //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

uses
  GIFImage,
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, Buttons;

type
  TFormMain = class(TForm)
    PanelGIF: TPanel;
    PanelAVI: TPanel;
    PanelButton: TPanel;
    OpenDialog: TOpenDialog;
    ButtonOpen: TBitBtn;
    ButtonSave: TBitBtn;
    SaveDialog: TSaveDialog;
    ProgressBar: TProgressBar;
    ButtonAbort: TBitBtn;
    LabelAVI: TLabel;
    LabelGIF: TLabel;
    ImageGIF: TImage;
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

  // Start in last used directory
  OpenDialog.InitialDir := ExtractFilePath(FileName);
  // Set default file extension to GIF
  OpenDialog.DefaultExt := GraphicExtension(TGIFImage);
  // Only use GIF file filter
  OpenDialog.Filter := GraphicFilter(TGIFImage);

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
    ImageGIF.Picture.Assign(nil);
    Filename := OpenDialog.FileName;
    ImageGIF.Picture.LoadFromFile(Filename);
    if (ImageGIF.Picture.Graphic = nil) or
      (not(ImageGIF.Picture.Graphic is TGIFImage)) or
      (TGIFImage(ImageGIF.Picture.Graphic).Empty) then
      exit;
    Filename := ChangeFileExt(Filename, '.AVI');
    ButtonSave.Enabled := True;
    ButtonSave.SetFocus;
{$ifndef VER90}
    AnimateAVI.Active := False;
    AnimateAVI.Filename := '';
{$endif}
  end;
end;

procedure TFormMain.ButtonAbortClick(Sender: TObject);
begin
  DoAbort := True;
  ButtonAbort.Enabled := False;
end;

procedure TFormMain.ButtonSaveClick(Sender: TObject);
var
  GIF			: TGIFImage;
  pFile			: PAVIFile;
  pStream		: PAVISTREAM;
  StreamInfo		: TAVIStreamInfo;
  BitmapInfo		: PBitmapInfoHeader;
  BitmapInfoSize	: Integer;
  BitmapSize		: longInt;
  BitmapBits		: pointer;
  Dummy			: LONG;
  i			: integer;
  HasLocalPalette	: boolean;
begin
  DoAbort := False;
  ButtonAbort.Enabled := True;
  ButtonAbort.Show;
  ButtonAbort.SetFocus;
  ButtonOpen.Enabled := False;
  Screen.Cursor := crAppStart;
  try
    GIF := ImageGIF.Picture.Graphic as TGIFImage;
    // Initialize the AVI file library
    AVIFileInit;
    try
      // Open AVI file for write
      if (AVIFileOpen(pFile, PChar(FileName), OF_WRITE or OF_CREATE OR OF_SHARE_EXCLUSIVE, nil) <> AVIERR_OK) then
        raise Exception.Create('Failed to create AVI file');
      try
        // Determine size of DIB
        InternalGetDIBSizes(GIF.Images[0].Bitmap.Handle, BitmapInfoSize, BitmapSize, pf8bit);
        if (BitmapInfoSize = 0) then
          raise Exception.Create('Failed to retrieve bitmap info');

        // Set stream info
        FillChar(StreamInfo, sizeof(StreamInfo), 0);

        // Convert GIF frame delay to AVI frame rate and..
        // ...determine if we need to equalize the palette
        StreamInfo.dwRate := 65; // Should be 100, but that's too fast... }
        StreamInfo.dwScale := 0;
        HasLocalPalette := False;
        for i := 0 to GIF.Images.Count-1 do
        begin
          if (GIF.Images[i].ColorMap.Count > 0) then
            HasLocalPalette := True;
          if (GIF.Images[i].GraphicControlExtension <> nil) then
            StreamInfo.dwScale := GIF.Images[i].GraphicControlExtension.Delay;
          // Break if both palette and delay has been determined
          if (HasLocalPalette) and (StreamInfo.dwScale > 0) then
            break;
        end;
        if (StreamInfo.dwScale = 0) then
          StreamInfo.dwScale := 1;

        StreamInfo.fccType := streamtypeVIDEO;
        StreamInfo.fccHandler := 0;
        StreamInfo.dwFlags := 0;
        StreamInfo.dwSuggestedBufferSize := BitmapSize;
        StreamInfo.rcFrame.Right := GIF.Width;
        StreamInfo.rcFrame.Bottom := GIF.Height;

        if (HasLocalPalette) then
          // Force GIFs to be dithered to a single common palette
          GIF.DrawOptions := GIF.DrawOptions + [goDither] - [goAutoDither];

        // Open AVI data stream
        if (AVIFileCreateStream(pFile, pStream, StreamInfo) <> AVIERR_OK) then
          raise Exception.Create('Failed to create AVI stream');
        try
          BitmapInfo := nil;
          BitmapBits := nil;
          try
            // Get DIB header and pixel buffers
            GetMem(BitmapInfo, BitmapInfoSize);
            GetMem(BitmapBits, BitmapSize);
            InternalGetDIB(GIF.Images[0].Bitmap.Handle, 0, BitmapInfo^, BitmapBits^, pf8bit);
            // Set AVI format from DIB info
            if (AVIStreamSetFormat(pStream, 0, BitmapInfo, BitmapInfoSize) <> AVIERR_OK) then
              raise Exception.Create('Failed to set AVI stream format');

            ProgressBar.Min := 0;
            ProgressBar.Max := GIF.Images.Count-1;
            ProgressBar.Position := 0;
            ProgressBar.Show;
            try
              for i := 0 to GIF.Images.Count-1 do
              begin
                ProgressBar.Position := i;
                Application.ProcessMessages;
                if (DoAbort) then
                  exit;
                if (GIF.Images[i].Empty) then
                  Continue;
                if (i > 0) then
                  InternalGetDIB(GIF.Images[i].Bitmap.Handle, 0, BitmapInfo^, BitmapBits^, pf8bit);
                // Write GIF frame to AVI
                if AVIStreamWrite(pStream, i, 1, BitmapBits, BitmapSize, AVIIF_KEYFRAME, Dummy, Dummy) <>AVIERR_OK then
                  raise Exception.Create('Failed to add frame to AVI');
              end;
            finally
              ProgressBar.Hide;
            end;
          finally
            if (BitmapInfo <> nil) then
              FreeMem(BitmapInfo);
            if (BitmapBits <> nil) then
              FreeMem(BitmapBits);
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
{$ifndef VER90}
    // Load and view AVI
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
  finally
    Screen.Cursor := crDefault;
    ButtonAbort.Hide;
    ButtonOpen.Enabled := True;
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
    ProgressBar.Show;
  end else
  if (Stage = psEnding) then
  begin
    ProgressBar.Position := 0;
    ProgressBar.Hide;
  end else
    ProgressBar.Position := PercentDone;
end;

end.


