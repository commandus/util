unit wbmpimage;
(*##*)
(*******************************************************************
*                                                                  *
*   W  B  M  P  I  M  A  G  E                                     *
*   classes and roitines to handle wireless bitmap                 *
*                                                                 *
*   Copyright (c) 2001 Andrei Ivanov. All rights reserved.         *
*                                                                 *
*   Conditional defines:                                           *
*                                                                 *
*   Last Revision: Jun 07 2001                                     *
*   Last fix     :                                                *
*   Lines        :                                                 *
*   History      :                                                *
*   Printed      : ---                                             *
*                                                                 *
********************************************************************)
(*##*)

{$DEFINE USE_GIFIMAGE}

interface

uses
  sysutils, Windows, Graphics, Classes
{$IFDEF USE_GIFIMAGE}
  , GifImage
{$ENDIF}
  ;

type
  // TWBMPImage mostly throws exceptions of type GIFException
  EWBMPException = class(EInvalidGraphic);

  TTransformOptions = set of (toAlign8, toNegative, toFlipH, toFlipV, toUnregistered);

  // Severity level as indicated in the Warning methods and the OnWarning event
  TWBMPImage = class(TGraphic)
  private
    FHeader,
    FData: String;
    FWidth, FHeight: Integer;
    FBitmap: TBitmap;
    FOnProgress: TProgressEvent;
    FDitherMode: TDitherMode;        // used in Assign method, dmNearest dmFloydSteinberg dmStucki dmSierra dmJaJuNI dmSteveArche dmBurkes
    FTransformOptions: TTransformOptions;
    procedure SetTransformOptions(AValue: TTransformOptions);
    procedure BuildHeader;
    procedure HeaderData2Bitmap;
    procedure ReduceBitmap1Bit(ABitmap: TBitmap);
    procedure Bitmap2HeaderData;
    procedure ProcessData;
    procedure Clear;
  protected
    function GetHeight: Integer; override;
    procedure SetHeight(Value: Integer); override;
    function GetWidth: Integer; override;
    procedure SetWidth(Value: Integer); override;
    procedure Draw(ACanvas: TCanvas; const Rect: TRect); override;
    function GetEmpty: Boolean; override;
    function Equals(Graphic: TGraphic): Boolean; override;
    procedure WriteData(Stream: TStream); override;
    function GetTransparent: Boolean; override;
    procedure SetTransparent(Value: Boolean); override;
    function GetBitmap: TBitmap;
    function GetPalette: HPALETTE; override;
    procedure SetPalette(Value: HPalette); override;

    procedure AssignTo(Dest: TPersistent); override;

    procedure Progress(Sender: TObject; Stage: TProgressStage;
      PercentDone: Byte;  RedrawNow: Boolean; const R: TRect; const Msg: string); override;
  public
    property Bitmap: TBitmap read GetBitmap; // Volatile - beware!
    property TransformOptions: TTransformOptions read FTransformOptions write SetTransformOptions;
    constructor Create; override;
    destructor Destroy; override;

    procedure SaveToStream(Stream: TStream); override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure LoadFromClipboardFormat(AFormat: Word; AData: THandle;
      APalette: HPALETTE); override;
    procedure SaveToClipboardFormat(var AFormat: Word; var AData: THandle;
      var APalette: HPALETTE); override;
    procedure Assign(Source: TPersistent); override;
    property OnProgress: TProgressEvent read FOnProgress write FOnProgress;
    property DitherMode: TDitherMode read FDitherMode write FDitherMode;   // used in Assign method
  end;

  // utility functions
  // get dimensions of wbmp stream
  function GetWBMPSizeStream(AStream: TStream; var ADimensions: TPoint): Boolean;
  // get dimensions of wbmp file
  function GetWBMPSizeFile(const AFileName: String; var ADimensions: TPoint): Boolean;

type
  CR = packed record
  case Integer of
  0:(
    R: Byte;
    G: Byte;
    B: Byte;
    P: Byte;
    );
  1:(
    C: TColor;
    );
  2:(
    v: packed array[0..3] of Byte;
    );

  end;

resourcestring
  // WBMP Error messages
  sOutOfData                 = 'Unexpexted end of data';
  sUnsupportedType           = 'This type is not supported';
  sBadSignature              = 'Invalid WBMP signature';
  sInvalidData               = 'Invalid WBMP data';
  sFailedPaste               = 'Failed to store WBMP on clipboard';
  sWBMPImageFile             = 'WBMP Image';
  sProgressLoading           = 'Loading WBMP';
  sProgressLoadingDone       = 'Loading WBMP done';
  sProgressCreating          = 'Creating WBMP';
  sProgressCreatingDone      = 'Creating WBMP done';
  sProgressConvertBitmap     = 'Converting to WBMP';
  sProgressConvertBitmapDone = 'Converting to WBMP done';
  sProgressImport            = 'Import image';
  sProgressImportDone        = 'Import image done';
  sProgressReduce            = 'Reduce image';
  sProgressReduceDone        = 'Reduce image done';

var
  CF_WBMP: Word;

implementation

uses
  messages, Consts;

type
  BA = array[0..1] of Byte;
  LogPal = record
    lpal: TLogPalette;
    dummy: packed array[1..255] of TPaletteEntry;
  end;

constructor TWBMPImage.Create;
begin
  inherited Create;
  FBitmap:= Nil;
  FDitherMode:= dmNearest;
  FTransformOptions:= [toAlign8];
  Clear;
end;

destructor TWBMPImage.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TWBMPImage.Clear;
begin
  FHeader:= '';
  FData:= '';
  FWidth:= 0;
  FHeight:= 0;
  if Assigned(FBitmap)
  then FBitmap.Free;
  FBitmap:= Nil;
  Palette:= 0;
end;

{$IFDEF USE_GIFIMAGE}
procedure TWBMPImage.ReduceBitmap1Bit(ABitmap: TBitmap);
const
  ColorReduction: TColorReduction = rmPalette; // rmMonochrome;
  ReductionBits: Integer = 1;
var
  SysPal: LogPal;
  bmp: TBitmap;
begin
  Progress(Self, psStarting, 0, False, Rect(0,0,0,0), sProgressReduce);
  SysPal.lPal.palVersion:= $300;
  SysPal.lPal.palNumEntries:= 2;
  with SysPal.lpal.palPalEntry[0] do begin
    peRed:= 0;
    peGreen:= 0;
    peBlue:= 0;
    peFlags:= PC_NOCOLLAPSE;
  end;
  with SysPal.dummy[1] do begin
    peRed:= $FF;
    peGreen:= $FF;
    peBlue:= $FF;
    peFlags:= PC_NOCOLLAPSE;
  end;
  bmp:= GifImage.ReduceColors(ABitmap, ColorReduction, DitherMode, ReductionBits, CreatePalette(SysPal.lpal));
  ABitmap.Assign(bmp);
  bmp.Free;
  with ABitmap do begin
    HandleType:= bmDIB;
    PixelFormat:= pf1bit;
    Palette:= CreatePalette(SysPal.lpal);
  end;
  Progress(Self, psEnding, 0, False, Rect(0,0,0,0), sProgressReduceDone);
end;

{$ELSE}

procedure TWBMPImage.ReduceBitmap1Bit(ABitmap: TBitmap);
var
  SysPal: LogPal;
begin
  Progress(Self, psStarting, 0, False, Rect(0,0,0,0), sProgressReduce);
  with ABitmap do begin
    {
    rowlen:= Width div 8;
    if (Width mod 8)>0
    then Inc(rowlen);
    }
    ABitmap.Monochrome:= True;
    with ABitmap.Canvas do begin
    {
      for y:= 0 to Height - 1 do begin
        Progress(Self, psRunning, 100* (y + 1) div Height, False, Rect(0,0,0,0), sProgressReduce);
        for x:= 0 to Width - 1 do begin
          with CR(Pixels[x, y]) do begin
            if v[0]+v[1]+v[3] < 3 * $FF div 2
            then Pixels[x, y]:= $0
            else Pixels[x, y]:= $FFFFFF;
          end;
        end;
      end;
      }
    end;
    HandleType:= bmDIB;
    PixelFormat:= pf1bit;
    SysPal.lPal.palVersion:= $300;
    SysPal.lPal.palNumEntries:= 2;
    with SysPal.lpal.palPalEntry[0] do begin
      peRed:= 0;
      peGreen:= 0;
      peBlue:= 0;
      peFlags:= PC_NOCOLLAPSE;
    end;
    with SysPal.dummy[1] do begin
      peRed:= $FF;
      peGreen:= $FF;
      peBlue:= $FF;
      peFlags:= PC_NOCOLLAPSE;
    end;
    Palette:= CreatePalette(SysPal.lpal);
  end;
  Progress(Self, psEnding, 0, False, Rect(0,0,0,0), sProgressReduceDone);
end;
{$ENDIF}

procedure TWBMPImage.HeaderData2Bitmap;
var
  y, rowlen: Integer;
  SysPal: LogPal;
begin
  Progress(Self, psStarting, 0, False, Rect(0,0,0,0), sProgressImport);
  rowlen:= FWidth div 8;
  if (FWidth mod 8)>0
  then Inc(rowlen);
  with FBitmap do begin
    HandleType:= bmDIB;
    PixelFormat:= pf1bit;
    SysPal.lPal.palVersion:= $300;
    SysPal.lPal.palNumEntries:= 2;
    with SysPal.lpal.palPalEntry[0] do begin
      peRed:= 0;
      peGreen:= 0;
      peBlue:= 0;
      peFlags:= 0;
    end;
    with SysPal.dummy[1] do begin
      peRed:= $FF;
      peGreen:= $FF;
      peBlue:= $FF;
      peFlags:= 0;
    end;
    Palette:= CreatePalette(SysPal.lpal);
    Width:= FWidth;
    Height:= FHeight;
    with Canvas do begin
      for y:= 0 to FHeight - 1 do begin
        Progress(Self, psRunning, 100* (y + 1) div FHeight, False, Rect(0,0,0,0), sProgressImport);
        Move(FData[(y * rowlen)+1], ScanLine[y]^, FWidth div 8);
      end;
    end;
  end;
  Progress(Self, psEnding, 0, False, Rect(0,0,0,0), sProgressImportDone);
end;

const
  stamp: packed array[0..4,0..30] of Byte = (
 (0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
 (0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
 (1, 1, 0, 1, 0, 1, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 1, 0, 1, 0, 1, 1, 0, 0, 1, 0, 1, 0, 0, 1, 1),
 (1, 0, 0, 1, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 0, 1, 0, 1, 1),
 (1, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 1, 1, 0, 1, 0, 1, 0, 0, 1, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0));

procedure TWBMPImage.SetTransformOptions(AValue: TTransformOptions);
var
  w, x, y, x0, y0, h: Integer;
begin
  FTransformOptions:= AValue;
  if not Assigned(FBitmap) then Exit;
  // transform bitmap
  if (toAlign8 in FTransformOptions) and ((FWidth and $7)>0) then begin
    w:= FBitmap.Width and $FFFFFFF8;
    if w = 0
    then w:= 8;
    FBitmap.Width:= w;
    FWidth:= w;
  end;

  h:= FBitmap.Height;
  w:= FBitmap.Width;

  if (toNegative in FTransformOptions) then begin
    for y:= 0 to h do begin
      for x:= 0 to w do begin
        with FBitmap.Canvas, CR(Pixels[x, y]) do begin
          if v[0]+v[1]+v[3] >= 3 * $FF div 2
          then Pixels[x, y]:= $0
          else Pixels[x, y]:= $FFFFFF;
        end;
      end;
    end;
  end;

  { add stamp to unregistered version }
  if (toUnregistered in FTransformOptions) then begin
    if (h >= 3 * 5) and (w >= 31) then begin
      y0:= h - 5;
      x0:= w - 31;
      for y:= 0 to 4 do begin
        for x:= 0 to 30 do begin
          with FBitmap.Canvas do begin
            if stamp[y, x] > 0
            then Pixels[x+x0, y+y0]:= $0
            else Pixels[x+x0, y+y0]:= $FFFFFF;
          end;
        end;
      end;
    end;
  end;
end;

procedure TWBMPImage.Bitmap2HeaderData;
var
  y, rowlen: Integer;
  SysPal: LogPal;
begin
  Progress(Self, psStarting, 0, False, Rect(0,0,0,0), sProgressConvertBitmap);
  with FBitmap do begin
    HandleType:= bmDIB;
    PixelFormat:= pf1bit;
    SysPal.lPal.palVersion:= $300;
    SysPal.lPal.palNumEntries:= 2;
    with SysPal.lpal.palPalEntry[0] do begin
      peRed:= 0;
      peGreen:= 0;
      peBlue:= 0;
      peFlags:= 0;
    end;
    with SysPal.dummy[1] do begin
      peRed:= $FF;
      peGreen:= $FF;
      peBlue:= $FF;
      peFlags:= 0;
    end;
    Palette:= CreatePalette(SysPal.lpal);
    FWidth:= Width;
    FHeight:= Height;
    BuildHeader;
    rowlen:= FWidth div 8;
    if (FWidth mod 8)>0
    then Inc(rowlen);
    SetLength(FData, rowlen * FHeight);
    FillChar(FData[1], Length(FData), 0);
    with Canvas do begin
      for y:= 0 to FHeight - 1 do begin
        Progress(Self, psRunning, 100 * (y + 1) div FHeight, False, Rect(0,0,0,0), sProgressConvertBitmap);
        Move(ScanLine[y]^, FData[(y * rowlen)+1], FWidth div 8);
      end;
    end;
  end;
  Progress(Self, psEnding, 100, False, Rect(0,0,0,0), sProgressConvertBitmapDone);
end;

type
  CBR = packed array [0..SizeOf(Cardinal)-1] of Byte;

// Read multibyte value from string, returns value and index to rest of string.
function ReadMb(const AString: String; var APosition: Integer): Integer;
var
  ch: Byte;
begin
  Result:= 0;
  repeat
    ch:= Byte(AString[APosition]);
    Result:= (Result shl 7) or (ch and $7f);
    Inc(APosition);
  until (ch and $80) = 0;
end;

// Convert value to a multi-byte encoded string.
function MkMb(AValue: Integer): String;
var
  o: Integer;
begin
  Result:= '';
  o:= 1;
  repeat
    Result:= Char((AValue and $7F) or $80) + Result;
    AValue:= AValue shr 7;
    Inc(o);
  until AValue = 0;
  Byte(Result[o-1]):= Byte(Result[o-1]) and $7F; // skip bit 7
end;

procedure TWBMPImage.ProcessData;
var
  BmpStart: Integer;
  rowlen: Integer;
begin
  if Length(FData) < 5 then begin
    raise EWBMPException.CreateFmt(sBadSignature, []);
  end;
  if Byte(FData[1]) <> 0 then begin
    raise EWBMPException.CreateFmt(sUnsupportedType, []);
  end;
  if Byte(FData[2]) <> 0 then begin
    raise EWBMPException.CreateFmt(sUnsupportedType, []);
  end;
  FWidth:= 0;
  FHeight:= 0;
  // read width and height (multi-byte values)
  BmpStart:= 3;
  FWidth:= ReadMb(FData, BmpStart);
  FHeight:= ReadMb(FData, BmpStart);
  Dec(BmpStart);
  rowlen:= FWidth div 8;
  if (FWidth mod 8)>0
  then Inc(rowlen);
  if Length(FData) - BmpStart + 1 < (FHeight * rowlen)
  then raise EWBMPException.CreateFmt(sOutOfData, []);
  FHeader:= Copy(FData, 1, BmpStart);
  Delete(Fdata, 1, BmpStart);
  // create bitmap
  if Assigned(FBitmap)
  then FBitmap.Free;
  FBitmap:= TBitmap.Create;
  HeaderData2Bitmap;
end;

procedure TWBMPImage.BuildHeader;
begin
  FHeader:= #0#0+MkMb(FWidth)+MkMb(FHeight);
end;

procedure TWBMPImage.LoadFromStream(Stream: TStream);
var
  n: Integer;
  Position: Integer;
begin
  Progress(Self, psStarting, 0, False, Rect(0,0,0,0), sProgressLoading);
  n:= 0;
  try
    // Zap old image
    Clear;
    Position:= Stream.Position;
    try
      // Read all data.
      n:= Stream.Size - Stream.Position;
      SetLength(FData, n);
      Stream.Read(FData[1], n);
      ProcessData; // parse data to FHeader and FData
//    SetTransformOptions(FTransformOptions);
    except
      // Restore stream position in case of error.
      // Not required, but "a nice thing to do"
      Stream.Position:= Position;
      raise;
    end;
  finally
    Progress(Self, psRunning, 100, False, Rect(0,0,0,0), sProgressLoading);
    Progress(Self, psEnding, n, True, Rect(1,1,FWidth,FHeight), sProgressLoadingDone);
    Changed(Self);
  end;
end;

function TWBMPImage.GetEmpty: Boolean;
begin
  Result:= Length(FData) = 0;
end;

procedure TWBMPImage.SaveToStream(Stream: TStream);
begin
  try
    // Write header
    Stream.Write(FHeader[1], Length(FHeader));
    // write all data.
    Stream.Write(FData[1], Length(FData));
  finally
  end;
end;

function TWBMPImage.GetHeight: Integer;
begin
  Result:= FHeight;
end;

function TWBMPImage.GetWidth: Integer;
begin
  Result:= FWidth;
end;

function TWBMPImage.GetTransparent: boolean;
begin
  Result:= False;
end;

procedure TWBMPImage.SetTransparent(Value: Boolean);
begin
end;

function TWBMPImage.GetPalette: HPALETTE;
begin
  // Use bitmaps own palette if possible
  if (FBitmap <> nil) and (FBitmap.Palette <> 0)
  then Result:= FBitmap.Palette
  else Result:= 0;
end;

procedure TWBMPImage.SetPalette(Value: HPalette);
begin
  Changed(Self);
end;

procedure TWBMPImage.SetHeight(Value: Integer);
begin
  if (Value <> FHeight) then begin
    FHeight:= Value;
    BuildHeader;
    Changed(self);
  end;
end;

procedure TWBMPImage.SetWidth(Value: Integer);
begin
  if (Value <> FWidth) then begin
    FWidth:= Value;
    BuildHeader;
    Changed(self);
  end;
end;

procedure TWBMPImage.WriteData(Stream: TStream);
begin
  inherited WriteData(Stream);
end;

procedure TWBMPImage.Assign(Source: TPersistent);
var
  w: Integer;
begin
  if (Source = self)
  then Exit;
  Progress(Self, psStarting, 100, True, Rect(0,0,0,0), sProgressCreating);
  if (Source = nil)
  then Clear
  else begin // TWBMPImage import
    Clear;
    if (Source is TWBMPImage) then begin
      FHeader:= TWBMPImage(Source).FHeader;
      FData:= TWBMPImage(Source).FData;
      FWidth:= TWBMPImage(Source).FWidth;
      FHeight:= TWBMPImage(Source).FHeight;
      // if (toAlign8 in FTransformOptions) then
      FBitmap:= TBitmap.Create;
      HeaderData2Bitmap;
    end else begin
      FBitmap:= TBitmap.Create;
      if (Source is TMetafile) or (Source is TIcon) then begin
        FBitmap.Width:= TGraphic(Source).Width;
        FBitmap.Height:= TGraphic(Source).Height;
        FBitmap.Canvas.Draw(0, 0, TGraphic(Source));
      end else begin
        try
          FBitmap.Assign(Source);
        except
          on E: EConvertError do begin
            inherited Assign(Source);
          end;
        end;
      end;
      ReduceBitmap1Bit(FBitmap);
      SetTransformOptions(FTransformOptions);
      Bitmap2HeaderData;
    end;
  end;
  Progress(Self, psEnding, 100, True, Rect(0,0,0,0), sProgressCreatingDone);
end;

procedure TWBMPImage.AssignTo(Dest: TPersistent);
begin
  if (Dest is TWBMPImage)
  then Dest.Assign(FBitmap) else

  if (Dest is TBitmap)
  then Dest.Assign(FBitmap)
  else inherited AssignTo(Dest); // AssignTo(Dest);
end;

procedure TWBMPImage.LoadFromClipboardFormat(AFormat: Word; AData: THandle;
  APalette: HPALETTE);
var
  Size: Longint;
  Buffer: Pointer;
  Stream: TMemoryStream;
  Bmp: TBitmap;
begin
  if (AData = 0)
  then AData:= GetClipboardData(AFormat);
  if (AData <> 0) and (AFormat = CF_WBMP) then begin
    // Get size and pointer to data
    Size:= GlobalSize(AData);
    Buffer:= GlobalLock(AData);
    try
      Stream := TMemoryStream.Create;
      try
        // Copy data to a stream
        Stream.SetSize(Size);
        Move(Buffer^, Stream.Memory^, Size);
        // Load GIF from stream
        LoadFromStream(Stream);
      finally
        Stream.Free;
      end;
    finally
      GlobalUnlock(AData);
    end;
  end else begin
    if (AData <> 0) and (AFormat = CF_BITMAP) then begin
      // No GIF on clipboard - try loading a bitmap instead
      Bmp := TBitmap.Create;
      try
        Bmp.LoadFromClipboardFormat(AFormat, AData, APalette);
        Assign(Bmp);
      finally
        Bmp.Free;
      end;
    end else raise EWBMPException.CreateFmt(sUnknownClipboardFormat, []);
  end;
end;

procedure TWBMPImage.SaveToClipboardFormat(var AFormat: Word; var AData: THandle;
  var APalette: HPALETTE);
var
  Stream: TMemoryStream;
  Data: THandle;
  Buffer: Pointer;
begin
  if Empty
  then exit;
  // First store a bitmap version on the clipboard...
  GetBitmap.SaveToClipboardFormat(AFormat, AData, APalette);
  // ...then store a GIF
  Stream:= TMemoryStream.Create;
  try
    // Save the WBMP to a memory stream
    SaveToStream(Stream);
    Stream.Position:= 0;
    // Allocate some memory for the WBMP data
    Data:= GlobalAlloc(HeapAllocFlags, Stream.Size);
    try
      if (Data <> 0) then begin
        Buffer:= GlobalLock(Data);
        try
          // Copy WBMP data from stream memory to clipboard memory
          Move(Stream.Memory^, Buffer^, Stream.Size);
        finally
          GlobalUnlock(Data);
        end;
        // Transfer data to clipboard
        if (SetClipboardData(CF_WBMP, Data) = 0)
        then raise EWBMPException.CreateFmt(sFailedPaste, []);
      end;
    except
      GlobalFree(Data);
      raise;
    end;
  finally
    Stream.Free;
  end;
end;

procedure TWBMPImage.Draw(ACanvas: TCanvas; const Rect: TRect);
begin
  ACanvas.Draw(Rect.Left, Rect.Top, FBitmap);
end;

function TWBMPImage.GetBitmap: TBitmap;
begin
  if not Empty then begin
    Result:= FBitmap;
    if Assigned(Result)
    then Exit;
    // create new bitmap FBitmap
    FBitmap:= TBitmap.Create;
    Result:= FBitmap;
    FBitmap.OnChange:= Changed;
    // Use first image as default
    if (FWidth = Width) and (FHeight = Height) then begin
      // Use first image as it has same dimensions
      // FBitmap.Assign(Images[0].Bitmap);
    end else begin
      // Draw first image on bitmap
    end;
  end else Result:= nil
end;

function TWBMPImage.Equals(Graphic: TGraphic): Boolean;
begin
  Result := (Graphic = self);
end;

procedure TWBMPImage.Progress(Sender: TObject; Stage: TProgressStage;
  PercentDone: Byte; RedrawNow: Boolean; const R: TRect; const Msg: string);
begin
  if Assigned(FOnProgress)
  then FOnProgress(Sender, Stage, PercentDone, RedrawNow, R, Msg);
end;

// utility functions -----------------------------------------------------------

// get dimensions of wbmp stream
function GetWBMPSizeStream(AStream: TStream; var ADimensions: TPoint): Boolean;
var
  BmpStart: Integer;
  rowlen: Integer;
  FData: String;
begin
  Result:= False;
  ADimensions.X:= 0;
  ADimensions.Y:= 0;
  try
    SetLength(FData, AStream.Size);
    AStream.Read(FData[1], Length(FData));
  except
    Exit;
  end;
  if Length(FData) < 5 then begin
    Exit;
  end;
  if Byte(FData[1]) <> 0 then begin
    Exit;
  end;
  if Byte(FData[2]) <> 0 then begin
    Exit;
  end;

  // read width and height (multi-byte values)
  BmpStart:= 3;
  ADimensions.X:= ReadMb(FData, BmpStart);
  ADimensions.Y:= ReadMb(FData, BmpStart);
  Dec(BmpStart);
  Result:= True;
end;

// get dimensions of wbmp file
function GetWBMPSizeFile(const AFileName: String; var ADimensions: TPoint): Boolean;
var
  Strm: TStream;
begin
  Result:= False;
  try
    Strm:= TFileStream.Create(AFileName, fmOpenRead);
  except
    Exit;
  end;
  Result:= GetWBMPSizeStream(Strm, ADimensions);
  Strm.Free;
end;

initialization
  TPicture.RegisterFileFormat('WBMP', sWBMPImageFile, TWBMPImage);
  CF_WBMP:= Windows.RegisterClipboardFormat(PChar(sWBMPImageFile));
  TPicture.RegisterClipboardFormat(CF_WBMP, TWBMPImage);

finalization
  TPicture.UnregisterGraphicClass(TWBMPImage);
end.
