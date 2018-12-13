unit
  GDIPL2;
(*##*)
(*******************************************************************************
*                                                                             *
*   G  D  I  +  2                                                              *
*   Delphi wrapper of GDI+ 2                                                  *
*   Copyright (c) 2006-2007, Andrei Ivanov, RealThinComponents                 *
*                                                                             *
*   Conditional defines:                                                       *
*                                                                             *
*   Revisions    : Aug 11 2006                                                 *
*   Last revision: Sep 01 2007                                                *
*   Lines        : 597                                                         *
*   History      :                                                            *
*     2007 Nov 08 TImage descendant class removed                              *
*                                                                             *
*   Printed      : ---                                                         *
*                                                                             *
********************************************************************************)
(*##*)

interface

uses
  Windows, SysUtils, Classes, ActiveX;

const
  ptGpsVer = $0000;
  ptGpsLatitudeRef = $0001;
  ptGpsLatitude = $0002;
  ptGpsLongitudeRef = $0003;
  ptGpsLongitude = $0004;
  ptGpsAltitudeRef = $0005;
  ptGpsAltitude = $0006;
  ptGpsGpsTime = $0007;
  ptGpsGpsSatellites = $0008;
  ptGpsGpsStatus = $0009;
  ptGpsGpsMeasureMode = $000A;
  ptGpsGpsDop = $000B;
  ptGpsSpeedRef = $000C;
  ptGpsSpeed = $000D;
  ptGpsTrackRef = $000E;
  ptGpsTrack = $000F;
  ptGpsImgDirRef = $0010;
  ptGpsImgDir = $0011;
  ptGpsMapDatum = $0012;
  ptGpsDestLatRef = $0013;
  ptGpsDestLat = $0014;
  ptGpsDestLongRef = $0015;
  ptGpsDestLong = $0016;
  ptGpsDestBearRef = $0017;
  ptGpsDestBear = $0018;
  ptGpsDestDistRef = $0019;
  ptGpsDestDist = $001A;

  ptNewSubfileType = $00FE;
  ptSubfileType = $00FF;
  ptImageWidth = $0100;
  ptImageHeight = $0101;
  ptBitsPerSample = $0102 ;
  ptCompression = $0103 ;
  ptPhotometricInterp = $0106 ;
  ptThreshHolding = $0107 ;
  ptCellWidth = $0108 ;
  ptCellHeight = $0109 ;
  ptFillOrder = $010A ;

  ptDocumentName = $010D ;
  ptImageDescription = $010E ;
  ptEquipMake = $010F ;
  ptEquipModel = $0110 ;
  ptStripOffsets = $0111 ;
  ptOrientation = $0112 ;
  ptSamplesPerPixel = $0115 ;
  ptRowsPerStrip = $0116 ;
  ptStripBytesCount = $0117 ;
  ptMinSampleValue = $0118 ;
  ptMaxSampleValue = $0119 ;
  ptXResolution = $011A ;
  ptYResolution = $011B ;
  ptPlanarConfig = $011C ;
  ptPageName = $011D ;
  ptXPosition = $011E ;
  ptYPosition = $011F ;
  ptFreeOffset = $0120 ;
  ptFreeByteCounts = $0121 ;
  ptGrayResponseUnit = $0122 ;
  ptGrayResponseCurve = $0123 ;
  ptT4Option = $0124 ;
  ptT6Option = $0125 ;
  ptResolutionUnit = $0128;
  ptPageNumber = $0129 ;
  ptTransferFunction = $012D ;
  ptSoftwareUsed = $0131 ;
  ptDateTime = $0132 ;
  ptArtist = $013B ;
  ptHostComputer = $013C ;
  ptPredictor = $013D ;
  ptWhitePoint = $013E ;
  ptPrimaryChromaticities = $013F ;
  ptColorMap = $0140 ;
  ptHalftoneHints = $0141 ;
  ptTileWidth = $0142 ;
  ptTileLength = $0143 ;
  ptTileOffset = $0144 ;
  ptTileByteCounts = $0145 ;
  ptInkSet = $014C ;
  ptInkNames = $014D ;
  ptNumberOfInks = $014E ;
  ptDotRange = $0150 ;
  ptTargetPrinter = $0151 ;
  ptExtraSamples = $0152 ;
  ptSampleFormat = $0153 ;
  ptSMinSampleValue = $0154 ;
  ptSMaxSampleValue = $0155 ;
  ptTransferRange = $0156 ;
  ptJPEGProc = $0200 ;
  ptJPEGInterFormat = $0201 ;
  ptJPEGInterLength = $0202 ;
  ptJPEGRestartInterval = $0203 ;
  ptJPEGLosslessPredictors = $0205 ;
  ptJPEGPointTransforms = $0206 ;
  ptJPEGQTables = $0207 ;
  ptJPEGDCTables = $0208 ;
  ptJPEGACTables = $0209 ;
  ptYCbCrCoefficients = $0211 ;
  ptYCbCrSubsampling = $0212 ;
  ptYCbCrPositioning = $0213 ;
  ptREFBlackWhite = $0214 ;
  ptGamma = $0301;
  ptICCProfileDescriptor = $0302 ;
  ptSRGBRenderingIntent = $0303 ;
  ptImageTitle = $0320 ;
  ptResolutionXUnit = $5001 ;
  ptResolutionYUnit = $5002 ;
  ptResolutionXLengthUnit = $5003 ;
  ptResolutionYLengthUnit = $5004 ;
  ptPrintFlags = $5005 ;
  ptPrintFlagsVersion = $5006 ;
  ptPrintFlagsCrop = $5007 ;
  ptPrintFlagsBleedWidth = $5008 ;
  ptPrintFlagsBleedWidthScale = $5009 ;
  ptHalftoneLPI = $500A ;
  ptHalftoneLPIUnit = $500B ;
  ptHalftoneDegree = $500C ;
  ptHalftoneShape = $500D ;
  ptHalftoneMisc = $500E ;
  ptHalftoneScreen = $500F ;
  ptJPEGQuality = $5010 ;
  ptGridSize = $5011 ;
  ptThumbnailFormat = $5012 ;
  ptThumbnailWidth = $5013 ;
  ptThumbnailHeight = $5014 ;
  ptThumbnailColorDepth = $5015 ;
  ptThumbnailPlanes = $5016 ;
  ptThumbnailRawBytes = $5017 ;
  ptThumbnailSize = $5018 ;
  ptThumbnailCompressedSize = $5019 ;
  ptColorTransferFunction = $501A ;
  ptThumbnailData = $501B ;
  ptThumbnailImageWidth = $5020 ;
  ptThumbnailImageHeight = $5021 ;
  ptThumbnailBitsPerSample = $5022 ;
  ptThumbnailCompression = $5023 ;
  ptThumbnailPhotometricInterp = $5024 ;
  ptThumbnailImageDescription = $5025 ;
  ptThumbnailEquipMake = $5026 ;
  ptThumbnailEquipModel = $5027 ;
  ptThumbnailStripOffsets = $5028;
  ptThumbnailOrientation = $5029 ;
  ptThumbnailSamplesPerPixel = $502A ;
  ptThumbnailRowsPerStrip = $502B ;
  ptThumbnailStripBytesCount = $502C ;
  ptThumbnailResolutionX = $502D ;
  ptThumbnailResolutionY = $502E ;
  ptThumbnailPlanarConfig = $502F ;
  ptThumbnailResolutionUnit = $5030 ;
  ptThumbnailTransferFunction = $5031 ;
  ptThumbnailSoftwareUsed = $5032 ;
  ptThumbnailDateTime = $5033 ;
  ptThumbnailArtist = $5034 ;
  ptThumbnailWhitePoint = $5035 ;
  ptThumbnailPrimaryChromaticities = $5036 ;
  ptThumbnailYCbCrCoefficients = $5037 ;
  ptThumbnailYCbCrSubsampling = $5038 ;
  ptThumbnailYCbCrPositioning = $5039 ;
  ptThumbnailRefBlackWhite = $503A ;
  ptThumbnailCopyRight = $503B ;
  ptLuminanceTable = $5090 ;
  ptChrominanceTable = $5091 ;
  ptFrameDelay = $5100 ;
  ptLoopCount = $5101 ;
  ptGlobalPalette = $5102 ;
  ptIndexBackground = $5103 ;
  ptIndexTransparent = $5104 ;
  ptPixelUnit = $5110 ;
  ptPixelPerUnitX = $5111 ;
  ptPixelPerUnitY = $5112 ;
  ptPaletteHistogram = $5113 ;
  ptCopyright = $8298 ;
  ptExifExposureTime = $829A ;
  ptExifFNumber = $829D ;
  ptExifIFD = $8769 ;
  ptICCProfile = $8773 ;
  ptExifExposureProg = $8822 ;
  ptExifSpectralSense = $8824 ;
  ptGpsIFD = $8825 ;
  ptExifISOSpeed = $8827;
  ptExifOECF = $8828 ;
  ptExifVer = $9000 ;
  ptExifDTOrig = $9003 ;
  ptExifDTDigitized = $9004 ;
  ptExifCompConfig = $9101 ;
  ptExifCompBPP = $9102 ;
  ptExifShutterSpeed = $9201 ;
  ptExifAperture = $9202 ;
  ptExifBrightness = $9203 ;
  ptExifExposureBias = $9204 ;
  ptExifMaxAperture = $9205 ;
  ptExifSubjectDist = $9206 ;
  ptExifMeteringMode = $9207 ;
  ptExifLightSource = $9208 ;
  ptExifFlash = $9209 ;
  ptExifFocalLength = $920A ;
  ptExifMakerNote = $927C ;
  ptExifUserComment = $9286 ;
  ptExifDTSubsec = $9290 ;
  ptExifDTOrigSS = $9291 ;
  ptExifDTDigSS = $9292 ;
  ptExifFPXVer = $A000 ;
  ptExifColorSpace = $A001 ;
  ptExifPixXDim = $A002 ;
  ptExifPixYDim = $A003 ;
  ptExifRelatedWav = $A004 ;
  ptExifInterop = $A005 ;
  ptExifFlashEnergy = $A20B ;
  ptExifSpatialFR = $A20C ;
  ptExifFocalXRes = $A20E ;
  ptExifFocalYRes = $A20F ;
  ptExifFocalResUnit = $A210 ;
  ptExifSubjectLoc = $A214 ;
  ptExifExposureIndex = $A215 ;
  ptExifSensingMethod = $A217 ;
  ptExifFileSource = $A300 ;
  ptExifSceneType = $A301 ;
  ptExifCfaPattern = $A302;

  // not sure about numbers: PixelFormat4bppIndexed
  PropertyTagTypeNothing   = 0;
  PropertyTagTypeByte      = 1;   // Specifies that the value data member is an array of bytes.
  PropertyTagTypeASCII     = 2;   // Specifies that the value data member is a null-terminated ASCII string. If you set the type data member of a PropertyItem object to PropertyTagTypeASCII, you should set the length data member to the length of the string including the NULL terminator. For example, the string HELLO would have a length of 6.
  PropertyTagTypeShort     = 3;   // Specifies that the value data member is an array of unsigned short (16-bit) integers.
  PropertyTagTypeLong      = 4;   // Specifies that the value data member is an array of unsigned long (32-bit) integers.
  PropertyTagTypeRational  = 5;   // Specifies that the value data member is an array of pairs of unsigned long integers. Each pair represents a fraction; the first integer is the numerator and the second integer is the denominator.
  PropertyTagTypeNothing6  = 6;
  PropertyTagTypeUndefined = 7;   // Specifies that the value data
  PropertyTagTypeNothing8  = 8;
  PropertyTagTypeSLONG     = 9;   // Specifies that the value data member is an array of signed long (32-bit) integers.
  PropertyTagTypeSRational = 10;   // Specifies that the value data member is an array of pairs of signed long integers. Each pair represents a fraction; the first integer is the numerator and the second integer is the denominator.
  PixelFormat4bppIndexed   = 11;   // Specifies that the format is 4 bits per pixel, indexed.

var
  GDIPlusActive: Boolean = False;

type
  TPropertyItem = record
    id: Integer;   // identifies the kind of metadata stored in this PropertyItem object. Constants that identify various kinds of metadata (PropertyTagEquipMake, PropertyTagEquipModel, and the like) are defined in Gdiplusimaging.h. The PROPID data type is defined in Wtypes.h.
    len: Cardinal; // Size, in bytes, of the value array.
    typ: Word;  // that identifies the data type of the values in the value array. Constants that identify various data types (PropertyTagTypeByte, PropertyTagTypeASCII, and the like) are defined in Gdiplusimaging.h.
    value: Pointer;
  end;

  TPropertyItems = array[0..0] of TPropertyItem;

  TPropItem = record
    id: Integer;        //  PROPID  Integer that identifies the kind of metadata stored in this PropertyItem object. Constants that identify various kinds of metadata (PropertyTagEquipMake, PropertyTagEquipModel, and the like) are defined in Gdiplusimaging.h. The PROPID data type is defined in Wtypes.h.
    length: Cardinal;   // ULONG  Size, in bytes, of the value array.
    typ: Word;          // Integer that identifies the data type of the values in the value array. Constants that identify various data types (PropertyTagTypeByte, PropertyTagTypeASCII, and the like) are defined in Gdiplusimaging.h.
    value: Pointer;     // Pointer to an array of values. Each value in the array has the data type specified by the type data member.
  end;
  TPropItems = array[0..0] of TPropItem;

  TImageCodecInfo = record
    Clsid: TCLSID;                  // Codec identifier.
    FormatID: TGUID;                // File format identifier. GUIDs that identify various file formats (ImageFormatBMP, ImageFormatEMF, and the like) are defined in Gdiplusimaging.h.
    CodecName: PWideChar;          // Pointer to a null-terminated string that contains the codec name.
    DllName: PWideChar;            // Pointer to a null-terminated string that contains the path name of the DLL in which the codec resides. If the codec is not in a DLL, this pointer is NULL.
    FormatDescription: PWideChar;  // Pointer to a null-terminated string that contains the name of the file format used by the codec.
    FilenameExtension: PWideChar;  // Pointer to a null-terminated string that contains all file-name extensions associated with the codec. The extensions are separated by semicolons.
    MimeType: PWideChar;           // Pointer to a null-terminated string that contains the mime type of the codec.
    Flags: DWORD;                  // Combination of flags from the ImageCodecFlags enumeration.
    Version: DWORD;                // Integer that indicates the version of the codec.
    SigCount: DWORD;               // Integer that indicates the number of signatures used by the file format associated with the codec.
    SigSize: DWORD;                // Integer that indicates the number of bytes in each signature.
    SigPattern: Pointer;           // Pointer to an array of bytes that contains the pattern for each signature.
    SigMask: Pointer;               // Pointer to an array of bytes that contains the mask for each signature.
  end;

  TImageCodecInfos = array[0..0] of TImageCodecInfo;

function InitGDIPlus: Boolean;
procedure DoneGDIPlus;
function GetImgTextProperty(AImg: Integer; AProperty: Integer): String;
function SetImgTextProperty(AImg: Integer; AProperty: Integer; AValue: String): Boolean;
function RemoveImgPropertyItem(AImg: Integer; AProperty: Integer): Boolean;
function LoadImageFromStream(const AStream: TStream; var Image: Integer): Integer;
function SaveImageToStream(const AStream: TStream; const Image: Integer;
  const AEncode: TClsId; AEncodeParameters: Pointer): Integer;

function PropertyValueAsString(AItem: TPropertyItem): String;

var
  GdiplusStartup: function(var Token: DWord; const Input, Output: Pointer): Integer; stdcall;
  GdiplusShutdown: procedure(Token: DWord); stdcall;
  GdipDeleteGraphics: function(Graphics: Integer): Integer; stdcall;
  GdipCreateFromHDC: function(hdc: HDC; var Graphics: Integer): Integer; stdcall;
  GdipDrawImageRectI: function(Graphics, Image, X, Y, Width, Height: Integer): Integer; stdcall;
  GdipLoadImageFromFile: function(const FileName: PWideChar; var Image: Integer): Integer; stdcall;
  GdipSaveImageToFile: function(Image: Integer; const FileName: PWideChar; const AEncode: TClsId; AEncodeParameters: Pointer): Integer; stdcall;
  GdipDisposeImage: function(Image: Integer): Integer; stdcall;
  GdipLoadImageFromStream: function(const AIStream: IStream; var Image: Integer): Integer; stdcall;
  GdipCreateBitmapFromResource: function(AHInstance: Windows.HINST;
    const ABitmapName: PWideChar; var Image: Integer): Integer; stdcall;

  GdipSaveImageToStream: function(Image: Integer; const AIStream: IStream; const AEncode: TClsId; AEncodeParameters: Pointer): Integer; stdcall;
  GdipGetImageWidth: function(Image: Integer; var Width: Integer): Integer; stdcall;
  GdipGetImageHeight: function(Image: Integer; var Height: Integer): Integer; stdcall;

  GdipGetPropertySize: function(image: Integer; var totalBufferSize, numProperties: Cardinal): Integer; stdcall;
  GdipGetPropertyItemSize: function(Image: Integer; ApropId: PropId; var size: Cardinal): Integer; stdcall;
  GdipGetPropertyItem: function(Image: Integer; APropId: PROPID; propSize: Cardinal; var AData): Integer; stdcall;
  GdipGetAllPropertyItems: function(Image: Integer; totalBufferSize: Cardinal; numProperties: Cardinal; var allItems): Integer; stdcall;
  GdipRemovePropertyItem: function(Image: Integer; ApropId: PROPID): Integer; stdcall;
  GdipSetPropertyItem: function(Image: Integer; const APropItem: TPropItem): Integer; stdcall;

  GdipGetImageEncoders: function(numEncoders, size: Cardinal; var encoders): Integer; stdcall;
  GdipGetImageEncodersSize: function(var numEncoders: Cardinal; var size: Cardinal): Integer; stdcall;
  GdipGetImageThumbnail: function (AImage: Integer; AThumbWidth, AThumbHeight: Cardinal;
    var AThumbImage: Integer; AGetThumbnailImageAbort: Pointer; ACallbackData: Pointer): Integer; stdcall;

  GdipGetDC: function (AGraphics: Integer; var ADC: HDC): Integer; stdcall;

function GetEncoderClsid(const AFormat: PWideChar; var AClsid: TClsId): Integer;

implementation

const
  GdiPlusLib = 'GdiPlus.dll';

type
  EGDIPlus = class(Exception);
  TRectF = record
    X: Single;
    Y: Single;
    Width: Single;
    Height: Single;
  end;

  TGDIStartup = packed record
    Version: Integer; // Must be one
    DebugEventCallback: Pointer; // Only for debug builds
    SuppressBackgroundThread: Bool; // True if replacing GDI+ background processing
    SuppressExternalCodecs: Bool; // True if only using internal codecs
  end;
  
var
  LibHandle: THandle;
  Err: Integer;
  InitToken: DWord;
  Startup: TGDIStartup;

function PropertyValueAsString(AItem: TPropertyItem): String;
var
  c, len: Integer;
  p: PChar;
begin
  Result:= '';
  c:= 0;
  while c < AItem.len do begin
    case AItem.typ of
      PropertyTagTypeByte: begin      // array of bytes.
          Result:= Result + IntToStr(Byte(AItem.value^));
          Inc(Cardinal(AItem.value));
          Inc(c);
        end;
      PropertyTagTypeASCII: begin     // null-terminated ASCII string. If you set the type data member of a PropertyItem object to PropertyTagTypeASCII, you should set the length data member to the length of the string including the NULL terminator. For example, the string HELLO would have a length of 6.
          len:= Length(PChar(AItem.value)) + 1;
          Result:= Result + PChar(AItem.value);
          Inc(Cardinal(AItem.value), len);
          Inc(c, len);
        end;
      PropertyTagTypeShort: begin     // array of unsigned short (16-bit) integers.
          Result:= Result + IntToStr(Word(AItem.value^));
          Inc(Cardinal(AItem.value), 2);
          Inc(c, 2);
        end;
      PropertyTagTypeLong: begin      // array of unsigned long (32-bit) integers.
          Result:= Result + IntToStr(LongWord(AItem.value^));
          Inc(Cardinal(AItem.value), 4);
          Inc(c, 4);
        end;
      PropertyTagTypeRational: begin  // an array of pairs of unsigned long integers. Each pair represents a fraction; the first integer is the numerator and the second integer is the denominator.
          Result:= Result + IntToStr(LongWord(AItem.value^)) + '.';
          Inc(c, 4);
          Inc(Cardinal(AItem.value), 4);
          if c < AItem.len then begin
            Result:= IntToStr(LongWord(AItem.value^));
            Inc(c, 4);
            Inc(Cardinal(AItem.value), 4);
          end;
        end;
      PropertyTagTypeSLONG: begin     // array of signed long (32-bit) integers.
          Result:= Result + IntToStr(LongInt(AItem.value^));
          Inc(Cardinal(AItem.value), 4);
          Inc(c, 4);
        end;
      PropertyTagTypeSRational: begin // array of pairs of signed long integers. Each pair represents a fraction; the first integer is the numerator and the second integer is the denominator.
          Result:= Result + IntToStr(LongInt(AItem.value^)) + '.';
          Inc(c, 4);
          Inc(Cardinal(AItem.value), 4);
          if c < AItem.len then begin
            Result:= Result + IntToStr(LongInt(AItem.value^));
            Inc(c, 4);
            Inc(Cardinal(AItem.value), 4);
          end;
        end;
      PixelFormat4bppIndexed: ;   // format is 4 bits per pixel, indexed.
      else Exit;
    end;
    // next array elenent
    if c < AItem.len
      then Result:= Result + ', ';
  end;
end;

// Retrieving the Class Identifier for an Encoder
function GetEncoderClsid(const Aformat: PWideChar; var AClsid: TClsId): Integer;
var
  c, enccount,            // number of image encoders
  size: Cardinal;         // size of the image encoder array in bytes
  ImageCodecInfos: Pointer; // TImageCodecInfos;
begin
  Result:= -1;
  GDIpl2.GdipGetImageEncodersSize(enccount, size);
  if (size = 0)
  then Exit;  // Failure
  GetMem(ImageCodecInfos, size);
  GDIpl2.GdipGetImageEncoders(enccount, size, ImageCodecInfos^);

  for c:= 0 to enccount - 1 do begin
    if (ANSICompareText(TImageCodecInfos(ImageCodecInfos^)[c].MimeType, AFormat) = 0) then begin
      AClsid:= TImageCodecInfos(ImageCodecInfos^)[c].Clsid;
      Result:= c;  // Success
      Break;
    end;
  end;
  FreeMem(ImageCodecInfos);
end;


function LoadImageFromStream(const AStream: TStream; var Image: Integer): Integer;
var
  adapter: IStream;
begin
  adapter:= TStreamAdapter.Create(AStream, soOwned);
  Result:= GdipLoadImageFromStream(adapter, Image);
end;

function SaveImageToStream(const AStream: TStream; const Image: Integer;
  const AEncode: TClsId; AEncodeParameters: Pointer): Integer;
var
  adapter: IStream;
  p: Int64;
begin
  adapter:= TStreamAdapter.Create(AStream, soReference);  // 
  // adapter.Seek(0, 0, p);
  Result:= GdipSaveImageToStream(Image, adapter, AEncode, AEncodeParameters);
  // adapter.Commit(1);
  // adapter._Release;
end;

function InitGDIPlus: Boolean;
begin
  Result:= True;
  if GDIPlusActive
  then Exit;
  GDIPlusActive:= False;
  LibHandle:= LoadLibrary(GdiPlusLib);
  if LibHandle <> 0 then begin
    @GdiplusStartup:= GetProcAddress(LibHandle, 'GdiplusStartup');
    @GdiplusShutdown:= GetProcAddress(LibHandle, 'GdiplusShutdown');
    @GdipDeleteGraphics:= GetProcAddress(LibHandle, 'GdipDeleteGraphics');
    @GdipCreateFromHDC:= GetProcAddress(LibHandle, 'GdipCreateFromHDC');
    @GdipDrawImageRectI:= GetProcAddress(LibHandle, 'GdipDrawImageRectI');
    @GdipLoadImageFromFile:= GetProcAddress(LibHandle, 'GdipLoadImageFromFile');
    @GdipSaveImageToFile:= GetProcAddress(LibHandle, 'GdipSaveImageToFile');
    @GdipLoadImageFromStream:= GetProcAddress(LibHandle, 'GdipLoadImageFromStream');
    @GdipCreateBitmapFromResource:= GetProcAddress(LibHandle, 'GdipCreateBitmapFromResource');

    @GdipSaveImageToStream:= GetProcAddress(LibHandle, 'GdipSaveImageToStream');

    @GdipDisposeImage:= GetProcAddress(LibHandle, 'GdipDisposeImage');
    @GdipGetImageWidth:= GetProcAddress(LibHandle, 'GdipGetImageWidth');
    @GdipGetImageHeight:= GetProcAddress(LibHandle, 'GdipGetImageHeight');

    @GdipGetPropertySize:= GetProcAddress(LibHandle, 'GdipGetPropertySize');
    @GdipGetPropertyItemSize:= GetProcAddress(LibHandle, 'GdipGetPropertyItemSize');
    @GdipGetPropertyItem:= GetProcAddress(LibHandle, 'GdipGetPropertyItem');
    @GdipGetAllPropertyItems:= GetProcAddress(LibHandle, 'GdipGetAllPropertyItems');
    @GdipRemovePropertyItem:= GetProcAddress(LibHandle, 'GdipRemovePropertyItem');
    @GdipSetPropertyItem:= GetProcAddress(LibHandle, 'GdipSetPropertyItem');

    @GdipGetImageEncoders:= GetProcAddress(LibHandle, 'GdipGetImageEncoders');
    @GdipGetImageEncodersSize:= GetProcAddress(LibHandle, 'GdipGetImageEncodersSize');
    @GdipGetImageThumbnail:= GetProcAddress(LibHandle, 'GdipGetImageThumbnail');

    @GdipGetDC:= GetProcAddress(LibHandle, 'GdipGetDC');

    FillChar(Startup, sizeof(Startup), 0);
    Startup.Version:= 1;
    Err:= GdiPlusStartup(InitToken, @Startup, nil);
    GDIPlusActive:= Err = 0;
    if not GDIPlusActive then
      FreeLibrary(LibHandle);
      Result:= False;
  end;
end;

procedure DoneGDIPlus;
begin
  if GDIPlusActive then begin
    GdiplusShutdown(InitToken);
    FreeLibrary(LibHandle);
    GDIPlusActive:= False;
  end;
end;

function RemoveImgPropertyItem(AImg: Integer; AProperty: Integer): Boolean;
var
  hr: HRESULT;
begin
  hr:= GdipRemovePropertyItem(AImg, AProperty);
  Result:= hr = S_OK;
end;

function GetImgTextProperty(AImg: Integer; AProperty: Integer): String;
var
  b: Pointer;
  hr: HRESULT;
  size: Cardinal;
begin
  Result:= '';
  hr:= GDIpl2.GdipGetPropertyItemSize(AImg, AProperty, size);
  if (hr = S_OK) then begin
    GetMem(b, size);
    hr:= GDIpl2.GdipGetPropertyItem(AImg, AProperty, size, b^);
    with TPropItem(b^) do begin
      Result:= PropertyValueAsString(TPropertyItem(b^));
    end;
    FreeMem(b);
  end;
end;

function SetImgTextProperty(AImg: Integer; AProperty: Integer; AValue: String): Boolean;
var
  propitem: TPropItem;
begin
  with propitem do begin
    typ:= PropertyTagTypeASCII;
    id:= AProperty;
    length:= System.Length(AValue) + 1;
    PChar(Value):= PChar(AValue);
  end;
  Result:= GDIpl2.GdipSetPropertyItem(Aimg, propitem) = S_OK;
end;

initialization

  // InitGDIPlus;

finalization

  // DoneGDIPlus;

end.
