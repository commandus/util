unit VFW;
////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                      Video for Windows                                     //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////
//                                                                            //
// Adapted from Thomas Schimming's VFW.PAS                                    //
// (c) 1996 Thomas Schimming, schimmin@iee1.et.tu-dresden.de                  //
// (c) 1998,99 Anders Melander                                                //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////
//                                                                            //
// Ripped all COM/ActiveX stuff and added some AVI stream functions.          //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////
interface

uses
{$ifdef VER90}
  ole2,
{$else}
  ActiveX,
{$endif}
  Windows;

type

 { TAVIFileInfoW record }

  LONG = Longint;
  PVOID = Pointer;

// TAVIFileInfo dwFlag values
const
  AVIF_HASINDEX		= $00000010;
  AVIF_MUSTUSEINDEX	= $00000020;
  AVIF_ISINTERLEAVED	= $00000100;
  AVIF_WASCAPTUREFILE	= $00010000;
  AVIF_COPYRIGHTED	= $00020000;
  AVIF_KNOWN_FLAGS	= $00030130;

type
  TAVIFileInfoW = record
    dwMaxBytesPerSec,	// max. transfer rate
    dwFlags,		// the ever-present flags
    dwCaps,
    dwStreams,
    dwSuggestedBufferSize,

    dwWidth,
    dwHeight,

    dwScale,
    dwRate,	// dwRate / dwScale == samples/second
    dwLength,

    dwEditCount: DWORD;

    szFileType: array[0..63] of WideChar;		// descriptive string for file type?
  end;
  PAVIFileInfoW = ^TAVIFileInfoW;

// TAVIStreamInfo dwFlag values
const
  AVISF_DISABLED	= $00000001;
  AVISF_VIDEO_PALCHANGES= $00010000;
  AVISF_KNOWN_FLAGS	= $00010001;

type
  TAVIStreamInfoA = record
    fccType,
    fccHandler,
    dwFlags,        // Contains AVITF_* flags
    dwCaps: DWORD;
    wPriority,
    wLanguage: WORD;
    dwScale,
    dwRate, // dwRate / dwScale == samples/second
    dwStart,
    dwLength, // In units above...
    dwInitialFrames,
    dwSuggestedBufferSize,
    dwQuality,
    dwSampleSize: DWORD;
    rcFrame: TRect;
    dwEditCount,
    dwFormatChangeCount: DWORD;
    szName:  array[0..63] of AnsiChar;
  end;
  TAVIStreamInfo = TAVIStreamInfoA;
  PAVIStreamInfo = ^TAVIStreamInfo;

  { TAVIStreamInfoW record }

  TAVIStreamInfoW = record
    fccType,
    fccHandler,
    dwFlags,        // Contains AVITF_* flags
    dwCaps: DWORD;
    wPriority,
    wLanguage: WORD;
    dwScale,
    dwRate, // dwRate / dwScale == samples/second
    dwStart,
    dwLength, // In units above...
    dwInitialFrames,
    dwSuggestedBufferSize,
    dwQuality,
    dwSampleSize: DWORD;
    rcFrame: TRect;
    dwEditCount,
    dwFormatChangeCount: DWORD;
    szName:  array[0..63] of WideChar;
  end;

  PAVIStream = pointer;
  PAVIFile = pointer;
  TAVIStreamList = array[0..0] of PAVIStream;
  PAVIStreamList = ^TAVIStreamList;
  TAVISaveCallback = function (nPercent: integer): LONG; stdcall;

  TAVICompressOptions = packed record
    fccType		: DWORD;
    fccHandler		: DWORD;
    dwKeyFrameEvery	: DWORD;
    dwQuality		: DWORD;
    dwBytesPerSecond	: DWORD;
    dwFlags		: DWORD;
    lpFormat		: pointer;
    cbFormat		: DWORD;
    lpParms		: pointer;
    cbParms		: DWORD;
    dwInterleaveEvery	: DWORD;
  end;
  PAVICompressOptions = ^TAVICompressOptions;

// Palette change data record
const
  RIFF_PaletteChange: DWORD = 1668293411;
type
  TAVIPalChange = packed record
    bFirstEntry		: byte;
    bNumEntries		: byte;
    wFlags		: WORD;
    peNew		: array[byte] of TPaletteEntry;
  end;
  PAVIPalChange = ^TAVIPalChange;

procedure AVIFileInit; stdcall;
procedure AVIFileExit; stdcall;
function AVIFileOpen(var ppfile: PAVIFile; szFile: PChar; uMode: UINT; lpHandler: pointer): HResult; stdcall;
function AVIFileCreateStream(pfile: PAVIFile; var ppavi: PAVISTREAM; var psi: TAVIStreamInfo): HResult; stdcall;
function AVIStreamSetFormat(pavi: PAVIStream; lPos: LONG; lpFormat: pointer; cbFormat: LONG): HResult; stdcall;
function AVIStreamReadFormat(pavi: PAVIStream; lPos: LONG; lpFormat: pointer; var cbFormat: LONG): HResult; stdcall;
function AVIStreamWrite(pavi: PAVIStream; lStart, lSamples: LONG; lpBuffer: pointer; cbBuffer: LONG; dwFlags: DWORD; var plSampWritten: LONG; var plBytesWritten: LONG): HResult; stdcall;
function AVIStreamWriteData(pavi: PAVIStream; ckid: DWORD; lpData: pointer; cbData: LONG): HResult; stdcall;
function AVIStreamRelease(pavi: PAVISTREAM): ULONG; stdcall;
function AVIFileRelease(pfile: PAVIFile): ULONG; stdcall;
function AVIFileGetStream(pfile: PAVIFile; var ppavi: PAVISTREAM; fccType: DWORD; lParam: LONG): HResult; stdcall;
function AVIStreamBeginStreaming(pavi: PAVISTREAM; lStart, lEnd: LONG; lRate: LONG): HResult; stdcall;
function AVIStreamEndStreaming(pavi: PAVISTREAM): HResult; stdcall;
function AVIStreamGetFrameOpen(pavi: PAVISTREAM; var lpbiWanted: TBitmapInfoHeader): pointer; stdcall;
function AVIStreamGetFrame(pgf: pointer; lPos: LONG): pointer; stdcall;
function AVIStreamGetFrameClose(pget: pointer): HResult; stdcall;
function AVIStreamInfo(pavi: PAVISTREAM; var psi: TAVIStreamInfo; lSize: LONG): HResult; stdcall;
function AVIStreamLength(pavi: PAVISTREAM): LONG; stdcall;
function CreateEditableStream(var ppsEditable: PAVISTREAM; psSource: PAVISTREAM): HResult; stdcall;
function EditStreamSetInfo(pavi: PAVISTREAM; var lpInfo: TAVIStreamInfo; cbInfo: LONG): HResult; stdcall;
function AVIMakeFileFromStreams(var ppfile: PAVIFILE; nStreams: integer; papStreams: PAVIStreamList): HResult; stdcall;
function AVISave(szFile: PChar; pclsidHandler: PCLSID; lpfnCallback: TAVISaveCallback;
  nStreams: integer; pavi: PAVISTREAM; lpOptions: PAVICompressOptions): HResult; stdcall;

const
  AVIERR_OK       = 0;

  AVIIF_LIST      = $01;
  AVIIF_TWOCC	  = $02;
  AVIIF_KEYFRAME  = $10;

  streamtypeVIDEO = $73646976; // DWORD( 'v', 'i', 'd', 's' )

implementation

  procedure AVIFileInit; stdcall; external 'avifil32.dll' name 'AVIFileInit';
  procedure AVIFileExit; stdcall; external 'avifil32.dll' name 'AVIFileExit';
  function AVIFileOpen; external 'avifil32.dll' name 'AVIFileOpenA';
  function AVIFileCreateStream; external 'avifil32.dll' name 'AVIFileCreateStreamA';
  function AVIStreamSetFormat; external 'avifil32.dll' name 'AVIStreamSetFormat';
  function AVIStreamReadFormat; external 'avifil32.dll' name 'AVIStreamReadFormat';
  function AVIStreamWrite; external 'avifil32.dll' name 'AVIStreamWrite';
  function AVIStreamWriteData; external 'avifil32.dll' name 'AVIStreamWriteData';
  function AVIStreamRelease; external 'avifil32.dll' name 'AVIStreamRelease';
  function AVIFileRelease; external 'avifil32.dll' name 'AVIFileRelease';
  function AVIFileGetStream; external 'avifil32.dll' name 'AVIFileGetStream';
  function AVIStreamBeginStreaming; external 'avifil32.dll' name 'AVIStreamBeginStreaming';
  function AVIStreamEndStreaming; external 'avifil32.dll' name 'AVIStreamEndStreaming';
  function AVIStreamGetFrameOpen; external 'avifil32.dll' name 'AVIStreamGetFrameOpen';
  function AVIStreamGetFrame; external 'avifil32.dll' name 'AVIStreamGetFrame';
  function AVIStreamGetFrameClose; external 'avifil32.dll' name 'AVIStreamGetFrameClose';
  function AVIStreamInfo; external 'avifil32.dll' name 'AVIStreamInfo';
  function AVIStreamLength; external 'avifil32.dll' name 'AVIStreamLength';
  function CreateEditableStream; external 'avifil32.dll' name 'CreateEditableStream';
  function EditStreamSetInfo; external 'avifil32.dll' name 'EditStreamSetInfo';
  function AVIMakeFileFromStreams; external 'avifil32.dll' name 'AVIMakeFileFromStreams';
  function AVISave; external 'avifil32.dll' name 'AVISave';
(*
typedef struct {
    unsigned long dwMicroSecPerFrame;
    unsigned long dwMaxBytesPerSec;
    unsigned long dwReserved1;
    unsigned long dwFlags;
    unsigned long dwTotalFrames;
    unsigned long dwInitialFrames;
    unsigned long dwStreams;
    unsigned long dwSuggestedBufferSize;
    unsigned long dwWidth;
    unsigned long dwHeight;
    unsigned long dwScale;
    unsigned long dwRate;
    unsigned long dwStart;
    unsigned long dwLength;
} MainAVIHeader;

#define AVIF_HASINDEX           0x00000010
#define AVIF_MUSTUSEINDEX       0x00000020
#define AVIF_ISINTERLEAVED      0x00000100
#define AVIF_WASCAPTUREFILE     0x00010000
#define AVIF_COPYRIGHTED        0x00020000

#define AVIF_KNOWN_FLAGS        0x00030130

#define Defined_MainAVIHeader_Size      (14*4)

typedef struct {
    unsigned long fccType;
    unsigned long fccHandler;
    unsigned long dwFlags;
    unsigned long dwReserved1;
    unsigned long dwInitialFrames;
    unsigned long dwScale;
    unsigned long dwRate;
    unsigned long dwStart;
    unsigned long dwLength;
    unsigned long dwSuggestedBufferSize;
    unsigned long dwQuality;
    unsigned long dwSampleSize;
    unsigned long dwReserved2;
    unsigned long dwReserved3;
} AVIStreamHeader;

#define AVISF_DISABLED          0x00000001
#define AVISF_VIDEO_PALCHANGES  0x00010000

#define AVISF_KNOWN_FLAGS       0x00010001

#define Defined_AVIStreamHeader_Size_old  (12*4)
#define Defined_AVIStreamHeader_Size      (14*4)

typedef struct {
    unsigned char bFirstEntry;
    unsigned char bNumEntries;
    unsigned short wFlags;
} AVIPALCHANGE;

typedef struct {
    unsigned long ckid;
    unsigned long dwFlags;
    unsigned long dwChunkOffset;
    unsigned long dwChunkLength;
} AVIINDEXENTRY;

#define AVIIF_LIST      0x00000001
#define AVIIF_KEYFRAME  0x00000010
#define AVIIF_FIRSTPART 0x00000020
#define AVIIF_LASTPART  0x00000040
#define AVIIF_MIDPART   (AVIIF_LASTPART | AVIFF_FIRSTPART)
#define AVIIF_NOTIME    0x00000100
#define AVIIF_COMPUSE   0x0fff0000

#define AVIIF_KNOWN_FLAGS 0x0fff0171

#define Defined_AVIINDEXENTRY_Size      (4*4)
*)
end.


