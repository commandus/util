unit
  starwars;
(*##*)
(*******************************************************************************
*                                                                             *
*   S  T  A  R  W  A  R  S                                                     *
*                                                                             *
*   Copyright © 2005-2006 Andrei Ivanov. All rights reserved.                  *
*   Can be useful like eastern eggs in "About" window                         *
*   Conditional defines:                                                       *
*                                                                             *
*   Revisions    : Jun 25 2001                                                 *
*   Last revision: Dec  3 2001                                                *
*   Lines        : 144                                                         *
*   History      :                                                            *
*   Printed      : ---                                                         *
*                                                                             *
********************************************************************************)
(*##*)

interface
uses
  Classes, Forms, Windows, SysUtils, Controls, ExtCtrls, Graphics, OpenGL,
  OpenGLTextures, jclUnicode;

const
  EYE_Z = 3.0;
  MAX_TEXT = 256;  // 128 for ANSI

procedure glBindTexture(target: GLenum; texture: GLuint); stdcall; external opengl32;
function gluBuild2DMipmaps(Target: GLenum; Components, Width, Height: GLint; Format, atype: GLenum; Data: Pointer): GLint; stdcall; external glu32;
procedure glGenTextures(n: GLsizei; var textures: GLuint); stdcall; external opengl32;

type
  TDPOINT = record
    x, y, z: GLFloat;
  end;

  TSTAR = record
    Start: TDPOINT;
    speed: TDPOINT;
    CurPos: packed array[0..2] of GLFloat; // a GLFloat[], so we can call glVertex3fv
    Colour: packed array[0..2] of GLFloat;
    TimeOffset: GLFloat;
  end;

  // PGlyphMetricsFloatArr = array[0..0] of TGlyphMetricsFloat;
  TSimpleObject = class
  private
    FAngle,
    FTimeOffset: GLDouble;
    FColour: array[0..2] of GLDouble;
    FStart: TDPOINT;
    FSlope: TDPoint;
    FCurPos: TDPOINT;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Draw; virtual; abstract;
  end;

  TStarText = class (TSimpleObject)
  private
    FText: String;
    // Calculates the length of a string
    function CalculateXLength(agmf: PGlyphMetricsFloat; const AStr: PANSIChar): GLFloat;
  public
    // Simple creation.  Supply text, and the colors and time offset
    constructor Create(agmf: PGlyphMetricsFloat; const AText: String; AColor: TColor;
      timeOffset: GLFloat);
    destructor Destroy; override;
    procedure Draw; override;
  end;

  TOpenGLWndControllerBase = class(TThread)
  private
    Fdc: HDC;
    FHandle: HWND;
    FTimeElapsed: GLFloat;
    FStarted: Boolean;
    FCStart,
    Freq: Int64;
    FStopped,
    FTerminated: Boolean;
  protected
    function GLExtensionExists(const extension: PChar): Boolean;
    function GetRandom(Amin, Amax: GLFloat): GLFloat;
    function ValidatePixelFormat(Ahdc: HDC; AsuggestedFormat: Integer): Integer; virtual;
    procedure WindowSized(const ARect: TRect); virtual;
  public
    constructor Create(ADC: HDC; ARect: TRect);
    destructor Destroy; override;
    procedure DoDraw;
    function Draw: Boolean; virtual; // False - continue, True - one loop finished
    procedure Init; virtual;
    procedure Done; virtual;
    procedure Idle; virtual;
    procedure Execute; override;
    procedure ExecuteOneLoop;
    property Terminated: Boolean read FTerminated write FTerminated;
  end;

  TOpenGLWndControllerStar = class (TOpenGLWndControllerBase)
  private
    FStarSpeed: GLFloat;
    FStarCount: Integer;
    FPointStars: Boolean;
    Fagmf: array[0..MAX_TEXT - 1] of GLYPHMETRICSFLOAT;
    FStarTexts: array of TStarText;
    FStars: array of TStar;
    FStarStart: Int64;
  public
    constructor Create(AStrings: TWideStrings; AStarSpeed: GLFloat; ANumStars: Integer; APointStars: Boolean;
      ADC: HDC; AFont: HFont; ARect: TRect);
    procedure Init; override;
    procedure Done; override;
    function Draw: Boolean; override;
    procedure Idle; override;
  end;

  TOpenGLWndControllerBall = class (TOpenGLWndControllerBase)
  private
    FSpeed: GLFloat;
    FCount: Integer;
    FStars: array of TStar;
    FBallStart: Int64;
    BallTex,
    ReflectMirrorTex: GLuint;
    MyQuadratic: gluQuadricObj;
    procedure DrawBmp(AX, AY, AZ: GLfloat);
  public
    constructor Create(ASpeed: GLFloat; ANum: Integer; ADC: HDC; AFont: HFont; ARect: TRect);
    procedure Init; override;
    procedure Done; override;
    function Draw: Boolean; override;
    procedure Idle; override;
  end;

  TOpenGLWndControllerBox = class (TOpenGLWndControllerBase)
  private
    FSpeed: GLFloat;
    FCount: Integer;
    FStars: array of TStar;
    FBallStart: Int64;
    // Textures
    RoomTexture,
    FloorTexture: GLUInt;
    CubeTexture: array [0..5] of GLuint;
    LightTexture: GLuint;

    MyQuadratic: gluQuadricObj;

    CubeDL: glUint;                    // cube display list
    CubeDLs: array[0..5] of glUint;    // cube display list
    RoomDL: glUint;                    // RoomDL display list
    FloorDL: glUint;                   // RoomDL display list

    XLightpos: GLfloat;                //X position of the light polygon
    YLightpos: GLfloat;                //Y position of the light polygon
  public
    constructor Create(ASpeed: GLFloat; ANum: Integer; ADC: HDC; AFont: HFont; ARect: TRect);
    procedure Init; override;
    procedure Done; override;
    function Draw: Boolean; override;
    procedure Idle; override;
  end;

implementation

const
  WGL_NUMBER_PIXEL_FORMATS_ARB   = $2000;
  WGL_DRAW_TO_WINDOW_ARB         = $2001;
  WGL_DRAW_TO_BITMAP_ARB         = $2002;
  WGL_ACCELERATION_ARB           = $2003;
  WGL_NEED_PALETTE_ARB           = $2004;
  WGL_NEED_SYSTEM_PALETTE_ARB    = $2005;
  WGL_SWAP_LAYER_BUFFERS_ARB     = $2006;
  WGL_SWAP_METHOD_ARB            = $2007;
  WGL_NUMBER_OVERLAYS_ARB        = $2008;
  WGL_NUMBER_UNDERLAYS_ARB       = $2009;
  WGL_TRANSPARENT_ARB            = $200A;
  WGL_TRANSPARENT_RED_VALUE_ARB  = $2037;
  WGL_TRANSPARENT_GREEN_VALUE_ARB = $2038;
  WGL_TRANSPARENT_BLUE_VALUE_ARB = $2039;
  WGL_TRANSPARENT_ALPHA_VALUE_ARB = $203A;
  WGL_TRANSPARENT_INDEX_VALUE_ARB = $203B;
  WGL_SHARE_DEPTH_ARB            = $200C;
  WGL_SHARE_STENCIL_ARB          = $200D;
  WGL_SHARE_ACCUM_ARB            = $200E;
  WGL_SUPPORT_GDI_ARB            = $200F;
  WGL_SUPPORT_OPENGL_ARB         = $2010;
  WGL_DOUBLE_BUFFER_ARB          = $2011;
  WGL_STEREO_ARB                 = $2012;
  WGL_PIXEL_TYPE_ARB             = $2013;
  WGL_COLOR_BITS_ARB             = $2014;
  WGL_RED_BITS_ARB               = $2015;
  WGL_RED_SHIFT_ARB              = $2016;
  WGL_GREEN_BITS_ARB             = $2017;
  WGL_GREEN_SHIFT_ARB            = $2018;
  WGL_BLUE_BITS_ARB              = $2019;
  WGL_BLUE_SHIFT_ARB             = $201A;
  WGL_ALPHA_BITS_ARB             = $201B;
  WGL_ALPHA_SHIFT_ARB            = $201C;
  WGL_ACCUFITS_ARB             = $201D;
  WGL_ACCUM_RED_BITS_ARB         = $201E;
  WGL_ACCUM_GREEN_BITS_ARB       = $201F;
  WGL_ACCUFLUE_BITS_ARB        = $2020;
  WGL_ACCUM_ALPHA_BITS_ARB       = $2021;
  WGL_DEPTH_BITS_ARB             = $2022;
  WGL_STENCIL_BITS_ARB           = $2023;
  WGL_AUX_BUFFERS_ARB            = $2024;
  WGL_NO_ACCELERATION_ARB        = $2025;
  WGL_GENERIC_ACCELERATION_ARB   = $2026;
  WGL_FULL_ACCELERATION_ARB      = $2027;
  WGL_SWAP_EXCHANGE_ARB          = $2028;
  WGL_SWAP_COPY_ARB              = $2029;
  WGL_SWAP_UNDEFINED_ARB         = $202A;
  WGL_TYPE_RGBA_ARB              = $202B;
  WGL_TYPE_COLORINDEX_ARB        = $202C;

  GL_MULTISAMPLE_ARB             = $809D;
  GL_SAMPLE_ALPHA_TO_COVERAGE_ARB= $809E;
  GL_SAMPLE_ALPHA_TO_ONE_ARB     = $809F;
  GL_SAMPLE_COVERAGE_ARB         = $80A0;
  GL_SAMPLE_BUFFERS_ARB          = $80A8;
  GL_SAMPLES_ARB                 = $80A9;
  GL_SAMPLE_COVERAGE_VALUE_ARB   = $80AA;
  GL_SAMPLE_COVERAGE_INVERT_ARB  = $80AB;
  GL_MULTISAMPLE_BIT_ARB         = $20000000;

  // NV_multisample_filter_hint
  GL_MULTISAMPLE_FILTER_HINT_NV  = $8534;

  WGL_SAMPLE_BUFFERS_ARB		     = $2041;
  WGL_SAMPLES_ARB                = $2042;


{------------------------------------------------------------------}
{  Load BMP file                                                   }
{------------------------------------------------------------------}
procedure LoadBitmap(Filename: String; out Width: Cardinal; out Height: Cardinal; out pData: Pointer);
var
  FileHeader: BITMAPFILEHEADER;
  InfoHeader: BITMAPINFOHEADER;
  Palette: array of RGBQUAD;
  BitmapFile: THandle;
  BitmapLength: Cardinal;
  PaletteLength: Cardinal;
  ReadBytes: Cardinal;
  Front: ^Byte;
  Back: ^Byte;
  Temp: Byte;
  I : Cardinal;
begin
  BitmapFile := CreateFile(PChar(Filename), GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, 0, 0);
  if (BitmapFile = INVALID_HANDLE_VALUE) then begin
    MessageBox(0, PChar('Error opening "' + Filename), PChar('BMP Unit'), MB_OK);
    Exit;
  end;

  // Get header information
  ReadFile(BitmapFile, FileHeader, SizeOf(FileHeader), ReadBytes, nil);
  ReadFile(BitmapFile, InfoHeader, SizeOf(InfoHeader), ReadBytes, nil);

  // Get palette
  PaletteLength := InfoHeader.biClrUsed;
  SetLength(Palette, PaletteLength);
  ReadFile(BitmapFile, Palette, PaletteLength, ReadBytes, nil);
  if (ReadBytes <> PaletteLength) then begin
    MessageBox(0, PChar('Error reading palette'), PChar('BMP Unit'), MB_OK);
    Exit;
  end;

  Width := InfoHeader.biWidth;
  Height := InfoHeader.biHeight;
  BitmapLength := InfoHeader.biSizeImage;
  if BitmapLength = 0 then
    BitmapLength := Width * Height * InfoHeader.biBitCount Div 8;

  // Get the actual pixel data
  GetMem(pData, BitmapLength);
  ReadFile(BitmapFile, pData^, BitmapLength, ReadBytes, nil);
  if (ReadBytes <> BitmapLength) then begin
    MessageBox(0, PChar('Error reading bitmap data'), PChar('BMP Unit'), MB_OK);
    Exit;
  end;
  CloseHandle(BitmapFile);

  // Bitmaps are stored BGR and not RGB, so swap the R and B bytes.
  for I :=0 to Width * Height - 1 do
  begin
    Front := Pointer(Cardinal(pData) + I*3);
    Back := Pointer(Cardinal(pData) + I*3 + 2);
    Temp := Front^;
    Front^ := Back^;
    Back^ := Temp;
  end;
end;


procedure setupPixelFormat(DC:HDC);
const
 pfd:TPIXELFORMATDESCRIPTOR = (
    nSize:sizeof(TPIXELFORMATDESCRIPTOR);	// size
    nVersion:1;				        // version
    dwFlags:PFD_SUPPORT_OPENGL or PFD_DRAW_TO_WINDOW or PFD_DOUBLEBUFFER; // support double-buffering
    iPixelType:PFD_TYPE_RGBA;		        // color type
    cColorBits:24;				// prefered color depth
    cRedBits:0; cRedShift:0;		        // color bits (ignored)
    cGreenBits:0;  cGreenShift:0;
    cBlueBits:0; cBlueShift:0;
    cAlphaBits:0;  cAlphaShift:0;               // no alpha buffer
    cAccumBits: 0;
    cAccumRedBits: 0;  		                // no accumulation buffer,
    cAccumGreenBits: 0;                         // accum bits (ignored)
    cAccumBlueBits: 0;
    cAccumAlphaBits: 0;
    cDepthBits: 16;				// depth buffer
    cStencilBits:0;				// no stencil buffer
    cAuxBuffers:0;				// no auxiliary buffers
    iLayerType:PFD_MAIN_PLANE;                  // main layer
    bReserved: 0;
    dwLayerMask: 0;
    dwVisibleMask: 0;
    dwDamageMask: 0;                            // no layer, visible, damage masks */
    );
{
 pfd:TPIXELFORMATDESCRIPTOR = (
    nSize:sizeof(TPIXELFORMATDESCRIPTOR);	// size
    nVersion:1;				        // version
    dwFlags:PFD_SUPPORT_OPENGL or PFD_DRAW_TO_WINDOW or PFD_DOUBLEBUFFER; // support double-buffering
    iPixelType:PFD_TYPE_COLORINDEX;		        // color type
    cColorBits:8;				// prefered color depth
    cRedBits:0; cRedShift:0;		        // color bits (ignored)
    cGreenBits:0;  cGreenShift:0;
    cBlueBits:0; cBlueShift:0;
    cAlphaBits:0;  cAlphaShift:0;               // no alpha buffer
    cAccumBits: 0;
    cAccumRedBits: 0;  		                // no accumulation buffer,
    cAccumGreenBits: 0;                         // accum bits (ignored)
    cAccumBlueBits: 0;
    cAccumAlphaBits: 0;
    cDepthBits: 16;				// depth buffer
    cStencilBits:0;				// no stencil buffer
    cAuxBuffers:0;				// no auxiliary buffers
    iLayerType:PFD_MAIN_PLANE;                  // main layer
    bReserved: 0;
    dwLayerMask: 0;
    dwVisibleMask: 0;
    dwDamageMask: 0;                            // no layer, visible, damage masks */
    );
}
var
  pixelFormat: Integer;
begin
  pixelFormat := ChoosePixelFormat(DC, @pfd);
  if (pixelFormat = 0) then begin
    MessageBox(WindowFromDC(DC), 'ChoosePixelFormat failed.', 'Error',
      MB_ICONERROR or MB_OK);
    Exit;
  end;
  if (not SetPixelFormat(DC, pixelFormat, @pfd)) then begin
    MessageBox(WindowFromDC(DC), 'SetPixelFormat failed.', 'Error',
      MB_ICONERROR or MB_OK);
    Exit;
  end;
end;

{ Basic object.
  Fill in the angle, color, starting coordinates and slope, and the StarWarsController
  will update the current position for you.  Override draw to do the work.  You will
  be translated, so 0,0,0 will be your origin
}

constructor TSimpleObject.Create;
begin
  inherited Create;
  with FStart do begin
    x:= 0.0;
    y:= -4.0;
    z:= 0.0;
  end;
  with FSlope do begin
    x:= 0.0;
    y:= 0.5;
    z:= -0.5;
  end;
  // based on their slopes, we calculate the angle that the words move along
  FAngle:= ArcTan(FSlope.z/FSlope.y)*180.0/PI;
end;

destructor TSimpleObject.Destroy;
begin
  inherited Destroy;
end;

// -----------------------------------------------------------------------------
//  This is a simple text object, for displaying text

constructor TStarText.Create(agmf: PGlyphMetricsFloat; const AText: String;
  AColor: TColor; timeOffset: GLFloat);
var
  RGB: Longint;
begin
  inherited Create;
	FText:= AText;

  RGB:= ColorToRGB(AColor);
	FColour[0]:= RGB and $FF;
	FColour[1]:= (RGB and $FF00) shr 8;
	FColour[2]:= (RGB and $FF0000) shr $10;

	FTimeOffset:= timeOffset;

	FStart.x:= - CalculateXLength(agmf, PANSIChar(FText)) / 2;
end;

destructor TStarText.Destroy;
begin
  inherited Destroy;
end;

type
  TGlyphMetricsFloatArr = array[0..0] of TGlyphMetricsFloat;

{$RANGECHECKS OFF}

function TStarText.CalculateXLength(agmf: PGlyphMetricsFloat; const AStr: PANSIChar): GLFloat;
var
  len: Integer;
  loop: Integer;
begin
	len:= Length(AStr);
	Result:= 0.0;
	for loop:= 0 to len - 1 do begin	// Loop To Find Text Length
	  Result:= Result + TGlyphMetricsFloatArr(agmf^)[Byte(AStr[loop])].gmfCellIncX;		// Increase Length By Each Characters Width
	end;
end;

procedure TStarText.Draw;
begin
	glListBase(1000); // Indicates the Start of display lists for the glyphs
	glCallLists(Length(FText), GL_UNSIGNED_BYTE, PANSIChar(FText));
end;

//---------------- TOpenGLWndControllerBase ------------------------------------

{ black screen
}

//  CreateInDC(GetDC(AParent.Handle), AParent.ClientRect);
constructor TOpenGLWndControllerBase.Create(ADC: HDC; ARect: TRect);
var
  i: Integer;
  RC: HGLRC;
begin
  FStarted:= False;
  // FHandle:= 0;
  FDC:= ADC;
  SetupPixelFormat(FDC);
  RC:= wglCreateContext(FDC);
  wglMakeCurrent(FDC, RC);

  QueryPerformanceFrequency(Freq);

  Init;

  WindowSized(ARect);
  FTerminated:= True;
  FStopped:= True;
  inherited Create(True);
end;

procedure TOpenGLWndControllerBase.Init;
begin
end;

procedure TOpenGLWndControllerBase.Done;
begin
end;

destructor TOpenGLWndControllerBase.Destroy;
var
  i: Integer;
begin
  FTerminated:= True;
  repeat
    Application.ProcessMessages;
    sleep(10);
  until FStopped;

  Done;

  ReleaseDC(FHandle, FDC);
end;

type
  PIntegerA = array[0..0] of Integer;
  PGLFloatA = array[0..0] of GLFloat;
  PFNWGLGETEXTENSIONSSTRINGARBPROC = function (Ahdc: HDC): PChar; stdcall;
  PFNWGLCHOOSEPIXELFORMATARBPROC = function (AHDC: HDC; piAttribIList: Pointer;
    pfAttribFList: Pointer; nMaxFormats: Cardinal; piFormats: Pointer;
    var nNumFormats: Cardinal): Boolean;

  // function wglGetProcAddress(AProcName: PChar): Pointer;  stdcall;
{ determines if a GL extension exists.  First it uses wglGetExtensionsStringARB.
  I chose to do that first because my Geforce 4 required that call in order to
  initialize extensions.
}
function TOpenGLWndControllerBase.GLExtensionExists(const extension: PChar): Boolean;
var
  wglGetExtensionsStringARB: PFNWGLGETEXTENSIONSSTRINGARBPROC;
  winsys_extensions: PChar;
  glExtensions: PChar;
begin
  wglGetExtensionsStringARB:= PFNWGLGETEXTENSIONSSTRINGARBPROC(wglGetProcAddress('wglGetExtensionsStringARB'));
  if Assigned(wglGetExtensionsStringARB) then begin
    winsys_extensions:= wglGetExtensionsStringARB(wglGetCurrentDC);
    if (winsys_extensions <> Nil) and (CompareText(winsys_extensions, extension) = 0) then begin
      Result:= True;
      Exit;
    end;
  end;
  glExtensions:= glGetString(GL_EXTENSIONS);
  if (glExtensions <> Nil) then begin
    Result:= True;
    Exit;
  end;

  Result:= CompareText(glExtensions, extension)= 0;
end;

// Returns a random number between min and max
function TOpenGLWndControllerBase.GetRandom(Amin, Amax: GLFloat): GLFloat;
begin
  Result:= Random;
  Result:= AMin + (Amax - Amin) * Result;
end;

// Moves the text/stars using the current time
procedure TOpenGLWndControllerBase.Idle;
var
  nowt: Int64;
begin
  // get current time
  QueryPerformanceCounter(nowt);
  FTimeElapsed:= (nowt - FCStart)/Freq;
end;

// Overridden to enable multisampling (FSAA)
function TOpenGLWndControllerBase.ValidatePixelFormat(Ahdc: HDC; AsuggestedFormat: Integer): Integer;
var
  h: HDC;
  wglChoosePixelFormatARB: PFNWGLCHOOSEPIXELFORMATARBPROC;
  pixelFormat: Integer;
  bStatus: Boolean;
  numFormats: Cardinal;
  fAttributes: array [0..1] of GLFloat;
  iAttributes: array [0..21] of Integer;
begin
  h:= wglGetCurrentDC;

  wglChoosePixelFormatARB:= PFNWGLCHOOSEPIXELFORMATARBPROC(wglGetProcAddress('wglChoosePixelFormatARB'));
  if not Assigned(wglChoosePixelFormatARB) then begin
    Result:= AsuggestedFormat;
    Exit;
  end;

  if (not GLExtensionExists('WGL_ARB_multisample ')) then begin
	  Result:= AsuggestedFormat;
    Exit;
  end;

  fAttributes[0]:= 0;
  fAttributes[1]:= 0;

  iAttributes[0]:= WGL_DRAW_TO_WINDOW_ARB;
  iAttributes[1]:= Integer(GL_True);
	iAttributes[2]:= WGL_SUPPORT_OPENGL_ARB;
  iAttributes[3]:= Integer(GL_True);
	iAttributes[4]:= WGL_ACCELERATION_ARB;
  iAttributes[5]:= WGL_FULL_ACCELERATION_ARB;
	iAttributes[6]:= WGL_COLOR_BITS_ARB;
  iAttributes[7]:= 24;
	iAttributes[8]:= WGL_ALPHA_BITS_ARB;
  iAttributes[9]:= 8;
	iAttributes[10]:= WGL_DEPTH_BITS_ARB;
  iAttributes[11]:= 16;
	iAttributes[12]:= WGL_STENCIL_BITS_ARB;
  iAttributes[13]:= 0;
	iAttributes[14]:= WGL_DOUBLE_BUFFER_ARB;
  iAttributes[15]:= Integer(GL_True);
	iAttributes[16]:= WGL_SAMPLE_BUFFERS_ARB;
  iAttributes[17]:= Integer(GL_True);
	iAttributes[18]:= WGL_SAMPLES_ARB;
  iAttributes[19]:= 4;
	iAttributes[20]:= 0;
  iAttributes[21]:= 0;

  bStatus:= wglChoosePixelFormatARB(h, @iAttributes, @fAttributes, 1, @pixelFormat, numFormats);
  if (bStatus = GL_True) and (numFormats = 1) then begin
    Result:= pixelFormat;
    Exit;
  end;
  // ok that failed, try using 2 samples now instead of 4
  iAttributes[19]:= 2;
  bStatus:= wglChoosePixelFormatARB(h, @iAttributes, @fAttributes,1, @pixelFormat,numFormats);
  if (bStatus = GL_True) and (numFormats = 1) then begin
    Result:= pixelFormat;
    Exit;
  end;
  // failed, return the suggested format and continue
  Result:= ASuggestedFormat;
end;

// Called when the window is sized, setup the viewport
procedure TOpenGLWndControllerBase.WindowSized(const ARect: TRect);
var
  w, h: Integer;
begin
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  with ARect do begin
    w:= Abs(Right - Left) + 1;
    h:= Abs(Bottom - Top) + 1;
  end;
  gluPerspective(60.0, w/h + 1, 1.0, 90.0);
  glViewport(0, 0, w, h);
end;

procedure TOpenGLWndControllerBase.DoDraw;
begin
  Draw;
end;

// Method to actually draw on the control
function TOpenGLWndControllerBase.Draw(): Boolean;
begin
  glClearColor(0.0,0.0,0.0,0.0);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  if (not FStarted) then Exit;
  SwapBuffers(wglGetCurrentDC);
  Result:= False;
end;

procedure TOpenGLWndControllerBase.Execute;
begin
  FStopped:= False;
  FTerminated:= False;
  QueryPerformanceCounter(FCStart);
  FStarted:= True;

  repeat
    //  Application.ProcessMessages;
    Idle;
    Synchronize(DoDraw);
    sleep(50);
  until FTerminated;
  FStopped:= True;
end;

procedure TOpenGLWndControllerBase.ExecuteOneLoop;
begin
  FStopped:= False;
  FTerminated:= False;
  QueryPerformanceCounter(FCStart);
  FStarted:= True;

  repeat
    Application.ProcessMessages;
    Idle;
    FTerminated:= Draw;
    sleep(50);
  until FTerminated;
  FStopped:= True;
end;

// ---------------- TOpenGLWndControllerStar -----------------------------------
{
The star wars controller. It shows stars and scrolling CObject's.  The CObjects
are created in Start().  You can control the star speed,number of stars, and if
the stars are points or polygons via the constructor.  The CObjects are hard-coded.
It wouldn't be hard to allow dynamic changes.
The Idle function moves everything.  Draw displays everything
}

constructor TOpenGLWndControllerStar.Create(AStrings: TWideStrings;
  AStarSpeed: GLFloat; ANumStars: Integer; APointStars: Boolean;
  ADC: HDC; AFont: HFont; ARect: TRect);
var
  i: Integer;
  hOld: Windows.HFONT;
  RC: HGLRC;
begin
  FStarSpeed:= AStarSpeed;
  FStarCount:= ANumStars;
  FPointStars:= APointStars;

  SetLength(FStars, ANumStars);
  for i:= 0 to Length(FStars) - 1 do begin
    FillChar(FStars[i], SizeOf(TStar), #0);
  end;

  inherited Create(ADC, ARect);

  hOld:= SelectObject(Adc, AFont);
  wglUseFontOutlines(Adc, 0, MAX_TEXT, 1000, 0.0, 0.1, WGL_FONT_POLYGONS, Windows.PGlyphMetricsFloat(@Fagmf));
  // SelectObject(dc, hOld);

  if Assigned(AStrings) then begin
    SetLength(FStarTexts, AStrings.Count);
    for i:= 0 to System.Length(FStarTexts) - 1 do begin
      FStarTexts[i]:= TStarText.Create(@Fagmf, AStrings[i], clYellow, i * 1.0);
    end;
  end else SetLength(FStarTexts, 0);
end;

procedure TOpenGLWndControllerStar.Init;
const
  PERCENTAGE_WHITE = 0.8; // 80% of the stars are white.  The rest are yellowish - redish
var
  i: Integer;
begin
  inherited Init;
  Randomize();
  for i:= 0  to FStarCount - 1 do begin
    FStars[i].Start.x:= GetRandom(-5.0, 5.0);
    FStars[i].Start.y:= GetRandom(-5.0, 5.0);
    FStars[i].Start.z:= GetRandom(-100.0, 1.0);

    FStars[i].speed.x:= 0.0;
    FStars[i].speed.y:= 0.0;
    FStars[i].speed.z:= FStarSpeed;

    FStars[i].Colour[0]:= 1.0;
    if (GetRandom(0.0, 1.0) > PERCENTAGE_WHITE) then begin
      // make it a colorful star
      FStars[i].Colour[1]:= GetRandom(0.5,1.0);
      FStars[i].Colour[2]:= 0.0;
    end else begin
      // make it white
      FStars[i].Colour[1]:= 1.0;
      FStars[i].Colour[2]:= 1.0;
    end;
    FStars[i].timeOffset:= 0.0;
  end;

  glHint(GL_MULTISAMPLE_FILTER_HINT_NV, GL_NICEST);
  glEnable(GL_MULTISAMPLE_ARB);
  glDisable(GL_BLEND);
  glCullFace(GL_BACK);
  glEnable(GL_CULL_FACE);

  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
  gluLookAt(0.0, 0.0, EYE_Z, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0);
end;

procedure TOpenGLWndControllerStar.Done;
var
  i: Integer;
begin
  for i:= 0 to Length(FStarTexts) - 1 do begin
    FreeAndNil(FStarTexts[i]);
  end;
  SetLength(FStars, 0);
end;

// Moves the text/stars using the current time
procedure TOpenGLWndControllerStar.Idle;
var
  i: Integer;
begin
  inherited Idle;
  // move the objects
  for i:= 0 to Length(FStarTexts) - 1 do with FStarTexts[i] do begin
    FCurPos.x:= FStart.x;
    FCurPos.y:= FStart.y + FSlope.y * (FTimeElapsed - FTimeOffset);
    FCurPos.z:= FStart.z + FSlope.z * (FTimeElapsed - FTimeOffset);
  end;
  // move the stars, calculate new time based on star Start time
  for i:= 0 to FStarCount - 1 do begin
    // update their z position
    FStars[i].CurPos[2]:= FStars[i].Start.z + FStars[i].speed.z * (FTimeElapsed - FStars[i].timeOffset);
    // ok they're out of view, respawn a new star
    if (FStars[i].CurPos[2] >= EYE_Z) then begin
      FStars[i].Start.x:= GetRandom(-5.0,5.0);
      FStars[i].Start.y:= GetRandom(-5.0,5.0);
      FStars[i].Start.z:= GetRandom(-100.0, -0.1);
      FStars[i].timeOffset:= FTimeElapsed;
    end else begin
	    FStars[i].CurPos[0]:= FStars[i].Start.x;
	    FStars[i].CurPos[1]:= FStars[i].Start.y;
    end;
  end;
end;

// Method to actually draw on the control
function TOpenGLWndControllerStar.Draw: Boolean;
const
  LENGTH = 0.02;
var
  i: Integer;
  distance, alpha: GLFloat;
begin
  glClearColor(0.0,0.0,0.0,0.0);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  if (not FStarted) then Exit;
  // now draw stars - as points
  if FPointStars then begin
    glBegin(GL_POINTS);
    for i:= 0 to FStarCount - 1 do begin
      glColor3fv(@(FStars[i].Colour));
      glVertex3fv(@(FStars[i].CurPos));
    end;
    glEnd();
  end else begin // draw stars as quads
    glBegin(GL_QUADS);
    for i:= 0 to FStarCount - 1 do begin
      glColor3fv(@(FStars[i].Colour));
	    glVertex3f(FStars[i].CurPos[0]-LENGTH,FStars[i].CurPos[1]-LENGTH,FStars[i].CurPos[2]);
      glVertex3f(FStars[i].CurPos[0]-LENGTH,FStars[i].CurPos[1]+LENGTH,FStars[i].CurPos[2]);
      glVertex3f(FStars[i].CurPos[0]+LENGTH,FStars[i].CurPos[1]+LENGTH,FStars[i].CurPos[2]);
      glVertex3f(FStars[i].CurPos[0]+LENGTH,FStars[i].CurPos[1]-LENGTH,FStars[i].CurPos[2]);
    end;
    glEnd();
  end;

  // now draw text

  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glEnable(GL_BLEND);

  for i:= 0 to System.Length(FStarTexts) - 1 do with FStarTexts[i] do begin
    if (not Assigned(FStarTexts[i]))
    then Continue;
    // determine distance from us
    // distance:= sqrt(FCurPos.x * FCurPos.x + FCurPos.y * FCurPos.y + FCurPos.z * FCurPos.z);
    distance:= sqrt(1 + FCurPos.y * FCurPos.y + FCurPos.z * FCurPos.z);

    // approximate the alpha value based on the distance away from us
    alpha:= 3.75 - sqrt(distance);
    if (alpha > 1.0) then alpha:= 1.0
    else if (alpha < 0.0) then alpha:= 0.0;
    if (alpha > 0.1) and (FCurPos.z <= 1) then begin
      glPushMatrix();

      // move everything into position
      glScalef(0.50, 0.50, 0.50);
      glTranslatef(FCurPos.x, FCurPos.y, FCurPos.z);
      glRotatef(FAngle,1.0, 0.0, 0.0);
      glColor4f(FColour[0], FColour[1], FColour[2], alpha);
      Draw();
      glPopMatrix();
    end;
  end;

  SwapBuffers(wglGetCurrentDC);

  // ok now we check the last alpha value, if it's <= 0.0, everything has faded away, and we restart

  Result:= (alpha <= 0.0) and (FStarTexts[System.Length(FStarTexts) - 1].FCurPos.z < 0);
  if Result
	then Init;
end;

//---------------- TOpenGLWndControllerBall ------------------------------------

procedure TOpenGLWndControllerBall.DrawBmp(AX, AY, AZ: GLfloat);
begin
  glPushMatrix();
  glFrontFace(GL_CCW);
  // Draw the ball using standard textures
  glScalef(AZ, AZ, AZ);     // squash the ball into shape
  glDisable(GL_BLEND);
  glTranslatef(AX, AY, AZ);
  // Draw top sphere
  glColor3f(1.0, 1.0, 1.0);
  glBindTexture(GL_TEXTURE_2D, BallTex);
  gluSphere(MyQuadratic, 1., 32, 32);
  glPopMatrix();
end;

// Method to actually draw on the control
function TOpenGLWndControllerBall.Draw: Boolean;
const
  LENGTH = 0.02;
var
  i: Integer;
  distance, alpha: GLFloat;
begin
  if (not FStarted) then Exit;

  glClearColor(0.0,0.0,0.0,0.0);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

  // draw bitmaps
  for i:= 0 to FCount - 1 do begin
    DrawBmp(FStars[i].CurPos[0], FStars[i].CurPos[1], FStars[i].CurPos[2]);
  end;

  distance:= 0;
  alpha:= 3.75 - sqrt(distance);

  SwapBuffers(wglGetCurrentDC);

  // ok now we check the last alpha value, if it's <= 0.0, everything has faded away, and we restart
  Result:= (alpha <= 0.0);
  if Result
	then Init;
end;

// -----------------------------------------------------------------------------

constructor TOpenGLWndControllerBall.Create(ASpeed: GLFloat; ANum: Integer; ADC: HDC; AFont: HFont; ARect: TRect);
var
  i: Integer;
  RC: HGLRC;
begin
  FSpeed:= ASpeed;
  FCount:= ANum;

  SetLength(FStars, ANum);
  for i:= 0 to Length(FStars) - 1 do begin
    FillChar(FStars[i], SizeOf(TStar), #0);
  end;

  inherited Create(ADC, ARect);

  MyQuadratic := gluNewQuadric();		       // Create A Pointer To The Quadric Object (Return 0 If No Memory) (NEW)
  gluQuadricNormals(MyQuadratic, GLU_SMOOTH);	       // Create Smooth Normals (NEW)
  gluQuadricTexture(MyQuadratic, GL_TRUE);	       // Create Texture Coords (NEW)

  glEnable(GL_TEXTURE_2D);          // Enable Texture Mapping
  glHint(GL_MULTISAMPLE_FILTER_HINT_NV, GL_NICEST);
  glEnable(GL_MULTISAMPLE_ARB);
  glDisable(GL_BLEND);
  glCullFace(GL_BACK);
  glEnable(GL_CULL_FACE);

  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
  gluLookAt(0.0, 0.0, EYE_Z, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0);

  LoadTexture('reflection.bmp', BallTex);
  LoadTexture('reflectionMirror.bmp', ReflectMirrorTex);
end;

procedure TOpenGLWndControllerBall.Init;
const
  PERCENTAGE_WHITE = 0.8; // 80% of the stars are white.  The rest are yellowish - redish
var
  i: Integer;
begin
  inherited Init;
  Randomize();
  for i:= 0  to FCount - 1 do begin
    FStars[i].Start.x:= GetRandom(-5.0, 5.0);
    FStars[i].Start.y:= GetRandom(-5.0, 5.0);
    FStars[i].Start.z:= GetRandom(-100.0, 1.0);

    FStars[i].speed.x:= 0.0;
    FStars[i].speed.y:= 0.0;
    FStars[i].speed.z:= FSpeed;

    FStars[i].Colour[0]:= 1.0;
    if (GetRandom(0.0, 1.0) > PERCENTAGE_WHITE) then begin
      // make it a colorful star
      FStars[i].Colour[1]:= GetRandom(0.5,1.0);
      FStars[i].Colour[2]:= 0.0;
    end else begin
      // make it white
      FStars[i].Colour[1]:= 1.0;
      FStars[i].Colour[2]:= 1.0;
    end;
    FStars[i].timeOffset:= 0.0;
  end;
end;

procedure TOpenGLWndControllerBall.Done;
begin
  SetLength(FStars, 0);
end;

// Moves the text/stars using the current time
procedure TOpenGLWndControllerBall.Idle;
var
  i: Integer;
begin
  inherited Idle;
  // move the stars, calculate new time based on star Start time
  for i:= 0 to FCount - 1 do begin
    // update their z position
    FStars[i].CurPos[2]:= FStars[i].Start.z + FStars[i].speed.z *
      (FTimeElapsed - FStars[i].timeOffset);
    // ok they're out of view, respawn a new star
    if (FStars[i].CurPos[2] >= EYE_Z) then begin
      FStars[i].Start.x:= GetRandom(-5.0, 0.0);
      FStars[i].Start.y:= GetRandom(-5.0, 0.0);
      FStars[i].Start.z:= GetRandom(-100.0, -0.1);
      FStars[i].timeOffset:= FTimeElapsed;
    end else begin
	    FStars[i].CurPos[0]:= FStars[i].Start.x;
	    FStars[i].CurPos[1]:= FStars[i].Start.y;
    end;
  end;
end;

//---------------- TOpenGLWndControllerBox ------------------------------------

//---------------- Display lists -----------------------------------------------

function CreateCubeDisplayList: glUint;
begin
  Result:= glGenLists(1);
  glNewList(Result, GL_COMPILE);
    glBegin(GL_QUADS);
      // Front Face
      glTexCoord2f(0.0, 0.0); glVertex3f(-1.0, -1.0,  1.0);
      glTexCoord2f(1.0, 0.0); glVertex3f( 1.0, -1.0,  1.0);
      glTexCoord2f(1.0, 1.0); glVertex3f( 1.0,  1.0,  1.0);
      glTexCoord2f(0.0, 1.0); glVertex3f(-1.0,  1.0,  1.0);
      // Back Face
      glTexCoord2f(1.0, 0.0); glVertex3f(-1.0, -1.0, -1.0);
      glTexCoord2f(1.0, 1.0); glVertex3f(-1.0,  1.0, -1.0);
      glTexCoord2f(0.0, 1.0); glVertex3f( 1.0,  1.0, -1.0);
      glTexCoord2f(0.0, 0.0); glVertex3f( 1.0, -1.0, -1.0);
      // Top Face
      glTexCoord2f(0.0, 1.0); glVertex3f(-1.0,  1.0, -1.0);
      glTexCoord2f(0.0, 0.0); glVertex3f(-1.0,  1.0,  1.0);
      glTexCoord2f(1.0, 0.0); glVertex3f( 1.0,  1.0,  1.0);
      glTexCoord2f(1.0, 1.0); glVertex3f( 1.0,  1.0, -1.0);
      // Bottom Face
      glTexCoord2f(1.0, 1.0); glVertex3f(-1.0, -1.0, -1.0);
      glTexCoord2f(0.0, 1.0); glVertex3f( 1.0, -1.0, -1.0);
      glTexCoord2f(0.0, 0.0); glVertex3f( 1.0, -1.0,  1.0);
      glTexCoord2f(1.0, 0.0); glVertex3f(-1.0, -1.0,  1.0);
      // Right face
      glTexCoord2f(1.0, 0.0); glVertex3f( 1.0, -1.0, -1.0);
      glTexCoord2f(1.0, 1.0); glVertex3f( 1.0,  1.0, -1.0);
      glTexCoord2f(0.0, 1.0); glVertex3f( 1.0,  1.0,  1.0);
      glTexCoord2f(0.0, 0.0); glVertex3f( 1.0, -1.0,  1.0);
      // Left Face
      glTexCoord2f(0.0, 0.0); glVertex3f(-1.0, -1.0, -1.0);
      glTexCoord2f(1.0, 0.0); glVertex3f(-1.0, -1.0,  1.0);
      glTexCoord2f(1.0, 1.0); glVertex3f(-1.0,  1.0,  1.0);
      glTexCoord2f(0.0, 1.0); glVertex3f(-1.0,  1.0, -1.0);
    glEnd();
  glEndList();
end;

function createRoomDisplayList: GLUInt;
begin
  Result:= glGenLists(1);
  glNewList(Result, GL_COMPILE);
    glBegin(GL_QUADS);
      // Left face
      glNormal3f( 1.0, 0.0, 0.0);
      glTexCoord2f(2.0, 0.0); glVertex3f( -10.0, -10.0, -10.0);
      glTexCoord2f(2.0, 2.0); glVertex3f( -10.0,  10.0, -10.0);
      glTexCoord2f(0.0, 2.0); glVertex3f( -10.0,  10.0,  10.0);
      glTexCoord2f(0.0, 0.0); glVertex3f( -10.0, -10.0,  10.0);

      //Back
      glNormal3f( 0.0, 0.0, 1.0);
      glTexCoord2f(0.0, 0.0); glVertex3f(-10.0, -10.0,  -10.0);
      glTexCoord2f(2.0, 0.0); glVertex3f( 10.0, -10.0,  -10.0);
      glTexCoord2f(2.0, 2.0); glVertex3f( 10.0,  10.0,  -10.0);
      glTexCoord2f(0.0, 2.0); glVertex3f(-10.0,  10.0,  -10.0);

      // Right
      glNormal3f(-1.0, 0.0, 0.0);
      glTexCoord2f(0.0, 0.0); glVertex3f( 10.0, -10.0, -10.0);
      glTexCoord2f(2.0, 0.0); glVertex3f( 10.0, -10.0,  10.0);
      glTexCoord2f(2.0, 2.0); glVertex3f( 10.0,  10.0,  10.0);
      glTexCoord2f(0.0, 2.0); glVertex3f( 10.0,  10.0, -10.0);
    glEnd();
  glEndList();
end;

function createFloorDisplayList: GLUInt;
begin
  Result:= glGenLists(1);
  glNewList(Result, GL_COMPILE);
    glBegin(GL_QUADS);
      // Floor
      glNormal3f( 0.0, 1.0, 0.0);
      glTexCoord2f(0.0, 1.0); glVertex3f(-10.0,  -10.0, -10.0);
      glTexCoord2f(0.0, 0.0); glVertex3f(-10.0,  -10.0,  10.0);
      glTexCoord2f(1.0, 0.0); glVertex3f( 10.0,  -10.0,  10.0);
      glTexCoord2f(1.0, 1.0); glVertex3f( 10.0,  -10.0, -10.0);

      // Roof
      glNormal3f( 0.0,-1.0, 0.0);
      glTexCoord2f(1.0, 1.0); glVertex3f(-10.0, 10.0, -10.0);
      glTexCoord2f(0.0, 1.0); glVertex3f( 10.0, 10.0, -10.0);
      glTexCoord2f(0.0, 0.0); glVertex3f( 10.0, 10.0,  10.0);
      glTexCoord2f(1.0, 0.0); glVertex3f(-10.0, 10.0,  10.0);
    glEnd();
  glEndList();
end;

// Method to actually draw on the control
function TOpenGLWndControllerBox.Draw: Boolean;
const
  LENGTH = 0.02;
var
  i: Integer;
  distance, alpha: GLFloat;
begin
  if (not FStarted) then Exit;
  glClearColor(0.0,0.0,0.0,0.0);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  glLoadIdentity();                                       // Reset The View

  //--- DRAW the cube with the CubeTexture texture ---//
  glColor3f(1, 1, 1);//Set the color to white so that we have a "clean" object to Texture

    glPushMatrix();
      // move and rotate scene
      glTranslatef(1.0, 0.0, -10);
      glRotatef(12*FTimeElapsed, 1, 0, 0);
      glRotatef(12*FTimeElapsed, 0, 1, 0);

      // Bind RoomTexture to the Cube
      glBindTexture(GL_TEXTURE_2D, CubeTexture[0]);
      glCallList(CubeDLs[0]);
      glBindTexture(GL_TEXTURE_2D, CubeTexture[1]);
      glCallList(CubeDLs[1]);
      glBindTexture(GL_TEXTURE_2D, CubeTexture[2]);
      glCallList(CubeDLs[2]);
      glBindTexture(GL_TEXTURE_2D, CubeTexture[3]);
      glCallList(CubeDLs[3]);
      glBindTexture(GL_TEXTURE_2D, CubeTexture[4]);
      glCallList(CubeDLs[4]);
      glBindTexture(GL_TEXTURE_2D, CubeTexture[5]);
      glCallList(CubeDLs[5]);

  glPopMatrix();

    //--- DRAW the Room with the RoomTexture and FloorTexture texture ---//

    glPushMatrix();
      //Move the Room back 24 units
      glTranslatef(0.0, 0.0,-24);

      glBindTexture(GL_TEXTURE_2D, RoomTexture);
      glCallList(RoomDL);
      glBindTexture(GL_TEXTURE_2D, FloorTexture);
      glCallList(FloorDL);
    glPopMatrix();


     //--- DRAW a polygon facing the viewport with the LightTexture texture ---//
     //Push a new matrix so that we dont affect the data already rendered

    glPushMatrix();

      glEnable(GL_BLEND); //Enable blending so that the Light Texture Blends with the Cube and Room we rendered earlier

      //Move the light Polygon in the scene
      glTranslatef(XLightpos,YLightpos,-2.0);

      glBindTexture(GL_TEXTURE_2D, LightTexture);
      glBegin(GL_QUADS);

        glColor3f(0.4, 0.4, 0.4);  //set the color for the Porygon to medium gray so that we get a nice blending effect

        glTexCoord2f(0, 0);  glVertex3f(-0.5,-0.5, 0);
        glTexCoord2f(1, 0);  glVertex3f( 0.5,-0.5, 0);
        glTexCoord2f(1, 1);  glVertex3f( 0.5, 0.5, 0);
        glTexCoord2f(0, 1);  glVertex3f(-0.5, 0.5, 0);
      glEnd();

      glDisable(GL_BLEND);//Disable the blend so we can render other poly's normally

    glPopMatrix();
    

//------------------------------------------------------------------------------

  {
  // draw bitmaps
  for i:= 0 to FStarCount - 1 do begin
    DrawBmp(FStars[i].CurPos[0], FStars[i].CurPos[1], FStars[i].CurPos[2]);
  end;
  }

  distance:= 0;
  alpha:= 3.75 - sqrt(distance);

  SwapBuffers(wglGetCurrentDC);

  // ok now we check the last alpha value, if it's <= 0.0, everything has faded away, and we restart
  Result:= (alpha <= 0.0);
  if Result
	then Init;
end;

// -----------------------------------------------------------------------------

constructor TOpenGLWndControllerBox.Create(ASpeed: GLFloat; ANum: Integer; ADC: HDC; AFont: HFont; ARect: TRect);
var
  i: Integer;
  RC: HGLRC;
begin
  FSpeed:= ASpeed;
  FCount:= ANum;

  SetLength(FStars, ANum);
  for i:= 0 to Length(FStars) - 1 do begin
    FillChar(FStars[i], SizeOf(TStar), #0);
  end;

  inherited Create(ADC, ARect);

  MyQuadratic := gluNewQuadric();		       // Create A Pointer To The Quadric Object (Return 0 If No Memory) (NEW)
  gluQuadricNormals(MyQuadratic, GLU_SMOOTH);	       // Create Smooth Normals (NEW)
  gluQuadricTexture(MyQuadratic, GL_TRUE);	       // Create Texture Coords (NEW)

  glShadeModel(GL_SMOOTH);                 // Enables Smooth Color Shading
  //glEnable(GL_CULL_FACE);
  glEnable(GL_TEXTURE_2D);          // Enable Texture Mapping
  // glTexGenf(GL_S, GL_TEXTURE_GEN_MODE, GL_SPHERE_MAP);
  // glTexGenf(GL_T, GL_TEXTURE_GEN_MODE, GL_SPHERE_MAP);
  glClearDepth(1.0);                       // Depth Buffer Setup
  glEnable(GL_DEPTH_TEST);                 // Enable Depth Buffer
  glDepthFunc(GL_LESS);		           // The Type Of Depth Test To Do
  glBlendFunc(GL_SRC_ALPHA, GL_ONE);

  glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);   //Realy Nice perspective calculations

//
  glHint(GL_MULTISAMPLE_FILTER_HINT_NV, GL_NICEST);
  glEnable(GL_MULTISAMPLE_ARB);
  glDisable(GL_BLEND);
  glCullFace(GL_BACK);
  glEnable(GL_CULL_FACE);

  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
  gluLookAt(0.0, 0.0, EYE_Z, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0);
//
  
  LoadTexture('marble.jpg', CubeTexture[0]);  // Load the Texture
  LoadTexture('texture1.jpg', CubeTexture[1]);  // Load the Texture
  LoadTexture('marble.jpg', CubeTexture[2]);  // Load the Texture
  LoadTexture('walls.jpg', CubeTexture[3]);  // Load the Texture
  LoadTexture('marble.jpg', CubeTexture[4]);  // Load the Texture
  LoadTexture('ceiling.jpg', CubeTexture[5]);  // Load the Texture

  LoadTexture('texture1.jpg', LightTexture);  // Load the Texture
  LoadTexture('walls.jpg', RoomTexture);  // Load the Texture
  LoadTexture('ceiling.jpg', FloorTexture);  // Load the Texture

  XLightpos:= 0.0;
  YLightpos:= 0.0;

  CubeDL:= CreateCubeDisplayList;
  // Front Face
  CubeDLs[0]:= glGenLists(1);
  glNewList(CubeDLs[0], GL_COMPILE);
  glBegin(GL_QUADS);
  glTexCoord2f(0.0, 0.0); glVertex3f(-1.0, -1.0,  1.0);
  glTexCoord2f(1.0, 0.0); glVertex3f( 1.0, -1.0,  1.0);
  glTexCoord2f(1.0, 1.0); glVertex3f( 1.0,  1.0,  1.0);
  glTexCoord2f(0.0, 1.0); glVertex3f(-1.0,  1.0,  1.0);
  glEnd();
  glEndList();

  // Back Face
  CubeDLs[1]:= glGenLists(1);
  glNewList(CubeDLs[1], GL_COMPILE);
  glBegin(GL_QUADS);
  glTexCoord2f(1.0, 0.0); glVertex3f(-1.0, -1.0, -1.0);
  glTexCoord2f(1.0, 1.0); glVertex3f(-1.0,  1.0, -1.0);
  glTexCoord2f(0.0, 1.0); glVertex3f( 1.0,  1.0, -1.0);
  glTexCoord2f(0.0, 0.0); glVertex3f( 1.0, -1.0, -1.0);
  glEnd();
  glEndList();

  // Top Face
  CubeDLs[2]:= glGenLists(1);
  glNewList(CubeDLs[2], GL_COMPILE);
  glBegin(GL_QUADS);
  glTexCoord2f(0.0, 1.0); glVertex3f(-1.0,  1.0, -1.0);
  glTexCoord2f(0.0, 0.0); glVertex3f(-1.0,  1.0,  1.0);
  glTexCoord2f(1.0, 0.0); glVertex3f( 1.0,  1.0,  1.0);
  glTexCoord2f(1.0, 1.0); glVertex3f( 1.0,  1.0, -1.0);
  glEnd();
  glEndList();

  // Bottom Face
  CubeDLs[3]:= glGenLists(1);
  glNewList(CubeDLs[3], GL_COMPILE);
  glBegin(GL_QUADS);
  glTexCoord2f(1.0, 1.0); glVertex3f(-1.0, -1.0, -1.0);
  glTexCoord2f(0.0, 1.0); glVertex3f( 1.0, -1.0, -1.0);
  glTexCoord2f(0.0, 0.0); glVertex3f( 1.0, -1.0,  1.0);
  glTexCoord2f(1.0, 0.0); glVertex3f(-1.0, -1.0,  1.0);
  glEnd();
  glEndList();

  // Right face
  CubeDLs[4]:= glGenLists(1);
  glNewList(CubeDLs[4], GL_COMPILE);
  glBegin(GL_QUADS);
  glTexCoord2f(1.0, 0.0); glVertex3f( 1.0, -1.0, -1.0);
  glTexCoord2f(1.0, 1.0); glVertex3f( 1.0,  1.0, -1.0);
  glTexCoord2f(0.0, 1.0); glVertex3f( 1.0,  1.0,  1.0);
  glTexCoord2f(0.0, 0.0); glVertex3f( 1.0, -1.0,  1.0);
  glEnd();
  glEndList();

  // Left Face
  CubeDLs[5]:= glGenLists(1);
  glNewList(CubeDLs[5], GL_COMPILE);
  glBegin(GL_QUADS);
  glTexCoord2f(0.0, 0.0); glVertex3f(-1.0, -1.0, -1.0);
  glTexCoord2f(1.0, 0.0); glVertex3f(-1.0, -1.0,  1.0);
  glTexCoord2f(1.0, 1.0); glVertex3f(-1.0,  1.0,  1.0);
  glTexCoord2f(0.0, 1.0); glVertex3f(-1.0,  1.0, -1.0);
  glEnd();
  glEndList();


  FloorDL:= createFloorDisplayList;
  RoomDL:= createRoomDisplayList;
end;

procedure TOpenGLWndControllerBox.Init;
const
  PERCENTAGE_WHITE = 0.8; // 80% of the stars are white.  The rest are yellowish - redish
var
  i: Integer;
begin
  inherited Init;
  Randomize();
  for i:= 0  to FCount - 1 do begin
    FStars[i].Start.x:= GetRandom(-5.0, 5.0);
    FStars[i].Start.y:= GetRandom(-5.0, 5.0);
    FStars[i].Start.z:= GetRandom(-100.0,1.0);

    FStars[i].speed.x:= 0.0;
    FStars[i].speed.y:= 0.0;
    FStars[i].speed.z:= FSpeed;

    FStars[i].Colour[0]:= 1.0;
    if (GetRandom(0.0, 1.0) > PERCENTAGE_WHITE) then begin
      // make it a colorful star
      FStars[i].Colour[1]:= GetRandom(0.5,1.0);
      FStars[i].Colour[2]:= 0.0;
    end else begin
      // make it white
      FStars[i].Colour[1]:= 1.0;
      FStars[i].Colour[2]:= 1.0;
    end;
    FStars[i].timeOffset:= 0.0;
  end;
end;

procedure TOpenGLWndControllerBox.Done;
begin
  SetLength(FStars, 0);
end;

// Moves the text/stars using the current time
procedure TOpenGLWndControllerBox.Idle;
var
  i: Integer;
begin
  inherited Idle;
  // move the stars, calculate new time based on star Start time
  for i:= 0 to FCount - 1 do begin
    // update their z position
    FStars[i].CurPos[2]:= FStars[i].Start.z + FStars[i].speed.z * (FTimeElapsed - FStars[i].timeOffset);
    // ok they're out of view, respawn a new star
    if (FStars[i].CurPos[2] >= EYE_Z) then begin
      FStars[i].Start.x:= GetRandom(-5.0,5.0);
      FStars[i].Start.y:= GetRandom(-5.0,5.0);
      FStars[i].Start.z:= GetRandom(-100.0, -0.1);
      FStars[i].timeOffset:= FTimeElapsed;
    end else begin
	    FStars[i].CurPos[0]:= FStars[i].Start.x;
	    FStars[i].CurPos[1]:= FStars[i].Start.y;
    end;
  end;
end;

end.
