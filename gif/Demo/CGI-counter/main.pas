unit main;
////////////////////////////////////////////////////////////////////////////////
//                                                                            //
// Project:	TGIFImage demo application.                                   //
// Description:	CGI Hit Counter using TGIFImage to create GIF images.         //
// Copyright	(c) 1997-99 Anders Melander.                                  //
//		All rights reserved.                                          //
// Formatting:	2 space indent, 8 space tabs, 80 columns.                     //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////
//                                                                            //
// The CGI Counter has been tested on the following platforms:                //
// * Netscape FastTrack 2.0 using NSAPI on Windows 95                         //
// * Netscape Enterprise Server 3.5 using NSAPI on Windows NT 4               //
// * Microsoft IIS 3.0 using ISAPI on Windows NT 4                            //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////
interface

uses
  GIFImage,
  Registry,
  Windows, Messages, SysUtils, Classes, graphics, HTTPApp;

type
  TWebModuleCounter = class(TWebModule)
    procedure WebActionItemCounterAction(Sender: TObject;
      Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
    procedure WebModuleCounterCreate(Sender: TObject);
    procedure WebModuleCounterDestroy(Sender: TObject);
  private
    { Private declarations }
    GIFImage		: TGIFImage;
    procedure LoadSetup(Item: string);
    procedure LoadDomains(Key, Item: string; List: TStrings);
    procedure IncrementCounter(Item: string; Increment: boolean);
  public
    { Public declarations }
    ValidReferers	: TStringList;
    IgnoreReferers	: TStringList;
    Count		: integer;
    Registry		: TRegistry;

    ImageDigits		,
    ImageBackground	: string; // Future

    ImageWidth		,
    ImageHeight		: integer;
    Interlaced		,
    Transparent		,
    DigitShadow		: boolean;
    ShadowHeight	,
    ImageMargin		: integer;
    ColorBackground	,
    ColorTransparent	,
    ColorFrame		,
    ColorDigits		,
    ColorShadow		: TColor;
    FrameWidth		: integer;
    DigitSize		: integer;
    DigitFont		,
    MessageFont		: string;
    CountFormat		: string;
    Message		: string;
  end;

var
  WebModuleCounter: TWebModuleCounter;

implementation

uses
  WinSock;

{$R *.DFM}

const
  // Registry paths
  ConfigRoot		= HKEY_LOCAL_MACHINE;
  ConfigPath		= '\SOFTWARE\Melander\CGI-Counter';

type
  TCacheItem = record
    IP: DWORD;
    Time: TDateTime;
  end;
  PCacheItem = ^TCacheItem;

var
  // Global client cache used to avoid that a reload increments counters
  ClientCache: TThreadList;

const
  MaxCacheSize: integer = 32;	// Max number of IP #s recorded
  MaxCacheTime: integer = 60000;// Max number of milli seconds to remember an IP #

procedure TWebModuleCounter.WebModuleCounterCreate(Sender: TObject);
begin
  GIFImage := TGIFImage.Create;
  Registry := TRegistry.Create;
  Registry.RootKey := ConfigRoot;
  ValidReferers := TStringList.Create;
  IgnoreReferers := TStringList.Create;
end;

procedure TWebModuleCounter.WebModuleCounterDestroy(Sender: TObject);
begin
  ValidReferers.Free;
  IgnoreReferers.Free;
  Registry.Free;
  GIFImage.Free;
end;

// Read and optionally Increment the counter value stored in the registry
procedure TWebModuleCounter.IncrementCounter(Item: string; Increment: boolean);
  procedure DoIncrementCounter(path: string; Increment: boolean);
  begin
    if not(Registry.OpenKey(Path, False)) then
      exit;
    try
      if Registry.ValueExists('Count') then
        Count := Registry.ReadInteger('Count');
      if (Increment) then
        Inc(Count);
      Registry.WriteInteger('Count', Count);
    finally
      Registry.CloseKey;
    end;
  end;
begin
  Count := 0;
  // Increment global counter
  DoIncrementCounter(ConfigPath, Increment);
  // Increment item-specific counter
  if (Item <> '') then
  begin
    Count := 0;
    DoIncrementCounter(ConfigPath+'\'+item, Increment);
  end;
end;

// Load a list of domains (or any other list for that matter)
procedure TWebModuleCounter.LoadDomains(Key, Item: string; List: TStrings);
  procedure DoLoadDomains(path: string);
  var
    s			: string;
    i			: integer;
  begin
    if not(Registry.OpenKey(Path, False)) then
      exit;
    try
      i := 0;
      while Registry.ValueExists(Key+IntToStr(i)) do
      begin
        s := Registry.ReadString(Key+IntToStr(i));
        if (s <> '') then
          List.Add(s);
        inc(i);
      end;
    finally
      Registry.CloseKey;
    end;
  end;
begin
  List.Clear;
  // Load global valid-referers
  DoLoadDomains(ConfigPath);
  // Load item-specific valid-referers
  if (Item <> '') then
    DoLoadDomains(ConfigPath+'\'+item);
end;

// Load configuration from registry
procedure TWebModuleCounter.LoadSetup(Item: string);
const
  MsgAbout		= 'TGIFImage'+#13+'CGI Counter';

  procedure DoLoadSetup(path: string);
    function ReadInteger(key: string; default: integer): integer;
    begin
      if Registry.ValueExists(key) then
        Result := Registry.ReadInteger(key)
      else
        Result := default;
    end;

    function ReadString(key: string; default: string): string;
    begin
      if Registry.ValueExists(key) then
        Result := Registry.ReadString(key)
      else
        Result := default;
    end;

    function ReadBool(key: string; default: boolean): boolean;
    begin
      if Registry.ValueExists(key) then
        Result := Registry.ReadBool(key)
      else
        Result := default;
    end;

    function ReadColor(key: string; default: TColor): TColor;
    begin
      if Registry.ValueExists(key) then
        Result := StringToColor(Registry.ReadString(key))
      else
        Result := default;
    end;

  begin
    if not(Registry.OpenKey(Path, False)) then
      exit;
    try
      ColorBackground := ReadColor('ColorBackground', ColorBackground);
      ColorDigits := ReadColor('ColorDigits', ColorDigits);
      ColorFrame := ReadColor('ColorFrame', ColorFrame);
      ColorTransparent := ReadColor('ColorTransparent', ColorTransparent);
      ColorShadow := ReadColor('ColorShadow', ColorShadow);
      ShadowHeight := ReadInteger('ShadowHeight', ShadowHeight);
      ImageMargin := ReadInteger('ImageMargin', ImageMargin);
      FrameWidth := ReadInteger('FrameWidth', FrameWidth);
      Transparent := ReadBool('Transparent', Transparent);
      Interlaced := ReadBool('Interlaced', Interlaced);
      DigitShadow := ReadBool('DigitShadow', DigitShadow);
      ImageWidth := ReadInteger('ImageWidth', ImageWidth);
      ImageHeight := ReadInteger('ImageHeight', ImageHeight);
      DigitFont := ReadString('DigitFont', DigitFont);
      DigitSize := ReadInteger('DigitSize', DigitSize);
      MessageFont := ReadString('MessageFont', MessageFont);
      CountFormat := ReadString('CountFormat', CountFormat);
      Message := ReadString('Message', Message);
    finally
      Registry.CloseKey;
    end;
  end;

begin
  // Set defaults
  ColorBackground := clWhite;
  ColorDigits := clBlack;
  ColorFrame := clGray;
  ColorTransparent := clWhite;
  ColorShadow := clGray;
  ShadowHeight := 2;
  ImageMargin := 5;
  FrameWidth := 1;
  Transparent := True;
  Interlaced := True;
  DigitShadow := True;
  ImageWidth := -1;
  ImageHeight := -1;
  DigitFont := 'Arial';
  DigitSize := 20;
  MessageFont := 'Arial Narrow';
  CountFormat := '%.6d';
  Message := MsgAbout;

  // Read global/default setup
  DoLoadSetup(ConfigPath);
  // Read item-specific setup
  if (Item <> '') then
    DoLoadSetup(ConfigPath+'\'+item);
end;

procedure TWebModuleCounter.WebActionItemCounterAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
var
  MemoryStream		: TMemoryStream;
  CountStr		: string;
  Index			: integer;
  n			: integer;
  Ext			: TGIFGraphicControlExtension;
  RefererOK		,
  IgnoreReferer		: boolean;
  Item			: string;
  CountRect		: TRect;
  Bitmap		: TBitmap;
const
  MsgGoAway		= 'Bad referer'+#13+'Go away!';

  // Add a bitmap to the GIF
  function AddBitmap(Bitmap: TBitmap; Animated: boolean): integer;
  begin
    Result := GIFImage.Add(Bitmap);
    // Optimize palette to remove unused entries from windows palette
    GIFImage.Images[Result].ColorMap.Optimize;
    // Set interlace
    GIFImage.Images[Result].Interlaced := Interlaced;
    // Set transparency
    if (Transparent) or (Animated) then
    begin
      Ext := TGIFGraphicControlExtension.Create(GIFImage.Images[Result]);
      GIFImage.Images[Result].Extensions.Add(Ext);
      Ext.Transparent := Transparent;
      if (Transparent) then
        Ext.TransparentColor := ColorTransparent;
      Ext.Delay := 500;
      Ext.Disposal := dmBackground;
    end;
  end;

  // Add a message to the GIF
  function AddMessage(Msg: string): integer;
  var
    h,w			: integer;
    TextRect		: TRect;
  begin
    if (Msg = '') then
    begin
      Result := -1;
      exit;
    end;
    Bitmap.Canvas.Brush.Color := ColorBackground;
    Bitmap.Canvas.Brush.Style := bsSolid;
    Bitmap.Canvas.FillRect(Bitmap.Canvas.ClipRect);
    Bitmap.Canvas.Brush.Style := bsClear;
    Bitmap.Canvas.Font.Name := MessageFont;
    Bitmap.Canvas.Font.Height := 20;
    Bitmap.Canvas.Font.Color := ColorDigits;
    // Setup bounding rectangle
    TextRect := Bitmap.Canvas.ClipRect;
    // Calculate height and width of message with an arbitrary font size
    DrawText(Bitmap.Canvas.Handle, PChar(Msg), -1, TextRect,
      DT_CALCRECT or DT_CENTER or DT_NOPREFIX);
    // Calculate the largest font that will fit on bitmap
    h := MulDiv(Bitmap.Height, 20, TextRect.Bottom);
    w := MulDiv(Bitmap.Width, 20, TextRect.Right);
    if (h < w) then
      Bitmap.Canvas.Font.Height := h
    else
      Bitmap.Canvas.Font.Height := w;
    // Calculate height and width of message with calculated font size
    TextRect := Bitmap.Canvas.ClipRect;
    DrawText(Bitmap.Canvas.Handle, PChar(Msg), -1, TextRect,
      DT_CALCRECT or DT_CENTER or DT_NOPREFIX);
    // Center text rect vertically
    TextRect.Right := Bitmap.Width;
    OffsetRect(TextRect, 0, (Bitmap.Height - TextRect.Bottom) DIV 2);
    // Draw message
    DrawText(Bitmap.Canvas.Handle, PChar(Msg), -1, TextRect,
      DT_CENTER or DT_NOPREFIX);

    // Add message image
    Result := AddBitmap(Bitmap, True);
  end;

  // Search for a domain or IP # in a list of domains/super domains
  function CheckDomain(Domain: string; List: TStrings): boolean;
  var
    i			: integer;
  begin
    Result := False;
    // Strip request protocol
    if (pos('http://', Domain) > 0) then
      delete(Domain, 1, pos('http://', Domain)+length('http://')-1);
    // Strip path
    if (pos('/', Domain) > 0) then
      Domain := copy(Domain, 1, pos('/', Domain)-1);
    // Validate domain
    for i := 0 to List.Count-1 do
      // To allow sub-domains, check from right to left for DNS names (may start with '.')
      if (copy(List[i],1,1) = '.') or (copy(List[i],length(List[i]),1) <> '.') then
      begin
        if (copy(Domain, length(Domain)-length(List[i])+1, length(List[i])) = List[i]) then
        begin
          Result := True;
          exit;
        end;
      end else
      // To allow sub-domains, check from right to left for IP numbers (ends with '.')
      begin
        if (copy(Domain, 1, length(List[i])) = List[i]) then
        begin
          Result := True;
          exit;
        end;
      end;
  end;

  // Search client cache for an IP #.
  // Refresh the entry if it is found, otherwise add a new entry
  function IPInCache(IP: DWORD): boolean;
  var
    CacheItem		: PCacheItem;
    TimeNow		: DWORD;
    i			: integer;
    List		: TList;
  begin
    Result := False;
    TimeNow := GetTickCount;
    List := ClientCache.LockList;
    try
      i := List.Count-1;
      while (i >= 0) do
      begin
        // Check for expired entry
        if (TimeNow-PCacheItem(List.Items[i])^.Time > MaxCacheTime) then
        begin
          Dispose(PCacheItem(List.Items[i]));
          List.Delete(i);
          Dec(i);
          Continue;
        end;
        if (PCacheItem(List.Items[i])^.IP = IP) then
        begin
          // Update time
          PCacheItem(List.Items[i])^.Time := TimeNow;
          // Move to front
          List.Move(i, 0);
          Result := True;
          exit;
        end;
        Dec(i);
      end;
      // Not in cache so add it
      New(CacheItem);
      CacheItem^.Time := TimeNow;
      CacheItem^.IP := IP;
      List.Insert(0, CacheItem);
      // Check for cache full and shrink if so
      while (List.Count > MaxCacheSize) do
      begin
        Dispose(PCacheItem(List.Items[List.Count-1]));
        List.Delete(List.Count-1);
      end;
    finally
      ClientCache.UnlockList;
    end;
  end;

begin
  if not(Request.MethodType in [mtGET, mtHEAD]) then
    exit;
  // Set up the response header
  Response.StatusCode := 200;
  Response.LastModified := Now;
  Response.Expires := 0;
  Response.Date := Now;
  Response.ContentType := 'image/gif';
  Response.Title := 'TGIFImage Counter Demo';
  if (Request.MethodType = mtHEAD) then
  begin
    Handled := True;
    exit;
  end;

  // Isolate the counted item
  Item := Request.PathInfo;
  while(pos('/', Item) > 0) do
    delete(Item, 1,1);

  // Check for valid refering site
  LoadDomains('ValidReferer', Item, ValidReferers);
  RefererOK := CheckDomain(Request.Referer, ValidReferers);

  // Only update/display counter values if we have a valid referer
  if (RefererOK) then
  begin
    // Check if refering site should increment counter
    LoadDomains('IgnoreReferer', Item, IgnoreReferers);
    // Do not increment counter if:
    // 1) The referer is in the "ignore" list
    IgnoreReferer := CheckDomain(Request.Referer, IgnoreReferers) or
    // 2) The request is "passive"
      (Uppercase(Request.QueryFields.Values['Passive']) = 'TRUE') or
    // 3) The client is in the IP# cache (probably because he/she is reloading a page)
      IPInCache(inet_addr(PChar(Request.RemoteAddr)));

    // Load and Increment counter
    IncrementCounter(Item, not(IgnoreReferer));
    // Load item-specific setup
    LoadSetup(Item);
  end else
  begin
    // Load global setup
    LoadSetup('');
    // Set values for bad referer
    Count := 0;
    Message := MsgGoAway;
  end;
  // Format count string
  CountStr := format(CountFormat, [Count]);

  // Now we are ready to construct the bitmaps
  Bitmap := TBitmap.Create;
  try
    // To draw anti-aliased text on systems that support it, the bitmap (and
    // the display driver on the server - stupid) must have at least 16 bit
    // colors or better.
    Bitmap.PixelFormat :=  pf16bit;

    // Set digit font and size
    Bitmap.Canvas.Font.Name := DigitFont;
    Bitmap.Canvas.Font.Height := DigitSize;
    // Calculate height and width of the count text
    if (ImageHeight <= 0) or (ImageWidth <= 0) then
    begin
      CountRect := Rect(0,0,0,0);
      DrawText(Bitmap.Canvas.Handle, PChar(CountStr), Length(CountStr), CountRect,
        DT_SINGLELINE or DT_CALCRECT or DT_CENTER or DT_VCENTER or DT_NOPREFIX);
    end;
    // Calculate and set height and width of bitmap
    if (ImageHeight > 0) then
    begin
      Bitmap.Height := ImageHeight;
      CountRect.Bottom := ImageHeight;
    end else
      Bitmap.height := CountRect.Bottom+2*ImageMargin+ShadowHeight;
    if (ImageWidth > 0) then
    begin
      Bitmap.Width := ImageWidth;
      CountRect.Left := ImageWidth;
    end else
      Bitmap.Width := CountRect.Right+2*ImageMargin+ShadowHeight;

    // Paint background
    Bitmap.Canvas.Brush.Color := ColorBackground;
    Bitmap.Canvas.Brush.Style := bsSolid;
    Bitmap.Canvas.FillRect(Bitmap.Canvas.ClipRect);
    // Draw frame
    if (FrameWidth > 0) then
    begin
      Bitmap.Canvas.Pen.Color := ColorFrame;
      Bitmap.Canvas.Pen.Mode := pmCopy;
      Bitmap.Canvas.Pen.Style := psInsideFrame;
      Bitmap.Canvas.Pen.Width := FrameWidth;
      Bitmap.Canvas.Rectangle(0,0, Bitmap.Width-1, Bitmap.Height-1);
    end;
    // Draw digits
    Bitmap.Canvas.Brush.Style := bsClear;

    // Setup a rectangle to center the count text on the bitmap
    CountRect.Left := 0;
    CountRect.Top := 0;
    CountRect.Right := Bitmap.Width;
    CountRect.Bottom := Bitmap.Height;

    if (DigitShadow) then
    begin
      // First draw shadow of digits...
      n := ShadowHeight DIV 2;
      Bitmap.Canvas.Font.Color := ColorShadow;
      OffsetRect(CountRect, n, n);
      DrawText(Bitmap.Canvas.Handle, PChar(CountStr), -1, CountRect,
        DT_SINGLELINE or DT_CENTER or DT_VCENTER or DT_NOPREFIX);
      // ...then digits
      OffsetRect(CountRect, -ShadowHeight, -ShadowHeight);
    end;
    Bitmap.Canvas.Font.Color := ColorDigits;
    DrawText(Bitmap.Canvas.Handle, PChar(CountStr), -1, CountRect,
      DT_SINGLELINE or DT_CENTER or DT_VCENTER or DT_NOPREFIX);

    // Create GIF
    GIFImage.Clear;

    // Add counter image
    Index := AddBitmap(Bitmap, True);
    // Add Netscape Loop Extension
    GIFImage.Images[Index].Extensions.Insert(0, TGIFAppExtNSLoop.Create(GIFImage.Images[Index]));

    // Add a message to image
    if (Message <> '') then
      AddMessage(Message);

    // Tell bad referers to go away
    if not(RefererOK) then
    begin
      AddMessage(MsgGoAway);
      AddMessage(Request.Referer);
    end;

  finally
    // Now we dont need the bitmap anymore
    Bitmap.Free;
  end;

  // Stream the GIF file to a memory stream
  MemoryStream := TMemoryStream.Create;
  try
    GIFImage.SaveToStream(MemoryStream);
    Response.ContentLength := MemoryStream.Size;
    MemoryStream.Position := 0;
  except
    // If sh*t happened we must free the memory stream ourself and bail out
    MemoryStream.Free;
    raise;
  end;
  // That's it - Hand the memory stream containing the GIF file over to
  // the dispatcher. The dispatcher will free it when it's done with it.
  Response.ContentStream := MemoryStream;
  Handled := True;
end;

initialization
  // Create the global client cache
  ClientCache := TThreadList.Create;
finalization
  // Delete the global client cache
  with (ClientCache.LockList) do
    try
      // Zap all entries
      while (Count > 0) do
      begin
        Dispose(PCacheItem(Items[0]));
        Delete(0);
      end;
    finally
      ClientCache.UnlockList;
    end;
  ClientCache.Free;
end.

// Please ignore the following code. It's just something I'm working on...

var
  // Image cache - Must have been loaded with 10 TBitmaps of equal height
  Images: array[0..9] of TBitmap;

function IntToGIF(Value: integer; Width: integer): TGIFImage;
var
  s: string;
  i: integer;
  Width: integer;
  Bitmap: TBitmap;
begin
  s := format('%.*d', [Width, Value]);
  // Convert ASCII to decimal
  for i := 1 to length(s) do
    s[i] := chr(ord(s[i])-ord('0'));


  // Calculate width of GIF
  Width := 0;
  for i := 1 to length(s) do
    inc(Width, Images[ord(s[i])].Width);

  // Create work bitmap
  Bitmap := TBitmap.Create;
  try
    // Set size of bitmap
    Bitmap.Height := Images[0].height;
    Bitmap.Width := Width;

    // Draw your gradient here...

    // Draw each digit on the bitmap
    Width := 0;
    for i := 1 to length(s) do
    begin
      Bitmap.Canvas.Draw(Width, 0, Images[ord(s[i])]);
      inc(Width, Images[ord(s[i])].Width);
    end;

    Result := TGIFimage.Create;
    try
      // Convert bitmap to GIF
      GIF.Assign(Bitmap);
    except
      Result.Free;
      raise;
    end;
  finally
    Bitmap.Free;
  end;
end;

