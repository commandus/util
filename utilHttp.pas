unit
  utilHttp;
(*##*)
(*******************************************************************
*                                                                 *
*   U  T  I  L  H  T  T  P   http loader component                 *
*                                                                 *
*   Copyright (c) 1999, 2000 A.Ivanov. All rights reserved.        *
*   Based on Delphi 4 TPageProducer component                     *
*   Conditional defines:                                           *
*                                                                 *
*   Last Revision: May 19 1999                                     *
*   Last fix     :                                                *
*   Lines        :                                                 *
*   History      : see CHANGES.TXT file                           *
*   Printed      : ---                                             *
*                                                                 *
********************************************************************)
(*##*)

interface
uses
  Classes,  Windows, SysUtils, Controls, ExtCtrls,
  scktcomp, Graphics,
  util1, UrlFuncs;

type
  THttpGifLoader = class(TImage)
  private
    FStarted: Boolean;
    FLoaded: Boolean;
    FIsResourced: Boolean;
    FResFile: String;
    FUrl: String;
    FClickUrl: String;
    FData: String;
    FClientSocket: TClientSocket;
    FProxy: String;
    FProxyPort: Integer;
    FGifImage: TGraphic;
    FLastLoad: TDateTime;
    FNextTimeOutSec: Integer;
    FPreloadedImageList: TStrings;
    FPreloadedImageIndex: Integer;
    function GetDataPtr: Pointer;
    function GetDataLen: Integer;
    procedure SetUrl(AValue: String);
    procedure SetProxy(AValue: String);
    procedure SetProxyPort(AValue: Integer);
    procedure SetTimeOutSec(AValue: Integer);
    procedure SetNextTimeOutSec(AValue: Integer);
    procedure SetStarted(AStart: Boolean);
    procedure SetDataGIF(const AData: String);
    procedure SetClickUrl(AClickURL: String);
    procedure SetPreloadedImageIndex(AIndex: Integer);
    procedure FClientSocketWrite(Sender: TObject; Socket: TCustomWinSocket);
    procedure FClientSocketDisconnect(Sender: TObject; Socket: TCustomWinSocket);
    procedure FClientSocketError(Sender: TObject; Socket: TCustomWinSocket; ErrorEvent: TErrorEvent; var ErrorCode: Integer);
    procedure FClientSocketRead(Sender: TObject; Socket: TCustomWinSocket);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    procedure ReadProxySettings;
    procedure LoadPreloadedBannersFromINIFile(const AIni: String; const ARootFolder: String);
    function  LoadPreloadedBannersFromResource(const AResourceFile: String; const AIni: String; const ARootFolder: String): Boolean;
    procedure NextPreloadedImage;
    property DataPtr: Pointer read GetDataPtr;
  published
    property Started: Boolean read FStarted write SetStarted;
    property IsLoaded: Boolean read FLoaded;
    property DataGIF: String read FData write SetDataGIF;
    property PreloadedImageIndex: Integer read FPreloadedImageIndex write SetPreloadedImageIndex;
    property DataLen: Integer read GetDataLen;
    property Url: String write SetUrl;
    property ClickUrl: String read FClickUrl write SetClickUrl;
    property Proxy: String write SetProxy;
    property ProxyPort: Integer write SetProxyPort;
    property TimeOutSec: Integer write SetTimeOutSec;
    property NextTimeOutSec: Integer read FNextTimeOutSec write SetNextTimeOutSec;
    property IsResourced: Boolean read FIsResourced;
  end;

function LoadGIF(const AResourceFile, AResourceName: String; APicture: TPicture; 
  const AResType: String = ''; const AFmt: Byte = 0): Boolean;  // AFmt - 0 by name, 1 - JPEG 2 - GIF

function LoadGIF2Bitmap(const AResourceFile, AResourceName: String; ABitmap: TBitmap;
  const AResType: String = ''; const AFmt: Byte = 0): Boolean;  // AFmt - 0 by name, 1 - JPEG 2 - GIF

implementation
{.$DEFINE USE_LITEGIF}
uses
  Jpeg,
{$IFDEF USE_LITEGIF}
  litegif2,
 {$ELSE}
  GifImage,
{$ENDIF}
  IniFiles;

function ExtractHeader(const Header, Fld: String): String;
var
  sl: TStrings;
  i: Integer;
  p: Integer;
  uc, uc1: String;
begin
  Result:= '';
  sl:= TStringList.Create;
  sl.Text:= Header;
  uc:= ANSIUpperCase(Fld);
  for i:= 0 to sl.Count - 1 do begin
    uc1:= ANSIUppercase(sl[i]);
    if Pos(uc, uc1) = 1 then begin
      p:= Pos(':', uc1);
      if p>0 then begin
        Result:= Copy(sl[i], p+2, MaxInt);
        util1.DeleteLeadTerminateSpaceStr(Result);
        Exit;
      end;
    end;
  end;
  sl.Free;
end;

procedure THttpGifLoader.SetStarted(AStart: Boolean);
begin
  if AStart then begin
    if FStarted
    then Exit;

    if Now < FLastLoad + (FNextTimeOutSec/sysutils.SecsPerDay)
    then Exit;

    FData:= '';
    try
      FClientSocket.Active:= True;
      FClientSocket.Socket.SendText('');
      FLastLoad:= Now;
    except
      Exit;
    end;
    FStarted:= True;
    FLoaded:= False;
  end else begin
    if FClientSocket.Active then begin
      try
        FClientSocket.Active:= False;
        // FLastLoad:= 0.0;
      except
      end;
    end;
    FStarted:= False;
    FLoaded:= False;
  end;
end;

procedure THttpGifLoader.SetDataGIF(const AData: String);
var
  t: TStream;
begin
  FData:= AData;
  t:= TStringStream.Create(FData);
  FGifImage.LoadFromStream(t);
  Picture.Graphic:= FGifImage;
  // Self.Left:= 0;
  Self.Width:= FGifImage.Width;
  Self.Height:= FGifImage.Height;
  t.Free;
  FStarted:= False;
  FLoaded:= True;
end;

procedure THttpGifLoader.SetClickUrl(AClickURL: String);
begin
  FClickUrl:= AClickURL;
end;

procedure THttpGifLoader.SetPreloadedImageIndex(AIndex: Integer);
var
  s: String;
  p, p1: Integer;
begin
  if AIndex >= FPreloadedImageList.Count
  then Exit;
  FPreloadedImageIndex:= AIndex;
  s:= FPreloadedImageList[AIndex];
  p:= Pos('=', s);
  if FIsResourced then begin
    if LoadGIF(FResFile, Copy(s, 1, p - 1), Picture) then begin
      // Self.Width:= FGifImage.Width;
      // Self.Height:= FGifImage.Height;
      FStarted:= False;
      FLoaded:= True;
    end;
  end else begin
    DataGIF:= util1.File2String(Copy(s, 1, p - 1));
  end;
  p1:= Pos('|', s);
  ClickUrl:= Copy(s, p + 1, p1 - p - 1);
  // img:= Copy(s, p1 + 1, MaxInt);
end;

procedure THttpGifLoader.SetUrl(AValue: String);
var
  protocol, user, password, host, IP, fn, bookmark: String;
  port: Integer;
begin
  urlFuncs.ParseUrl(AValue, protocol, user, password, host, IP, fn, bookmark, port, 'http',80 );
  if ansiCompareStr(protocol, 'http') <> 0
  then raise Exception.CreateFmt('invalid protocol: %s', [protocol]);
  if FProxy = '' then begin
    FClientSocket.Host:= host;
    FClientSocket.Port:= port;
    FUrl:= fn;
  end else begin
    FClientSocket.Host:= FProxy;
    FClientSocket.Port:= FProxyPort;
    FUrl:= host+':'+IntToStr(port)+fn;
  end;
end;

procedure THttpGifLoader.SetProxy(AValue: String);
begin
  FProxy:= AValue;
end;

procedure THttpGifLoader.SetProxyPort(AValue: Integer);
begin
  FProxyPort:= AValue;
end;

procedure THttpGifLoader.SetTimeOutSec(AValue: Integer);
begin
  // FClientSocket.Socket.TimeOut:= AValue * 1000; // ms
end;

procedure THttpGifLoader.SetNextTimeOutSec(AValue: Integer);
begin
  FNextTimeOutSec:= AValue;
  FLastLoad:= Now;
end;

procedure THttpGifLoader.FClientSocketDisconnect(Sender: TObject; Socket: TCustomWinSocket);
var
  p: Integer;
begin
  p:= Pos(#13#10#13#10, FData);
  if (p > 0) and
    (ANSICompareText(ExtractHeader(Copy(FData, 1, p), 'Content-Type'), 'image/gif') = 0) then begin
    Delete(FData, 1, p+3);
    SetDataGIF(FData);
  end;
end;

procedure THttpGifLoader.FClientSocketError(Sender: TObject; Socket: TCustomWinSocket; ErrorEvent: TErrorEvent; var ErrorCode: Integer);
begin
  // ClientSocket1.Active:= False;
  ErrorCode:= 0;
  FStarted:= False;
  FLoaded:= False;
  // SetStarted(True);
end;

procedure THttpGifLoader.Clear;
begin
  FData:= '';
end;

function THttpGifLoader.GetDataPtr: Pointer;
begin
  Result:= @FData;
end;

function THttpGifLoader.GetDataLen: Integer;
begin
  Result:= Length(FData);
end;

constructor THttpGifLoader.Create(AOwner: TComponent);
begin
  inherited;
  FIsResourced:= False;
  FPreloadedImageList:= TStringList.Create;
  FPreloadedImageIndex:= -1;  // indicate do not use preloaded images
  FClickUrl:= '';
  try
    FClientSocket:= TClientSocket.Create(Self);
  except
  end;
  FProxy:= '';
  FProxyPort:= 0;

  FClientSocket.OnRead:= FClientSocketRead;
  FClientSocket.OnWrite:= FClientSocketWrite;
  FClientSocket.OnDisconnect:= FClientSocketDisconnect;
  FClientSocket.OnError:= FClientSocketError;
{$IFDEF USE_LITEGIF}
  FGifImage:= ThtBitmap.Create;
{$ELSE}
  FGifImage:= TGifImage.Create;
{$ENDIF}
  FStarted:= False;
  FLoaded:= False;
  FNextTimeOutSec:= 0; // sec, no wait to load next image
  FLastLoad:= 0.0;       // TDateTime
  ShowHint:= True;
  Clear;
  Stretch:= False;
  AutoSize:= True;
end;

destructor THttpGifLoader.Destroy;
begin
  SetStarted(False);
  FGifImage.Free;
  FClientSocket.Free;
  FPreloadedImageList.Free;
  inherited;
end;

procedure THttpGifLoader.LoadPreloadedBannersFromINIFile(const AIni: String; const ARootFolder: String);
var
  ini: TInifile;
  cnt: Integer;
  img, pre, click: String;
  deffn: String;
begin
// util1.String2File('c:\a.txt', 'LoadPreloadedBannersFromINIFile' +#13#10);
  FPreloadedImageList.Clear;
  try
    ini:= TIniFile.Create(util1.ConcatPath(ARootFolder, AIni));
  except
    Exit;
  end;
  try
    cnt:= ini.ReadInteger('common', 'count', 0);
    FIsResourced:= ini.ReadInteger('common', 'resource', 0) > 0;
    if FIsResourced then begin
      deffn:= ini.ReadString('common', 'defaultimage', '');
      FResFile:= ini.ReadString('common', 'resourcefile', '')
    end else deffn:= util1.ConcatPath(ARootFolder, ini.ReadString('common', 'defaultimage', ''));
    for cnt:= 1 to cnt do begin
      if FIsResourced then begin
        pre:= ini.ReadString('pre', IntToStr(cnt), '');
      end else begin
        pre:= util1.ConcatPath(ARootFolder, ini.ReadString('pre', IntToStr(cnt), ''));
        if not FileExists(pre)
        then pre:= deffn;
      end;
      img:= ini.ReadString('img', IntToStr(cnt), '');
      click:= ini.ReadString('url', IntToStr(cnt), '');
      FPreloadedImageList.Add(pre + '=' +  click + '|' + img);
    end;
  except
  end;
  ini.Free;
end;

function THttpGifLoader.LoadPreloadedBannersFromResource(const AResourceFile: String; const AIni: String; const ARootFolder: String): Boolean;
var
  rs: TResourceStream;
  hlib: HINST;
  f: TFileStream;
  fn: array[0..2 * MAX_PATH] of Char;
  temppath: array[0..MAX_PATH + 1] of Char;
begin
  Result:= False;
  hlib:= Windows.LoadLibrary(PChar(AResourceFile));
  if hlib > 0 then begin
    try
      rs:= TResourceStream.Create(hlib, AIni, 'INI');
    except
      FreeLibrary(hlib);
      Exit;
    end;
    GetTempPath(254, temppath);
    GetTempFileName(temppath, 'apoo', 0, fn);
    try
      f:= TFileStream.Create(fn, fmCreate);
      f.CopyFrom(rs, rs.Size);
      f.Free;
      LoadPreloadedBannersFromINIFile(fn, ARootFolder);
      SysUtils.DeleteFile(fn);
    except
    end;
    FreeLibrary(hlib);
    Result:= True
  end;
end;

procedure THttpGifLoader.NextPreloadedImage;
begin
  if Height + Top > 4 then begin
    Top:= Top - 4;
  end else begin
    if PreloadedImageIndex + 1 >= FPreloadedImageList.Count
    then PreloadedImageIndex:= 0
    else PreloadedImageIndex:= PreloadedImageIndex + 1;
    Top:= 0;
  end;
end;

procedure THttpGifLoader.ReadProxySettings;
var
  vProxy: String;
  vProxyPort: Integer;
begin
  if util1.ReadIEProxySettings('http', FClientSocket.Host, vProxy, vProxyPort) then begin
    Proxy:= vProxy;
    ProxyPort:= vProxyPort;
  end;
end;

procedure THttpGifLoader.FClientSocketRead(Sender: TObject; Socket: TCustomWinSocket);
begin
  try
    FData:= FData + Socket.ReceiveText;
  except
  end;
end;

procedure THttpGifLoader.FClientSocketWrite(Sender: TObject; Socket: TCustomWinSocket);
begin
  Socket.SendText('GET ' + FURL + ' HTTP/1.0'#13#10+
    'Connection: Keep-Alive'#13#10+
    'User-Agent: Mozilla/4.51 [en] (WinNT; I)'#13#10+
//  'Host: '+ FClientSocket.Host + #13#10+
    'Accept: image/gif, image/jpeg, */*'#13#10#13#10);
end;

{$IFDEF USE_LITEGIF}
function LoadGIF(const AResourceFile, AResourceName: String; APicture: TPicture): Boolean;
var
  rs: TResourceStream;
  hlib: HINST;
  g: TGraphic;
  hr: THandle;
  rn: String;
begin

// util1.String2File('c:\a.txt', 'LoadGif name:' + AResourceName + ' file:' + AResourceFile + ' ' +#13#10);
  Result:= False;
  hlib:= Windows.LoadLibrary(PChar(AResourceFile));
  if hlib > 0 then begin
    rn:= 'GIF';
    hr:= FindResource(hlib, PChar(AResourceName), PChar(rn));
    if hr = 0 then begin
      rn:= 'JPEG';
      hr:= FindResource(hlib, PChar(AResourceName), PChar(rn));
    end;
    if hr = 0
    then Exit;
    try
      rs:= TResourceStream.Create(hlib, AResourceName, PChar(rn));
    except
      FreeLibrary(hlib);
      Exit;
    end;

    if rn[1] = 'G'
    then g:= ThtBitmap.Create
    else g:= TJPEGImage.Create;

    try
      g.LoadFromStream(rs);
      // cannot stops drawing if gif is animated. Is there bug in TGifImage?
      if rn[1] = 'G'
      then APicture.Graphic:= TGifImage(g).Bitmap
      else APicture.Graphic:= TJPEGImage(g);
    except
    end;
    g.Free;
    FreeLibrary(hlib);
    Result:= True
  end;
end;

function LoadGIF2Bitmap(const AResourceFile, AResourceName: String; ABitmap: TBitmap;
  const AResType: String = ''; const AFmt: Byte = 0): Boolean;  // AFmt - 0 by name, 1 - JPEG 2 - GIF
var
  pict: TPicture;
begin
  pict:= TPicture.Create;
  Result:= LoadGIF(AResourceFile, AResourceName, Pict, AResType, AFmt);
  if Result then begin
    ABitmap.Assign(pict.Graphic);
  end;
  pict.Free;
end;

{$ELSE}

function LoadGIF(const AResourceFile, AResourceName: String; APicture: TPicture;
  const AResType: String = ''; const AFmt: Byte = 0): Boolean;  // AFmt - 0 by name, 1 - JPEG 2 - GIF
var
  rs: TResourceStream;
  hlib: HINST;
  g: TGraphic;
  hr: THandle;
  rn: String;
begin
// util1.String2File('c:\a.txt', 'LoadGif name:' + AResourceName + ' file:' + AResourceFile + ' ' +#13#10);
  Result:= False;
  hlib:= Windows.LoadLibrary(PChar(AResourceFile));
  if hlib > 0 then begin
    if Length(AResType) > 0 then begin
      rn:= AResType;
      hr:= FindResource(hlib, PChar(AResourceName), PChar(rn));
    end else begin
      rn:= 'GIF';
      hr:= FindResource(hlib, PChar(AResourceName), PChar(rn));
      if hr = 0 then begin
        rn:= 'JPEG';
        hr:= FindResource(hlib, PChar(AResourceName), PChar(rn));
      end;
    end;
    if hr = 0
    then Exit;
    try
      rs:= TResourceStream.Create(hlib, AResourceName, PChar(rn));
    except
      FreeLibrary(hlib);
      Exit;
    end;
    GIFImageDefaultDrawOptions:= GIFImageDefaultDrawOptions - [goAnimate, goLoop, goLoopContinously] - [goAsync, goDirectDraw];
    // Exclude(GIFImageDefaultDrawOptions, goDirectDraw);
    case AFmt of
    0: begin
        if rn[1] = 'G'
        then g:= TGIFImage.Create
        else g:= TJPEGImage.Create;
       end;
    1: begin
        g:= TJPEGImage.Create;
       end;
    2: begin
        g:= TGIFImage.Create;
       end;
    end;

    try
      g.LoadFromStream(rs);
      // cannot stops drawing if gif is animated. Is there bug in TGifImage?
      //if Assigned(APicture.Graphic) and (APicture.Graphic is TGIFImage)
      //then TGIFImage(APicture.Graphic).StopDraw;
      //
      //APicture.Graphic:= g;
      //
// util1.String2File('c:\a.txt', 'LoadGif 2' +#13#10);
      if rn[1] = 'G'
      then APicture.Graphic:= TGifImage(g).Bitmap
      else APicture.Graphic:= TJPEGImage(g);
// util1.String2File('c:\a.txt', 'LoadGif 3' +#13#10);      
      Include(GIFImageDefaultDrawOptions, goDirectDraw);
    except
    end;
    g.Free;
    FreeLibrary(hlib);
    Result:= True
  end;
end;

function LoadGIF2Bitmap(const AResourceFile, AResourceName: String; ABitmap: TBitmap;
  const AResType: String = ''; const AFmt: Byte = 0): Boolean;  // AFmt - 0 by name, 1 - JPEG 2 - GIF
var
  pict: TPicture;
begin
  pict:= TPicture.Create;
  Result:= LoadGIF(AResourceFile, AResourceName, Pict, AResType, AFmt);
  if Result then begin
    ABitmap.Assign(pict.Graphic);
  end;
  pict.Free;
end;
{$ENDIF}

end.
