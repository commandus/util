unit zdownload;

interface
uses
  Windows, Registry, WinSock, SysUtils, Classes, ZLib,
  NMFtp, NMHttp,
  util1;

{ just decompress string }
function DecompressString(ACompression: Integer; ABuf: String): String;
{ протоколы http и file, ftp не поддерживается
  ACompression: 0- нет компрессии, 1- компрессия
}
function ReadCompressedFromUrl(ACompression: Integer; AUrl: String;
  Chttp: TNMHTTP; Cftp: TNMftp): String;

implementation

function DecompressString(ACompression: Integer; ABuf: String): String;
const
  BUFSIZE= 255;
var
  Mem: TMemoryStream;
  DCS: TCustomZLibStream;
  S: String[BUFSIZE];
  b: Integer;
begin
  Result:= '';
  case ACompression of
  1: begin
      Mem:= TMemoryStream.Create;
      Mem.WriteBuffer(ABuf[1], Length(ABuf));
      DCS:= TDecompressionStream.Create(Mem);
      SetLength(S, BUFSIZE);
      DCS.Seek(0, soFromBeginning);
      repeat
        b:= DCS.Read(S[1], BUFSIZE);
        Result:= Result + S;
      until b < BUFSIZE;
      DCS.Free;
      Mem.Free;
    end;
  else begin
    // default no compression
    Result:= ABuf;
    end;
  end;
end;

{ протоколы http и file, ftp не поддерживается
  ACompression: 0- нет компрессии, 1- компрессия
}
function ReadCompressedFromUrl(ACompression: Integer; AUrl: String;
  Chttp: TNMHTTP; Cftp: TNMftp): String;
var
  Proxy: String;
  SPort: Integer;
  rhost: String;
  protocol, ip, fn, bookmark: String;
  port: Integer;
begin
  Result:= '';
  util1.ParseUrl(AUrl, protocol, rhost, ip, fn, bookmark, port, 'http', 80);
  if rhost = ''
  then rhost:= ip;
  if protocol = 'file' then begin
    Result:= util1.LoadString(Copy(AUrl, Pos('//', AUrl)+2, MaxInt));
    Result:= DecompressString(ACompression, Result);
    Exit;
  end;

  if (Cftp <> Nil) and (protocol = 'ftp') then begin
    if ReadIEProxySettings(protocol, rhost, Proxy, SPort) then begin
      Cftp.Port:= 0;
      Cftp.Proxy:= Proxy;
      Cftp.ProxyPort:= SPort;
    end else begin
      Cftp.Port:= 80;
      Cftp.Proxy:= '';
      Cftp.ProxyPort:= 0;
    end;
    {
    CFtp.Connect;
    CFtp.Mode(MODE_IMAGE);
    Result:= CHttp.Body;
    case ACompression of
    1: Result:= DecompressString(Result);
    end;
    }
    Exit;
  end;
  if (Chttp <> Nil) and (protocol = 'http') then begin
    if ReadIEProxySettings(protocol, rhost, Proxy, SPort) then begin
      Chttp.Port:= 0;
      Chttp.Proxy:= Proxy;
      Chttp.ProxyPort:= SPort;
    end else begin
      Chttp.Port:= 80;
      Chttp.Proxy:= '';
      Chttp.ProxyPort:= 0;
    end;
    CHttp.Get(AUrl);
    Result:= CHttp.Body;
    Result:= DecompressString(ACompression, Result);
    Exit;
  end;
end;

end.
