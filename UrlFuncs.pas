unit
  UrlFuncs;
(*##*)
(*******************************************************************************
*                                                                             *
*   u  r  l  F  u  n  c  s                                                     *
*                                                                             *
*   Routines for URL and associated actions (icons)                            *
*                                                                             *
*   Copyright (c) 2007, Andrei Ivanov, RealThinComponents                      *
*   No components                                                             *
*   Conditional defines:                                                       *
*                                                                             *
*   Last Revision: Oct 31 2007                                                 *
*   Last fix:                                                                 *
*   Lines:         390                                                         *
*   History:                                                                  *
*   Notes                                                                      *
*                                                                             *
********************************************************************************)
(*##*)

interface

{ FileExtIcon()
  Get Icon associated with file extension in URL or file name
  Parameters:
    AExtension - .ext file extension
  Return
    AIconFN     - icon file name
    AIconNumber - icon number
}
function FileExtIcon(const AExtension: String; var AIconFN: String; var AIconNumber: Integer): Boolean;

{ ReplaceDNInLdapUrl
  replace DN in ldap url
}
function ReplaceDNInLdapUrl(const ANewDN: String; const AUrl: String): String;

// function IsValidHostName(host: String): Boolean;
{ ReplaceDNInLdapUrl
  Validate IPv4 address
}
function IsIPaddress(const host: String): Boolean;

{ IsFtpUrl
  Checks is url started with 'ftp://'
}
function IsFtpUrl(const AUrl: String): Boolean;

{ IsLdapUrl
  Checks is url started with 'ldap://'
}
function IsLdapUrl(const AUrl: String): Boolean;

{ IsHttpUrl
  Checks is url started with 'http://'
}
function IsHttpUrl(const AUrl: String): Boolean;

{ ParseUrl()
  "http:" "//" [user:password@] host [ ":" port ] [ abs_path ]
  return True, if ok
  return False if url is empty, or is DOS file name (port=80 anyway)
  bookamrk is strted with # if exists
}
function ParseUrl(url: String; var protocol, user, password, host, IPaddress, fn, bookmark: String; var port: Integer;
  const ADefprotocol: String; ADefPort: Integer): Boolean;

{ ParseFtpUrl(
  substitute anonymous user and password if user does not specified
}
function ParseFtpUrl(url: String; var protocol, user, password, host, IPaddress, fn, bookmark: String; var port: Integer;
  const ADefprotocol: String; ADefPort: Integer): Boolean;

{ ParseLdapUrl()
  parse ldap url with default substitutes
}
function ParseLdapUrl(url: String; var protocol, user, password, host, baseDN, Attributes, Scope, Filter: String; var port: Integer;
  const ADefprotocol: String; ADefPort: Integer): Boolean;

{ ComposeUrl()
  compose http, ftp urls
}
function ComposeUrl(const AProtocol, AUser, APassword, AHost, AFn, ABookmark: String; Aport: Integer): String;

{ ComposeLdapUrl()
  compose ldap url
}
function ComposeLdapUrl(const AProtocol, AUser, APassword, AHost,
  ADn, AAttr, AScope, AFilter: String; Aport: Integer): String;

implementation
uses
  SysUtils, Windows, StrUtils, Registry,
  ShellApi;

function FileExtIcon(const AExtension: String; var AIconFN: String; var AIconNumber: Integer): Boolean;
var
  Reg: TRegistry;
  r, FileType: String;
  p: Integer;
begin
  Result:= False;
  r:= '';
  AIconNumber:= 0;
  Reg:= TRegistry.Create;
  try
    with Reg do begin
      RootKey:= HKEY_CLASSES_ROOT;
      if KeyExists(AExtension) then begin
        // read file type description
        if OpenKeyReadOnly(AExtension) then begin
          // is file type link exists
          FileType:= ReadString('');
          // read file type description
          if OpenKeyReadOnly('\' + FileType) then begin
            // file icon
            if OpenKeyReadOnly('DefaultIcon')
            then r:= ReadString('');
          end;
        end;
      end;
    end;
    if Length(r) > 0 then begin
      // %SystemRoot%
      SetLength(r, MAX_PATH);
      ShellApi.DoEnvironmentSubst(PChar(r), MAX_PATH);
      r:= PChar(r); // remove #0 and other junk
      p:= Pos(',', r);
      if p > 0 then begin
        AIconFN:= Copy(r, 1, p - 1);
        AIconNumber:= StrToIntDef(Copy(r, p + 1, MaxInt), 0);
      end else begin
        AIconFN:= r;
        AIconNumber:= 0;
      end;
      Result:= True;
    end;
  finally
    Reg.Free;
  end;
end;

function IsValidHostName(host: String): Boolean;
begin
  Result:= (Pos(#9, host) = 0) and
    (Pos(#32, host) = 0) and
    (Pos('/', host) = 0) and
    (Pos(':', host) = 0) and
    (Pos('\', host) = 0);
end;

function IsIPaddress(const host: String): Boolean;
var
  curp, L: Integer;
  dotposition: array [0..4] of Byte;
  code: Integer;
  v: Cardinal;
begin
  L:= Length(host);
  IsIPaddress:= False;
  dotposition[0]:= 1;
  curp:= 1;
  repeat
    dotposition[curp]:= PosEx('.', host, dotposition[curp - 1]);
    if dotposition[curp] = 0
    then dotposition[curp]:= L + 1;
    Val(Copy(host, dotposition[curp-1], dotposition[curp] - dotposition[curp-1]), v, code);
    if (code = 0)
    then Inc(dotposition[curp])
    else Exit;
    Inc(curp);
  until curp > 4;
  if dotposition[4] <> L + 2
  then Exit;
  IsIPaddress:= True;
end;

// is url started with 'ftp://'
function IsFtpUrl(const AUrl: String): Boolean;
begin
  Result:= (Length(AUrl) > 5) and (Pos('ftp://', AUrl) = 1);
end;

// is url started with 'ldap://'
function IsLdapUrl(const AUrl: String): Boolean;
begin
  Result:= (Length(AUrl) > 6) and (Pos('ldap://', AUrl) = 1);
end;

// is url started with 'http://'
function IsHttpUrl(const AUrl: String): Boolean;
begin
  Result:= (Length(AUrl) > 6) and (Pos('http://', AUrl) = 1);
end;

function ParseUrl(url: String; var protocol, user, password, host, IPaddress, fn, bookmark: String; var port: Integer;
  const ADefprotocol: String; ADefPort: Integer): Boolean;
var
  L, p, pport, pfn: Integer;
  S: String;
  code: Integer;
begin
  ParseUrl:= False;
  user:= '';
  password:= '';
  host:= '';
  IPAddress:= '';
  Port:= ADefPort; { port missed, return 80 }
  fn:= '';
  bookmark:= '';
  L:= Length(url);
  if L = 0
  then Exit;
  { bookmark }
  p:= PosEx('#', url, 1);
  if p > 0 then begin
    bookmark:= Copy(url, p, MaxInt);
    Delete(url, p, MaxInt);
  end;

  { protocol }
  p:= Pos(':', url);  { Pos return 0 on fail }
  if p > 0
  then protocol:= LowerCase(Copy(url, 1, p-1))
  else protocol:= ADefProtocol;             { protocol missed, return "http" }
  { //host }
  Inc(p);
  if Pos('//', url) = p then begin    { "//" does not specified.. }
    Inc(p, 2);
  end;
  if protocol = 'file' then  begin
    fn:= Copy(url, p, MaxInt);
    Result:= True;
    Exit;
  end;

  pfn:= PosEx('/', url, p);         { search abs_path part of url }
  if pfn > 0 then begin
    fn:= Copy(url, pfn, L - pfn + 1);
  end else begin
    fn:= '';                          { abs_path missed, return "/" }
    pfn:= L + 1;
  end;

  { default port and host }
  host:= Copy(url, p, pfn - p);
  // if '@' in host, there user[password]
  pport:= PosEx('@', host, 1);
  if pport > 0 then begin
    // host starts from after '@'
    Inc(p, pport);
    user:= Copy(host, 1, pport - 1);
    Delete(host, 1, pport);
    // if ':' in user, there password
    pport:= Pos(':', user);
    if pport > 0 then begin
      password:= Copy(user, pport + 1, MaxInt);
      Delete(user, pport, MaxInt);
    end;
  end;

  pport:= PosEx(':', url, p);
  if pport > 0 then begin
    S:= Copy(url, pport + 1, pfn - pport - 1);
    Val(S, port, code);
    if code = 0 then begin
      host:= Copy(url, p, pport - p);
    end;
  end;
  { usually host name returned, but if '/' or '.htm[l]' suffix specified- it is file name }
  { pp:= PosBack('.', UpperCase(host)); }
  if (Pos('/', host) > 0) or (Pos('\', host) > 0) then begin  { or ((pp>0) and (pp <= (Length(host)-3))) }
    fn:= host;
    host:= '';
    if (Pos('\', fn) > 0) then begin
      { ms-dos path }
      if Length(protocol) = 1 then begin
        fn:= protocol + ':' + fn;
        protocol:= 'file';
      end;
    end;
    { it is absolutely wrong url, return True }
    Result:= True;
  end else begin
    if IsIPaddress(host) then begin { for example, 127.0.0.1 - Address, no host }
      IPAddress:= host;
    end else begin
      IPAddress:= '';
      if not IsValidHostName(host)
      then Exit;
    end;
    if host = '.'
    then host:= '';
    Result:= True;
  end;
end;

function ParseFtpUrl(url: String; var protocol, user, password, host, IPaddress, fn, bookmark: String; var port: Integer;
  const ADefprotocol: String; ADefPort: Integer): Boolean;
begin
  Result:= ParseUrl(url, protocol, user, password, host, IPaddress, fn, bookmark, port, ADefprotocol, ADefPort);
  if (Length(user) = 0) then begin
    user:= 'anonymous';
    if (Length(password) = 0)
    then password:= user + '@' + host;
  end;
end;

function ParseLdapUrl(url: String; var protocol, user, password, host, baseDN, Attributes, Scope, Filter: String; var port: Integer;
  const ADefprotocol: String; ADefPort: Integer): Boolean;
var
  bookmark, IPaddress: String;
  s: String;
  p0, p1, len: Integer;
begin
  Result:= ParseUrl(url, protocol, user, password, host, IPaddress, baseDN, bookmark, port, ADefprotocol, ADefPort);
  if Length(bookmark) > 0
  then baseDN:= baseDN + '#' + bookmark;

  s:= basedn;
  p0:= 1;
  len:= Length(s) + 1;
  p1:= PosEx('?', s, p0);
  if p1 <= 0 then p1:= len;
  basedn:= Copy(s, p0, p1 - p0);
  p0:= p1 + 1;

  p1:= PosEx('?', s, p0);
  if p1 <= 0 then p1:= len;
  Attributes:= Copy(s, p0, p1 - p0);
  p0:= p1 + 1;

  p1:= PosEx('?', s, p0);
  if p1 <= 0 then p1:= len;
  Scope:= Copy(s, p0, p1 - p0);
  p0:= p1 + 1;

  p1:= PosEx('?', s, p0);
  if p1 <= 0 then p1:= len;
  Filter:= Copy(s, p0, p1 - p0);
  p0:= p1 + 1;

  // delete '/' from basedn
  if (Length(baseDN) > 0) and (basedn[1] = '/')
  then Delete(basedn, 1, 1);
end;

function ComposeUrl(const AProtocol, AUser, APassword, AHost, AFn, ABookmark: String; Aport: Integer): String;
begin
  if Length(AProtocol) > 0
  then Result:= AProtocol + '://'
  else Result:= '';
  if Length(AUser) > 0
  then Result:= Result + AUser + ':' + APassword + '@';
  Result:= Result + AHost;
  if APort > 0
  then Result:= Result + ':' + IntToStr(APort);
  if (Length(AFn) > 0) or (Length(ABookmark) > 0) then begin
    Result:= Result + '/' + AFn;
    if Length(ABookmark) > 0
    then Result:= Result + '#' + ABookmark;
  end;
end;

function ComposeLdapUrl(const AProtocol, AUser, APassword, AHost,
  ADn, AAttr, AScope, AFilter: String; Aport: Integer): String;
var
  ldapurltail, sc, f: String;
  l: Integer;
begin
  sc:= Trim(AScope);
  f:= Trim(AFilter);
  if (Length(sc) > 0) and (Upcase(sc[1]) = 'B')
  then sc:= '';
  if CompareText('(objectClass=*)', f) = 0
  then f:= '';

  ldapurltail:= Trim(ADN) + '?' + Trim(AAttr) + '?' + sc + '?' + f;
  // delete last '?'
  l:= Length(ldapurltail);
  while l > 0 do begin
    if ldapurltail[l] <> '?'
    then Break;
    Dec(l);
  end;
  Delete(ldapurltail, l + 1, MaxInt);
  Result:= ComposeUrl('ldap', Trim(AUser), Trim(APassword), Trim(AHost), ldapurltail, '', APort);
end;

{ replace DN in ldap url, return True if success }
function ReplaceDNInLdapUrl(const ANewDN: String; const AUrl: String): String;
var
  protocol, user, password, host, DN, Attributes, Scope, Filter: String;
  ldapPort: Integer;
const
  Defprotocol = 'ldap';
  DefPort = 389;
begin
  if ParseLdapUrl(Aurl, protocol, user, password, host, DN, Attributes, Scope, Filter, ldapPort,
    Defprotocol, DefPort)
  then Result:= ComposeLdapUrl(Protocol, user, password, host, ANewDN, Attributes, Scope, Filter, ldapPort)
  else Result:= AUrl;
end;


end.
