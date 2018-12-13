unit
  UtilISAPI;
(*##*)
(*******************************************************************
*                                                                 *
*   U  T  I  L  I  S  A  P  I    isapi access routines for Delphi  *
*                                                                 *
*                                                                  *
*   Copyright © 2000, Andrei Ivanov. All rights reserved.         *
*   Conditional defines:                                           *
*                                                                 *
*   Last Revision: May 23 2000                                     *
*   Last fix     : May 23 2000                                    *
*   Lines        : 304                                             *
*   History      :                                                *
*   Printed      : ---                                             *
*                                                                 *
********************************************************************)
(*##*)

interface

{$IFDEF VER140}{$DEFINE D6+}{$ENDIF}
{$IFDEF VER150}{$DEFINE D6+}{$ENDIF}
{$IFDEF VER160}{$DEFINE D6+}{$ENDIF}
{$IFDEF VER170}{$DEFINE D6+}{$ENDIF}
{$IFDEF VER180}{$DEFINE D6+}{$ENDIF}
{$IFDEF VER190}{$DEFINE D6+}{$ENDIF}
uses
  Windows, SysUtils, Classes, ISAPI,
{$IFDEF D6+}
    IsapiHTTP, HTTPProd,
{$ENDIF}

  HttpApp, ISAPIApp, ieprot,
  util1;

type
  TGetExtensionVersion = function (var Ver: THSE_VERSION_INFO): Bool; stdcall;
  THttpExtensionProc = function (var ECB: TEXTENSION_CONTROL_BLOCK ): DWORD; stdcall;


function GetVersionISAPI(AModuleName: String): String;

// CallISAPI load library, execute and unload
function CallISAPI(AProt: TProtocol; AModuleName, AMethod, APathInfo: String;
  AParameterlist, AServerVarList: TStrings; var AStatusCode: Integer; var AStatusText: String): String;

// or you can load DLL, and then call
// load library
function LoadISAPI(AModuleName: String; var Handle: LongWord; var HttpExtensionProc: THttpExtensionProc): Boolean;
// function do call loaded DLL (Handle, HttpExtensionProc)
function DoCallISAPI(AProt: TProtocol; Handle: LongWord; HttpExtensionProc: THttpExtensionProc;
  AMethod, APathInfo: String; AParameterlist, AServerVarList: TStrings): String;
  //  var AStatusCode: Integer; var AStatusText: String): String;

function GetServerVarList(ARequest: TWebRequest): TStrings;
function ReadStringSmallPortions(ARequest: TWebRequest; var S: String): Boolean;
{ utility }
function CreateParameterList(AList: TStrings): PChar;

implementation
const
  ServerVariables: array[0..28] of string = (
    '*METHOD',
    'SERVER_PROTOCOL',
    'URL',
    '*GET_QUERY_STRING',
    '*PATH_INFO',
    '*PATH_TRANSLATED',
    'HTTP_CACHE_CONTROL',
    'HTTP_DATE',
    'HTTP_ACCEPT',
    'HTTP_FROM',
    'HTTP_HOST',
    'HTTP_IF_MODIFIED_SINCE',
    'HTTP_REFERER',
    'HTTP_USER_AGENT',
    'HTTP_CONTENT_ENCODING',
    'CONTENT_TYPE',
    'CONTENT_LENGTH',
    'HTTP_CONTENT_VERSION',
    'HTTP_DERIVED_FROM',
    'HTTP_EXPIRES',
    'HTTP_TITLE',
    'REMOTE_ADDR',
    'REMOTE_HOST',
    'SCRIPT_NAME',
    'SERVER_PORT',
    '*POST_DATA',
    'HTTP_CONNECTION',
    'HTTP_COOKIE',
    'HTTP_AUTHORIZATION');

type
  TConnRec = record
    prot: TProtocol; 
    ResultString: ^String;
    ServerVars: TStrings;
  end;

  PConnRec = ^TConnRec;

// TGetServerVariableProc = function(hConn: HCONN; VariableName: PChar; Buffer: Pointer; var Size: DWORD): BOOL stdcall;
// request for server variables
function MyGetServerVariableProc(ConnID: HCONN; VariableName: PChar; Buffer: Pointer; var Size: DWORD): BOOL stdcall;
var
  Dest: PConnRec;
begin
  Result:= True;
  Dest:= Pointer(ConnID);
  StrPLCopy(Buffer, Dest^.ServerVars.Values[VariableName], Size);
// util1.String2File('a.txt', Format('%s=%s', [VariableName, Dest^.ServerVars.Values[VariableName]]));
  Size:= Length(PChar(Buffer))+1;
  // Dest^.Reserved^:= Dest^.ServerVars.Values[VariableName];
  // Buffer:= @(Dest^.Reserved^[1]);
  // if Length(Dest^.Reserved^) >= Size then SetLength(Dest^.Reserved^, Size-1);
end;

// TWriteClientProc = function(ConnID: HCONN; Buffer: Pointer; var Bytes: DWORD; dwReserved: DWORD ): BOOL stdcall;
function MyWriteClientProc(ConnID: HCONN; Buffer: Pointer; var Bytes: DWORD;
  dwReserved: DWORD): BOOL stdcall;
var
  L: Cardinal;
  Dest: PConnRec;
begin
  Result:= True;
  Dest:= Pointer(ConnID);
  L:= Length(Dest^.ResultString^);
  SetLength(Dest^.ResultString^, L + Bytes);
  if Buffer <> Nil
  then Move(Buffer^, Dest^.ResultString^[L+1], Bytes);
end;

// TReadClientProc  = function(ConnID: HCONN; Buffer: Pointer; var Size: DWORD ): BOOL stdcall;
function MyReadClientProc(ConnID: HCONN; Buffer: Pointer; var Bytes: DWORD;
  dwReserved: DWORD ): BOOL stdcall;
begin
  Result:= True;
end;

// TServerSupportFunctionProc = function(hConn: HCONN; HSERRequest: DWORD; Buffer: Pointer; var Size: DWORD; var DataType: DWORD ): BOOL stdcall;
function MyServerSupportFunction(hConn: HCONN; HSERRequest: DWORD; Buffer: Pointer; var Size: DWORD; var DataType: DWORD ): BOOL stdcall;
var
  Dest: PConnRec;
  s: String;
begin
  Result:= False;
  Dest:= Pointer(HConn);
  case HSERRequest of
    HSE_REQ_SEND_URL_REDIRECT_RESP: begin // This sends a 302 (URL Redirect) message to the client. No further processing is needed after the call. This operation is similar to specifying "URI: <URL>" in a CGI script header. The lpvBuffer variable should point to a null-terminated string of URL. The variable lpdwSize should have the size of lpvBuffer. The variable lpdwDataType is ignored.
      // Dest^.prot.Start('/index.htm', )
      {
      s:= 'index.htm';
      Size:= Length(s);
      PChar(Buffer):= @s[1];
      }
      Result:= True;
    end;
    HSE_REQ_SEND_URL: ; // This sends the data specified by the URL to the client as if the client had requested that URL. The null-terminated URL pointed to by lpvBuffer must be on the server and must not specify protocol information (that is, it must begin with a "/"). No further processing is required after this call. The parameter lpdwSize points to a DWORD holding the size of lpvBuffer. The parameter lpdwDataType is ignored.
    HSE_REQ_SEND_RESPONSE_HEADER:;  //This sends a complete HTTP server response header including the status, server version, message time, and MIME version. The ISAPI application should append other HTTP headers such as the content type and content length, followed by an extra "\r\n". This function only takes textual data, up to the first '\0' terminator
    HSE_REQ_MAP_URL_TO_PATH:; // The lpvBuffer parameter is a pointer to the buffer that contains the logical path on entry and the physical path on exit. The lpdwSize parameter is a pointer to the DWORD containing the size of the buffer passed in lpvBuffer on entry, and the number of bytes placed in the buffer on exit. The lpdwDataType parameter is ignored.
    HSE_REQ_DONE_WITH_SESSION:; //
  end;
end;

function GetVersionISAPI(AModuleName: String): String;
var
  GetExtensionVersion: TGetExtensionVersion;
  Handle: LongWord;
  FTHSE_VERSION_INFO: THSE_VERSION_INFO;
begin
  Handle:= LoadLibrary(PChar(AModuleName));
  if Handle <> 0 then begin
    @GetExtensionVersion:= GetProcAddress(Handle, 'GetExtensionVersion');
    if @GetExtensionVersion <> nil then  begin
      if GetExtensionVersion(FTHSE_VERSION_INFO) then begin
        Result:= FTHSE_VERSION_INFO.lpszExtensionDesc;
      end else begin
        Result:= 'Error';
      end;
    end else begin
      raise Exception.CreateFmt('No GetExtensionVersion in DLL %s.', [AModuleName]);
    end;
    FreeLibrary(Handle);
  end else begin
    raise Exception.CreateFmt('DLL %s not found.', [AModuleName]);
  end;
end;

function CreateParameterList(AList: TStrings): PChar;
var
  i, L: Integer;
  S: String;
begin
  if Assigned(AList) then begin
    S:= '';
    for i:= 0 to AList.Count - 1 do begin
      S:= S + AList[i] + '&';
    end;
    L:= Length(S);
    if L > 0 then begin
      Delete(S, L, 1);
      Dec(L);
    end;
    GetMem(Result, L + 1);
    StrLCopy(Result, PChar(S), L);
  end else Result:= Nil;
end;

function DoCallISAPI(AProt: TProtocol; Handle: LongWord; HttpExtensionProc: THttpExtensionProc;
  AMethod, APathInfo: String; AParameterlist, AServerVarList: TStrings): String;
//  var AStatusCode: Integer; var AStatusText: String): String;
var
  err: Integer;
  ECB: TEXTENSION_CONTROL_BLOCK;
  Destination: TConnRec;
begin
  FillChar(ECB, SizeOf(ECB), 0);
  with ECB do begin
    // cbSize:= SizeOf(ECB); must be 0
    Destination.ResultString:= @Result;
    Destination.ServerVars:= AServerVarList;
    Destination.Prot:= AProt;
    Pointer(ConnID):= @Destination;

    lpszMethod:= PChar(AMethod);
    GetMem(lpszPathInfo, Length(APathInfo) + 1);
    StrPCopy(lpszPathInfo, APathInfo);
    if CompareText(AMethod, 'GET') = 0 then begin
      lpszQueryString:= CreateParameterList(AParameterList);
    end else begin
      lpbData:= CreateParameterList(AParameterList);
      cbAvailable:= Length(PChar(lpbData));
      cbTotalBytes:= cbAvailable
    end;
    WriteClient:= @MyWriteClientProc;
    ReadClient:= @MyReadClientProc;
    GetServerVariable:= @MyGetServerVariableProc;
    ServerSupportFunction:= @MyServerSupportFunction;

    err:= HttpExtensionProc(ECB) ;
    FreeMem(lpszQueryString);
    FreeMem(lpszPathInfo);
  end;
  {  HSE_STATUS_SUCCESS                      = 1;
     HSE_STATUS_SUCCESS_AND_KEEP_CONN        = 2;
     HSE_STATUS_PENDING                      = 3;
     HSE_STATUS_ERROR                        = 4;
  }
  case err of
    HSE_STATUS_SUCCESS:;
    HSE_STATUS_ERROR: begin
//        raise Exception.CreateFmt('HttpExtensionProc error status (%d) in ISAPI module %s.', [err, '']);
      end;
    else raise Exception.CreateFmt('HttpExtensionProc unsupported status (%d) in ISAPI module %s.', [err, '']);
    end;
end;

function LoadISAPI(AModuleName: String; var Handle: LongWord; var HttpExtensionProc: THttpExtensionProc): Boolean;
begin
  Result:= False;
  // load library
  Handle:= LoadLibrary(PChar(AModuleName));
  if Handle <> 0 then begin
    @HttpExtensionProc:= GetProcAddress(Handle, 'HttpExtensionProc');
    if @HttpExtensionProc <> nil
    then Result:= True
    else FreeLibrary(Handle);
  end;
end;

function CallISAPI(AProt: TProtocol; AModuleName, AMethod, APathInfo: String;
  AParameterlist, AServerVarList: TStrings; var AStatusCode: Integer; var AStatusText: String): String;
var
  Handle: LongWord;
  HttpExtensionProc: THttpExtensionProc;
begin
  // load library
  if LoadISAPI(AModuleName, Handle, HttpExtensionProc) then  begin
    // execute
    Result:= DoCallISAPI(AProt, Handle, HttpExtensionProc,
      AMethod, APathInfo, AParameterlist, AServerVarList);
      //, AStatusCode, AStatusText);
  end else begin
    // if fails
    if Handle = 0
    then raise Exception.CreateFmt('Не найдена DLL %s.', [AModuleName]);
    if not Assigned(HttpExtensionProc)
    then raise Exception.CreateFmt('Библиотека %s не содержит функцию записи.', [AModuleName]);
  end;
  // do not! if Handle <> 0 then FreeLibrary(Handle);
end;

function GetServerVarList(ARequest: TWebRequest): TStrings;
var
  ind: Integer;
  S: String;
begin
  Result:= TStringList.Create;
  for ind:= 0 to 28 do with ARequest as TISAPIRequest do begin
    S:= ServerVariables[Ind];
    if S = ''
    then Continue;
    S:= S+'=';
    case Ind of
    0: S:= S + ECB.lpszMethod;
    3: S:= S + ECB.lpszQueryString;
    4: S:= S + ECB.lpszPathInfo;
    5: S:= S + ECB.lpszPathTranslated;
    1..2, 6..24, 26..28: S:= S + GetFieldByName(ServerVariables[Ind]);
    25: if ECB.cbAvailable > 0 then begin
        SetString(S, PChar(ECB.lpbData), ECB.cbAvailable);
        S:= ServerVariables[25] + '=' + S;
      end;
    else
      S:= '';
    end;
    Result.Add(S);
  end;
end;

function ReadStringSmallPortions(ARequest: TWebRequest; var S: String): Boolean;
const
  MAXREADSIZE1 = 8192;
var
  RSize, Bytes2Read, Tryes: Integer;
begin
  Result:= True;
  Bytes2Read:= ARequest.ContentLength - Length(S);
  Tryes:= Bytes2Read div MAXREADSIZE1 + 2;
  while Bytes2Read > 0 do begin
    RSize := MAXREADSIZE1;
    if RSize > Bytes2Read then RSize:= Bytes2Read;
    try
      S:= S + ARequest.ReadString(RSize);
    except
      Dec(Tryes);
      if Tryes <= 0
      then Exit;
    end;
    Bytes2Read:= ARequest.ContentLength - Length(S);
  end;
end;

end.
