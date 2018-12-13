unit
  udpSocket;
{
  Delphi 5 remake, Andrei Ivanov Apr 20 2001
  Author    : Frank Dekervel http://kervel.home.ml.org

* Properties
 NAME          RO DT   DESC
 Sockethandle   X      Returns the socket handle used by TUDPSocket.
 Winhandle      X      Returns the windows handle used by " ".
                       CAUTION : do not use closehandle or closesocket
                       on one of those properties.
 IsBound        X      True when the socket is bound and 'listening'
 RemoteHostInfo X      Gives u info about the host that is set up
                       for sending packets.
 SendPort          X   The port of the machine u send packets to
 Location          X   The location (hostname/ip) of the machine u send packets to
                       YOU DON'T HAVE TO REBIND WHEN YOU CHANGE THESE 2
 port              X   The port the local machine is bound to. If you don't
                       need a fixed port, use 0.
 reverseDNS        X   do a reverse DNS for each IP address given. ONLY
                       ENABLE THIS IF YOU REALLY NEED IT. IT IS SUPER-
                       SLOW ! (if you need it one time, e.g u're writing
                       a winnuke-protector using a Tsockets component,
                       and u want to know the hostname of ur aggressor,
                       set to true, call DNSlookup and set to false )
* Events
  Create               constructor
  Destroy              destructor
  DNSlookup            looks up the given hostname, or if it is an IP
                       address, and reverseDNS is enabled, you'll get
                       a hostname.
  S_open               Opens a socket, and bind it to the port in the
                       PORT propterty.
  S_close              Closes the socket and releases the port.

  OnError              Occurs when winsock detects an error, or when a
                       winsock operation fails. it is recommended that
                       you specify one, because errors are verry current,
                       and it is important to take care of them.
  OnReceive            Occurs when data arrives at your bound socket.
                       In the handler, it is safe to call ReadBuf
                       or ReadString.
  OnWriteReady         Dunno if it works on UDP. occurs when buffers are
                       sent, and you can send new data. If you get a
                       'operation would block' error while sending, you'll
                       have to wait until this event occurs before trying again.
  OnClose              Occurs when the socket is closed. Useless.

* Methods

  SendBuf             Sends a buffer to the machine in the location propterty,
                       and the port in the SendPort property
  ReadBuf             Fills a pchar (memory allocated or variabele/array
                       declared by you) with received data. The second
                       argument (len) lets you specify a maximum length,
                       but check the len variable again after reading,
                       now it contains the number of bytes received.
                       ReadBuf returns also information about the host
                       the packet was received from. If ReverseDNS is
                       specified, you also ll get a hostname.
  SendString
  ReadString

* Types

  TUDPSocket           The actual UDP socket
  Terrorproc           procedure type for error handlers
  Teventproc           same as TnotifyEvent
  ThostAbout           record that contains host information, such
                       as IP address or DNS name or both. can also
                       contain a port.
  TSockMessage         Winsock Asynchronous mode Windows Message type
}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  winsock;
const
  WM_SOCKET=WM_USER+323;
  WSA_VERSION_REQUIRED= $101; // Winsock version 1.01 for UDP protocol
  MAXSTRINGLENGTH = 512;           // maximum String length for strings to send.

type
  TErrorProc = procedure(msg:String;num:Integer) of object;
  TEventProc = procedure(sender:Tobject) of object;
  ThostAbout = record
    IP_addr: DWORD;
    DNS_name : String;
    IP_dotdot : String;
    location : String;
    Port : Integer; // port, used for sending | receiving
  end;
  TSockMessage = record
    Msg: Cardinal;
    SockID: Thandle;
    SelectEvent: Word;
    SelectError: Word;
    Result: Longint;
  end;
// ---------------------------------------------------------------------
// ---------------------------------------------------------------------
  TUDPSocket = class(TComponent)
  private
    //Handles
    Fsockethandle: Winsock.TSocket;
    FwinHandle: THandle;
    // Winsock info
    Fsession:TWSAdata;
    // Port to bind on
    Fport: DWORD;
    // Event handlers
    FerrorProc: TErrorProc;
    FonReceive,
    FonReady,
    FonClose: TEventProc;
    // Host to send to
    FHost: THostAbout;
    // bound ???
    Fbnd: Boolean;
    // Perform Reverse DNS ?
    FperformReverseDNS: Boolean;
  protected
    // Property settings
    procedure SetLocation(ALocation: String);
    // Error stuff.
    procedure HandleLastException;
    function  ErrToString(err:Integer): String;
    procedure MakeException(num: Integer; str: String);
    // Winsock stuff
    procedure PStartWSA;
    procedure PStopWSA;
    procedure PDNSlookup(var hostabout:Thostabout);
    procedure UDP_Bind;
    procedure UDP_Unbind;
    // Event handler stuff
    procedure _WM_SOCKET(var msg:TsockMessage); message WM_SOCKET;
    procedure WinsockEvent(var msg:TMessage);
    // Misc functions
    function IPtoDotDot(ip:DWORD): String;
  public
    // the constructor\destructor
    constructor Create(Aowner:Tcomponent); override;
    destructor Destroy; override;
    // highlevel winsock
    function DNSlookup(a_location:String):Thostabout;
    procedure S_Open;
    procedure S_Close;
    procedure SendBuf(var buf; var len: Integer);
    function  ReadBuf(var buf; var len: Integer): THostAbout;
    // Super - highlevel winsock
    procedure SendString(s: String);
    function  ReadString(var s: String): Thostabout;
    // Informative READ-ONLY properties
    property SocketHandle: TSocket read FSockethandle;
    property WinHandle: THandle read FWinHandle;
    property IsBound: Boolean read Fbnd;
    property RemoteHostInfo : Thostabout read Fhost;
    // you may look at these , but don't touch them !! (no close etc...)
  published
    // The event handlers
    property OnError       : TErrorProc Read Ferrorproc write Ferrorproc;
    property OnReceive     : TEventproc Read FonReceive write FonReceive;
    property OnWriteReady  : TEventProc Read FonReady write FonReady;
    property OnCloseSocket : TEventProc Read FonClose write FonClose;
    // the properties
    property SendPort: Integer read Fhost.port write Fhost.port;
    property Port: DWORD read Fport write Fport;
    // Location of host to send
    property Location: String read Fhost.ip_DotDot write setLocation;
    // have i to perform reverse dns on each packet i receive ??
    property ReverseDNS: Boolean read FperformReverseDNS write FperformReverseDNS;
  end;

procedure Register;

implementation

constructor TUDPSocket.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPort:= 0;
  Fbnd:= false;
  FPerformReverseDNS:= False;
  FwinHandle:= allocateHWND(WinsockEvent);
  PStartWSA;
end;

destructor TUDPSocket.Destroy;
begin
  if Fbnd
  then UDP_unbind;
  // CloseHandle(FWinHandle);
  if FWinHandle <> 0
  then DeallocateHWnd(FWinHandle);
  PStopWSA;
  inherited Destroy;
end;

procedure TUDPSocket.WinsockEvent(var msg:TMessage);
// Dispatch windows messages to specific event handlers
begin
  if msg.Msg = WM_SOCKET then begin
    // if we parse each message, the destructor
    // will be called by the form, but also a
    // WM_CLOSE event will be sent to this component.
    // when the form ll call the destructor, the
    // object ll already be destroyed, resulting
    // in ... an axxess violation. Are there
    // better ways to do this ?? kervel@hotmail.com !
  try
    dispatch(msg);
  except
    application.HandleException(self);
  end;
  end;
end;

procedure TUDPSocket._WM_SOCKET(var msg:TsockMessage);
// Specific event handler for WM_SOCKET
begin
  // this should never happen in UDP, but to
  // be complete , the handlers are ther.
  if msg.SelectError <> 0 then begin
    case msg.SelectEvent of
    FD_CONNECT :MakeException(wsagetlasterror,'+Error while connecting.');
    FD_CLOSE   :MakeException(wsagetlasterror,'+Error while disconnecting.');
    FD_READ    :MakeException(wsagetlasterror,'+Error while receiving.');
    FD_WRITE   :MakeException(wsagetlasterror,'+Error while sending.');
    FD_ACCEPT  :MakeException(wsagetlasterror,'+Error while accepting incoming connection.');
    FD_OOB     :MakeException(wsagetlasterror,'+Error OOB.');
    else
      MakeException(wsagetlasterror,'+Undefined error.');
    end;
    // no error, just an event
  end else begin
    case msg.selectevent of
    FD_READ   :    if Assigned(FonReceive) then Fonreceive(self) ;
    FD_WRITE  :    if Assigned(FonReady)   then FonReady(self)   ;
    FD_CLOSE  :    if Assigned(FonClose)   then FonClose(self)   ;
    //FD_ACCEPT :    if Assigned() then ; //          ""
    //FD_CONNECT:    if assigned() then ; // this is TCP
    //FD_OOB    :    if assigned() then ; //          ""
    end;
  end;
end;

procedure TUDPSocket.PStartWSA;
// Start winsock
var
  errNum: Integer;
begin
  errNum:= WSAstartup(WSA_VERSION_REQUIRED, Fsession);
  if errNum <> 0
  then MakeException(wsagetlasterror,' No Winsock, this app ll be boring without it.');
end;

procedure TUDPSocket.PStopWSA;
// Stop winsock
var
  errNum: Integer;
begin
  errNum := WSAcleanup;
  if errNum <> 0
  then MakeException(wsagetlasterror, ' Winsock doesnt want to stop.');
end;

procedure TUDPSocket.UDP_unBind;
// Closes the socket and release the port
begin
  if closesocket(Fsockethandle) <> 0
  then HandleLastException;
  Fbnd:= false;
end;

procedure TUDPSocket.S_Close;
// The same, but this one is called by the user
begin
  UDP_unbind;
end;

procedure TUDPSocket.UDP_Bind;
// Opens a socket, and bind to port.
var
  protoent: PProtoEnt;
  sain: TsockAddrIn;
begin
  // learn about the UDP protocol
  if Fbnd
  then UDP_unbind;
  protoent:= getprotobyname('udp');
  // initialise
  sain.sin_family:= AF_INET;
  sain.sin_port:= Fport;
  sain.sin_addr.S_addr:= 0;
  // create a nice socket
  FsocketHandle:= socket(PF_INET, SOCK_DGRAM, protoent^.p_proto);
  if FsocketHandle = INVALID_SOCKET
  then HandleLastException else begin
    // socket created !
    if Bind(Fsockethandle,sain,sizeof(sain)) = 0 then begin
       // Bound ! , now we have to set Async mode
       if WSAAsyncSelect(FsocketHandle, FwinHandle, WM_SOCKET,FD_READ or FD_WRITE or FD_CLOSE) = 0 then begin
         // Async mode suxxessfully set up
         Fbnd:= true;
       end else begin
         handlelastexception;
         UDP_unbind;
       end;
    end else begin
      handlelastexception;
      UDP_unbind;
    end;
  end;
end;

procedure TUDPSocket.S_Open;
// The same, but this one is called by the user
begin
  UDP_bind;
end;

procedure TUDPSocket.SetLocation(ALocation: String);
// Say where to send UDP data. perform a lookup if needed
// this is for property Location
begin
  Fhost.location:= ALocation;
  PDNSlookup(Fhost);
end;

procedure TUDPSocket.PDNSlookup(var hostabout:Thostabout);
// The core of the DNS part, this asks windows to give as much
// information as possible about the given location.
var
  Buff: array[0..256] of Char;
  SockAddrIn: TsockAddrIn;
  hostent: Phostent;
  L_string: String;
begin
  L_string:= hostAbout.location;
  strPcopy(buff, l_string);
  // first test if the thingy is a dotted IP
  SockAddrIn.sin_addr.S_addr:= inet_addr(buff);
  if SockAddrIn.sin_addr.S_addr = u_long(INADDR_NONE) then begin
    // well, the location was probably a DNS name, lets resolve it
    hostent:= gethostbyname(buff);
    if hostent <> nil then begin
      // OK, it WAS a DNS name. fill in the struct and were done
      hostabout.DNS_name:=hostabout.location;
      hostabout.IP_addr:=longint(plongint(hostent^.h_addr_list^)^);
      // Convert Addr to DOTDOT format.
      hostabout.IP_dotdot:=iptodotdot(hostabout.IP_addr);
    end else begin
      // Not an IP address, not a DNS name, NOTHING !!
      hostabout.IP_addr:=0;
      hostabout.DNS_name:='';
      hostabout.IP_dotdot:='';
      hostabout.location:='error';
    end;
  end else begin
    // Yeh, it was an IP address. letz look for a name !
    hostabout.IP_addr:=SockAddrIn.sin_addr.S_addr;
    // dotdot
    hostabout.IP_dotdot:=iptodotdot(hostabout.IP_addr);
    // Now do a reverse DNS to find out a hostname.
    // set property reverseDNS to false if too slow.
    hostabout.DNS_name:='NO REVERSE DNS!';
    if FperformReverseDNS then begin
      hostent:=gethostbyaddr(@(hostabout.Ip_addr), 4, AF_INET);
      if hostent <> nil
      then hostabout.DNS_name:= hostent.h_name  // strpas(
      else hostabout.DNS_name:= 'reverse dns lookup error';
    end;
  end;
end;

function TUDPSocket.DNSlookup(a_location:String):Thostabout;
//A function for the user, does the same
var
  tt: THostAbout;
begin
  FillChar(tt,sizeof(tt),0);
  tt.location:= a_location;
  PDNSlookup(tt);
  Result:= tt;
end;

procedure TUDPSocket.SendBuf(var buf; var len:Integer);
var
  intt: Integer;
  to_len: DWORD;
  addrto: TsockAddrIn;
begin
  FillChar(addrto,sizeof(addrto),0);
  addrto.sin_family:= AF_INET;
  addrto.sin_port:= Fhost.Port;
  addrto.sin_addr.S_addr:= Fhost.IP_addr;
  to_len:= sizeof(addrto);
  intt:= sendto(Fsockethandle, buf, len, 0, addrto, to_len);
  if intt < 0
  then HandleLastException
  else len:= intt;
end;

//Receives a PCHAR, and say from who
function TUDPSocket.ReadBuf(var buf; var len:Integer): THostAbout;
var
  TT: THostAbout;
  intt: Integer;
  sockaddr_from: TsockAddrIn;
  len_from: Integer;
begin
  FillChar(sockaddr_from, sizeof(sockaddr_from), 0);
  sockaddr_from.sin_family:= AF_INET;
  sockaddr_from.sin_port:= Fport;
  len_from:= sizeof(sockaddr_from);
  FillChar(TT, sizeof(TT), 0);
  intt:= recvfrom(FsocketHandle, buf, len - 1, 0, sockaddr_from, len_from);
  if intt < 0 then begin
    HandleLastException;
    TT.location:= 'error receiving';
  end else begin
    len:= intt;
    TT.location:= IpToDotDot(sockaddr_from.sin_addr.S_addr);
    TT.port:= sockaddr_from.sin_port;
    PDNSlookup(tt);
  end;
  Result:= tt;
end;

procedure TUDPSocket.SendString(s: String);
//Send a String. Whats the use ??
var
  len: Integer;
begin
  len:= Length(s);
  if len > (MAXSTRINGLENGTH - 1)
  then len:=(MAXSTRINGLENGTH - 1);
  SendBuf(s[1], len);
end;

function  TUDPSocket.ReadString(var s: String): THostAbout;
var
  len: Integer;
begin
  len:= MAXSTRINGLENGTH;
  SetLength(S, len);
  Result:= ReadBuf(S[1], len);
  SetLength(S, len);
end;

function TUDPSocket.IPtoDotDot(ip:DWORD):String;
//Yeh, translates  3232235521 to 192.168.0.1
type
  P_rec = ^T_rec;
  T_rec = packed record
    b1: Byte;
    b2: Byte;
    b3: Byte;
    b4: Byte;
  end;
var
  p: P_rec;
  i: DWORD;
  s: String;
begin
  i:=ip;
  p:=@i;
  s:= inttostr(p^.b1)+'.'+inttostr(p^.b2)+'.'+inttostr(p^.b3)+'.'+inttostr(p^.b4);
  Result:=s;
end;

// handle the last exception occured in winsock.dll
procedure TUDPSocket.HandleLastException;
var
  n:Integer;
begin
  n:= WSAgetLastError;
  MakeException(n, '');
end;

procedure TUDPSocket.MakeException(num: Integer; str: String);
// call the OnError event handler.
// Num = a valid winsock error code number
// STR = a String, when the error is non-winsock.
// if the String is not empty, the String is used instead of the code.
// if the String begins with a '+', both are used.
var
  s: String;
begin
  if str = ''
  then s:= ErrToString(num) else begin
    if pos('+',str) <> 1
    then s:=str else begin
      s:= ' ('+copy(str,2,length(str))+').';
      s:= ErrToString(num) + s;
    end;
  end;
  if Assigned(FErrorProc)
  then Ferrorproc(s,num)
  else raise ERangeError.CreateFmt('Error : 0x%x', [num]);
end;

function  TUDPSocket.ErrToString(err: Integer): String;
// Thanks to Gary T. Desrosiers , this procedure translates error codes
// into readable strings.
begin
 case err of
    WSAEINTR:
      Result := 'Interrupted system call';
    WSAEBADF:
      Result := 'Bad file number';
    WSAEACCES:
      Result := 'Permission denied';
    WSAEFAULT:
      Result := 'Bad address';
    WSAEINVAL:
      Result := 'Invalid argument';
    WSAEMFILE:
      Result := 'Too many open files';
    WSAEWOULDBLOCK:
      Result := 'Operation would block';
    WSAEINPROGRESS:
      Result := 'Operation now in progress';
    WSAEALREADY:
      Result := 'Operation already in progress';
    WSAENOTSOCK:
      Result := 'Socket operation on non-socket';
    WSAEDESTADDRREQ:
      Result := 'Destination address required';
    WSAEMSGSIZE:
      Result := 'Message too long';
    WSAEPROTOTYPE:
      Result := 'Protocol wrong type for socket';
    WSAENOPROTOOPT:
      Result := 'Protocol not available';
    WSAEPROTONOSUPPORT:
      Result := 'Protocol not supported';
    WSAESOCKTNOSUPPORT:
      Result := 'Socket type not supported';
    WSAEOPNOTSUPP:
      Result := 'Operation not supported on socket';
    WSAEPFNOSUPPORT:
      Result := 'Protocol family not supported';
    WSAEAFNOSUPPORT:
      Result := 'Address family not supported by protocol family';
    WSAEADDRINUSE:
      Result := 'Address already in use';
    WSAEADDRNOTAVAIL:
      Result := 'Can''t assign requested address';
    WSAENETDOWN:
      Result := 'Network is down';
    WSAENETUNREACH:
      Result := 'Network is unreachable';
    WSAENETRESET:
      Result := 'Network dropped connection on reset';
    WSAECONNABORTED:
      Result := 'Software caused connection abort';
    WSAECONNRESET:
      Result := 'Connection reset by peer';
    WSAENOBUFS:
      Result := 'No buffer space available';
    WSAEISCONN:
      Result := 'Socket is already connected';
    WSAENOTCONN:
      Result := 'Socket is not connected';
    WSAESHUTDOWN:
      Result := 'Can''t send after socket shutdown';
    WSAETOOMANYREFS:
      Result := 'Too many references: can''t splice';
    WSAETIMEDOUT:
      Result := 'Connection timed out';
    WSAECONNREFUSED:
      Result := 'Connection refused';
    WSAELOOP:
      Result := 'Too many levels of symbolic links';
    WSAENAMETOOLONG:
      Result := 'File name too long';
    WSAEHOSTDOWN:
      Result := 'Host is down';
    WSAEHOSTUNREACH:
      Result := 'No route to host';
    WSAENOTEMPTY:
      Result := 'Directory not empty';
    WSAEPROCLIM:
      Result := 'Too many processes';
    WSAEUSERS:
      Result := 'Too many users';
    WSAEDQUOT:
      Result := 'Disc quota exceeded';
    WSAESTALE:
      Result := 'Stale NFS file handle';
    WSAEREMOTE:
      Result := 'Too many levels of remote in path';
    WSASYSNOTREADY:
      Result := 'Network sub-system is unusable';
    WSAVERNOTSUPPORTED:
      Result := 'WinSock DLL cannot support this application';
    WSANOTINITIALISED:
      Result := 'WinSock not initialized';
    WSAHOST_NOT_FOUND:
      Result := 'Host not found';
    WSATRY_AGAIN:
      Result := 'Non-authoritative host not found';
    WSANO_RECOVERY:
      Result := 'Non-recoverable error';
    WSANO_DATA:
      Result := 'No Data';
    else Result := 'Not a WinSock error';
  end;
end;

procedure Register;
begin
  RegisterComponents('TCP/IP', [TUDPSocket]);
end;

end.
