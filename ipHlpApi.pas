unit
  ipHlpApi;
(*##*)
(*******************************************************************************
*                                                                             *
*   I  P  H  L  P  A  P  I                                                     *
*                                                                             *
*   Copyright © 2006- 2007, Andrei Ivanov. All rights reserved.                *
*   IP helper API Object Pascal wrapper                                       *
*   Conditional defines:                                                       *
*                                                                             *
*   Last Revision: Oct 01 2007                                                 *
*   Last fixes   : Oct 01 2007                                                *
*                                                                              *
*                                                                             *
*                                                                              *
*                                                                             *
*   Lines        : 106                                                         *
*   History      : see CHANGES.TXT file                                       *
*   Printed      : ---                                                         *
*                                                                             *
********************************************************************************)
(*##*)

interface
uses
  WinSock;

const
  MAX_ADAPTER_NAME = 777; // ?!!

type
  PIP_ADAPTER_INDEX_MAP = ^TIP_ADAPTER_INDEX_MAP;
  TIP_ADAPTER_INDEX_MAP = record
    Index: Cardinal;
    Name: array [0..MAX_ADAPTER_NAME - 1] of WideChar;
  end;

  PIP_INTERFACE_INFO = ^TIP_INTERFACE_INFO;
  TIP_INTERFACE_INFO = record
    NumAdapters: Integer;
    Adapter: array[0..0] of TIP_ADAPTER_INDEX_MAP;
  end;

  PIPForwardRow = ^TIPForwardRow;
  TIPForwardRow = record
    dwForwardDest,
    dwForwardMask,
    dwForwardPolicy,
    dwForwardNextHop,
    dwForwardIfIndex,
    dwForwardType,
    dwForwardProto,
    dwForwardAge,
    dwForwardNextHopAS,
    dwForwardMetric1,
    dwForwardMetric2,
    dwForwardMetric3,
    dwForwardMetric4,
    dwForwardMetric5: Cardinal;
  end;

function GetNumberOfInterfaces(var AValue: Cardinal): Cardinal; stdcall;

function GetInterfaceInfo(ABuf: PIP_INTERFACE_INFO; var ABufLen: Cardinal): Cardinal; stdcall;

// For information about the IPAddr data type, see Windows Simple Data Types. To convert an IP address between dotted decimal notation and IPAddr format, use the inet_addr and inet_ntoa functions.
function GetBestInterface(ADestAddr: TInAddr; var AInterfaceIndex: Cardinal): Cardinal; stdcall;

// For information about the IPAddr data type, see Windows Simple Data Types. To convert an IP address between dotted decimal notation and IPAddr format, use the inet_addr and inet_ntoa functions.
function GetBestRoute(ADestAddr, ASourceAddr: TInAddr; var AIPForwardRow: TIPForwardRow): Cardinal; stdcall;

function GetBestRouterIP(ADestAddr: String): String; // return '' if failed

implementation

const
  DLLIPHLPAPI = 'iphlpapi.dll';

function GetNumberOfInterfaces(var AValue: Cardinal): Cardinal; stdcall;
  external DLLIPHLPAPI name 'GetNumberOfInterfaces';

function GetInterfaceInfo(ABuf: PIP_INTERFACE_INFO; var ABufLen: Cardinal): Cardinal; stdcall;
  external DLLIPHLPAPI name 'GetInterfaceInfo';

function GetBestInterface(ADestAddr: TInAddr; var AInterfaceIndex: Cardinal): Cardinal; stdcall;
  external DLLIPHLPAPI name 'GetBestInterface';

function GetBestRoute(ADestAddr, ASourceAddr: TInAddr; var AIPForwardRow: TIPForwardRow): Cardinal; stdcall;
  external DLLIPHLPAPI name 'GetBestRoute';

// Get IP address of the next hop in the best route for destination
function GetBestRouterIP(ADestAddr: String): String; // return '' if failed
var
  IPForwardRow: TIPForwardRow;
  src: In_Addr;
  // wsaData: TWSAData;
begin
  // if WSAStartup($101, wsaData) = 0 then begin     WSACleanup;
  src.S_addr:= 0;
  if GetBestRoute(in_addr(inet_addr(PChar(ADestAddr))), src, IPForwardRow) = S_OK then begin
    src:= in_addr(IPForwardRow.dwForwardNextHop);
    Result:= winSock.inet_ntoa(src);
    // Fix loopback Oct 03 2007
    if (Result = '127.0.0.1') and (Length(ADestAddr) > 0)
      then Result:= ADestAddr;
    
  end else Result:= '';
end;

end.
