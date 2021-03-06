unit LCons;
(*##*)
(*******************************************************************
*                                                                 *
*   L  C  O  N  S   TConnections class manage list of server       *
*                   connections                                   *
*   Copyright (c) 1998, �.������. ��� ����� ���������������        *
*   Designed for some tcp server implementations                  *
*   Conditional defines: ---                                       *
*                                                                 *
*   Last Revision: Jun    1998                                     *
*   Last fix     : Jul 03 1998                                    *
*   Lines        : 391                                             *
*   History      : see CHANGES.TXT file                           *
*   Printed      : ---                                             *
*                                                                 *
********************************************************************)
(*##*)

interface

uses
  Windows, Messages, SysUtils, Classes;

type
  URLString   = String[32];
  SenderString= String[16];
  ipString    = String[16];
  TConnectionState= set of (csReady2Delete, csReady2Store);
  TConnectPhase= (phDisconnected, phAuthorization, phTransaction, phDataFlow, phUpdate);
  MHString = String;
  TMessageHeader = record
    ReturnPath: MHString;
    Received: MHString;
    RDate: MHString;
    From: MHString;
    Subject: MHString;
    Sender: MHString;
    Too: MHString;
    Cc: MHString;
  end;

  TConnection = record
    // ��� ���� ����������� ��� ��������
    handle: Integer;                                         //  0
    connected: TDateTime;                                    //  4
    // ��� ���� ����������� ��� ����������
    dt: TDateTime;                                           //  12
    // ��������� ���� ���������� ����
    regtryes: Byte;      // ���-�� ������� ����������
    Enabled: Boolean;    // ������� ��� ���
    host: URLString;
    ip: ipString;
    Phase: TConnectPhase;// ������
    State: TConnectionState;
    User:  SenderString; // ��� �������
    SendMsg: String;
    Recipients:TStrings;
    MailBox:TStrings;
  end;

  TAConnections = array[0..9] of TConnection; // 9 ������� - ��� ����� �������
  PAConnections = ^TAConnections;

  TConnectionChangeEvent = procedure (Sender: TObject; Connection: TConnection);

  TConnections = class (TPersistent)
    private
    FConnections: PAConnections;
    FSessionID: String[32];
    FCount, FLimit, FConnectionsLimit, FStep: Integer;
    FTimeout,
    FNoMoreTime: TDateTime; // ����- ���
    FCurrentNumber: Integer;
    FOnChange: TConnectionChangeEvent;
    function getnobyhandle(Ahandle: Integer; var no: Integer): Boolean;
    function GetConnectionByOrder(no: Integer): TConnection;
    procedure SetConnectionByOrder(no: Integer; AConnection: TConnection);
    function GetConnected(Ahandle: Integer): Boolean;
    function GetConnection(Ahandle: Integer): TConnection;
    procedure SetConnection(Ahandle: Integer; AConnection: TConnection);
    function GetConnectPhase(Ahandle: Integer): TConnectPhase;
    procedure SetConnectPhase(Ahandle: Integer; APhase: TConnectPhase);
    procedure SetNewLimit(ACount: Integer);
    function AllocSpace(NewCount: Integer): Boolean;
    function Add(Ahandle: Integer): Boolean;
    public
    RegTryesLimit: Integer;
    property TimeOut: TDateTime read FTimeout write FTimeOut;
    property Count: Integer read FCount write SetNewLimit; // ����� ��������
    property Connected[no: Integer]: Boolean read GetConnected;
    property Connections[no: Integer]: TConnection read GetConnection write SetConnection;
    property Phases[no: Integer]: TConnectPhase read GetConnectPhase write SetConnectPhase;
    property ConnectionsByOrder[no: Integer]: TConnection read GetConnectionByOrder write SetConnectionByOrder;
    property ConnectionsLimit: Integer  read FConnectionsLimit write FConnectionsLimit;
    property OnChange: TConnectionChangeEvent read FOnChange write FOnChange;
    constructor Create;
    destructor  Destroy; override;
    procedure DeleteByNo(no: Integer);
    procedure Delete(Ahandle: Integer);
    function  MarkTimeOut(what: TConnectionState): Integer;
    procedure DeleteMarked;
    procedure Clear;
    function EmptyConnection: TConnection;
    function MkUniqueID: String;
  end;

function PhaseString(AConnection: TConnection): String;
function MakeHeaderString(const MessageHeader: TMessageHeader): String;

const
  CR = #13#10;

implementation
uses
  util1;
var
  FEmptyConnection: TConnection;

function PhaseString(AConnection: TConnection): String;
var
  S: String;
begin
  case AConnection.Phase of
  phDisconnected:
    S:= '������������';
  phAuthorization:
    S:= '�����������:'+IntToStr(AConnection.regtryes);
  phTransaction:
    S:= '����������';
  phDataFlow:
    S:= '���� ���������';
  phUpdate:
    S:= '���������� ����������';
  else
    S:= '???';
  end;
  Result:= S;
end;

function MakeHeaderString(const MessageHeader: TMessageHeader): String;
begin
  with MessageHeader do begin
    Result:=
      'Date: ' + RDate + CR +     // ?!!
      'From: ' + From + CR +
      'To: ' + Too  + CR +
      'Subject: ' + Subject + CR +
      {
      'Return-Path: ' + ReturnPath + CR +
      'Received: ' + Received + CR +
      'Sender: ' + Sender + CR +
      'Cc: ' + Cc  + CR +
      }
      CR;
  end;
end;

constructor TConnections.Create;
var
  TmS: TTimeStamp;
begin
  inherited Create;
  FOnChange:= Nil;
  FCurrentNumber:= 0;
  TmS:= DateTimeToTimeStamp(Now);
  FSessionID:= Int2Alpha(TmS.Date mod 512) + // ����� ��� ����
    Int2Alpha(TmS.Time div 2048);           // ����� 2 ������
  FCount:= 0;
  FStep:= 5;
  FTimeOut:= -1;
  FNoMoreTime:= -1;
  FConnections:= Nil;
  RegTryesLimit:= 3;
  FConnectionsLimit:= 2000;
  SetNewLimit(0);
end;

function    TConnections.MkUniqueID: String;
begin
  Result:= FSessionID + Int2Alpha(FCurrentNumber);
  Inc(FCurrentNumber);
end;

destructor  TConnections.Destroy;
begin
  SetNewLimit(0);
  inherited Destroy;
end;

function    TConnections.EmptyConnection: TConnection;
begin
  Result:= FEmptyConnection;
end;

procedure   TConnections.SetNewLimit(ACount: Integer);
begin
  // �������� ��- ����� ������ �� ����������?
  ReAllocMem(FConnections, ACount*SizeOf(TConnection));  // ���������� ������� ������ �� ��������� �� ��
  // ������ ������ ����� ��������� ������
  if ACount > FLimit then begin
    FillChar(FConnections^[FLimit], SizeOf(TConnection)*(ACount-FLimit), 0);
  end;
  // ������� ����� ������
  FLimit:= ACount;
  // �� ������ ������ ��� ��������
  if ACount = 0 then begin
    FConnections:= Nil;
  end;
end;

// �������������� ���������� FCount
function TConnections.AllocSpace(NewCount: Integer): Boolean;
var
  NewLimit: Integer;
begin
  // �������� ��- ����� ������ �� ����������?
  if NewCount > FConnectionsLimit then begin
    AllocSpace:= False;
  end else begin
    NewLimit:= (NewCount div FStep) * FStep;
    if NewLimit < NewCount
    then Inc(NewLimit, FStep);
    if NewLimit <> FLimit
    then SetNewLimit(NewLimit);
    FCount:= NewCount;
    AllocSpace:= True;
  end;
end;

// �������� ����� �� handle
function    TConnections.getnobyhandle(Ahandle: Integer; var no: Integer): Boolean;
var
  i: Integer;
begin
  for i:= 0 to FCount - 1 do begin
    if FConnections^[i].handle = Ahandle then begin
      Result:= True;
      no:= i;
      Exit;
    end;
  end;
  Result:= False;
end;

function    TConnections.GetConnectionByOrder(no: Integer): TConnection;
begin
  if (no >= 0) and (no < FCount)
  then Result:= FConnections^[no]
  else Result:= EmptyConnection;
end;

function    TConnections.GetConnected(Ahandle: Integer): Boolean;
var
  no: Integer;
begin
  Result:= getnobyhandle(AHandle, no);
end;

function    TConnections.GetConnection(Ahandle: Integer): TConnection;
var
  no: Integer;
begin
  if getnobyhandle(AHandle, no)
  then Result:= FConnections^[no]
  else Result:= EmptyConnection;
end;

procedure   TConnections.SetConnectionByOrder(no: Integer; AConnection: TConnection);
begin
  if (no >= 0) and (no < FCount) then begin
    FConnections^[no]:= AConnection;
    FConnections^[no].Dt:= Date+Time;
  end;
end;

procedure   TConnections.SetConnection(Ahandle: Integer; AConnection: TConnection);
var
  no: Integer;
  curDt: TDateTime;
begin
  curdt:= Date+Time;
  if getnobyhandle(AHandle, no) then begin
    FConnections^[no]:= AConnection;
    // ��������� ���� ���� �����
    FConnections^[no].Handle:= AHandle;
    FConnections^[no].Dt:= curdt;
  end else begin
    // ���� ������ ��� ��������� ����
    if Add(Ahandle) and getnobyhandle(AHandle, no) then begin
      FConnections^[no]:= AConnection;
      // ��������� ���� ���� �����
      FConnections^[no].Handle:= AHandle;
      // ��������� ����� ����������
      FConnections^[no].Connected:= curdt;
      FConnections^[no].Dt:= curdt;
    end;
  end;
  if Assigned(FOnChange) then begin
    FOnChange(Self, AConnection);
  end;
end;

function  TConnections.GetConnectPhase(Ahandle: Integer): TConnectPhase;
var
  Connection: TConnection;
begin
  Connection:= Connections[AHandle];
  Result:= Connection.Phase;
end;

procedure TConnections.SetConnectPhase(Ahandle: Integer; APhase: TConnectPhase);
var
  Connection: TConnection;
begin
  Connection:= Connections[AHandle];
  Connection.Phase:= APhase;
  Connections[AHandle]:= Connection;
end;

function TConnections.Add(Ahandle: Integer): Boolean;
begin
  Add:= False;
  if FCount < FConnectionsLimit then begin
    if AllocSpace(FCount + 1) then with FConnections^[FCount-1] do begin
      handle:= AHandle;
      regtryes:= 0;
      Add:= True;
    end;  
  end;
end;

procedure   TConnections.DeleteByNo(no: Integer);
var
  shift: Integer;
begin
  if (no < FCount) and (no >= 0) then begin
    FConnections^[no].Recipients.Free;
    FConnections^[no].Mailbox.Free;
    FConnections^[no].SendMsg:= '';
    shift:= FCount - 1 - no;
    if shift > 0
    then Move(FConnections^[no+1], FConnections^[no], shift * SizeOf(TConnection));
    AllocSpace(FCount-1);
  end;
  if Assigned(FOnChange) then begin
    FOnChange(Self, FEmptyConnection);
  end;
end;

procedure   TConnections.Delete(Ahandle: Integer);
var
  no: Integer;
begin
  if getnobyhandle(AHandle, no) then begin
    DeleteByNo(no);
  end;
end;

function  TConnections.MarkTimeOut(what: TConnectionState): Integer;
var
  no, Count: Integer;
  curdt: TDateTime;
begin
  curdt:= Date + Time;
  no:= 0;
  Count:= 0;
  while no < FCount do begin
    if FConnections^[no].dt + ftimeout < curdt then begin
      FConnections^[no].State:= what;
      Inc(Count);
    end else begin
      FConnections^[no].State:= [];
    end;
    Inc(no);
  end;
  Result:= Count;
end;

procedure   TConnections.DeleteMarked;
var
  no: Integer;
begin
  no:= 0;
  while no < FCount do begin
    if csReady2Delete in FConnections^[no].State
    then DeleteByNo(no)
    else Inc(no);
  end;
end;

procedure   TConnections.Clear;
begin
  while FCount > 0 do begin
    // if csReady2Delete in FConnections^[0].State then
    DeleteByNo(0);
  end;
end;

begin
  FillChar(FEmptyConnection, SizeOf(TConnection), 0);
end.
