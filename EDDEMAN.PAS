unit EDdeMan;
(*##*)
(*******************************************************************
*                                                                 *
*   E  D  D  E  M  A  N        EPAGER's unit.                      *
*                                                                 *
*   Copyright (c) 1998, ј.»ванов. ¬се права зарезервированы        *
*   replacement for Borland's DDEMAN uses additional TOPIC and    *
*   ITEM for EDDEServerConv|Item properties (allow use spaces etc) *
*   You may register component Component|Install Component|EDDEReg*
*                                                                  *
*                                                                 *
*   Conditional defines:                                           *
*                                                                 *
*   Last Revision: Mar 19 1998                                     *
*   Last fix     : Apr 09 1998                                    *
*   Lines        : 1835                                            *
*   History      : see CHANGES.TXT file                           *
*   Printed      : ---                                             *
*                                                                 *
********************************************************************)
(*##*)

{$R-}

interface

uses
  Windows, Classes, Graphics, Forms, Controls, DDEML, StdCtrls;

type
  TDataMode = (ddeAutomatic, ddeManual);
  TEDdeServerConv = class;

  TMacroEvent = procedure(Sender: TObject; Msg: TStrings) of object;

  TEDdeClientItem = class;

{ TEDdeClientConv }

  TEDdeClientConv = class(TComponent)
  private
    FDdeService: string;
    FDdeTopic: string;
    FConv: HConv;
    FCnvInfo: TConvInfo;
    FItems: TList;
    FHszApp: HSZ;
    FHszTopic: HSZ;
    FDdeFmt: Integer;
    FOnClose: TNotifyEvent;
    FOnOpen: TNotifyEvent;
    FAppName: string;
    FDataMode: TDataMode;
    FConnectMode: TDataMode;
    FWaitStat: Boolean;
    FFormatChars: Boolean;
    procedure SetDdeService(const Value: string);
    procedure SetDdeTopic(const Value: string);
    procedure SetService(const Value: string);
    procedure SetTopic(const Value: string);
    procedure SetConnectMode(NewMode: TDataMode);
    procedure SetFormatChars(NewFmt: Boolean);
    procedure XactComplete;
    procedure SrvrDisconnect;
    procedure DataChange(DdeDat: HDDEData; hszIt: HSZ);
  protected
    function CreateDdeConv(FHszApp: HSZ; FHszTopic: HSZ): Boolean;
    function GetCliItemByName(const ItemName: string): TPersistent;
    function GetCliItemByCtrl(ACtrl: TEDdeClientItem): TPersistent;
    procedure Loaded; override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadLinkInfo(Reader: TReader);
    procedure WriteLinkInfo(Writer: TWriter);
    function OnSetItem(aCtrl: TEDdeClientItem; const S: string): Boolean;
    procedure OnAttach(aCtrl: TEDdeClientItem);
    procedure OnDetach(aCtrl: TEDdeClientItem);
    procedure Close; dynamic;
    procedure Open; dynamic;
    function ChangeLink(const App, Topic, Item: string): Boolean;
    procedure ClearItems;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function PasteLink: Boolean;
    function OpenLink: Boolean;
    function SetLink(const Service, Topic: string): Boolean;
    procedure CloseLink;
    function StartAdvise: Boolean;
    function PokeDataLines(const Item: string; Data: TStrings): Boolean;
    function PokeData(const Item: string; Data: PChar): Boolean;
    function ExecuteMacroLines(Cmd: TStrings; waitFlg: Boolean): Boolean;
    function ExecuteMacro(Cmd: PChar; waitFlg: Boolean): Boolean;
    function RequestData(const Item: string): PChar;
    property DdeFmt: Integer read FDdeFmt;
    property WaitStat: Boolean read FWaitStat;
    property Conv: HConv read FConv;
    property DataMode: TDataMode read FDataMode write FDataMode;
  published
    property ServiceApplication: string read FAppName write FAppName;
    property DdeService: string read FDdeService write SetDdeService;
    property DdeTopic: string read FDdeTopic write SetDdeTopic;
    property ConnectMode: TDataMode read FConnectMode write SetConnectMode default ddeAutomatic;
    property FormatChars: Boolean read FFormatChars write SetFormatChars default False;
    property OnClose: TNotifyEvent read FOnClose write FOnClose;
    property OnOpen: TNotifyEvent read FOnOpen write FOnOpen;
  end;

{ TEDdeClientItem }

  TEDdeClientItem = class(TComponent)
  private
    FLines: TStrings;
    FDdeClientConv: TEDdeClientConv;
    FDdeClientItem: string;
    FOnChange: TNotifyEvent;
    function GetText: string;
    procedure SetDdeClientItem(const Val: string);
    procedure SetDdeClientConv(Val: TEDdeClientConv);
    procedure SetText(const S: string);
    procedure SetLines(L: TStrings);
    procedure OnAdvise;
  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Text: string read GetText write SetText;
    property Lines: TStrings read FLines write SetLines;
    property DdeConv: TEDdeClientConv read FDdeClientConv write SetDdeClientConv;
    property DdeItem: string read FDdeClientItem write SetDdeClientItem;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

{ TEDdeServerConv }

  TEDdeServerConv = class(TComponent)
  private
    FOnOpen: TNotifyEvent;
    FOnClose: TNotifyEvent;
    FOnExecuteMacro: TMacroEvent;
    FTopic: String;
  protected
    procedure Connect; dynamic;
    procedure Disconnect; dynamic;
    procedure SetTopic(ATopic: String);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ExecuteMacro(Data: HDdeData): LongInt;
  published
    property Topic: String read FTopic write SetTopic;
    property OnOpen: TNotifyEvent read FOnOpen write FOnOpen;
    property OnClose: TNotifyEvent read FOnClose write FOnClose;
    property OnExecuteMacro: TMacroEvent read FOnExecuteMacro write FOnExecuteMacro;
  end;

{ TEDdeServerItem }
  TEDdeServerItem = class(TComponent)
  private
    FLines: TStrings;
    FServerConv: TEDdeServerConv;
    FOnChange: TNotifyEvent;
    FOnPokeData: TNotifyEvent;
    FFmt: Integer;
    FItem: String;
    procedure ValueChanged;
  protected
    function GetText: string;
    procedure SetText(const Item: string);
    procedure SetLines(Value: TStrings);
    procedure SetServerConv(SConv: TEDdeServerConv);
    procedure SetItem(AItem: String);
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function PokeData(Data: HDdeData): LongInt;
    procedure CopyToClipboard;
    procedure Change; dynamic;
    property Fmt: Integer read FFmt;
  published
    property ServerConv: TEDdeServerConv read FServerConv write SetServerConv;
    property Text: string read GetText write SetText;
    property Lines: TStrings read FLines write SetLines;
    property Item: String read FItem write SetItem;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnPokeData: TNotifyEvent read FOnPokeData write FOnPokeData;
  end;

{ TEDdeMgr }

  TEDdeMgr = class(TComponent)
  private
    FAppName: string;
    FHszApp: HSZ;
    FConvs: TList;
    FCliConvs: TList;
    FConvCtrls: TList;
    FDdeInstId: Longint;
    FLinkClipFmt: Word;
    procedure Disconnect(DdeSrvrConv: TComponent);
    function GetSrvrConv(const Topic: string ): TComponent;
    function AllowConnect(hszApp: HSZ; hszTopic: HSZ): Boolean;
    function AllowWildConnect(hszApp: HSZ; hszTopic: HSZ): HDdeData;
    function Connect(Conv: HConv; hszTopic: HSZ; SameInst: Boolean): Boolean;
    procedure PostDataChange(const Topic: string; Item: string);
    procedure SetAppName(const AName: string);
    procedure ResetAppName;
    function  GetServerConv(const Topic: string): TEDdeServerConv;
    procedure InsertServerConv(SConv: TEDdeServerConv);
    procedure RemoveServerConv(SConv: TEDdeServerConv);
//    procedure DoError;
    function  GetForm(const Topic: string): TForm;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetExeName: string;     // obsolete
    property DdeInstId: LongInt read FDdeInstId write FDdeInstId;
    property AppName: string read FAppName write SetAppName;
    property LinkClipFmt: Word read FLinkClipFmt;
  end;

  function GetPasteLinkInfo(var Service: string; var Topic: string;
    var Item: string): Boolean;
var
  ddeMgr: TEDdeMgr;

const
  NoResponse: Boolean = False;

type
  // типы массивов тем и элементов, граница 99- дл€ отладки (watchdog)
  FAEDDEServConv = array [0..99] of TEDdeServerConv;
  FAEDDEServItem = array [0..99] of TEDdeServerItem;
  // дл€ динамического распределени€ пам€ти массивов- ссылки
  TAEDDEServConv = ^FAEDDEServConv;
  TAEDDEServItem = ^FAEDDEServItem;

implementation

uses SysUtils, Dialogs, Consts, Clipbrd;

type
  EDdeError = class(Exception);
  TDdeSrvrConv = class;

{ TDdeSrvrItem }

  TDdeSrvrItem = class(TComponent)
  private
    FConv: TDdeSrvrConv;
    FItem: string;
    FHszItem: HSZ;
    FSrvr: TEDdeServerItem;
  protected
    procedure SetItem(const Value: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function RequestData(Fmt: Word): HDdeData;
    procedure PostDataChange;
    property Conv: TDdeSrvrConv read FConv write FConv;
    property Item: string read FItem write SetItem;
    property Srvr: TEDdeServerItem read FSrvr write FSrvr;
    property HszItem: HSZ read FHszItem;
  end;

{ TDdeSrvrConv }

  TDdeSrvrConv = class(TComponent)
  private
    FTopic: string;
    FHszTopic: HSZ;
    FForm: TForm;
    FSConv: TEDdeServerConv;
    FConv: HConv;
//    FCnvInfo: TConvInfo;
//    FDdeFmt: Integer;
    FItems: TList;
  protected
    function GetControl(WinCtrl: TWinControl; DdeConv: TEDdeServerConv; const ItemName: string): TEDdeServerItem;
    function GetSrvrItem(hszItem: HSZ): TDdeSrvrItem;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function RequestData(Conv: HConv; hszTopic: HSZ; hszItem: HSZ;
      Fmt: Word): HDdeData;
    function AdvStart(Conv: HConv; hszTopic: HSZ; hszItem: HSZ;
      Fmt: Word): Boolean;
    procedure AdvStop(Conv: HConv; hszTopic: HSZ; hszItem: HSZ);
    function PokeData(Conv: HConv; hszTopic: HSZ; hszItem: HSZ; Data: HDdeData;
      Fmt: Integer): LongInt;
    function ExecuteMacro(Conv: HConv; hszTopic: HSZ; Data: HDdeData): Integer;
    function GetItem(const ItemName: string): TDdeSrvrItem;
    property Conv: HConv read FConv;
    property Form: TForm read FForm;
    property SConv: TEDdeServerConv read FSConv;
    property Topic: string read FTopic write FTopic;
    property HszTopic: HSZ read FHszTopic;
  end;

{ TDdeCliItem }

  TDdeCliItem = class(TPersistent)
  protected
    FItem: string;
    FHszItem: HSZ;
    FCliConv: TEDdeClientConv;
    FCtrl: TEDdeClientItem;
    function StartAdvise: Boolean;
    function StopAdvise: Boolean;
    procedure StoreData(DdeDat: HDDEData);
    procedure DataChange;
    function AccessData(DdeDat: HDDEData; pDataLen: PDWORD): Pointer;
    procedure ReleaseData(DdeDat: HDDEData);
  public
    constructor Create(ADS: TEDdeClientConv);
    destructor Destroy; override;
    function RefreshData: Boolean;
    function SetItem(const S: string): Boolean;
    procedure SrvrDisconnect;
    property HszItem: HSZ read FHszItem;
    property Control: TEDdeClientItem read FCtrl write FCtrl;
  published
    property Item: string read FItem;
  end;

procedure DDECheck(Success: Boolean);
var
  err: Integer;
  ErrStr: string;
begin
  if Success then Exit;
  err := DdeGetLastError(DDEMgr.DdeInstId);
  case err of
    DMLERR_LOW_MEMORY, DMLERR_MEMORY_ERROR:
      ErrStr := Format(SDdeMemErr, [err]);
    DMLERR_NO_CONV_ESTABLISHED:
      ErrStr := Format(SDdeConvErr, [err]);
  else
    ErrStr := Format(SDdeErr, [err]);
  end;
  raise EDdeError.Create(ErrStr);
end;

function DdeMgrCallBack(CallType, Fmt : UINT; Conv: HConv; hsz1, hsz2: HSZ;
  Data: HDDEData; Data1, Data2: DWORD): HDDEData; stdcall;
var
  ci: TConvInfo;
  ddeCli: TComponent;
  ddeSrv: TDdeSrvrConv;
  ddeObj: TComponent;
  xID: Integer;
begin
  Result := 0;
  case CallType of
    XTYP_CONNECT:
      Result := HDdeData(ddeMgr.AllowConnect(hsz2, hsz1));
    XTYP_WILDCONNECT:
      Result := ddeMgr.AllowWildConnect(hsz2, hsz1);
    XTYP_CONNECT_CONFIRM:
      ddeMgr.Connect(Conv, hsz1, Boolean(Data2));
  end;
  if Conv <> 0 then
  begin
    ci.cb := sizeof(TConvInfo);
    if CallType = XTYP_XACT_COMPLETE then
      xID := Data1
    else
      xID := QID_SYNC;
    if DdeQueryConvInfo(Conv, xID, @ci) = 0 then Exit;
    case CallType of
      XTYP_ADVREQ:
        begin
          ddeSrv := TDdeSrvrConv(ci.hUser);
          // специальный случай - не отвечать на запрос
          if NoResponse
          then Result:= 0
          else Result := ddeSrv.RequestData(Conv, hsz1, hsz2, Fmt);
        end;
      XTYP_REQUEST:
        begin
          ddeSrv := TDdeSrvrConv(ci.hUser);
          // специальный случай - не отвечать на запрос
          if NoResponse
          then Result:= 0
          else Result := ddeSrv.RequestData(Conv, hsz1, hsz2, Fmt);
        end;
      XTYP_ADVSTOP:
        begin
          ddeSrv := TDdeSrvrConv(ci.hUser);
          ddeSrv.AdvStop(Conv, hsz1, hsz2);
        end;
      XTYP_ADVSTART:
        begin
          ddeSrv := TDdeSrvrConv(ci.hUser);
          Result := HDdeData(ddeSrv.AdvStart(Conv, hsz1, hsz2, Fmt));
        end;
      XTYP_POKE:
        begin
          ddeSrv := TDdeSrvrConv(ci.hUser);
          Result := HDdeData(ddeSrv.PokeData(Conv, hsz1, hsz2, Data, Fmt));
        end;
      XTYP_EXECUTE:
        begin
          ddeSrv := TDdeSrvrConv(ci.hUser);
          Result := HDdeData(ddeSrv.ExecuteMacro(Conv, hsz1, Data));
        end;
      XTYP_XACT_COMPLETE:
        begin
          ddeCli := TComponent(ci.hUser);
          if ddeCli <> nil then TEDdeClientConv(ddeCli).XactComplete
        end;
      XTYP_ADVDATA:
        begin
          ddeCli := TComponent(ci.hUser);
          TEDdeClientConv(ddeCli).DataChange(Data, hsz2);
        end;
      XTYP_DISCONNECT:
        begin
          ddeObj := TComponent(ci.hUser);
          if ddeObj <> nil then
          begin
            if ddeObj is TEDdeClientConv then
              TEDdeClientConv(ddeObj).SrvrDisconnect
            else
              ddeMgr.Disconnect(ddeObj);
          end;
        end;
    end;
  end;
end;

function GetPasteLinkInfo(var Service, Topic, Item: string): Boolean;
var
  hData: THandle;
  pData: Pointer;
  P: PChar;
begin
  Result := False;
  Clipboard.Open;
  hData := Clipboard.GetAsHandle(ddeMgr.LinkClipFmt);
  if hData <> 0 then
  begin
    pData := GlobalLock(hData);
    try
      P := PChar(pData);
      Service := PChar(pData);
      P := P + Length(Service) + 1;
      Topic := P;
      P := P + Length(Topic) + 1;
      Item := P;
    finally
      GlobalUnlock(hData);
    end;
    Result := True;
  end;
  Clipboard.Close;
end;


{ TEDdeMgr }

constructor TEDdeMgr.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLinkClipFmt := RegisterClipboardFormat('Link');
  FDdeInstId := 0;
  DDECheck(DdeInitialize(FDdeInstId, DdeMgrCallBack, APPCLASS_STANDARD, 0) = 0);
  FConvs := TList.Create;
  FCliConvs := TList.Create;
  FConvCtrls := TList.Create;
  AppName := ParamStr(0);
end;

destructor TEDdeMgr.Destroy;
var
  I: Integer;
begin
  if FConvs <> nil then
  begin
    for I := 0 to FConvs.Count - 1 do
      TDdeSrvrConv(FConvs[I]).Free;
    FConvs.Free;
    FConvs := nil;
  end;
  if FCliConvs <> nil then
  begin
    for I := 0 to FCliConvs.Count - 1 do
      TDdeSrvrConv(FCliConvs[I]).Free;
    FCliConvs.Free;
    FCliConvs := nil;
  end;
  if FConvCtrls <> nil then
  begin
    FConvCtrls.Free;
    FConvCtrls := nil;
  end;
  ResetAppName;
  DdeUnInitialize(FDdeInstId);
  inherited Destroy;
end;

function TEDdeMgr.AllowConnect(hszApp: HSZ; hszTopic: HSZ): Boolean;
var
  Topic: string;
  Buffer: array[0..4095] of Char;
  Form: TForm;
  SConv: TEDdeServerConv;
begin
  Result := False;
  if (hszApp = 0) or (DdeCmpStringHandles(hszApp, FHszApp) = 0)  then
  begin
    SetString(Topic, Buffer, DdeQueryString(FDdeInstId, hszTopic, Buffer,
      SizeOf(Buffer), CP_WINANSI));
    SConv := GetServerConv(Topic);
    if SConv <> nil then
      Result := True
    else begin
      Form := GetForm(Topic);
      if Form <> nil then Result := True;
    end;
  end;
end;

function TEDdeMgr.AllowWildConnect(hszApp: HSZ; hszTopic: HSZ): HDdeData;
var
  conns: packed array[0..1] of THSZPair;
begin
  Result := 0;
  if hszTopic = 0 then Exit;
  if AllowConnect(hszApp, hszTopic) = True then
  begin
    conns[0].hszSvc := FHszApp;
    conns[0].hszTopic := hszTopic;
    conns[1].hszSvc := 0;
    conns[1].hszTopic := 0;
    Result := DdeCreateDataHandle(ddeMgr.DdeInstId, @conns,
      2 * sizeof(THSZPair), 0, 0, CF_TEXT, 0);
  end;
end;

function TEDdeMgr.Connect(Conv: HConv; hszTopic: HSZ; SameInst: Boolean): Boolean;
var
  Topic: string;
  Buffer: array[0..4095] of Char;
  DdeConv: TDdeSrvrConv;
begin
  DdeConv := TDdeSrvrConv.Create(Self);
  SetString(Topic, Buffer, DdeQueryString(FDdeInstId, hszTopic, Buffer,
    SizeOf(Buffer), CP_WINANSI));
  DdeConv.Topic := Topic;
  DdeConv.FSConv := GetServerConv(Topic);
  if DdeConv.FSConv = nil
  then DdeConv.FForm := GetForm(Topic);
  DdeConv.FConv := Conv;
  DdeSetUserHandle(Conv, QID_SYNC, LongInt(DdeConv));
  FConvs.Add(DdeConv);
  if DdeConv.FSConv <> nil then DdeConv.FSConv.Connect;
  Result := True;
end;

procedure TEDdeMgr.Disconnect(DdeSrvrConv: TComponent);
var
  DdeConv: TDdeSrvrConv;
begin
  DdeConv := TDdeSrvrConv(DdeSrvrConv);
  if DdeConv.FSConv <> nil then DdeConv.FSConv.Disconnect;
  if DdeConv.FConv <> 0 then DdeSetUserHandle(DdeConv.FConv, QID_SYNC, 0);
  DdeConv.FConv := 0;
  if FConvs <> nil then
  begin
    FConvs.Remove(DdeConv);
    DdeConv.Free;
  end;
end;

function TEDdeMgr.GetExeName: string;
begin
  Result := ParamStr(0);
end;

procedure TEDdeMgr.SetAppName(const AName: string);
var
  Dot: Integer;
begin
  ResetAppName;
  FAppName := ExtractFileName(AName);
  Dot := Pos('.', FAppName);
  if Dot <> 0 then
    Delete(FAppName, Dot, Length(FAppName));
  FHszApp := DdeCreateStringHandle(FDdeInstId, PChar(FAppName), CP_WINANSI);
  DdeNameService(FDdeInstId, FHszApp, 0, DNS_REGISTER);
end;

procedure TEDdeMgr.ResetAppName;
begin
  if FHszApp <> 0 then
  begin
    DdeNameService(FDdeInstId, FHszApp, 0, DNS_UNREGISTER);
    DdeFreeStringHandle(FDdeInstId, FHszApp);
  end;
  FHszApp := 0;
end;

function TEDdeMgr.GetServerConv(const Topic: string): TEDdeServerConv;
var
  I: Integer;
  SConv: TEDdeServerConv;
begin
  Result := nil;
  for I := 0 to FConvCtrls.Count - 1 do begin
    SConv := TEDdeServerConv(FConvCtrls[I]);
    if AnsiCompareText(SConv.Topic, Topic) = 0 then begin // name
      Result := SConv;
      Exit;
    end;
  end;
end;

function TEDdeMgr.GetForm(const Topic: string): TForm;
var
  I: Integer;
  Form: TForm;
begin
  Result := nil;
  for I := 0 to Screen.FormCount - 1 do
  begin
    Form := TForm(Screen.Forms[I]);
    if AnsiCompareText(Form.Caption, Topic) = 0 then
    begin
      Result := Form;
      Exit;
    end;
  end;
end;

function TEDdeMgr.GetSrvrConv(const Topic: string ): TComponent;
var
  I: Integer;
  Conv: TDdeSrvrConv;
begin
  Result := nil;
  for I := 0 to FConvs.Count - 1 do
  begin
    Conv := FConvs[I];
    if AnsiCompareText(Conv.Topic, Topic) = 0 then
    begin
      Result := Conv;
      Exit;
    end;
  end;
end;

procedure TEDdeMgr.PostDataChange(const Topic: string; Item: string);
var
  Conv: TDdeSrvrConv;
  Itm: TDdeSrvrItem;
begin
  Conv := TDdeSrvrConv(GetSrvrConv (Topic));
  If Conv <> nil then begin
    Itm := Conv.GetItem(Item);
    if Itm <> nil then Itm.PostDataChange;
  end;
end;

procedure TEDdeMgr.InsertServerConv(SConv: TEDdeServerConv);
begin
  FConvCtrls.Insert(FConvCtrls.Count, SConv);
end;

procedure TEDdeMgr.RemoveServerConv(SConv: TEDdeServerConv);
begin
  FConvCtrls.Remove(SConv);
end;

{procedure TEDdeMgr.DoError;
begin
  DDECheck(False);
end;}

constructor TEDdeClientConv.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FItems := TList.Create;
end;

destructor TEDdeClientConv.Destroy;
begin
  CloseLink;
  FItems.Free;
  FItems := nil;
  inherited Destroy;
end;

procedure TEDdeClientConv.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('LinkInfo', ReadLinkInfo, WriteLinkInfo,
    not ((DdeService = '') and (DdeTopic = '')));
end;

procedure TEDdeClientConv.Loaded;
var
  Service, Topic: string;
begin
  inherited Loaded;
  Service := DdeService;
  Topic := DdeTopic;
  if (Length(Service) <> 0) and (ConnectMode <> ddeManual) then
    ChangeLink(Service, Topic, '');
end;

procedure TEDdeClientConv.ReadLinkInfo (Reader: TReader);
var
  Value: string;
  Text: string;
  Temp: Integer;
begin
  Reader.ReadListBegin;
  while not Reader.EndOfList do
  begin
    Value := Reader.ReadString;
    Temp := Pos(' ', Value);
    Text := Copy(Value, Temp + 1, Length (Value) - Temp);
    case Value[1] of
      'S': SetService(Text);
      'T': SetTopic(Text);
    end;
  end;
  Reader.ReadListEnd;
end;

procedure TEDdeClientConv.WriteLinkInfo (Writer: TWriter);
var
  Value: string;
begin
  Writer.WriteListBegin;
  Value := DdeService;
  Writer.WriteString(Format('Service %s', [Value]));
  Value := DdeTopic;
  Writer.WriteString(Format('Topic %s', [Value]));
  Writer.WriteListEnd;
end;

procedure TEDdeClientConv.OnAttach(aCtrl: TEDdeClientItem);
var
  ItemLnk: TDdeCliItem;
begin
  ItemLnk := TDdeCliItem.Create(Self);
  FItems.Insert(FItems.Count, ItemLnk);
  ItemLnk.Control := aCtrl;
  ItemLnk.SetItem('');
end;

procedure TEDdeClientConv.OnDetach(aCtrl: TEDdeClientItem);
var
  ItemLnk: TDdeCliItem;
begin
  ItemLnk := TDdeCliItem(GetCliItemByCtrl(aCtrl));
  if ItemLnk <> nil then
  begin
    ItemLnk.SetItem('');
    FItems.Remove(ItemLnk);
    ItemLnk.Free;
  end;
end;

function TEDdeClientConv.OnSetItem(aCtrl: TEDdeClientItem; const S: string): Boolean;
var
  ItemLnk: TDdeCliItem;
begin
  Result := True;
  ItemLnk := TDdeCliItem(GetCliItemByCtrl(aCtrl));

  if (ItemLnk = nil) and (Length(S) > 0) then
  begin
    OnAttach (aCtrl);
    ItemLnk := TDdeCliItem(GetCliItemByCtrl(aCtrl));
  end;

  if (ItemLnk <> nil) and (Length(S) = 0) then
  begin
    OnDetach (aCtrl);
  end
  else if ItemLnk <> nil then
  begin
    Result := ItemLnk.SetItem(S);
    if Not (Result) and Not (csLoading in ComponentState) then
      OnDetach (aCtrl);  {error occurred, do cleanup}
  end;
end;

function TEDdeClientConv.GetCliItemByCtrl(ACtrl: TEDdeClientItem): TPersistent;
var
  ItemLnk: TDdeCliItem;
  I: word;
begin
  Result := nil;
  I := 0;
  while I < FItems.Count do
  begin
    ItemLnk := FItems[I];
    if ItemLnk.Control = aCtrl then
    begin
      Result := ItemLnk;
      Exit;
    end;
    Inc(I);
  end;
end;

function TEDdeClientConv.PasteLink: Boolean;
var
  Service, Topic, Item: string;
begin
  if GetPasteLinkInfo(Service, Topic, Item) = True then
    Result := ChangeLink(Service, Topic, Item) else
    Result := False;
end;

function TEDdeClientConv.ChangeLink(const App, Topic, Item: string): Boolean;
begin
  CloseLink;
  SetService(App);
  SetTopic(Topic);
  Result := OpenLink;
  if Not Result then
  begin
    SetService('');
    SetTopic('');
  end;
end;

function TEDdeClientConv.OpenLink: Boolean;
var
  CharVal: array[0..255] of Char;
  Res: Boolean;
begin
  Result := False;
  if FConv <> 0 then Exit;

  if (Length(DdeService) = 0) and (Length(DdeTopic) = 0) then
  begin
    ClearItems;
    Exit;
  end;

  if FHszApp = 0 then
  begin
    StrPCopy(CharVal, DdeService);
    FHszApp := DdeCreateStringHandle(ddeMgr.DdeInstId, CharVal, CP_WINANSI);
  end;
  if FHszTopic = 0 then
  begin
    StrPCopy(CharVal, DdeTopic);
    FHszTopic := DdeCreateStringHandle(ddeMgr.DdeInstId, CharVal, CP_WINANSI);
  end;
  Res := CreateDdeConv(FHszApp, FHszTopic);
  if Not Res then
  begin
    if Not((Length(DdeService) = 0) and
      (Length(ServiceApplication) = 0)) then
    begin
      if Length(ServiceApplication) <> 0 then
        StrPCopy(CharVal, ServiceApplication)
      else
        StrPCopy(CharVal, DdeService + ' ' + DdeTopic);
      if WinExec(CharVal, SW_SHOWMINNOACTIVE) >= 32 then
        Res := CreateDdeConv(FHszApp, FHszTopic);
    end;
  end;
  if Not Res then
  begin
    ClearItems;
    Exit;
  end;
  if FCnvInfo.wFmt <> 0 then FDdeFmt := FCnvInfo.wFmt
  else FDdeFmt := CF_TEXT;
  if StartAdvise = False then Exit;
  Open;
  DataChange(0, 0);
  Result := True;
end;

procedure TEDdeClientConv.CloseLink;
var
  OldConv: HConv;
begin
  if FConv <> 0 then
  begin
    OldConv := FConv;
    SrvrDisconnect;
    FConv := 0;
    DdeSetUserHandle(OldConv, QID_SYNC, 0);
    DdeDisconnect(OldConv);
  end;

  if FHszApp <> 0 then
  begin
    DdeFreeStringHandle(ddeMgr.DdeInstId, FHszApp);
    FHszApp := 0;
  end;

  if FHszTopic <> 0 then
  begin
    DdeFreeStringHandle(ddeMgr.DdeInstId, FHszTopic);
    FHszTopic := 0;
  end;
  SetService('');
  SetTopic('');
end;

procedure TEDdeClientConv.ClearItems;
var
  ItemLnk: TDdeCliItem;
  i: word;
begin
  if FItems.Count = 0 then Exit;

  for I := 0 to FItems.Count - 1 do
  begin
    ItemLnk := TDdeCliItem(FItems [0]);
    ItemLnk.Control.DdeItem := EmptyStr;
  end;
end;

function TEDdeClientConv.CreateDdeConv(FHszApp: HSZ; FHszTopic: HSZ): Boolean;
var
  Context: TConvContext;
begin
  FillChar(Context, SizeOf(Context), 0);
  with Context do
  begin
    cb := SizeOf(TConvConText);
    iCodePage := CP_WINANSI;
  end;
  FConv := DdeConnect(ddeMgr.DdeInstId, FHszApp, FHszTopic, @Context);
  Result := FConv <> 0;
  if Result then
  begin
    FCnvInfo.cb := sizeof(TConvInfo);
    DdeQueryConvInfo(FConv, QID_SYNC, @FCnvInfo);
    DdeSetUserHandle(FConv, QID_SYNC, LongInt(Self));
  end;
end;

function TEDdeClientConv.StartAdvise: Boolean;
var
  ItemLnk: TDdeCliItem;
  i: word;
begin
  Result := False;
  if FConv = 0 then Exit;

  i := 0;
  while i < FItems.Count do
  begin
    ItemLnk := TDdeCliItem(FItems [i]);
    if Not ItemLnk.StartAdvise then
    begin
      ItemLnk.Control.DdeItem := EmptyStr;
    end else
      Inc(i);
    if i >= FItems.Count then
      break;
  end;
  Result := True;
end;

function TEDdeClientConv.ExecuteMacroLines(Cmd: TStrings; waitFlg: Boolean): Boolean;
begin
  Result := False;
  if (FConv = 0) or FWaitStat then Exit;
  Result := ExecuteMacro(PChar(Cmd.Text), waitFlg);
end;

function TEDdeClientConv.ExecuteMacro(Cmd: PChar; waitFlg: Boolean): Boolean;
var
  hszCmd: HDDEData;
  hdata: HDDEData;
  ddeRslt: LongInt;
begin
  Result := False;
  if (FConv = 0) or FWaitStat then Exit;
  hszCmd := DdeCreateDataHandle(ddeMgr.DdeInstId, Cmd, StrLen(Cmd) + 1,
    0, 0, FDdeFmt, 0);
  if hszCmd = 0 then Exit;
  if waitFlg = True then FWaitStat := True;
  hdata := DdeClientTransaction(Pointer(hszCmd), Cardinal(-1), FConv, 0, FDdeFmt,
     XTYP_EXECUTE, TIMEOUT_ASYNC, @ddeRslt);
  if hdata = 0 then FWaitStat := False
  else Result := True;
end;

function TEDdeClientConv.PokeDataLines(const Item: string; Data: TStrings): Boolean;
begin
  Result := False;
  if (FConv = 0) or FWaitStat then Exit;
  Result := PokeData(Item, PChar(Data.Text));
end;

function TEDdeClientConv.PokeData(const Item: string; Data: PChar): Boolean;
var
  hszDat: HDDEData;
  hdata: HDDEData;
  hszItem: HSZ;
begin
  Result := False;
  if (FConv = 0) or FWaitStat then Exit;
  hszItem := DdeCreateStringHandle(ddeMgr.DdeInstId, PChar(Item), CP_WINANSI);
  if hszItem = 0 then Exit;
  hszDat := DdeCreateDataHandle (ddeMgr.DdeInstId, Data, StrLen(Data) + 1,
    0, hszItem, FDdeFmt, 0);
  if hszDat <> 0 then
  begin
    hdata := DdeClientTransaction(Pointer(hszDat), Cardinal(-1), FConv, hszItem,
      FDdeFmt, XTYP_POKE, TIMEOUT_ASYNC, nil);
    Result := hdata <> 0;
  end;
  DdeFreeStringHandle (ddeMgr.DdeInstId, hszItem);
end;

function TEDdeClientConv.RequestData(const Item: string): PChar;
var
  hData: HDDEData;
  ddeRslt: LongInt;
  hItem: HSZ;
  pData: Pointer;
  Len: Integer;
begin
  Result := nil;
  if (FConv = 0) or FWaitStat then Exit;
  hItem := DdeCreateStringHandle(ddeMgr.DdeInstId, PChar(Item), CP_WINANSI);
  if hItem <> 0 then
  begin
    hData := DdeClientTransaction(nil, 0, FConv, hItem, FDdeFmt,
      XTYP_REQUEST, 10000, @ddeRslt);
    DdeFreeStringHandle(ddeMgr.DdeInstId, hItem);
    if hData <> 0 then
    try
      pData := DdeAccessData(hData, @Len);
      if pData <> nil then
      try
        Result := StrAlloc(Len + 1);
        StrCopy(Result, pData);
      finally
        DdeUnaccessData(hData);
      end;
    finally
      DdeFreeDataHandle(hData);
    end;
  end;
end;

function TEDdeClientConv.GetCliItemByName(const ItemName: string): TPersistent;
var
  ItemLnk: TDdeCliItem;
  i: word;
begin
  Result := nil;
  i := 0;
  while i < FItems.Count do
  begin
    ItemLnk := TDdeCliItem(FItems[i]);
    if ItemLnk.Item = ItemName then
    begin
      Result := ItemLnk;
      Exit;
    end;
    Inc(i);
  end;
end;

procedure TEDdeClientConv.XactComplete;
begin
   FWaitStat := False;
end;

procedure TEDdeClientConv.SrvrDisconnect;
var
  ItemLnk: TDdeCliItem;
  i: word;
begin
  if FConv <> 0 then Close;
  FConv := 0;
  i := 0;
  while i < FItems.Count do
  begin
    ItemLnk := TDdeCliItem(FItems [i]);
    ItemLnk.SrvrDisconnect;
    inc(i);
  end;
end;

procedure TEDdeClientConv.DataChange(DdeDat: HDDEData; hszIt: HSZ);
var
  ItemLnk: TDdeCliItem;
  i: word;
begin
  i := 0;
  while i < FItems.Count do
  begin
    ItemLnk := TDdeCliItem(FItems [i]);
    if (hszIt = 0) or (ItemLnk.HszItem = hszIt) then
    begin
        { data has changed and we found a link that might be interested }
      ItemLnk.StoreData(DdeDat);
    end;
    Inc(i);
  end;
end;

function TEDdeClientConv.SetLink(const Service, Topic: string): Boolean;
begin
  CloseLink;
  if FConnectMode = ddeAutomatic then
    Result := ChangeLink(Service, Topic, '')
  else begin
    SetService(Service);
    SetTopic(Topic);
    DataChange(0,0);
    Result := True;
  end;
end;

procedure TEDdeClientConv.SetConnectMode(NewMode: TDataMode);
begin
  if FConnectMode <> NewMode then
  begin
    if (NewMode = ddeAutomatic) and (Length(DdeService) <> 0) and
      (Length(DdeTopic) <> 0) and not OpenLink then
      raise Exception.Create(SDdeNoConnect);
    FConnectMode := NewMode;
  end;
end;

procedure TEDdeClientConv.SetFormatChars(NewFmt: Boolean);
begin
  if FFormatChars <> NewFmt then
  begin
    FFormatChars := NewFmt;
    if FConv <> 0 then DataChange(0, 0);
  end;
end;

procedure TEDdeClientConv.SetDdeService(const Value: string);
begin
end;

procedure TEDdeClientConv.SetDdeTopic(const Value: string);
begin
end;

procedure TEDdeClientConv.SetService(const Value: string);
begin
  FDdeService := Value;
end;

procedure TEDdeClientConv.SetTopic(const Value: string);
begin
  FDdeTopic := Value;
end;

procedure TEDdeClientConv.Close;
begin
  if Assigned(FOnClose) then FOnClose(Self);
end;

procedure TEDdeClientConv.Open;
begin
  if Assigned(FOnOpen) then FOnOpen(Self);
end;

procedure TEDdeClientConv.Notification(AComponent: TComponent;
  Operation: TOperation);
var
  ItemLnk: TDdeCliItem;
  i: word;
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FItems <> nil) then
  begin
    i := 0;
    while i < FItems.Count do
    begin
      ItemLnk := TDdeCliItem(FItems [i]);
      if (AComponent = ItemLnk.Control) then
        ItemLnk.Control.DdeItem := EmptyStr;
      if i >= FItems.Count then break;
      Inc(I);
    end;
  end;
end;

constructor TEDdeClientItem.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLines := TStringList.Create;
end;

destructor TEDdeClientItem.Destroy;
begin
  FLines.Free;
  inherited Destroy;
end;

procedure TEDdeClientItem.SetDdeClientConv(Val: TEDdeClientConv);
var
  OldItem: string;
begin
  if Val <> FDdeClientConv then
  begin
    OldItem := DdeItem;
    FDdeClientItem := '';
    if FDdeClientConv <> nil then
      FDdeClientConv.OnDetach (Self);

    FDdeClientConv := Val;
    if FDdeClientConv <> nil then
    begin
      FDdeClientConv.FreeNotification(Self);
      if Length(OldItem) <> 0 then SetDdeClientItem (OldItem);
    end;
  end;
end;

procedure TEDdeClientItem.SetDdeClientItem(const Val: string);
begin
  if FDdeClientConv <> nil then
  begin
    FDdeClientItem := Val;
    if Not FDdeClientConv.OnSetItem (Self, Val) then
    begin
      if Not (csLoading in ComponentState) or
        not ((FDdeClientConv.FConv = 0) and
        (FDdeClientConv.ConnectMode = ddeManual)) then
        FDdeClientItem := '';
    end;
  end
  else if (csLoading in ComponentState) then
    FDdeClientItem := Val;
end;

procedure TEDdeClientItem.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FDdeClientConv) then
  begin
    FDdeClientConv.OnDetach (Self);
    FDdeClientConv := nil;
    FDdeClientItem := '';
  end;
end;

procedure TEDdeClientItem.OnAdvise;
begin
  if csDesigning in ComponentState then
  begin
    if Owner.InheritsFrom (TForm) and (TForm(Owner).Designer <> nil) then
      TForm(Owner).Designer.Modified;
  end;
  if Assigned(FOnChange) then FOnChange(Self);
end;

function TEDdeClientItem.GetText: string;
begin
  if FLines.Count > 0 then
    Result := FLines.Strings[0]
  else Result := '';
end;

procedure TEDdeClientItem.SetText(const S: string);
begin
end;

procedure TEDdeClientItem.SetLines(L: TStrings);
begin
end;

constructor TDdeCliItem.Create(ADS: TEDdeClientConv);
begin
  inherited Create;
  FHszItem := 0;
  FCliConv := ADS;
end;

destructor TDdeCliItem.Destroy;
begin
  StopAdvise;
  inherited Destroy;
end;

function TDdeCliItem.SetItem(const S: string): Boolean;
var
  OldItem: string;
begin
  Result := False;
  OldItem := Item;
  if FHszItem <> 0 then StopAdvise;

  FItem := S;
  FCtrl.Lines.Clear;

  if (Length(Item) <> 0) then
  begin
    if (FCliConv.Conv <> 0) then
    begin
      Result := StartAdvise;
      if Not Result then
        FItem := '';
    end
    else if FCliConv.ConnectMode = ddeManual then Result := True;
  end;
  RefreshData;
end;

procedure TDdeCliItem.StoreData(DdeDat: HDDEData);
var
  Len: Longint;
  Data: string;
  I: Integer;
begin
  if DdeDat = 0 then
  begin
    RefreshData;
    Exit;
  end;

  Data := PChar(AccessData(DdeDat, @Len));
  if Data <> '' then
  begin
    FCtrl.Lines.Text := Data;
    ReleaseData(DdeDat);
    if FCliConv.FormatChars = False then
    begin
      for I := 1 to Length(Data) do
        if (Data[I] > #0) and (Data[I] < ' ') then Data[I] := ' ';
      FCtrl.Lines.Text := Data;
    end;
  end;
  DataChange;
end;

function TDdeCliItem.RefreshData: Boolean;
var
  ddeRslt: LongInt;
  DdeDat: HDDEData;
begin
  Result := False;
  if (FCliConv.Conv <> 0) and (FHszItem <> 0) then
  begin
    if FCliConv.WaitStat = True then Exit;
    DdeDat := DdeClientTransaction(nil, Cardinal(-1), FCliConv.Conv, FHszItem,
      FCliConv.DdeFmt, XTYP_REQUEST, 1000, @ddeRslt);
    if DdeDat = 0 then Exit
    else begin
      StoreData(DdeDat);
      DdeFreeDataHandle(DdeDat);
      Result := True;
      Exit;
    end;
  end;
  DataChange;
end;

function TDdeCliItem.AccessData(DdeDat: HDDEData; pDataLen: PDWORD): Pointer;
begin
  Result := DdeAccessData(DdeDat, pDataLen);
end;

procedure TDdeCliItem.ReleaseData(DdeDat: HDDEData);
begin
  DdeUnaccessData(DdeDat);
end;

function TDdeCliItem.StartAdvise: Boolean;
var
  ddeRslt: LongInt;
  hdata: HDDEData;
begin
  Result := False;
  if FCliConv.Conv = 0 then Exit;
  if Length(Item) = 0 then Exit;
  if FHszItem = 0 then
    FHszItem := DdeCreateStringHandle(ddeMgr.DdeInstId, PChar(Item), CP_WINANSI);
  hdata := DdeClientTransaction(nil, Cardinal(-1), FCliConv.Conv, FHszItem,
    FCliConv.DdeFmt, XTYP_ADVSTART or XTYPF_NODATA, 1000, @ddeRslt);
  if hdata = 0 then
  begin
    DdeGetLastError(ddeMgr.DdeInstId);
    DdeFreeStringHandle(ddeMgr.DdeInstId, FHszItem);
    FHszItem := 0;
    FCtrl.Lines.Clear;
  end else
    Result := True;
end;

function TDdeCliItem.StopAdvise: Boolean;
var
  ddeRslt: LongInt;
begin
  if FCliConv.Conv <> 0 then
    if FHszItem <> 0 then
      DdeClientTransaction(nil, Cardinal(-1), FCliConv.Conv, FHszItem,
        FCliConv.DdeFmt, XTYP_ADVSTOP, 1000, @ddeRslt);
  SrvrDisconnect;
  Result := True;
end;

procedure TDdeCliItem.SrvrDisconnect;
begin
  if FHszItem <> 0 then
  begin
    DdeFreeStringHandle(ddeMgr.DdeInstId, FHszItem);
    FHszItem := 0;
  end;
end;

procedure TDdeCliItem.DataChange;
begin
  FCtrl.OnAdvise;
end;

constructor TEDdeServerItem.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFmt := CF_TEXT;
  FLines := TStringList.Create;
end;

destructor TEDdeServerItem.Destroy;
begin
  FLines.Free;
  inherited Destroy;
end;

procedure TEDdeServerItem.SetServerConv(SConv: TEDdeServerConv);
begin
  FServerConv := SConv;
  if SConv <> nil then SConv.FreeNotification(Self);
end;

function TEDdeServerItem.GetText: string;
begin
  if FLines.Count > 0 then
    Result := FLines.Strings[0]
  else Result := '';
end;

procedure TEDdeServerItem.SetText(const Item: string);
begin
  FFmt := CF_TEXT;
  FLines.Clear;
  FLines.Add(Item);
  ValueChanged;
end;

procedure TEDdeServerItem.SetLines(Value: TStrings);
begin
  if CompareStr(Value.Text, FLines.Text) <> 0 then
  begin
    FFmt := CF_TEXT;
    FLines.Assign(Value);
    ValueChanged;
  end;
end;

procedure TEDdeServerItem.ValueChanged;
begin
  if Assigned(FOnChange) then FOnChange(Self);
  if FServerConv <> nil then
    ddeMgr.PostDataChange(FServerConv.Topic, Item)  // Name
  else if (Owner <> nil) and (Owner is TForm) then
    ddeMgr.PostDataChange(TForm(Owner).Caption, Item);
end;

function TEDdeServerItem.PokeData(Data: HDdeData): LongInt;
var
  Len: Integer;
  pData: Pointer;
begin
  Result := dde_FNotProcessed;
  pData := DdeAccessData(Data, @Len);
  if pData <> nil then
  begin
    Lines.Text := PChar(pData);
    DdeUnaccessData(Data);
    ValueChanged;
    if Assigned(FOnPokeData) then FOnPokeData(Self);
    Result := dde_FAck;
  end;
end;

procedure TEDdeServerItem.CopyToClipboard;
var
  Data: THandle;
  LinkData: string;
  DataPtr: Pointer;
begin
  if FServerConv <> nil then
    LinkData := ddeMgr.AppName + #0 + FServerConv.Topic + #0 + Item // name
  else if (Owner =nil) then Exit
  else if Owner is TForm then
    LinkData := ddeMgr.AppName + #0 + TForm(Owner).Caption + #0 + Item;
  try
    Clipboard.AsText := Text;
    Data := GlobalAlloc(GMEM_MOVEABLE, Length(LinkData) + 1);
    try
      DataPtr := GlobalLock(Data);
      try
        Move(PChar(LinkData)^, DataPtr^, Length(LinkData) + 1);
        Clipboard.SetAsHandle(DdeMgr.LinkClipFmt, Data);
      finally
        GlobalUnlock(Data);
      end;
    except
      GlobalFree(Data);
      raise;
    end;
  finally
    Clipboard.Close;
  end;
end;

procedure TEDdeServerItem.Change;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TEDdeServerItem.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FServerConv) and (Operation = opRemove) then
    FServerConv := nil;
end;

constructor TEDdeServerConv.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  // назначить им€ компонента как им€ темы сначала
  FTopic:= Self.Name;
  ddeMgr.InsertServerConv (Self);
end;

destructor TEDdeServerConv.Destroy;
begin
  ddeMgr.RemoveServerConv(Self);
  inherited Destroy;
end;

procedure TEDdeServerConv.SetTopic(ATopic: String);
begin
  FTopic:= ATopic;
end;

function TEDdeServerConv.ExecuteMacro(Data: HDdeData): LongInt;
var
  Len: Integer;
  pData: Pointer;
  MacroLines: TStringList;
begin
  Result := dde_FNotProcessed;
  pData := DdeAccessData(Data, @Len);
  if pData <> nil then
  begin
    if Assigned(FOnExecuteMacro) then
    begin
      MacroLines := TStringList.Create;
      MacroLines.Text := PChar(pData);
      FOnExecuteMacro(Self, MacroLines);
      MacroLines.Destroy;
    end;
    Result := dde_FAck;
  end;
end;

procedure TEDdeServerConv.Connect;
begin
  if Assigned(FOnOpen) then FOnOpen(Self);
end;

procedure TEDdeServerConv.Disconnect;
begin
  if Assigned(FOnClose) then FOnClose(Self);
end;

constructor TDdeSrvrConv.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FItems := TList.Create;
end;

destructor TDdeSrvrConv.Destroy;
var
  I: Integer;
begin
  if FItems <> nil then
  begin
    for I := 0 to FItems.Count - 1 do
      TDdeSrvrItem(FItems[I]).Free;
    FItems.Free;
    FItems := nil;
  end;
  if FConv <> 0 then DdeDisconnect(FConv);
  if FHszTopic <> 0 then
  begin
    DdeFreeStringHandle(ddeMgr.DdeInstId, FHszTopic);
    FHszTopic := 0;
  end;
  inherited Destroy;
end;

function TDdeSrvrConv.AdvStart(Conv: HConv; hszTopic: HSZ; hszItem: HSZ;
  Fmt: Word): Boolean;
var
  Srvr: TEDdeServerItem;
  Buffer: array[0..4095] of Char;
  SrvrItem: TDdeSrvrItem;
begin
  Result := False;
  if Fmt <> CF_TEXT then Exit;
  DdeQueryString(ddeMgr.DdeInstId, hszItem, Buffer, SizeOf(Buffer), CP_WINANSI);
  Srvr := GetControl(FForm, FSConv, Buffer);
  if Srvr = nil then Exit;
  SrvrItem := TDdeSrvrItem.Create(Self);
  SrvrItem.Srvr := Srvr;
  SrvrItem.Item := Buffer;
  FItems.Add(SrvrItem);
  SrvrItem.FreeNotification(Self);
  if FHszTopic = 0 then
    FHszTopic := DdeCreateStringHandle(ddeMgr.DdeInstId, PChar(Topic), CP_WINANSI);
  Result := True;
end;

procedure TDdeSrvrConv.AdvStop(Conv: HConv; hszTopic: HSZ; hszItem :HSZ);
var
  SrvrItem: TDdeSrvrItem;
begin
  SrvrItem := GetSrvrItem(hszItem);
  if SrvrItem <> nil then
  begin
    FItems.Remove(SrvrItem);
    SrvrItem.Free;
  end;
end;

function TDdeSrvrConv.PokeData(Conv: HConv; hszTopic: HSZ; hszItem: HSZ;
  Data: HDdeData; Fmt: Integer): LongInt;
var
  Srvr: TEDdeServerItem;
  Buffer: array[0..4095] of Char;
begin
  Result := dde_FNotProcessed;
  if Fmt <> CF_TEXT then Exit;
  DdeQueryString(ddeMgr.DdeInstId, hszItem, Buffer, SizeOf(Buffer), CP_WINANSI);
  Srvr := GetControl(FForm, FSConv, Buffer);
  if Srvr <> nil then Result := Srvr.PokeData(Data);
end;

function TDdeSrvrConv.ExecuteMacro(Conv: HConv; hszTopic: HSZ;
  Data: HDdeData): Integer;
begin
  Result := dde_FNotProcessed;
  if (FSConv <> nil)  then
    Result := FSConv.ExecuteMacro(Data);
end;

function TDdeSrvrConv.RequestData(Conv: HConv; hszTopic: HSZ; hszItem :HSZ;
  Fmt: Word): HDdeData;
var
  Data: string;
  Buffer: array[0..4095] of Char;
  SrvrIt: TDdeSrvrItem;
  Srvr: TEDdeServerItem;
  R: HDdeData;
begin
  R:= 0;
  SrvrIt := GetSrvrItem(hszItem);
  if SrvrIt <> nil then begin
    R:= SrvrIt.RequestData(Fmt)
  end else begin
    DdeQueryString(ddeMgr.DdeInstId, hszItem, Buffer, SizeOf(Buffer), CP_WINANSI);
    Srvr := GetControl(FForm, FSConv, Buffer);
    if Srvr <> nil then begin
      if Fmt = CF_TEXT then begin
        Data:= Srvr.Lines.Text;
        R:= DdeCreateDataHandle(ddeMgr.DdeInstId, PChar(Data),
          Length(Data) + 1, 0, hszItem, Fmt, 0 );
      end;
    end;
  end;
  Result:=R;
end;

function TDdeSrvrConv.GetControl(WinCtrl: TWinControl; DdeConv: TEDdeServerConv; const ItemName: string): TEDdeServerItem;
var
  I: Integer;
  Ctrl: TComponent;
  MainCtrl: TWinControl;
  Srvr: TEDdeServerItem;
begin
  Result := nil;
  MainCtrl := WinCtrl;
  if MainCtrl = nil then
  begin
    if (DdeConv <> nil) and (DdeConv.Owner <> nil) and
      (DdeConv.Owner is TForm) then
      MainCtrl := TWinControl(DdeConv.Owner);
  end;
  if MainCtrl = nil then Exit;
  for I := 0 to MainCtrl.ComponentCount - 1 do
  begin
    Ctrl := MainCtrl.Components[I];
    if Ctrl is TEDdeServerItem then begin
      if ((Ctrl as TEDdeServerItem).Item = ItemName) and
        (TEDdeServerItem(Ctrl).ServerConv = DdeConv) then
      begin
        Result := TEDdeServerItem(Ctrl);
        Exit;
      end;
    end;
    if Ctrl is TWinControl then
    begin
      Srvr := GetControl(TWinControl(Ctrl), DdeConv, ItemName);
      if Srvr <> nil then
      begin
        Result := Srvr;
        Exit;
      end;
    end;
  end;
end;

function TDdeSrvrConv.GetItem(const ItemName: string): TDdeSrvrItem;
var
  I: Integer;
  Item: TDdeSrvrItem;
begin
  Result := nil;
  for I := 0 to FItems.Count - 1 do
  begin
    Item := FItems[I];
    If Item.Item = ItemName then
    begin
      Result := Item;
      Exit;
    end;
  end;
end;

function TDdeSrvrConv.GetSrvrItem(hszItem: HSZ): TDdeSrvrItem;
var
  I: Integer;
  Item: TDdeSrvrItem;
begin
  Result := nil;
  for I := 0 to FItems.Count - 1 do
  begin
    Item := FItems[I];
    If DdeCmpStringHandles(Item.HszItem, hszItem) = 0 then
    begin
      Result := Item;
      Exit;
    end;
  end;
end;

constructor TDdeSrvrItem.Create(AOwner: TComponent);
begin
  FConv := TDdeSrvrConv(AOwner);
  // им€ элемента
  FItem:= Self.Name;
  inherited Create(AOwner);
end;

destructor TDdeSrvrItem.Destroy;
begin
  if FHszItem <> 0 then
  begin
    DdeFreeStringHandle(ddeMgr.DdeInstId, FHszItem);
    FHszItem := 0;
  end;
  inherited Destroy;
end;

procedure TEDdeServerItem.SetItem(AItem: String);
begin
  FItem:= AItem;
end;

function TDdeSrvrItem.RequestData(Fmt: Word): HDdeData;
var
  Data: string;
  Buffer: array[0..4095] of Char;
begin
  Result := 0;
  SetString(FItem, Buffer, DdeQueryString(ddeMgr.DdeInstId, FHszItem, Buffer,
    SizeOf(Buffer), CP_WINANSI));
  if Fmt = CF_TEXT then
  begin
    Data := FSrvr.Lines.Text;
    Result := DdeCreateDataHandle(ddeMgr.DdeInstId, PChar(Data), Length(Data) + 1,
      0, FHszItem, Fmt, 0 );
  end;
end;

procedure TDdeSrvrItem.PostDataChange;
begin
  DdePostAdvise(ddeMgr.DdeInstId, FConv.HszTopic, FHszItem);
end;

procedure TDdeSrvrItem.SetItem(const Value: string);
begin
  FItem := Value;
  if FHszItem <> 0 then
  begin
    DdeFreeStringHandle(ddeMgr.DdeInstId, FHszItem);
    FHszItem := 0;
  end;
  if Length(FItem) > 0 then
    FHszItem := DdeCreateStringHandle(ddeMgr.DdeInstId, PChar(FItem), CP_WINANSI);
end;

begin
  ddeMgr := TEDdeMgr.Create(Application);
end.
