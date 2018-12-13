unit
  IdxStruct;
(*##*)
(*******************************************************************************
*                                                                             *
*                                                                              *
*   I  D  X  S  T  R  U  C  T    ISO2709 bibliography records structures      *
*                                                                              *
*   Copyright © Andrei Ivanov 1996, 1997, 2002-2003. All rights reserved.     *
*   functions 2 access 2 files in US MARC format for bibliography              *
*   and fundamental marc record iterator GetRecordsQTY                        *
*   known as LDB files.                                                        *
*   Conditional defines: STRINGSVALUES- store '090a=' to each TStrings        *
*   ISO 2709                                                                   *
*   Last Revision: Dec 31 1996                                                *
*   Last fix     : May 02 1997                                                 *
*   haircut        Jul 25 1998 (some routines moved to marc_acc)              *
*   Lines        : 385                                                         *
*   Printed      : ---                                                        *
*                                                                              *
*******************************************************************************)
(*##*)

interface
{$DEFINE XML_IDX}
uses
  RTLConsts, Windows, Classes, SysUtils,
  jclStrHashMap, jclUnicode;

const
  WORDLENMAX = 32;
  MAX_FLD_IDX = 2; // 3 characters of Marc field
  MAXFILEBUFSIZE = $F000;

type
  { words.ind record header structure}
  TWHdrRec = packed record
    fs: Cardinal;  { fld start offset }
    ff: Cardinal;  { fld finish offset }
  end;

  { words.ind record structure}
  PWordRec = ^TWordRec;
  TWordRec = packed record
    h: TWHdrRec;
    w: ShortString;
  end;

{$IFDEF XML_IDX}
  TWordFld2LocItemFld = Word;
const
  EMPTY_FLD = High(TWordFld2LocItemFld);
{$ELSE}
  TWordFld2LocItemFld = packed array[0..MAX_FLD_IDX] of Char;
const
  EMPTY_FLD = #0;
{$ENDIF}
type
  { flds.ind record structure}
  TFldRec = packed record
    fld: idxStruct.TWordFld2LocItemFld;
    ls: Cardinal;  { record start offset }
    lf: Cardinal;  { record finish offset }
  end;

  { locs.ind record structure}
  TLocRec = Cardinal;

  // merge structures

  PWordBEs = ^TWordBEs;
  TWordBEs = packed record
    msk: Cardinal;
    l: Byte;
    a: array [0..31] of TWHdrRec;
  end;

  TReportProc = procedure of object;

  THashWordList = class(jclStrHashMap.TStringHashMap)
  private
    FFldOrder: Word;
    FStringHashMapTraits: TStringHashMapTraits;
    FStringList: TStrings;
    FForceRecreateStringList: Boolean;
    function GetWordOccurances(AIndex: Integer): Cardinal;
    function GetWordUp(AIndex: Integer): string;
    procedure SetWordOccurances(AIndex: Integer; AValue: Cardinal);
    function IterateCreateStringList(AUserData: Pointer; const AStr: string; var APtr: Pointer): Boolean;
    function IterateStoreHash(AUserData: Pointer; const AStr: string; var APtr: Pointer): Boolean;
  protected
    function CheckCreateStringList: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure CheckSortList;
    procedure Prepare2StoreIndex;
    procedure AddNewWord(const ASrch: string);
    procedure AddNewWord32(const ASrch: string; AMask: Cardinal);
    procedure AddNewWordMax(const AWr: TWordRec; AOrder, AMax: Byte);
    procedure AddNewWordOrd(const ASrch: string);  //     TWordFld2LocItemFld
    procedure LoadFromStream(AStream: TStream); virtual;
    procedure LoadFromWrdStream(AStream: TStream);
    procedure LoadFromWrdStream32(AStream: TStream; AOrder: Byte);
    procedure LoadFromWrdStreamMax(AStream: TStream; AOrder, AMax: Byte);
    procedure SaveToStream(const AStream: TStream);
    procedure LoadFromFile(const AFileName: string); virtual;
    procedure SaveToFile(const AFileName: string);
    // before - CheckCreateStringList must be called!
    property WordOccurances[AIndex: Integer]: Cardinal read GetWordOccurances write SetWordOccurances;
    property WordUp[AIndex: Integer]: string read GetWordUp;
  end;

  TStringFldHash = class(jclStrHashMap.TStringHashMap)
  private
    FStringHashMapTraits: TStringHashMapTraits;
  public
    constructor Create;
    destructor Destroy; override;
    property Node[const s: string]: PPHashNode read FindNode;
  end;

  TWordList = class(TStringList)
  private
    function GetWordOccurances(AIndex: Integer): Cardinal;
    procedure SetWordOccurances(AIndex: Integer; AValue: Cardinal);
    function GetWordUp(AIndex: Integer): string;
  protected
  public
    constructor Create;
    procedure InsertObject(Index: Integer; const S: string; AObject: TObject); override;
    procedure AddNewWord(const ASrch: string);
    procedure CheckSortList;
    procedure Prepare2StoreIndex;

    property WordOccurances[AIndex: Integer]: Cardinal read GetWordOccurances write SetWordOccurances;
    property WordUp[AIndex: Integer]: string read GetWordUp;
  end;

  TWideWordList = class(TWideStringList)
  private
    function GetWordOccurances(AIndex: Integer): Cardinal;
    procedure SetWordOccurances(AIndex: Integer; AValue: Cardinal);
    function GetWordUp(AIndex: Integer): string;
  protected
  public
    constructor Create;
    procedure Insert(Index: Integer; const S: WideString); override;
    procedure AddNewWord(const ASrch: WideString);
    procedure CheckSortList;
    procedure Prepare2StoreIndex;

    property WordOccurances[AIndex: Integer]: Cardinal read GetWordOccurances write SetWordOccurances;
    property WordUp[AIndex: Integer]: string read GetWordUp;
  end;

{ TWordFld2RecList class }

type
  TWordFld2LocMemoryList = class;
  PWordFld2LocItem = ^TWordFld2LocItem;

  TWordFld2LocItem = packed record
    Wrd: Cardinal;
    Fld: TWordFld2LocItemFld;
    Loc: Cardinal; // Int64
  end;

  // for merge
  TFld2LocItem = packed record
    Fld: TWordFld2LocItemFld;
    Loc: Cardinal; // Int64
  end;
  TFld2LocItems = array of TFld2LocItem;

  TWordFld2LocArr = array[0..0] of TWordFld2LocItem; //MaxInt div SizeOf(TWordFld2LocItem)
  PWordFld2LocArr = ^TWordFld2LocArr;

  TWordFld2LocCustomList = class(TPersistent)
  private
    FCount: Integer;
    FCapacity: Integer;
    procedure Grow; virtual;
  protected
    procedure Error(const Msg: string; Data: Integer); overload;
    procedure Error(Msg: PResStringRec; Data: Integer); overload;
    function GetCapacity: Integer; // override;
    procedure SetCapacity(NewCapacity: Integer); virtual;
    function CompareItems(const I1, I2: TWordFld2LocItem): Integer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Sort; virtual; abstract;
    function RmDuplicates: Cardinal; virtual; abstract;

    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read FCount;
  end;

  TWordFld2LocMemoryList = class(TWordFld2LocCustomList)
  private
    FList: PWordFld2LocArr;
    procedure QuickSort(L, R: Integer);
  protected
    function Get(Index: Integer): TWordFld2LocItem;
    procedure Put(Index: Integer; AItem: TWordFld2LocItem);
    procedure InsertItem(Index: Integer; const AItem: TWordFld2LocItem);
    procedure SetCapacity(NewCapacity: Integer); override;
  public
    constructor Create;
    destructor Destroy; override;
    function Add(const AItem: TWordFld2LocItem): Integer;
    procedure Clear; // override;
    procedure Delete(Index: Integer); // override;
    function IndexOf(const AItem: TWordFld2LocItem): Integer;
    procedure Insert(Index: Integer; const AItem: TWordFld2LocItem);
    function GetText(AWordsStrings: TStrings): string;

    procedure DeleteRange(AFirstIdx, ALastIdx: Integer);
    function Find(AItem: TWordFld2LocItem; var Index: Integer): Boolean;

    procedure DirectAddItem(const AItem: TWordFld2LocItem);
    procedure Sort; override;
    function RmDuplicates: Cardinal; override;

    property Items[Index: Integer]: TWordFld2LocItem read Get write Put; default;
  end;

  TFileBuffer = array[0..MAXFILEBUFSIZE - 1] of Byte;

  TWordFld2LocFileList = class(TWordFld2LocCustomList)
  private
    Flistn: string; // temprorary file name
    FList: TStream;
    FSortPtr: array of Cardinal;
    FFileBuffer: TFileBuffer;
    procedure QuickSort(L, R: Integer);
    procedure MergeSort;
    procedure ExternalSort;
  protected
    function Get(Index: Integer): TWordFld2LocItem;
    procedure Put(Index: Integer; AItem: TWordFld2LocItem);
    procedure SetCapacity(NewCapacity: Integer); override;
  public
    constructor Create;
    destructor Destroy; override;

    procedure DeleteRange(AFirstIdx, ALastIdx: Integer);
    function Find(AItem: TWordFld2LocItem; var Index: Integer): Boolean;

    procedure DirectAddItem(const AItem: TWordFld2LocItem);
    procedure Sort; override;
    function RmDuplicates: Cardinal; override;

    property Items[Index: Integer]: TWordFld2LocItem read Get write Put; default;
  end;

  TCustomProgressThread = class(TThread)
  private
    FReportProc: TReportProc;
  public
    Progress,
    ProgressCount: Cardinal;
    ProgressState: String;
    procedure Report(AProgress: Cardinal; AProgressCount: Cardinal; const AProgressStatus: String);
    constructor Create(ASuspended: Boolean; AReportProc: TReportProc);
  end;

procedure StoreOfsList(AStrings: TStrings; AStream: TStream);

implementation
uses
  util1, ShellAPI;
// -------------------------- TWordFld2LocCustomList ---------------------------

constructor TWordFld2LocCustomList.Create;
begin
  inherited Create;
  FCount:= 0;
  SetCapacity(0);
end;

destructor TWordFld2LocCustomList.Destroy;
begin
  inherited Destroy;
  FCount:= 0;
  SetCapacity(0);
end;

procedure TWordFld2LocCustomList.Error(const Msg: string; Data: Integer);
  function ReturnAddr: Pointer;
  asm
          MOV     EAX,[EBP+4]
  end;
begin
  raise EStringListError.CreateFmt(Msg, [Data])at ReturnAddr;
end;

procedure TWordFld2LocCustomList.Error(Msg: PResStringRec; Data: Integer);
begin
  Error(LoadResString(Msg), Data);
end;

function TWordFld2LocCustomList.GetCapacity: Integer;
begin
  Result:= FCapacity;
end;

procedure TWordFld2LocCustomList.Grow;
var
  Delta: Integer;
begin
  if FCapacity > 2048 * 64
    then Delta:= FCapacity div 64
  else Delta:= 2048;
  SetCapacity(FCapacity + Delta);
end;

procedure TWordFld2LocCustomList.SetCapacity(NewCapacity: Integer);
begin
  FCapacity:= NewCapacity;
end;

function TWordFld2LocCustomList.CompareItems(const I1, I2: TWordFld2LocItem): Integer;
begin
  if (I1.Wrd > I2.Wrd)
    then Result:= 1
  else if (I1.Wrd < I2.Wrd)
    then Result:= -1
  else begin
      // Wrd is equil
    if (I1.Fld > I2.Fld)
      then Result:= 1
    else if (I1.Fld < I2.Fld)
      then Result:= -1
    else begin
          // Wrd and Fld are equil
      if (I1.Loc > I2.Loc)
        then Result:= 1
      else if (I1.Loc < I2.Loc)
        then Result:= -1
      else Result:= 0;
    end;
  end;
end;

// -------------------------------- TWordFld2LocMemoryList ---------------------------

constructor TWordFld2LocMemoryList.Create;
begin
  inherited Create;
end;

destructor TWordFld2LocMemoryList.Destroy;
begin
  inherited Destroy;
end;

function TWordFld2LocMemoryList.Add(const AItem: TWordFld2LocItem): Integer;
begin
  if Find(AItem, Result)
    then Exit;
  InsertItem(Result, AItem);
end;

procedure TWordFld2LocMemoryList.Clear;
begin
  if FCount <> 0 then begin
    FCount:= 0;
    SetCapacity(0);
  end;
end;

procedure TWordFld2LocMemoryList.Delete(Index: Integer);
begin
  if (Index < 0) or (Index >= FCount)
    then Error(@SListIndexError, Index);
  Dec(FCount);
  if Index < FCount then
    System.Move(FList^[Index + 1], FList^[Index],
      (FCount - Index) * SizeOf(TWordFld2LocItem));
end;

procedure TWordFld2LocMemoryList.DeleteRange(AFirstIdx, ALastIdx: Integer);
var
  c: Integer;
begin
  if (AFirstIdx < 0) or (ALastIdx >= FCount) or (ALastIdx < AFirstIdx)
    then Error(@SListIndexError, AFirstIdx);
  c:= ALastIdx - AFirstIdx + 1;
  FCount:= FCount - c;
  // if AFirstIdx < FCount then
  System.Move(FList^[ALastIdx + 1], FList^[AFirstIdx], // ALastIdx + 1  == AFirstIdx + c
    (FCount - AFirstIdx) * SizeOf(TWordFld2LocItem)); // FCount(before) - ALastIdx - 1 == FCount(after) - AFirstIdx
end;

function TWordFld2LocMemoryList.Find(AItem: TWordFld2LocItem; var Index: Integer): Boolean;
var
  L, H, I, C: Integer;
begin
  Result:= False;
  L:= 0;
  H:= FCount - 1;
  while L <= H do begin
    I:= (L + H) shr 1;
    C:= CompareItems(FList^[I], AItem);
    if C < 0 then L:= I + 1 else begin
      H:= I - 1;
      if C = 0 then begin
        Result:= True;
        L:= I; // if Duplicates <> dupAccept then
      end;
    end;
  end;
  Index:= L;
end;

function TWordFld2LocMemoryList.Get(Index: Integer): TWordFld2LocItem;
begin
  if (Index < 0) or (Index >= FCount) then Error(@SListIndexError, Index);
  Result:= FList^[Index];
end;

function TWordFld2LocMemoryList.IndexOf(const AItem: TWordFld2LocItem): Integer;
begin
  if not Find(AItem, Result)
    then Result:= -1;
end;

procedure TWordFld2LocMemoryList.Insert(Index: Integer; const AItem: TWordFld2LocItem);
begin
  if (Index < 0) or (Index > FCount) then Error(@SListIndexError, Index);
  InsertItem(Index, AItem);
end;

procedure TWordFld2LocMemoryList.InsertItem(Index: Integer; const AItem: TWordFld2LocItem);
begin
  if FCount = FCapacity then Grow;
  if Index < FCount then
    System.Move(FList^[Index], FList^[Index + 1],
      (FCount - Index) * SizeOf(TWordFld2LocItem));
  with FList^[Index] do begin
    FList^[Index]:= AItem;
  end;
  Inc(FCount);
end;

procedure TWordFld2LocMemoryList.DirectAddItem(const AItem: TWordFld2LocItem);
begin
  if FCount = FCapacity
    then Grow;
  FList^[FCount]:= AItem;
  Inc(FCount);
end;

procedure TWordFld2LocMemoryList.Put(Index: Integer; AItem: TWordFld2LocItem);
begin
  if (Index < 0) or (Index >= FCount)
    then Error(@SListIndexError, Index);
  FList^[Index]:= AItem;
end;

procedure TWordFld2LocMemoryList.QuickSort(L, R: Integer);
var
  I, J, P: Integer;
  Temp: TWordFld2LocItem;
begin
  repeat
    I:= L;
    J:= R;
    P:= (L + R) shr 1;
    repeat
      while CompareItems(FList^[I], FList^[P]) < 0 do Inc(I);
      while CompareItems(FList^[J], FList^[P]) > 0 do Dec(J);
      {
      while SCompare(Self, I, P) < 0 do Inc(I);
      while SCompare(Self, J, P) > 0 do Dec(J);
      }
      if I <= J then begin
//      ExchangeItems(I, J); faster a litte
        Temp:= FList^[J];
        FList^[J]:= FList^[I];
        FList^[I]:= Temp;
//      exchange
        if P = I then
          P:= J
        else if P = J then
          P:= I;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then QuickSort(L, J);
    L:= I;
  until I >= R;
end;

procedure TWordFld2LocMemoryList.SetCapacity(NewCapacity: Integer);
begin
  ReallocMem(FList, NewCapacity * SizeOf(TWordFld2LocItem));
  inherited SetCapacity(NewCapacity);
end;

function TWordFld2LocMemoryList.GetText(AWordsStrings: TStrings): string;
var
  c: Integer;
begin
  Result:= '';
  for c:= 0 to FCount - 1 do with FList^[c] do begin
    // Result:= Result + IntToHex(Wrd, 8) + ' ' + Fld[0] + Fld[1] + Fld[2] + ' ' + IntToHex(Loc, 8) + #13#10;
{$IFDEF XML_IDX}
      Result:= Result + AWordsStrings[Wrd] + ' ' + IntToStr(Fld) + ' ' + IntToHex(Loc, 8) + #13#10;
{$ELSE}
      Result:= Result + AWordsStrings[Wrd] + ' ' + Fld[0] + Fld[1] + Fld[2] + ' ' + IntToHex(Loc, 8) + #13#10;
{$ENDIF}
    end;
end;

procedure TWordFld2LocMemoryList.Sort;
begin
  if (FCount > 1) then begin
    QuickSort(0, FCount - 1);
  end;
end;

function TWordFld2LocMemoryList.RmDuplicates: Cardinal;
var
  i: Integer;
  it, cur: TWordFld2LocItem;
  tmpfn: string;
  tmpstrm: TFileStream;
begin
  Result:= 0;
  tmpfn:= '!duplicates.tmp';
  tmpstrm:= TFileStream.Create(tmpfn, fmCreate);
  with it do begin
    Wrd:= High(Wrd);
{$IFDEF XML_IDX}
    Fld:= High(TWordFld2LocItemFld);
{$ELSE}
    Fld:= '#@!';
{$ENDIF}
    Loc:= High(Loc);
  end;
  i:= 0;
  while i < FCount do begin
    cur:= FList^[i];
    if (cur.Wrd = it.Wrd) and (cur.Fld = it.Fld) and (cur.Loc = it.Loc) then begin
      Result:= Result + 1;
      // Delete(i);
      Inc(i);
    end else begin
      // if (cur.Wrd < it.Wrd) then raise Exception.CreateFmt('Not sorted yet', []);
      tmpstrm.Write(cur, SizeOf(TWordFld2LocItem));
      it:= FList^[i];
      Inc(i);
    end;
  end;
  // free space
  // FCount:= tmpstrm.Size div SizeOf(TWordFld2LocItem);
  FCount:= FCount - Result;
  FCapacity:= FCount;
  ReallocMem(FList, FCapacity * SizeOf(TWordFld2LocItem));
  // read compacted data
  with tmpstrm do begin
    Position:= 0;
    Read(FList^[0], Size);
    // free up
    Free;
  end;
  DeleteFile(tmpfn);
end;

function WinExec32AndWait(const Cmd: string; const CmdShow: Integer): Cardinal;
var
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
begin
  Result := Cardinal($FFFFFFFF);
  FillChar(StartupInfo, SizeOf(TStartupInfo), #0);
  StartupInfo.cb := SizeOf(TStartupInfo);
  StartupInfo.dwFlags := STARTF_USESHOWWINDOW;
  StartupInfo.wShowWindow := CmdShow;
  if CreateProcess(nil, PChar(Cmd), nil, nil, False, NORMAL_PRIORITY_CLASS,
    nil, nil, StartupInfo, ProcessInfo) then
  begin
    WaitForInputIdle(ProcessInfo.hProcess, INFINITE);
    if WaitForSingleObject(ProcessInfo.hProcess, INFINITE) = WAIT_OBJECT_0 then
    begin
      if not GetExitCodeProcess(ProcessInfo.hProcess, Result) then
        Result := Cardinal($FFFFFFFF);
    end;
    CloseHandle(ProcessInfo.hThread);
    CloseHandle(ProcessInfo.hProcess);
  end;
end;

// ------------------------- TWordFld2LocFileList ------------------------------

constructor TWordFld2LocFileList.Create;
begin
  flistn:= util1.CreateTemporaryFileName('t0');
  FList:= TFileStream.Create(flistn, fmCreate);
  inherited Create;
end;

destructor TWordFld2LocFileList.Destroy;
begin
  FList.Free;
  if FileExists(flistn)
    then DeleteFile(flistn);
end;

procedure TWordFld2LocFileList.ExternalSort;
var
  i: Cardinal;
  wl: TWordFld2LocItem;
  // oldwl: TWordFld2LocItem;
  tmpfn, tmpfn1: String;
  tmp: Text;
  pars, s: String;
begin
  tmpfn:= util1.CreateTemporaryFileName('in');
  tmpfn1:= util1.CreateTemporaryFileName('out');
  AssignFile(tmp, tmpfn);
  Rewrite(tmp);
  FList.Position:= 0;
  for i:= 0 to FCount - 1 do begin
    FList.ReadBuffer(wl, SizeOf(TWordFld2LocItem));
    with wl do Writeln(tmp, Format('%8.8x%4.4x%8.8x', [Wrd, Fld, Loc]));
  end;
  Flush(tmp);
  CloseFile(tmp);
  FList.Free;

  pars:= tmpfn + ' /o ' + tmpfn1;

  if WinExec32AndWait('sort ' + pars, SW_NORMAL) <> 0 then begin
    raise Exception.Create('Error: external SORT.');
  end;
  DeleteFile(tmpfn);

  Flist:= TFileStream.Create(Flistn, fmOpenReadWrite);

  AssignFile(tmp, tmpfn1);
  Reset(tmp);
  {
  with oldwl do begin
    Wrd:= High(Wrd);
    Fld:= High(Fld);
    Loc:= High(Loc);
  end;
  }
  for i:= 0 to FCount - 1 do begin
    with wl do begin
      Readln(tmp, s);
      Wrd:= StrToInt('$' + Copy(s, 1, 8));
      Fld:= StrToInt('$' + Copy(s, 9, 4));
      Loc:= StrToInt('$' + Copy(s, 13, 8));
    end;
    {
    if (oldwl.Wrd <> wl.Wrd) or (oldwl.Fld <> wl.Fld) or (oldwl.Loc <> wl.Loc) then begin
      FList.WriteBuffer(wl, SizeOf(TWordFld2LocItem));
      oldwl:= wl;
    end;
    }
    FList.WriteBuffer(wl, SizeOf(TWordFld2LocItem));
  end;

  CloseFile(tmp);
  DeleteFile(tmpfn1);
end;

procedure TWordFld2LocFileList.MergeSort;
type
  tmots = array of TWordFld2LocItem;
var
  ff1, ff2, fg1, fg2: TStream;
  ff1n, ff2n, fg1n, fg2n: ShortString;
  fini, switch: Boolean;
  maxmot, i, j, k: Cardinal;
  bs: Cardinal;
  item: tmots;
  k1: Integer;

  function CalcBufferSize: Integer;
  var
    MemoryStatus: TMemoryStatus;
  begin
    FillChar(MemoryStatus, SizeOf(MemoryStatus), 0);
    MemoryStatus.dwLength:= SizeOf(MemoryStatus);
    GlobalMemoryStatus(MemoryStatus);
    Result:= MemoryStatus.dwAvailPhys * 8 div (10 * SizeOf(TWordFld2LocItem));
    if Result > FCount
    then Result:= FCount + 1;
    Result:= 1024 * 1024;
  end;

  procedure merge(lek: Cardinal; var f1, f2, g1, g2: TStream);
  var
    outswitch: Boolean;
    winner: Cardinal;
    used: array[1..2] of Cardinal;
    fin: array[1..2] of Boolean;
    current: array[1..2] of TWordFld2LocItem;
    numg1, numg2: Cardinal;

    procedure getrecord(i: integer);
    begin
      if (used[i] = lek) or ((i = 1) and (f1.Position >= f1.Size)) or
        ((i = 2) and (f2.Position >= f2.Size))
        then fin[i]:= True else begin
        Inc(used[i]);
        if i = 1
        then f1.ReadBuffer(current[1], SizeOf(TWordFld2LocItem))
        else f2.ReadBuffer(current[2], SizeOf(TWordFld2LocItem));
      end;
    end;

  begin
    outswitch:= true;
    with TFileStream(g1) do begin
      Position:= 0;
      Windows.SetEndOfFile(Handle);
    end;
    with TFileStream(g2) do begin
      Position:= 0;
      Windows.SetEndOfFile(Handle);
    end;
    f1.Seek(soFromBeginning, 0);
    f2.Seek(soFromBeginning, 0);
    numg1:= 0;
    numg2:= 0;
    while (f1.Position < f1.Size) or (f2.Position < f2.Size) do begin
      FillChar(used, SizeOf(used), 0);
      FillChar(fin, SizeOf(fin), false);
      FillChar(current, SizeOf(current), 0);
      getrecord(1);
      getrecord(2);
      while not fin[1] or not fin[2] do begin
        if fin[1] then winner:= 2
        else if fin[2] then winner:= 1
        else if CompareItems(current[1], current[2]) < 0 then winner:= 1
        else winner:= 2;
        if outswitch then begin
          g1.WriteBuffer(current[winner], SizeOf(TWordFld2LocItem));
          Inc(numg1);
        end
        else begin
          g2.WriteBuffer(current[winner], SizeOf(TWordFld2LocItem));
          Inc(numg2);
        end;
        getrecord(winner);
      end;
      outswitch:= not outswitch;
    end;
    fini:= numg2 = 0;
  end;

  procedure inSort(item: tmots; last: Cardinal);
  var
    i, j: Cardinal;
    span: integer;
  begin
    span:= last shr 1;
    while span > 0 do begin
      for i:= span to last - 1 do begin
        for j:= (i - span + 1) downto 1 do
          if CompareItems(item[j], item[j + span]) <= 0 then Break
          else begin
            item[0]:= item[j];
            item[j]:= item[j + span];
            item[j + span]:= item[0];
          end;
      end;
      span:= span shr 1;
    end;
  end;

begin {main}
  // init;
  ff1n:= util1.CreateTemporaryFileName('f1');
  ff2n:= util1.CreateTemporaryFileName('f2');
  fg1n:= util1.CreateTemporaryFileName('g1');
  fg2n:= util1.CreateTemporaryFileName('g2');

  ff1:= TFileStream.Create(ff1n, fmCreate);
  ff2:= TFileStream.Create(ff2n, fmCreate);
  fg1:= TFileStream.Create(fg1n, fmCreate);
  fg2:= TFileStream.Create(fg2n, fmCreate);

  maxmot:= 0;
  fini:= false;

  // passe1;
  FList.Seek(soFromBeginning, 0);
  // GetMem(item, (k1 + 1) * SizeOf(TWordFld2LocItem));
  // calculate size
  k1:= CalcBufferSize;
  SetLength(item, k1 + 1);
  FillChar(item[0], (k1 + 1) * SizeOf(TWordFld2LocItem), 0);
  switch:= true;
  while (FList.Position < FList.Size) do begin
    // get count of records to read
    bs:= (FList.Size - FList.Position) div SizeOf(TWordFld2LocItem);
    if bs > k1
    then bs:= k1;
    j:= bs;
    bs:= bs * SizeOf(TWordFld2LocItem);
    FList.ReadBuffer(item[i], bs);

    Inc(maxmot, j);
    insort(item, j);

    if switch
    then ff1.WriteBuffer(item[i], SizeOf(TWordFld2LocItem) * j)
    else ff2.WriteBuffer(item[i], SizeOf(TWordFld2LocItem) * j);

    switch:= not switch;
  end;

  SetLength(item, 0);

  // externe
  i:= 1;
  k:= k1;
  switch:= true;
  while not fini {maxmot > k} do begin
    inc(i);
    if switch
      then merge(k, ff1, ff2, fg1, fg2)
    else merge(k, fg1, fg2, ff1, ff2);
    switch:= not switch;
    k:= k * 2;
  end;

  ff2.Free;
  DeleteFile(ff2n);
  fg2.Free;
  DeleteFile(fg2n);
  ff1.Free;
  fg1.Free;

  // swap temp files
  FList.Free;
  if FileExists(flistn)
    then DeleteFile(flistn);

  if switch then begin
    // FList.CopyFrom(ff1, FCount * SizeOf(TWordFld2LocItem));
    RenameFile(ff1n, flistn);
    DeleteFile(fg1n);
  end else begin
    // FList.CopyFrom(fg1, FCount * SizeOf(TWordFld2LocItem));
    RenameFile(fg1n, flistn);
    DeleteFile(ff1n);
  end;
  FList:= TFileStream.Create(flistn, fmOpenReadWrite);

  // Windows.SetEndOfFile(TFileStream(FList).Handle); // just in case
end;

procedure TWordFld2LocFileList.QuickSort(L, R: Integer);
var
  I, J, P: Integer;
  IItem, JItem, PItem: TWordFld2LocItem;
  tmpptr: Cardinal;
begin
  repeat
    I:= L;
    J:= R;
    P:= (L + R) shr 1;
    repeat
      with FList do begin
        Position:= FSortPtr[I] * SizeOf(TWordFld2LocItem);
        ReadBuffer(IItem, SizeOf(TWordFld2LocItem));
        Position:= FSortPtr[J] * SizeOf(TWordFld2LocItem);
        ReadBuffer(JItem, SizeOf(TWordFld2LocItem));
        Position:= FSortPtr[P] * SizeOf(TWordFld2LocItem);
        ReadBuffer(PItem, SizeOf(TWordFld2LocItem));
      end;

      while CompareItems(IItem, PItem) < 0 do begin
        Inc(I);
        with FList do begin
          Position:= FSortPtr[I] * SizeOf(TWordFld2LocItem);
          ReadBuffer(IItem, SizeOf(TWordFld2LocItem));
        end;
      end;
      while CompareItems(JItem, PItem) > 0 do begin
        Dec(J);
        with FList do begin
          Position:= FSortPtr[J] * SizeOf(TWordFld2LocItem);
          ReadBuffer(JItem, SizeOf(TWordFld2LocItem));
        end;
      end;

      if I <= J then begin
//      ExchangeItems(I, J); faster a litte
        tmpptr:= FSortPtr[J];
        FSortPtr[J]:= FSortPtr[I];
        FSortPtr[I]:= tmpptr;
//      exchange
        if P = I then
          P:= J
        else if P = J then
          P:= I;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then QuickSort(L, J);
    L:= I;
  until I >= R;
end;

function TWordFld2LocFileList.Get(Index: Integer): TWordFld2LocItem;
begin
{$IFDEF RANGE_CHECK}
  if (Index < 0) or (Index >= FCount) then Error(@SListIndexError, Index);
{$ENDIF}
  FList.Position:= Index * SizeOf(TWordFld2LocItem);
  FList.Read(Result, SizeOf(TWordFld2LocItem));
end;

procedure TWordFld2LocFileList.Put(Index: Integer; AItem: TWordFld2LocItem);
begin
{$IFDEF RANGE_CHECK}
  if (Index < 0) or (Index >= FCount) then Error(@SListIndexError, Index);
{$ENDIF}
  FList.Position:= Index * SizeOf(TWordFld2LocItem);
  FList.Write(AItem, SizeOf(TWordFld2LocItem));
end;

procedure TWordFld2LocFileList.SetCapacity(NewCapacity: Integer);
var
  p: Cardinal;
begin
  inherited SetCapacity(NewCapacity);
  p:= FCapacity * SizeOf(TWordFld2LocItem);
  if FList.Size < p then begin
    // what for? is it faster?
    FList.Position:= p;
    FList.Write(p, 4); // for speed I mean
  end;
end;

procedure TWordFld2LocFileList.DeleteRange(AFirstIdx, ALastIdx: Integer);
var
  c: Integer;
  cnt, bufsize, n, p1, p2: Cardinal;
begin
{$IFDEF RANGE_CHECK}
  if (AFirstIdx < 0) or (ALastIdx >= FCount) or (ALastIdx < AFirstIdx)
    then Error(@SListIndexError, AFirstIdx);
{$ENDIF}
  c:= ALastIdx - AFirstIdx + 1;
  FCount:= FCount - c;
  cnt:= (FCount - AFirstIdx) * SizeOf(TWordFld2LocItem);

  p1:= (ALastIdx + 1) * SizeOf(TWordFld2LocItem); // from
  p2:= AFirstIdx * SizeOf(TWordFld2LocItem);
  if cnt > MAXFILEBUFSIZE
    then bufsize:= MAXFILEBUFSIZE
  else bufsize:= cnt;
  while Cnt > 0 do begin
    if Cnt > BufSize
      then N:= BufSize
    else N:= Count;
    with FList do begin
      Position:= p1;
      ReadBuffer(FFileBuffer, N);
      Position:= p2;
      WriteBuffer(FFileBuffer, N);
    end;
    Inc(p1, N);
    Inc(p2, N);
    Dec(cnt, N);
  end;
end;

function TWordFld2LocFileList.Find(AItem: TWordFld2LocItem; var Index: Integer): Boolean;
var
  L, H, I, C: Integer;
  CItem: TWordFld2LocItem;
begin
  Result:= False;
  L:= 0;
  H:= FCount - 1;
  while L <= H do begin
    I:= (L + H) shr 1;
    with FList do begin
      Position:= I * SizeOf(TWordFld2LocItem);
      Read(CItem, SizeOf(TWordFld2LocItem));
    end;
    C:= CompareItems(CItem, AItem);
    if C < 0 then L:= I + 1 else begin
      H:= I - 1;
      if C = 0 then begin
        Result:= True;
        L:= I; // if Duplicates <> dupAccept then
      end;
    end;
  end;
  Index:= L;
end;

procedure TWordFld2LocFileList.DirectAddItem(const AItem: TWordFld2LocItem);
begin
  if FCount = FCapacity
    then Grow;
  FList.Position:= FCount * SizeOf(TWordFld2LocItem);
  FList.Write(AItem, SizeOf(TWordFld2LocItem));
  Inc(FCount);
end;

{
procedure TWordFld2LocFileList.Sort;
var
  i: Cardinal;
  CItem: TWordFld2LocItem;
  FTmpList: TStream;

begin
  FTmpList:= TFileStream.Create('!tmplist.tmp', fmCreate);
  SetLength(FSortPtr, FCount);
  for i:= 0 to FCount - 1 do begin
    FSortPtr[i]:= i;
  end;
  if (FCount > 1) then begin
    QuickSort(0, FCount - 1);
  end;

  FList.Position:= 0;
  for i:= 0 to FCount - 1 do begin
    FList.ReadBuffer(CItem, SizeOf(TWordFld2LocItem));
    with FTmpList do begin
      Position:= FSortPtr[i] * SizeOf(TWordFld2LocItem);
      WriteBuffer(CItem, SizeOf(TWordFld2LocItem));
    end;
  end;

  // swap temp files
  FTmpList.Free;
  FList.Free;
  FList:= TFileStream.Create('!tmplist.tmp', fmOpenReadWrite);
  FTmpList:= TFileStream.Create('!list.tmp', fmCreate); // actually is not required

  SetLength(FSortPtr, 0);
  FTmpList.Free;
  if FileExists('!tmplist.tmp')
  then DeleteFile('!tmplist.tmp');
end;
}

procedure TWordFld2LocFileList.Sort;
var
  i: Cardinal;
  CItem: TWordFld2LocItem;
begin
  if (FCount > 1) then begin
    // anti-Grow
    FList.Position:= FCount * SizeOf(TWordFld2LocItem);
    Windows.SetEndOfFile(TFileStream(FList).Handle);
    ExternalSort;
    {
    MergeSort;
    }
  end;
end;

function TWordFld2LocFileList.RmDuplicates: Cardinal;
var
  i: Integer;
  it, cur: TWordFld2LocItem;
  tmpfn: string;
  tmpstrm: TFileStream;
begin
  Result:= 0;
  tmpfn:= '!duplicates.tmp';
  tmpstrm:= TFileStream.Create(tmpfn, fmCreate);
  with it do begin
    Wrd:= High(Wrd);
{$IFDEF XML_IDX}
    Fld:= High(TWordFld2LocItemFld);
{$ELSE}
    Fld:= '#@!';
{$ENDIF}
    Loc:= High(Loc);
  end;
  i:= 0;
  while i < FCount do begin
    with FList do begin
      Position:= I * SizeOf(TWordFld2LocItem);
      ReadBuffer(cur, SizeOf(TWordFld2LocItem));
    end;
    if (cur.Wrd = it.Wrd) and (cur.Fld = it.Fld) and (cur.Loc = it.Loc) then begin
      Result:= Result + 1;
      // Delete(i);
    end else begin
      // if (cur.Wrd < it.Wrd) then raise Exception.CreateFmt('Not sorted yet', []);
      tmpstrm.Write(cur, SizeOf(TWordFld2LocItem));
      with FList do begin
        Position:= I * SizeOf(TWordFld2LocItem);
        ReadBuffer(it, SizeOf(TWordFld2LocItem));
      end;
    end;
    Inc(i);
  end;
  // free space
  // FCount:= tmpstrm.Size div SizeOf(TWordFld2LocItem);
  FCount:= FCount - Result;
  SetCapacity(FCount);

  // read compacted data
  tmpstrm.Position:= 0;
  FList.Position:= 0;
  FList.CopyFrom(tmpstrm, FCount * SizeOf(TWordFld2LocItem));

  Windows.SetEndOfFile(TFileStream(FList).Handle);
  // free up
  tmpstrm.Free;
  DeleteFile(tmpfn);
end;

// ---------------------------- THashWordList ----------------------------------

const
  DEF_INIT_Hash_Size = 2 shl 16 - 1; // power of 2 minus 1

constructor THashWordList.Create;
begin
  FStringHashMapTraits:= TCaseSensitiveTraits.Create; // is it faster than insesitive?
  FStringList:= nil;
  FForceRecreateStringList:= False;
  FFldOrder:= 0;
  inherited Create(FStringHashMapTraits, DEF_INIT_Hash_Size);
end;

destructor THashWordList.Destroy;
begin
  FStringHashMapTraits.Free;
  inherited Destroy;
end;

function THashWordList.GetWordOccurances(AIndex: Integer): Cardinal;
begin
  Result:= Cardinal(FStringList.Objects[AIndex]);
end;

function THashWordList.GetWordUp(AIndex: Integer): string;
begin
  Result:= FStringList[AIndex];
end;

procedure THashWordList.SetWordOccurances(AIndex: Integer; AValue: Cardinal);
begin
  FStringList.Objects[AIndex]:= Pointer(AValue);
end;

procedure THashWordList.AddNewWord(const ASrch: string);
var
  ppn: PPHashNode;
begin
  ppn:= FindNode(ASrch);
  { if reordered from SetData because ppn^ = nil is more common for Add }
  if ppn^ = nil then begin
    { add }
    ppn^:= AllocNode;
    { we increment after in case of exception }
    Inc(FCount);
    ppn^^.Str:= ASrch;
    ppn^^.Ptr:= Pointer(1);
  end else begin
    Inc(Cardinal(ppn^^.Ptr));
  end;
end;

procedure THashWordList.AddNewWordOrd(const ASrch: string);  //     TWordFld2LocItemFld
var
  ppn: PPHashNode;
begin
  ppn:= FindNode(ASrch);
  { if reordered from SetData because ppn^ = nil is more common for Add }
  if ppn^ = nil then begin
    { add }
    ppn^:= AllocNode;
    { we increment after in case of exception }
    Inc(FCount);
    ppn^^.Str:= ASrch;
    ppn^^.Ptr:= Pointer(FFldOrder);
    Inc(FFldOrder);
  end else begin
    // Inc(Cardinal(ppn^^.Ptr));
  end;
end;

procedure THashWordList.AddNewWord32(const ASrch: string; AMask: Cardinal);
var
  ppn: PPHashNode;
begin
  ppn:= FindNode(ASrch);
  { if reordered from SetData because ppn^ = nil is more common for Add }
  if ppn^ = nil then begin
    { add }
    ppn^:= AllocNode;
    { we increment after in case of exception }
    Inc(FCount);
    ppn^^.Str:= ASrch;
    ppn^^.Ptr:= Pointer(AMask);
  end else begin
    Cardinal(ppn^^.Ptr):= Cardinal(ppn^^.Ptr) or AMask;
  end;
end;

// Add new entry in hash array and allocate TWordBEs structure indicates in which input
// index words are occured; to minimize memory usage intensivity TWordBEs contains
// msk bit field used as mask in procedure of looking in which index words occured
procedure THashWordList.AddNewWordMax(const AWr: TWordRec; AOrder, AMax: Byte);
var
  ppn: PPHashNode;
  p: Pointer;
begin
  ppn:= FindNode(AWr.w);
  { if reordered from SetData because ppn^ = nil is more common for Add }
  if ppn^ = nil then begin
    { add }
    ppn^:= AllocNode;
    { we increment after in case of exception }
    Inc(FCount);
    ppn^^.Str:= AWr.w;
    // reason is memory economy - allocate TWHdrRec less then 32 elements in array; but AMax can NOT be greater than 32
    // because TWordBEs.msk is 32 bit long points to 32 array elements as bit mask
    // each TWHdrRec contains 2 4-byte long pointers to record (usually file offset in LDB or record number in file list)
    // GetMem(p, SizeOf(TWordBEs.l) +  SizeOf(TWordBEs.msk) + AMax * SizeOf(TWHdrRec));
    GetMem(p, 1 +  4 + AMax * SizeOf(TWHdrRec));
    with TWordBEs(p^) do begin
      msk:= (1 shl AOrder);
      l:= 1;
      a[0]:= AWr.h;
    end;
    ppn^^.Ptr:= p;
  end else begin
    with TWordBEs(ppn^^.Ptr^) do begin
      msk:= msk or (1 shl AOrder);
      a[l]:= AWr.h;
      Inc(l);
    end;
  end;
end;

procedure THashWordList.LoadFromStream(AStream: TStream);
var
  len: Integer;
  s: string;
  cnt: Cardinal;
begin
  while AStream.Position < AStream.Size do begin
    AStream.Read(len, 4);
    SetLength(s, len);
    AStream.Read(s[1], len);
    AStream.Read(cnt, 4);
    AddNewWord(s);
  end;
end;

procedure THashWordList.LoadFromWrdStream(AStream: TStream);
var
  cnt: Cardinal;
  wr: TWordRec;
begin
  cnt:= 0;
  with AStream do while (Position < Size) do begin
    ReadBuffer(wr, 4 + 4 + 1);
    ReadBuffer(wr.w[1], Byte(wr.w[0]));
    AddNewWord(wr.w);
    Inc(cnt);
  end;
end;

procedure THashWordList.LoadFromWrdStream32(AStream: TStream; AOrder: Byte);
var
  cnt: Cardinal;
  wr: TWordRec;
  msk: Cardinal;
begin
  if AOrder >= 32
  then raise Exception.Create('Too many word streams');
  cnt:= 0;
  msk:= 1 shl AOrder;
  with AStream do while (Position < Size) do begin
    ReadBuffer(wr, 4 + 4 + 1);
    ReadBuffer(wr.w[1], Byte(wr.w[0]));
    AddNewWord32(wr.w, msk);
    Inc(cnt);
  end;
end;

// load words into hash array
// Note: you must load no more than 32 input index using this function
//   one by one (AOrder=0,1..N) where N < 32
// Parameters:
//   AStream - opened '.wrd' file
//   AOrder - number of added .wrd, 0..31 (you must remember relation between this number and .wrd somewhere)
//   AMax - count of .wrd indexes you plan to merge in one hash array
procedure THashWordList.LoadFromWrdStreamMax(AStream: TStream; AOrder, AMax: Byte);
var
  cnt: Cardinal;
  wr: TWordRec;
begin
  // TWordBEs contains msk bit fields to indicate in which input indexes words occured
  // it is limited by 32 bit Cardinal
  if AOrder >= 32
  then raise Exception.Create('Too many word streams');
  cnt:= 0;
  with AStream do while (Position < Size) do begin
    ReadBuffer(wr, 4 + 4 + 1);
    ReadBuffer(wr.w[1], Byte(wr.w[0]));

    // Add new entry in hash array and allocate TWordBEs structure indicates in which input
    // index words are occured; to minimize memory usage intensivity TWordBEs contains
    // msk bit field used as mask in procedure of looking in which index words occured
    AddNewWordMax(wr, AOrder, AMax);
    Inc(cnt);
  end;
end;

procedure THashWordList.LoadFromFile(const AFileName: string);
var
  f: TStream;
begin
  f:= TFileStream.Create(AFileName, fmOpenRead);
  try
    LoadFromStream(f);
  finally
    f.Free;
  end;
end;

function THashWordList.IterateCreateStringList(AUserData: Pointer; const AStr: string; var APtr: Pointer): Boolean;
begin
  TStrings(AUserData).AddObject(AStr, APtr);
  Result:= True;
end;

function THashWordList.IterateStoreHash(AUserData: Pointer; const AStr: string; var APtr: Pointer): Boolean;
var
  len: Integer;
begin
  len:= Length(AStr);
  TStream(AUserData).Write(len, 4);
  TStream(AUserData).Write(AStr[1], len);
  TStream(AUserData).Write(APtr, SizeOf(Cardinal));
  Result:= True;
end;

function THashWordList.CheckCreateStringList: Boolean;
var
  i: Integer;
  cnt: Integer;
begin
  Result:= True;
  if Assigned(FStringList) then begin
    if FForceRecreateStringList
      then FStringList.Free
    else Exit;
  end;
  FStringList:= TStringList.Create;

  IterateMethod(FStringList, IterateCreateStringList);

  with TStringList(FStringList) do begin
    Duplicates:= dupIgnore;
    Sorted:= True;
  end;

  // assign order number to hash pointer
  cnt:= FStringList.Count - 1;
  for i:= 0 to cnt do begin
    Self.Data[FStringList[i]]:= Pointer(i);
  end;
end;

procedure THashWordList.CheckSortList;
begin
  if CheckCreateStringList then begin
  end;
end;

procedure THashWordList.Prepare2StoreIndex;
begin
  if CheckCreateStringList then begin
  end;
end;

procedure THashWordList.SaveToStream(const AStream: TStream);
begin
  IterateMethod(AStream, IterateStoreHash);
end;

procedure THashWordList.SaveToFile(const AFileName: string);
var
  f: TStream;
begin
  f:= TFileStream.Create(AFileName, fmCreate);
  try
    SaveToStream(f);
  finally
    f.Free;
  end;
end;

constructor TStringFldHash.Create;
begin
  FStringHashMapTraits:= TCaseInsensitiveTraits.Create;
  inherited Create(FStringHashMapTraits, 1024);
end;

destructor TStringFldHash.Destroy;
begin
  Self.Iterate(nil, Iterate_FreeMem); // remove all 'A'..'Z'
  inherited Destroy;
  FStringHashMapTraits.Free;
end;

{--------------------------------------------------------------}

constructor TWordList.Create;
begin
  inherited Create;
  Duplicates:= dupIgnore;
  Sorted:= True;
end;

procedure TWordList.InsertObject(Index: Integer; const S: string;
  AObject: TObject);
begin
  if (Index < 0) or (Index > Count)
    then Error(@SListIndexError, Index);
  InsertItem(Index, S, AObject);
end;

function TWordList.GetWordOccurances(AIndex: Integer): Cardinal;
begin
  Result:= Cardinal(Objects[AIndex]);
end;

procedure TWordList.SetWordOccurances(AIndex: Integer; AValue: Cardinal);
begin
  Objects[AIndex]:= Pointer(AValue);
end;

procedure TWordList.CheckSortList;
begin
end;

procedure TWordList.Prepare2StoreIndex;
begin
end;

procedure TWordList.AddNewWord(const ASrch: string);
var
  cnt: Integer;
  idx: Integer;
begin
  if Find(ASrch, idx) then begin
    Objects[idx]:= Pointer(Cardinal(Objects[idx]) + 1);
  end else begin
    Insert(idx, ASrch);
    Objects[idx]:= Pointer(1);
  end;
end;

function TWordList.GetWordUp(AIndex: Integer): string;
begin
  Result:= Get(AIndex);
end;

{--------------------------------------------------------------}

constructor TWideWordList.Create;
begin
  inherited Create;
  Duplicates:= dupIgnore;
  Sorted:= True;
end;

procedure TWideWordList.Insert(Index: Integer; const S: WideString);
begin
  if (Index < 0) or (Index > Count)
    then Error(SListIndexError, Index);
  InsertItem(Index, S);
end;

function TWideWordList.GetWordOccurances(AIndex: Integer): Cardinal;
begin
  Result:= Cardinal(Objects[AIndex]);
end;

procedure TWideWordList.SetWordOccurances(AIndex: Integer; AValue: Cardinal);
begin
  Objects[AIndex]:= Pointer(AValue);
end;

procedure TWideWordList.AddNewWord(const ASrch: WideString);
var
  cnt: Integer;
  idx: Integer;
begin
  if Find(ASrch, idx) then begin
    Objects[idx]:= Pointer(Cardinal(Objects[idx]) + 1);
  end else begin
    Insert(idx, ASrch);
    Objects[idx]:= Pointer(1);
  end;
end;

procedure TWideWordList.CheckSortList;
begin
end;

procedure TWideWordList.Prepare2StoreIndex;
begin
end;

function TWideWordList.GetWordUp(AIndex: Integer): string;
begin
  Result:= Get(AIndex);
end;

procedure StoreOfsList(AStrings: TStrings; AStream: TStream);
var
  strm: TStream; // never used
  recno: Cardinal; // never used
  c, cnt, dataofs, len, curpos: Cardinal;
  key: ShortString;
begin
  cnt:= AStrings.Count;
  // 1. count of keys
  AStream.Write(cnt, SizeOf(Cardinal));
  curpos:= (cnt + 1) * SizeOf(Cardinal);

  c:= 0;
  while (c < cnt) do begin
    try
      // get key
      key:= AStrings[c];

      // 2. offset table
      AStream.Position:= (c + 1) * SizeOf(Cardinal);
      AStream.Write(curpos, SizeOf(Cardinal));

      AStream.Position:= curpos;
      len:= Length(Key) + 1; // + length descriptor [0]
      // 3. len of key
      // 4. key
      AStream.Write(key[0], len);

      Inc(curpos, len);
    finally
      Inc(c);
    end;
  end;
end;

// ----------------------------- TCustomProgressThread -------------------------

constructor TCustomProgressThread.Create(ASuspended: Boolean; AReportProc: TReportProc);
begin
  Progress:= 0;
  ProgressCount:= 1;
  ProgressState:= '';
  FReportProc:= AReportProc;
  inherited Create(ASuspended);
end;

procedure TCustomProgressThread.Report(AProgress: Cardinal; AProgressCount: Cardinal; const AProgressStatus: String);
begin
  Progress:= AProgress;
  ProgressCount:= AProgressCount;
  ProgressState:= AProgressStatus;
  if Assigned(FReportProc)
  then Synchronize(FReportProc);
end;

end.

