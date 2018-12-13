unit
  cpcoll;
(*##*)
(*******************************************************************
*                                                                  *
*   C  P  C  O  L  L       Code page clat collection object       *
*                          Part of EPager                          *
*   Copyright (c) 1998, Andrey Ivanov. All rights reserved.       *
*   Delphi 2, 3 only (uses huge strings)                           *
*   Conditional defines:                                          *
*                                                                  *
*   Functions keeps xlat tables in one library file               *
*   Based on part of tap unit, rev. Apr 21 1998                    *
*   Based on common EPAGER transliteration PageXlat unit.         *
*   Conditional defines:                                           *
*                                                                 *
*   Last Revision: Apr 19 1998                                     *
*   Last fix     : Apr 21 1998                                    *
*   Moved 4 routines from cp2ecc: InitCPName2Ecc DoneCPName2Ecc    *
*   StoreCPName2EccToRegistry CPName2Ecc                          *
*   Lines        :                                                 *
*   History      : see CHANGES.TXT file                           *
*   Printed      : ---                                             *
*                                                                 *
********************************************************************)
(*##*)

interface
{$IFDEF VER70}
uses
  Objects, strutil;
{$ELSE}
uses
  Classes, SysUtils, IniFiles;
{$ENDIF}
const
  { flags constants 0..15 }
  pgt_first  = 0;
  pgt_used   = 0;
  pgt_num    = 1;
  pgt_latU   = 2;
  pgt_latl   = 3;
  pgt_cyrU   = 4;
  pgt_cyrl   = 5;
  pgt_hassort= 6;  { reserved }
  pgt_last   = 6;

type
  TUSESet  = set of pgt_first..15;
  TInUseSet= set of Byte;
  TXLAT    = array [0..255] of Byte;
  PCPDefinition = ^TCPDefinition;
  TCPDefinition = record
    Name: String[31]; { 32  }
    Flags: TUSESet;   { 2   }
    BaseCP: Word;     { 2   }
    CP: Word;         { 2   }
    InUse: TInUseSet; { 32  }
    XLAT: TXLAT;      { 256 }
    DWordAlign: Word; { 2   } { 328 }
  end;

  PCPDefinitions = ^TCPDefinitions;
  TCPDefinitions = array [0..1] of TCPDefinition;

{$IFDEF VER70}
{$ELSE}
  { code page editor class
    Codes,Checked[no, x, y] properties give access to codepage NO
      x, y- coords in 2D table (similar to windows charmap utility)
      but firstly set GridX property (*32)
    See example project Exlat1.dpr
  }
  TCodePageEditor = class (TObject)
  private
    FDefinitionsMem: Integer;
    FDefinitions: PCPDefinitions;
    FSetCount: SmallInt;
    FFileName: String;
    function GetGridCodes(no, x, y: Integer): Integer;
    procedure SetGridCodes(no, x, y: Integer; NewValue: Integer);
    function GetGridChecked(no, x, y: Integer): Boolean;
    procedure SetGridChecked(no, x, y: Integer; Check: Boolean);
    function GetDefinition(No: Integer): PCPDefinition;
    function GetTextDescription(No: Integer): String;
  public
    GridX: Integer;
    constructor Create;
    destructor  Destroy; override;
    procedure Reset(no: Integer);
    procedure Add;
    procedure Delete(no: Integer);

    function LoadFromFile(fn: String): Boolean;
    { NewStyle 0  store all code page in EPC format
              1 store one codepage (NewStyle means number) in extended ECC format
              2 store one codepage (NewStyle means number) in ECC format}
    function StoreToFile(fn: String; cp, NewStyle: Integer): Boolean;

    property Count: SmallInt read FSetCount;
    property Codes[no, x, y: Integer]: Integer read GetGridCodes write SetGridCodes;
    property Checked[no, x, y: Integer]: Boolean read GetGridChecked write SetGridChecked;
    property Definitions[no: Integer]: PCPDefinition read GetDefinition;
    property TextDescription[no: Integer]: String read GetTextDescription;
  end;
{$ENDIF}
{ LoadECC
  Parameters:
    LoadSelectedOnly - load only if flag pgt_used
  return:
    -1 if failed
     0 if file exists but no definitions stored
     1.. - qty loaded defs
     Defs- pointer to allocated array
}
{$IFDEF VER70}
function LoadECC(const FN: String; LoadALLDefs: Boolean;
  var Defs: PCPDefinitions): Integer;

{ get number of xlat table by name, -1 if fails }
function GetXlatNoByName(Defs: PCPDefinitions; Count: Integer; AName: String): Integer;

{$ELSE}
function LoadECC(const FN: String; LoadALLDefs: Boolean;
  var Defs: PCPDefinitions; ANames: TStrings): Integer;
{$ENDIF}

{ simple routines to simplify access from DOS application }

{ extract xlat table from definition NO from DEFS collection }
procedure FillXlatFromDefs(no: Integer; Defs: PCPDefinitions; var Dest);
function  GetXlatFromDefs(no: Integer; Defs: PCPDefinitions): Pointer;
{ return "," delimited string }
function  GetDefNames(Defs: PCPDefinitions; Count: Integer): String;

{ create inverse xlat }
procedure XlatInverse(orig256, invr256: Pointer);

{ initialize code page alias routines
  AIni = '' : from registry
  AIni > '' : from .INI file }
{$IFNDEF VER70}
function InitCPName2Ecc(AIni, xlatFn: String): Boolean;
procedure DoneCPName2Ecc;
{ store changes to registry }
procedure StoreCPName2EccToRegistry;
{ return pointer to xlat table in xlat collection. Nil if fails }
function CPName2Ecc(ACPName: String): Pointer;
{ convert String to CP (by name or by nymber (1..)) }
procedure xlatByName(AxlatName: String; ADefs: PCPDefinitions; ACPNames: TStrings; var ABuf: String);
{$ENDIF}

implementation

{$IFDEF VER70}
{$ELSE}
constructor TCodePageEditor.Create;
begin
  inherited Create;
  GridX:= 32;
  FSetCount:= 1;
  FDefinitionsMem:= FSetCount * SizeOf(TCPDefinition);
  FDefinitions:= AllocMem(FDefinitionsMem);
end;

destructor  TCodePageEditor.Destroy;
begin
  if FDefinitionsMem > 0
  then FreeMem(FDefinitions, FDefinitionsMem);
  inherited Destroy;
end;

procedure TCodePageEditor.Reset(no: Integer);
var
  i: Word;
begin
  if (FDefinitions = Nil) or (no > FSetCount)
  then Exit;
  FDefinitions^[no].Name:= 'Ќовый тип';
  for i:= 0 to 255
  do FDefinitions^[no].XLAT[i] := i;
  FDefinitions^[no].InUse:= [0..255];
  FDefinitions^[no].Flags:= [pgt_used, pgt_num];
  FDefinitions^[no].CP:= 0;
end;

function TCodePageEditor.GetGridCodes(no, x, y: Integer): Integer;
begin
  if (FDefinitions = Nil) or (no > FSetCount)
  then  GetGridCodes:= -1
  else GetGridCodes:= FDefinitions^[no].xlat[GridX * y + x];
end;

procedure TCodePageEditor.SetGridCodes(no, x, y: Integer; NewValue: Integer);
begin
  if (FDefinitions = Nil) or (no > FSetCount) or (NewValue < 0) or (NewValue > 255)
  then Exit;
  FDefinitions^[no].xlat[GridX * y + x]:= NewValue;
end;

{ if no, x or y is wrong, False returned }
function TCodePageEditor.GetGridChecked(no, x, y: Integer): Boolean;
begin
  if (FDefinitions = Nil) or (no > FSetCount)
  then GetGridChecked:= False
  else GetGridChecked:= (GridX * y + x) in FDefinitions^[no].InUse;
end;

procedure TCodePageEditor.SetGridChecked(no, x, y: Integer; Check: Boolean);
begin
  if (FDefinitions = Nil) or (no > FSetCount)
  then Exit;
  if Check
  then FDefinitions^[no].InUse:= FDefinitions^[no].InUse + [Byte(GridX * y + x)]
  else FDefinitions^[no].InUse:= FDefinitions^[no].InUse - [Byte(GridX * y + x)];
end;

function TCodePageEditor.GetDefinition(No: Integer): PCPDefinition;
begin
  if No >= FSetCount
  then GetDefinition:= Nil
  else GetDefinition:= @(FDefinitions^[No]);
end;

function TCodePageEditor.StoreToFile(fn: String; cp, NewStyle: Integer): Boolean;
var
  f: TFileStream;
  i: Word;
  Signature: array[0..1] of Char;
begin
  StoreToFile:= False;
  try
    f:= TFileStream. Create(FN, fmCreate);
  except
    Exit;
  end;
  try
    Signature[0]:= 'E';
    Signature[1]:= 'P';
    case newStyle of
      0:begin
          f.Write(Signature, 2);
          { число наборов }
          f.write(fSetCount, SizeOf(fSetCount));
          for i:= 1 to FSetCount do begin
            f.write(FDefinitions^[i-1], SizeOf(TCPDefinition));
          end;
        end;
      1:begin  { стиль extended ECC }
          { store current page }
          if (FDefinitions <> Nil) and (NewStyle < FSetCount) then begin
            f.Write(FDefinitions^[NewStyle].xlat, 256);
            f.Write(FDefinitions^[NewStyle].inuse, 256 div 8);
          end;
        end;
      2:begin  { стиль ECC }
          { store current page }
          if (FDefinitions <> Nil) and (NewStyle < FSetCount) then begin
            f.Write(FDefinitions^[NewStyle].xlat, 256);
          end;
        end;
    end;
    f. Free;
    FFileName:= fn;
  except
    Exit;
  end;
  StoreToFile:= True;
end;

function TCodePageEditor.LoadFromFile(fn: String): Boolean;
begin
  if FDefinitionsMem <> 0
  then FreeMem(FDefinitions, FDefinitionsMem);
  FSetCount:= LoadECC(fn, True, FDefinitions, Nil);
  FDefinitionsMem:= FSetCount * SizeOf(TCPDefinition);
  FFileName:= fn;
  LoadFromFile:= FSetCount > 0;
end;

function TCodePageEditor.GetTextDescription(No: Integer): String;
var
  i: Integer;
  ch: Char;
  S: String;
  st, fin: Integer;
begin
  GetTextDescription:= '';
  S:= '';
  if (FDefinitions = Nil) or (no > FSetCount)
  then Exit;
  if no < 0 then begin
    st:= 0;
    fin:= FSetCount - 1;
  end else begin
    st:= no;
    fin:= no;
  end;

  for ch:= 'ј' to 'я'
  do S:= S + #32 + ch + #32;
  S:= S + #13#10;
  for ch:= 'а' to '€'
  do S:= S + #32 + ch + #32;
  S:= S + #13#10;
  { число наборов }
  for i:= st to fin do begin
    S:= S + FDefinitions^[i].Name +
    ', страница: ' + IntToStr(FDefinitions^[i].CP)+
    ', базова€ страница: ' + IntToStr(FDefinitions^[i].BaseCP);

    if not (pgt_used in FDefinitions^[i].Flags)
    then S:= S + ', не используетс€';
    if pgt_latU in FDefinitions^[i].Flags
    then S:= S + ' |LAT'
    else S:= S + ' |   ';
    if pgt_latl in FDefinitions^[i].Flags
    then S:= S + '|lat'
    else S:= S + '|   ';
    if pgt_cyrU in FDefinitions^[i].Flags
    then S:= S + '|CYR'
    else S:= S + '|   ';
    if pgt_cyrl in FDefinitions^[i].Flags
    then S:= S + '|cyr'
    else S:= S + '|   ';
    S:= S + '|'#13#10;
    for ch:= 'ј' to 'я' do begin
      if ord(ch) in FDefinitions^[i].inuse
      then S:= S + IntToHex(FDefinitions^[i].xlat[Byte(ch)], 2)+#32
      else S:= S + #32#32#32;
    end;
    S:= S + #13#10;
    for ch:= 'а' to '€' do begin
      if ord(ch) in FDefinitions^[i].inuse
      then S:= S + IntToHex(FDefinitions^[i].xlat[Byte(ch)], 2)+#32
      else S:= S + #32#32#32;
    end;
    S:= S + #13#10;
  end;
  GetTextDescription:= S;
end;

procedure TCodePageEditor.Add;
var
  NewSize: LongInt;
begin
  NewSize:= FDefinitionsMem + SizeOf(TCPDefinition);
  { Definitions:= ReallocMem(Definitions, DefinitionsMem, NewSize); }
{$IFDEF VER80}
  ReallocMem(FDefinitions, FDefinitionsMem, NewSize); { ?!! }
{$ELSE}
  ReallocMem(FDefinitions, NewSize);
{$ENDIF}
  FDefinitionsMem:= NewSize;
  Inc(FSetCount);
  Reset(FSetCount - 1);
end;

procedure TCodePageEditor.Delete(No: Integer);
var
  NewSize, BytesShift: LongInt;
begin
  if No > FSetCount - 1
  then Exit;
  NewSize:= FDefinitionsMem - SizeOf(TCPDefinition);
  BytesShift:= (FSetCount - no - 1) * SizeOf(TCPDefinition);
  if BytesShift > 0  { == no >= FSetCount - 1 }
  then Move(FDefinitions^[no+1], FDefinitions^[no], BytesShift);
{$IFDEF VER80}
  ReallocMem(FDefinitions, FDefinitionsMem, NewSize); { ?!! }
{$ELSE}
  ReallocMem(FDefinitions, NewSize);
{$ENDIF}
  FDefinitionsMem:= NewSize;
  Dec(FSetCount);
end;

function LoadECC(const FN: String; LoadALLDefs: Boolean;
  var Defs: PCPDefinitions; ANames: TStrings): Integer;
var
  f: TFileStream;
  Signature: array[0..1] of Char;
  Buf: TCPDefinition; { временный буфер, где провер€етс€, подходит или нет }
  i, sets, used: Word;
begin
  { clear all }
  LoadECC:= 0;
  if ANames <> Nil
  then ANames.Clear;
{$IFDEF VER80}
  ReAllocMem(Defs, 0, 0); { this is wrong }
{$ELSE}
  ReAllocMem(Defs, 0);
{$ENDIF}
  if (FN = '') or (not FileExists(FN))
  then Exit;
  try
    f:= TFileStream. Create(FN, fmOpenRead + fmShareDenyNone);
  except
    LoadEcc:= -1;
    Exit;
  end;
  if f.Size < 256 then begin
    f.Free;
    Exit;
  end;

  f.Read(Signature, 2);
  if (Signature[0] = 'E') and (Signature[1] = 'P') then begin
    { число всего наборов }
    f.read(sets, SizeOf(Word));
    if not LoadALLDefs then begin
      { подсчитать кол- во с выставленным флагом }
      used:= 0;
      for i:= 1 to sets do begin
        f.read(Buf, SizeOf(TCPDefinition));
        if pgt_used in Buf.Flags
        then Inc(used);
      end;
    end else used:= sets;
    f.seek(4, 0);
    { выделить пам€ть }
{$IFDEF VER80}
    ReAllocMem(Defs, 0, used * SizeOf(TCPDefinition)); { this is wrong }
{$ELSE}
    ReAllocMem(Defs, used * SizeOf(TCPDefinition));
{$ENDIF}
    { заполнить }
    used:= 0;
    for i:= 1 to sets do begin
      f.read(Buf, SizeOf(TCPDefinition));
      if LoadALLDefs or (pgt_used in Buf.Flags) then begin
        Defs^[used]:= Buf;
        if ANames <> Nil
        then ANames.Add(Buf.Name);
        Inc(used);
      end;
    end;
  end else begin
    { стиль ECC }
    used:= 1;
    f.Seek(0, soFromBeginning);
{$IFDEF VER80}
    ReAllocMem(Defs, 0, SizeOf(TCPDefinition)); { this is wrong }
{$ELSE}
    ReAllocMem(Defs, SizeOf(TCPDefinition));
{$ENDIF}
    if f.Size < 256 then begin
    end else begin
      Defs^[0].Name:= ExtractFileName(fn);
      f.Read(Defs^[0].xlat, 256);
      { с добавочным хвостом }
      if f.Size >= 256 + (256 div 8)
      then f.Read(Defs^[0].InUse, 256 div 8)
      else Defs^[0].InUse:= [];
      if ANames <> Nil
      then ANames.Add(Defs^[0].Name);
    end;
  end;
  f. Free;
  LoadECC:= used;
end;

{$ENDIF}

{$IFDEF VER70}
function LoadECC(const FN: String; LoadALLDefs: Boolean;
  var Defs: PCPDefinitions): Integer;
var
  f: PDosStream;
  Signature: array[0..1] of Char;
  Buf: TCPDefinition; {}
  i, sets, used: Word;
begin
  LoadECC:= 0;
  New(f, Init(FN, stOpenRead));
  if (f^.Status <> stOk) or (f^.GetSize < 256) then begin
    LoadEcc:= -1;
    Exit;
  end;
  f^.Read(Signature, 2);
  if (Signature[0] = 'E') and (Signature[1] = 'P') then begin
    f^.read(sets, SizeOf(Word));
    { count used codepages, else codepages = sets }
    if not LoadALLDefs then begin
      used:= 0;
      for i:= 1 to sets do begin
        f^.read(Buf, SizeOf(TCPDefinition));
        if pgt_used in Buf.Flags
        then Inc(used);
      end;
    end else used:= sets;
    f^.seek(4);
    GetMem(Defs, used * SizeOf(TCPDefinition));
    used:= 0;
    for i:= 1 to sets do begin
      f^.read(Buf, SizeOf(TCPDefinition));
      if LoadALLDefs or (pgt_used in Buf.Flags) then begin
        Defs^[used]:= Buf;
        Inc(used);
      end;
    end;
  end else begin
    { native ECC }
    used:= 1;
    f^.Seek(0);
    GetMem(Defs, SizeOf(TCPDefinition));
    if f^.GetSize < 256 then begin
    end else begin
      Defs^[0].Name:= NameOnly(fn);
      f^.Read(Defs^[0].xlat, 256);
      if f^.GetSize >= 256 + (256 div 8)
      then f^.Read(Defs^[0].InUse, 256 div 8)
      else Defs^[0].InUse:= [];
    end;
  end;
  Dispose(f, Done);
  LoadECC:= used;
end;
{$ENDIF}

procedure FillXlatFromDefs(no: Integer; Defs: PCPDefinitions; var Dest);
begin
  Move(Defs^[no].xlat, Dest, 256);
end;

function  GetXlatFromDefs(no: Integer; Defs: PCPDefinitions): Pointer;
begin
  GetXlatFromDefs:= @(Defs^[no].xlat);
end;

{ return "," delimited string }
function  GetDefNames(Defs: PCPDefinitions; Count: Integer): String;
var
  i: Integer;
  S: String;
begin
  S:= '';
  for i:= 0 to Count - 1 do begin
    S:= S + Defs^[i].Name;
  end;
  GetDefNames:= S;
end;

{$IFDEF VER70}
{ get number of xlat table by name }
function GetXlatNoByName(Defs: PCPDefinitions; Count: Integer; AName: String): Integer;
var
  i, r: Integer;
  S: String;
begin
  r:= -1;
  for i:= 0 to Count - 1 do begin
    if UpCaseStr(AName) = UpCaseStr(Defs^[i].Name) then begin
      r:= i;
      Break;
    end;
  end;
  GetXlatNoByName:= r;
end;
{$ENDIF}

{ create inverse xlat }
procedure XlatInverse(orig256, invr256: Pointer);
var
  i: Integer;
begin
  if (orig256 = Nil) or (invr256 = Nil)
  then Exit;
  for i:= 0 to 255
  do TXLAT(invr256^)[i]:= i;

  for i:= 0 to 255
  do if TXLAT(orig256^)[i] <> i
  then TXLAT(invr256^)[TXLAT(orig256^)[i]]:= i;
end;

{$IFNDEF VER70}
{ codepage name alias routines implementation }
const
  SECTXLAT = 'cp2xlat'; { section of pair }
var
  FxlatDefs: PCPDefinitions;
  FxlatNames: TStringList;

function InitCPName2Ecc(AIni, xlatFn: String): Boolean;
var
  IniFile: TIniFile;
  Sect, TmpSL: TStringList;
  i, j: Integer;
  p: Pointer;
  s, vl: String;
begin
  InitCPName2Ecc:= False;

  Sect:= TStringList.Create;
  FxlatNames:= TStringList.Create;
  if LoadEcc(xlatFN, False, FxlatDefs, FxlatNames) <= 0
  then Exit;

  if AIni = '' then Exit;
  IniFile:= TIniFile.Create(AIni);
  IniFile.ReadSectionValues(SECTXLAT, Sect);
  IniFile.Free;

  TmpSL:= TStringList.Create;
  { replace each value to appropriate number }
  for i:= 0 to Sect.Count - 1 do begin
    j:= Pos('=', Sect[i]);
    if j > 0
    then S:= Copy(Sect[i], 1, j)
    else S:= Sect[i];
    vl:= Copy(Sect.Strings[i], Length(S)+ 2, MaxInt);
    Integer(p):= FxlatNames.IndexOf(vl);
    TmpSL.AddObject(S, P);
    { set default index }
  end;
  FxlatNames.Free;
  Sect.Free;
  FxlatNames:= TmpSL;
  InitCPName2Ecc:= True;
end;

{ do nothing if AxlatName doesn't exists }
procedure xlatByName(AxlatName: String; ADefs: PCPDefinitions; ACPNames: TStrings; var ABuf: String);
type
  CA = array[0..1] of Char;
var
  ind: Integer;
  i: Integer;
  xlat256: TXlat;
begin
  if ACPNames = Nil
  then Exit;
  ind:= ACPNames.IndexOf(AxlatName);
  if ind < 0
  then Exit;
  XlatInverse(@(ADefs^[ind].xlat), @xlat256);
  for i:= 1 to Length(ABuf) do begin
    Byte(ABuf[i]):= xlat256[Byte(ABuf[i])];
  end;
end;

{ store changes to registry }
procedure StoreCPName2EccToRegistry;
begin
  { not implemented yet }
end;

{ return Nil if fails }
function CPName2Ecc(ACPName: String): Pointer;
var
  ind: Integer;
begin
  CPName2Ecc:= Nil;
  if (FxlatNames = Nil) then Exit;
  ind:= FxlatNames.IndexOf(ACPName);
  if ind >= 0
  then CPName2Ecc:= @(FxlatDefs^[ind].xlat);
end;

procedure DoneCPName2Ecc;
begin
  LoadEcc('', False, FxlatDefs, FxlatNames);
end;
{$ENDIF}
end.
