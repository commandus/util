unit
  Marc;
(*##*)
(*******************************************************************************
*                                                                             *
*                                                                              *
*   M  A  R  C   routines for ISO2709 bibliography records                    *
*                                                                              *
*   Andrei Ivanov (c) 1996, 1997, 2002. All rights reserved.                  *
*   functions 2 access 2 files in US MARC format for bibliography              *
*   and fundamental marc record iterator GetRecordsQTY                        *
*   known as LDB files.                                                        *
*   Conditional defines: STRINGSVALUES- store '090a=' to each TStrings        *
*   ISO 2709                                                                   *
*   Last Revision: Dec 31 1996                                                *
*   Last fix     : May 02 1997                                                 *
*   haircut        Jul 25 1998 (some routines moved to marc_acc)              *
*   Lines        : 1649                                                        *
*   Printed      : ---                                                        *
*                                                                              *
*******************************************************************************)
(*##*)

interface
{$IFDEF VER70}
uses
  Objects, strutil, Strings;
{$ELSE}
uses
  Classes, SysUtils, Util1;
{$ENDIF}

const
  COPYRIGHT  = '© Andrei Ivanov, 1996, 1998, 2000. All rights reserved.';
  CONTACTS   = 'http://ensen.8m.com aivanov@hotmail.ru';
  MARC_INI   = 'MARC.INI';
  EOD        = Char($1D);
  grFIRSTRECORD = 1;
  grALLRECORD   = -1;

  CodesRG1Let: String[6]  = #32'acdij';
  CodesRG2Let: String[5]  = #32'masc';
  TAGPREFIX: String[1]    = '#'; { '~' }
  TAGPREFIXLEN            = 1;
  DEFAULTDLMT             = '; ';

type
{$IFDEF VER70}
  TFileStream = PDOSStream;
  SmallInt    = Integer;
{$ENDIF}
  CA3 = array[0..2] of Char;
  CA4 = array[0..3] of Char;
  CA5 = array[0..4] of Char;
  {
  IA  = array[0..10] of SmallInt;
  CA  = array[0..10] of Char;
  }
  { structure keeps field number, field repeat number and subfield char in
    Objects property of TStrings (as pointer)
  }
  TFld4 = record  { 4 bytes long: subfield, order, field }
  case Integer of
  0: (P: Pointer;);
  1: (L: LongInt;);
  2: (o: Byte; s: Byte; f: Word;); { backward order for ascending sorting as Integer }
  end;

  { structure keeps field(subfield) number, field length  in
    Objects property  (as pointer)
  }
  TTag4 = record  { 4 bytes long }
  case Integer of
  0: (P: Pointer;);
  1: (L: LongInt;);
  2: (len: Word; rpt: Word;);
  end;

  TMarcLeader = record
  case Integer of
  0: (C: array [0..$17] of Char;);
  1: (Len: CA5;                         { '00480' длина записи в байтах (=смещение след. записи) }
      State: Char;                      { 'n', 'd' }
      UseCodes: CA4;                    { 'am0 '   }
      IndicatorLen: Char;               { '2'      }
      SubFieldIdLen: Char;              { '2'      }
      Base: CA5;                        { '00145' смещение данных (1ый байт Leader-0)}
      User: CA3;                        { '   '    }
      EntryLen: Char;                   { '4'      }
      FirstCharPosLen: Char;            { '5'      }
      Reserved: array[0..1] of Char;);  { '  ' }
  end;

  TMarcRec = record
    Leader: TMarcLeader;
    Data: array[0..0] of Char;
  end;

  CharSet = set of Char;

  { затем - директори€ (длина=Base-$18) с терминатором $1E(#30)
    данные (длина=Len-Base), пол€ терминируютс€ $1E(#30), подпол€ раздел€ютс€ $1F(#31)
    вс€ запись терминируетс€ EOD $1D(#29) (и в LDB можно поставить CRLF и
    все что угодно - но до указываемой LEN следующей записи)
  }
  THeaderBuf = array[0..$17] of Char;
  TLeaderQueryFunc= function (Strm: TFileStream; const Leader: TMarcLeader;
    var AllowNext: Boolean): Boolean;
  TLeaderQueryProc= procedure(const Leader: TMarcLeader);

{ tag.dat access object }
{$IFDEF VER70}
  PTagDesc = ^TTagDesc;
  TTagDesc = object(TObject)
{$ELSE}
  TTagDesc = class(TObject)
{$ENDIF}
    private
    FldOfss: ^IA;
    Text: ^CA;
    TextLen: Word;
    fc_Desc: String;
    fc_Len: Integer;
    fc_Rpt: Integer;
    procedure  ParseFldSubFld(FldSubFld: String);
    public
    FldsCount: Integer;
    Status: Integer;
{$IFDEF VER70}
    constructor Init(FN: String);
    destructor Done; virtual;
{$ELSE}
    constructor Create(FN: String); virtual;
    destructor Destroy; override;
{$ENDIF}
  { return name of field or subfield }
    function  GetTagDescription(FldSubFld: String): String;
    { TTag4: lo byte length, hi- repeats }
    function  GetTagLengths(FldSubFld: String): LongInt;
    function  GetFldEnabled(fld: Integer): Boolean;
    procedure SetFldEnabled(fld: Integer; Enable: Boolean);
    function  GetFldNoByOrder(Order: Integer): Integer; { order - 0,.. }
  end;
{$IFNDEF VER70}
    PTagDesc = TTagDesc;
{$ENDIF}

{ basic routines (read and parse marc field's directory) }
function EntriesInDir(var Buf; Limit: Word): Word;
{ return length of the record, calculated by Leader
  record = header + directory + data }
function GetRecLen(const Leader): LongInt;  { TMarcLeader }
{ return length of the directory, calculated by Leader WITH terminator }
function GetDirLen(const Leader): LongInt;

{ return offset of data }
function GetDataOfs(const Leader): LongInt;
{ return length of data }
function GetDataLen(const Leader): LongInt;

{ marcbuf - char array, NOT a pointer }
function  TagValue(FieldSubField, Delimiter: String; const MarcBuf; var Dest): Word;
{ TagValue called from FillCard }
{ format '%s' }
function  TagValueFmt(const FieldSubField, Fmt, dlmt: String; const MarcBuf; var Dest): Word;


{$IFDEF VER70}
procedure ReadMarcRecord(StrmLDB: TDosStream; var Buf: Pointer; var buflen: Word);
{$ELSE}
procedure ReadMarcRecord(StrmLDB: TStream; var Buf: Pointer; var buflen: Integer);

{ read next record from stream }
function ReadRecordAsString(LDBStrm: TStream; AOfs: Longint): String;
{$ENDIF}

{ S: ab-df}
procedure ParseSFldList(S: String; var R: CharSet);

{ record formatting routines (view record contents as string) }
procedure BibSimpleCard(const MarcBuf; var Dest);
{ before call BibSimpleList initialize tagdesc object first! }
procedure BibSimpleList(tagdesc: PTagDesc; const MarcBuf; var Dest);

{$IFNDEF VER70}
  { override virtual method SORT (by Objects property keeps TFld4) }
type
  TFld4StringList = class (TStringList)
  private
    procedure ExchangeItems(Index1, Index2: Integer);
    procedure QuickSortFld4(L, R: Integer);
  public
    procedure Sort; override;
  end;

{ read entire marc record and fill TStringList, for example:
  100a=Author
  Object pointer as TFld4 (subfield number by order (0..))
  Return qty of fields (0 if nothing)
  return DEST strings sorted (dupAccept). Use TFld4StringList instead TStringList
}
function Flds2Strings(const MarcBuf; Dest: TStrings): Integer;

{ store entire marc record by TStringList contents
  Return size of bytes stored in Marc buf (0 if nothing)
}
function Strings2Fld(Src: TStringList; var MarcBuf): Integer;

{ context # - current record number, * - total records , x- current codepage xlat name }
procedure Bib2HTML(const MarcBuf; const tagform: String; Context: TStrings; var Dest: String);
{$ENDIF}

{ passthru - true- unrecognized tag stay live
           - false remove unrecognized (not like <~100a> tags)
}
procedure Bib2HTML0(const MarcBuf; const tagform; var Dest; PassThru: Boolean);

var
  { offset in MARC's file, default value is 0 (for .LDB files) }
  MarcStartFOffset: LongInt;
  { current file offset at the beginning of record }
  CurrentRecordFOffset: LongInt;
  { do no change RECS ans RECSAct, chaned from }
  Recs: LongInt;     { any records processed counter }
  RecsAct: LongInt;  { 'valid' records (records selected) }

{ GetRecordsQTY
  Parameters:
  LBD_FileName - file name of LDB file.
  LeaderQuery  - function that do smth on each found record or NIL value
    for example, use Action_CopyAnyRecordsAsActual or NIL values
    Note: if NIL then GetRecordsQTY return quantity of all (+deleted too!) records
    Note: if LeaderQuery return False, qty is not increments
  qty -   0 - only one physical record process (or not).
          grFIRSTRECORD        ==  1 after first found stop
          grALLRECORD == grALL == -1 process while not EOF
  return value:
    0 if not anything found or LDB open or read failed
    if fault, return negative value (equil actually processed
  Effects:
    if GetRecordsQTYProgressGauge = NIL then nothing
    else show progress bar
}
function GetRecordsQTY(const LBD_FileName: String; LeaderQuery: TLeaderQueryFunc;
  Qty: Longint): LongInt;

{ read marc fields description stored in text file named FN
  input text format:
    XXXXyyyy Field description text
 return values:
  Nil      - if failed to open and/or read file FN
  TStrings - string list contains marc's field descriptions
    output string list format:
    XXXX=Field description text
}
{$IFNDEF VER70}
function ReadFieldDescriptions(const FN: String): TStrings;
{$ENDIF}

{ extract field and subfield from string. If no subfield, return #0 in sfld
  If S is invalid string, return False
}
function ExtractFldSubFld(S: String; var fld, sfld: Integer): Boolean;

{ construct feld name from field number and subfield. If no subfield, return #0 in sfld
  if sfld = 0, no subfield adds
}
function MkFldSubFld(fld, sfld: Integer): String;

const
  { names of sections of .REL (or, traditionally, .INI) ini-files }
  DBSectionName = 'TagDB';
  DbSectionNameSQL= 'TagDBSQL';
  DescSectionName = 'TagDesc';

function ReadKey(AStrmKeys: TStream; AKeyNo: Cardinal; var BufKey; MaxBufLen: Integer): Boolean;

implementation
{$IFNDEF VER70}
uses
  Consts;
{$ENDIF}
{ basic routines --------------------------------------------------------------}

function GetRecLen(const Leader): LongInt;  { Leader: TMarcLeader }
var
  LenS: String[5];
begin
  LenS[0]:= #5;
  Move(Leader, LenS[1], 5);
  GetReclen:= StrToIntDef(LenS, 0);
end;

{ return 0 if fails
}
function GetDirLen(const Leader): LongInt;
var
  LenS: String[5];
begin
  LenS[0]:= #5;
  Move(TMarcLeader(Leader).Base, LenS[1], 5);
  { terminator occupies 1 byte GetDirlen:= StrToIntDef(LenS, $18+1) - $18-1; }
  GetDirlen:= StrToIntDef(LenS, $18) - $18;
  { 1 terminator bytes includes in Directory }
end;

function GetDataOfs(const Leader): LongInt;
var
  LenS: String[5];
begin
  LenS[0]:= #5;
  Move(TMarcLeader(Leader).Base, LenS[1], 5);
  GetDataOfs:= StrToIntDef(LenS, 0);
end;

function GetDataLen(const Leader): LongInt;
begin
  GetDataLen:= GetRecLen(Leader) - GetDataOfs(Leader);
end;

{$IFNDEF VER70}
{ not tested! }
function ReadRecordAsString(LDBStrm: TStream; AOfs: Longint): String;
var
  RL: Word;
begin
  LDBStrm.Position:= AOfs;
  SetLength(Result, $18);
  LDBStrm.Read(Result[1], $18);
  RL:= GetRecLen(Result[1]);
  SetLength(Result, RL);
 { read directory and all fields to data buffer }
  LDBStrm.Read(Result[$19], RL - SizeOf(TMarcLeader));
end;

function ReadFieldDescriptions(const FN: String): TStrings;
var
  TF: TStringList;
  F: TextFile;
  S: ShortString;
begin
  ReadFieldDescriptions:= Nil;
  if not FileExists(FN) then begin
    Exit;
  end;
  try
    TF:= TStringList.Create;
    AssignFile(F, FN);
    Reset(F);
    while not EOF(F) do begin
      Readln(F, S);
      if Length(S) > 8 then begin
        if S[1] in ['0'..'9'] then begin
          { field
          }
          System.Delete(S, 4, 4);
          S[4]:= '=';
          TF.Add(S);
        end;
      end;
    end;
  finally
    CloseFile(F);
  end;
  ReadFieldDescriptions:= TF;
end;
{$ENDIF}

{ not tested
}
function EntriesInDir(var Buf; Limit: Word): Word;
var
  i, L: Word;
begin
  EntriesInDir:= 0;
  L:= Limit div (3+4+5);
  if L = 0
  then Exit;
  for i:= 0 to L - 1 do begin
    if CA(Buf)[i*12] = Char($1E) then begin
      EntriesInDir:= i;
      Exit;
    end;
  end;
  EntriesInDir:= L;
end;

{ TTagDesc object keeps description of fields and subfields--------------------}
{ return count (in old version KnownMarcFieldsCount)
  KnownMarcFields: ^IA;
}
{ return count (in old version KnownMarcFieldsCount)
  KnownMarcFields: ^IA;
}

{$IFDEF VER70}
constructor TTagDesc.Init(FN: String);
{$ELSE}
constructor TTagDesc.Create(FN: String);
{$ENDIF}
var
{$IFDEF VER70}
  F: PDosStream;
{$ELSE}
  F: TFileStream;
  SS: ANSIString;
{$ENDIF}
  S: String[3];
  L: LongInt;
  p: ^IA;
  fp, fofs: Integer;
begin
{$IFDEF VER70}
  inherited Init;
{$ELSE}
  inherited Create;
{$ENDIF}
  { do not free up stream object if failed, it's ok }
  Status:= - 1;
  FldOfss:= Nil;
  FldsCount:= 0;
  Text:= Nil;
  TextLen:= 0;

  if not FileExists(FN)
  then Exit;
{$IFDEF VER70}
  New(F, Init(FN, stOpenRead));
  L:= F^.GetSize;
  if (F^.Status <> stOk ) or (L > $F000)
  then Exit;
  TextLen:= L + 3;
  GetMem(Text, TextLen);
  if Text = Nil
  then Exit;
  F^.Read(Text^[2], TextLen-3);
  if F^.Status <> stOk then begin
    FreeMem(Text, TextLen);
    Text:= Nil;
    Exit;
  end;
  Dispose(F, Done);
{$ELSE}
  try
    F:= TFileStream.Create(FN, fmOpenRead);
    L:= F.Size;
  except
    Exit;
  end;
  if (L > $F000)
  then Exit;
  TextLen:= L + 3;
  GetMem(Text, TextLen);
  if Text = Nil
  then Exit;
  try
    F.Read(Text^[2], TextLen-3);
  except
    FreeMem(Text, TextLen);
    Exit;
  end;
  F.Free;
{$ENDIF}
  Text^[0]:= #13;
  Text^[1]:= #10;
  Text^[TextLen-1]:= #13;
  fofs:= 0;
  FldsCount:= 0;
  GetMem(p, $8000);
  S[0]:= #3;
  while (fofs <= TextLen) do begin
{$IFDEF VER70}
    fp:= Scan(Text^[fofs+2], TextLen - fofs - 2, #13#10);
{$ELSE}
    SS:= StrPas(@(Text^[fofs+2]));
    fp:= Scan32(SS, 0, 1, #13) - 1;
{$ENDIF}
    if fp < 0 then begin
      Break;
    end;
    { +2-2=0}
    Text^[fofs]:= #10;
    Text^[fofs+1]:= Chr(fp);
    if (fofs+4 < TextLen) and (Text^[fofs+2] > #32) then begin
      Move(Text^[fofs+2], S[1], 3);
      if isDecimal(S) then begin
        p^[FldsCount]:= StrToInt(S);
        Inc(FldsCount);
      end;
    end;
    fofs:= fofs + fp + 2;
  end;
  GetMem(FldOfss, FldsCount * SizeOf(Word));
  Move(p^, FldOfss^, FldsCount * SizeOf(Word));
  FreeMem(p, $8000);
  Status:= 0;
end;

{$IFDEF VER70}
destructor TTagDesc.Done;
{$ELSE}
destructor TTagDesc.Destroy;
{$ENDIF}
begin
  if Text <> Nil
  then FreeMem(Text, TextLen);
  if FldOfss <> Nil
  then FreeMem(FldOfss, FldsCount * SizeOf(Word));
{$IFDEF VER70}
  inherited Done;
{$ELSE}
  inherited Destroy;
{$ENDIF}
end;

{$IFDEF VER70}
type
  ShortString = String;
{$ENDIF}

procedure  TTagDesc.ParseFldSubFld(FldSubFld: String);
var
  fp: Word;
  L: Integer;
  subfld: Char;
  S: ^ShortString;
  SFld: ShortString;
begin
  fc_Desc:= '';
  fc_Len:= 0;
  fc_Rpt:= 0;
  if FldOfss = Nil
  then Exit;

  L:= Length(FldSubFld);
  if L <= 0
  then Exit;
  case L of
  3: begin
       subfld:= #0;
       { FSub:= ''; }
     end;
  4: subfld:= Upcase(FldSubFld[4]);
  else Exit;
  end;
  SFld:= Copy(FldSubFld, 1, 3);
  fp:= 1;
  while fp < TextLen do begin
    S:= @(Text^[fp]);
    if Pos(SFld, S^) = 1 then begin
      SFld:= Copy(S^, 9, 255);
      if subfld <> #0 then begin
        repeat
          fp:= fp + Byte(S^[0]) + 2;
          S:= @(Text^[fp]);
          if (Length(S^) < 8) or (S^[1] <> #32)
          then Exit;
          if Upcase(S^[7]) = subfld then begin
            SFld:= Copy(S^, 8, 255);
            fc_Desc:= SFld;
            fc_Len:= StrToInt(Copy(S^, 4, 3));
            if S^[3] = 'r'
            then fc_Rpt:= 999
            else fc_Rpt:= 1; { standard allways, but "r".. }
            Exit;
          end;
        until fp >= TextLen;
      end else begin
        fc_Desc:= SFld;
        fc_Len:= StrToInt(Copy(S^, 4, 3));
        fc_Rpt:= StrToInt(Copy(S^, 7, 2));
        Exit;
      end;
    end;
    fp:= fp + Byte(S^[0]) + 2;
  end;
end;

function  TTagDesc.GetTagDescription(FldSubFld: String): String;
begin
  ParseFldSubFld(FldSubFld);
  GetTagDescription:= fc_Desc;
end;

function  TTagDesc.GetTagLengths(FldSubFld: String): LongInt;
var
  L: TTag4;
begin
  ParseFldSubFld(FldSubFld);
  L.len:= fc_Len;
  L.Rpt:= fc_Rpt;
  GetTagLengths:= L.l;
end;

function  TTagDesc.GetFldEnabled(fld: Integer): Boolean;
var
  i: Integer;
begin
  GetFldEnabled:= False;
  for i:= 0 to FldsCount - 1 do begin
    if Abs(FldOfss^[i]) = fld then begin
      GetFldEnabled:= FldOfss^[i] > 0;
      Exit;
    end;
  end;
end;

procedure TTagDesc.SetFldEnabled(fld: Integer; Enable: Boolean);
var
  i: Integer;
begin
  for i:= 0 to FldsCount - 1 do begin
    if Abs(FldOfss^[i]) = fld then begin
      if Enable
      then FldOfss^[i]:= Abs(FldOfss^[i])
      else FldOfss^[i]:= - Abs(FldOfss^[i]);
      Exit;
    end;
  end;
end;

function  TTagDesc.GetFldNoByOrder(Order: Integer): Integer; { order - 0,.. }
var
  i, count: Integer;
begin
  GetFldNoByOrder:= 0;
  { if (Order < 0) or (Order >= FldsCount) then Exit; }
  count:= -1;
  for i:= 0 to FldsCount - 1 do begin
    if FldOfss^[i] > 0 then begin
      Inc(Count);
      if count = order then begin
        GetFldNoByOrder:= fldOfss^[i];
        Exit;
      end;
    end;
  end;
end;

{ iterator for file -----------------------------------------------------------}
function GetRecordsQTY(const LBD_FileName: String; LeaderQuery: TLeaderQueryFunc;
  Qty: Longint): LongInt;
var
  SrcStrm: TFileStream;
  Leader: TMarcLeader;
  FileLen: LongInt;
  NextStepAllowed: Boolean; { Set to False if you want break iteration on CALLBACK}
   { (no matter, pass parameter true or false in entry) }
begin
  Recs:= 0;
  RecsAct:= 0;
  CurrentRecordFOffset:= MarcStartFOffset;
  { check presense }
  if not FileExists(LBD_FileName) then begin
    GetRecordsQTY:= 0;
    Exit;
  end;
{$IFDEF VER70}
  New(SrcStrm, Init(LBD_FileName, stOpenRead));
  if SrcStrm^.Status <> stOk then begin
    GetRecordsQTY:= 0;
    Exit;
  end;
  FileLen:= SrcStrm^.GetSize;
  repeat
    SrcStrm^.Seek(CurrentRecordFOffset);
    SrcStrm^.Read(Leader, $18);
    { call Application.ProcessMessages in GaugeProc yourself }
    NextStepAllowed:= True;
    if (@LeaderQuery <> Nil) and LeaderQuery(SrcStrm, Leader, NextStepAllowed)
    then Inc(RecsAct);
    Inc(Recs);
    CurrentRecordFOffset:= CurrentRecordFOffset + GetRecLen(Leader);
  until (not NextStepAllowed) or (CurrentRecordFOffset >= FileLen) or (RecsAct = Qty);
  Dispose(SrcStrm, Done);
{$ELSE}
  try
    SrcStrm:= TFileStream.Create(LBD_FileName, fmOpenRead+fmShareDenyNone);
  except
    GetRecordsQTY:= 0;
    Exit;
  end;
  try
    FileLen:= SrcStrm.Size;
    repeat
      SrcStrm.Position:= CurrentRecordFOffset;
      SrcStrm.Read(Leader, $18);
      { call Application.ProcessMessages in GaugeProc yourself }
      NextStepAllowed:= True;
      if (@LeaderQuery <> Nil) and LeaderQuery(SrcStrm, Leader, NextStepAllowed)
      then Inc(RecsAct);
      Inc(Recs);
      CurrentRecordFOffset:= CurrentRecordFOffset + GetRecLen(Leader);
    until (not NextStepAllowed) or (CurrentRecordFOffset >= FileLen) or (RecsAct = Qty);
  except
    Recs:= -Recs;
    RecsAct:= -RecsAct;
  end;
  SrcStrm.Free;
{$ENDIF}
  { DoneTMP; }
  if @LeaderQuery <> Nil
  then GetRecordsQTY:= RecsAct
  else GetRecordsQTY:= Recs;
end;

{ simple file routines --------------------------------------------------------}

{ read marc record in 2 parts: leader and  data from current file offset }
{$IFDEF VER70}
procedure ReadMarcRecord(StrmLDB: TDosStream; var Buf: Pointer; var buflen: Word);
{$ELSE}
procedure ReadMarcRecord(StrmLDB: TStream; var Buf: Pointer; var buflen: Integer);
{$ENDIF}
var
  rlen: Integer;
begin
  StrmLDB.Read(Buf^, SizeOf(TMarcLeader)); { SizeOf(TMarcLeader) = $18 }
  { read directory and all fields to data buffer }
  rlen:= GetRecLen(Buf^);
  { check space }
  if rlen > buflen then begin
{$IFDEF VER80}
    ReAllocMem(Buf, buflen, rlen + 1);
{$ELSE}
{$IFDEF VER70}
    { if (buflen > 0) and (Buf <> Nil) then }
    FreeMem(Buf, buflen);
    GetMem(Buf, rlen + 1);
    { re-read leader }
    StrmLDB.Seek(StrmLDB.GetPos - SizeOf(TMarcLeader));
    StrmLDB.Read(Buf^, SizeOf(TMarcLeader)); { SizeOf(TMarcLeader) = $18 }
{$ELSE}
    ReAllocMem(Buf, rlen + 1);
{$ENDIF}
{$ENDIF}
  end;
  StrmLDB.Read(CA(Buf^)[SizeOf(TMarcLeader)], rlen - SizeOf(TMarcLeader));
end;

{ formatting record to vew routines -------------------------------------------}
 { SubFieldParseBuf and SubFieldParseBufLen
  are: pointer to loaded from file buffer
  and length of subfield
}

var
  marcfldname: String[3]; { this is string buffers }
  S4: String[4];
  S5: String[5];

{ return position if marc field in Data record. If field is repeatable, loop.
  Called from TagValue
  Parameters:
  curdirofs - 0..n*12 - first time set to 0. Next time DO NOT CHANGE value
  Return: true- next occurance found, false- no
  Start - offset in MarcBuf
  Len   - length of field
}
function  NextMarcField(var curdirofs: Word; const Tag: String;
  const MarcBuf; var FldOfs, FldLen: Integer): Boolean;
var
  DirLen: Integer;
begin
  NextMarcField:= False;
  { changes Oct 18 1998 - IT IS WRONG!
   DirLen:= GetRecLen(Leader) - $18 - GetDataLen(Leader);
  }
  DirLen:= GetDirLen(MarcBuf);                  { +1 byte length terminator }
  while curdirofs < DirLen - 12 do begin        { <= if no terminator }
    Move(TMarcRec(MarcBuf).Data[0+curdirofs], marcfldname[1], 3);
    if marcfldname = Tag then begin
      Move(TMarcRec(MarcBuf).Data[3+curdirofs], S4[1], 4);
      FldLen:= StrToInt(S4);
      Move(TMarcRec(MarcBuf).Data[7+curdirofs], S5[1], 5);
      FldOfs:= StrToInt(S5) + GetDataOfs(MarcBuf) - $18;
      NextMarcField:= True;
      Inc(curdirofs, 12);
      Exit;
    end;
    Inc(curdirofs, 12);
  end;
end;

{ return offset and len of next occurance of subfield in field in record Data
  call NextMarcField first to take field offset and length
  first time SubFldOfs must be equil FldOfs (returned NextMarcField)
  To go to the next occurance, do:
     Inc(SubFldOfs, SubFldLen); NextSubField(..
}
function NextSubField(SubFieldName: Char; const Data;
  const FldFinish: Integer; var SubFldOfs, SubFldLen: Integer): Boolean;
var
  Count: Word;
begin
  NextSubField:= False;
  { produce header for field .. }
  count:= subfldofs;
  while Count <=  FldFinish do begin
    if TMarcRec(Data).Data[Count] = SubFieldName then begin { subfield found }
      { check: is it start of subfield (1.first subfield 2.following by #31 subfield delimiter)}
      if (count = subfldofs) or (TMarcRec(Data).Data[Count-1] = #31) then begin
        SubFldOfs:= Count;
        Break;
      end;
    end;
    Inc(Count);
  end;
  while Count <=  FldFinish do begin
    if (TMarcRec(Data).Data[Count] = #30) or (TMarcRec(Data).Data[Count] = #31) then begin
      { end of field or subfield encountoured }
      SubFldLen:= Count - SubFldOfs + 1;
      NextSubField:= True;
      Exit;
    end;
    Inc(Count);
  end;
end;

procedure ParseSFldList(S: String; var R: CharSet);
var
  Diap: Boolean;
  stChar: char;
  i: Integer;
  c: Char;
begin
  R:= [];
  Diap:= False;
  stChar:= #0;
  for i:=  1 to Length(S) do begin
    if S[i] = '-' then begin
      Diap:= True;
      Continue;
    end else begin
      if Diap then begin
        if S[i] >= stchar then begin
          for c:= stChar to S[i] do Include(R, c)
        end else begin
          for c:= S[i] to stChar do Include(R, c);
        end;
      end else begin
        Include(R, S[i]);
      end;
      Diap:= False;
      stChar:= S[i];
    end;
  end;
end;

{ next function simplify calls NextMarcField and NextSubField for PChar result
  if no = 0, return list of delimited fields(subfields)
  else Delimiter igrored
  Examples: if TagValue('100a', '%s', .. then ...
               TagValue('100a-z')
}
function  TagValue(FieldSubField, Delimiter: String; const MarcBuf;
  var Dest): Word;
var
  tag: String[3];
  cursbfld: Char;
  FldOfs, FldLen: Integer;
  curdirofs,
  i, p, L: Word;
  NewSubFld,
  Delimitered: Boolean;
  sfldset: CharSet;
  dl_len: Word;
begin
  cursbfld:= #0; { just for disable compiler warning }
  curdirofs:= 0;
  p:= 0;
  Tag:= Copy(FieldSubField, 1, 3);
  dl_Len:= Length(Delimiter);
  L:= Length(FieldSubField);
  if L > 3 then begin
    ParseSFldList(Copy(FieldSubField, 4, MaxInt), sfldset);
  end else sfldset:= [#0..#255];
  Delimitered:= True;
  NewSubFld:= True;
  while NextMarcField(curdirofs, Tag, MarcBuf, FldOfs, FldLen) do begin
    if dl_Len = 0 then begin
      Move(TMarcRec(MarcBuf).Data[fldofs], CA(Dest)[p], fldlen);
      Inc(p, fldlen);
    end else begin
      i:= 0;
      while i < fldlen do begin
        case TMarcRec(MarcBuf).Data[fldofs+i] of
        #30: Break;
        #31: begin
             if not Delimitered then begin
               Move(Delimiter[1], CA(Dest)[p], dl_Len);
               Inc(p, dl_Len);
               Delimitered:= True;
             end;
             NewSubFld:= True;
             end;
        else begin
             if NewSubFld then begin
               cursbfld:= TMarcRec(MarcBuf).Data[fldofs+i];
               NewSubFld:= False;
             end else begin
               if cursbfld in sfldset then begin
                 CA(Dest)[p]:= TMarcRec(MarcBuf).Data[fldofs+i];
                 Inc(p);
                 Delimitered:= False;
               end;
             end;
             end;
        end;
        Inc(i);
      end;
    end;
  end;
  CA(Dest)[p]:= #0;
  TagValue:= p;
end;

function  TagValueFmt(const FieldSubField, Fmt, dlmt: String; const MarcBuf;
  var Dest): Word;
var
  tag: String[3];
  cursbfld: Char;
  FldOfs, FldLen: Integer;
  curdirofs,
  i, p, L: Word;
  NewSubFld,
  flddelimiterered,
  Delimitered: Boolean;
  sfldset: CharSet;
  dlmtlen,
  fmt_pos, fmt_fnpos, fmt_len, fmt_stlen, fmt_fnlen: Integer;
begin
  cursbfld:= #0; { just for disable compiler warning }
  curdirofs:= 0;
  Tag:= Copy(FieldSubField, 1, 3);
  fmt_Len:= Length(fmt);

  fmt_pos:= Pos('%s', fmt);

  dlmtlen:= Length(dlmt);
  if fmt_pos <= 0 then begin
    fmt_stlen:= 0;
    fmt_fnlen:= fmt_len;
    fmt_fnpos:= 0;
  end else begin
    fmt_stlen:= fmt_pos - 1;
    fmt_fnlen:= fmt_Len - fmt_pos - 1;
    fmt_fnpos:= fmt_pos + 2;
  end;

  p:= 0;
  L:= Length(FieldSubField);
  if L > 3 then begin
    ParseSFldList(Copy(FieldSubField, 4, MaxInt), sfldset);
  end else sfldset:= [#0..#255];
  Delimitered:= True;
  flddelimiterered:= False;
  NewSubFld:= True;
  while NextMarcField(curdirofs, Tag, MarcBuf, FldOfs, FldLen) do begin
    i:= 0; { 1 - Mar 2001 }
    while i < fldlen do begin
      case TMarcRec(MarcBuf).Data[fldofs+i] of
      #30,#31: begin
           if not Delimitered then begin
             if fmt_fnpos > 0 then begin
               { formatting suffix }
               Move(Fmt[fmt_fnpos], CA(Dest)[p], fmt_fnlen);
               Inc(p, fmt_fnlen);
               { add fld delimiter }
               flddelimiterered:= True;
               Move(dlmt[1], CA(Dest)[p], dlmtlen);
               Inc(p, dlmtlen);
             end;
             Delimitered:= True;
           end;
           NewSubFld:= True;
         end;
      else begin
        if NewSubFld then begin
          cursbfld:= TMarcRec(MarcBuf).Data[fldofs+i];
          if (cursbfld in sfldset) and (fmt_stlen > 0) then begin
            { formatting prefix }
            Move(Fmt[1], CA(Dest)[p], fmt_stlen);
            Inc(p, fmt_stlen);
          end;
          NewSubFld:= False;
        end else begin
          if cursbfld in sfldset then begin
            if fmt_pos > 0 then begin
              { formatting data }
              CA(Dest)[p]:= TMarcRec(MarcBuf).Data[fldofs+i];
              Inc(p);
            end;
            Delimitered:= False;
          end;
        end;
      end;
    end;
    if TMarcRec(MarcBuf).Data[fldofs+i] = #30
    then Break;
    Inc(i);
  end;
    {}
  end;
  if flddelimiterered then begin
    { delete last delimiter }
    Dec(p, dlmtlen);
  end;
  CA(Dest)[p]:= #0;
  TagValueFmt:= p;
end;

procedure BibSimpleCard(const MarcBuf; var Dest);
const
  LAST = 9;
  COLS = 50;
  LEFT = 10;
var
  i: Integer;
  S: array [0..LAST] of String[255];
  CurRow, CurCol: Integer;
  L: Integer;

procedure InsertTo(Col, Row, Limit: Integer; const Index, Delimiter: String);
var
  SS: String;
  Len: Integer;
  irow, count, fr: Integer;
  p: array [0..1023] of Char;
begin
  TagValue(Index, Delimiter, MarcBuf, p);
  SS:= StrPas(p);
  if Row = -1 then begin
    Row:= curRow+1;
    curRow:= Row;
    Col:= LEFT;
    curCol:= Col;
  end else begin
    if Row = 0 then begin
      Row:= curRow;
      if Col >= COLS-1 then begin
        Col:= LEFT;
        Inc(Row);
        CurRow:= Row;
      end;
    end;
    if Col = 0 then begin
      Col:= curCol;
    end;
  end;
  if Limit < 0 then begin
    Limit:= Abs(Limit);
    Len:= Length(SS);
    if Len + col > COLS then begin
      irow:= row-1;
      count:= COLS-col;
      fr:= 1;
      repeat
        if Len - Count < 0
        then count:= Len
        else S[iRow][COLS]:= '-';
        Dec(len, count);
        if irow > LAST
        then Move(SS[fr], S[LAST][Col], count)
        else Move(SS[fr], S[iRow][Col], count);
        Inc(irow);
        CurRow:=iRow;
        CurCol:=Col+Count;
        Inc(fr, count);
      until (len = 0) or (irow > Limit);
    end else begin
      if Len > Limit
      then Len:= Limit;
      if row-1 > LAST
      then Move(SS[1], S[LAST][Col], Len)
      else Move(SS[1], S[Row-1][Col], Len);
      CurRow:=Row;
      CurCol:=Col+Len;
    end;
  end else begin
    Len:= Length(SS);
    if Len > Limit
    then Len:= Limit;
    if row-1 > LAST
    then Move(SS[1], S[LAST][Col], Len)
    else Move(SS[1], S[Row-1][Col], Len);
    CurRow:=Row;
    CurCol:=Col+Len;
  end;
end;

begin
  for i:= 0 to LAST do begin
    S[i][0]:= Char(COLS);
    FillChar(S[i][1], COLS, #32);
  end;
  { Col, Row, Limit, Index, Prefix, Delimiters }
  InsertTo(1, 1, LEFT-1,  '090a', ' '); { ѓЃЂЃз≠л© ®≠§•™б }
  InsertTo(1, 2, LEFT-1,  '090x', ' '); { †ҐвЃаб™®© І≠†™  }
  InsertTo(LEFT, 1, COLS-LEFT-1, '100a', ';'); { †ҐвЃа           }
  InsertTo(LEFT, 2, -COLS, '245a-z', ';');  { Н†ІҐ†≠®•        }
  InsertTo(LEFT, -1, 0, '260', ':-;'); { ВлеЃ§≠л• §†≠≠л• }
  InsertTo(LEFT, -1, 0, '773', ';');  { ИбвЃз≠®™ ®≠дЃађ†ж®® }
  InsertTo(0, -1, 35, '020', ':');  { ISBN            }
  InsertTo(0, -1, 35, '520', ' ');  { А≠≠Ѓв†ж®п       }
  L:= 0;
  for i:= 0 to LAST do begin
    Inc(L, StrLen(StrPCopy(@CA(Dest)[L], S[i])));
    CA(Dest)[L]:= #13;
    CA(Dest)[L+1]:= #10;
    Inc(L, 2);
  end;
  CA(Dest)[L]:= #0;
end;

{ ListField called from BibSimpleList }
procedure ListField(tagdesc: PTagDesc; const fld: String; fldofs, fldfinish: Word; const MarcBuf;
  var Buf; var bufofs: Word);
var
  Count: Word;
  SubFldFinish,
  SubFldOfs: Word;
  L: Word;
  S: String;
begin
  { produce header for field .. }
  count:= fldofs;
  SubFldOfs:= fldofs;
  repeat
    while Count <= FldFinish do begin
      { check: is it start of subfield (1.first subfield 2.following by #31 subfield delimiter)}
      if (count = fldofs) or (TMarcRec(MarcBuf).Data[Count-1] = #31) then begin
        SubFldOfs:= Count;
        Inc(Count);
        Break;
      end;
      Inc(Count);
    end;
    while Count <=  FldFinish do begin
      if (TMarcRec(MarcBuf).Data[Count] = #30) or (TMarcRec(MarcBuf).Data[Count] = #31) then begin
        { end of field or subfield encountoured }
        SubFldFinish:= Count;
        if fld <= '008'
{$IFDEF VER70}
        then S:= tagdesc^.GetTagDescription(fld) + ':'
        else S:= tagdesc^.GetTagDescription(fld+TMarcRec(MarcBuf).Data[SubFldOfs]) + ':';
{$ELSE}
        then S:= tagdesc.GetTagDescription(fld) + ':'
        else S:= tagdesc.GetTagDescription(fld+TMarcRec(MarcBuf).Data[SubFldOfs]) + ':';
{$ENDIF}
        L:= Length(S);
        if L = 1 then Break;
        Move(S[1], CA(Buf)[bufofs], L);
        Inc(bufofs, L);
        if fld <= '008' then begin
          L:= SubFldFinish-SubFldOfs+1-1;
          Move(TMarcRec(MarcBuf).Data[SubFldOfs], CA(Buf)[bufofs], L);
        end else begin
          L:= SubFldFinish-SubFldOfs+1-2;
          Move(TMarcRec(MarcBuf).Data[SubFldOfs+1], CA(Buf)[bufofs], L);
        end;
        S:= #13#10;
        Inc(bufofs, L);
        L:= Length(S);
        Move(S[1], CA(Buf)[bufofs], L);
        Inc(bufofs, L);

        Break;
      end;
      Inc(Count);
    end;
  until Count >=  FldFinish;
end;

procedure BibSimpleList(tagdesc: PTagDesc; const MarcBuf; var Dest);
var
  curdirofs,
  Dirlen, FldLen, FldOfs: Integer;
  p: Word;
begin
  curdirofs:= 0;
  p:= 0;
  { changes Oct 18 1998 - IT IS WRONG!
   DirLen:= GetRecLen(Leader) - $18 - GetDataLen(Leader);
  }
  DirLen:= GetDirLen(MarcBuf);  { +1 byte length terminator }
  while curdirofs < DirLen - 12 do begin
    Move(TMarcRec(MarcBuf).Data[0+curdirofs], marcfldname[1], 3);
    Move(TMarcRec(MarcBuf).Data[3+curdirofs], S4[1], 4);
    FldLen:= StrToInt(S4);
    Move(TMarcRec(MarcBuf).Data[7+curdirofs], S5[1], 5);
    FldOfs:= StrToInt(S5) + GetDataOfs(MarcBuf) - $18;
    ListField(tagdesc, marcfldname, FldOfs, FldOfs + FldLen - 1, MarcBuf, Dest, p);
    Inc(curdirofs, 12);
  end;
  CA(Dest)[p]:= #0;
end;

procedure ExtractFormatDelimiter(var tag, fmt, dlmt: String);
var
  p, p2: Integer;
begin
  p:= Pos('|', tag);
  if p > 0 then begin
    fmt:= Copy(tag, p+1, MaxInt);
    p2:= Pos('|', fmt);
    if p2 > 0 then begin
      dlmt:= Copy(fmt, p2+1, MaxInt);
      Delete(fmt, p2, MaxInt);
    end else begin
      dlmt:= DEFAULTDLMT;
    end;
    Delete(tag, p, MaxInt);
  end else begin
    fmt:= '%s';
    dlmt:= DEFAULTDLMT;
  end;
end;

{$IFNDEF VER70}
{ convert one Marc field to TStrings. Object property keeps TFld4 (field, order, subfield)
}
procedure Field2String(fldorder: Word; const fld: String; fldofs, fldnextofs: Integer;
  const MarcBuf; Dest: TStrings);
var
  Count,
  SubFldFinish,
  SubFldOfs,
  L: Integer;
  S: String;
  FldOrderSubFld: TFld4;
begin
  FldOrderSubFld.f:= StrToIntDef(fld, 0); { field }
  FldOrderSubFld.o:= fldorder;            { field order (for repeatable fields) }
  count:= fldofs;
  repeat
    SubFldOfs:= Count;
    repeat
      if (TMarcRec(MarcBuf).Data[Count] = #30) or (TMarcRec(MarcBuf).Data[Count] = #31) then begin
        { end of field or subfield encountoured }
        SubFldFinish:= Count;
        if fld <= '008' then begin
          L:= SubFldFinish-SubFldOfs+1-1;
          SetLength(S, L);
          Move(TMarcRec(MarcBuf).Data[SubFldOfs], S[1], L);
          FldOrderSubFld.s:= 0;          { no subfield }
        end else begin
          L:= SubFldFinish-SubFldOfs+1-2;
          SetLength(S, L);
          Move(TMarcRec(MarcBuf).Data[SubFldOfs+1], S[1], L);
          FldOrderSubFld.s:= Ord(Upcase(TMarcRec(MarcBuf).Data[SubFldOfs]));  { subfield }
        end;
{$IFDEF STRINGSVALUES}
        { add values part like "090a=" to each line }
        if fld <= '008'
        then S:= fld + '=' + S
        else S:= fld + TMarcRec(MarcBuf).Data[SubFldOfs] + '=' + S;
{$ENDIF}
        Dest.AddObject(S, Pointer(FldOrderSubFld));
        Inc(Count);
        Break;
      end;
      Inc(Count);
    until False;
  until Count >=  fldnextofs;
end;

{ read entire marc record and fill TStringList, for example:
  100a=Author
  Object property as TFld4 (Field number, order(if repeats), Subfield)
  Return qty of fields (0 if nothing)
  return DEST strings sorted (dupAccept).
}
function Flds2Strings(const MarcBuf; Dest: TStrings): Integer;
var
  fldorder,
  curdirofs,
  Dirlen, FldLen, FldOfs: Integer;
  oldmarcfldname: String[3];
begin
  if Dest is TStringList
  then TStringList(Dest).Sorted:= False;
  Dest.Clear;
  oldmarcfldname:= 'xxx';
  fldorder:= 0;           { same to previous, but safe! }
  curdirofs:= 0;
  { DirLen:= GetDirLen(Leader) - GetDataLen(Leader); }
  DirLen:= GetDirLen(MarcBuf);           { +1 byte length terminator }
  while curdirofs < DirLen - 12 do begin { <= if no terminator           }
    Move(TMarcRec(MarcBuf).Data[0+curdirofs], marcfldname[1], 3);
    Move(TMarcRec(MarcBuf).Data[3+curdirofs], S4[1], 4);
    FldLen:= StrToInt(S4);
    Move(TMarcRec(MarcBuf).Data[7+curdirofs], S5[1], 5);
    FldOfs:= StrToInt(S5) + GetDataOfs(MarcBuf) - $18;
    if oldmarcfldname <> marcfldname then begin
      fldorder:= 0;
      oldmarcfldname:= marcfldname;
    end;
    Field2String(fldorder, marcfldname, FldOfs, FldOfs + FldLen, MarcBuf, Dest);
    Inc(curdirofs, 12);
    Inc(fldorder);
  end;
  if Dest is TStringList then begin
    TStringList(Dest).Duplicates:= dupAccept;
    TStringList(Dest).Sorted:= True;
  end;
  Flds2Strings:= Dest.Count;
end;

{ return  new offset in MarcBuf = (fldofs + stored bytes len)
}
function String2Field(Src: TStrings; var SrcIndex: Integer; var fldofs: Integer;
  var MarcBuf): String;
var
  len: Integer;
  fldstr, curfldstr: String[3];
  subf: String[1];
  vl: String;
begin
{$IFDEF VER80}
  fldstr:= Copy(Src[SrcIndex], 1, 3);
{$ELSE}
  fldstr:= Copy(Src.Names[SrcIndex], 1, 3);
{$ENDIF}
  if fldstr <= '008' then begin
    vl:= Copy(Src[SrcIndex], 3 + 2, MaxInt)+#30;
    len:= Length(vl);
    Move(vl[1], TMarcRec(MarcBuf).Data[fldofs], len);
    Inc(SrcIndex);
    Inc(fldofs, len);
  end else begin
    repeat
      if SrcIndex >= Src.Count
      then Break;
{$IFDEF VER80}
      curfldstr:= Copy(Src[SrcIndex], 1, 3);
      subf:= Copy(Src[SrcIndex], 4, 1);
{$ELSE}
      curfldstr:= Copy(Src.Names[SrcIndex], 1, 3);
      subf:= Copy(Src.Names[SrcIndex], 4, 1);
{$ENDIF}
      if (subf = '') or (curfldstr <> fldstr)
      then Break;
      vl:= subf + Copy(Src[SrcIndex], 4 + 2, MaxInt) + #31;
      len:= Length(vl);
      { Move(Pointer(vl)^, TMarcRec(MarcBuf).Data[fldofs], len); }
      Move(vl[1], TMarcRec(MarcBuf).Data[fldofs], len);
      { CHANGED Feb 04 2002 !!! }
      Inc(fldofs, len);
      Inc(SrcIndex);
    until False;
    TMarcRec(MarcBuf).Data[fldofs-1]:= #30; { исправить разделитель на терминатор }
  end;
  String2Field:= fldstr;
end;

{ store entire marc record by TStringList contents
  Return size of bytes stored in Marc buf (0 if nothing)
}
function Strings2Fld(Src: TStringList; var MarcBuf): Integer;
var
  curdirofs,
  fldscount,
  dataofs,
  FldLen,
  OldOfs,
  FldOfsInMarcBuf,        { FldOfsInMarcBuf - смещение относительно начала marcBuf }
  FldOfs: Integer;        { fldofs - смещение относительно dataofs }
  fldstr, old: String[4];
  s: String[6];
  index, i: Integer;
begin
  { FillChar(MarcBuf, 500, #0); }
  { подсчитать пол€ (*12=размер директории) }
  fldscount:= 0;
  old:= 'xxx';
  for index:= 0 to Src.Count - 1 do begin
{$IFDEF VER80}
    fldstr:= Copy(Src[index], 1, 3);
{$ELSE}
    fldstr:= Copy(Src.Names[index], 1, 3);
{$ENDIF}
    if fldstr = old
    then Continue;
    old:= fldstr;
    Inc(fldscount);
  end;
  { размер директории кратен числу полей,  }
  dataofs:= fldscount * (3+4+5);         { + терминатор директории }
  { поставить терминатор директории }
  { ?!! }
  TMarcRec(MarcBuf).Data[dataofs]:= Char($1E);    { +размер leader ($18) - это смещ. данных }
  Inc(dataofs);
  index:= 0;
  fldofs:= 0;
  FldOfsInMarcBuf:= dataofs;
  curdirofs:= 0;
  for i:= 1 to fldscount do begin
    { сохранить предыдущее смещение }
    OldOfs:= FldOfsInMarcBuf;
    { следующее смещение }
    fldstr:= String2Field(Src, index, FldOfsInMarcBuf, MarcBuf);
    { длина пол€- разница между новым и старым смещени€ми }
    FldLen:= FldOfsInMarcBuf - OldOfs;
    { заполнить смещение }
    S5:= LeadZero(IntToStr(FldOfs), 5);
    S4:= LeadZero(IntToStr(FldLen), 4);
    { след. смещение дл€ записи в директорию }
    Inc(FldOfs, FldLen);
    { заполнить директорию }
    Move(fldstr[1], TMarcRec(MarcBuf).Data[0+curdirofs], 3); { номер пол€  }
    Move(S4[1], TMarcRec(MarcBuf).Data[3+curdirofs], 4);     { длина пол€  }
    Move(S5[1], TMarcRec(MarcBuf).Data[7+curdirofs], 5);     { смещение данных пол€ }
    Inc(curdirofs, 12);
  end;
  { FldOfsInMarcBuf указывает на терминатор записи }
  TMarcRec(MarcBuf).Data[FldOfsInMarcBuf]:= EOD;             { $1D }
  { PChar #0 }
  TMarcRec(MarcBuf).Data[FldOfsInMarcBuf+1]:= #0;
  Inc(FldOfsInMarcBuf);                           { +терминатор записи  }
  { заполнить leader }
  Inc(FldOfsInMarcBuf, $18);                      { исправим FldOfsInMarcBuf - размер }
  with TMarcLeader(MarcBuf) do begin
    s:= LeadZero(IntToStr(FldOfsInMarcBuf), 5);
    Move(s[1], Len, 5);
    State:= 'n';
    UseCodes:= 'am  ';
    IndicatorLen:= '2';
    SubFieldIdLen:= '2';
    Inc(dataofs, $18);                            { +leader  }
    s:= LeadZero(IntToStr(dataofs), 5);
    Move(s[1], Base, 5);
    User:= '   ';
    EntryLen:= '4';
    FirstCharPosLen:= '5';
    Reserved:= '  ';
  end;
  Strings2Fld:= FldOfsInMarcBuf;
end;

const
  TAGMAX = 8192;  { maximum length of tag (allocate string buffer) }

{ Usage:
  Bib2HTML(TMarcLeader(Pointer(AMarcBuf)^),
    Pointer(Cardinal(Pointer(AMarcBuf))+$18)^, AHtmlForm, Result);
}
procedure Bib2HTML(const MarcBuf; const tagform: String; Context: TStrings; var Dest: String);
var
  i, L, LL: Word;
  tag, fmt, dlmt: String;
  tagencountoured: Integer;
begin
  dest:= '';
  tagencountoured:= 0;
  for i:= 1 to Length(tagform) do begin
    case tagform[i] of
    '<':begin
          Inc(tagencountoured);
          if tagencountoured = 1
          then tag:= ''
          else tag:= tag + '<'
        end;
    '>':begin
          Dec(tagencountoured);
          if tagencountoured = 0 then begin  //
            { do smth with tag }
            if Pos(TAGPREFIX, tag) = 1 then begin
              Delete(tag, 1, TAGPREFIXLEN); { delete '#' from '#100a..'}
              ExtractFormatDelimiter(tag, fmt, dlmt);
              L:= Length(dest);
{$IFDEF VER80}
              Byte(dest[0]):= L + TAGMAX;
{$ELSE}
              SetLength(dest, L + TAGMAX);
{$ENDIF}
              {
              if Length(tag) = 1 then begin
                if Assigned(Context)
                then tag:= Context.Values[tag]
                else tag:= '';
                LL:= Length(tag);
                // if LL > 0 then
                Move(tag[1], Dest[L+1], LL);
              end else }
              LL:= TagValueFmt(tag, fmt, dlmt, MarcBuf, dest[L+1]);
{$IFDEF VER80}
              Byte(dest[0]):= L + LL;
{$ELSE}
              SetLength(dest, L+LL);
{$ENDIF}
            end else begin
              dest:= dest + '<' +tag + '>';
            end;
          end else begin
            tag:= tag + '>';
          end;
        end;
    else begin
        if tagencountoured > 0 then begin
          tag:= tag + tagform[i];
        end else begin
          dest:= dest + tagform[i];
        end;
      end;
    end;
  end;
end;
{$ENDIF}

{ look pchar tagform and produce pchar dest, replace <~100a> tags to appropriate values,
  Values retivies from MarcBuf
  if Passthru parameter is true, any other tags stored in dest, else- remove them
}
procedure Bib2HTML0(const MarcBuf; const tagform; var Dest; PassThru: Boolean);
var
  i, j, L: Word;
  tag, fmt, dlmt: string;
  tagencountoured: Integer;
begin
  i:= 0;
  j:= 0;
  tagencountoured:= 0;
  tag:= '';
  while CA(tagform)[i] <> #0 do begin
    case CA(tagform)[i] of
    '<':begin
          Inc(tagencountoured);
          if tagencountoured = 1
          then tag:= ''
          else tag:= tag + '<'
        end;
    '>':begin
          Dec(tagencountoured);
          if tagencountoured = 0 then begin
            { do smth with tag }
            if Pos(TAGPREFIX, tag) = 1 then begin
              Delete(tag, 1, TAGPREFIXLEN); { delete '~' from '~100a'}
              ExtractFormatDelimiter(tag, fmt, dlmt);
              L:= TagValueFmt(tag, fmt, dlmt, MarcBuf, CA(dest)[j]);
              Inc(j, L);
            end else begin
              if PassThru then begin
                L:= Length(tag)+2;
                Move(CA(tagform)[i-L+1],CA(dest)[j], L);
                Inc(j, L);
              end;
            end;
          end else begin
            tag:= tag + '>';
          end;
        end;
    else begin
        if tagencountoured > 0 then begin
         tag:= tag + CA(tagform)[i];
        end else begin
          CA(dest)[j]:= CA(tagform)[i];
          Inc(j);
        end;
      end;
    end;
    Inc(i);
  end;
  CA(Dest)[j]:= #0;
end;

{ extract field and subfield from string. If no subfield, return #0 in sfld
  If S is invalid string, retuen False
}
function ExtractFldSubFld(S: String; var fld, sfld: Integer): Boolean;
var
  fldstr: String[4];
  sfldstr: String[1];
begin
  ExtractFldSubFld:= False;
  fld:= 0;
  sfld:= 0;

  fldstr:= Copy(S, 1, 3);
  sfldstr:= Copy(S, 4, 1);
  if IsDecimal(fldstr) then begin
    fld:= StrToIntDef(fldstr, 0);
    if Length(sfldstr) = 1
    then sfld:= Ord(sfldstr[1]);
    ExtractFldSubFld:= True;
  end;
end;

{ construct field name from field number and subfield. If no subfield, return #0 in sfld
  if sfld = 0, no subfield adds
}
function MkFldSubFld(fld, sfld: Integer): String;
var
  S: String[4];
begin
  S:= LeadZero(IntToStr(fld), 3);
  if sfld > 0
  then S:= S + Chr(sfld);
  MkFldSubFld:= S;
end;

{ TFld4StringList class implementation ----------------------------------------}
{$IFNDEF VER70}
procedure TFld4StringList.ExchangeItems(Index1, Index2: Integer);
var
  Temp: String;
  TempObj: Pointer;
begin
  Temp:= Strings[Index1];
  Strings[Index1]:= Strings[Index2];
  Strings[Index2]:= Temp;
  TempObj:= Objects[Index1];
  Objects[Index1]:= Objects[Index2];
  Objects[Index2]:= TempObj;
end;

procedure TFld4StringList.QuickSortFld4(L, R: Integer);
var
  I, J: Integer;
  P: Integer;
begin
  repeat
    I:= L;
    J:= R;
    P:= Integer(Objects[(L + R) shr 1]);
    repeat
      while Integer(Objects[I]) < P do Inc(I);
      while Integer(Objects[J]) > P do Dec(J);
      if I <= J then begin
        ExchangeItems(I, J);
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then QuickSortFld4(L, J);
    L := I;
  until I >= R;
end;

procedure TFld4StringList.Sort;
begin
  if (not Sorted) and (Count > 1) then begin
    Changing;
    QuickSortFld4(0, Count - 1);
    Changed;
  end;
end;
{$ENDIF}

function ReadKey(AStrmKeys: TStream; AKeyNo: Cardinal; var BufKey; MaxBufLen: Integer): Boolean;
var
  kofs: Cardinal;
  len: Byte;
begin
  Result:= False;
  try
    with AStrmKeys do begin
      Position:= (AKeyNo + 1) * SizeOf(Cardinal);
      Read(kofs, SizeOf(Cardinal));
      Position:= kofs;
      Read(len, 1);
      Read(BufKey, len);
    end;
    CA(BufKey)[len]:= #0;
    Result:= True;
  finally
  end;
end;

begin
  MarcStartFOffset:= 0;
  marcfldname:= '123';
  S4:= '1234';
  S5:= '12345';
end.
