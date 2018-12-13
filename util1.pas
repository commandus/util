unit Util1;
(*##*)
(*******************************************************************************
*                                                                             *
*   U  T  I  L  1      collection of string routines for Delphi                *
*                                                                             *
*   Copyright © 1998- 2001, Andrei Ivanov. All rights reserved.                *
*   Based on strutil unit (part of VGAX graphic library) for BP7              *
*   Conditional defines: WIN32 VER80_32 VER90                                  *
*                                                                             *
*   Last Revision: Mar 19 1998                                                 *
*   Last fixes   : Sep 10 2001 -> utillog                                     *
*                : Nov 17 2001 ParseURL                                        *
*                : Dec 17 2001 DiffPath use ../ sequences                     *
*                : Apr 16 2002                                                 *
*                                                                             *
*   Lines        : 4771                                                        *
*   History      : see CHANGES.TXT file                                       *
*   Printed      : ---                                                         *
*                                                                             *
********************************************************************************)
(*##*)

{  define
}
{$IFDEF WIN32}
{$DEFINE VER80_32}
{$ENDIF}
{$IFDEF VER90}
{.$DEFINE VER80_32}
{$ENDIF}
{$IFNDEF VER80} { Delphi 1.0}
{$IFNDEF VER90} { Delphi 2.0}
{$IFNDEF VER93} { C++Builder 1.0}
{$IFNDEF VER100} { Borland Delphi 3.0}
{$DEFINE D4_} { Delphi 4.0 or higher}
{$ENDIF}
{$ENDIF}
{$ENDIF}
{$ENDIF}

interface
uses
  { нельзя одновременно объявлять и DDENam и EDDEMan }
  SysUtils, Classes, Types,  jclUnicode
{$IFNDEF VER80}
  ,WinSock, Math, StrUtils
{$ENDIF}
;
{$IFDEF VER80}
type
  ANSIString = String;
  Cardinal = LongInt;

{$ENDIF}

{$IFDEF VER100}
type
  EExternal = Exception;
{$ENDIF}

type
  FNameStr = String;
  DirStr  = FNameStr;
  NameStr = FNameStr;
  ExtStr  = FNameStr;
  PathStr = FNameStr;

{ opendb unit
}
const
  DBdlg_ShowAlias      = 1;
  DBdlg_ShowTable      = 2;
  DBdlg_CheckIntegrity = 3;

type
{$IFNDEF VER80_32}
  ShortString = String;
{$ENDIF}
  TBS = set of byte;
  CA  = array[0..40000] of Char;
  BA  = array[0..40000] of Byte;
  WA  = array[0..20000] of Word;
  IA  = array[0..20000] of SmallInt;
  LA  = array[0..10000] of LongInt;
  TCS = set of Char;

  CA3 = array [0..2] of Char;
  CA4 = array [0..3] of Char;
  CA5 = array [0..4] of Char;

  TFixStrings = class(TObject)
    private
    StrBuf: Pointer;
    BufLen,
    LinesQTY,
    StrOfLen: LongInt;
    function  Get(Index: SmallInt): ShortString; virtual;
    function  GetValuable(Index: SmallInt): ShortString; virtual;
    procedure Put(Index: SmallInt; const S: ShortString); virtual;
    public
    Exists: TBS;
    FilledLines: SmallInt;
    { for debug information purposes only
    }
    NameOfList: String[12];
    function Adresses(Index: SmallInt): Pointer;
    property Strings[Index: SmallInt]: ShortString read Get write Put; default;
    property Valuables[Index: SmallInt]: ShortString read GetValuable;
    constructor Create(ALines, ALen: Word);
    destructor  Destroy; override;
    function    ValuableIndex(Index: SmallInt): SmallInt;
    procedure   Clear;
    procedure   EraseString(Index: Word);
    procedure   MarkNotExist(Index: Word);
  end;

  TDelimitedStrings = class(TPersistent)
  private
    FInfos: TStringList;
    procedure SetInfo(AHsocket, no: Integer; AInfo: String);
    function  GetInfo(AHsocket, no: Integer): String;
    function  GetIndex(AHsocket: Integer): Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(AHsocket: Integer);
    procedure Delete(AHsocket: Integer);
    property  Info[AHsocket, No: Integer]: String read GetInfo write SetInfo;
  end;

  { not implemented }
  TNumberedTextFile = class(TObject)
    private
    Mode: Word;
    Stream: TFileStream;
    public
    EndOfFile: Boolean;
    constructor Create(FN: ShortString; AMode: Word);
    function    GetLines: LongInt;
    procedure   ReadLn(var S: ShortString);
    procedure   ReadList(var SL: TStringList);
    destructor  Done;
  end;

LogProc = procedure(const S: ShortString);
{ callback- функция для ReplaceTag }
TChecktag = function (Tag: String): String;

{ VER80 32bit support routines
}
{$IFNDEF VER80_32}
procedure SetLength(var S: ShortString; Len: Byte);
{$ENDIF}
type
  StateBarproc = procedure(const S: String);

type
  TBitSet = set of 0..31;
{ bits }
{ extract integer from specified bits (from lowest to highest) }
function ExtractBits(L: Integer; Bits: TBitSet): Integer;
{ set integer in specified bits (from lowest to highest) }
procedure SetBits(L: Integer; Bits: TBitSet; var R: Integer);

{ minmax functions }
function MaxFrom2(V1, V2: LongInt): LongInt;

{$IFNDEF VER80}

{  x:    0-1 2-3 4-7 8-F 10-1F ..
   exp2:  0   1   2   3    4   ..
}
function exp2(x: Word): Word;

{ x      :   0-1 2-3 4-7 8-F 10-1F ..
  near2m :    1   2   4   8    10  ..
}
function near2m(x: Word): Word;
{$ENDIF}

function File2String(const AFN: String): String; overload;
function File2String(const AFN: WideString): WideString; overload;
function String2File(const AFN: String; const S: String; ARewrite: Boolean = False): Boolean; overload;
function String2File(const AFN: WideString; const S: WideString; ARewrite: Boolean = False): Boolean; overload;

function SortFile(const AFN: String; AReserved: Integer): Boolean;
function CreateTemporaryFileName(const APrefix: String): String;

{$IFNDEF VER80}
procedure ANSI2OEM(var AData: String);
procedure OEM2ANSI(var AData: String);

{ convert text file AFN to oem character set }
function CvtFile2OEM(AFN: String): Boolean;
{$ENDIF}

{ file routines
}
function IsValidURL(AURL: String): Boolean;
function isValidURLs(AURL: String): Boolean;
// function IsIPaddress(const host: String): Boolean;
function  IP2Str(AIP: Cardinal): string;
{$IFNDEF VER80}
function  GetBias: Integer;
function MkAbsoluteURL(Aroot, Aurl: String): String;
function ConcatAliasPath(const Aliases: TStrings; const DefPath: String; FileName: String): String;
{$ENDIF}
{
function ParseUrl(url: String; var protocol, user, password, host, IPaddress, fn, bookmark: String; var port: Integer;
  const ADefprotocol: String; ADefPort: Integer): Boolean;
}
{ compose http, ftp urls }
// function ComposeUrl(const AProtocol, AUser, APassword, AHost, AFn, ABookmark: String; Aport: Integer): String;
{ compose ldap url }
{
function ComposeLdapUrl(const AProtocol, AUser, APassword, AHost,
  ADn, AAttr, AScope, AFilter: String; Aport: Integer): String;
}
{ substitute anonymous user and password if user does not specified }
{
function ParseFtpUrl(url: String; var protocol, user, password, host, IPaddress, fn, bookmark: String; var port: Integer;
  const ADefprotocol: String; ADefPort: Integer): Boolean;
}
{ parse ldap url with default substitutes }
{
function ParseLdapUrl(url: String; var protocol, user, password, host, baseDN, Attributes, Scope, Filter: String; var port: Integer;
  const ADefprotocol: String; ADefPort: Integer): Boolean;
}
{ replace DN in ldap url, return True if success }
// function ReplaceDNInLdapUrl(const ANewDN: String; const AUrl: String): String;

// is url started with 'ftp://'
//function IsFtpUrl(const AUrl: String): Boolean;
// is url started with 'ldap://'
// function IsLdapUrl(const AUrl: String): Boolean;

{ validate url and if it is relative path, add root }
function ExtractUrlAddress(url: String): String;
function ExtractUrlProtocol(url: String): String;
function ExtractUrlHost(url: String): String;
function ExtractUrlPort(url: String): Integer;
function ExtractUrlFileName(url: String): String;
function GetFileNameOnly(const s: String): String;

function IsFileMask(const FN: ShortString): Boolean;
function ConcatPath(const Path: String; FileName: String; APathDelimiter: Char = #0): String;
function httpConcatPath(ARoot, FileName: String): String;
{$IFNDEF VER80}
function HTTPDecode(const AStr: String; AUtf8: Boolean): WideString;
function HTTPEncode(const AStr: WideString; AUtf8: Boolean): String;
function HTTPEncode2(const AStr: String): String;
{$ENDIF}

function ReplaceExt(Ext: ExtStr; Pat: PathStr): String;
function ExtractFileNameWOext(AFn: String): String;

{ if file or folder "NESTED" is nested in "PARENT" folder return TRUE
}
function IsFolderParent(Parent, Nested: String): Boolean;
function DiffPath(Parent, Nested: String): String;
function AnsiDiffText(AS1, AS2: String): Integer;
function FilesSimilar(S1, S2: String): Boolean;
{ возвращает количество файлов, удовлетворяющих маске }
function CountFilesLike(AMask: String): Integer;
{ удаляет файлы по маске. Возвращает кол-во удаленных файлов }
function DeleteFiles(AMask: String): Integer;
{ путь DOS или unix (или URL)}
function IsDosPath(const APath: String): Boolean;
{ абсолютный или относительный путь }
function IsAbsolutePath(const APath: String): Boolean;
{ <--> ExtractRelativePath
}
function ExpandRelativePath(const BaseName, AFN: String): String;
function DirExists(fn: String):  Boolean;
function UnixPathToDosPath(const Path: string): string;
function DosPathToUnixPath(const Path: string): string;

type
  TOnProcess_File = function (const FN: String; AEnv: TObject = Nil): Boolean;
{ AFilePattern must be file mask like '*.*' }
procedure Walk_Tree(AFilePattern, Start: String; Attr: Integer; Recursive: boolean;
  DoIt: TOnProcess_File; AEnv: TObject);

procedure Walk_DirTree(Start: String; DoIt: TOnProcess_File;
  AEnv: TObject);

{ Transliterate routines
}
procedure strxlat(var xlat; var S: String);
procedure strxlat0(var xlat; var S);

{ возвращает латинский аналог буквы кириллицы и наоборот
  кир лат
  A   A
  Б   B, etc
}
function CyrLatEQ(Key: Char): Char;

{ пн - 0 вт - 1 вс -6 }
function CyrDayOfWeek(Dt: TDateTime): Integer;

{ String routines
}
function SkhUpperCase(IC: Char): Char;
function SkhLowerCase(IC: Char): Char;

{ for ex: '&File' -> 'File' }
{ Char Return
  A    A
  #1  ^A
  #13 ^M
  #27 ^[
}
function ControlChar(Ch: Char): String;
function ControlString(S: String): String;

{ преобразует строку вида '^MAssa' в '#13Assa'
}
function   ParseControlCode(S: String): String;

function   DeleteChars(Ch: Char; const S: String): String;
function   Hex8(S: ShortString; var AA): LongInt;
{ преобразует число в представление знаками 0-9;A-z }
function   Int2Alpha(AValue: Integer): String;
function   Alpha2Int(const AValue: String): Integer;
function   IsExtendedASCII(S: String): Boolean;
function   IsNotBlank(const S: ShortString): Boolean;
function   IsNotBlank0(const S: PChar): Boolean;
function   IsEmptyString(S: String): Boolean;
procedure  DeleteLeadSpaceStr(var S: String);
procedure  DeleteLeadTerminateSpaceStr(var S: String); overload;
procedure  DeleteLeadTerminateSpaceStr(var S: WideString); overload;
procedure  DeleteDoubledSpaceStr(var S: String); overload;
procedure  DeleteDoubledSpaceStr(var S: WideString); overload;
procedure  DeleteLeadTerminateDoubledSpaceStr(var S: String); overload;
procedure  DeleteLeadTerminateDoubledSpaceStr(var S: WideString); overload;

{ удаляет все управляющие символы кроме пробела }
procedure  DeleteControlsStr(var S: String);
{ удаляет все управляющие символы кроме пробела, а CRLF -> пробел }
procedure  DeleteControlsStrCRLF2Space(var S: String); overload;
procedure  DeleteControlsStrCRLF2Space(var S: WideString); overload;
{ проверяет наличие префикса Prefix и суффикса Suffix и возвращает параметр без них }
function CheckParameter(const Prefix: ShortString; const Quotes: ShortString;
  var AParameter: String; const Suffix: ShortString):Boolean;

{ parse SS string and return words from STARTPOSITION
  (if possible) no more than LessThanBytes charactters of them
  return new STARTPOSITION }
function GetLimitQtyWords(const S: String; var StartPosition: Integer;
  LessThanBytes: Integer): String;

{ parse SS string and return words from STARTPOSITION
  (if possible) no more than LessThanBytes charactters of them
  return new STARTPOSITION
  Rerurn: TRUE  - parsed
          FALSE - no more words (empty string : ALL var undefined!!!
                  NOT: StartPosition > LastPosition=Length(S))
}
function LimitQtyWordsPos(const S: String; var StartPosition: Word;
  var LastPosition: Word; LessThanBytes: Integer): Boolean;

function   TryReadDecimal(S: String; Bytes: Byte; var Res: Integer): Integer;
{ replace  '|' to ':' (SQL!) }
function   CopyColon(S: String; Index, Count: Integer): String;
{ change chars, NOT return number of replacements
}
function  ChangeChars(src, dest: Char; const S: String): String;
{ change chars, return number of replacements
}
function   ChangeChar(src, dest: Char; var S: String): Byte;
{ return position on next srch substring
  if not found, return 0
}
function   PosFrom(From: Integer; const srch, where: String; ACaseSensitive: Boolean = True): Integer;
{ Pos except search from back
}
function   PosBack(const srch, where: String): Integer; overload;
function   PosBack(const srch, where: WideString): Integer; overload;

{ NotQuotedPos is Pos but it check is search character is in " or '
}
function   NotQuotedPos(const ASrchChar, AOChar, ACChar: Char; const where: String): Integer;
{ Pos except search from back
}
function   PosBackFrom(from: Integer; const srch, where: String): Integer;
{ return lowest byte from set, but search start from start
}
function   LowestInSet(var setofbyte; start: Byte): Byte;
{ return set of characters presents in string
}
procedure String2SetOfChar(AString: String; var AsetOfChar);

{ return TRUE if string is Longint decimal representation
}
function   isDecimal(const S: String): Boolean;

{ return TRUE if substring is Longint decimal representation
}
function   PartisDecimal(b, e: Integer; const S: String): Boolean;

{ return TRUE if string is Longint hexadecimal representation
}
function   isHexaDecimal(const S: String): Boolean;

{ return TRUE if string is Longint Pascal hexadecimal or decimal representation
}
function   isPascalInt(const S: String): Boolean;

{ return TRUE if string is Longint hexadecimal representation
}
function   PartisHexaDecimal(b, e: Integer; const S: String): Boolean;

{ 12A&*34 -> 1234;   100% -> 100
}
function GetOnlyDecimalDigits(const S: String): Longint;

{ тоже, но возвращает строку
}
procedure SaveDecimalDigitsOnly(var S: String);
{ select YYY from XXYYYXXX in decimal
  Positions must be set of Byte
}
function   DecimalDigits(V: LongInt; var Positions): LongInt;

{ &#123;
  return 1..3 - qty of decimal digits in TOKENSTRING terminated by ';' character
  or 0 if no digits terminated by ';' found
}
function DigitsTerminatedBySemicolonQTY(const TokenString: String; Position: Integer): Integer;

{ return character from POSITION in TOKENSTRING
  Character- char type
             ASCII extended ASCII character code in WEB- like notation (&#xxx;)
  automatically search an &#xxx; expression and return appropriate character
  return POSITION of next character
}
function GetWEBStyleCharFromString(TokenString: String; var Position: Integer): Char;

{ Translate ASCII extended ASCII character code in WEB- like notation (&#xxx;)
  in TOKENSTRING
}
function WEBStyleString2ASCII(TokenString: String): String;

{ parse html character entity: &lt; &gt; &amp; &quot; &#123;}
{ Translate ASCII extended ASCII character code in WEB- like notation (&#xxx;)
  in TOKENSTRING and Control char ^C
}
function WEBStyleAndControlString2ASCII(TokenString: String): String;

function ASCII2HTML(TokenString: String): String;

{ 1-> 1, 9->9, 10-> A, (0..'Z')
  Digits- отсекает первые оставляет последние
}
function IntToAplhaNumeric(Value: Byte; Digits: Integer): String;
{ -1 if failed }
function AplhaNumericToInt(Value: String): Integer;

{ 0->A, 1-> B, ('A'..'Z')
  Digits- отсекает первые оставляет последние (если 0- возвращается пустая строка )
}
function IntToAplha(Value: Byte; Digits: Integer): String;
{ -1 if failed }
function AplhaToInt(Value: String): Integer;

{ TStrings routines }
procedure DeleteLeadTerminateSpaceSL(S: TStrings);
function MkStringsByComma(S: String): TStrings;
procedure ValidateQuoteStringValues(R: TStrings);

{ TStrings translate coordinates }

function SLTranslateLines2Ofs(AStrings: TStrings; const APos: TPoint): Cardinal; overload;
function SLTranslateOfs2Pos(AStrings: TStrings; AOfs: Cardinal): TPoint; overload;
function SLTranslateLines2Ofs(AStrings: TWideStrings; const APos: TPoint): Cardinal; overload;
function SLTranslateOfs2Pos(AStrings: TWideStrings; AOfs: Cardinal): TPoint; overload;

{ ищет в отсортированном списке строк строку, наиболее близкую S
}
function FindSortedStrings(Strings: TStrings; const S: String): Integer;

{ Если в TStrings несколько строк с одинаковым именем значения
  получить значение (не только первое, но и последующие)
  extract next value (with similar name) from TStrings
  Parameter: N= 0.., where N=0 - first occurance
             Name - name of value
             R- value, '' if -1 returned
  Return -1 if does not exists or 0.. index of N occurance
}
function NextValue(N: Integer; Name: String; S: TStrings; var R: String): Integer;

{ ищет в отсортированном списке строк строку, начинающуюся с S
}
function FindExactStartSortedStrings(Strings: TStrings; const S: String): Integer;
{ -1 if no
}
function FirstRemarkLine(const RemStart: ShortString; SL: TStrings; from: Integer): Integer;
{ -1 if no
}
function LastRemarkLine(const RemStart, RemFinish: ShortString; SL: TStrings; from: Integer): Integer;
{ for compatibility only }

{ text line routines }
function    LinesInFile(const FN: String): LongInt;
function    LinesInLongFile(FN: ShortString): LongInt;
function    GetFileLine(FileName: ShortString; line: LongInt): ShortString;
function    RecordLinesInLongFile(Nums, NumFLen: Byte; FN: ShortString;
  ProgressIndicator: StateBarproc): LongInt;
function    NotEmptyLinesInLongFile(FN: ShortString): LongInt;

function    BlankStringTo(const S: ShortString; blanks: Word): ShortString;
function    BlankToZero(const S: ShortString): ShortString;
{ заполняет лидирующими нулями строку до Bytes }
function    LeadZero(const S: String; Bytes: Byte): String;
{ copy file src to dest
  bytes in source file is returned, if faulkt - less 0
}
function    CopyFile(const SrcN, DestN: ShortString): LongInt;
procedure   UpCaseBuffer(Data: array of Char; Len: Word);
function    PCountWords(var Len: LongInt; P: PChar): LongInt;
function    PosCharsFrom(From: Word; srch: ShortString; s: String): Word;
function    GetToken(No: Word; const delimiter, S: String): String;
function    GetWord(var S: String; var WordLen: Integer): String;

{ возвращает отделенное упр. символами и возвращает в p начало следующего }
function ExtractToken(const S: String; var p: Integer): String;
function ExtractTokenBack(const S: String; var p: Integer): String;
function    TokenStart(No: Word; const delimiter, S: String): Integer;
function    TokensTail(No: Word; const delimiter, S: String): String;
procedure   SetToken(No: Word; const delimiter, Value: String; var S: String);

{ возвращает номер строки Srch в списке SepList с разделителями Sep, с 1.
  Если не найдено, 0
}
function    OrderInSeparatedList(const Srch, SepList, Sep: String): Integer;
function    EraseChars(const Chars: TCS; const S: ShortString): ShortString;
function    EraseCharsUP(const Chars: TCS; const S: ShortString): ShortString;
{ заменяет первое вхождение Fnd на Repl в строке Dest
  Dest- целевая строка с макросами(?)
  CaseSens: заменять чувствительно к регистру символов
  Fnd  - что искать
  Repl - на что заменять
  возвращает
  True - замена произведена
  False- нет больше вхождений
}
function    ReplaceStr(var Dest: String; CaseSens: Boolean; const Fnd, Repl: String): Boolean; overload;
function    ReplaceStr(var Dest: WideString; CaseSens: Boolean; const Fnd, Repl: WideString): Boolean; overload;

{ Dest- строка с тегами
  Prefix, Suffix- начало и конец тега
  Quote- обычно символ '"', если #0- нет строковых литералов
  возвращает число замен
  callback- функция для ReplaceTag
  TChecktag = function (Tag: String): String;
}
function    ReplaceTag(var Dest: String; const Prefix, Suffix: String; Quote: Char; ReplFunc: TChecktag): Integer;

{ get ShortString from buffer
}
function    ExtractString(var B; const Len: Word): ShortString;
procedure   FillMemo(SL: TStrings; var B; const Len: Word);
{ set
}
function ReadSet(const S: ShortString): TBS;
{ return set as longint
}
function SetAsLong(var B; Bytes: Byte): LongInt;
{ marc field conversion
}
function ExtractLongInt(var B; const Len: Word): LongInt;
function ExtractExtended(var B; const Len: Word): Extended;
function ExtractCurrency(const S: ShortString): Extended;
function ExtractDateTime(const S: ShortString): Extended;

function ExtractCurrencyFromBuf(var B; const Len: Word): Extended;
function ExtractDateTimeFromBuf(var B; const Len: Word): Extended;
{ compress siglas list like set
  Return: length of compressed list -
  !!!BUG - result is qty of delimiters found in +1
}
function CompressList(CompCAPtr: Pointer; PrefixLen: SmallInt): SmallInt;

{ date
}
procedure CyrDates;
{ возвращает год из даты/времени }
function  GetYear(Tm: TDateTime): Integer;
{ возвращает номер месяца из даты/времени }
function  GetMonth(Tm: TDateTime): Integer;
{ возвращает число месяца из даты/времени }
function  GetDay(Tm: TDateTime): Integer;
{ возвращает час из даты/времени }
function  GetHour(Tm: TDateTime): Integer;
{ устанавливает новое значение часа }
procedure SetHour(var Tm: TDateTime; NewHour: Integer);
{ возвращает минуты из даты/времени }
function  GetMin(Tm: TDateTime): Integer;
{ устанавливает новое значение часа }
procedure SetMin(var Tm: TDateTime; NewMin: Integer);
{ инкоемент/декремент на заданное время  }
procedure IncTime(var Tm: TDateTime; AddHour, AddMinutes: Integer);
{ округляет время, если ноль- не огругляет }
procedure RoundTime(var Tm: TDateTime; AHour, AMinutes: Word);
{$IFNDEF VER80}
function ParseHTTPDate(const DateStr: string): TDateTime;

function DateTimeGMTToHttpStr(const GMTValue: TDateTime) : String;
function DateTimeToInternetStr(const Value: TDateTime; const AIsGMT : Boolean = False) : String;// should adhere to RFC 2616
function StrInternetToDateTime(Value: string): TDateTime;

{$ENDIF}

{ Boolean
}
function YesNoS(Yes: Boolean): ShortString;

{ unix&dos file name
}
function ExtractUnixFileName(AFn: String): String;
function ExtractUnixDosFileName(AFn: String): String;

{$IFNDEF VER80}
{ ExecuteFile описан в справочной системе, но эта функция не реализована так что затычка на букву E }
function EExecuteFile(AFileName: String): Boolean;
{$ENDIF}

{ Error messaging
}
function WinExecErr(No: Word): ShortString;

function GetLastErrorDescription(AError: DWORD): WideString;

{ database BMP has 8 byte length header.
  This routine erase header
}
procedure GraphicFieldToBMP(const FN: String);

{$IFNDEF VER80}
{ registry functions }
function  GetRegString(ARoot: Cardinal; const APath, APar, DefVal: String): String;
procedure SetRegString(ARoot: Cardinal; const APath, APar, AValue: String);
function ReadLocalTCPName: String;
{ is dial-up ip connction present? }
function IsIPPresent: Boolean;
function ReadIEProxySettings(AProtocol: String; ARemoteHost: String; var AProxy: String; var APort: Integer): Boolean;

{ читает полный раздел из реестра (HKEY_LOCAL_MACHINE). Все параметры должны быть строками
}
function AddEntireKey(AKey: String; R: TStrings): Boolean;

{ get mime description
  example ".jpg" (or "jpg") -> "image/jpeg"
  if file extension not found, return empty string. (or text/plain?!!)
}
function  MimeByExt(ext: String): String;
{$ENDIF}

{ from = 1..}
function Scan32(const Block: ANSIString; Len, From: Integer; const Str: String): Integer;
function ReplaceChars(setOfChar: TSysCharSet; newchar: Char; var s: String): Integer;
function  LoadPChar(AFN: String; var dest: Pointer): Integer;
{$IFNDEF VER80}
function  LoadString(AFN: String): String;
function  StoreString(AFN, S: String): Boolean;
{$ENDIF}

{ locale support functions }
{ set Application default locale }
{$IFNDEF VER80}
procedure OverrideAppLocale(LocaleOverride: String);
{ return in SL string list, items looks like 'RUS=Русский' (False) or 'Русский=RUS (True)'
  LCID(Objects) = LocaleID (LCID is Cardinal type)
}
{$IFDEF D4_}
procedure GetLocaleNames(SL: TStrings; AOrderByName:Boolean);
function GetDllName: String;
function GetWindowsDirectory: String;
{$ENDIF}
{$ENDIF}

procedure SetStringsDelimitedTextWithSpace(AStrings: TStrings; AQuoteChar: Char; ADelimiter: Char; const Value: string);

function PascalString(const S: String): String;
function PascalWideString(const S: String): WideString;
function PascalString2WideString(const S: String; var AIsString: Boolean): WideString;
function WideString2PascalString(const S: WideString): String;

{ Keyboard layout added Nov-27-2006. '00000419' - RU '00000409' - EN US
}
function GetKbdLayout: ShortString;             // get current keyboard layout name
procedure SetKbdLayout(AKbdLayoutName: String); // activate keyboard layout with specified name

// kilo, mega..
function BytesToFriendlyString(AValue: DWord): String;
function BitsToFriendlyString(AValue: DWord) : String;

// binary
function Binvector2Str(const AVector: String): String;

var
  GOffsetFromUTC: TDateTime = 0;

implementation

uses
  ShellAPI,
{$IFNDEF VER90}
  WinTypes
{$ENDIF}
{$IFNDEF VER80}
  ,Windows, Registry
{$ENDIF}
;

{$IFDEF VER80}
 function Min(A, B: LongInt): LongInt;
 begin
   if A > B
   then Result:= B
   else Result:= A;
 end;

function DupeString(Dup: String; Cnt: Integer): String;
var
  i: Integer;
begin
  Result:= '';
  for i:= 1 to Cnt do begin
    Result:= Result + Dup;
  end;
end;
{$ENDIF}

{$IFNDEF VER80_32}
procedure SetLength(var S: ShortString; Len: Byte);
begin
  S[0]:= Char(Len);
end;
{$ENDIF}

{ возвращает номер строки Srch в списке SepList с разделителями Sep, с 1.
  Если не найдено, 0
}
function   OrderInSeparatedList(const Srch, SepList, Sep: String): Integer;
var
  No: Integer;
  S: String;
begin
  No:= 0;
  repeat
    Inc(No);
    S:= GetToken(No, Sep, SepList);
    if ANSICompareText(S, Srch) = 0 then begin
      Result:= No;
      Exit;
    end;
  until s = '';
  Result:= 0;
end;

function   MkStringsByComma(S: String): TStrings;
var
  Strs: TStringList;
  No: Integer;
  SS: String;
begin
  Strs:= TStringList. Create;
  SS:= GetToken(1, ',',S);
  no:= 1;
  while SS <> '' do begin
    Strs.Add(SS);
    Inc(No);
    SS:= GetToken(No,',',S);
  end;
  Result:= Strs;
end;

{ ищет строковые значения с пробелами и упр. символами и закавычивает
  одной кавычкой, если еще нет одинарной или двойной кавычки. Если кавычки дублируются,
  внешняя пара удаляется
}
procedure  ValidateQuoteStringValues(R: TStrings);
const
  DELIMITERS = [#0..#32]; { simple! }
var
  i, j, p, L: Integer;
  S: String;
begin
  for i:= 0 to R.Count - 1 do begin
    S:= R.Strings[i];
    p:= Pos('=', S);
    L:= Length(S);
    if (p >= 1) and (p < L) then begin
      if S[p+1] in ['''', '"'] then begin
        if S[p+2] in ['''', '"'] then begin
          { убрать лишнюю пару кавычек }
          if S[L] in ['''', '"']
          then Delete(S, L, 1);
          Delete(S, 1, 1);
          { Dec(L, 2); }
        end;
        Continue;  { кавычки уже стоят }
      end;
      { кавычек нет, если есть пробелы- закавычить }
      for j:= p+1 to L do begin           { проверить на наличие пробельных символов }
        if S[j] in DELIMITERS then begin
          { закавычить }
          S:= S + '''';         { сначала сзади }
          Insert('''', S, p+1); { теперь спереди }
          R.Strings[i]:= S;
          Break;   { следующее значение }
        end;
      end;
    end;
  end;
end;

{ translate coordinates }

function SLTranslateLines2Ofs(AStrings: TStrings; const APos: TPoint): Cardinal;
var
  i, C: Integer;
begin
  Result:= 0;
  C:= AStrings.Count;
  if APos.X >= C then Exit;  // range error
  // if APos.X < 0 then Exit;    // range error

  // count previous lines
  for i:= 0 to APos.X - 1 do begin
    Inc(Result, Length(AStrings[i]));
  end;
  // calc CRLF
  Inc(Result, 2 * APos.X);
  // add position in line
  Inc(Result, APos.Y);
end;

function SLTranslateOfs2Pos(AStrings: TStrings; AOfs: Cardinal): TPoint;
var
  i, p, C: Integer;
begin
  C:= AStrings.Count;
  p:= 0;
  i:= 0;
  while i < C do begin
    Inc(p, Length(AStrings[i]));
    if p >= AOfs then begin
      Result.X:= i;
      Result.Y:= AOfs - p + Length(AStrings[i]) + 1;
      Exit;
    end;
    // Increment CRLF pair
    Inc(p, 2);
    Inc(i);
  end;
  Result.X:= 0;
  Result.Y:= 1;
end;

function SLTranslateLines2Ofs(AStrings: TWideStrings; const APos: TPoint): Cardinal;
var
  i, C: Integer;
begin
  Result:= 0;
  C:= AStrings.Count;
  if APos.X >= C then Exit;  // range error
  // if APos.X < 0 then Exit;    // range error

  // count previous lines
  for i:= 0 to APos.X - 1 do begin
    Inc(Result, Length(AStrings[i]));
  end;
  // calc EOL char
  Inc(Result, APos.X);
  // add position in line
  Inc(Result, APos.Y);
end;

function SLTranslateOfs2Pos(AStrings: TWideStrings; AOfs: Cardinal): TPoint;
var
  i, p, C: Integer;
begin
  C:= AStrings.Count;
  p:= 0;
  i:= 0;
  while i < C do begin
    Inc(p, Length(AStrings[i]));
    if p >= AOfs then begin
      Result.X:= i;
      Result.Y:= AOfs - p + Length(AStrings[i]) + 1;
      Exit;
    end;
    // Increment EOL char
    Inc(p);
    Inc(i);
  end;
  Result.X:= 0;
  Result.Y:= 1;
end;

function IsEmptyString(S: String): Boolean;
var
  i: Integer;
begin
  Result:= True;
  for i:= 1 to Length(S) do begin
    if S[i] > #32 then begin
      Result:= False;
      Exit;
    end;
  end;
end;

procedure  DeleteLeadSpaceStr(var S: String);
var
  L: Integer;
begin
  L:= Length(S);
  if L = 0
  then Exit;
  while (L > 0) and (S[1] <= #32) do begin
    Delete(S, 1, 1);
    Dec(L);
  end;
end;

procedure  DeleteLeadTerminateSpaceStr(var S: String);
var
  L: Integer;
begin
  L:= Length(S);
  if L = 0
  then Exit;
  while (L > 0) and (S[1] in [#0..#32]) do begin { #0, #9, #32 }
    Delete(S, 1, 1);
    Dec(L);
  end;
  while (L > 0) and (S[L] in [#0..#32]) do begin { #0, #9, #32 }
    Delete(S, L, 1);
    Dec(L);
  end;
end; { DeleteLeadTerminateSpace }

procedure  DeleteLeadTerminateSpaceStr(var S: WideString);
var
  L: Integer;
begin
  L:= Length(S);
  if L = 0
  then Exit;
  while (L > 0) and (UnicodeIsWhiteSpace(Cardinal(S[1])) or UnicodeIsControl(Cardinal(S[1]))) do begin
    Delete(S, 1, 1);
    Dec(L);
  end;
  { конец }
  while (L > 0) and (UnicodeIsWhiteSpace(Cardinal(S[L])) or UnicodeIsControl(Cardinal(S[L]))) do begin
    Delete(S, L, 1);
    Dec(L);
  end;
end; { DeleteLeadTerminateSpace }

procedure  DeleteDoubledSpaceStr(var S: String);
var
  i, L: Integer;
begin
  L:= Length(S);
  { двойные пробелы }
  i:= 1;
  while (i <= L) do begin
    if (S[i] <= #32) and (i + 1 <=L) and ((S[i+1] <= #32)) then begin
      { единообразно заменить на пробелы }
      S[i]:= #32;
      Delete(S, i+1, 1);
      Dec(L);
      { нельзя переходить дальше- вдруг несколько пробедов подряд}
    end else Inc(i);
  end;
end; { DeleteDoubledSpaceStr }

procedure  DeleteDoubledSpaceStr(var S: WideString); overload;
var
  i, L: Integer;
begin
  L:= Length(S);
  { двойные пробелы }
  i:= 1;
  while (i <= L) do begin
    if (UnicodeIsWhiteSpace(Cardinal(S[i])) or UnicodeIsControl(Cardinal(S[i]))) and
    (UnicodeIsWhiteSpace(Cardinal(S[i + 1])) or UnicodeIsControl(Cardinal(S[i + 1]))) and (i + 1 <= L) then begin
      { единообразно заменить на пробелы }
      S[i]:= #32;
      Delete(S, i + 1, 1);
      Dec(L);
      { нельзя переходить дальше- вдруг несколько пробедов подряд}
    end else Inc(i);
  end;
end; { DeleteDoubledSpaceStr }

{ удалить все пробелы- лидирующие, терминирующие и двойные
}
procedure  DeleteLeadTerminateDoubledSpaceStr(var S: String);
var
  i, L: Integer;
begin
  L:= Length(S);
  if L = 0
  then Exit;
  { начало }
  while (L > 0) and (S[1] <= #32) do begin
    Delete(S, 1, 1);
    Dec(L);
  end;
  { конец }
  while (L > 0) and (S[L] <= #32) do begin
    Delete(S, L, 1);
    Dec(L);
  end;
  { двойные пробелы }
  i:= 1;
  while (i <= L) do begin
    if (S[i] <= #32) and (i + 1 <=L) and ((S[i+1] <= #32)) then begin
      { единообразно заменить на пробелы }
      S[i]:= #32;
      Delete(S, i+1, 1);
      Dec(L);
      { нельзя переходить дальше- вдруг несколько пробедов подряд}
    end else Inc(i);
  end;
end; { DeleteLeadTerminateDoubledSpaceStr }

procedure  DeleteLeadTerminateDoubledSpaceStr(var S: WideString); overload;
const
  SPCALL = [ccSeparatorSpace, ccSpaceOther, ccWhiteSpace, ccSeparatorParagraph, ccSegmentSeparator, ccOtherControl, ccOtherFormat];
var
  i, L: Integer;
  wc, wc1: Cardinal;
begin
  L:= Length(S);
  { начало }
  while (L > 0) and (UnicodeIsWhiteSpace(Cardinal(S[1])) or UnicodeIsControl(Cardinal(S[1]))) do begin
    Delete(S, 1, 1);
    Dec(L);
  end;
  { конец }
  while (L > 0) and (UnicodeIsWhiteSpace(Cardinal(S[L])) or UnicodeIsControl(Cardinal(S[L]))) do begin
    Delete(S, L, 1);
    Dec(L);
  end;
  { двойные пробелы }
  i:= 1;
  while (i < L) do begin // do not <= !
    wc:= Cardinal(S[i]);
    wc1:= Cardinal(S[i + 1]);
    // (WC = $D) or (WC = $A) or (WC = $2028) or (WC = $2029);
    // UnicodeIsWhiteSpace(wc) or UnicodeIsControl(wc) or IsSeparator(wc)
    if jclUnicode.CategoryLookup(wc, SPCALL) then begin
      { единообразно заменить на пробелы }
      S[i]:= #32;
      if jclUnicode.CategoryLookup(wc1, SPCALL) then begin
        Delete(S, i + 1, 1);
        Dec(L);
      { нельзя переходить дальше- вдруг несколько пробелов подряд}
      end else Inc(i);
    end else Inc(i);
  end;
end; { DeleteDoubledSpaceStr }

{ удаляет все управляющие символы кроме пробела }
procedure  DeleteControlsStr(var S: String);
var
  i: Integer;
begin
  if Length(S) = 0
  then Exit;
  i:= 1;
  while i <= Length(S) do begin
    if S[i] in [#0..#31]
    then Delete(S, i, 1)
    else Inc(i);
  end;
end;

{ parse SS string and return words from STARTPOSITION
  (if possible) no more than LessThanBytes charactters of them
  return new STARTPOSITION
}
function GetLimitQtyWords(const S: String; var StartPosition: Integer;
  LessThanBytes: Integer): String;
var
  SS: String;
  L: Integer;
begin
  SS:= System.Copy(S, StartPosition, LessThanBytes);
  L:= Length(SS);
  while L > 0 do begin
    if SS[L] <= #32 then begin
      { удалить пробел или упр. символ на конце }
      System.Delete(SS, L, 1);
      Break;
    end else begin
      if SS[L] in ['!'..'@','['..'`','{'..'~'] then begin
        { знаки пунктуации на конце не удалять }
        Break;
      end else begin
        { это часть слова- удалить }
        System.Delete(SS, L, 1);
        Dec(L);
      end;
    end;
  end;
  if L = 0 then begin
    { не вышло, опять- просто нарезать }
    SS:= System.Copy(S, StartPosition, LessThanBytes);
    L:= Length(SS);
  end;
  { вернуть новое значение старта }
  StartPosition:= StartPosition + L;
  Result:= SS;
end;

{ parse SS string and return words from STARTPOSITION
  (if possible) no more than LessThanBytes characters of them
  Parameters:
  Set:    LastPosition to 0 in first time (RW)
          StartPosition - useless         (WO)
          LessThanBytes                   (RO)
  return new STARTPOSITION
  Rerurn: TRUE  - parsed
          FALSE - no more words (empty string : ALL var undefined!!!
                  NOT: StartPosition > LastPosition=Length(S))
}
function LimitQtyWordsPos(const S: String; var StartPosition: Word;
  var LastPosition: Word; LessThanBytes: Integer): Boolean;
var
  L, tried_last: Integer;
  st, fin: Integer;
begin
  Result:= False;
  L:= Length(S);
  st:= LastPosition + 1;
  if (L < 0) or (st > L) or (LessThanBytes <=0)
  then Exit;
  while (st < L) and (S[st] <= #32) do begin
    Inc(St);
  end;
  fin:= st + LessThanBytes-1;
  if fin >= L then begin
    { строка короче или равна пределу }
    { вырезать не гадо, выдать целый кусок }
    StartPosition:= st;
    LastPosition:= fin;
    Result:= True;
    Exit;
  end;
  { tried_last-  текущий конец  }
  tried_last:= fin;
  while tried_last > st do begin
    if S[tried_last] <= #32 then begin
      { удалить пробел или упр. символ на конце }
      Dec(tried_last);
      Break;
    end else begin
      if S[tried_last] in ['!'..'@','['..'`','{'..'~'] then begin
        { знаки пунктуации на конце не удалять }
        Break;
      end else begin
        { это часть слова- удалить }
        Dec(tried_last);
      end;
    end;
  end;
  if tried_last > st then begin
    { если удалось найти конец по слову то назначить его }
    fin:= tried_last;
  end;
  { вернуть новые значение старта и финиша (все включительно) }
  StartPosition:= st;
  LastPosition:= fin;
  Result:= fin >= st;
end;

{ удаляет все управляющие символы кроме пробела, а CRLF -> пробел }
procedure  DeleteControlsStrCRLF2Space(var S: String);
var
  i: Integer;
begin
  if Length(S) = 0
  then Exit;
  i:= 1;
  while i <= Length(S) do begin
    if S[i] in [#0..#9, #11..#31]
    then Delete(S, i, 1)
    else begin
      if S[i] = #10
      then S[i]:= #32;
      Inc(i);
    end;
  end;
end;

procedure  DeleteControlsStrCRLF2Space(var S: WideString);
var
  i: Integer;
begin
  if Length(S) = 0
  then Exit;
  i:= 1;
  while i <= Length(S) do begin
    if (S[i] <= #9) or ((S[i] >= #11) and (S[i] <= #31))
    then Delete(S, i, 1)
    else begin
      if S[i] = #10
      then S[i]:= #32;
      Inc(i);
    end;
  end;
end;

{ проверяет наличие префикса Prefix и суффикса Suffix и возвращает параметр без них }
function CheckParameter(const Prefix: ShortString; const Quotes: ShortString;
  var AParameter: String; const Suffix: ShortString):Boolean;
var
  L, Lp, Ls, Lq: Integer;
begin
  DeleteLeadTerminateSpaceStr(AParameter);
  L:= Length(AParameter);
  Lp:=Length(Prefix);
  Ls:=Length(Suffix);
  Lq:= Length(Quotes);
  if (L <= Lp+Ls) then begin
    Result:= False;
    Exit;
  end;
  if (ANSICompareText(System.Copy(AParameter, 1, Lp), Prefix)<>0) or
    (ANSICompareText(System.Copy(AParameter, L - Ls + 1, Ls), Suffix)<>0) then begin
    Result:= False;
    Exit;
  end;
  if (L >= Lq+Lp+Ls) and (Lq >= 2) then begin
    if (AParameter[Lp+1] = Quotes[1]) and (AParameter[L-Ls] = Quotes[2]) then begin
      Inc(Lp);
      Dec(Ls);
    end;
  end;
  AParameter:= System.Copy(AParameter, Lp+1, L-Lp-Ls);
  DeleteLeadTerminateSpaceStr(AParameter);
  Result:= True;
end;

procedure  DeleteLeadTerminateSpaceSL(S: TStrings);
var
  SS: ShortString;
  cou: Word;
begin
  if S. Count <= 0
  then Exit;
  for Cou:= 0 to S.Count -1 do begin
    if Length(S. Strings[0]) = 0
    then Continue;
    SS:= S.Strings[Cou];
    while (Length(SS) > 0) and (SS[1] in [#0, #9, #32])
    do Delete(SS, 1, 1);
    while (Length(SS) > 0) and (SS[Length(SS)] in [#0, #9, #32])
    do Delete(SS, Length(SS), 1);
    S.Strings[Cou]:= SS;
  end;
end; { DeleteLeadTerminateSpace }

{ return-
  >1 - position of last valid digit
}
function TryReadDecimal(S: String; Bytes: Byte; var Res: Integer): Integer;
var
  Shift, i: Word;
  SS: ShortString;
begin
  TryReadDecimal:= 0;
  Res:= 0;
  { bug- shift must be repesent start of digits
  }
  Shift:= 0;
  { is ShortString shorter than required?
  }
  if Length(S) < Bytes + Shift
  then Exit;

  SS:= '';
  for i:= 1 + Shift to Bytes do begin
    if not (S[i] in ['0'..'9']) then begin
      { return what we have
      }
      if SS = '' then begin
        Res:= 0;
      end else begin
        try
          Res:= StrToInt(SS);
        except
        end;
      end;
      TryReadDecimal:= i;
      Exit;
    end;
    SS:= SS+S[i];
  end;
  try
    Res:= StrToInt(SS);
  except
  end;
  TryReadDecimal:= Bytes + Shift;
end;

{ replace  '|' to ':' (SQL!) }
function   CopyColon(S: String; Index, Count: Integer): String;
var
  i: Integer;
  SS: String;
begin
  SS:= System.Copy(S, Index, Count);
  for i:= 1 to Length(SS) do begin
    if SS[i] = '|' then SS[i]:= ':';
  end;
  CopyColon:= SS;
end;

{ change chars, NOT return number of replacements
}
function  ChangeChars(src, dest: Char; const S: String): String;
var
  i: Integer;
begin
  Result:= S;
  for i:= 1 to Length(S) do begin
    if src = S[i] then begin
      Result[i]:= dest;
    end; { if }
  end; { for }
end; { ChangeChars }

{ return position on next srch substring
  if not return 0
}
function PosFrom(From: Integer; const srch, where: String; ACaseSensitive: Boolean = True): Integer;
var
  p: Integer;
begin
  if from <= 0
  then from:= 1;
  { это наиболее эффективный неассембированный путь. БОЛЬШЕ не пробовать оптимизировать!!!
    compiler Copy function use different code for short and long strings }
  if ACaseSensitive then begin
    p:= System.Pos(srch, System.Copy(where, From, Length(where) - From + 1));
  end else begin
    p:= System.Pos(ANSIUpperCase(srch), ANSIUpperCase(System.Copy(where, From, Length(where) - From + 1)));
  end;

  if p > 0
  then Result:= p + from - 1
  else Result:= 0;
end; { PosFrom }

{ Pos except search from back
}
function   PosBack(const srch, where: String): Integer;
var
  i, j: Integer;
  f: Boolean;
begin
  PosBack:= 0;
  if Length(srch) > Length(where)
  then Exit;
  for i:= Length(where) - Length(srch) + 1 downto 1 do begin
    f:= True;
    for j:= 1 to Length(srch) do begin
      if srch[j] <> where[j+i-1] then begin
        f:= False;
        Break;
      end;
    end;
    if f then begin
      PosBack:= i;
      Exit;
    end;
  end;
end; { PosBack }

function   PosBack(const srch, where: WideString): Integer;
var
  i, j: Integer;
  f: Boolean;
begin
  PosBack:= 0;
  if Length(srch) > Length(where)
  then Exit;
  for i:= Length(where) - Length(srch) + 1 downto 1 do begin
    f:= True;
    for j:= 1 to Length(srch) do begin
      if srch[j] <> where[j+i-1] then begin
        f:= False;
        Break;
      end;
    end;
    if f then begin
      PosBack:= i;
      Exit;
    end;
  end;
end; { PosBack }

{ NotQuotedPos is Pos but it check is search character is in " or '
}
function   NotQuotedPos(const ASrchChar, AOChar, ACChar: Char; const where: String): Integer;
var
  i, cntochar, cntcchar: Integer;
  f: Boolean;
begin
  Result:= 0;
  cntochar:= 0;
  cntcchar:= 0;
  for i:= 1 to Length(where) do begin
    f:= True;
    if where[i] = AOChar then begin
      if (AOChar = ACChar)
      then
        cntochar:= cntochar xor 1
      else
        Inc(cntochar);
    end else
      if where[i] = ACChar then Inc(cntcchar) else
      if where[i] = ASrchChar then begin
        if cntcchar >= cntochar then begin
          Result:= i;
          Exit;
        end;
      end;
  end;
end;

function   PosBackFrom(from: Integer; const srch, where: String): Integer;
var
  i, j: Integer;
  f: Boolean;
begin
  PosBackFrom:= 0;
  if (Length(srch) > Length(where)) or
    (from > Length(where) - Length(srch) + 1) or (from < 0)
  then Exit;
  for i:= from downto 1 do begin
    f:= True;
    for j:= 1 to Length(srch) do begin
      if srch[j] <> where[j+i-1] then begin
        f:= False;
        Break;
      end;
    end;
    if f then begin
      PosBackFrom:= i;
      Exit;
    end;
  end;
end;

{ change chars, return number of replacements
}
function  ChangeChar(src, dest: Char; var S: String): Byte;
var
  i, Count: Word;
begin
  Count:= 0;
  for i:= 1 to Length(S) do begin
    if src = S[i] then begin
      Inc(Count);
      S[i]:= dest;
    end; { if }
  end; { for }
  ChangeChar:= Count;
end; { ChangeChar }

{ return TRUE if string is Longint decimal representation
  if S parameter is empty string, return False
}
function   isDecimal(const S: String): Boolean;
var
  p: Integer;
  L: Integer;
begin
  L:= Length(S);
  Result:= False;
  for p:= 2 to L do begin
    if not (S[p] in ['0'..'9'])
    then Exit;
  end;
  case L of
    0:;
    1: Result:= S[1] in ['0'..'9'];
    else Result:= S[1] in ['-', '0'..'9'];
  end;
end;

{ return TRUE if substring is Longint decimal representation
}
function   PartisDecimal(b, e: Integer; const S: String): Boolean;
var
  p: Integer;
  L: Integer;
begin
  PartisDecimal:= False;
  L:= Length(S);
  if (L = 0) or (b > e) or (L < e)
  then Exit;
  for p:= b to e do begin
    if not (S[p] in ['0'..'9'])
    then Exit;
  end;
  { in case of empty string }
  PartisDecimal:= True;
end;

{ return TRUE if string is Longint Pascal hexadecimal or decimal representation
}
function   isPascalInt(const S: String): Boolean;
var
  p: Integer;
  L: Integer;
begin
  Result:= False;
  L:= Length(S);
  if L = 0
  then Exit;
  if s[1] = '$' then begin
    for p:= 2 to L do begin
      if not (S[p] in ['0'..'9', 'A'..'F', 'a'..'f'])
      then Exit;
    end;
  end else begin
    for p:= 1 to L do begin
      if not (S[p] in ['0'..'9'])
      then Exit;
    end;
  end;
  { in case of empty string }
  Result:= True;
end;

{ return TRUE if string is Longint hexadecimal representation
}
function   isHexaDecimal(const S: String): Boolean;
begin
  Result:= PartisHexaDecimal(1, Length(S), S);
end;

function   PartisHexaDecimal(b, e: Integer; const S: String): Boolean;
var
  p: Integer;
  L: Integer;
begin
  Result:= False;
  L:= Length(S);
  if (b>e) or (e>L)
  then Exit;
  Result:= False;
  for p:= b to e - 1 do begin
    if not (S[p] in ['0'..'9', 'A'..'F', 'a'..'f'])
    then Exit;
  end;
  if not (S[e] in ['0'..'9', 'A'..'F', 'a'..'f', 'H', 'h'])
  then Exit;
  { in case of empty string }
  Result:= True;
end;

function LowestInSet(var setofbyte; start: Byte): Byte;
var
  i: Byte;
begin
  LowestInSet:= 0;
  for i:= start to 255 do begin
    if not (i in TBS(setofbyte))
    then Continue;
    LowestInSet:= i;
    Break;
  end;
end;

function DecimalDigits(V: LongInt; var Positions): LongInt;
var
  Start, Finish, Digits: Byte;
  Code: Integer;
  Res: LongInt;
begin
  Start:= LowestInSet(Positions, 0);
  Finish:= LowestInSet(Positions, Start+1);
  Digits:= LowestInSet(Positions, Finish+1);
  if Digits = 0
  then Digits:= Finish;
  Val(System.Copy(Format('%'+IntToStr(Digits)+'d',[V]),
  Start, Finish-Start+1), Res, Code);
  DecimalDigits:= Res;
end;

{ очень плохой метод- в strutil метод был лучше
}
function LinesInFile(const FN: String): LongInt;
var
  SL: TStringList;
begin
  SL:= TStringList.Create;
  try
    SL.LoadFromFile(FN);
    LinesInFile:= SL.Count;
  finally
    SL.Free;
  end;
end;

function LinesInLongFile(FN: ShortString): LongInt;
var
  T: TextFile;
  Line: LongInt;
  S: ShortString;
begin
  LinesInLongFile:= 0;
  try
    AssignFile(T, FN);
    Reset(T);
  except
    Exit;
  end;
  Line:= 0;
  try
    while not EOF(T) do begin
      Readln(T, S);
      Inc(Line)
    end;
  finally
    CloseFile(T);
  end;
  LinesInLongFile:= Line;
end; { LinesInLongFile }

function    GetFileLine(FileName: ShortString; line: LongInt): ShortString;
var
  T: TextFile;
  RLine: LongInt;
  S: ShortString;
begin
  GetFileLine:= '';
  try
    AssignFile(T, FileName);
    Reset(T);
  except
    Exit;
  end;
  RLine:= 0;
  try
    while not EOF(T) do begin
      Readln(T, S);
      Inc(RLine);
      if (RLine = Line) then begin
        Break;
      end;
    end;
  finally
    CloseFile(T);
  end;
  GetFileLine:= S;
end; { GetFileLine }

function    NotEmptyLinesInLongFile(FN: ShortString): LongInt;
var
  T: TextFile;
  Line: LongInt;
  S: String[1];
begin
  NotEmptyLinesInLongFile:= 0;
  try
    AssignFile(T, FN);
    Reset(T);
  except
    Exit;
  end;
  Line:= 0;
  try
    while not EOF(T) do begin
      Readln(T, S);
      if Length(S) <> 0
      then Inc(Line)
    end;
  finally
    CloseFile(T);
  end;
  NotEmptyLinesInLongFile:= Line;
end; { LinesInLongFile }

function    RecordLinesInLongFile(Nums, NumFLen: Byte; FN: ShortString;
  ProgressIndicator: StateBarproc): LongInt;
var
  T: TextFile;
  Num_Prev,
  Num: Integer;
  Line,
  Rec : LongInt;
  S: ShortString;

function Err(const S: ShortString): Boolean;
begin
{$IFNDEF VER80}
  raise EExternal.CreateFmt('Строка в файле: %d'#13#10'Запись %d, № в записи: %d'+
    #13#10'', [Line, Rec+1, Num, s]);
{$ENDIF}
  Err:= False;
end;

begin
  RecordLinesInLongFile:= 0;
  try
    AssignFile(T, FN);
    Reset(T);
  except
    Exit;
  end;
  Line:= 1;
  Rec:=  0;
  Num_Prev:= 0;
  try
    while not EOF(T) do begin
      Readln(T, S);
      try
        Num:= StrToInt(Copy(S, 1, NumFLen));
      except
        if Err('Неверный № в записи: '+ Copy(S, 1, NumFLen))
        then Exit;
      end;
      if Num <> Num_Prev + 1 then begin
        if Err('№ не последовательный: '+ IntToStr(Num)+
        ' за №'+IntToStr(Num_Prev))
        then Exit;
      end;
      if Num = Nums then begin
        Num_Prev:= 0;
        Inc(Rec);
        if (Rec and $F) = $F
        then ProgressIndicator('Проверено записей: '+IntToStr(Rec));
      end else begin
        Inc(Num_Prev);
      end;
      Inc(Line);
    end;
  finally
    Close(T);
  end;
  if Num <> Nums then begin
    if Err('Остановка на строке: '+IntToStr(Num))
    then Exit;
  end;
  RecordLinesInLongFile:= Line - 1;
end; { LinesInLongFile }

function  PosCharsFrom(From: Word; srch: ShortString; s: String): Word;
var
  SS: String;
  ff: Word;
begin
  SS:= System.Copy(S, From, Length(S)-From+1);
  ff:= System.Pos(srch, SS);
  if ff > 0
  then ff:= ff + From - 1;
  Result:= ff;
end; { PosCharsFrom }

function    TokenStart(No: Word; const delimiter, S: String): Integer;
var
  i: Integer;
  fr, DL: Word;
begin
  fr:= 1;
  DL:= Length(delimiter);
  for i:= 1 to No - 1 do begin
    fr:= PosCharsFrom(fr, delimiter, S);
    if fr = 0
    then Break;
    Inc(fr, DL);
  end;
  Result:= fr;
end;

function GetToken(No: Word; const delimiter, S: String): String;
var
  i: Integer;
  fr, relative_last, DL: Word;
begin
  Result:= '';
  fr:= 1;
  DL:= Length(delimiter);
  for i:= 1 to No - 1 do begin
    fr:= PosCharsFrom(fr, delimiter, S);
    if fr = 0
    then Exit;
    Inc(fr, DL);
  end;
  relative_last:= PosCharsFrom(fr, delimiter, S);
  if relative_last = 0
  then relative_last:= Length(S)
  else Dec(relative_last);
  { 123456
    1;2;3;
  }
  Result:= System.Copy(s, fr, relative_last - fr + 1);
end; { GetToken }

function  GetWord(var S: String; var WordLen: Integer): String;
const
  DELIMITERS = [#0..'/', ':', '@', '['..'`', '{'..#127];
var
  i, L, st, fin: Integer;
begin
  L:= Length(S);
  st:= 1;
  fin:= L + 1;
  for i:= 1 to L do begin
    if not (S[i] in DELIMITERS) then begin
      st:= i;
      Break;
    end;
  end;
  for i:= st to L do begin
    if S[i] in DELIMITERS then begin
      fin:= i;
      Break;
    end;
  end;
  WordLen:= fin - st;
  Result:= System.Copy(S, st, WordLen);
  System.Delete(S, 1, fin);
end;

{ возвращает отделенное упр. символами и возвращает в p начало следующего }
function ExtractToken(const S: String; var p: Integer): String;
var
  L, st: Integer;
begin
  st:= p;
  L:= Length(S);
  if p > L then begin
    { это излишне но так надежнее }
    Result:= '';
    Exit;
  end;
  { начальные пробелы }
  while (st < L) and (S[st] <= #32)
  do Inc(st);
  p:= st;
  while (p <= L) and (S[p] > #32)
  do Inc(p);
  Result:= System. Copy(s, st, p - st);
  Inc(p);
end;

function ExtractTokenBack(const S: String; var p: Integer): String;
var
  st: Integer;
begin
  if p < 1
  then p:= Length(S);
  st:= p;
  { начальные задние пробелы }
  while (st > 0) and (S[st] <= #32)
  do Dec(st);
  p:= st;
  while (p > 0) and (S[p] > #32)
  do Dec(p);
  Result:= System. Copy(s, p + 1, st - p);
  Inc(p);
end;

{ как GetToken, но возвращает весь остаток }
function    TokensTail(No: Word; const delimiter, S: String): String;
var
  i: Integer;
  fr, DL: Word;
begin
  Result:= '';
  fr:= 1;
  DL:= Length(delimiter);
  for i:= 1 to No - 1 do begin
    fr:= PosCharsFrom(fr, delimiter, S);
    if fr = 0
    then Exit;
    Inc(fr, DL);
  end;
  Result:= System. Copy(s, fr, Length(S)-fr+1);
end;

procedure   SetToken(No: Word; const delimiter, Value: String; var S: String);
begin
  { bugs here
  var
  i, j: Integer;
  fr, frr, relative_last, DL: Word;

  DL:= Length(delimiter);
  // ищем начало
  fr:= 1;
  for i:= 1 to No do begin
    frr:= fr;
    fr:= PosCharsFrom(fr, delimiter, S);
    if (fr = 0) then begin
      for j:= i to No - 1
      do S:= S + delimiter;
      frr:= Length(S);
      Break;
    end;
    Inc(fr, DL);
  end;
  // ищем конец-
  relative_last:= PosCharsFrom(frr, delimiter, S);
  if relative_last = 0 then begin
    if no = 1
    then s:= s + value
    else s:= s + delimiter + value;
  end else begin
    if no = 1
    then frr:= 1;
    Dec(relative_last);
    System.Delete(s, frr, relative_last - frr + 1);
    System.Insert(value, s, frr+1);
  end;
  }
end;

{ заменяет первое вхождение Fnd на Repl в строке Dest
  Dest- целевая строка с макросами(?)
  CaseSens: заменять чувствительно к регистру символов
  Fnd  - что искать
  Repl - на что заменять
  возвращает
  True - замена произведена
  False- нет больше вхождений
}
function    ReplaceStr(var Dest: String; CaseSens: Boolean; const Fnd, Repl: String): Boolean;
var
  CFnd, SS: String;
  p: Integer;
begin
  if CaseSens then begin
    SS:= Dest;
    CFnd:=Fnd;
  end else begin
    SS:=  ANSIUpperCase(Dest);
    CFnd:= ANSIUpperCase(Fnd);
  end;
  p:= System.Pos(CFnd, SS);
  if p <= 0 then begin
    { вхождений нет, нет и замен }
    Result:= False;
  end else begin
    { найденное заменяем }
    Result:= True;
    System.Delete(Dest, p, Length(CFnd));
    System.Insert(Repl, Dest, p);
  end;
end;

{ заменяет первое вхождение Fnd на Repl в строке Dest
  Dest- целевая строка с макросами(?)
  CaseSens: заменять чувствительно к регистру символов
  Fnd  - что искать
  Repl - на что заменять
  возвращает
  True - замена произведена
  False- нет больше вхождений
}
function  ReplaceStr(var Dest: WideString; CaseSens: Boolean; const Fnd, Repl: WideString): Boolean;
var
  CFnd, SS: WideString;
  p: Integer;
begin
  if CaseSens then begin
    SS:= Dest;
    CFnd:= Fnd;
  end else begin
    SS:= jclUnicode.WideUpperCase(Dest);
    CFnd:= jclUnicode.WideUpperCase(Fnd);
  end;
  p:= Cardinal(jclUnicode.StrPosW(PWideChar(SS), PWideChar(CFnd))) - Cardinal(PWideChar(SS));
  if p < 0 then begin
    { вхождений нет, нет и замен }
    Result:= False;
  end else begin
    { найденное заменяем }
    p:= (p div 2)+ 1;
    dest:= System.Copy(dest, 1, p - 1) + Repl + System.Copy(dest, p + Length(CFnd), MaxInt);
    Result:= True;
  end;
end;

{ Dest- строка с тегами
  Prefix, Suffix- начало и конец тега
  Quote- обычно символ '"', если #0- нет строковых литералов
  Возвращает 0. (возвращает число замен - не реализовано)
  callback- функция для ReplaceTag
  TChecktag = function (Tag: String): String;
}
function    ReplaceTag(var Dest: String; const Prefix, Suffix: String;
  Quote: Char; ReplFunc: TChecktag): Integer;
var
  st: Integer;
  fin: Integer;
  tag: String;
begin
  Result:= 0;
  st:= 1;
  if Quote = #0 then begin
    while True do begin
      st:= PosFrom(st, Prefix, Dest);
      if st <= 0
      then Exit;
      fin:= PosFrom(st+1, Suffix, Dest);
      if fin <= 0
      then Exit;
      { 0 длины тоже ничего }
      tag:= System.Copy(Dest, st+1, fin-st-1);
      System.Delete(Dest, st, fin-st+1);
      tag:= ReplFunc(tag);
      System.Insert(tag, Dest, st);
      { следующее начать за вставленным выражением во избежание рекурсии }
      st:= st + Length(tag);
    end;
  end else begin
    { не реализовано }
  end;
end;

function   BlankStringTo(const S: ShortString; blanks: Word): ShortString;
var
  i: Word;
  SS: ShortString;
begin
  SS:= S;
  for i:= blanks downto Length(S) + 1 do begin
    SS:= SS + #32;
  end;
  BlankStringTo:=SS;
end;

function   BlankToZero(const S: ShortString): ShortString;
var
  i: Word;
  SS: ShortString;
begin
  for i:= 1 to Length(S) do begin
    if S[i] = #32
    then SS[i]:= '0'
    else SS[i]:= S[i];
  end;
  SetLength(SS, Length(S));
  BlankToZero:= SS;
end;

function CopyFile(const SrcN, DestN: ShortString): LongInt;
var
  SrcStrm, DestStrm: TFileStream;
  Len: LongInt;
begin
{  Result:= 0; }
  try
    SrcStrm:= TFileStream.Create(SrcN,  fmOpenReadWrite);
  except
{$IFNDEF VER80}
    raise EExternal.CreateFmt('Ошибка открытия копируемого файла %s', [SrcN]);
{$ENDIF}
  end;
  try
    DestStrm:= TFileStream.Create(DestN, fmCreate);
  except
    SrcStrm.Destroy;
{$IFNDEF VER80}
    raise EExternal.CreateFmt('Ошибка открытия целевого файла %s', [DestN]);
{$ENDIF}
  end;
  Len:= SrcStrm. Size;
  try
    DestStrm. CopyFrom(SrcStrm, Len);
  except
    SrcStrm. Destroy;
    DestStrm.Destroy;
  end;
  SrcStrm. Destroy;
  DestStrm.Destroy;
  Result:= Len;
end;

{ заполняет лидирующими нулями строку до Bytes }
function   LeadZero(const S: String; Bytes: Byte): String;
var
  i: Integer;
  SS: String;
begin
  SS:= S;
  for i:= 1 to Bytes - Length(SS)
  do SS:= '0'+SS;
  Result:= SS;
end;

{ remove all CHARS from ShortString
}
function EraseChars(const Chars: TCS; const S: ShortString): ShortString;
var
  i: Word;
  SS: ShortString;
begin
  SS:= S;
  {
  if Byte(SS[0]) = 0
  then Exit;
  }
  i:= 1;
  while i <= Length(SS) do begin
    if SS[i] in Chars
    then Delete(SS, i, 1)
    else Inc(i);
  end;
  EraseChars:= SS;
end;

{ remove all CHARS from ShortString and shift next word to up
}
function EraseCharsUP(const Chars: TCS; const S: ShortString): ShortString;
var
  i: Word;
  SS: ShortString;
  Up: Boolean;
begin
  SS:= S;
  {
  if Byte(SS[0]) = 0
  then Exit;
  }
  i:= 1;
  Up:= True;
  while i <= Length(SS) do begin
    if SS[i] in Chars
    then begin
      UP:= True;
      Delete(SS, i, 1)
    end else begin
      if Up
      then
        if SS[i] in ['a'..'z','а'..'я']
        then SS[i]:= Char(Byte(SS[i])-32);
      Inc(i);
      Up:= False;
    end;
  end;
  EraseCharsUP:= SS;
end;

{ get ShortString from buffer
}
function ExtractString(var B; const Len: Word): ShortString;
var
  S: ShortString;
begin
  SetLength(S, WordRec(Len).Lo);
  Move(B, S[1], Length(S));
  ExtractString:= S;
end;

{ get SmallInt from buffer
}
function ExtractLongInt(var B; const Len: Word): LongInt;
var
  S: ShortString;
begin
  SetLength(S, WordRec(Len).Lo);
  Move(B, S[1], Length(S));
  try
    ExtractLongInt:= StrToInt(S);
  except
    ExtractLongInt:= 0;
  end;
end;

{ get Extended from buffer
}
function ExtractExtended(var B; const Len: Word): Extended;
var
  S: ShortString;
begin
  SetLength(S, WordRec(Len).Lo);
  Move(B, S[1], Length(S));
  try
    ExtractExtended:= StrToFloat(S);
  except
    ExtractExtended:= 0.0;
  end;
end;

const
  MemoCharInLine = 80;

{ Take B text buffer with LEN charcters
  result ShortString's list is returned in SL
}
procedure FillMemo(SL: TStrings; var B; const Len: Word);
var
  S: ShortString;
  Ofs: Word;
  Processed: LongInt;
begin
  SL.Clear;
  if Len = 0
  then Exit;
  Processed:= 0;
  Ofs:= 0;
  repeat
    Inc(Processed, MemoCharInLine);
    if Processed > Len then begin
      SetLength(S, MemoCharInLine-Processed + Len);
      Move(CA(B)[Ofs], S[1], MemoCharInLine-Processed + Len);
      SL.Add(S);
      Exit;
    end else begin
      SetLength(S, MemoCharInLine);
      Move(CA(B)[Ofs], S[1], MemoCharInLine);
      SL.Add(S);
    end;
    Inc(Ofs, MemoCharInLine);
  until Processed >= Len;
end;


function TFixStrings.Adresses(Index: SmallInt): Pointer;
begin
  Adresses:= @(CA(StrBuf^)[Index * StrOfLen]);
end;

procedure TFixStrings.Clear;
begin
  { just for nice looking in Watches
  }
  FillChar(StrBuf^, BufLen, #0);
  FilledLines:= 0;
  Exists:= [];
end;

procedure  TFixStrings.EraseString(Index: Word);
begin
  if Index in Exists then begin
    Exclude(Exists, Index);
    BA(StrBuf^)[Index * StrOfLen]:= 0;
    Dec(FilledLines);
  end;
end;

procedure  TFixStrings.MarkNotExist(Index: Word);
begin
  if Index in Exists then begin
    Exclude(Exists, Index);
    Dec(FilledLines);
  end;
end;

function  TFixStrings.Get(Index: SmallInt): ShortString;
var
  SS: ShortString;
  Len: Word;
begin
  {
  if not (Index in Exists) then begin
    Get:= '';
    Exit;
  end;
  }
  if (Index < 0) or (Index >= LinesQTY) then begin
    Index:= 0;
  end;
  Len:= BA(StrBuf^)[Index * StrOfLen];
  Inc(Len);
  Move(CA(StrBuf^)[Index * StrOfLen], SS, Len);
  Get:= SS;
end;

function  TFixStrings.ValuableIndex(Index: SmallInt): SmallInt;
var
  i: Word;
  Ind: SmallInt;
begin
  Ind:= -1;
  for i:= 0 to LinesQTY - 1 do begin
    if i in Exists
    then Inc(Ind);
    if Ind = Index then begin
      ValuableIndex:= i;
      Exit;
    end;
  end;
  ValuableIndex:= -1;
end;

function  TFixStrings.GetValuable(Index: SmallInt): ShortString;
var
  SS: ShortString;
  Len: Word;
  Ind: SmallInt;
begin
  GetValuable:= '';
  Ind:= ValuableIndex(Index);
  if Ind <> -1 then begin
    Len:= BA(StrBuf^)[Ind * StrOfLen];
    Inc(Len);
    Move(CA(StrBuf^)[Ind * StrOfLen], SS, Len);
    GetValuable:= SS;
  end;
end;

procedure TFixStrings.Put(Index: SmallInt; const S: ShortString);
var
  Len: Word;
begin
  Len:= Length(S);
  { just truncate if too long
  }
  if Len + 1 > StrOfLen then begin
    Len:= StrOfLen;
  end;

  if (Index < 0) or (Index >= LinesQTY) then begin
    Exit;
  end;

  BA(StrBuf^)[Index * StrOfLen]:= Len;
  Move(S[1], CA(StrBuf^)[Index * StrOfLen+1], Len);
  if not (Index in Exists) then begin
    Include(Exists, Index);
    Inc(FilledLines);
  end;
end;

constructor TFixStrings. Create(ALines, ALen: Word);
begin
  { stupid name contains qty of lines and len of the each line
  }
  NameOfList:= '['+IntToStr(ALines)+']['+IntToStr(ALen)+']';
  LinesQTY:= ALines;
  if ALen > 255
  then StrOfLen:= 255
  else StrOfLen:= WordRec(ALen).Lo;
  Inc(StrOfLen);
  BufLen:= LinesQTY * StrOfLen;
  GetMem(StrBuf, BufLen);
  Clear;
end;

destructor  TFixStrings. Destroy;
begin
  FreeMem(StrBuf, BufLen);
end;

constructor TDelimitedStrings.Create;
begin
  inherited Create;
  FInfos:= TStringList.Create;
  FInfos.Sorted:= True;
end;

destructor TDelimitedStrings.Destroy;
begin
  FInfos.Free;
  inherited Destroy;
end;

procedure TDelimitedStrings.Add(AHsocket: Integer);
begin
  FInfos.Add(IntToStr(AHsocket)+'=');
end;

function  TDelimitedStrings.GetIndex(AHsocket: Integer): Integer;
var
  ind: Integer;
  SocketStr: String[20];
  S: String;
begin
  SocketStr:= IntToStr(AHsocket);
  S:= SocketStr + '=' + FInfos.Values[SocketStr];
  ind:= FInfos.IndexOf(S);
  Result:= ind;
end;

procedure TDelimitedStrings.Delete(AHsocket: Integer);
var
  ind: Integer;
begin
  ind:= GetIndex(AHsocket);
  if ind < 0
  then Exit;
  FInfos.Delete(ind);
end;

procedure TDelimitedStrings.SetInfo(AHsocket, No: Integer; AInfo: String);
var
  ind, L: Integer;
  S: String;
  SocketStr: String[20];
begin
  SocketStr:= IntToStr(AHsocket);
  L:= Length(SocketStr)+1; { + "=" }
  ind:= GetIndex(AHsocket);
  { in does not exists, create new one }
  if ind < 0 then begin
    Add(AHsocket);
    ind:= GetIndex(AHsocket);
  end;
  { if smth wrong, exit }
  if ind < 0
  then Exit;
  S:= FInfos.Strings[ind];
  System.Delete(S, 1, L);
  SetToken(No+1, ',', AInfo, S);
  FInfos.Delete(ind);
  FInfos.Add(SocketStr+'='+S);
end; { SetInfo }

function  TDelimitedStrings.GetInfo(AHsocket, No: Integer): String;
var
  S: String;
  SocketStr: String[20];
  L, ind: Integer;
begin
  Result:= '';
  SocketStr:= IntToStr(AHsocket);
  L:= Length(SocketStr)+1; { + "=" }
  ind:= GetIndex(AHsocket);
  if ind < 0
  then Exit;
  S:= FInfos.Strings[ind];
  System.Delete(S, 1, L);
  Result:= GetToken(No+1, ',', S);
end; { GetInfo }


{ ShortString }
function IsNotBlank(const S: ShortString): Boolean;
var
  i: Word;
begin
  IsNotBlank:= False;
  if Length(S) = 0 then begin
    Exit;
  end;
  for i:= 1 to Length(S) do begin
    if not (S[i] in [#9,#32]) then begin
      IsNotBlank:= True;
      Exit;
    end;
  end;
end;

function IsNotBlank0(const S: PChar): Boolean;
var
  i: Word;
begin
  IsNotBlank0:= False;
  if StrLen(S) = 0 then begin
    Exit;
  end;
  for i:= 0 to StrLen(S) - 1 do begin
    if not (S[i] in [#9,#32]) then begin
      IsNotBlank0:= True;
      Exit;
    end;
  end;
end;

procedure UpCaseBuffer(Data: array of Char; Len: Word);
begin
{$IFNDEF VER80}
  AnsiUpperBuff(@Data, Len);
{$ENDIF}
end;

function    PCountWords(var Len: LongInt; P: PChar): LongInt;
var
  i, WCount: LongInt;
  Delim: Boolean;
  PrevDelim: Boolean;
begin
  Result:= 0;
  if Len <= 0
  then Len:= StrLen(P);
  if Len = 0
  then Exit;
  PrevDelim:= True;
  WCount:= 0;
  Delim:= False;
  for i:= 0 to Len - 1 do begin
    Delim:= P[i] in [#1..'/', ':'..'@', '['..'`', '{'..'~'];
    if Delim and (not PrevDelim) then begin
      Inc(WCount);
    end;
    PrevDelim:= Delim;
  end;
  if not Delim
  then Inc(WCount);
  Result:= WCount;
end;

function ReadSet(const S: ShortString): TBS;
var
  SS: ShortString;
  i: SmallInt;
  No: Word;
  TmpSet: TBS;
begin
  TmpSet:= [];
  SS:= '';
  for i:= 1 to Length(S) do begin
    case S[i] of
    ',',#32,';',':',#9: begin
          try
            No:= StrToInt(SS);
            Include(TmpSet, No);
          except
          end;
          SS:= '';
        end
    else begin
          SS:= SS + S[i];
        end;
    end; { case }
  end;
  { if last symbol is not colon ","}
  if SS <> '' then begin
    try
      No:= StrToInt(SS);
      Include(TmpSet, No);
    except
    end;
  end;
  ReadSet:= TmpSet;
end;

{ return set as longint
}
function SetAsLong(var B; Bytes: Byte): LongInt;
begin
  Result:= LongInt(B) and (1 shl (8*Bytes) - 1);
end;

function ExtractCurrency(const S: ShortString): Extended;
var
  SS: ShortString;
  NowCopeck: Boolean;
  roubles: String[20];
  copecks: String[20];
  i: SmallInt;
begin
  roubles:= '';
  copecks:= '';
  NowCopeck:= False;
  SS:= ANSIUpperCase(S);
  for i:= 1 to Length(SS) do begin
    case Upcase(SS[i]) of
    '0'..'9': begin
                if NowCopeck
                then Copecks:= Copecks+SS[i]
                else Roubles:= Roubles+SS[i];
              end;
    else      begin
                { skip any  non- digit chars }
                NowCopeck:= Roubles[0] > #0;
              end;
    end; { case }
  end;
  if Copecks = '' then begin
    { may be "69 коп." or "69 руб."}
    if Pos('К', SS) > Byte(Roubles[0]) then begin
      Copecks:= Roubles;
      Roubles:= '';
    end else begin
      Copecks:= '0';
    end;
  end; { if }
  SS:= Roubles+SysUtils.DecimalSeparator+Copecks;
  try
    ExtractCurrency:= StrToFloat(SS);
  except
    ExtractCurrency:= 0.0;
  end;
end;

{ MARC'S DT stamp is YYYYMMDDHH''"".D
             offset  1234567890123456
  for example,       19960924175127.0
  other dates may be in format YYYY
  for example,                 1991
}
function ExtractDateTime(const S: ShortString): Extended;
begin
  { verify format }
  if (Length(S) < 16) or (S[15] <> '.') then begin
    { return today date as result if format is wrong }
    try
      { as declared in Windows ("/"- delimited) }
      ExtractDateTime:= StrToDate(S);
    except
      try
        { may be 1991 year only? }
        ExtractDateTime:= EncodeDate(StrToInt(Copy(S,1,4)),1, 1);
      except
        ExtractDateTime:= Date;
      end;
    end;
    Exit;
  end;
  try
    ExtractDateTime:=EncodeDate(StrToInt(Copy(S,1,4)), StrToInt(Copy(S,5,2)), StrToInt(Copy(S,7,2)))+
    EncodeTime(StrToInt(Copy(S,9,2)), StrToInt(Copy(S,11,2)), StrToInt(Copy(S,13,2)),0);
  except
    { return today date as result if format is wrong }
    ExtractDateTime:= Date;
    Exit;
  end;
end;

function ExtractCurrencyFromBuf(var B; const Len: Word): Extended;
begin
  ExtractCurrencyFromBuf:= ExtractCurrency(ExtractString(B, Len));
end;

function ExtractDateTimeFromBuf(var B; const Len: Word): Extended;
begin
  ExtractDateTimeFromBuf:= ExtractDateTime(ExtractString(B, Len));
end;

constructor TNumberedTextFile. Create(FN: ShortString; AMode: Word);
begin
  Mode:= AMode;
  EndOfFile:= False;
  try
    Stream:= TFileStream.Create(FN, Mode);
  except
  EndOfFile:= True;
  end;
end;

function  TNumberedTextFile. GetLines: LongInt;
begin
  Result:= 0;
end;

procedure TNumberedTextFile. ReadLn(var S: ShortString);
begin
end;

procedure TNumberedTextFile. ReadList(var SL: TStringList);
begin
end;

destructor TNumberedTextFile. Done;
begin
  Stream. Free;
end;

{ compress siglas list like set
  Return: length of compressed list
}
const
  { step of numbers in list }
  LISTSTEP = 1;
{$DEFINE TOANSI}
{$IFNDEF TOANSI}
  HANTDELIMITER = #253;
{$ELSE}
  HANTDELIMITER = #164;
{$ENDIF}

const
  MAXSIGLEN = 13;
  TLC_LIMIT = 1000;
type
  tlc = record
    L: LongInt;
    count: Word;
    num: String[MAXSIGLEN];
  end;

  tlcar = array [0..TLC_LIMIT] of tlc;

{    for i:= 1 to Adds do begin
      Siglas.Strings[i-1]:= ChangeChar('*', '-', Siglas.Strings[i-1]);
    end;
}
function CompressList(CompCAPtr: Pointer; PrefixLen: SmallInt): SmallInt;
var
  Len: SmallInt;
  i, c, Count: SmallInt;
  Buffer: ShortString;
  SS: String;
  Ids, IdSorted: ^TLCAr;
  mini, found, rpt: SmallInt;
  min, min_prev, ID: LongInt;
  continue: Boolean;

procedure DoIt;
begin
  if (PrefixLen >= 0) and (PrefixLen < Length(SS)) then begin
    { it is number with prefix. Skip prefix first }
    System.Delete(SS, 1, PrefixLen);
    { delete '*' symbol (change to '-') }
    ChangeChar('*', '-', SS);
    try
      Ids^[C].L:= StrToInt(SS);
    except
      Ids^[C].L:= -9999;
    end;
  end else begin
    { not a number, convert }
    Ids^[C].L:= LongInt((@(SS[1]))^);
  end;
  Ids^[C].num:= SS;
  Inc(C);
  SS:= '';
end;

begin
  Buffer:= '';
  Len:= StrLen(CompCAPtr);
  CompressList:= 0;
  if PrefixLen >= Len
  then Exit;
  { count components }
  Count:= 1;
  for i:= 0 to Len - 1 do begin
    if CA(CompCAPtr^)[i]= HANTDELIMITER
    then Inc(Count);
  end;
  CompressList:= Count;
  { components array allocation }
  GetMem(Ids, Count * SizeOf(TLC));
  GetMem(IdSorted, Count * SizeOf(TLC));
  { parse }
  c:= 0;
  SS:= '';
  for i:= 0 to Len - 1 do begin
    if (CA(CompCAPtr^)[i]= HANTDELIMITER) then begin
      DoIt;
    end else begin
      SS:= SS + CA(CompCAPtr^)[i];
    end;
  end;
  DoIt;
  { sort }
  found:= 0;
  min_Prev:= -MaxLongInt;
  for c:= 0 to Count - 1 do begin
    min:= MaxLongInt;
    mini:= 0; { just skip warning about not initialized variable  }
    { счит след меньшее не найдено }
    Continue:= False;
    for i:= 0 to Count - 1 do begin
      if (Ids^[i].L < min) and (Ids^[i].L > min_prev) then begin
        min:= Ids^[i].L;
        mini:= i;
        { след меньшее найдено }
        Continue:= True;
      end;
    end;
    if Continue then begin
      { если быйло найдено следующее меньшее, добавить }
      IdSorted^[found]:= Ids^[mini];
      Inc(found);
    end;
    min_Prev:= min;
  end;
  { count qty of each }
  for c:= 0 to Found - 1 do begin
    rpt:= 0;
    ID:= Idsorted^[c].L;
    for i:= 0 to Count - 1 do begin
      if Ids^[i].L = ID
      then Inc(rpt);
    end;
    IdSorted^[c].Count:= rpt;
  end;
  { free up 2 buffer }
  FreeMem(Ids, Count * SizeOf(TLC));
  { build a list }
  continue:= False;
  if (PrefixLen >= 0) then begin
    { put the first }
    Buffer:= IntToStr(IdSorted^[0].L);
    ID:= IdSorted^[0].L;
    { loop other }
    for c:= 1 to Found - 1 do begin
      { последовательно }
      if (IdSorted^[c].L = ID + LISTSTEP) then begin
        continue:= True;
        { if last }
        if (c = Found-1) then begin
          Buffer:= Buffer + '-' + IntToStr(IdSorted^[c].L);
        end else begin
          { just accumulate }
        end;
      end else begin
        { элемент не последовательный }
        { старую последовательность закрыть если она была }
        if continue
        then Buffer:= Buffer + '-' + IdSorted^[c-1].Num;
        { неполедовательный элемент вписать }
        Buffer:= Buffer + ';' + IdSorted^[c].Num;
        continue:= False;
      end;
      ID:= IdSorted^[c].L;
    end;
  end else begin
    { not a number. Siglas of course }
    for c:= 0 to Found - 1 do begin
      Buffer:= Buffer + IdSorted^[c].num + '-'+
        IntToStr(IdSorted^[c].Count)+';';
    end;
  end;
  { free up 1 buffer }
  FreeMem(IdSorted, Count * SizeOf(TLC));
  StrPCopy(CompCAPtr, Buffer);
end;

function YesNoS(Yes: Boolean): ShortString;
begin
  if Yes
  then yesNoS:= 'Да'
  else yesNoS:= 'Нет';
end;

function ExtractUnixFileName(AFn: String): String;
var
  p: Integer;
begin
  p:= PosBack('/', AFn);
  if p > 1
  then Result:= Copy(AFn, p+1, MaxInt)
  else Result:= AFn;
end;

function ExtractUnixDosFileName(AFn: String): String;
var
  p: Integer;
begin
  p:= PosBack('/', AFn);
  if p > 1
  then Result:= Copy(AFn, p+1, MaxInt)
  else begin
    p:= PosBack('\', AFn);
    if p > 1
    then Result:= Copy(AFn, p+1, MaxInt)
    else Result:= AFn;
  end;
end;

{ stupid validation, what about win32? }
function IsValidURL(AURL: String): Boolean;
begin
  Result:= (Length(AUrl)>0) and (Pos(#9, AURL) <= 0) and
    (Pos(#32, AURL) <= 0);
end;

function isValidURLs(AURL: String): Boolean;
begin
  Result:= (Length(AUrl)>0) and (Pos(#9, AURL) <= 0) and
    (Pos(#32, AURL) <= 0);
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
begin
  L:= Length(host);
  IsIPaddress:= False;
  dotposition[0]:= 1;
  curp:= 1;
  repeat
    dotposition[curp]:= PosFrom(dotposition[curp - 1], '.', host);
    if dotposition[curp] = 0
    then dotposition[curp]:= L + 1;
    if isDecimal(Copy(host, dotposition[curp-1],
      dotposition[curp] - dotposition[curp-1]))
    then Inc(dotposition[curp])
    else Exit;
    Inc(curp);
  until curp > 4;
  if dotposition[4] <> L + 2
  then Exit;
  IsIPaddress:= True;
end;

function IP2Str(AIP: Cardinal): string;
type
  TIP = record
    a, b, c, d: Byte
  end;
begin
  Result := IntToStr(TIP(AIP).a)+'.'+IntToStr(TIP(AIP).b)+'.'+IntToStr(TIP(AIP).c)+'.'+IntToStr(TIP(AIP).d);
end;

{$IFNDEF VER80}
function GetBias: Integer;
var
  tzInfo: TTimeZoneInformation;
begin
  GetTimeZoneInformation(tzInfo);
  Result:= tzInfo.Bias;
end;
{$ENDIF}

{ "http:" "//" [user:password@] host [ ":" port ] [ abs_path ]
  return True, if ok
  return False if url is empty, or is DOS file name (port=80 anyway)
  bookamrk is strted with # if exists
}
function ParseUrl(url: String; var protocol, user, password, host, IPaddress, fn, bookmark: String; var port: Integer;
  const ADefprotocol: String; ADefPort: Integer): Boolean;
var
  L, p, pport, pfn: Integer;
  S: String;
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
  p:= PosBack('#', url);
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

  pfn:= PosFrom(p, '/', url);         { search abs_path part of url }
  if pfn > 0 then begin
    fn:= Copy(url, pfn, L - pfn + 1);
  end else begin
    fn:= '';                          { abs_path missed, return "/" }
    pfn:= L + 1;
  end;

  { default port and host }
  host:= Copy(url, p, pfn - p);
  // if '@' in host, there user[password]
  pport:= PosBack('@', host);
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

  pport:= PosFrom(p, ':', url);
  if pport > 0 then begin
    S:= Copy(url, pport + 1, pfn - pport - 1);
    if IsDecimal(S) then begin
      port:= StrToIntDef(S, ADefPort);
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
      // host:= '';
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
begin
  Result:= ParseUrl(url, protocol, user, password, host, IPaddress, baseDN, bookmark, port, ADefprotocol, ADefPort);
  if Length(bookmark) > 0
  then baseDN:= baseDN + '#' + bookmark;
  Attributes:= GetToken(2, '?', baseDN);
  Scope:= GetToken(3, '?', baseDN);
  Filter:= GetToken(4, '?', baseDN);
  // delete '/' from basedn
  baseDN:= GetToken(1, '?', baseDN);
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
  Result:= util1.ComposeUrl('ldap', Trim(AUser), Trim(APassword), Trim(AHost), ldapurltail, '', APort);
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

{ S           Result
  /index.htm  '/'
  my.html     ''
  /a/df       '/a/'
}
function ExtractPathurlfn(S: String): String;
var
  p: Integer;
begin
  p:= PosBack('/', S);
  if p > 0
  then ExtractPathurlfn:= Copy(S, 1, p)
  else ExtractPathurlfn:= '';
end;

{ validate url and if it is relative path, add root }
{$IFNDEF VER80}
function MkAbsoluteURL(Aroot, Aurl: String): String;
var
  prot, user, password, host, IP, fn: String;
  rprot, ruser, rpassword, rhost, rIP, rfn: String;
  rlbl, lbl: String;
  port, rport: Integer;
  i: Integer;
begin
  ParseUrl(ARoot, rprot, ruser, rpassword, rhost, rIP, rfn, rlbl, rport, 'http', 80);
  i:= LastDelimiter('/', rfn);
  rfn:= Copy(rfn, 1, i);

  ParseUrl(Aurl, prot, user, password, host, IP, fn, lbl, port, 'http', 80);
  if (host = '') and (ip = '') then begin
    if (Length(fn) >=1) and (fn[1] = '/')
    then
    else fn:= ExtractPathurlfn(rfn) + fn;
    { or IP, or HOST - -> host+ip = host or ip }
    Result:= rprot + '://' + rhost + rip + ':' + IntToStr(rport) + httpConcatPath(rfn, fn) + lbl;
  end else begin
    Result:= AUrl;
  end;
end;
{$ENDIF}

function ExtractUrlAddress(url: String): String;
var
  protocol, user, password, host, ip, fn, lbl: String;
  port: Integer;
begin
  ParseUrl(url, protocol, user, password, host, ip, fn, lbl, port, 'http', 80);
  ExtractUrlAddress:= ip;
end;

function ExtractUrlHost(url: String): String;
var
  protocol, user, password, host, ip, fn, lbl: String;
  port: Integer;
begin
  ParseUrl(url, protocol, user, password, host, ip, fn, lbl, port, 'http', 80);
  ExtractUrlHost:= host;
end;

function ExtractUrlPort(url: String): Integer;
var
  protocol, user, password, host, ip, fn, lbl: String;
  port: Integer;
begin
  ParseUrl(url, protocol, user, password, host, ip, fn, lbl, port, 'http', 80);
  ExtractUrlPort:= port;
end;

function ExtractUrlFileName(url: String): String;
var
  protocol, user, password, host, ip, fn, lbl: String;
  port: Integer;
begin
  ParseUrl(url, protocol, user, password, host, ip, fn, lbl, port, 'http', 80);
  ExtractUrlFileName:= fn;
end;

function GetFileNameOnly(const s: String): String;
var
  p: Integer;
begin
  Result:= s;
  p:= Pos('\', Result);
  while p > 0 do begin
    Delete(Result, 1, p);
    p:= Pos('\', Result);
  end;
  p:= Length(Result) - 3;
  if (p > 1) and (Result[p] = '.')
  then Delete(Result, p, MaxInt);
end;

function ExtractUrlProtocol(url: String): String;
var
  protocol, user, password, host, ip, fn, lbl: String;
  port: Integer;
begin
  ParseUrl(url, protocol, user, password, host, ip, fn, lbl, port, 'http', 80);
  ExtractUrlProtocol:= protocol;
end;

function IsFileMask(const FN: ShortString): Boolean;
begin
  IsFileMask:= (Pos('?', FN) > 0) or
    (Pos('*', FN) > 0);
end;

{ compare 2 file names
}
function FilesSimilar(S1, S2: String): Boolean;
begin
  Result:= AnsiCompareText(ExpandFileName(S1), ExpandFileName(S2)) = 0;
end;

function AnsiDiffText(AS1, AS2: String): Integer;
var
  S1, S2: String;
  i, L, L2: Integer;
begin
  S1:= ANSIUpperCase(AS1);
  S2:= ANSIUpperCase(AS2);
  L:= Length(S1);
  L2:= Length(S2);
  if L2 < L
  then L:= L2;
  for i:= 1 to L do begin
    if s1[i] <> s2[i] then begin
      AnsiDiffText:= i - 1;
      Exit;
    end;
  end;
  AnsiDiffText:= L;
end;

{ if file or folder "NESTED" is nested in "PARENT" folder return TRUE
}
function IsFolderParent(Parent, Nested: String): Boolean;
var
  w, nlen, plen: Integer;
begin
  nlen:= Length(Nested);
  plen:= Length(Parent);
  if nlen = 0 then begin
    IsFolderParent:= True;
    Exit;
  end;
  IsFolderParent:= False;
  if (plen > nlen) or (plen = 0)
  then Exit;
  w:= AnsiDiffText(Parent, Nested);
  if w = 0 then begin
    Exit;
  end;
  if nested[w] in ['/','\'] then begin
    IsFolderParent:= w = plen;
  end else begin
    IsFolderParent:= (w = plen) and (Nested[w+1] in ['/','\']);
  end;
end;

function DiffPath(Parent, Nested: String): String;
var
  w: Integer;
  i, j: Integer;
  cunix, ccpm: Integer;
begin
  if IsFolderParent(Parent, Nested) then begin
    w:= AnsiDiffText(Parent, Nested);
    Result:= Copy(Nested, w + 1, Length(Nested) - w);
  end else begin
    Result:= Nested;
    if IsAbsolutePath(Result)
    then Exit;
    { try to find out common parts }
    cunix:= 0;
    ccpm:= 0;

    for i:= 1 to Min(Length(Parent), Length(Nested)) do begin
      if CompareText(Parent[i], Nested[i]) <> 0 then begin
        { return to first '/' or '\' }
        w:= i;
        for j:= i downto 1 do begin
          case Nested[j] of
            '/', '\': begin
                w:= j + 1;
                Break;
              end;
          end;
        end;
        { copy differ part }
        Result:= Copy(Nested, w, MaxInt);
        { calc count of ..\ }
        for j:= w to Length(Parent) do begin
          case Parent[j] of
            '/': Inc(cunix);
            '\': Inc(ccpm);
          end;
        end;
        if cunix > ccpm then begin
          Result:= DupeString('../', cunix) + Result;
        end else begin
          Result:= DupeString('..\', ccpm) + Result;
        end;
        Break;
      end; { if }
    end; { for }
  end;
end;

function CountFilesLike(AMask: String): Integer;
var
  SearchRec: TSearchRec;
begin
  if FindFirst(AMask, faAnyFile, SearchRec)=0 then begin
    Result:= 1;
    while FindNext(SearchRec) = 0
    do Inc(Result);
  end else Result:= 0;
{$IFDEF VER80}
  FindClose(SearchRec);
{$ELSE}
  Windows.FindClose(SearchRec.FindHandle);
{$ENDIF}
end;

function DeleteFiles(AMask: String): Integer;
var
  SearchRec: TSearchRec;
begin
  Result:= 0;
  if FindFirst(AMask, faAnyFile, SearchRec)=0 then begin
    try
      SysUtils.DeleteFile(SearchRec.Name);
      Result:= 1;
    except
    end;
    while FindNext(SearchRec) = 0 do begin
      try
        SysUtils.DeleteFile(SearchRec.Name);
        Inc(Result);
      except
      end;
    end;
  end;
{$IFDEF VER80}
  FindClose(SearchRec);
{$ELSE}
  Windows.FindClose(SearchRec.FindHandle);
{$ENDIF}
end;

function IsAbsolutePath(const APath: String): Boolean;
var
  L: Integer;
begin
  L:= Length(APath);
  Result:= ((L > 0) and (APath[1] in ['/','\'])) or { unc- \\servername\path }
    ((L>=2) and (APath[2]=':')); { dos- like (C:\) }
end;

function IsDosPath(const APath: String): Boolean;
begin
  Result:= (Pos('\', APath) > 0) { or (Pos(':', APath) > 0); }
end;

function ExpandRelativePath(const BaseName, AFn: String): String;
begin
  if IsAbsolutePath(AFn)
  then Result:= AFn
  else Result:= ConcatPath(BaseName, AFn);
end;

function MkFullFromRelativeFileName(const AFN, AFolder: String): String;
var
  L: Integer;
  Path1, Folder1: String;
begin
  Path1:= ANSIUppercase(ExtractFilePath(AFN));
  Folder1:= ANSIUppercase(ExtractFilePath(AFolder));
  if (Path1 = '') or (AFolder = '') then begin { если не указан путь, относительно чего.. }
    { точно в папке- нет пути }
    Result:= AFn;
  end else begin
    if Pos(Folder1, Path1) = 1 then begin
      { пути совпадают в начале }
      L:= Length(Folder1);
      { сделать относительный путь }
      Result:= Copy(Path1, L+1, 255) + ExtractFileName(AFN);
    end else begin
      { начало не совпадает, вернуть как есть }
      Result:= AFn;
    end;
  end;
end;

function DirExists(fn: String):  Boolean;
var
  DirInfo: TSearchRec;
begin
  DirExists:= False;
  if fn = ''
  then Exit;
  if fn[Length(fn)]='\'
  then System.Delete(fn, Length(fn),1);
  DirExists:= FindFirst(fn, faDirectory + faArchive, Dirinfo ) = 0;
{$IFDEF VER80}
  FindClose(DirInfo);
{$ELSE}
  Windows.FindClose(DirInfo.FindHandle);
{$ENDIF}
end; { DirExists }

function TranslateChar(const Str: string; FromChar, ToChar: Char): string;
var
  I: Integer;
begin
  Result := Str;
  for I := 1 to Length(Result)
  do if Result[I] = FromChar
    then Result[I] := ToChar;
end;

function UnixPathToDosPath(const Path: string): string;
begin
  Result := TranslateChar(Path, '/', '\');
end;

function DosPathToUnixPath(const Path: string): string;
begin
  Result := TranslateChar(Path, '\', '/');
end;

procedure Walk_Tree(AFilePattern, Start: String; Attr: Integer; Recursive: boolean; DoIt: TOnProcess_File;
  AEnv: TObject);
var
  SR: TSearchRec;
  Status: Integer;
  L: Integer;
begin
  if not Assigned(Doit)
  then Exit;

  L:= Length(Start);
  if (L > 0) and (Start[L] = '\')
  then SetLength(Start, L - 1); { strip trailing '\' just in case}
  Attr:= Attr and (not faDirectory);
  Status:= FindFirst(ConCatPath(Start, AFilePattern, '\'), Attr, SR);
  while Status = 0 do begin
    if not DoIt(ConcatPath(Start, SR.Name, '\'), AEnv)
    then Exit;
    Status:= FindNext(SR);
  end;

  if Recursive then begin
    SysUtils.FindClose(SR); { Needed for WIN32 }
    Attr:= Attr or faDirectory;
    Status:= FindFirst(ConCatPath(Start, '*.*', '\'), Attr, SR);
    while Status = 0 do begin
      if (SR.name <> '.') and (SR.name <> '..') then begin
        Walk_Tree(AFilePattern, ConCatPath(Start, SR.name, '\'), Attr, True, DoIt, AEnv);
      end;
      Status:= FindNext(SR);
    end;
  end;
  SysUtils.FindClose(SR); { Needed for WIN32 }
end;

procedure Walk_DirTree(Start: String; DoIt: TOnProcess_File;
  AEnv: TObject);
var
  SR: TSearchRec;
  Status: Integer;
  L: Integer;
begin
  if not Assigned(Doit)
  then Exit;

  L:= Length(Start);

//  if (L > 0) and (Start[L] = '\')
//  then Start:= Start + '.'; // SetLength(Start, L - 1); // strip trailing '\' just in case

  SysUtils.FindClose(SR); { Needed for WIN32 }
  Status:= FindFirst(ConCatPath(Start, '*.*', '\'), faDirectory, SR);
  while Status = 0 do begin
    if (SR.name = '.') or (SR.name = '..') or ((sr.Attr and faDirectory) = 0) then begin
    end else begin
      if not DoIt(ConcatPath(Start, SR.Name, '\'), AEnv)
      then Exit;
      Walk_DirTree(ConCatPath(Start, SR.name, '\'), DoIt, AEnv);
    end;
    Status:= FindNext(SR);
  end;

  SysUtils.FindClose(SR); { Needed for WIN32 }
end;

{ add '\' if needed }
function  ConcatPath(const Path: String; FileName: String; APathDelimiter: Char = #0): String;
var
  Dlmt: Char;
begin
  if (Length(FileName) = 0)  then begin
    ConcatPath:= Path;
    Exit;
  end;
  if (Length(Path) = 0)  then begin
    ConcatPath:= FileName;
    Exit;
  end;
  if IsAbsolutePath(FileName)
  then ConcatPath:= FileName
  else begin
    if APathDelimiter = #0 then begin
      if IsDosPath(Path) or IsDosPath(FileName)
      then Dlmt:= '\'
      else Dlmt:= '/';
    end else Dlmt:= APathDelimiter;
    if FileName[1] = '.'
    then Delete(FileName, 1, 1);

    if (Length(FileName) > 0) and (FileName[1] = Dlmt)
    then Delete(FileName, 1, 1);
    if (Length(FileName) > 0) and (Path[Length(Path)] <> Dlmt)
    then Result:= Path + Dlmt + FileName
    else Result:= Path + FileName;
  end;
end; { ConcatPath }

{$IFNDEF VER80}
{ Aliases - строки вида: /Алиас=C:\path,,4 }
function ConcatAliasPath(const Aliases: TStrings; const DefPath: String; FileName: String): String;
var
  S: String;
  p, L: Integer;
  defpathphys: String;
begin
  Result:= FileName;

  if Length(Result) <= 0
  then Exit;

  defpathphys:= DefPath;
  if (Length(defpathphys) > 0) and (defpathphys[1] = '/') then begin
    p:= Aliases.IndexOfName(defpathphys);
    if p >= 0 then begin
      defpathphys:= GetToken(1, ',', Aliases.ValueFromIndex[p]);
    end;
  end;
  { первый элемент пути: /scripts/  из /scripts/dir/... }
  if Result[1] = '/' then begin
    p:= PosFrom(2, '/', Result);
    if p = 0 then begin
      Delete(Result, 1, 1);
      Result:= httpConcatPath(GetToken(1, ',', Aliases.Values['/']), Result);
    end else begin
      S:= Copy(Result, 1, p);
      L:= Length(S);
      if L > 0 then begin
        Delete(S, L, 1); { /a/ удалить последний слеш из алиаса }
        if Aliases.IndexOfName(S) >= 0 then begin
          { присоединить алиас, на конце не должно быть "\" }
          Delete(Result, 1, L); { удалить алиас из имени файла - имя файла без переднего слеша }
          Result:= httpConcatPath(GetToken(1, ',', Aliases.Values[S]), Result);
        end else begin
          if Aliases.IndexOfName('/') >= 0
          then Result:= httpConcatPath(GetToken(1, ',', Aliases.Values['/']), Result)
          else Result:= httpConcatPath(GetToken(1, ',', defpathphys), Result);
        end;
      end;
    end;
  end else begin
    { unix (web) -> dos directory delimiter }
    // ChangeChar('/', '\', Result);
    // delete c:\
    if IsAbsolutePath(FileName) then begin
      S:= '';
    end else begin
      // relative path
      if IsAbsolutePath(defpathphys) // Sep 30 2003
      then S:= defpathphys
      else begin
        if Aliases.IndexOfName('/') >= 0
        then S:= GetToken(1, ',', Aliases.Values['/'])
        else if Aliases.IndexOfName('') >= 0
          then S:= GetToken(1, ',', Aliases.Values[''])
          else S:= defpathphys;
      end;
    end;
    Result:= httpConcatPath(S, Result);
  end;
  ChangeChar('/', '\', Result);
end;
{$ENDIF}

{ similar to ConcatPath except (..) }
function httpConcatPath(ARoot, FileName: String): String;
var
  p: Integer;
  u: Boolean;
begin
  if (Length(FileName) >= 1) and (Length(ARoot) > 0) then begin
    if (FileName[1] in ['/', '\']) then begin
      // extract root from root
      p:= Pos('//', ARoot);
      if p <= 0
      then p:= -1;
      p:= PosFrom(p + 2, FileName[1], ARoot);
      Delete(ARoot, p, MaxInt);
      if ARoot[Length(ARoot)] in ['/','\']
      then Delete(FileName, 1, 1);
    end else begin
      p:= Length(ARoot);
      if not (ARoot[p] in ['/','\']) then begin
        u:= util1.PosBack('/', ARoot) > util1.PosBack('\', ARoot);
        if u
        then FileName:= '/' + FileName
        else FileName:= '\' + FileName;
      end;
    end;
  end;
  Result:= ARoot + FileName;
  { just change /../ to /./ }
  repeat
    p:= Pos('..', Result);
    if p > 0
    then Delete(Result, p, 1)
    else Break;
  until False;
end;

{$IFNDEF VER80}{Revised: Dec 18 2006. Was: Rp: PWideChar -- it was wrong! }
function Char2Hex(DEC: WideChar): ShortString;
const
  HEXDigts: string[16] = '0123456789ABCDEF';
var
  I, J: LONGINT;
begin
  if (DEC = #0) then begin
    Result:= '%00';
    Exit;
  end;
  Result:= '%';
  I:= 0;
  while (1 shl ((I + 1) * 4)) <= Word(DEC) do
    I:= I + 1;
  for J:= 0 to I do begin
    Result:= Result + HEXDigts[(Word(DEC) shr ((I - J) * 4)) + 1];
    Word(DEC):= Word(DEC) and ((1 shl ((I - J) * 4)) - 1);
  end;
  if Length(Result) = 2 then Insert('0', Result, 2);
end;

function HTTPDecode(const AStr: String; AUtf8: Boolean): WideString;
var
  Sp, Cp: PANSIChar;
  Rp: PChar;
  r: String;
begin
  SetLength(r, Length(AStr));
  Sp:= PChar(AStr);
  Rp:= PChar(r);
  while Sp^ <> #0 do begin
    if Sp^ in ['+','%'] then begin
      if Sp^ = '+' then begin
        Rp^:= ' '
      end else begin
        Inc(Sp);
        if Sp^ = '%' then begin
          Rp^:= '%'
        end else begin
          Cp:= Sp;
          Inc(Sp);
          Rp^:= Char(StrToInt(Format('$%s%s',[Cp^, Sp^])));
        end;
      end;
    end else begin
      Rp^:= Sp^;
    end;
    Inc(Rp);
    Inc(Sp);
  end;
  SetLength(r, Rp - PChar(r));

  if AUtf8 then begin
    // try to decode
    Result:= UTF8Decode(r);
    if Length(Result) = 0 // if failed, recover
    then Result:= r;
  end else Result:= r;
end;

function HTTPEncode(const AStr: WideString; AUtf8: Boolean): String;
const
  NoConversion = ['A'..'Z','a'..'z','*','@','.','_','-',
                  '0'..'9','$','!','''','(',')'];
var
  Sp: PChar;
  Rp: PChar;
  buf: String;
  wc: WideChar;
begin
  if AUTF8
  then buf:= utf8Encode(AStr)
  else buf:= AStr;

  SetLength(Result, Length(buf) * 3); // 3
  Sp:= PChar(buf);
  Rp:= PChar(Result);
  while Sp^ <> #0 do begin
    if Sp^ in NoConversion then
      Rp^:= PChar(Sp)^
    else
      if Sp^ = #32 then Rp^:= '+' else begin
        FormatBuf(Rp^, 3, '%%%.2x', 6, [Ord(Sp^)]);
        Inc(Rp,2);
      end;
    Inc(Rp);
    Inc(Sp);
  end;
  SetLength(Result, Rp - PChar(Result));
end;

function HTTPEncode2(const AStr: String): String;
const
  NoConversion = ['A'..'Z','a'..'z','*','@','.','_','-',
                  '0'..'9','$','!','''','(',')'];
var
  Sp, Rp: PChar;
  wc: Char;
begin
  SetLength(Result, Length(AStr) * 3);
  Sp:= PChar(AStr);
  Rp:= PChar(Result);
  while Sp^ <> #0 do begin
    if Sp^ in NoConversion then begin
      Rp^:= Sp^
    end else begin
      FormatBuf(Rp^, 3, '%%%.2x', 6, [Ord(Sp^)]);
      Inc(Rp,2);
    end;
    Inc(Rp);
    Inc(Sp);
  end;
  SetLength(Result, Rp - PChar(Result));
end;

{$ENDIF}

function ReplaceExt(Ext: ExtStr;Pat: PathStr): String;
var
  f: Integer;
begin
 if Ext[1] <> '.' then Ext:='.' + Ext;
 f:= PosBack('.', Pat);
 if f < PosBack('\', Pat)
 then f:= 0;
 if f <> 0 then
   System. Delete(Pat, f, MaxInt);
   ReplaceExt:= Pat + Ext;
end; { ReplaceExt }

function ExtractFileNameWOext(AFn: String): String;
begin
  {!!---- I am not sure about alone dot }
  Result:= ChangeFileExt(ExtractFileName(AFn), '');
end;

{ ExecuteFile описан в справочной системе, но эта функция не реализована
  так что winapi }
{$IFNDEF VER80}
function EExecuteFile(AFileName: String): Boolean;
var
  i: Integer;
  p, pars, defdir: array[0..127] of Char;
  pcmd, ppars, pdefdir: Pointer;
  ErrCode: Integer;
  hwnd1: HWND;
  cmd: array[0..15] of Char;

begin
  Result:= True;
  StrPCopy(defdir, ExtractFilePath(AFileName));

  i:= Pos(#32, AFileName);
  if i > 0 then begin
    StrPCopy(p, Copy(AFileName, 1, i -1));
    StrPCopy(pars, Copy(AFileName, i + 1, MaxInt));
    ppars:= @pars;
  end else begin
    StrPCopy(p, AFileName);
    pars:= #0;
    ppars:= Nil;
  end;
  // if isDosPath(p) then p:= ExpandFileName(String(PChar(@p)));
  pdefdir:= @defdir;
  cmd:= 'open';
  if Pos('.EXE', UpperCase(p)) > 0
  then pcmd:= Nil
  else pcmd:= @cmd;
  // hwnd1:= Application.Handle;
  hwnd1:= 0;
  if ShellAPI.ShellExecute(hwnd1, pcmd, p, ppars, pdefdir, SW_SHOWNORMAL) <=32 then begin
    // от оболочки отказ, тогда как текст
    StrPCopy(p, 'notepad ' + AFileName);
    ErrCode:= WinExec(p, SW_SHOWNORMAL);
    if ErrCode <=32 then begin
      // Result:= False;
      raise EExternal.CreateFmt('Ошибка %s при выполнении  %s', [WinExecErr(ErrCode), AFileName]);
    end;
  end;
end;
{$ENDIF}

function WinExecErr(No: Word): ShortString;
var
  S: ShortString;
begin
  case No of
  0: S:= 'Недостаточно памяти, EXE файл поврежден';
  2: S:= 'Программа не найдена';
  3: S:= 'Путь не найден';
  5: S:= 'Attempt was made to dynamically link to a task';
  6: S:= 'Library required separate data segments for each task.';
  8: S:= 'There was insufficient memory to start the application.';
  10: S:= 'Windows version was incorrect.';
  11: S:= 'Executable file was invalid. Either it was not a Windows application';
  12: S:= 'Application was designed for a different operating system.';
  13: S:= 'Application was designed for MS-DOS 4.0.';
  14: S:= 'Type of executable file was unknown.';
  15: S:= 'Attempt was made to load a real-mode application';
  16: S:= 'Attempt was made to load a second instance of an executable file';
  19: S:= 'Attempt was made to load a compressed executable file.';
  20: S:= 'Dynamic-link library (DLL) file was invalid.';
  21: S:= 'Application requires 32-bit extensions.';
  else
  S:= 'Неизвестная ошибка';
  end;
  WinExecErr:= S;
end;

function GetLastErrorDescription(AError: DWORD): WideString;
begin
  if (AError = 0)
  then Result:= ''
  else begin
    SetLength(Result, 1024);
    SetLength(Result, FormatMessageW(FORMAT_MESSAGE_FROM_SYSTEM, Nil,
      AError, LOCALE_USER_DEFAULT, PWideChar(Result), Length(Result), Nil));
  end;
end;

{$IFNDEF VER80}
{  x:    0-1 2-3 4-7 8-F 10-1F ..
   exp2:  0   1   2   3    4   ..
}
function exp2(x: Word): Word; assembler;asm
  MOV BX,X
  BSF AX,BX
end;
{$ENDIF}

{ x      :   0-1 2-3 4-7 8-F 10-1F ..
  near2m :    1   2   4   8    10  ..
}
function near2m(x: Word): Word; assembler;asm
  mov   ax,1
  mov   bx,x
@cont: shr   bx,1
  { cmp   bx,0 }
  jz    @find
  shl   ax,1
  jmp   @cont
@find:
end;

function File2String(const AFN: String): String;
var
  Cnt: Integer;
  OFile: TFileStream;
begin
  Result:= '';
  if not FileExists(AFN)
  then Exit;
  try
    OFile:= TFileStream.Create(AFN, fmOpenRead);
  except
    Exit;
  end;
  try
    cnt:= OFile.Size;
    SetLength(Result, cnt);
    if Cnt > 0
    then OFile.Read(Result[1], Cnt);
  finally
    OFile.Free;
  end;
end;

function File2String(const AFN: WideString): WideString; overload;
var
  OFile: TFileStream;

  Size,
  BytesRead: Integer;
  Order: WideChar;
  SW: WideString;
  SA: string;
begin
  Result:= '';
  if not FileExists(AFN)
  then Exit;
  try
    OFile:= TFileStream.Create(AFN, fmOpenRead);
  except
    Exit;
  end;
  try
    Size:= OFile.Size;
    BytesRead:= OFile.Read(Order, 2);
    if (Order = BOM_LSB_FIRST) or (Order = BOM_MSB_FIRST) then begin
      SetLength(Result, (Size - 2) div 2);
      OFile.Read(PWideChar(Result)^, Size - 2);
      if Order = BOM_MSB_FIRST then
        StrSwapByteOrder(PWideChar(Result));
    end else begin
      // without byte order mark it is assumed that we are loading ANSI text
      OFile.Seek(-BytesRead, soFromCurrent);
      SetLength(Result, Size);
      OFile.Read(PChar(Result)^, Size);
    end;
  finally
    OFile.Free;
  end;
end;

function String2File(const AFN: String; const S: String; ARewrite: Boolean = False): Boolean;
var
  OFile: TFileStream;
  L: Integer;
begin
  Result:= False;
  try
    if FileExists(AFN) and (not ARewrite) then begin
      OFile:= TFileStream.Create(AFN, fmOpenReadWrite);
      if ARewrite
      then
      else OFile.Position:= OFile.Size;
    end else begin
      OFile:= TFileStream.Create(AFN, fmCreate);
    end;
    L:= Length(S);
    if L > 0
    then OFile.Write(S[1], L);
    OFile.Free;
  finally
  end;
  Result:= True;
end;

function String2File(const AFN: WideString; const S: WideString; ARewrite: Boolean = False): Boolean; overload;
var
  OFile: TFileStream;
  L: Integer;
  BOM: WideString;

begin
  Result:= False;
  try
    if FileExists(AFN) then begin
      OFile:= TFileStream.Create(AFN, fmOpenReadWrite);
      if ARewrite
      then
      else OFile.Position:= OFile.Size;
    end else begin
      OFile:= TFileStream.Create(AFN, fmCreate);
    end;
    L:= Length(S);
    if L > 0 then begin
      BOM := BOM_LSB_FIRST;
      OFile.WriteBuffer(PWideChar(BOM)^, 2);
      // SW has already been filled
      OFile.WriteBuffer(PWideChar(S)^, 2 * Length(S));
     end;
    OFile.Free;
  finally
  end;
  Result:= True;
end;

function SortFile(const AFN: String; AReserved: Integer): Boolean;
var
  sl: TStringList;
begin
  Result:= False;
  try
    if FileExists(AFN) then begin
      sl:= TStringList.Create;
      with sl do begin
        LoadFromFile(AFn);
        Sorted:= True;
        SaveToFile(AFn);
        Free;
      end;
    end;
  finally
  end;
  Result:= True;
end;

// create temporary file name
function CreateTemporaryFileName(const APrefix: String): String;
var
  ptempfn: array[0..511] of Char;
  temppath: array[0..255] of Char;
begin
  Windows.GetTempPath(255, PChar(@temppath));
  Result:= APrefix;
  Windows.GetTempFileName(PChar(@temppath), PChar(Result), 0, ptempfn);
  Result:= ptempfn;
end;

{$IFNDEF VER80}
procedure ANSI2OEM(var AData: String);
var
  L: Integer;
begin
  L:= Length(AData);
  SetLength(AData, L + 4);
  AnsiToOem(PChar(AData), PChar(AData));
  SetLength(AData, L);
end;

procedure OEM2ANSI(var AData: String);
var
  L: Integer;
begin
  L:= Length(AData);
  SetLength(AData, L + 4);
  OemToANSI(PChar(AData), PChar(AData));
  SetLength(AData, L);
end;

function CvtFile2OEM(AFN: String): Boolean;
var
  OFile: TextFile;
  c: Char;
  SS: String;
  L: Integer;
begin
  Result:= False;
  if FileExists(AFN) then begin
    AssignFile(OFile, AFN);
    Reset(OFile);
    if IOResult <> 0
    then Exit;
    SS:= '';
    while not EOF(OFile) do begin
      Read(OFile, c);
      SS:= SS + c;
    end;
    L:= Length(SS);
    SetLength(SS, L+4);
    AnsiToOem(PChar(SS), PChar(SS));
    SetLength(SS, L);
    CloseFile(OFile);
    DeleteFile(PChar(AFN));
    String2File(AFN, SS);
  end;
end;
{$ENDIF}

{ 12A&*34 -> 1234 }
function GetOnlyDecimalDigits(const S: String): Longint;
var
  i: Integer;
  SS: String;
begin
  SS:= '';
  for i:= 1 to Length(S) do begin
    if s[i] in ['0'..'9'] then begin
      SS:= SS + s[i];
    end;
  end;
  try
    if Length(SS) > 0
    then GetOnlyDecimalDigits:= StrToInt(SS)
    else GetOnlyDecimalDigits:= 0;
  except
    GetOnlyDecimalDigits:= 0;
  end;
end;

procedure SaveDecimalDigitsOnly(var S: String);
var
  i: Integer;
  SS: String;
begin
  for i:= 1 to Length(S) do begin
    if s[i] in ['0'..'9'] then begin
      SS:= SS + s[i];
    end;
  end;
  S:= SS;
end;

function  GetSeg(var Size: Word): Pointer;
var
  p : pointer;
  S : LongInt;
begin
{$IFNDEF VER80_32}
  S:= MaxAvail;
{$ELSE}
  S:= $FFF8;
{$ENDIF}
  if S > $FFF8 then S:= $FFF8;  { Borland Pascal heap manager limitation }
  GetMem(p,S);
  Size:= S;
  GetSeg:= p;
end; { GetSeg }

procedure ExpandHole(Strm: TFileStream; Pos, OldSize, NewSize: LongInt);
var
  EndPos,
  MoveChain,
  Cur,
  FS,
  Shift: LongInt;
  Buf: Pointer;
  Swap,
  SwapSize: Word;
begin
  Shift:= NewSize - OldSize;
  FS:= Strm. Size;
  EndPos:= FS - 1;
  MoveChain:= FS - Pos - OldSize;  { size of moving cluster }
  if (Shift=0) or (MoveChain<=0) then Exit;

  Buf:= GetSeg(SwapSize);
  if SwapSize = 0 then begin
    Exit;
  end;
  if MoveChain < SwapSize then
    Swap:= MoveChain else Swap:= SwapSize;
  if Shift>0 then begin  { begin from tail }
    Cur:= FS-Swap;
    repeat
      Strm. Position:= Cur;
      Strm. Read(Buf^, Swap);
      Strm. Position:= Cur+Shift;
      Strm. Write(Buf^, Swap);
      if Cur=Pos then Exit;
      Dec( Cur, Swap);
      if Cur < Pos then begin
        Dec(Swap, Pos - Cur);
        Cur:= Pos;
      end;
    until False;
  end else begin { begin from Pos }
    Cur:= Pos+OldSize;
    repeat
      Strm.Position:= Cur;
      Strm.Read(Buf^, Swap);
      Strm.Position:= Cur+Shift;{ Shift < 0 }
      Strm.Write(Buf^, Swap);
      Inc(Cur, Swap);
      if Cur + Swap > FS then begin
        Swap:= FS - Cur;
      end; { if }
    until Cur>=EndPos;
  end;
  FreeMem(Buf, SwapSize);
end; { ExpandHole }

procedure GraphicFieldToBMP(const FN: String);
begin
  { not implemented }

end;


type
  A_A = array[0..7] of Byte;

function Hex8(S: ShortString; var AA): LongInt;
var
  i, j   : Byte;
  Shift  : Word;
  A: Word;
begin
  Hex8:= 0;
  for i:= 1 to Byte(S[0]) do S[i]:= Upcase(S[i]);
  if (Pos('H',S) <> 0)
  then Delete(S, Pos('H',S), 255);
  Shift:= 0;
  for i:= Byte(S[0]) downto 1 do begin
    if s[i] >= 'A'
    then A:= Byte(s[i]) - 65 + 10
    else begin
      if s[i] in [#0, #32]
      then A:= 0
      else A:= Byte(s[i]) - 48;
    end;
    if A >= 16 then begin
      for j:= 0 to Shift div 2
      do A_A(AA)[j]:= 0;
      Exit;
    end;
    A_A(AA)[Shift div 2]:= A_A(AA)[Shift div 2] or (A shl (4 * (Shift mod 2)));
    Inc(Shift);
  end; { for }
  Hex8 := LongInt(AA);
end; { Hex8 }

procedure strxlat(var xlat; var S: String);
var
  i: Integer;
begin
  for i:= 1 to Length(S) do begin
    S[i]:= CA(xlat)[Byte(S[i])];
  end;
end;

procedure strxlat0(var xlat; var S);
var
  i: Word;
begin
  i:= 0;
  while CA(S)[i] <> #0 do begin
    CA(S)[i]:= CA(xlat)[BA(S)[i]];
    Inc(i);
  end;
end;

{ возвращает латинский аналог буквы кириллицы и наоборот
  кир лат
  A   A
  Б   B, etc
}
function CyrLatEQ(Key: Char): Char;
begin
  { НЕ РЕАЛИЗОВАНО }
  Result:= Key;
end;

const
  BASE62 = 10+26*2;
{ преобразует число в представление знаками 0-9;A-z }
function   Int2Alpha(AValue: Integer): String;
var
  S: String;
  R: Integer;
  M: Byte;
begin
  {}
  S:= '';
  R:= AValue;
  repeat
    M:= R mod BASE62;
    if M < 10
    then M:= Byte('0') + M
    else begin
      if M < (26 + 10)
      then M:= Byte('A') + M - 10
      else M:= Byte('a') + M - 10 - 26;
    end;
    S:= Char(M) + S;
    R:= R div BASE62;
  until R = 0;
  Result:= S;
end;

function   Alpha2Int(const AValue: String): Integer;
var
  R: Integer;
  Shft: Integer;
  i, V: Byte;
begin
  {}
  R:= 0;
  Shft:= 1;
  for i:= Length(Avalue) downto 1 do begin
    V:= Byte(AValue[i]);
    if Char(V) < 'A'
    then V:= V - Byte('0')
    else begin
      if Char(V) < 'a'
      then V:= V - Byte('A') + 10
      else V:= V - Byte('a') + 10 + 26;
    end;
    R:= R + V * Shft;
    if i > 1
    then Shft:= Shft * BASE62;
  end;
  Result:= R;
end;

function   IsExtendedASCII(S: String): Boolean;
var
  i: Integer;
begin
  Result:= False;
  for i:= 1 to Length(S) do begin
    if S[i] > #127 then begin
      Result:= True;
      Exit;
    end;
  end;
end;

function SkhUpperCase(IC: Char): Char; assembler;
asm
        MOV	AL,IC
        CMP	AL,'a'
        JB	@cyr
        CMP	AL,'z'
        JA	@cyr
        SUB	AL,'a'-'A'
        JMP     @exit
@cyr:
        CMP     AL, 32
        JB      @cyrb
        CMP     AL, Byte('Ї')
        JA      @cyrb
        SUB     AL, Byte(32 - Byte('Ђ'))
        JMP     @exit
@cyrb:
        CMP     AL, Byte('а')
        JB      @sakha
        CMP     AL, Byte('п')
        JA      @sakha
        SUB     AL,'а'-'ђ'
        JMP     @exit
@sakha:
        CMP     AL,0F3h
        JE      @DOsakha
        CMP     AL,0F5h
        JE      @DOsakha
        CMP     AL,0F7h
        JE      @DOsakha
        CMP     AL,0F9h
        JE      @DOsakha
        CMP     AL,0FBh
        JE      @DOsakha
        JMP     @exit
@DOsakha:
        DEC     AL
@exit:
end; { SkhUpperCase }

function SkhLowerCase(IC: Char): Char; assembler;
asm
	MOV	AL,IC
	CMP	AL,'A'
	JB	@cyr
	CMP	AL,'Z'
	JA	@cyr
	ADD	AL,'a'-'A' ;
        JMP     @exit
@cyr:
        CMP     AL, Byte('Ђ')
        JB      @cyrb
        CMP     AL,Byte('Џ')
        JA      @cyrb
        ADD     AL,Byte(32-Byte('Ђ'))
        JMP     @exit
@cyrb:
        CMP     AL, Byte('ђ')
        JB      @exit
        CMP     AL, Byte('џ')
        JA      @exit
        ADD     AL, Byte(Byte('а')- Byte('ђ'))
@exit:
end; { SkhLowerCase }

function   ControlChar(Ch: Char): String;
begin
  case Ch of
  #0: Result:= '#0';
  #1..#26 : Result:= '^'+Chr(Ord(Ch)+ Ord('A') - 1);
  { #0,#27..#31: Result:= '#'+IntToStr(Ord(Ch)); }
  else Result:= Ch;
  end;
end;

function ControlString(S: String): String;
var
  i: Integer;
  SS: String;
begin
  SS:= '';
  for i:= 1 to Length(S) do begin
    SS:= SS + ControlChar(S[i]);
  end;
  Result:= SS;
end;

{ преобразует строку вида '^MAssa' в #13'Assa'
  '\^' в '^'
}
function   ParseControlCode(S: String): String;
var
  SS: String;
  i, L: Integer;
begin
  SS:= '';
  L:= Length(S);
  { удалить последний CRLF }
  if L > 2 then begin
    if (S[L-1]=#13) and (S[L]=#10)
    then Dec(L, 2);
  end;
  i:= 1;
  { ^^ - это RS
    \^- это ^
  }
  while i <= L do begin
    if S[i] = '^' then begin
      if (i < L) then begin
        { след символ }
        Inc(i);
        SS:= SS+Chr(Ord(Upcase(S[i]))-Ord('A')+1);
      end else begin
        { это последний символ, что делать неясно- оставить }
        SS:= SS+'^';
      end;
    end else begin
      if (S[i] = '\') and (i<L) and (S[i+1]='^')
      then SS:= SS+ '^'
      else SS:= SS+ S[i];
    end;
    Inc(i);
  end;
  Result:= SS;
end;

function   DeleteChars(Ch: Char; const S: String): String;
var
  i: Integer;
  SS: String;
begin
  SS:= Uppercase(S);
  i:= Pos(Ch, SS);
  while i > 0 do begin
    System.Delete(SS, i, 1);
    i:= Pos(Ch, SS);
  end;
  DeleteChars:= SS;
end;

{ bits }

{ extract integer from specified bits (from lowest to highest) }
function ExtractBits(L: Integer; Bits: TBitSet): Integer;
var
  i: Integer;
  R: Integer;
  bitord: Integer;
begin
  R:= 0;
  bitord:= 0;
  for i:= 0 to 31 do begin
    if i in Bits then begin
      if (L and (1 shl i)) <> 0
      then R:= R + (1 shl bitord);
      Inc(bitord);
    end;
  end;
  ExtractBits:= R;
end;

{ set integer in specified bits (from lowest to highest) }
procedure SetBits(L: Integer; Bits: TBitSet; var R: Integer);
var
  i: Integer;
  bitord: Integer;
begin
  bitord:= 0;
  for i:= 0 to 31 do begin
    if i in Bits then begin
      if (L and (1 shl bitord)) <> 0
      then R:= R or (1 shl i)
      else R:= R and (not (1 shl i));
      Inc(bitord);
    end;
  end;
end;

function MaxFrom2(V1, V2: LongInt): LongInt;
begin
  if V1 > V2
  then Result:= V1
  else Result:= V2;
end;

function FirstRemarkLine(const RemStart: ShortString; SL: TStrings; from: Integer): Integer;
var
  i, last: Integer;
  found: Integer;
begin
  FirstRemarkLine:= -1;
  if SL = Nil
  then Exit;
  last:= SL.Count;
  if last <= from
  then Exit;
  Dec(last);
  found:= -1;
  for i:= from to last do begin
    if Pos(Uppercase(RemStart), Uppercase(SL.Strings[i])) > 0
    then begin
      found:= i;
      Break;
    end;
  end;
  if found >= from
  then FirstRemarkLine:= found;
end;

{ quicksort
}
function FindSortedStrings(Strings: TStrings; const S: String): Integer;
var
  L, H, I, C: Integer;
begin
  L:= 0;
  H:= Strings.Count - 1;
  while L <= H do begin
    I:= (L + H) shr 1;
    C:= AnsiCompareText(Strings[I], S);
    if C < 0 then L:= I + 1 else begin
      H := I - 1;
      if C = 0 then begin
        L:= I;
      end;
    end;
  end;
  Result:= L;
end;

{ в отличие от предыдущего, проверяет начало. Если начала не совпадают, возвращает -1
}
function FindExactStartSortedStrings(Strings: TStrings; const S: String): Integer;
var
  ind: Integer;
begin
  ind:= FindSortedStrings(Strings, S);
  if ANSICompareText(S, Copy(Strings[ind], 1, Length(S))) = 0
  then Result:= ind
  else Result:= -1;
end;

{ extract next value (with similar name) from TStrings
  Parameter: N= 0.., where N=0 - first occurance
             Name - name of value
             R- value, '' if -1 returned
  Return -1 if does not exists or 0.. index of N occurance

  Возможно, поиск первого вхождения с применением IndexOfName чуть быстрее,
  но прямой перебор надежнее (вдруг IndexOfName возвратит не первый элемент?)
  ind:= S.IndexOfName(Name);
}
function NextValue(N: Integer; Name: String; S: TStrings; var R: String): Integer;
var
  i, occ: Integer;
  upname: String;
begin
  R:= '';
  NextValue:= -1;
  upname:= ANSIUppercase(Name)+'=';
  occ:= -1;
  for i:= 0 to S.Count - 1 do begin
    if Pos(UpName, ANSIUppercase(S.Strings[i])) = 1
    then Inc(occ);
    if occ = N then begin
      { found N occurance }
      R:= Copy(S.Strings[i], Length(UpName)+1, Length(S.Strings[i]) - Length(UpName));
      NextValue:= i;
      Exit;
    end;
  end;
end;

{ -1 if no
}
function LastRemarkLine(const RemStart, RemFinish: ShortString; SL: TStrings; from: Integer): Integer;
var
  i, first, last: Integer;
  found: Integer;
begin
  Result:= -1;
  first:= FirstRemarkLine(RemStart,SL,from);
  if first = -1
  then Exit;
  last:= SL.Count;
  if last <= from
  then Exit;
  Dec(last);
  found:= -1;
  if first > from
  then from:= first;
  for i:= from to last do begin
    if Pos(Uppercase(RemFinish), Uppercase(SL.Strings[i])) > 0
    then begin
      found:= i;
      Break;
    end;
  end;
  if found >= from
  then LastRemarkLine:= found;
end;

procedure CyrDates;
begin
  LongMonthNames[1] := 'Январь';
  LongMonthNames[2] := 'Февраль';
  LongMonthNames[3] := 'Март';
  LongMonthNames[4] := 'Апрель';
  LongMonthNames[5] := 'Май';
  LongMonthNames[6] := 'Июнь';
  LongMonthNames[7] := 'Июль';
  LongMonthNames[8] := 'Август';
  LongMonthNames[9] := 'Сентябрь';
  LongMonthNames[10]:= 'Октябрь';
  LongMonthNames[11]:= 'Ноябрь';
  LongMonthNames[12]:= 'Декабрь';
  ShortMonthNames[1] := 'Янв';
  ShortMonthNames[2] := 'Фев';
  ShortMonthNames[3] := 'Мар';
  ShortMonthNames[4] := 'Апр';
  ShortMonthNames[5] := 'Май';
  ShortMonthNames[6] := 'Июн';
  ShortMonthNames[7] := 'Июл';
  ShortMonthNames[8] := 'Авг';
  ShortMonthNames[9] := 'Сен';
  ShortMonthNames[10]:= 'Окт';
  ShortMonthNames[11]:= 'Ноя';
  ShortMonthNames[12]:= 'Дек';
  LongDayNames[1]:= 'Воскресенье';
  LongDayNames[2]:= 'Понедельник';
  LongDayNames[3]:= 'Вторник';
  LongDayNames[4]:= 'Среда';
  LongDayNames[5]:= 'Четверг';
  LongDayNames[6]:= 'Пятница';
  LongDayNames[7]:= 'Суббота';
  ShortDayNames[1]:= 'Вс';
  ShortDayNames[2]:= 'Пн';
  ShortDayNames[3]:= 'Вт';
  ShortDayNames[4]:= 'Ср';
  ShortDayNames[5]:= 'Чт';
  ShortDayNames[6]:= 'Пт';
  ShortDayNames[7]:= 'Сб';
end; { CyrDates }

{ возвращает год из даты/времени }
function  GetYear(Tm: TDateTime): Integer;
var
  Year, Month, Day: Word;
begin
  DecodeDate(Tm, Year, Month, Day);
  GetYear:= Year;
end;

{ возвращает номер месяца из даты/времени }
function  GetMonth(Tm: TDateTime): Integer;
var
  Year, Month, Day: Word;
begin
  DecodeDate(Tm, Year, Month, Day);
  GetMonth:= Month;
end;

{ возвращает число месяца из даты/времени }
function  GetDay(Tm: TDateTime): Integer;
var
  Year, Month, Day: Word;
begin
  DecodeDate(Tm, Year, Month, Day);
  GetDay:= Day;
end;

{ возвращает час из даты/времени }
function  GetHour(Tm: TDateTime): Integer;
var
  h, m, s, ms: Word;
begin
  DecodeTime(Tm, h, m, s, ms);
  GetHour:= h;
end;

{ устанавливает новое значение часа }
procedure SetHour(var Tm: TDateTime; NewHour: Integer);
var
  h, m, s, ms: Word;
begin
  DecodeTime(Tm, h, m, s, ms);
  h:= NewHour;
  Tm:= EncodeTime(h, m, s, ms);
end;

{ возвращает час из даты/времени }
function  GetMin(Tm: TDateTime): Integer;
var
  h, m, s, ms: Word;
begin
  DecodeTime(Tm, h, m, s, ms);
  GetMin:= m;
end;

{ устанавливает новое значение часа }
procedure SetMin(var Tm: TDateTime; NewMin: Integer);
var
  h, m, s, ms: Word;
begin
  DecodeTime(Tm, h, m, s, ms);
  m:= NewMin;
  Tm:= EncodeTime(h, m, s, ms);
end;

procedure IncTime(var Tm: TDateTime; AddHour, AddMinutes: Integer);
var
  DT: TDateTime;
begin
  Dt:= EncodeTime(Abs(AddHour), Abs(AddMinutes), 0, 0);
  if (AddHour < 0) or (AddMinutes < 0)
  then Tm:= Tm + Dt
  else Tm:= Tm + Dt;
end;

procedure RoundTime(var Tm: TDateTime; AHour, AMinutes: Word);
var
  h, m, s, ms: Word;
  newm, newh: Word;
begin
  DecodeTime(Tm, h, m, s, ms);
  if AHour in [1..23] then begin
    newh:= 0;
    while newh < h do begin
      Inc(newh, AHour);
    end;
    h:= newh mod 24;
  end;
  if AMinutes in [1..59] then begin
    newm:= 0;
    while newm < m do begin
      Inc(newm, AMinutes);
    end;
    m:= newm mod 60;
  end;
  Tm:= EncodeTime(h, m, s, ms);
end;

{ These strings are NOT to be resourced }
{$IFNDEF VER80}
const
  Months: array[1..12] of string = (
    'Jan', 'Feb', 'Mar', 'Apr',
    'May', 'Jun', 'Jul', 'Aug',
    'Sep', 'Oct', 'Nov', 'Dec');

function ParseHTTPDate(const DateStr: string): TDateTime;
var
  Month, Day, Year, Hour, Minute, Sec: Integer;
  Parser: TParser;
  StringStream: TStringStream;

  function GetMonth: Boolean;
  begin
    Month := 1;
    while not Parser.TokenSymbolIs(Months[Month]) and (Month < 13) do Inc(Month);
    Result := Month < 13;
  end;

  procedure GetTime;
  begin
    with Parser do
    begin
      Hour := TokenInt;
      NextToken;
      if Token = ':' then NextToken;
      Minute := TokenInt;
      NextToken;
      if Token = ':' then NextToken;
      Sec := TokenInt;
      NextToken;
    end;
  end;

begin
  StringStream := TStringStream.Create(DateStr);
  try
    Parser := TParser.Create(StringStream);
    with Parser do
    try
      NextToken;
      if Token = ':' then NextToken;
      NextToken;
      if Token = ',' then NextToken;
      if GetMonth then
      begin
        NextToken;
        Day := TokenInt;
        NextToken;
        GetTime;
        Year := TokenInt;
      end else
      begin
        Day := TokenInt;
        NextToken;
        if Token = '-' then NextToken;
        GetMonth;
        NextToken;
        if Token = '-' then NextToken;
        Year := TokenInt;
        if Year < 100 then Inc(Year, 1900);
        NextToken;
        GetTime;
      end;
      Result := EncodeDate(Year, Month, Day) + EncodeTime(Hour, Minute, Sec, 0);
    finally
      Free;
    end;
  finally
    StringStream.Free;
  end;
end;
{---------------------------- Indy9-------------------------------------------}
const
  wdays: array[1..7] of string = ('Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri'    {Do not Localize}
   , 'Sat'); {do not localize}
  monthnames: array[1..12] of string = ('Jan', 'Feb', 'Mar', 'Apr', 'May'    {Do not Localize}
   , 'Jun',  'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'); {do not localize}

{This should never be localized}
function DateTimeGMTToHttpStr(const GMTValue: TDateTime) : String;
var
  wDay,
  wMonth,
  wYear: Word;
begin
  DecodeDate(GMTValue, wYear, wMonth, wDay);
  Result := Format('%s, %.2d %s %.4d %s %s',    {do not localize}
                   [wdays[DayOfWeek(GMTValue)], wDay, monthnames[wMonth],
                    wYear, FormatDateTime('HH":"NN":"SS', GMTValue), 'GMT']);  {do not localize}
end;

function DateTimeToGmtOffSetStr(ADateTime: TDateTime; SubGMT: Boolean): string;
var
  AHour, AMin, ASec, AMSec: Word;
begin
  if (ADateTime = 0.0) and SubGMT then
  begin
    Result := 'GMT'; {do not localize}
    Exit;
  end;
  DecodeTime(ADateTime, AHour, AMin, ASec, AMSec);
  Result := Format(' %0.2d%0.2d', [AHour, AMin]); {do not localize}
  if ADateTime < 0.0 then
  begin
    Result[1] := '-'; {do not localize}
  end
  else
  begin
    Result[1] := '+';  {do not localize}
  end;
end;

function OffsetFromUTC: TDateTime;
begin
  //TODO: Fix OffsetFromUTC for Linux to be automatic from OS
  Result := GOffsetFromUTC;
end;

{This should never be localized}
function DateTimeToInternetStr(const Value: TDateTime; const AIsGMT : Boolean = False) : String;
var
  wDay,
  wMonth,
  wYear: Word;
begin
  DecodeDate(Value, wYear, wMonth, wDay);
  Result := Format('%s, %d %s %d %s %s',    {do not localize}
                   [wdays[DayOfWeek(Value)], wDay, monthnames[wMonth],
                    wYear, FormatDateTime('HH":"NN":"SS', Value),  {do not localize}
                    DateTimeToGmtOffSetStr(OffsetFromUTC, AIsGMT)]);
end;

function FetchCaseInsensitive(var AInput: string; const ADelim: string = ' ';
 const ADelete: Boolean = True): String;
var
  LPos: integer;
begin
  if ADelim = #0 then begin
    // AnsiPos does not work with #0
    LPos := Pos(ADelim, AInput);
  end else begin
    //? may be AnsiUpperCase?
    LPos := Pos(UpperCase(ADelim), UpperCase(AInput));
  end;
  if LPos = 0 then begin
    Result := AInput;
    if ADelete then begin
      AInput := '';    {Do not Localize}
    end;
  end else begin
    Result := Copy(AInput, 1, LPos - 1);
    if ADelete then begin
      //This is faster than Delete(AInput, 1, LPos + Length(ADelim) - 1);
      AInput := Copy(AInput, LPos + Length(ADelim), MaxInt);
    end;
  end;
end;

function Fetch(var AInput: string; const ADelim: string = ' ';
 const ADelete: Boolean = True;
 const ACaseSensitive: Boolean = True): String;
var
  LPos: integer;
begin
  if ACaseSensitive then begin
    if ADelim = #0 then begin
      // AnsiPos does not work with #0
      LPos := Pos(ADelim, AInput);
    end else begin
      LPos := Pos(ADelim, AInput);
    end;
    if LPos = 0 then begin
      Result := AInput;
      if ADelete then begin
        AInput := '';    {Do not Localize}
      end;
    end
    else begin
      Result := Copy(AInput, 1, LPos - 1);
      if ADelete then begin
        //slower Delete(AInput, 1, LPos + Length(ADelim) - 1);
        AInput:=Copy(AInput, LPos + Length(ADelim), MaxInt);
      end;
    end;
  end else begin
    Result := FetchCaseInsensitive(AInput, ADelim, ADelete);
  end;
end;

function PosInStrArray(const SearchStr: string; Contents: array of string; const CaseSensitive: Boolean=True): Integer;
begin
  for Result := Low(Contents) to High(Contents) do begin
    if CaseSensitive then begin
      if SearchStr = Contents[Result] then begin
        Exit;
      end;
    end else begin
      if ANSISameText(SearchStr, Contents[Result]) then begin
        Exit;
      end;
    end;
  end;  //for Result := Low(Contents) to High(Contents) do
  Result := -1;
end;

function StrToMonth(const AMonth: string): Byte;
begin
  Result := Succ(PosInStrArray(Uppercase(AMonth),
    ['JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG','SEP','OCT','NOV','DEC']));   {do not localize}
end;

function StrToDay(const ADay: string): Byte;
begin
  Result := Succ(PosInStrArray(Uppercase(ADay),
    ['SUN','MON','TUE','WED','THU','FRI','SAT']));   {do not localize}
end;

function RawStrInternetToDateTime(var Value: string): TDateTime;
var
  i: Integer;
  Dt, Mo, Yr, Ho, Min, Sec: Word;
  sTime: String;
  ADelim: string;

  Procedure ParseDayOfMonth;
  begin
    Dt :=  StrToIntDef( Fetch(Value, ADelim), 1);
    Value := TrimLeft(Value);
  end;

  Procedure ParseMonth;
  begin
    Mo := StrToMonth( Fetch ( Value, ADelim )  );
    Value := TrimLeft(Value);
  end;
begin
  Result := 0.0;
  Value := Trim(Value);
  if Length(Value) = 0 then begin
    Exit;
  end;

  try
    {Day of Week}
    if StrToDay(Copy(Value, 1, 3)) > 0 then begin
      //workaround in case a space is missing after the initial column
      if (Copy(Value,4,1)=',') and (Copy(Value,5,1)<>' ') then
      begin
        System.Insert(' ',Value,5);
      end;
      Fetch(Value);
      Value := TrimLeft(Value);
    end;

    // Workaround for some buggy web servers which use '-' to separate the date parts.    {Do not Localize}
    if (Pos('-', Value) > 1) and (Pos('-', Value) < Pos(' ', Value)) then begin    {Do not Localize}
      ADelim := '-';    {Do not Localize}
    end
    else begin
      ADelim := ' ';    {Do not Localize}
    end;
    //workaround for improper dates such as 'Fri, Sep 7 2001'    {Do not Localize}
    //RFC 2822 states that they should be like 'Fri, 7 Sep 2001'    {Do not Localize}
    if (StrToMonth(Fetch(Value, ADelim,False)) > 0) then
    begin
      {Month}
      ParseMonth;
      {Day of Month}
      ParseDayOfMonth;
    end
    else
    begin
      {Day of Month}
      ParseDayOfMonth;
      {Month}
      ParseMonth;
    end;
    {Year}
    // There is sometrage date/time formats like
    // DayOfWeek Month DayOfMonth Time Year

    sTime := Fetch(Value);
    Yr := StrToIntDef(sTime, 1900);
    // Is sTime valid Integer
    if Yr = 1900 then begin
      Yr := StrToIntDef(Value, 1900);
      Value := sTime;
    end;
    if Yr < 80 then begin
      Inc(Yr, 2000);
    end else if Yr < 100 then begin
      Inc(Yr, 1900);
    end;

    Result := EncodeDate(Yr, Mo, Dt);
    // SG 26/9/00: Changed so that ANY time format is accepted
    i := Pos(':', Value); {do not localize}
    if i > 0 then begin
      // Copy time string up until next space (before GMT offset)
      sTime := fetch(Value, ' ');  {do not localize}
      {Hour}
      Ho  := StrToIntDef( Fetch ( sTime,':'), 0);  {do not localize}
      {Minute}
      Min := StrToIntDef( Fetch ( sTime,':'), 0);  {do not localize}
      {Second}
      Sec := StrToIntDef( Fetch ( sTime ), 0);
      {The date and time stamp returned}
      Result := Result + EncodeTime(Ho, Min, Sec, 0);
    end;
    Value := TrimLeft(Value);
  except
    Result := 0.0;
  end;
end;

function StrInternetToDateTime(Value: string): TDateTime;
begin
  Result := RawStrInternetToDateTime(Value);
end;
{-------------------------------- Indy9 ---------------------------------------}
{$ENDIF}


{ return 1..3 - qty of decimal digits in TOKENSTRING terminated by ';' character
  or 0 if no digits terminated by ';' found
}
function DigitsTerminatedBySemicolonQTY(const TokenString: String; Position: Integer): Integer;
begin
  Result:= 0;
  if TokenString[Position] <> '&'
  then Exit;
  Inc(Position);
  if TokenString[Position] <> '#'
  then Exit;
  Inc(Position);
  if (TokenString[Position] in ['0'..'9']) then begin
    if (TokenString[Position+1] = ';')
    then begin Result:= 1; Exit; end;
    if (TokenString[Position+1] in ['0'..'9']) then begin
      if (TokenString[Position+2] = ';')
      then begin Result:= 2; Exit; end;
      if (TokenString[Position+2] in ['0'..'9']) then begin
        if (TokenString[Position+3] = ';')
        then begin Result:= 3; Exit; end;
      end;
    end;
  end;
end;

const
  ASCIIMNEMONIC: array[0..31] of String[3] =
  ('NUL', 'SOH', 'STX', 'ETX', 'EOT', 'ENQ', 'ACK', 'BEL', 'BS', 'HT',
  'LF', 'VT', 'FF', 'CR', 'SO', 'SI', 'DEL', 'DC1','DC2', 'DC3', 'DC4',
  'NAK', 'SYN', 'TB', 'CAN', 'EM', 'SUB', 'ESC', 'FS', 'GS', 'RS', 'US'
   );
{ return 0..255 character
  or -1 if no mnemonic found
}
function ASCIIMnenonicTerminatedBySemicolonQTY(const TokenString: String; Position: Integer): Integer;
var
  S: String;
  p: Integer;
begin
  Result:= -1;
  if TokenString[Position] <> '&'
  then Exit;
  Inc(Position);
  S:= Copy(TokenString, Position, 3);
  p:= Pos(';', S);
  if p > 0
  then Delete(S, p, 3);
  S:= ANSIUpperCase(S);
  for p:= 0 to 31 do begin
    if ANSICompareText(S, ASCIIMNEMONIC[p]) = 0 then begin
      Result:= p;
      Exit;
    end;
  end;
  if ANSICompareText(S, 'GT') = 0 then begin
    Result:= Ord('>');
    Exit;
  end;
  if ANSICompareText(S, 'LT') = 0 then begin
    Result:= Ord('<');
    Exit;
  end;
  if ANSICompareText(S, 'AMP') = 0 then begin
    Result:= Ord('&');
    Exit;
  end;
  if ANSICompareText(S, 'QUOT') = 0 then begin
    Result:= Ord('"');
    Exit;
  end;
end;


{ return character from POSITION in TOKENSTRING
  Character- char type
             ASCII extended ASCII character code in WEB- like notation (&#xxx;)
  automatically search an &#xxx; expression and return appropriate character
  return POSITION of next character
}
function GetWEBStyleCharFromString(TokenString: String; var Position: Integer): Char;
var
  L, code: Integer;
  S: String;
begin
  if (TokenString[Position] = '&') and
    ((DigitsTerminatedBySemicolonQTY(TokenString, Position) > 0) or
    (ASCIIMnenonicTerminatedBySemicolonQTY(TokenString, Position) > -1)) then begin
    L:= PosFrom(Position + 1, ';', TokenString) - Position;
    S:= Copy(TokenString, Position+ 2, L-2);
    Code:= StrToIntDef(S, -1);
    if code = -1 then begin
      S:= Copy(TokenString, Position, L);
      code:= ASCIIMnenonicTerminatedBySemicolonQTY(S, 1);
      if code = -1
      then code:= 0;
    end;
    Result:= Chr(Code);
    Inc(Position, L+1); { &#; - 3 characters }
  end else begin
    Result:= TokenString[Position];
    Inc(Position);
  end;
end;

{ Translate ASCII extended ASCII character code in WEB- like notation (&#xxx;)
  in TOKENSTRING
}
function WEBStyleString2ASCII(TokenString: String): String;
var
  p, L: Integer;
  SS: String;
begin
  Result:= '';
  L:= Length(TokenString);
  if L = 0 then Exit;
  SS:= '';
  p:= 1;
  repeat
    SS:= SS + GetWEBStyleCharFromString(TokenString, p);
  until p > L;
  Result:= SS;
end;

{ Translate ASCII extended ASCII character code in WEB- like notation (&#xxx;)
  in TOKENSTRING and Control char ^C
}
function WEBStyleAndControlString2ASCII(TokenString: String): String;
var
  p, L: Integer;
  SS: String;
begin
  Result:= '';
  { сначала транслировать выражения ^M^J в #13#10
    если строка пустая ничего
  }
  TokenString:= ParseControlCode(TokenString);
  { теперь только определить длину }
  L:= Length(TokenString);
  if L = 0
  then Exit;
  { наконец транслировать выражения &#xxx; }
  SS:= '';
  p:= 1;
  repeat
    SS:= SS + GetWEBStyleCharFromString(TokenString, p);
  until p > L;
  Result:= SS;
end;

function ASCII2HTML(TokenString: String): String;
var
  i: Integer;
  ch: String[6];
begin
  Result:= '';
  for i:= 1 to Length(TokenString) do begin
    case TokenString[i] of
    '>': ch:= '&gt;';
    '<': ch:= '&lt;';
    '&': ch:= '&amp;';
    '"': ch:= '&quot;';
    else ch:= TokenString[i];
    end;
    Result:= Result + ch;
  end;
end;

{ 1-> 1, 9->9, 10-> A, (0..'Z')
  Digits- отсекает первые оставляет последние (если 0- возвращается пустая строка )
}
function IntToAplhaNumeric(Value: Byte; Digits: Integer): String;
var
  OneDigit, L: Integer;
  Ch: Char;
  S: String;
begin
  S:= '';
  L:= 0;
  { 26 букв и 10 цифр - 36 цифр от 0 до 36 }
  while (L < Digits) and (Value > 0) do begin
    OneDigit:= Value mod 36;
    if OneDigit > 9
    then Ch:= Chr(OneDigit+ Ord('A') - 10)
    else Ch:= Chr(OneDigit + Ord('0'));
    System.Insert(Ch, S, 1);
    Value:= Value div 36;
    Inc(L);
  end;
  if (S = '') and (Digits > 0)
  then S:= '0';
  Result:= S;
end;

{ 0->A, 1-> B, ('A'..'Z')
  Digits- отсекает первые оставляет последние (если 0- возвращается пустая строка )
}
function IntToAplha(Value: Byte; Digits: Integer): String;
var
  OneDigit, L: Integer;
  S: String;
begin
  S:= '';
  L:= 0;
  { 26 букв  }
  while (L < Digits) and (Value > 0) do begin
    OneDigit:= Value mod 26;
    System.Insert(Chr(OneDigit+ Ord('A')), S, 1);
    Value:= Value div 26;
    Inc(L);
  end;
  if (S = '') and (Digits > 0)
  then S:= 'A';
  Result:= S;
end;

{ -1 if failed }
function AplhaNumericToInt(Value: String): Integer;
var
  OneDigit, i, power: Integer;
  R: Integer;
begin
  Result:= -1;
  oneDigit:= 0; { just to avoid compiler warning }
  R:= 0;
  power:= 1;
  { 36 букв и цифр }
  for i:= Length(Value) downto 1 do begin
    { проверка }
    case Value[i] of
    '0'..'9': OneDigit:= (Ord(Value[i]) - Ord('0')) * power;
    'A'..'Z': OneDigit:= (Ord(Value[i]) - Ord('A') + 10) * power;
    else Exit;
    end;
    Inc(R, OneDigit);
    power:= power * 36;
  end;
  Result:= R;
end;

{ -1 if failed }
function AplhaToInt(Value: String): Integer;
var
  OneDigit, i, power: Integer;
  R: Integer;
begin
  Result:= -1;
  R:= 0;
  power:= 1;
  { 26 букв  }
  for i:= Length(Value) downto 1 do begin
    { проверка }
    if (Value[i] > 'Z') or (Value[i] < 'A')
    then Exit;
    OneDigit:= (Ord(Value[i]) - Ord('A')) * power;
    Inc(R, OneDigit);
    power:= power * 26;
  end;
  Result:= R;
end;

{ пн - 0 вт - 1 вс -6 }
function CyrDayOfWeek(Dt: TDateTime): Integer;
var
  d: Integer;
begin
  d:= DayOfWeek(Dt);
  if d = 1
  then Result:= 6
  else Result:= d-2;
end;

{ registry functions }
{$IFNDEF VER80}
function GetRegString(ARoot: Cardinal; const APath, APar, DefVal: String): String;
var
  R: TRegistry;
begin
  Result:= DefVal;
  R:= TRegistry.Create;
  R.RootKey:= ARoot;
{$IFDEF D4_}
  if R.OpenKeyReadOnly(APath) then begin
{$ELSE}
  if R.OpenKey(APath, False) then begin
{$ENDIF}
    try
      Result:= R.ReadString(APar);
    except
    end;
  end;
  R.CloseKey;
  R.Free;
end;
{$ENDIF}

{$IFNDEF VER80}
{ SOFTWARE\Microsoft\Windows\CurrentVersion }
procedure SetRegString(ARoot: Cardinal; const APath, APar, AValue: String);
var
  R: TRegistry;
begin
  R:= TRegistry.Create;
  R.RootKey:= ARoot;
  R.CreateKey(APath);
{$IFDEF D4_}
  if R.OpenKeyReadOnly(APath) then begin
{$ELSE}
  if R.OpenKey(APath, False) then begin
{$ENDIF}
    try
      R.WriteString(APar, AValue);
    except
    end;
  end;
  R.CloseKey;
  R.Free;
end;
{$ENDIF}

{$IFNDEF VER80}
function ReadLocalTCPName: String;
var
  R: TRegistry;
begin
  Result:= '';
  R:= TRegistry.Create;
  try
    R.RootKey:= HKEY_LOCAL_MACHINE;
{$IFDEF D4_}
    R.OpenKeyReadOnly('\SYSTEM\CurrentControlSet\Services\Tcpip\Parameters');
{$ELSE}
    R.OpenKey('\SYSTEM\CurrentControlSet\Services\Tcpip\Parameters', False);
{$ENDIF}
    Result:= R.ReadString('Hostname');
  except
  end;
  R.Free;
end;
{$ENDIF}

function IsLocalArea(AHostName: String): Boolean;
begin
  Result:= Pos('.', AHostname) >= 1;
end;

{$IFNDEF VER80}
function IsIPPresent: Boolean;
const
  KEYREMOTEACCCESS = 'System\CurrentControlSet\Services\RemoteAccess';
  PREMOTEACCCESS = 'Remote Connection';
var
  Key: hKey;
  PC: array[0..4] of Char;
  Size: Integer;

function IsIPPresentBySocket: Boolean;
type
  TaPInAddr = array[0..10] of PInAddr;
  PaPInAddr = ^TaPInAddr;
var
  phe: PHostEnt;
  pptr: PaPInAddr;
  Buffer: Array[0..63] of Char;
  I: Integer;
  GInitData: TWSAData;
  IP: String;
  FCurrentIP : String;       {<--RLM Diagnostics}
begin
  WSAStartup($101, GInitData);
  Result:= False;
  GetHostName(Buffer, SizeOf(Buffer));
  phe:= GetHostByName(buffer);
  if phe = nil
  then Exit;
  pPtr:= PaPInAddr(phe^.h_addr_list);
  I:= 0;
  while pPtr^[I] <> nil do begin
    IP:= inet_ntoa(pptr^[I]^);
    Inc(I);
  end;
  FCurrentIP:= IP;
  WSACleanup;
  Result:= (IP <> '') and (IP <> '127.0.0.1');
end;

begin
  Result:= IsIPPresentBySocket;
  if Result
  then Exit;
  try
    if RegOpenKey(HKEY_LOCAL_MACHINE, KEYREMOTEACCCESS, Key) = ERROR_SUCCESS then begin
      Size:= 4;
      if RegQueryValueEx(Key, PREMOTEACCCESS, nil, nil, @PC, @Size) = ERROR_SUCCESS then begin
        Result:= PC[0] = #1;
      end;
      RegCloseKey(Key);
    end;
  except
  end
end;

function ReadIEProxySettings(AProtocol: String; ARemoteHost: String; var AProxy: String; var APort: Integer): Boolean;
const
  RGProxyEnable = 'ProxyEnable';
var
  pe: Boolean;
  R: TRegistry;
  i: Integer;
  S, SS: String;
  localname: String;
  b: DWORD;
begin
  Result:= False;
  AProxy:= '';
  APort:= 0;
  localname:= ReadLocalTCPName;
  R:= TRegistry.Create;
  try
    R.RootKey:= HKEY_CURRENT_USER;
{$IFDEF D4_}
    R.OpenKeyReadOnly('\Software\Microsoft\Windows\CurrentVersion\Internet Settings');
{$ELSE}
    R.OpenKey('\Software\Microsoft\Windows\CurrentVersion\Internet Settings', False);
{$ENDIF}
    if R.GetDataType(RGProxyEnable) = rdInteger
    then pe:= R.ReadBool('ProxyEnable')
    else begin
      R.ReadBinaryData('ProxyEnable', b, 4);
      pe:= b > 0;
    end;
    if pe then begin
      // check is address in local zone
      S:= R.ReadString('ProxyOverride'); //looks like 10.8.1.14;<local>
      i:= 1;
      repeat
        SS:= util1.GetToken(i, ';', S);
        if Pos('<local>', SS) = 1 then begin
          if IsLocalArea(ARemoteHost) then begin
            Exit;
          end;
        end;
        if Pos(ARemoteHost, SS) = 1 then begin
          Exit;
        end;
        Inc(i);
      until SS = '';
      // read proxy settings
      S:= R.ReadString('ProxyServer'); //looks like ftp=10.8.1.1:8021;http=10.8.1.1:8080
      i:= 1;
      repeat
        SS:= util1.GetToken(i, ';', S);
        if Pos(AProtocol+'=', SS) = 1 then begin
          Delete(SS, 1, 5);
          AProxy:= GetToken(1, ':', SS);
          APort:= StrToIntDef(GetToken(2, ':', SS), 8080);
          Result:= True;
          Break;
        end;
        Inc(i);
      until SS = '';
    end else begin
    end;
  except
  end;
  R.Free;
end;

{ читает полный раздел из реестра (HKEY_LOCAL_MACHINE). Все параметры должны быть строками
}
function AddEntireKey(AKey: String; R: TStrings): Boolean;
var
  i: Integer;
  SL: TStringList;
  Rg: TRegistry;
begin
  Result:= True;
  SL:= TStringList.Create;
  Rg:= TRegistry.Create;
  Rg.RootKey:= HKEY_LOCAL_MACHINE;
{$IFDEF D4_}
  Rg.OpenKeyReadOnly(AKey);
{$ELSE}
  Rg.OpenKey(AKey, False);
{$ENDIF}
  try
    Rg.GetValueNames(SL);
    for i:= 1 to SL.Count do begin
      try
        R.Add(SL[i-1] + '=' + Rg.ReadString(SL[i-1]));
      except
      end;
    end;
  except
    Result:= False;
  end;
  Rg.Free;
  SL.Free;
end;

{ get mime description
  example ".jpg" (or "jpg") -> "image/jpeg"
  if file extension not found, return empty string. (or text/plain?!!)
}
function  MimeByExt(ext: String): String;
var
  R: TRegistry;
begin
  Result:= '';
  Result:= ''; // 'text/x-oeb1-document';
  if Length(ext) = 0 then Exit;
  ext:= LowerCase(ExtractFileExt(ext));
  if Length(ext) = 0 then Exit;

  if ext[1] <> '.'
  then ext:= '.' + ext;

  if CompareText(ext, '.css') = 0
  then Result:= 'text/css' else
    if (CompareText(ext, '.jpeg') = 0) or (CompareText(ext, '.jpg') = 0)
    then Result:= 'image/jpeg' else
      if CompareText(ext, '.gif') = 0
      then Result:= 'image/gif' else
        if CompareText(ext, '.png') = 0
        then Result:= 'image/png' else
          if CompareText(ext, '.htm') = 0
          then Result:= 'text/html' else
            if CompareText(ext, '.txt') = 0
            then Result:= 'text/plain' else
              if CompareText(ext, '.ico') = 0
              then Result:= 'image/x-icon' else
                if CompareText(ext, '.pdf') = 0
                then Result:= 'application/pdf' else
                  if CompareText(ext, '.fdf') = 0
                  then Result:= 'application/vnd.fdf' else
                    if CompareText(ext, '.xfdf') = 0
                    then Result:= 'application/vnd.adobe.xfdf' else begin
                      R:= TRegistry.Create;
                      try
                        R.RootKey:= HKEY_CLASSES_ROOT;
                      {$IFDEF D4_}
                        if R.OpenKeyReadOnly(ext) then begin
                      {$ELSE}
                        if R.OpenKey(ext, False) then begin
                      {$ENDIF}
                          try
                            Result:= R.ReadString('Content Type');
                          except
                          end;
                        end;
                        R.CloseKey;
                      finally
                        R.Free;
                      end;
                    end;
end;
{$ENDIF}

function Scan32(const Block: ANSIString; Len, From: Integer; const Str: String): Integer;
var
  i, j, l, blocklen: Integer;
begin
  Result:= -1;
  L:= Length(str);
  if l <= 0
  then Exit;
  if (len <= 0)
  then blocklen:= Length(Block)
  else blocklen:= Len;
  for i:= from to blocklen - L + 1 do begin
    if Str[1] = Block[i] then begin
      for j:= 2 to L - 1 do begin
        if Str[j] <> Block[i+j-1]
        then Break;
      end;
      if Str[L] = Block[i+L-1] then begin
        Result:= i;
        Exit;
      end;
    end;
  end;
end;

type
  tsetofchar = set of char;

procedure String2SetOfChar(AString: String; var AsetOfChar);
var
  ch: Char;
begin
  tsetofchar(ASetOfChar):= [];
  for ch:= #0 to #255 do begin
    if Pos(ch, AString) > 0
    then Include(tsetofchar(ASetOfChar), ch);
  end;
end;

function ReplaceChars(setOfChar: TSysCharSet;  newchar: Char; var s: String): Integer;
var
  i: Word;
  j: Byte;
begin
  Result:= 0;
  for j:= 0 to 255 do begin
    if Char(j) in setofchar then begin
      for i:= 1 to Length(s) do begin
        if s[i] = Char(j) then begin
          s[i]:= newchar;
          Inc(Result);
        end;
      end;
    end;
  end;
end; {  ReplaceChar }

function  LoadPChar(AFN: String; var dest: Pointer): Integer;
var
  L: LongInt;
  F: TFileStream;
begin
  LoadPChar:= 0;
  if not FileExists(AFN)
  then Exit;
  F:= TFileStream.Create(AFN, fmOpenRead + fmShareDenyNone);
  L:= F.Size;
  try
    GetMem(dest, L+1); {#0 terminate pchar}
    F.Read(dest^, L);
  except
    FreeMem(dest, L+1);
    Exit;
  end;
  CA(dest^)[L]:= #0;
  LoadPChar:= L+1;
end;

{$IFNDEF VER80}
function  LoadString(AFN: String): String;
var
  L: LongInt;
  F: TFileStream;
begin
  LoadString:= '';
  if not FileExists(AFN)
  then Exit;
  try
    F:= TFileStream.Create(AFN, fmOpenRead + fmShareDenyNone);
  except
    Exit;
  end;
  try
    L:= F.Size;
    SetString(Result, Nil, L);
    F.Read(Pointer(Result)^, L);
  finally
    F.Free;
  end;
end;

function  StoreString(AFN, S: String): Boolean;
var
  F: TFileStream;
  L: Integer;
begin
  StoreString:= False;
  L:= Length(S);
  try
    F:= TFileStream.Create(AFN, fmCreate);
  except
    Exit;
  end;
  try
    F.Write(S[1], L);
    F.Size:= L;
  finally
    F.Free;
  end;
end;

procedure OverrideAppLocale(LocaleOverride: String);
var
  Reg: TRegistry;
  EXENAME: String;
  fn: array[0..511] of Char;
begin
  SetString(EXENAME, FN, GetModuleFileName(hInstance, FN, SizeOf(FN)));
  Reg:= TRegistry.Create;
  Reg.RootKey:= HKEY_CURRENT_USER;
  try
    if Reg.OpenKey('Software\Borland\Locales', True) then
      Reg.WriteString(EXENAME, LocaleOverride);
  finally
    Reg.Free;
  end;
end;

{$IFDEF D4_}
procedure GetLocaleNames(SL: TStrings; AOrderByName:Boolean);
var
  SearchRec: sysutils.TSearchRec;
  AMask: String;
  fn: array[0..511] of Char;

  function AddLocale: Boolean;
  var
    i: Integer;
    ext: String;
    langName: String;
    langID: Windows.LCID;
  begin
    langName:= '';
    for i:= 0 to Languages.Count - 1 do begin
      ext:= UpperCase(Copy(ExtractFileExt(SearchRec.Name), 2, MaxInt));
      if ext = UpperCase(Languages.Ext[i]) then begin
        langName:= Languages.Name[i];
        langID:= Languages.LocaleID[i];
        if AOrderByName
        then SL.AddObject(langName+'='+Ext, TObject(langID))
        else SL.AddObject(Ext+'='+langName, TObject(langID));
        Break;
      end;
    end;
    Result:= langName > '';
  end;

begin
  SetString(AMask, FN, GetModuleFileName(hInstance, FN, SizeOf(FN)));
  AMask:= util1.ReplaceExt('*', AMask);
  if FindFirst(AMask, faAnyFile, SearchRec)=0 then begin
    AddLocale;
    while FindNext(SearchRec) = 0
    do AddLocale;
  end;
  Windows.FindClose(SearchRec.FindHandle);
  // Result:= SL.Count;
end;

const
  MAX_PATH = 511;

function GetDllName: String;
var
  FN: array[0..MAX_PATH - 1] of Char;
begin
  SetString(Result, FN, GetModuleFileName(hInstance, FN, SizeOf(FN)));
end;

function GetWindowsDirectory: String;
var
  FN: array[0..MAX_PATH - 1] of Char;
begin
  SetString(Result, FN, GetSystemDirectory(FN, SizeOf(FN)));
end;

{$ENDIF}
{$ENDIF}

procedure SetStringsDelimitedTextWithSpace(AStrings: TStrings; AQuoteChar: Char; ADelimiter: Char; const Value: string);
var
  P, P1: PChar;
  q0, q1: Integer;
  n, v: string;
  S: string;
begin
  with AStrings do begin
    BeginUpdate;
    try
      Clear;
      P:= PChar(Value);
      while P^ in [#1..#32] do
        P:= CharNext(P);
      while P^ <> #0 do
      begin
        if P^ = QuoteChar then
          S:= AnsiExtractQuotedStr(P, AQuoteChar)
        else
        begin
          P1:= P;
          while (P^ >= #32) and (P^ <> ADelimiter) do
            P:= CharNext(P);
          SetString(S, P1, P - P1);
        end;
        q0:= Pos('=', S);
        if q0 > 0 then begin
          n:= Copy(S, 1, q0);
          v:= Trim(Copy(S, q0 + 1, MaxInt));
          q1:= Length(v);
          if q1 >= 2 then begin
            if v[q1] in ['''', '"'] then System.Delete(v, q1, 1);
            if v[1] in ['''', '"'] then System.Delete(v, 1, 1);
          end;
        end;
        Add(n + v);
        while P^ in [#1..#32] do
          P:= CharNext(P);
        if P^ = ADelimiter then
        begin
          P1:= P;
          if CharNext(P1)^ = #0 then
            Add('');
          repeat
            P:= CharNext(P);
          until not (P^ in [#1..#32]);
        end;
      end;
    finally
      EndUpdate;
    end;
  end;
end;

function PascalString(const S: String): String;
var
  SS, SSS: String;
  i, L: Integer;
  DIGITS: set of Char;
  isHex: Boolean;
begin
  SS:= '';
  L:= Length(S);
  i:= 1;
  { ^^ - это RS
    \^- это ^
  }
  while i <= L do begin
    case S[i] of
    '^':begin
          if (i < L) then begin
            Inc(i);  // next symbol
            SS:= SS + Chr(Ord(Upcase(S[i]))-Ord('A')+1);
          end else begin
            SS:= SS + '^'; // это последний символ, что делать неясно, поэтому- оставить
          end;
        end;
    '\':begin
          if (i < L) and (S[i + 1] = '^')
          then SS:= SS + '^'
          else SS:= SS + S[i];
        end;
    '#':begin
          if (i < L) and (S[i + 1] in ['0'..'9', '$']) then begin
            Inc(i);
            if S[i] = '$' then begin
              isHex:= True;
              Digits:= ['0'..'9', 'A'..'F', 'a'..'f'];
              Inc(i);
            end else begin
              isHex:= False;
              Digits:= ['0'..'9'];
            end;
            SSS:= '';
            repeat
              SSS:= SSS + s[i];
              Inc(i);
            until (i > L) or (not (S[i] in DIGITS));
            if isHex
            then SSS:= '$' + SSS;
            SS:= SS + Char(StrToIntDef(SSS, 32));
          end else SS:= SS + S[i];
        end;
    else SS:= SS + S[i];
    end;
    Inc(i);
  end;
  Result:= SS;
end;

function PascalWideString(const S: String): WideString;
var
  SS, SSS: WideString;
  i, L: Integer;
  DIGITS: set of Char;
  isHex, nondigit: Boolean;
begin
  SS:= '';
  L:= Length(S);
  i:= 1;
  { ^^ - это RS
    \^- это ^
  }
  while i <= L do begin
    case S[i] of
    '^':begin
          if (i < L) then begin
            Inc(i);  // next symbol
            SS:= SS + Chr(Ord(Upcase(S[i]))-Ord('A')+1);
          end else begin
            SS:= SS + '^'; // это последний символ, что делать неясно, поэтому- оставить
          end;
        end;
    '\':begin
          if (i < L) and (S[i + 1] = '^')
          then SS:= SS + '^'
          else SS:= SS + S[i];
        end;
    '#':begin
          if (i < L) and (S[i + 1] in ['0'..'9', '$']) then begin
            Inc(i);
            if S[i] = '$' then begin
              isHex:= True;
              Digits:= ['0'..'9', 'A'..'F', 'a'..'f'];
              Inc(i);
            end else begin
              isHex:= False;
              Digits:= ['0'..'9'];
            end;
            SSS:= '';
            repeat
              SSS:= SSS + s[i];
              Inc(i);
              nondigit:= not (S[i] in DIGITS);
            until (i > L) or nondigit;
            if nondigit then Dec(i);
            if isHex
            then SSS:= '$' + SSS;
            SS:= SS + WideChar(StrToIntDef(SSS, 32));
          end else SS:= SS + S[i];
        end;
    else SS:= SS + S[i];
    end;
    Inc(i);
  end;
  Result:= SS;
end;

function PascalString2WideString(const S: String; var AIsString: Boolean): WideString;
var
  SS, SSS: WideString;
  i, L: Integer;
  DIGITS: set of Char;
  isHex, nondigit: Boolean;
  state: Integer;
begin
  AIsString:= False;
  state:= 0;

  SS:= '';
  L:= Length(S);
  i:= 1;
  { ^^ - это RS
    \^- это ^
  }
  while i <= L do begin
    case S[i] of
    '''': begin
      AIsString:= True;
      if state = 0 then state:= 1 else begin
        // state = 1
        if (i > 1) and (S[i - 1] = '''') then begin
          SS:= SS + '''';
        end else begin
        end;
        state:= 0;
      end;

    end;
    '^':begin
          if (i < L) then begin
            Inc(i);  // next symbol
            SS:= SS + Chr(Ord(Upcase(S[i]))-Ord('A')+1);
          end else begin
            SS:= SS + '^'; // это последний символ, что делать неясно, поэтому- оставить
          end;
        end;
    '\':begin
          if (i < L) and (S[i + 1] = '^')
          then SS:= SS + '^'
          else SS:= SS + S[i];
        end;
    '#':begin
          AIsString:= True;
          if (i < L) and (S[i + 1] in ['0'..'9', '$']) then begin
            Inc(i);
            if S[i] = '$' then begin
              isHex:= True;
              Digits:= ['0'..'9', 'A'..'F', 'a'..'f'];
              Inc(i);
            end else begin
              isHex:= False;
              Digits:= ['0'..'9'];
            end;
            SSS:= '';
            repeat
              SSS:= SSS + s[i];
              Inc(i);
              nondigit:= not (S[i] in DIGITS);
            until (i > L) or nondigit;
            if nondigit then Dec(i);
            if isHex
            then SSS:= '$' + SSS;
            SS:= SS + WideChar(StrToIntDef(SSS, 32));
          end else SS:= SS + S[i];
        end;
    else SS:= SS + S[i];
    end;
    Inc(i);
  end;
  Result:= SS;
end;

function WideString2PascalString(const S: WideString): String;
var
  i, len: Integer;
  st: Boolean;
begin
  Result:= '';
  st:= False;
  len:= Length(S);
  for i:= 1 to len do begin
    if (s[i] > #127) or (s[i] < #32) then begin
      if st
        then Result:= Result + '''';
      st:= False;
      Result:= Result + '#' + IntToStr(Word(s[i]));
    end else begin
      if not st
        then Result:= Result + '''';
      if s[i] = ''''
        then Result:= Result + ''''''
        else Result:= Result + S[i];
      st:= True;
    end;
  end;
  if st then Result:= Result + '''';
  
end;

{ added Nov-27-2006
}
// get current keyboard layout name
// '00000419' - RU
// '00000409' - EN-US
function GetKbdLayout: ShortString;
begin
  SetLength(Result, Windows.KL_NAMELENGTH);
  if Windows.GetKeyboardLayoutName(@Result[1]) then begin
    SetLength(Result, Length(PChar(@Result[1])));
  end else SetLength(Result, 0);
end;

// activate keyboard layout with specified name
// '00000419' - RU
// '00000409' - EN-US
procedure SetKbdLayout(AKbdLayoutName: String);
begin
  Windows.LoadKeyboardLayout(PChar(AKbdLayoutName), Windows.KLF_ACTIVATE);
end;

function BytesToFriendlyString(AValue: DWord): String;
const
  OneKB = 1024;
  OneMB = OneKB * 1024;
  OneGB = OneMB * 1024;
begin
  if AValue < OneKB then
    Result := FormatFloat('#,##0.00 B',AValue)
  else
    if AValue < OneMB then
      Result := FormatFloat('#,##0.00 KB', AValue / OneKB)
    else
      if AValue < OneGB then
        Result := FormatFloat('#,##0.00 MB', AValue / OneMB)
end;

function BitsToFriendlyString(AValue: DWord) : String;
const
  OneKB = 1000;
  OneMB = OneKB * 1000;
  OneGB = OneMB * 1000;
begin
  if AValue < OneKB then
    Result := FormatFloat('#,##0.00 bps',AValue)
  else
    if AValue < OneMB then
      Result := FormatFloat('#,##0.00 Kbps', AValue / OneKB)
    else
      if AValue < OneGB then
        Result := FormatFloat('#,##0.00 Mbps', AValue / OneMB)
end;

function Binvector2Str(const AVector: String): String;
const
  BCD: array [0..15] of String[4] =
    ('0000', '0001', '0010', '0011', '0100', '0101', '0110', '0111', '1000', '1001', '1010', '1011', '1100', '1101', '1110', '1111');
var
  i: Integer;
  iszero: Boolean;
begin
  Result:= '';
  iszero:= True;
  for i:= Length(AVector) downto 1 do begin
    if iszero then begin
      iszero:= AVector[i] = #0;
      if iszero then Continue;
    end;
    Result:= Result + BCD[Byte(AVector[i]) shr 4] + BCD[Byte(AVector[i]) and 15]
  end;
  while (Length(Result) > 0) and (Result[1] = '0') do begin
    Delete(Result, 1, 1);
  end;
end;

end.
