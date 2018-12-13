unit isutil1;
(*##*)
(*******************************************************************
*                                                                 *
*   I  S  U  T  I  L  1     IS2SQL routines                        *
*                                                                 *
*   Copyright (c) 1999, А. Иванов                                  *
*   вспомогательные функции                                       *
*   Part of IS2SQL                                                 *
*                                                                 *
*   for more information see readme.txt                            *
*                                                                 *
*   Conditional defines:                                           *
*                                                                 *
*   Last Revision: Jan 18 1999                                     *
*   Last fix     : Jan 18 1999                                    *
*   Lines        :                                                 *
*   History      :                                                *
*   Printed      : ---                                             *
*                                                                 *
********************************************************************)
(*##*)

interface
uses
  WinProcs, Classes, filecoll, Registry;

type
  TMBillInfo = record
    MaxConnections: Integer;
    ActiveCount: Integer;
    InActiveCount: Integer;
    QueryCount: Integer;
  end;

type
  TLogStruc = record
    remoteIP: String[4*4];    { имя журнала (файла журнала) }
    t0,                       { начало по времени }
    dt: TDateTime;            { продолжительность }
    len: Integer;             { длина ответа, отправляемого клиенту }
    empno: Integer;           { номер сотрудника }
    lst: String[255];         { хост,алиас,имя,парольБД,форма,семейство,действие }
  end;

  TLogStart = record
    FN: String[255];
  end;

  TGetWebInfo = procedure(var AMBill: TMBillInfo); stdcall;
  { islog.DLL function TLogFunc write a log file }
  TLogFunc = function(ALogP: TLogStruc): Boolean; stdcall;
  TLogStartFunc = function(ALogStart: TLogStart): Boolean; stdcall;
  { islog.DLL function TInfoPanel displays about of is2sql.dll }

  TInfoPanelFunc = function(AShow: Boolean): Boolean; stdcall;

  TLogThread = record
    hLogdllInst: THandle;
    LogFunc: TLogFunc;
    LogStartFunc: TLogStartFunc;
  end;

  PLogThread = ^TLogThread;

  { formatting routine <#f name= fmt=external dll= func= [modifier=]>}
  TFuncFmtDll = function(AData, Aspecifier: PChar; ABuf: PChar; ABufLen: Integer): Boolean; stdcall;

//  procedure GetWebInfo(var AMBill: TMBillInfo); stdcall;
  // external 'isutil1.dll' index 1;

function BoolToStr(A: Boolean): String;

{ fill up report }
function BoolToStr1(A: Boolean): String;

{ fill up COMBOBOX tag }
function Bool2Checked(A: Boolean): String;

{ SetFileCollection читает полностью файл коллекции файлов в кеш.
  Файл коллекции файлов - это записанные в один файл для удобства корректирования несколько
  файлов.
  Тип коллекции задает метод, как записывать и читать из файла
    - 0, определить тип автоматически
  Если имя файла коллекции = '', то коллекция не задана и
  файл читается функцией LoadCasheString по заданному пути, а не из
  коллекции (по умолчанию ).
}
function SetFileCollection(const ACollectionFileName: String): Boolean;

{ LoadCacheString читает строку из файла с именем AFileName
  и возвращает его содержимое.
  В случае загрузки нескольких файлов по маске возвращает
  содежимое последнего файла
  В случае ошибок также возвращает ''.
}
function LoadCacheString(const AFileName: String): String;

function ExtractFirstInTag(AToken: String; var Tail: String): String;

{ name="value"  -> name=value
  name='value'  -> name=value
}
function NoQuotes(A: String): String;
{ name="value"  -> name=value}
function No2Quotes(A: String): String;

{ search '+' at the end of string and remove it
  return true if found
         false- not found
  '..\+' at the end of string -> '+'
  '..+' at the end of string -> '..' - remove plus sign from tail
}
function ValidateConcatenate(var S: String): Boolean;
{ Examples
  val1="1 2" -> val1='1 2'    False
  val2="12+"  -> val1=12      True
  val3="'12'" -> val1=12      False
  val4="'1 2+'" -> val1='1 2' True

}
function  ValidateQuotedStringValue(var S: String): Boolean;

const
  { registry constants }
  { version }
  LNVERSION = '1.0';
  { resource language }
  LNG = ''; { DLL language usa, 409 }
  { registry path }
  RGPATH = 'Software\ensen\is2sql\'+ LNVERSION;
  RGW2SVCALIAS = '\SYSTEM\CurrentControlSet\Services\W3SVC\Parameters\Virtual Roots';

  logstampT0 = 1;
  logstampDT = 2;

{ ставит временной штамп в журнале. logstampT0 также обнуляет все поля
  кроме поля t0. Можно указывать комбинацию logstampXX + logstampXX.
  используется маска из констант logstampT0, logstampDT
}
procedure LogStamp(var ALogStruc: TLogStruc; AAction: Integer);

{ APaths- список путей, разделенных запятой }
function StartLog(var ALogStruc: TLogStruc;
  APaths, ADll, AFunc, AStartFunc, ALogFile: String;
  ALogThread: PLogThread): Boolean;

procedure StopLog(ALogThread: PLogThread);

{ calc current password for phone }
function CalcHashAccount(AHashKey: String; const phone: String): String;

{ verify current phone password }
{ 0 - hash string NOT verified 1 - hash verified }
function VerifyHashAccount(AHashKey, phone, hash: String): Integer;

{ AFmt = java }
function FormatFmt(S, AFmt: String): String;

function MoneyStr(AMoney: Extended; ACurrencySimple: Boolean): String;

procedure MkValuesQuote(var SL: TStrings);

// Load a list of domains (or any other list for that matter)
// where Item := Request.PathInfo;
procedure LoadDomains(Key, Item: String; List: TStrings);

function CheckDomain(Domain: String; List: TStrings): Boolean;

implementation
uses
  sysutils,
  util1, secure;

function BoolToStr(A: Boolean): String;
begin
  if A
  then BoolToStr:= '1'
  else BoolToStr:= '0';
end;

{ fill up report }
function BoolToStr1(A: Boolean): String;
begin
  if A
  then BoolToStr1:= 'yes'
  else BoolToStr1:= 'no';
end;

{ fill up COMBOBOX tag }
function Bool2Checked(A: Boolean): String;
begin
  if A
  then Bool2Checked:= 'CHECKED'
  else Bool2Checked:= '';
end;

var
  FFileColl: TFileColl;

{ устанавливает имя файла коллекции FFileCollection
}
function SetFileCollection(const ACollectionFileName: String): Boolean;
begin
  Result:= True;
  if FFileColl = Nil
  then FFileColl:= TFileColl.Create;
  try
    FFileColl.CollectionFile:= ACollectionFileName;
  except
    FFileColl.Free;
    FFileColl:= Nil;
  end;
end;

{ load one string }
procedure LoadCasheString1(const AFileName: String; var R: String);
begin
  if FFileColl = Nil then begin
    R:= LoadString(AFileName);
  end else begin
    R:= FFileColl.Content[ExtractFileName(AFileName)];
    { если нет в коллекции, читать из каталога }
    if R = ''
    then R:= LoadString(AFileName);
  end;
end;

{ LoadCachedString читает строку из файла с именем AKey
  и возвращает его содержимое.
  В случае загрузки нескольких файлов по маске возвращает
  содежимое последнего файла
  В случае ошибок также возвращает ''.
}
function LoadCacheString(const AFileName: String): String;
var
  SearchRec: TSearchRec;
begin
  Result:= '';
  if IsFileMask(AFileName) then begin
    if FindFirst(AFileName, faAnyFile, SearchRec)=0 then begin
      while FindNext(SearchRec) = 0 do begin
        { загрузить файл }
        LoadCasheString1(SearchRec.Name, Result);
      end;
    end;
    // ??!
    // FindClose(SearchRec.FindHandle);
    FindClose(SearchRec);
  end else begin
    LoadCasheString1(AFileName, Result);
  end;
end;

{ APath - каталог[файл/маска] - обновляет кеш, перечитывая файлы
            Для форсирования при загрузке.
         '' - просто сбрасывает кеш
}
function ForceCasheString(APath: String): Boolean;
begin
  ForceCasheString:=True;
end;

{ name="value"  -> name=value}
function NoQuotes(A: String): String;
var
  L: Integer;
begin
  Result:= A;
  L:= Length(Result);
  while (L >= 2) and (Result[1] in ['"', '''']) and (Result[L] in ['"', '''']) do begin
    Result:= Copy(Result, 2, L - 2);
    Dec(L, 2);
  end;
end;

function No2Quotes(A: String): String;
var
  L: Integer;
begin
  Result:= A;
  L:= Length(Result);
  while (L >= 2) and (Result[1] ='"') and (Result[L] = '"') do begin
    Result:= Copy(Result, 2, L - 2);
    Dec(L, 2);
  end;
end;

function ValidateConcatenate(var S: String): Boolean;
var
  L: Integer;
begin
  Result:= False;
  L:= Length(S);
  if L = 0 then Exit;
  if S[L] = '+' then begin
    if (L>1) and (S[L-1] = '\') then begin
      Delete(S, L-1, 1);      // удалить символ '\'
    end else begin
      Delete(S, L, 1);      // удалить символ конкатенации '+'
      Result:= True;
    end;
  end;
end;

{ Examples
  val1="1 2" -> val1='1 2'    False
  val2="12+"  -> val1=12      True
  val2="12\+"  -> val1=12+    False - backslash indicate symbol
  val3="'12'" -> val1=12      False
  val4="'1 2+'" -> val1='1 2' True
}
function  ValidateQuotedStringValue(var S: String): Boolean;
const
  DELIMITERS = [#0..#32]; // simple!
var
  j, p, L: Integer;
begin
  Result:= False;
  p:= Pos('=', S); // if p = 0 that is ok
  L:= Length(S);
  if L <= 1 then Exit;
  if S[p+1] = '"' then begin
    // убрать двойные кавычки
    if S[L] in ['''', '"']
    then Delete(S, L, 1);
    Delete(S, 1, 1);
    Dec(L, 2);
  end;
  if S[p+1] = '''' then begin // одинарные кавычки уже стоят
    if (L >= 4) and (S[L-1] = '+') then begin
      if S[L-2] = '\' then begin
        Delete(S, L-2, 1);      // удалить символ '\'
      end else begin
        Delete(S, L-1, 1);      // удалить символ конкатенации '+'
        Result:= True;
      end;
    end;
    Exit;
  end;
  // никаких кавычек нет
  Result:= ValidateConcatenate(S);
  // если есть пробелы- закавычить одинарными
  for j:= p+1 to L do begin           // проверить на наличие пробельных символов
    if S[j] in DELIMITERS then begin
      // закавычить
      S:= S + '''';         // сначала сзади
      Insert('''', S, p+1); // теперь спереди
      Break;
    end;
  end;
end;

function ExtractFirstInTag(AToken: String; var Tail: String): String;
var
  p: Integer;
begin
  Tail:= WEBStyleString2ASCII(ChangeChars(#10, #32, ChangeChars(#13, #32, AToken)));
  DeleteLeadTerminateDoubledSpaceStr(Tail);
  p:= Pos('`', Tail);
  if p = 1 then begin
    p:= PosFrom(2, '`', Tail);
    if p > 2 then begin
      Result:= Copy(Tail, 2, p-1-1);
      Tail:= Copy(Tail, p+1, MaxInt);
    end else begin
      Result:= '';
    end;
  end else begin
    p:= Pos(#32, Tail);
    if p <= 1 then begin { один пробел? }
      Result:= Tail;
      Tail:= '';
    end else begin
      Result:= Copy(Tail, 1, p-1);
      Tail:= Copy(Tail, p+1, MaxInt);
    end;
  end;
end;

{ logstampT0 = 1
  logstampDT = 2
}
procedure LogStamp(var ALogStruc: TLogStruc; AAction: Integer);
begin
  with ALogStruc do begin
    try
      { начальное заполнение структуры }
      if (AAction and logstampT0) = logstampT0
      then t0:= Now;
      { разница между текущим временем и стартовым }
      if (AAction and logstampDT) = logstampDT
      then dt:= Now - t0;
    except
    end;
  end;
end;

function StartLog(var ALogStruc: TLogStruc;
  APaths, ADll, AFunc, AStartFunc, ALogFile: String;
  ALogThread: PLogThread): Boolean;
var
  i: Integer;
  AltPath: String;
  LogStart: TLogStart;
begin
  Result:= False;
  ALogThread^.hLogdllInst:= 0;
  ALogThread^.LogFunc:= Nil;
  ALogThread^.LogStartFunc:= Nil;
  if ALogFile = '' { не указан файл журнала}
  then Exit;
  i:= 1;
  repeat
    AltPath:= ExtractFilePath(GetToken(i, ',', APaths));
    { последний раз указывается '' }
    ALogThread^.hLogdllInst:= LoadLibrary(PChar(ConcatPath(AltPath, ADll, '\')));
    Inc(i);
  until (ALogThread^.hLogdllInst > 0) or (AltPath = '');
  if ALogThread^.hLogdllInst =0  { dll не загружена }
  then Exit;

  ALogThread^.LogStartFunc:= GetProcAddress(ALogThread^.hLogdllInst, PChar(AStartFunc));
  ALogThread^.LogFunc:= GetProcAddress(ALogThread^.hLogdllInst, PChar(AFunc));
  if (not Assigned(ALogThread^.LogFunc)) or (not Assigned(ALogThread^.LogStartFunc))
  then Exit;       { процедура в dll не найдена }

  { init }
  if not util1.IsAbsolutePath(ALogFile)
  then ALogFile:= ConcatPath(AltPath, ALogFile, '\');
  LogStart.FN:= ALogFile;
  Result:= ALogThread^.LogStartFunc(LogStart);
  if not Result
  then Exit;

  { попробовать поместить запись в журнал }
  ALogStruc.remoteIP:= '0.0.0.0';
  LogStamp(ALogStruc, logstampT0 + logstampDT);
  ALogStruc.len:= -1;
  ALogStruc.empno:= -1;
  ALogStruc.lst:= 'start'#9;
  { если сейчас (при старте) невозможно сделать записи, вернуть False}
  Result:= ALogThread^.LogFunc(ALogStruc);
end;

procedure StopLog(ALogThread: PLogThread);
begin
  if ALogThread^.hLogdllInst > 0
  then FreeLibrary(ALogThread^.hLogdllInst);
  FillChar(ALogThread^, SizeOf(ALogThread^), 0);
end;

function CalcHashAccount(AHashKey: String; const phone: String): String;
begin
  SetMachine_Id(AHashKey+phone);
  Result:= GetSecure;
end;

{ verify user account }
{ Return:
  0 - hash string NOT verified
  1 - hash verified
}
function VerifyHashAccount(AHashKey, phone, hash: String): Integer;
begin
  Result:= 0;
  SetMachine_Id(AHashKey+phone);
  case IsValidPasswd(hash) of
  True:  Result:= 1;
  end;
end;

function FormatFmt(S, AFmt: String): String;
begin
  Result:= S;
  if ANSICompareStr(AFmt, 'java') = 0 then begin
    while ReplaceStr(Result, False, '"', '\"') do;
    while ReplaceStr(Result, False, #13, '') do;
    while ReplaceStr(Result, False, #10, '\n') do;
  end else begin
  end;
end;

function MoneyStr(AMoney: Extended; ACurrencySimple: Boolean): String;
begin
  if ACurrencySimple
  then Result:= IntToStr(Trunc(AMoney)) + '=' + Format('%-2.2d', [Round(Frac(AMoney)*100)])
  else Result:= Format('%m', [AMoney]);
end;

{ TStrings проверяет значения и если не закавычено закавычивает значения}
procedure MkValuesQuote(var SL: TStrings);
var
  i, p: Integer;
begin
  for i:= 0 to SL.Count - 1 do begin
    p:= Pos('=', SL[i]);
    if p > 0 then begin
       if PosFrom(p+1, '"', SL[i]) = p+1 then begin
       end else begin
         SL[i]:= Copy(SL[i], 1, p) + '"' + Copy(SL[i], p + 1, MaxInt) + '"';
       end;
    end;
  end;
end;

// Load a list of domains (or any other list for that matter)
// where Item := Request.PathInfo;
procedure LoadDomains(Key, Item: String; List: TStrings);
  procedure DoLoadDomains(APath: String);
  var
    s: String;
    i: Integer;
    R: TRegistry;
  begin
    R:= TRegistry.Create;
    R.RootKey:= HKEY_LOCAL_MACHINE;
    if not(R.OpenKeyReadOnly(APath))
    then Exit;
    try
      i := 0;
      while R.ValueExists(Key+IntToStr(i)) do begin
        s := R.ReadString(Key+IntToStr(i));
        if (s <> '')
        then List.Add(s);
        Inc(i);
      end;
    finally
      R.CloseKey;
      R.Free;
    end;
  end;

begin
  List.Clear;
  // Load item-specific valid-referers
  if (Item <> '')
  then DoLoadDomains(RGPATH+'\'+item);
end;

// Search for a domain or IP # in a list of domains/super domains
function CheckDomain(Domain: String; List: TStrings): Boolean;
var
  i: integer;
begin
  Result:= False;
  // Strip request protocol
  if (Pos('http://', Domain) > 0)
  then Delete(Domain, 1, pos('http://', Domain)+length('http://')-1);
  // Strip path
  if (pos('/', Domain) > 0)
  then Domain:= Copy(Domain, 1, pos('/', Domain)-1);
  // Validate domain
  for i := 0 to List.Count-1 do begin
    // To allow sub-domains, check from right to left for DNS names (may start with '.')
    if (copy(List[i],1,1) = '.') or (copy(List[i],length(List[i]),1) <> '.') then begin
      if (copy(Domain, length(Domain)-length(List[i])+1, length(List[i])) = List[i]) then begin
        Result := True;
        exit;
      end;
    end else begin
    // To allow sub-domains, check from right to left for IP numbers (ends with '.')
      if (copy(Domain, 1, length(List[i])) = List[i]) then begin
        Result := True;
        exit;
      end;
    end;
  end;
end;

begin
  FFileColl:= Nil;
end.
