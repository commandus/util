unit utilini;
(*##*)
(*******************************************************************
*                                                                 *
*   U  T  I  L  I  N  I   Ensen's ini files routines for Delphi    *
*                                                                 *
*   Copyright (c) 1998, A.Ivanov. All rights reserved.             *
*   Based on strutil unit (part of VGAX graphic library) for BP7  *
*   Conditional defines:                                           *
*                                                                 *
*   Last Revision: Mar 19 1998                                     *
*   Last fix     : Oct 01 1998                                    *
*   Lines        : 42                                              *
*   History      : Moved from util1 Mar 09 2000                   *
*   Printed      : ---                                             *
*                                                                 *
********************************************************************)
(*##*)
{ ini file routines }
interface
uses
  Classes, SysUtils, Windows, IniFiles, Grids;

{ ищет ini file BaseIniFn в папках по списку (;)
  Если список не задан, то в текущей папке
  Возвращает полный путь
  если не найдено, возвращает BaseIniFn без пути для использования пути WINDOWS
}
function    SearchIni(PathsList: String; BaseIniFn: String): String;
procedure   FillSGColumn(SG: TStringGrid; const Ini, Section: ShortString;
  StartCol, StartRow: Word; const DefaultValue: ShortString);
procedure   ReadSGColumn(SG: TStringGrid; const Ini, Section: ShortString;
  IndexCol, ReadCol, StartRow: Word; const DefaultValue: ShortString;
  IndexExists: Boolean; AllowResize: Boolean);
{ store to ini file entire section from string list Par=xxx}
procedure   StoreListIni(const AIniFileName, Section: String; SL: TStrings);
procedure   StoreSGColumn(SG: TStringGrid; const Ini, Section: ShortString;
  IndexCol, StoreCol, StartRow: Word; const DefaultValue: ShortString);
function    SectionsQTY(Ini: ShortString;var List: TStringList): LongInt;
function    SectionItems(Ini, Section: ShortString): LongInt;
function    StoreTAG(SG: TStringGrid; FN: ShortString): Boolean;
function    IsSubField(SGRows: TStrings): Boolean;

procedure   SGOEM2ANSI(SG: TStringGrid; const ExceptCols);
procedure   SGANSI2OEM(SG: TStringGrid; const ExceptCols);

{ do not use!
}
function GetSectionName(const S: ShortString): ShortString;

implementation
uses
  util1;
{ ищет ini file BaseIniFn в папках по списку (;)
  Если список не задан, то в текущей папке
  Возвращает полный путь
  если не найдено, возвращает BaseIniFn без пути для использования пути WINDOWS
}
function    SearchIni(PathsList: String; BaseIniFn: String): String;
var
  Where: String;
  count: Integer;
begin
  if PathsList = '' then begin
    PathsList:= GetCurrentDir; // GetWindowsDirectory();
  end;
  count:= 1;
  repeat
    Where:= GetToken(count, ';', PathsList);
    if Where = '' then begin
      // не найден  файл
      Result:= BaseIniFN;
      Exit;
    end;
    Where:= ConCatPath(Where, BaseIniFN);
    if FileExists(Where) then begin
      // файл найден
      Result:= Where;
      Exit;
    end;
    Inc(count);
  until False;
end;

{ read TString from ini file and put him to the
  ShortString grid column
}
procedure FillSGColumn(SG: TStringGrid; const Ini, Section: ShortString;
  StartCol, StartRow: Word; const DefaultValue: ShortString);
begin
  ReadSGColumn(SG,Ini,Section,
  StartCol, StartCol+1, StartRow, DefaultValue, False, True);
end;

{ read TString from ini file and put him to the
  ShortString grid column
}
procedure ReadSGColumn(SG: TStringGrid; const Ini, Section: ShortString;
  IndexCol, ReadCol, StartRow: Word; const DefaultValue: ShortString;
  IndexExists: Boolean; AllowResize: Boolean);
var
  WIni: TIniFile;
  items, i, last: Word;
  SL: TStringList;
begin
  SL:= TStringList.Create;
  try
    WIni := TIniFile.Create(INI);
  except
    raise EExternal.CreateFmt('Ошибка чтения инициализационного файла %s', [INI]);
    Exit;
  end;
  Items:= SectionItems(Ini, Section);
  if Items = 0 then begin
    {
    ShowMessage('Инициализационный файл пуст');
    }
    if AllowResize
    then SG. RowCount:= StartRow+1;
    Exit;
  end;
  if AllowResize
  then SG. RowCount:= Items+StartRow;
  {
  SG.Cols[IndexCol].Clear;
  SG.Cols[ReadCol].Clear;
  }
  Last:= SG.RowCount - 1;
  if IndexExists then begin
    for i:= StartRow to Last do begin
      SG. Cells[ReadCol, i]:= WIni. ReadString(Section,
      SG.Cells[IndexCol, i], DefaultValue);
    end;
  end else begin
    WIni.ReadSection(Section, SL);
    if SL.Count + StartRow < SG.RowCount
    then Last:= SL.Count + StartRow - 1;
    for i:= StartRow to Last do begin
      SG.Cells[IndexCol, i]:= SL.Strings[i-StartRow];
      SG. Cells[ReadCol, i]:= WIni. ReadString(Section,
        SL.Strings[i-StartRow], DefaultValue);
    end;
  end;
  WIni.Free;
  SL.Free;
end;

{ store TString from ini file and put him to the
  ShortString grid column
}
procedure StoreSGColumn(SG: TStringGrid; const Ini, Section: ShortString;
  IndexCol, StoreCol, StartRow: Word; const DefaultValue: ShortString);
var
  WIni: TIniFile;
  i: Word;
begin
  try
    WIni := TIniFile.Create(INI);
  except
    raise EExternal.CreateFmt('Ошибка открытия для записи инициализационного файла %s', [INI]);
    Exit;
  end;
  for i:= StartRow to SG.RowCount - 1 do begin
    WIni. WriteString(Section,
      SG.Cells[IndexCol, i], SG. Cells[StoreCol, i]);
  end;
  WIni.Free;
end;

function FirstChar(S: ShortString): Char;
var
  i: Word;
  C: Char;
begin
  C:= #0;
  for i:= 1 to Length(S) do begin
    if S[i] in [#0,#9,#32]
    then Continue;
    C:= S[i];
    Break;
  end;
  FirstChar:= C;
end;

function GetSectionName(const S: ShortString): ShortString;
var
  SS: ShortString;
  E: Word;
begin
  GetSectionName:= '';
  SS:= S;
  if Length(SS) = 0
  then Exit;
  while (Length(SS) > 1) and (SS[1] <> '[' ) do begin
    System.Delete(SS,1,1);
  end;
  System.Delete(SS,1,1);
  E:= System.Pos(']', SS);
  if E <= 1
  then Exit;
  System.Delete(SS, E, Length(SS) - E + 1);
  GetSectionName:= SS;
end;

procedure StoreListIni(const AIniFileName, Section: String; SL: TStrings);
var
  IniFile: TIniFile;
  i: Integer;
  S: String;
begin
  IniFile:= TIniFile.Create(AIniFileName);
  if SL <> Nil then begin
    for i:= 1 to SL.Count do begin
      S:= GetToken(1,'=',SL.Strings[i-1]);
      IniFile.WriteString(Section, S, SL.Values[S]);
    end;
  end;
  IniFile.Free;
end;

function SectionItems(Ini, Section: ShortString): LongInt;
var
  SL: TStringList;
  WIni: TIniFile;
begin
  SL:= TStringList.Create;
  try
    try
      WIni := TIniFile.Create(INI);
      WIni.ReadSection(Section, SL);
      SectionItems:= SL.Count;
    except
      SectionItems:= 0;
    end;
  finally
    SL.Free;
  end;
end;

function SectionsQTY(Ini: ShortString;var List: TStringList): LongInt;
var
  SL: TStringList;
  SN: ShortString;
  Sect, i: Word;
begin
  SL:= TStringList.Create;
  Sect:= 0;  
  try
    SL.LoadFromFile(INI);
    for i:= 0 to SL.Count - 1 do begin
      SN:= GetSectionName(SL[i]);
      case FirstChar(SL[i]) of
      '[': begin
           try
             List.Insert(Sect,SN);
             Inc(Sect);
           except
             raise EExternal.CreateFmt('Слишком много секций в %s', [INI]);
             Break;
           end;
           end;
      #0:  begin
           end
      else begin
           end;
      end;
    end;
  finally
    Result:= Sect;
    SL.Free;
  end;
end;

function isSubField(SGRows: TStrings): Boolean;
var
  S: ShortString;
begin
  {
  S:= SGRows.Strings[1];
  DeleteLeadTerminateSpaceStr(S);
  Val(S, V, Code);
  isSubField:= Code <> 0;
  }
  S:= SGRows.Strings[0];
  isSubField:= Pos('*', S) = 0;
end;

function StoreTAG(SG: TStringGrid; FN: ShortString): Boolean;
var
  F: Text;
  y: Word;
  S: ShortString;
  AttrChar: Char;
begin
  StoreTAG:= True;
  try
    try
      Assign(F, FN);
      Rewrite(F);
    except
      StoreTAG:= False;
      raise EExternal.CreateFmt('Ошибка создания файла %s', [FN]);
      // Exit;
    end;
    for y:= 1 to SG. RowCount - 1 do begin
      DeleteLeadTerminateSpaceSL(SG.Rows[y]);
      try
        if isSubField(SG.Rows[y]) then begin
          { is it reapeatadly?
          }
          if Pos(#133, SG.Cells[3, y]) > 0
          then AttrChar:= 'r'
          else AttrChar:= #32;
          S:= Format(#32#32'%1.1s%3.3d%1.1s%s',
          [AttrChar, StrToInt(SG.Cells[2,y]), SG.Cells[1,y],
          SG.Cells[4,y]]);
        end else begin
          S:= Format('%3.3d%3.3d%2.2d%s',
          [StrToInt(SG.Cells[1,y]), StrToInt(SG.Cells[2,y]),
          StrToInt(SG.Cells[3,y]), SG.Cells[4,y]]);
        end;
      except
        StoreTAG:= False;
        raise EExternal.CreateFmt('Ошибка в строке %d', [y]);
      end;
      Writeln(F, S);
    end;
  finally
    Close(F);
  end;
end;

procedure SGOEM2ANSI(SG: TStringGrid; const ExceptCols);
var
  x, y: Word;
  POEM, PANSI: array[0..255] of Char;
begin
  for y:= 0 to SG. RowCount - 1 do begin
    for x:= 0 to SG. ColCount - 1 do begin
      if not (x in TBS(ExceptCols)) then begin
        StrPCopy(POEM, SG.Cells[x,y]);
        OEMToANSI(POEM, PANSI);
        SG.Cells[x,y]:= StrPas(PANSI);
      end;
    end;
  end;
end;

procedure SGANSI2OEM(SG: TStringGrid; const ExceptCols);
var
  x, y: Word;
  POEM, PANSI: array[0..255] of Char;
begin
  for y:= 0 to SG. RowCount - 1 do begin
    for x:= 0 to SG. ColCount - 1 do begin
      if not (x in TBS(ExceptCols)) then begin
        StrPCopy(POEM, SG.Cells[x,y]);
        OEMToANSI(POEM, PANSI);
        SG.Cells[x,y]:= StrPas(PANSI);
      end;
    end;
  end;
end;

end.
