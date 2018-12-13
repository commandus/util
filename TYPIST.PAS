unit 
  typist;
(*##*)
(*******************************************************************
*                                                                 *
*   T  Y  P  I  S  T           EPAGER's unit.                      *
*                                                                 *
*   Copyright (c) 1998, А.Иванов. Все права зарезервированы        *
*   Routines to assist operator type word (choose then from list) *
*   Interface routines for windows edit control                    *
*   (generate events missed in VCL                                *
*   Conditional defines: ---                                       *
*                                                                 *
*   Last Revision: Apr 15 1998                                     *
*   Last fix     : Apr 15 1998                                    *
*   Lines        : 118                                             *
*   History      : see CHANGES.TXT file                           *
*   Printed      : ---                                             *
*                                                                 *
********************************************************************)
(*##*)

interface
uses
  sysutils, windows, stdctrls, comctrls, RichEdit, Messages;

const
  MSGDELIMS = [#0..'@', '['..'`', '{'..'~'];

// возвращает текущую строку и заполняет параметры позиции
function GetRichPos(Ed: TRichEdit; var line, column: Integer): String;
function GetMemoPos(Ed: TMemo; var line, column: Integer): String;

// возвращает позицию текущего символа
procedure GetSelCharXY(Ed: TCustomEdit; var x, y: Integer);

// возвращает слово,
// в параметрах возвращает начало и длину слова
function GetLineWord(const Line: String; var column, len: Integer): String;

// вставляет слово сохраняя регистр уже введенных слов
procedure InsertWord(NewWord: String; var Line: String; AtPos: Integer);

implementation
uses
  util1;

// возвращает текущую строку и заполняет параметры позиции
function GetRichPos(Ed: TRichEdit; var line, column: Integer): String;
begin
  line:= SendMessage(Ed.Handle, EM_EXLINEFROMCHAR, 0, Ed.SelStart);
  column:= (Ed.SelStart-SendMessage(Ed.Handle, EM_LINEINDEX, line, 0));
  // Inc(line);
  Inc(column);
  Result:= Ed.Lines[line];
end;

// возвращает позицию текущего символа
procedure GetSelCharXY(Ed: TCustomEdit; var x, y: Integer);
var
  p: Integer;
  xy: record
    case integer of
    0: (w: LongInt);
    1:(x, y: Word;);
  end;
begin
  //
  if (x = -1) then begin
    p:= Ed.SelStart;
  end else begin
    p:= SendMessage(Ed.Handle, EM_LINEINDEX, y, 0) + x - 1;
  end;
  xy.w:= SendMessage(Ed.Handle, EM_POSFROMCHAR, p, 0);
  x:= xy.x;
  y:= xy.y;
end;

// возвращает текущую строку и заполняет параметры позиции
// всегда возвращает 0 в line
function GetMemoPos(Ed: TMemo; var line, column: Integer): String;
var
  start: Integer;
begin
  line:= SendMessage(Ed.Handle, EM_LINEFROMCHAR, -1, 0);
  start:= SendMessage(Ed.Handle, EM_LINEINDEX, -1, 0);
  column:= Ed.SelStart-start;
  // Inc(line);
  Inc(column);
  Result:= Ed.Lines[line];
end;

// возвращает слово,
// в параметрах возвращает начало и длину слова
function GetLineWord(const Line: String; var column, len: Integer): String;
var
  i, L, st, fin: Integer;
  s: String;
begin
  Result:= '';
  S:= Line; // позиция может быть посередине слова
  L:= Length(S);
  if (L=0)
  then Exit;
  if (S[column] in MSGDELIMS) then begin
    if ((column <= 1) or (S[column-1] in MSGDELIMS))
    then Exit
    else Dec(column);
  end;
  st:= column;
  i:= st;
  // поищем начало
  repeat
    if i = 1 then begin
      column:= 1;
      Break;
    end;
    if S[i] in MSGDELIMS then begin
      column:= i+1;
      Break;
    end;
    Dec(i);
  until False;
  // поищем конец
  i:= st;
  fin:= st;
  repeat
    if S[i] in MSGDELIMS then begin
      fin:= i-1;
      Break;
    end;
    // именно в этом порядке- сначала проверить последний символ
    if i = L then begin
      fin:= L;
      Break;
    end;
    Inc(i);
  until False;
  len:= fin - column + 1;
  Result:= Copy(S, column, len);
end;

// вставляет слово сохраняя регистр уже введенных слов
procedure InsertWord(NewWord: String; var Line: String; AtPos: Integer);
var
  OldWord: String;
  len: Integer;
  sts: String;
begin
  OldWord:= GetLineWord(line, AtPos, len);
  sts:= Copy(NewWord, 1, len);
  if ANSICompareText(OldWord, sts) = 0 then begin
    // заменить хвост если начало совпадает (для сохранения регистра)
    Delete(NewWord, 1, len);
    Inc(AtPos, len);
  end else begin
    // заменить все
    Delete(Line, AtPos, len);
  end;
  Insert(NewWord, Line, AtPos);
end;

end.
