
unit account;
(*##*)
(*******************************************************************
*                                                                 *
*   A  C  C  O  U  N  T                                            *
*                                                                 *
*   Copyright (c) 1998, �.������. ��� ����� ���������������        *
*   Russian currency (roubles&copeck) format routine(s)           *
*   Based on Clipper public domain version                         *
*                                                                 *
*   Conditional defines: ---                                       *
*                                                                 *
*   Last Revision: Jul 05 1998                                     *
*   Last fix     : Jul 05 1998                                    *
*   Lines        :                                                 *
*   History      :                                                *
*   Printed      : ---                                             *
*                                                                 *
********************************************************************)
(*##*)

interface
{$IFDEF VER70}
{$N+}
{$ELSE}
uses
  sysutils;
{$ENDIF}
function cyrmoney(nSum: Extended): string;

implementation

const
  QTYTRIAD = 5 + 4;

type
  TTriada = record
    n: string[11];
    t: array[0..2] of string[2];
    s: Boolean;
  end;

const
{$IFDEF VER70}
  NULRUB = '����';
  over10: string[7] = '�������';
  akop: array[0..3] of string[4] = ('����', '���', '���', '��');
  arub: array[0..QTYTRIAD - 1] of TTriada = (
    (n: '����'; t: ('�', '�', '��'); s: True),
    (n: '�����'; t: ('�', '�', ''); s: False),
    (n: '�������'; t: ('', '�', '��'); s: True),
    (n: '��������'; t: ('', '�', '��'); s: True),
    (n: '��������'; t: ('', '�', '��'); s: True),
    (n: '�����������'; t: ('', '�', '��'); s: True),
    (n: '�����������'; t: ('', '�', '��'); s: True),
    (n: '�����������'; t: ('', '�', '��'); s: True),
    (n: '����������'; t: ('', '�', '��'); s: True));

  adigits1: array[0..19] of string[6] =
  ('', '����', '���', '���', '������', '����', '�����', '����', '������', '������',
    '������', '����', '���', '���', '�����', '���', '����', '���', '�����', '�����');
  adigitsT: array[0..2] of string[4] = ('', '����', '���');
  adigit10: array[0..7] of string[11] =
  ('��������', '��������', '�����', '���������', '����������', '���������', '�����������', '���������');
  adigitmore100: array[0..9] of string[9] =
  ('', '���', '������', '������', '���������', '�������', '��������', '�������', '���������', '���������');
{$ELSE}
  NULRUB = '����';
  over10: string[7] = '�������';
  akop: array[0..3] of string[4] = ('����', '���', '���', '��');
  arub: array[0..QTYTRIAD - 1] of TTriada = (
    (n: '����'; t: ('�', '�', '��'); s: True),
    (n: '�����'; t: ('�', '�', ''); s: False),
    (n: '�������'; t: ('', '�', '��'); s: True),
    (n: '��������'; t: ('', '�', '��'); s: True),
    (n: '��������'; t: ('', '�', '��'); s: True),
    (n: '�����������'; t: ('', '�', '��'); s: True),
    (n: '�����������'; t: ('', '�', '��'); s: True),
    (n: '�����������'; t: ('', '�', '��'); s: True),
    (n: '����������'; t: ('', '�', '��'); s: True));

  adigits1: array[0..19] of string[6] =
  ('', '����', '���', '���', '������', '����', '�����', '����', '������', '������',
    '������', '����', '���', '���', '�����', '���', '����', '���', '�����', '�����');
  adigitsT: array[0..2] of string[4] = ('', '����', '���');
  adigit10: array[0..7] of string[11] =
  ('��������', '��������', '�����', '���������', '����������', '���������', '�����������', '���������');
  adigitmore100: array[0..9] of string[9] =
  ('', '���', '������', '������', '���������', '�������', '��������', '�������', '���������', '���������');
{$ENDIF}

function Ing(n: Integer): Integer;
var
  R: Integer;
begin
  if (n >= 11) and (n <= 14)
  then R:= 2 // 11, 12, 13, 14 ������
  else begin
    n:= n mod 10; { ?!! }
    case n of
      1: R:= 0;    // 1 �����
      2..4: R:= 1; // 2, 3, 4 �����
    else R:= 2;    // 0, 5, 6, 7, 8, 9 ������
    end;
  end;
  Ing:= R;
end;

function StrTriad(t: Integer; lTriad: Boolean): string;
var
  digit: Integer;
  c: string;
begin
  c:= '';
  if t > 99 then begin
    digit:= t div 100;
    c:= adigitmore100[digit] + #32;
    t:= t mod 100;
  end;
  if t > 19 then begin
    digit:= t div 10;
    c:= c + adigit10[digit - 2] + #32;
    t:= t mod 10;
  end;
  if (not lTriad) and (t < 3)
    then c:= c + adigitsT[t]
  else begin
    c:= c + adigits1[t];
    if t > 10
    then c:= c + OVER10;
  end;
  StrTriad:= c;
end;

function IntToStrLeadZero(AInt: Integer; ALeadZeroes: Integer): string;
var
  i: Integer;
  c: Byte;
  ch: Char;
  S: string;
begin
  S:= '';
  for i:= 1 to ALeadZeroes do begin
    c:= AInt mod 10;
    AInt:= AInt div 10;
    ch:= Char(Byte('0') + c);
    S:= ch + S;
  end;
  IntToStrLeadZero:= S;
end;

{ based on Clipper, Vladimit Varfolomeev, ���������� + ��������� 1(2), 1996 }

function cyrmoney(nSum: Extended): string;
var
  nTriads, nKop, nTriad, t, i, a20: Integer;
  exp10, n: Integer;
  cSum: string;
begin
  cSum:= '';
  n:= Trunc(Abs(nSum));
  if (Abs(n) > 0) then begin
    nTriads:= Trunc(ln(n) / ln(10)) div 3;
    exp10:= 1;
    for i:= 1 to 3 * nTriads
      do exp10:= 10 * exp10;
  end else begin
    nTriads:= 0;
    exp10:= 1;
  end;
  nKop:= Round(nSum * 100 - 100.0 * n); { original - Trunc }
  { ����� }
  if (n > 0) then begin
    if (nTriads < QTYTRIAD) then begin
      for nTriad:= nTriads downto 0 do begin
        t:= n div exp10;
        if (t > 0) or (nTriad = 0) then begin
          cSum:= cSum + StrTriad(t, aRub[nTriad].s);
          if t <> 0 then cSum:= cSum + #32;
          if (t mod 100) < 20
          then a20:= t mod 100
          else a20:= t mod 10;
          cSum:= cSum + aRub[nTriad].n + aRub[nTriad].t[Ing(a20)] + #32;
          n:= n mod exp10;
        end;
        exp10:= exp10 div 1000;
      end;
    end else begin
      cSum:= '!!!';
    end;
  end else begin
    csum:= NULRUB + #32 + aRub[0].n + aRub[0].t[2] + #32;
  end;
  { ������� }
  if nKop < 20
    then a20:= nKop
  else a20:= nKop mod 10;
  cSum:= cSum + IntToStrLeadZero(nKop, 2) + #32 + aKop[0] + aKop[Ing(a20) + 1];
  cyrmoney:= cSum;
end;

{}
//**************************************************************
// ����������� �.�.   Almaty AnvarT@resp.narbank.kz
// ����-������ ���� '99999999999999,99' ����������,
//**************************************************************
var
  vlaNumb: array[1..19] of string = ('', '', '��� ', '������ ', '����',
    '����� ', '���� ', '������ ', '������ ', '������ ',
    '����������� ', '���������� ', '���������� ', '������������ ',
    '���������� ', '����������� ', '���������� ', '������������ ',
    '������������ ');
  vlaNumb2: array[1..9] of string = ('', '�������� ', '�������� ',
    '����� ', '��������� ', '���������� ', '��������� ', '����������� ', '��������� ');
  vlaNumb3: array[1..9] of string = ('��� ', '������ ', '������ ',
    '��������� ', '������� ', '�������� ', '������� ', '��������� ', '���������');
  vlaCurr: array[1..10, 1..4] of string =
  (('������� ', '�������', '������ ', 'A'),
    ('����� ', '����� ', '������ ', 'M'),
    ('������ ', '������', '����� ', 'A'),
    ('������� ', '��������', '���������', 'M'),
    ('�������� ', '��������� ', '���������� ', 'M'),
    ('�������� ', '��������� ', '���������� ', 'M'),
    ('����������� ', '������������ ', '������������� ', 'M'),
    ('����������� ', '������������ ', '������������ ', 'M'),
    ('����������� ', '������������ ', '������������� ', 'M'),
    ('���������� ', '����������� ', '������������ ', 'M'));

function flsCurr2Str(vlsCurr: string): string;

{ fpsStrTran - �-��� ������� �� ������ vlsStr ��������� vlsSubStr }
  function fpsStrTran(vlsStr: string; vlsSubStr: string): string;
  begin
    Delete(vlsStr, Pos(vlsSubStr, vlsStr), Length(vlsSubStr));
    Result:= vlsStr
  end;

{ flsNumb2Str - �-��� �������� 2-������������ ����� � ������ �.�. '43' � '����� ���' }
  function flsNumb2Str(vlsStr: string; vlaCurr: array of string): string;
  var
    vlsStrCurr: string;
    vliI: Integer;

{ Coop - �-��� ��������� ���������� '�������', '�������', '������', '����� ', '�����' }
    function Coop(vliI: Integer; vlaCurr: array of string): string;
    begin
      if vliI = 1 then begin
        Result:= vlaCurr[0]
      end else begin
        if vliI > 4
          then Result:= vlaCurr[2]
        else Result:= vlaCurr[1]
      end;
    end;

  begin
    vliI:= StrToInt(copy(vlsStr, Length(vlsStr) - 1, 2));
    if vliI > 19 then vliI:= StrToInt(copy(vlsStr, Length(vlsStr), 1));
    if Length(vlsStr) < 3 then begin
      if StrToInt(vlsStr) < 20 then begin
        vlsStrCurr:= vlaNumb[StrToInt(vlsStr)] + Coop(vliI, vlaCurr);
      end else begin
        vlsStrCurr:= vlaNumb2[StrToInt(copy(vlsStr, 1, 1))] +
          vlaNumb[StrToInt(copy(vlsStr, 2, 1))] + Coop(vliI, vlaCurr);
      end;
    end else begin
      vlsStrCurr:= vlaNumb3[StrToInt(copy(vlsStr, 1, 1))] +
        flsNumb2Str(copy(vlsStr, 2, 2), vlaCurr);
    end;
    Result:= vlsStrCurr;
  end;

var
  vlsStr: string;
  vliJ: Integer;
begin
  vliJ:= 1;
  while Length(vlsCurr) <> 0 do begin
    if vlaCurr[vliJ, 4] = 'M' then begin
      vlaNumb[1]:= '���� ';
      vlaNumb[2]:= '��� ';
    end else begin
      vlaNumb[1]:= '���� ';
      vlaNumb[2]:= '��� ';
    end;
    vlsStr:= flsNumb2Str(fpsStrTran(copy(vlsCurr, Length(vlsCurr) - 2, 3), ','), vlaCurr[vliJ])
      + vlsStr;
    vlsCurr:= fpsStrTran(vlsCurr, copy(vlsCurr,
      Length(vlsCurr) - 2, 3));
    vliJ:= vliJ + 1;
  end;
  Result:= vlsStr;
end;

end.

