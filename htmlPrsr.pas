unit
  HtmlPrsr;
(*##*)
(*******************************************************************
*                                                                 *
*   H  T  M  L  P  R  S  R  Simple html parser                     *
*                                                                 *
*   Copyright (c) 1997-1998, A.Ivanov. All rights reserved.        *
*   Based on TSimpleHTMLParser class, (c) Borland Intl, 1996      *
*   Conditional defines:                                           *
*                                                                 *
*   Last Revision: Sep 25 1998                                     *
*   Last fix     : Sep 25 1998                                    *
*   Lines        : 90                                              *
*   History      : see CHANGES.TXT file                           *
*   Printed      : ---                                             *
*                                                                 *
********************************************************************)
(*##*)

interface
uses
  Windows, Messages, SysUtils, Classes;

type

  TToken = (etEnd, etSymbol, etHTMLTag);
  TCaseSensitive = (csNone, csUppercase, csLowercase);

  TSimpleHTMLParser = class
  private
    FText: string;
    FSourcePtr: PChar;
    FTokenPtr: PChar;
    FTokenString: string;
    FToken: TToken;
    FCaseSensitive: TCaseSensitive;
    procedure NextToken;
    function TokenHTMLTagIs(const S: string): Boolean;
    procedure SetText(const NewString: String);
    function GetTitle: String;
  public
    StripHeader: Boolean; { set StripHeader before assign Text property }
    constructor Create;
    { remove all the HTML tags and only display the text }
    function TextOnly: String;
    function UrlList(ARoot: String): TStringList;   { extract related A HREF  links }
    function UrlNameList(): TStringList;   { extract related A HREF  links }
    function MailList: TStringList;  { extract related A HREF=mailto links }
    { extract related HREF http links }
    procedure AddUrlList(ARoot: String; R: TStringList);
    procedure AddUrlNameList(R: TStringList);
    procedure AddMailList(R: TStringList);
    function WordList: TStringList; { return aplpahbetically sorted TStringList of words }
    { looking for META tag and produce list of values }
    function TagParams(ATag, ARequiredParam, ARequiredValue: String): TStringList;
    { looking for META tag and charset parameter value }
    function MetaParam(AMetaType, AMetaTypeValue, AMetaParam: String): String;
    { translate entire FText}
    procedure DoXlat(Axlat: Pointer);
  published
    property Text: String read FText write SetText;
    property Title: String read GetTitle;
    property CaseSensitive: TCaseSensitive read FCaseSensitive write FCaseSensitive;
  end;

implementation

uses
  util1, cpcoll;

const
  FEmptyString: String = '';
  WORDCHARS   = [#0..#255] - [#0..'@', '['..'`', '{'..#127];
  DELIMITERS1 = [#1..';', '='..'@', '['..'`', '{'..#127];

constructor TSimpleHTMLParser.Create;
begin
  FCaseSensitive:= csUppercase; // uppercase
  StripHeader:= False;
  SetText(FEmptyString);
end;

procedure TSimpleHTMLParser.SetText(const NewString: String);
var
  st: Integer;
begin
  if StripHeader then begin
    { search header delimiter (pair of CRLF) }
    st:= Pos(#13#10#13#10, NewString);
    if st <=0
    then FText:= NewString
    else FText:= Copy(NewString, st + 4, MaxInt);
  end else begin
    FText:= NewString;
  end;
  FSourcePtr:= PChar(FText);
  NextToken;
end;

procedure TSimpleHTMLParser.NextToken;
var
  P, TokenStart: PChar;
begin
  FTokenString := '';
  P := FSourcePtr;
  while (P^ <> #0) and (P^ <= #32)
  do Inc(P);
  FTokenPtr := P;
  case P^ of
  '<':begin
    Inc(P);
    TokenStart:= P;
    while (P^ <> '>') and (P^ <> #0) do Inc(P);
    SetString(FTokenString, TokenStart, P - TokenStart);
    FToken:= etHTMLTag;
    Inc(P);
  end;
  #0: FToken:= etEnd;
  else begin
    while (P^ <> #0) and (P^ in DELIMITERS1) do Inc(P);
    TokenStart:= P;
    while (P^ <> #0) and (P^ in WORDCHARS) do Inc(P);
    FToken:= etSymbol;
    SetString(FTokenString, TokenStart, P - TokenStart);
  end;
  end;
  FSourcePtr:= P;
end;

function TSimpleHTMLParser.TokenHTMLTagIs(const S: string): Boolean;
begin
  Result:= (FToken = etHTMLTag) and (AnsiPos(AnsiUppercase(S), AnsiUppercase(FTokenString)) = 1);
end;

{ remove all the HTML tags and only display the text }
function TSimpleHTMLParser.TextOnly: String;
var
  R: String;
begin
  R:= '';
  { from start }
  FSourcePtr:= PChar(FText);
  NextToken;
  { while not #0 }
  while FToken <> etEnd do begin
    case FToken of
    etHTMLTag: begin
      if TokenHTMLTagIs('BR')
      then R:= R + #13#10;
      if TokenHTMLTagIs('P')
      then R:= R + #13#10#13#10;
    end;
    etSymbol:
      R:= R + #32 + FTokenString;
    end;
    NextToken;
  end;
  TextOnly:= R;
end;

{ looking for META tag and produce list of values
  ARequiredParam, ARequiredValue are optional. For example, if you set values to:
  'NAME'          'GENERATOR'
  and ATag='META', <META NAME=GENERATOR CONTENT="WWW"> tag returns only.
  If ARequiredParam, ARequiredValue are empty, any META tag(s) returns in mix
  If ARequiredParam is not empty, but ARequiredValue is empty, ..
  Restriction:
  ARequiredValue must be alone (list of values like "a; b") is not parsed
}
function TSimpleHTMLParser.TagParams(ATag, ARequiredParam, ARequiredValue: String): TStringList;
var
  i, L, st, fn: Integer;
  pstate: Integer;
  TagName,
  vl: String;
begin
  fn:= 1;
  st:= 1;   { just for disable compiler warnings }
  Result:= TStringList.Create;
  Result.Duplicates:= dupIgnore;
  Result.Sorted:= True;
  { from start }
  FSourcePtr:= PChar(FText);
  NextToken;
  { while not #0 }
  while FToken <> etEnd do begin
    if (FToken = etHTMLTag) and TokenHTMLTagIs(ATag) then begin
      i:= 1;
      L:= Length(FTokenString);
      { skip forward spaces }
      while i <= L do begin
        if FTokenString[i] > #32
        then Break;
        Inc(i);
      end;
      { extract tag name }
      TagName:= '';
      while i <= L do begin
        if FTokenString[i] <= #32
        then Break;
        TagName:= TagName + FTokenString[i];
        Inc(i);
      end;
      if UpperCase(ATag) <> UpperCase(TagName)then begin
        Exit;
      end;

      { states: 0- name, 1-wait_value 2-value 3-value with " 4- next parameter }
      pstate:= 4;
      while i <= L do begin
        if pstate = 4 then begin
          { skip spaces before parameter pair start }
          while i < L do begin
            if FTokenString[i] > #32
            then Break;
            Inc(i);
          end;
          st:= i;
          pstate:= 0;
        end;
        case FTokenString[i] of
        #1..#32: begin
               if pstate = 2 then begin
                 Result.Add(vl + Copy(FTokenString, st, fn-st+1));
                 st:= i;
                 pstate:= 4;
               end;
             end;
        '=': begin
               if pstate = 0 then begin
                 vl:= Copy(FTokenString, st, fn-st+1) + '=';
                 st:= i + 1;
                 pstate:= 1;
               end;
             end;
        '''', '"': begin
               case pstate of
               1:begin
                   pstate:= 3;
                   st:= i + 1;
                 end;
               3:begin
                   Result.Add(vl + Copy(FTokenString, st, i-st));
                   pstate:= 4;
                 end;
               end;
             end;
          else begin
            if pstate = 1 then begin
              pstate:= 2;
              st:= i;
            end;
            fn:= i;
          end;
        end;
        Inc(i);
      end;
      NextToken;
    end;
    if (ARequiredParam = '') then begin
      { not specified required parameter in tag, go to next }
      NextToken;
    end else begin
      { required parameter must be exists }
      if (Result.IndexOfName(ARequiredParam) < 0) then begin
        { does not exists required parameter, go to next }
        Result.Clear; { it was wrong tag (no required parameter) }
        NextToken;
      end else begin
        { required tag found, now check parameter value }
        { if not specified value for required parameter, continue }
        if ARequiredValue = '' then begin
          { add next }
          NextToken;
        end;
        if CompareText(Result.Values[ARequiredParam], ARequiredValue) = 0 then begin
          Exit;
        end;
        NextToken;
      end;
    end;
  end;
end;

{ looking for META tag and charset parameter value }
{}
function TSimpleHTMLParser.MetaParam(AMetaType, AMetaTypeValue, AMetaParam: String): String;
var
  R: TStringList;
  tk, tkn, Vl: String;
  i: Integer;
begin
  { extract META tag keeps AMetaType=AMetaTypeValue parameter }
  R:= TagParams('META', AMetaType, AMetaTypeValue);
  { extract CONTENT parameter value }
  Vl:= R.Values['CONTENT']; { META tag keeps values in CONTENT parameter }
  R.Free;
  { vl looks like: CONTENT=text/html; charset=windows-1251 }
  { now get token, separated by ";", compare with AMetaParam }
  i:= 1;
  tk:= '';
  repeat
    tk:= GetToken(i, ';', vl);
    if tk = '' then Break;
    tkn:= GetToken(1, '=', tk); { parameter name  }
    tk:= GetToken(2, '=', tk);  { parameter value }
    DeleteLeadTerminateSpaceStr(tkn);
    DeleteLeadTerminateSpaceStr(tk);
    if CompareText(tkn, AMetaParam) = 0 then Break;
    Inc(i);
  until False;
  MetaParam:= tk;
end;

{ create word list }
function TSimpleHTMLParser.WordList: TStringList;
begin
  Result:= TStringList.Create;
  Result.Duplicates:= dupIgnore;
  Result.Sorted:= True;
  { from start }
  FSourcePtr:= PChar(FText);
  NextToken;
  { while not #0 }
  case FCaseSensitive of
  csNone:while FToken <> etEnd do begin
      case FToken of
      etSymbol: begin
        Result.Add(FTokenString);
      end;
      end;
      NextToken;
    end;
  csUppercase:while FToken <> etEnd do begin
      case FToken of
      etSymbol: begin
        Result.Add(ANSIUpperCase(FTokenString));
      end;
      end;
      NextToken;
    end;
  csLowercase:while FToken <> etEnd do begin
      case FToken of
      etSymbol: begin
        Result.Add(ANSILowerCase(FTokenString));
      end;
      end;
      NextToken;
    end;
  end;
end;

function StripQuotes(S: String): String;
var
  i, j, L: Integer;
begin
  i:= 1;
  L:= Length(S);
  while (i <= L) and (S[i] in [#9..#32,'''', '"']) do Inc(i);
  j:= i;
  while (j <= L) and (not(S[j] in [#9..#32,'''', '"'])) do Inc(j);
  if (j > i)
  then Result:= Copy(S, i, j - i)
  else Result:= S;
end;

{ create related sorted url list }
function TSimpleHTMLParser.UrlList(ARoot: String): TStringList;  { extract related HREF http links }
begin
  Result:= TStringList.Create;
  Result.Duplicates:= dupIgnore;
  Result.Sorted:= True;
  AddUrlList(ARoot, Result);
end;

{ create related sorted url list }
function TSimpleHTMLParser.UrlNameList(): TStringList;  { extract related HREF http links }
begin
  Result:= TStringList.Create;
  Result.Duplicates:= dupIgnore;
  Result.Sorted:= True;
  AddUrlNameList(Result);
end;

{ create related sorted url list }
function TSimpleHTMLParser.MailList: TStringList;  { extract related HREF=mailto http links }
begin
  Result:= TStringList.Create;
  Result.Duplicates:= dupIgnore;
  Result.Sorted:= True;
  AddMailList(Result);
end;

const
  INVALIDFNCHARS     = [#32, '@', '!'..',', ';'..'?', '['..'^', '{'..'}'];
  INVALIDFN_CGICHARS = [' '..'$','&'..'*',',',';'..'<','>','@','['..'^','{'..'}'];

{ просто провер€ет на наличие правильных символов }
function Valid_Uri(AUri: String): Boolean;
var
  i: Integer;
begin
  for i:= 1 to Length(AUri) do begin
    if (AUri[i] in INVALIDFN_CGICHARS) then begin
      Result:= False;
      Exit;
    end;
  end;
  Result:= True;
end;

const
  INVALIDEMAILCHARS = [#32, '!'..',', ';'..'?', '['..'^', '{'..'}'];

{ просто провер€ет на наличие правильных символов }
function Valid_EMail(AUri: String): Boolean;
var
  i: Integer;
begin
  for i:= 1 to Length(AUri) do begin
    if (AUri[i] in INVALIDEMAILCHARS) then begin
      Result:= False;
      Exit;
    end;
  end;
  Result:= True;
end;

function SearchFileName(var from: Integer; const S: String; var R: String): Boolean;
var
  i, L: Integer;
  Token: String;
  signFound,
  hasExtension: Boolean;
begin
  Result:= False;
  R:= '';
  L:= Length(S);
  if (from >= L)
  then Exit;
  signFound:= False;
  hasExtension:= False;
  Token:= '';
  for i:= from to L do begin
    case S[i] of
    #0..#32, '<', '>', ';', ',': begin
        { добавить файлы только с точкой и после знака = }
        if signFound and HasExtension and Valid_Uri(Token) then begin
          R:= Token;
          Result:= True;
          from:= i;
          Exit;
        end;
        { сбросить все }
        signFound:= False;
        hasExtension:= False;
        Token:= '';
      end;
    '''', '"': begin
        { кавычки отбрасываем }
      end;
    '=': begin
        signFound:= signFound or (S[i] = '=');
        Token:= '';
      end;
    '.': begin
        { дл€ имени файла точка, дл€ email @ }
        hasExtension:= True;
        Token:= Token + S[i];
      end;
    else begin
        Token:= Token + S[i];
      end;
    end; { case }
  end; { for }
  { последний. ћожно было бы поставить сторож - пробел в конце.. }
  if signFound and HasExtension and Valid_Uri(Token) then begin
    R:= Token;
    Result:= True;
    from:= L;
  end;
end;

function SearchEMail(var from: Integer; const S: String; var R: String): Boolean;
var
  i, L: Integer;
  Token: String;
  signFound,
  hasAT: Boolean;
begin
  Result:= False;
  R:= '';
  L:= Length(S);
  if (from >= L)
  then Exit;
  signFound:= False;
  hasAt:= False;
  Token:= '';
  for i:= from to L do begin
    case S[i] of
    #0..#32, '<', '>', ';', ',': begin
        { добавить файлы только с точкой и после знака = }
        if signFound and HasAt and Valid_EMail(Token) then begin
          R:= Token;
          Result:= True;
          from:= i;
          Exit;
        end;
        { сбросить все }
        signFound:= False;
        hasAt:= False;
        Token:= '';
      end;
    '''', '"': begin
        { кавычки отбрасываем }
      end;
    '=': begin
        signFound:= signFound or (S[i] = '=');
        Token:= '';
      end;
    '@': begin
        { дл€ имени файла точка, дл€ email @ }
        hasAt:= True;
        Token:= Token + S[i];
      end;
    else begin
        Token:= Token + S[i];
      end;
    end; { case }
  end; { for }
  { последний. ћожно было бы поставить сторож - пробел в конце.. }
  if signFound and HasAt and Valid_EMail(Token) then begin
    R:= Token;
    Result:= True;
    from:= L;
  end;
end;

function NormalizeDots(AUrl: String): String;
var
  p, p0: Integer;
begin
  { заменить '..' }
  repeat
    p:= Pos('../', AUrl);
    if p = 0
    then Break;
    { Ќазад 1 }
    p0:= PosBackFrom(p, '/', AUrl);
    { назад 2 }
    p0:= PosBackFrom(p0, '/', AUrl);
    Delete(AUrl, p0 + 1, p - p0 + 2);
  until false;
  { заменить '.' }
  while ReplaceStr(AUrl, False, './', '') do;
  Result:= AUrl;
end;

function MkAbsoluteURLFromRelative(ARoot, AUrl: String): String;
var
  ipprotocol, iphost, IPaddress, ipfn: String;
  ipport, L: Integer;
  user, pass, bookmark: String;
begin
  if not util1.ParseUrl(ARoot, ipprotocol, user, pass, iphost, IPaddress, ipfn, bookmark, ipport, 'file', 80) then begin
    Exit;
  end;
  L:= Length(AUrl);
  if L >= 1 then begin
    if AUrl[1] = '/' then begin
      { абсолютный }
      Result:= NormalizeDots('http://'+iphost+AUrl);
      Exit;
    end;
    if Pos('http://', AUrl) = 1 then begin
      { полный url }
      Result:= NormalizeDots(AUrl);
      Exit;
    end;
    if Pos('mailto://', AUrl) = 1 then begin
      { полный url }
      Result:= NormalizeDots(AUrl);
      Exit;
    end;
  end;
  { просто соединить }
  L:= Length(ARoot);
  if (L > 0) and (ARoot[L] = '/')
  then Result:= ARoot + AUrl
  else Result:= ARoot + '/' + AUrl;
  Result:= NormalizeDots(Result);
end;

{ extract related HREF http links }
procedure TSimpleHTMLParser.AddUrlList(ARoot: String; R: TStringList);
var
  url, FUpperTokenString: String;
  p: Integer;
begin
  { from start }
  FSourcePtr:= PChar(FText);
  NextToken;
  { while not #0 }
  while FToken <> etEnd do begin
    FUpperTokenString:= ANSIUppercase(FTokenString);
    case FToken of
    etHTMLTag: begin
      p:= 1;
      repeat
        if not SearchFileName(p, FTokenString, url)
        then Break;
        R.Add(MkAbsoluteURLFromRelative(Aroot, url));
      until False;
    end;
    end;
    NextToken;
  end;
end;

{ extract related HREF http links }
procedure TSimpleHTMLParser.AddUrlNameList(R: TStringList);
var
  url: String;
  p: Integer;
  v: String;
begin
  { from start }
  FSourcePtr:= PChar(FText);
  NextToken;
  { while not #0 }
  while FToken <> etEnd do begin
    case FToken of
      etHTMLTag: begin
        if (Length(FTokenString) > 2) and (FTokenString[1] in ['A', 'a']) and (FTokenString[2] = #32) then begin
          p:= 1;
          repeat
            if not SearchFileName(p, FTokenString, url)
            then Break;
            v:= '';
            repeat
              NextToken;
              case FToken of
                etEnd: Break;
                etHTMLTag: if (Length(FTokenString) >= 2) and (FTokenString[2] in ['A', 'a']) and (FTokenString[1] = '/') then Break;
                etSymbol: v:= v + FTokenString + #32;
              end; // case
            until False;

            R.Add(url + '=' + Trim(v));
          until False;
        end;
      end;
    end; // case
    NextToken;
  end;
end;

{ extract related HREF=mailto http links }
procedure TSimpleHTMLParser.AddMailList(R: TStringList);
var
  email, FUpperTokenString: String;
  p: Integer;
begin
  { from start }
  FSourcePtr:= PChar(FText);
  NextToken;
  { while not #0 }
  while FToken <> etEnd do begin
    FUpperTokenString:= ANSIUppercase(FTokenString);
    { mailto tag only }
    case FToken of
    etHTMLTag: begin
      p:= 1;
      repeat
        if not SearchEmail(p, FTokenString, email)
        then Break;
        R.Add(email);
      until False;
    end;
    end;
    NextToken;
  end;
end;

{ extract related HREF http links }
function TSimpleHTMLParser.GetTitle: String;
var
  phase, L: Integer;
  p1, p2: PChar;
begin
  { from start }
  FSourcePtr:= PChar(FText);
  NextToken;
  { while not #0 }
  phase:= 0;
  p1:= Nil; { just for compiler warning disable }
  while FToken <> etEnd do begin
    case FToken of
    etHTMLTag: begin
      if phase >= 1 then begin
        if TokenHTMLTagIs('/TITLE') then begin
          if phase = 1 then begin
            Result:= '';
          end else begin
            { phase 2 }
            p2:= FTokenPtr;
            L:= Integer(p2) - Integer(p1);
            SetLength(Result, L);
            Move(p1^, Result[1], L);
          end;
          Exit;
        end;
      end else begin
        if TokenHTMLTagIs('TITLE') then begin
          phase:= 1;
        end;
      end;
    end;
    else begin
      if phase = 1 then begin
        p1:= FTokenPtr;
        phase:= 2;
      end;
    end;
    end; { case }
    NextToken;
  end;
end;

procedure TSimpleHTMLParser.DoXlat(Axlat: Pointer);
var
  i: Integer;
begin
  if Axlat <> Nil then begin
    for i:= 1 to Length(Ftext) do begin
      FText[i]:= Char(TXLAT(Axlat^)[Byte(FText[i])]);
    end;
  end;
end;

{
      if Pos('FRAME', FUpperTokenString) > 0 then begin
        if GetToken(1, #32, FUpperTokenString) = 'FRAME' then begin
          p:= Pos('SRC', FUpperTokenString);
          url:= StripQuotes(Copy(FTokenString, PosFrom(p, '=', FTokenString) + 1, MaxInt));
          MkAbsoluteURL(Aroot, url);
          R.Add(url);
        end;
      end;

      if (Pos('HREF', FUpperTokenString) > 0) then begin
        if GetToken(1, #32, FUpperTokenString) = 'HREF' then begin
          url:= StripQuotes(Copy(FTokenString, Pos('=', FTokenString) + 1, MaxInt));
          MkAbsoluteURL(Aroot, url);
          R.Add(url);
        end;
      end;

      if Pos('META', FUpperTokenString) > 0 then begin
        p:= Pos('URL', FUpperTokenString);
        if p > 0 then begin
          url:= StripQuotes(Copy(FTokenString, PosFrom(p, '=', FTokenString) + 1, MaxInt));
          MkAbsoluteURL(Aroot, url);
          R.Add(url);
        end;
      end;
}

end.
