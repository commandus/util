unit htmlprod;
(*##*)
(*******************************************************************
*                                                                 *
*   H  T  M  L  P  R  O  D   TPageProducer replacement             *
*                                                                 *
*   Copyright (c) 1999, 2000 A.Ivanov. All rights reserved.        *
*   Based on Delphi 4 TPageProducer component                     *
*   Conditional defines:                                           *
*                                                                 *
*   Last Revision: May 19 1999                                     *
*   Last fix     :                                                *
*   Lines        :                                                 *
*   History      : see CHANGES.TXT file                           *
*   Printed      : ---                                             *
*                                                                 *
********************************************************************)
(*##*)
interface
uses
  SysUtils, Classes;

const
  PREFIXLEN = 16;

type
  TTag = (tgCustom, tgLink, tgImage, tgTable, tgImageMap, tgObject, tgEmbed);

  THTMLTagEvent = procedure (Sender: TObject; Tag: TTag; const TagString: string;
    TagParams: TStrings; var ReplaceText: string) of object;

  TECustomPageProducer = class(TComponent)
  private
    FStripParamQuotes: Boolean;
    FTag1Char: Char;
    FTag2Chars: String[PREFIXLEN-1];
    FHTMLFile: TFileName;
    FHTMLDoc: String;           // TPageProducer.FHTMLDoc: TStrings;
    FContentRequestCount: Integer;
    FOldValues: TStrings;
    FEnableCollectOldValues: Boolean;
    procedure SetHTMLFile(const Value: TFileName);
    procedure SetHTMLDoc(Value: String);
    procedure SetTagPrefix(Value: String);
    function GetTagPrefix: String;
    procedure SetEnableCollectOldValues(AValue: Boolean);
    function GetOldValue(AInd: String): String;
    procedure SetOldValue(AInd: String; const AValue: String);
  protected
    property StripParamQuotes: Boolean read FStripParamQuotes write FStripParamQuotes default True;
    function HandleTag(const TagString: string; TagParams: TStrings): string; virtual;
    property HTMLDoc: String read FHTMLDoc write SetHTMLDoc;
    property HTMLFile: TFileName read FHTMLFile write SetHTMLFile;
    property ContentRequestCount: Integer read FContentRequestCount;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Content: string;
    function ContentFromStream(Stream: TStream): string;
    function ContentFromString(const S: string): string;
    property TagPrefix: String read GetTagPrefix write SetTagPrefix;
    property EnableCollectOldValues: Boolean read FEnableCollectOldValues write SetEnableCollectOldValues;
    property OldValue[ind: String]: String read GetOldValue write SetOldValue;
    procedure ClearOldValues;
  end;

  TEPageProducer = class(TECustomPageProducer)
  private
    FOnHTMLTag: htmlProd.THTMLTagEvent;
  protected
    function HandleTag(const TagString: string; TagParams: TStrings): string; override;
    procedure DoTagEvent(Tag: TTag; const TagString: string; TagParams: TStrings;
      var ReplaceText: string); dynamic;
  published
    property HTMLDoc;
    property HTMLFile;
    property OnHTMLTag: htmlProd.THTMLTagEvent read FOnHTMLTag write FOnHTMLTag;
  end;

implementation

uses
  Consts, Windows, WebConst, CopyPrsr;

const
  DEFAULT1CHAR = '#';
  SPACE = #32;
{ TCopyParser }
  ParseBufSize = 4096;

function HTTPDecode(const AStr: String): String;
var
  Sp, Rp, Cp: PChar;
  S: String;
begin
  SetLength(Result, Length(AStr));
  Sp := PChar(AStr);
  Rp := PChar(Result);
  Cp := Sp;
  try
    while Sp^ <> #0 do
    begin
      case Sp^ of
        '+': Rp^ := ' ';
        '%': begin
               // Look for an escaped % (%%) or %<hex> encoded character
               Inc(Sp);
               if Sp^ = '%' then
                 Rp^ := '%'
               else
               begin
                 Cp := Sp;
                 Inc(Sp);
                 if (Cp^ <> #0) and (Sp^ <> #0) then
                 begin
                   S := '$' + Cp^ + Sp^;
                   Rp^ := Chr(StrToInt(S));
                 end
                 else
                   raise Exception.CreateFmt(sErrorDecodingURLText, [Cp - PChar(AStr)]);
               end;
             end;
      else
        Rp^ := Sp^;
      end;
      Inc(Rp);
      Inc(Sp);
    end;
  except
    on E:EConvertError do
      raise EConvertError.CreateFmt(sInvalidURLEncodedChar,
        ['%' + Cp^ + Sp^, Cp - PChar(AStr)])
  end;
  SetLength(Result, Rp - PChar(Result));
end;


function HTTPEncode(const AStr: String): String;
const
  NoConversion = ['A'..'Z','a'..'z','*','@','.','_','-',
                  '0'..'9','$','!','''','(',')'];
var
  Sp, Rp: PChar;
begin
  SetLength(Result, Length(AStr) * 3);
  Sp := PChar(AStr);
  Rp := PChar(Result);
  while Sp^ <> #0 do
  begin
    if Sp^ in NoConversion then
      Rp^ := Sp^
    else
      if Sp^ = ' ' then
        Rp^ := '+'
      else
      begin
        FormatBuf(Rp^, 3, '%%%.2x', 6, [Ord(Sp^)]);
        Inc(Rp,2);
      end;
    Inc(Rp);
    Inc(Sp);
  end;
  SetLength(Result, Rp - PChar(Result));
end;

const
// These strings are NOT to be resourced

  Months: array[1..12] of string = (
    'Jan', 'Feb', 'Mar', 'Apr',
    'May', 'Jun', 'Jul', 'Aug',
    'Sep', 'Oct', 'Nov', 'Dec');
  DaysOfWeek: array[1..7] of string = (
    'Sun', 'Mon', 'Tue', 'Wed',
    'Thu', 'Fri', 'Sat');

function ParseDate(const DateStr: string): TDateTime;
var
  Month, Day, Year, Hour, Minute, Sec: Integer;
  Parser: TParser;
  StringStream: TStringStream;

  function GetMonth: Boolean;
  begin
    if Month < 13 then
    begin
      Result := False;
      Exit;
    end;
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
  Month := 13;
  StringStream := TStringStream.Create(DateStr);
  try
    Parser := TParser.Create(StringStream);
    with Parser do
    try
      Month := TokenInt;
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

procedure ExtractHeaderFields(Separators, WhiteSpace: TSysCharSet; Content: PChar;
  Strings: TStrings; Decode: Boolean; StripQuotes: Boolean = False);
var
  Head, Tail: PChar;
  EOS, InQuote, LeadQuote: Boolean;
  QuoteChar: Char;

  function DoStripQuotes(const S: string): string;
  var
    I: Integer;
  begin
    Result := S;
    if StripQuotes then
      for I := Length(Result) downto 1 do
        if Result[I] in ['''', '"'] then
          Delete(Result, I, 1);
  end;

begin
  if (Content = nil) or (Content^ = #0) then Exit;
  Tail := Content;
  QuoteChar := #0;
  repeat
    while Tail^ in WhiteSpace + [#13, #10] do Inc(Tail);
    Head := Tail;
    InQuote := False;
    LeadQuote := False;
    while True do
    begin
      while (InQuote and not (Tail^ in [#0, #13, #10, '"'])) or
        not (Tail^ in Separators + [#0, #13, #10, '"']) do Inc(Tail);
      if Tail^ = '"' then
      begin
        if (QuoteChar <> #0) and (QuoteChar = Tail^) then
          QuoteChar := #0
        else
        begin
          LeadQuote := Head = Tail;
          QuoteChar := Tail^;
          if LeadQuote then Inc(Head);
        end;
        InQuote := QuoteChar <> #0;
        if InQuote then
          Inc(Tail)
        else Break;
      end else Break;
    end;
    if not LeadQuote and (Tail^ <> #0) and (Tail^ = '"') then
      Inc(Tail);
    EOS := Tail^ = #0;
    Tail^ := #0;
    if Head^ <> #0 then
      if Decode then
        Strings.Add(DoStripQuotes(HTTPDecode(Head)))
      else Strings.Add(DoStripQuotes(Head));
    Inc(Tail);
  until EOS;
end;

procedure ExtractHTTPFields(Separators, WhiteSpace: TSysCharSet; Content: PChar;
  Strings: TStrings; StripQuotes: Boolean = False);
begin
  ExtractHeaderFields(Separators, WhiteSpace, Content, Strings, True, StripQuotes);
end;

{ TECustomPageProducer }


constructor TECustomPageProducer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FContentRequestCount:= 0;
  FOldValues:= Nil;
  FEnableCollectOldValues:= False;

  FTag1Char:= DEFAULT1CHAR;
  RPR;
  FHTMLDoc:= ''; // TStringList.Create;
  FHTMLFile:= '';
  FStripParamQuotes:= True;
end;

procedure TECustomPageProducer.SetEnableCollectOldValues(AValue: Boolean);
begin
  if FEnableCollectOldValues then begin
    if not AValue then begin
      FOldValues.Free; // off
      FEnableCollectOldValues:= False;
    end;
  end else begin
    if AValue then begin
      FOldValues:= TStringList.Create;
      FEnableCollectOldValues:= True;
    end;
  end;
end;

function TECustomPageProducer.GetOldValue(AInd: String): String;
begin
  if FEnableCollectOldValues
  then Result:= FOldValues.Values[AInd]
  else Result:= '';
end;

procedure TECustomPageProducer.SetOldValue(AInd: String; const AValue: String);
begin
  if FEnableCollectOldValues
  then FOldValues.Values[AInd]:= AValue;
end;

procedure TECustomPageProducer.ClearOldValues;
begin
  if FEnableCollectOldValues
  then FOldValues.Clear;
end;

procedure TECustomPageProducer.SetTagPrefix(Value: String);
begin
  if Length(Value) = 0 then begin
    FTag1Char:= DEFAULT1CHAR;
    FTag2Chars:= '';
  end else begin
    FTag1Char:= Value[1];
    FTag2Chars:= Copy(Value, 2, PREFIXLEN-1);
  end;
end;

function TECustomPageProducer.GetTagPrefix: String;
begin
  Result:= FTag1Char + FTag2Chars;
end;

destructor TECustomPageProducer.Destroy;
begin
  FHTMLDoc:= ''; // FHTMLDoc.Free;
  if FEnableCollectOldValues
  then FOldValues.Free;
  inherited Destroy;
end;

function TECustomPageProducer.Content: string;
var
  InStream: TStream;
begin
  Result:= '';
  if FHTMLFile <> '' then begin
    InStream:= TFileStream.Create(FHTMLFile, fmOpenRead + fmShareDenyWrite);
    try
      Result:= ContentFromStream(InStream);
    finally
      InStream.Free;
    end;
  end else begin
    Result:= ContentFromString(FHTMLDoc);
  end;
  Inc(FContentRequestCount);
end;

function TECustomPageProducer.ContentFromStream(Stream: TStream): string;
var
  Parser: TCopyParser;
  OutStream: TStringStream;
  ParamStr, ReplaceStr, TokenStr: string;
  ParamList: TStringList;
begin
  OutStream := TStringStream.Create('');
  try
    Parser := TCopyParser.Create(Stream, OutStream);
    with Parser do
    try
      while True do
      begin
        while not (Token in [toEof, '<']) do
        begin
          CopyTokenToOutput;
          SkipToken(True);
        end;
        if Token = toEOF then Break;
        if Token = '<' then
        begin
          if SkipToken(False) = '#' then begin
            SkipToken(False);
            TokenStr := TokenString;
            ParamStr := TrimLeft(TrimRight(SkipToToken('>')));
            ParamList := TStringList.Create;
            try
              ExtractHTTPFields([' '], [' '], PChar(ParamStr), ParamList, FStripParamQuotes);
              ReplaceStr := HandleTag(TokenStr, ParamList);
              OutStream.WriteString(ReplaceStr);
            finally
              ParamList.Free;
            end;
            SkipToken(True);
          end else
          begin
            OutStream.WriteString('<');
            CopyTokenToOutput;
            SkipToken(True);
          end;
        end;
      end;
    finally
      Parser.Free;
    end;
    Result := OutStream.DataString;
  finally
    OutStream.Free;
  end;
end;

function TECustomPageProducer.ContentFromString(const S: string): string;
var
  ParamStr, ReplaceStr, TokenStr: string;
  ParamList: TStringList;
  p, Len, st: Integer;
begin
  p:= 1;
  Len:= Length(S);
  Result:= '';
  while True do begin
    { looking for first tag symbol "<" }
    while (p <= Len) and (s[p] <> '<') do begin
      Result:= Result + s[p];
      Inc(p);
    end;
    Inc(p);
    if p > Len then Break; // all was copied
    // while (p < Len) and (s[p] <= SPACE) do Inc(p);
    if s[p] = FTag1Char then begin
      Inc(p);
      // skip spaces after <#..
      while (p<=Len) and (s[p]<=SPACE) and (s[p]<>'>') do Inc(p);
      st:= p;
      // search the end of the first token
      while (p<=Len) and (s[p]>SPACE) and (s[p]<>'>') do Inc(p);
      TokenStr:= Copy(S, st, p - st);
      // search the end tag: '>'
      st:= p;
      while (p<=Len) and (s[p]<>'>') do Inc(p);
      ParamStr:= TrimLeft(TrimRight(Copy(S, st, p-st)));
      ParamList:= TStringList.Create;
      try
        ExtractHTTPFields([' '], [' '], PChar(ParamStr), ParamList, True);
        ReplaceStr:= HandleTag(TokenStr, ParamList);
        Result:= Result + ReplaceStr;
      finally
        ParamList.Free;
      end;
      Inc(p);
    end else begin
      Result:= Result + '<';
    end;
  end;
end;

function TECustomPageProducer.HandleTag(const TagString: string; TagParams: TStrings): string;
begin
  Result:= Format('<#%s>', [TagString]);
end;

procedure TECustomPageProducer.SetHTMLFile(const Value: TFileName);
begin
  if CompareText(FHTMLFile, Value) <> 0 then begin
    FContentRequestCount:= 0;
    FHTMLDoc:= '';
    FHTMLFile:= Value;
  end;
end;

procedure TECustomPageProducer.SetHTMLDoc(Value: String);
begin
  FContentRequestCount:= 0;
  FHTMLDoc:= Value;
  FHTMLFile:= '';
end;

{ TEPageProducer }

var
  TagSymbols: array[TTag] of string =
    ('', 'LINK', 'IMAGE', 'TABLE', 'IMAGEMAP', 'OBJECT', 'EMBED');

function TEPageProducer.HandleTag(const TagString: string; TagParams: TStrings): string;
var
  Tag: TTag;
begin
  Tag := High(TTag);
  while Tag >= Low(TTag) do begin
    if (Tag = tgCustom) or (CompareText(TagSymbols[Tag], TagString) = 0) then Break;
    Dec(Tag);
  end;
  Result := '';
  DoTagEvent(Tag, TagString, TagParams, Result);
end;

procedure TEPageProducer.DoTagEvent(Tag: TTag; const TagString: string;
  TagParams: TStrings; var ReplaceText: string);
begin
  if Assigned(FOnHTMLTag)
  then FOnHTMLTag(Self, Tag, TagString, TagParams, ReplaceText);
end;

end.
