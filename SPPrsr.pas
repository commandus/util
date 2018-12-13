unit
  SPPrsr;
(*##*)
(*******************************************************************
*                                                                 *
*   S  Q  L  P  P  R  S  R  Simple sql procedure parser            *
*                                                                 *
*   Copyright (c) 1999-1999, A.Ivanov. All rights reserved.        *
*   Based on TStoredProcParser class, (c) Borland Intl, 1996      *
*                                                                  *
*   Conditional defines: USE_BDE|USE_IB|USE_NCOCI                 *
*   [EXECUTE|PROCEDURE]NAME(PARAMETER [IN|OUT][DEFAULT VALUE],..)  *
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
  Windows, Messages, Classes,
{$IFDEF USE_BDE}
  Db, DBTables,
{$ENDIF}
{$IFDEF USE_NCOCI}
  Db, NCOci, NCOciWrapper, NCOciDB,
{$ENDIF}
  SysUtils;

type
{$IFDEF USE_IB}
  Interbase Express no this require
{$ENDIF}
{$IFDEF USE_NCOCI}
  TEStoredProc = TOCIStoredProc;
{$ENDIF}
{$IFDEF USE_BDE}
  TEStoredProc = TStoredProc;
{$ENDIF}
  TToken = (etEnd, etCOLON, etOPAREN, etCPAREN, etVAR, etSymbol);

  TStoredProcParser = class
  private
    FText: string;
    FSourcePtr: PChar;
    FTokenPtr: PChar;
    FTokenString: string;
    FToken: TToken;
    procedure NextToken;
    function TokenIs(const S: string): Boolean;
    procedure SetText(const NewString: String);
  public
    constructor Create;
    { create stringlist with objects (lo word- parameter type index, hi- length of type of parameter)  }
    function ParNames(var R: TStrings): String;
    { set parameters to TStoredProc component }
    function SetStoredProc(AStoredProc: TEStoredProc): String;
  published
    property Text: String read FText write SetText;
  end;

implementation

uses
  util1, cpcoll;

const
  FEmptyString: String = '';
  WORDCHARS   = [#0..#255] - [#0..'@', '['..'`', '{'..#127] + ['.']+['_', '$', '%','0'..'9'];
  DELIMITERS1 = [#1..';', '='..'@', '['..'`', '{'..#127] - ['_', '$', '%','0'..'9'];

constructor TStoredProcParser.Create;
begin
  SetText(FEmptyString);
end;

procedure TStoredProcParser.SetText(const NewString: String);
begin
  FText:= NewString;
  FSourcePtr:= PChar(FText);
  NextToken;
end;

procedure TStoredProcParser.NextToken;
var
  P, TokenStart: PChar;
begin
  FTokenString := '';
  P := FSourcePtr;
  while (P^ <> #0) and (P^ <= #32)
  do Inc(P);
  FTokenPtr := P;
  case P^ of
  ',':begin
    Inc(P);
    // TokenStart:= P;
    FToken:= etCOLON;
  end;
  '(':begin
    Inc(P);
    // TokenStart:= P;
    FToken:= etOPAREN;
  end;
  ')':begin
    Inc(P);
    // TokenStart:= P;
    FToken:= etCPAREN;
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

function TStoredProcParser.TokenIs(const S: string): Boolean;
begin
  Result:= (FToken = etSymbol) and (AnsiPos(AnsiUppercase(S), AnsiUppercase(FTokenString)) = 1)
  and (Length(S) = Length(FTokenString));
end;

{ remove all the HTML tags and only display the text }
function TStoredProcParser.ParNames(var R: TStrings): String;
var
  stProcP, stParP, stTypeLen: Byte;
  ParFlags: LongRec;
  ParType, ParName: String;
begin
  R:= TStringList.Create;
  { from start }
  FSourcePtr:= PChar(FText);
  NextToken;
  stProcP:= 0; // ready for 'procedure', 'execute' or name of procedure
  stParP:= 0; // ?
  stTypeLen:= 0; // ?
  while FToken <> etEnd do begin
    case FToken of
    etOPAREN: begin // if name parsed allready..
                case stProcP of
                1:begin
                    stProcP:= 2;  // list of parameters
                    ParFlags.Lo:= 0;
                    ParFlags.Hi:= 0;
                    ParName:= '';
                    ParType:= '';
                    stParP:= 0;  // start of parameter
                    stTypeLen:= 0;
                   end;
                 2:begin
                     case stParP of
                     2: stTypeLen:= 1; // start of type length
                     else Break;
                     end;
                   end;
                else Break;
                end;
              end;
    etCPAREN: begin
        case stProcP of
        2 :begin // list of parameter started
             case stParP of
             0, 2: begin
                     case stTypeLen of
                     0: begin
                       if ParFlags.Lo = 0  // default IN
                       then ParFlags.Lo:= 1;
                       R.AddObject(ParName + '=' + ParType, Pointer(ParFlags));
                       stProcP:= 3    // finish parse procedure
                       end;
                     1: begin
                       stTypeLen:= 0;
                     end;
                     else Break;
                     end;
                end;
             else Break;
             end;
           end
        else Break;
        end;
      end;
    etCOLON: begin
        if stProcP = 2 then begin // list of parameter started
          if not (stParP in [0,2])
          then Break;
          if ParFlags.Lo = 0  // default IN
          then ParFlags.Lo:= 1;

          R.AddObject(ParName + '=' + ParType, Pointer(ParFlags)); // add parsed paramter
          ParFlags.Lo:= 0;       // get ready to new one
          ParFlags.Hi:= 0;
          ParName:= '';
          ParType:= '';
          stParP:= 0;     // parameter finished, start another
          stTypeLen:= 0;
        end else Break;
      end;
    etSymbol: begin
      case stProcP of
      0:begin
          if not (TokenIs('PROCEDURE') or TokenIs('EXECUTE')) then begin
            Result:= FTokenString;
            stProcP:= 1;        // procedure name
          end;
        end;
      2:begin
          case stParP of
          0: begin
               if TokenIs('IN') or TokenIs('OUT') then begin
                 Break;
               end;
               ParName:= FTokenString;
               stParP:= 1;  // go to parameter type
             end;
          1: begin
               if TokenIs('IN') or TokenIs('OUT') then begin
                 if TokenIs('IN')
                 then ParFlags.Lo:= ParFlags.Lo or 1;
                 if TokenIs('OUT')
                 then ParFlags.Lo:= ParFlags.Lo or 2;
               end else begin
                 stParP:= 2; // type of parameter specified, ready to cparen or ccolon
                 ParType:= FTokenString;
               end;
             end;
          2: begin
               if stTypeLen = 1 then begin
                 ParFlags.Hi:= StrToIntDef(FTokenString, 0);
               end else begin
                 if TokenIs('DEFAULT') then begin
                   stParP:= 3;  // ready for 'default' clause
                 end;
               end;
             end;
          3: begin
               stParP:= 2;  // just skip it
             end;
          end; { case }
        end;
      else
        begin
          Break;
        end;
      end;
    end;
    end;
    NextToken;
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

function TStoredProcParser.SetStoredProc(AStoredProc: TEStoredProc): String;
var
  sl: TStrings;
  i: Integer;
  pn, s: String;
  ft: TFieldType;
  pt: TParamType;
begin
  Result:= ParNames(sl);
  AStoredProc.StoredProcName:= Result;
  AStoredProc.Active:= False;
  AStoredProc.Params.Clear;
  for i:= 0 to sl.Count - 1 do begin
    { parameter name }
    pn:= sl.Names[i];
    { parameter type }
    case Integer(LongRec(sl.Objects[i]).lo) of
    1: pt:= ptInput;
    2: pt:= ptOutput;
    3: pt:= ptInputOutput;
    4: pt:= ptResult;
    else pt:= ptUnknown;
    end;
    { data type }
    ft:= ftUnknown;
    s:= UpperCase(sl.Values[sl.Names[i]]);
    { CHAR NCHAR VARCHAR2 VARCHAR NVARCHAR2 CLOB NCLOB LONG NUMBER DATE BLOB BFILE RAW LONG RAW}
    if (s = 'VARCHAR') or (s = 'VARCHAR2') or (s = 'NVARCHAR2') or (s = 'CHAR') or (s = 'NCHAR')
    then ft:= ftString;
    if (s = 'BOOLEAN')
    then ft:= ftBoolean;

    if (s = 'LONG') or (s = 'INTEGER') or (s = 'NUMBER')
    then ft:= ftInteger;
    if (s = 'DATE') or (s = 'TIME')
    then ft:= ftDateTime;
    if (s = 'RAW') or (s = 'BLOB') or (s = 'CLOB') or (s = 'NCLOB')
    then ft:= ftBlob;
    AStoredProc.Params.CreateParam(ft, pn, pt);
  end;
  sl.Free;
end;

end.
