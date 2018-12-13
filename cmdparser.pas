unit
  cmdparser;
(*##*)
(*******************************************************************************
*                                                                             *
*   c  m  d  p  a  r  s  e  r                                                  *
*                                                                             *
*   Copyright © 1998- 2001, Andrei Ivanov. All rights reserved.                *
*   Simple command line parser                                                *
*   Conditional defines:                                                       *
*                                                                             *
*   Last Revision: Mar 19 1998                                                 *
*   Last fixes   :                                                            *
*   Lines        : 240                                                         *
*   History      : see CHANGES.TXT file                                       *
*   Printed      : ---                                                         *
*                                                                             *
********************************************************************************)
(*##*)

interface

uses
  SysUtils, Classes;

function ParseCommandLine(var Rslt: TStrings; AOptionsHasExt: TSysCharSet): String;
function GetSwitchesCount(const ASwitches: TSysCharSet; opts: TStrings): Integer;
{ extract switch values
}
function ExtractSwitchValue(ASwitches: TSysCharSet; opts: TStrings): String;
{ extract switch values
}
function ExtractSwitchValues(ASwitches: TSysCharSet; opts: TStrings; ARslt: TStrings): Integer;

// to url
procedure Cmd2Request(const ACmdLine: String; AOptionsHasExt, AReqOptions, AParOptions: TSysCharSet; var AReqPath, ACmd: ShortString);

var
  cmdoptions: TStrings;

implementation

function ParseCmd(const AConfig: String; AOptionsHasExt: TSysCharSet; var Rslt: TStrings): String;
var
  S, os: String;
  o: Char;
  filecount, p, i, L: Integer;
  state: Integer;
  Quoted: Boolean;
begin
  if not Assigned(Rslt)
    then Rslt:= TStringList.Create;
  
  Rslt.Clear;
  S:= AConfig;
  state:= 0;
  i:= 1;
  filecount:= 0;
  L:= Length(S);
  // skip program name
  while i <= L do begin
    if S[i] = #32 then begin
      Inc(i);
      Break;
    end;
    Inc(i);
  end;
  Result:= Copy(S, 1, i - 2);

  Quoted:= False;
  // parse
  while i <= L do begin
    o:= S[i];
    case o of
      '''', '"': begin
          Quoted:= not Quoted;
          Inc(i);
        end;
      '-': begin
          case state of
            0: if not Quoted then state:= 1;  // option started
          end;
          Inc(i);
        end;
      #0..#32: begin
          state:= 0;
          Inc(i);
        end;
      else begin
        case state of
          0:begin  // file name or mask
              p:= i;
              if Quoted then begin
                repeat
                  Inc(i);
                until (i > L) or (S[i] in ['''', '"', #0]);
                Quoted:= False;
              end else begin
                repeat
                  if (not Quoted) and (S[i] = '-')
                  then Break;
                  Inc(i);
                until (i > L) or (S[i] in [#0..#32]);
              end;
              os:= Copy(s, p, i - p);
              Inc(filecount);
              Rslt.AddObject('=' + os, TObject(filecount));
            end;
          1: begin  // option
              if S[i] in AOptionsHasExt then begin  // option has next parameter
                //
                // skip spaces if exists
                Inc(i);
                while (i <= L) and (S[i] <= #32) do Inc(i);
                // get option's parameter
                p:= i;
                Quoted:= (i <= L) and (s[i] in ['''', '"']);
                if Quoted then begin
                  Inc(p);
                  repeat
                    Inc(i);
                  until (i > L) or (S[i] in [#0, '''', '"']);
                  os:= Copy(s, p, i - p);
                  Inc(i);
                  Quoted:= False;
                end else begin
                  repeat
                    if (not Quoted) and (S[i] = '-') 
                    then Break;
                    Inc(i);
                  until (i > L) or (S[i] in [#0..#32]);
                  os:= Copy(s, p, i - p);
                end;
                // add option
                Rslt.Add(o + '=' + os);
              end else begin
                // add option
                Rslt.Add(o + '=');
                Inc(i);
              end;
            end;
          else begin
            Inc(i);
          end;
        end; { case state }
        //
      end; { else case }
    end; { case }
  end;
end;

function ParseCommandLine(var Rslt: TStrings; AOptionsHasExt: TSysCharSet): String;
begin
  Result:= ParseCmd(System.CmdLine, AOptionsHasExt, Rslt);
end;

procedure Cmd2Request(const ACmdLine: String; AOptionsHasExt, AReqOptions, AParOptions: TSysCharSet; var AReqPath, ACmd: ShortString);
var
  sl: TStrings;
  i, len: Integer;
  s, vl: String;
begin
  sl:= TStringList.Create;
  s:= ParseCmd(System.CmdLine, AOptionsHasExt, sl);
  ACmd:= '';
  if sl.Count = 0 then begin
    AReqPath:= '';
  end else begin
    for i:= 0 to sl.Count - 1 do begin
      s:= sl.Names[i];
      if (Length(s) > 0) and (s[1] in AReqOptions)
      then begin
        AReqPath:= sl.ValueFromIndex[i]  + #0;
        len:= Length(AReqPath);
        Delete(AReqPath, len, 1); // delete last '&'
      end else if (Length(s) > 0) and (s[1] in AParOptions) then begin
        vl:= sl.ValueFromIndex[i];
        if Length(vl) > 0
        then ACmd:= ACmd + s + '=' + vl + '&';
      end;
    end;
    len:= Length(ACmd);    
    if len > 0 then begin
      ACmd[len]:= #0;
      Delete(ACmd, len, 1); // delete last '&'
    end;
  end;
  sl.Free;
end;

{ calc count of switches
}
function GetSwitchesCount(const ASwitches: TSysCharSet; opts: TStrings): Integer;
var
  i: Integer;
  s: String;
begin
  Result:= 0;
  for i:= 0 to opts.Count - 1 do begin
    s:= opts.Names[i];
    if (Length(s) > 0) and (s[1] in ASwitches)
    then Result:= Result + 1;
  end;
end;

{ extract switch values
}
function ExtractSwitchValue(ASwitches: TSysCharSet; opts: TStrings): String;
var
  i, l: Integer;
  s: String;
begin
  Result:= '';
  for i:= 0 to opts.Count - 1 do begin
    s:= opts.Names[i];
    l:= Length(s);
    if ((l > 0) and (s[1] in ASwitches))
      or ((l = 0) and (ASwitches = [])) then begin
      Result:= Copy(opts[i], l + 2, MaxInt);
    end;
  end;
end;

{ extract switch values
}
function ExtractSwitchValues(ASwitches: TSysCharSet; opts: TStrings; ARslt: TStrings): Integer;
var
  i, l: Integer;
  s: String;
begin
  Result:= 0;
  for i:= 0 to opts.Count - 1 do begin
    s:= opts.Names[i];
    l:= Length(s);
    if ((l > 0) and (s[1] in ASwitches))
      or ((l = 0) and (ASwitches = [])) then begin
      s:= Copy(opts[i], l + 2, MaxInt);
      ARslt.AddObject(s, opts.Objects[i]);
      Result:= Result + 1;
    end;
  end;
end;

end.
