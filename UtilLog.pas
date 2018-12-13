unit UtilLog;
(*##*)
(*******************************************************************
*                                                                 *
*   U  T  I  L  L  O  G      Ensen's logging routines for Delphi   *
*                                                                 *
*   Copyright (c) 1998, 2001 Andrei Ivanov. All rights reserved.   *
*   Based on util1 unit, vgax (part of VGAX graphic BP7 library)  *
*   Conditional defines: WIN32 VER80_32 VER90                      *
*                                                                 *
*   Last Revision: Sep 10 2001                                     *
*   Last fix     :                                                *
*   Lines        :                                                 *
*   History      :                                                *
*   Printed      : ---                                             *
*                                                                 *
********************************************************************)
(*##*)

interface

{ error logging
}
procedure ErrLog(const S: ShortString);
{ or FN <> ''
  or LogProc <> Nil
}
function  OpenErrLog(const FN: ShortString; LProc: LogProc): Boolean;
procedure CloseErrLog;


implementation

{ error logging
}
const
  { 0- none, 1- to file, 2- thru proc }
  log_NONE = 0;
  log_FILE = 1;
  log_PROC = 2;
  Logging: Byte= log_NONE;

var
  ERPT: Text;
  LoggingProc: LogProc;

procedure ErrLog(const S: ShortString);
begin
  if Logging = log_FILE
  then Writeln(ERPT, S);
  if Logging = log_PROC
  then LoggingProc(S);
end;

{ or FN <> ''
  or LogProc <> Nil
}
function  OpenErrLog(const FN: ShortString; LProc: LogProc): Boolean;
begin
  OpenErrLog:= False;
  if logging <> log_NONE
  then CloseErrLog;
  Logging:= log_NONE;
  if FN <> '' then begin
    AssignFile(ERPT, FN);
    Rewrite(ERPT);
    if IOResult = 0
    then Logging:= log_FILE
    else Exit;
  end else begin
    LoggingProc:= LProc;
    Logging:= log_PROC;
  end;
  OpenErrLog:= True;
end;

procedure CloseErrLog;
begin
  if Logging = log_FILE
  then CloseFile(ERPT);
  Logging:= log_None;
end;

end.
 