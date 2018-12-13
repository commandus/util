unit utilmenu;
(*##*)
(*******************************************************************
*                                                                 *
*   U  T  I  L  M  E  N  U   Ensen's misc. routines for Delphi     *
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

interface
uses
  Menus;
{ menus
}
function  GetNameFromMenu(MI: TMenuItem): String;
procedure SetNameInMenu(MI: TMenuItem; const Prefix, S: String);

implementation

function  GetNameFromMenu(MI: TMenuItem): String;
begin
  GetNameFromMenu:= Copy(MI.Caption, Pos(':',MI.Caption)+1, 255);
end;

procedure SetNameInMenu(MI: TMenuItem; const Prefix, S: String);
begin
  if Prefix = ''
  then MI.Caption:= Copy(MI.Caption, 1, Pos(':',MI.Caption)) + S
  else MI.Caption:= Prefix + ':' + S;
end;

end.
