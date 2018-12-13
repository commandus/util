unit Versions;
(*##*)
(*******************************************************************
*                                                                 *
*   V  E  R  S  I  O  N  S   get application version info unit     *
*                                                                 *
*   Copyright (c) 1998, A.Ivanov. All rights reserved.             *
*   get application version info unit                             *
*   Conditional defines:                                           *
*                                                                 *
*   Last Revision: Dec 07 1998                                     *
*   Last fix     : Dec 07 1998                                    *
*   Lines        :                                                 *
*   History      :                                                *
*   Printed      : ---                                             *
*                                                                 *
********************************************************************)
(*##*)

interface

{ Return value of \StringFileInfo\040904E4\.. resource values
  Paramerer AVer:
  CompanyName                FileDescription
  FileVersion                InternalName
  LegalCopyright             LegalTrademarks
  OriginalFilename           ProductName
  ProductVersion             Comments
  for example, \StringFileInfo\041904E4\FileVersion

}
function GetVersionInfo(ALang, AWhat: String): String;

implementation

uses
  SysUtils, Windows;

const
  DEFVERLANG = '040904E4';    { usa language charset id }
  VERPTH = '\StringFileInfo\';

function GetVersionInfo(ALang, AWhat: String): String;
var
  VersionSize,
  Dummy: DWord;
  VersionBuf, Qry: String;
  FN: array[0..MAX_PATH - 1] of Char;
  r: Pointer;
begin
  Result:= '';
  GetModuleFileName(hInstance, FN, SizeOf(FN));
  VersionSize:= GetFileVersionInfoSize(FN, Dummy);
  if VersionSize > 0 then begin
    SetLength(VersionBuf, VersionSize);
    if not GetFileVersionInfo(FN, Dummy, VersionSize, PChar(VersionBuf))
    then Exit;
    if ALang = ''
    then ALang:= DEFVERLANG; { default usa language charset id }
    Qry:= VERPTH + ALang + '\'+AWhat;
    if not VerQueryValue(PChar(VersionBuf), PChar(Qry), r, VersionSize)
    then Exit;
    Result:= PChar(r);
  end;
end;

end.
