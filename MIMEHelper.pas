unit
  MIMEHelper;

interface
uses
  Windows, SysUtils, Registry;

{ get mime description
  example ".jpg" (or "jpg") -> "image/jpeg"
  if file extension not found, return empty string. (or text/plain?!!)
}
function  MimeByExt(ext: String): String;

implementation
{ get mime description
  example ".jpg" (or "jpg") -> "image/jpeg"
  if file extension not found, return empty string. (or text/plain?!!)
}
function  MimeByExt(ext: String): String;
var
  R: TRegistry;
begin
  Result:= '';
  if Length(ext) = 0 then Exit;
  ext:= LowerCase(ExtractFileExt(ext));
  if Length(ext) = 0 then Exit;

  if ext[1] <> '.'
  then ext:= '.' + ext;

  R:= TRegistry.Create;
  try
    R.RootKey:= HKEY_CLASSES_ROOT;
{$IFDEF D4_}
    if R.OpenKeyReadOnly(ext) then begin
{$ELSE}
    if R.OpenKey(ext, False) then begin
{$ENDIF}
      try
        Result:= R.ReadString('Content Type');
      except
      end;
    end;
    R.CloseKey;
  finally
    R.Free;
  end;
end;

end.
