unit
  htmlTidy;

interface
uses
  Windows, Classes;

function HTML2XHTML(const ASrc: WideString): WideString;

implementation
uses
  util1, customxml, xmlParse, xHTML;

function HTML2XHTML(const ASrc: WideString): WideString;
var
  TempXMLCollection: TxmlElementCollection;
  p: Integer;
begin
  Result:= '';
  TempXMLCollection:= TxmlElementCollection.Create(THTMBody, Nil, wciOne);
  TempXMLCollection.Clear1;
  xmlParse.xmlCompileText('<body>' + ASrc + '</body>', Nil, Nil, Nil, TempXMLCollection.Items[0], THTMContainer);
  Result:= TempXMLCollection.Items[0].CreateText(0, []);
  p:= Pos('<body>', Result);
  if p > 0 then Delete(Result, p, 6);
  p:= util1.PosBack('</body>', Result);
  if p > 0 then Delete(Result, p, 7);
  TempXMLCollection.Free;
end;

end.
