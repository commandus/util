unit filecoll;
(*##*)
(*******************************************************************
*                                                                 *
*   F  I  L  E  C  O  L  L      collection of file routines        *
*                                                                 *
*   Copyright (c) 1999, Andrey Ivanov.                             *
*                                                                 *
*   <## file.ext>CRLF (на отдельной стр)                           *
*   last entry terminate <##>                                     *
*   for more information see                                       *
*                                                                 *
*   Conditional defines:                                           *
*                                                                 *
*   Last Revision: Mar 25 1999                                     *
*   Last fix     : Jul 28 1999                                    *
*   Lines        : 329                                             *
*   History      :                                                *
*   Printed      : ---                                             *
*                                                                 *
********************************************************************)
(*##*)

interface

uses
  Classes, SysUtils;

const
  filecollNONE = 0;
  filecollEH   = 1;

type
  TFileColl = class(TObject)
  private
    FKind: Byte;
    FFileCollection: String;
    FFileCollectionString: String; { cache }
    FLastPos: Integer;
    function Format2File: String;  { форматирует FFileCollectionString дл€ записи в файл }
    function GetContent(Index: String): String;
    procedure SetContent(Index: String; AContent: String);
    function GetNameByIndex(Index: Integer): String;
    function GetContentByIndex(Index: Integer): String;
    { добавл€ет один файл (AIsFileName: True - читает файл, False- заполн€ет из AFileContent) }
    function AddFile1(AFileName, AFileContent: String; AIsFileName: Boolean): Integer;
    procedure SetCollectionFile(ACollectionFileName: String);
  public
    FFilePtr: TStringList;         { keeps filename=offset,length }
    property CollectionFile: String read FFileCollection write SetCollectionFile;
    constructor Create;
    destructor  Destroy; override;
    property Content[AFileName: String]: String read GetContent write SetContent;
    procedure StoreChanges;
    { если ''- добавл€ет NewFileXX.txt }
    function NewFile(ANewFileName, AContent: String): Boolean;
    { добавл€ет один или несколько файлов }
    { можно использовать маски файлов *.* или указать каталог (вложенные не будут обработаны) }
    function AddFile(AFileNameOrMask: String): Integer;
    function DeleteFile(AFileName: String): Integer;
    function RenameFile(AFileName, ANewFileName: String): Boolean;
    { читает файл коллекции }
  end;

implementation

uses
  Util1;

{ return last position (to insert) }
function FileCollPosSize(AFileCollString: String; AFilePtr: TStringList): Integer;
var
  i, p1, p2, p3, oldpos, count, L: Integer;
  S: String;
begin
  AFilePtr.Sorted:= False;
  AFilePtr.Clear;
  oldpos:= 0;
  count:= 0;
  Result:= 0;
  L:= Length(AFileCollString);
  { искать метки <## file=FILENAME [folder=PATHNAME]..>}
  i:= 0;
  while (i < L) do begin
    Inc(i);
    { найти тег }
    if (AFileCollString[i] <> '<')
    then Continue;
    { до конца файла достаточно места - <##> (+3 bytes minimum)}
    if (i + 2 >= L) or (AFileCollString[i+1] <> '#') or (AFileCollString[i+2] <> '#')
    then Continue;
    Result:= i;
    Inc(i, 2);
    p1:= 0;
    { искать закрывающий первый непробельный символ }
    repeat
      Inc(i);    { теперь i указывает видимо на пробел за '<##' }
      if AFileCollString[i] >= #32 then begin
        p1:= i;
        Break;
      end;
    until i >= L;
    { искать закрывающий '>' }
    p2:= 0;
    repeat
      Inc(i);
      if AFileCollString[i] = '>' then begin
        p2:= i;
        Break;
      end;
    until i >= L;
    p3:= 0;
    { искать конец строки }
    repeat
      Inc(i);
      if AFileCollString[i] = #10 then begin
        p3:= i;
        Break;
      end;
    until i >= L;
    { теперь p3 указывает на #10 после закрывающей '>' либо 0 }
    { если нет конца строки, то по концу > или концу файла }
    if (p3 = 0)
    then p3:= p2;
    Inc(p3);
    { теперь p1 указывает на начало тега, p2 указывает на начало тела }
    Inc(p2);
    Inc(i);
    { добавл€ем }
    S:= Copy(AFileCollString, p1, p2-p1-1);
    DeleteLeadTerminateDoubledSpaceStr(S);
    { завершающий пустой, не добавл€ть }
    if S = '' then begin
    end else begin
      AFilePtr.Add(S+ '='+IntToStr(p3)+',');
    end;
    if count > 0 then begin
      AFilePtr[count-1]:= AFilePtr[count-1] + IntToStr(p1 - oldpos);
    end;
    oldpos:= p3 + 5;
    Inc(count);
  end;
  { сортируем список }
  AFilePtr.Duplicates:= dupIgnore; { одинаковые имена не допускаютс€ }
  AFilePtr.Sorted:= True;
end;

{ если ''- добавл€ет NewFileXX.txt }
function TFileColl.NewFile(ANewFileName, AContent: String): Boolean;
begin
  if ANewFileName = '' then begin
    ANewFileName:= 'NewFile' + DateTimeToStr(Now) + '.txt';
  end;
  AddFile1(ANewFileName, AContent, False);
  Result:= True;
end;

{ добавл€ет в коллекцию файл. —читаем, что список отсортирован
  ≈сли существовал уже, будет изменен
  ¬озвращает позицию дл€ вставки
}
function TFileColl.AddFile(AFileNameOrMask: String): Integer;
var
  SearchRec: TSearchRec;
begin
  if IsFileMask(AFileNameOrMask) then begin
    if FindFirst(AFileNameOrMask, faAnyFile, SearchRec)=0 then begin
      AddFile1(SearchRec.Name, '', True);
      while FindNext(SearchRec) = 0 do begin
        AddFile1(SearchRec.Name, '', True);
      end;
    end;
    // ??!
    // FindClose(SearchRec.FindHandle);
    FindClose(SearchRec);
  end else begin
    AddFile1(AFileNameOrMask, '', True);
  end;
  AddFile:= FLastPos;
end;

{ добавл€ет один файл (AIsFileName: True - читает файл, False- заполн€ет из AFileContent) }
function TFileColl.AddFile1(AFileName, AFileContent: String; AIsFileName: Boolean): Integer;
var
  L, L1, L2: Integer;
  S: String;
begin
  { удалить старое }
  DeleteFile(AFileName);
  if AIsFileName then begin
    { загрузить файл }
    S:= LoadString(AFileName);
  end else begin
    S:= AFileContent;
  end;
  L1:= Length(S);
  L2:= Length(AFileName);
  L:= L1 + L2 + 7;
  Insert('<##'#32 + AFileName + '>'#13#10 + S, FFileCollectionString, FLastPos);
  FFilePtr.Add(AFileName + '=' + IntToStr(FLastPos + L2 + 7) + ',' + IntToStr(L1));
  Inc(FLastPos, L);
  Result:= FLastPos;
end;

function TFileColl.DeleteFile(AFileName: String): Integer;
var
  ind: Integer;
begin
  ind:= FFilePtr.IndexOfName(AFileName);
  if ind >= 0 then begin
    FFilePtr.Delete(ind);
  end;
  { сжати€ не производитс€ }
  Result:= FLastPos;
end;

function TFileColl.RenameFile(AFileName, ANewFileName: String): Boolean;
var
  ind, p: Integer;
  S: String;
begin
  Result:= False;
  ind:= FFilePtr.IndexOfName(AFileName);
  if ind < 0 then begin
    Exit;
  end;
  S:= FFilePtr[ind];
  p:= Pos('=', S);
  if p = 0
  then Exit;
  Inc(p);
  FFilePtr.Delete(ind);
  FFilePtr.Add(ANewFileName + '=' + Copy(S, p, MaxInt));
  Result:= True;
end;

procedure TFileColl.SetCollectionFile(ACollectionFileName: String);
begin
  FKind:= filecollnone;
  FFileCollection:= ACollectionFileName;
  if ACollectionFileName = ''
  then Exit;
  try
    FFileCollectionString:= LoadString(ACollectionFileName);
    FLastPos:= FileCollPosSize(FFileCollectionString, FFilePtr);
  except
    Exit;
  end;
  if FLastPos <= 0
  then Exit;
  FKind:= filecollEH;
end;

constructor TFileColl.Create;
begin
  inherited Create;
  FKind:= filecollNONE;
  FFileCollection:= '';
  FFilePtr:= TStringList.Create;
end;

destructor  TFileColl.Destroy;
begin
  FFilePtr.Free;
  inherited Destroy;
end;

function TFileColl.GetNameByIndex(Index: Integer): String;
begin
  if Index >= FFilePtr.Count then begin
    Result:= '';
    Exit;
  end;
  { potentially may cause error (length parameter = -1)}
  Result:= Copy(FFilePtr[Index], 1, Pos('=', FFilePtr[Index]) - 1);
end;

function TFileColl.GetContentByIndex(Index: Integer): String;
var
  S: String;
  no, len: Integer;
begin
  if Index >= FFilePtr.Count then begin
    Result:= '';
    Exit;
  end;
  S:= Copy(FFilePtr[Index], Pos('=', FFilePtr[Index]) + 1, MaxInt);
  no:= StrToIntDef(GetToken(1, ',', S), 0);
  len:= StrToIntDef(GetToken(2, ',', S), 0);
  Result:= Copy(FFileCollectionString, no, len);
end;

procedure TFileColl.SetContent(Index: String; AContent: String);
begin
  AddFile1(Index, AContent, False);
end;

{ search index in file collection
  Return file contents ('' if no file exists in collection)
}
function TFileColl.GetContent(Index: String): String;
var
  S: String;
  no, len: Integer;
begin
  S:= FFilePtr.Values[Index];
  no:= StrToIntDef(GetToken(1, ',', S), 0);
  len:= StrToIntDef(GetToken(2, ',', S), 0);
  Result:= Copy(FFileCollectionString, no, len);
end;

function TFileColl.Format2File: String;
var
  i: Integer;
begin
  Result:= '';
  for i:= 0 to FFilePtr.Count - 1 do begin
    Result:= Result + '<##'#32 + GetNameByIndex(i) + '>'#13#10 + GetContentByIndex(i) +#13#10;
  end;
  Result:= Result + '<##>';
end;

procedure TFileColl.StoreChanges;
begin
  StoreString(FFileCollection, Format2File);
end;

end.
