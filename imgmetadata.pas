unit
  imgmetadata;
(*##*)
(*******************************************************************************
*                                                                             *
*   I  M  G  M  E  T  A  D  A  T  A                                            *
*   Helper functions for GDI+ 2                                               *
*   Copyright (c) 2006-2007, Andrei Ivanov, RealThinComponents                 *
*   Part of GDI+ 2 Delphi wrapper                                             *
*   Conditional defines:                                                       *
*                                                                             *
*   Revisions    : Aug 11 2006                                                 *
*   Last revision: Sep 01 2007                                                *
*   Lines        : 189                                                         *
*   History      :                                                            *
*   Printed      : ---                                                         *
*                                                                             *
********************************************************************************)
(*##*)

interface
uses
  SysUtils, Windows, ActiveX, Classes;

const
  IMG_PREFIX = 'image/';
  DEF_ENCODER = IMG_PREFIX + 'jpeg';

function GetImageEncoder(const AURL: String; const ADefaultEncoder: String = DEF_ENCODER): String;

function GetImageEncoderClsId(const AURL: String; const ADefaultEncoder: String = DEF_ENCODER): ActiveX.TClsId;

function SetImgTagValue(const AUrl: WideString; const ATag: Word; const AValue: String;
  AOutput: TStream): Boolean;

// Get tag value AProperty from the AUrl image resource
function GetImgTagValue(const AUrl: WideString; AProperty: Integer): String;

// Get all tag values from the AUrl image resource
// Set TObject to tag number (Word)
function GetImgTagValues(const AUrl: WideString): TStrings;

function RemoveImgProperty(const AUrl: WideString; AProperty: Integer;
  AOutput: TStream): Boolean;

// create thumbnail image with specified Width and Height
// save created image into stream (can be Nil- anre memory stream 'll be created)
// set property to the string value specified
function CreateJpegThumbTagValue(const AUrl: String; AProperty: Integer; AValue: String;
  AThumbWidth, AThumbHeight: Cardinal; var AStrm: TStream): Boolean;

implementation
uses
  util1, UrlMon, GDIPl2;

const
  DEF_PTARTIST = 'commandus.com';
  DEF_PTSOFTWAREUSED = 'makecall';
  DEF_PTEQUIPMAKE = 'uri2dec';

function GetImageEncoder(const AURL: String; const ADefaultEncoder: String = DEF_ENCODER): String;
begin
  Result:= util1.MimeByExt(ExtractFileExt(AURL));
  if (Length(Result) = 0) or (Pos(IMG_PREFIX, Result) <> 1)
    then Result:= ADefaultEncoder;
end;

function GetImageEncoderClsId(const AURL: String; const ADefaultEncoder: String = DEF_ENCODER): ActiveX.TClsId;
var
  hr: HRESULT;
  fmt: WideString;
begin
  fmt:= GetImageEncoder(AUrl, ADefaultEncoder);
  hr:= GDIpl2.GetEncoderClsid(PWideChar(fmt), Result);
end;

// Get tag value AProperty from the AUrl image resource
function GetImgTagValue(const AUrl: WideString; AProperty: Integer): String;
var
  hr: HRESULT;
  img: Integer;
  Clsid: ActiveX.TClsId;
  istrm: IStream;
  gdiloaded: Boolean;
begin
  Result:= '';
  gdiloaded:= GDIpl2.GDIPlusActive; // if other functions initiate GDI+, use it and DO NOT release GDI+
  if not gdiloaded
  then GDIpl2.InitGDIPlus;
  Clsid:= GetImageEncoderClsId(AUrl);
  // istrm:= TStreamAdapter.Create(strm, soReference) as IStream;
  hr:= URLOpenBlockingStream(Nil, PChar(AUrl), istrm, 0, Nil);
  if (hr = S_OK) then begin
    hr:= GDIpl2.GdipLoadImageFromStream(istrm, img);
  end else begin
    hr:= GDIpl2.GdipLoadImageFromFile(PWideChar(AUrl), img);
  end;
  if (hr = S_OK) then begin
    Result:= GetImgTextProperty(img, Aproperty);
    GdipDisposeImage(img);
  end;

  if not gdiloaded
  then GDIpl2.DoneGDIPlus;
end;

// Get all tag values from the AUrl image resource
// Set TObject to tag number (Word)
function GetImgTagValues(const AUrl: WideString): TStrings;
var
  hr: HRESULT;
  img: Integer;
  Clsid: ActiveX.TClsId;
  istrm: IStream;
  gdiloaded: Boolean;
  totalBufferSize, numProperties: Cardinal;
  allItems: Pointer;
  itemno, id: Integer;
begin
  Result:= TStringList.Create;
  gdiloaded:= GDIpl2.GDIPlusActive; // if other functions initiate GDI+, use it and DO NOT release GDI+
  if not gdiloaded
  then GDIpl2.InitGDIPlus;
  Clsid:= GetImageEncoderClsId(AUrl);
  // istrm:= TStreamAdapter.Create(strm, soReference) as IStream;
  hr:= URLOpenBlockingStream(Nil, PChar(AUrl), istrm, 0, Nil);
  if (hr = S_OK) then begin
    hr:= GDIpl2.GdipLoadImageFromStream(istrm, img);
  end else begin
    hr:= GDIpl2.GdipLoadImageFromFile(PWideChar(AUrl), img);
  end;
  if (hr = S_OK) then begin
    // gets total size, in bytes. Also gets the number of property items stored in this Image object.
    hr:= GDIPl2.GdipGetPropertySize(img, totalBufferSize, numProperties);
    if (hr = S_OK) then begin
      GetMem(allItems, totalBufferSize);
      if (hr = S_OK) then begin
        hr:= GDIPl2.GdipGetAllPropertyItems(img, totalBufferSize, numProperties, allItems^);
        if (hr = S_OK) then begin
          for itemno:= 0 to numProperties - 1 do begin
            //
            id:= TPropertyItems(allitems^)[itemno].id;
            Result.AddObject(PropertyValueAsString(TPropertyItems(allitems^)[itemno]), TObject(id));
          end;
        end;
      end;
      FreeMem(allItems);
    end;
    GdipDisposeImage(img);
  end;
  if not gdiloaded
  then GDIpl2.DoneGDIPlus;
end;


function RemoveImgProperty(const AUrl: WideString; AProperty: Integer;
  AOutput: TStream): Boolean;
var
  hr: HRESULT;
  img: Integer;
  istrm: IStream;
  gdiloaded: Boolean;
  Clsid: ActiveX.TClsId;
begin
  Result:= False;
  gdiloaded:= GDIpl2.GDIPlusActive; // if other functions initiate GDI+, use it and DO NOT release GDI+
  if not gdiloaded
  then GDIpl2.InitGDIPlus;
  Clsid:= GetImageEncoderClsId(AUrl);
  hr:= URLOpenBlockingStream(Nil, PChar(AUrl), istrm, 0, Nil);
  if (hr = S_OK) then begin
    hr:= GDIpl2.GdipLoadImageFromStream(istrm, img);
  end else begin
    hr:= GDIpl2.GdipLoadImageFromFile(PWideChar(AUrl), img);
  end;
  if (hr = S_OK) then begin
    Result:= RemoveImgPropertyItem(img, Aproperty);
    if Result then begin
      AOutput.Position:= 0;
      hr:= SaveImageToStream(AOutput, img, Clsid, Nil);
      Result:= hr = S_OK;
    end;
    GdipDisposeImage(img);
  end;

  if not gdiloaded
  then GDIpl2.DoneGDIPlus;
end;

// create thumbnail image with specified Width and Height
// save created image into stream (can be Nil- anre memory stream 'll be created)
// set property to the string value specified
//  do not destroy stream - already destroyed
function CreateJpegThumbTagValue(const AUrl: String; AProperty: Integer; AValue: String;
  AThumbWidth, AThumbHeight: Cardinal; var AStrm: TStream): Boolean;
var
  hr: HRESULT;
  img,
  ThumbImage: Integer;
  Clsid: ActiveX.TClsId;
  istrm: IStream;
  gdiloaded: Boolean;
begin
  Result:= False;
  gdiloaded:= GDIpl2.GDIPlusActive; // if other functions initiate GDI+, use it and DO NOT release GDI+
  if not gdiloaded
  then GDIpl2.InitGDIPlus;

  Clsid:= GetImageEncoderClsId(AUrl);

  hr:= URLOpenBlockingStream(Nil, PChar(AUrl), istrm, 0, Nil);
  if (hr = S_OK) then begin
    hr:= GDIpl2.GdipLoadImageFromStream(istrm, img);
  end else begin
    hr:= GDIpl2.GdipLoadImageFromFile(PWideChar(AUrl), img);
  end;

  if (hr = S_OK) then begin
    hr:= GdipGetImageThumbnail(img, AThumbWidth, AThumbHeight, ThumbImage, Nil, Nil);
    if hr = S_OK then begin
      if SetImgTextProperty(ThumbImage, Aproperty, AValue) then begin
        if not Assigned(AStrm) then begin
          AStrm:= TMemoryStream.Create;
        end else begin
          AStrm.Position:= 0;
        end;
        hr:= GDIpl2.SaveImageToStream(Astrm, ThumbImage, clsid, Nil);
        Result:= hr = S_OK;
      end;
    end;
  end;

    { // get all properties
    hr:= GDIpl2.GdipGetPropertySize(img, size, count);
    if hr = S_OK then begin
      GetMem(b, size);
      hr:= GDIpl2.GdipGetAllPropertyItems(img, size, count, b^);
      if hr = S_OK then begin
        for c:= 0 to count - 1 do with TPropItems(b^)[c] do begin
          if typ = PropertyTagTypeASCII then begin
            ShowMessage(PChar(value));
          end;
        end;
      end;
      FreeMem(b);
    end;
    }

  GdipDisposeImage(ThumbImage);
  GdipDisposeImage(img);
  if not gdiloaded
  then GDIpl2.DoneGDIPlus;
end;

function SetImgTagValue(const AUrl: WideString; const ATag: Word; const AValue: String;
  AOutput: TStream): Boolean;
var
  hr: HRESULT;
  img: Integer;
  Clsid: ActiveX.TClsId;
  istrm: IStream;
  gdiloaded: Boolean;
begin
  Result:= False;
  gdiloaded:= GDIpl2.GDIPlusActive; // if other functions initiate GDI+, use it and DO NOT release GDI+
  if not gdiloaded
  then GDIpl2.InitGDIPlus;

  Clsid:= GetImageEncoderClsId(AUrl);

  // hr:= GDIpl2.LoadImageFromStream(strm, img);
  hr:= URLOpenBlockingStream(Nil, PChar(AUrl), istrm, 0, Nil);
  if (hr = S_OK) then begin
    hr:= GDIpl2.GdipLoadImageFromStream(istrm, img);
  end else begin
    hr:= GDIpl2.GdipLoadImageFromFile(PWideChar(AUrl), img);
  end;

  if (hr = S_OK) then begin
    // write new value
    if SetImgTextProperty(img, ATag, AValue) then begin
      if SetImgTextProperty(img, ptArtist, DEF_PTARTIST) then
        if SetImgTextProperty(img, ptSoftwareUsed, DEF_PTSOFTWAREUSED) then
          if SetImgTextProperty(img, ptEquipMake, DEF_PTEQUIPMAKE) then begin
            Result:= True;
          end;
    end;
  end;

  AOutput.Position:= 0;
  hr:= SaveImageToStream(AOutput, img, Clsid, Nil);

  GdipDisposeImage(img);
  // strm.Free;
  if not gdiloaded
  then GDIpl2.DoneGDIPlus;
end;

end.
