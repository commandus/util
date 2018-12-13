unit
  utilxml;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes,
  Provider, DbClient, DsIntf,
  xmlXForm, xmlDOM, xmlutil,
  util1;

function GetDataAsXml(AProvider: TCustomProvider; const ATransDom: IDOMDocument;
  const AEncoding: String): IDOMDocument;

implementation

procedure Transform(TransNode, SrcNode, DestNode: IDOMNode;
    Count: Integer; InFromList, InDestList, InIdStrList, InValueList,
    InOptionalList, InDateFormatList, InDateFormatTypeList,
    InMapValuesList: TStringList;
    ADirectionToCds: Boolean);
var
  I, J: Integer;
  From, Dest: string;
  Value, AttrName: string;
  IdStr, RepeatName: string;
  Optional, Map_Values: string;
  DefaultValue, DateFormat, DateFormatType: string;
  More: Boolean;
  RepeatDestNode, AttrNode, TmpNode: IDOMNode;
  FromList, DestList, IdStrList, ValueList, OptionalList: TStringList;
  DateFormatList, DateFormatTypeList, MapValuesList: TStringList;
begin
  if TransNode.NodeName = mx_TranslateEach then
  begin
    AttrNode := TransNode.attributes.getNamedItem(mx_FROM);
    if AttrNode <> nil then From := AttrNode.nodeValue else From := '';
    AttrNode := TransNode.attributes.getNamedItem(mx_DEST);
    if AttrNode <> nil then Dest := AttrNode.nodeValue else Dest := '';
    AttrNode := TransNode.attributes.getNamedItem(mx_ID);
    if AttrNode <> nil then IdStr := AttrNode.nodeValue else IdStr := '';
    SrcNode := SelectNode(SrcNode, From);
    if SrcNode <> nil then
    begin
      RepeatName := SrcNode.nodeName;
      DestNode := SelectCreateNode(DestNode, Dest, AttrName);
    end
    else
    begin
      RepeatName := '';
      DestNode := SelectCreateNode(DestNode, Dest, AttrName);
      if (DestNode <> nil) and (DestNode.parentNode <> nil) then
      begin
        TmpNode := DestNode;
        DestNode := DestNode.parentNode;
        DestNode.removeChild(TmpNode);
      end;
    end;
    if (SrcNode <> nil) and (DestNode <> nil) then
    begin
      More := True;
      RepeatDestNode := DestNode.cloneNode(True);
    end
    else
    begin
      More := False;
      RepeatDestNode := nil;
    end;

    FromList := TStringList.Create;
    DestList := TStringList.Create;
    IdStrList := TStringList.Create;
    ValueList := TStringList.Create;
    OptionalList := TStringList.Create;
    DateFormatList := TStringList.Create;
    DateFormatTypeList := TStringList.Create;
    MapValuesList := TStringList.Create;
    try
      while More do
      begin

        for I := 0 to TransNode.childNodes.length-1 do
          TransForm(TransNode.childNodes.item[i], SrcNode, DestNode,
             I, FromList, DestList, IdStrList, ValueList, OptionalList,
             DateFormatList, DateFormatTypeList, MapValuesList, ADirectionToCds);


        while More do
        begin
          SrcNode := SrcNode.nextSibling;
          if SrcNode = nil then
            More := False
          else
          begin
            if SrcNode.nodeName = RepeatName then
            begin //found next\
              DestNode := SelectCreateSibling(DestNode, RepeatDestNode);
              if DestNode = nil then More := False;
              Break;
            end;
          end;
        end;
      end; //while More

    finally
      FromList.Free;
      DestList.Free;
      IdStrList.Free;
      ValueList.Free ;
      OptionalList.Free;
      DateFormatList.Free;
      DateFormatTypeList.Free;
      MapValuesList.Free;
    end;
  end // TransNode.NodeName = mx_TranslateEach
  else if TransNode.NodeName = mx_Translate then //Field-translation
  begin
    if (InFromList = nil) or (Count >= InFromList.Count) then
    begin
      From := ''; Dest := ''; IdStr := ''; Value := ''; Optional := '';
      DateFormat := ''; DateFormatType  := ''; Map_Values := '';
      for J := 0 to TransNode.attributes.length-1 do
      begin
        TmpNode := TransNode.attributes.item[J];
        if TmpNode.NodeName = mx_FROM then
          From := TmpNode.nodeValue
        else if TmpNode.NodeName = mx_DEST then
          Dest := TmpNode.nodeValue
        else if TmpNode.NodeName = mx_VALUE then
          Value := TmpNode.nodeValue
        else if TmpNode.NodeName = mx_OPTIONAL then
          Optional := TmpNode.nodeValue
        else if TmpNode.NodeName = mx_ID then
          Idstr := TmpNode.nodeValue
        else if TmpNode.NodeName = mx_DEFAULT then
          DefaultValue := TmpNode.nodeValue
        else if (TmpNode.NodeName = mx_DATETIMEFORMAT) or
           (TmpNode.NodeName = mx_DATEFORMAT) or
           (TmpNode.NodeName = mx_TIMEFORMAT) then
        begin
          DateFormat := TmpNode.NodeValue;
          DateFormatType := TmpNode.NodeName;
        end
        else if TmpNode.NodeName = mx_MAPVALUES then
           Map_Values := TmpNode.NodeValue;
      end; // for

      if InFromList <> nil then
      begin
        InFromList.Add(From);
        InDestList.Add(Dest);
        InIdStrList.Add(IdStr);
        InValueList.Add(Value);
        InOptionalList.Add(Optional);
        InDateFormatList.Add(DateFormat);
        InDateFormatTypeList.Add(DateFormatType);
        InMapValuesList.Add(Map_Values);
      end;
    end // if (InFromList = nil) ...
    else
    begin
      From := InFromList[Count];
      Dest := InDestList[Count];
      IdStr := InIdStrList[Count];
      Value := InValueList[Count];
      Optional := InOptionalList[Count];
      DateFormat := InDateFormatList[Count];
      DateFormatType := InDateFormatTypeList[Count];
      Map_Values := InMapValuesList[Count];
    end;

    SrcNode := SelectNode(SrcNode, From);
    if SrcNode <> nil then
    begin
      if Value = '' then
      begin
        if SrcNode.nodeType = ELEMENT_NODE then
          Value := (SrcNode as IDOMNodeEx).Text
        else
          Value := SrcNode.nodeValue;
      end;
    end;

    if Value = '' then
      if DefaultValue <> '' then
        Value := DefaultValue;

    if Value <> '' then
    begin
      if Map_Values <> '' then
        Value := MapValues(Map_Values, Value);
      if Value = '' then
        if DefaultValue <> '' then
          Value := DefaultValue;
      if DateFormatType <> '' then
        Value := MapDateTime(DateFormatType, DateFormat, Value, ADirectionToCds);
      if (Optional = '') or (Value <> '') then
        PutValue(DestNode, Dest, Value);
    end
    else if Optional = '' then // = '1'
      PutValue(DestNode, Dest, '');
    if (Optional <> '') and (Value = '') and
       (Dest <> '') and (Dest[1] <> '@') then
    begin
      TmpNode := SelectNode(DestNode, Dest);
      if TmpNode <> nil then
        TmpNode.parentNode.removeChild(TmpNode);
    end;
  end; // else if TransNode.NodeName = mx_Translate
end;

function DoTransform(const XMLSrc, XMLExtract, XMLOut: IDOMDocument): IDOMDocument;
var
  TransRoot: IDOMNode;
  SrcRoot, DestRoot, DestRootClone, Node, TransformNode: IDOMNode;
  I: Integer;
  cdata_skeleton: string;
  Skeleton: IDOMDocument;
  Direction: string;

  DirectionToCds: Boolean;
  EncodingTrans,
  Encoding: String;
begin
  Result:= nil;
  Encoding := GetEncoding(XMLSrc);
  TransRoot := XMLExtract.documentElement;
  SrcRoot := XMLSrc.documentElement;
  if XMLOut <> nil then
    DestRootClone:= XMLOut.documentElement
  else begin
    TransformNode := SelectNode(TransRoot, mx_Root + '\'+ mx_Transform);
    EncodingTrans:= GetAttribute(TransformNode, mx_DataEncoding);
    Direction := GetAttribute(TransformNode, mx_Direction);
    DirectionToCds:= (Direction = mx_ToCds);
    DestRoot := SelectNode(TransRoot,mx_Root+'\'+mx_Skeleton);
    if DestRoot.ChildNodes.item[0].nodeType = ELEMENT_NODE then
      DestRootClone := DestRoot.CloneNode(True)
    else if DestRoot.ChildNodes.item[0].nodeType = CDATA_SECTION_NODE then
    begin
      cdata_skeleton := DestRoot.ChildNodes.item[0].nodeValue;
      Skeleton := LoadDocFromString(cdata_skeleton);
      DestRootClone := Skeleton.documentElement;
    end;
  end;

  Node := SelectNode(TransRoot, mx_Root + '\' + mx_Transform);
  if Node <> nil then
    for I := 0 to Node.childNodes.length - 1 do
      Transform(Node.childNodes.item[I], SrcRoot, DestRootClone,
      0, nil, nil, nil, nil, nil, nil, nil, nil, DirectionToCds);

  if XmlOut <> nil then
  begin
    Result:= XMLOut;
  end
  else
  begin
    if Skeleton = nil then
      Result:= (DestRootClone.childNodes.item[0] as IDOMDocument)
    else
    begin
      Result:= Skeleton;
    end;
  end;
end;

function GetDataAsXml(AProvider: TCustomProvider; const ATransDom: IDOMDocument;
  const AEncoding: String): IDOMDocument;
var
  RecsOut: Integer;
  Params, OwnerData, VarPacket: OleVariant;
  XmlData: string;
  Options: TGetRecordOptions;
  DataCache: TClientDataSet;
  PacketDOM: IDOMDocument;
  EncodingTrans: String;

  TransRoot,
  TransformNode: IDOMNode;
  p, bp, l: Integer;
begin
  DataCache:= TClientDataSet.Create(Nil);
  DataCache.AppServer:= TLocalAppServer.Create(AProvider);

  if DataCache.Params.Count > 0
  then Params:= PackageParams(DataCache.Params);
  Options:=  [grMetaData, grXML];
  VarPacket:= DataCache.AppServer.AS_GetRecords(DataCache.ProviderName, -1,
    RecsOut, Byte(Options), '', Params, OwnerData);
  XmlData:= VariantArrayToString(VarPacket);
  if Length(AEncoding) > 0
  then Insert(' encoding="'+AEncoding+'"', xmlData, Pos(' stand', xmlData));
  p:= 1;
  bp:= 1;
  l:= Length(XmlData);
  repeat
    XmlData[bp]:= GetWEBStyleCharFromString(XmlData, p);
    Inc(bp);
  until p > L;
  SetLength(XmlData, bp -1);
  PacketDOM:= LoadDocFromString(xmlData);

  Result:= DoTransform(PacketDOM, ATransDom, Nil);
  // GetAttribute(Selectnode(PacketDOM.documentelement, 'ROWDATA\ROW'), 'Name')
  TransRoot:= ATransDom.documentElement;
  TransformNode:= SelectNode(TransRoot, mx_Root + '\'+ mx_Transform);
  EncodingTrans:= GetAttribute(TransformNode, mx_DataEncoding);
  if (Result <> Nil) and (Result <> nil) and (EncodingTrans <> '') then begin
    SetEncoding(Result, EncodingTrans, True);
  end;

  DataCache.Free;
end;

end.
