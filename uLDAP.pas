unit
  uLDAP;
(******************************************************************************)
(*                    uLDAP - Higher level classes functions                  *)
(*                            for LDAP client routines                        *)
(*                    Note  - Binary attributes NOT supported                 *)
(******************************************************************************)

interface

uses
  Classes,  SysUtils,
  Linldap; // WinLDAP;  

type
  ELDAP = class(Exception);
  PCharArray = array of PChar;
  PLDAPModArray = array of PLDAPMod;
  PPCharArray = ^PCharArray;
  PPLDAPModArray = ^PLDAPModArray;

  TldapAttribute = class(Tobject)
  private
    fName: String;
    fValues: TStrings;
    function GetValue(I: Integer): String;
    procedure Add(S:String);
    procedure Delete(S:String);
    function getExists(V: String): Boolean;
  public
    constructor Create(Name:String);
    destructor Destroy; override;
    property Name:String read fName;
    property Exists[V:String]:Boolean read getExists;
    property ValueList: TStrings read fValues;
    property Value[I: Integer]:String read GetValue;  default;
  end;

  TldapEntryList = class;

  TldapEntry = class(TObject)
  private
    fdn: String;
    P: array of PLDAPMod;
    fParent: TldapEntry;
    fAttributes: TStringList;
    fChildren: TldapEntryList;
    function GetAttribute(Name: String): TldapAttribute;
    function GetAttributeValue(Name: String; Idx: Integer): String;
    function GetNiceName:String;
    procedure MakeLDAPArray;
    procedure MakeModLDAPArray(le: TldapEntry);
    procedure ApplyChangesInMemory;
    procedure DisposeLDAPArray;
    function GetExists(Name,  Value: String): Boolean;
    function GetLDIF: String;
    procedure SetLDIF(S:String);
    function Equal(Name:String;le:TldapEntry;var SL:TStringList):Boolean;
    function AddsOnly(Name: String; le: TldapEntry; var SL:TStringList): Boolean;
    function DeletesOnly(Name: String; le: TldapEntry; var SL: TStringList): Boolean;
    procedure ClearAll;
  public
    property DN: String read fDN write fDN;
    property AttributeValue[Name:String;Idx: Integer]:String read GetAttributeValue; default;
    property Attribute[Name: String]: TldapAttribute read GetAttribute;
    property Children:TldapEntryList read fChildren;
    property Parent:TldapEntry read fParent;
    property Exists[Name, Value:String]:Boolean read GetExists;
    property NameList: TStringList read fAttributes;
    property NiceName:String read GetNiceName;
    property AsLDIF: String read GetLDIF write SetLDIF;
    constructor Create; overload;
    constructor Create(le:TldapEntry); overload;
    procedure Assign(le: TldapEntry);
    destructor Destroy; override;
    function FreeChild(ee: TldapEntry): Boolean;
    procedure Add(Name, Value:String); overload;
    procedure Add(Name: String; Value: array of String); overload;
    procedure Modify(Name:String;Value:array of String); overload;
    procedure Modify(Name, Value:String); overload;
    procedure Delete(Name:String); overload;
    procedure Delete(Name, Value:String); overload;
  end;

  TldapEntryList = class(TStringList)
    function GetEntry(Idx: Integer):TldapEntry;
    function GetDN(Idx: Integer):String;
    function GetEntryValue(Idx: Integer;Attribute:String;AttributeIndex: Integer):String;
  public
    property DN[Idx: Integer]: String read GetDN;
    property Entry[Idx: Integer]: TldapEntry read GetEntry; default;
    property EntryValue[Idx: Integer; Attribute: String; AttributeIndex: Integer]: String read GetEntryValue;
    function NewEntry(const DN: String): TldapEntry;
    procedure ClearAll;
    destructor Destroy; override;
  end;

  TldapConnection = class(TObject)
  private
    fConn: PLDAP;
    fHost,
    fDN,
    fPWD: String;
    fPort: Integer;
    fBaseDN: String;
    FDefScope: String;
    procedure ldapError(const s: String);
    procedure LDAPCheck(err: LongWord);
  protected
  public
    Name: String; // 2006 May 14
    Tag: Integer; // 2006 May 14
    constructor Create;
    destructor Destroy; override;
    property Host: String read fHost write fHost;
    property Port: Integer read fPort write fPort;
    property BindDN: String read fDN write fDN;
    property BaseDN: String read fBaseDN write fBaseDN;
    property BindPwd: String read fPWD write fPWD;
    property DefaultScope: String read FDefScope write FDefScope;
    function UidToDN(const Uid: String; const Base: String): String;
    procedure Open; overload;
    procedure Open(const Hostname: String; const BindAs: String=''; const BindPassword:String = ''; const PortNumber: Integer = 389); overload;
    procedure ReBind(const BindAs,  BindPassword: String);
    procedure Search(var Results: TldapEntryList; const Base: String; const Search: String='(objectclass=*)'; const Scope:String='sub'); overload;
    procedure Search(var Results: TldapEntryList; const DNs: TldapAttribute); overload;
    procedure Add(const Entry: TldapEntry);
    procedure Fill(var Entry: TldapEntry);
    procedure Modify(const Entry,  NewEntry: TldapEntry);
    procedure Delete(const DN: String); overload;
    procedure Delete(const Entry: TldapEntry); overload;
    procedure Close; virtual;
    function  GetConnected: Boolean;
    procedure SetConnected(AValue: Boolean);
  published
    property Connected: Boolean read GetConnected write SetConnected;
  end;

  TldapDataset = class(TldapConnection)
  private
    FCursor: Integer;
    FldapEntryList: TldapEntryList;
    FScope: String;
    function GetEOF: Boolean;
    function GetField(AAttrIndex: Integer): TldapAttribute;
  protected
    FFilter: TStrings;
  public
    constructor Create(AOwner: TObject);
    destructor Destroy; override;
    procedure Open(); overload;
    procedure Open(const AFilter: String; const AScope:String='sub'); overload; // ='(objectclass=*)
    procedure Close; overload;
    property Scope: String read FScope write FScope;
    property ldapEntryList: TldapEntryList read FldapEntryList;
    property EOF: Boolean read GetEOF;
    property Fields[AttrIndex: Integer]: TldapAttribute read GetField;
    procedure Next;
    procedure First;
    function MoveBy(Distance: Integer): Integer;
    property Cursor: Integer read FCursor;
  published
    property sql: TStrings read FFilter;
  end;

function AttributeName(S:String): String;
function AttributeValue(S:String): String;
function FindDn(const server,  base,  Uid:String):String;
function ValidDn(const server,  dn,  uid,  pwd:String):Boolean;
function RDN(dn: String): String;
function FullDN(rdn,  basedn: String):String;
function Check(const Server: String; const PortNumber: Integer; const Uid,  Password,  BaseDN:String): Boolean;

function SafeUtf8Decode(AString: String): WideString;

  // keep ldap connection environment
type
  TldapConnEnv = class(TPersistent)
  private
  protected
    FEnv: TStrings;
    FldapConn: TldapConnection;
    function GetEnvVar(Index: Integer): String;
    procedure SetEnvVar(Index: Integer; const AValue: String);
  public
    constructor Create(const AHost: String; APort: Integer; const ABindDN, ABindPwd, ABaseDN: String;
        AOpen: Boolean);
    destructor Destroy; override;
    property Connection: TldapConnection read FldapConn;
    property Env: TStrings read FEnv;
    property Caption: String index 0 read GetEnvVar write SetEnvVar;
    property Image: String index 1 read GetEnvVar write SetEnvVar;
    property ClassPerson: String index 2 read GetEnvVar write SetEnvVar;
    property ClassOrganization: String index 3 read GetEnvVar write SetEnvVar;
    property ClassPersonAdd: String index 4 read GetEnvVar write SetEnvVar;
    property ClassOrganizationAdd: String index 5 read GetEnvVar write SetEnvVar;
    property RuleNewPersonDNWhere: String index 6 read GetEnvVar write SetEnvVar;
    property RuleNewPersonName: String index 7 read GetEnvVar write SetEnvVar;
    property RuleNewOrgDNWhere: String index 8 read GetEnvVar write SetEnvVar;
    property RuleNewOrgName: String index 9 read GetEnvVar write SetEnvVar;
    property BaseDN: String index 10 read GetEnvVar write SetEnvVar;
    property Host: String index 11 read GetEnvVar write SetEnvVar;
    property Port: String index 12 read GetEnvVar write SetEnvVar;
    property BindDN: String index 13 read GetEnvVar write SetEnvVar;
    property BindPwd: String index 14 read GetEnvVar write SetEnvVar;
    property SearchAttr: String index 15 read GetEnvVar write SetEnvVar;
    property PersonSearchScope: String index 16 read GetEnvVar write SetEnvVar;
    property RulePersonInRoot: String index 17 read GetEnvVar write SetEnvVar;
    property ShowOUAttrs: String index 18 read GetEnvVar write SetEnvVar;    
  end;

implementation

function SafeUtf8Decode(AString: String): WideString;
begin
  Result:= Utf8Decode(AString);
  if Length(Result) = 0
  then Result:= AString;
end;

function AttributeName(S:String):String;
begin
  if Pos(':', S)>0 then
    Result:= Copy(S,  1,  Pos(':',  S) - 1)
  else
    Result:= '';
end;

function AttributeValue(S:String):String;
begin
  if Pos(':', S)>0 then
    Result:= Copy(S, Pos(':', S)+1, 999)
  else
    Result:= '';
end;

function RDN(dn:String):String;
var
  I: Integer;
begin
  I:= Pos(', ', dn);
  if I>1 then
  begin
    Result:= Trim(Copy(dn, 1, I-1))
  end
  else
    Result:= dn;
end;

function FullDN(rdn, basedn:String):String;
begin
  if Pos(', ', rdn)=0 then
    Result:= rdn+', '+basedn
  else
    Result:= rdn;
end;

{------------------------- TldapAttribute -------------------------}

constructor TldapAttribute.Create(Name:String);
begin
  inherited Create;
  fName:= Name;
  fValues:= TStringList.Create;
  with TStringList(fValues) do begin
    Duplicates:= dupIgnore;
    Sorted:= False;
  end;  
end;

destructor TldapAttribute.Destroy;
begin
  fValues.Free;
  inherited;
end;

function TldapAttribute.getExists(V: String): Boolean;
begin
 Result:= Self.fValues.IndexOf(V) >= 0;
end;

function TldapAttribute.Getvalue(I: Integer):String;
begin
  if (I >= 0) and (I < fValues.Count)
  then Result:= fValues[I]
  else Result:= '';
end;

procedure TldapAttribute.Delete(S:String);
var
  I: Integer;
begin
  I:= fValues.IndexOf(S);
  if I >= 0
  then fValues.Delete(I);
end;

procedure TldapAttribute.Add(S: String);
begin
  fValues.Add(S);
end;

{--------------------------- TldapEntry ---------------------------}

constructor TldapEntry.Create;
begin
 inherited;
 fAttributes:= TStringList.Create;
 fattributes.Duplicates:= dupIgnore;
 fattributes.sorted:= False;
 fChildren:= TldapEntryList.Create;
end;

constructor TldapEntry.Create(le: TldapEntry);
begin
 Create;
 Assign(le);
end;

procedure TldapEntry.Assign(le: TldapEntry);
var
  I: Integer;
  la: TldapAttribute;
  newle: TldapEntry;
begin
  Self.ClearAll;
  fDN:= le.fdn;
  for I:= 0 to le.NameList.count-1 do begin
    la:= TldapAttribute.Create(le.NameList[i]);
    la.ValueList.Assign(le.Attribute[le.NameList[i]].ValueList);
    Self.fAttributes.AddObject(le.NameList[i], la);
  end;
  for I:= 0 to le.fChildren.count-1 do begin
    newle:= TldapEntry.Create(le.Children[I]);
    Self.Children.AddObject(le.Children[I].DN, newle);
    newle.fParent:= Self;
  end;
end;

procedure TldapEntry.ClearAll;
var
  I: Integer;
begin
  fdn:= '';
  Self.DisposeLDAPArray;
  if Assigned(fAttributes) then begin
    for I:= 0 to fAttributes.count-1
    do TldapAttribute(fAttributes.Objects[I]).Free;
    fAttributes.Clear;
  end;
end;

destructor TldapEntry.Destroy;
begin
 ClearAll;
 fChildren.Free;
 fAttributes.Free;
 inherited;
end;

function TldapEntry.GetNiceName: String;
var
  I: Integer;
begin
  Result:= '';
  if Self.Exists['objectclass', 'groupofuniquenames'] then
    Result:= '['+Self.GetAttributeValue('cn', 0)+']'
  else if Self.GetAttributeValue('sn', 0)<>'' then
    Result:= Trim(Self.GetAttributeValue('sn', 0)+',  ' + Self.GetAttributeValue('personaltitle', 0)+' '+Self.GetAttributeValue('givenname', 0))
  else if Self.GetAttributeValue('cn', 0)<>'' then
    Result:= Self.GetAttributeValue('cn', 0)
  else begin
    if Self.AttributeValue['ou', 0]<>'' then begin
      for I:= 0 to Self.Attribute['ou'].ValueList.Count-1 do
        if Pos(uppercase(Self.GetAttributeValue('ou', I)), uppercase(RDN(self.DN)))<>0 then
          Result:= Self.GetAttributeValue('ou', I);
    end;
    if Result='' then
      Result:= RDN(Self.DN);
  end;
end;

procedure TldapEntry.Modify(Name: String; Value:array of String);
var
  I: Integer;
begin
  if uppercase(name)='DN'
  then raise ELDAP.Create('Cannot Modify DN of Entry')
  else begin
    Self.Delete(Name);
    for I:= Low(value) to High(value)
    do Self.Add(Name, value[I]);
  end;
end;

procedure TldapEntry.Modify(Name, Value:String);
begin
  if uppercase(name)='DN'
  then raise ELDAP.Create('Cannot Modify DN of Entry')
  else begin
    Self.Delete(Name);
    Self.Add(Name, value);
  end;
end;

procedure TldapEntry.Add(Name: String; Value: array of String);
var
  I: Integer;
begin
  for I:= Low(value) to High(value)
  do Add(Name, value[I]);
end;

procedure TldapEntry.Add(Name, Value:String);
var
  la: TldapAttribute;
begin
  if UpperCase(name) = 'DN' then
    raise ELDAP.Create('Cannot Modify DN of Entry')
  else begin
    la:= Self.Attribute[Name];
    if not Assigned(la) then begin
      la:= TldapAttribute.Create(Name);
      self.NameList.AddObject(Name, la);
    end;
    la.Add(Value);
  end;
end;

procedure TldapEntry.Delete(Name:String);
var
  la: TldapAttribute;
  Idx: Integer;
begin
  la:= Self.Attribute[Name];
  if Assigned(la) then begin
    idx:= self.NameList.IndexOfObject(la);
    if idx >= 0
    then Self.NameList.Delete(Idx);
    la.Free;
  end;
end;

procedure TldapEntry.Delete(Name, Value:String);
var
  la: TldapAttribute;
begin
  la:= Self.Attribute[Name];
  la.Delete(Value);
  if la.ValueList.Count = 0
  then Self.Delete(Name);
end;

function TldapEntry.GetAttribute(Name: String): TldapAttribute;
var
  I: Integer;
begin
  I:= fAttributes.Indexof(Name);
  if I>=0
  then Result:= TldapAttribute(fAttributes.Objects[I])
  else Result:= Nil;
end;

function TldapEntry.GetAttributeValue(Name: String; Idx: Integer): String;
var
  lda: TldapAttribute;
begin
  lda:= self.GetAttribute(Name);
  if lda = Nil
  then Result:= ''
  else Result:= lda[idx];
end;

procedure TldapEntry.SetLDIF(S: String);
var
  SL: TStringList;
  I, p: Integer;
  N, V: String;
begin
  try
    Self.ClearAll;
    SL:= TStringList.Create;
    SL.Text:= S;
    for I:= 0 to SL.Count - 1 do begin
      p:= Pos(':', SL[I]);
      N:= Copy(SL[I], 1, p - 1);
      V:= Copy(SL[I], p + 1, 32000);
      if (Length(V) > 0) and (V[1] = ':') then begin
        System.Delete(V, 1, 1);
      end;
      if UpperCase(N) = 'DN'
      then Self.FDN:= V
      else Self.Add(N, V);
    end;
  finally
    SL.Free;
  end;
end;

function TldapEntry.GetLDIF: String;
var
  I, A: Integer;
  AVal, isUTF8Sign: String;
begin
  Result:= 'dn:' + Self.DN + ^M^J;
  for I:= 0 to NameList.Count - 1 do begin
    for A:= 0 to Self.Attribute[NameList[I]].ValueList.Count-1 do begin
      if Integer(Attribute[NameList[I]].ValueList.Objects[A]) = LDAP_MOD_DELETE
      then Continue;
      aval:= Utf8Decode(Attribute[NameList[I]].ValueList[A]);
      if Length(aval) <> Length(Attribute[NameList[I]].ValueList[A])
      then isUTF8Sign:= '::'
      else isUTF8Sign:= ':';
      Result:= Result + NameList[I] + isUTF8Sign + Attribute[NameList[I]].ValueList[A] + ^M^J;
    end;
  end;
end;

function TldapEntry.AddsOnly(Name:String;le:TldapEntry;var SL:TStringList):Boolean;
var
  I: Integer;
  a1, a2: TldapAttribute;
begin
  a1:= Self.Attribute[Name];
  a2:= le.Attribute[Name];
  if Assigned(a1) and not Assigned(a2)
  then Result:= False
  else begin
    if not Assigned(a1) and Assigned(a2) then begin
      Result:= True;
      SL.Assign(a2.ValueList);
    end else begin // Adds only - all "a1" are in "a2" - some "a2" not in "a1"
      Result:= True;
      for i:= 0 to a1.ValueList.Count - 1 do begin
        if a2.ValueList.IndexOf(a1.ValueList[I])= -1 then begin
          Result:= False; break;
        end;
      end;
    Result:= Result and (a2.ValueList.Count > a1.ValueList.Count);
    if Result then begin
      for I:= 0 to a2.ValueList.Count-1 do begin
        if a1.ValueList.IndexOf(a2.ValueList[I]) = -1
        then SL.Add(a2.ValueList[I])
      end;
    end;
   end;
   end;
end;

function TldapEntry.DeletesOnly(Name:String;le:TldapEntry;var SL:TStringList):Boolean;
begin
 Result:= le.AddsOnly(Name, Self, SL);
end;

function TldapEntry.Equal(Name:String;le:TldapEntry;var SL:TStringList):Boolean;
var
  I: Integer;
  a1, a2: TldapAttribute;
begin
  a1:= Self.Attribute[Name];
  a2:= le.Attribute[Name];
  if not (Assigned(a1) and Assigned(a2)) then Result:= False
  else if a1.ValueList.Count<>a2.ValueList.Count then Result:= False
  else begin
    Result:= True;
    for I:= 0 to a1.ValueList.Count-1 do
      if a2.ValueList.IndexOf(a1.ValueList[I])=-1 then begin Result:= False; break; end;
    if not Result then
      SL.Assign(a2.ValueList);   
  end;
end;

const
  LDAP_MOD_NOCHANGE       = $0F; // -----

procedure TldapEntry.MakeModLDAPArray(le: TldapEntry);
var
  I, J: Integer;
  full_at_list, done_at:TStringList;
  cmd: Integer;
  cChanges: Integer;

  function IncludeAttribute(at:String):Boolean;
  begin
    Result:= (UpperCase(at) <> 'CREATORSNAME') and (UpperCase(at) <> 'MODIFIERSNAME') and
      (UpperCase(at) <> 'CREATETIMESTAMP') and (UpperCase(at) <> 'MODIFYTIMESTAMP');
  end;

begin
  try
    done_at:= TStringList.Create;
    full_at_list:= TStringList.Create;
    full_at_list.Duplicates:= dupIgnore;
    full_at_list.Sorted:= True;

    DisposeLDAPArray;
    cChanges:= 0;

    for I:= 0 to Namelist.Count-1 do begin
     if IncludeAttribute(NameList[i])
     then full_at_list.Add(NameList[i]);
    end;

    for I:= 0 to le.Namelist.Count-1 do begin
      if IncludeAttribute(le.NameList[i])
      then full_at_list.Add(le.NameList[i]);
    end;

    for I:= 0 to full_at_list.Count-1 do begin
      done_at.Clear;
      if AddsOnly(full_at_list[I], le, done_at)
      then cmd:= LDAP_MOD_ADD
      else if DeletesOnly(full_at_list[I], le, done_at)
        then cmd:= LDAP_MOD_DELETE
        else if not Equal(full_at_list[I], le, done_at)
          then cmd:= LDAP_MOD_REPLACE
          else cmd:= LDAP_MOD_NOCHANGE;
      if Cmd <> LDAP_MOD_NOCHANGE then begin
        Inc(cChanges);
        SetLength(P, cChanges + 1);
        New(P[cChanges - 1]);
        P[cChanges - 1]^.mod_op:= Cmd;
        P[cChanges - 1]^.mod_type:= StrNew(PChar(full_at_list[i]));
        SetLength(P[cChanges-1]^.modv_strvals, done_at.count + 1);
        for j:= 0 to done_at.Count - 1
        do P[cChanges - 1]^.modv_strvals[J]:= StrNew(PChar(done_at[J]));
        P[cChanges - 1]^.modv_strvals[done_at.Count]:= Nil;  // ---
      end;
    end;
  finally
    full_at_list.Free;
    done_at.Free;
  end;
end;

procedure TldapEntry.MakeLDAPArray;
var
  A: TldapAttribute;
  I, J: Integer;
begin
  SetLength(P, NameList.Count + 1);
  for I:= 0 to NameList.Count - 1 do begin
    New(P[I]);
    P[I]^.mod_op:= LDAP_MOD_ADD;
    P[I]^.mod_type:= StrNew(PChar(NameList[I]));
    A:= Attribute[NameList[I]];
    SetLength(P[I]^.modv_strvals, A.ValueList.Count + 1);
    for J:= 0 to A.ValueList.Count - 1
    do P[I]^.modv_strvals[J]:= StrNew(PChar(A.Valuelist[J]));
    P[I]^.modv_strvals[A.ValueList.Count]:= Nil;  // ---
  end;
  P[NameList.Count]:= Nil;  // ---
end;

procedure TldapEntry.ApplyChangesInMemory;
var
  I, J: Integer;
begin
  if Assigned(P) then begin
    for I:= Low(P) to High(P) do begin
      if not Assigned(P[I])
      then Continue;

      if P[I]^.mod_op = LDAP_MOD_REPLACE
      then Self.Delete(P[I]^.mod_type);

      for J:= Low(P[I]^.modv_strvals) to High(P[I]^.modv_strvals) do begin
        if not Assigned(P[I]^.modv_strvals[J])
        then Continue;
        case P[I]^.mod_op of
          LDAP_MOD_ADD: Self.Add(P[I]^.mod_type, P[I]^.modv_strvals[J]);
          LDAP_MOD_DELETE: Self.Delete(P[I]^.mod_type, P[I]^.modv_strvals[J]);
          LDAP_MOD_REPLACE: Self.Add(P[I]^.mod_type, P[I]^.modv_strvals[J]);
        end;
      end;
    end;
  end;
end;

procedure TldapEntry.DisposeLDAPArray;
var
  I, J: Integer;
begin
  if Assigned(P) then begin
    for I:= low(P) to high(P) do
      if Assigned(P[I]) then begin
        StrDispose(P[I]^.mod_type);
        for J:= Low(P[I]^.modv_strvals) to High(P[I]^.modv_strvals) do begin
          if Assigned(P[I]^.modv_strvals[J])
          then StrDispose(P[I]^.modv_strvals[J]);
        end;
        P[I]^.modv_strvals:= Nil;
        Dispose(P[I]);
      end;
      P:= Nil;
    end;
end;

function TldapEntry.GetExists(Name,  Value: String): Boolean;
begin
 if Assigned(Self.Attribute[Name]) then
   Result:= Self.Attribute[Name].Exists[Value]
 else
   Result:= False;
end;

function TldapEntry.FreeChild(ee:TldapEntry):Boolean;
begin
  if Assigned(ee) and (Self.Children.IndexOfObject(ee)>=0) then
  begin
    Children.Delete(Children.IndexofObject(ee));
    ee.Free;
    Result:= True;
  end
  else
    Result:= False;
end;


{ TldapEntryList }

function TldapEntryList.GetDN(Idx: Integer):String;
begin
 if (idx>=0) and (Idx<Self.Count) then
   Result:= self.strings[Idx]
 else
   Result:= '';
end;

function TldapEntryList.GetEntryValue(Idx: Integer;Attribute:String;AttributeIndex: Integer):String;
var
  lde:TldapEntry;
begin
  lde:= Self.GetEntry(Idx);
  Result:= lde.AttributeValue[Attribute, AttributeIndex];
end;

function TldapEntryList.GetEntry(Idx: Integer):TldapEntry;
begin
 if (idx>=0) and (Idx<Self.Count) then
   Result:= TldapEntry(self.objects[Idx])
 else
   Result:= Nil;
end;

function TldapEntryList.NewEntry(const DN:String):TldapEntry;
var
  Idx: Integer;
begin
  Idx:= Self.AddObject(DN, TldapEntry.Create);
  Result:= TldapEntry(Self.Objects[Idx]);
  Result.fdn:= DN;
end;

procedure TldapEntryList.ClearAll;
var
 I: Integer;
begin
 for I:= 0 to Self.count-1 do
   TldapEntry(Self.Objects[I]).Free;
 Self.Clear;  
end;

destructor TldapEntryList.Destroy;
begin
 ClearAll;
 inherited;
end;

{ TldapConnection }

constructor TldapConnection.Create;
begin
  inherited Create;
  fConn:= Nil;
  fHost:= '';
  fDN:= '';
  fPWD:= '';
  fPort:= 389;
  fBaseDN:= '';
  fDefScope:= 'sub';
  Name:= '';
  Tag:= 0;
end;

destructor TldapConnection.Destroy;
begin
  inherited Destroy;
end;

procedure TldapConnection.ldapError(const s: String);
begin
  raise ELDAP.Create(s);
end;

procedure TldapConnection.LDAPCheck(err: LongWord);
begin
  if (err <> LDAP_SUCCESS) then ldapError(Format('Сервер ldap вернул сообщение об ошибке %d: %s', [err, ldap_err2string(err)]));
end;

procedure TldapConnection.Fill(var Entry:TldapEntry);
var
  el:TldapEntryList;
  old_dn: String;
begin
  try
    el:= TldapEntryList.Create;
    old_dn:= Entry.dn;
    Entry.ClearAll;
    Entry.dn:= old_dn;
    Search(el, Entry.dn, '(objectclass=*)', 'base');
    if el.Count = 1
    then Entry.Assign(el[0]);
  finally
    el.Free;
  end;
end;

function TldapConnection.UidToDN(const Uid:String;const Base:String):String;
var
  plmSearch, plmEntry: PLDAPMessage;
begin
  if Assigned(fConn) then begin
    LDAPCheck(ldap_simple_bind_s(fConn,  Nil,  Nil));
    LDAPCheck(ldap_search_s(fconn,  PChar(Base),  LDAP_SCOPE_SUBTREE , PChar('(uid='+uid+')'),  Nil,  0,  @plmSearch));  // --- @
    plmEntry:= ldap_first_entry(fConn,  plmSearch);
    if Assigned(plmEntry) then
      Result:= ldap_get_DN(fConn, plmEntry)
    else
      ldapError('UID not found');
  end
  else
    ldapError('Error Opening Connection to Server');
end;

procedure TldapConnection.Search(var Results: TldapEntryList; const DNs: TldapAttribute);
var
  I: Integer;
  el:TldapEntryList;
begin
  if Assigned(Results) and Assigned(DNs) then
  begin
    try
      el:= TldapEntryList.Create;
      for I:= 0 to DNs.ValueList.Count-1 do
      begin
        try
          el.ClearAll;
          Search(el, DNs.ValueList[I], '(objectclass=*)', 'base');
          if el.Count=1 then
            Results.AddObject(el[0].DN, TldapEntry.Create(el[0]))
        except
        end;
      end;
    finally
      el.ClearAll;
      el.Free;
    end;
  end;
end;

procedure TldapConnection.Search(var Results: TldapEntryList; const Base: String; const Search: String = '(objectclass=*)'; const Scope: String = 'sub');
var
  plmSearch,  plmEntry: PLDAPMessage;
  i, iScope: Integer;
  psEntryDN: PChar;
  CurrentEntry: TldapEntry;
  pszAttr: PChar;
  pbe: PBerElement;
  ppcVals: PPChar;
begin
  if not Assigned(fconn)
  then Exit;
  if Lowercase(scope) = 'base' then iScope:= LDAP_SCOPE_BASE
  else if Lowercase(scope) = 'one' then iScope:= LDAP_SCOPE_ONELEVEL
  else if Lowercase(scope) = 'sub' then iScope:= LDAP_SCOPE_SUBTREE;
  try
    LDAPCheck(ldap_search_s(fconn,  PChar(Base),  iScope , PChar(Search),  Nil,  0,  @plmSearch));  // @
    try
      plmEntry:= ldap_first_entry(fConn,  plmSearch);
      while Assigned(plmEntry) do begin
        try
          try
            psEntryDN:= ldap_get_DN(fConn, plmEntry);
            CurrentEntry:= Results.NewEntry(psEntryDN);
          except
            ldapError('Error Retrieving DN for entry');
          end;
        finally
          ldap_memfree(psEntryDN);
        end;
        pszAttr:= ldap_first_attribute(fConn,  plmEntry,  pbe);
        while Assigned(pszAttr) do begin
          ppcVals:= ldap_get_values(fConn,  plmEntry,  pszAttr);
          if Assigned(ppcVals) then
            try
              i:=  0;
              while Assigned(pchararray(ppcVals)[i]) do begin
                CurrentEntry.Add(pszAttr, pchararray(ppcVals)[i]);
                Inc(i);
              end;
            finally
              LDAPCheck(ldap_value_free(ppcVals));
          end;
          pszAttr:= ldap_next_attribute(fConn,  plmEntry,  pbe);
        end;
        plmEntry:= ldap_next_entry(fConn,  plmEntry);
      end;
    finally
    end;
  finally
   ldap_msgfree(plmSearch);
  end;
end;

procedure TldapConnection.Delete(const DN: String);
begin
  LDAPCheck(ldap_delete_s(fConn, PChar(DN)));
end;

procedure TldapConnection.Delete(const Entry: TldapEntry);
begin
  LDAPCheck(ldap_delete_s(fConn, PChar(Entry.DN)));
end;

procedure TldapConnection.Add(const Entry: TldapEntry);
begin
  try
    Entry.MakeLDAPArray;
    LDAPCheck(ldap_add_s(fConn, PChar(Entry.fdn), PLDAPMod(Entry.P)));
  finally
    Entry.DisposeLDAPArray;
  end;
end;

procedure TldapConnection.Modify(const Entry, NewEntry: TldapEntry);
begin
  try
    Entry.MakeModLDAPArray(NewEntry);
    if (Assigned(Entry.P)) and (High(Entry.P) <> Low(Entry.P))
    then LDAPCheck(ldap_modify_s(fConn, PChar(Entry.FDN), PldapMod(Entry.P)));
    Entry.ApplyChangesInMemory;
  finally
    Entry.DisposeLDAPArray;
  end;
end;

function  TldapConnection.GetConnected: Boolean;
begin
  Result:= fConn <> Nil;
end;

procedure TldapConnection.SetConnected(AValue: Boolean);
begin
  if AValue then begin
    if not GetConnected
    then Open;
  end else begin
    if GetConnected
    then Close;
  end;
end;

procedure TldapConnection.Close;
begin
  if Assigned(fConn)
  then LDAPCheck(ldap_unbind_s(fConn));
  fConn:= Nil;
end;

procedure TldapConnection.Open;
begin
  fConn:= ldap_open(PChar(fHost),  fPort);
  if Assigned(fConn) then
    if (fPWD='') or (fDN='') then
      LDAPCheck(ldap_simple_bind_s(fConn,  Nil,  Nil))
    else
      LDAPCheck(ldap_simple_bind_s(fConn,  PChar(fDN),  PChar(fPwd)))
  else
    ldapError('Error Opening Connection to Server');
end;

procedure TldapConnection.ReBind(const BindAs,  BindPassword: String);
begin
  fDN:= BindAs;
  fPWD:= BindPassword;
  if Assigned(fConn) then
    if (BindPassword = '') or (BindAs = '') then
      LDAPCheck(ldap_simple_bind_s(fConn,  Nil,  Nil))
    else
      LDAPCheck(ldap_simple_bind_s(fConn,  PChar(BindAs),  PChar(BindPassword)))
  else
    ldapError('No Exisiting Connection to Server');
end;

procedure TldapConnection.Open(const Hostname,  BindAs,  BindPassword: String; const PortNumber: Integer);
begin
  fHost:= Hostname;
  fPwd:= BindPassword;
  fPort:= PortNumber;
  fDN:= BindAS;
  Open;
end;

function FindDn(const server, base, Uid:String):String;
var
  conn: TldapConnection;
  el: TldapEntryList;
begin
  try
    try
      Conn:= TldapConnection.Create;
      Conn.Open(server);
      el:= TldapEntryList.Create;
      Conn.Search(el, base, '(uid='+uid+')');
      Result:= el.Dn[0];
    finally
      el.Free;
      conn.Close;
      conn.Free;
    end;
  except
    Result:= '';
  end;
end;

function ValidDn(const server, dn, uid, pwd: String): Boolean;
var
  conn: TldapConnection;
  el: TldapEntryList;
begin
  try
    try
      Conn:= TldapConnection.Create;
      Conn.Open(Server, dn, pwd);
      el:= TldapEntryList.Create;
      Conn.Search(el, dn, '(uid='+uid+')', 'base');
      Result:= UpperCase(el.GetEntryValue(0, 'uid', 0))=uppercase(uid);
    finally
      el.Free;
      conn.Close;
      conn.Free;
    end;
  except
    Result:= False;
  end;
end;

function Check(const Server: String; const PortNumber: Integer; const Uid, Password, BaseDN: String): Boolean;
var
  dn: String;
  conn: TldapConnection;
  el1: TldapEntryList;
begin
  Result:= True;
  try
    try
      Conn:= TldapConnection.Create;
      Conn.Open(Server, '', '', PortNumber);
      el1:= TldapEntryList.Create;
      Conn.Search(el1,  baseDN, '(uid=' + uid + ')');
      if (el1.Count=1)
      then dn:= el1.dn[0]
      else dn:= '';
      if dn = ''
      then Result:= False
      else Conn.ReBind(dn, Password);
    finally
      el1.Free;
      conn.Close;
      conn.Free;
    end;
  except
    Result:= False;
  end;
end;

//------------------------------- TldapDataset ---------------------------------

constructor TldapDataset.Create(AOwner: TObject);
begin
  inherited Create;
  FCursor:= 0;
  FFilter:= TStringList.Create;

  FldapEntryList:= TldapEntryList.Create;
  FScope:= 'sub';
end;

destructor TldapDataset.Destroy;
begin
  FldapEntryList.Free;
  FFilter.Free;

  inherited Destroy;
end;

procedure TldapDataset.Open();
var
  s: String;
begin
  inherited Open;
  s:= Trim(FFilter.Text);
  Search(FldapEntryList, BaseDN, s, FScope);
end;

procedure TldapDataset.Open(const AFilter: String; const AScope: String='sub');
begin
  FFilter.Text:= AFilter;
  FScope:= AScope;
  Open();
end;

procedure TldapDataset.Close;
begin
  FldapEntryList.ClearAll;
  inherited Close;
end;

function TldapDataset.GetEOF: Boolean;
begin
  Result:= FCursor >= FldapEntryList.Count;
end;

function TldapDataset.GetField(AAttrIndex: Integer): TldapAttribute;
begin
  if EOF
  then Result:= Nil
  else Result:= TldapAttribute(ldapEntryList[FCursor].NameList.Objects[AAttrIndex]);
end;

procedure TldapDataset.Next;
begin
  if EOF
  then Exit;
  Inc(FCursor);
end;

procedure TldapDataset.First;
begin
  FCursor:= 0;
end;

function TldapDataset.MoveBy(Distance: Integer): Integer;
begin
  FCursor:= FCursor + Distance;
end;

//------------------------------- TldapConnEnv ---------------------------------

// keep ldap connection environment

constructor TldapConnEnv.Create(const AHost: String; APort: Integer; const ABindDN, ABindPwd, ABaseDN: String;
  AOpen: Boolean);
begin
  FEnv:= TStringList.Create;
  BaseDN:= ABaseDN;
  Host:= AHost;
  Port:= IntToStr(APort);
  BindDN:= ABindDN;
  BindPwd:= ABindPwd;

  FldapConn:= TldapConnection.Create;
  with FldapConn do begin
    Host:= AHost;
    Port:= APort;
    BindDN:= ABindDN;
    BindPwd:= ABindPwd;
    BaseDN:= ABaseDN;
    if AOpen then
    try
      Open;
    except
    end;
  end;
end;

destructor TldapConnEnv.Destroy;
begin
  FldapConn.Free;
  FEnv.Free;
  inherited Destroy;
end;

function TldapConnEnv.GetEnvVar(Index: Integer): String;
begin
  Result:= FEnv.Values[IntToStr(Index)];
end;

procedure TldapConnEnv.SetEnvVar(Index: Integer; const AValue: String);
begin
  FEnv.Values[IntToStr(Index)]:= AValue;
end;

end.
