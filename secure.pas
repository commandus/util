unit
  secure;
(*******************************************************************
 *                                                                 *
 * S  E  C  U  R  E -set of message-digest algorithms              *
 *                                                                 *
 * (c) 1993,1994,1998 A.Ivanov                                     *
 * functions to generate a pseudo-random, float uniformly          *
 * distributed on [0,1)                                            *
 * based on code:                                                  *
 * (c) Golden Geo Corp., Golden CO   (303) 277-0420                *
 * D. Kahaner, C. Moler, S. Nash, "Numerical Methods and Software",*
 * Prentice Hall, 1988.                                            *
 * Marsaglia G., "Comments on the perfect uniform random number    *
 * generator", Unpublished notes, Wash S. U.                       *
 * Pascal version (c) 1993-1994, Andrei Ivanov                     *
 *                                                                 *
 * The MD5 Message-Digest Algorithm implementation based on        *
 * MIT Laboratory for Computer Science & RSA Data Security, Inc.   *
 * RFC1321 http://www.rsa.com/pub/                                 *
 * Pascal version (c) 1998, A.Ivanov                               *
 *                                                                 *
 * SHA-1   http://csrc.ncsl.nist.gov/fips/                         *
 * SECURE HASH STANDARD, U.S. DEPARTMENT OF COMMERCE/National      *
 * Institute of Standards and Technology                           *
 * FEDERAL INFORMATION PROCESSING STANDARDS PUBLICATION            *
 * FIPS PUB 180 - 1993 May 11, 1995 April 17                       *
 * SHA-1 algorithm is a bug-fix of original SHA(fips180.txt)       *
 * Dr.Dobbs Journal, Jan 1994 have bug.                            *
 * Then flaw has been discovered and corrected by adding a rotate  *
 * instruction in the calculation of Wt. Look Sect. 7 FIPS PUB 180 *
 * (page 9): "b) For t=16 to 79                                    *
 *   let Wt = S1(Wt-3 XOR Wt-8 XOR Wt-14 XOR Wt-16)."              *
 * has replaced: "b) For t=16 to 79                                *
 *   let Wt = Wt-3 XOR Wt-8 XOR Wt-14 XOR Wt-16."                  *
 * Pascal version (c) 1998, A.Ivanov                               *
 *                                                                 *
 * Last Revision: Dec 16 1993                                      *
 * Last Fix:      May  3, Jun 30 1994                              *
 * Line 143 fixed (8 chars, old- 7) May 20 1998                    *
 * Last Fix:      Oct 28 1998 delete initialization code for       *
 *                  co- processor emulation library                *
 * Last Line:     704                                              *
 *                                                                 *
 ******************************************************************)
{ Borland Pascal 7.0 floating point emulation library use }
{$E+,N+}
{ You must apply that switches in program, no effect used in unit  }
{ in CONFIG.SYS: SET 87 = N }
{ Test8087 }
interface
const
  standard_secure = ''; { pass to getsecure like machine id }
type
  AutotentifyFuncType = function: String;
var
  { You will override this function
  }
  User_Autotentify: AutotentifyFuncType;
  { You will use this variable for
  }
  SaveTest8087: Word;

{ or IsValidPasswd() or Security() }
function  IsValidPasswd(const Passwd: String): Boolean;
procedure security(var return_code: Word);
function  getsecure: String;
procedure SetMachine_Id(Amachine_id: String);
procedure AssignAutotentificationRoutine(NewRoutine: AutotentifyFuncType);

{ base functions }
{ Base conversion functions -------------------------------------------------- }
function DecodeBase (const S: String; const Base: Byte): Int64;
function EncodeBase (const I: Int64; const Base: Byte) : String;

{ return 16 bytes in string (hex) representation }
function GetMD5Digest(Buffer: Pointer; BufSize: Integer; ABase: Byte): ShortString;
{ See RFC 2104 (HMAC)  }
function CalcKeyedMD5(const Key, Data : String; ABase: Byte): String;
function getMD5EncryptedPassword(const APassword: ShortString): ShortString;
{ return 20 bytes in string (hex) representation }
function GetSHADigest(Buffer: Pointer; BufSize: Integer; ABase: Byte): string;

{ MDA ----------------------------------------------------------------------}

type
  TMD5Context = packed record
    State: packed array[0..3] of LongInt;
    Count: packed array[0..1] of LongInt;
    case Integer of
      0: (BufChar: packed array[0..63] of Byte);
      1: (BufLong: packed array[0..15] of LongInt)
  end;

  TMD5Digest = packed array[0..15] of Char;

{  Start MD5 accumulation. }
procedure MD5Init(var MD5Context: TMD5Context);
{  Update context to reflect the concatenation of another buffer full  }
procedure MD5Update(var MD5Context: TMD5Context; const Data; Len: Integer);
procedure MD5Final(var MD5Context: TMD5Context; var R: TMD5Digest);

{ SHA ----------------------------------------------------------------------}

type
  TSHAContext = packed record
    State: packed array[0..4] of LongWord;
    Count: packed array[0..1] of LongWord;
    case Integer of
      0: (BufChar: packed array[0..63] of Byte);
      1: (BufLong: packed array[0..15] of LongWord)
  end;

  TSHADigest = packed array[0..19] of Char;

{ Start SHA accumulation }
procedure SHAInit(var SHAContext: TSHAContext);
{ Update context to reflect the concatenation of another buffer full of bytes. }
procedure SHAUpdate(var SHAContext: TSHAContext; const Data; Len: Word);
procedure SHAFinal(var SHAContext: TSHAContext; var R: TSHADigest);
                                   
implementation
{$IFDEF VER180}{$DEFINE D6+}{Turbo Delphi 2006}{$ENDIF}
{$IFDEF VER170}{$DEFINE D6+}{Borland Delphi 2005}{$ENDIF}
{$IFDEF VER160}{$DEFINE D6+}{$ENDIF}
{$IFDEF VER150}{$DEFINE D6+}{$ENDIF}
{$IFDEF VER140}{$DEFINE D6+}{$ENDIF}

{$IFDEF VER70}
uses
  Objects, StrUtil, Diags;
{$ELSE}
uses
  SysUtils, Math, Util1, IdGlobal, Classes, IdCoder3to4;
{$ENDIF}
type
  r17 = packed array[0..16] of Real;
const
{  This pseudo-random generator period is 10^19 }
{ constants used to generate random numbers (16777216=2^24) }
  CS    = 362436.0/16777216.0;
  CD    = 7654321.0/16777216.0;
  CM    = 16777213.0/16777216.0;
  NBITS = 24;

{$IFDEF D6+}
var
{$ELSE}
const
{$ENDIF}
  fml_i: Integer = 16;
  fml_j: Integer = 4;
  fml_c: Real = CS;
  u: r17 = (
	0.8668672834288,  0.3697986366357,  0.8008968294805,
	0.4173889774680,  0.8254561579836,  0.9640965269077,
	0.4508667414265,  0.6451309529668,  0.1645456024730,
	0.2787901807898,  0.06761531340295, 0.9663226330820,
	0.01963343943798, 0.02947398211399, 0.1636231515294,
	0.3976343250467,  0.2631008574685
	   );
  { next code similar to SetMachine_Id(standard_secure); }
  Machine_Id: String[80] = '';

{$IFDEF D6+}
const
  ConversionAlphabeth: String [64] = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
type
  TIdEncoderRadix = class(TIdEncoder3to4)
  public
    constructor Create(AOwner: TComponent; ARadix: Byte; const AFillChar: Char = '='); reintroduce;
    function Encode(const ASrc: string): string; overload;
  end;

  TIdDecoderRadix = class(TIdDecoder4to3)
  public
    constructor Create(AOwner: TComponent; ARadix: Byte; const AFillChar: Char = '='); reintroduce;
  end;

constructor TIdEncoderRadix.Create(AOwner: TComponent; ARadix: Byte; const AFillChar: Char = '=');
begin
  inherited Create(AOwner);
  FCodingTable:= ConversionAlphabeth; // Wrong - Copy(ConversionAlphabeth, 1, ARadix);
  FFillChar:= AFillChar;   {Do not Localize}
end;

function TIdEncoderRadix.Encode(const ASrc: string): string;
var
  i: Integer;
begin
  Result:= inherited Encode(ASrc);
  if FillChar <> #0
  then Exit;
  i:= Length(Result);
  while i > 0 do begin
    if Result[i] <> FillChar
    then Break;
    Delete(Result, i, 1);
    Dec(i);
  end;
end;

constructor TIdDecoderRadix.Create(AOwner: TComponent; ARadix: Byte; const AFillChar: Char = '=');
var
  RadixDecodeTable: TIdDecodeTable;
begin
  inherited Create(AOwner);
  TIdDecoder4to3.ConstructDecodeTable(Copy(ConversionAlphabeth, 1, ARadix), RadixDecodeTable);
  FDecodeTable:= RadixDecodeTable;
  FFillChar:= AFillChar;  {Do not Localize}
end;
{$ENDIF}

function franuni: real; { return a psuedo-random real [0.0..1.0) }
var
  uni: Real;
begin
  { basic generator is Fibonacci }
  uni:= u[fml_i]-u[fml_j];
  if (uni<0.0) then uni:= uni+1.0;
  u[fml_i]:= uni;
  Dec(fml_i);
  if (fml_i<0) then fml_i:= 16;
  Dec(fml_j);
  if (fml_j<0) then fml_j:= 16;

  { second generator is congruential }
  fml_c:= fml_c-CD;
  if (fml_c<0.0) then fml_c:= fml_c+CM;

  { combination generator }
  uni:= uni-fml_c;
  if (uni<0.0) then uni:= uni+1.0;
  franuni:= uni;
end; { franuni }

procedure sranuni (seed: LongWord); { seed random number generator  }
{ different seeds yield different sequences of random numbers }
var
  ii, jj, i1, j1, k1, l1, m1: Word;
  s, t : Real;

begin
  { convert seed to four smallish words }
  i1:= (Abs(seed) mod 177)+1;
  j1:= (Abs(seed) mod 167)+1;
  k1:= (Abs(seed) mod 157)+1;
  l1:= (Abs(seed) mod 147)+1;

  { generate random bit pattern in array based on given seed }
  for ii:=0 to 16 do begin
    s:= 0.0;
    t:= 0.5;
    { loop over bits in the float mantissa }
    for jj:=0 to NBITS-1 do begin
      m1:= (((i1*j1) mod 179)*k1) mod 179;
      i1:= j1;
      j1:= k1;
      k1:= m1;
      l1:= (53*l1+1) mod 169;
      if (((l1*m1) mod 64)>=32) then s:= s+t;
      t:= t*0.5;
    end;
    u[ii]:= s;
  end;
  { initialize generators }
  fml_i:= 16;
  fml_j:= 4;
  fml_c:= CS;
end; { sranuni }

function Ensen_Hash_Method (ident: LongWord ): String;
var
  l: Integer;
  hash_string: String[8];
begin
  { generate symbols in range 65..65+25 'A'..'Z'}
  sranuni(ident);
  hash_string[0]:= #8;    { #7, for Turbo Pascal 7.0 #8 is possible }
  for l:= 1 to Byte(hash_string[0]) do { 0..25 } { in origin 1..7, #7 }
    hash_string[l]:= Chr(65 + Trunc(franuni*1000.0) mod 26);
  Ensen_Hash_Method:= Hash_String;
end; { Ensen_Hash_Method }

{$IFDEF VER70}
function Get_from_CMOS: LongWord;
var
  LongR: packed record X, Y: Word; end;
begin
  WordRec(LongR. Y).Hi:= GetCMOS($12); { hard disk parameters(type) }
  WordRec(LongR. Y).Lo:= GetCMOS($14); { equipment bit field        }
  WordRec(LongR. X).Hi:= GetCMOS($2e); { CMOS CRC value             }
  WordRec(LongR. X).Lo:= GetCMOS($2f);
  Get_from_CMOS:= LongWord(LongR);
end; { GetCMOSCheckSumValue }
{$ENDIF}

function get_machine_name: String;
var
  S: String;
begin
{$IFDEF VER70}
  Str(Get_from_CMOS, S);
{$ELSE}
  S:= 'NONAME';
{$ENDIF}
  Get_Machine_Name:= S;
end; { get_machine_name }

function extract_number(S: String): LongWord;
{ routine convert variable-len string to 4-byte integer }
{ simulatinously CRC calculation                        }
type
  s4type = packed array[0..3] of Byte;
var
  sp: Integer;
  Num: LongWord;
  ss: String[255];
  s4: s4type;
begin
  Num:= 0;
  { add blanked }
  FillChar(ss,255,#32);
  ss:= s;
  Byte(ss[0]):= Byte(ss[0])+Byte(ss[0]) mod 4 ;
  { more than 1 quarters! }
  for sp:= 0 to Byte(ss[0]) shr 2 - 1 do begin
    Move(ss[sp shl 2 + 1], s4, 4);
    Num:= LongWord(s4)+Num;
  end; { do }
  Extract_Number:= Num;
end; { extract_number }

{$IFDEF VER70}
function Default_User_Autotentify: String; far;
{ you may override this function for your user interface }
var
  s: String;
begin
  Write('Enter access code:');
  Readln(s);
  Default_User_Autotentify:= UpcaseStr(s);
end; { Default_User_Autotentify }

{$ELSE}

function Default_User_Autotentify: String; far;
{ Windows dialog }
begin
  Result:= 'DEFAULT';
end; { Default_User_Autotentify }
{$ENDIF}

{ or procedure ValidatePasswd() or security - what you prefer
}
function  IsValidPasswd(const Passwd: String): Boolean;
begin
   if(Passwd<>Ensen_Hash_Method(Extract_Number(Machine_Id))) then begin
    IsValidPasswd:= False;
    { user is not licensed }
  end else begin
    { user is licensed }
    IsValidPasswd:= True;
  end;
end;

procedure security(var return_code: Word);
begin
   if(User_Autotentify<>Ensen_Hash_Method(Extract_Number(Machine_Id))) then begin
    return_code:= 0;
    { user is not licensed }
  end else begin
    { user is licensed }
    return_code:= 1;
  end;
end; { security }

procedure SetMachine_Id(Amachine_id: String);
begin
  if Amachine_id = '' then Machine_Id:= Get_Machine_Name
  else Machine_Id:= Amachine_id;
end;

function  GetSecure: String;
begin
  GetSecure:= Ensen_Hash_Method(Extract_Number(Machine_Id));
end; { getsecure }

{ not necessary if ValidatePasswd() used }
procedure AssignAutotentificationRoutine(NewRoutine: AutotentifyFuncType);
begin
  User_Autotentify:= NewRoutine;
end;

{ Base conversion functions -------------------------------------------------- }

function DecodeBase (const S : String; const Base : Byte): Int64;
var
  Tot, L: Int64;
  P, F: Byte;
begin
  if Base > Length(ConversionAlphabeth)
  then raise Exception.Create ('Invalid base');
  P:= Length (S);
  if P = 0
  then raise Exception.Create ('Invalid value');
  L:= 1;
  Tot:= 0;
  repeat
    F:= Pos (UpCase (S [P]), ConversionAlphabeth);
    if (F = 0) or (F > Base)
    then raise Exception.Create ('Invalid character ''' + S [P] + '''');
    Tot:= Tot + L * (F - 1);
    L:= L * Base;
    Dec (P);
  until P = 0;
  DecodeBase := Tot;
end;

function EncodeBase(const I: Int64; const Base: Byte) : String;
var
  D, J: Int64;
  N: Byte;
begin
  {
  Result:= Format('%x', [I]);
  Exit;
  }
  if I = 0 then begin
    Result:= '0';
    Exit;
  end;
  D:= Round(Power(Base, Trunc (Log10 (I) / Log10 (Base)) + 0)); // was + 1: to fix occasional real "fuzz"
  J:= I;
  Result:= '';
  while D > 0 do begin
    N := J div D;
    if (N <> 0) or (Result <> '')
    then Result := Result + ConversionAlphabeth [N + 1]; // "fuzz" bug 
    J:= J mod D;
    D:= D div Base;
  end;
end;

{ MDA----------------------------------------------------------------------}

procedure MD5Init(var MD5Context: TMD5Context);
{  Start MD5 accumulation.  Set bit count to 0 and State to mysterious  }
{  initialization constants.                                            }
begin
  FillChar(MD5Context, SizeOf(TMD5Context), #0);
  { set magic initialization constants }
  with MD5Context do begin
    State[0] := LongInt($67452301);
    State[1] := LongInt($EFCDAB89);
    State[2] := LongInt($98BADCFE);
    State[3] := LongInt($10325476);
  end
end;

procedure MD5Transform(var Buf: array of LongInt; const Data: array of LongInt); forward;

{ Data: input block, len:                     }
procedure MD5Update(var MD5Context: TMD5Context; const Data; Len: Integer);
{  Update context to reflect the concatenation of another buffer full  }
{  of bytes.                                                           }
type
  TByteArray = packed array[0..0] of Byte;
var
  Index: Word;
  t: LongInt;
begin
  with MD5Context do begin
    T := Count[0];
    Inc(Count[0], LongInt(Len) shl 3);
    if Count[0] < T
    then Inc(Count[1]);
    Inc(Count[1], Len shr 29);
    T := (T shr 3) and $3F;
    Index := 0;
    if T <> 0 then begin
      Index := T;
      T := 64 - T;
      if Len < T then begin
        Move(Data, BufChar[Index], Len);
        Exit;
      end;
      Move(Data, BufChar[Index], T);
      MD5Transform(State, BufLong);
      Dec(Len, T);
      Index := T;  { Wolfgang Klein, 05/06/99 }
    end;
    while Len >= 64 do begin
      Move(TByteArray(Data)[Index], BufChar, 64);
      MD5Transform(State, BufLong);
      Inc(Index, 64);
      Dec(Len, 64);
    end;
    Move(TByteArray(Data)[Index], BufChar, Len);
  end
end;

const
  MaxBufSize = 16384;
type
  PMD5Buffer = ^TMD5Buffer;
  TMD5Buffer = array[0..(MaxBufSize - 1)] of Char;

procedure MD5UpdateBuffer(var MD5Context: TMD5Context; Buffer: Pointer; BufSize: Integer);
var
  BufTmp : PMD5Buffer;
  BufPtr : PChar;
  Bytes  : Word;
begin
  New(BufTmp);
  BufPtr:= Buffer;
  try
    repeat
      if BufSize > MaxBufSize
      then Bytes := MaxBufSize
      else Bytes := BufSize;
      Move(BufPtr^, BufTmp^, Bytes);
      Inc(BufPtr, Bytes);
      Dec(BufSize, Bytes);
      if Bytes > 0
      then MD5Update(MD5Context, BufTmp^, Bytes);
    until Bytes < MaxBufSize;
  finally
    Dispose(BufTmp);
  end;
end;

procedure MD5Final(var MD5Context: TMD5Context; var R: TMD5Digest);
var
  Cnt: Word;
  p: Byte;
begin
  with MD5Context do begin
    { Compute number of bytes mod 64 }
    Cnt := (Count[0] shr 3) and $3F;

    { Set the first char of padding to $80 }
    p := Cnt;
    BufChar[p] := $80;
    Inc(p);

    { Bytes of padding needed to make 64 bytes }
    Cnt := 64 - 1 - Cnt;

    { Pad out to 56 mod 64 }
    if Cnt < 8 then begin
      { Two lots of padding:  Pad the first block to 64 bytes }
      FillChar(BufChar[p], Cnt, #0);
      MD5Transform(State, BufLong);

      { Now fill the next block with 56 bytes }
      FillChar(BufChar, 56, #0)
    end else
      { Pad block to 56 bytes }
      FillChar(BufChar[p], Cnt - 8, #0);

    { Append length in bits and transform }
    BufLong[14] := Count[0];
    BufLong[15] := Count[1];
    MD5Transform(State, BufLong);

    Move(State, R, 16)
  end;

  FillChar(MD5Context, SizeOf(TMD5Context), #0)
end;


const
  MD5Table_1 : array [0..15] of LongInt = (
      LongInt($D76AA478), LongInt($E8C7B756), LongInt($242070DB), LongInt($C1BDCEEE),
      LongInt($F57C0FAF), LongInt($4787C62A), LongInt($A8304613), LongInt($FD469501),
      LongInt($698098D8), LongInt($8B44F7AF), LongInt($FFFF5BB1), LongInt($895CD7BE),
      LongInt($6B901122), LongInt($FD987193), LongInt($A679438E), LongInt($49B40821));
  MD5Table_2 : array [0..15] of LongInt = (
      LongInt($F61E2562), LongInt($C040B340), LongInt($265E5A51), LongInt($E9B6C7AA),
      LongInt($D62F105D), LongInt($02441453), LongInt($D8A1E681), LongInt($E7D3FBC8),
      LongInt($21E1CDE6), LongInt($C33707D6), LongInt($F4D50D87), LongInt($455A14ED),
      LongInt($A9E3E905), LongInt($FCEFA3F8), LongInt($676F02D9), LongInt($8D2A4C8A));
  MD5Table_3 : array [0..15] of LongInt = (
      LongInt($FFFA3942), LongInt($8771F681), LongInt($6D9D6122), LongInt($FDE5380C),
      LongInt($A4BEEA44), LongInt($4BDECFA9), LongInt($F6BB4B60), LongInt($BEBFBC70),
      LongInt($289B7EC6), LongInt($EAA127FA), LongInt($D4EF3085), LongInt($04881D05),
      LongInt($D9D4D039), LongInt($E6DB99E5), LongInt($1FA27CF8), LongInt($C4AC5665));
  MD5Table_4 : array [0..15] of LongInt = (
      LongInt($F4292244), LongInt($432AFF97), LongInt($AB9423A7), LongInt($FC93A039),
      LongInt($655B59C3), LongInt($8F0CCC92), LongInt($FFEFF47D), LongInt($85845DD1),
      LongInt($6FA87E4F), LongInt($FE2CE6E0), LongInt($A3014314), LongInt($4E0811A1),
      LongInt($F7537E82), LongInt($BD3AF235), LongInt($2AD7D2BB), LongInt($EB86D391));

{ MD5 basic transformation. Transforms state based on block.                  }
{$OVERFLOWCHECKS OFF}
procedure MD5Transform(var Buf: array of LongInt; const Data: array of LongInt);
var
  A, B, C, D: LongInt;
  I: Integer;
  J: Byte;
begin
  A := Buf [0];
  B := Buf [1];
  C := Buf [2];
  D := Buf [3];

  for I := 0 to 3 do begin
    J := I * 4;
    Inc(A, Data [J]     + MD5Table_1 [J]     + (D xor (B and (C xor D)))); A := A shl  7 or A shr 25 + B;
    Inc(D, Data [J + 1] + MD5Table_1 [J + 1] + (C xor (A and (B xor C)))); D := D shl 12 or D shr 20 + A;
    Inc(C, Data [J + 2] + MD5Table_1 [J + 2] + (B xor (D and (A xor B)))); C := C shl 17 or C shr 15 + D;
    Inc(B, Data [J + 3] + MD5Table_1 [J + 3] + (A xor (C and (D xor A)))); B := B shl 22 or B shr 10 + C;
  end;
  for I := 0 to 3 do begin
    J := I * 4;
    Inc (A, Data [J + 1]           + MD5Table_2 [J]     + (C xor (D and (B xor C)))); A := A shl  5 or A shr 27 + B;
    Inc (D, Data [(J + 6) mod 16]  + MD5Table_2 [J + 1] + (B xor (C and (A xor B)))); D := D shl  9 or D shr 23 + A;
    Inc (C, Data [(J + 11) mod 16] + MD5Table_2 [J + 2] + (A xor (B and (D xor A)))); C := C shl 14 or C shr 18 + D;
    Inc (B, Data [J]               + MD5Table_2 [J + 3] + (D xor (A and (C xor D)))); B := B shl 20 or B shr 12 + C;
  end;

  for I := 0 to 3 do begin
    J := 16 - (I * 4);
    Inc (A, Data [(J + 5) mod 16]  + MD5Table_3 [16 - J]     + (B xor C xor D)); A := A shl  4 or A shr 28 + B;
    Inc (D, Data [(J + 8) mod 16]  + MD5Table_3 [16 - J + 1] + (A xor B xor C)); D := D shl 11 or D shr 21 + A;
    Inc (C, Data [(J + 11) mod 16] + MD5Table_3 [16 - J + 2] + (D xor A xor B)); C := C shl 16 or C shr 16 + D;
    Inc (B, Data [(J + 14) mod 16] + MD5Table_3 [16 - J + 3] + (C xor D xor A)); B := B shl 23 or B shr  9 + C;
  end;

  for I := 0 to 3 do begin
    J := 16 - (I * 4);
    Inc (A, Data [J mod 16]        + MD5Table_4 [16 - J]     + (C xor (B or not D))); A := A shl  6 or A shr 26 + B;
    Inc (D, Data [(J + 7) mod 16]  + MD5Table_4 [16 - J + 1] + (B xor (A or not C))); D := D shl 10 or D shr 22 + A;
    Inc (C, Data [(J + 14) mod 16] + MD5Table_4 [16 - J + 2] + (A xor (D or not B))); C := C shl 15 or C shr 17 + D;
    Inc (B, Data [(J + 5) mod 16]  + MD5Table_4 [16 - J + 3] + (D xor (C or not A))); B := B shl 21 or B shr 11 + C;
  end;

  Inc (Buf [0], A);
  Inc (Buf [1], B);
  Inc (Buf [2], C);
  Inc (Buf [3], D);
end;

{ SHA----------------------------------------------------------------------}

procedure ReverseBytes(var Buf; ByteCnt: Word);
var
  BufLong: packed array[0..0] of LongWord absolute Buf;
  Tmp: LongWord;
  i: Word;
begin
  ByteCnt := ByteCnt div 4;
  for i := 0 to ByteCnt - 1 do begin
    Tmp := (BufLong[i] shl 16) or (BufLong[i] shr 16);
    BufLong[i] := ((Tmp and $00FF00FF) shl 8) or ((Tmp and $FF00FF00) shr 8)
  end
end;

procedure SHAInit(var SHAContext: TSHAContext);
{  Start SHA accumulation.  Set bit count to 0 and State to mysterious  }
{  initialization constants.                                            }
begin
  FillChar(SHAContext, SizeOf(TSHAContext), #0);
  with SHAContext do begin
    State[0] := $67452301;
    State[1] := $EFCDAB89;
    State[2] := $98BADCFE;
    State[3] := $10325476;
    State[4] := $C3D2E1F0
  end
end;

procedure SHATransform(var Buf: array of LongWord; const Data: array of LongWord); forward;

procedure SHAUpdate(var SHAContext: TSHAContext; const Data; Len: Word);
{  Update context to reflect the concatenation of another buffer full of bytes. }
type
  TByteArray = packed array[0..0] of Byte;
var
  Index: Word;
  t: LongWord;
begin
  { Update bitcount }
  with SHAContext do begin
    t := Count[0];
    Inc(Count[0], LongWord(Len) shl 3);
    if Count[0] < t
    then Inc(Count[1]);
    Inc(Count[1], Len shr 29); { Makes no sense for Len of type Word, will be 0 }
    t := (t shr 3) and $3F;

    Index := 0;
    { Handle any leading odd-sized chunks }
    if t <> 0 then begin
      Index:= t;
      t:= 64 - t;
      if Len < t then begin
        Move(Data, BufChar[Index], Len);
        Exit
      end;
      Move(Data, BufChar[Index], t);
      SHATransform(State, BufLong);
      Dec(Len, t)
    end;

    { Process data in 64-byte chunks }
    while Len >= 64 do begin
      Move(TByteArray(Data)[Index], BufChar, 64);
      SHATransform(State, BufLong);
      Inc(Index, 64);
      Dec(Len, 64)
    end;

    { Handle any remaining bytes of data. }
    Move(TByteArray(Data)[Index], BufChar, Len)
  end
end;

procedure SHAFinal(var SHAContext: TSHAContext; var R: TSHADigest);
var
  Cnt: Word;
  p: Byte;
begin
  with SHAContext do begin
    { Compute number of bytes mod 64 }
    Cnt := (Count[0] shr 3) and $3F;

    { Set the first char of padding to $80 }
    p := Cnt;
    BufChar[p] := $80;
    Inc(p);

    { Bytes of padding needed to make 64 bytes }
    Cnt := 64 - 1 - Cnt;

    { Pad out to 56 mod 64 }
    if Cnt < 8 then begin
      { Two lots of padding:  Pad the first block to 64 bytes }
      FillChar(BufChar[p], Cnt, #0);
      SHATransform(State, BufLong);

      { Now fill the next block with 56 bytes }
      FillChar(BufChar, 56, #0)
    end else
      { Pad block to 56 bytes }
      FillChar(BufChar[p], Cnt - 8, #0);

    { Append length in bits and transform }
    BufLong[14] := Count[1];
    BufLong[15] := Count[0];
    ReverseBytes(BufLong[14], 8);
    SHATransform(State, BufLong);

    { Resulting digest equals current State }
    Move(State, R, SizeOf(TSHADigest));
    ReverseBytes(R, SizeOf(TSHADigest))
  end;

  FillChar(SHAContext, SizeOf(TSHAContext), #0)
end;

function rol(x: LongWord; cnt: Byte): LongWord;
{ Rotate left }
begin
  Rol:= (x shl cnt) or (x shr (32 - cnt))
end;

procedure SHATransform(var Buf: array of LongWord; const Data: array of LongWord);
var
  a, b, c, d, e: LongWord;
  Tmp: LongWord;
  w: packed array[0..15] of LongWord;
  i: Word;
begin
  a := Buf[0];
  b := Buf[1];
  c := Buf[2];
  d := Buf[3];
  e := Buf[4];

  Move(Data, w, 64);
  ReverseBytes(w, 64);

  for i := 0 to 79 do begin
    if i > 15 then
      w[i and 15] := rol(w[ i       and 15] xor w[(i - 14) and 15] xor
                         w[(i -  8) and 15] xor w[(i -  3) and 15], 1);
    if i <= 19 then
      Tmp := rol(a, 5) + e + w[i and 15] + $5A827999 + ((b and c) or ((not b) and d))
    else if i <= 39 then
      Tmp := rol(a, 5) + e + w[i and 15] + $6ED9EBA1 + (b xor c xor d)
    else if i <= 59 then
      Tmp := rol(a, 5) + e + w[i and 15] + $8F1BBCDC + ((b and c) or (b and d) or (c and d))
    else
      Tmp := rol(a, 5) + e + w[i and 15] + $CA62C1D6 + (b xor c xor d);
    e := d;
    d := c;
    c := rol(b, 30);
    b := a;
    a := Tmp
  end;

  Inc(Buf[0], a);
  Inc(Buf[1], b);
  Inc(Buf[2], c);
  Inc(Buf[3], d);
  Inc(Buf[4], e)
end;

function getMD5EncryptedPassword(const APassword: ShortString): ShortString;
var
  i: Integer;
  MD5Digest: TMD5Digest;
  MD5Context: TMD5Context;
begin
  Result:= '';
  for i:= 0 to 15
  do Byte(MD5Digest[i]):= i + 1;
  MD5Init(MD5Context);
  MD5UpdateBuffer(MD5Context, @APassword[1], Length(APassword));
  MD5Final(MD5Context, MD5Digest);
  for i:= 0 to 15
  do Result:= Result + IntToHex(Byte(MD5Digest[i]) and $FF, 2);
  Result:= LowerCase(Result);
end;

function GetMD5Digest(Buffer: Pointer; BufSize: Integer; ABase: Byte): ShortString;
var
  i: Integer;
  MD5Digest: TMD5Digest;
  MD5Context: TMD5Context;
  enc: TIdEncoderRadix;
  // enc: TIdEncoder3to4;
  md5s: String;
begin
  Result:= '';
  for i:= 0 to 15
  do Byte(MD5Digest[i]):= i + 1;
  MD5Init(MD5Context);
  MD5UpdateBuffer(MD5Context, Buffer, BufSize);
  MD5Final(MD5Context, MD5Digest);
  enc:= TIdEncoderRadix.Create(Nil, ABase, #0);  // it is look like indy can not process base other than 64 ;(

  SetLength(md5s, 16);
  Move(MD5Digest[0], md5s[1], 16);
  Result:= enc.Encode(md5s);
//  md5s:= Result;
  enc.Free;
end;

{ See RFC 2104 (HMAC)  }
procedure XORBlock (var Buf : String; const XOR8 : Byte);
var
  i : Integer;
begin
  for i:= 1 to Length(Buf)
  do Buf [I]:= Char(Byte(Buf [I]) xor XOR8);
end;

{ See RFC 2104 (HMAC)  }
function CalcKeyedMD5(const Key, Data : String; ABase: Byte): String;
const
  B = 64;
  L = 16;
  ipad = $36;
  opad = $5C;
var
  K, S, T: String;
begin
  if Length (Key) > B
  then K:= GetMD5Digest(@Key[1], Length(Key), ABase)
  else K:= Key;
  S:= K + StringOfChar(#0, B - Length (K));
  T:= S;
  XORBlock(T, ipad);
  T:= T + Data;
  T:= GetMD5Digest(@T[1], Length(T), ABase);
  XORBlock(S, opad);
  S:= S + T;
  Result:= GetMD5Digest(@S[1], Length(S), ABase);
end;

function GetSHADigest(Buffer: Pointer; BufSize: Integer; ABase: Byte): string;
var
  i: Integer;
  SHADigest: TSHADigest;
  SHAContext: TSHAContext;
begin
  Result:= '';
  for i:= 0 to 19
  do Byte(SHADigest[i]):= i + 1;
  SHAInit(SHAContext);
  SHAUpdate(SHAContext, Buffer, BufSize);
  SHAFinal(SHAContext, SHADigest);
  for i:= 0 to 19
  do Result:= Result + EncodeBase(Byte(SHADigest[I]), ABase);
end;

begin
{$IFDEF VER70}
  { for old machines may be actual to use co- processor emulation library
  SaveTest8087:= System. Test8087; System. Test8087:= 0; }
  User_Autotentify:= Default_User_Autotentify;
{$ENDIF}
end.
