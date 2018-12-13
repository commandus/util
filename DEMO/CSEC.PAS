(*******************************************************************
 *                                                                 *
 *                           C  S  E  C                            *
 *                                                                 *
 *      Ensen's Utilities                                          *
 *      А.Иванов (c) 1993-1994                                     *
 *      Secure unit demonstration program. Get customer's bill     *
 *      number, return access code                                 *
 *                                                                 *
 *                   Last Revision: Dec 16 1993                    *
 *                   Last Fix:      May  3, Jun 30 1994            *
 *                   Last Line:                                    *
 *                                                                 *
 ******************************************************************)
{ Borland Pascal 7.0 floating point emulation library use }
{$E+,N+}
program csec;
{$E+,N+}
uses
  secure, crt, dos, objects, strutil;
var
  access: Word;
  i: Integer;
  s: string[80];
  MD5Digest: TMD5Digest;
  MD5Context: TMD5Context;

function Sample_Autotentify: String; far;
{ you will override this function for your user interface
}
var
  s: String;
begin
  Write('Enter host''s access password string (see higher):');
  Readln(s);
  Sample_Autotentify:= UpcaseStr(s);
end; { Default_User_Autotentify }

begin
  Writeln(#13#10'Sample security module. (c) Ensen Utilities, 1993-1994'#13#10);
  { it is not necessary assign your routine
    your routine may read string from some devices like keyboard.
  }
  AssignAutotentificationRoutine(Sample_Autotentify);

  { GetSecure return access string
    if You pass zero-string '' (STANDARD_SECURE),
    standard identification routines use
  }
  Write('Enter host''s original identificator (CR for standard):');
  Readln(s);
  SetMachine_Id(s);
  Writeln(GetSecure,' - that is host''s access password');
  Writeln('Caution: GetSecure routine don''t be placed in application!');

  { next routine get the password (by call Sample_Autotentify) and
    assign 0 or 1 to your variable access
  }
  security(access);
  Writeln('access code (1-Ok, 0-access denied):', csec. access);
  { assign value to access var is   1 - OK   0 - access denied
  }
  Writeln('Now try MD5 cipher');
  MD5Init(MD5Context);
  Write('Enter string to cipher: '); Readln(S);
  MD5Update(MD5Context, S[1], Byte(S[0]));
  MD5Final(MD5Context, MD5Digest);
  S:= '';
  for i := 0 to 15
  do S:= S + inHex8(Byte(MD5Digest[i]), 2);
  Writeln('Digest: ', S);
  {System. Test8087:= Secure. SaveTest8087;}
end.