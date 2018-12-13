(*******************************************************************
 *                                                                 *
 *                        B  U  Y  E  R                            *
 *                                                                 *
 *      Ensen's Utilities                                          *
 *      А.Иванов (c) 1993-1994                                     *
 *      Secure unit demonstration program. Get customer's bill     *
 *      number, return access code                                 *
 *                   Last Revision: Dec 16 1993                    *
 *                   Last Fix:      May  3, Jun 30 1994            *
 *                   Last Line:                                    *
 *                                                                 *
 ******************************************************************)
{ Borland Pascal 7.0 floating point emulation library use }
{$E+,N+}
program Buyer;
{$E+,N+}
uses
  secure, crt, dos, objects, strutil;
const
  days : array [0..6] of String[3] =
    ('Su',
     'Mo',
     'Tu',
     'We',
     'Th',
     'Fr',
     'Sa');

var
  s, ss: string[80];
  T: Text;
  y, m, d, dow : Word;
begin
  Writeln(#13#10'Buyer''s diskette code maker. Ensen'' Utilities, (c) 1993,1994'#13#10);
  Write('Enter buyer''s number(for example, 001):');
  Readln(s);
  SetMachine_Id(s);
  SetMachine_Id(GetSecure);
  ss:= GetSecure;
  Writeln('Diskette #', ss,'');
  Assign(T, 'LST\buyer');
  Append(T);
  Write(T,s:3, '-',ss:7,' , ');
  GetDate(y,m,d,dow);
  Writeln(T, days[dow],', ', m:0, '/', d:0, '/', y:0);
  Close(T);
end.