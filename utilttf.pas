unit
  utilttf;
(*##*)
(*******************************************************************************
*                                                                             *
*   u  t  i  l  t  t  f                                                        *
*                                                                             *
*   Copyright © 2003- 2003 Andrei Ivanov. All rights reserved.                 *
*   True type font routines                                                   *
*   Conditional defines:                                                       *
*   Based on Microsoft Knowledge Base Article - 241020                        *
*   HOWTO: Translate Unicode Character Codes to TrueType Glyph Indices         *
*                                                                             *
*   Revisions    : Mar 19 2003                                                 *
*   Last revision: Mar 19 2003                                                *
*   Lines        : 46                                                          *
*   History      :                                                            *
*                                                                              *
*                                                                             *
*   Printed      :                                                             *
*                                                                             *
********************************************************************************)
(*##*)


interface

uses
  SysUtils, Windows;

function GetTTUnicodeGlyphIndex(Ahdc: HDC; ACh: Word): Word;

function GetTTUnicodeCharCount(Ahdc: HDC): Word;

// ---------------- unicode block definitions and routines ---------------------

type

  TUnicodeBlockDesc = packed record
    b: Byte;
    C0, C1: Word;
    Desc: String[36];
  end;

  TUnicodeBlockDescRange = array[0..83] of TUnicodeBlockDesc;

const
  UnicodeBlockRange: TUnicodeBlockDescRange =
   ((b:0; c0:$0020; c1:$007E; desc: 'Latin basic'),
    (b:1; c0:$00A0; c1:$00FF; desc: 'Latin1 supplement'),
    (b:2; c0:$0100; c1:$017F; desc: 'Latin extended A'),
    (b:3; c0:$0180; c1:$024F; desc: 'Latin extended B'),
    (b:4; c0:$0250; c1:$02AF; desc: 'IPA extensions'),
    (b:5; c0:$02B0; c1:$02FF; desc: 'Spacing modifier letters'),
    (b:6; c0:$0300; c1:$036F; desc: 'Combining diacritical marks'),
    (b:7; c0:$0370; c1:$03FF; desc: 'Greek basic'),
    (b:9; c0:$0400; c1:$04FF; desc: 'Cyrillic'),
    (b:10; c0:$0530; c1:$058F; desc: 'Armenian'),
    (b:11; c0:$0590; c1:$05FF; desc: 'Hebrew basic'),
    (b:13; c0:$0600; c1:$06FF; desc: 'Arabic basic'),
    (b:71; c0:$0700; c1:$074F; desc: 'Syriac'),
    (b:72; c0:$0780; c1:$07BF; desc: 'Thaana'),
    (b:15; c0:$0900; c1:$097F; desc: 'Devanagari'),
    (b:16; c0:$0980; c1:$09FF; desc: 'Bengali'),
    (b:17; c0:$0A00; c1:$0A7F; desc: 'Gurmukhi'),
    (b:18; c0:$0A80; c1:$0AFF; desc: 'Gujarati'),
    (b:19; c0:$0B00; c1:$0B7F; desc: 'Oriya'),
    (b:20; c0:$0B80; c1:$0BFF; desc: 'Tamil'),
    (b:21; c0:$0C00; c1:$0C7F; desc: 'Telugu'),
    (b:22; c0:$0C80; c1:$0CFF; desc: 'Kannada'),
    (b:23; c0:$0D00; c1:$0D7F; desc: 'Malayalam'),
    (b:73; c0:$0D80; c1:$0DFF; desc: 'Sinhala'),
    (b:24; c0:$0E00; c1:$0E7F; desc: 'Thai'),
    (b:25; c0:$0E80; c1:$0EFF; desc: 'Lao'),
    (b:70; c0:$0F00; c1:$0FFF; desc: 'Tibetan'),
    (b:74; c0:$1000; c1:$109F; desc: 'Myanmar'),
    (b:26; c0:$10A0; c1:$10FF; desc: 'Georgian basic'),
    (b:28; c0:$1100; c1:$11FF; desc: 'Hangul Jamo'),
    (b:75; c0:$1200; c1:$137F; desc: 'Ethiopic'),
    (b:76; c0:$13A0; c1:$13FF; desc: 'Cherokee'),
    (b:77; c0:$1400; c1:$167F; desc: 'Canadian aboriginal syllabics'),
    (b:78; c0:$1680; c1:$169F; desc: 'Ogham'),
    (b:79; c0:$16A0; c1:$16FF; desc: 'Runic'),
    (b:80; c0:$1780; c1:$17FF; desc: 'Khmer'),
    (b:81; c0:$1800; c1:$18AF; desc: 'Mongolian'),
    (b:29; c0:$1E00; c1:$1EFF; desc: 'Latin extended additional'),
    (b:30; c0:$1F00; c1:$1FFF; desc: 'Greek extended'),
    (b:31; c0:$2000; c1:$206F; desc: 'General punctuation'),
    (b:32; c0:$2070; c1:$209F; desc: 'Superscripts and subscripts'),
    (b:33; c0:$20A0; c1:$20CF; desc: 'Currency symbols'),
    (b:34; c0:$20D0; c1:$20FF; desc: 'Combining marks for symbols'),
    (b:35; c0:$2100; c1:$214F; desc: 'Letter like symbols'),
    (b:36; c0:$2150; c1:$218F; desc: 'Number forms'),
    (b:37; c0:$2190; c1:$21FF; desc: 'Arrows'),
    (b:38; c0:$2200; c1:$22FF; desc: 'Mathematical operators'),
    (b:39; c0:$2300; c1:$23FF; desc: 'Miscellaneous technical'),
    (b:40; c0:$2400; c1:$243F; desc: 'Control pictures'),
    (b:41; c0:$2440; c1:$245F; desc: 'Optical character recognition'),
    (b:42; c0:$2460; c1:$24FF; desc: 'Enclosed alphanumerics'),
    (b:43; c0:$2500; c1:$257F; desc: 'Box drawing'),
    (b:44; c0:$2580; c1:$259F; desc: 'Block elements'),
    (b:45; c0:$25A0; c1:$25FF; desc: 'Geometric shapes'),
    (b:46; c0:$2600; c1:$26FF; desc: 'Miscellaneous symbols'),
    (b:47; c0:$2700; c1:$27BF; desc: 'Dingbats'),
    (b:82; c0:$2800; c1:$28FF; desc: 'Braille patterns'),
    (b:59; c0:$2E80; c1:$2EFF; desc: 'CJK radicals supplement'),
    (b:59; c0:$2F00; c1:$2FDF; desc: 'Kangxi radicals'),
    (b:59; c0:$2FF0; c1:$2FFF; desc: 'Ideographic description characters'),
    (b:48; c0:$3000; c1:$303F; desc: 'CJK symbols and punctuation'),
    (b:49; c0:$3040; c1:$309F; desc: 'Hiragana'),
    (b:50; c0:$30A0; c1:$30FF; desc: 'Katakana'),
    (b:51; c0:$3100; c1:$312F; desc: 'Bopomofo'),
    (b:52; c0:$3130; c1:$318F; desc: 'Hangul compatibility jamo'),
    (b:53; c0:$3190; c1:$319F; desc: 'Kanbun'),
    (b:51; c0:$31A0; c1:$31BF; desc: 'Bopomofo extended'),
    (b:54; c0:$3200; c1:$32FF; desc: 'Enclosed CJK letters and months'),
    (b:55; c0:$3300; c1:$33FF; desc: 'CJK compatibility'),
    (b:59; c0:$3400; c1:$4DB5; desc: 'CJK unified ideographs extension A'),
    (b:59; c0:$4E00; c1:$9FFF; desc: 'CJK unified ideographs'),
    (b:83; c0:$A000; c1:$A48F; desc: 'Yi syllables'),
    (b:56; c0:$AC00; c1:$D7A3; desc: 'Hangul syllables'),
    (b:57; c0:$D800; c1:$DB7F; desc: 'High surrogates'),
    (b:60; c0:$E000; c1:$F8FF; desc: 'Private use 1'),
    (b:61; c0:$F900; c1:$FAFF; desc: 'CJK compatibility ideographs'),
    (b:62; c0:$FB00; c1:$FB4F; desc: 'Alphabetic presentation forms'),
    (b:63; c0:$FB50; c1:$FDFF; desc: 'Arabic presentation forms A'),
    (b:64; c0:$FE20; c1:$FE2F; desc: 'Combining half marks'),
    (b:65; c0:$FE30; c1:$FE4F; desc: 'CJK compatibility forms'),
    (b:66; c0:$FE50; c1:$FE6F; desc: 'Small form variants'),
    (b:67; c0:$FE70; c1:$FEFE; desc: 'Arabic presentation forms B'),
    (b:69; c0:$FFF0; c1:$FFFD; desc:  'Specials 2'),
    (b:68; c0:$FF00; c1:$FFEF; desc:  'Halfwidth and fullwidth forms'));
    {
    (b:68; c0:$A490; c1:$A4CF; desc: 'Yi radicals'),
    (c0:$DB80; c1:$DBFF; desc: 'High private use surrogates'),
    (c0:$DC00; c1:$DFFF; desc: 'Low surrogates'),
    (c0:$F0000; c1:$FFFFD; desc: 'Private use 2'),
    (c0:$100000; c1:$10FFFD; desc: 'Private use 3'),
    (c0:$FEFF; c1:$FEFF; desc:  'Specials 1'),
    (c0:$10300; c1:$1032F; desc: 'OldItalic'),
    (c0:$10330; c1:$1034F; desc: 'Gothic'),
    (c0:$10400; c1:$1044F; desc: 'Deseret'),
    (c0:$1D000; c1:$1D0FF; desc: 'Byzantine musical symbols'),
    (c0:$1D100; c1:$1D1FF; desc: 'Musicals symbols'),
    (c0:$1D400; c1:$1D7FF; desc: 'Mathematical alphanumeric symbols'),
    (c0:$20000; c1:$2A6D6; desc: 'CJK unified ideographs extension B'),
    (c0:$2F800; c1:$2FA1F; desc: 'CJK compatibility ideographs suppl'),
    (c0:$E0000; c1:$E007F; desc: 'Tags'));
    }

function GetUnicodeRangeByDesc(const ADesc: String; var c0, c1: Cardinal): Boolean;

implementation

{$A1}
type
  TWordArr = array[0..0] of Word;

  TCMap4 = packed record  // From the TrueType Spec. revision 1.66
    format: Word;          // Format number is set to 4.
    length: Word;          // Length in bytes.
    version: Word;         // Version number (starts at 0).
    segCountX2: Word;      // 2 x segCount.
    searchRange: Word;     // 2 x (2**floor(log2(segCount)))
    entrySelector: Word;   // log2(searchRange/2)
    rangeShift: Word;      // 2 x segCount - searchRange
    Arrays: array[0..0] of Word;       // Placeholder symbol for address of arrays following
  end;

  PCMAP4 = ^TCMap4;

  TCMapEncoding = packed record
    PlatformId: Word;
    EncodingId: Word;
    Offset: Cardinal;
  end;

  PCMapEncoding = ^TCMapEncoding;

const
  CMAPHEADERSIZE  = SizeOf(Word) * 2;  // CMAP table Data
  ENCODINGSIZE =  SizeOf(Word) * 2 + SizeOf(DWORD);

var
  dwCmapName: DWORD = $70616d63;// = MAKETABLENAME( 'c', 'm', 'a', 'p' );;

function GetEndCountArray(pBuff: Pointer): PWord;
begin
  Result:= Pointer(Cardinal(pBuff) + 7 * SizeOf(Word));
end;

function GetStartCountArray(pBuff: Pointer): PWord;
var
  segCount: DWORD;
begin
  segCount:= PCMAP4(pBuff)^.segCountX2 div 2;
  // 7 header + 1 reserved Word
  Result:= Pointer(Cardinal(pBuff) +  8 * SizeOf(Word) + segCount * SizeOf(Word));
end;

function GetIdDeltaArray(pBuff: Pointer): PWord;
var
  segCount: DWORD;
begin
  segCount:= PCMAP4(pBuff)^.segCountX2 div 2;
  // 7 header + 1 reserved Word
  Result:= Pointer(Cardinal(pBuff) + 8 * SizeOf(Word) + segCount * 2 * SizeOf(Word));
end;

function GetIdRangeOffsetArray(pBuff: Pointer): PWord;
var
  segCount: DWORD;
begin
  segCount:= PCMAP4(pBuff)^.segCountX2 div 2;
  // 7 header + 1 reserved Word
  Result:= Pointer(Cardinal(pBuff) + 8 * SizeOf(Word) + segCount * 3 * SizeOf(Word));
end;

function SwapLong(ADword: DWORD): DWORD; assembler;
asm
  push ebx;
  mov eax, ADword;
  mov ebx, eax;
  xchg al, ah;
  shl eax, 16
  shr ebx, 16
  xchg bl, bh;
  mov ax, bx;
  pop ebx;
end;

procedure SwapArrays(pFormat4: PCMAP4);
var
  segCount: DWORD;
  i: Integer;
  pGlyphId,
  pEndOfBuffer,
  pstartCount,
  pidDelta,
  pidRangeOffset,
  pendCount: PWord;
begin
  segCount:= pFormat4^.segCountX2 div 2;
  pstartCount:= GetStartCountArray(pFormat4);
  pidDelta:= GetIdDeltaArray(pFormat4);
  pidRangeOffset:= GetIdRangeOffsetArray(pFormat4);
  pendCount:= GetEndCountArray(pFormat4);

  // Swap the array elements for Intel.
  for i:= 0 to segCount - 1 do begin
    pendCount^:= Swap(pendCount^);
    pstartCount^:= Swap(pstartCount^);
    pidDelta^:= Swap(pidDelta^);
    pidRangeOffset^:= Swap(pidRangeOffset^);
    Inc(pendCount);
    Inc(pstartCount);
    Inc(pidDelta);
    Inc(pidRangeOffset);
  end;

  // Swap the Glyph Id array
  pGlyphId:= PWord(Cardinal(pidRangeOffset) + segCount);
  pEndOfBuffer:= PWord(Cardinal(pFormat4) + pFormat4^.length);
  while Cardinal(pGlyphId) < Cardinal(pEndOfBuffer) do begin
    pGlyphId^:= Swap(pGlyphId^);
    Inc(pGlyphId);
  end;
end;

function GetFontEncoding(Ahdc: HDC; ApEncoding: PCMAPENCODING; AiEncoding: Integer): Boolean;
var
  dwResult: DWORD;
begin
  // Get the structure data from the TrueType font
  dwResult:= GetFontData(AHdc, dwCmapName, CMAPHEADERSIZE + ENCODINGSIZE * AiEncoding,
    ApEncoding, SizeOf(TCMAPENCODING));

  Result:= dwResult = SizeOf(TCMAPENCODING);
  // swap the Platform Id for Intel
  ApEncoding^.PlatformId:= Swap(ApEncoding^.PlatformId);
  // swap the Specific Id for Intel
  ApEncoding^.EncodingId:= Swap(ApEncoding^.EncodingId);
  // swap the subtable offset for Intel
  ApEncoding^.Offset:= SwapLong(ApEncoding^.Offset); // swap DWORD
end;

function GetFontFormat4Header(Ahdc: HDC; ApFormat4: PCMAP4; AdwOffset: DWORD): Boolean;
var
  dwResult: DWORD;
  i: Integer;
  pField: PWord;
begin
  // Loop and Alias a writeable pointer to the field of interest
  pField:= PWord(ApFormat4);

  Result:= True;
  i:= 0;
  while i < 7 do begin
    // Get the field from the subtable
    dwResult:= GetFontData(AHdc, dwCmapName, AdwOffset + SizeOf(Word) * i,
      pField, SizeOf(Word));
    pField^:= Swap(pField^);   // swap it to make it right for Intel.
    Inc(pField); // move on to the next 2 bytes
    Inc(i);
    Result:= Result and (dwResult = SizeOf(Word)); // accumulate our success
  end;
end;

function GetFontFormat4Subtable(AHdc: HDC; ApFormat4Subtable: PCMAP4; AdwOffset: DWORD): Boolean;
var
  dwResult: DWORD;
  len: Word;
begin
  Result:= False;
  // Retrieve the header values in swapped order
  if (not GetFontFormat4Header(Ahdc, ApFormat4Subtable, AdwOffset)) then begin
    Exit;
  end;
  // Get the rest of the table
  len:= ApFormat4Subtable^.length - (7 * SizeOf(Word));
  dwResult:= GetFontData(Ahdc, dwCmapName, AdwOffset + 7 * SizeOf(Word),      // pos of arrays
    @(ApFormat4Subtable^.Arrays[0]), len);

  if (dwResult <> len) then begin
    Exit;  // We really shouldn't ever get here
  end;
  // Swamp the arrays
  SwapArrays(ApFormat4Subtable);
  Result:= TRUE;
end;

{  Count the # of glyphs
   ApFormat4 - pointer to a valid Format4 subtable
}
function GetFontFormat4CharCount(ApFormat4: PCMAP4): Word;
var
  i,
  idResult, // Intermediate id calc.
  ch: Word;
  pendCount,
  pstartCount,
  idRangeOffset: PWord;
begin
  pendCount:= GetEndCountArray(ApFormat4);
  pstartCount:= GetStartCountArray(ApFormat4);
  idRangeOffset:= GetIdRangeOffsetArray(ApFormat4 );
  Result:= 0;
  if (ApFormat4 = Nil) then begin
    Exit;
  end;
  // by adding up the coverage of each segment
  i:= 0;
  while i < ApFormat4^.segCountX2 div 2 do begin
    if idRangeOffset^ = 0 then begin
      // if per the TT spec, the idRangeOffset element is zero,
      // all of the characters in this segment exist.
      Inc(Result, pendCount^ - pstartCount^ + 1);
    end else begin
      // otherwise we have to test for glyph existence for
      // each character in the segment.
      for ch:= pstartCount^ to pendCount^ do begin
        // determine if a glyph exists
        idResult:= PWord(idRangeOffset^ div 2 + (ch - pstartCount^) + Cardinal(idRangeOffset))^;  // indexing equation from TT spec
        if (idResult <> 0)
        then Inc(Result);  // Yep, count it.
      end;
    end;
    Inc(pendCount);
    Inc(pstartCount);
    Inc(idRangeOffset);
    Inc(i);
  end;
end;

{
  if cbSize is to small or zero, or if pBuffer is NULL the function
  will fail and return the required buffer size in *pcbNeeded.
  if another error occurs, the function will fail and *pcbNeeded will be zero.
  When the function succeeds, *pcbNeeded contains the number of bytes copied to pBuffer.

  hdc        DC with TT font
  pBuffer    Properly allocated buffer
  cbSize     Size of properly allocated buffer
  pcbNeeded  Size of buffer needed
}

function GetTTUnicodeCoverage(AHdc: HDC; ApBuffer: PCMAP4; AcbSize: DWORD; ApcbNeeded: PDWORD): Boolean;
var
  nEncodings: Word;         // # of encoding in the TT font
  Encoding: TCMAPENCODING;  // The current encoding
  i,
  iUnicode,                 // The Unicode encoding
  dwResult: DWORD;
  Format4: TCMAP4;          // Unicode subtable format
  pFormat4Subtable: PCMAP4;  // Working buffer for subtable
begin
  Result:= FALSE;
  ApcbNeeded^:= 0;
  // Get the number of subtables in the CMAP table from the CMAP header
  // The # of subtables is the second Word in the CMAP table, per the TT Spec.
  dwResult:= GetFontData(Ahdc, dwCmapName, SizeOf(Word), @nEncodings, SizeOf(Word) );
  nEncodings:= Swap(nEncodings);
  if (dwResult <> SizeOf(Word)) then begin
    // We probably got GDI_ERROR back, this means that the Device Context does not have a TrueType font selected into it.
    Exit;
  end;

  // Get the encodings and look for a Unicode Encoding
  iUnicode:= nEncodings;
  i:= 0;
  while i < nEncodings do begin
    // Get the encoding entry for each encoding
    if (not GetFontEncoding(Ahdc, @Encoding, i)) then begin
      Exit;
    end;

    // Take note of the Unicode encoding.
    //
    // A Unicode encoding per the TrueType specification has a
    // Platform Id of 3 and a Platform specific encoding id of 1
    // Note that Symbol fonts are supposed to have a Platform Id of 3
    // and a specific id of 0. If the TrueType spec. suggestions were
    // followed then the Symbol font's Format 4 encoding could also
    // be considered Unicode because the mapping would be in the
    // Private Use Area of Unicode. We assume this here and allow
    // Symbol fonts to be interpreted. If they do not contain a
    // Format 4, we bail later. If they do not have a Unicode
    // character mapping, we'll get wrong results.
    // Code could infer from the coverage whether 3-0 fonts are
    // Unicode or not by examining the segments for placement within
    // the Private Use Area Subrange.
    if (Encoding.PlatformId = 3) and ((Encoding.EncodingId = 1) or (Encoding.EncodingId = 0)) then begin
      iUnicode:= i;       // Set the index to the Unicode encoding
      Break;
    end;
    Inc(i);
  end;

  // index out of range means failure to find a Unicode mapping
  if (iUnicode >= nEncodings) then begin
    // No Unicode encoding found.
    Exit;
  end;

  // Get the header entries(first 7 USHORTs) for the Unicode encoding.
  if (not GetFontFormat4Header(Ahdc, @Format4, Encoding.Offset)) then begin
    Exit;
  end;

  // Check to see if we retrieved a Format 4 table
  if (Format4.format <> 4) then begin
    // Bad, subtable is not format 4 - happen if the font is corrupt or if there is a new font format we don't understand.
    Exit;
  end;

  // Figure buffer size and tell caller if buffer to small
  ApcbNeeded^:= Format4.length;
  if (ApcbNeeded^ > AcbSize) or (ApBuffer = Nil) then begin
    // Either test indicates caller needs to know the buffer size and the parameters are not setup to continue.
    Exit;
  end;

  // allocate a full working buffer
  GetMem(pFormat4Subtable, Format4.length);
  if (pFormat4Subtable = Nil) then begin
    // Bad things happening if we can't allocate memory
    Exit;
  end;

  // get the entire subtable
  if (not GetFontFormat4Subtable(AHdc, pFormat4Subtable, Encoding.Offset)) then begin
    // Bad things happening if we can't allocate memory
    Exit;
  end;

  // Copy the retrieved table into the buffer
  Move(pFormat4Subtable^, ApBuffer^, pFormat4Subtable^.length);

  FreeMem(pFormat4Subtable);
  Result:= TRUE;
end;

{
  if the Unicode character ch is not contained in one of the
  segments the function returns FALSE.
  if the Unicode character ch is found in a segment, the index
  of the segment is placed in*piSeg and the function returns TRUE.

  ApTable - a valid Format4 subtable buffer
  ch      - Unicode character to search for
  piSeg   - out: index of segment containing ch
}
{$RANGECHECKS OFF}
function FindFormat4Segment(ApTable: PCMAP4; ACh: Word; ApiSeg: PWord): Boolean;
var
  i,
  segCount: Word;
  pendCount,
  pstartCount: PWord;
begin
  Result:= FALSE;
  segCount:= ApTable^.segCountX2 div 2;
  pendCount:= GetEndCountArray(ApTable);
  pstartCount:= GetStartCountArray(ApTable);

  // Find segment that could contain the Unicode character code
  i:= 0;
  while (i < segCount) and (pendCount^ < ACh) do begin
    Inc(i);
    Inc(pendCount);
  end;
  // We looked in them all, ch not there, or character code not within the range of the segment
  if (i >= segCount) or (TWordArr(pstartCount^)[i] > ACh) then begin
    Exit;
  end;

  // this segment contains the character code
  ApiSeg^:= i;
  Result:= TRUE;
end;

{ Returns the number of Unicode character glyphs that are in the TrueType font that is selected into the hdc.
}
function GetTTUnicodeCharCount(AHdc: HDC): Word;
var
  pUnicodeCMapTable: PCMAP4;
  cChar: Word;
  dwSize: DWORD;
begin
  // Get the Unicode CMAP table from the TT font
  // dwSize:= 0;
  GetTTUnicodeCoverage(AHdc, Nil, 0, @dwSize);
  GetMem(pUnicodeCMapTable, dwSize);
  if not GetTTUnicodeCoverage(AHdc, pUnicodeCMapTable, dwSize, @dwSize) then begin
    // possibly no Unicode cmap, not a TT font selected,...
    FreeMem(pUnicodeCMapTable);
    Result:= 0;
    Exit;
  end;

  cChar:= GetFontFormat4CharCount(pUnicodeCMapTable);
  FreeMem(pUnicodeCMapTable);
  Result:= cChar;
end;

{
  When the TrueType font contains a glyph for ch, the function returns the glyph index for that character.
  If an error occurs, or there is no glyph for ch, the function will return the missing glyph index of zero.

  hdc - DC with a TrueType font selected
  ch  - Unicode character to convert to Index
}
function GetTTUnicodeGlyphIndex(AHDc: HDC; ACh: Word): Word;
var
  pUnicodeCMapTable: PCMAP4;
  dwSize: DWORD;
  iSegment: Word;
  idResult,    // Intermediate id calc.
  idRangeOffset,
  idDelta,
  startCount: PWord;
begin
  Result:= 0;     // Initialize to missing glyph
  // How big a buffer do we need for Unicode CMAP?
  GetTTUnicodeCoverage(AHdc, Nil, 0, @dwSize );
  GetMem(pUnicodeCMapTable, dwSize);
  if not GetTTUnicodeCoverage(AHdc, pUnicodeCMapTable, dwSize, @dwSize) then begin
    // Either no Unicode cmap, or some other error occurred like font in DC is not TT.
    FreeMem(pUnicodeCMapTable);
    Exit;  // return missing glyph on error
  end;

  // Find the cmap segment that has the character code.
  if not FindFormat4Segment(pUnicodeCMapTable, ACh, @iSegment) then begin
    FreeMem(pUnicodeCMapTable);
    Exit;       // ch not in cmap, return missing glyph
  end;

  // Get pointers to the cmap data
  idRangeOffset:= GetIdRangeOffsetArray(pUnicodeCMapTable);
  idDelta:= GetIdDeltaArray(pUnicodeCMapTable);
  startCount:= GetStartCountArray(pUnicodeCMapTable);

  // Per TT spec, if the RangeOffset is zero,
  if (TWordArr(idRangeOffset^)[iSegment] = 0) then begin
    // calculate the glyph index directly
    Result:= (TWordArr(idDelta^)[iSegment] + ACh) mod $10000;
  end else begin
    // otherwise, use the glyph id array to get the index
    idResult:= Pointer(TWordArr(idRangeOffset^)[iSegment] div 2 +
      (ACh - TWordArr(startCount^)[iSegment]) +  Cardinal(@(TWordArr(idRangeOffset^)[iSegment])));
    // indexing equation from TT spec
    if (idResult <> Nil)
    then Result:= (TWordArr(idDelta^)[iSegment] + Cardinal(idResult)) mod $10000; // Per TT spec, nonzero means there is a glyph
    // otherwise, return the missing glyph
  end;
  FreeMem(pUnicodeCMapTable);
end;

// ---------------- unicode block definitions and routines ---------------------

function GetUnicodeRangeByDesc(const ADesc: String; var c0, c1: Cardinal): Boolean;
var
  i: Integer;
begin
  Result:= False;
  for i:= Low(UnicodeBlockRange) to High(UnicodeBlockRange) do begin
    if CompareText(ADesc, UnicodeBlockRange[i].Desc) = 0 then begin
      c0:= UnicodeBlockRange[i].C0;
      c1:= UnicodeBlockRange[i].C1;
      Result:= True;
      Exit;
    end;
  end;
end;

end.
