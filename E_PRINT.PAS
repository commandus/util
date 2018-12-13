unit
  E_print;
(*##*)
(*******************************************************************
*                                                                 *
*   E  _  P  R  I  N  T    Simple print routines                   *
*                          Part of HIRE                           *
*   Copyright (c) 1998, A.Ivanov. All rights reserved.             *
*   Delphi 2, 3 only (uses huge strings)                          *
*                                                                  *
*   Nice printing of some VCL visible objects (CheckBox, etc)     *
*                                                                  *
*                                                                 *
*   Conditional defines:                                           *
*                                                                 *
*   Last Revision: Apr 19 1998                                     *
*   Last fix     : Apr 21 1998                                    *
*   Lines        :                                                 *
*   History      : see CHANGES.TXT file                           *
*   Printed      : ---                                             *
*                                                                 *
********************************************************************)
(*##*)

interface
uses
  Printers, Graphics, Grids, WinTypes, Classes, StdCtrls;

type
  PrinterStates= set of (ps_fontselected, ps_inprogress, ps_tofile,
    ps_rubberX, ps_rubberY);
{ ps_rubberX, ps_rubberY - expand printing area to paper size
}
const
  PrinterState: PrinterStates = [];

procedure BeginPrint(const ATitle: String; AFont: TFont);
procedure EndPrint;
function  NextLine: Word;
procedure PrintLine(S: String);
procedure PrintSG(SG: TStringGrid);
procedure PrintCheckBox(ChBox: TCheckBox);

implementation
uses
  Forms;

var
  Line: Word;
  LineH: Integer;
  Px, Py: Word;
  PpageX, PpageY: Word;
  LeftMargin, TopMargin: Word;
  ScreenToPrinterScale: Word;

procedure BeginPrint(const ATitle: String; AFont: TFont);

procedure SetFont;
begin
  with Printer. Canvas do begin
    Font:= AFont;
    { change only one - or size, or height!
    }
    Font. Height:= Font. Height * ScreenToPrinterScale;
    LineH:= Abs(Font. Height);
  end;
end;

begin
  if ps_inProgress in PrinterState then begin
    SetFont;
    Exit;
  end;
  Include(PrinterState, ps_inProgress);
  { wisiwig
  }
  ScreenToPrinterScale:= Printer. PageWidth div Screen.Width;
  if ATitle <> ''
  then Printer.Title:= ATitle;
  SetFont;
  { start
  }
  with Printer, Canvas do begin
    { set print properties
    }
    with Printer, Canvas, Brush do begin
      Bitmap:= Nil;
      Color:= clWhite;
      Style:= bsClear;
    end;

    { print controls
    }
    Line:= 0;
    Px:= LeftMargin;
    Py:= TopMargin;

    LeftMargin:= 20;
    TopMargin:= 100;
    Ppagex:= PageWidth;
    Ppagey:= PageHeight;

    BeginDoc;
  end;
end;

function NextLine: Word;
begin
  Inc(Line);
  Px:= LeftMargin;
  Py:= Py + LineH;
  if Py > PpageY then begin
    Py:= TopMargin;
    Printer. NewPage;
  end;
  NextLine:= Py;
end;

procedure PrintLine(S: String);
begin
  Printer. Canvas. TextOut(LeftMargin, NextLine, S);
end;

function RubberCoef(SG: TStringGrid): Integer;
begin
  if (ps_rubberX in PrinterState) then begin
    RubberCoef:= (Printer. PageWidth - LeftMargin) div SG. GridWidth;
  end else begin
    RubberCoef:= ScreenToPrinterScale;
  end;
end;

procedure PrintSG(SG: TStringGrid);
var
  ccol, crow: LongInt;
  cx: Word;
  R: TRect;

procedure PrintSGCell(X: Integer; S: String; R: TRect);
begin
  Printer. Canvas. TextRect(R, X * RubberCoef(SG)+LeftMargin, Py, S);
end;

begin
  with SG do begin
    if (RowCount = 0) or (ColCount = 0)
    then Exit;
    Printer. Canvas. Font. Style:= [fsBold, fsItalic];
    PrintLine(Name);
    Printer. Canvas. Font. Style:= [fsItalic];
    PrintLine(Hint);
    PrintLine('');
    BeginPrint('', Font);
    for crow:= 0 to RowCount - 1 do begin
      cx:= 0;
      for ccol:= 0 to ColCount - 1 do begin
        if ccol = 0 then with r do begin
          R:= Rect(LeftMargin, Py,
            LeftMargin + ColWidths[0] * RubberCoef(SG), Py + LineH);
          Printer. Canvas. Rectangle(left, top, right, bottom);
          PrintSGCell(0, Cells[0, crow], R);
          Inc(cx, ColWidths[0]);
        end else begin
          R:= Rect(Cx * RubberCoef(SG) + LeftMargin, Py,
            (Cx + ColWidths[ccol]) * RubberCoef(SG) + LeftMargin, Py + LineH);
          Printer. Canvas. Rectangle(r.left, r.top, r.right, r.bottom);
          PrintSGCell(cx, Cells[ccol, crow], R);
          Inc(cx, ColWidths[ccol]);
        end;
      end;
      PrintLine('');
    end;
  end;
end;

procedure PrintCheckBox(ChBox: TCheckBox);
var
  R: TRect;
  x, y: Integer;
begin
  with ChBox do begin
    BeginPrint('', Font);
    x:= LeftMargin + LineH;
    y:= Py + LineH;
    R:= Rect(LeftMargin, Py, x, y);
    { rectangle
    }
    Printer. Canvas. Rectangle(LeftMargin, Py,
      LeftMargin + LineH, y);
    if Checked then with Printer. Canvas do begin
      Moveto(LeftMargin, Py);
      Lineto(LeftMargin+LineH, y);
      Moveto(LeftMargin+LineH, Py);
      Lineto(LeftMargin, y);
    end;
    { text
    }
    R:= Rect(x + LineH, Py, PPageX - 1, y);
    Printer. Canvas. Font. Style:= [fsItalic];
    Printer. Canvas. TextRect(R, LeftMargin + 2 * LineH, Py,
      Caption + '  ' + Hint);
    NextLine;
  end;
end;

procedure EndPrint;
begin
  { fine
  }
  with Printer do begin
    EndDoc;
    Exclude(PrinterState, ps_inProgress);
  end;
end;

end.
