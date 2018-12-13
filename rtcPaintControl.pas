unit
  rtcPaintControl;

interface

uses
  Windows, Messages, Graphics, Classes,
  Controls, StdCtrls, ExtCtrls, SysUtils,
  Forms, Dialogs,
  GDIPl2, GDIPlHelper;

type
  {
  Used for "editing text on canvas"
  }
  TrtcTransparentEdit = class (TEdit)
  private
//    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
  protected
//    procedure SetOnControl(Value: TControl); virtual;
    procedure CreateParams(var Params: TCreateParams); override;
  end;

  // like speed button
  TrtcPanelButton = class(TPanel) // TButton
  private
    FGlyph: TBitmap;
    FCanvas: TCanvas;
    FTextRect: TRect;
    FChecked: Boolean;
    procedure SetChecked(AValue: Boolean);
    procedure SetGlyph(const Value: TBitmap);
    function GetBackColor: TColor;
    procedure SetBackColor(AValue: TColor);
    procedure CNDrawItem(var Msg: TWMDrawItem);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure WndProc(var Msg: TMessage); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Canvas: TCanvas read FCanvas write FCanvas;
  published
    property Checked: Boolean read FChecked write SetChecked;
    property Glyph: TBitmap read FGlyph write SetGlyph;
    property BackColor: TColor read GetBackColor write SetBackColor;
  end;

  // button swaps colors
  TrtcSwapPanelButton = class (TrtcPanelButton)
  protected
    procedure WndProc(var Msg: TMessage); override;
  end;

  // button make colors deafult
  TrtcDefaultPanelButton = class (TrtcPanelButton)
  protected
    DefForeColor: TColor;
    DefBackColor: TColor;
    procedure WndProc(var Msg: TMessage); override;
  end;

  // color picker with fore and back colors shown
  TrtcColorPicker = class(TCustomPanel)
  private
    FForeColorRect: TRect;
    FBackColorRect: TRect;
    FButtonForeground: TrtcPanelButton;
    FButtonBackground: TrtcPanelButton;
    FButtonSwapColors: TrtcSwapPanelButton;
    FButtonDefaultColors: TrtcDefaultPanelButton;
    FDialogOptions: TColorDialogOptions;
    FFlat: Boolean;
    FDefaultForeColor,
    FDefaultBackColor: TColor;
    function GetForeColor: TColor;
    function GetBackColor: TColor;
    procedure SetForeColor(const AValue: TColor);
    procedure SetBackColor(const AValue: TColor);
    procedure SetFlatButton(const AValue: Boolean);
  protected
    function DoColorSelection(AFore: Boolean = True): Boolean;
    procedure ButtonForegroundClicked(Sender: TObject);
    procedure ButtonBackgroundClicked(Sender: TObject);
    procedure ButtonSwapClicked(Sender: TObject);
    procedure ButtonDefaultClicked(Sender: TObject);
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetBounds(ALeft,ATop,AWidth,AHeight: Integer); override;
  published
    property DefaultForeColor: TColor read FDefaultForeColor write FDefaultForeColor default clRed;
    property DefaultBackColor: TColor read FDefaultBackColor write FDefaultBackColor default clWhite;
    property ForeColor: TColor read GetForeColor write SetForeColor default clRed;
    property BackColor: TColor read GetBackColor write SetBackColor default clWhite;
    property DialogOptions: TColorDialogOptions read FDialogOptions write FDialogOptions;
    property FlatButton: Boolean read FFlat write SetFlatButton;
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property BevelInner;
    property BevelOuter;
    property BevelWidth;
    property BiDiMode;
    property BorderWidth;
    property BorderStyle;
    property Color;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FullRepaint;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnCanResize;
    property OnClick;
    property OnConstrainedResize;
{$IFDEF D5Up}
    property OnContextPopup;
{$ENDIF}
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDrag;
  end;

  // select pen style
  TrtcPenCombobox = class(TCustomComboBox)
  protected
    procedure DrawItem(AIndex: Integer; ARect: TRect; AState: TOwnerDrawState); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  // select pen thickness
  TrtcPenWidthCombobox = class(TCustomComboBox)
  protected
    procedure DrawItem(AIndex: Integer; ARect: TRect; AState: TOwnerDrawState); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  // seelct brush style
  TrtcBrushCombobox = class(TCustomComboBox)
  protected
    procedure DrawItem(AIndex: Integer; ARect: TRect; AState: TOwnerDrawState); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  // array of buttons used in radiogroup
  TrtcPanelButtons = array of TrtcPanelButton;

  // radiobuttons panel with 1 or more columns
  TrtcPanelRadioButtonGroup = class(TPanel)
  private
    FColumns: Integer;
    FImageList: TImageList;
    FSelectedIndex: Integer;
    FButtons: TrtcPanelButtons;
    function GetItem(AIndex: Integer): TrtcPanelButton;
    function GetSelected: TrtcPanelButton;
    procedure SetColumns(AValue: Integer);
    procedure SetImageList(AValue: TImageList);
  protected
    procedure Rearrange;
    procedure ToolClick(Sender: TObject);
  public
    ColorPicker: TrtcColorPicker;
    constructor Create(AOwner: TComponent); override;
    procedure Clear;
    procedure Add(const ACaption, AHint: String; ABitmap: TBitmap);
    property Items[AIndex: Integer]: TrtcPanelButton read GetItem;
    property Selected: TrtcPanelButton read GetSelected;
    property SelectedIndex: Integer read FSelectedIndex;
    property Columns: Integer read FColumns write SetColumns;
    property ImageList: TImageList read FImageList write SetImageList;
  end;

  // layers of edtied image
  TBitmaps = array of Graphics.TBitmap;

  // simple paint control panel like paintbrush
  // controls ControlImage TPanel or TImage (need WM_PAINT so use TDrawPanel instead)

  TrtcPaintControl = class(TPanel)
  private
    FLayerBitmaps: TBitmaps;
    FCurrentLayerIndex: Integer;
    FDrawing: Boolean;
    FOrigin, FDestination: TPoint;
    FPreviewOrigin, FPreviewDestination: TPoint;
    FControlImage: TControl;
    FControlCanvas: TControlCanvas;
    FEdit: TrtcTransparentEdit;
    FScale: Integer;
    procedure SetControlImage(AValue: TControl);
    procedure ResizeLayerBitmaps(AWidth, AHeight: Integer);
    procedure DrawShape(AToolIndex: Integer; ACanvas: TCanvas; ATopLeft, ABottomRight: TPoint;
      AMode: TPenMode);
    procedure DoEditExit(ASender: TObject);
    function GetBackgroundBitmap: TBitmap;
    procedure SetBackgroundBitmap(AValue: TBitmap);
    procedure SetScale(AValue: Integer);
  protected
    procedure AddLayer;
    procedure ClearLayers;
    procedure DoMouseDown(ASender: TObject; AButton: TMouseButton;
      AShift: TShiftState; AX, AY: Integer);
    procedure DoMouseUp(ASender: TObject; AButton: TMouseButton;
      AShift: TShiftState; AX, AY: Integer);
    procedure DoMouseMove(ASender: TObject; AShift: TShiftState; AX, AY: Integer);
    procedure SyncLayers;
  public
    Tools: TrtcPanelRadioButtonGroup;
    PenWidthCombobox: TrtcPenWidthCombobox;
    ColorPicker: TrtcColorPicker;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SaveLayers();
    property LayerBitmaps: TBitmaps read FLayerBitmaps;
    property Background: TBitmap read GetBackgroundBitmap write SetBackgroundBitmap;
    property IsDrawing: Boolean read FDrawing;
  published
    property ControlImage: TControl read FControlImage write SetControlImage;
    property Scale: Integer read FScale write SetScale default 100; // %
  end;

  TrtcDrawPanel = class(TPanel)
  private
    FPaintControl: TrtcPaintControl;
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property PaintControl: TrtcPaintControl read FPaintControl write FPaintControl;
  end;

implementation

procedure CheckRectCoords(var ARect: TRect);
var
  v: Integer;
begin
  // swap rectangle coordinates
  if ARect.Right < ARect.Left then begin
    v:= ARect.Left;
    ARect.Left:= ARect.Right;
    ARect.Right:= v;
  end;
  if ARect.Bottom < ARect.Top then begin
    v:= ARect.Top;
    ARect.Top:= ARect.Bottom;
    ARect.Bottom:= v;
  end;
end;

{ ------------------------------ TrtcTransparentEdit ------------------------- }
{
procedure TrtcTransparentEdit.WMPaint(var Message: TWMPaint);
var
  DC: THandle;
  PS: TPaintStruct;
begin
  if not Invisible then
  begin
    if Message.DC = 0 then
      DC := BeginPaint(Handle, PS)
    else
      DC := Message.DC;

    PatBlt(DC, 0, 0, 5, 5, BLACKNESS);
    PatBlt(DC, Width - 6, 0, 5, 5, BLACKNESS);
    PatBlt(DC, 0, Height - 6, 5, 5, BLACKNESS);
    PatBlt(DC, Width - 6, Height - 6, 5, 5, BLACKNESS);

    if Message.DC = 0 then
      EndPaint(Handle, PS);
  end;
end;
}
{
procedure TrtcTransparentEdit.SetOnControl(Value: TControl);
var
  Rect: TRect;
begin
  if Value <> fOnControl then
  begin
    if Invisible and (Parent <> nil) then
      Parent.Perform(WM_SETREDRAW, 0, 0);

    if fOnControl <> nil then
      Visible := False;

    if Value <> nil then
    begin
      Rect := Value.BoundsRect;
      InflateRect(Rect, 2, 2);
      BoundsRect := Rect;
    end;
    fOnControl := Value;

    if fOnControl <> nil then
      Visible := True;

    if Invisible and (Parent <> nil) then
      Parent.Perform(WM_SETREDRAW, 1, 0);
  end;
end;
}

procedure TrtcTransparentEdit.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.ExStyle := Params.ExStyle or WS_EX_TRANSPARENT;
end;

procedure DrawBitmapText(ACanvas: TCanvas; ADrawRect: TRect; ABitmapRect: TRect;
  const ACaption: String; ATextAlign: TAlignSet;
  AColor: TColor; ABitmap: TBitmap);
var
  TxtRect: TRect;
  SavedFontHeight,
  BmpHeight,
  BmpWidth,
  BmpLeft,
  BmpTop: Integer;
  TxtPos: Cardinal;
begin
  BmpHeight := 0;
  BmpWidth := 0;
  TxtRect:= ADrawRect;
  if Assigned(ABitmap) then begin
    with ABitmap do begin
      if not Empty then begin
        Transparent:= True;
        BmpHeight:= Height;
        BmpWidth:= Width;
      end;
    end;
  end;

  if Assigned(ABitmap) then begin
    BmpTop:= ABitmapRect.Top + (((ABitmapRect.Bottom - ABitmapRect.Top) - BmpHeight) div 2);
    BmpLeft:= ABitmapRect.Left + (((ABitmapRect.Right - ABitmapRect.Left) - BmpWidth) div 2);
      //BmpLeft := DrawRect.Left + (((DrawRect.Right - DrawRect.Left) - BmpWidth) div 2) + 1;
//    TxtRect.Left:= BmpLeft + BmpWidth + 2;
    //if Bitmap.Height <= txtRect.Bottom-txtRect.Top then
    ACanvas.Draw(BmpLeft, BmpTop, ABitmap);
  end;

  //  or DT_VCENTER
  // DrawText() Format Flags
  // alNone, alTop, alBottom, alLeft, alRight, alClient, alCustom
  TxtPos:= 0;
  if alLeft in ATextAlign then TxtPos:= TxtPos or DT_LEFT;
  if alRight in ATextAlign then TxtPos := TxtPos or DT_RIGHT;
  if alClient in ATextAlign then TxtPos := TxtPos or DT_CENTER or DT_VCENTER;

  if alTop in ATextAlign then TxtPos:= TxtPos or DT_RIGHT;
  if alBottom in ATextAlign then TxtPos:= TxtPos or DT_BOTTOM;

  SetBkMode(ACanvas.Handle, TRANSPARENT);

  SavedFontHeight:= ACanvas.Font.Size;
  // if (TxtRect.Bottom-TxtRect.Top > 0) and (DrawCanvas.TextHeight(Caption) > TxtRect.Bottom-TxtRect.Top) then
  //   DrawCanvas.Font.Height:= TxtRect.Bottom-TxtRect.Top-1;
  ACanvas.Font.Color:= clBlack;
  // ACanvas.Brush.Color:= clred;
  DrawText(ACanvas.Handle, PChar(ACaption), Length(ACaption), TxtRect, txtPos or DT_SINGLELINE or DT_END_ELLIPSIS);
  ACanvas.Font.Size:= SavedFontHeight;
end;

procedure DrawTextAngle(ACanvas: TCanvas;
  ARect: TRect; AOptions: Cardinal; const AStr: String; const AAngle: Integer);
var
  logfont: TLogFont;
  font: Thandle;
begin
  with LogFont do begin
    lfHeight:= ACanvas.Font.Height;
    lfWidth := 0;
    lfEscapement:= AAngle * 10;
    lfOrientation := 0;
    if fsBold in ACanvas.Font.Style then
      lfWeight:= FW_BOLD
    else
      lfWeight:= FW_NORMAL;
    lfItalic:= Byte(fsItalic in ACanvas.Font.Style);
    lfUnderline:= Byte(fsUnderline in ACanvas.Font.Style);
    lfStrikeOut:= Byte(fsStrikeOut in ACanvas.Font.Style);
    lfCharSet:= DEFAULT_CHARSET;
    StrPcopy(lfFaceName, ACanvas.Font.Name);
    lfQuality:= DEFAULT_QUALITY;
    lfOutPrecision:= OUT_DEFAULT_PRECIS;
    lfClipPrecision:= CLIP_DEFAULT_PRECIS;
    case ACanvas.Font.Pitch of
      fpVariable: lfPitchAndFamily:= VARIABLE_PITCH;
      fpFixed: lfPitchAndFamily:= FIXED_PITCH;
    else
        lfPitchAndFamily:= DEFAULT_PITCH;
    end;
  end;
  font:= Createfontindirect(logfont);
  SelectObject(ACanvas.handle, font);
  DrawText(ACanvas.Handle, PChar(AStr), Length(AStr), ARect, AOptions);
  DeleteObject(font);
end;

{ ------------------------------ TrtcPanelButton ----------------------------- }

procedure TrtcPanelButton.CNDrawItem(var Msg: TWMDrawItem);
begin
  inherited;
end;

constructor TrtcPanelButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FChecked:= False;
  FGlyph:= TBitmap.Create;
  FCanvas:= TCanvas.Create;
end;

procedure TrtcPanelButton.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
end;

destructor TrtcPanelButton.Destroy;
begin
  FGlyph.Free;
  FGlyph:= Nil;
  FCanvas.Free;
  FCanvas:= Nil;
  inherited;
end;

procedure TrtcPanelButton.SetGlyph(const Value: TBitmap);
begin
   FGlyph.Assign(Value);
  //if Glyph <> Nil then
  //  SendMessage(Handle,BM_SETIMAGE,IMAGE_BITMAP,Glyph.Handle);
end;

function TrtcPanelButton.GetBackColor: TColor;
begin
  Result:= Brush.Color;
end;

procedure TrtcPanelButton.SetChecked(AValue: Boolean);
var
  v: Boolean;
begin
  v:= FChecked = AValue;
  if not v then begin
    FChecked:= AValue;
    Invalidate;
  end;
end;

procedure TrtcPanelButton.SetBackColor(AValue: TColor);
var
  v: Boolean;
begin
  v:= Brush.Color = AValue;
  Brush.Color:= AValue;

  if not v
  then Invalidate;
end;

procedure TrtcPanelButton.WndProc(var Msg: TMessage);
var
  TxtWidth: Integer;
begin
  inherited WndProc(Msg);
  if (msg.Msg = WM_PAINT) then begin
    FCanvas.Handle:= GetDC(Handle);
    FCanvas.Font.Assign(Font);
    FCanvas.Brush.Color:= BackColor;
    FTextRect:= ClientRect;
    FCanvas.FillRect(FTextRect);
    with FCanvas.Pen do begin
      Color:= clWhite;
      Width:= 1;
      Mode:= pmCopy;
      Style:= psSolid;
    end;
    if FChecked then begin
      FCanvas.Rectangle(FTextRect);
      FCanvas.Pen.Color:= clSilver;
      FCanvas.MoveTo(0, 0);
      FCanvas.LineTo(0, FTextRect.Bottom);
      FCanvas.MoveTo(0, 0);
      FCanvas.LineTo(FTextRect.Right, 0);
    end else begin
    end;

    if Length(Caption) > 0 then begin
      TxtWidth:= FCanvas.TextWidth(Caption);
      FTextRect.Left:= (Width - TxtWidth) div 2;
      FTextRect.Right:= FTextRect.Left + TxtWidth;
    end;
    // inherited;

    DrawBitmapText(FCanvas, FTextRect, ClientRect, Caption, [alClient], BackColor, Glyph);
    ReleaseDC(Handle, FCanvas.Handle);
    end
  else;
end;

procedure TrtcSwapPanelButton.WndProc(var Msg: TMessage);
var
  TxtWidth,
  TextAndBmpWidth: Integer;
  glyphWidth: Integer;
  w: Integer;
begin
  inherited;
  if (msg.Msg = WM_PAINT) then begin
    FCanvas.Handle:= GetDC(Handle);
    FCanvas.Font.Assign(Font);
    FCanvas.Brush.Color:= BackColor;
    FTextRect:= ClientRect;
    FCanvas.FillRect(FTextRect);

    if Length(Caption) > 0 then begin
      TxtWidth:= FCanvas.TextWidth(Caption);
      if Assigned(Glyph) and (not Glyph.Empty) then
        glyphWidth:= Glyph.Width
      else begin
       glyphWidth:= 0;
      end;
      TextAndbmpWidth:= TxtWidth + glyphWidth;
      FTextRect.Left:= (Width - TextAndBmpWidth) div 2;
      FTextRect.Right:= FTextRect.Left + TextAndBmpWidth;
    end;
    // inherited;
    //DrawBitmapText(FCanvas, FTextRect, Caption, [alClient], BackColor, Nil);
    w:= (FTextRect.Right - FTextRect.Left) div 2;
    Dec(FTextRect.Left, w);
    Dec(FTextRect.Top, w);
    Inc(FTextRect.Right, w);
    Inc(FTextRect.Bottom, w);
    DrawTextAngle(Canvas, FTextRect,
      DT_SINGLELINE or DT_END_ELLIPSIS or DT_CENTER or DT_VCENTER,
      Caption, 0);
    ReleaseDC(Handle, FCanvas.Handle);
    end
  else;
end;

procedure TrtcDefaultPanelButton.WndProc(var Msg: TMessage);
var
  ow, oh: Integer;
  Rect: TRect;
begin
  inherited WndProc(Msg);
  if (msg.Msg = WM_PAINT) or (msg.Msg = WM_LBUTTONUP) then begin
    FCanvas.Handle:= GetDC(Handle);
    FCanvas.Brush.Color:= BackColor;
    Rect:= ClientRect;
    FCanvas.FillRect(Rect);

    ow:= (Self.Width) div 3;
    oh:= (Self.Height) div 3;

    with FCanvas.Pen do begin
      Color:= clBlack;
      Width:= 1;
      Mode:= pmCopy;
      Style:= psSolid;
    end;

    Inc(Rect.Left, ow);
    Inc(Rect.Top, oh);
    FCanvas.Brush.Color:= DefBackColor;
    FCanvas.FillRect(Rect);
    FCanvas.Rectangle(Rect);

    with FCanvas.Pen do begin
      Color:= clBlack;
    end;
    Dec(Rect.Left, ow);
    Dec(Rect.Top, oh);
    Dec(Rect.Right, ow);
    Dec(Rect.Bottom, oh);
    FCanvas.Brush.Color:= DefForeColor;
    FCanvas.FillRect(Rect);
    FCanvas.Rectangle(Rect);

    ReleaseDC(Handle, FCanvas.Handle);
    end
  else;
end;

{ TrtcColorPicker }

procedure TrtcColorPicker.ButtonForegroundClicked(Sender: TObject);
begin
  DoColorSelection(True);
end;

procedure TrtcColorPicker.ButtonBackgroundClicked(Sender: TObject);
begin
  DoColorSelection(False);
end;

procedure TrtcColorPicker.ButtonSwapClicked(Sender: TObject);
var
  c: TColor;
begin
  // swap colors
  c:= FButtonForeground.BackColor;
  FButtonForeground.BackColor:= FButtonBackground.BackColor;
  FButtonBackground.BackColor:= c;
end;

procedure TrtcColorPicker.ButtonDefaultClicked(Sender: TObject);
begin
  // set default colors
  ForeColor:= DefaultForeColor;
  BackColor:= DefaultBackColor;
end;

constructor TrtcColorPicker.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle:= ControlStyle - [csAcceptsControls, csSetCaption];
  FDefaultForeColor:= clRed;
  FDefaultBackColor:= clWhite;
  FButtonBackground:= TrtcPanelButton.Create(Self);
  FButtonForeground:= TrtcPanelButton.Create(Self);
  FButtonSwapColors:= TrtcSwapPanelButton.Create(Self);
  FButtonDefaultColors:= TrtcDefaultPanelButton.Create(Self);

  with FButtonBackground do begin
    Parent:= Self;
    Caption:= ''; // B
    Hint:= 'Background color';
    ShowHint:= True;
    Color:= FDefaultBackColor;
    OnClick:= ButtonBackgroundClicked;
    Checked:= True;
  end;
  with FButtonForeground do begin
    Parent:= Self;
    Caption:= ''; // F
    Hint:= 'Foreground color';
    ShowHint:= True;
    Color:= FDefaultForeColor;
    OnClick:= ButtonForegroundClicked;
    Checked:= True;
  end;
  with FButtonSwapColors do begin
    Parent:= Self;
    Font.Size:= 8;
    Font.Name:= 'Wingdings';
    Caption:= #$F3;
    Hint:= 'Swap colors';
    ShowHint:= True;
    OnClick:= ButtonSwapClicked;
  end;
  with FButtonDefaultColors do begin
    Parent:= Self;
    Caption:= 'Default';
    Hint:= 'Default colors';
    ShowHint:= True;
    OnClick:= ButtonDefaultClicked;
    DefForeColor:= FDefaultForeColor;
    DefBackColor:= FDefaultBackColor;
  end;
  SetBounds(Left, Top, 64, 64);
end;

function TrtcColorPicker.DoColorSelection(AFore: Boolean = True): Boolean;
begin
  Result:= False;
  with TColorDialog.Create(Self) do try
    if AFore then
      Color:= ForeColor
    else
      Color:= BackColor;
    Options:= DialogOptions;
    if Execute then begin
      Result:= True;
      if AFore then
        ForeColor:= Color
      else
        BackColor:= Color;
    end;
  finally
    Free;
  end;
end;

procedure TrtcColorPicker.Paint;
begin
  inherited;
  with Canvas do begin
    Brush.Color:= ForeColor;
    Rectangle(FBackColorRect);
    Brush.Color:= BackColor;
    Rectangle(FForeColorRect);
  end;
end;

procedure TrtcColorPicker.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
var
  w, h,
  ow, oh: Integer;
begin
  inherited;
  if (not Assigned(FButtonBackground)) or (not Assigned(FButtonForeground)) then Exit;
  w:= (Self.Width * 2) div 3;
  h:= (Self.Height * 2) div 3;
  ow:= (Self.Width) div 3;
  oh:= (Self.Height) div 3;

  FButtonBackground.SetBounds(ow + 2, oh + 2, w - 4, h - 4);
  FButtonForeground.SetBounds(2, 2, w - 4, h - 4);

  FButtonSwapColors.SetBounds(w + 2, 2, ow - 4, oh - 4);
  FButtonDefaultColors.SetBounds(2, w + 2, ow - 4, oh - 4);

  FBackColorRect:= Rect(5, 5, w - 5, h - 5);
  FForeColorRect:= Rect(5, 5, w - 5, h - 5);
end;

procedure TrtcColorPicker.SetFlatButton(const AValue: Boolean);
begin
  FFlat:= AValue;
end;

function TrtcColorPicker.GetForeColor: TColor;
begin
  if Assigned(FButtonForeground) then
    Result:= FButtonForeground.BackColor
  else
    Result:= clBlack;
end;

function TrtcColorPicker.GetBackColor: TColor;
begin
  if Assigned(FButtonForeground) then
    Result:= FButtonBackground.BackColor
  else
    Result:= clWhite;
end;

procedure TrtcColorPicker.SetForeColor(const AValue: TColor);
begin
  FButtonForeground.BackColor:= AValue;
end;

procedure TrtcColorPicker.SetBackColor(const AValue: TColor);
begin
  FButtonBackground.BackColor:= AValue;
end;

{--------------------------- TrtcPenCombobox ----------------------------------}

constructor TrtcPenCombobox.Create(AOwner: TComponent);
var
  ps: TPenStyle;
begin
  inherited Create(AOwner);
  Self.Style:= csOwnerDrawFixed;
  if not (AOwner is TWinControl) then Exit;
  Parent:= TWinControl(AOwner);
  with Self.Items do begin
    for ps:= Low(TPenStyle) to Pred(Pred(High(TPenStyle))) do begin
      AddObject('', TObject(ps));
    end;
  end;
  Self.ItemIndex:= 0;
end;

procedure TrtcPenCombobox.DrawItem(AIndex: Integer; ARect: TRect; AState: TOwnerDrawState);
var
  s: String;
  ps: TPenStyle;
  p, w: Integer;
begin
  TControlCanvas(Canvas).UpdateTextFlags;
  if Assigned(OnDrawItem) then
    OnDrawItem(Self, AIndex, ARect, AState)
  else begin
    Canvas.Brush.Color:= clWhite;
    Canvas.FillRect(ARect);
    if AIndex >= 0 then begin
      s:= Items[AIndex];
      w:= 1;
      try
        ps:= TPenStyle(Items.Objects[AIndex]);
        with Canvas.Pen do begin
          Style:= ps;
          Color:= clBlack;
          Width:= w;
        end;
        p:= ARect.Top + ((ARect.Bottom - ARect.Top - w) div 2);
        Canvas.MoveTo(ARect.Left, p);
        Canvas.LineTo(ARect.Right, p);
      finally
      end;
      if Length(s) > 0 then
        Canvas.TextOut(ARect.Left + 2, ARect.Top, s);
    end;
  end;
end;

{ ------------------------------ TrtcPenWidthCombobox -------------------------}

constructor TrtcPenWidthCombobox.Create(AOwner: TComponent);
var
  i: Integer;
  wmax: Integer;
begin
  inherited Create(AOwner);
  Self.Width:= 42;
  Self.BevelKind:= bkFlat;
  Self.Style:= csOwnerDrawFixed;
  Hint:= 'Pen thikness';
  ShowHint:= True;
  if not (AOwner is TWinControl) then Exit;
  Parent:= TWinControl(AOwner);
  wmax:= Self.Height - 8;
  with Self.Items do begin
    i:= 1;
    while i < wmax do begin
      AddObject('', TObject(i));
      Inc(i, 2);
    end;
  end;
  Self.ItemIndex:= 0;
end;

procedure TrtcPenWidthCombobox.DrawItem(AIndex: Integer; ARect: TRect; AState: TOwnerDrawState);
var
  s: String;
  PenWidth: Integer;
  x, y, h: Integer;
  r: TRect;
begin
  TControlCanvas(Canvas).UpdateTextFlags;
  if Assigned(OnDrawItem) then
    OnDrawItem(Self, AIndex, ARect, AState)
  else begin
    Canvas.Pen.Color:= clSilver;
    Canvas.Brush.Color:= clWhite;
    Canvas.FillRect(ARect);
    if AIndex >= 0 then begin
      s:= Items[AIndex];
      try
        PenWidth:= Integer(Items.Objects[AIndex]);
        with Canvas.Pen do begin
          Style:= psSolid;
          Color:= clBlack;
          Width:= 1;
        end;
        with Canvas.Brush do begin
          Canvas.Brush.Style:= bsSolid;
          Color:= clSilver;
        end;
        h:= ARect.Bottom - ARect.Top;
        x:= ARect.Left + ((h - PenWidth) div 2);
        y:= ARect.Top + ((h - PenWidth) div 2);

        r.Left:= x;
        r.Right:= x + PenWidth;
        r.Top:= y;
        r.Bottom:= y +  PenWidth;
        if PenWidth <= 1 then begin
          Canvas.MoveTo(r.Left, r.Top);
          Canvas.LineTo(r.Left + PenWidth, r.Top);
        end else
          Canvas.Ellipse(r);
      finally
      end;
      if Length(s) > 0 then
        Canvas.TextOut(ARect.Left + 2, ARect.Top, s);
    end;
  end;
end;

{ ------------------------- TrtcBrushCombobox ------------------------------- }

constructor TrtcBrushCombobox.Create(AOwner: TComponent);
var
  bs: TBrushStyle;
begin
  inherited Create(AOwner);
  Self.Width:= 42;
  Self.Style:= csOwnerDrawFixed;
  if not (AOwner is TWinControl) then Exit;
  Parent:= TWinControl(AOwner);
  with Self.Items do begin
    for bs:= Low(TBrushStyle) to High(TBrushStyle) do begin
      AddObject('', TObject(bs));
    end;
  end;
  Self.ItemIndex:= 0;
end;

procedure TrtcBrushCombobox.DrawItem(AIndex: Integer; ARect: TRect; AState: TOwnerDrawState);
var
  s: String;
  bs: TBrushStyle;
  h: Integer;
  r: TRect;
begin
  TControlCanvas(Canvas).UpdateTextFlags;
  if Assigned(OnDrawItem) then
    OnDrawItem(Self, AIndex, ARect, AState)
  else begin
    Canvas.Brush.Color:= clWhite;
    Canvas.FillRect(ARect);
    if AIndex >= 0 then begin
      s:= Items[AIndex];
      try
        bs:= TBrushStyle(Items.Objects[AIndex]);
        with Canvas.Pen do begin
          Style:= psSolid;
          Color:= clWhite;
          Width:= 1;
        end;
        with Canvas.Brush do begin
          Canvas.Brush.Style:= bs;
          Color:= clWhite;
        end;
        h:= ARect.Bottom - ARect.Top;

        r.Left:= ARect.Left;
        r.Right:= r.Left + h;
        r.Top:= ARect.Top;
        r.Bottom:= r.Top + h;
        Canvas.FillRect(r);
      finally
      end;
      if Length(s) > 0 then
        Canvas.TextOut(ARect.Left + 2, ARect.Top, s);
    end;
  end;
end;

{ -------------------------- TrtcPaintControl -------------------------------- }

constructor TrtcPaintControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Self.Parent:= TWinControl(AOwner);

  ColorPicker:= TrtcColorPicker.Create(Self);
  ColorPicker.Parent:= TWinControl(Self);
  ColorPicker.Align:= alBottom;

  PenWidthCombobox:= TrtcPenWidthCombobox.Create(Self);
  with PenWidthCombobox do begin
    Align:= alBottom;
  end;
  Tools:= TrtcPanelRadioButtonGroup.Create(Self);
  with Tools do begin
    Align:= alTop;
    Columns:= 2;
  end;

  FControlImage:= Nil;
  FControlCanvas:= Nil;
  FDrawing:= False;

  FEdit:= TrtcTransparentEdit.Create(Self);
  with FEdit do begin
    Parent:= TWinControl(Self.Parent);
    Visible:= False;
    BorderStyle:= Forms.bsNone;
    OnExit:= DoEditExit;
    Hint:= 'Enter text';
    ShowHint:= True;
  end;

  FScale:= 100;

  SetLength(FLayerBitmaps, 0);
  AddLayer; // temporary bitmap to avoid flicks
  AddLayer; // background
  AddLayer; // drawing layer 0
end;

destructor TrtcPaintControl.Destroy;
begin
  if Assigned(FControlCanvas) then
    FControlCanvas.Free;
  ClearLayers;
  inherited Destroy;
end;

procedure TrtcPaintControl.AddLayer;
var
  len: Integer;
begin
  len:= Length(FLayerBitmaps);
  SetLength(FLayerBitmaps, len + 1);
  FLayerBitmaps[len]:= TBitmap.Create;
  FCurrentLayerIndex:= len;
end;

function TrtcPaintControl.GetBackgroundBitmap: TBitmap;
var
  len: Integer;
begin
  len:= Length(FLayerBitmaps);
  if len > 1 then
    Result:= FLayerBitmaps[1]
    else Result:= Nil;
end;

procedure TrtcPaintControl.SetBackgroundBitmap(AValue: TBitmap);
var
  len: Integer;
begin
  len:= Length(FLayerBitmaps);
  if len > 1 then begin
    FLayerBitmaps[1].Assign(AValue);
    ResizeLayerBitmaps(AValue.Width, AValue.Height);
  end;
end;

procedure TrtcPaintControl.SetScale(AValue: Integer);
begin
  if AValue <= 0 then Exit;
  FScale:= AValue;
end;

procedure TrtcPaintControl.ClearLayers;
var
  i, len: Integer;
begin
  len:= Length(FLayerBitmaps);
  for i:= 0 to len - 1 do
    FLayerBitmaps[i].Free;
  SetLength(FLayerBitmaps, 0);
  FCurrentLayerIndex:= -1;
end;

procedure DrawTransparentBmp(Cnv: TCanvas; x,y: Integer; Bmp: TBitmap; clTransparent: TColor);
var
  bmpXOR, bmpAND, bmpINVAND, bmpTarget: TBitmap;
  oldcol: Longint;
begin
  try
    bmpAND:= TBitmap.Create;
    bmpAND.Width:= Bmp.Width;
    bmpAND.Height:= Bmp.Height;
    bmpAND.Monochrome:= True;
    oldcol:= SetBkColor(Bmp.Canvas.Handle, ColorToRGB(clTransparent));
    BitBlt(bmpAND.Canvas.Handle, 0, 0, Bmp.Width, Bmp.Height, Bmp.Canvas.Handle, 0, 0, SRCCOPY);
    SetBkColor(Bmp.Canvas.Handle, oldcol);

    bmpINVAND:= TBitmap.Create;
    bmpINVAND.Width:= Bmp.Width;
    bmpINVAND.Height:= Bmp.Height;
    bmpINVAND.Monochrome:= True;
    BitBlt(bmpINVAND.Canvas.Handle, 0, 0, Bmp.Width, Bmp.Height, bmpAND.Canvas.Handle, 0, 0, NOTSRCCOPY);

    bmpXOR:= TBitmap.Create;
    bmpXOR.Width:= Bmp.Width;
    bmpXOR.Height:= Bmp.Height;
    BitBlt(bmpXOR.Canvas.Handle, 0, 0, Bmp.Width, Bmp.Height, Bmp.Canvas.Handle, 0, 0, SRCCOPY);
    BitBlt(bmpXOR.Canvas.Handle, 0, 0, Bmp.Width, Bmp.Height, bmpINVAND.Canvas.Handle, 0, 0, SRCAND);

    bmpTarget:= TBitmap.Create;
    bmpTarget.Width:= Bmp.Width;
    bmpTarget.Height:= Bmp.Height;
    BitBlt(bmpTarget.Canvas.Handle, 0, 0, Bmp.Width, Bmp.Height, Cnv.Handle, x, y, SRCCOPY);
    BitBlt(bmpTarget.Canvas.Handle, 0, 0, Bmp.Width, Bmp.Height, bmpAND.Canvas.Handle, 0, 0, SRCAND);
    BitBlt(bmpTarget.Canvas.Handle, 0, 0, Bmp.Width, Bmp.Height, bmpXOR.Canvas.Handle, 0, 0, SRCINVERT);
    BitBlt(Cnv.Handle, x, y, Bmp.Width, Bmp.Height, bmpTarget.Canvas.Handle, 0, 0, SRCCOPY);
  finally
    bmpXOR.Free;
    bmpAND.Free;
    bmpINVAND.Free;
    bmpTarget.Free;
  end;
end;

procedure TrtcPaintControl.SyncLayers;
var
  s, d: TRect;
  i, len: Integer;
begin
  //
  if FDrawing then Exit;
  
  s.Left:= 0;
  s.Top:= 0;
  if FCurrentLayerIndex < 0 then Exit;

  s.Right:= FLayerBitmaps[FCurrentLayerIndex].Width;
  s.Bottom:= FLayerBitmaps[FCurrentLayerIndex].Height;
  d:= s;
  try
    // copy background
    if FCurrentLayerIndex > 1 then
      FLayerBitmaps[0].Canvas.CopyRect(d, FLayerBitmaps[1].Canvas, s);
    // get transparent and draw on temporary layer 0
    i:= 2;
    len:= Length(FLayerBitmaps);
    while i < len do begin
      DrawTransparentBmp(FLayerBitmaps[0].Canvas, 0, 0, FLayerBitmaps[i], clWhite);
      Inc(i);
    end;
  finally
    // show temporary layer 0
    // scale bitmap

    if FScale <> 100 then begin
      if GDIPl2.GDIPlusActive then begin
        GDIplHelper.ScaleBitmap(FLayerBitmaps[0], FScale, d);
        s:= d;
      end else begin
        d.Right:= d.Left + (FScale * FLayerBitmaps[0].Width div 100);
        d.Bottom:= d.Top + (FScale * FLayerBitmaps[0].Height div 100);
      end;
    end;

    if FControlImage is TrtcDrawPanel then begin
      TrtcDrawPanel(FControlImage).PaintControl:= Self;
      TrtcDrawPanel(FControlImage).Invalidate;
    end else begin
      FControlCanvas.CopyRect(d, FLayerBitmaps[0].Canvas, s);
    end;
    if (FScale <> 100) then begin
      // restore bitmap size
      if GDIPl2.GDIPlusActive then begin
        FLayerBitmaps[0].Width:= FLayerBitmaps[1].Width;
        FLayerBitmaps[0].Height:= FLayerBitmaps[1].Height;
      end;
    end;
  end;
//  FControlCanvas.CopyRect(d, FLayerBitmaps[FCurrentLayerIndex].Canvas, s);
end;

procedure TrtcPaintControl.SaveLayers();
var
  i, len: Integer;
begin
  len:= Length(FLayerBitmaps);
  for i:= 0 to len - 1 do begin
    FLayerBitmaps[i].SaveToFile(IntToStr(i) + '.bmp');
  end;
end;

procedure TrtcPaintControl.SetControlImage(AValue: TControl);
var
  len: Integer;
begin
  FControlImage:= AValue;
  if FCurrentLayerIndex < 0 then Exit;
  if Assigned(FControlImage) then begin
    with FControlImage do begin
//      if FControlImage is TPanel then
      TPanel(FControlImage).OnMouseMove:= DoMouseMove;
      TPanel(FControlImage).OnMouseDown:= DoMouseDown;
      TPanel(FControlImage).OnMouseUp:= DoMouseUp;
      if FControlImage is TrtcDrawPanel then begin
        TrtcDrawPanel(FControlImage).PaintControl:= Self;
      end;
    end;
    if not Assigned(FControlCanvas) then
      FControlCanvas:= TControlCanvas.Create;
    FControlCanvas.Control:= FControlImage;

    // do not resize bitmaps if SetBackgroundBitmap() is called (already resized)
    if Assigned(FLayerBitmaps) then begin
      len:= Length(FLayerBitmaps);
      if (len > 0) and FLayerBitmaps[0].Empty then
        ResizeLayerBitmaps(FControlImage.Width, FControlImage.Height);
    end;
  end;
end;

procedure TrtcPaintControl.ResizeLayerBitmaps(AWidth, AHeight: Integer);
var
  i, len: Integer;
begin
  len:= Length(FLayerBitmaps);
  for i:= 0 to len - 1 do begin
    FLayerBitmaps[i].Width:= AWidth;
    FLayerBitmaps[i].Height:= AHeight;
  end;
  {
  if len > 1 then begin
    with FLayerBitmaps[1].Canvas do begin
      Brush.Color:= clYellow;
      FillRect(Rect(0, 0, AWidth, AHeight));
    end;
  end;
  }
end;

function CalcScalePoint(AScale: Integer; const AXY: TPoint): TPoint;
begin
  if (AScale = 100) or (AScale <= 0) then begin
    Result:= AXY;
  end else begin
    Result.X:= 100 * AXY.X div AScale;
    Result.Y:= 100 * AXY.Y div AScale;
  end;
end;

procedure TrtcPaintControl.DoMouseDown(ASender: TObject; AButton: TMouseButton;
  AShift: TShiftState; AX, AY: Integer);
begin
  if FEdit.Visible then begin
    DoEditExit(Self);
    Exit;
  end;
  FDrawing:= True;
  if FCurrentLayerIndex >= 0 then begin
//    FLayerBitmaps[FCurrentLayerIndex].Canvas.MoveTo(AX, AY);
//    SyncLayers;
  end;
  // FControlImage.Canvas.MoveTo(AX, AY);
  FPreviewOrigin:= Point(AX, AY);
  FOrigin:= CalcScalePoint(FScale, FPreviewOrigin);
  FPreviewDestination:= FPreviewOrigin;
  FDestination:= FOrigin;
end;

procedure TrtcPaintControl.DoMouseUp(ASender: TObject; AButton: TMouseButton;
  AShift: TShiftState; AX, AY: Integer);
var
  sh,
  w, h: Integer;
  r: TRect;
begin
  // if not Assigned(FControlImage) then Exit; // ;)) why?
  if FDrawing then begin
    sh:= Tools.SelectedIndex;
    if sh = 5 then begin
      // uncomment if text on shaped rectange
      // comment if text only
//      DrawShape(1, FLayerBitmaps[FCurrentLayerIndex].Canvas, FOrigin, FDestination, pmNotXor);
    end else begin
      DrawShape(Tools.SelectedIndex, FLayerBitmaps[FCurrentLayerIndex].Canvas, FOrigin, FDestination, pmCopy);
    end;
    if (sh = 5) then begin
      R.Left:= FControlImage.Left + FPreviewOrigin.X;
      R.Top:= FControlImage.Top + FPreviewOrigin.Y;
      R.Right:= FControlImage.Left + FPreviewDestination.X;
      R.Bottom:= FControlImage.Top + FPreviewDestination.Y;
      CheckRectCoords(R);
//      R.TopLeft:= CalcScalePoint(FScale, R.TopLeft);
//      R.BottomRight:= CalcScalePoint(FScale, R.BottomRight);
      FEdit.Left:= R.Left;
      FEdit.Top:= R.Top;
      if R.Right > FControlImage.Left + FControlImage.Width then
        w:= FControlImage.Left + FControlImage.Width - R.Left
      else
        w:= R.Right - R.Left;
      if R.Bottom > FControlImage.Top + FControlImage.Height then
        h:= FControlImage.Top + FControlImage.Height - R.Top
      else
        h:= R.Bottom - R.Top;
      with FEdit do begin
        Width:= w;
        Height:= h;
        Font.Color:= ColorPicker.ForeColor;
        Color:= ColorPicker.BackColor;
        Visible:= True;
        BringToFront;
        SetFocus;
      end;
    end;
    FDrawing:= False;
    SyncLayers;
  end;
end;

procedure TrtcPaintControl.DoMouseMove(ASender: TObject; AShift: TShiftState; AX, AY: Integer);
var
  sh: Integer;
begin
  if FDrawing then begin
    sh:= Tools.SelectedIndex;
    if sh = 5 then sh:= 1;
    DrawShape(sh, FControlCanvas, FPreviewOrigin, FPreviewDestination, pmNotXor);
    FPreviewDestination:= Point(AX, AY);
    FDestination:= CalcScalePoint(FScale, FPreviewDestination);
    DrawShape(sh, FControlCanvas, FPreviewOrigin, FPreviewDestination, pmNotXor);
  end;
end;

procedure Draw_Text(const ARect: TRect; const AStr: String; ACanvas: TCanvas;
  AForeColor: TColor);
var
  OldBkMode: Integer;
  v: Integer;
begin
  if Length(AStr) <= 0 then Exit;
  OldBkMode:= SetBkMode(ACanvas.Handle, TRANSPARENT);
  ACanvas.Pen.Mode:= pmCopy;
  ACanvas.Font.Color:= AForeColor;

  ACanvas.TextRect(ARect, ARect.Left, ARect.Top, AStr);
  //ACanvas.TextOut(ARect.Left, ARect.Top, AStr);
  SetBkMode(ACanvas.Handle, OldBkMode);
end;

procedure TrtcPaintControl.DoEditExit(ASender: TObject);
var
  r: TRect;
begin
  if not FEdit.Visible then Exit;
  FDrawing:= False;
  FEdit.Visible:= False;
  R.Left:= FOrigin.X;
  R.Top:= FOrigin.Y;
  R.Right:= FDestination.X;
  R.Bottom:= FDestination.Y;
  CheckRectCoords(R);
  if FCurrentLayerIndex >= 0 then begin
//    SyncLayers;
    Draw_Text(R, FEdit.Text, FLayerBitmaps[FCurrentLayerIndex].Canvas, ColorPicker.ForeColor); //FControlImage.Left
    SyncLayers;
    FControlImage.Invalidate;
  end;
  FEdit.Text:= '';
end;

procedure TrtcPaintControl.DrawShape(AToolIndex: Integer; ACanvas: TCanvas; ATopLeft, ABottomRight: TPoint;
  AMode: TPenMode);
begin
  if FCurrentLayerIndex < 0 then Exit;
  with ACanvas do begin // FControlImage.Canvas
    Pen.Mode:= AMode;
    Pen.Color:= ColorPicker.ForeColor;
    Brush.Color:= ColorPicker.BackColor;
    case AToolIndex of
      0:begin
        MoveTo(ATopLeft.X, ATopLeft.Y);
        LineTo(ABottomRight.X, ABottomRight.Y);
      end;
      1:begin
        Rectangle(ATopLeft.X, ATopLeft.Y, ABottomRight.X, ABottomRight.Y);
      end;
      2:begin
        Ellipse(ATopleft.X, ATopLeft.Y, ABottomRight.X, ABottomRight.Y);
      end;
      3:begin
          RoundRect(ATopLeft.X, ATopLeft.Y, ABottomRight.X,
            ABottomRight.Y, (ATopLeft.X - ABottomRight.X) div 2,
            (ATopLeft.Y - ABottomRight.Y) div 2);
      end;
      4:begin
        PolyBezier([ATopLeft, Point(ATopLeft.X, ABottomRight.Y),
          Point(ABottomRight.X, ATopLeft.Y), ABottomRight]);
      end;
      5:begin
//        Draw_Text(ATopLeft.X, ATopLeft.Y, FEdit.Text, ControlImage.Canvas);
      end;
    end;
  end;
{
MoveTo(0,0);
TextOut(20, 50, 'x: ' + IntToStr(APreviewTopLeft.X) + 'y: ' + IntToStr(APreviewTopLeft.y));
}
end;

{ ---------------------- TrtcPanelRadioButtonGroup --------------------------- }

constructor TrtcPanelRadioButtonGroup.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Self.Parent:= TWinControl(AOwner);
  FColumns:= 1;
  FSelectedIndex:= 0;
  SetLength(FButtons, 0);
end;

function TrtcPanelRadioButtonGroup.GetItem(AIndex: Integer): TrtcPanelButton;
begin
  Result:= Nil;
  if (AIndex < 0) or (AIndex >= Length(FButtons)) then Exit;
  Result:= FButtons[AIndex];
end;

function TrtcPanelRadioButtonGroup.GetSelected: TrtcPanelButton;
begin
  Result:= Nil;
  if (FSelectedIndex < 0) or (FSelectedIndex >= Length(FButtons)) then Exit;
  Result:= FButtons[FSelectedIndex];
end;

procedure TrtcPanelRadioButtonGroup.Rearrange;
var
  i, len, w, x, y: Integer;
  bmp: TBitmap;
begin
  len:= Length(FButtons);
  w:= Self.Width div FColumns;
  if w <= 2 then w:= 1 else Dec(w, 2);

  i:= len div FColumns;
  if (len mod FColumns) <> 0 then Inc(i);
  Height:= w * i;

  bmp:= TBitmap.Create;
  try
    for i:= 0 to len - 1 do begin
      x:= i mod FColumns;
      y:= i div FColumns;
      with FButtons[i] do begin
        bmp.Canvas.FillRect(Rect(0, 0, bmp.Width, bmp.Height));
        Left:= x * w;
        Top:= y * w;
        Width:= w;
        Height:= w;
        if Assigned(FImageList) then begin
          FImageList.GetBitmap(i, bmp);
          Glyph:= bmp;
        end;
      end;
    end;
  finally
    bmp.Free;
  end;
end;

procedure TrtcPanelRadioButtonGroup.Clear;
var
  i, len: Integer;
begin
  len:= Length(FButtons);
  for i:= 0 to len - 1 do begin
    FButtons[i].Free;
  end;
  SetLength(FButtons, 0);
end;

procedure TrtcPanelRadioButtonGroup.Add(const ACaption, AHint: String; ABitmap: TBitmap);
var
  len: Integer;
  bmp: TBitmap;
begin
  len:= Length(FButtons);
  SetLength(FButtons, len + 1);
  FButtons[len]:= TrtcPanelButton.Create(Self);
  with FButtons[len] do begin
    Parent:= TWinControl(Owner);
    Hint:= AHint;
    OnClick:= ToolClick;
    if Assigned(ABitmap) then
      Glyph:= ABitmap
    else begin
      if Assigned(FImageList) then begin
        bmp:= TBitmap.Create;
        FImageList.GetBitmap(len, bmp);
        Glyph:= bmp;
        bmp.Free;
      end;
    end;

    ShowHint:= Length(AHint) > 0;
    Caption:= ACaption;
//    Rearrange;
  end;
end;

procedure TrtcPanelRadioButtonGroup.SetColumns(AValue: Integer);
begin
  if AValue <= 0 then Exit;
  FColumns:= AValue;
  // Invalidate;
end;

procedure TrtcPanelRadioButtonGroup.SetImageList(AValue: TImageList);
var
  i: Integer;
begin
  FImageList:= AValue;
  Clear;
  for i:= 0 to AValue.Count - 1 do begin
    Add('', 'Hint', Nil);
  end;
  {
  if AValue.Count > 0 then
    Items[0].Checked:= True;
  }
  Rearrange;
  Invalidate;
end;

procedure TrtcPanelRadioButtonGroup.ToolClick(Sender: TObject);
var
  b: TrtcPanelButton;
  i, len: Integer;
begin
  //
  if Sender is TrtcPanelButton then begin
    len:= Length(FButtons);
    for i:= 0 to len - 1 do begin
      if FButtons[i] = TrtcPanelButton(Sender) then
        FSelectedIndex:= i
      else
        FButtons[i].Checked:= False;
    end;

    b:= TrtcPanelButton(Sender);
    b.Checked:= True;
    b.Invalidate;
  end;
end;

{ ------------------------------- TrtcDrawPanel ------------------------------ }

constructor TrtcDrawPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPaintControl:= Nil;
end;

procedure TrtcDrawPanel.Paint;
var
  srect, drect: TRect;
begin
  if not Assigned(FPaintControl) then
    Exit;
  if FPaintControl.IsDrawing then
    Exit;
  with Canvas do begin
    try
//      Canvas.Lock;
      sRect:= GetClientRect;
      dRect:= sRect;
      if FPaintControl.Scale <> 100 then begin
        dRect.Right:= dRect.Left + (FPaintControl.Scale * (sRect.Right - srect.Left) div 100);
        dRect.Bottom:= dRect.Top + (FPaintControl.Scale * (sRect.Bottom - srect.Top) div 100);
      end;

      Brush.Color:= Color;
      if Assigned(FPaintControl.LayerBitmaps) and (Length(FPaintControl.LayerBitmaps) > 0) then begin
        if FPaintControl.Scale = 100 then
          BitBlt(Canvas.Handle, dRect.Left, dRect.Top, dRect.Right - dRect.Left,
            dRect.Bottom - dRect.Top, FPaintControl.LayerBitmaps[0].Canvas.Handle, sRect.Left, sRect.Top, SRCCOPY)
        else;
          Canvas.CopyRect(dRect, FPaintControl.LayerBitmaps[0].Canvas, sRect);
      end else begin
        FillRect(dRect);
      end;
    finally
//      Canvas.Unlock;
    end;
  end;
end;

initialization

  GDIPl2.InitGDIPlus;

finalization

  GDIPl2.DoneGDIPlus;

end.
