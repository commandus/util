unit GIFButton;
////////////////////////////////////////////////////////////////////////////////
//                                                                            //
// Project:	TGIFImage demo application.                                   //
// Description:	TBitBtn like component that displays GIFs instead of BMPs.    //
// Copyright	(c) 1997-99 Anders Melander.                                  //
//		All rights reserved.                                          //
// Formatting:	2 space indent, 8 space tabs, 80 columns.                     //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////
//                                                                            //
// This component is based on Inprise's TBitBtn component.                    //
// Portions Copyright (c) 1995,98 Inprise Corporation                         //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////
interface
{$ifdef BCB}
{$ObjExportAll On}
{$endif}
uses
  StdCtrls, GIFimage, Buttons, ExtCtrls, Classes, Graphics, Windows, Messages,
  Controls;

type
  TGIFButton = class(TButton)
  private
    FCanvas: TCanvas;
    FCurrentGlyph: TGIFImage;
    FGlyph: TGIFImage;
    FGlyphMouseOver: TGIFImage;
    FStyle: TButtonStyle;
    FLayout: TButtonLayout;
    FSpacing: Integer;
    FMargin: Integer;
    IsFocused: Boolean;
    FModifiedGlyph: Boolean;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseExit: TNotifyEvent;
    FInsideButton: boolean;
    GlyphRect: TRect;
    AnimationUpdate: boolean;
    procedure DrawItem(const DrawItemStruct: TDrawItemStruct);
    procedure SetGlyph(Index: integer; Value: TGIFImage);
    procedure GlyphChanged(Sender: TObject);
    procedure SetStyle(Value: TButtonStyle);
    procedure SetLayout(Value: TButtonLayout);
    procedure SetSpacing(Value: Integer);
    procedure SetMargin(Value: Integer);
    procedure CNMeasureItem(var Message: TWMMeasureItem); message CN_MEASUREITEM;
    procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMVisibleChanged(var Message: TMessage); message CM_VISIBLECHANGED;
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk);
      message WM_LBUTTONDBLCLK;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    function GetPalette: HPALETTE; override;
    procedure SetButtonStyle(ADefault: Boolean); override;
    procedure CMMouseEnter(var msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var msg: TMessage); message CM_MOUSELEAVE;
    function DoDraw(Canvas: TCanvas; const Client: TRect; const Offset: TPoint;
      const Caption: string; Layout: TButtonLayout; Margin, Spacing: Integer;
      State: TButtonState; Transparent: Boolean; BiDiFlags: Longint): TRect;
    procedure DrawButtonGlyph(Canvas: TCanvas; const GlyphRect: TRect);
    procedure DrawButtonText(Canvas: TCanvas; const Caption: string;
      TextBounds: TRect; State: TButtonState; BiDiFlags: Longint);
    procedure CalcButtonLayout(Canvas: TCanvas; const Client: TRect;
      const Offset: TPoint; const Caption: string; Layout: TButtonLayout;
      Margin, Spacing: Integer; var GlyphRect: TRect; var TextBounds: TRect;
      BiDiFlags: Longint);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
{$IFDEF VER120}
    property Action;
    property Anchors;
    property BiDiMode;
    property Constraints;
    property ParentBiDiMode;
{$ENDIF}
    property Cancel;
    property Caption;
    property Default;
    property Enabled;
    property Glyph: TGIFImage index 1 read FGlyph write SetGlyph;
    property GlyphMouseOver: TGIFImage index 2 read FGlyphMouseOver write SetGlyph;
    property Layout: TButtonLayout read FLayout write SetLayout default blGlyphLeft;
    property Margin: Integer read FMargin write SetMargin default -1;
    property ModalResult;
    property ParentShowHint;
    property ShowHint;
    property Style: TButtonStyle read FStyle write SetStyle default bsAutoDetect;
    property Spacing: Integer read FSpacing write SetSpacing default 4;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnEnter;
    property OnExit;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseExit:  TNotifyEvent read FOnMouseExit  write FOnMouseExit;
  end;


procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Additional', [TGIFButton]);
end;

constructor TGIFButton.Create(AOwner: TComponent);
begin
  FGlyph := TGIFImage.Create;
  FGlyph.DrawOptions :=
    FGlyph.DrawOptions - [goDirectDraw]+[goLoopContinously];
  if (csDesigning in ComponentState) then
    FGlyph.DrawOptions := FGlyph.DrawOptions - [goAnimate];
  FGlyph.OnChange := GlyphChanged;

  FGlyphMouseOver := TGIFImage.Create;
  FGlyphMouseOver.DrawOptions :=
    FGlyphMouseOver.DrawOptions - [goDirectDraw]+[goLoopContinously];
  FGlyphMouseOver.OnChange := GlyphChanged;

  FCurrentGlyph := FGlyph;

  inherited Create(AOwner);
  FCanvas := TCanvas.Create;
  FStyle := bsAutoDetect;
  FLayout := blGlyphLeft;
  FSpacing := 4;
  FMargin := -1;
{$ifndef VER90}
  ControlStyle := ControlStyle + [csReflector];
{$endif}
  AnimationUpdate := False;
end;

destructor TGIFButton.Destroy;
begin
  if (Assigned(FCurrentGlyph)) then
    FCurrentGlyph.StopDraw;
  FGlyph.Free;
  FGlyphMouseOver.Free;
  FCanvas.Free;
  inherited Destroy;
end;

procedure TGIFButton.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do Style := Style or BS_OWNERDRAW;
end;

procedure TGIFButton.SetButtonStyle(ADefault: Boolean);
begin
  if ADefault <> IsFocused then
  begin
    IsFocused := ADefault;
    Refresh;
  end;
end;

procedure TGIFButton.CNMeasureItem(var Message: TWMMeasureItem);
begin
  with Message.MeasureItemStruct^ do
  begin
    itemWidth := Width;
    itemHeight := Height;
  end;
end;

procedure TGIFButton.CNDrawItem(var Message: TWMDrawItem);
begin
  DrawItem(Message.DrawItemStruct^);
end;

procedure TGIFButton.DrawButtonGlyph(Canvas: TCanvas; const GlyphRect: TRect);
begin
  if not(FCurrentGlyph.Empty) then
    Canvas.Draw(GlyphRect.Left, GlyphRect.Top, FCurrentGlyph);
  FModifiedGlyph := False;
end;

procedure TGIFButton.DrawButtonText(Canvas: TCanvas; const Caption: string;
  TextBounds: TRect; State: TButtonState; BiDiFlags: LongInt);
begin
  with Canvas do
  begin
    Brush.Style := bsClear;
    if State = bsDisabled then
    begin
      OffsetRect(TextBounds, 1, 1);
      Font.Color := clBtnHighlight;
      DrawText(Handle, PChar(Caption), Length(Caption), TextBounds,
        DT_CENTER or DT_VCENTER or BiDiFlags);
      OffsetRect(TextBounds, -1, -1);
      Font.Color := clBtnShadow;
      DrawText(Handle, PChar(Caption), Length(Caption), TextBounds,
        DT_CENTER or DT_VCENTER or BiDiFlags);
    end else
      DrawText(Handle, PChar(Caption), Length(Caption), TextBounds,
        DT_CENTER or DT_VCENTER or BiDiFlags);
  end;
end;

procedure TGIFButton.CalcButtonLayout(Canvas: TCanvas; const Client: TRect;
  const Offset: TPoint; const Caption: string; Layout: TButtonLayout; Margin,
  Spacing: Integer; var GlyphRect: TRect; var TextBounds: TRect;
  BiDiFlags: LongInt);
var
  TextPos: TPoint;
  ClientSize, TextSize: TPoint;
  TotalSize: TPoint;
begin
  if (BiDiFlags and DT_RIGHT) = DT_RIGHT then
    if Layout = blGlyphLeft then Layout := blGlyphRight
    else
      if Layout = blGlyphRight then Layout := blGlyphLeft;
  { calculate the item sizes }
  ClientSize := Point(Client.Right - Client.Left, Client.Bottom -  Client.Top);

  GlyphRect := Rect(0,0, FCurrentGlyph.Width, FCurrentGlyph.Height);

  if Length(Caption) > 0 then
  begin
    TextBounds := Rect(0, 0, Client.Right - Client.Left, 0);
    DrawText(Canvas.Handle, PChar(Caption), Length(Caption), TextBounds,
      DT_CALCRECT or BiDiFlags);
    TextSize := Point(TextBounds.Right - TextBounds.Left, TextBounds.Bottom -
      TextBounds.Top);
  end else
  begin
    TextBounds := Rect(0, 0, 0, 0);
    TextSize := Point(0,0);
  end;

  { If the layout has the glyph on the right or the left, then both the
    text and the glyph are centered vertically.  If the glyph is on the top
    or the bottom, then both the text and the glyph are centered horizontally.}
  if Layout in [blGlyphLeft, blGlyphRight] then
  begin
    GlyphRect.Top := (ClientSize.Y - GlyphRect.Bottom + 1) div 2;
    TextPos.Y := (ClientSize.Y - TextSize.Y + 1) div 2;
  end
  else
  begin
    GlyphRect.Left := (ClientSize.X - GlyphRect.Right + 1) div 2;
    TextPos.X := (ClientSize.X - TextSize.X + 1) div 2;
  end;

  { if there is no text or no bitmap, then Spacing is irrelevant }
  if (TextSize.X = 0) or (GlyphRect.Right = 0) then
    Spacing := 0;

  { adjust Margin and Spacing }
  if Margin = -1 then
  begin
    if Spacing = -1 then
    begin
      TotalSize := Point(GlyphRect.Right + TextSize.X, GlyphRect.Bottom + TextSize.Y);
      if Layout in [blGlyphLeft, blGlyphRight] then
        Margin := (ClientSize.X - TotalSize.X) div 3
      else
        Margin := (ClientSize.Y - TotalSize.Y) div 3;
      Spacing := Margin;
    end else
    begin
      TotalSize := Point(GlyphRect.Right + Spacing + TextSize.X,
        GlyphRect.Bottom + Spacing + TextSize.Y);
      if Layout in [blGlyphLeft, blGlyphRight] then
        Margin := (ClientSize.X - TotalSize.X + 1) div 2
      else
        Margin := (ClientSize.Y - TotalSize.Y + 1) div 2;
    end;
  end else
  begin
    if Spacing = -1 then
    begin
      TotalSize := Point(ClientSize.X - (Margin + GlyphRect.Right),
        ClientSize.Y - (Margin + GlyphRect.Bottom));
      if Layout in [blGlyphLeft, blGlyphRight] then
        Spacing := (TotalSize.X - TextSize.X) div 2
      else
        Spacing := (TotalSize.Y - TextSize.Y) div 2;
    end;
  end;

  case Layout of
    blGlyphLeft:
      begin
        GlyphRect.Left := Margin;
        TextPos.X := GlyphRect.Left + GlyphRect.Right + Spacing;
      end;
    blGlyphRight:
      begin
        GlyphRect.Left := ClientSize.X - Margin - GlyphRect.Right;
        TextPos.X := GlyphRect.Left - Spacing - TextSize.X;
      end;
    blGlyphTop:
      begin
        GlyphRect.Top := Margin;
        TextPos.Y := GlyphRect.Top + GlyphRect.Bottom + Spacing;
      end;
    blGlyphBottom:
      begin
        GlyphRect.Top := ClientSize.Y - Margin - GlyphRect.Bottom;
        TextPos.Y := GlyphRect.Top - Spacing - TextSize.Y;
      end;
  end;

  { fixup the result variables }
  Inc(GlyphRect.Left, Client.Left + Offset.X);
  Inc(GlyphRect.Top, Client.Top + Offset.Y);
  Inc(GlyphRect.Right, GlyphRect.Left);
  Inc(GlyphRect.Bottom, GlyphRect.Top);

  OffsetRect(TextBounds, TextPos.X + Client.Left + Offset.X,
    TextPos.Y + Client.Top + Offset.X);
end;

{ return the text rectangle }
function TGIFButton.DoDraw(Canvas: TCanvas; const Client: TRect; const Offset: TPoint;
  const Caption: string; Layout: TButtonLayout; Margin, Spacing: Integer;
  State: TButtonState; Transparent: Boolean; BiDiFlags: Longint): TRect;
begin
  CalcButtonLayout(Canvas, Client, Offset, Caption, Layout, Margin, Spacing,
    GlyphRect, Result, BiDiFlags);
  DrawButtonGlyph(Canvas, GlyphRect);
  DrawButtonText(Canvas, Caption, Result, State, BiDiFlags);
end;

procedure TGIFButton.DrawItem(const DrawItemStruct: TDrawItemStruct);
var
  IsDown, IsDefault: Boolean;
  State: TButtonState;
  R: TRect;
  Flags: Longint;
begin
  FCanvas.Handle := DrawItemStruct.hDC;

  R := FCanvas.ClipRect;
  AnimationUpdate := AnimationUpdate and EqualRect(r, GlyphRect);
  if (AnimationUpdate) then
  begin
    DrawButtonGlyph(FCanvas, GlyphRect);
    AnimationUpdate := False;
  end else
  begin
    R := ClientRect;

    with DrawItemStruct do
    begin
      IsDown := itemState and ODS_SELECTED <> 0;
      IsDefault := itemState and ODS_FOCUS <> 0;

      if not Enabled then State := bsDisabled
      else if IsDown then State := bsDown
      else State := bsUp;
    end;

    Flags := DFCS_BUTTONPUSH or DFCS_ADJUSTRECT;
    if IsDown then Flags := Flags or DFCS_PUSHED;
    if DrawItemStruct.itemState and ODS_DISABLED <> 0 then
      Flags := Flags or DFCS_INACTIVE;

    { DrawFrameControl doesn't allow for drawing a button as the
        default button, so it must be done here. }
    if IsFocused or IsDefault then
    begin
      FCanvas.Pen.Color := clWindowFrame;
      FCanvas.Pen.Width := 1;
      FCanvas.Brush.Style := bsClear;
      FCanvas.Rectangle(R.Left, R.Top, R.Right, R.Bottom);

      { DrawFrameControl must draw within this border }
      InflateRect(R, -1, -1);
    end;

    { DrawFrameControl does not draw a pressed button correctly }
    if IsDown then
    begin
      FCanvas.Pen.Color := clBtnShadow;
      FCanvas.Pen.Width := 1;
      FCanvas.Brush.Color := clBtnFace;
      FCanvas.Rectangle(R.Left, R.Top, R.Right, R.Bottom);
      InflateRect(R, -1, -1);
    end
    else
      DrawFrameControl(DrawItemStruct.hDC, R, DFC_BUTTON, Flags);

    if IsFocused then
    begin
      R := ClientRect;
      InflateRect(R, -1, -1);
    end;

    FCanvas.Font := Self.Font;
    if IsDown then
      OffsetRect(R, 1, 1);
{$IFDEF VER120}
    DoDraw(FCanvas, R, Point(0,0), Caption, FLayout, FMargin,
      FSpacing, State, False, DrawTextBiDiModeFlags(0));
{$ELSE}
    DoDraw(FCanvas, R, Point(0,0), Caption, FLayout, FMargin,
      FSpacing, State, False, DT_LEFT);
{$ENDIF}

    if IsFocused and IsDefault then
    begin
      R := ClientRect;
      InflateRect(R, -4, -4);
      FCanvas.Pen.Color := clWindowFrame;
      FCanvas.Brush.Color := clBtnFace;
      DrawFocusRect(FCanvas.Handle, R);
    end;

  end;

  if (csDesigning in ComponentState) then
    DrawFocusRect(FCanvas.Handle, GlyphRect);

  FCanvas.Handle := 0;
end;

procedure TGIFButton.CMFontChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TGIFButton.CMEnabledChanged(var Message: TMessage);
begin
  inherited;

  if (Enabled) then
    FGlyph.DrawOptions := FGlyph.DrawOptions + [goAnimate]
  else
    FGlyph.DrawOptions := FGlyph.DrawOptions - [goAnimate];
  Invalidate;
end;

procedure TGIFButton.CMVisibleChanged(var Message: TMessage);
begin
  inherited;
  if ((not Visible) or (Parent = nil)) and (Assigned(FCurrentGlyph)) then
    FCurrentGlyph.StopDraw;
end;

procedure TGIFButton.WMLButtonDblClk(var Message: TWMLButtonDblClk);
begin
  Perform(WM_LBUTTONDOWN, Message.Keys, Longint(Message.Pos));
end;

function TGIFButton.GetPalette: HPALETTE;
begin
  Result := FCurrentGlyph.Palette;
end;

procedure TGIFButton.SetGlyph(Index: integer; Value: TGIFImage);
begin
  case (Index) of
    1: FGlyph.Assign(Value);
    2: FGlyphMouseOver.Assign(Value);
  end;
  FModifiedGlyph := True;
  AnimationUpdate := False;
  Invalidate;
end;

procedure TGIFButton.GlyphChanged(Sender: TObject);
begin
  if (not Visible) or (Parent = nil) then
    // Ignore updates when hidden or destroying
    exit;
  AnimationUpdate := True;
  if (FModifiedGlyph) then
    // Repaint everything to recalculate GlyphRect
    Invalidate
  else
    // Only repaint glyph
    InvalidateRect(Handle, @GlyphRect, False);
end;

procedure TGIFButton.SetStyle(Value: TButtonStyle);
begin
  if Value <> FStyle then
  begin
    FStyle := Value;
    Invalidate;
  end;
end;

procedure TGIFButton.SetLayout(Value: TButtonLayout);
begin
  if FLayout <> Value then
  begin
    FLayout := Value;
    Invalidate;
  end;
end;

procedure TGIFButton.SetSpacing(Value: Integer);
begin
  if FSpacing <> Value then
  begin
    FSpacing := Value;
    Invalidate;
  end;
end;

procedure TGIFButton.SetMargin(Value: Integer);
begin
  if (Value <> FMargin) and (Value >= - 1) then
  begin
    FMargin := Value;
    Invalidate;
  end;
end;

procedure TGIFButton.CMMouseEnter(var msg: TMessage);
begin
  if FInsideButton then
    exit;
  FInsideButton := True;

  if (Assigned(FOnMouseEnter)) then
    FOnMouseEnter(Self);

  if (Assigned(FCurrentGlyph)) then
    FCurrentGlyph.StopDraw;
  FModifiedGlyph := True;
  FCurrentGlyph := FGlyphMouseOver;
  Invalidate;
end;

procedure TGIFButton.CMMouseLeave(var msg: TMessage);
begin
  if not(FInsideButton) then
    exit;
  FInsideButton := False;

  if (Assigned(FOnMouseExit)) then
    FOnMouseExit(Self);

  if (Assigned(FCurrentGlyph)) then
    FCurrentGlyph.StopDraw;
  FModifiedGlyph := True;
  FCurrentGlyph := FGlyph;
  Invalidate;
end;


end.
