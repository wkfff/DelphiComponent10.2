unit acMeter;
{$I sDefs.inc}
//{$DEFINE LOGGED}

interface

uses
  {$IFNDEF DELPHI5} Types, {$ENDIF}
  Windows, Graphics, SysUtils, Classes, Controls, Messages, ImgList;


{$IFNDEF NOTFORHELP}
const
  DefaultSize = 160;
{$ENDIF}


type
{$IFNDEF NOTFORHELP}
  TMeterPaintData = class;

  TMeterDialType = (dtNumbers, dtGradient);
  TContentType = (ctGradient, ctValues, ctNone, ctCustomImage);

  TMeterShadowData = class(TPersistent)
  private
    FSize,
    FTransparency: byte;
    FVisible: boolean;
    procedure SetSize(const Value: byte);
    procedure SetTransparency(const Value: byte);
    procedure SetVisible(const Value: boolean);
  protected
    FOwner: TMeterPaintData;
  public
    constructor Create(AOwner: TMeterPaintData);
  published
    property Size: byte read FSize write SetSize default 5;
    property Transparency: byte read FTransparency write SetTransparency default 200;
    property Visible: boolean read FVisible write SetVisible default True;
  end;
{$ENDIF}

  TMeterPaintData = class(TPersistent)
{$IFNDEF NOTFORHELP}
  private
    FDialShadow,
    FArrowShadow: TMeterShadowData;

    FStretched,
    FTransparent: boolean;

    FDialPenWidth,
    FArrowPenWidth: byte;

    FColor,
    FDialColor,
    FArrowColor: TColor;

    FBackgroundImage: TBitmap;
    procedure SetBackgroundImage(const Value: TBitmap);
    procedure SetShadowData     (const Index: Integer; const Value: TMeterShadowData);
    procedure SetBoolean        (const Index: Integer; const Value: boolean);
    procedure SetColor          (const Index: Integer; const Value: TColor);
    procedure SetByte           (const Index: Integer; const Value: byte);
  protected
    FOwner: TGraphicControl;
  public
    constructor Create(AOwner: TGraphicControl);
    destructor Destroy; override;
  published
{$ENDIF}
    property BackgroundImage: TBitmap read FBackgroundImage write SetBackgroundImage;
    property ArrowColor: TColor index 0 read FArrowColor write SetColor default clBlack;
    property Color:      TColor index 1 read FColor      write SetColor default clWhite;
    property DialColor:  TColor index 2 read FDialColor  write SetColor default clBlack;

    property DialPenWidth:  byte index 0 read FDialPenWidth  write SetByte default 2;
    property ArrowPenWidth: byte index 1 read FArrowPenWidth write SetByte default 2;

    property ArrowShadow: TMeterShadowData index 0 read FArrowShadow write SetShadowData;
    property DialShadow:  TMeterShadowData index 0 read FDialShadow  write SetShadowData;
    property Stretched:   boolean index 0 read FStretched   write SetBoolean default False;
    property Transparent: boolean index 1 read FTransparent write SetBoolean default False;
  end;


{$IFDEF DELPHI_XE3}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]{$ENDIF}
  TsMeter = class(TGraphicControl)
{$IFNDEF NOTFORHELP}
  private
    FMin,
    FMax,
    FPosition,
    ArrowLength,
    UpdateCount,
    FGlyphIndex,
    FTickStepBig,
    FTickStepSmall: integer;

    FTextMax,
    FTextMin: string;

    FShowTicks,
    FShowMinMax,
    FShowCaption,
    FIgnoreBounds: boolean;

    FImages: TCustomImageList;
    FShowMinMaxValue: boolean;
    FContentType: TContentType;
    FShowCaptionValue: boolean;
    FPaintData: TMeterPaintData;
    FImageChangeLink: TChangeLink;
    procedure ImageListChange(Sender: TObject);
    procedure SetContentType(const Value: TContentType);
    procedure SetPaintData  (const Value: TMeterPaintData);
    procedure SetImages     (const Value: TCustomImageList);
    procedure SetInteger    (const Index, Value: integer);
    procedure SetBoolean    (const Index: Integer; const Value: boolean);
    procedure SetText       (const Index: Integer; const Value: string);
  protected
    Screw,
    Cache,
    ColorLine,
    ShadowLayer: TBitmap;

    Center: TPoint;
    MeterSize: TSize;
    procedure Paint; override;
    procedure PrepareCache;
    function GetMargin: integer;
    function GetMinRect: TRect;
    function GetMaxRect: TRect;
    procedure Init;
    function GetLineCoord(aAngle: real; aRadius: real = 0): TPoint; // Angle from 0 to 120
    function GetPaintColor(IsArrow: boolean): TColor;
    function PosToRad(Pos: integer): real;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function ScaleValue(Value: integer): integer;
  public
    function GetCaptionRect: TRect;
    constructor Create(AOwner: TComponent); override;
    procedure WndProc(var Message: TMessage); override;
    procedure AfterConstruction; override;
    destructor Destroy; override;
    procedure Loaded; override;
    procedure BeginUpdate;
    procedure EndUpdate(DoRepaint: boolean);
  published
    property Align;
    property Anchors;
    property Caption;
    property Font;
    property Constraints;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Height default DefaultSize;
    property Width default DefaultSize;
    property Visible;
{$ENDIF}
    property ContentType: TContentType    read FContentType write SetContentType default ctGradient;
    property PaintData:   TMeterPaintData read FPaintData   write SetPaintData;

    property Max:           integer index 0 read FMax           write SetInteger default 100;
    property Min:           integer index 1 read FMin           write SetInteger default 0;
    property Position:      integer index 2 read FPosition      write SetInteger default 40;
    property TickStepSmall: integer index 3 read FTickStepSmall write SetInteger default 5;
    property TickStepBig:   integer index 4 read FTickStepBig   write SetInteger default 25;
    property GlyphIndex:    integer index 5 read FGlyphIndex    write SetInteger default -1;

    property TextMax: string index 0 read FTextMax write SetText;
    property TextMin: string index 1 read FTextMin write SetText;

    property ShowCaption:      boolean index 0 read FShowCaption      write SetBoolean default True;
    property ShowMinMax:       boolean index 1 read FShowMinMax       write SetBoolean default True;
    property ShowTicks:        boolean index 2 read FShowTicks        write SetBoolean default True;
    property IgnoreBounds:     boolean index 3 read FIgnoreBounds     write SetBoolean default False;
    property ShowCaptionValue: boolean index 4 read FShowCaptionValue write SetBoolean default False;
    property ShowMinMaxValue:  boolean index 5 read FShowMinMaxValue  write SetBoolean default False;

    property Images: TCustomImageList read FImages write SetImages;

{$IFNDEF NOTFORHELP}
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
{$ENDIF}
  end;

implementation

uses Math,
  sGraphUtils, sVCLUtils, sCommonData, sConst, acntUtils, acPng, sAlphaGraph, sMessages, acgpUtils, sDefaults, sSkinManager;

{$R acMeter.res}

const
  s_Max = 'MAX';
  s_Min = 'MIN';
  ImgSize = DefaultSize - 2;
  MinSize = 16;

var
  resScrew,
  resColorLine: TBitmap;

procedure TsMeter.AfterConstruction;
begin
  inherited;
  if FTextMax = '' then
    FTextMax := s_Max;

  if FTextMin = '' then
    FTextMin := s_Min;
end;


procedure TsMeter.BeginUpdate;
begin
  inc(UpdateCount);
end;


constructor TsMeter.Create(AOwner: TComponent);
begin
  inherited;
  Height := DefaultSize;
  Width := DefaultSize;
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageListChange;
  FMax := 100;
  FMin := 0;
  FPosition := 40;
  FTickStepSmall := 5;
  FTickStepBig := 25;
  FGlyphIndex := -1;
  FShowCaption := True;
  FShowMinMax  := True;
  FShowTicks   := True;
  FIgnoreBounds := False;
  FShowCaptionValue := False;
  FShowMinMaxValue  := False;

  FContentType := ctGradient;
  FPaintData := TMeterPaintData.Create(Self);
  ControlStyle := ControlStyle + [csOpaque];

  Screw := TBitmap.Create;
  Screw.Assign(resScrew);
end;


destructor TsMeter.Destroy;
begin
  FPaintData.Free;
  Screw.Free;
  Cache.Free;
  ColorLine.Free;
  ShadowLayer.Free;
  FreeAndNil(FImageChangeLink);
  inherited;
end;


function TsMeter.ScaleValue(Value: integer): integer;
begin
  if DefaultManager <> nil then
    Result := DefaultManager.ScaleInt(Value)
  else
    Result := Value;
end;


procedure TsMeter.EndUpdate(DoRepaint: boolean);
begin
  dec(UpdateCount);
  if DoRepaint and (UpdateCount <= 0) then
    Repaint;
end;


function TsMeter.GetPaintColor(IsArrow: boolean): TColor;
var
  p: TPoint;
  C: TsColor;

  function GetGradColor: TColor;
  begin
    p := GetLineCoord(PosToRad(FPosition), 8 + GetMargin + ArrowLength);
    C := GetAPixel(ColorLine, p.X, p.Y);
    C.A := 0;
    Result := SwapRedBlue(C.C);
  end;

begin
  if IsArrow then
    if PaintData.FArrowColor = clNone then
      Result := GetGradColor
    else
      Result := PaintData.FArrowColor
  else
    if PaintData.FDialColor = clNone then
      Result := GetGradColor
    else
      Result := PaintData.FDialColor
end;


function TsMeter.GetCaptionRect: TRect;
begin
  Result.Top    := MeterSize.cy div 2 + MeterSize.cy div 7;
  Result.Left   := MeterSize.cx div 4;
  Result.Bottom := MeterSize.cy - MeterSize.cy div 7;
  Result.Right  := MeterSize.cx - MeterSize.cx div 4;
end;


function TsMeter.GetLineCoord(aAngle: real; aRadius: real = 0): TPoint;
begin
  if aRadius = 0 then begin
    Result.X := Center.X - Round(cos(aAngle) * ArrowLength);
    Result.Y := Center.Y - Round(sin(aAngle) * ArrowLength);
  end
  else begin
    Result.X := Center.X - Round(cos(aAngle) * aRadius);
    Result.Y := Center.Y - Round(sin(aAngle) * aRadius);
  end;
end;


function TsMeter.GetMargin: integer;
begin
  Result := 2;
end;


function TsMeter.GetMaxRect: TRect;
var
  Size: TSize;
begin
  Size := acTextExtent(Cache.Canvas, TextMax);
  Result.Left   := MeterSize.cx - Center.X div 7 * 2 - Size.cx;
  Result.Top    := Center.Y + Center.X div 3 + 4;
  Result.Right  := Result.Left + 10;
  Result.Bottom := Result.Top + 10;
end;


function TsMeter.GetMinRect: TRect;
begin
  Result.Left   := Center.X div 7 * 2;
  Result.Top    := Center.Y + Center.X div 3 + 4;
  Result.Right  := Result.Left + 10;
  Result.Bottom := Result.Top + 10;
end;


procedure TsMeter.Init;
begin
  if (Width > MinSize) and (Height > MinSize) then begin
    if not FPaintData.Stretched then begin
      MeterSize.cx := ScaleValue(ImgSize);
      MeterSize.cy := ScaleValue(ImgSize);
    end
    else begin
      MeterSize.cx := Width;
      MeterSize.cy := Height;
    end;

    if FPaintData.Stretched then begin
      MeterSize.cx := Math.min(Width, Height);
      MeterSize.cy := MeterSize.cx;
    end;

    Center.X := MeterSize.cx div 2;
    Center.Y := Center.X;

    ArrowLength := Center.X div 5 * 3;

    ColorLine.Free;
    if (MeterSize.cx = resColorLine.Width) and (MeterSize.cy = resColorLine.Height) then begin
      ColorLine := TBitmap.Create;
      ColorLine.Assign(resColorLine)
    end
    else begin
      ColorLine := CreateBmp32(MeterSize.cx, MeterSize.cy);
      Stretch(resColorLine, ColorLine, ColorLine.Width, ColorLine.Height, ftMitchell);
    end;

    Cache.Free;
    Cache := CreateBmp32(MeterSize);
  end;
end;


procedure TsMeter.Loaded;
begin
  inherited;
  if FTextMax = '' then
    FTextMax := s_Max;

  if FTextMin = '' then
    FTextMin := s_Min;
end;


procedure TsMeter.Paint;
var
  BGInfo: TacBGInfo;

  procedure FillBG(DC: hdc; R: TRect);
  begin
    if BGInfo.BgType = btCache then
      BitBlt(DC, R.Left, R.Top, WidthOf(R), HeightOf(R), BGInfo.Bmp.Canvas.Handle, Left + R.Left + BGInfo.Offset.X, Top + R.Top +BGInfo.Offset.Y, SRCCOPY)
    else
      if BGInfo.BgType = btFill then
        FillDC(DC, R, BGInfo.Color)
      else
        FillDC(DC, R, TsAccessControl(Parent).Color);
  end;

begin
  if (Width > MinSize) and (Height > MinSize) and (UpdateCount <= 0) then begin
    Init;
    BGInfo.BgType := btUnknown;
    BGInfo.PleaseDraw := False;
    BGInfo.FillRect := MkRect;
    SendMessage(Parent.Handle, SM_ALPHACMD, AC_GETBG_HI, LPARAM(@BGInfo));

    FillBG(Cache.Canvas.Handle, MkRect(Cache));
    PrepareCache;

    if Cache.Width < Width then
      FillBG(Canvas.Handle, Rect(Cache.Width, 0, Width, Height));

    if Cache.Height < Height then
      FillBG(Canvas.Handle, Rect(0, Cache.Height, Width, Height));

    BitBlt(Canvas.Handle, 0, 0, Cache.Width, Cache.Height, Cache.Canvas.Handle, 0, 0, SRCCOPY);
  end;
end;


function TsMeter.PosToRad(Pos: integer): real;
const
  ExtAngle = 20;
var
  Grad: real;
  iRange, iPos: integer;
begin
  iRange := FMax - FMin;
  iPos := Pos - FMin;
  Grad := ((180 + 2 * ExtAngle) / iRange) * iPos - ExtAngle;
  Result := Grad * Pi / 180;
end;


procedure TsMeter.PrepareCache;
const
  ShadowOffset: TPoint = (X: 1; Y: 1);
var
  R: TRect;
  Rad: real;
  s: string;
  C_: TsColor;
  MinMask: byte;
  RealTickStepSmall, Delta: integer;
  TextSize: TSize;
  CustomBmp: TBitmap;
  S0, SA: PRGBAArray_;
  Coord1, Coord2: TPoint;
  x, y, i, gr, Margin,
  DialPenWidth, DialShadowSize, ArrowPenWidth, ArrowShadowSize: integer;
  ShMaskCircle, ShMaskArrow: TsColor;

  function PaintText: TRect;
  begin
    Cache.Canvas.Font.Assign(Font);
    Cache.Canvas.Brush.Style := bsClear;
    SelectObject(Cache.Canvas.Handle, Cache.Canvas.Font.Handle);
    if ShowMinMax then begin
      R := GetMinRect;
      if ShowMinMaxValue then
        acDrawText(Cache.Canvas.Handle, IntToStr(Min), R, DT_NOCLIP)
      else
        acDrawText(Cache.Canvas.Handle, FTextMin, R, DT_NOCLIP);

      R := GetMaxRect;
      if ShowMinMaxValue then
        acDrawText(Cache.Canvas.Handle, IntToStr(Max), R, DT_NOCLIP)
      else
        acDrawText(Cache.Canvas.Handle, FTextMax, R, DT_NOCLIP);
    end;
    Result := GetCaptionRect;
    if ShowCaption then
      if ShowCaptionValue then
        acDrawText(Cache.Canvas.Handle, IntToStr(Position), Result, DT_NOCLIP or DT_CENTER or DT_VCENTER or DT_WORDBREAK)
      else
        acDrawText(Cache.Canvas.Handle, Caption, Result, DT_NOCLIP or DT_CENTER or DT_VCENTER or DT_WORDBREAK)
  end;

  procedure BlendTransBmpByMask(SrcBmp, MskBmp: Graphics.TBitMap; const BlendColor: TsColor);
  var
    S0, S: PRGBAArray_;
    DeltaS, X, Y: Integer;
  begin
    if InitLine(MskBmp, Pointer(S0), DeltaS) then
      for Y := 0 to MskBmp.Height - 1 do begin
        S := Pointer(PAnsiChar(S0) + DeltaS * Y);
        for X := 0 to MskBmp.Width - 1 do
          with S[X] do
            if I = 16777215 then
              I := -1;
      end;

    BlendBmpByMask(SrcBmp, MskBmp, BlendColor);
  end;

  procedure DrawAngledText(Canvas: TCanvas; s: string; aAngle: integer; Pos: TPoint);
  begin
    SetBkMode(Canvas.Handle, TRANSPARENT);
    MakeAngledFont(Canvas.Handle, Canvas.Font, aAngle);
    TextOut(Canvas.Handle, Pos.X, Pos.Y, PChar(s), Length(s));
  end;

begin
  Margin := GetMargin;
  ShMaskCircle.A := 0;
  ShMaskCircle.R := PaintData.DialShadow.Transparency;
  ShMaskCircle.G := ShMaskCircle.R;
  ShMaskCircle.B := ShMaskCircle.R;
  ShMaskArrow.A := 0;
  ShMaskArrow.R := PaintData.ArrowShadow.Transparency;
  ShMaskArrow.G := ShMaskArrow.R;
  ShMaskArrow.B := ShMaskArrow.R;

  ArrowShadowSize := ScaleValue(PaintData.ArrowShadow.Size);
  ArrowPenWidth   := ScaleValue(PaintData.ArrowPenWidth);
  DialShadowSize  := ScaleValue(PaintData.DialShadow.Size);
  DialPenWidth    := ScaleValue(PaintData.DialPenWidth);

  // Shadows
  if FPaintData.FDialShadow.Visible and (ctCustomImage <> ContentType) or FPaintData.FArrowShadow.Visible then
    if ShadowLayer = nil then
      ShadowLayer := CreateBmp32(MeterSize.cx, MeterSize.cy)
    else begin
      ShadowLayer.Width := MeterSize.cx;
      ShadowLayer.Height := MeterSize.cy;
      FillDC(ShadowLayer.Canvas.Handle, MkRect(ShadowLayer), $FFFFFF);
    end;

  // Circle shadow
  if FPaintData.FDialShadow.Visible and (ctCustomImage <> ContentType) then begin
    i := Margin + DialPenWidth;
    gr := DialPenWidth div 2;
    acGPDrawEllipse(ShadowLayer.Canvas.Handle, i - gr + 1, i - gr + 1, MeterSize.cx - 2 * i + gr - 3, MeterSize.cy - 2 * i + gr - 3, ShMaskCircle.C, DialShadowSize);
    if InitLine(ShadowLayer, Pointer(S0), Delta) then
      for y := 0 to ShadowLayer.Height - 1 do begin
        SA := Pointer(PAnsiChar(S0) + Delta * Y);
        MinMask := mini(MaxByte, ShMaskCircle.R + (MaxByte - ShMaskCircle.R) * y * 3 div (ShadowLayer.Height * 2));
        for x := 0 to ShadowLayer.Width - 1 do begin
          SA[X].R := maxi(MinMask, SA[X].R);
          SA[X].G := SA[X].R;
          SA[X].B := SA[X].R;
        end;
      end;
  end;

  // Arrow shadow
  if FPaintData.FArrowShadow.Visible then begin
    acGPFillEllipse(ShadowLayer.Canvas.Handle,
                    Center.X - Screw.Width  div 2 - ArrowShadowSize div 2,
                    Center.Y - Screw.Height div 2 - ArrowShadowSize div 2,
                    Screw.Width + ArrowShadowSize,
                    Screw.Height + ArrowShadowSize,
                    ShMaskArrow.C);

    Coord1 := GetLineCoord(PosToRad(FPosition));
    acGPDrawLine(ShadowLayer.Canvas.Handle, Coord1.X + ShadowOffset.X, Coord1.Y + ShadowOffset.Y, Center.X + ShadowOffset.X, Center.Y + ShadowOffset.Y, ShMaskArrow.C, ArrowShadowSize + ArrowPenWidth - 2);
  end;

  if ctCustomImage <> ContentType then begin
    // BG fill
    if not FPaintData.Transparent then
      acGPFillEllipse(Cache.Canvas.Handle, Margin, Margin, MeterSize.cx - 2 * Margin, MeterSize.cx - 2 * Margin, {FF000000 or }ColorToRGB(FPaintData.Color));

    if FShowTicks then begin
      RealTickStepSmall := math.max(TickStepSmall, (Max - Min) div 240);
      i := Min;
      if FContentType = ctGradient then
        PaintBmp32(Cache, ColorLine);

      // Ticks big
      Cache.Canvas.Font.Assign(Font);
      while i <= Max do begin
        if i mod TickStepBig = 0 then begin
          Rad := PosToRad(i);
          Coord1 := GetLineCoord(Rad, Center.X - Margin - 1);
          Coord2 := GetLineCoord(Rad, Center.X - ArrowLength div 4);
          acGPDrawLine(Cache.Canvas.Handle, Coord1.X, Coord1.Y, Coord2.X, Coord2.Y, Font.Color, ArrowPenWidth);
          if FContentType = ctValues then begin
            s := IntToStr(i);
            TextSize := GetStringSize(Cache.Canvas.Handle, s);
            gr := 90 - Round(Rad * 180 / Pi);

            Coord2.X := Round(Coord2.X - sin(Rad) * (TextSize.cx / 2) + sin(Rad));
            Coord2.Y := Round(Coord2.Y + cos(Rad) * (TextSize.cy / 2) - 2 * cos(Rad));

            DrawAngledText(Cache.Canvas, IntToStr(i), gr * 10, Coord2);
          end;
        end;
        inc(i, RealTickStepSmall);
      end;
      // Ticks small
      i := Min;
      while i <= Max do begin
        if i mod TickStepBig <> 0 then begin
          Coord1 := GetLineCoord(PosToRad(i), Center.X - Margin - 1);
          Coord2 := GetLineCoord(PosToRad(i), Center.X - ArrowLength div 5);
          acGPDrawLine(Cache.Canvas.Handle, Coord1.X, Coord1.Y, Coord2.X, Coord2.Y, BlendColors(Font.Color, PaintData.Color, DefBlendDisabled), ArrowPenWidth);
        end;
        inc(i, RealTickStepSmall);
      end;
    end;
  end
  else
    if not FPaintData.BackgroundImage.Empty then begin
      if (MeterSize.cx = resColorLine.Width) and (MeterSize.cy = resColorLine.Height) then begin
        CustomBmp := TBitmap.Create;
        CustomBmp.Assign(FPaintData.BackgroundImage)
      end
      else begin
        CustomBmp := CreateBmp32(MeterSize.cx, MeterSize.cy);
        Stretch(FPaintData.BackgroundImage, CustomBmp, CustomBmp.Width, CustomBmp.Height, ftMitchell);
      end;

      if CustomBmp.PixelFormat <> pf32bit then
        BitBlt(Cache.Canvas.Handle, 0, 0, CustomBmp.Width, CustomBmp.Height, CustomBmp.Canvas.Handle, 0, 0, SRCCOPY)
      else
        PaintBmp32(Cache, CustomBmp);

      CustomBmp.Free;
    end;

  C_.C := 0;
  if FPaintData.FDialShadow.Visible and (ctCustomImage <> ContentType) or FPaintData.FArrowShadow.Visible then
    BlendTransBmpByMask(Cache, ShadowLayer, C_);

  C_.I := GetPaintColor(False);
  // Circle
  if (ctCustomImage <> ContentType) and (DialPenWidth > 0) then
    acGPDrawEllipse(Cache.Canvas.Handle, Margin, Margin, MeterSize.cx - 2 * Margin - DialPenWidth div 2, MeterSize.cy - 2 * Margin - DialPenWidth div 2, C_.C, DialPenWidth);


  // Arrow
  Coord1 := GetLineCoord(PosToRad(FPosition));
  acGPDrawLine(Cache.Canvas.Handle, Coord1.X, Coord1.Y, Center.X, Center.Y, GetPaintColor(True), ArrowPenWidth);


  C_.C := GetPaintColor(True);
  acGPfILLEllipse(Cache.Canvas.Handle, Center.X - 8, Center.y - 8, 15, 15, GetPaintColor(True));


  R := PaintText;
  if (Images <> nil) and (FGlyphIndex >= 0) then begin
    Coord1.X := Center.X - Images.Width div 2;
    Coord1.Y := MeterSize.cy - Images.Height - MeterSize.cy div 14;
    Images.Draw(Cache.Canvas, Coord1.X, Coord1.Y, FGlyphIndex);
  end;
  PaintBmpRect32(Cache, Screw, MkRect(Screw), Point(Center.X - Screw.Width div 2, Center.Y - Screw.Height div 2));
end;


procedure TsMeter.SetBoolean(const Index: Integer; const Value: boolean);

  procedure ChangeProp(var Prop: boolean; Value: boolean);
  begin
    if Prop <> Value then begin
      Prop := Value;
      Repaint;
    end;
  end;

begin
  case Index of
    0: ChangeProp(FShowCaption, Value);
    1: ChangeProp(FShowMinMax, Value);
    2: ChangeProp(FShowTicks, Value);
    3: ChangeProp(FIgnoreBounds, Value);
    4: ChangeProp(FShowCaptionValue, Value);
    5: ChangeProp(FShowMinMaxValue, Value);
  end;
end;


procedure TsMeter.SetContentType(const Value: TContentType);
begin
  if FContentType <> Value then begin
    FContentType := Value;
    Repaint;
  end;
end;


procedure TsMeter.SetInteger(const Index, Value: integer);

  procedure ChangeProp(var Prop: integer; Value: integer);
  begin
    if Prop <> Value then begin
      Prop := Value;
      Repaint;
    end;
  end;

begin
  case Index of
    0: ChangeProp(FMax, Value);
    1: ChangeProp(FMin, Value);

    2: if FPosition <> Value then begin
      if not FIgnoreBounds and (Max <> Min) then
        FPosition := math.max(math.min(Value, Max), Min)
      else
        FPosition := Value;

      Repaint;
    end;

    3: ChangeProp(FTickStepSmall, Value);
    4: ChangeProp(FTickStepBig, Value);
    5: ChangeProp(FGlyphIndex, Value);
  end;
end;


constructor TMeterShadowData.Create(AOwner: TMeterPaintData);
begin
  FOwner := AOwner;
  FVisible := True;
  FSize := 5;
  FTransparency := 200;
end;


procedure TMeterShadowData.SetVisible(const Value: boolean);
begin
  if FVisible <> Value then begin
    FVisible := Value;
    FOwner.FOwner.Repaint;
  end;
end;


procedure TMeterShadowData.SetSize(const Value: byte);
begin
  if FSize <> Value then begin
    FSize := Value;
    FOwner.FOwner.Repaint;
  end;
end;


procedure TMeterShadowData.SetTransparency(const Value: byte);
begin
  if FTransparency <> Value then begin
    FTransparency := Value;
    FOwner.FOwner.Repaint;
  end;
end;


constructor TMeterPaintData.Create(AOwner: TGraphicControl);
begin
  FOwner := AOwner;
  Color := clWhite;
  FTransparent := False;
  FStretched := False;
  FArrowColor := clBlack;
  FDialShadow  := TMeterShadowData.Create(Self);
  FArrowShadow := TMeterShadowData.Create(Self);
  FDialPenWidth  := 2;
  FArrowPenWidth := 2;
  FBackgroundImage := TBitmap.Create;
end;


destructor TMeterPaintData.Destroy;
begin
  FDialShadow.Free;
  FArrowShadow.Free;
  FBackgroundImage.Free;
  inherited;
end;


procedure TMeterPaintData.SetBackgroundImage(const Value: TBitmap);
begin
  FBackgroundImage.Assign(Value);
  if not (csLoading in FOwner.ComponentState) then
    FOwner.Repaint;
end;


procedure TMeterPaintData.SetBoolean(const Index: Integer; const Value: boolean);

  procedure ChangeProp(var Prop: boolean; Value: boolean);
  begin
    if Prop <> Value then begin
      Prop := Value;
      FOwner.Repaint;
    end;
  end;

begin
  case Index of
    0: ChangeProp(FStretched, Value);
    1: ChangeProp(FTransparent, Value);
  end;
end;


procedure TMeterPaintData.SetColor(const Index: Integer; const Value: TColor);

  procedure ChangeProp(var Prop: TColor; Value: TColor);
  begin
    if Prop <> Value then begin
      Prop := Value;
      FOwner.Repaint;
    end;
  end;

begin
  case Index of
    0: ChangeProp(FArrowColor, Value);
    1: ChangeProp(FColor, Value);
    2: ChangeProp(FDialColor, Value);
  end;
end;


procedure TMeterPaintData.SetByte(const Index: Integer; const Value: byte);

  procedure ChangeProp(var Prop: byte; Value: byte);
  begin
    if Prop <> Value then begin
      Prop := Value;
      FOwner.Repaint;
    end;
  end;

begin
  case Index of
    0: ChangeProp(FDialPenWidth, Value);
    1: ChangeProp(FArrowPenWidth, Value);
  end;
end;


procedure TMeterPaintData.SetShadowData(const Index: Integer; const Value: TMeterShadowData);
begin
  case Index of
    0: if FArrowShadow <> Value then begin
      FArrowShadow.Assign(Value);
      FOwner.Repaint;
    end;

    1: if FDialShadow <> Value then begin
      FDialShadow.Assign(Value);
      FOwner.Repaint;
    end;
  end;
end;


procedure TsMeter.SetPaintData(const Value: TMeterPaintData);
begin
  FPaintData.Assign(Value);
end;


procedure TsMeter.SetImages(const Value: TCustomImageList);
begin
  if Images <> Value then begin
    if Images <> nil then
      Images.UnRegisterChanges(FImageChangeLink);

    FImages := Value;
    if Images <> nil then begin
      Images.RegisterChanges(FImageChangeLink);
      Images.FreeNotification(Self);
    end;
    if (Visible or ([csDesigning, csLoading] * ComponentState = [])) then
      Repaint;
  end;
end;


procedure TsMeter.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = Images) then
    Images := nil;
end;


procedure TsMeter.ImageListChange(Sender: TObject);
begin
  Repaint;
end;


procedure TsMeter.SetText(const Index: Integer; const Value: string);

  procedure ChangeProp(var Prop: string; Value: string);
  begin
    if Prop <> Value then begin
      Prop := Value;
      Repaint;
    end;
  end;

begin
  case Index of
    0: ChangeProp(FTextMax, Value);
    1: ChangeProp(FTextMin, Value);
  end;
end;


procedure TsMeter.WndProc(var Message: TMessage);
begin
  inherited;
  case Message.Msg of
    CM_TEXTCHANGED:
      if not (csDestroying in ComponentState) then
        Repaint;
  end;
end;


function LoadBmpFromRes(Name: string): TBitmap;
var
  rs: TResourceStream;
begin
  rs := TResourceStream.Create(hInstance, Name, RT_RCDATA);
  Result := TPNGGraphic.Create;
  Result.LoadFromStream(rs);
  rs.Free;
end;


initialization
  resScrew := LoadBmpFromRes('Screw');
  resColorLine := LoadBmpFromRes('ColorLine');


finalization
  resScrew.Free;
  resColorLine.Free;

end.
