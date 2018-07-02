unit acgpUtils;
{$I sDefs.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  sConst, sGradient;

procedure acgpDrawLine   (DC: hdc; X1, Y1, X2, Y2: Single; Color: TColor; PenWidth: Single = 1);
procedure acgpDrawEllipse(DC: hdc; X, Y, Width, Height: Single; Color: TColor; PenWidth: Single = 1);
procedure acgpFillEllipse(DC: hdc; X, Y, Width, Height: Single; Color: TColor);
procedure acgpDrawArc    (DC: hdc; X, Y, Width, Height, StartAngle, SweepAngle: Single; Color: TColor; PenWidth: Single = 1);
procedure acgpStretchRect(DstBmp, SrcBmp: TBitmap; SrcX, SrcY, SrcWidth, SrcHeight: integer); overload;
procedure acgpStretchRect(DstBmp, SrcBmp: TBitmap; DstRect, SrcRect: TRect); overload;
procedure acgpBlur       (Bmp: TBitmap; Radius: Single);

{$IFNDEF NOTFORHELP}
function acgpCreateLineBrush(DC: hdc; Point1, Point2: TPoint; Color1, Color2: TColor): Pointer;
{$ENDIF} // NOTFORHELP

procedure acgpFillRectangle     (DC: hdc; Brush: Pointer; X, Y, Width, Height: Single);
procedure acgpGradientEllipse   (DC: hdc; X, Y, Width, Height: Single; Color1, Color2: TColor);
procedure acgpGradientRing      (DC: hdc; X, Y, Width, Height: Single; Color1, Color2: TColor; PenWidth: Single = 1);
procedure acgpGradientRectangleH(DC: hdc; X, Y, Width, Height: Single; Color1, Color2: TColor);
procedure acgpGradientRectangleV(DC: hdc; X, Y, Width, Height: Single; Color1, Color2: TColor);
procedure acgpGradientRectangle (DC: hdc; X, Y, Width, Height: Single; Points: TGradPoints; Colors: TGradColors; CenterColor: TsColor);

implementation

uses
  Math,
  sSkinProvider, sAlphaGraph, acntTypes, acntUtils, sGraphUtils;

type
  TStatus = (Ok, GenericError, InvalidParameter, OutOfMemory, ObjectBusy, InsufficientBuffer, NotImplemented, Win32Error, WrongState,
    Aborted, FileNotFound, ValueOverflow, AccessDenied, UnknownImageFormat, FontFamilyNotFound, FontStyleNotFound, NotTrueTypeFont,
    UnsupportedGdiplusVersion, GdiplusNotInitialized, PropertyNotFound, PropertyNotSupported
  );

  TUnit = (UnitWorld, UnitDisplay, UnitPixel, UnitPoint, UnitInch, UnitDocument, UnitMillimeter);
  TDebugEventLevel = (DebugEventLevelFatal, DebugEventLevelWarning);

  TacgpWrapMode = (
    WrapModeTile,        // 0
    WrapModeTileFlipX,   // 1
    WrapModeTileFlipY,   // 2
    WrapModeTileFlipXY,  // 3
    WrapModeClamp        // 4
  );

  TGpPen      = Pointer;
  TGpGraphics = Pointer;
  TGpBrush    = Pointer;
  TGpBitmap   = Pointer;
  TGpImage    = Pointer;

  TPointF = record
    X: Single;
    Y: Single;
  end;
  PPointF = ^TPointF;

  TGpPoints = array [0..3] of TPointF;
  PGpPoints = ^TGpPoints;

  PARGB = ^ARGB;
  ARGB  = DWORD;

  NotificationUnhookProc = procedure (token: ULONG); stdcall;
  DebugEventProc         = procedure (level: TDebugEventLevel; message: PChar); stdcall;
  NotificationHookProc   = function (out token: ULONG): TStatus; stdcall;

  TGdiplusStartupInput = record
    GdiplusVersion: Cardinal;
    DebugEventCallback: DebugEventProc;
    SuppressBackgroundThread: BOOL;
    SuppressExternalCodecs: BOOL;
  end;
  PGdiplusStartupInput = ^TGdiplusStartupInput;

  TGdiplusStartupOutput = record
    NotificationHook  : NotificationHookProc;
    NotificationUnhook: NotificationUnhookProc;
  end;
  PGdiplusStartupOutput = ^TGdiplusStartupOutput;

  ImageAbort = function: BOOL; stdcall;
  DrawImageAbort = ImageAbort;

  TCompositingMode = (
    CompositingModeSourceOver, // 0
    CompositingModeSourceCopy  // 1 - faster
  );

{$IFDEF DELPHI6UP}
  TQualityMode = (
    QualityModeInvalid = -1,
    QualityModeDefault = 0,
    QualityModeLow     = 1,
    QualityModeHigh    = 2
  );

  TSmoothingMode = (
    SmoothingModeInvalid     = -1,
    SmoothingModeDefault     = 0,
    SmoothingModeHighSpeed   = 1,
    SmoothingModeHighQuality = 2,
    SmoothingModeNone,
    SmoothingModeAntiAlias8x4,
    SmoothingModeAntiAlias = SmoothingModeAntiAlias8x4,
    SmoothingModeAntiAlias8x8
  );

  TCompositingQuality = (
    CompositingQualityInvalid          = ord(QualityModeInvalid),
    CompositingQualityDefault          = ord(QualityModeDefault),
    CompositingQualityHighSpeed        = ord(QualityModeLow),
    CompositingQualityHighQuality      = ord(QualityModeHigh),
    CompositingQualityGammaCorrected,
    CompositingQualityAssumeLinear
  );
{$ELSE}
  TQualityMode = Integer;
  TSmoothingMode = Integer;
  TCompositingQuality = Integer;

const
  QualityModeInvalid = -1;
  QualityModeDefault = 0;
  QualityModeLow     = 1;
  QualityModeHigh    = 2;

  SmoothingModeInvalid     = -1;
  SmoothingModeDefault     = 0;
  SmoothingModeHighSpeed   = 1;
  SmoothingModeHighQuality = 2;
  SmoothingModeNone        = 3;
  SmoothingModeAntiAlias   = 4;

  CompositingQualityInvalid          = QualityModeInvalid;
  CompositingQualityDefault          = QualityModeDefault;
  CompositingQualityHighSpeed        = QualityModeLow;
  CompositingQualityHighQuality      = QualityModeHigh;
  CompositingQualityGammaCorrected   = 3;
  CompositingQualityAssumeLinear     = 4;

{$ENDIF}

type

  PGPRect = ^TGPRect;
  TGPRect = record
    X     : Integer;
    Y     : Integer;
    Width : Integer;
    Height: Integer;
  end;

  TBitmapData = record
    Width       : UINT;
    Height      : UINT;
    Stride      : Integer;
    PixelFormat : integer;
    Scan0       : Pointer;
    Reserved    : {$IFDEF DELPHI7UP}NativeUInt{$ELSE}Cardinal{$ENDIF};
  end;
  PBitmapData = ^TBitmapData;

var
  acDefaultSmoothingMode: TSmoothingMode = SmoothingModeHighQuality;
  StartupInput: TGDIPlusStartupInput;
  hGP: HMODULE = 0;
  gdiplusToken: ULONG;

  acGdipCreateFromHDC:         function (hdc: HDC; out graphics: TGpGraphics): TStatus; stdcall;
  acGdipCreatePen1:            function (Color: ARGB; Width: Single; unit_: TUnit; out pen: TGpPen): TStatus; stdcall;
  acGdipCreateSolidFill:       function (color: ARGB; out brush: TGpBrush): TStatus; stdcall;
  acGdipSetSmoothingMode:      function (graphics: TGpGraphics; smoothingMode: TSmoothingMode): TStatus; stdcall;
  acGdipDrawEllipse:           function (graphics: TGpGraphics; pen: TGpPen; X, Y, Width, Height: Single): TStatus; stdcall;
  acGdipFillEllipse:           function (graphics: TGpGraphics; brush: TGpBrush; x, y, width, height: Single): TStatus; stdcall;
  acGdipDrawLine:              function (graphics: TGpGraphics; pen: TGpPen; x1, y1, x2, y2: Single): TStatus; stdcall;
  acGdipDrawArc:               function (graphics: TGpGraphics; pen: TGpPen; X, Y, Width, Height, StartAngle, SweepAngle: Single): TStatus; stdcall;
  acGdiplusStartup:            function (out token: ULONG; input: PGdiplusStartupInput; output: PGdiplusStartupOutput): TStatus; stdcall;
  acGdiplusShutdown:           procedure (token: ULONG); stdcall;
  acGdipDeleteGraphics:        function (graphics: TGpGraphics): TStatus; stdcall;
  acGdipSetCompositingQuality: function (graphics: TGpGraphics; compositingQuality: TCompositingQuality): TStatus; stdcall;
  acGdipSetCompositingMode:    function (graphics: TGpGraphics; compositingMode: TCompositingMode): TStatus; stdcall;
  acGdipDisposeImage:          function (Image: Pointer): TStatus; stdcall;
  acGdipBitmapLockBits:        function (bitmap: TGpBitmap; rect: PGPRect; flags: UINT; format: integer; lockedBitmapData: PBITMAPDATA): TStatus; stdcall;
  acGdipCreateBitmapFromScan0: function (width: Integer; height: Integer; stride: Integer; format: integer; scan0: PBYTE; out bitmap: TGpBitmap): TStatus; stdcall;
  acGdipBitmapUnlockBits:      function (bitmap: TGpBitmap; lockedBitmapData: PBITMAPDATA): TStatus; stdcall;
  acGdipCreateLineBrush:       function (Point1, Point2: PPointF; Color1, Color2: ARGB; WrapMode: TacgpWrapMode; out LineGradient: Pointer): TStatus; stdcall;
  acGdipFillRectangle:         function (Graphics: TGpGraphics; Brush: TGpBrush; X: Single; Y: Single; Width: Single; Height: Single): TStatus; stdcall;
  acGdipDeleteBrush:           function (Brush: TGpBrush): TStatus; stdcall;
  acGdipDeletePen:             function (Pen: TGpPen): TStatus; stdcall;
  acGdipCreateEffect:          function (Guid: TGUID; out Effect: Pointer): TStatus; stdcall;
  acGdipSetEffectParameters:   function (Effect: Pointer; const Params: Pointer; const Size: UINT): TStatus; stdcall;

  acGdipCreatePathGradient:    function (const Points: PPointF; Count: Integer; WrapMode: TacgpWrapMode; out PolyGradient: Pointer): TStatus; stdcall;
  acGdipSetPathGradientCenterColor: function (Brush: TGpBrush; Color: ARGB): TStatus; stdcall;
  acGdipSetPathGradientSurroundColorsWithCount: function (Brush: TGpBrush; const Color: PARGB; out Count: Integer): TStatus; stdcall;
  acSetPenBrushFill:           function (Pen: TGpPen; Brush: TGpBrush): TStatus; stdcall;

  acGdipBitmapCreateApplyEffect: function (const InputBitmaps: Pointer; NumInputs: Integer; Effect: Pointer; Roi: Windows.PRect; OutputRect: Windows.PRect;
                                           out OutputBitmap: Pointer; UseAuxData: Bool; out AuxData: Pointer; out AuxDataSize: Integer): TStatus; stdcall;

  acGdipCreateHBITMAPFromBitmap: function(Bitmap: Pointer; out HbmReturn: HBitmap; Background: ARGB): TStatus; stdcall;
  acGdipCreateBitmapFromHBITMAP: function(Hbm: HBitmap; Hpal: HPalette; out Bitmap: Pointer): TStatus; stdcall;
  acGdipDrawImageRectRect:       function(Graphics: TGpGraphics; image: TGpImage;
                                         dstx: Single; dsty: Single; dstwidth: Single; dstheight: Single;
                                         srcx: Single; srcy: Single; srcwidth: Single; srcheight: Single;
                                         srcUnit: TUnit; imageAttributes: Pointer; callback: DRAWIMAGEABORT; callbackData: Pointer): TStatus; stdcall;

procedure acGDIBegin(Allowed: boolean);
begin
  if Allowed and (hGP <> 0) then
    if Assigned(acGdiplusStartup) then begin
      StartupInput.DebugEventCallback := nil;
      StartupInput.SuppressBackgroundThread := False;
      StartupInput.SuppressExternalCodecs := False;
      StartupInput.GdiplusVersion := 1;
      acGdiplusStartup(gdiplusToken, @StartupInput, nil);
    end;
end;


procedure acGDIEnd(Allowed: boolean);
begin
  if Allowed and (hGP <> 0) then
    acGdiplusShutdown(gdiplusToken);
end;


procedure acgpDrawEllipse(DC: hdc; X, Y, Width, Height: Single; Color: TColor; PenWidth: Single = 1);
var
  GpP: TGpPen;
  GpG: TGpGraphics;
begin
  acGDIBegin(IsLibrary);
  try
    if Assigned(acGdipDrawEllipse) and (acGdipCreateFromHDC(DC, GpG) = Ok) then
      try
        acGdipSetSmoothingMode(GpG, acDefaultSmoothingMode);
        if acGdipCreatePen1($FF000000 or Cardinal(SwapRedBlue(Color)), PenWidth, UnitWorld, GpP) = Ok then begin
          acGdipDrawEllipse(GpG, GpP, X, Y, Width, Height);
          acGdipDeletePen(GpP);
        end;
      finally
        acGdipDeleteGraphics(GpG);
      end;
  finally
    acGDIEnd(IsLibrary);
  end;
end;


procedure acgpDrawLine(DC: hdc; X1, Y1, X2, Y2: Single; Color: TColor; PenWidth: Single = 1);
var
  C: TsColor;
  GpP: TGpPen;
  GpG: TGpGraphics;
begin
  acGDIBegin(IsLibrary);
  try
    if Assigned(acGdipDrawLine) and (acGdipCreateFromHDC(DC, GpG) = Ok) then
      try
        acGdipSetSmoothingMode(GpG, acDefaultSmoothingMode);
        C.I := SwapRedBlue(Color);
        if C.A = 0 then
          C.I := integer($FF000000 or Cardinal(C.C));

        if acGdipCreatePen1(Cardinal(C.I), PenWidth, UnitWorld, GpP) = Ok then begin
          acGdipDrawLine(GpG, GpP, X1, Y1, X2, Y2);
          acGdipDeletePen(GpP);
        end;
      finally
        acGdipDeleteGraphics(GpG);
      end;
  finally
    acGDIEnd(IsLibrary);
  end;
end;


procedure acgpFillEllipse(DC: hdc; X, Y, Width, Height: Single; Color: TColor);
var
  GpB: TGpBrush;
  GpG: TGpGraphics;
begin
  acGDIBegin(IsLibrary);
  try
    if Assigned(acGdipFillEllipse) and (acGdipCreateFromHDC(DC, GpG) = Ok) then
      try
        acGdipSetSmoothingMode(GpG, acDefaultSmoothingMode);
        if acGdipCreateSolidFill($FF000000 or Cardinal(SwapRedBlue(Color)), GpB) = Ok then begin
          acGdipFillEllipse(GpG, GpB, X, Y, Width, Height);
          acGdipDeleteBrush(GpB);
        end;
      finally
        acGdipDeleteGraphics(GpG);
      end;
  finally
    acGDIEnd(IsLibrary);
  end;
end;


procedure acgpDrawArc(DC: hdc; X, Y, Width, Height, StartAngle, SweepAngle: Single; Color: TColor; PenWidth: Single = 1);
var
  GpP: TGpPen;
  GpG: TGpGraphics;
begin
  acGDIBegin(IsLibrary);
  try
    if Assigned(acGdipDrawEllipse) and (acGdipCreateFromHDC(DC, GpG) = Ok) then
      try
        acGdipSetSmoothingMode(GpG, acDefaultSmoothingMode);
        if acGdipCreatePen1($FF000000 or Cardinal(SwapRedBlue(Color)), PenWidth, UnitWorld, GpP) = Ok then begin
          acGdipDrawArc(GpG, GpP, X, Y, Width, Height, StartAngle, SweepAngle);
          acGdipDeletePen(GpP);
        end;
      finally
        acGdipDeleteGraphics(GpG);
      end;
  finally
    acGDIEnd(IsLibrary);
  end;
end;


procedure acgpStretchRect(DstBmp, SrcBmp: TBitmap; SrcX, SrcY, SrcWidth, SrcHeight: integer);
begin
  acgpStretchRect(DstBmp, SrcBmp, MkRect(DstBmp.Width, DstBmp.Height), Rect(SrcX, SrcY, SrcX + SrcWidth, SrcY + SrcHeight));
end;


procedure acgpStretchRect(DstBmp, SrcBmp: TBitmap; DstRect, SrcRect: TRect); overload;
const
  PixelFormat32bppARGB = 2498570;
type
  PGPColorRec = ^TsColor_;
var
  GpG: TGpGraphics;
  bitmap: TGpBitmap;
  S0, S: PRGBAArray_;
  bmData: TBitmapData;
  Delta, X, Y: integer;
  gpaPColor: PGPColorRec;
begin
  acGDIBegin(IsLibrary);
  try
    if acGdipCreateFromHDC(DstBmp.Canvas.Handle, GpG) = Ok then begin
      if acGdipCreateBitmapFromScan0(SrcBmp.Width, SrcBmp.Height, SrcBmp.Width * 4, PixelFormat32bppARGB, nil, bitmap) = OK then begin
        if InitLine(SrcBmp, Pointer(S0), Delta) then begin
          acGdipBitmapLockBits(bitmap, nil, 3{ImageLockModeRead or ImageLockModeWrite}, PixelFormat32bppARGB, @bmData);
          for Y := 0 to SrcBmp.Height - 1 do begin
            S := Pointer(PAnsiChar(S0) + Delta * Y);
            gpaPColor := Pointer(Integer(bmData.Scan0) + Y * bmData.Stride);
            for X := 0 to SrcBmp.Width - 1 do begin
              gpaPColor.C := S[X].C;
              gpaPColor.A := 127 + S[X].A shr 1;
              inc(gpaPColor);
            end;
          end;
          acGdipBitmapUnlockBits(bitmap, @bmData);
        end;
        acGdipSetCompositingMode(GpG, CompositingModeSourceCopy);
        acGdipSetCompositingQuality(GpG, CompositingQualityHighSpeed);
        acGdipDrawImageRectRect(GpG, bitmap, DstRect.Left, DstRect.Top, WidthOf(DstRect), HeightOf(DstRect), SrcRect.Left, SrcRect.Top, WidthOf(SrcRect), HeightOf(SrcRect), UnitPixel, nil, nil, nil);
        acGdipDisposeImage(bitmap);
      end;
      acGdipDeleteGraphics(GpG);
      if InitLine(DstBmp, Pointer(S0), Delta) then
        for Y := 0 to DstBmp.Height - 1 do begin
          S := Pointer(PAnsiChar(S0) + Delta * Y);
          for X := 0 to DstBmp.Width - 1 do
            with S[X] do
              if A <> 0 then begin
                R := min(MaxByte, R shl 8 div A);
                G := min(MaxByte, G shl 8 div A);
                B := min(MaxByte, B shl 8 div A);
                A := max(0, (A + 1) shl 1 - 257);
              end;
        end;
    end;
  finally
    acGDIEnd(IsLibrary);
  end;
end;


const
  BlurEffectGuid: TGUID = '{633C80A4-1843-482b-9EF2-BE2834C5FDD4}';

type
  TBlurParams = record
    Radius: Single;
    ExpandEdge: BOOL;
  end;
  PBlurParams = ^TBlurParams;


procedure acgpBlur(Bmp: TBitmap; Radius: Single);
var
  BlurParams: TBlurParams;
  NativeHandle: Pointer;
  GPBitmap: Pointer;
  OutAux: Pointer;
  OutAuxSize: integer;
  OutBitmap: Pointer;
  OutBmp: HBitmap;
  R: TRect;
  NativeInputs: array of Pointer;
begin
  if not Bmp.Empty then begin
    acGDIBegin(IsLibrary);
    try
      acGdipCreateEffect(BlurEffectGuid, NativeHandle);
      BlurParams.Radius := Radius;
      BlurParams.ExpandEdge := False;
      acGdipSetEffectParameters(NativeHandle, @BlurParams, SizeOf(BlurParams));

      acGdipCreateBitmapFromHBITMAP(Bmp.Handle, Bmp.Palette, GPBitmap);
      R := MkRect(Bmp);
      if Assigned(acGdipBitmapCreateApplyEffect) then begin
        SetLength(NativeInputs, 1);
        NativeInputs[0] := GPBitmap;
        acGdipBitmapCreateApplyEffect(@NativeInputs[0], 1, NativeHandle, @R, @R, OutBitmap, False, OutAux, OutAuxSize);
        acGdipCreateHBITMAPFromBitmap(OutBitmap, OutBmp, 0);
        Bmp.Handle := OutBmp;
      end;
    finally
      acGDIEnd(IsLibrary);
    end;
  end;
end;


function acgpCreateLineBrush(DC: hdc; Point1, Point2: TPoint; Color1, Color2: TColor): Pointer;
var
  p1, p2: TPointF;
begin
  acGDIBegin(IsLibrary);
  try
    p1.X := Point1.X;
    p1.Y := Point1.Y;
    p2.X := Point2.X;
    p2.Y := Point2.Y;
    acGdipCreateLineBrush(@p1, @p2, Color1, Color2, WrapModeTileFlipXY, Result);
  finally
    acGDIEnd(IsLibrary);
  end;
end;


procedure acgpFillRectangle(DC: hdc; Brush: Pointer; X, Y, Width, Height: Single);
var
  GpG: TGpGraphics;
begin
  acGDIBegin(IsLibrary);
  try
    if acGdipCreateFromHDC(DC, GpG) = Ok then
      try
        acGdipFillRectangle(GpG, Brush, X, Y, Width, Height);
      finally
        acGdipDeleteGraphics(GpG);
      end;
  finally
    acGDIEnd(IsLibrary);
  end;
end;


procedure acgpGradientRing(DC: hdc; X, Y, Width, Height: Single; Color1, Color2: TColor; PenWidth: Single = 1);
var
  GpP: TGpPen;
  p1, p2: TPointF;
  GpG: TGpGraphics;
  Brush: TGpBrush;
begin
  acGDIBegin(IsLibrary);
  try
    if Assigned(acGdipDrawEllipse) and (acGdipCreateFromHDC(DC, GpG) = Ok) then
      try
        p1.X := X;
        p1.Y := Y - PenWidth;
        p2.X := X;
        p2.Y := Y + Height + PenWidth;
        acGdipCreateLineBrush(@p1, @p2, Cardinal(SwapRedBlue(Color1)), Cardinal(SwapRedBlue(Color2)), WrapModeTile, Brush);
        if acGdipCreatePen1($FFFFFFFF, PenWidth, UnitWorld, GpP) = Ok then
          if acSetPenBrushFill(GpP, Brush) = Ok then begin;
            acGdipSetSmoothingMode(GpG, acDefaultSmoothingMode);
            acGdipDrawEllipse(GpG, GpP, X, Y, Width, Height);
            acGdipDeletePen(GpP);
          end;
      finally
        acGdipDeleteBrush(Brush);
        acGdipDeleteGraphics(GpG);
      end;
  finally
    acGDIEnd(IsLibrary);
  end;
end;


procedure acgpGradientEllipse(DC: hdc; X, Y, Width, Height: Single; Color1, Color2: TColor);
var
  GpG: TGpGraphics;
  p1, p2: TPointF;
  Brush: Pointer;
begin
  acGDIBegin(IsLibrary);
  try
    if acGdipCreateFromHDC(DC, GpG) = Ok then
      try
        acGdipSetSmoothingMode(GpG, acDefaultSmoothingMode);

        p1.X := X;
        p1.Y := Y;
        p2.X := X;
        p2.Y := Y + Height;
        acGdipCreateLineBrush(@p1, @p2, Cardinal(SwapRedBlue(Color1)), Cardinal(SwapRedBlue(Color2)), WrapModeTile, Brush);

        acGdipFillEllipse(GpG, Brush, X, Y, Width, Height);
      finally
        acGdipDeleteBrush(Brush);
        acGdipDeleteGraphics(GpG);
      end;
  finally
    acGDIEnd(IsLibrary);
  end;
end;


procedure acgpGradientRectangleH(DC: hdc; X, Y, Width, Height: Single; Color1, Color2: TColor);
var
  GpG: TGpGraphics;
  p1, p2: TPointF;
  Brush: Pointer;
begin
  acGDIBegin(IsLibrary);
  try
    if acGdipCreateFromHDC(DC, GpG) = Ok then
      try
        p1.X := X;
        p1.Y := Y;
        p2.X := X + Width;
        p2.Y := Y;
        acGdipCreateLineBrush(@p1, @p2, Cardinal(SwapRedBlue(Color1)), Cardinal(SwapRedBlue(Color2)), WrapModeTile, Brush);
        acGdipFillRectangle(GpG, Brush, X, Y, Width, Height);
      finally
        acGdipDeleteBrush(Brush);
        acGdipDeleteGraphics(GpG);
      end;
  finally
    acGDIEnd(IsLibrary);
  end;
end;


procedure acgpGradientRectangleV(DC: hdc; X, Y, Width, Height: Single; Color1, Color2: TColor);
var
  GpG: TGpGraphics;
  p1, p2: TPointF;
  Brush: Pointer;
begin
  acGDIBegin(IsLibrary);
  try
    if acGdipCreateFromHDC(DC, GpG) = Ok then
      try
        p1.X := X;
        p1.Y := Y;
        p2.X := X;
        p2.Y := Y + Height;
        acGdipCreateLineBrush(@p1, @p2, Cardinal(SwapRedBlue(Color1)), Cardinal(SwapRedBlue(Color2)), WrapModeTile, Brush);
        acGdipFillRectangle(GpG, Brush, X, Y, Width, Height);
      finally
        acGdipDeleteBrush(Brush);
        acGdipDeleteGraphics(GpG);
      end;
  finally
    acGDIEnd(IsLibrary);
  end;
end;



procedure acgpGradientRectangle(DC: hdc; X, Y, Width, Height: Single; Points: TGradPoints; Colors: TGradColors; CenterColor: TsColor);
var
  GpG: TGpGraphics;
  Brush: Pointer;
  GpPoints: array [0..3] of TPointF;
  GpColors: array [0..3] of ARGB;
  c, i: integer;
begin
  acGDIBegin(IsLibrary);
  try
    if acGdipCreateFromHDC(DC, GpG) = Ok then
      try
        for i := 0 to 3 do begin
          GpPoints[i].X := Points[i].X;
          GpPoints[i].Y := Points[i].Y;
          GpColors[i] := DWORD(SwapRedBlue(Colors[i].I));
        end;

        acGdipCreatePathGradient(@GpPoints[0], Length(GpPoints), WrapModeClamp, Brush);
        acGdipSetPathGradientCenterColor(Brush, Cardinal(SwapRedBlue(CenterColor.I)));
        c := 4;
        acGdipSetPathGradientSurroundColorsWithCount(Brush, @GpColors[0], c);
        acGdipFillRectangle(GpG, Brush, X, Y, Width, Height);
      finally
        acGdipDeleteBrush(Brush);
        acGdipDeleteGraphics(GpG);
      end;
  finally
    acGDIEnd(IsLibrary);
  end;
end;


initialization
  if hGP = 0 then
    hGP := LoadLibrary('gdiplus.dll');

  if (hGP <> 0) and not Assigned(acGdipCreateFromHDC) then begin
    acGdipDrawArc                 := GetProcAddress(hGP, 'GdipDrawArc');
    acGdipDrawLine                := GetProcAddress(hGP, 'GdipDrawLine');
    acGdipCreatePen1              := GetProcAddress(hGP, 'GdipCreatePen1');
    acGdiplusStartup              := GetProcAddress(hGP, 'GdiplusStartup');
    acGdiplusShutdown             := GetProcAddress(hGP, 'GdiplusShutdown');
    acGdipDrawEllipse             := GetProcAddress(hGP, 'GdipDrawEllipse');
    acGdipFillEllipse             := GetProcAddress(hGP, 'GdipFillEllipse');
    acGdipFillRectangle           := GetProcAddress(hGP, 'GdipFillRectangle');
    acGdipCreateFromHDC           := GetProcAddress(hGP, 'GdipCreateFromHDC');
    acGdipCreateSolidFill         := GetProcAddress(hGP, 'GdipCreateSolidFill');
    acGdipSetSmoothingMode        := GetProcAddress(hGP, 'GdipSetSmoothingMode');
    acGdipDrawImageRectRect       := GetProcAddress(hGP, 'GdipDrawImageRectRect');
    acGdipCreateLineBrush         := GetProcAddress(hGP, 'GdipCreateLineBrush');
    acGdipSetCompositingMode      := GetProcAddress(hGP, 'GdipSetCompositingMode');
    acGdipSetCompositingQuality   := GetProcAddress(hGP, 'GdipSetCompositingQuality');
    acGdipDeleteGraphics          := GetProcAddress(hGP, 'GdipDeleteGraphics');
    acGdipDisposeImage            := GetProcAddress(hGP, 'GdipDisposeImage');
    acGdipBitmapLockBits          := GetProcAddress(hGP, 'GdipBitmapLockBits');
    acGdipCreateBitmapFromScan0   := GetProcAddress(hGP, 'GdipCreateBitmapFromScan0');
    acGdipBitmapUnlockBits        := GetProcAddress(hGP, 'GdipBitmapUnlockBits');
    acGdipCreatePathGradient      := GetProcAddress(hGP, 'GdipCreatePathGradient');
    acGdipDeleteBrush             := GetProcAddress(hGP, 'GdipDeleteBrush');
    acGdipDeletePen               := GetProcAddress(hGP, 'GdipDeletePen');
    acGdipCreateEffect            := GetProcAddress(hGP, 'GdipCreateEffect');
    acGdipSetEffectParameters     := GetProcAddress(hGP, 'GdipSetEffectParameters');
    acGdipBitmapCreateApplyEffect := GetProcAddress(hGP, 'GdipBitmapCreateApplyEffect');
    acGdipCreateBitmapFromHBITMAP := GetProcAddress(hGP, 'GdipCreateBitmapFromHBITMAP');
    acGdipCreateHBITMAPFromBitmap := GetProcAddress(hGP, 'GdipCreateHBITMAPFromBitmap');
    acSetPenBrushFill             := GetProcAddress(hGP, 'GdipSetPenBrushFill');
//    acGdipCreatePathGradientFromPath := GetProcAddress(hGP, 'GdipCreatePathGradientFromPath');
    acGdipSetPathGradientCenterColor := GetProcAddress(hGP, 'GdipSetPathGradientCenterColor');
    acGdipSetPathGradientSurroundColorsWithCount := GetProcAddress(hGP, 'GdipSetPathGradientSurroundColorsWithCount');

    acGDIBegin(not IsLibrary);
  end;


finalization
  acGDIEnd(not IsLibrary);
  if hGP <> 0 then
    FreeLibrary(hGP);

end.
