unit sGradient;
{$I sDefs.inc}

interface

uses
  {$IFDEF FPC} JwaWinGDI, {$ENDIF}
  {$IFNDEF DELPHI5} Types, {$ENDIF}
  Windows, Graphics, Classes, SysUtils, math,
  sConst;

const
  PM_VERTICAL    = 1;
  PM_DIAGONAL    = 2;
  PM_OPACITY     = 4; // Opacity point in editor
  PM_ALL         = 8;
  PM_TRANSPARENT = $10;
  PM_OVERLAY     = $20;

  MASK_DIRECTION = $3;
  MASK_VALUES    = 12;

type
  TGradColors = array [0..3] of TsColor;
  TGradPoints = array [0..3] of TPoint;

function MakeShadow(Color: TColor; Radius, Offset, BodyHeight, BodyWidth, BlurSize: integer; MaskMode: boolean = False): TBitmap;
//procedure MakeRadialGrad(Bmp: TBItmap; aRect: TRect; Color1, Color2: TColor);
procedure PaintGrad   (Bmp: TBitMap; aRect: TRect; const Gradient: string); overload;
procedure PaintGrad   (Bmp: TBitMap; aRect: TRect; const Data: TsGradArray; OffsetX: integer = 0; OffsetY: integer = 0); overload;
procedure PaintGradTxt(Bmp: TBitMap; aRect: TRect; const Data: TsGradArray; TextureBmp: TBitmap; TextureRect: TRect; TextureAlpha: byte; AlphaChannel: byte = MaxByte);
procedure PrepareGradArray(const GradientStr: string; var GradArray: TsGradArray);

implementation

uses
  acntUtils, sAlphaGraph, sGraphUtils, acgpUtils;


type
  TRIVERTEX = packed record
    X, Y: DWORD;
    Red, Green, Blue, Alpha: Word;
  end;


var
  GradientFillAC: function(DC: hDC; pVertex: Pointer; dwNumVertex: DWORD; pMesh: Pointer; dwNumMesh, dwMode: DWORD): DWORD; stdcall;


function MakeShadow(Color: TColor; Radius, Offset, BodyHeight, BodyWidth, BlurSize: integer; MaskMode: boolean = False): TBitmap;
var
  l, r: real;
  aRect: TRect;
  C1, C2: TsColor;
  D: PRGBAArray_D;
  S0, S: PRGBAArray_S;
  BodyRadius, Delta: integer;
  X, Y, dx, dy, PosColor1, w, Blend1, Blend2, xmod, ymod, yCenter, xCenter: integer;

  procedure ExpandBodyWidth;
  var
    X: integer;
  begin
    BitBlt(Result.Canvas.Handle, Result.Width - Radius, 0, Radius, Result.Height, Result.Canvas.Handle, xCenter + 1, 0, SRCCOPY);
    for X := 0 to BodyWidth - 1 do
      BitBlt(Result.Canvas.Handle, xCenter + X + 1, 0, 1, Result.Height, Result.Canvas.Handle, xCenter, 0, SRCCOPY);
  end;

begin
  w := 2 * Radius;
  Result := CreateBmp32(w + BodyWidth + 1, w + BodyHeight + Offset + 1);
  if MaskMode then
    FillRect32(Result, MkRect(Result), clWhite, MaxByte)
  else
    FillRect32(Result, MkRect(Result), 0, 0);

  if InitLine(Result, Pointer(S0), Delta) then begin
    aRect.Top := Offset;
    aRect.Left := 0;
    aRect.Right := aRect.Left + w + 1;
    aRect.Bottom := aRect.Top + w + 1;

    yCenter := aRect.Top + w div 2;
    xCenter := aRect.Left + WidthOf(aRect) div 2;
    xmod := (WidthOf (aRect) + 1) mod 2;
    ymod := (HeightOf(aRect) + 1) mod 2;

    C2.C := Color;
    PosColor1 := aRect.Left - 1;
    w := xCenter;

    if BlurSize > 0 then
      BodyRadius := xCenter - BlurSize
    else begin
      BodyRadius := 0;
      BlurSize := w;
    end;

    if MaskMode then begin
      C1.C := TColor($FFFFFFFF);
      for Y := aRect.Top to yCenter do begin
        D := Pointer(PAnsiChar(S0) + Delta * Y);
        for X := 0 to xCenter do
          with D[X] do begin
            dx := xCenter - X;
            dy := yCenter - Y;
            l := sqrt(dx * dx + dy * dy);

            if l <= w then begin
              if l{ - 1} > BodyRadius then begin
                r := 1 - (l - BodyRadius) / (BlurSize + 1);
                Blend1 := Round(r * MaxByte * (Radius + 1 - l) / (BlurSize + 1))
              end
              else
                Blend1 := MaxByte;

              Blend2 := MaxByte - Blend1;
              DA := MaxByte;
              DR := (C1.A * Blend2 + C2.A * Blend1) shr 8;
              DG := DR;
              DB := DR;
            end
            else
              DC := TColor($FFFFFFFF);

            D[xCenter + dx - xmod].DC := DC;
          end;

        S := Pointer(PAnsiChar(S0) + Delta * (yCenter - ymod + yCenter - Y + BodyHeight));
        for X := 0 to xCenter do
          with D[X], S[X] do begin
            SC := DC;
            S[xCenter + xCenter - X - xmod].SC := DC;
          end;
      end;
    end
    else begin
      C1.C := Color and $FFFFFF;
      for Y := aRect.Top to yCenter do begin
        D := Pointer(PAnsiChar(S0) + Delta * Y);
        for X := 0 to xCenter do
          with D[X] do begin
            dx := xCenter - X;
            dy := yCenter - Y;
            l := sqrt(dx * dx + dy * dy);
            if l <= w then begin
              if l > BodyRadius then
                Blend2 := Round(MaxByte * ((w - l - PosColor1) * (1 - (l - BodyRadius) / (w - BodyRadius)) / (w - BodyRadius)))
              else
                Blend2 := MaxByte;

              Blend1 := MaxByte - Blend2;
              DA := (C1.A * Blend1 + C2.A * Blend2) shr 8;
              DR := (C1.R * Blend1 + C2.R * Blend2) * DA shr 16;
              DG := (C1.G * Blend1 + C2.G * Blend2) * DA shr 16;
              DB := (C1.B * Blend1 + C2.B * Blend2) * DA shr 16;
            end
            else
              DC := 0;

            D[xCenter + dx - xmod].DC := DC;
          end;

        S := Pointer(PAnsiChar(S0) + Delta * (yCenter - ymod + yCenter - Y + BodyHeight));
        for X := 0 to xCenter do
          with D[X], S[X] do begin
            SC := DC;
            S[xCenter + xCenter - X - xmod].SC := DC;
          end;
      end;
    end;

    S := Pointer(PAnsiChar(S0) + Delta * (aRect.Top + Radius));
    for Y := 0 to BodyHeight do begin
      D := Pointer(PAnsiChar(S0) + Delta * (aRect.Top + Radius + Y));
      CopyMemory(@D[0], @S[0], Result.Width * 4);
    end;

    ExpandBodyWidth;
  end;
end;

{
procedure MakeRadialGrad(Bmp: TBItmap; aRect: TRect; Color1, Color2: TColor);
var
  l: real;
  R: TRect;
  S0, D2: PRGBAArray_S;
  D1: PRGBAArray_D;
  DeltaS: integer;
  C1, C2: TsColor;
  X, Y, dx, dy, PosColor1, PosColor2, w, Blend1, Blend2, xmod, ymod: integer;
begin
  R.Left := aRect.Left;
  R.Bottom := aRect.Top + HeightOf(aRect) div 2;
  R.Top := R.Bottom - 1;
  R.Right := R.Left + WidthOf(aRect) div 2;
  xmod := (WidthOf (aRect) + 1) mod 2;
  ymod := (HeightOf(aRect) + 1) mod 2;
  if InitLine(Bmp, Pointer(S0), DeltaS) then begin
    C1.C := Color1;
    C2.C := Color2;
    PosColor1 := R.Left;
    PosColor2 := R.Right;
    w := WidthOf(R);
    for Y := aRect.Top to R.Bottom do begin
      D1 := Pointer(PAnsiChar(S0) + DeltaS * Y);
      for X := R.Left to R.Right do
        with D1[X] do begin
          dx := (R.Right - X);
          dy := (R.Bottom - Y);
          l := sqrt(dx * dx + dy * dy);
          if l <= w then begin
            Blend2 := Round(MaxByte * ((PosColor2 - l) - PosColor1) / w);
            Blend1 := MaxByte - Blend2;
            DR := (C1.R * Blend1 + C2.R * Blend2) shr 8;
            DG := (C1.G * Blend1 + C2.G * Blend2) shr 8;
            DB := (C1.B * Blend1 + C2.B * Blend2) shr 8;
            DA := (C1.A * Blend1 + C2.A * Blend2) shr 8;
          end
          else
            DC := 0;

          D1[R.Right + dx - xmod].DC := DC;
        end;

      D2 := Pointer(PAnsiChar(S0) + DeltaS * (R.Bottom - ymod + R.Bottom - Y));
      for X := R.Left to R.Right do
        with D1[X], D2[X] do begin
          SC := DC;
          D2[R.Right + R.Right - X - xmod].SC := DC;
      end;
    end;
  end;
end;
}

procedure PaintGrad(Bmp: TBitMap; aRect: TRect; const Data: TsGradArray; OffsetX: integer = 0; OffsetY: integer = 0); overload;
var
  SavedDC: hdc;
  R, G, B: single;
  CurrentColor: TsColor_;
  C, Color1, Color2: TsColor;
  S0, SSrc1, SSrc: PRGBAArray_;
  RStep, GStep, BStep, p: real;
  vert:  array [0..4] of TRIVERTEX;
  gRect: array [0..3] of GRADIENT_TRIANGLE;
  i, dX, dY, w, Y, X, DeltaLine: integer;
  bHeight, bWidth, Count, Percent, CurrentX, MaxX, CurrentY, MaxY: integer;

  Points: TGradPoints;
  GradColors: TGradColors;

  procedure InitVert(var Vert: TRIVERTEX; C: TsColor; p: TPoint);
  begin
    with Vert do begin
      Alpha := $FF00;
      x     := p.X;
      y     := p.Y;
      Red   := c.R shl 8;
      Green := c.G shl 8;
      Blue  := c.B shl 8;
    end;
  end;

  procedure InitRect(var R: GRADIENT_TRIANGLE; I0, I1, I2: integer);
  begin
    R.Vertex1 := I0;
    R.Vertex2 := I1;
    R.Vertex3 := I2;
  end;

begin
  if not IsRectEmpty(aRect) then begin
    bHeight := Bmp.Height;
    bWidth := Bmp.Width;

    if aRect.Right > bWidth then
      aRect.Right := bWidth;

    if aRect.Bottom > bHeight then
      aRect.Bottom := bHeight;

    if aRect.Left < 0 then
      aRect.Left := 0;

    if aRect.Top < 0 then
      aRect.Top := 0;

    CurrentColor.A := MaxByte;
    Count := Length(Data) - 1;
    if Count >= 0 then
      case Data[0].Mode and MASK_DIRECTION of
        0: begin
          C.A := MaxByte;
          MaxY := aRect.Top + OffsetY;
          w := min(WidthOf(aRect) + aRect.Left, bWidth) - 1;
          dX := w - aRect.Left + 1;
          if InitLine(Bmp, Pointer(S0), DeltaLine) then
            for i := 1 to Count do begin

              Color1.C := Data[i - 1].Color.C;
              Color2.C := Data[i].Color.C;
              Percent  := Data[i].Percent - Data[i - 1].Percent;
              CurrentY := MaxY;
              MaxY := CurrentY + ((HeightOf(aRect) + OffsetY) * Percent) div 100;
              if i = Count then
                MaxY := min(aRect.Bottom, bHeight)// - 1)
              else
                MaxY := min(MaxY, min(aRect.Bottom, bHeight));// - 1));

              if MaxY - CurrentY > 0 then begin
                R := Color1.R;
                G := Color1.G;
                B := Color1.B;
                if (i = Count) or (MaxY >= bHeight) then
                  MaxY := min(aRect.Bottom, bHeight);// - 1);

                dY := MaxY - CurrentY;
                if dY > 0 then
                  if (Data[i].Mode and PM_TRANSPARENT <> 0) or (Data[i - 1].Mode and PM_TRANSPARENT <> 0) then begin
                    SavedDC := SaveDC(Bmp.Canvas.Handle);
                    try
                      ExcludeClipRect(Bmp.Canvas.Handle, aRect.Left, CurrentY - 1, aRect.Left + WidthOf(aRect), CurrentY);
                      acgpGradientRectangleV(Bmp.Canvas.Handle, aRect.Left, CurrentY - 1, dX, MaxY - CurrentY + 1, Color1.C, Color2.C)
                    finally
                      RestoreDC(Bmp.Canvas.Handle, SavedDC);
                    end;
                  end
                  else
                    if Color1.C = Color2.C then begin
                      CurrentColor.R := Color1.R;
                      CurrentColor.G := Color1.G;
                      CurrentColor.B := Color1.B;
                      for Y := CurrentY to MaxY - 1 do begin
                        SSrc := Pointer(PAnsiChar(S0) + DeltaLine * Y);
{$IFDEF WIN64}
                        for X := aRect.Left to aRect.Left + dX - 1 do
                          SSrc[X] := CurrentColor;
{$ELSE}
                        FillLongword(SSrc[aRect.Left], dX, Cardinal(CurrentColor.C));
{$ENDIF}
                      end
                    end
                    else begin
                      RStep := (Color2.R - Color1.R) / dY;
                      GStep := (Color2.G - Color1.G) / dY;
                      BStep := (Color2.B - Color1.B) / dY;
                      for Y := CurrentY to MaxY - 1 do begin
                        CurrentColor.R := Round(R);
                        CurrentColor.G := Round(G);
                        CurrentColor.B := Round(B);
                        SSrc := Pointer(PAnsiChar(S0) + DeltaLine * Y);
{$IFDEF WIN64}
                        for X := aRect.Left to aRect.Left + dX - 1 do
                          SSrc[X] := CurrentColor;
{$ELSE}
                        FillLongword(SSrc[aRect.Left], dX, Cardinal(CurrentColor.C));
{$ENDIF}
                        R := R + RStep;
                        G := G + GStep;
                        B := B + BStep;
                      end
                    end;
              end;
            end;
        end;

        1:
          if InitLine(Bmp, Pointer(S0), DeltaLine) then begin
            p := WidthOf(aRect) / 100;
            // Paint first line
            SSrc1 := Pointer(PAnsiChar(S0) + DeltaLine * aRect.Top);
            MaxX := aRect.Left;
            for i := 1 to Count do begin
              Color1.C := Data[i - 1].Color.C;
              Color2.C := Data[i].Color.C;
              Percent  := Data[i].Percent - Data[i - 1].Percent;
              CurrentX := MaxX;
              MaxX := Round(CurrentX + (p * Percent));

              if (Data[i].Mode and PM_TRANSPARENT <> 0) or (Data[i - 1].Mode and PM_TRANSPARENT <> 0) then begin
                if i = Count then
                  MaxX := min(aRect.Right, bWidth)
                else
                  MaxX := min(MaxX, min(aRect.Right, bWidth));

                SavedDC := SaveDC(Bmp.Canvas.Handle);
                try
                  ExcludeClipRect(Bmp.Canvas.Handle, CurrentX - 1, aRect.Top, CurrentX, aRect.Top + HeightOf(aRect));
                  acgpGradientRectangleH(Bmp.Canvas.Handle, CurrentX - 1, aRect.Top, MaxX - CurrentX + 1, HeightOf(aRect), Color1.C, Color2.C);
                finally
                  RestoreDC(Bmp.Canvas.Handle, SavedDC);
                end;
              end
              else begin
                if i = Count then
                  MaxX := min(aRect.Right, bWidth - 1)
                else
                  MaxX := min(MaxX, min(aRect.Right, bWidth - 1));

                if MaxX - CurrentX > 0 then begin
                  dX := MaxX - CurrentX;
                  R := Color1.R;
                  G := Color1.G;
                  B := Color1.B;
                  RStep := (Color2.R - Color1.R) / dX;
                  GStep := (Color2.G - Color1.G) / dX;
                  BStep := (Color2.B - Color1.B) / dX;
                  for X := CurrentX to MaxX do begin
                    CurrentColor.R := Round(R);
                    CurrentColor.G := Round(G);
                    CurrentColor.B := Round(B);
                    SSrc1[X].I := CurrentColor.I;
                    R := R + RStep;
                    G := G + GStep;
                    B := B + BStep;
                  end;
                end;
                // Clone lines
                if WidthOf(aRect) > 0 then
                  for CurrentY := aRect.Top + 1 to aRect.Bottom - 1 do begin
                    SSrc := Pointer(PAnsiChar(S0) + DeltaLine * CurrentY);
                    CopyMemory(@SSrc[CurrentX], @SSrc1[CurrentX], (MaxX - CurrentX + 1) * 4);
                  end;
              end;
            end
          end;

        2: // Triangles
          if Length(Data) > 4 then
            if Data[0].Mode and PM_OVERLAY <> 0 then begin
              dX := WidthOf(aRect);
              dY := HeightOf(aRect);
              GradColors[0].C := Data[0].Color.C;
              GradColors[1].C := Data[2].Color.C;
              GradColors[2].C := Data[3].Color.C;
              GradColors[3].C := Data[4].Color.C;
              Points[0].X := aRect.Left;
              Points[0].Y := aRect.Top;

              Points[1].X := aRect.Left + dX;
              Points[1].Y := aRect.Top;

              Points[2].X := aRect.Left + dX;
              Points[2].Y := aRect.Top + dY;

              Points[3].X := aRect.Left;
              Points[3].Y := aRect.Top + dY;

              acgpGradientRectangle(Bmp.Canvas.Handle, aRect.Left, aRect.Top, dX, dY, Points, GradColors, TsColor(Data[1].Color.C));
            end
            else begin
              InitVert(vert[0], Data[0].Color, aRect.TopLeft);                    // Left-top
              InitVert(vert[1], Data[1].Color, Point(aRect.Left + WidthOf(aRect) div 2, aRect.Top + HeightOf(aRect) div 2)); // Center
              InitVert(vert[2], Data[2].Color, Point(aRect.Right, aRect.Top));    // Right-top
              InitVert(vert[3], Data[3].Color, Point(aRect.Right, aRect.Bottom)); // Right-bottom
              InitVert(vert[4], Data[4].Color, Point(aRect.Left, aRect.Bottom));  // Left-bottom

              InitRect(gRect[0], 0, 1, 2); // Top
              InitRect(gRect[1], 1, 2, 3); // Right
              InitRect(gRect[2], 0, 1, 4); // Left
              InitRect(gRect[3], 4, 1, 3); // Bottom

              if Assigned(GradientFillAC) then
                GradientFillAC(Bmp.Canvas.Handle, @vert, 5, @gRect, 4, GRADIENT_FILL_TRIANGLE);
            end;
      end;
  end;
end;


procedure PaintGrad(Bmp: TBitMap; aRect: TRect; const Gradient: string); overload;
var
  ga: TsGradArray;
begin
  PrepareGradArray(Gradient, ga);
  PaintGrad(Bmp, aRect, ga);
end;


procedure PaintGradTxt(Bmp: TBitMap; aRect: TRect; const Data: TsGradArray; TextureBmp: TBitmap; TextureRect: TRect; TextureAlpha: byte; AlphaChannel: byte = MaxByte);
var
  SSrc: PRGBAArray_D;
  STxt: PRGBAArray_S;
  snR, snG, snB: single;
  Color1, Color2: TsColor;
  CurrentColor, C_: TsColor_;
  RStep, GStep, BStep, p: real;
  S0, SSrc1, STxt1: PRGBAArray_;
  YPos, XPos, DeltaS, DeltaT: integer;
  i, w, h, dX, dY, TxtW, TxtH: Integer;
  bWidth, bHeight, Count, Percent, CurrentX, MaxX, CurrentY, MaxY: integer;
begin
  if not IsRectEmpty(aRect) then begin
    bWidth := Bmp.Width;
    bHeight := Bmp.Height;
    if aRect.Right > bWidth then
      aRect.Right := bWidth;

    if aRect.Bottom > bHeight then
      aRect.Bottom := bHeight;

    if aRect.Left < 0 then
      aRect.Left := 0;

    if aRect.Top < 0 then
      aRect.Top := 0;

    CurrentColor.A := AlphaChannel;
    C_.A := AlphaChannel;
    if IsRectEmpty(TextureRect) then
      TextureRect := MkRect(TextureBmp); // Compatibility with old skins

    TxtW := WidthOf (TextureRect, True);
    TxtH := HeightOf(TextureRect, True);
    if TextureRect.Top + TxtH >= TextureBmp.Height then
      TxtH := TextureBmp.Height - 1 - TextureRect.Top;

    if TextureRect.Left + TxtW >= TextureBmp.Width then
      TxtW := TextureBmp.Width - 1 - TextureRect.Left;

    if (TxtH < 0) or (TxtW < 0) then
      Exit;

    Count := Length(Data) - 1;
    if Count >= 0 then
      case Data[0].Mode and MASK_DIRECTION of
        0: begin
          MaxY := aRect.Top;
          for i := 1 to Count do begin
            Color1.C := Data[i - 1].Color.C;
            Color2.C := Data[i].Color.C;
            Percent  := Data[i].Percent - Data[i - 1].Percent;
            CurrentY := MaxY;
            MaxY := CurrentY + (HeightOf(aRect) * Percent) div 100;
            if i = Count then
              MaxY := min(aRect.Bottom, bHeight - 1)
            else
              MaxY := min(MaxY, min(aRect.Bottom, bHeight - 1));

            if MaxY - CurrentY > 0 then begin
              snR := Color1.R;
              snG := Color1.G;
              snB := Color1.B;
              if (i = Count) or (MaxY >= bHeight) then
                MaxY := min(aRect.Bottom, bHeight) - 1;

              dY := MaxY - CurrentY;
              if dY > 0 then begin
                RStep := (Color2.R - Color1.R) / dY;
                GStep := (Color2.G - Color1.G) / dY;
                BStep := (Color2.B - Color1.B) / dY;
                if InitLine(Bmp, Pointer(S0), DeltaS) and InitLine(TextureBmp, Pointer(STxt1), DeltaT) then
                  for YPos := CurrentY to MaxY do
                    with CurrentColor do begin
                      R := Round(snR);
                      G := Round(snG);
                      B := Round(snB);
                      SSrc := Pointer(PAnsiChar(S0) + DeltaS * YPos);
                      STxt := Pointer(PAnsiChar(STxt1) + DeltaT * (TextureRect.Top + YPos mod TxtH));
                      for XPos := aRect.Left to aRect.Right - 1 do
                        with SSrc[XPos], STxt[TextureRect.Left + XPos mod TxtW] do begin
                          DR := ((SR - R) * TextureAlpha + R shl 8) shr 8;
                          DG := ((SG - G) * TextureAlpha + G shl 8) shr 8;
                          DB := ((SB - B) * TextureAlpha + B shl 8) shr 8;
                          DA := AlphaChannel;
                        end;

                      snR := snR + RStep;
                      snG := snG + GStep;
                      snB := snB + BStep;
                    end;
              end;
            end;
          end;
        end;

        1: begin
          if InitLine(Bmp, Pointer(S0), DeltaS) and InitLine(TextureBmp, Pointer(STxt1), DeltaT) then begin
            p := WidthOf(aRect) / 100;
            SSrc1 := Pointer(PAnsiChar(S0) + DeltaS * aRect.Top);
            // Paint first line
            MaxX := aRect.Left;
            for i := 1 to Count do begin
              Color1.C := Data[i - 1].Color.C;
              Color2.C := Data[i].Color.C;
              Percent  := Data[i].Percent - Data[i - 1].Percent;
              CurrentX := MaxX;
              MaxX := Round(CurrentX + (p * Percent));
              if i = Count then
                MaxX := min(aRect.Right, bWidth - 1)
              else
                MaxX := min(MaxX, min(aRect.Right, bWidth - 1));

              if MaxX - CurrentX > 0 then begin
                dX := MaxX - CurrentX;
                snR := Color1.R;
                snG := Color1.G;
                snB := Color1.B;
                RStep := (Color2.R - Color1.R) / dX;
                GStep := (Color2.G - Color1.G) / dX;
                BStep := (Color2.B - Color1.B) / dX;
                for XPos := CurrentX to MaxX do
                  with SSrc1[XPos] do begin
                    R := Round(snR);
                    G := Round(snG);
                    B := Round(snB);
                    snR := snR + RStep;
                    snG := snG + GStep;
                    snB := snB + BStep;
                  end;
              end;
            end;
            h := min(TxtH, HeightOf(aRect, True)) - 1;
            // Clone lines with using a texture
            for CurrentY := aRect.Top + 1 to h + aRect.Top do begin
              SSrc := Pointer(PAnsiChar(S0) + DeltaS * CurrentY);
              STxt := Pointer(PAnsiChar(STxt1) + DeltaT * (TextureRect.Top + CurrentY mod TxtH));
              for XPos := aRect.Left to aRect.Right - 1 do
                with SSrc[XPos], STxt[TextureRect.Left + XPos mod TxtW], SSrc1[XPos] do begin
                  DR := ((SR - R) * TextureAlpha + R shl 8) shr 8;
                  DG := ((SG - G) * TextureAlpha + G shl 8) shr 8;
                  DB := ((SB - B) * TextureAlpha + B shl 8) shr 8;
                  DA := AlphaChannel;
                end;
            end;
            // Texture for the first line
            CurrentY := aRect.Top;
            STxt := Pointer(PAnsiChar(STxt1) + DeltaT * (TextureRect.Top + CurrentY mod TxtH));
            for XPos := aRect.Left to aRect.Right - 1 do
              with STxt[TextureRect.Left + XPos mod TxtW], SSrc1[XPos] do begin
                R := ((SR - R) * TextureAlpha + R shl 8) shr 8;
                G := ((SG - G) * TextureAlpha + G shl 8) shr 8;
                B := ((SB - B) * TextureAlpha + B shl 8) shr 8;
                A := AlphaChannel;
              end;

            CurrentY := aRect.Top + h;
            w := WidthOf(aRect);
            if w > 0 then
              while CurrentY < aRect.Bottom - 1 - h do begin
                BitBlt(Bmp.Canvas.Handle, aRect.Left, CurrentY, w, h, Bmp.Canvas.Handle, aRect.Left, aRect.Top, SRCCOPY);
                inc(CurrentY, h);
              end;

            if CurrentY < aRect.Bottom then
              BitBlt(Bmp.Canvas.Handle, aRect.Left, CurrentY, w, aRect.Bottom - CurrentY, Bmp.Canvas.Handle, aRect.Left, aRect.Top, SRCCOPY);
          end;
        end;
      end;
  end;
end;

const
  GradDivider = [CharSemicolon];


procedure PrepareGradArray(const GradientStr: string; var GradArray: TsGradArray);
var
  s: string;
  c: AnsiChar;
  CurPercent, Count, i: integer;
begin
  SetLength(GradArray, 0);
  if Length(GradientStr) > 4 then begin
    C := AnsiChar(GradientStr[Length(GradientStr)]);
    if C <> ZeroChar then begin // New mode
      Count := Ord(C) - $40;
      SetLength(GradArray, Count);
      for i := 0 to Count - 1 do begin
        s := ExtractWord(i * 3 + 1, GradientStr, GradDivider);
        GradArray[i].Color.C := StrToInt64(s);
        GradArray[i].Percent := max(0, min(100, StrToInt(ExtractWord(i * 3 + 2, GradientStr, GradDivider))));
        GradArray[i].Mode := StrToInt64(ExtractWord(i * 3 + 3, GradientStr, GradDivider));
        GradArray[i].Index := i;
        if GradArray[i].Mode and PM_TRANSPARENT <> 0 then
          GradArray[0].Mode := GradArray[0].Mode or PM_OVERLAY;
      end;
    end
    else begin
      Count := WordCount(GradientStr, GradDivider) div 5;
      CurPercent := 0;
      SetLength(GradArray, Count);
      for i := 0 to Count - 1 do begin
        GradArray[i].Color.I := acNativeUInt(StrToInt64(ExtractWord(i * 5 + 1, GradientStr, GradDivider))) and $FFFFFFFF;
        GradArray[i].Percent := min(100, CurPercent);
        GradArray[i].Mode := StrToInt64(ExtractWord(i * 5 + 4, GradientStr, GradDivider));
        GradArray[i].Index := i;
        inc(CurPercent, max(0, min(100, StrToInt(ExtractWord(i * 5 + 3, GradientStr, GradDivider)))));
        GradArray[i].Color.A := MaxByte;
        GradArray[i].Mode := GradArray[i].Mode and not PM_TRANSPARENT;
      end;
      if (Count > 0) and (GradArray[0].Mode and MASK_DIRECTION = PM_DIAGONAL) then
        for i := 0 to 4 do
          if Length(GradArray) < i + 1 then begin
            SetLength(GradArray, i + 1);
            GradArray[i] := GradArray[i - 1];
          end;
    end;
    GradArray[Length(GradArray) - 1].Percent := 100;
  end;
end;


var
  hmsimg32: HMODULE = 0;


initialization
  hmsimg32 := LoadLibrary('msimg32.dll');
  if hmsimg32 <> 0 then
    GradientFillAC := GetProcAddress(hmsimg32, 'GradientFill');


finalization
  if hmsimg32 <> 0 then
    FreeLibrary(hmsimg32);
end.


