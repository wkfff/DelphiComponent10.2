unit sColorDialog;
{$I sDefs.inc}

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Buttons, Mask, StdCtrls, ExtCtrls,
  {$IFDEF DELPHI_XE2} UITypes, {$ENDIF}
  {$IFNDEF DELPHI5} Types, {$ENDIF}
  sSkinProvider, sBitBtn, sPanel, sMaskEdit, sCurrencyEdit, sLabel, sConst, sSpeedButton, sGraphUtils,
  sCurrEdit, sCustomComboEdit, sEdit, acPopupCtrls, sTrackBar;


type
  TsColorDialogForm = class(TForm)
    sBitBtn1: TsBitBtn;
    sBitBtn2: TsBitBtn;
    sBitBtn3: TsBitBtn;
    sBitBtn4: TsBitBtn;
    sBitBtn5: TsBitBtn;

    sLabel2: TsLabel;
    sLabel4: TsLabel;
    sLabel5: TsLabel;
    sLabel6: TsLabel;

    Panel1: TPanel;
    GradPanel:  TsPanel;
    ColorPanel: TsPanel;
    sREdit: TsTrackEdit;
    sGEdit: TsTrackEdit;
    sBEdit: TsTrackEdit;
    sHEdit: TsTrackEdit;
    sSEdit: TsTrackEdit;
    sVEdit: TsTrackEdit;

    AddPal:  TsColorsPanel;
    MainPal: TsColorsPanel;
    OldPanel: TPaintBox;
    SelectedPanel: TPaintBox;

    sEditHex: TsMaskEdit;
    sEditDecimal: TsCurrencyEdit;
    sSpeedButton1: TsSpeedButton;
    sSkinProvider1: TsSkinProvider;
    sDragBar1: TsDragBar;
    sAEdit: TsTrackEdit;

    procedure CreateExtBmp;
    procedure UpdateGradient;
    procedure FormShow(Sender: TObject);
    procedure sBitBtn2Click(Sender: TObject);
    procedure sBitBtn1Click(Sender: TObject);

    procedure ColorPanelPaint(Sender: TObject; Canvas: TCanvas);
    procedure ColorPanelMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ColorPanelMouseUp  (Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ColorPanelMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);

    procedure GradPanelPaint(Sender: TObject; Canvas: TCanvas);
    procedure GradPanelMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure GradPanelMouseUp  (Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure GradPanelMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);

    procedure MainPalChange(Sender: TObject);
    procedure sEditHexKeyPress(Sender: TObject; var Key: Char);
    function GetColorCoord(const aColor: TsColor): integer;
    procedure FormPaint(Sender: TObject);
    procedure sEditHexChange(Sender: TObject);
    procedure sEditDecimalChange(Sender: TObject);
    procedure sHEditChange (Sender: TObject);
    procedure sREditChange (Sender: TObject);
    procedure FormCreate   (Sender: TObject);
    procedure sBitBtn4Click(Sender: TObject);
    procedure sBitBtn5Click(Sender: TObject);
    procedure sBitBtn3Click(Sender: TObject);
    procedure sSpeedButton1Click(Sender: TObject);
    procedure PickFormPaint(Sender: TObject);
    procedure PickFormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure PickFormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure PickFormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);

    procedure FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseUp  (Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure MainPalDblClick(Sender: TObject);
    procedure AddPalDblClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure SetSelectedColor(C: TColor);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure SelectedPanelPaint(Sender: TObject);
    procedure PaintColorRect(DC: hdc; Ctrl: TPaintBox);
  protected
    OldXY: TPoint;
    OldY: integer;
    procedure SetDecimalEdit(C: TColor);
    procedure SetHEXEdit(C: TColor);
  public
    ExtBmp,
    BGBmp,
    GradBmp: TBitmap;
    BGUpdating,
    UseAlpha: boolean;

    sColor: TsColor;
    PickPanel: TsPanel;
    Owner: TColorDialog;
    destructor Destroy; override;
    procedure SetMarker;
    function UpdateColorAlpha(Value: TColor): TColor;
    procedure InitControls(FullOpen, ShowHelp: boolean; aColor: TColor; aBorderStyle: TFormBorderStyle);
    procedure PaintCursor(mX, mY: integer; Canvas: TCanvas);
    function GradToColor(Coord: integer): TColor;
    procedure SetCurrentColor(c: TColor; CheckAlpha: boolean);
    procedure SetInternalColor(h: integer; s: real);
    procedure SetColorFromEdit(Color: TColor; var Flag: boolean);
    procedure ExitPanels;
    procedure InitLngCaptions;
    function MarkerRect: TRect;
  end;


var
  sColorDialogForm: TsColorDialogForm = nil;
  acCustomColors: TStringList = nil;
  ColDlg: TColorDialog = nil;


implementation

uses Messages, math,
  sCommonData, acntUtils, sDialogs, sAlphaGraph, sVclUtils, sSkinProps, acPopupController, acgpUtils, sComboBoxes, sGlyphUtils;

{$R *.DFM}
{$R ASCursors.res}

var
  Bmp: TBitmap;
  SelectedHsv: TsHSV;
  ColorCoord: TPoint;
  GradY, CurrCustomIndex: integer;
  InternalColor, PickColor: TColor;
  UseCoords, ExPressed, GradPressed: boolean;
  ColorChanging, HexChanging, DecChanging, HsvChanging, RgbChanging: boolean;


type
  TAccessPanel = class(TPanel);


const
  iMinWidth = 272;
  iMaxWidth = 590;
  crASPipette = -50;
  ArrowSize = 7;//5;
  iNear = 4;


procedure Marker(DC: hdc; Pos: TPoint);
var
  aSize, x, y: integer;
  C: TColor;
begin
  if (sColorDialogForm <> nil) and sColorDialogForm.sSkinProvider1.SkinData.Skinned then
    C := sColorDialogForm.sSkinProvider1.SkinData.SkinManager.GetGlobalFontColor
  else
    C := clBlack;

  if sColorDialogForm.sSkinProvider1.SkinData.SkinManager <> nil then
    aSize := sColorDialogForm.sSkinProvider1.SkinData.SkinManager.ScaleInt(ArrowSize)
  else
    aSize := ArrowSize;

  for y := 0 to aSize do
    for x := 0 to aSize do begin
      if x > y then
        SetPixel(DC, Pos.X + X, Pos.Y + Y, C);

      if x > ArrowSize - y then
        SetPixel(DC, Pos.X + X, Pos.Y + Y - aSize, C);
    end;
end;


procedure SkinMarker(Form: TsColorDialogForm);
var
  CI: TCacheInfo;
  aSize, x, y: integer;
  Bmp: TBitmap;
  DC: hdc;
begin
  DC := Form.Canvas.Handle;
  with Form do
    if not BGUpdating then
      if not sSkinProvider1.SkinData.BGChanged then begin
        if sSkinProvider1.SkinData.SkinManager <> nil then
          aSize := sSkinProvider1.SkinData.SkinManager.ScaleInt(ArrowSize)
        else
          aSize := ArrowSize;

        CI := MakeCacheInfo(sSkinProvider1.SkinData.FCacheBmp, sSkinProvider1.OffsetX, sSkinProvider1.OffsetY);
        x := Form.GradPanel.Left + Form.GradPanel.Width;
        y := Form.GradPanel.Top - aSize;
        Bmp := TBitmap.Create;
        if BGBmp = nil then begin // Save BG
          BGUpdating := True;
          sSkinProvider1.SkinData.Invalidate;
          BGBmp := CreateBmp32(10, GradPanel.Height + aSize * 2);
          if CI.Ready then
            BitBlt(BGBmp.Canvas.Handle, 0, 0, BGBmp.Width, BGBmp.Height, CI.Bmp.Canvas.Handle, x, y, SRCCOPY)
          else
            FillDC(BGBmp.Canvas.Handle, MkRect(BGBmp), CI.FillColor);

          BGUpdating := False;
        end;
        Bmp.Assign(BGBmp);

        if not InAnimationProcess then begin
          if GradY < 0 then
            GradY := GetColorCoord(sColor);

          Marker(Bmp.Canvas.Handle, Point(0, GradY + aSize));
        end;

        BitBlt(DC, GradPanel.Left + GradPanel.Width, GradPanel.Top - aSize, 20, BGBmp.Height, Bmp.Canvas.Handle, 0, 0, SRCCOPY);
        Bmp.Free;
      end;
end;


procedure RepaintRect(Form: TsColorDialogForm);
var
  UR: TRect;
begin
  UR := Form.MarkerRect;
  RedrawWindow(Form.Handle, @UR, 0, RDW_INVALIDATE or RDW_ERASE or RDW_UPDATENOW);
end;


procedure TsColorDialogForm.sBitBtn2Click(Sender: TObject);
begin
  ModalResult := mrCancel;
  if BorderStyle = bsNone then
    Close;
end;


type
  TAccessCombobox = class(TsColorBox);


procedure TsColorDialogForm.sBitBtn1Click(Sender: TObject);
var
  Ndx: integer;
begin
  ModalResult := mrOk;
  if BorderStyle = bsNone then begin
    if acIntController <> nil then
      Ndx := acIntController.GetFormIndex(Self)
    else
      Ndx := -1;

    if (Ndx >= 0) and (acIntController.FormHandlers[Ndx].PopupCtrl is TsColorBox) then
      with TAccessCombobox(acIntController.FormHandlers[Ndx].PopupCtrl) do begin
        if UseAlpha then
          Selected := sColor.C
        else
          Selected := clWhite and sColor.C;

        Change;
      end;

    Close;
  end;
end;


procedure TsColorDialogForm.ColorPanelPaint(Sender: TObject; Canvas: TCanvas);
begin
  if ExtBmp <> nil then begin
    BitBlt(Canvas.Handle, 0, 0, ExtBmp.Width, ExtBmp.Height, ExtBmp.Canvas.Handle, 0, 0, SRCCOPY);
    if not ExPressed then
      if UseCoords then
        PaintCursor(ColorCoord.x, ColorCoord.y, Canvas)
      else
        PaintCursor(SelectedHsv.h * ColorPanel.Width div 360, Round((1 - SelectedHsv.s) * ColorPanel.Height), Canvas)
  end;
end;


procedure TsColorDialogForm.CreateExtBmp;
var
  S0, S: PRGBAArray_;
  x, y, ImgWidth, ImgHeight, DeltaS: integer;
begin
  ImgWidth := ColorPanel.Width;
  ImgHeight := ColorPanel.Height;
  if (ImgWidth > 0) and (ImgHeight > 0) then
    try
      ExtBmp := CreateBmp32(ImgWidth, ImgHeight);
    finally
      if ExtBmp <> nil then
        if InitLine(ExtBmp, Pointer(S0), DeltaS) then
          for y := 0 to ImgHeight - 1 do begin
            S := Pointer(PAnsiChar(S0) + DeltaS * Y);
            for x := 0 to ImgWidth - 1 do
              S[x].C := SwapRedBlue(Hsv2Rgb(x * 360 / ImgWidth, 1 - y / ImgHeight, 1 - y / (ImgHeight * 3)).C);
          end;
    end;
end;


procedure TsColorDialogForm.FormShow(Sender: TObject);
begin
  InternalColor := sColor.C;
  CreateExtBmp;
  UpdateGradient;
  if Owner is TsColorDialog then begin
    if TsColorDialog(Owner).CustomColors.Count = 0 then
      AddPal.Colors.Assign(acCustomColors);

    sDragBar1.Visible := False;
    SetCurrentColor(TsColorDialog(Owner).Color, False);
    TsColorDialog(Owner).DoShow;
  end
  else
    sDragBar1.Visible := True;

  if sBitBtn1.CanFocus then
    sBitBtn1.SetFocus;
end;


procedure TsColorDialogForm.ColorPanelMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  R: TRect;
  P: TPoint;
begin
  ColorPanel.SetFocus;
  ExPressed := True;
  SetInternalColor(Round(x * 360 / ColorPanel.Width), 1 - y / ColorPanel.Height);
  ColorPanel.SkinData.BGChanged := True;
  ColorPanelPaint(ColorPanel, TAccessPanel(ColorPanel).Canvas);
  P := ColorPanel.ClientToScreen(MkPoint);
  R := MkRect(ColorPanel);
  OffsetRect(R, P.x, P.y);
  ClipCursor(@R);
end;


procedure TsColorDialogForm.GradPanelPaint(Sender: TObject; Canvas: TCanvas);
begin
  BitBlt(Canvas.Handle, 0, 0, GradBmp.Width, GradBmp.Height, GradBmp.Canvas.Handle, 0, 0, SRCCOPY);
end;


procedure TsColorDialogForm.ColorPanelMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if ExPressed and ((OldXY.X <> X) or (OldXY.Y <> Y)) then begin
    OldXY := Point(X, Y);
    SetInternalColor(x * 360 div ColorPanel.Width, 1 - y / ColorPanel.Height);
  end;
end;


procedure TsColorDialogForm.GradPanelMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  GradPanel.SetFocus;
  GradPressed := True;
  GradY := Y;
  UseCoords := True;
  SetCurrentColor(GradToColor(y), False);
  UseCoords := False;
  FormPaint(Self);
end;


procedure TsColorDialogForm.SetMarker;
var
  CI: TCacheInfo;
  c: TsColor;
begin
  if IsWindowVisible(Handle) then begin
    GradPanel.SkinData.BGChanged := True;
    UpdateGradient;
    GradPanelPaint(GradPanel, TAccessPanel(GradPanel).Canvas);
    CI := sCommonData.GetParentCache(GradPanel.SkinData);
    if CI.Ready then
      BitBlt(Canvas.Handle, GradPanel.Left + GradPanel.Width, GradPanel.Top - 5, 20, GradPanel.Height + 10, CI.Bmp.Canvas.Handle, GradPanel.Left + GradPanel.Width + CI.X, GradPanel.Top + CI.Y - 5, SRCCOPY)
    else
      FillDC(Canvas.Handle, Rect(GradPanel.Left + GradPanel.Width, GradPanel.Top - 5, GradPanel.Left + GradPanel.Width + 20, GradPanel.Top + GradPanel.Height + 5), CI.FillColor);

    c.C := UpdateColorAlpha(TColor(SelectedPanel.Tag));
    GradY := GetColorCoord(c);
  end;
end;


procedure TsColorDialogForm.SetSelectedColor(C: TColor);
begin
  sColor.C := C;
  SelectedPanel.Tag := C;
  PaintColorRect(SelectedPanel.Canvas.Handle, SelectedPanel);
end;


procedure TsColorDialogForm.GradPanelMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ClipCursor(nil);
  GradPressed := False;
  RepaintRect(Self);
end;


procedure TsColorDialogForm.GradPanelMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if GradPressed then begin
    if OldY <> Y then begin
      GradY := min(GradPanel.Height, max(0, Y));
      RepaintRect(Self);
      UseCoords := True;
      SetCurrentColor(GradToColor(y), False);
      UseCoords := False;
    end;
    OldY := Y;
  end;
end;


procedure TsColorDialogForm.ColorPanelMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ClipCursor(nil);
  ExPressed := False;
  ColorPanel.SkinData.BGChanged := True;
  ColorPanelPaint(ColorPanel, TAccessPanel(ColorPanel).Canvas);
end;


function TsColorDialogForm.GradToColor(Coord: integer): TColor;
var
  CtrlMul, RStep, GStep, BStep, R, G, B: real;
  y, ImgHeight: integer;
  Gray_, Gray: integer;
  c: TsColor;
begin
  c.C := InternalColor;
  if UseAlpha then
    C.A := sAEdit.asInteger;

  ImgHeight := GradPanel.Height;
  CtrlMul := MaxByte / ImgHeight;
  Gray := Round((c.R + c.G + c.B) div 3 * CtrlMul);
  inc(Gray, (ImgHeight - MaxByte) div 2);
  Gray_ := ImgHeight - Gray; // Vertical coord of current color in panel

  R := MaxByte;
  G := MaxByte;
  B := MaxByte;

  if Gray_ = 0 then begin
    RStep := 0;
    GStep := 0;
    BStep := 0;
  end
  else begin
    RStep := (MaxByte - c.R) / Gray_;
    GStep := (MaxByte - c.G) / Gray_;
    BStep := (MaxByte - c.B) / Gray_;
  end;
  y := 0;
  while y < Gray_ do begin
    R := R - RStep;
    G := G - GStep;
    B := B - BStep;
    c.R := Round(R);
    c.G := Round(G);
    c.B := Round(B);
    if y = Coord then begin
      if c.R < iNear then c.R := 0;
      if c.G < iNear then c.G := 0;
      if c.B < iNear then c.B := 0;
      if c.R > MaxByte - iNear then c.R := MaxByte;
      if c.G > MaxByte - iNear then c.G := MaxByte;
      if c.B > MaxByte - iNear then c.B := MaxByte;
      Result := c.C;
      Exit;
    end;
    inc(y)
  end;
  if Gray = 0 then begin
    RStep := 0;
    GStep := 0;
    BStep := 0;
  end
  else begin
    RStep := c.R / Gray;
    GStep := c.G / Gray;
    BStep := c.B / Gray;
  end;
  while y < ImgHeight do begin
    R := R - RStep;
    G := G - GStep;
    B := B - BStep;
    c.R := Round(R);
    c.G := Round(G);
    c.B := Round(B);
    if y = Coord then begin
      if c.R < iNear then c.R := 0;
      if c.G < iNear then c.G := 0;
      if c.B < iNear then c.B := 0;
      if c.R > MaxByte - iNear then c.R := MaxByte;
      if c.G > MaxByte - iNear then c.G := MaxByte;
      if c.B > MaxByte - iNear then c.B := MaxByte;
      Result := c.C;
      Exit;
    end;
    inc(y)
  end;
  if Coord <= 0 then
    Result := clWhite
  else
    Result := c.C;
end;


procedure TsColorDialogForm.SetCurrentColor(c: TColor; CheckAlpha: boolean);
var
  TempValue: real;
begin
  if CheckAlpha and UseAlpha then
    c := TColor($FF000000 or Cardinal(c));

  ColorChanging := True;
  if not UseAlpha then begin
    TsColor(c).A := 0;
    SetSelectedColor(ColorToRGB(c));
  end
  else
    SetSelectedColor(c);

  ColorPanel.SkinData.BGChanged := True;
  GradPanel.SkinData.BGChanged := True;
  if not UseCoords then begin
    SelectedHsv := Rgb2Hsv(sColor);
    InternalColor := max(Hsv2Rgb(SelectedHsv.h, SelectedHsv.s, 1).C, 0);
  end;
  if not HsvChanging then begin
    sHEdit.Value := Round(SelectedHsv.H);
    if not UseCoords then begin
      sSEdit.Value := SelectedHsv.S * 100;
      sVEdit.Value := SelectedHsv.V * 100
    end
    else
      if GradY < GradPanel.Height div 2 then begin
        TempValue := SelectedHsv.S * 100;
        sSEdit.Value := (TempValue * 2) / GradPanel.Height * (GradY + 1);
      end
      else
        sVEdit.Value := (1 - abs(GradPanel.Height div 2 - GradY) / (GradPanel.Height div 2)) * 100;
  end;

  if not RgbChanging then begin
    sREdit.Value := sColor.R;
    sGEdit.Value := sColor.G;
    sBEdit.Value := sColor.B;
    sAEdit.Value := sColor.A;
  end;

  SetDecimalEdit(sColor.C);
  SetHEXEdit(sColor.C);

  if not UseCoords then
    ColorCoord := Point(SelectedHsv.h * ColorPanel.Width div 360, Round((1 - SelectedHsv.s) * ColorPanel.Height));

  SetMarker;
  sSkinProvider1.SkinData.BGChanged := True;
  RepaintRect(Self);
  ExitPanels;
  ColorChanging := False;
end;


procedure TsColorDialogForm.SetDecimalEdit(C: TColor);
begin
  if not DecChanging then
    if UseAlpha then
      sEditDecimal.Text := IntToStr(Cardinal(UpdateColorAlpha(C)))
    else
      sEditDecimal.Text := IntToStr(Cardinal(C and clWhite));
end;


procedure TsColorDialogForm.SetHEXEdit(C: TColor);
var
  l: cardinal;
begin
  if not HexChanging then
    if UseAlpha then begin
      l := Cardinal(UpdateColorAlpha(SwapInteger(C and clWhite)));
      sEditHex.Text := IntToHex(Int64(l), 8)
    end
    else
      sEditHex.Text := IntToHex(Cardinal(SwapInteger(C and clWhite)), 6);
end;


procedure TsColorDialogForm.PaintColorRect(DC: hdc; Ctrl: TPaintBox);
var
  Bmp: TBitmap;
begin
  if UseAlpha then begin
    Bmp := CreateBmp32(Ctrl);
    PaintTransBG(Bmp, MkRect(Bmp), clWhite, clSilver, acArrowSize);
    BlendColorRect(Bmp, MkRect(Bmp), MaxByte - TsColor(integer(Ctrl.Tag)).A, Cardinal(integer(Ctrl.Tag)) and clWhite);
    BitBlt(DC, 0, 0, Bmp.Width, Bmp.Height, Bmp.Canvas.Handle, 0, 0, SRCCOPY);
    Bmp.Free;
  end
  else
    FillDC(DC, MkRect(Ctrl), TColor(Ctrl.Tag and $FFFFFF));
end;


procedure TsColorDialogForm.PaintCursor(mX, mY: integer; Canvas: TCanvas);
begin
  Canvas.Ellipse(mX - 5, mY - 5, mX + 5, mY + 5);
end;


procedure TsColorDialogForm.MainPalChange(Sender: TObject);
begin
  if (TsColorsPanel(Sender).ItemIndex >= 0) and not ((Sender = AddPal) and (AddPal.Colors[AddPal.ItemIndex] = 'FFFFFF')) then begin
    SetCurrentColor(TsColorsPanel(Sender).ColorValue, True);
    ColorPanel.SkinData.BGChanged := True;
    ColorPanelPaint(ColorPanel, TAccessPanel(ColorPanel).Canvas);
  end;
end;


procedure TsColorDialogForm.sEditHexKeyPress(Sender: TObject; var Key: Char);
begin
  if CharInSet(Key, [ZeroChar..'9', 'A'..'F', 'a'..'f', Chr(3) {Ctrl-C}, Chr(22) {Ctrl-V}, Chr(27), Chr(13)]) then
    inherited
  else
    Key := #0;
end;


function TsColorDialogForm.GetColorCoord(const aColor: TsColor): integer;
var
  y: integer;
  C: TsColor;
begin
  for y := 0 to GradBmp.Height - 1 do begin
    C := GetAPixel(GradBmp, 1, y);
    with C do
      if (abs(aColor.R - B) < iNear) and (abs(aColor.G - G) < iNear) and (abs(aColor.B - R) < iNear) then begin
        Result := y;
        Exit;
      end;
  end;
  Result := 0;
end;


procedure TsColorDialogForm.SetInternalColor(h: integer; s: real);
var
  sC: TsColor;
begin
  ColorChanging := True;
  SelectedHsv.h := h;
  SelectedHsv.s := s;
  sColor := Hsv2Rgb(h, s, 1);
  InternalColor := max(sColor.C, 0);

  sHEdit.Value := Round(SelectedHsv.H);
  sSEdit.Value := SelectedHsv.S * 100;
  if GradY < GradPanel.Height div 2 then
    sVEdit.Value := 100
  else
    sVEdit.Value := (1 - abs(GradPanel.Height div 2 - GradY) / (GradPanel.Height div 2)) * 100;

  UpdateGradient;
  GradPanel.SkinData.BGChanged := True;
  GradPanelPaint(GradPanel, TAccessPanel(GradPanel).Canvas);
  sC.C := GradToColor(GradY);
  sREdit.Value := sC.R;
  sGEdit.Value := sC.G;
  sBEdit.Value := sC.B;
  SetDecimalEdit(sC.C);
  SetHEXEdit(sC.C);
  SetSelectedColor(sC.C);
  ColorCoord := Point(SelectedHsv.h * ColorPanel.Width div 360, Round((1 - SelectedHsv.s) * ColorPanel.Height));
  ExitPanels;
  ColorChanging := False;
end;


procedure TsColorDialogForm.FormPaint(Sender: TObject);
begin
  if sSkinProvider1.SkinData.Skinned and not sSkinProvider1.SkinData.FCacheBmp.Empty then
    SkinMarker(Self)
  else
    Marker(Canvas.Handle, Point(GradPanel.Left + GradPanel.Width, GradPanel.Top + GradY));
end;


procedure TsColorDialogForm.sEditHexChange(Sender: TObject);
var
  i: int64;
  s: string;
begin
  s := ExtractWord(1, sEditHex.Text, [s_Space]);
  i := HexToInt(s);
  SetColorFromEdit(TColor(SwapInteger(integer(i))), HexChanging);
end;


procedure TsColorDialogForm.sEditDecimalChange(Sender: TObject);
begin
  SetColorFromEdit(TColor(sEditDecimal.AsInteger), DecChanging);
end;


procedure TsColorDialogForm.sHEditChange(Sender: TObject);
begin
  SetColorFromEdit(Hsv2Rgb(sHEdit.asInteger, sSEdit.Value / 100, sVEdit.Value / 100).C, HsvChanging);
end;


procedure TsColorDialogForm.SelectedPanelPaint(Sender: TObject);
begin
  PaintColorRect(TPaintBox(Sender).Canvas.Handle, TPaintBox(Sender));
end;


procedure TsColorDialogForm.SetColorFromEdit(Color: TColor; var Flag: boolean);
begin
  if not ColorChanging then begin
    Flag := True;
    SetCurrentColor(Color, False);
    MainPal.ItemIndex := -1;
    ColorPanel.SkinData.BGChanged := True;
    ColorPanelPaint(ColorPanel, TAccessPanel(ColorPanel).Canvas);
    Flag := False;
  end;
end;


procedure TsColorDialogForm.sREditChange(Sender: TObject);
var
  c: TsColor;
begin
  if not (csLoading in ComponentState) then begin
    c.R := sREdit.AsInteger;
    c.G := sGEdit.AsInteger;
    c.B := sBEdit.AsInteger;
    c.A := sAEdit.AsInteger;
    SetColorFromEdit(c.C, RgbChanging);
  end;
end;


procedure TsColorDialogForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if Assigned(Owner) and (TsColorDialog(Owner).CustomColors.Count <> 0) then
    TsColorDialog(Owner).CustomColors.Assign(AddPal.Colors);

  acCustomColors.Assign(AddPal.Colors);
end;


procedure TsColorDialogForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  with acIntController do
    CanClose := (acIntController = nil) or (PopupCount(Self) = 0) and (PopupChildCount(Self) = 0);
end;


procedure TsColorDialogForm.FormCreate(Sender: TObject);
begin
  sSpeedButton1.Images := acCharImages;
  sBitBtn1.Images := acCharImages;
  sBitBtn2.Images := acCharImages;
  BGUpdating := False;
  CurrCustomIndex := 0;
  if BidiMode = bdRightToLeft then
    ReflectControls(Self, True);
end;


procedure TsColorDialogForm.sBitBtn4Click(Sender: TObject);
begin
  Width := Constraints.MaxWidth;                                                           
  sBitBtn4.Enabled := False;
end;


procedure TsColorDialogForm.sBitBtn5Click(Sender: TObject);
begin
  if Owner <> nil then
    Application.HelpContext(Owner.Helpcontext);
end;


procedure TsColorDialogForm.sBitBtn3Click(Sender: TObject);
begin
  if AddPal.ItemIndex >= 0 then
    AddPal.Colors[AddPal.ItemIndex] := sEditHex.Text
  else begin
    AddPal.Colors[CurrCustomIndex] := sEditHex.Text;
    if CurrCustomIndex < AddPal.Colors.Count - 1 then
      inc(CurrCustomIndex)
  end;
  AddPal.GenerateColors;
  AddPal.SkinData.Invalidate;
end;


procedure TsColorDialogForm.ExitPanels;
begin
  if MainPal.ItemIndex >= 0 then
    if MainPal.ColorValue <> TColor(SelectedPanel.Tag) then
      MainPal.ItemIndex := -1;

  if AddPal.ItemIndex >= 0 then
    if AddPal.ColorValue <> TColor(SelectedPanel.Tag) then begin
      CurrCustomIndex := AddPal.ItemIndex;
      AddPal.ItemIndex := -1;
    end;
end;


procedure TsColorDialogForm.sSpeedButton1Click(Sender: TObject);
var
  DC, SavedDC: hdc;
  PickForm: TForm;
  p: TPoint;
begin
  GetIntController.ClosingForbide(Self);
  Bmp := TBitmap.Create;
  Bmp.Width := Monitor.Width;
  Bmp.Height := Monitor.Height;
  DC := GetDC(0);
  SavedDC := SaveDC(DC);
  try
    BitBlt(Bmp.Canvas.Handle, 0, 0, Bmp.width, Bmp.height, dc, Monitor.Left, Monitor.Top, SRCCOPY);
    PickForm := TForm.Create(Application);
    PickForm.Visible := False;
    PickForm.BorderStyle := bsNone;
    PickForm.Tag := ExceptTag;
    PickForm.DoubleBuffered := True;

    p := SelectedPanel.ClientToScreen(MkPoint);
    PickPanel := TsPanel.Create(PickForm);
    PickPanel.SkinData.CustomColor := True;
    PickPanel.SkinData.SkinSection := s_Transparent;
    PickPanel.BorderStyle := bsNone;
    PickPanel.BevelOuter := bvNone;
    PickPanel.ParentCtl3D := False;
    PickPanel.Ctl3D := False;
    PickPanel.Width := SelectedPanel.Width;
    PickPanel.Height := SelectedPanel.Height;
    PickPanel.Color := TColor(SelectedPanel.Tag);
    PickPanel.Left := p.x - Monitor.Left;
    PickPanel.Top := p.y - Monitor.Top;
    PickPanel.Parent := PickForm;
    PickPanel.Visible := True;

    PickForm.OnMouseMove := PickFormMouseMove;
    PickForm.Cursor := crHandPoint;
    PickForm.Cursor := crASPipette;
    PickForm.WindowState := wsMaximized;
    PickForm.OnPaint := PickFormPaint;
    PickForm.OnMouseDown := PickFormMouseDown;
    PickForm.OnKeyDown := PickFormKeyDown;
    PickForm.ShowModal;
    FreeAndNil(PickPanel);
    FreeAndNil(PickForm);
  finally
    RestoreDC(DC, SavedDC);
    FreeAndNil(Bmp);
  end;
  GetIntController.ClosingAllow(Self);
end;


function TsColorDialogForm.UpdateColorAlpha(Value: TColor): TColor;
begin
  Result := clWhite and Value;
  if UseAlpha then
    Result := TColor(Cardinal(Result) or Cardinal((sAEdit.AsInteger shl 24)))
  else
    Result := TColor(Cardinal(Result) and clWhite);
end;


procedure TsColorDialogForm.UpdateGradient;
var
  Gray_, Gray, x, y, ImgWidth, ImgHeight: integer;
  CtrlMul, R, G, B, RStep, GStep, BStep: real;
  S: PRGBAArray_;
  C: TsColor_;
begin
  if GradBmp = nil then
    GradBmp := CreateBmp32(GradPanel.Width, GradPanel.Height);

  ImgWidth := GradPanel.Width;
  ImgHeight := GradPanel.Height;
  GradBmp.Width := ImgWidth;
  GradBmp.Height := ImgHeight;
  C.C := SwapRedBlue(InternalColor);
  CtrlMul := MaxByte / ImgHeight;
  Gray := Round((c.R + c.G + c.B) div 3 * CtrlMul);
  inc(Gray, (ImgHeight - MaxByte) div 2);
  Gray_ := ImgHeight - Gray; // Vertical coord of current color in panel

  R := MaxByte;
  G := MaxByte;
  B := MaxByte;

  if Gray_ = 0 then begin
    RStep := 0;
    GStep := 0;
    BStep := 0;
  end
  else begin
    RStep := (MaxByte - c.R) / Gray_;
    GStep := (MaxByte - c.G) / Gray_;
    BStep := (MaxByte - c.B) / Gray_;
  end;
  y := 0;
  while y < Gray_ do begin
    R := R - RStep;
    G := G - GStep;
    B := B - BStep;
    C.R := max(round(R), 0);
    C.G := max(round(G), 0);
    C.B := max(round(B), 0);
    S := GradBmp.ScanLine[y];
    for x := 0 to ImgWidth - 1 do
      S[X] := C;

    inc(y)
  end;

  if Gray = 0 then begin
    RStep := 0;
    GStep := 0;
    BStep := 0;
  end
  else begin
    RStep := C.R / Gray;
    GStep := C.G / Gray;
    BStep := C.B / Gray;
  end;

  dec(ImgWidth);
  while y < ImgHeight do begin
    R := max(0, R - RStep);
    G := max(0, G - GStep);
    B := max(0, B - BStep);

    C.R := max(min(round(R), MaxByte), 0);
    C.G := max(min(round(G), MaxByte), 0);
    C.B := max(min(round(B), MaxByte), 0);

    S := GradBmp.ScanLine[y];
    for x := 0 to ImgWidth do
      S[X] := C;

    inc(y);
  end;
end;


procedure TsColorDialogForm.PickFormPaint(Sender: TObject);
begin
  BitBlt(TForm(Sender).Canvas.Handle, 0, 0, Bmp.width, Bmp.height, Bmp.Canvas.Handle, 0, 0, SRCCOPY);
end;


procedure TsColorDialogForm.PickFormMouseDown(Sender: TObject;  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  PickColor := ColorToRGB(Bmp.Canvas.Pixels[x + 1, y - 2]);
  SetCurrentColor(PickColor, True);
  TForm(Sender).Close;
end;


procedure TsColorDialogForm.PickFormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = 27 then
    if (acIntController = nil) or (acIntController.PopupCount(Self) = 0) then
      TForm(Sender).Close;
end;


procedure TsColorDialogForm.PickFormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if PickPanel <> nil then begin
    PickColor := ColorToRGB(Bmp.Canvas.Pixels[x + 1, y - 2]);
    PickPanel.Color := PickColor;
    PickPanel.Repaint;
  end;
end;


procedure TsColorDialogForm.InitControls(FullOpen, ShowHelp: boolean; aColor: TColor; aBorderStyle: TFormBorderStyle);
begin
  InitLngCaptions;
  GradY := -1;
  BorderStyle := aBorderStyle;
  sBitBtn4.Enabled := not FullOpen;

{$IFNDEF D2007}
  if sSkinProvider1.SkinData.SkinManager <> nil then begin
    Constraints.MaxWidth := sSkinProvider1.SkinData.SkinManager.ScaleInt(iMaxWidth);
    Constraints.MinWidth := sSkinProvider1.SkinData.SkinManager.ScaleInt(iMinWidth);
  end
  else
{$ENDIF}
  begin
    Constraints.MaxWidth := iMaxWidth;
    Constraints.MinWidth := iMinWidth;
  end;

  if sBitBtn4.Enabled then
    Width := Constraints.MinWidth
  else
    Width := Constraints.MaxWidth;

  sBitBtn5.Visible := ShowHelp;
  sAEdit.Visible := UseAlpha;

  ColorChanging := True;
  if UseAlpha then begin
    sEditHex.MaxLength := 8;
    sEditHex.EditMask := 'AAAAAAAA;1;';
  end
  else begin
    sEditHex.MaxLength := 6;
    sEditHex.EditMask := 'AAAAAA;1;';
  end;

  sColor.C := aColor;
  SelectedPanel.Tag := aColor;
  OldPanel.Tag := aColor;
  if not Assigned(Owner) or (TsColorDialog(Owner).CustomColors.Count = 0) then begin
    AddPal.Colors.Assign(acCustomColors);
    AddPal.GenerateColors;
  end
  else
    if Assigned(Owner) then
      AddPal.Colors.Assign(TsColorDialog(Owner).CustomColors);

  if aBorderStyle = bsNone then begin
    sColorDialogForm.Position := poDesigned;
    ClientHeight := 392 * sSkinProvider1.SkinData.ScalePercent div 100;
  end
  else begin
    sColorDialogForm.Position := poScreenCenter;
    ClientHeight := 388 * sSkinProvider1.SkinData.ScalePercent div 100;
  end;
  sSkinProvider1.PrepareForm;
end;


procedure TsColorDialogForm.InitLngCaptions;
begin
  sBitBtn1.Caption := acs_MsgDlgOK;
  sBitBtn2.Caption := acs_MsgDlgCancel;
  sBitBtn3.Caption := acs_ColorDlgAdd;
  sBitBtn4.Caption := acs_ColorDlgDefine;
  sBitBtn5.Caption := acs_MsgDlgHelp;
  sLabel2. Caption := acs_ColorDlgAddPal;
  Caption := acs_ColorDlgTitle;
  sREdit.BoundLabel.Caption := acs_ColorDlgRed;
  sGEdit.BoundLabel.Caption := acs_ColorDlgGreen;
  sBEdit.BoundLabel.Caption := acs_ColorDlgBlue;
  sEditHex.BoundLabel.Caption := acs_ColorDlgHex;
  sEditDecimal.BoundLabel.Caption := acs_ColorDlgDecimal;
end;


function TsColorDialogForm.MarkerRect: TRect;
begin
  Result := Rect(GradPanel.Left + GradPanel.Width, 0, Width, GradPanel.Top + GradPanel.Height + 4);
end;


procedure TsColorDialogForm.FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  R: TRect;
begin
  R := MarkerRect;
  inc(R.Top, GradPanel.Top);
  R.Bottom := GradPanel.Top + GradPanel.Height;
  if PtInRect(R, Point(x, y)) then
    GradPanel.OnMouseDown(GradPanel, Button, Shift, X - GradPanel.Left - GradPanel.Width, Y - GradPanel.Top);
end;


procedure TsColorDialogForm.FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  R: TRect;
begin
  R := MarkerRect;
  inc(R.Top, GradPanel.Top);
  R.Bottom := GradPanel.Top + GradPanel.Height;
  if PtInRect(R, Point(x, y)) then
    GradPanel.OnMouseMove(GradPanel, Shift, X - GradPanel.Left - GradPanel.Width, Y - GradPanel.Top);
end;


procedure TsColorDialogForm.FormMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  R: TRect;
begin
  R := MarkerRect;
  inc(R.Top, GradPanel.Top);
  R.Bottom := GradPanel.Top + GradPanel.Height;
  if PtInRect(R, Point(x, y)) then
    GradPanel.OnMouseUp(GradPanel, Button, Shift, X - GradPanel.Left - GradPanel.Width, Y - GradPanel.Top);
end;


procedure TsColorDialogForm.MainPalDblClick(Sender: TObject);
begin
  sBitBtn1.Click
end;


procedure TsColorDialogForm.AddPalDblClick(Sender: TObject);
begin
  sBitBtn1.Click;
end;


destructor TsColorDialogForm.Destroy;
begin
  FreeAndNil(ExtBmp);
  FreeAndNil(BGBmp);
  FreeAndNil(GradBmp);
  inherited;
end;


procedure InitCustomColors;
var
  ic: integer;
begin
  acCustomColors := TStringList.Create;
  for ic := 0 to 17 do
    acCustomColors.Add('FFFFFF');
end;


initialization
  Screen.Cursors[crASPipette] := LoadCursor(HInstance, 'CRASPIPETTE');
  InitCustomColors;


finalization
  acCustomColors.Free;

end.
