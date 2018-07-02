unit acHintPage;
{$I sDefs.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, StdCtrls, ComCtrls, ExtCtrls, ExtDlgs, Dialogs, Buttons, 
  {$IFDEF DELPHI6} Variants, {$ENDIF}
  {$IFDEF DELPHI7UP} Types, {$ENDIF}
  sSpeedButton, sScrollBox, sFrameAdapter, sLabel, sTrackBar, sPageControl, sRadioButton, sGroupBox, acAlphaHints, sPanel,
  sEdit, sSpinEdit;


type
  TFrameHintPage = class(TFrame)
    sScrollBox1: TsScrollBox;
    sFrameAdapter1: TsFrameAdapter;
    PaintBox1: TPaintBox;
    OpenPictureDialog1: TOpenPictureDialog;
    sGroupBox2: TsGroupBox;
    sLabel14: TsLabel;
    sLabel19: TsLabel;
    sLabel2: TsLabel;
    sLabel9: TsLabel;
    sGroupBox3: TsGroupBox;
    sGroupBox4: TsGroupBox;
    sGroupBox5: TsPanel;
    sRadioButton1: TsRadioButton;
    sRadioButton2: TsRadioButton;
    sGroupBox6: TsPanel;
    sRadioButton3: TsRadioButton;
    sRadioButton4: TsRadioButton;
    sLabel26: TsLabel;
    sLabel27: TsLabel;
    sLabel28: TsLabel;
    sLabel29: TsLabel;
    sPanel1: TsPanel;
    sPanel2: TsPanel;
    sRadioButton5: TsRadioButton;
    sRadioButton6: TsRadioButton;
    sLabel34: TsLabel;
    sLabel35: TsLabel;
    sLabel36: TsLabel;
    sLabel37: TsLabel;
    sRadioButton7: TsRadioButton;
    sRadioButton8: TsRadioButton;
    sPanel3: TsPanel;
    sRadioButton9: TsRadioButton;
    sRadioButton10: TsRadioButton;
    sLabel38: TsLabel;
    sGroupBox7: TsGroupBox;
    sLabel1: TsLabel;
    sLabel3: TsLabel;
    sLabel5: TsLabel;
    sLabel6: TsLabel;
    sLabel7: TsLabel;
    sLabel8: TsLabel;
    sLabel11: TsLabel;
    sLabel12: TsLabel;
    sTrackBar9: TsTrackBar;
    sTrackBar10: TsTrackBar;
    sTrackBar11: TsTrackBar;
    sTrackBar12: TsTrackBar;
    sSpeedButton3: TsSpeedButton;
    sLabel21: TsLabel;
    sLabel23: TsLabel;
    sLabel22: TsLabel;
    sLabel24: TsLabel;
    sSpinEdit1: TsSpinEdit;
    sSpinEdit2: TsSpinEdit;
    sSpeedButton1: TsSpeedButton;
    sSpeedButton2: TsSpeedButton;
    sRangeSelector1: TsRangeSelector;
    sRangeSelector2: TsRangeSelector;
    sRangeSelector3: TsRangeSelector;
    sRangeSelector4: TsRangeSelector;
    sLabel41: TsLabel;
    sLabel4: TsLabel;
    sLabel43: TsLabel;
    sLabel10: TsLabel;
    sLabel42: TsLabel;
    sLabel15: TsLabel;
    sLabel144: TsLabel;
    sLabel20: TsLabel;
    procedure PaintBox1Paint(Sender: TObject);
    procedure sSpeedButton3Click(Sender: TObject);
    procedure sRadioButton1Click(Sender: TObject);
    procedure sTrackBar10Change(Sender: TObject);
    procedure sTrackBar12Change(Sender: TObject);
    procedure sSpinEdit1Change(Sender: TObject);
    procedure sSpinEdit2Change(Sender: TObject);
    procedure sSpeedButton1Click(Sender: TObject);
    procedure sRangeSelector1Change(Sender: TObject);
    procedure sRangeSelector2Change(Sender: TObject);
    procedure sRangeSelector3Change(Sender: TObject);
    procedure sRangeSelector4Change(Sender: TObject);
    procedure sTrackBar9Change(Sender: TObject);
    procedure sTrackBar11Change(Sender: TObject);
  protected
    procedure ChangeProperty(aBordersSizes: TacBordersSizes; Index1, Index2: integer; aRangeSelector: TsRangeSelector; aLabel1, aLabel2: TsLabel);
    procedure ChangeShadowSize(aTrackBar: TsTrackBar; aLabel: TsLabel);
  public
    Image: TacHintImage;
    procedure LoadImage(Img: TacHintImage; Repaint: boolean = True);
    function CreateAlphaBmp: TBitmap;
    procedure UpdateData(Repaint: boolean = True);
    procedure RefreshPaintBox;
    procedure WndProc(var Message: TMessage); override;
  end;


var
  acUpdating: boolean = False;
  acPreviewScale: integer = 0;


implementation

uses math, sAlphaGraph, acAlphaHintsEdit, sGraphUtils, sConst, sVCLUtils, acPNG, sSkinProvider, acntUtils,
  acDesignData;

{$R *.dfm}

const
  iTOP = 0;
  iLFT = 1;
  iBTM = 2;
  iRGT = 3;

procedure TFrameHintPage.LoadImage(Img: TacHintImage; Repaint: boolean = True);
begin
  Image := Img;
  UpdateData(Repaint);
end;


procedure TFrameHintPage.sTrackBar10Change(Sender: TObject);
begin
  ChangeShadowSize(TsTrackBar(Sender), sLabel3);
end;


procedure TFrameHintPage.sTrackBar11Change(Sender: TObject);
begin
  ChangeShadowSize(TsTrackBar(Sender), sLabel5);
end;


procedure TFrameHintPage.sTrackBar12Change(Sender: TObject);
begin
  ChangeShadowSize(TsTrackBar(Sender), sLabel6);
end;


procedure TFrameHintPage.sTrackBar9Change(Sender: TObject);
begin
  ChangeShadowSize(TsTrackBar(Sender), sLabel1);
end;


procedure TFrameHintPage.ChangeShadowSize;
begin
  if Image <> nil then begin
    if not acUpdating then
      Image.ShadowSizes.SetInteger(aTrackBar.Tag and $F, aTrackBar.Position);

    aLabel.Caption := IntToStr(aTrackBar.Position);
  end;
end;


procedure TFrameHintPage.PaintBox1Paint(Sender: TObject);
var
  BorderColor: TsColor;
  SrcBmp: TBitmap;
  X, Y: integer;
  R: TRect;

  procedure PaintCtrl;
  var
    R: TRect;
    Size: integer;
  begin
    Size := ac_ArrowWidth * 5;
    if Image = Image.FOwner.ImageDefault then
      if Image.FOwner.HintAlign = haHorzCenter then
        R.TopLeft := Point(X - Size div 2, Y - Size)
      else
        R.TopLeft := Point(X - Size, Y - Size div 2)

    else
      if Image = Image.FOwner.Img_RightTop then
        if Image.FOwner.HintAlign = haHorzCenter then
          R.TopLeft := Point(X + SrcBmp.Width - Size div 2, Y - Size)
        else
          R.TopLeft := Point(X + SrcBmp.Width, Y - Size div 2)
      else
        if Image = Image.FOwner.Img_RightBottom then
          if Image.FOwner.HintAlign = haHorzCenter then
            R.TopLeft := Point(X + SrcBmp.Width - Size div 2, Y + SrcBmp.Height)
          else
            R.TopLeft := Point(X + SrcBmp.Width, Y + SrcBmp.Height - Size div 2)
        else
          if Image.FOwner.HintAlign = haHorzCenter then
            R.TopLeft := Point(X - Size div 2, Y + SrcBmp.Height)
          else
            R.TopLeft := Point(X - Size, Y + SrcBmp.Height -Size div 2);

    dec(R.Left, Image.OffsetHorz);
    dec(R.Top,  Image.OffsetVert);

    R.Right := R.Left + Size;
    R.Bottom := R.Top + Size;
    PreviewBmp.Canvas.Brush.Color := clWhite;
    PreviewBmp.Canvas.Pen.Style := psSolid;
    PreviewBmp.Canvas.Pen.Color := 0;
    PreviewBmp.Canvas.Pen.Width := 1;
    PreviewBmp.Canvas.Rectangle(R);
    PreviewBmp.Canvas.Pen.Color := clSilver;

    acPaintLine(PreviewBmp.Canvas.Handle, R.Left + Size div 2, R.Top + 1,          R.Left + Size div 2, R.Bottom - 1);
    acPaintLine(PreviewBmp.Canvas.Handle, R.Left  + 1,         R.Top + Size div 2, R.Right - 1,         R.Top + Size div 2);
  end;

begin
  if PreviewBmp = nil then begin
    PreviewBmp := CreateBmp32(PaintBox1.Width, PaintBox1.Height);
    PaintTransBG(PreviewBmp, MkRect(PreviewBmp), clWhite, clSilver, acArrowSize * 2);
    if (CurrentTemplate <> nil) and (Image.Image <> nil) then begin
      if Image.Image.Empty and (Image.FImageData.Size <> 0) then
        Image.Image.LoadFromStream(Image.FImageData);

      if not Image.Image.Empty then begin
        SrcBmp := CreateAlphaBmp;
        X := (PaintBox1.Width  - SrcBmp.Width)  div 2;
        Y := (PaintBox1.Height - SrcBmp.Height) div 2;
        PaintCtrl;
        CopyBmp32(Rect(X, Y, X + SrcBmp.Width, Y + SrcBmp.Height), MkRect(SrcBmp), PreviewBmp, SrcBmp, EmptyCI, False, clNone, 0, False);
        BorderColor.C := 0;

        if sRangeSelector1.Focused then begin
          FadeBmp(PreviewBmp, Rect(X, Y, X + Image.BordersWidths.Left, Y + SrcBmp.Height), 50, BorderColor, 0, 0);
          FadeBmp(PreviewBmp, Rect(X + SrcBmp.Width - Image.BordersWidths.Right, Y, X + SrcBmp.Width, Y + SrcBmp.Height), 50, BorderColor, 0, 0);
        end
        else
          if sRangeSelector2.Focused then begin
            FadeBmp(PreviewBmp, Rect(X, Y, X + SrcBmp.Width, Y + Image.BordersWidths.Top), 50, BorderColor, 0, 0);
            FadeBmp(PreviewBmp, Rect(X, Y + SrcBmp.Height - Image.BordersWidths.Bottom, X + SrcBmp.Width, Y + SrcBmp.Height), 50, BorderColor, 0, 0);
          end;

        if sTrackBar9.Focused then
          FadeBmp(PreviewBmp, Rect(X, Y, X + Image.ShadowSizes.Left, Y + SrcBmp.Height), 50, BorderColor, 0, 0)
        else
          if sTrackBar10.Focused then
            FadeBmp(PreviewBmp, Rect(X, Y, X + SrcBmp.Width, Y + Image.ShadowSizes.Top), 50, BorderColor, 0, 0)
          else
            if sTrackBar11.Focused then
              FadeBmp(PreviewBmp, Rect(X + SrcBmp.Width - Image.ShadowSizes.Right, Y, X + SrcBmp.Width, Y + SrcBmp.Height), 50, BorderColor, 0, 0)
            else
              if sTrackBar12.Focused then
                FadeBmp(PreviewBmp, Rect(X, Y + SrcBmp.Height - Image.ShadowSizes.Bottom, X + SrcBmp.Width, Y + SrcBmp.Height), 50, BorderColor, 0, 0);

        FreeAndNil(SrcBmp);
      end;
    end;
  end;
  if acPreviewScale > 0 then begin
    R.TopLeft := Point((PreviewBmp.Width  - PreviewBmp.Width div acPreviewScale) div 2,
                       (PreviewBmp.Height - PreviewBmp.Height div acPreviewScale) div 2);

    R.Right  := R.Left + PreviewBmp.Width div acPreviewScale;
    R.Bottom := R.Top + PreviewBmp.Height div acPreviewScale;

    SetStretchBltMode(PreviewBmp.Canvas.Handle, COLORONCOLOR);
    StretchBlt(PaintBox1.Canvas.Handle, 0, 0, PaintBox1.Width, PaintBox1.Height, PreviewBmp.Canvas.Handle,
               R.Left, R.Top, WidthOf(R), HeightOf(R), SRCCOPY);

  end
  else
    BitBlt(PaintBox1.Canvas.Handle, 0, 0, PaintBox1.Width, PaintBox1.Height, PreviewBmp.Canvas.Handle, 0, 0, SRCCOPY);
end;


procedure TFrameHintPage.ChangeProperty(ABordersSizes: TacBordersSizes; Index1, Index2: integer; ARangeSelector: TsRangeSelector; ALabel1, ALabel2: TsLabel);
begin
  if (Image <> nil) then begin
    if not acUpdating then begin
      ABordersSizes.SetInteger(Index1, ARangeSelector.Position1);
      ABordersSizes.SetInteger(Index2, ARangeSelector.Max - ARangeSelector.Position2);
    end;
    ALabel1.Caption := IntToStr(ARangeSelector.Position1);
    ALabel2.Caption := IntToStr(ARangeSelector.Max - ARangeSelector.Position2);
  end;
end;


function TFrameHintPage.CreateAlphaBmp: TBitmap;
const
  PreviewText = 'Hint preview';
var
  TempBmp: TBitmap;
  b: boolean;
begin
  Result := CreateBmp32;
  Result.Canvas.Font.Assign(CurrentTemplate.Font);
  Result.Canvas.Font.Size := Font.Size;
  Result.Width  := Result.Canvas.TextWidth (PreviewText);
  Result.Height := Result.Canvas.TextHeight(PreviewText);
  Result.Width  := max(Result.Width  + Image.ClientMargins.Left + Image.ClientMargins.Right, Image.BordersWidths.Left + Image.BordersWidths.Right);
  Result.Height := max(Result.Height + Image.ClientMargins.Top  + Image.ClientMargins.Bottom, Image.BordersWidths.Top + Image.BordersWidths.Bottom);

  PaintControlByTemplate(Result, Image.Image, MkRect(Result), MkRect(Image.Image),
      Rect(Image.BordersWidths.Left, Image.BordersWidths.Top, Image.BordersWidths.Right, Image.BordersWidths.Bottom),
      Rect(MaxByte, MaxByte, MaxByte, MaxByte),
      Rect(ord(Image.BorderDrawModes.Left), ord(Image.BorderDrawModes.Top), ord(Image.BorderDrawModes.Right), ord(Image.BorderDrawModes.Bottom)), boolean(ord(Image.BorderDrawModes.Center)));

  TempBmp := TBitmap.Create;
  TempBmp.Assign(Result);
  Result.Canvas.Brush.Style := bsClear;
  if sFrameAdapter1.SkinData.Skinned then begin
    b := sFrameAdapter1.SkinData.SkinManager.Options.ChangeSysColors;
    sFrameAdapter1.SkinData.SkinManager.Options.ChangeSysColors := False;
    SetTextColor(Result.Canvas.Handle, Cardinal(ColorToRGB(CurrentTemplate.Font.Color)));
    sFrameAdapter1.SkinData.SkinManager.Options.ChangeSysColors := b;
  end;
  Result.Canvas.TextOut(Image.ClientMargins.Left, Image.ClientMargins.Top, PreviewText);
  CopyChannel32(Result, TempBmp, 3);
  FreeAndNil(TempBmp);
end;


procedure TFrameHintPage.UpdateData(Repaint: boolean = True);
var
  w, h: integer;

  procedure ResetSelector(aSelector: TsRangeSelector);
  begin
    aSelector.Max := 0;
    aSelector.Position1 := 0;
    aSelector.Position2 := 0;
  end;

begin
  if not acUpdating then begin
    acUpdating := True;
    if not (AlphaHintsEdit.ActiveControl is TsRadioButton) then
      if (Image <> nil) then begin
        if Image.Image.Empty and (Image.FImageData.Size <> 0) then
          Image.Image.LoadFromStream(Image.FImageData);

        if not Image.Image.Empty then begin
          w := Image.ImageWidth;
          h := Image.ImageHeight;
          sLabel23.Caption := IntToStr(w);
          sLabel24.Caption := IntToStr(h);

          sRangeSelector1.Max := w - 1;
          sRangeSelector3.Max := w - 1;
          sRangeSelector2.Max := h - 1;
          sRangeSelector4.Max := h - 1;

          sSpinEdit1.Value := Image.OffsetHorz;
          sSpinEdit2.Value := Image.OffsetVert;

          // Border width
          if Image.BordersWidths.Left + Image.BordersWidths.Top + Image.BordersWidths.Right + Image.BordersWidths.Bottom = 0 then begin // First init
            Image.BordersWidths.Left := w div 2;
            Image.BordersWidths.Right := Image.BordersWidths.Left;
            Image.BordersWidths.Top := h div 2;
            Image.BordersWidths.Bottom := Image.BordersWidths.Top;
          end;
          sRangeSelector1.Position1 := Image.BordersWidths.Left;
          sRangeSelector1.Position2 := sRangeSelector1.Max - Image.BordersWidths.Right;
          sRangeSelector2.Position1 := Image.BordersWidths.Top;;
          sRangeSelector2.Position2 := sRangeSelector2.Max - Image.BordersWidths.Bottom;

          // Drawing modes
          if Image.BorderDrawModes.Left   = dmRepeat then sRadioButton1.Checked := True else sRadioButton2.Checked  := True;
          if Image.BorderDrawModes.Top    = dmRepeat then sRadioButton3.Checked := True else sRadioButton4.Checked  := True;
          if Image.BorderDrawModes.Right  = dmRepeat then sRadioButton5.Checked := True else sRadioButton6.Checked  := True;
          if Image.BorderDrawModes.Bottom = dmRepeat then sRadioButton7.Checked := True else sRadioButton8.Checked  := True;
          if Image.BorderDrawModes.Center = dmRepeat then sRadioButton9.Checked := True else sRadioButton10.Checked := True;

          // Margins
          if Image.ClientMargins.Left + Image.ClientMargins.Top + Image.ClientMargins.Right + Image.ClientMargins.Bottom = 0 then begin // First init
            Image.ClientMargins.Left := w div 2;
            Image.ClientMargins.Right := Image.ClientMargins.Left;
            Image.ClientMargins.Top := h div 2;
            Image.ClientMargins.Bottom := Image.ClientMargins.Top;
          end;
          sRangeSelector3.Position1 := Image.ClientMargins.Left;
          sRangeSelector3.Position2 := sRangeSelector3.Max - Image.ClientMargins.Right;
          sRangeSelector4.Position1 := Image.ClientMargins.Top;
          sRangeSelector4.Position2 := sRangeSelector4.Max - Image.ClientMargins.Bottom;

          // Shadows
          sTrackBar9 .Position := Image.ShadowSizes.Left;
          sTrackBar10.Position := Image.ShadowSizes.Top;
          sTrackBar11.Position := Image.ShadowSizes.Right;
          sTrackBar12.Position := Image.ShadowSizes.Bottom;

          if AlphaHintsEdit.FrameTL = Self then
            ChangeStates(Self, 32, True);

          ChangeStates(Self, 64, not Image.Image.Empty);
        end;
      end
      else begin
        sLabel23.Caption := ZeroChar;
        sLabel24.Caption := ZeroChar;
        ResetSelector(sRangeSelector1);
        ResetSelector(sRangeSelector2);
        ResetSelector(sRangeSelector3);
        ResetSelector(sRangeSelector4);
        sTrackBar9 .Position := 0;
        sTrackBar10.Position := 0;
        sTrackBar11.Position := 0;
        sTrackBar12.Position := 0;
        ChangeStates(AlphaHintsEdit, 64, False);
      end;

    case acPreviewScale of
      0: sSpeedButton1.Down := True;
      1: sSpeedButton2.Down := True;
    end;

    if Repaint and not InAnimationProcess then
      RefreshPaintBox;

    acUpdating := False;
  end;
end;


procedure TFrameHintPage.sSpeedButton3Click(Sender: TObject);
var
  Bmp: TBitmap;
begin
  if OpenPictureDialog1.Execute then begin
    Image.FImageData.LoadFromFile(OpenPictureDialog1.FileName);
    Image.StreamChanged;
    Bmp := TBitmap.Create;
    Bmp.Assign(Image.FImage);
    Bmp.PixelFormat := pf32bit;
  end;
end;


procedure TFrameHintPage.WndProc(var Message: TMessage);
begin
  inherited;
  if (Message.Msg = CM_FOCUSCHANGED) and not InAnimationProcess and not AeroIsEnabled and (PaintBox1 <> nil) and sFrameAdapter1.SkinData.Skinned then
    RefreshPaintBox;
end;


procedure TFrameHintPage.sRadioButton1Click(Sender: TObject);
var
  bm: TacBorderDrawMode;
begin
  bm := TacBorderDrawMode((TsRadioButton(Sender).Tag and 1) mod 2);
  if Image <> nil then
    with Image.BorderDrawModes do
      case (TsRadioButton(Sender).Tag shr 1) and $F of
        0: Left   := bm;
        1: Top    := bm;
        2: Right  := bm;
        3: Bottom := bm;
        4: Center := bm;
      end;
end;


procedure TFrameHintPage.sRangeSelector1Change(Sender: TObject);
begin
  ChangeProperty(Image.BordersWidths, iLFT, iRGT, sRangeSelector1, sLabel2, sLabel14);
end;


procedure TFrameHintPage.sRangeSelector2Change(Sender: TObject);
begin
  ChangeProperty(Image.BordersWidths, iTOP, iBTM, sRangeSelector2, sLabel9, sLabel19);
end;


procedure TFrameHintPage.sRangeSelector3Change(Sender: TObject);
begin
  ChangeProperty(Image.ClientMargins, iLFT, iRGT, sRangeSelector3, sLabel4, sLabel15);
end;


procedure TFrameHintPage.sRangeSelector4Change(Sender: TObject);
begin
  ChangeProperty(Image.ClientMargins, iTOP, iBTM, sRangeSelector4, sLabel10, sLabel20);
end;


procedure TFrameHintPage.RefreshPaintBox;
begin
  FreeAndNil(PreviewBmp); // Update bitmap
  if IsWindowVisible(Handle) then
    PaintBox1.Perform(WM_PAINT, WParam(PaintBox1.Canvas.Handle), 0);
end;


procedure TFrameHintPage.sSpinEdit1Change(Sender: TObject);
begin
  if not acUpdating and (Image <> nil) then
    Image.OffsetHorz := sSpinEdit1.Value;
end;


procedure TFrameHintPage.sSpinEdit2Change(Sender: TObject);
begin
  if not acUpdating and (Image <> nil) then
    Image.OffsetVert := sSpinEdit2.Value;
end;


procedure TFrameHintPage.sSpeedButton1Click(Sender: TObject);
begin
  acPreviewScale := TsSpeedButton(Sender).Tag;
  RefreshPaintBox;
end;

end.
