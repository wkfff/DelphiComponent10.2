unit sGroupBox;
{$I sDefs.inc}
//{$DEFINE LOGGED}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, StdCtrls, ImgList,
  {$IFNDEF DELPHI5} Types, {$ENDIF}
  {$IFDEF TNTUNICODE} TntGraphics, TntControls, TntActnList, TntClasses, TntSysUtils, TntForms, TntStdCtrls, {$ENDIF}
  sCommonData, sRadioButton, sConst;


type
  TsCaptionLayout = (clTopLeft, clTopCenter, clTopRight);

{$IFNDEF NOTFORHELP}
  TsMargin = class(TPersistent)
  private
    FTop,
    FLeft,
    FRight,
    FBottom: TacIntProperty;

    FControl: TControl;
    procedure SetMargin(const Index: Integer; Value: TacIntProperty);
  public
    constructor Create(Control: TControl);
  published
    property Left:   TacIntProperty index 0 read FLeft   write SetMargin default 2;
    property Top:    TacIntProperty index 1 read FTop    write SetMargin default 0;
    property Right:  TacIntProperty index 2 read FRight  write SetMargin default 2;
    property Bottom: TacIntProperty index 3 read FBottom write SetMargin default 0;
  end;
{$ENDIF}


{$IFDEF DELPHI_XE3}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]{$ENDIF}
{$IFDEF TNTUNICODE}
  TsGroupBox = class(TTntGroupBox)
{$ELSE}
  TsGroupBox = class(TGroupBox)
{$ENDIF}
{$IFNDEF NOTFORHELP}
  private
    FChecked,
    FCheckBoxVisible: boolean;

    FImageIndex,
    FCaptionYOffset: integer;

    FCaptionMargin: TsMargin;
    FCommonData: TsCommonData;
    FCaptionSkin: TsSkinSection;
    FCaptionWidth: TacIntProperty;
    FCaptionLayout: TsCaptionLayout;

    FOnCheckBoxChanged: TNotifyEvent;
    FImageChangeLink: TChangeLink;
    FImages: TCustomImageList;
    procedure WMFontChanged   (var Message: TMessage); message CM_FONTCHANGED;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure SetCaptionSkin  (const Value: TsSkinSection);
    procedure SetCaptionWidth (const Value: TacIntProperty);
    procedure SetCaptionLayout(const Value: TsCaptionLayout);
    procedure SetImages       (const Value: TCustomImageList);

    procedure SetInteger(const Index: Integer; const Value: integer); virtual;
    procedure SetBoolean(const Index: Integer; const Value: boolean); virtual;
  protected
    CheckHot,
    CheckPressed: boolean;

    TopRect,
    TextRect,
    GlyphRect,
    CheckBoxRect: TRect;

    TextIndex: integer;

    procedure ImageListChange(Sender: TObject);
    procedure AdjustClientRect(var Rect: TRect); override;
    procedure WndProc(var Message: TMessage); override;
    procedure CheckBoxChanged; virtual;
    function CheckBoxHeight: integer;
    function CheckBoxWidth: integer;
    function TextHeight: integer;
    function MouseInCheckBox: boolean;
    function CheckBoxIndex: integer;
    procedure PaintCheckBoxSkin(Bmp: TBitmap; R: TRect; State: integer);
    procedure PaintCheckBoxStd (DC:  hdc;     R: TRect; State: integer);
    procedure RepaintCheckBox(State: integer; DoAnimation: boolean = False);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    CaptionHeight: integer;
    CaptionRect: TRect;
    procedure AfterConstruction; override;
    procedure Loaded; override;
    procedure InitPaintData;
    function ImageExists: boolean;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    procedure PaintToDC(DC: hdc);
    function PrepareCache: boolean;
    procedure PaintCaptionArea(cRect: TRect; CI: TCacheInfo; AState: integer);
    procedure WriteText(R: TRect);
  published
{$ENDIF} // NOTFORHELP
    property CaptionLayout: TsCaptionLayout read FCaptionLayout write SetCaptionLayout default clTopLeft;
    property CaptionMargin: TsMargin read FCaptionMargin write FCaptionMargin;
    property SkinData: TsCommonData read FCommonData write FCommonData;
    property CaptionSkin: TsSkinSection read FCaptionSkin write SetCaptionSkin;
    property CaptionWidth: TacIntProperty read FCaptionWidth write SetCaptionWidth default 0;
    property Images: TCustomImageList read FImages write SetImages;

    property CaptionYOffset: integer index 0 read FCaptionYOffset write SetInteger default 0;
    property ImageIndex:     integer index 1 read FImageIndex     write SetInteger default -1;

    property CheckBoxVisible: boolean index 0 read FCheckBoxVisible write SetBoolean default False;
    property Checked:         boolean index 1 read FChecked         write SetBoolean default False;
{$IFDEF D2010}
    property Touch;
{$ENDIF}
    property OnCheckBoxChanged: TNotifyEvent read FOnCheckBoxChanged write FOnCheckBoxChanged;
{$IFNDEF NOTFORHELP}
    property OnKeyDown;
    property OnKeyPress;
{$ENDIF} // NOTFORHELP
  end;


{$IFNDEF NOTFORHELP}
  TacIndexChangingEvent = procedure(Sender: TObject; NewIndex: Integer; var AllowChange: Boolean) of object;
{$ENDIF}

  TsRadioGroup = class(TsGroupBox)
{$IFNDEF NOTFORHELP}
  private
    FReading,
    FUpdating,
    FShowFocus: boolean;

    FColumns,
    FItemIndex,
    FContentVOffset: integer;

    FOnChange: TNotifyEvent;
    FAnimatEvents: TacAnimatEvents;
    FOnChanging: TacIndexChangingEvent;
    FItems: {$IFDEF TNTUNICODE}TTntStrings{$ELSE}TStrings{$ENDIF};
    FDisableItemsIfUnchecked: boolean;

    procedure UpdateButtons;
    procedure ArrangeButtons;
    procedure ButtonClick(Sender: TObject);
    procedure ItemsChange(Sender: TObject);
    procedure SetButtonCount(Value: Integer);
    procedure WMSize          (var Message: TWMSize);  message WM_SIZE;
    procedure CMFontChanged   (var Message: TMessage); message CM_FONTCHANGED;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    function GetButtons(Index: Integer): TsRadioButton;
    procedure SetItems(Value: {$IFDEF TNTUNICODE}TTntStrings{$ELSE}TStrings{$ENDIF});
    procedure SetInteger(const Index: integer; const Value: integer); override;
    procedure SetDisableItemsIfUnchecked(const Value: boolean);
  protected
    procedure ReadState(Reader: TReader); override;
    procedure WndProc(var Message: TMessage); override;
    procedure CheckBoxChanged; override;
  public
    FButtons: TList;
    procedure Loaded; override;
    function CanModify(NewIndex: integer): boolean; virtual;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure FlipChildren(AllLevels: boolean); override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    property Buttons[Index: Integer]: TsRadioButton read GetButtons;
  published
    property AnimatEvents: TacAnimatEvents read FAnimatEvents write FAnimatEvents default [aeGlobalDef];
{$ENDIF} // NOTFORHELP
    property Columns:        integer index 10 read FColumns        write SetInteger default 1;
    property ItemIndex:      integer index 11 read FItemIndex      write SetInteger default -1;
    property ContentVOffset: integer index 12 read FContentVOffset write SetInteger default 0;
    property Items: {$IFDEF TNTUNICODE}TTntStrings{$ELSE}TStrings{$ENDIF} read FItems write SetItems;
    property ShowFocus: boolean read FShowFocus write FShowFocus default True;
    property DisableItemsIfUnchecked: boolean read FDisableItemsIfUnchecked write SetDisableItemsIfUnchecked default False;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChanging: TacIndexChangingEvent read FOnChanging write FOnChanging;
  end;


implementation

uses
  math,
  {$IFDEF TNTUNICODE}TntWindows, {$ENDIF}
  {$IFDEF LOGGED}sDebugMsgs, {$ENDIF}
  {$IFDEF DELPHI7UP}Themes, {$ENDIF}
  acntUtils, sStyleSimply, sMessages, sVCLUtils, sGraphUtils, sSkinProps, sSkinManager, sAlphaGraph,
  acntTypes, sDefaults, sMaskData, acThdTimer, sThirdParty, acAlphaImageList;


const
  xOffset = 6;


function UpdateCheckBox_CB(Data: TObject; iIteration: integer): boolean;
var
  sd: TsCommonData;
  gb: TsGroupBox;
  cRect: TRect;
  Bmp: TBitmap;
  Alpha: byte;
  DC: HDC;
begin
  Result := False;
  if Data is TsCommonData then begin
    sd := TsCommonData(Data);
    if sd.FOwnerControl is TsGroupBox then begin
      gb := TsGroupBox(sd.FOwnerControl);
      cRect := SumRects(gb.CaptionRect, gb.CheckBoxRect);
      Bmp := CreateBmp32(cRect);
      BitBlt(Bmp.Canvas.Handle, 0, 0, Bmp.Width, Bmp.Height, sd.AnimTimer.BmpFrom.Canvas.Handle, 0, 0, SRCCOPY);

      with sd.AnimTimer do
        case State of
          0, 2: Glow := Glow - GlowStep;
          1:    Glow := Glow + GlowStep
          else  Glow := MaxByte - (Iteration / Iterations) * MaxByte;
        end;

      Alpha := LimitIt(Round(sd.AnimTimer.Glow), 0, MaxByte);
      SumBmpRect(Bmp, sd.FCacheBmp, iff(sd.AnimTimer.State = 1, MaxByte - Alpha, Alpha), cRect, MkPoint);
      DC := GetDC(gb.Handle);
      try
        BitBlt(DC, cRect.Left, cRect.Top, WidthOf(cRect), HeightOf(cRect), Bmp.Canvas.Handle, 0, 0, SRCCOPY);
      finally
        ReleaseDC(gb.Handle, DC);
      end;
      Bmp.Free;

      with sd.AnimTimer do
        if sd.AnimTimer.Iteration >= sd.AnimTimer.Iterations then begin
          if (State = 0) and (Alpha > 0) then begin
            Iteration := Iteration - 1;
            UpdateCheckBox_CB(Data, iIteration);
            Exit;
          end;
          if State = 0 then
            StopTimer(sd);
        end
        else
          Result := True;
    end;
  end;
end;


procedure TsGroupBox.AdjustClientRect(var Rect: TRect);
begin
  if not (csDestroying in ComponentState) then begin
    inherited AdjustClientRect(Rect);
    inc(Rect.Top, CaptionYOffset);
  end;
end;


procedure TsGroupBox.AfterConstruction;
begin
  inherited;
  FCommonData.Loaded;
end;


procedure TsGroupBox.CheckBoxChanged;
begin
  if Assigned(OnCheckBoxChanged) then
    OnCheckBoxChanged(Self);
end;


function TsGroupBox.CheckBoxHeight: integer;
var
  GlyphNdx: integer;
begin
  if SkinData.Skinned then begin
    GlyphNdx := CheckBoxIndex;
    if SkinData.SkinManager.IsValidImgIndex(GlyphNdx) then
      Result := FCommonData.SkinManager.ma[GlyphNdx].Height
    else
      Result := 0;
  end
  else
{$IFDEF DELPHI7UP}
    if acThemesEnabled then
      Result := 13//6
    else
{$ENDIF}    
      Result := 13;
end;


function TsGroupBox.CheckBoxIndex: integer;
const
  GlyphStates: array [boolean] of TCheckBoxState = (cbUnchecked, cbChecked);
begin
  Result := FCommonData.SkinManager.ConstData.CheckBox[GlyphStates[Checked]];
end;


function TsGroupBox.CheckBoxWidth: integer;
var
  GlyphNdx: integer;
begin
  if SkinData.Skinned then begin
    GlyphNdx := CheckBoxIndex;
    if SkinData.SkinManager.IsValidImgIndex(GlyphNdx) then
      Result := FCommonData.SkinManager.ma[GlyphNdx].Width
    else
      Result := 0;
  end
  else
{$IFDEF DELPHI7UP}
    if acThemesEnabled then
      Result := 16
    else
{$ENDIF}    
      Result := 13;
end;


constructor TsGroupBox.Create(AOwner: TComponent);
begin
  FCommonData := TsCommonData.Create(Self, True);
  FCommonData.COC := COC_TsGroupBox;
  inherited Create(AOwner);
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageListChange;
  FCaptionLayout := clTopLeft;
  FCaptionYOffset := 0;
  FCaptionMargin := TsMargin.Create(Self);
  FCaptionWidth := 0;
  FCheckBoxVisible := False;
  FImageIndex := -1;
  FChecked := False;
  CheckPressed := False;
  CheckHot := False;
end;


destructor TsGroupBox.Destroy;
begin
  FreeAndNil(FCommonData);
  FreeAndNil(FImageChangeLink);
  FCaptionMargin.Free;
  inherited Destroy;
end;


function TsGroupBox.ImageExists: boolean;
begin
  Result := (Images <> nil) and IsValidIndex(FImageIndex, Images.Count);
end;


procedure TsGroupBox.ImageListChange(Sender: TObject);
begin
  FCommonData.Invalidate;
end;


procedure TsGroupBox.InitPaintData;
var
  iImgWidth,
  iImgHeight,
  iTextWidth,
  iCheckWidth,
  iCheckHeight,
  iContentWidth: integer;
begin
  InitCacheBmp(SkinData);

  iImgHeight := GetImageHeight(Images);
  iImgWidth := GetImageWidth(Images);

  CaptionHeight := TextHeight + FCaptionYOffset + 2;
  if ImageExists then
    CaptionHeight := max(CaptionHeight, iImgHeight);

  CaptionRect.Top := FCaptionMargin.Top;
  CaptionRect.Bottom := CaptionRect.Top + CaptionHeight;

  if FCaptionYOffset < 0 then
    inc(CaptionRect.Top, FCaptionYOffset);

  iTextWidth := acTextWidth(FCommonData.FCacheBmp.Canvas, Caption);
  if CaptionWidth = 0 then
    if ImageExists then
      iContentWidth := iTextWidth + iImgWidth + acSpacing
    else
      iContentWidth := iTextWidth
  else
    iContentWidth := CaptionWidth;

  if FCheckBoxVisible then begin
    iCheckWidth := CheckBoxWidth;
    iCheckHeight := CheckBoxHeight;
    inc(iContentWidth, iCheckWidth + acSpacing);
    CaptionHeight := max(CaptionHeight, iCheckHeight);
  end
  else begin
    iCheckWidth := 0;
    iCheckHeight := 0;
  end;

  if FCaptionLayout = clTopCenter then
    CaptionRect.Left := (Width - iContentWidth) div 2
  else
    if (FCaptionLayout = clTopLeft) and not UseRightToLeftAlignment or (FCaptionLayout = clTopRight) and UseRightToLeftAlignment then
      CaptionRect.Left := xOffset + FCaptionMargin.Left
    else
      CaptionRect.Left := Width - iContentWidth - FCaptionMargin.Right - xOffset;

  if CaptionRect.Left < 0 then
    CaptionRect.Left := 0;

  CaptionRect.Right := CaptionRect.Left + iContentWidth;

  if CaptionRect.Right >= Width then
    CaptionRect.Right := Width - 1;

  if iContentWidth < 2 * BevelWidth then begin
    CaptionRect.Left := CaptionRect.Left - BevelWidth;
    CaptionRect.Right := CaptionRect.Right + BevelWidth;
  end;

  if BiDiMode = bdLeftToRight then begin
    if CheckBoxVisible then begin
      CheckBoxRect.Left := CaptionRect.Left;
      CheckBoxRect.Right := CheckBoxRect.Left + iCheckWidth;
      if ImageExists then begin
        GlyphRect.Left := CheckBoxRect.Right + acSpacing;
        GlyphRect.Right := GlyphRect.Left + iImgWidth
      end
      else begin
        GlyphRect.Left := CheckBoxRect.Right;
        GlyphRect.Right := GlyphRect.Left;
      end;
      TextRect.Left := GlyphRect.Right + acSpacing;
    end
    else begin
      CheckBoxRect := Rect(CaptionRect.Left, 0, CaptionRect.Left, 0);
      GlyphRect.Left := CheckBoxRect.Right;
      if ImageExists then begin
        GlyphRect.Right := GlyphRect.Left + iImgWidth;
        TextRect.Left := GlyphRect.Right + acSpacing;
      end
      else begin
        GlyphRect.Right := GlyphRect.Left;
        TextRect.Left := GlyphRect.Right;
      end;
    end;
    TextRect.Right := TextRect.Left + iTextWidth;
  end
  else begin
    if CheckBoxVisible then begin
      CheckBoxRect.Right := CaptionRect.Right;
      CheckBoxRect.Left := CheckBoxRect.Right - iCheckWidth;
      if ImageExists then begin
        GlyphRect.Right := CheckBoxRect.Left - acSpacing;
        GlyphRect.Left := GlyphRect.Right - iImgWidth;
      end
      else begin
        GlyphRect.Right := CheckBoxRect.Left;
        GlyphRect.Left := GlyphRect.Right;
      end;
      TextRect.Right := GlyphRect.Left - acSpacing;
    end
    else begin
      CheckBoxRect := Rect(CaptionRect.Right, 0, CaptionRect.Right, 0);
      GlyphRect.Right := CheckBoxRect.Left;
      if ImageExists then
        GlyphRect.Left := GlyphRect.Right - iImgWidth
      else
        GlyphRect.Left := CheckBoxRect.Right;

      TextRect.Right := GlyphRect.Left;
    end;
    TextRect.Left := TextRect.Right - iTextWidth;
  end;

  if CheckBoxVisible then begin
    CheckBoxRect.Top := CaptionRect.Top + (CaptionHeight - iCheckHeight) div 2;
    CheckBoxRect.Bottom := CheckBoxRect.Top + iCheckHeight;
  end;
  if ImageExists then begin
    GlyphRect.Top := CaptionRect.Top + (CaptionHeight - iImgHeight) div 2;
    GlyphRect.Bottom := CaptionRect.Top + iCheckHeight;
  end
  else
    if not CheckBoxVisible then
      OffsetRect(TextRect, (iContentWidth - iTextWidth) div 2, 0);

  TextRect.Top := CaptionRect.Top + (CaptionHeight - TextHeight) div 2;
  TextRect.Bottom := TextRect.Top + TextHeight;

  TopRect := Rect(0, 0, Width, max(0, CaptionHeight div 2 + FCaptionMargin.Top));
end;


procedure TsGroupBox.Loaded;
begin
  inherited;
  FCommonData.Loaded;
end;


function TsGroupBox.MouseInCheckBox: boolean;
begin
  Result := not (csDesigning in ComponentState) and (PtInRect(CheckBoxRect, ScreenToClient(acMousePos)) or PtInRect(CaptionRect, ScreenToClient(acMousePos)));
end;


procedure TsGroupBox.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = Images) then
    Images := nil;
end;


procedure TsGroupBox.Paint;
begin
  PaintToDC(Canvas.Handle)
end;


procedure TsGroupBox.PaintCaptionArea(cRect: TRect; CI: TCacheInfo; AState: integer);
var
  R: TRect;
  C: TColor;

  function PaintCaptionBG: integer;
  var
    R: TRect;
    Bmp: TBitmap;
    cOffset: TPoint;
    LocalCI: TCacheInfo;
  begin
    if (Caption <> '') or ImageExists or CheckBoxVisible then begin
      if FCaptionSkin = '' then begin
        LocalCI := CI;
        Result := FCommonData.SkinManager.ConstData.Sections[ssTransparent];
        cOffset := Point(Left, Top);
      end
      else begin
        LocalCI := MakeCacheInfo(FCommonData.FCacheBmp);
        Result := FCommonData.SkinManager.GetSkinIndex(FCaptionSkin);
        cOffset := MkPoint;
      end;
      R.Left   := CaptionRect.Left   - CaptionMargin.Left;
      R.Top    := CaptionRect.Top    - CaptionMargin.Top;
      R.Right  := CaptionRect.Right  + CaptionMargin.Right;
      R.Bottom := CaptionRect.Bottom + CaptionMargin.Bottom + 1;

      Bmp := CreateBmp32(R);
      Bmp.Canvas.Font.Assign(Font);
      PaintItem(Result, LocalCI, True, integer(Focused or CheckHot), MkRect(Bmp), Point(cOffset.X + R.Left, cOffset.Y + R.Top), Bmp, FCommonData.SkinManager);
      BitBlt(FCommonData.FCacheBMP.Canvas.Handle, R.Left, R.Top, Bmp.Width, Bmp.Height, BMP.Canvas.Handle, 0, 0, SRCCOPY);
      Bmp.Free;

      if NeedParentFont(SkinData.SkinManager, Result, integer(CheckHot)) then
        Result := GetFontIndex(Parent, Result, SkinData.SkinManager, integer(CheckHot))
      else
        Result := SkinData.SkinIndex;
    end
    else
      Result := -1;
  end;

begin
  TextIndex := PaintCaptionBG;

  if CheckBoxVisible then
    PaintCheckBoxSkin(FCommonData.FCacheBMP, CheckBoxRect, AState);

  if TextIndex >= 0 then begin
    C := SkinData.SkinManager.gd[TextIndex].Props[0].Color;
    FCommonData.FCacheBMP.Canvas.Font.Color := SkinData.SkinManager.gd[TextIndex].Props[0].FontColor.Color;
  end
  else
    C := 0;

  if ImageExists then
    if (Images is TacImageList) and (SkinData.SkinManager <> nil) and SkinData.SkinManager.Effects.DiscoloredGlyphs then
      DrawAlphaImgList(Images, FCommonData.FCacheBMP, GlyphRect.Left, GlyphRect.Top, ImageIndex, 0, C, 0, 1, False)
    else
      Images.Draw(FCommonData.FCacheBMP.Canvas, GlyphRect.Left, GlyphRect.Top, ImageIndex, True);

  if Caption <> '' then
    WriteText(TextRect);

  if not Enabled then
    if CI.Ready then begin
      R := cRect;
      OffsetRect(R, CI.X + Left, CI.Y + Top);
      BlendTransRectangle(FCommonData.FCacheBMP, cRect.Left, cRect.Top, CI.Bmp, R, DefBlendDisabled);
    end
    else
      FadeBmp(FCommonData.FCacheBMP, cRect, DefBlendDisabled, TsColor(CI.FillColor), 0, 0);
end;


procedure TsGroupBox.PaintCheckBoxSkin(Bmp: TBitmap; R: TRect; State: integer);
var
  GlyphNdx: integer;
begin
  GlyphNdx := CheckBoxIndex;
  if SkinData.SkinManager.IsValidImgIndex(GlyphNdx) then
    DrawSkinGlyph(Bmp, R.TopLeft, State, 1, FCommonData.SkinManager.ma[GlyphNdx], MakeCacheInfo(Bmp))
end;


procedure TsGroupBox.PaintCheckBoxStd(DC: hdc; R: TRect; State: integer);
const
  CheckStates: array [boolean] of Cardinal = (DFCS_ADJUSTRECT, DFCS_CHECKED);
{$IFDEF DELPHI7UP}
  ThemedChecks: array [boolean, 0..3] of TThemedButton = ((tbCheckBoxUncheckedNormal, tbCheckBoxUncheckedHot, tbCheckBoxUncheckedPressed, tbCheckBoxUncheckedDisabled),
                                                          (tbCheckBoxCheckedNormal,   tbCheckBoxCheckedHot,   tbCheckBoxCheckedPressed, tbCheckBoxCheckedDisabled));
{$ENDIF}
begin
{$IFDEF DELPHI7UP}
  if acThemesEnabled then
    acThemeServices.DrawElement(DC, acThemeServices.GetElementDetails(ThemedChecks[Checked, iff(Enabled, State, 3)]), R)
  else
{$ENDIF}
    DrawFrameControl(DC, R, DFC_BUTTON, CheckStates[Checked]);
end;


procedure TsGroupBox.PaintToDC(DC: hdc);
var
  R: TRect;
  b: boolean;
  i: integer;

{$IFDEF DELPHI7UP}
  procedure PaintThemedGroupBox;
  var
    OuterRect: TRect;
    Box: TThemedButton;
    Details: TThemedElementDetails;
  begin
    with Canvas do begin
      OuterRect := ClientRect;
      OuterRect.Top := CaptionMargin.Top + CaptionHeight div 2;

      if Enabled then
        Box := tbGroupBoxNormal
      else
        Box := tbGroupBoxDisabled;

      Details := acThemeServices.GetElementDetails(Box);
      acThemeServices.DrawElement(Handle, Details, OuterRect);
      FillDC(DC, Rect(CaptionRect.Left - CaptionMargin.Left,
                                 CaptionRect.Top - CaptionMargin.Top,
                                 CaptionRect.Right + CaptionMargin.Right,
                                 CaptionRect.Bottom + CaptionMargin.Bottom), TacAccessControl(Parent).Color);
      SelectClipRgn(Handle, 0);
      if Text <> '' then begin
        SelectObject(DC, Font.Handle);
        SetTextColor(DC, ColorToRGB(Font.Color));
        SetBkMode(DC, TRANSPARENT);
        acDrawText(DC, Text, Self.TextRect, DrawTextBiDiModeFlags(DT_SINGLELINE or DT_CENTER));
      end;
    end;
  end;
{$ENDIF}

  procedure PaintGroupBox;
  var
    R: TRect;
    Flags: Cardinal;
  begin
    with Canvas do begin
      R := Rect(0, CaptionMargin.Top + CaptionHeight div 2 - 1, Width, Height);
      if Ctl3D then begin
        Inc(R.Left);
        Inc(R.Top);
        Brush.Color := clBtnHighlight;
        FrameRect(R);
        OffsetRect(R, -1, -1);
        Brush.Color := clBtnShadow;
      end
      else
        Brush.Color := clWindowFrame;

      FrameRect(R);

      FillDC(Canvas.Handle, Rect(CaptionRect.Left - CaptionMargin.Left,
                                 CaptionRect.Top - CaptionMargin.Top,
                                 CaptionRect.Right + CaptionMargin.Right,
                                 CaptionRect.Bottom + CaptionMargin.Bottom), TacAccessControl(Parent).Color);

      if Caption <> '' then begin
        Flags := DrawTextBiDiModeFlags(DT_SINGLELINE);
        Brush.Color := Color;
        acDrawText(Handle, PacChar(Caption), Self.TextRect, Flags);
      end;
    end;
  end;

begin
  if FCommonData.Skinned(True) then begin
    if not (csDestroying in ComponentState) and (Visible or (csDesigning in ComponentState)) then
      if not InAnimationProcess or (DC = SkinData.PrintDC) then
        if not InUpdating(FCommonData) then begin
          i := GetClipBox(DC, R);
          if i = ERROR {or IsRectEmpty(R) is not redrawn while resizing }then
            Exit;

          if i = NULLREGION then begin
            b := False;
            for I := 0 to ControlCount - 1 do
              if Controls[i].Align = alNone then begin
                b := True;
                Break;
              end;

            if b and RectVisible(DC, MkRect(Self)) then begin
              SkinData.FUpdating := True;
              Exit;
            end;
          end;
          // If transparent and form resizing processed
          b := FCommonData.HalfVisible or FCommonData.BGChanged;
          if SkinData.RepaintIfMoved and not (csPaintCopy in ControlState) then
            FCommonData.HalfVisible := (WidthOf(R, True) <> Width) or (HeightOf(R, True) <> Height)
          else
            FCommonData.HalfVisible := False;

          if b then
            PrepareCache;

          CopyWinControlCache(Self, FCommonData, MkRect, MkRect(Self), DC, False);
          sVCLUtils.PaintControls(DC, Self, b, MkPoint);
          SetParentUpdated(Self);
        end;
  end
  else begin
    Canvas.Font.Assign(Self.Font);
    InitPaintData;
{$IFDEF DELPHI7UP}
    if acThemesEnabled then
      PaintThemedGroupBox
    else
{$ENDIF}
      PaintGroupBox;

    if CheckBoxVisible then
      PaintCheckBoxStd(DC, CheckBoxRect, integer(CheckHot));

    if ImageExists then
      Images.Draw(Canvas, GlyphRect.Left, GlyphRect.Top, ImageIndex, True);
  end
end;


function TsGroupBox.PrepareCache: boolean;
var
  CI: TCacheInfo;
  BG: TacBGInfo;
begin
  GetBGInfo(@BG, Parent);
  if BG.BgType = btNotReady then begin
    Result := False;
    SkinData.Updating := True;
  end
  else begin
    InitPaintData;
    CI := BGInfoToCI(@BG);
    // Caption BG painting
    if CI.Ready then
      BitBlt(FCommonData.FCacheBmp.Canvas.Handle, TopRect.Left, TopRect.Top, Width, TopRect.Bottom, CI.Bmp.Canvas.Handle, Left + CI.X, Top + CI.Y, SRCCOPY)
    else
      if Parent <> nil then
        FillDC(FCommonData.FCacheBmp.Canvas.Handle, TopRect, CI.FillColor);

    PaintItem(FCommonData, CI, False, 0, Rect(0, TopRect.Bottom, Width, Height), Point(Left, Top + TopRect.Bottom), FCommonData.FCacheBMP, True);
    PaintCaptionArea(CaptionRect, CI, 0);
    SkinData.PaintOuterEffects(Self, MkPoint);
    FCommonData.BGChanged := False;
    Result := True;
  end;
end;


procedure TsGroupBox.RepaintCheckBox(State: integer; DoAnimation: boolean = False);
var
  DC: hdc;
  cRect: TRect;
  i: integer;
begin
  if SkinData.Skinned then begin
    cRect := CaptionRect;
    cRect.Bottom := CaptionRect.Bottom + 1;
    if DoAnimation and SkinData.SkinManager.Effects.AllowAnimation and (State <> 2) then begin
      i := GetNewTimer(SkinData.AnimTimer, SkinData.FOwnerControl, State);
      if (SkinData.AnimTimer.State >= 0) and (State = SkinData.AnimTimer.State) then // Started already
        Exit;

      if SkinData.AnimTimer.BmpFrom <> nil then
        FreeAndNil(SkinData.AnimTimer.BmpFrom);

      SkinData.AnimTimer.BmpFrom := CreateBmp32(cRect);
      BitBlt(SkinData.AnimTimer.BmpFrom.Canvas.Handle, 0, 0, SkinData.AnimTimer.BmpFrom.Width, SkinData.AnimTimer.BmpFrom.Height, SkinData.FCacheBmp.Canvas.Handle, cRect.Left, cRect.Top, SRCCOPY);
      PaintCaptionArea(CaptionRect, GetParentCache(SkinData), State);
      SkinData.AnimTimer.InitData(SkinData, i, UpdateCheckBox_CB, State);
    end
    else begin
      if SkinData.AnimTimer <> nil then
        FreeAndNil(SkinData.AnimTimer);

      DC := GetDC(Handle);
      try
        PaintCaptionArea(cRect, GetParentCache(SkinData), State);
        BitBlt(DC, cRect.Left, cRect.Top, WidthOf(cRect), HeightOf(cRect), SkinData.FCacheBmp.Canvas.Handle, cRect.Left, cRect.Top, SRCCOPY);
      finally
        ReleaseDC(Handle, DC);
      end;
    end
  end
  else
    InvalidateRect(Handle, @CaptionRect, False);
end;


procedure TsGroupBox.SetBoolean(const Index: Integer; const Value: boolean);
begin
  case Index of
    0: if FCheckBoxVisible <> Value then begin
      FCheckBoxVisible := Value;
      FCommonData.Invalidate;
    end;

    1: if FChecked <> Value then begin
      FChecked := Value;
      if not (csLoading in ComponentState) then begin
        if not CheckPressed then
          FCommonData.Invalidate;

        CheckBoxChanged;
      end;
    end;
  end;
end;


procedure TsGroupBox.SetCaptionLayout(const Value: TsCaptionLayout);
begin
  if FCaptionLayout <> Value then begin
    FCaptionLayout := Value;
    if Caption <> '' then
      SkinData.Invalidate;
  end;
end;


procedure TsGroupBox.SetCaptionSkin(const Value: TsSkinSection);
begin
  if FCaptionSkin <> Value then begin
    FCaptionSkin := Value;
    FCommonData.Invalidate
  end;
end;


procedure TsGroupBox.SetCaptionWidth(const Value: TacIntProperty);
begin
  if FCaptionWidth <> Value then begin
    FCaptionWidth := Value;
    FCommonData.Invalidate
  end;
end;


procedure TsGroupBox.SetImages(const Value: TCustomImageList);
begin
  if Images <> Value then begin
    if Images <> nil then
      Images.UnRegisterChanges(FImageChangeLink);

    FImages := Value;
    if Images <> nil then begin
      Images.RegisterChanges(FImageChangeLink);
      Images.FreeNotification(Self);
    end;
    FCommonData.Invalidate;
  end;
end;


procedure TsGroupBox.SetInteger(const Index, Value: integer);
begin
  case Index of
    0: if FCaptionYOffset <> Value then begin
      FCaptionYOffset := Value;
      SkinData.Invalidate;
    end;

    1: if FImageIndex <> Value then begin
      FImageIndex := Value;
      SkinData.Invalidate;
    end;
  end;
end;


function TsGroupBox.TextHeight: integer;
begin
  Result := Maxi(4, FCommonData.FCacheBmp.Canvas.TextHeight('W')) + 2;
end;


procedure TsGroupBox.WMFontChanged(var Message: TMessage);
begin
  inherited;
  FCommonData.FCacheBmp.Canvas.Font.Assign(Font);
  if Caption <> '' then
    FCommonData.Invalidate;
end;


procedure TsGroupBox.CMEnabledChanged(var Message: TMessage); 
begin
  inherited;
  FCommonData.Invalidate;
end;


procedure TsGroupBox.WndProc(var Message: TMessage);
var
  PS: TPaintStruct;
  DC: hdc;
begin
{$IFDEF LOGGED}
  AddToLog(Message, Name);
{$ENDIF}
  case Message.Msg of
    SM_ALPHACMD:
      case Message.WParamHi of
        AC_CTRLHANDLED: begin
          Message.Result := 1;
          Exit
        end;

        AC_SETSCALE:
          if SkinData.SkinManager <> nil then begin
            CaptionWidth := MulDiv(CaptionWidth, Message.LParam, SkinData.ScalePercent);
            CommonMessage(Message, SkinData);
            Exit;
          end;

        AC_SETNEWSKIN: begin
          AlphaBroadCast(Self, Message);
          if ACUInt(Message.LParam) = ACUInt(SkinData.SkinManager) then
            CommonWndProc(Message, FCommonData);

          Exit;
        end;

        AC_GETBG:
          if CaptionSkin <> '' then
            with PacBGInfo(Message.LParam)^ do begin // If BG of groupbox used
              Bmp := SkinData.FCacheBmp;
              Offset := MkPoint;
              BgType := btCache;
              Exit;
            end;

        AC_PREPARECACHE: begin
          if SkinData.Skinned and not InUpdating(SkinData) and not PrepareCache then
            SkinData.FUpdating := True;

          Exit;
        end;

        AC_GETOUTRGN: begin
          PRect(Message.LParam)^ := MkRect(Width, Height);
          if FCaptionYOffset < 0 then
            PRect(Message.LParam)^.Top := 0
          else begin
            InitPaintData;
            PRect(Message.LParam)^.Top := CaptionMargin.Top + CaptionHeight div 2;
          end;

          OffsetRect(PRect(Message.LParam)^, Left, Top);
          Exit;
        end;

        AC_MOUSELEAVE:
          if CheckBoxVisible then
            if CheckHot then begin
              CheckHot := False;
              RepaintCheckBox(0);
            end;

        AC_REFRESH, AC_REMOVESKIN: begin
          if ACUInt(Message.LParam) = ACUInt(SkinData.SkinManager) then begin
            CommonWndProc(Message, FCommonData);
            AlphaBroadCast(Self, Message);
            Repaint;
          end
          else
            AlphaBroadCast(Self, Message);

          Exit;
        end;

        AC_GETDEFINDEX: begin
          if FCommonData.SkinManager <> nil then
            Message.Result := FCommonData.SkinManager.ConstData.Sections[ssGroupBox] + 1;

          Exit;
        end;
      end;

    CM_MOUSELEAVE, WM_MOUSELEAVE:
      if CheckBoxVisible then
        if CheckHot then begin
          CheckHot := False;
          RepaintCheckBox(0, True);
        end;

    WM_MOUSEMOVE:
      if CheckBoxVisible then
        if MouseInCheckBox then begin
          if not CheckHot then begin
            CheckHot := True;
            RepaintCheckBox(1, True);
          end;
        end
        else
          if CheckHot then begin
            CheckHot := False;
            RepaintCheckBox(0, True);
          end;

    WM_LBUTTONDBLCLK, WM_LBUTTONDOWN:
      if CheckBoxVisible then
        if MouseInCheckBox then begin
          CheckPressed := True;
          RepaintCheckBox(2);
        end;

    WM_LBUTTONUP:
      if CheckBoxVisible then
        if CheckPressed then begin
          if MouseInCheckBox then
            Checked := not Checked;

          CheckPressed := False;
          RepaintCheckBox(1);
        end;
  end;

  if not ControlIsReady(Self) or not FCommonData.Skinned then
    inherited
  else begin
    if Message.Msg = SM_ALPHACMD then
      case Message.WParamHi of
        AC_ENDPARENTUPDATE: begin
          if FCommonData.FUpdating then begin
            if Showing and not InUpdating(FCommonData, True) then begin
              RedrawWindow(Handle, nil, 0, RDWA_NOCHILDRENNOW);
              SetParentUpdated(Self);
            end;
          end;
          Exit;
        end;

        AC_PREPARECACHE: begin
          if not PrepareCache then
            SkinData.FUpdating := True;

          Exit;
        end

        else begin
          CommonMessage(Message, FCommonData);
          Exit;
        end;
      end
    else
      case Message.Msg of
        WM_PAINT: begin
          BeginPaint(Handle, PS);
          if Message.WParam = 0 then
            DC := GetDC(Handle)
          else
            DC := hdc(Message.WParam);

          try
            PaintToDC(DC);
          finally
            if Message.WParam = 0 then
              ReleaseDC(Handle, DC);
          end;
          EndPaint(Handle, PS);
          Exit;
        end;

        WM_PRINT: begin
          SkinData.Updating := False;
          PaintToDC(TWMPaint(Message).DC);
          Exit;
        end;

        CM_TEXTCHANGED: begin
          SkinData.Invalidate;
          Exit;
        end;

        WM_ERASEBKGND: begin
          if not (csPaintCopy in ControlState) and (Message.WParam <> WParam(Message.LParam) {PerformEraseBackground, TntSpeedButtons}) then begin
            Message.Result := 1;
            if csDesigning in ComponentState then
              inherited; // Drawing in the BDS IDE
          end
          else
            if Message.WParam <> 0 then // From PaintTo
              if not FCommonData.BGChanged then
                BitBlt(TWMPaint(Message).DC, 0, 0, Width, Height, FCommonData.FCacheBmp.Canvas.Handle, 0, 0, SRCCOPY);

          Exit;
        end;

        CM_VISIBLECHANGED: begin
          FCommonData.Updating := False;
          FCommonData.BGChanged := True;
        end;

        WM_WINDOWPOSCHANGED:
          FCommonData.BGChanged := True;

        WM_KILLFOCUS, WM_SETFOCUS: begin
          inherited;
          Exit
        end;

        WM_MOVE:
          FCommonData.BGChanged := FCommonData.BGChanged or FCommonData.RepaintIfMoved;
      end;

    if not CommonWndProc(Message, FCommonData) then begin
      inherited;
      case Message.Msg of
        WM_SIZE:
          if csDesigning in ComponentState then
            SendMessage(Handle, WM_PAINT, 0, 0);
      end
    end;
  end;
end;


procedure TsGroupBox.WriteText(R: TRect);
var
  Flags: Cardinal;
begin
  Flags := DT_SINGLELINE or DT_VCENTER or iff((CaptionWidth = 0) or (CaptionLayout = clTopCenter), DT_CENTER, DT_END_ELLIPSIS);
  if UseRightToLeftReading then
    Flags := Flags or DT_RTLREADING;

  FCommonData.FCacheBMP.Canvas.Font.Assign(Font);
  FCommonData.FCacheBMP.Canvas.Brush.Style := bsClear;
  if not SkinData.CustomFont then
    acWriteTextEx(FCommonData.FCacheBMP.Canvas, PacChar(Caption), True, TextRect, Flags, TextIndex, CheckHot, FCommonData.SkinManager)
  else
    acWriteText(FCommonData.FCacheBMP.Canvas, PacChar(Caption), True, TextRect, Flags);
end;


type
  TsGroupButton = class(TsRadioButton)
  private
    FInClick,
    FInModifying: boolean;
    procedure CNCommand(var Message: TWMCommand); message CN_COMMAND;
  protected
    function CanModify: boolean; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
  public
    constructor InternalCreate(RadioGroup: TsRadioGroup);
    destructor Destroy; override;
  end;


constructor TsGroupButton.InternalCreate(RadioGroup: TsRadioGroup);
begin
  inherited Create(RadioGroup);
  SkinData.SkinManager := RadioGroup.SkinData.SkinManager;
  AutoSize := False;
  ShowFocus := True;
  RadioGroup.FButtons.Add(Self);
  Visible := False;
  Enabled := RadioGroup.Enabled;
  ParentShowHint := False;
  OnClick := RadioGroup.ButtonClick;
  TabStop := False;
  SkinData.CustomFont := RadioGroup.SkinData.CustomFont;
  AnimatEvents := RadioGroup.AnimatEvents;
  Parent := RadioGroup;
  ControlStyle := ControlStyle + [csOpaque];
end;


destructor TsGroupButton.Destroy;
begin
  TsRadioGroup(Owner).FButtons.Remove(Self);
  inherited Destroy;
end;


procedure TsGroupButton.CNCommand(var Message: TWMCommand);
begin
  if not FInClick and not FInModifying and not (csLoading in ComponentState) then begin
    FInClick := True;
    try
      with TsRadioGroup(Parent) do
        if (Message.NotifyCode in [BN_CLICKED, BN_DOUBLECLICKED]) and CanModify(FButtons.IndexOf(Self)) then begin
          inherited;
          if Assigned(FOnChange) then
            FOnChange(Self.Parent);
        end;
    except
      Application.HandleException(Self);
    end;
    FInClick := False;
  end;
end;


procedure TsGroupButton.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  with TsRadioGroup(Parent) do begin
    KeyPress(Key);
    if ((Key = #8) or (Key = s_Space)) and not CanModify(FButtons.IndexOf(Self)) then
      Key := #0;
  end;
end;


procedure TsGroupButton.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  TsRadioGroup(Parent).KeyDown(Key, Shift);
end;


function TsGroupButton.CanModify: boolean;
begin
  FInModifying := True;
  Result := TsRadioGroup(Parent).CanModify(TsRadioGroup(Parent).FButtons.IndexOf(Self));
  FInModifying := False;
end;


type
  TAccessCommonData = class(TsCommonData);


procedure TsRadioGroup.ArrangeButtons;
var
  ALeft: Integer;
  FontSize: TSize;
  DeferHandle: THandle;
  ButtonsPerCol, ButtonWidth, ButtonHeight, TopMargin, I: Integer;
begin
  if (FButtons.Count <> 0) and not FReading and not (csLoading in ComponentState) then begin
    acGetTextExtent(0, s_Yy, FontSize, Font.Handle);

    ButtonsPerCol := (FButtons.Count + FColumns - 1) div FColumns;
    ButtonWidth := (Width - 10) div FColumns;
    I := Height - 5 - ContentVOffset;
    if (Caption <> '') or (SkinData.BorderIndex >= 0) then
      dec(I, FontSize.cy);

    ButtonHeight := I div ButtonsPerCol;
    TopMargin := ContentVOffset + 1 + (I mod ButtonsPerCol) div 2;
    if (Caption <> '') or (SkinData.BorderIndex >= 0) then
      inc(TopMargin, FontSize.cy);

    DeferHandle := BeginDeferWindowPos(FButtons.Count);
    try
      for I := 0 to FButtons.Count - 1 do
        with TsGroupButton(FButtons[I]) do begin
          TAccessCommonData(SkinData).FSkinManager := Self.SkinData.SkinManager;
          BiDiMode := Self.BiDiMode;
          ShowFocus := Self.ShowFocus;
          ALeft := (I div ButtonsPerCol) * ButtonWidth + 8;
          if UseRightToLeftAlignment then
            ALeft := Self.Width - ALeft - ButtonWidth;

          DeferHandle := DeferWindowPos(DeferHandle, Handle, 0, ALeft, (I mod ButtonsPerCol) * ButtonHeight + TopMargin,
                                        ButtonWidth, ButtonHeight, SWP_NOZORDER or SWP_NOACTIVATE);

          Visible := True;
        end;
    finally
      EndDeferWindowPos(DeferHandle);
    end;
  end;
end;


procedure TsRadioGroup.ButtonClick(Sender: TObject);
begin
  if not FUpdating then begin
    FItemIndex := FButtons.IndexOf(Sender);
    Changed;
    Click;
  end;
end;


function TsRadioGroup.CanModify(NewIndex: integer): boolean;
begin
  Result := True;
  if Assigned(FOnChanging) then
    FOnChanging(Self, NewIndex, Result);
end;


procedure EnableDisable(Ctrl: TControl; Data: integer);
begin
  Ctrl.Enabled := boolean(Data);
end;


procedure TsRadioGroup.CheckBoxChanged;
begin
  if FDisableItemsIfUnchecked then
    IterateControls(Self, integer(FChecked), EnableDisable);

  inherited;
end;


procedure TsRadioGroup.CMEnabledChanged(var Message: TMessage);
var
  I: Integer;
begin
  inherited;
  for I := 0 to FButtons.Count - 1 do begin
    TsGroupButton(FButtons[I]).Enabled := Enabled;
    TsGroupButton(FButtons[I]).SkinData.BGChanged := True;
  end;
end;


procedure TsRadioGroup.CMFontChanged(var Message: TMessage);
begin
  inherited;
  ArrangeButtons;
end;


constructor TsRadioGroup.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csSetCaption, csDoubleClicks{$IFDEF DELPHI7UP}, csParentBackground{$ENDIF}];
  FButtons := TList.Create;
{$IFDEF TNTUNICODE}
  FItems := TTntStringList.Create;
  TTntStringList(FItems).OnChange := ItemsChange;
{$ELSE}
  FItems := TStringList.Create;
  TStringList(FItems).OnChange := ItemsChange;
{$ENDIF}
  FItemIndex := -1;
  FColumns := 1;
  FContentVOffset := 0;
  FDisableItemsIfUnchecked := False;
  FAnimatEvents := [aeGlobalDef];
  FShowFocus := True;
end;


destructor TsRadioGroup.Destroy;
begin
  SetButtonCount(0);
{$IFDEF TNTUNICODE}
  TTntStringList(FItems).OnChange := Nil;
{$ELSE}
  TStringList(FItems).OnChange := Nil;
{$ENDIF}
  FreeAndNil(FItems);
  FButtons.Free;
  inherited Destroy;
end;


procedure TsRadioGroup.FlipChildren(AllLevels: Boolean);
begin
//
end;


function TsRadioGroup.GetButtons(Index: Integer): TsRadioButton;
begin
  Result := TsRadioButton(FButtons[Index]);
end;


procedure TsRadioGroup.GetChildren(Proc: TGetChildProc; Root: TComponent);
begin
//
end;


procedure TsRadioGroup.ItemsChange(Sender: TObject);
var
  i: integer;
begin
  if not FReading and not(csLoading in ComponentState) then begin
    if FItemIndex >= FItems.Count then
      FItemIndex := FItems.Count - 1;

    UpdateButtons;
  end;
  for i := 0 to FButtons.Count - 1 do
    TsRadioButton(FButtons[i]).Loaded;
end;


procedure TsRadioGroup.Loaded;
begin
  inherited Loaded;
  UpdateButtons;
  if IsValidIndex(FItemIndex, FButtons.Count) then begin
    FUpdating := True;
    TsGroupButton(FButtons[FItemIndex]).Checked := True;
    FUpdating := False;
  end;
end;


procedure TsRadioGroup.ReadState(Reader: TReader);
begin
  FReading := True;
  inherited ReadState(Reader);
  FReading := False;
  UpdateButtons;
end;


procedure TsRadioGroup.SetButtonCount(Value: Integer);
begin
  while FButtons.Count < Value do
    TsGroupButton.InternalCreate(Self);

  while FButtons.Count > Value do
    TsGroupButton(FButtons.Last).Free;
end;


procedure TsRadioGroup.SetDisableItemsIfUnchecked(const Value: boolean);
begin
  if FDisableItemsIfUnchecked <> Value then begin
    FDisableItemsIfUnchecked := Value;
    IterateControls(Self, integer(Checked or not Value), EnableDisable);
  end;
end;


procedure TsRadioGroup.SetInteger(const Index, Value: integer);
var
  i: integer;
begin
  case Index of
    10: begin
      if Value < 1 then
        i := 1
      else
        if Value > 16 then
          i := 16
        else
          i := Value;

      if FColumns <> i then begin
        FColumns := i;
        if ([csLoading] * ComponentState = []) then begin
          ArrangeButtons;
          Invalidate;
        end;
      end;
    end;

    11:
      if FReading then
        FItemIndex := Value
      else begin
        if Value < -1 then
          i := -1
        else
          if Value >= FButtons.Count then
            i := FButtons.Count - 1
          else
            i := Value;

        if FItemIndex <> i then begin
          if FItemIndex >= 0 then
            TsGroupButton(FButtons[FItemIndex]).Checked := False;

          FItemIndex := i;
          if FItemIndex >= 0 then
            TsGroupButton(FButtons[FItemIndex]).Checked := True;

          if Assigned(FOnChange) then
            FOnChange(Self);
        end;
      end;

    12: begin
      if FContentVOffset <> Value then begin
        FContentVOffset := Value;
        if ([csLoading] * ComponentState = []) then begin
          ArrangeButtons;
          Invalidate;
        end;
      end;
    end

    else
      inherited;
  end;
end;


procedure TsRadioGroup.SetItems(Value: {$IFDEF TNTUNICODE}TTntStrings{$ELSE}TStrings{$ENDIF});
begin
  FItems.Assign(Value);
end;


procedure TsRadioGroup.UpdateButtons;
var
  I: Integer;
begin
  if not FReading and not (csLoading in ComponentState) then begin
    SetButtonCount(FItems.Count);
    for I := 0 to FButtons.Count - 1 do begin
      TsGroupButton(FButtons[I]).Caption := FItems[I];
      TsGroupButton(FButtons[I]).Cursor := Cursor;
    end;
    ArrangeButtons;
    Invalidate;
  end;
end;


procedure TsRadioGroup.WMSize(var Message: TWMSize);
begin
  inherited;
  ArrangeButtons;
end;


procedure TsRadioGroup.WndProc(var Message: TMessage);
begin
  if Message.Msg = SM_ALPHACMD then
    case Message.WParamHi of
      AC_REFRESH:
        if ACUInt(Message.LParam) = ACUInt(SkinData.SkinManager) then
          ArrangeButtons;
    end;

  inherited;
end;


constructor TsMargin.Create(Control: TControl);
begin
  inherited Create;
  FControl := Control;
  FLeft   := 2;
  FRight  := 2;
  FTop    := 0;
  FBottom := 0;
end;


procedure TsMargin.SetMargin(const Index: Integer; Value: TacIntProperty);

  procedure ChangeProp(var Prop: TacIntProperty; Value: TacIntProperty);
  begin
    if Prop <> Value then begin
      Prop := Value;
      if not (csLoading in FControl.ComponentState) and (FControl is TsGroupBox) then
        TsGroupBox(FControl).SkinData.Invalidate;
    end;
  end;

begin
  case Index of
    0: ChangeProp(FLeft,   Value);
    1: ChangeProp(FTop,    Value);
    2: ChangeProp(FRight,  Value);
    3: ChangeProp(FBottom, Value);
  end;
end;

end.
