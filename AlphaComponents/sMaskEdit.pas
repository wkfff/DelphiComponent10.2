unit sMaskEdit;
{$I sDefs.inc}
//{$DEFINE LOGGED}

interface


uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, StdCtrls, Mask,
  {$IFNDEF DELPHI5} types, {$ENDIF}
  {$IFDEF LOGGED} sDebugMsgs, {$ENDIF}
  sCommonData, sConst, sDefaults, sGlyphUtils;

type
  TValidateErrorEvent = procedure(Sender: TObject; Text: string) of object;

{$IFDEF DELPHI_XE3}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]{$ENDIF}
  TsMaskEdit = class(TMaskEdit)
{$IFNDEF NOTFORHELP}
  private
    FAddedGlyphRect: TRect;
    FCheckOnExit: boolean;
    FBoundLabel: TsBoundLabel;
    FAddedGlyph: TacAddedGlyph;
    FCommonData: TsCtrlSkinData;
    FDisabledKind: TsDisabledKind;
    FOnValidateError: TValidateErrorEvent;
    procedure SetDisabledKind(const Value: TsDisabledKind);
    function FontStored: boolean;
    function ColorStored: boolean;
    procedure WMNCCalcSize(var Message: TWMNCCalcSize); message WM_NCCALCSIZE;
  protected
    function AddedGlyphVisible: boolean;
    function AddedGlyphSpace: integer;
    function AddedGlyphRect: TRect;
    procedure PaintAddedGlyph;
    procedure PaintAddedGlyphStd;
    function TextRect: TRect; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Change; override;
    procedure PaintBorder(DC: hdc); virtual;
    function PrepareCache: boolean; virtual;
    procedure PaintText; virtual;
    procedure OurPaintHandler(aDC: hdc = 0); virtual;
    function BorderWidth: integer;
  public
    procedure AfterConstruction; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ValidateEdit; override;
    procedure Loaded; override;
    procedure WndProc(var Message: TMessage); override;
  published
    property Align;
{$ENDIF} // NOTFORHELP
    property AddedGlyph: TacAddedGlyph read FAddedGlyph write FAddedGlyph;
    property CheckOnExit: boolean read FCheckOnExit write FCheckOnExit;
    property BoundLabel: TsBoundLabel read FBoundLabel write FBoundLabel;
    property DisabledKind: TsDisabledKind read FDisabledKind write SetDisabledKind default DefDisabledKind;
    property SkinData: TsCtrlSkinData read FCommonData write FCommonData;
{$IFDEF D2005}
    property OnMouseActivate;
{$ENDIF}
{$IFDEF D2009}
    property TextHint;
    property ParentDoubleBuffered;
    property OnMouseEnter;
    property OnMouseLeave;
{$ENDIF}
    property OnValidateError: TValidateErrorEvent read FOnValidateError write FOnValidateError;
{$IFDEF D2010}
    property Touch;
    property OnGesture;
{$ENDIF}
    property Color stored ColorStored;
    property Font stored FontStored;
    property ParentFont stored FontStored;
  end;


implementation

uses
  ExtCtrls,
  {$IFDEF DELPHI7UP} Themes, {$ENDIF}
  {$IFDEF TNTUNICODE} TntStdCtrls, {$ENDIF}
  sStyleSimply, sVCLUtils, sMessages, acntUtils, sGraphUtils, sAlphaGraph, sSkinProps, sEdit, sThirdParty, acAlphaImageList;

function TsMaskEdit.AddedGlyphRect: TRect;
var
  bWidth: integer;
begin
  if FAddedGlyphRect.Left < 0 then begin
    bWidth := EditBorderWidth({$IFDEF TNTUNICODE}TTntEdit{$ELSE}TEdit{$ENDIF}(Self));
    if IsRightToLeft then
      FAddedGlyphRect.Left := Width - bWidth - FAddedGlyph.Width
    else
      FAddedGlyphRect.Left := bWidth;

    FAddedGlyphRect.Right := FAddedGlyphRect.Left + FAddedGlyph.Width;
    FAddedGlyphRect.Top := bWidth;
    FAddedGlyphRect.Bottom := Height - bWidth;
  end;
  Result := FAddedGlyphRect;
end;


function TsMaskEdit.AddedGlyphSpace: integer;
begin
  Result := integer(AddedGlyphVisible) * (FAddedGlyph.Width + acSpacing div 2);
end;


function TsMaskEdit.AddedGlyphVisible: boolean;
begin
  Result := Assigned(AddedGlyph.Images) and IsValidIndex(AddedGlyph.ImageIndex, GetImageCount(AddedGlyph.Images));
end;


procedure TsMaskEdit.AfterConstruction;
begin
  inherited AfterConstruction;
  FCommonData.Loaded;
end;


constructor TsMaskEdit.Create(AOwner: TComponent);
begin
  FCommonData := TsCtrlSkinData.Create(Self, True);
  FCommonData.COC := COC_TsEdit;
  inherited Create(AOwner);
  FAddedGlyph := TacAddedGlyph.Create(Self);
  FAddedGlyphRect.Left := -1;
  ControlStyle := ControlStyle - [csSetCaption];
  FCheckOnExit := True;
  FDisabledKind := DefDisabledKind;
  FBoundLabel := TsBoundLabel.Create(Self, FCommonData);
end;


destructor TsMaskEdit.Destroy;
begin
  FreeAndNil(FBoundLabel);
  FreeAndNil(FCommonData);
  FreeAndNil(FAddedGlyph);
  inherited Destroy;
end;


procedure TsMaskEdit.Loaded;
begin
  inherited Loaded;
  FCommonData.Loaded;
end;


procedure TsMaskEdit.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (FAddedGlyph <> nil) and (AComponent = FAddedGlyph.Images) then
    FAddedGlyph.Images := nil;
end;


procedure TsMaskEdit.OurPaintHandler(aDC: hdc = 0);
var
  DC, SavedDC: hdc;
  PS: TPaintStruct;
begin
  if not InAnimationProcess or (aDC = 0) then
    BeginPaint(Handle, PS);

  SavedDC := 0;
  if aDC = 0 then begin
    DC := GetWindowDC(Handle);
    SavedDC := SaveDC(DC);
  end
  else
    DC := aDC;

  try
    if not InUpdating(FCommonData) then begin
      FCommonData.BGChanged := FCommonData.BGChanged or FCommonData.HalfVisible or GetBoolMsg(Parent, AC_GETHALFVISIBLE);
      FCommonData.HalfVisible := not RectInRect(Parent.ClientRect, BoundsRect);
      if FCommonData.BGChanged then
        if not PrepareCache then begin
          if aDC = 0 then begin
            RestoreDC(DC, SavedDC);
            ReleaseDC(Handle, DC);
          end;
          if not InAnimationProcess then
            EndPaint(Handle, PS);

          FCommonData.FUpdating := True;
          Exit;
        end;

      UpdateCorners(FCommonData, 0);
      BitBlt(DC, 0, 0, Width, Height, FCommonData.FCacheBmp.Canvas.Handle, 0, 0, SRCCOPY);
    end;
  finally
    if aDC = 0 then begin
      RestoreDC(DC, SavedDC);
      ReleaseDC(Handle, DC);
    end;
    if not InAnimationProcess or (aDC = 0) then
      EndPaint(Handle, PS);
  end;
end;


procedure TsMaskEdit.PaintAddedGlyph;
var
  DrawData: TacDrawGlyphData;
  h, w: integer;
  C: TColor;
  R: TRect;
begin
  R := AddedGlyphRect;
  DrawData.Glyph := nil;

  DrawData.Blend := AddedGlyph.Blend;
  DrawData.Images := AddedGlyph.Images;
  DrawData.NumGlyphs := 1;
  DrawData.ImageIndex := AddedGlyph.ImageIndex;
  if not IsValidIndex(DrawData.ImageIndex, GetImageCount(AddedGlyph.Images)) then
    DrawData.ImageIndex := AddedGlyph.ImageIndex;

  DrawData.CurrentState := 0;
  DrawData.Down := False;
  DrawData.Enabled := Enabled;
  DrawData.Reflected := False;
  DrawData.DstBmp := SkinData.FCacheBmp;
  DrawData.ImgRect := R;
  w := GetImageWIdth(AddedGlyph.Images, AddedGlyph.ImageIndex) div AddedGlyph.ImageCount;
  h := GetImageHeight(AddedGlyph.Images, AddedGlyph.ImageIndex);

  DrawData.ImgRect.Left   := R.Left + (WidthOf(R) - w) div 2;
  DrawData.ImgRect.Top    := R.Top + (HeightOf(R) - h) div 2;
  DrawData.ImgRect.Right  := DrawData.ImgRect.Left + w;
  DrawData.ImgRect.Bottom := DrawData.ImgRect.Top + h;

  DrawData.SkinManager := SkinData.SkinManager;
  DrawData.DisabledGlyphKind := DefDisabledGlyphKind;
  if SkinData.Skinned then begin
    DrawData.Canvas := SkinData.FCacheBmp.Canvas;
    DrawData.Grayed := (DrawData.CurrentState = 0) and (AddedGlyph.Grayed or SkinData.SkinManager.Effects.DiscoloredGlyphs);
    if DrawData.Grayed then
      DrawData.BGColor := SkinData.SkinManager.gd[SkinData.SkinIndex].Props[DrawData.CurrentState].Color
    else
      DrawData.BGColor := clNone;
  end
  else begin
    DrawData.Canvas := nil;
    DrawData.Grayed := AddedGlyph.Grayed;
    if DrawData.Grayed then
      DrawData.BGColor := Color
    else
      DrawData.BGColor := clNone;
  end;
  if not DrawData.Enabled then
    DrawData.Blend := DrawData.Blend + (100 - DrawData.Blend) div 2;

  C := clNone;
  if AddedGlyph.ColorTone <> clNone then
    if (DrawData.Images is TsCharImageList) or ((DrawData.Images is TsVirtualImageList) and (TsVirtualImageList(DrawData.Images).AlphaImageList is TsCharImageList)) then begin
      DrawData.Grayed := False;
      if DrawData.DstBmp <> nil then begin
        C := DrawData.DstBmp.Canvas.Font.Color;
        DrawData.DstBmp.Canvas.Font.Color := AddedGlyph.ColorTone;
        DrawData.DstBmp.Canvas.Font.Size := 0; // Def color is not allowed
      end;
    end
    else begin
      DrawData.Grayed := True;
      DrawData.BGColor := AddedGlyph.ColorTone;
    end;

  acDrawGlyphEx(DrawData);
  if C <> clNone then // Restore font color
    if DrawData.DstBmp <> nil then
      DrawData.DstBmp.Canvas.Font.Color := C;
end;


procedure TsMaskEdit.PaintAddedGlyphStd;
var
  DC: hdc;
  bWidth: integer;
begin
  if not SkinData.Skinned and AddedGlyphVisible then begin
    bWidth := EditBorderWidth({$IFDEF TNTUNICODE}TTntEdit{$ELSE}TEdit{$ENDIF}(Self));
    DC := GetWindowDC(Handle);
    try
      PrepareCache;
      if not SkinData.Skinned then begin
        ExcludeClipRect(DC, 0, 0, Width, bWidth);
        ExcludeClipRect(DC, 0, bWidth, bWidth, Height);
        ExcludeClipRect(DC, bWidth, Height - bWidth, Width, Height);
        ExcludeClipRect(DC, Width - bWidth, bWidth, Width, Height - bWidth);
      end;
      if IsRightToLeft then
        BitBlt(DC, AddedGlyphRect.Left - acSpacing div 2, AddedGlyphRect.Top, WidthOf(AddedGlyphRect) + acSpacing div 2, HeightOf(AddedGlyphRect), SkinData.FCacheBmp.Canvas.Handle,
                   AddedGlyphRect.Left - acSpacing div 2, AddedGlyphRect.Top, SRCCOPY)
      else
        BitBlt(DC, AddedGlyphRect.Left, AddedGlyphRect.Top, WidthOf(AddedGlyphRect) + acSpacing div 2, HeightOf(AddedGlyphRect), SkinData.FCacheBmp.Canvas.Handle,
                   AddedGlyphRect.Left, AddedGlyphRect.Top, SRCCOPY);
    finally
      ReleaseDC(Handle, DC);
    end;
  end;
end;


procedure TsMaskEdit.PaintBorder(DC: hdc);
const
  BordWidth = 2;
var
  NewDC, SavedDC: HDC;
begin
  if Assigned(Parent) and Visible and Parent.Visible and not (csCreating in ControlState) and (BorderStyle <> bsNone) then
    if not InUpdating(SkinData) then begin
      if DC = 0 then
        NewDC := GetWindowDC(Handle)
      else
        NewDC := DC;

      SavedDC := SaveDC(NewDC);
      try
        if FCommonData.BGChanged then
          PrepareCache;

        UpdateCorners(FCommonData, 0);
        BitBltBorder(NewDC, 0, 0, Width, Height, FCommonData.FCacheBmp.Canvas.Handle, 0, 0, BordWidth);
        if AddedGlyphVisible then
          if IsRightToLeft then
            BitBlt(DC, BordWidth, 0, AddedGlyphSpace, Height, SkinData.FCacheBmp.Canvas.Handle, BordWidth, 0, SRCCOPY)
          else
            BitBlt(DC, AddedGlyphRect.Left - acSpacing div 2, 0, AddedGlyphSpace + acSpacing div 2, Height, SkinData.FCacheBmp.Canvas.Handle, AddedGlyphRect.Left - acSpacing div 2, 0, SRCCOPY);
      finally
        RestoreDC(NewDC, SavedDC);
        if DC = 0 then
          ReleaseDC(Handle, NewDC);
      end;
    end;
end;


procedure TsMaskEdit.PaintText;
var
  R: TRect;
  aText: acString;
begin
  aText := EditText;
  if aText = '' then begin
{$IFDEF D2009}
    if TextHint <> '' then begin
      FCommonData.FCacheBMP.Canvas.Font.Assign(Font);
      R := TextRect;
      if FCommonData.Skinned then
        FCommonData.FCacheBMP.Canvas.Font.Color := BlendColors(ColorToRGB(Font.Color), ColorToRGB(Color), 167)
      else
        FCommonData.FCacheBMP.Canvas.Font.Color := clGrayText;

      acDrawText(FCommonData.FCacheBMP.Canvas.Handle, TextHint, R, DT_NOPREFIX or DT_TOP or DT_EXTERNALLEADING or GetStringFlags(Self, {$IFDEF D2009}Alignment{$ELSE}taLeftJustify{$ENDIF}));
    end
    else
{$ENDIF}
      Exit;
  end;
  FCommonData.FCacheBMP.Canvas.Font.Assign(Font);
  R := TextRect;
  if PasswordChar = #0 then
    acWriteTextEx(FCommonData.FCacheBMP.Canvas, PacChar(aText), True, R,
                  DT_NOPREFIX or DT_TOP or DT_EXTERNALLEADING or GetStringFlags(Self, {$IFDEF D2009}Alignment{$ELSE}taLeftJustify{$ENDIF}),
                  FCommonData, ControlIsActive(FCommonData))
  else begin
{$IFDEF WIDETEXT}
    if PasswordChar = '*' then
      acFillString(aText, Length(aText), acChar(#$25CF))
    else
{$ENDIF}    
      acFillString(aText, Length(aText), acChar(PasswordChar));

    acWriteTextEx(FCommonData.FCacheBMP.Canvas, PacChar(aText), True, R, DT_NOPREFIX or DT_TOP or GetStringFlags(Self, {$IFDEF D2009}Alignment{$ELSE}taLeftJustify{$ENDIF}), FCommonData, ControlIsActive(FCommonData));
  end;
end;


{$IFDEF DELPHI7UP}
const
  {$IFDEF DELPHI_XE2}
  EditStates: array[0..1] of TThemedEdit = (teEditBorderNoScrollNormal, teEditBorderNoScrollHot);
  {$ELSE}
  EditStates: array[0..1] of TThemedEdit = (teEditTextNormal, teEditTextHot);
  {$ENDIF}
{$ENDIF}


function TsMaskEdit.PrepareCache: boolean;
var
  BGInfo: TacBGInfo;
{$IFDEF DELPHI7UP}
  cEdit: TThemedEdit;
  Details: TThemedElementDetails;
{$ENDIF}
begin
  Result := True;
  BGInfo.BgType := btUnknown;
  GetBGInfo(@BGInfo, Parent);
  if BGInfo.BgType <> btNotReady then begin
    InitCacheBmp(SkinData);
    if SkinData.Skinned then begin
      if BorderStyle = bsSingle then
        PaintItem(FCommonData, BGInfoToCI(@BGInfo), True, integer(ControlIsActive(FCommonData)), MkRect(Self), Point(Left, top), FCommonData.FCacheBmp, False)
      else
        PaintItemBG(FCommonData, BGInfoToCI(@BGInfo), 0, MkRect(Self), Point(Left, top), FCommonData.FCacheBmp, 0, 0);

      PaintText;
      if not Enabled then
        BmpDisabledKind(FCommonData.FCacheBmp, FDisabledKind, Parent, BGInfoToCI(@BGInfo), Point(Left, Top));

      SkinData.BGChanged := False;
    end
    else begin
{$IFDEF DELPHI7UP}
      if acThemesEnabled then begin
        cEdit := EditStates[integer(ControlIsActive(SkinData))];
        Details := acThemeServices.GetElementDetails(cEdit);
        acThemeServices.DrawElement(FCommonData.FCacheBmp.Canvas.Handle, Details, MkRect(Self));
        if Color <> clWindow then
          FillDC(FCommonData.FCacheBmp.Canvas.Handle, MkRect(Self), Color);
      end
      else
{$ENDIF}
        FillDC(FCommonData.FCacheBmp.Canvas.Handle, MkRect(Self), Color);

      PaintText;
    end;
    if AddedGlyphVisible then
      PaintAddedGlyph;
  end
  else
    Result := False;
end;


procedure TsMaskEdit.SetDisabledKind(const Value: TsDisabledKind);
begin
  if FDisabledKind <> Value then begin
    FDisabledKind := Value;
    FCommonData.Invalidate;
  end;
end;


function TsMaskEdit.TextRect: TRect;
var
  bw, iSingle: integer;
begin
  if BorderStyle <> bsNone then
    bw := EditBorderWidth({$IFDEF TNTUNICODE}TTntEdit{$ELSE}TEdit{$ENDIF}(Self)) {$IFDEF DELPHI7UP} + integer(BevelKind <> bkNone) * (integer(BevelOuter <> bvNone) + integer(BevelInner <> bvNone)) {$ENDIF}
  else
    bw := 0;

  iSingle := integer(BorderStyle = bsSingle);
  if IsRightToLeft then
    Result := Rect(bw + iSingle, bw + iSingle, Width - bw - AddedGlyphSpace - iSingle, Height - bw)
  else
    Result := Rect(AddedGlyphSpace + bw + iSingle, bw + iSingle, Width - bw - iSingle, Height - bw);
end;


procedure TsMaskEdit.WMNCCalcSize(var Message: TWMNCCalcSize);
begin
  inherited;
  if AddedGlyphVisible then
    with Message.CalcSize_Params.rgrc[0] do
      if IsRightToLeft then
        dec(Right, AddedGlyphSpace)
      else
        inc(Left, AddedGlyphSpace);
end;


procedure TsMaskEdit.WndProc(var Message: TMessage);
var
  PS: TPaintStruct;
  DC, SavedDC: hdc;
  bw: integer;
begin
{$IFDEF LOGGED}
  AddToLog(Message);
{$ENDIF}
  case Message.Msg of
    SM_ALPHACMD:
      case Message.WParamHi of
        AC_SETSCALE: begin
          if BoundLabel <> nil then
            BoundLabel.UpdateScale(Message.LParam);

          Exit;
        end;

        AC_CTRLHANDLED: begin
          Message.Result := 1;
          Exit;
        end; // AlphaSkins supported

        AC_SETNEWSKIN:
          if ACUInt(Message.LParam) = ACUInt(SkinData.SkinManager) then begin
            CommonMessage(Message, FCommonData);
            Exit;
          end;

        AC_REFRESH:
          if ACUInt(Message.LParam) = ACUInt(SkinData.SkinManager) then begin
            CommonMessage(Message, FCommonData);
            if HandleAllocated then
              SendMessage(Handle, WM_NCPaint, 0, 0);

            Repaint;
            Exit;
          end;

        AC_REMOVESKIN:
          if ACUInt(Message.LParam) = ACUInt(SkinData.SkinManager) then begin
            CommonMessage(Message, FCommonData);
            Invalidate;
            Exit
          end;

        AC_ENDPARENTUPDATE:
          if FCommonData.Updating then begin
            FCommonData.Updating := False;
            InvalidateRect(Handle, nil, True);
            SendMessage(Handle, WM_NCPAINT, 0, 0);
            Exit;
          end;

        AC_GETDEFINDEX: begin
          if FCommonData.SkinManager <> nil then
            Message.Result := FCommonData.SkinManager.ConstData.Sections[ssEdit] + 1;

          Exit;
        end

        else
          if CommonMessage(Message, SkinData) then
            Exit;
      end;

    WM_SIZE, WM_WINDOWPOSCHANGED:
      FAddedGlyphRect.Left := -1;

    CM_ENABLEDCHANGED: begin
      FCommonData.Invalidate;
      inherited;
      Exit;
    end;
  end;

  if (FCommonData = nil) or not FCommonData.Skinned or not ControlIsReady(Self) then
    case Message.Msg of
      WM_PRINT: begin
        PaintTo(TWMPaint(Message).DC, 0, 0);
        Exit;
      end;

      CM_MOUSEENTER:
        SkinData.FMouseAbove := True;

      CM_MOUSELEAVE:
        SkinData.FMouseAbove := False;


      WM_NCPAINT: begin
        inherited;
        PaintAddedGlyphStd;
        Exit;
      end;

      WM_PAINT: begin
        if SkinData.BGChanged then
          PrepareCache;

        if not Focused or (csDesigning in ComponentState) then begin
          BeginPaint(Handle, PS);
          bw := integer(BorderStyle <> bsNone) * 2;
          DC := GetWindowDC(Handle);
          BitBlt(DC, bw, bw, SkinData.FCacheBmp.Width - 2 * bw, SkinData.FCacheBmp.Height - 2 * bw, SkinData.FCacheBmp.Canvas.Handle, bw, bw, SRCCOPY);
          ReleaseDC(Handle, DC);
          EndPaint(Handle, PS);
          if not (Enabled and ControlIsActive(FCommonData)) and (csDesigning in ComponentState) then
            inherited;
        end
        else
          inherited;
      end

      else
        inherited
    end
  else begin
    case Message.Msg of
      WM_ERASEBKGND, CN_DRAWITEM: begin
        if not InAnimationProcess and not InUpdating(SkinData) then
          inherited;
          
        Exit;
      end;

      WM_NCPAINT: begin
        if not InAnimationProcess and not InUpdating(SkinData) then begin
          DC := GetWindowDC(Handle);
          SavedDC := SaveDC(DC);
          try
            PaintBorder(DC);
          finally
            RestoreDC(DC, SavedDC);
            ReleaseDC(Handle, DC);
          end;
        end;
        Exit;
      end;

      WM_PRINT: begin
        SkinData.Updating := False;
        DC := TWMPaint(Message).DC;
        if SkinData.BGChanged then
          PrepareCache;

        UpdateCorners(SkinData, 0);
        bw := BorderWidth;
        OurPaintHandler(DC);
        BitBltBorder(DC, 0, 0, SkinData.FCacheBmp.Width, SkinData.FCacheBmp.Height, SkinData.FCacheBmp.Canvas.Handle, 0, 0, bw);
        Exit;
      end;

      WM_PAINT: begin
        if not Focused then begin
          OurPaintHandler(TWMPaint(Message).DC);
          if not (Enabled and ControlIsActive(FCommonData)) and (csDesigning in ComponentState) then
            inherited;
        end
        else begin
          inherited;
          PaintBorder(0);
        end;
        Exit;
      end;

      CM_COLORCHANGED, CM_CHANGED:
        FCommonData.BGChanged := True;

      WM_SETTEXT:
        FCommonData.BGChanged := True;
    end;
    if CommonWndProc(Message, FCommonData) then
      Exit;

    inherited;
    case Message.Msg of
      CM_VISIBLECHANGED, {CM_ENABLEDCHANGED, }WM_SETFONT:
        FCommonData.Invalidate;
    end;
  end;
  if Assigned(BoundLabel) then
    BoundLabel.HandleOwnerMsg(Message, Self);
end;


procedure TsMaskEdit.Change;
begin
  if not (csLoading in ComponentState) then
    inherited;
end;


function TsMaskEdit.ColorStored: boolean;
begin
  Result := not SkinData.Skinned or SkinData.CustomColor;
end;


function TsMaskEdit.BorderWidth: integer;
begin
  Result := integer(BorderStyle <> bsNone) * (2 + integer(Ctl3d));
end;


function TsMaskEdit.FontStored: boolean;
begin
  Result := IsCustomFont(Self, Font, not SkinData.Skinned or SkinData.CustomFont);
end;


procedure TsMaskEdit.ValidateEdit;
var
  Text: string;
begin
  if FCheckOnExit then
    try
      inherited ValidateEdit;
    except
      on E: EDBEditError do begin
        Text := E.Message;
        if Assigned(FOnValidateError) then
          FOnValidateError(Self, Text);

        raise EDBEditError.Create(Text);
      end;
    end;
end;

end.
