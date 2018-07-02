unit sEdit;
{$I sDefs.inc}
//{$DEFINE LOGGED}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, StdCtrls,
  {$IFDEF LOGGED} sDebugMsgs, {$ENDIF}
  {$IFNDEF DELPHI5} Types, {$ENDIF}
  {$IFDEF TNTUNICODE} TntControls, TntActnList, TntStdCtrls, TntClasses, {$ENDIF}
  sCommonData, sConst, sDefaults, sGlyphUtils;


type
{$IFDEF DELPHI_XE3}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]{$ENDIF}
{$IFDEF TNTUNICODE}
  TsEdit = class(TTntEdit)
{$ELSE}
  TsEdit = class(TEdit)
{$ENDIF}
{$IFNDEF NOTFORHELP}
  private
    FAddedGlyph: TacAddedGlyph;
    FCommonData: TsCtrlSkinData;
    FDisabledKind: TsDisabledKind;
    FBoundLabel: TsBoundLabel;
{$IFNDEF DELPHI6UP}
    FOnMouseLeave: TNotifyEvent;
    FOnMouseEnter: TNotifyEvent;
{$ENDIF}
    FAddedGlyphRect: TRect;
    procedure SetDisabledKind(const Value: TsDisabledKind);
    function ColorStored: boolean;
    function FontStored: boolean;
    function AddedGlyphRect: TRect;
    procedure WMNCCalcSize(var Message: TWMNCCalcSize); message WM_NCCALCSIZE;
  protected
    function AddedGlyphVisible: boolean;
    function AddedGlyphSpace: integer;
    procedure PaintAddedGlyph;
    procedure PaintBorder;
    procedure PaintAddedGlyphStd;
    function PrepareCache: boolean; virtual;
    function TextRect: TRect;
    procedure PaintText; virtual;
    procedure OurPaintHandler(aDC: hdc = 0);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    procedure AfterConstruction; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    procedure WndProc(var Message: TMessage); override;
  published
    property Align;
{$IFDEF DELPHI7UP}
    property MaxLength;
{$ENDIF}
    property Font stored FontStored;
    property ParentFont stored FontStored;
{$ENDIF} // NOTFORHELP
    property AddedGlyph: TacAddedGlyph read FAddedGlyph write FAddedGlyph;
    property DisabledKind: TsDisabledKind read FDisabledKind write SetDisabledKind default DefDisabledKind;
    property SkinData: TsCtrlSkinData read FCommonData write FCommonData;
    property BoundLabel: TsBoundLabel read FBoundLabel write FBoundLabel;
{$IFNDEF DELPHI6UP}
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
{$ENDIF}
    property Color stored ColorStored;
  end;


{$IFNDEF NOTFORHELP}
function EditBorderWidth(aEditCtrl: {$IFDEF TNTUNICODE}TTntEdit{$ELSE}TEdit{$ENDIF}): integer;
{$ENDIF} // NOTFORHELP

implementation

uses
  ExtCtrls,
  sStyleSimply, sVCLUtils, sMessages, sGraphUtils, sAlphaGraph, acntUtils, sThirdParty, acAlphaImageList;


function EditBorderWidth(aEditCtrl: {$IFDEF TNTUNICODE}TTntEdit{$ELSE}TEdit{$ENDIF}): integer;
begin
  with aEditCtrl do begin
    Result := integer(BorderStyle <> bsNone) * (1 + integer(Ctl3d));
{$IFDEF DELPHI7UP}
    inc(Result, integer(BevelKind <> bkNone) * (integer(BevelOuter <> bvNone) + integer(BevelInner <> bvNone)));
{$ENDIF}
  end;
end;


function TsEdit.AddedGlyphRect: TRect;
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


function TsEdit.AddedGlyphSpace: integer;
begin
  Result := integer(AddedGlyphVisible) * (FAddedGlyph.Width + acSpacing div 2);
end;


function TsEdit.AddedGlyphVisible: boolean;
begin
  Result := Assigned(AddedGlyph.Images) and IsValidIndex(AddedGlyph.ImageIndex, GetImageCount(AddedGlyph.Images));
end;


procedure TsEdit.AfterConstruction;
begin
  inherited AfterConstruction;
  FCommonData.Loaded;
end;


function TsEdit.ColorStored: boolean;
begin
  Result := not SkinData.Skinned or SkinData.CustomColor;
end;


constructor TsEdit.Create(AOwner: TComponent);
begin
  FCommonData := TsCtrlSkinData.Create(Self, True);
  FCommonData.COC := COC_TsEdit;
  inherited Create(AOwner);
  FAddedGlyph := TacAddedGlyph.Create(Self);
  FAddedGlyphRect.Left := -1;
  FDisabledKind := DefDisabledKind;
  FBoundLabel := TsBoundLabel.Create(Self, FCommonData);
end;


destructor TsEdit.Destroy;
begin
  FreeAndNil(FBoundLabel);
  FreeAndNil(FCommonData);
  FreeAndNil(FAddedGlyph);
  inherited Destroy;
end;


procedure TsEdit.Loaded;
begin
  inherited Loaded;
  FCommonData.Loaded;
end;


procedure TsEdit.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (FAddedGlyph <> nil) and (AComponent = FAddedGlyph.Images) then
    FAddedGlyph.Images := nil;
end;


procedure TsEdit.OurPaintHandler(aDC: hdc = 0);
var
  DC, SavedDC: hdc;
  PS: TPaintStruct;
begin
  if not InAnimationProcess and ((aDC <> SkinData.PrintDC) or (aDC = 0)) then
    BeginPaint(Handle, PS);

  if not InUpdating(FCommonData) then begin
    SavedDC := 0;
    if aDC = 0 then begin
      DC := GetWindowDC(Handle);
      SavedDC := SaveDC(DC);
    end
    else
      DC := aDC;

    try
      FCommonData.BGChanged := FCommonData.BGChanged or FCommonData.HalfVisible or GetBoolMsg(Parent, AC_GETHALFVISIBLE);
      FCommonData.HalfVisible := not RectInRect(Parent.ClientRect, BoundsRect);
      if not FCommonData.BGChanged or PrepareCache then begin
        UpdateCorners(FCommonData, 0);
        BitBlt(DC, 0, 0, Width, Height, FCommonData.FCacheBmp.Canvas.Handle, 0, 0, SRCCOPY);
      end
      else
        FCommonData.FUpdating := True;
    finally
      if aDC = 0 then begin
        RestoreDC(DC, SavedDC);
        ReleaseDC(Handle, DC);
      end;
    end;
  end;
  if not InAnimationProcess and ((aDC <> SkinData.PrintDC) or (aDC = 0)) then
    EndPaint(Handle, PS);
end;


function TsEdit.PrepareCache: boolean;
var
  BGInfo: TacBGInfo;
begin
  Result := True;
  if SkinData.Skinned then begin
    BGInfo.BgType := btUnknown;
    GetBGInfo(@BGInfo, Parent);
    if BGInfo.BgType <> btNotReady then begin
      InitCacheBmp(SkinData);
      if BorderStyle = bsSingle then
        PaintItem(FCommonData, BGInfoToCI(@BGInfo), True, integer(ControlIsActive(FCommonData)), MkRect(Self), Point(Left, top), FCommonData.FCacheBmp, False)
      else
        PaintItemBG(FCommonData, BGInfoToCI(@BGInfo), integer(ControlIsActive(FCommonData)), MkRect(Self), Point(Left, top), FCommonData.FCacheBmp);

      PaintText;
      if not Enabled then
        BmpDisabledKind(FCommonData.FCacheBmp, FDisabledKind, Parent, BGInfoToCI(@BGInfo), Point(Left, Top));

      FCommonData.BGChanged := False;
    end
    else begin
      Result := False;
      Exit;
    end;
  end
  else begin
    InitCacheBmp(SkinData);
    FillDC(FCommonData.FCacheBmp.Canvas.Handle, MkRect(Self), Color);
  end;
  if AddedGlyphVisible then
    PaintAddedGlyph;
end;


procedure TsEdit.PaintAddedGlyph;
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


procedure TsEdit.PaintBorder;
var
  DC, SavedDC: HDC;
  BordWidth: integer;
begin
  if not InUpdating(FCommonData) then begin
    DC := GetWindowDC(Handle);
    SavedDC := SaveDC(DC);
    try
      if FCommonData.BGChanged then
        if not PrepareCache then begin
          RestoreDC(DC, SavedDC);
          ReleaseDC(Handle, DC);
          FCommonData.FUpdating := True;
          Exit;
        end;

      BordWidth := EditBorderWidth({$IFDEF TNTUNICODE}TTntEdit{$ELSE}TEdit{$ENDIF}(Self));
{$IFDEF DELPHI7UP}
      if BordWidth = 0 then begin
        if BevelInner <> bvNone then
          inc(BordWidth);

        if BevelOuter <> bvNone then
          inc(BordWidth);
      end;
{$ENDIF}
      UpdateCorners(FCommonData, 0);
      BitBltBorder(DC, 0, 0, Width, Height, FCommonData.FCacheBmp.Canvas.Handle, 0, 0, BordWidth);
      if AddedGlyphVisible then

        if IsRightToLeft then
          BitBlt(DC, BordWidth, 0, AddedGlyphSpace, Height, SkinData.FCacheBmp.Canvas.Handle, BordWidth, 0, SRCCOPY)
        else
          BitBlt(DC, AddedGlyphRect.Left - acSpacing div 2, 0, AddedGlyphSpace + acSpacing div 2, Height, SkinData.FCacheBmp.Canvas.Handle, AddedGlyphRect.Left - acSpacing div 2, 0, SRCCOPY);
{$IFDEF DYNAMICCACHE}
      if Assigned(FCommonData.FCacheBmp) then
        FreeAndNil(FCommonData.FCacheBmp);
{$ENDIF}
    finally
      RestoreDC(DC, SavedDC);
      ReleaseDC(Handle, DC);
    end;
  end;
end;


procedure TsEdit.PaintAddedGlyphStd;
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


procedure TsEdit.PaintText;
var
  R: TRect;
  pc: acChar;
  s: acString;
  Flags: Cardinal;
  i: integer;
begin
  FCommonData.FCacheBMP.Canvas.Font.Assign(Font);
  Flags := DT_TOP or DT_NOPREFIX or DT_SINGLELINE;
  R := TextRect;
{$IFNDEF D2009}
  if IsRightToLeft then begin
    Flags := Flags or DT_RTLREADING or DT_RIGHT;
    dec(R.Right);
  end;
{$ENDIF}
  if Text <> '' then begin
    if PasswordChar <> #0 then begin
{$IFDEF D2009}
      if PasswordChar = '*' then
        pc := #9679
      else
{$ENDIF}
        pc := PasswordChar;

      for i := 1 to Length(Text) do
        s := s + pc;
    end
    else
      s := Text;

    acWriteTextEx(FCommonData.FCacheBMP.Canvas, PacChar(s), True, R,
                  Flags {$IFDEF D2009}or Cardinal(GetStringFlags(Self, Alignment)) and not DT_VCENTER{$ENDIF},
                  FCommonData, ControlIsActive(FCommonData));
  end
{$IFDEF D2009}
  else
    if TextHint <> '' then begin
      FCommonData.FCacheBMP.Canvas.Brush.Style := bsClear;
      if FCommonData.Skinned then
        FCommonData.FCacheBMP.Canvas.Font.Color := BlendColors(ColorToRGB(Font.Color), ColorToRGB(Color), 166)
      else
        FCommonData.FCacheBMP.Canvas.Font.Color := clGrayText;

      acDrawText(FCommonData.FCacheBMP.Canvas.Handle, TextHint, R, Flags {$IFDEF D2009}or Cardinal(GetStringFlags(Self, Alignment)) and not DT_VCENTER{$ENDIF});
    end;
{$ENDIF}
end;


procedure TsEdit.SetDisabledKind(const Value: TsDisabledKind);
begin
  if FDisabledKind <> Value then begin
    FDisabledKind := Value;
    FCommonData.Invalidate;
  end;
end;


function TsEdit.TextRect: TRect;
var
  bw, iSingle: integer;
begin
  if BorderStyle <> bsNone then
    bw := EditBorderWidth({$IFDEF TNTUNICODE}TTntEdit{$ELSE}TEdit{$ENDIF}(Self)) {$IFDEF DELPHI7UP} + integer(BevelKind <> bkNone) * (integer(BevelOuter <> bvNone) + integer(BevelInner <> bvNone)) {$ENDIF}
  else
    bw := 0;

  iSingle := integer(BorderStyle = bsSingle);
  if IsRightToLeft then
    Result := Rect(bw + 2 * integer(not Ctl3D), bw + iSingle, Width - bw - AddedGlyphSpace - acSpacing div 2 + iSingle, Height - bw)
  else
    Result := Rect(AddedGlyphSpace + bw + iSingle, bw + iSingle, Width - bw - iSingle, Height - bw);
end;


procedure TsEdit.WMNCCalcSize(var Message: TWMNCCalcSize);
begin
  inherited;
  if AddedGlyphVisible then
    with Message.CalcSize_Params.rgrc[0] do
      if IsRightToLeft then
        dec(Right, AddedGlyphSpace)
      else
        inc(Left, AddedGlyphSpace);
end;


procedure TsEdit.WndProc(var Message: TMessage);
var
  DC: hdc;
  bw: integer;
  PS: TPaintStruct;
begin
{$IFDEF LOGGED}
  AddToLog(Message, IntToStr(Color));
{$ENDIF}
  case Message.Msg of
    SM_ALPHACMD:
      case Message.WParamHi of
        AC_REMOVESKIN:
          if ACUInt(Message.LParam) = ACUInt(SkinData.SkinManager) then begin
            CommonWndProc(Message, FCommonData);
            Exit;
          end;

        AC_REFRESH:
          if ACUInt(Message.LParam) = ACUInt(SkinData.SkinManager) then begin
            CommonWndProc(Message, FCommonData);
            if not InAnimationProcess and HandleAllocated and Visible then
              RedrawWindow(Handle, nil, 0, RDWA_REPAINT);

            Exit;
          end;

        AC_SETNEWSKIN:
          if ACUInt(Message.LParam) = ACUInt(SkinData.SkinManager) then begin
            CommonWndProc(Message, FCommonData);
            Exit;
          end;

        AC_CHILDCHANGED: begin
          Message.LParam := 0; // Internal buttons not required in the repainting
          Exit;
        end;

        AC_GETCONTROLCOLOR:
          if not FCommonData.Skinned then begin
            Message.Result := acColorToRGB(Color);
            Exit;
          end;

        AC_PREPARECACHE: begin
          PrepareCache;
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

  if not ControlIsReady(Self) or not FCommonData.Skinned then begin
    case Message.Msg of
      WM_PRINT: begin
        PaintTo(TWMPaint(Message).DC, 0, 0);
        Exit;
      end;

      WM_NCPAINT: begin
        inherited;
        PaintAddedGlyphStd;
        Exit;
      end;
    end;
    inherited
  end
  else begin
    case Message.Msg of
      WM_ERASEBKGND, CN_DRAWITEM: begin
        if not InAnimationProcess and not InUpdating(SkinData) and Enabled then
          inherited;

        Exit;
      end;

      WM_NCPAINT: if IsWindowVisible(Handle) then begin
        PaintBorder;
        Exit;
      end;

      WM_PAINT: begin
        if InAnimationProcess or InUpdating(SkinData) then begin // Exit if parent is not ready yet
          BeginPaint(Handle, PS);
          EndPaint(Handle, PS);
          Exit;
        end;
        if not (Enabled and TWinControl(FCommonData.FOwnerControl).Focused) then begin
          OurPaintHandler(TWMPaint(Message).DC);
          if csDesigning in ComponentState then
            inherited;

          Exit;
        end
      end;

      WM_PRINT: begin
        SkinData.Updating := False;
        DC := TWMPaint(Message).DC;
        PrepareCache;
        UpdateCorners(SkinData, 0);
        bw := integer(BorderStyle <> bsNone) * (1 + integer(Ctl3d));
        if PRF_CLIENT and Message.LParam = PRF_CLIENT then begin
          MoveWindowOrg(DC, -bw, -bw);
          IntersectClipRect(DC, 0, 0, SkinData.FCacheBmp.Width - bw, SkinData.FCacheBmp.Height - bw);
        end
        else
          BitBltBorder(DC, 0, 0, SkinData.FCacheBmp.Width, SkinData.FCacheBmp.Height, SkinData.FCacheBmp.Canvas.Handle, 0, 0, bw);

        OurPaintHandler(DC);
        Exit;
      end;

      CM_COLORCHANGED:
        if FCommonData.CustomColor then
          FCommonData.BGChanged := True;
    end;
    if CommonWndProc(Message, FCommonData) then
      Exit;

    inherited;
    case Message.Msg of
      SM_ALPHACMD:
        case Message.WParamHi of
          AC_ENDPARENTUPDATE:
            if SkinData.FUpdating then
              if not InUpdating(FCommonData, True) then begin
                Repaint;
                SendMessage(Handle, WM_NCPAINT, 0, 0);
              end;

        end;

{$IFNDEF DELPHI6UP}
      CM_MOUSEENTER, CM_MOUSELEAVE:
        if not (csDestroying in ComponentState) and not (csDesigning in ComponentState) and Enabled then begin
          SkinData.FMouseAbove := Message.Msg = CM_MOUSEENTER;
          if SkinData.FMouseAbove then begin
            if Assigned(FOnMouseEnter) then
              FOnMouseEnter(Self);
          end
          else
            if Assigned(FOnMouseLeave) then
              FOnMouseLeave(Self);

          RedrawWindow(Handle, nil, 0, RDW_INVALIDATE or RDW_NOERASE or RDW_UPDATENOW);
        end;
{$ENDIF}

      WM_SETTEXT, CM_TEXTCHANGED, CM_VISIBLECHANGED, CM_ENABLEDCHANGED, WM_SETFONT:
        if not (csLoading in ComponentState) and not InAnimationProcess then begin
          FCommonData.BGChanged := True;
          Repaint;
          SendMessage(Handle, WM_NCPAINT, 0, 0);
        end;

      WM_SIZE:
        SendMessage(Handle, WM_NCPAINT, 0, 0);

      CM_FOCUSCHANGED:
        SkinData.BGChanged := True;
    end;
  end;
  if Assigned(BoundLabel) then
    BoundLabel.HandleOwnerMsg(Message, Self);
end;


function TsEdit.FontStored: boolean;
begin
  Result := IsCustomFont(Self, Font, not SkinData.Skinned or SkinData.CustomFont);
end;

end.
