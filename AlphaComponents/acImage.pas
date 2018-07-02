unit acImage;
{$I sDefs.inc}
//{$DEFINE LOGGED}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Imglist,
  {$IFNDEF DELPHI5} Types, {$ENDIF}
  sConst, ExtCtrls, acAlphaImageList, sCommonData;


type
{$IFNDEF NOTFORHELP}
  TsCustomImage = class(TImage)
  private
    FGrayed,
    FReflected,
    FUseFullSize: boolean;

    FBlend: TPercent;
    FImageIndex: integer;
    FImages: TCustomImageList;
    FCommonData: TsCtrlSkinData;
    FImageChangeLink: TChangeLink;
{$IFNDEF D2010}
    FOnMouseLeave,
    FOnMouseEnter: TNotifyEvent;
{$ENDIF}
    procedure SetBlend       (const Value: TPercent);
    procedure SetImageIndex  (const Value: integer);
    procedure SetImages      (const Value: TCustomImageList);
    procedure SetBoolean     (const Index: Integer; const Value: boolean);
    procedure ImageListChange(Sender: TObject);
  protected
    ImageChanged: boolean;
    function OwnDrawing: boolean;
    function PrepareCache(DC: HDC): boolean;
    function CanAutoSize(var NewWidth, NewHeight: Integer): Boolean; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function GetCanvas: TCanvas;
  public
    procedure AfterConstruction; override;
    procedure Loaded; override;
    constructor Create(AOwner:TComponent); override;
    procedure UpdateImage;
    property Canvas: TCanvas read GetCanvas;

    function Empty: boolean;
    destructor Destroy; override;
    procedure acWM_Paint(var Message: TWMPaint); message WM_PAINT;
    procedure WndProc(var Message: TMessage); override;

    property Blend: TPercent read FBlend write SetBlend default 0;

    property UseFullSize: boolean Index 0 read FUseFullSize write SetBoolean default False;
    property Reflected:   boolean Index 1 read FReflected   write SetBoolean default False;
    property Grayed:      boolean Index 2 read FGrayed      write SetBoolean default False;

    property ImageIndex: integer read FImageIndex write SetImageIndex default -1;
    property Images: TCustomImageList read FImages write SetImages;
    property SkinData: TsCtrlSkinData read FCommonData write FCommonData;
{$IFNDEF D2010}
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
{$ENDIF}
  end;
{$ENDIF}


{$IFDEF DELPHI_XE3}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]{$ENDIF}
  TsImage = class(TsCustomImage)
  published
{$IFNDEF NOTFORHELP}
    property Picture;
    property OnMouseEnter;
    property OnMouseLeave;
{$ENDIF}
    property Blend;
    property ImageIndex;
    property Images;
    property Grayed;
    property Reflected;
    property SkinData;
    property UseFullSize;
  end;


implementation

uses
  math,
  {$IFDEF LOGGED} sDebugMsgs, {$ENDIF}
  sGraphUtils, acntUtils, sAlphaGraph, sSkinManager, sThirdParty, acPNG, sVCLUtils, sMessages;


procedure TsCustomImage.AfterConstruction;
begin
  inherited;
  FCommonData.Loaded;
end;


constructor TsCustomImage.Create(AOwner: TComponent);
begin
  FCommonData := TsCtrlSkinData.Create(Self, True);
  FCommonData.COC := COC_TsImage;
  inherited;
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageListChange;
  ImageChanged := True;
  FImageIndex := -1;
end;


destructor TsCustomImage.Destroy;
begin
  FreeAndNil(FCommonData);
  FreeAndNil(FImageChangeLink);
  inherited;
end;


procedure TsCustomImage.SetBlend(const Value: TPercent);
begin
  if FBlend <> Value then begin
    FBlend := Value;
    Skindata.Invalidate;
  end;
end;


procedure TsCustomImage.SetBoolean(const Index: Integer; const Value: boolean);

  procedure ChangeProp(var Prop: boolean; Value: boolean);
  begin
    if Prop <> Value then begin
      Prop := Value;
      Skindata.Invalidate;
    end;
  end;

begin
  case Index of
    0: ChangeProp(FUseFullSize, Value);
    1: ChangeProp(FReflected, Value);
    2: ChangeProp(FGrayed, Value);
  end;
end;


procedure TsCustomImage.SetImageIndex(const Value: integer);
begin
  if FImageIndex <> Value then begin
    FImageIndex := Value;
    if AutoSize then
      AdjustSize;

    Skindata.Invalidate;
  end;
end;


procedure TsCustomImage.SetImages(const Value: TCustomImageList);
begin
  if FImages <> Value then begin
    if FImages <> nil then
      Images.UnRegisterChanges(FImageChangeLink);

    FImages := Value;
    if Images <> nil then begin
      Images.RegisterChanges(FImageChangeLink);
      Images.FreeNotification(Self);
    end;
    UpdateImage;
  end;
end;


procedure TsCustomImage.ImageListChange(Sender: TObject);
begin
  Skindata.Invalidate;
end;


function TsCustomImage.OwnDrawing: boolean;
begin
  Result := Reflected or Grayed or (Blend > 0);
end;


function TsCustomImage.PrepareCache(DC: HDC): boolean;
var
  R: TRect;
  C: TsColor;
  CI: TCacheInfo;
  BGInfo: TacBGInfo;

  procedure DrawImage;
  var
    l, t: integer;
    StretchSize, Size: TSize;
    TmpBmp, StretchSrc: TBitmap;

    function GetSrcSize(CalcStretch: boolean = True): TSize;
    var
      SrcHeight: integer;
{$IFNDEF DELPHI5}
      xyaspect: real;
{$ENDIF}
    begin
      if (ImageIndex >= 0) and (Images <> nil) then
        Result := MkSize(GetImageWidth(Images, ImageIndex), GetImageHeight(Images, ImageIndex))
      else
        Result := MkSize(Picture.Width, Picture.Height);

      if (Result.cx <> 0) and (Result.cy <> 0) then // Picture is not empty?
        if CalcStretch then begin
          SrcHeight := Height;
          if Reflected then
            SrcHeight := SrcHeight * 2 div 3;
{$IFNDEF DELPHI5}
          if Proportional then begin
            xyaspect := Result.cx / Result.cy;
            if Result.cx > Result.cy then begin
              Result.cx := Width;
              Result.cy := Round(Width / xyaspect);
              if Result.cy > SrcHeight then begin
                Result.cy := SrcHeight;
                Result.cx := Round(Result.cy * xyaspect);
              end;
            end
            else begin
              Result.cy := SrcHeight;
              Result.cx := Round(Result.cy * xyaspect);
              if Result.cx > Width then begin
                Result.cx := Width;
                Result.cy := Round(Result.cx / xyaspect);
              end;
            end;
          end
          else
{$ENDIF}
            Result := MkSize(Width, SrcHeight);
        end;
    end;

  begin
    Size := GetSrcSize(False);
    StretchSize := GetSrcSize(Stretch);
    TmpBmp := CreateBmp32(Size);
    TmpBmp.Transparent := Transparent;
    if (ImageIndex >= 0) and (Images <> nil) then begin
      // Get bitmap from imagelist
      TmpBmp.Width := GetImageWidth(Images, ImageIndex);
      TmpBmp.Height := GetImageHeight(Images, ImageIndex);
      TmpBmp.Canvas.Font.Color := Font.Color;
      if Images is TacImageList then
        TacImageList(Images).GetBitmap32(ImageIndex, TmpBmp)
      else
        if Images is TsVirtualImageList then
          TsVirtualImageList(Images).GetBitmap32(ImageIndex, TmpBmp)
        else
          Images.GetBitmap(ImageIndex, TmpBmp);

      if Stretch then begin
        StretchSrc := TmpBmp;
        TmpBmp := CreateBmp32(StretchSize);
        sGraphUtils.Stretch(StretchSrc, TmpBmp, StretchSize.cx, StretchSize.cy, ftMitchell);
        StretchSrc.Free;
      end;
    end
    else
      if not (Picture.Graphic is TBitmap) and not (Picture.Graphic is TPNGGraphic) (*{$IFDEF D2010}or not Picture.Graphic.SupportsPartialTransparency {$ENDIF}*) then begin
        TmpBmp.Width := StretchSize.cx;
        TmpBmp.Height := StretchSize.cy;
        BitBlt(TmpBmp.Canvas.Handle, 0, 0, Width, Height, FCommonData.FCacheBmp.Canvas.Handle, 0, 0, SRCCOPY);
        R := MkRect(TmpBmp);
        if Stretch then begin
          R.Right := min(Width, R.Left + StretchSize.cx);
          R.Bottom := min(Height, R.Top + StretchSize.cy);
        end;
        TmpBmp.Canvas.StretchDraw(R, Picture.Graphic);
        FillAlphaRect(TmpBmp, R, MaxByte);
      end
      else
        if not Picture.Bitmap.Empty then begin
          TmpBmp.Assign(Picture.Bitmap);
          if TmpBmp.PixelFormat <> pf32bit then begin
            TmpBmp.PixelFormat := pf32bit;
            if Transparent then begin
              C.C := TmpBmp.Canvas.Pixels[0, Height - 1];
              C.A := 0;
              FillAlphaRect(TmpBmp, MkRect(TmpBmp), MaxByte, C.C);
            end
            else
              FillAlphaRect(TmpBmp, MkRect(TmpBmp), MaxByte);
          end
          else
            if (DefaultManager = nil) or DefaultManager.Options.CheckEmptyAlpha then
              CheckEmptyAChannel(Picture.Bitmap);

          if Stretch then begin
            StretchSrc := TmpBmp;
            TmpBmp := CreateBmp32(StretchSize);
            sGraphUtils.Stretch(StretchSrc, TmpBmp, StretchSize.cx, StretchSize.cy, ftMitchell);
            StretchSrc.Free;
          end;
        end;

    if Stretch {$IFDEF DELPHI6UP}and not Proportional{$ENDIF} or not Center then begin
      l := 0;
      t := 0;
    end
    else begin
      l := (Width - TmpBmp.Width) div 2;
      t := (Height - TmpBmp.Height - integer(FUseFullSize) * (TmpBmp.Height div 2)) div 2;
    end;
    CopyBmp32(Rect(l, t, l + TmpBmp.Width, t + TmpBmp.Height), MkRect(TmpBmp), FCommonData.FCacheBmp, TmpBmp, CI, False,
                                                               iff(Grayed, $FFFFFF, clNone), Blend, Reflected);
    TmpBmp.Free;
  end;

begin
  Result := True;

  InitCacheBmp(SkinData);
  BGInfo.Bmp := nil;
  BGInfo.BgType := btUnknown;
  BGInfo.PleaseDraw := False;
  BGInfo.FillRect := MkRect;
  if not ((SkinData.SkinManager <> nil) and SkinData.SkinManager.Options.StdImgTransparency) then
    SendAMessage(Parent, AC_GETBG, LPARAM(@BGInfo));

  if (BGInfo.BgType = btUnknown) or (SkinData.SkinManager <> nil) and SkinData.SkinManager.Options.StdImgTransparency then begin
    BitBlt(FCommonData.FCacheBmp.Canvas.Handle, 0, 0, Width, Height, DC, 0, 0, SRCCOPY);
    CI := BGInfoToCI(@BGInfo);
    DrawImage;
    FCommonData.BGChanged := False;
  end
  else
    if BGInfo.BgType = btNotReady then begin
      FCommonData.FUpdating := True;
      Result := False;
    end
    else begin
      CI := BGInfoToCI(@BGInfo);
      if not CI.Ready or not CI.Bmp.Empty then begin
        if Transparent then
          if not CI.Ready and (CI.FillColor = sFuchsia.C) then
            BitBlt(FCommonData.FCacheBmp.Canvas.Handle, 0, 0, Width, Height, DC, 0, 0, SRCCOPY)
          else
            if not CI.Ready or (CI.Bmp = nil) then
              FillDC(FCommonData.FCacheBmp.Canvas.Handle, MkRect(Self), CI.FillColor)
            else
//              BitBlt(FCommonData.FCacheBmp.Canvas.Handle, 0, 0, Width, Height, CI.Bmp.Canvas.Handle, CI.X{ + Left}, CI.Y{ + Top}, SRCCOPY);
              BitBlt(FCommonData.FCacheBmp.Canvas.Handle, 0, 0, Width, Height, CI.Bmp.Canvas.Handle, CI.X + Left, CI.Y + Top, SRCCOPY);

        if FCommonData.SkinSection <> '' then
          PaintItem(FCommonData, CI, True, 0, MkRect(Self), MkPoint(Left, Top), FCommonData.FCacheBMP, True, 0, 0);

        DrawImage;
        FCommonData.BGChanged := False;
      end;
    end;
end;


procedure TsCustomImage.WndProc(var Message: TMessage);
begin
{$IFDEF LOGGED}
  AddToLog(Message);
{$ENDIF}
  if FCommonData <> nil then
    case Message.Msg of
      SM_ALPHACMD:
        case Message.WParamHi of
          AC_ISOPAQUE: begin
            if (SkinData.SkinManager <> nil) and SkinData.SkinManager.CommonSkinData.Active then
              Message.Result := 1
            else
              Message.Result := integer(not (csOpaque in ControlStyle));

            Exit;
          end;
        end;

      WM_ERASEBKGND:
        Exit;

      WM_WINDOWPOSCHANGED, WM_SIZE:
        if Visible then
          FCommonData.BGChanged := True;

      CM_VISIBLECHANGED: begin
        FCommonData.BGChanged := True;
        SkinData.FMouseAbove := False
      end;
    end;

  inherited;
{$IFNDEF D2010}
  case Message.Msg of
    CM_MOUSEENTER: if Assigned(FOnMouseEnter) then FOnMouseEnter(Self);
    CM_MOUSELEAVE: if Assigned(FOnMouseLeave) then FOnMouseLeave(Self);
  end;
{$ENDIF}
end;


procedure TsCustomImage.acWM_Paint(var Message: TWMPaint);
var
  SavedDC: hdc;
begin
  if not InUpdating(FCommonData) and not (Empty and (SkinData.SkinIndex < 0)) then begin
    SavedDC := SaveDC(Message.DC);
    try
      if FCommonData.BGChanged or ImageChanged then begin
        FCommonData.UpdateIndexes;
        PrepareCache(Message.DC);
      end;
      BitBlt(Message.DC, 0, 0, Width, Height, FCommonData.FCacheBmp.Canvas.Handle, 0, 0, SRCCOPY);
    finally
      RestoreDC(Message.DC, SavedDC);
    end;
  end
  else
    inherited;
end;


function TsCustomImage.Empty: boolean;
begin
  if (FImages <> nil) and IsValidIndex(FImageIndex, GetImageCount(FImages)) then
    Result := False
  else
    if Picture.Graphic <> nil then
      Result := Picture.Graphic.Empty
    else
      if Picture.Bitmap <> nil then
        Result := Picture.Bitmap.Empty
      else
        Result := True;
end;


function TsCustomImage.GetCanvas: TCanvas;
begin
  if (Picture.Graphic = nil) or not (Picture.Graphic is TBitmap) then
    Result := inherited Canvas
  else begin
//    TBitmap(Picture.Graphic).Width := Width;
//    TBitmap(Picture.Graphic).Height := Height;
    Result := TBitmap(Picture.Graphic).Canvas
  end;
end;


procedure TsCustomImage.UpdateImage;
begin
  if [csLoading, csDestroying] * ComponentState = [] then
    Repaint;
end;


function TsCustomImage.CanAutoSize(var NewWidth, NewHeight: Integer): Boolean;
var
  w, h: integer;
begin
  Result := True;
  if not (csDesigning in ComponentState) or not Empty then begin
    if (Images <> nil) and IsValidIndex(ImageIndex, GetImageCount(Images)) then begin
      w := GetImageWidth(Images, ImageIndex);
      h := GetImageHeight(Images, ImageIndex);
    end
    else begin
      w := Picture.Width;
      h := Picture.Height;
    end;
    if Align in [alNone, alLeft, alRight] then
      NewWidth := w;

    if Align in [alNone, alTop, alBottom] then begin
      NewHeight := h;
      if FUseFullSize and FReflected then
        inc(NewHeight, NewHeight div 2)
    end;
  end
  else
    inherited CanAutoSize(NewWidth, NewHeight);
end;


procedure TsCustomImage.Loaded;
begin
  inherited;
  FCommonData.Loaded;
end;


procedure TsCustomImage.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
    if AComponent = Images then
      Images := nil;
end;

end.
