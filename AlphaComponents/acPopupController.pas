unit acPopupController;
{$I sDefs.inc}
//{$DEFINE LOGGED}
//{$DEFINE AC_NOSHADOW}
//{$DEFINE AC_NOHANDLECTRL}
//{$DEFINE AC_NOHANDLEFORM}

interface

uses
  {$IFNDEF DELPHI5} Types, {$ENDIF}
  {$IFDEF LOGGED} sDebugMsgs, {$ENDIF}
  Forms, Windows, Graphics, SysUtils, Classes, Controls, Messages,
  acThdTimer;


type
  TsPopupController = class;

  TacFormHandler = class(TObject)
  protected
    ClientCtrl: TWinControl;
{$IFNDEF AC_NOHANDLEFORM}
    FormWndProc: TWndMethod;
{$ENDIF}
{$IFNDEF AC_NOHANDLECTRL}
    CtrlWndProc: TWndMethod;
{$ENDIF}
    Initialized,
    ClosingForbidden: boolean;
    CaptureHandle: THandle;
    OldOnClose: TCloseEvent;
    CheckTimer: TacThreadedTimer;
    Controller: TsPopupController;
    procedure UnInitControls;
    procedure DoTimer(Sender: TObject);
    procedure UpdateRgnBmp(aBmp: TBitmap);
    procedure CloseForm(CallProc: boolean = True);
    constructor Create(AForm: TForm; ACtrl: TWinControl);
    procedure InitControls(AForm: TForm; ACtrl: TWinControl);
{$IFNDEF AC_NOHANDLEFORM}
    procedure DoWndProc (var Message: TMessage);
{$ENDIF} // AC_NOHANDLEFORM
{$IFNDEF AC_NOHANDLECTRL}
    procedure DoCtrlProc(var Message: TMessage);
{$ENDIF}
  public
    Closed: boolean;
    PopupForm: TForm;
    ParentForm: TForm;
    PopupCtrl: TWinControl;
{$IFNDEF AC_NOSHADOW}
    ShadowForm: TCustomForm;
{$ENDIF}
    destructor Destroy; override;
  end;


{$IFDEF DELPHI_XE3}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]{$ENDIF}
  TsPopupController = class(TComponent)
  protected
    AnimDirection: integer;
    procedure DoDeactivate(Sender: TObject);
    procedure DoClose(Sender: TObject; var Action: TCloseAction);
    function InitFormHandler(AForm: TForm; Ctrl: TWinControl): integer;
  public
    SkipOpen,
    MousePressed,
    IgnoreCapture: boolean;
    FormHandlers: array of TacFormHandler;
    function GetFormIndex(Form: TForm): integer;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure KillAllForms(ExceptChild: TForm);
    function PopupCount(ExceptForm: TForm): integer;
    function HasChild(Parent: TForm): boolean;
    function PopupChildCount(ExceptForm: TForm): integer;
    procedure AnimShowPopup(aForm: TForm; wTime: word = 0; BlendValue: byte = MaxByte);
    procedure ShowForm(AForm: TForm; AOwnerControl: TWinControl; ALeft: integer = -1; ATop: integer = -1; Animated: boolean = True);
    procedure ShowFormPos(AForm: TForm; ALeftTop: TPoint; Animated: boolean = True);
    procedure ClosingForbide(AForm: TForm);
    procedure ClosingAllow(AForm: TForm);
  end;


function GetIntController: TsPopupController;
procedure ShowPopupForm(AForm: TForm; AOwnerControl: TWinControl; ALeft: integer = -1; ATop: integer = -1; Animated: boolean = True); overload;
procedure ShowPopupForm(AForm: TForm; ALeftTop: TPoint; Animated: boolean = True); overload;
{$IFNDEF AC_NOSHADOW}
function AttachShadowForm(aForm: TForm; DoShow: boolean = True): TCustomForm;
{$ENDIF}

var
  acIntController: TsPopupController = nil;

implementation

uses
  math,
  {$IFNDEF DELPHI6UP} acD5Ctrls, {$ENDIF}
  sMessages, sGraphUtils, acntUtils, sVclUtils, StdCtrls, sComboBox, sConst, sSkinProvider,
  sAlphaGraph, acntTypes, acPopupCtrls, sComboBoxes, sCommonData, sSkinManager, acSBUtils, sGradient;

var
  rShadowBmp: TBitmap = nil;


function GetIntController: TsPopupController;
begin
  if acIntController = nil then
    acIntController := TsPopupController.Create(nil);

  Result := acIntController;
end;


procedure ShowPopupForm(AForm: TForm; AOwnerControl: TWinControl; ALeft: integer = -1; ATop: integer = -1; Animated: boolean = True);
begin
  GetIntController.ShowForm(AForm, AOwnerControl, ALeft, ATop, Animated);
end;


procedure ShowPopupForm(AForm: TForm; ALeftTop: TPoint; Animated: boolean = True);
begin
  GetIntController.KillAllForms(AForm);
  acIntController.ShowFormPos(AForm, ALeftTop, Animated);
end;


procedure TsPopupController.KillAllForms(ExceptChild: TForm);
var
  i: integer;
  ParentForm: TForm;
begin
  if ExceptChild <> nil then
    ParentForm := TForm(GetOwnerForm(ExceptChild.Owner))
  else
    ParentForm := nil;

  for i := 0 to Length(FormHandlers) - 1 do
    if (FormHandlers[i] <> nil) and (FormHandlers[i].PopupForm <> nil) and (FormHandlers[i].PopupForm <> ParentForm) and FormHandlers[i].PopupForm.Visible then
      FormHandlers[i].PopupForm.Close;

  if PopupCount(nil) = 0 then begin
    for i := 0 to Length(FormHandlers) - 1 do
      if FormHandlers[i] <> nil then
        FreeAndNil(FormHandlers[i]);

    SetLength(FormHandlers, 0);
  end;
end;


function TsPopupController.PopupChildCount(ExceptForm: TForm): integer;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to Length(FormHandlers) - 1 do
    if (FormHandlers[i] <> nil) and
         (FormHandlers[i].PopupForm <> nil) and
           (FormHandlers[i].ParentForm = ExceptForm) and
             IsWindowVisible(FormHandlers[i].PopupForm.Handle) then
      inc(Result);
end;


function TsPopupController.PopupCount(ExceptForm: TForm): integer;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to Length(FormHandlers) - 1 do
    if (FormHandlers[i] <> nil) and
         (FormHandlers[i].PopupForm <> nil) and
           (FormHandlers[i].PopupForm <> ExceptForm) and
              IsWindowVisible(FormHandlers[i].PopupForm.Handle) then
      inc(Result);
end;


function TsPopupController.GetFormIndex(Form: TForm): integer;
var
  i, l: integer;
begin
  Result := -1;
  l := Length(FormHandlers);
  for i := 0 to l - 1 do
    if (FormHandlers[i] <> nil) and (FormHandlers[i].PopupForm = Form) then begin
      Result := i;
      Exit;
    end;
end;


function TsPopupController.HasChild(Parent: TForm): boolean;
var
  i: integer;
begin
  Result := False;
  for i := 0 to Length(FormHandlers) - 1 do
    if (FormHandlers[i].PopupForm <> Parent) and (FormHandlers[i].PopupCtrl <> nil) then
      if GetParentForm(FormHandlers[i].PopupCtrl) = Parent then begin
        Result := True;
        Exit;
      end;
end;


procedure TsPopupController.ClosingAllow(AForm: TForm);
var
  i: integer;
begin
  for i := 0 to Length(FormHandlers) - 1 do
    if (FormHandlers[i].PopupForm = AForm) then
      FormHandlers[i].ClosingForbidden := False;
end;


procedure TsPopupController.ClosingForbide(AForm: TForm);
var
  i: integer;
begin
  for i := 0 to Length(FormHandlers) - 1 do
    if (FormHandlers[i].PopupForm = AForm) then
      FormHandlers[i].ClosingForbidden := True;
end;


constructor TsPopupController.Create(AOwner: TComponent);
begin
  inherited;
  SkipOpen := False;
end;


destructor TsPopupController.Destroy;
var
  i: integer;
begin
  for i := 0 to Length(FormHandlers) - 1 do
    FreeAndNil(FormHandlers[i]);

  SetLength(FormHandlers, 0);
  inherited;
end;


procedure TsPopupController.DoClose(Sender: TObject; var Action: TCloseAction);
var
  i: integer;
begin
  i := GetFormIndex(TForm(Sender));
  if i >= 0 then
    FormHandlers[i].CloseForm(False);
end;


procedure TsPopupController.DoDeactivate(Sender: TObject);
begin
  if Sender <> nil then
    TForm(Sender).Close
end;


procedure TsPopupController.AnimShowPopup(aForm: TForm; wTime: word = 0; BlendValue: byte = MaxByte);
const
{$IFDEF ACDEBUG}
  DebugOffsX = 100; DebugOffsY = -100;
{$ENDIF}
  acwPopupDiv = 3;
var
  AnimBmp, acDstBmp: TBitmap;
  AnimForm: TacGlowForm;
  MaxTransparency: byte;

  DC: hdc;
  h: hwnd;
  fR: TRect;
  lTicks: DWord;
  Flags: Cardinal;
  FBmpSize: TSize;
  FBmpTopLeft: TPoint;
  FBlend: TBlendFunction;
  hNdx, i, StepCount: integer;
  dx, dy, l, t, r, b, trans, p: real;

  procedure Anim_Init;
  begin
    trans := 0;
    p := MaxTransparency / StepCount;
    if AnimDirection and 1 = 0 then begin
      t := 0;
      b := 0;
      dy := (acDstBmp.Height - b) / acwPopupDiv;
    end
    else begin
      t := acDstBmp.Height;
      b := acDstBmp.Height;
      dy := acDstBmp.Height / acwPopupDiv;
    end;
    if AnimDirection and 2 = 0 then begin
      l := 0;
      if FormHandlers[GetFormIndex(aForm)].PopupCtrl <> nil then
        r := FormHandlers[GetFormIndex(aForm)].PopupCtrl.Width
      else
        r := 0;

      dx := (acDstBmp.Width - r) / acwPopupDiv;
    end
    else begin
      if FormHandlers[GetFormIndex(aForm)].PopupCtrl <> nil then
        l := acDstBmp.Width - FormHandlers[GetFormIndex(aForm)].PopupCtrl.Width
      else
        l := 0;

      r := acDstBmp.Width;
      dx := acDstBmp.Width / acwPopupDiv;
    end;
  end;

  procedure Anim_DoNext;
  begin
    trans := min(Trans + p, MaxTransparency);
    FBlend.SourceConstantAlpha := Round(Trans);
    StretchBlt(AnimBmp.Canvas.Handle, Round(l), Round(t), Round(r - l), Round(b - t), acDstBmp.Canvas.Handle, 0, 0, acDstBmp.Width, acDstBmp.Height, SRCCOPY);
  end;

  procedure Anim_GoToNext;
  begin
    if AnimDirection and 1 = 0 then begin
      t := 0;
      b := acDstBmp.Height - dy;
    end
    else begin
      t := dy;
      b := acDstBmp.Height;
    end;
    if AnimDirection and 2 = 0 then begin
      l := 0;
      r := acDstBmp.Width - dx;
    end
    else begin
      l := dx;
      r := acDstBmp.Width;
    end;
    dx := dx / acwPopupDiv;
    dy := dy / acwPopupDiv;
  end;

  procedure UpdateShadowPos(AHandle: THandle; OwnerBounds: TacBounds; ABlend: byte);
  var
    lPos: TacLayerPos;
  begin
    with lPos do begin
      Bounds := OwnerBounds;
      lPos.LayerBlend := ABlend;
    end;
    SendAMessage(AHandle, AC_UPDATESHADOW, LParam(@lPos));
  end;

begin
  AnimForm := TacGlowForm.CreateNew(nil);
  InAnimationProcess := True;
  acDstBmp := CreateBmp32(aForm);
  acDstBmp.Canvas.Lock;
  SkinPaintTo(acDstBmp, aForm);
  if acDstBmp <> nil then begin
    FillAlphaRect(acDstBmp, MkRect(acDstBmp), MaxByte);
    acDstBmp.Canvas.UnLock;
    FBmpSize := MkSize(acDstBmp);
    StepCount := wTime div acTimerInterval;
    FBmpTopLeft := MkPoint;
{$IFDEF DELPHI7UP}
    MaxTransparency := iff(aForm.AlphaBlend, aForm.AlphaBlendValue, MaxByte);
{$ELSE}
    MaxTransparency := MaxByte;
{$ENDIF}
    if StepCount > 0 then
      InitBlendData(FBlend, 0)
    else
      InitBlendData(FBlend, MaxTransparency);

    GetWindowRect(aForm.Handle, fR);
    h := HWND_TOPMOST;

    if GetWindowLong(AnimForm.Handle, GWL_EXSTYLE) and WS_EX_LAYERED = 0 then
      SetWindowLong(AnimForm.Handle, GWL_EXSTYLE, GetWindowLong(AnimForm.Handle, GWL_EXSTYLE) or WS_EX_LAYERED);

    DC := GetDC(0);
    UpdateLayeredWindow(AnimForm.Handle, DC, nil, @FBmpSize, acDstBmp.Canvas.Handle, @FBmpTopLeft, clNone, @FBlend, ULW_ALPHA);
    ReleaseDC(0, DC);

    AnimForm.SetBounds(fR.Left{$IFDEF ACDEBUG} + DebugOffsX{$ENDIF}, fR.Top{$IFDEF ACDEBUG} + DebugOffsY{$ENDIF}, acDstBmp.Width, acDstBmp.Height);

    Flags := SWP_NOACTIVATE or SWP_NOREDRAW or SWP_NOCOPYBITS or SWP_NOOWNERZORDER or SWP_NOSENDCHANGING;
    SetWindowPos(AnimForm.Handle, h, AnimForm.Left, AnimForm.Top, FBmpSize.cx, FBmpSize.cy, Flags);// or SWP_NOREDRAW);

    hNdx := GetFormIndex(aForm);
    if hNdx >= 0 then
      FormHandlers[hNdx].UpdateRgnBmp(acDstBmp);

    ShowWindow(AnimForm.Handle, SW_SHOWNOACTIVATE);

    AnimBmp := CreateBmp32(FBmpSize);
    FillDC(AnimBmp.Canvas.Handle, MkRect(AnimBmp), 0);
    SetStretchBltMode(AnimBmp.Canvas.Handle, COLORONCOLOR);

    if StepCount > 0 then begin
      Anim_Init;
      i := 0;
      while i <= StepCount do begin
        Anim_DoNext;
        lTicks := GetTickCount;

        DC := GetDC(0);
        UpdateLayeredWindow(AnimForm.Handle, DC, nil, @FBmpSize, AnimBmp.Canvas.Handle, @FBmpTopLeft, clNone, @FBlend, ULW_ALPHA);
        UpdateShadowPos(aForm.Handle, acBounds(aForm.Left + Round(l), aForm.Top + Round(t), Round(r - l), Round(b - t)), FBlend.SourceConstantAlpha);
        ReleaseDC(0, DC);

        inc(i);
        Anim_GoToNext;
        if StepCount > 0 then
          WaitTicks(lTicks);
      end;
      FBlend.SourceConstantAlpha := MaxTransparency;
    end;
    SetWindowPos(AnimForm.Handle, 0, fR.Left{$IFDEF ACDEBUG} + DebugOffsX{$ENDIF}, fr.Top{$IFDEF ACDEBUG} + DebugOffsY{$ENDIF}, FBmpSize.cx, FBmpSize.cy, Flags or SWP_NOZORDER);
    DC := GetDC(0);
    UpdateLayeredWindow(AnimForm.Handle, DC, nil, @FBmpSize, acDstBmp.Canvas.Handle, @FBmpTopLeft, clNone, @FBlend, ULW_ALPHA);
    ReleaseDC(0, DC);
    FreeAndNil(AnimBmp);

    DoLayered(aForm.Handle, True, BlendValue);

    aForm.Visible := True;
    SetWindowPos(aForm.Handle, AnimForm.Handle, 0, 0, 0, 0, SWPA_ZORDER);
    InAnimationProcess := False;
    while not RedrawWindow(aForm.Handle, nil, 0, RDWA_ALLNOW and not RDW_FRAME) do;
    FreeAndNil(acDstBmp);

    if AeroIsEnabled then
      Sleep(2 * acTimerInterval); // Removing of blinking in Aero

    SetWindowPos(AnimForm.Handle, aForm.Handle, 0, 0, 0, 0, SWPA_ZORDER);
    FreeAndNil(AnimForm)
  end;
end;


{$IFNDEF AC_NOSHADOW}
type
  TacShadowForm = class(TacGlowForm)
  protected
    sd: TsCommonData;
    FOwnerForm: TForm;
    FDestroyed: boolean;
    OldWndProc: TWndMethod;
    function OwnerBlend: byte;
    procedure AC_WMNCHitTest(var Message: TMessage); message WM_NCHITTEST;
  public
    Locked: boolean;
    procedure HideWnd;
    procedure KillShadow;
    procedure UpdateShadowPos;
    function ShadowSize: TRect;
    function SkinManager: TsSkinManager;
    function ShadowTemplate: TBitmap;
    procedure NewWndProc(var Message: TMessage);
    constructor CreateNew(AOwner: TComponent; Dummy: Integer  = 0); override;
    procedure SetNewPos(aLeft, aTop, aWidth, aHeight: integer; BlendValue: byte);
  end;


procedure TacShadowForm.AC_WMNCHitTest(var Message: TMessage);
begin
  Message.Result := HTTRANSPARENT;
end;


constructor TacShadowForm.CreateNew(AOwner: TComponent; Dummy: Integer  = 0);
begin
  inherited;
  Tag := ExceptTag;
  FOwnerForm := TForm(AOwner);
  OldWndProc := FOwnerForm.WindowProc;
  FOwnerForm.WindowProc := NewWndProc;

  sd := GetCommonData(FOwnerForm.Handle);
  if sd <> nil then
    sd.WndProc := NewWndProc;

  FDestroyed := False;
end;


procedure TacShadowForm.HideWnd;
begin
  Hide;
  ShowWindow(Handle, SW_HIDE);
  Locked := True;
end;


procedure TacShadowForm.SetNewPos(aLeft, aTop, aWidth, aHeight: integer; BlendValue: byte);

  procedure UpdateLayer;
  var
    DC: hdc;
    cy, sbw: integer;
    ShSizes: TRect;
    FBmpSize: TSize;
    FBmpTopLeft: TPoint;
    FBlend: TBlendFunction;
    ShadowBmp, AlphaBmp: TBitmap;
  begin
    with ShadowSize do
      FBmpSize := MkSize(aWidth + Left + Right, aHeight + Top + Bottom);

    AlphaBmp := CreateBmp32(FBmpSize);
    // Paint shadow bmp
    ShadowBmp := ShadowTemplate;
    ShSizes := ShadowSize;
    FillRect32(AlphaBmp, MkRect(AlphaBmp), 0, 0);
    sbw := (ShadowBmp.Width - 1) div 2;
    cy := (ShadowBmp.Height - 1) div 2;
    PaintControlByTemplate(AlphaBmp, ShadowBmp, MkRect(AlphaBmp),
        MkRect(ShadowBmp),
        Rect(sbw, CY, sbw, CY),
        ShSizes, Rect(1, 1, 1, 1), False, False); // For internal shadows - stretch only allowed

    InitBlendData(FBlend, BlendValue);
    FBmpTopLeft := MkPoint;

    SetWindowLong(Handle, GWL_EXSTYLE, GetWindowLong(Handle, GWL_EXSTYLE) or WS_EX_LAYERED);

    DC := GetDC(0);
    try
      UpdateLayeredWindow(Handle, DC, nil, @FBmpSize, AlphaBmp.Canvas.Handle, @FBmpTopLeft, clNone, @FBlend, ULW_ALPHA);
    finally
      ReleaseDC(0, DC);
    end;
    with ShadowSize do
      SetBounds(aLeft - Left, aTop - Top, FBmpSize.cx, FBmpSize.cy);

    ShowWindow(Handle, SW_SHOWNOACTIVATE);
    SetWindowPos(Handle, FOwnerForm.Handle, 0, 0, 0, 0, SWPA_SHOWZORDERONLY);
    AlphaBmp.Free;
  end;

begin
  if not FDestroyed then
    UpdateLayer;
end;


function TacShadowForm.ShadowTemplate: TBitmap;
begin
  if (SkinManager = nil) or not SkinManager.CommonSkinData.Active then
    Result := rShadowBmp
  else
    Result := SkinManager.CommonSkinData.GetPassiveShadow;
end;


function TacShadowForm.SkinManager: TsSkinManager;
begin
  if sd = nil then
    Result := DefaultManager
  else
    Result := sd.SkinManager;
end;


function TacShadowForm.ShadowSize: TRect;
const
  sSize = 13;
begin
  if (SkinManager = nil) or not SkinManager.CommonSkinData.Active then
    Result := Rect(sSize, sSize, sSize, sSize)
  else
    with SkinManager.CommonSkinData do
      Result := Rect(WndShadowSize, max(0, WndShadowSize - WndShadowOffset), WndShadowSize, max(0, WndShadowSize + WndShadowOffset));
end;


procedure TacShadowForm.KillShadow;
var
  sd: TsCommonData;
begin
  if not FDestroyed then begin
    FDestroyed := True;
    FOwnerForm.WindowProc := OldWndProc;

    sd := GetCommonData(FOwnerForm.Handle);
    if (sd <> nil) and Assigned(sd.WndProc) then
      sd.WndProc := OldWndProc;

    OldWndProc := nil;
    FOwnerForm := nil;
    HideWnd;
    Release;
  end;
end;


procedure TacShadowForm.NewWndProc(var Message: TMessage);
begin
  case Message.Msg of
    SM_ALPHACMD:
      case Message.WParamHi of
        AC_UPDATESHADOW: begin
          Locked := False;
          with PacLayerPos(Message.LParam)^ do
            if Bounds.BHeight > 0 then
              SetNewPos(Bounds.BLeft, Bounds.BTop, Bounds.BWidth, Bounds.BHeight, LayerBlend);

          Exit;
        end;
      end;

    WM_CLOSE:
      HideWnd;

    WM_SHOWWINDOW:
      if Message.WParam = 0 then
        HideWnd;
  end;
  OldWndProc(Message);
  case Message.Msg of
    WM_WINDOWPOSCHANGED, WM_WINDOWPOSCHANGING, WM_SIZE:
      if not Locked and ((FOwnerForm = nil) or not IsWindowVisible(FOwnerForm.Handle)) then
        HideWnd
      else
        UpdateShadowPos;
  end;
end;


function TacShadowForm.OwnerBlend: byte;
begin
{$IFDEF DELPHI7UP}
  if FOwnerForm.AlphaBlend then
    Result := FOwnerForm.AlphaBlendValue
  else
{$ENDIF}
    Result := MaxByte;
end;


procedure TacShadowForm.UpdateShadowPos;
var
  OwnerBounds: TSrcRect;
begin
  if not Locked and (FOwnerForm <> nil) and FOwnerForm.HandleAllocated and IsWindowVisible(FOwnerForm.Handle) then begin
    GetWindowRect(FOwnerForm.Handle, TRect(OwnerBounds));
    with OwnerBounds do
      SetNewPos(SLeft, STop, FOwnerForm.Width, FOwnerForm.Height, OwnerBlend);
  end;
end;


function AttachShadowForm(aForm: TForm; DoShow: boolean = True): TCustomForm;
begin
  Result := TacShadowForm.CreateNew(aForm);
  TacShadowForm(Result).Locked := not DoShow;
end;
{$ENDIF} // AC_NOSHADOW


procedure TsPopupController.ShowForm(AForm: TForm; AOwnerControl: TWinControl; ALeft: integer = -1; ATop: integer = -1; Animated: boolean = True);
var
  c, h, HandlerIndex: integer;
  ctrlRect, formRect: TRect;
  ParentForm: TCustomForm;
  bAlphaBlendValue: byte;
  bAnimated: boolean;
  sp: TsSkinProvider;
begin
  if (AForm <> nil) and (AOwnerControl <> nil) and not SkipOpen then begin
    if (DefaultManager <> nil) and DefaultManager.Active and DefaultManager.Effects.AllowAnimation then
      bAnimated := True
    else
      bAnimated := False;

    sp := GetSkinProvider(aForm);
    if sp <> nil then begin
      bAnimated := bAnimated and sp.AllowAnimation;
      sp.AllowAnimation := False;
    end;

    AForm.Visible := False;
    AForm.Position := poDesigned;
{$IFDEF DELPHI7UP}
    bAlphaBlendValue := iff(AForm.AlphaBlend, AForm.AlphaBlendValue, MaxByte);
{$ELSE}
    bAlphaBlendValue := MaxByte;
{$ENDIF}
    if AForm.BorderStyle <> bsNone then begin
      h := AForm.Height;
      c := SysCaptHeight(AForm);
      AForm.BorderStyle := bsNone;
      AForm.Height := h - c;
    end;
    HandlerIndex := InitFormHandler(AForm, AOwnerControl);
{$IFNDEF AC_NOSHADOW}
    if FormHandlers[HandlerIndex].ShadowForm = nil then
      FormHandlers[HandlerIndex].ShadowForm := TacShadowForm(AttachShadowForm(AForm, False));
{$ENDIF}
    with FormHandlers[HandlerIndex] do begin
      if IgnoreCapture then
        CaptureHandle := 0
      else
        CaptureHandle := GetCapture;

      if CheckTimer = nil then begin
        CheckTimer := TacThreadedTimer.Create(nil);
        CheckTimer.Interval := 100;
        CheckTimer.OnTimer := DoTimer;
      end;
      CheckTimer.Enabled := True;
    end;
{$IFDEF DELPHI7UP}
    SetWindowLong(AForm.Handle, GWL_STYLE, GetWindowLong(AForm.Handle, GWL_STYLE) or WS_CLIPSIBLINGS or NativeInt(WS_POPUP));
{$ELSE}
    SetWindowLong(AForm.Handle, GWL_STYLE, GetWindowLong(AForm.Handle, GWL_STYLE) or Longint(WS_CLIPSIBLINGS or WS_POPUP));
{$ENDIF}
    SetWindowLong(AForm.Handle, GWL_EXSTYLE, GetWindowLong(AForm.Handle, GWL_EXSTYLE) or WS_EX_TOOLWINDOW);
    DoLayered(AForm.Handle, True);

    if ((sp = nil) or sp.AllowScale) and (DefaultManager <> nil) then
      DefaultManager.UpdateScale(AForm);

    GetWindowRect(AOwnerControl.Handle, ctrlRect);
    with TSrcRect(acWorkRect(ctrlRect.TopLeft)), TDstRect(formRect) do begin
      if (ALeft = -1) and (ATop = -1) then begin
        DTop := ctrlRect.Bottom - 2;
        if AOwnerControl.BidiMode = bdRightToLeft then
          DLeft := ctrlRect.Right - AForm.Width
        else
          DLeft := ctrlRect.Left;
      end
      else begin
        DTop := ATop;
        DLeft := ALeft;
      end;
      DBottom := DTop + AForm.Height;
      DRight := DLeft + AForm.Width;
      AForm.SetBounds(DLeft, DTop, AForm.Width, AForm.Height);
      if DBottom > SBottom then begin
        DBottom := ctrlRect.Top - 1;
        DTop := DBottom - AForm.ClientHeight;
        if DTop < STop then begin
          DTop := STop;
          DBottom := DTop + AForm.Height;
        end;
        AForm.SetBounds(DLeft, DTop, AForm.Width, AForm.Height);
        AnimDirection := 1;
      end
      else
        AnimDirection := 0;

      if DRight > SRight then begin
        DRight := SRight;
        DLeft := DRight - AForm.ClientWidth;
        AForm.SetBounds(DLeft, DTop, AForm.Width, AForm.Height);
        AnimDirection := AnimDirection or 2;
      end;
    end;

    ParentForm := FormHandlers[HandlerIndex].ParentForm;
    if (ParentForm is TForm) and (TForm(ParentForm).FormStyle = fsStayOnTop) then
      AForm.FormStyle := fsStayOnTop;

    SetWindowPos(AForm.Handle, HWND_TOPMOST, 0, 0, 0, 0, SWPA_SHOWZORDERONLY);

    AForm.Visible := True;
    if bAnimated then
      AnimShowPopup(AForm, 80, bAlphaBlendValue)
    else begin
      DoLayered(AForm.Handle, False);
      SetParentUpdated(AForm);
      if (sp <> nil) and sp.SkinData.Skinned then
        SetWindowPos(AForm.Handle, HWND_TOPMOST, 0, 0, 0, 0, SWP_SHOWWINDOW or SWPA_SHOWZORDERONLY)
      else
        ShowWindow(AForm.Handle, SW_SHOWNOACTIVATE);
{$IFNDEF AC_NOSHADOW}
      if HandlerIndex >= 0 then
        if FormHandlers[HandlerIndex].ShadowForm <> nil then
          TacShadowForm(FormHandlers[HandlerIndex].ShadowForm).Locked := False;
{$ENDIF}
    end;
    if sp <> nil then // If not animated
      sp.AllowAnimation := bAnimated;

{$IFNDEF AC_NOSHADOW}
    if not TacShadowForm(FormHandlers[HandlerIndex].ShadowForm).FDestroyed then
      TacShadowForm(FormHandlers[HandlerIndex].ShadowForm).UpdateShadowPos;
{$ENDIF}
  end;
  SkipOpen := False;
end;


procedure TsPopupController.ShowFormPos(AForm: TForm; ALeftTop: TPoint; Animated: boolean = True);
var
  c, h, HandlerIndex: integer;
  ctrlRect, formRect: TRect;
  ParentForm: TCustomForm;
  bAlphaBlendValue: byte;
  sp: TsSkinProvider;
  b: boolean;
begin
  if (AForm <> nil) and not SkipOpen then begin
    sp := GetSkinProvider(aForm);
    if sp <> nil then begin
      b := sp.AllowAnimation;
      sp.AllowAnimation := False;
    end
    else
      b := True;

    AForm.Visible := False;
    AForm.Position := poDesigned;
{$IFDEF DELPHI7UP}
    bAlphaBlendValue := iff(AForm.AlphaBlend, AForm.AlphaBlendValue, MaxByte);
{$ELSE}
    bAlphaBlendValue := MaxByte;
{$ENDIF}

    if AForm.BorderStyle <> bsNone then begin
      h := AForm.Height;
      c := SysCaptHeight(AForm);
      AForm.BorderStyle := bsNone;
      AForm.Height := h - c;
    end;
    HandlerIndex := InitFormHandler(AForm, nil);
    with FormHandlers[HandlerIndex] do begin
      if IgnoreCapture then
        CaptureHandle := 0
      else
        CaptureHandle := GetCapture;

      if CheckTimer = nil then begin
        CheckTimer := TacThreadedTimer.Create(nil);
        CheckTimer.Interval := 100;
        CheckTimer.OnTimer := DoTimer;
      end;
      CheckTimer.Enabled := True;
    end;
{$IFDEF DELPHI7UP}
    SetWindowLong(AForm.Handle, GWL_STYLE, GetWindowLong(AForm.Handle, GWL_STYLE) or WS_CLIPSIBLINGS or NativeInt(WS_POPUP));
{$ELSE}
    SetWindowLong(AForm.Handle, GWL_STYLE, GetWindowLong(AForm.Handle, GWL_STYLE) or Longint(WS_CLIPSIBLINGS or WS_POPUP));
{$ENDIF}

    SetWindowLong(AForm.Handle, GWL_EXSTYLE, GetWindowLong(AForm.Handle, GWL_EXSTYLE) or WS_EX_TOOLWINDOW);
    DoLayered(AForm.Handle, True);

    if DefaultManager <> nil then
      DefaultManager.UpdateScale(AForm);

    ctrlRect.TopLeft := ALeftTop;
    ctrlRect.BottomRight := ALeftTop;
    with TSrcRect(acWorkRect(ctrlRect.TopLeft)), TDstRect(formRect) do begin
      DTop := ctrlRect.Bottom - 2;
      DLeft := ctrlRect.Left;
      DBottom := DTop + AForm.Height;
      DRight := DLeft + AForm.Width;
      AForm.SetBounds(DLeft, DTop, AForm.Width, AForm.Height);
      if DBottom > SBottom then begin
        DBottom := ctrlRect.Top - 1;
        DTop := DBottom - AForm.ClientHeight;
        if DTop < STop then begin
          DTop := STop;
          DBottom := DTop + AForm.Height;
        end;
        AForm.SetBounds(DLeft, DTop, AForm.Width, AForm.Height);
        AnimDirection := 1;
      end
      else
        AnimDirection := 0;

      if DRight > SRight then begin
        DRight := SRight - 1;
        DLeft := DRight - AForm.ClientWidth;
        AForm.SetBounds(DLeft, DTop, AForm.Width, AForm.Height);
        AnimDirection := AnimDirection or 2;
      end;
    end;

    ParentForm := FormHandlers[HandlerIndex].ParentForm;
    if (ParentForm is TForm) and (TForm(ParentForm).FormStyle = fsStayOnTop) then
      AForm.FormStyle := fsStayOnTop;

    SetWindowPos(AForm.Handle, HWND_TOPMOST, 0, 0, 0, 0, SWPA_SHOWZORDERONLY);

    AForm.Visible := True;
    AnimShowPopup(AForm, 80, bAlphaBlendValue);

{$IFNDEF AC_NOSHADOW}
    if not TacShadowForm(FormHandlers[HandlerIndex].ShadowForm).FDestroyed then
      TacShadowForm(FormHandlers[HandlerIndex].ShadowForm).UpdateShadowPos;
{$ENDIF}
    if sp <> nil then
      sp.AllowAnimation := b;
  end;
  SkipOpen := False;
end;


procedure TacFormHandler.UpdateRgnBmp(aBmp: TBitmap);
var
  i: integer;
  sd: TsCommonData;
begin
  ClientCtrl := nil;
  for i := 0 to PopupForm.ControlCount - 1 do
    if (PopupForm.Controls[i].Align = alClient) and (PopupForm.Controls[i] is TWinControl) then begin
      ClientCtrl := TWinControl(PopupForm.Controls[i]);
      Break;
    end;

  if (ClientCtrl <> nil) and (ClientCtrl.Width = aBmp.Width) and (ClientCtrl.Height = aBmp.Height) then begin
    sd := TsCommonData(ClientCtrl.Perform(SM_ALPHACMD, AC_GETSKINDATA_HI, 0));
    if (sd <> nil) and sd.Skinned then
      GetTransCorners(sd.SkinIndex, aBmp, sd.SkinManager);
  end;
end;


type
  TAccessComboBox   = class(TsCustomComboBox);
  TAccessComboBoxEx = class(TsCustomComboBoxEx);

procedure TacFormHandler.CloseForm(CallProc: boolean = True);
var
  ca: TCloseAction;
begin
  CheckTimer.Enabled := False;
  if not Closed and not ClosingForbidden and (PopupForm <> nil) and PopupForm.CloseQuery then begin
    Closed := True;
    ca := caHide;
    if Assigned(OldOnClose) then
      OldOnClose(PopupForm, ca);

    if ca = caNone then
      Closed := False
    else begin
      if CallProc then
        case ca of
          caHide:
            PopupForm.Hide;

          caFree: begin
            PopupForm.Close;
//            PopupForm.Free;
            FreeAndNil(PopupForm);
          end;
        end;

      if PopupCtrl is TsCustomComboBox then
        with TAccessComboBox(PopupCtrl) do begin
          FDropDown := False;
          SkinData.Invalidate;
        end
      else
        PopupCtrl.Perform(SM_ALPHACMD, AC_POPUPCLOSED shl 16, LParam(PopupForm));

      if not Application.Active and (ParentForm <> nil) then
        SendMessage(ParentForm.Handle, WM_NCACTIVATE, 0, 0); // Update caption as inactive
    end;
    UnInitControls;
  end;
end;


constructor TacFormHandler.Create(AForm: TForm; ACtrl: TWinControl);
begin
  inherited Create;
  Closed := False;
  ClosingForbidden := False;
  PopupCtrl := nil;
{$IFNDEF AC_NOSHADOW}
  ShadowForm := nil;
{$ENDIF}
  InitControls(AForm, ACtrl);
end;


destructor TacFormHandler.Destroy;
begin
//  UnInitControls;
  FreeAndNil(CheckTimer);
  inherited;
end;


{$IFNDEF AC_NOHANDLECTRL}
procedure TacFormHandler.DoCtrlProc(var Message: TMessage);
var
  i: integer;
begin
{$IFDEF LOGGED}
//  AddToLog(Message);
{$ENDIF}
  case Message.Msg of
    CN_KEYDOWN:
      if not Closed and (Message.WParam = 27) then // Do not send to the parent form
        Exit
      else
        CtrlWndProc(Message);

    WM_DESTROY: begin
{$IFNDEF AC_NOSHADOW}
      if ShadowForm <> nil then begin
        TacShadowForm(ShadowForm).KillShadow;
        ShadowForm := nil;
      end;
{$ENDIF}

{$IFNDEF AC_NOHANDLEFORM}
      FormWndProc(Message);
{$ENDIF}
      UnInitControls;
      for i := 0 to Length(Controller.FormHandlers) do
        if Controller.FormHandlers[i] = Self then
          Controller.FormHandlers[i] := nil;

      Exit;
    end;

    WM_MOUSEWHEEL:
      PopupForm.DefaultHandler(Message);

    CM_FOCUSCHANGED, CM_CANCELMODE, WM_RBUTTONDOWN, WM_LBUTTONDOWN: begin
      if not Closed and (PopupForm <> nil) then
        CloseForm
      else
        CtrlWndProc(Message);
    end
    else
      CtrlWndProc(Message);
  end;
end;
{$ENDIF} // AC_NOHANDLECTRL


procedure TacFormHandler.DoTimer(Sender: TObject);
var
  CaptHandle: THandle;
begin
  if (PopupForm <> nil) and not Controller.MousePressed then
    if not Application.Active then
      Closeform
    else begin
      CaptHandle := GetCapture;
      if (SysPopupCount = 0) and (CaptHandle <> 0) and (CaptureHandle <> CaptHandle) then
        if not ContainsWnd(CaptHandle, PopupForm.Handle) and ((PopupCtrl = nil) or not ContainsWnd(CaptHandle, PopupCtrl.Handle)) then
          if not Controller.HasChild(PopupForm) then
            Closeform;
    end;
end;


{$IFNDEF AC_NOHANDLEFORM}
procedure TacFormHandler.DoWndProc(var Message: TMessage);
begin
{$IFDEF LOGGED}
  AddToLog(Message);
{$ENDIF}
  case Message.Msg of
    SM_ALPHACMD:
      case Message.WParamHi of
        AC_REMOVESKIN: begin
          if Assigned(FormWndProc) then
            FormWndProc(Message);

          UnInitControls;
          Exit;
        end

        else
          FormWndProc(Message);
      end;

    WM_DESTROY: begin
      if Assigned(FormWndProc) then
        FormWndProc(Message);

      UnInitControls;
      Exit;
    end;

    WM_WINDOWPOSCHANGING: begin
      FormWndProc(Message);
      if (TWMWindowPosMsg(Message).WindowPos.Flags = SWP_NOACTIVATE or SWP_NOMOVE or SWP_NOSIZE) and (TWMWindowPosMsg(Message).WindowPos.hwnd <> PopupForm.Handle) then
        Closeform;
    end;

    WM_MOUSEACTIVATE: begin
      FormWndProc(Message);
      Message.Result := MA_NOACTIVATE
    end

    else
      if Assigned(FormWndProc) then
        FormWndProc(Message);
  end;
end;
{$ENDIF} // AC_NOHANDLEFORM


procedure TacFormHandler.InitControls(AForm: TForm; ACtrl: TWinControl);
var
  sd: TsCommonData;
  sp: TsSkinProvider;
begin
  Closed := False;
  PopupForm := AForm;
  PopupCtrl := ACtrl;
{$IFNDEF AC_NOHANDLEFORM}
  sp := GetSkinProvider(AForm);
  if sp <> nil then
    FreeAndNil(sp.ListSW);

  FormWndProc := AForm.WindowProc;
  AForm.WindowProc := DoWndProc;
  sd := GetCommonData(AForm.Handle);
  if sd <> nil then
    sd.WndProc := DoWndProc;
{$ENDIF}

  if ACtrl <> nil then begin
{$IFNDEF AC_NOHANDLECTRL}
    CtrlWndProc := ACtrl.WindowProc;
    PopupCtrl.WindowProc := DoCtrlProc;
{$ENDIF} // AC_NOHANDLECTRL
    ParentForm := TForm(GetParentForm(ACtrl));
  end
  else
    ParentForm := nil;

  Initialized := True;
end;


procedure TacFormHandler.UnInitControls;
var
  sd: TsCommonData;
  sw: TacScrollWnd;
begin
  if Initialized then begin
{$IFNDEF AC_NOHANDLEFORM}
    if PopupForm <> nil then begin
      if not (csDestroying in PopupForm.ComponentState) then begin
        sw := Ac_GetScrollWndFromHwnd(PopupForm.Handle);
        if sw <> nil then
          UninitializeACWnd(PopupForm.Handle, False, False, TacMainWnd(sw));

        if ShadowForm <> nil then begin
          TacShadowForm(ShadowForm).KillShadow;
          ShadowForm := nil;
        end;                                  
        sd := GetCommonData(PopupForm.Handle);
        if sd <> nil then
          sd.WndProc := nil; // FormWndProc;
      end;
      PopupForm.WindowProc := FormWndProc;
    end;
    FormWndProc := nil;
{$ENDIF}
{$IFNDEF AC_NOHANDLECTRL}
    if PopupCtrl <> nil then
      PopupCtrl.WindowProc := CtrlWndProc;
{$ENDIF}
    if PopupForm <> nil then begin
      PopupForm.OnDeactivate := nil;
      if Assigned(OldOnClose) then
        PopupForm.OnClose := OldOnClose
      else
        PopupForm.OnClose := nil;
    end;
    PopupForm := nil;
    PopupCtrl := nil;
    Initialized := False;
  end;
end;


function TsPopupController.InitFormHandler(AForm: TForm; Ctrl: TWinControl): integer;
begin
  Result := GetFormIndex(AForm);
  if Result < 0 then begin
    Result := Length(FormHandlers);
    SetLength(FormHandlers, Result + 1);
    FormHandlers[Result] := TacFormHandler.Create(AForm, Ctrl);
    FormHandlers[Result].Controller := Self;

    AForm.OnDeactivate := DoDeactivate;

    if Assigned(AForm.OnClose) and not Assigned(FormHandlers[Result].OldOnClose) then
      FormHandlers[Result].OldOnClose := AForm.OnClose;

    AForm.OnClose := DoClose;
  end;
  FormHandlers[Result].Closed := False;
{$IFNDEF AC_NOSHADOW}
    if FormHandlers[Result].ShadowForm = nil then
      FormHandlers[Result].ShadowForm := TacShadowForm(AttachShadowForm(AForm, False));
{$ENDIF}
end;


initialization
  rShadowBmp := MakeShadow($44000000, 17, 2, 8, 0, 0);


finalization
  FreeAndNil(rShadowBmp);
  FreeAndNil(acIntController);

end.

