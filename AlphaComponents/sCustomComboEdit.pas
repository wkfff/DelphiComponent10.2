unit sCustomComboEdit;
{$I sDefs.inc}
//{$DEFINE LOGGED}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, StdCtrls, Mask, buttons, menus, ExtCtrls,
  {$IFDEF TNTUNICODE} TntControls, {$ENDIF}
  {$IFNDEF DELPHI5} Types, {$ENDIF}
  {$IFDEF DELPHI6UP} Variants, {$ENDIF}
  acntUtils, sConst, sSpeedButton, sGraphUtils, sCommonData, sDefaults, sMaskEdit,
  sSkinProps, sGlyphUtils;


type
{$IFNDEF NOTFORHELP}
  TCloseUpEvent = procedure (Sender: TObject; Accept: Boolean) of object;
{$ENDIF} // NOTFORHELP

  TacHintTimer = class(TTimer)
  protected
    FOwner: TComponent;
  public
    procedure TimerEvent(Sender: TObject);
    constructor Create(AOwner: TComponent); override;
  end;


  TsCustomComboEdit = class(TsMaskEdit)
{$IFNDEF NOTFORHELP}
  private
    FBtnState,
    FPopupWidth,
    FPopupHeight: integer;

    FReadOnly,
    FDirectInput,
    FShowButton: boolean;
{$IFDEF TNTUNICODE}
    HintWnd: TTntHintWindow;
{$ELSE}
    HintWnd: THintWindow;
{$ENDIF}

    FBtnRect: TRect;
    FClickKey: TShortCut;
{$IFNDEF D2009}
    FAlignment: TAlignment;
{$ENDIF}
    BtnTimer: TTimer;
    HintTimer: TacHintTimer;
    FGlyphMode: TsGlyphMode;
    FDisabledKind: TsDisabledKind;
    FPopupWindowAlign: TPopupWindowAlign;
    function GetDroppedDown: boolean;
    procedure SetDirectInput(Value: boolean);
{$IFNDEF D2009}
    procedure SetAlignment(Value: TAlignment);
{$ENDIF}
    procedure SetDisabledKind(const Value: TsDisabledKind);
    procedure SetShowButton  (const Value: boolean);
    procedure WMNCCalcSize    (var Message: TWMNCCalcSize);   message WM_NCCALCSIZE;
    procedure CMFocuseChanged (var Message: TCMFocusChanged); message CM_FOCUSCHANGED;
    procedure WMNCHitTest     (var Message: TWMNCHitTest);    message WM_NCHITTEST;
    procedure WMNCLButtonDown (var Message: TWMLButtonDown);  message WM_NCLBUTTONDOWN;
    procedure WMNCLButtonUp   (var Message: TWMLButtonUp);    message WM_NCLBUTTONUP;
    procedure WMNCLDblClick   (var Message: TWMMouse);        message WM_NCLBUTTONDBLCLK;
    procedure CMCancelMode    (var Message: TCMCancelMode);   message CM_CANCELMODE;
    procedure WMPaste         (var Message: TWMPaste);        message WM_PASTE;
    procedure WMCut           (var Message: TWMCut);          message WM_CUT;
    procedure SetBtnState(const Value: integer);
    function GlyphSpace: integer;
    function BtnOffset: integer;
    function GlyphEnabled: boolean;
    function GetBtnState: integer;
    procedure CacheToDC(R: TRect);
    procedure TryShowBtnHint;
    procedure HideHint;
  protected
    FDroppedDown: boolean;
    FOnButtonClick: TNotifyEvent;
    procedure PaintText; override;
    function TextRect: TRect; override;
    function MouseInRect(R: TRect): boolean;

    procedure PaintBorder(DC: hdc); override;
    procedure PaintBtn;
    procedure OurPaintHandler(DC: hdc); override;

    procedure SetReadOnly(Value: Boolean); virtual;
    function GetReadOnly: Boolean; virtual;
    procedure KeyPress(var Key: Char); override;
    function BordWidth: integer;
    function AllowBtnStyle: boolean;
    function BtnRect: TRect;
    procedure PaintBtnStd; virtual;

    function GlyphWidth: integer;
    function GlyphHeight: integer;

    procedure PopupWindowShow; virtual;
    procedure PopupWindowClose; virtual;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure StartBtnTimer;
    procedure MouseLeaveTimer(Sender: TObject);

    procedure Change; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure ButtonClick; dynamic;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function PrepareCache: boolean; override;
{$IFNDEF D2009}
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
{$ENDIF}
    property PopupAlign: TPopupWindowAlign read FPopupWindowAlign write FPopupWindowAlign default pwaRight;
    property BtnState: integer read GetBtnState write SetBtnState default 0;
  public
    FDefBmpID: integer;
    FPopupWindow: TWinControl;
    constructor Create(AOwner: TComponent); override;
    function ComboBtn: boolean;
    destructor Destroy; override;
    procedure AfterConstruction; override;
    procedure Loaded; override;

    procedure WndProc (var Message: TMessage); override;
    procedure DoClick;
    procedure SelectAll; virtual;
    property DroppedDown: Boolean read GetDroppedDown;
    property PopupWidth: integer read FPopupWidth write FPopupWidth default 230;
    property PopupHeight: integer read FPopupHeight write FPopupHeight default 166;
{$ENDIF} // NOTFORHELP
  published
{$IFNDEF NOTFORHELP}
    property Align;
    property Anchors;
    property AutoSelect;
    property DragCursor;
    property DragMode;
    property EditMask;
    property Enabled;
    property Font;
    property HideSelection;
    property ImeMode;
    property ImeName;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property OnContextPopup;
{$ENDIF} // NOTFORHELP
    property OnButtonClick: TNotifyEvent read FOnButtonClick write FOnButtonClick;
    property ClickKey: TShortCut read FClickKey write FClickKey default scAlt + vk_Down;
    property DisabledKind: TsDisabledKind read FDisabledKind write SetDisabledKind default DefDisabledKind;
    property GlyphMode: TsGlyphMode read FGlyphMode write FGlyphMode;

    property DirectInput: boolean read FDirectInput write SetDirectInput default True;
    property ShowButton: boolean read FShowButton write SetShowButton default True;
    property ReadOnly: boolean read GetReadOnly write SetReadOnly default False;
  end;


implementation

uses
  math,
  {$IFDEF LOGGED}sDebugMsgs, {$ENDIF}
  {$IFDEF DELPHI7UP} Themes, {$ENDIF}
  sMaskData, sToolEdit, sVCLUtils, sMessages, sAlphaGraph, sThirdParty, acSBUtils,
  acPopupController, sStyleSimply, sSkinManager, acntTypes, acAlphaImageList;


constructor TsCustomComboEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SkinData.COC := COC_TsComboEdit;
  FDisabledKind := DefDisabledKind;
{$IFNDEF D2009}
  FAlignment := taLeftJustify;
{$ENDIF}
  FDirectInput := True;
  FClickKey := scAlt + vk_Down;
  FPopupWindowAlign := pwaRight;
  FPopupWindow := nil;
  FShowButton := True;
  BtnTimer := nil;
  HintWnd := nil;

  FDefBmpID := iBTN_ELLIPSIS;
  FDroppedDown := False;
  FBtnRect.Left := -1;
  FBtnState := 0;
  FGlyphMode := TsGlyphMode.Create(Self);

  Height := 21;
  FPopupWidth := 230;
  FPopupHeight := 166;
end;


destructor TsCustomComboEdit.Destroy;
begin
  OnKeyDown := nil;
  FreeAndNil(FGlyphMode);
  FreeAndNil(BtnTimer);
  inherited Destroy;
end;


procedure TsCustomComboEdit.PopupWindowShow;
var
  P: TPoint;
  Y: Integer;
  Flags: Cardinal;
  Form: TCustomForm;
  ActAlign: TPopupWindowAlign;
begin
  if (FPopupWindow <> nil) and not (ReadOnly or DroppedDown) then begin
    FPopupWindow.Visible := False;
    FPopupWindow.Width := FPopupWidth;
    FPopupWindow.Height := FPopupHeight;
    P := Parent.ClientToScreen(Point(Left, Top));
    Y := P.Y + Height;

    if Y + FPopupWindow.Height > Screen.DesktopHeight then
      Y := P.Y - FPopupWindow.Height;

    ActAlign := FPopupWindowAlign;
    if BiDiMode = bdRightToLeft then begin
      case FPopupWindowAlign of
        pwaRight: ActAlign := pwaLeft;
        pwaLeft:  ActAlign := pwaRight;
      end;
      ReflectControls(FPopupWindow, True);
    end;

    case ActAlign of
      pwaRight: begin
        Dec(P.X, FPopupWindow.Width - Width);
        if P.X < Screen.DesktopLeft then
          Inc(P.X, FPopupWindow.Width - Width);
      end;

      pwaLeft:
        if P.X + FPopupWindow.Width > Screen.DesktopWidth then
          Dec(P.X, FPopupWindow.Width - Width);
    end;
    if P.X < Screen.DesktopLeft then
      P.X := Screen.Desktopleft
    else
      if P.X + FPopupWindow.Width > Screen.DesktopWidth then
        P.X := Screen.DesktopWidth - FPopupWindow.Width;

    Form := GetParentForm(Self);
    if CanFocus then begin
      SetFocus;
      ValidateRect(Handle, nil);
    end;

    if Form <> nil then begin
      if (FPopupWindow is TForm) and (TForm(Form).FormStyle = fsStayOnTop) then
        TForm(FPopupWindow).FormStyle := fsStayOnTop;

      Flags := SWPA_ZORDER;
      SetWindowPos(FPopupWindow.Handle, HWND_TOPMOST, P.X, Y, FPopupWindow.Width, FPopupWindow.Height, Flags);
    end;
    if FPopupWindow is TForm then
      ShowPopupForm(TForm(FPopupWindow), Self)
    else
      FPopupWindow.Visible := True;
  end;
end;


function TsCustomComboEdit.PrepareCache: boolean;
begin
  Result := inherited PrepareCache;
  if Result and FShowButton then
    PaintBtn;
end;


procedure TsCustomComboEdit.CacheToDC(R: TRect);
var
  DC: hdc;
  bWidth: integer;
begin
  if SkinData.FCacheBmp <> nil then begin
    DC := GetWindowDC(Handle);
    try
      if not SkinData.Skinned then begin
        bWidth := BordWidth;
        ExcludeClipRect(DC, 0, 0, Width, bWidth);
        ExcludeClipRect(DC, 0, bWidth, bWidth, Height);
        ExcludeClipRect(DC, bWidth, Height - bWidth, Width, Height);
        ExcludeClipRect(DC, Width - bWidth, bWidth, Width, Height - bWidth);
      end;
      BitBlt(DC, R.Left, R.Top, WidthOf(R), HeightOf(R), SkinData.FCacheBmp.Canvas.Handle, R.Left, R.Top, SRCCOPY);
    finally
      ReleaseDC(DC, Handle);
    end;
  end;
end;


procedure TsCustomComboEdit.Change;
begin
  if not DroppedDown then
    inherited Change;
end;


procedure TsCustomComboEdit.KeyDown(var Key: Word; Shift: TShiftState);
var
  M: tagMsg;
  sc: TShortCut;
begin
  inherited KeyDown(Key, Shift);
  sc := ShortCut(Key, Shift);
  if (sc = FClickKey) and (GlyphWidth > 0) then begin
    if GlyphEnabled then
      ButtonClick;

    Key := 0;
  end
  else
    if sc = scCtrl + ord('A') then begin
      SelectAll;
      Key := 0;
      PeekMessage(M, Handle, WM_CHAR, WM_CHAR, PM_REMOVE);
    end;
end;


procedure TsCustomComboEdit.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if not RestrictDrawing then
    SkinData.BGChanged := True;

  inherited MouseDown(Button, Shift, X, Y);
  if DroppedDown then
    PopupWindowClose;
end;


function TsCustomComboEdit.MouseInRect(R: TRect): boolean;
var
  p: TPoint;
  WndRect: TRect;
begin
  GetWindowRect(Handle, WndRect);
  p := acMousePos;
  dec(p.X, WndRect.Left);
  dec(p.Y, WndRect.Top);
  Result := PtInRect(R, p);
end;


procedure TsCustomComboEdit.MouseLeaveTimer(Sender: TObject);
begin
  if SkinData.FMouseAbove and not acMouseInControl(Self) then begin
    BtnTimer.Enabled := False;
    SendMessage(Handle, CM_MOUSELEAVE, 0, 0);
  end;
end;


function TsCustomComboEdit.GetBtnState: integer;
begin
  if DroppedDown and ComboBtn then
    Result := 2
  else
    Result := FBtnState;
end;


function TsCustomComboEdit.GetDroppedDown: Boolean;
begin
  Result := FDroppedDown or (FPopupWindow <> nil) and FPopupWindow.Visible;
end;


procedure TsCustomComboEdit.CMCancelMode(var Message: TCMCancelMode);
begin
  with Message do
    if (Sender <> Self) and
         (Sender <> FPopupWindow) and
           (FPopupWindow <> nil) and IsWindowVisible(FPopupWindow.Handle) and
             not FPopupWindow.ContainsControl(Sender) then
      PopupWindowClose;
end;


procedure TsCustomComboEdit.DoClick;
begin
  if GlyphEnabled and not ComboBtn then
    ButtonClick;
end;


function TsCustomComboEdit.AllowBtnStyle: boolean;
begin
  Result := False;
end;


function TsCustomComboEdit.BordWidth: integer;
begin
  if not SkinData.Skinned {$IFDEF DELPHI7UP} and acThemesEnabled {$ENDIF} then
    Result := 0
  else
    Result := integer(BorderStyle <> bsNone) * 2;
end;


function TsCustomComboEdit.BtnOffset: integer;
begin
  if not ac_OldGlyphsMode or ComboBtn then
    if SkinData.Skinned then
      Result := SkinData.SkinManager.CommonSkinData.ComboBoxMargin
    else
      Result := BordWidth
  else
    Result := BordWidth;
end;


function TsCustomComboEdit.BtnRect: TRect;
var
  bWidth, w: integer;
begin
  if FBtnRect.Left < 0 then begin
    w := GlyphSpace;
    bWidth := BtnOffset;

    if IsRightToLeft then
      FBtnRect.Left := bWidth
    else
      FBtnRect.Left := Width - bWidth - w;

    FBtnRect.Right := FBtnRect.Left + w;
    FBtnRect.Top := bWidth;
    FBtnRect.Bottom := Height - bWidth;
  end;
  Result := FBtnRect;
end;


procedure TsCustomComboEdit.PaintBtnStd;
var
  DC: hdc;
  bWidth: integer;
begin
  if not SkinData.Skinned then begin
    bWidth := BordWidth;
    DC := GetWindowDC(Handle);
    try
      PrepareCache;
      if not SkinData.Skinned then begin
        ExcludeClipRect(DC, 0, 0, Width, bWidth);
        ExcludeClipRect(DC, 0, bWidth, bWidth, Height);
        ExcludeClipRect(DC, bWidth, Height - bWidth, Width, Height);
        ExcludeClipRect(DC, Width - bWidth, bWidth, Width, Height - bWidth);
      end;
      BitBlt(DC, BtnRect.Left - bWidth, BtnRect.Top, WidthOf(BtnRect) + 2 * bWidth, HeightOf(BtnRect), SkinData.FCacheBmp.Canvas.Handle,
                 BtnRect.Left - bWidth, BtnRect.Top, SRCCOPY);
    finally
      ReleaseDC(Handle, DC);
    end;
  end;
end;


procedure TsCustomComboEdit.ButtonClick;
begin
  if Assigned(FOnButtonClick) then
    FOnButtonClick(Self);

  if DroppedDown then
    PopupWindowClose
  else
    PopupWindowShow;
end;


procedure TsCustomComboEdit.SelectAll;
begin
  if Text <> '' then
    SendMessage(Handle, EM_SETSEL, 0, -1);
end;


procedure TsCustomComboEdit.SetDirectInput(Value: Boolean);
begin
  inherited ReadOnly := not Value or FReadOnly;
  FDirectInput := Value;
end;


procedure TsCustomComboEdit.WMPaste(var Message: TWMPaste);
begin
  if FDirectInput and not ReadOnly then
    inherited;
end;


procedure TsCustomComboEdit.WMCut(var Message: TWMCut);
begin
  if FDirectInput and not ReadOnly then
    inherited;
end;


procedure TsCustomComboEdit.WMNCLButtonDown(var Message: TWMLButtonDown);
var
  oldR: TRect;
begin
  if not (csDesigning in ComponentState) then begin
    oldR := BtnRect;
    if MouseInRect(oldR) and GlyphEnabled then begin
      BtnState := 2;
      if not SkinData.Skinned then
        CacheToDC(oldR);

      if ComboBtn then
        ButtonClick;
    end;
  end
  else
    inherited;
end;


procedure TsCustomComboEdit.WMNCLButtonUp(var Message: TWMLButtonUp);
var
  oldR: TRect;
begin
  if not (csDesigning in ComponentState) then begin
    if (FBtnState = 2) and MouseInRect(BtnRect) then
      if GlyphEnabled and not ComboBtn then begin
        oldR := BtnRect;
        FBtnState := 1;
        SkinData.Invalidate(True);
        if not SkinData.Skinned then
          CacheToDC(oldR);

        ButtonClick;
      end;
  end
  else
    inherited;
end;


procedure TsCustomComboEdit.WMNCLDblClick(var Message: TWMMouse);
begin
  if not (csDesigning in ComponentState) then begin
    if MouseInRect(BtnRect) then
      WMNCLButtonDown(Message);
  end
  else
    inherited;
end;


procedure TsCustomComboEdit.WMNCCalcSize(var Message: TWMNCCalcSize);
begin
  inherited;
  if ShowButton then
    with Message.CalcSize_Params.rgrc[0] do
      if IsRightToLeft then
        inc(Left, GlyphSpace)
      else
        dec(Right, GlyphSpace);
end;


procedure TsCustomComboEdit.WMNCHitTest(var Message: TWMNCHitTest);
begin
  inherited;
  if not (csDesigning in ComponentState) and ShowButton then
    if MouseInRect(BtnRect) then begin
      Message.Result := HTBORDER;
      if (BtnState = 0) then begin
        if GlyphEnabled then begin
          Perform(CM_MOUSEENTER, 0, 0);
          BtnState := 1;
        end;
        StartBtnTimer;
        if HintTimer = nil then
          HintTimer := TacHintTimer.Create(Self)
        else
          HintTimer.Enabled := True;
      end;
    end
    else begin
      BtnState := 0;
      HideHint;
    end;
end;


procedure TsCustomComboEdit.SetReadOnly(Value: Boolean);
begin
  if Value <> FReadOnly then begin
    FReadOnly := Value;
    inherited ReadOnly := Value or not FDirectInput;
  end;
end;


{$IFNDEF D2009}
procedure TsCustomComboEdit.SetAlignment(Value: TAlignment);
begin
  if FAlignment <> Value then begin
    FAlignment := Value;
    if HandleAllocated and Visible then
      SkinData.Invalidate;
  end;
end;
{$ENDIF}


procedure TsCustomComboEdit.SetBtnState(const Value: integer);
begin
  if (FBtnState <> 2) or not DroppedDown then // If not pressed
    if FBtnState <> Value then begin
      FBtnState := Value;
      SkinData.Invalidate(True);
    end;
end;


procedure TsCustomComboEdit.WndProc(var Message: TMessage);
var
  DC: hdc;
  bWidth: integer;
begin
{$IFDEF LOGGED}
  AddToLog(Message);
{$ENDIF}
  case Message.Msg of
    SM_ALPHACMD:
      case Message.WParamHi of
        AC_POPUPCLOSED: begin
          PopupWindowClose;
          Exit;
        end;
      end;

    WM_ERASEBKGND:
      if SkinData.Skinned and IsWindowVisible(Handle) then
        if InUpdating(SkinData) then
          Exit;

    CM_MOUSEENTER:
      if SkinData.FMouseAbove then
        Exit;

    CM_MOUSELEAVE: begin
      BtnState := 0;
      HideHint;
    end;

    WM_PRINT:
      if SkinData.Skinned then begin
        SkinData.Updating := False;
        DC := TWMPaint(Message).DC;
        if SkinData.BGChanged then
          PrepareCache;

        UpdateCorners(SkinData, 0);
        OurPaintHandler(DC);
        bWidth := BordWidth;
        BitBltBorder(DC, 0, 0, SkinData.FCacheBmp.Width, SkinData.FCacheBmp.Height, SkinData.FCacheBmp.Canvas.Handle, 0, 0, bWidth);
        BitBlt(DC, BtnRect.Left, BtnRect.Top, GlyphSpace, GlyphHeight, SkinData.FCacheBmp.Canvas.Handle, BtnRect.Left, BtnRect.Top, SRCCOPY);
        Exit;
      end;

    WM_KILLFOCUS:
      if (Self is TsDateEdit) and (FPopupWindow <> nil) then
        TForm(FPopupWindow).Close;

    WM_NCPAINT:
      if not SkinData.Skinned then begin
        inherited;
        PaintBtnStd;
        Exit;
      end;
  end;
  inherited;
  case Message.Msg of
    SM_ALPHACMD:
      case Message.WParamHi of
        AC_REMOVESKIN, AC_SETNEWSKIN, AC_REFRESH: begin
          if ACUInt(Message.LParam) = ACUInt(SkinData.SkinManager) then
            AlphaBroadcast(Self, Message);

          if (AC_REFRESH = Message.WParamHi) then begin
            if not HandleAllocated then
              HandleNeeded;
              
            FBtnRect.Left := -1;
            SetWindowPos(Handle, 0, 0, 0, 0, 0, SWPA_FRAMECHANGED);
          end;
        end;

        AC_PREPARING:
          Message.Result := 0;
      end;

    WM_SETFOCUS:
      if AutoSelect then
        SelectAll;

    CM_EXIT:
      Repaint;

    WM_SIZE, WM_WINDOWPOSCHANGED:
      FBtnRect.Left := -1;
  end
end;


procedure TsCustomComboEdit.PopupWindowClose;
begin
  if Assigned(FPopupWindow) and TForm(FPopupWindow).Visible then
    TForm(FPopupWindow).Close;
end;


procedure TsCustomComboEdit.CreateParams(var Params: TCreateParams);
const
  Alignments: array[TAlignment] of Longword = (ES_LEFT, ES_RIGHT, ES_CENTER);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or Alignments[Alignment] or WS_CLIPCHILDREN;
end;


procedure TsCustomComboEdit.KeyPress(var Key: Char);
var
  Form: TCustomForm;
begin
  if (Key = Char(VK_RETURN)) or (Key = Char(VK_ESCAPE)) then begin
    { must catch and remove this, since is actually multi-line }
    Form := GetParentForm(Self);
    if Form.KeyPreview then
      Form.Perform(CM_DIALOGKEY, Byte(Key), 0);
      
    if Key = Char(VK_RETURN) then begin
      inherited KeyPress(Key);
      Key := #0;
      Exit;
    end;
  end;
  inherited KeyPress(Key);
end;


procedure TsCustomComboEdit.AfterConstruction;
begin
  inherited;
  SkinData.Loaded;
end;


procedure TsCustomComboEdit.Loaded;
begin
  inherited;
  SkinData.Loaded;
end;


function TsCustomComboEdit.GetReadOnly: Boolean;
begin
  Result := FReadOnly;
end;


function TsCustomComboEdit.GlyphEnabled: boolean;
begin
  Result := Enabled and not FReadOnly and GlyphMode.Enabled;
end;


function TsCustomComboEdit.GlyphHeight: integer;
begin
  if ComboBtn then
    if SkinData.Skinned then
      Result := Height - 2 * SkinData.SkinManager.CommonSkinData.ComboBoxMargin
    else
      Result := Height
  else
    Result := FGlyphMode.Height;
end;


function TsCustomComboEdit.GlyphSpace: integer;
begin
  if ComboBtn then
    Result := GetComboBtnSize(SkinData.SkinManager) - 1
  else
    Result := integer(FShowButton) * (GlyphWidth + acSpacing {* 3 div 2});
end;


function TsCustomComboEdit.GlyphWidth: integer;
begin
  if ComboBtn then
    Result := GetComboBtnSize(SkinData.SkinManager)
  else
    Result := FGlyphMode.Width;
end;


procedure TsCustomComboEdit.HideHint;
begin
  acHideHintWnd(HintWnd);
end;


procedure TsCustomComboEdit.SetDisabledKind(const Value: TsDisabledKind);
begin
  if FDisabledKind <> Value then begin
    FDisabledKind := Value;
    SkinData.Invalidate;
  end;
end;


procedure TsCustomComboEdit.OurPaintHandler(DC: hdc);
var
  NewDC, SavedDC: hdc;
  PS: TPaintStruct;
begin
  if not InAnimationProcess then
    BeginPaint(Handle, PS);

  if DC = 0 then
    NewDC := GetWindowDC(Handle)
  else
    NewDC := DC;

  SavedDC := SaveDC(NewDC);
  try
    if SkinData.Skinned then begin
      if not InUpdating(SkinData) then begin
        SkinData.BGChanged := SkinData.BGChanged or SkinData.HalfVisible or GetBoolMsg(Parent, AC_GETHALFVISIBLE);
        SkinData.HalfVisible := not RectInRect(Parent.ClientRect, BoundsRect);
        if SkinData.BGChanged then
          PrepareCache;

        UpdateCorners(SkinData, 0);
        BitBlt(NewDC, 0, 0, Width, Height, SkinData.FCacheBmp.Canvas.Handle, 0, 0, SRCCOPY);
      end;
    end
    else begin
      PrepareCache;
      BitBlt(NewDC, 3, 3, Width - 6, Height - 6, SkinData.FCacheBmp.Canvas.Handle, 3, 3, SRCCOPY);
    end;
  finally
    RestoreDC(NewDC, SavedDC);
    if DC = 0 then
      ReleaseDC(Handle, NewDC);

    if not InAnimationProcess then
      EndPaint(Handle, PS);
  end;
end;


procedure TsCustomComboEdit.PaintBorder(DC: hdc);
var
  NewDC, SavedDC: HDC;
begin
  if Assigned(Parent) and Visible and Parent.Visible and not (csCreating in ControlState) and not SkinData.Updating then begin
    if DC = 0 then
      NewDC := GetWindowDC(Handle)
    else
      NewDC := DC;

    SavedDC := SaveDC(NewDC);
    try
      if SkinData.BGChanged then
        PrepareCache;

      UpdateCorners(SkinData, 0);
      BitBltBorder(NewDC, 0, 0, Width, Height, SkinData.FCacheBmp.Canvas.Handle, 0, 0, BordWidth);
      if FShowButton then
        if IsRightToLeft then
          BitBlt(NewDC, BtnOffset, 0, GlyphSpace, Height, SkinData.FCacheBmp.Canvas.Handle, BtnOffset, 0, SRCCOPY)
        else
          BitBlt(NewDC, BtnRect.Left - acSpacing div 2, 0, GlyphSpace + acSpacing div 2, Height, SkinData.FCacheBmp.Canvas.Handle, BtnRect.Left - acSpacing div 2, 0, SRCCOPY);

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


{$IFDEF DELPHI7UP}
const
  {$IFDEF DELPHI_XE2}
  ComboStates: array[0..2] of TThemedComboBox = (tcDropDownButtonRightNormal, tcDropDownButtonRightHot, tcDropDownButtonRightPressed);
  {$ELSE}
  ComboStates: array[0..2] of TThemedComboBox = (tcDropDownButtonNormal, tcDropDownButtonHot, tcDropDownButtonPressed);
  {$ENDIF}
  BtnStates: array[0..2] of TThemedHeader = (thHeaderItemNormal, thHeaderItemHot, thHeaderItemPressed);
{$ENDIF}


procedure TsCustomComboEdit.PaintBtn;
var
  DrawData: TacDrawGlyphData;
  w, Mode, Ndx: integer;
  C: TColor;
  R: TRect;
{$IFDEF DELPHI7UP}
  cBox: TThemedComboBox;
  cBtn: TThemedHeader;
  Details: TThemedElementDetails;
{$ENDIF}
begin
  R := BtnRect;
  if ComboBtn then begin
    with SkinData do
      if Skinned then begin
        Mode := min(BtnState, ac_MaxPropsIndex);
        with SkinManager, ConstData.ComboBtn do
          if not AllowBtnStyle then begin
            Ndx := SkinData.SkinManager.ConstData.Sections[ssComboBtn];
            if Ndx >= 0 then
              PaintItem(Ndx, MakeCacheInfo(FCacheBmp), True, BtnState, R, MkPoint, FCacheBmp, SkinData.SkinManager);

            if not gd[SkinData.SkinIndex].GiveOwnFont and IsValidImgIndex(GlyphIndex) then
              DrawSkinGlyph(FCacheBmp, Point(R.Left + (WidthOf(R) - ma[GlyphIndex].Width) div 2,
                            (Height - ma[GlyphIndex].Height) div 2), Mode, 1, ma[GlyphIndex], MakeCacheInfo(FCacheBmp))
            else begin // Paint without glyph
              if Ndx >= 0 then // If COMBOBTN used
                C := SkinData.SkinManager.gd[Ndx].Props[Mode].FontColor.Color
              else
                if SkinData.SkinIndex >= 0 then
                  C := gd[SkinData.SkinIndex].Props[Mode].FontColor.Color
                else
                  C := ColorToRGB(clWindowText);

              DrawArrow(FCacheBmp, C, clNone, R, asBottom, 0, 0, 0, Options.ActualArrowStyle);
            end
          end
          else
            if not gd[SkinData.SkinIndex].GiveOwnFont and IsValidImgIndex(GlyphIndex) then
              DrawSkinGlyph(FCacheBmp, Point(R.Left + (WidthOf(R) - ma[GlyphIndex].Width) div 2,
                            (Height - ma[GlyphIndex].Height) div 2), Mode, 1, ma[GlyphIndex], MakeCacheInfo(FCacheBmp))
            else begin // Paint without glyph
              if SkinData.SkinIndex >= 0 then
                C := gd[SkinData.SkinIndex].Props[Mode].FontColor.Color
              else
                C := ColorToRGB(clWindowText);

              DrawArrow(FCacheBmp, C, clNone, R, asBottom, 0, 0, 0, Options.ActualArrowStyle);
            end;
      end
      else begin
        FCacheBmp.PixelFormat := pf32bit;
        FCacheBmp.Width := Width;
        FCacheBmp.Height := Height;
        R := BtnRect;
{$IFDEF DELPHI7UP}
        if acThemesEnabled then begin
          cBox := ComboStates[BtnState];
          Details := acThemeServices.GetElementDetails(cBox);
          acThemeServices.DrawElement(FCacheBmp.Canvas.Handle, Details, R);
        end
        else
{$ENDIF}
        begin
          FillDC(FCacheBmp.Canvas.Handle, BtnRect, ColorToRGB(Color));
          DrawArrow(FCacheBmp, Font.Color, clNone, R, asBottom, 0, 0, 0, arsSolid1);
        end;
      end;
  end
  else begin
    if (BtnState > 0) and not ac_OldGlyphsMode then begin
      if SkinData.Skinned then begin
        Ndx := SkinData.SkinManager.ConstData.Sections[ssComboBtn];
        if Ndx < 0 then
          Ndx := SkinData.SkinManager.ConstData.Sections[ssUpDown];

        if Ndx < 0 then
          Ndx := SkinData.SkinManager.ConstData.Sections[ssSpeedButton_Small];

        if Ndx >= 0 then begin
          PaintItem(Ndx, MakeCacheInfo(SkinData.FCacheBmp), True, BtnState, R, MkPoint, SkinData.FCacheBmp, SkinData.SkinManager);
          SkinData.FCacheBmp.Canvas.Font.Color := SkinData.SkinManager.gd[Ndx].Props[BtnState].FontColor.Color;
        end;
      end
{$IFDEF DELPHI7UP}
      else
        if acThemesEnabled then begin
          cBtn := BtnStates[BtnState];
          Details := acThemeServices.GetElementDetails(cBtn);
          inc(R.Top);
          acThemeServices.DrawElement(SkinData.FCacheBmp.Canvas.Handle, Details, R);
          dec(R.Top);
        end
{$ENDIF}
    end;
    DrawData.Glyph := nil;
    if Assigned(GlyphMode.Images) and IsValidIndex(GlyphMode.ImageIndex, GetImageCount(GlyphMode.Images)) then begin
      DrawData.Blend := GlyphMode.Blend;
      DrawData.Images := GlyphMode.Images;
      DrawData.NumGlyphs := 1;
      case BtnState of
        0: DrawData.ImageIndex := GlyphMode.ImageIndex;
        1: DrawData.ImageIndex := GlyphMode.ImageIndexHot;
        2: DrawData.ImageIndex := GlyphMode.ImageIndexPressed;
      end;
      if not IsValidIndex(DrawData.ImageIndex, GetImageCount(GlyphMode.Images)) then
        DrawData.ImageIndex := GlyphMode.ImageIndex;
    end
    else begin
      DrawData.Blend := GlyphMode.Blend;
      DrawData.Images := acResImgList;
      DrawData.ImageIndex := FDefBmpID;
      DrawData.NumGlyphs := DrawData.Images.Width div DrawData.Images.Height;
    end;
    DrawData.CurrentState := BtnState;
    DrawData.Down := False;
    DrawData.Enabled := Enabled;
    DrawData.Reflected := False;
    DrawData.DstBmp := SkinData.FCacheBmp;
    DrawData.ImgRect := R;
    w := GlyphWidth;
    DrawData.ImgRect.Left := R.Left + (WidthOf(R) - w) div 2 + 1;
    DrawData.ImgRect.Top := R.Top + (HeightOf(R) - DrawData.Images.Height) div 2;
    DrawData.ImgRect.Right := DrawData.ImgRect.Left + w;
    DrawData.ImgRect.Bottom := DrawData.ImgRect.Top + DrawData.Images.Height;

    DrawData.SkinManager := SkinData.SkinManager;
    DrawData.DisabledGlyphKind := DefDisabledGlyphKind;
    if SkinData.Skinned then begin
      DrawData.Canvas := SkinData.FCacheBmp.Canvas;
      DrawData.Grayed := (DrawData.CurrentState = 0) and (GlyphMode.Grayed or SkinData.SkinManager.Effects.DiscoloredGlyphs);
      if DrawData.Grayed then
        DrawData.BGColor := SkinData.SkinManager.gd[SkinData.SkinIndex].Props[DrawData.CurrentState].Color
      else
        DrawData.BGColor := clNone;
    end
    else begin
      DrawData.Canvas := nil;
      DrawData.Grayed := GlyphMode.Grayed;
      if DrawData.Grayed then
        DrawData.BGColor := Color
      else
        DrawData.BGColor := clNone;
    end;
    if not GlyphEnabled then
      DrawData.Blend := DrawData.Blend + (100 - DrawData.Blend) div 2;

  if GlyphMode.ColorTone <> clNone then
    if (DrawData.Images is TsCharImageList) or ((DrawData.Images is TsVirtualImageList) and (TsVirtualImageList(DrawData.Images).AlphaImageList is TsCharImageList)) then begin
      DrawData.Grayed := False;
      if DrawData.DstBmp <> nil then begin
        DrawData.DstBmp.Canvas.Font.Color := GlyphMode.ColorTone;
        DrawData.DstBmp.Canvas.Font.Size := 0; // Def color is not allowed
      end;
    end
    else begin
      DrawData.Grayed := True;
      DrawData.BGColor := GlyphMode.ColorTone;
    end;

    acDrawGlyphEx(DrawData);
  end
end;


procedure TsCustomComboEdit.PaintText;
var
  SavedDC: hdc;
  R: TRect;
  aText: acString;
begin
  with SkinData.FCacheBMP do begin
    Canvas.Font.Assign(Font);
    aText := EditText;
    R := TextRect;
    if aText <> '' then begin
      if PasswordChar <> #0 then
        if PasswordChar = '*' then
          acFillString(aText, Length(aText), acChar(#$25CF))
        else
          acFillString(aText, Length(aText), acChar(PasswordChar));

      SavedDC := SaveDC(Canvas.Handle);
      IntersectClipRect(Canvas.Handle, R.Left, R.Top, R.Right, R.Bottom);
      try
        acWriteTextEx(Canvas, PacChar(aText), Enabled or SkinData.Skinned, R, DT_TOP or GetStringFlags(Self, Alignment) or DT_NOPREFIX, SkinData, ControlIsActive(SkinData));
      finally
        RestoreDC(Canvas.Handle, SavedDC);
      end;
    end
{$IFDEF D2009}
    else
      if TextHint <> '' then begin
        Canvas.Brush.Style := bsClear;
        if SkinData.Skinned then
          Canvas.Font.Color := BlendColors(ColorToRGB(Font.Color), ColorToRGB(Color), 166)
        else
          Canvas.Font.Color := clGrayText;

        SavedDC := SaveDC(Canvas.Handle);
        IntersectClipRect(Canvas.Handle, 0, 0, R.Right, R.Bottom);
        try
          acDrawText(Canvas.Handle, TextHint, R, DT_TOP or GetStringFlags(Self, Alignment) and not DT_VCENTER);
        finally
          RestoreDC(Canvas.Handle, SavedDC);
        end;
      end;
{$ENDIF}
  end;
end;


procedure TsCustomComboEdit.CMFocuseChanged(var Message: TCMFocusChanged);
begin
  if not (csDesigning in ComponentState) and DroppedDown and (Message.Sender <> Self) then
    PopupWindowClose;

  inherited;
end;


function TsCustomComboEdit.ComboBtn: boolean;
begin
  Result := (FDefBmpID = iBTN_ARROW) and not ((GlyphMode.Images <> nil) and IsValidIndex(GlyphMode.ImageIndex, GetImageCount(GlyphMode.Images)));
end;


procedure TsCustomComboEdit.SetShowButton(const Value: boolean);
begin
  if FShowButton <> Value then begin
    FShowButton := Value;
    SkinData.Invalidate;
    if not (csLoading in ComponentState) and HandleAllocated then
      SetWindowPos(Handle, 0, 0, 0, 0, 0, SWPA_FRAMECHANGED);
  end;
end;


procedure TsCustomComboEdit.StartBtnTimer;
begin
  if BtnTimer = nil then begin
    BtnTimer := TTimer.Create(Self);
    BtnTimer.Interval := 100;
  end;
  BtnTimer.OnTimer := MouseLeaveTimer;
  BtnTimer.Enabled := True;
end;


function TsCustomComboEdit.TextRect: TRect;
begin
  Result := inherited TextRect;
  if IsRightToLeft then
    Result.Left := GlyphSpace + BtnOffset
  else
    Result.Right := Width - BtnOffset - GlyphSpace - acSpacing div 2;
end;


procedure TsCustomComboEdit.TryShowBtnHint;
begin
  if Application.ShowHint and ShowHint and (GlyphMode.Hint <> '') then
    if HintWnd = nil then
      if HintWindowClass = THintWindow then
        HintWnd := acShowHintWnd(GlyphMode.Hint,  Point(acMousePos.X, acMousePos.Y))
      else
        acShowHintWnd(GlyphMode.Hint, Point(acMousePos.X, acMousePos.Y + 16));
end;


procedure TsCustomComboEdit.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (FGlyphMode <> nil) and (AComponent = FGlyphMode.Images) then
    FGlyphMode.Images := nil;
end;


constructor TacHintTimer.Create(AOwner: TComponent);
begin
  inherited;
  FOwner := AOwner;
  Interval := Application.HintPause;
  OnTimer := TimerEvent;
  Enabled := True;
end;


procedure TacHintTimer.TimerEvent(Sender: TObject);
begin
  Enabled := False;
  TsCustomComboEdit(FOwner).TryShowBtnHint;
end;

end.
