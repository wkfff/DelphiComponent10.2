unit sVclUtils;
{$I sDefs.inc}
//{$DEFINE LOGGED}
//{$DEFINE ACDEBUG}

interface

uses
  Classes, Controls, SysUtils, StdCtrls, windows, Graphics, Forms, Messages, extctrls, comctrls,
  {$IFDEF FPC} lcltype, {$ENDIF}
  {$IFNDEF DELPHI5} Types, {$ENDIF}
  {$IFDEF USEDB} db, dbgrids, dbCtrls, {$ENDIF}
  {$IFDEF DELPHI7UP} Themes, {$ENDIF}
  {$IFDEF LOGGED} sDebugMsgs, {$ENDIF}
  {$IFDEF TNTUNICODE} TntControls, {$ENDIF}
  acntTypes, sSkinProvider, acSBUtils, sConst, acntUtils, sCommonData, acDials, acThdTimer, sGraphUtils;


type
  TacIterProc = procedure(Ctrl: TControl; Data: integer);
  TacCtrlArray = array of TControl;

const
  AlignToInt: array [TAlignment] of Longint = (DT_LEFT, DT_RIGHT, DT_CENTER); // TAlignment = (taLeftJustify, taRightJustify, taCenter)
  BidiAlign: array [boolean {if RTL}, TAlignment] of TAlignment = ((taLeftJustify, taRightJustify, taCenter), (taRightJustify, taLeftJustify, taCenter));


function acColorToRGB(Value: TColor; SkinManager: TObject = nil): TColor; // Converting of special colors (from sConst.pas) to colors from a current skin palette
function acMousePos: TPoint;
function acMouseInControl(Control: TControl): boolean;
function acMouseInControlDC(Handle: THandle): boolean;

function acGetFormImage(aForm: TForm): TBitmap;

function ContainsWnd(AHandle, AParent: THandle): boolean;
function CtrlUnderMouse: TControl;
function GetRootParent(Handle: THandle): THandle;
function LeftToRight(const Control: TControl; const NormalAlignment: boolean = True): boolean;
procedure AddToAdapter(const Frame: TWinControl);
procedure BroadCastMsg(const Ctrl: hwnd; const Message: TMessage);
procedure IterateControls(Owner: TWinControl; Data: integer; CallBack: TacIterProc);

procedure SaveGraphCtrls(Wnd: TWinControl; var cArray: TacCtrlArray);
procedure RestoreGraphCtrls(Wnd: TWinControl; var cArray: TacCtrlArray);

procedure PaintChildCtrls(Ctrl: TWinControl; Bmp: TBitmap);
procedure SkinPaintTo(const Bmp: TBitmap; const Ctrl: TControl; const Left: integer = 0; const Top: integer = 0; const SkinProvider: TComponent = nil; RootCtrl: boolean = False);
procedure StdPaintTo(const Bmp: TBitmap; const Ctrl: TWinControl);

procedure AnimShowDlg(ListSW: TacDialogWnd; wTime: word = 0; MaxTransparency: integer = MaxByte; AnimType: TacAnimType = atAero);
procedure AnimShowForm(sp: TsSkinProvider;  wTime: word = 0; MaxTransparency: integer = MaxByte; AnimType: TacAnimType = atAero);

procedure PrepareForAnimation(const Ctrl: TWinControl; AnimType: TacAnimTypeCtrl = atcFade);
procedure AnimShowControl(Ctrl: TWinControl; wTime: word = 0; MaxTransparency: integer = MaxByte; AnimType: TacAnimTypeCtrl = atcFade);

procedure AnimHideForm(SkinProvider: TObject);
procedure PrintDlgClient(ListSW: TacDialogWnd; acDstBmp: TBitmap; CopyScreen: boolean = False);
procedure AnimHideDlg(ListSW: TacDialogWnd);

function DoLayered(FormHandle: Hwnd; Layered: boolean; AlphaValue: byte = 1): boolean;

function acShowHintWnd(HintText: string; Pos: TPoint): {$IFDEF TNTUNICODE}TTntHintWindow{$ELSE}THintWindow{$ENDIF};
{$IFNDEF ALITE}
procedure acHideHintWnd(var Wnd: {$IFDEF TNTUNICODE}TTntHintWindow{$ELSE}THintWindow{$ENDIF});
{$ENDIF}
function acWorkRect(Form: TForm): TRect; overload;
function acWorkRect(Coord: TPoint): TRect; overload;

procedure SetParentUpdated(const wc: TWinControl); overload;
procedure SetParentUpdated(const pHwnd: hwnd); overload;
procedure ChangeControlColors(AControl: TControl; AFontColor, AColor: TColor); // clNone will reset a color to default
function GetControlColor(const Control: TControl): TColor; overload;
function GetControlColor(const Handle: THandle): TColor; overload;
function GetControlFontColor(const Control: TControl; SkinManager: TObject): TColor;
function IsCustomFont(Ctrl: TControl; AFont: TFont; SaveColor: boolean = True): boolean;
function AllEditSelected(Ctrl: TCustomEdit): Boolean;

procedure PaintControls(DC: HDC; OwnerControl: TWinControl; ChangeCache: boolean; Offset: TPoint; AHandle: THandle = 0; CheckVisible: boolean = True);
procedure PaintParentBG(AControl: TControl; ABitmap: TBitmap);

procedure SetRedraw(Handle: THandle;         Value: integer = 0); overload;
procedure SetRedraw(Ctrl:   TGraphicControl; Value: integer = 0); overload;

function SendAMessage(const Handle: hwnd; const Cmd: Integer; LParam: LPARAM = 0): LRESULT; overload; // may be removed later
function SendAMessage(const Control: TControl; const Cmd: Integer; LParam: LPARAM = 0): LRESULT; overload;
function TrySendMessage(const Control: TControl; Message: TMessage): LRESULT; overload;
function TrySendMessage(const Control: TControl; Msg: Cardinal; WParam: WPARAM; LParam: LPARAM = 0): LRESULT; overload;
function TrySendMessage(AHandle: THandle; Msg: Cardinal; WParam: WPARAM; LParam: LPARAM = 0): LRESULT; overload;
function GetBoolMsg(const Control: TWinControl; const Cmd: Cardinal): boolean; overload;
function GetBoolMsg(const CtrlHandle: hwnd; const Cmd: Cardinal): boolean; overload;
function ControlIsReady(const Control: TControl): boolean;
function GetOwnerForm (const Component: TComponent): TCustomForm;
function GetOwnerFrame(const Component: TComponent): TCustomFrame;
procedure SetControlsEnabled(Parent: TWinControl; Value: boolean; Recursion: boolean = False);
function GetStringFlags(const Control: TControl; const al: TAlignment): Cardinal;
procedure RepaintsControls(const Owner: TWinControl; const BGChanged: boolean);
procedure AlphaBroadCast(const Control: TWinControl; var Message); overload;
procedure AlphaBroadCast(const Handle: hwnd; var Message); overload;
function acClientRect(const Handle: hwnd): TRect;
function acMouseInWnd(const Handle: hwnd; X, Y: integer): TPoint;
function GetAlignShift(const Ctrl: TWinControl; const Align: TAlign; const GraphCtrlsToo: boolean = False): integer;
function GetParentFormHandle(const CtrlHandle: hwnd): hwnd;
function TrySetSkinSection(const Control: TControl; const SectionName: string): boolean;
function GetWndClassName(const Hwnd: THandle): string;
procedure SetFormBlendValue(FormHandle: THandle; Bmp: TBitmap; Value: integer; NewPos: PPoint = nil);
function GetShiftState: TShiftState;
procedure CheckLastError;
procedure ReflectControls(ParentWnd: TWinControl; Recursion: boolean);

type
  TOutputWindow = class(TCustomControl)
  private
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
    procedure WMNCPaint(var Message: TWmEraseBkgnd); message WM_NCPAINT;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    constructor Create(AOwner: TComponent); override;
    property Canvas;
  end;


  TacHideTimer = class(TacThreadedTimer)
  public
    Dlg: TacDialogWnd;
    ParentWnd: THandle;
    DC: hdc;
    Form: TacGlowForm;
    FBmpSize: TSize;
    FBmpTopLeft: TPoint;
    FBlend: TBlendFunction;
    Trans, p, dx, dy, l, t, r, b: real;
    i, StartBlendValue, StepCount: integer;
    AnimType: TacAnimType;
    EventCalled: boolean;
    SrcBmp, DstBmp: TBitmap;
    procedure Anim_Init;
    procedure CallEvent;
    procedure Anim_DoNext;
    function Anim_GoToNext: boolean;
    procedure OnTimerProc(Sender: TObject);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;


var
  ow: TOutPutwindow = nil;
  InAnimationProcess: boolean = False;
  acGraphPainting: boolean = False;
  uxthemeLib: HModule = 0;
  Ac_SetWindowTheme: function(hwnd: HWND; pszSubAppName: LPCWSTR; pszSubIdList: LPCWSTR): HRESULT; stdcall;
  acHideTimer: TacHideTimer = nil;

{$IFDEF ACDEBUG}
  acDebugAnimBmp: TBitmap = nil;
{$ENDIF}

{$IFDEF DELPHI_XE2}
  acThemeServices: TCustomStyleServices;
{$ELSE}
  {$IFDEF DELPHI7UP}
    acThemeServices: TThemeServices;
  {$ENDIF}
{$ENDIF}


{$IFDEF DELPHI7UP}
function acThemesEnabled: boolean;
{$ENDIF}


implementation

uses
  math, Buttons,
{$IFNDEF ALITE}
  sStatusBar,
{$ENDIF}
  acGPUtils, sStyleSimply, sMessages, sSkinManager, sThirdParty, sAlphaGraph, acGlow{$IFNDEF ALITE}, acAlphaHints{$ENDIF};


function acMousePos: TPoint;
begin
  if not GetCursorPos(Result) then
    Result := MkPoint;
end;


function acColorToRGB(Value: TColor; SkinManager: TObject = nil): TColor;
var
  sm: TsSkinManager;
begin
  if Value < 0 then begin
    if SkinManager = nil then
      sm := DefaultManager
    else
      sm := TsSkinManager(SkinManager);

    if (sm <> nil) and sm.CommonSkinData.Active then
      case Value of
        slBtnRed:             Result := sm.Palette[pcBtnToneRed];
        slBtnRedActive:       Result := sm.Palette[pcBtnToneRedActive];
        slBtnGreen:           Result := sm.Palette[pcBtnToneGreen];
        slBtnGreenActive:     Result := sm.Palette[pcBtnToneGreenActive];
        slBtnBlue:            Result := sm.Palette[pcBtnToneBlue];
        slBtnBlueActive:      Result := sm.Palette[pcBtnToneBlueActive];

        slBtnRedText:         Result := sm.Palette[pcBtnRedText];
        slBtnRedTextActive:   Result := sm.Palette[pcBtnRedTextActive];
        slBtnGreenText:       Result := sm.Palette[pcBtnGreenText];
        slBtnGreenTextActive: Result := sm.Palette[pcBtnGreenTextActive];
        slBtnBlueText:        Result := sm.Palette[pcBtnBlueText];
        slBtnBlueTextActive:  Result := sm.Palette[pcBtnBlueTextActive];

        slEditGreen:          Result := sm.Palette[pcEditBG_Ok];
        slEditYellow:         Result := sm.Palette[pcEditBG_Warning];
        slEditRed:            Result := sm.Palette[pcEditBG_Alert];

        slEditGreenText:      Result := sm.Palette[pcEditText_Ok];
        slEditYellowText:     Result := sm.Palette[pcEditText_Warning];
        slEditRedText:        Result := sm.Palette[pcEditText_Alert]
        else
          if sm.Options.ChangeSysColors then
            case Value of
              // Some std colors
              clMenu,
      {$IFDEF DELPHI7UP}
              clMenuBar,
      {$ENDIF}
              clScrollBar,
              clBackground,
              clAppWorkSpace,
              clBtnFace:            Result := sm.Palette[pcMainColor];

              clMenuText,
              clBtnText:            Result := sm.Palette[pcLabelText];

              clWindow:             Result := sm.Palette[pcEditBG];
              clWindowText:         Result := sm.Palette[pcEditText];

              clActiveBorder,
              clInactiveBorder,
              clWindowFrame:        Result := sm.Palette[pcBorder];

      {$IFDEF DELPHI7UP}
              clGradientActiveCaption,
      {$ENDIF}
              clActiveCaption:
                if sm.ConstData.Sections[ssFormTitle] >= 0 then
                  Result := sm.gd[sm.ConstData.Sections[ssFormTitle]].Props[1].Color
                else
                  Result := sm.Palette[pcMainColor];

      {$IFDEF DELPHI7UP}
              clGradientInactiveCaption,
      {$ENDIF}
              clInactiveCaption:
                if sm.ConstData.Sections[ssFormTitle] >= 0 then
                  Result := sm.gd[sm.ConstData.Sections[ssFormTitle]].Props[0].Color
                else
                  Result := sm.Palette[pcMainColor];

              clCaptionText:
                if sm.ConstData.Sections[ssFormTitle] >= 0 then
                  Result := sm.gd[sm.ConstData.Sections[ssFormTitle]].Props[1].FontColor.Color
                else
                  Result := sm.Palette[pcLabelText];

              clInactiveCaptionText:
                if sm.ConstData.Sections[ssFormTitle] >= 0 then
                  Result := sm.gd[sm.ConstData.Sections[ssFormTitle]].Props[0].FontColor.Color
                else
                  Result := sm.Palette[pcLabelText];

      {$IFDEF DELPHI7UP}
              clMenuHighlight,
      {$ENDIF}
              clHighlight: Result := sm.Palette[pcSelectionBG_Focused];

              clHighlightText: Result := sm.Palette[pcSelectionText_Focused];
      {$IFDEF DELPHI7UP}
              clHotLight:  Result := sm.Palette[pcWebTextHot];
      {$ENDIF}

              cl3DDkShadow,
              clBtnShadow:    Result := BlendColors(sm.Palette[pcMainColor], clBlack, 180);
              clGrayText:     Result := BlendColors(sm.Palette[pcMainColor], sm.Palette[pcLabelText], 127);
              clBtnHighlight: Result := BlendColors(sm.Palette[pcMainColor], sm.Palette[pcSelectionBG_Focused], 127);
              cl3DLight:      Result := BlendColors(sm.Palette[pcMainColor], clWhite, 180);

              clInfoText:
                if sm.ConstData.Sections[ssHint] >= 0 then
                  Result := sm.gd[sm.ConstData.Sections[ssFormTitle]].Props[0].FontColor.Color
                else
                  Result := sm.Palette[pcEditText];

              clInfoBk:
                if sm.ConstData.Sections[ssHint] >= 0 then
                  Result := sm.gd[sm.ConstData.Sections[ssFormTitle]].Props[0].Color
                else
                  Result := sm.Palette[pcEditBG];

              else
                Result := ColorToRGB(Value);
            end
          else
            Result := ColorToRGB(Value);
      end
    else
      if $FFFFFF and Value > 100 then
        Result := $FFFFFF and Value
      else
        Result := ColorToRGB(Value);
  end
  else
    Result := ColorToRGB(Value);
end;


function GetRootParent(Handle: THandle): THandle;
begin
  Result := GetParent(Handle);
  if Result <> 0 then
    Result := GetRootParent(Result)
  else
    Result := Handle;
end;


function acMouseInControl(Control: TControl): boolean;
var
  DC: hdc;
  R: TRect;
  p: TPoint;
  Form: TCustomForm;
  VclWnd: THandle;
begin
  if Control.Visible then begin
    if Control is TWinControl then begin
      GetWindowRect(TWinControl(Control).Handle, R);
      Result := PtInRect(R, acMousePos);
    end
    else
      if (Control.Parent <> nil) and Control.Parent.HandleAllocated then begin
        Form := GetParentForm(Control);
{$IFDEF D2010}
        if (Form <> nil) and (Application.ModalLevel > 0) and (Form.Handle <> Application.ActiveFormHandle) then // If parent form is under modal form
{$ELSE}
        if (Form <> nil) and not IsWindowEnabled(Form.Handle) then
{$ENDIF}
          Result := False
        else
          if (Form = nil) and not IsWindowEnabled(GetParentFormHandle(Control.Parent.Handle)) then
            Result := False
          else begin
            p := Control.ScreenToClient(acMousePos);
            Result := PtInRect(MkRect(Control), p);
          end;
      end
      else
        Result := False;

    if Result then
      if Control is TGraphicControl then begin
        DC := GetDC(Control.Parent.Handle);
        try
          if GetClipBox(DC, R) = 0 then
            Result := False
          else
            if (R.Left - Control.Left > p.X) or (R.Top - Control.Top > p.Y) then
              Result := False
            else
              if (R.Right < Control.Left + p.X) or (R.Bottom < Control.Top + p.Y) then
                Result := False;
        finally
          ReleaseDC(Control.Parent.Handle, DC);
        end;
      end
      else
        if (Control is TWinControl) and TWinControl(Control).HandleAllocated then begin
          // Find window under mouse
          VclWnd := WindowFromPoint(acMousePos);
          // Is this a child window of the Control?
          if VclWnd <> 0 then
            Result := ContainsWnd(VclWnd, TWinControl(Control).Handle);
        end
        else
          Result := False;
  end
  else
    Result := False;
end;


function acMouseInControlDC(Handle: THandle): boolean;
var
  DC: hdc;
  WndRect, R: TRect;
begin
  if IsWindowVisible(Handle) then begin
    DC := GetWindowDC(Handle);
    GetWindowRect(Handle, WndRect);
    try
      case GetClipBox(DC, R) of
        SIMPLEREGION: begin
          OffsetRect(R, WndRect.Left, WndRect.Top);
          Result := PtInRect(R, acMousePos);
        end
        else
          Result := False
      end;
    finally
      ReleaseDC(Handle, DC);
    end;
  end
  else
    Result := False;
end;


function acGetFormImage(aForm: TForm): TBitmap;
var
  SavedDC: hdc;
  BgBmp: TBitmap;
  SkinProvider: TsSkinProvider;
begin
  SkinProvider := TsSkinProvider(aForm.Perform(SM_ALPHACMD, AC_GETPROVIDER_HI, 0));
  if SkinProvider.SkinData.Skinned then begin
    Result := CreateBmp32(aForm.ClientRect);
    BgBmp := TBitmap.Create;
    if SkinProvider.SkinData.BGChanged or SkinProvider.SkinData.FCacheBmp.Empty then
      SkinProvider.PaintAll;

    BgBmp.Assign(SkinProvider.SkinData.FCacheBmp);
    SavedDC := SaveDC(BgBmp.Canvas.Handle);
    SkinPaintTo(BgBmp, aForm, 0, 0, SkinProvider, True);
    RestoreDC(BgBmp.Canvas.Handle, SavedDC);
    BitBlt(Result.Canvas.Handle, 0, 0, Result.Width, Result.Height, BgBmp.Canvas.Handle, SkinProvider.OffsetX, SkinProvider.OffsetY, SRCCOPY);
    BgBmp.Free;
  end
  else
    Result := aForm.GetFormImage;
end;


function ContainsWnd(AHandle, AParent: THandle): boolean;
begin
  if AHandle = AParent then
    Result := True
  else
    if AHandle <> 0 then
      Result := ContainsWnd(GetParent(AHandle), AParent)
    else
      Result := False
end;


function CtrlUnderMouse: TControl;
var
  p: TPoint;
  i: integer;
begin
  if (DefaultManager <> nil) and (DefaultManager.ActiveGraphControl <> nil) then
    Result := DefaultManager.ActiveGraphControl
  else begin
    Result := FindVCLWindow(acMousePos);
    if Result <> nil then
      with TWinControl(Result) do begin
        p := ScreenToClient(acMousePos);
        for i := ControlCount - 1 downto 0 do
          if Controls[i].Visible and PtInRect(Controls[i].BoundsRect, p) then begin
            Result := Controls[i];
            Exit;
          end;
      end;
  end;
end;


function LeftToRight(const Control: TControl; const NormalAlignment: boolean = True): boolean;
begin
  if NormalAlignment then
    Result := (Control.BidiMode = bdLeftToRight) or not SysLocale.MiddleEast
  else
    Result := (Control.BidiMode <> bdLeftToRight) and SysLocale.MiddleEast;
end;


procedure AddToAdapter(const Frame: TWinControl);
var
  c: TWinControl;
begin
  if Frame <> nil then
    if [csDesigning, csLoading] * Frame.ComponentState = [] then begin
      c := GetParentForm(Frame);
      if c <> nil then
        TrySendMessage(c.Handle, SM_ALPHACMD, AC_CONTROLLOADED shl 16, LPARAM(Frame));
    end;
end;


procedure BroadCastMsg(const Ctrl: hwnd; const Message: TMessage);
var
  hCtrl: THandle;
begin
  hCtrl := GetTopWindow(Ctrl);
  while hCtrl <> 0 do begin
    if GetWindowLong(hCtrl, GWL_STYLE) and WS_CHILD <> 0 then
      TrySendMessage(hCtrl, Message.Msg, Message.WParam, Message.LParam);

    hCtrl := GetNextWindow(hCtrl, GW_HWNDNEXT);
  end;
end;


procedure IterateControls(Owner: TWinControl; Data: integer; CallBack: TacIterProc);
var
  i: integer;
begin
  for i := 0 to Owner.ControlCount - 1 do
    if Owner.Controls[i] is TControl then begin
      CallBack(TWinControl(Owner.Controls[i]), Data);
      if Owner.Controls[i] is TWinControl then
        IterateControls(TWinControl(Owner.Controls[i]), Data, CallBack);
    end;
end;


procedure SaveGraphCtrls(Wnd: TWinControl; var cArray: TacCtrlArray);
var
  i: integer;
begin
  Wnd.DoubleBuffered := True;
  for i := 0 to Wnd.ControlCount - 1 do
    if not (csOpaque in TGraphicControl(Wnd.Controls[i]).ControlStyle) then begin
      Wnd.Controls[i].ControlStyle := Wnd.Controls[i].ControlStyle + [csOpaque];
      SetLength(cArray, Length(cArray) + 1);
      cArray[Length(cArray) - 1] := Wnd.Controls[i];
    end;
end;


procedure RestoreGraphCtrls(Wnd: TWinControl; var cArray: TacCtrlArray);
var
  i: integer;
begin
  Wnd.DoubleBuffered := True;
  for i := 0 to Length(cArray) -1 do
    cArray[i].ControlStyle := cArray[i].ControlStyle - [csOpaque];

  SetLength(cArray, 0);
end;


procedure PaintChildCtrls(Ctrl: TWinControl; Bmp: TBitmap);
var
  i: integer;
  SaveIndex: hdc;
begin
  for I := 0 to Ctrl.ControlCount - 1 do
    if Ctrl.Controls[I] is TWinControl then
      with TWinControl(Ctrl.Controls[I]) do
        if (Visible or (csDesigning in ComponentState)) and HandleAllocated then begin
          SaveIndex := SaveDC(Bmp.Canvas.Handle);
          try
            if not (Ctrl.Controls[I] is TCustomForm) or (Parent <> nil) then
              MoveWindowOrg(Bmp.Canvas.Handle, Left, Top);

            SkinPaintTo(Bmp, Ctrl.Controls[I], Left, Top);
          finally
            RestoreDC(Bmp.Canvas.Handle, SaveIndex);
          end;
        end;
end;


type
  TAccessControl = class(TWinControl);

procedure SkinPaintTo(const Bmp: TBitmap; const Ctrl: TControl; const Left: integer = 0; const Top: integer = 0; const SkinProvider: TComponent = nil; RootCtrl: boolean = False);
var
  DC: hdc;
  SaveIndex: hdc;
  ParentBG: TacBGInfo;
  RBig, RSmall, cR: TRect;
  I, Res, NextIndex: Integer;

  procedure PaintNextCtrls(AControl: TControl);
  var
    i: integer;
  begin
    if (AControl.Parent <> nil) then
      with TWinControl(AControl.Parent) do begin
        NextIndex := -1;
        for i := 0 to ControlCount - 1 do
          if Controls[i] = AControl then begin
            NextIndex := i + 1;
            Break;
          end;

        for i := NextIndex to ControlCount - 1 do
          if not (Controls[i] is TOutputWindow) and Controls[i].Visible then begin
            RBig := AControl.BoundsRect;
            RSmall := Controls[i].BoundsRect;
            if RectInRect(RBig, RSmall, False) then begin
              MoveWindowOrg(Bmp.Canvas.Handle, RSmall.Left - RBig.Left, RSmall.Top - RBig.Top);
              SkinPaintTo(Bmp, Controls[i]);
              MoveWindowOrg(Bmp.Canvas.Handle, -(RSmall.Left - RBig.Left), -(RSmall.Top - RBig.Top));
            end;
          end;
      end;
  end;

begin
  if (SkinProvider = nil) and (Ctrl.Parent <> nil) and not (Ctrl.Visible or (csDesigning in Ctrl.ComponentState)) then begin
    ParentBG.DrawDC := 0;
    ParentBG.PleaseDraw := False;
    GetBGInfo(@ParentBG, Ctrl.Parent);
    if ParentBG.BgType = btCache then
      BitBlt(Bmp.Canvas.Handle, Left, Top, Ctrl.Width, Ctrl.Height, ParentBG.Bmp.Canvas.Handle, ParentBG.Offset.X + Ctrl.Left, ParentBG.Offset.Y + Ctrl.Top, SRCCOPY)
    else
      FillDC(Bmp.Canvas.Handle, Rect(Left, Top, Ctrl.Width, Ctrl.Height), ParentBG.Color);
  end
  else begin
    DC := Bmp.Canvas.Handle;
    Bmp.Canvas.Lock;
    if (SkinProvider = nil) or (TsSkinProvider(SkinProvider).BorderForm = nil) then begin
      GetWindowRect(TWinControl(Ctrl).Handle, cR);
      IntersectClipRect(DC, 0, 0, Ctrl.Width, Ctrl.Height);
    end;
    if Ctrl is TWinControl then begin
      if (Ctrl is TForm) and (TForm(Ctrl).FormStyle = fsMDIForm) then
        for I := 0 to TForm(Ctrl).MDIChildCount - 1 do begin
          SaveIndex := SaveDC(DC);
          MoveWindowOrg(DC, TForm(Ctrl).MDIChildren[i].Left, TForm(Ctrl).MDIChildren[i].Top);
          SkinPaintTo(Bmp, TForm(Ctrl).MDIChildren[i], TForm(Ctrl).MDIChildren[i].Left, TForm(Ctrl).MDIChildren[i].Top);
          RestoreDC(DC, SaveIndex);
        end;

      if (Ctrl is TTabsheet) and (TTabSheet(Ctrl).BorderWidth <> 0) then
        MoveWindowOrg(DC, TTabSheet(Ctrl).BorderWidth, TTabSheet(Ctrl).BorderWidth);

      try
        Res := SendAMessage(TWinControl(Ctrl).Handle, AC_PRINTING, LPARAM(DC));
        if (Res = 1) or WndIsSkinned(TWinControl(Ctrl).Handle) then begin
          Res := TrySendMessage(TWinControl(Ctrl).Handle, WM_PRINT, WPARAM(DC), 0);
          SendAMessage(TWinControl(Ctrl).Handle, AC_PRINTING, 0);
        end
        else
          TWinControl(Ctrl).PaintTo(DC, 0, 0);
      finally
      end;

      if Res <> NOCHILDRENPRINT then
        PaintChildCtrls(TWinControl(Ctrl), Bmp);

      if SkinProvider <> nil then begin
        if TsSkinProvider(SkinProvider).BorderForm <> nil then begin
          cR := Ctrl.ClientRect;
          OffsetRect(cR, TsSkinProvider(SkinProvider).OffsetX, TsSkinProvider(SkinProvider).OffsetY);
          if cR.Bottom >= Bmp.Height then
            cR.Bottom := Bmp.Height - 1;

          FillAlphaRect(Bmp, cR, MaxByte); // Fill AlphaChannell in client area
        end;
        SendAMessage(TWinControl(Ctrl).Handle, AC_PAINTFLOATITEMS, LPARAM(Bmp)); // Paint float buttons
      end
      else
        if (Ctrl is TTabsheet) and (TTabSheet(Ctrl).BorderWidth <> 0) then
          MoveWindowOrg(DC, -TTabSheet(Ctrl).BorderWidth, -TTabSheet(Ctrl).BorderWidth);
    end
    else begin
      SendAMessage(Ctrl, AC_PRINTING, LPARAM(DC));
      Ctrl.Perform(WM_PRINT, WPARAM(DC), 0);
      SendAMessage(Ctrl, AC_PRINTING, 0);
    end;
    Bmp.Canvas.Unlock;
  end;
  if RootCtrl then
    PaintNextCtrls(Ctrl);
end;


procedure PaintChildStdCtrls(Ctrl: TWinControl; Bmp: TBitmap);
var
  ChildWnd: TWinControl;
  R: TRect;
  i: integer;
  SaveIndex: hdc;
begin
  for I := 0 to Ctrl.ControlCount - 1 do
    if Ctrl.Controls[I] is TWinControl then begin
      ChildWnd := TWinControl(Ctrl.Controls[I]);
      with ChildWnd do
        if (Visible or (csDesigning in ComponentState)) and HandleAllocated then begin
          R := ChildWnd.BoundsRect;
            SaveIndex := SaveDC(Bmp.Canvas.Handle);
            try
              if not (ChildWnd is TCustomForm) or (ChildWnd.Parent <> nil) then
                MoveWindowOrg(Bmp.Canvas.Handle, ChildWnd.Left, ChildWnd.Top);

              ChildWnd.PaintTo(Bmp.Canvas.Handle, 0, 0);
              PaintChildStdCtrls(ChildWnd, Bmp);
            finally
              RestoreDC(Bmp.Canvas.Handle, SaveIndex);
            end;
          end;
    end
    else begin
      SaveIndex := SaveDC(Bmp.Canvas.Handle);
      try
        MoveWindowOrg(Bmp.Canvas.Handle, Ctrl.Controls[I].Left, Ctrl.Controls[I].Top);
        Ctrl.Controls[I].Perform(WM_PAINT, WPARAM(Bmp.Canvas.Handle), 0);
      finally
        RestoreDC(Bmp.Canvas.Handle, SaveIndex);
      end;
    end;
end;


procedure StdPaintTo(const Bmp: TBitmap; const Ctrl: TWinControl);
var
  ClientOffset: TPoint;
begin
  ClientOffset.X := 0;
  ClientOffset.Y := 0;
  Bmp.Canvas.Lock;
  SendMessage(Ctrl.Handle, WM_ERASEBKGND, WParam(Bmp.Canvas.Handle), 0);

  if Ctrl is TWinControl then begin
    if (Ctrl is TTabsheet) and (TTabSheet(Ctrl).BorderWidth <> 0) then
      MoveWindowOrg(Bmp.Canvas.Handle, TTabSheet(Ctrl).BorderWidth, TTabSheet(Ctrl).BorderWidth);

    PaintChildStdCtrls(TWinControl(Ctrl), Bmp);
    if (Ctrl is TTabsheet) and (TTabSheet(Ctrl).BorderWidth <> 0) then
      MoveWindowOrg(Bmp.Canvas.Handle, -TTabSheet(Ctrl).BorderWidth, -TTabSheet(Ctrl).BorderWidth);
  end
  else
    Ctrl.Perform(WM_PRINT, WPARAM(Bmp.Canvas.Handle), 0);

  FillAlphaRect(Bmp, MkRect(Bmp), MaxByte); // Fill AlphaChannell in client area
  Bmp.Canvas.Unlock;
end;


type
  TAccessWinControl = class(TWinControl);
  TAccessProvider = class(TsSkinProvider);


procedure SetChildOrderAfter(Child: TWinControl; Control: TControl);
var
  i: Integer;
begin
  for i := 0 to Child.Parent.ControlCount do
    if Child.Parent.Controls[i] = Control then begin
      TAccessWinControl(Child.Parent).SetChildOrder(Child, i + 1);
      break;
    end;
end;


const
  acwDivider = 32;
  acwPopupDiv = 2;


procedure CleanPixelsByRects(const Rects: TRects; Bmp: TBitmap);
var
  i: integer;
begin
  for i := 0 to Length(Rects) - 1 do
    FillRect32(Bmp, Rects[i], 0, 0);
end;


procedure AnimShowForm(sp: TsSkinProvider; wTime: word = 0; MaxTransparency: integer = MaxByte; AnimType: TacAnimType = atAero);
const
  ShowCommands: array[TWindowState] of Integer = (SW_SHOWNORMAL, SW_SHOWMINNOACTIVE, SW_SHOWMAXIMIZED);
//  DebugOffsX = 100; DebugOffsY = -100;
  DebugOffsX = 0; DebugOffsY = 0;
var
  DC: hdc;
  h: hwnd;
  fR: TRect;
  lTicks: DWord;
  Flags: Cardinal;
  FBmpSize: TSize;
  AnimForm: TacGlowForm;
  FBmpTopLeft: TPoint;
  FBlend: TBlendFunction;
  AnimBmp, acDstBmp: TBitmap;
  i, StepCount, cy, cx: integer;
  dx, dy, l, t, r, b, trans, p: real;

  procedure Anim_Init;
  begin
    trans := 0;
    p := MaxTransparency / StepCount;
    case AnimType of
      atDropDown: begin
        l := 0;
        t := 0;
        r := acDstBmp.Width;
        b := 0;
        dx := (acDstBmp.Width  - r) / acwPopupDiv;
        dy := (acDstBmp.Height - b) / acwPopupDiv;
      end;

      atAero: begin
        l := acDstBmp.Width / acwDivider;
        t := acDstBmp.Height / acwDivider;
        dx := l / StepCount;
        dy := t / StepCount;
        r := acDstBmp.Width  - l;
        b := acDstBmp.Height - t;
      end

      else begin
        dx := 0;
        dy := 0;
        l := 0;
        t := 0;
        r := acDstBmp.Width;
        b := acDstBmp.Height;
      end;
    end
  end;

  procedure Anim_DoNext;
  begin
    trans := min(Trans + p, MaxTransparency);
    FBlend.SourceConstantAlpha := Round(Trans);
    case AnimType of
      atDropDown: begin
        FBlend.SourceConstantAlpha := MaxByte;
        if (l < 0) or (t < 0) then
          BitBlt(AnimBmp.Canvas.Handle, 0, 0, acDstBmp.Width, acDstBmp.Height, acDstBmp.Canvas.Handle, 0, 0, SRCCOPY)
        else
          StretchBlt(AnimBmp.Canvas.Handle, Round(l), Round(t), Round(r - l), Round(b - t), acDstBmp.Canvas.Handle, 0, 0, acDstBmp.Width, acDstBmp.Height, SRCCOPY);
      end;

      atAero:
        if (l < 0) or (t < 0) then
          BitBlt(AnimBmp.Canvas.Handle, 0, 0, acDstBmp.Width, acDstBmp.Height, acDstBmp.Canvas.Handle, 0, 0, SRCCOPY)
        else
          StretchBlt(AnimBmp.Canvas.Handle, Round(l), Round(t), Round(r - l), Round(b - t), acDstBmp.Canvas.Handle, 0, 0, acDstBmp.Width, acDstBmp.Height, SRCCOPY);

      else
        if l = 0 then begin
          BitBlt(AnimBmp.Canvas.Handle, 0, 0, acDstBmp.Width, acDstBmp.Height, acDstBmp.Canvas.Handle, 0, 0, SRCCOPY);
          l := 1;
        end
    end
  end;

  function Anim_GoToNext: boolean;
  begin
    Result := True;
    case AnimType of
      atDropDown: begin
        l := 0;
        t := 0;
        r := acDstBmp.Width - dx;
        b := acDstBmp.Height - dy;
        dx := dx / acwPopupDiv;
        dy := dy / acwPopupDiv;
        if (dx < 2) and (dy < 2) then
          Result := False;
      end;

      atAero: begin
        l := l - dx;
        t := t - dy;
        r := r + dx;
        b := b + dy;
      end
    end
  end;

  procedure UpdateBlend;
  begin
{$IFDEF DELPHI7UP}
    if sp.Form.AlphaBlend then
      DoLayered(sp.Form.Handle, True, sp.Form.AlphaBlendValue)
    else
{$ENDIF}
    begin
      // Update of form blend. Calling it, because "not WS_EX_LAYERED" style changes a form blending not always
      // Solving of the problem with empty form
      if AeroIsEnabled then
        DoLayered(sp.Form.Handle, True, MaxByte);

      if SetWindowLong(sp.Form.Handle, GWL_EXSTYLE, GetWindowLong(sp.Form.Handle, GWL_EXSTYLE) and not WS_EX_LAYERED) = 0 then
        CheckLastError;
    end;
  end;

begin
  if (sp.SkinData <> nil) and not IsIconic(sp.Form.Handle) then begin
    InAnimationProcess := True;
    if sp.BorderForm <> nil then begin
      if sp.BorderForm.AForm = nil then
        sp.BorderForm.CreateNewForm;

      AnimForm := sp.BorderForm.AForm;
      if SetWindowRgn(AnimForm.Handle, 0, False) = 0 then
        CheckLastError;

      sp.BorderForm.PaintAll;
    end
    else begin
      TAccessProvider(sp).PaintAll;
      AnimForm := TacGlowForm.CreateNew(Application);
    end;

    if sp.SkinData.FCacheBmp <> nil then begin
      acDstBmp := CreateBmp32(sp.SkinData.FCacheBmp);
{$IFDEF ACDEBUG}
      acDebugAnimBmp := acDstBmp;
{$ENDIF}
      acDstBmp.Canvas.Lock;
      SkinPaintTo(acDstBmp, sp.Form, 0, 0, sp);
      if sp.BorderForm = nil then
        FillAlphaRect(acDstBmp, MkRect(acDstBmp), MaxByte);

      if acDstBmp <> nil then begin
        acDstBmp.Canvas.UnLock;
        FBmpSize := MkSize(acDstBmp);
        StepCount := wTime div acTimerInterval;
        Flags := SWP_NOACTIVATE or SWP_NOREDRAW or SWP_NOCOPYBITS or SWP_NOOWNERZORDER or SWP_NOSENDCHANGING;
        FBmpTopLeft := MkPoint;
        if StepCount > 0 then
          InitBlendData(FBlend, 0)
        else
          InitBlendData(FBlend, MaxTransparency);

        cy := 0;
        cx := 0;
        if sp.BorderForm <> nil then begin
          fr := sp.Form.BoundsRect;
          TAccessProvider(sp).FSysExHeight := IsZoomed(sp.Form.Handle) and (sp.CaptionHeight < SysCaptHeight(sp.Form) + 4);
          if TAccessProvider(sp).FSysExHeight then
            cy := sp.ShadowSize.Top + DiffTitle(sp.BorderForm) + SysBorderWidth(sp.Form.Handle, sp.BorderForm, False) //  4
          else
            cy := sp.BorderForm.OffsetY;

          cx := SkinBorderWidth(sp.BorderForm) - SysBorderWidth(sp.Form.Handle, sp.BorderForm, False) + sp.ShadowSize.Left;
        end
        else begin
          GetWindowRect(sp.Form.Handle, fR);
          FillArOR(sp);
          CleanPixelsByRects(TAccessProvider(sp).Rects, acDstBmp);
        end;

        if SetWindowLong(AnimForm.Handle, GWL_EXSTYLE, GetWindowLong(AnimForm.Handle, GWL_EXSTYLE) or WS_EX_NOACTIVATE) = 0 then
          CheckLastError;

        if GetWindowLong(sp.Form.Handle, GWL_EXSTYLE) and WS_EX_TOPMOST <> 0 then begin
          TForm(AnimForm).FormStyle := fsStayOnTop;
          AnimForm.Width := 0;
          AnimForm.Height := 0;
          if SetWindowLong(AnimForm.Handle, GWL_EXSTYLE, GetWindowLong(AnimForm.Handle, GWL_EXSTYLE) or WS_EX_LAYERED) = 0 then
            CheckLastError;

          h := HWND_TOPMOST;
        end
        else begin
          if fsModal in sp.Form.FormState then
            TForm(AnimForm).FormStyle := fsStayOnTop;

          h := sp.Form.Handle;
        end;

        DC := GetDC(0);
        if GetWindowLong(AnimForm.Handle, GWL_EXSTYLE) and WS_EX_LAYERED = 0 then
          if SetWindowLong(AnimForm.Handle, GWL_EXSTYLE, GetWindowLong(AnimForm.Handle, GWL_EXSTYLE) or WS_EX_LAYERED) = 0 then
            CheckLastError;

        if not UpdateLayeredWindow(AnimForm.Handle, DC, nil, @FBmpSize, acDstBmp.Canvas.Handle, @FBmpTopLeft, clNone, @FBlend, ULW_ALPHA) then
          CheckLastError;

        AnimForm.SetBounds(fR.Left - cx + DebugOffsX, fR.Top - cy + DebugOffsY, acDstBmp.Width, acDstBmp.Height);

        if not SetWindowPos(AnimForm.Handle, 0, 0, 0, 0, 0, SWPA_SHOW or SWP_NOMOVE or SWP_NOSIZE) then
          CheckLastError;

        ShowWindow(AnimForm.Handle, SW_SHOWNOACTIVATE);

        if not SetWindowPos(AnimForm.Handle, h, AnimForm.Left, AnimForm.Top, FBmpSize.cx, FBmpSize.cy, Flags) then
          CheckLastError;

        AnimBmp := CreateBmp32(FBmpSize);
        FillDC(AnimBmp.Canvas.Handle, MkRect(AnimBmp), 0);
        SetStretchBltMode(AnimBmp.Canvas.Handle, COLORONCOLOR);

        if StepCount > 0 then begin
          Anim_Init;
          i := 0;
          while i <= StepCount do begin
            Anim_DoNext;
            lTicks := GetTickCount;
            if not UpdateLayeredWindow(AnimForm.Handle, DC, nil, @FBmpSize, AnimBmp.Canvas.Handle, @FBmpTopLeft, clNone, @FBlend, ULW_ALPHA) then
              CheckLastError;

            if not Anim_GoToNext then
              Break;

            inc(i);
            if StepCount > 0 then
              WaitTicks(lTicks);
          end;
          FBlend.SourceConstantAlpha := MaxTransparency;
        end;
        if not SetWindowPos(AnimForm.Handle, 0, fR.Left - cx + DebugOffsX, fr.Top - cy + DebugOffsY, FBmpSize.cx, FBmpSize.cy, Flags or SWP_NOZORDER) then
          CheckLastError;

        if not UpdateLayeredWindow(AnimForm.Handle, DC, nil, @FBmpSize, acDstBmp.Canvas.Handle, @FBmpTopLeft, clNone, @FBlend, ULW_ALPHA) then
          CheckLastError;

        FreeAndNil(AnimBmp);
        ReleaseDC(0, DC);
        if sp <> nil then begin
          sp.FInAnimation := False;

          DoLayered(sp.Form.Handle, True, 0);
          ShowWindow(sp.Form.Handle, ShowCommands[sp.Form.WindowState]);

          if not SetWindowPos(sp.Form.Handle, AnimForm.Handle, 0, 0, 0, 0, SWPA_ZORDER) then
            CheckLastError;

          UpdateBlend;

          InAnimationProcess := False;
          if sp.BorderForm <> nil then
            sp.BorderForm.ExBorderShowing := True; // Do not update extended borders

          if (Win32MajorVersion = 6) and (Win32MinorVersion = 0) then // Patch for Vista
            if SetWindowLong(sp.Form.Handle, GWL_STYLE, GetWindowLong(sp.Form.Handle, GWL_STYLE) or WS_VISIBLE) = 0 then // Form must be visible
              CheckLastError;

          if sp.BorderForm <> nil then
            sSkinProvider.UpdateRgn(sp, True, True); // Guarantees that region is updated

          if not RedrawWindow(sp.Form.Handle, nil, 0, RDWA_ALLNOW) then
            CheckLastError;

          if sp.BorderForm <> nil then
            sp.BorderForm.ExBorderShowing := False;

          FreeAndNil(acDstBmp);
          if AeroIsEnabled then
            Sleep(2 * acTimerInterval); // Removing of blinking in Aero

          if not SetWindowPos(AnimForm.Handle, sp.Form.Handle, 0, 0, 0, 0, SWPA_ZORDER) then
            CheckLastError;

          if sp.BorderForm = nil then
            FreeAndNil(AnimForm)
          else begin
            if SetWindowRgn(AnimForm.Handle, sp.BorderForm.MakeRgn, False) = 0 then
              CheckLastError;

//            if sp.SkinData.SkinManager.CommonSkinData.UseAeroBluring or (sp.SkinData.SkinManager.SysFontScale > 0) and (acWinVer > 7) then
//              sp.BorderForm.UpdateExBordersPos(True);

            UpdateBlend;
          end;
          if Assigned(sp.OnAfterAnimation) then
            sp.OnAfterAnimation(aeShowing);
        end;
      end;
    end;
  end;
end;


procedure HideAnimForm(Form: TacGlowForm; SrcBmp: TBitmap; ATime: integer; StartBlendValue: integer; AnimType: TacAnimType; ParentWnd: THandle);
var
  h: hwnd;
  lTicks: DWord;
begin
  if (acHideTimer <> nil) and (acHideTimer.Form <> nil) then
    FreeAndNil(acHideTimer.Form);

  if ATime div acTimerInterval = 0 then begin
    FreeAndNil(Form);
    FreeAndNil(SrcBmp);
  end
  else begin
    if acHideTimer = nil then
      acHideTimer := TacHideTimer.Create(Application);

    acHidetimer.ParentWnd := ParentWnd;
    acHideTimer.EventCalled := False;
    acHideTimer.i := 0;
    if (acHideTimer.SrcBmp <> nil) and (acHideTimer.SrcBmp <> SrcBmp) then
      FreeAndNil(acHideTimer.SrcBmp);

    acHideTimer.SrcBmp := SrcBmp;
    acHideTimer.StartBlendValue := StartBlendValue;
    acHideTimer.Form := Form;
    acHideTimer.AnimType := AnimType;
    acHideTimer.Interval := acTimerInterval;

    acHideTimer.StepCount := ATime div acTimerInterval;

    acHideTimer.FBmpSize := MkSize(acHideTimer.SrcBmp);
    acHideTimer.FBmpTopLeft := MkPoint;

    InitBlendData(acHideTimer.FBlend, StartBlendValue);

    acHideTimer.DC := GetDC(0);

    SetWindowLong(Form.Handle, GWL_EXSTYLE, GetWindowLong(Form.Handle, GWL_EXSTYLE) or WS_EX_LAYERED or WS_EX_NOACTIVATE);

    UpdateLayeredWindow(Form.Handle, acHideTimer.DC, nil, @acHideTimer.FBmpSize, SrcBmp.Canvas.Handle, @acHideTimer.FBmpTopLeft, clNone, @acHideTimer.FBlend, ULW_ALPHA);
    SetWindowPos(Form.Handle, 0, 0, 0, 0, 0, SWPA_NOCOPYBITS);
    ShowWindow(Form.Handle, SW_SHOWNOACTIVATE);
//    Form.Show;

    if GetWindowLong(Form.Handle, GWL_EXSTYLE) and WS_EX_TOPMOST <> 0 then
      h := HWND_TOPMOST
    else
      h := Form.Handle;

    SetWindowPos(Form.Handle, h, Form.Left, Form.Top, acHideTimer.FBmpSize.cx, acHideTimer.FBmpSize.cy, SWP_NOACTIVATE or SWP_NOREDRAW or SWP_NOCOPYBITS or SWP_NOOWNERZORDER or SWP_NOREDRAW);
    FreeAndNil(acHideTimer.DstBmp);

    acHideTimer.DstBmp := CreateBmp32(acHideTimer.FBmpSize);
    ReleaseDC(0, acHideTimer.DC);
    acHideTimer.Anim_Init;
    if not Application.Terminated then begin
      acHideTimer.OnTimer := acHideTimer.OnTimerProc;
      acHideTimer.i := 0;
      acHideTimer.OnTimerProc(acHideTimer);
      acHideTimer.Enabled := True;
    end
    else
      while acHideTimer.i <= acHideTimer.StepCount do begin
        lTicks := GetTickCount;
        acHideTimer.OnTimerProc(acHideTimer);
        WaitTicks(lTicks);
      end;
  end;
end;


procedure AnimHideForm(SkinProvider: TObject);
var
  sp: TAccessProvider;
  AnimForm: TacGlowForm;
  acDstBmp: TBitmap;
  b: byte;
begin
  ClearGlows(True);
  sp := TAccessProvider(SkinProvider);
  if (sp <> nil) and (sp.Form.FormStyle <> fsMDIChild) then begin
    if sp.SkinData.FCacheBmp <> nil then begin
{$IFDEF DELPHI7UP}
      if sp.Form.AlphaBlend then
        b := sp.Form.AlphaBlendValue
      else
{$ENDIF}
        b := MaxByte;

      InAnimationProcess := True;
      if sp.FormState and FS_ANIMCLOSING = 0 then
        sp.PaintAll;

      if sp.BorderForm <> nil then begin
        if sp.BorderForm.AForm = nil then
          sp.BorderForm.CreateNewForm;

        AnimForm := sp.BorderForm.AForm;
        if sp.BorderForm.ParentHandle <> 0 then
          SetWindowLong(sp.BorderForm.AForm.Handle, GWL_HWNDPARENT, LONG_PTR(sp.BorderForm.ParentHandle));

        AnimForm.WindowProc := sp.BorderForm.OldBorderProc;
        acDstBmp := CreateBmp32(sp.SkinData.FCacheBmp);
{$IFDEF ACDEBUG}
        acDebugAnimBmp := acDstBmp;
{$ENDIF}
        BitBlt(acDstBmp.Canvas.Handle, 0, 0, sp.SkinData.FCacheBmp.Width, sp.SkinData.FCacheBmp.Height, sp.SkinData.FCacheBmp.Canvas.Handle, 0, 0, SRCCOPY);
        sp.BorderForm.AForm := nil;
{.$IFDEF D2011}
        if not AeroIsEnabled then
          SetWindowPos(sp.Form.Handle, 0, 0, 0, 0, 0, SWP_HIDEWINDOW or SWP_NOSIZE or SWP_NOMOVE)
        else
          DoLayered(sp.Form.Handle, True, 1);
{.$ENDIF}
      end
      else begin
        if sp.CoverForm <> nil then begin
          AnimForm := sp.CoverForm;
          sp.CoverForm := nil;
        end
        else
          AnimForm := MakeCoverForm(sp.Form.Handle);

        DoLayered(sp.Form.Handle, True, 1);
        acDstBmp := CreateBmp32(sp.SkinData.FCacheBmp);
{$IFDEF ACDEBUG}
        acDebugAnimBmp := acDstBmp;
{$ENDIF}
        BitBlt(acDstBmp.Canvas.Handle, 0, 0, sp.SkinData.FCacheBmp.Width, sp.SkinData.FCacheBmp.Height, sp.SkinData.FCacheBmp.Canvas.Handle, 0, 0, SRCCOPY);
        CleanPixelsByRects(sp.Rects, acDstBmp);
      end;
      ////////////////////////////////
      HideAnimForm(AnimForm, acDstBmp, sp.SkinData.SkinManager.AnimEffects.FormHide.Time, b{MaxByte}, sp.SkinData.SkinManager.AnimEffects.FormHide.Mode, sp.Form.Handle);
      if sp.Form <> nil then
        DoLayered(sp.Form.Handle, b <> MaxByte, b);

      if sp.BorderForm <> nil then
        if Application.Terminated then
          FreeAndNil(sp.BorderForm)
        else
          FreeAndNil(sp.BorderForm.AForm);

        FreeAndNil(sp.CoverForm);
      ////////////////////////////////
      InAnimationProcess := False;
    end;
  end;
end;


procedure PrintDlgClient(ListSW: TacDialogWnd; acDstBmp: TBitmap; CopyScreen: boolean = False);
var
  SavedDC, DC: hdc;
  R, cR, fR: TRect;
begin
  if CopyScreen then begin
    DC := GetWindowDC(ListSW.CtrlHandle);
    SavedDC := SaveDC(DC);
    try
      R := ACClientRect(ListSW.CtrlHandle);
      BitBlt(acDstBmp.Canvas.Handle, ListSW.OffsetX, ListSW.OffsetY, WidthOf(R), HeightOf(R), DC, R.Left, R.Top, SRCCOPY);
      FillAlphaRect(acDstBmp, Rect(ListSW.OffsetX, ListSW.OffsetY, min(ListSW.OffsetX + WidthOf(R), acDstBmp.Width), min(ListSW.OffsetY + HeightOf(R), acDstBmp.Height)), MaxByte);
    finally
      RestoreDC(DC, SavedDC);
      ReleaseDC(ListSW.CtrlHandle, DC);
    end;
  end
  else begin
    GetClientRect(ListSW.CtrlHandle, cR);
    acDstBmp.Canvas.Lock;
    SavedDC := SaveDC(acDstBmp.Canvas.Handle);

    fR.TopLeft := Point(ListSW.OffsetX, ListSW.OffsetY);

    MoveWindowOrg(acDstBmp.Canvas.Handle, fR.Left, fR.Top);
    IntersectClipRect(acDstBmp.Canvas.Handle, 0, 0, WidthOf(cR), HeightOf(cR));
    ListSW.Provider.PrintHwndControls(ListSW.CtrlHandle, acDstBmp.Canvas.Handle);

    if ListSW.BorderForm <> nil then begin
      fR.TopLeft := Point(ListSW.OffsetX, ListSW.OffsetY);
      fR.Right := fR.Left + WidthOf(cR);
      fR.Bottom := fR.Top + HeightOf(cR);
      FillAlphaRect(acDstBmp, fR, MaxByte);
    end
    else
      FillAlphaRect(acDstBmp, MkRect(acDstBmp), MaxByte);

    RestoreDC(acDstBmp.Canvas.Handle, SavedDC);
    acDstBmp.Canvas.UnLock;
  end;
end;


procedure AnimHideDlg(ListSW: TacDialogWnd);
var
  AnimForm: TacGlowForm;
  acDstBmp: TBitmap;
begin
  InAnimationProcess := True;
  ClearGlows;
  if ListSW.BorderForm <> nil then begin
    AnimForm := ListSW.BorderForm.AForm;
    if ListSW.BorderForm.ParentHandle <> 0 then
      SetWindowLong(ListSW.BorderForm.AForm.Handle, GWL_HWNDPARENT, LONG_PTR(ListSW.BorderForm.ParentHandle));

    AnimForm.WindowProc := ListSW.BorderForm.OldBorderProc;
    acDstBmp := CreateBmp32(ListSW.SkinData.FCacheBmp);
{$IFDEF ACDEBUG}
      acDebugAnimBmp := acDstBmp;
{$ENDIF}
    BitBlt(acDstBmp.Canvas.Handle, 0, 0, ListSW.SkinData.FCacheBmp.Width, ListSW.SkinData.FCacheBmp.Height, ListSW.SkinData.FCacheBmp.Canvas.Handle, 0, 0, SRCCOPY);
  end
  else begin
    if ListSW.CoverForm <> nil then
      AnimForm := ListSW.CoverForm
    else
      AnimForm := MakeCoverForm(ListSW.CtrlHandle);

    acDstBmp := CreateBmp32(ListSW.SkinData.FCacheBmp);
{$IFDEF ACDEBUG}
    acDebugAnimBmp := acDstBmp;
{$ENDIF}
    BitBlt(acDstBmp.Canvas.Handle, 0, 0, acDstBmp.Width, acDstBmp.Height, ListSW.SkinData.FCacheBmp.Canvas.Handle, 0, 0, SRCCOPY);
    if ListSW.BorderForm = nil then
      CleanPixelsByRects(ListSW.ArOR, acDstBmp);
  end;
  HideAnimForm(AnimForm, acDstBmp, ListSW.SkinData.SkinManager.AnimEffects.DialogHide.Time, MaxByte, ListSW.SkinData.SkinManager.AnimEffects.DialogHide.Mode, 0);
  if ListSW.BorderForm <> nil then begin
    ListSW.BorderForm.AForm := nil;
    FreeAndNil(ListSW.BorderForm);
  end;
  InAnimationProcess := False;
end;


function DoLayered(FormHandle: Hwnd; Layered: boolean; AlphaValue: byte = 1): boolean;
begin
  Result := False;
  if Layered and acLayered then begin
    if GetWindowLong(FormHandle, GWL_EXSTYLE) and WS_EX_LAYERED = 0 then begin
      if SetWindowLong(FormHandle, GWL_EXSTYLE, GetWindowLong(FormHandle, GWL_EXSTYLE) or WS_EX_LAYERED) = 0 then
        CheckLastError;

      Result := True;
    end;
    if not SetLayeredWindowAttributes(FormHandle, clNone, AlphaValue, ULW_ALPHA) then
      CheckLastError;
  end
  else begin
    if SetWindowLong(FormHandle, GWL_STYLE, GetWindowLong(FormHandle, GWL_STYLE) and not WS_VISIBLE) = 0 then // Avoid a form showing
      CheckLastError;

    if SetWindowLong(FormHandle, GWL_EXSTYLE, GetWindowLong(FormHandle, GWL_EXSTYLE) and not WS_EX_LAYERED) = 0 then
            CheckLastError;
  end;
end;


function acShowHintWnd(HintText: string; Pos: TPoint): {$IFDEF TNTUNICODE}TTntHintWindow;{$ELSE}THintWindow;{$ENDIF}
var
  R, wR: TRect;
{$IFNDEF DELPHI5}
  Animate: BOOL;
{$ENDIF}
begin
  Result := nil;
{$IFNDEF ALITE}
  if (Manager <> nil) and Manager.Active then begin
    Manager.ShowHint(Pos, HintText, Application.HintHidePause);
    Result := Manager.HintWindow;
  end
  else
{$ENDIF}
    if HintText <> '' then begin
{$IFDEF TNTUNICODE}
      Result := TTntHintWindow.Create(nil);
{$ELSE}
      Result := HintWindowClass.Create(nil);
{$ENDIF}
      with Result do begin
        Visible := False;
        Color := clInfoBk;
        R := CalcHintRect(800, HintText, nil);
        OffsetRect(R, Pos.X + 8, Pos.Y + 16);
        wR := acWorkRect(Pos);
        if R.Right > wR.Right then
          OffsetRect(R, wR.Right - R.Right, 0);

        if R.Bottom > wR.Bottom then
          OffsetRect(R, 0, wR.Bottom - R.Bottom);

        if TrySendMessage(Handle, SM_ALPHACMD, MakeWParam(0, 7 {AC_CTRLHANDLED}), 0) = 1 then
          ActivateHint(R, HintText)
        else begin
          // < Solving the "Owner Z-ordering" problem when BorderForm is used
          Caption := HintText;
          Inc(R.Bottom, 4);

          ParentWindow := {$IFNDEF FPC}Application.Handle{$ELSE}Application.MainFormHandle{$ENDIF};
          SetBounds(R.Left, R.Top, WidthOf(R, True), HeightOf(R, True));
          SetWindowPos(Handle, HWND_TOPMOST, R.Left, R.Top, Width, Height,
                       SWP_NOACTIVATE or SWP_NOSIZE or SWP_NOMOVE or SWP_NOOWNERZORDER);
{$IFNDEF DELPHI5}
{$IFNDEF FPC}
          if (Length(HintText) < 100) and Assigned(AnimateWindowProc) then begin
            SystemParametersInfo(SPI_GETTOOLTIPANIMATION, 0, {$IFNDEF CLR}@{$ENDIF}Animate, 0);
            if Animate then begin
              SystemParametersInfo(SPI_GETTOOLTIPFADE, 0, {$IFNDEF CLR}@{$ENDIF}Animate, 0);
              if Animate then
                AnimateWindowProc(Handle, 200, AW_BLEND);
            end;
          end;
{$ENDIF}
{$ENDIF}
          ShowWindow(Handle, SW_SHOWNOACTIVATE);
          Invalidate;
          // >
        end;
      end;
    end;
end;


{$IFNDEF ALITE}
procedure acHideHintWnd(var Wnd: {$IFDEF TNTUNICODE}TTntHintWindow{$ELSE}THintWindow{$ENDIF});
begin
  if (Manager <> nil) and Manager.Active and (Manager.HintWindow <> nil) then
    FreeAndNil(Manager.HintWindow)
  else
    FreeAndNil(Wnd);
end;
{$ENDIF}


function acWorkRect(Form: TForm): TRect;
begin
{$IFDEF DELPHI6UP}
  Result := Form.Monitor.WorkareaRect;
{$ELSE}
  SystemParametersInfo(SPI_GETWORKAREA, 0, @Result, 0);
{$ENDIF}
end;


function acWorkRect(Coord: TPoint): TRect; overload;
{$IFDEF DELPHI6UP}
var
  i: integer;
{$ENDIF}
begin
{$IFDEF DELPHI6UP}
  Result := MkRect;
  for i := 0 to Screen.MonitorCount - 1 do
    if PtInRect(Screen.Monitors[i].WorkareaRect, Coord) then begin
      Result := Screen.Monitors[i].WorkareaRect;
      Exit;
    end;
{$ELSE}
  SystemParametersInfo(SPI_GETWORKAREA, 0, @Result, 0);
{$ENDIF}
end;


procedure AnimShowDlg(ListSW: TacDialogWnd; wTime: word = 0; MaxTransparency: integer = MaxByte; AnimType: TacAnimType = atAero);
var
  dx, dy, l, t, r, b, trans, p: real;
  cy, cx, i, StepCount: integer;
  DstBmp, AnimBmp: TBitmap;
  FBlend: TBlendFunction;
  AnimForm: TacGlowForm;
  FBmpTopLeft: TPoint;
  PrintDC, DC: hdc;
  Flags: Cardinal;
  FBmpSize: TSize;
  lTicks: DWord;
  fR, cR: TRect;
  h: hwnd;

  procedure Anim_Init;
  begin
    trans := 0;
    p := MaxTransparency / (StepCount + 1);
    case AnimType of
      atDropDown: begin
        l := 0;
        t := 0;
        r := DstBmp.Width;
        b := 0;
        dx := (DstBmp.Width  - r) / acwPopupDiv;
        dy := (DstBmp.Height - b) / acwPopupDiv;
      end;

      atAero: begin
        l := DstBmp.Width / acwDivider;
        t := DstBmp.Height / acwDivider;
        dx := l / StepCount;
        dy := t / StepCount;
        r := DstBmp.Width - l;
        b := DstBmp.Height - t;
      end

      else begin
        dx := 0;
        dy := 0;
        l := 0;
        t := 0;
        r := 0;
        b := 0;
      end;
    end
  end;

  procedure Anim_DoNext;
  begin
    trans := min(trans + p, MaxTransparency);
    FBlend.SourceConstantAlpha := Round(trans);
    case AnimType of
      atDropDown: begin
        FBlend.SourceConstantAlpha := MaxByte;
        if (l < 0) or (t < 0) then
          BitBlt(AnimBmp.Canvas.Handle, 0, 0, DstBmp.Width, DstBmp.Height, DstBmp.Canvas.Handle, 0, 0, SRCCOPY)
        else
          StretchBlt(AnimBmp.Canvas.Handle, Round(l), Round(t), Round(r - l), Round(b - t), DstBmp.Canvas.Handle, 0, 0, DstBmp.Width, DstBmp.Height, SRCCOPY);
      end;

      atAero:
        if (l < 0) or (t < 0) then
          BitBlt(AnimBmp.Canvas.Handle, 0, 0, DstBmp.Width, DstBmp.Height, DstBmp.Canvas.Handle, 0, 0, SRCCOPY)
        else
          StretchBlt(AnimBmp.Canvas.Handle, Round(l), Round(t), Round(r - l), Round(b - t), DstBmp.Canvas.Handle, 0, 0, DstBmp.Width, DstBmp.Height, SRCCOPY);

      else
        if l = 0 then begin
          BitBlt(AnimBmp.Canvas.Handle, 0, 0, DstBmp.Width, DstBmp.Height, DstBmp.Canvas.Handle, 0, 0, SRCCOPY);
          l := 1;
        end
    end
  end;

  function Anim_GoToNext: boolean;
  begin
    Result := True;
    case AnimType of
      atDropDown: begin
        l := 0;
        t := 0;
        r := DstBmp.Width - dx;
        b := DstBmp.Height - dy;
        dx := dx / acwPopupDiv;
        dy := dy / acwPopupDiv;
        if (dx < 2) and (dy < 2) then
          Result := False;
      end;

      atAero: begin
        l := l - dx;
        t := t - dy;
        r := r + dx;
        b := b + dy;
      end
    end
  end;

begin
  InAnimationProcess := True;
  if ListSW.BorderForm <> nil then
    AnimForm := ListSW.BorderForm.AForm
  else
    AnimForm := TacGlowForm.CreateNew(Application);

  ListSW.PaintAll;
  GetClientRect(ListSW.CtrlHandle, cR);
  DstBmp := CreateBmp32(ListSW.SkinData.FCacheBmp);
  BitBlt(DstBmp.Canvas.Handle, 0, 0, DstBmp.Width, DstBmp.Height, ListSW.SkinData.FCacheBmp.Canvas.Handle, 0, 0, SRCCOPY);
  DstBmp.Canvas.Lock;
  PrintDC := DstBmp.Canvas.Handle;

  DC := SaveDC(PrintDC);
  fR.TopLeft := Point(ListSW.OffsetX, ListSW.OffsetY);
  MoveWindowOrg(PrintDC, fR.Left, fR.Top);
  IntersectClipRect(PrintDC, 0, 0, WidthOf(cR), HeightOf(cR));
  ListSW.Provider.PrintHwndControls(ListSW.CtrlHandle, PrintDC);

  if ListSW.BorderForm <> nil then begin
    fR.TopLeft := Point(ListSW.OffsetX, ListSW.OffsetY);
    fR.Right := fR.Left + WidthOf(cR);
    fR.Bottom := fR.Top + HeightOf(cR);
    FillAlphaRect(DstBmp, fR, MaxByte);
  end
  else
    FillAlphaRect(DstBmp, MkRect(DstBmp), MaxByte);

  RestoreDC(PrintDC, DC);
  if DstBmp = nil then
    Exit;

  DstBmp.Canvas.UnLock;
  FBmpSize := MkSize(DstBmp);
  StepCount := wTime div acTimerInterval;
  Flags := SWP_NOACTIVATE or SWP_NOREDRAW or SWP_NOCOPYBITS or SWP_NOSENDCHANGING or SWP_NOOWNERZORDER;
  FBmpTopLeft := MkPoint;
  if StepCount > 0 then
    InitBlendData(FBlend, 0)
  else
    InitBlendData(FBlend, MaxTransparency);

  if ListSW.BorderForm <> nil then begin
    cy := SkinTitleHeight(ListSW.BorderForm) + ListSW.ShadowSize.Top - ListSW.CaptionHeight(False) - SysBorderWidth(ListSW.CtrlHandle, ListSW.BorderForm, False);
    cx := SkinBorderWidth(ListSW.BorderForm) - SysBorderWidth(ListSW.CtrlHandle, ListSW.BorderForm, False) + ListSW.ShadowSize.Left;
  end
  else begin
    cy := 0;
    cx := 0;
  end;
  GetWindowRect(ListSW.CtrlHandle, fR);
  if GetWindowLong(ListSW.CtrlHandle, GWL_EXSTYLE) and WS_EX_TOPMOST <> 0 then begin
    TForm(AnimForm).FormStyle := fsStayOnTop;
    h := HWND_TOPMOST;
  end
  else
    h := ListSW.CtrlHandle;

  if ListSW.BorderForm = nil then begin
    FillDlgArOR(ListSW);
    CleanPixelsByRects(ListSW.ArOR, DstBmp);
  end;

  DC := GetDC(0);
  SetWindowLong(AnimForm.Handle, GWL_EXSTYLE, GetWindowLong(AnimForm.Handle, GWL_EXSTYLE) or WS_EX_LAYERED or WS_EX_NOACTIVATE);
  AnimForm.SetBounds(fR.Left - cx, fR.Top - cy, DstBmp.Width, DstBmp.Height);
  UpdateLayeredWindow(AnimForm.Handle, DC, nil, @FBmpSize, DstBmp.Canvas.Handle, @FBmpTopLeft, clNone, @FBlend, ULW_ALPHA);
  ShowWindow(AnimForm.Handle, SW_SHOWNOACTIVATE);

  SetWindowPos(AnimForm.Handle, h, AnimForm.Left, AnimForm.Top, FBmpSize.cx, FBmpSize.cy, Flags or SWP_NOREDRAW);
  AnimBmp := CreateBmp32(FBmpSize);
  FillDC(AnimBmp.Canvas.Handle, MkRect(AnimBmp), 0);
  SetStretchBltMode(AnimBmp.Canvas.Handle, COLORONCOLOR);

  if StepCount > 0 then begin
    Anim_Init;
    i := 0;
    while i <= StepCount do begin
      lTicks := GetTickCount;
      Anim_DoNext;
      UpdateLayeredWindow(AnimForm.Handle, DC, nil, @FBmpSize, AnimBmp.Canvas.Handle, @FBmpTopLeft, clNone, @FBlend, ULW_ALPHA);
      if not Anim_GoToNext then
        Break;

      inc(i);
      if StepCount > 0 then
        WaitTicks(lTicks);
    end;
    FBlend.SourceConstantAlpha := MaxTransparency;
  end;
  SetWindowRgn(AnimForm.Handle, 0, False);
  SetWindowPos(AnimForm.Handle, 0, 0, 0, 0, 0, Flags or SWP_NOSIZE or SWP_NOMOVE);
  UpdateLayeredWindow(AnimForm.Handle, DC, nil, @FBmpSize, DstBmp.Canvas.Handle, @FBmpTopLeft, clNone, @FBlend, ULW_ALPHA);

  FreeAndNil(AnimBmp);
  ReleaseDC(0, DC);
  acDials.UpdateRgn(ListSW, False);

  SetWindowPos(ListSW.CtrlHandle, AnimForm.Handle, 0, 0, 0, 0, SWPA_ZORDER);
  SetWindowLong(ListSW.CtrlHandle, GWL_STYLE, GetWindowLong(ListSW.CtrlHandle, GWL_STYLE) or WS_VISIBLE); // Form must be visible

  ListSW.ProcessMessage(WM_SETREDRAW, 1); // Vista
  InAnimationProcess := False;

  if ListSW.BorderForm <> nil then begin
    ListSW.BorderForm.ExBorderShowing := True;
    RedrawWindow(ListSW.CtrlHandle, nil, 0, RDWA_ALLNOW);
    acDials.UpdateRgn(ListSW, False);
    ListSW.BorderForm.ExBorderShowing := False;
    if AeroIsEnabled then
      Sleep(4 * acTimerInterval); // Blinking in Aero removing

    ListSW.BorderForm.UpdateExBordersPos(True); // Guarantees that client area will be visible
    SetWindowPos(AnimForm.Handle, ListSW.CtrlHandle, 0, 0, 0, 0, Flags or SWP_NOSIZE or SWP_NOMOVE);
    acDials.UpdateRgn(ListSW, False{True}); // Guarantees that region is updated
  end
  else begin
    RedrawWindow(ListSW.CtrlHandle, nil, 0, RDWA_ALLNOW);
    if AeroIsEnabled then
      Sleep(4 * acTimerInterval); // Blinking in Aero removing

    SetWindowPos(AnimForm.Handle, ListSW.CtrlHandle, 0, 0, 0, 0, Flags or SWP_NOSIZE or SWP_NOMOVE);
    FreeAndNil(AnimForm);
  end;
  FreeAndNil(DstBmp);
end;


var
  acAnimBmp:  TBitmap = nil;
  acAnimBmp2: TBitmap = nil;
  acCtrlRects1: TRects;

procedure PrepareForAnimation(const Ctrl: TWinControl; AnimType: TacAnimTypeCtrl = atcFade);
var
  Y, X, j, DeltaS, DeltaD: integer;
  CtrlsParent: TWinControl;
  D0, D: PRGBAArray_D;
  S0, S: PRGBAArray_S;
  ScrDC, SavedDC: hdc;
  BGInfo: TacBGInfo;
  R: TRect;
begin
  if IsWindowVisible(Ctrl.Handle) and not IsIconic(GetRootParent(Ctrl.Handle)) then begin
    GetWindowRect(Ctrl.Handle, R);
    if acAnimBmp = nil then
      acAnimBmp := CreateBmp32(Ctrl.width, Ctrl.Height);

    ScrDC := GetDC(0);
    BitBlt(acAnimBmp.Canvas.Handle, 0, 0, Ctrl.width, Ctrl.Height, ScrDC, R.Left, R.Top, SRCCOPY);
    ReleaseDC(0, ScrDC);

    case AnimType of
      atcBlur: begin
        CtrlsParent := Ctrl;
        if AnimType = atcBlur then begin
          for j := 0 to Ctrl.ControlCount - 1 do
            if (Ctrl.Controls[j] is TWinControl) and
                 Ctrl.Controls[j].Visible and
                   (Ctrl.Controls[j].Align = alClient) then begin
              CtrlsParent := TWinControl(Ctrl.Controls[j]); // Found better parent
              Break;
            end;

          Y := 0;
          SetLength(acCtrlRects1, 0);
          for X := 0 to CtrlsParent.ControlCount - 1 do
            if CtrlsParent.Controls[X].Visible then begin
              inc(Y);
              SetLength(acCtrlRects1, Y);
              acCtrlRects1[Y - 1] := CtrlsParent.Controls[Y - 1].BoundsRect;
              with acCtrlRects1[Y - 1] do begin
                if Left < 0 then begin
                  Left := 0;
                  if Right < Left then
                    Right := Left;
                end
                else
                  if Right > CtrlsParent.Width then begin
                    Right := CtrlsParent.Width;
                    if Left > Right then
                      Left := Right;
                  end;

                if Top < 0 then begin
                  Top := 0;
                  if Bottom < Top then
                    Bottom := Top;
                end
                else
                  if Bottom > CtrlsParent.Height then begin
                    Bottom := CtrlsParent.Height;
                    if Top > Bottom then
                      Top := Bottom;
                  end;
              end;
            end;
        end;
        if acAnimBmp2 = nil then
          acAnimBmp2 := CreateBmpLike(acAnimBmp);
        // Draw BG of control
        SavedDC := SaveDC(acAnimBmp2.Canvas.Handle);
        try
          acAnimBmp2.Canvas.Lock;
          BGInfo.DrawDC := acAnimBmp2.Canvas.Handle;
          BGInfo.R := MkRect(Ctrl);
          BGInfo.Offset := MkPoint;
          BGInfo.BgType := btUnknown;
          GetBGInfo(@BGInfo, Ctrl.Handle, True);
          acAnimBmp2.Canvas.Unlock;
        finally
          RestoreDC(acAnimBmp2.Canvas.Handle, SavedDC);
        end;
        // Leave not equal pixels only
        if InitLine(acAnimBmp2, Pointer(S0), DeltaS) and InitLine(acAnimBmp, Pointer(D0), DeltaD) then
          for Y := 0 to acAnimBmp2.Height - 1 do begin
            D := Pointer(PAnsiChar(D0) + DeltaD * Y);
            S := Pointer(PAnsiChar(S0) + DeltaS * Y);
            for X := 0 to acAnimBmp2.Width - 1 do
              with S[X], D[X] do
                if (DR = SR) and (DG = SG) and (DB = SB) then
                  DA := 0
                else
                  DA := MaxByte;
          end;
      end;
    end;

    if ow = nil then
      ow := TOutPutWindow.Create(Application);

    if Ctrl.Parent <> nil then begin
      ow.Parent := Ctrl.Parent;
      if ow = nil then
        Exit;

      ow.BringToFront;
      ow.BoundsRect := Ctrl.BoundsRect;
    end
    else
      ow.BoundsRect := R;

    if ow.Parent = nil then
      SetWindowPos(ow.Handle, GetWindow(TWinControl(Ctrl).Handle, GW_HWNDPREV), 0, 0, 0, 0, SWP_NOZORDER or SWP_SHOWWINDOW or SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE)
    else
      ShowWindow(ow.Handle, SW_SHOWNA);
  end;
end;


procedure SumBmpRect32(BmpTo: TBitmap; RectTo: TRect; BmpFrom: TBitmap; PointFrom: TPoint; BlendValue: byte = MaxByte);
var
  DeltaS, DeltaD, Y, X, Y1, X1: integer;
  D, D0: PRGBAArray_D;
  S, S0: PRGBAArray_;
  hh, ww: integer;
  aa_, aa: byte;
begin
  // Correct Left
  if RectTo.Left < 0 then begin
    PointFrom.X := PointFrom.X - RectTo.Left;
    RectTo.Left := 0;
  end;
  if PointFrom.X < 0 then begin
    RectTo.Left := RectTo.Left - PointFrom.X;
    PointFrom.X := 0;
  end;
  // Correct Top
  if RectTo.Top < 0 then begin
    PointFrom.Y := PointFrom.Y - RectTo.Top;
    RectTo.Top := 0;
  end;
  if PointFrom.Y < 0 then begin
    RectTo.Top := RectTo.Top - PointFrom.Y;
    PointFrom.Y := 0;
  end;
  // Correct width
  ww := WidthOf(RectTo, True);
  if RectTo.Left + ww > BmpTo.Width then begin
    ww := BmpTo.Width - RectTo.Left;
    RectTo.Right := RectTo.Left + ww;
  end;
  if PointFrom.X + ww > BmpFrom.Width then begin
    ww := BmpFrom.Width - PointFrom.X;
    RectTo.Right := RectTo.Left + ww;
  end;
  if ww > 0 then begin
    // Correct height
    hh := HeightOf(RectTo, True);
    if RectTo.Top + hh > BmpTo.Height then begin
      hh := BmpTo.Height - RectTo.Top;
      RectTo.Bottom := RectTo.Top + hh;
    end;
    if PointFrom.Y + hh > BmpFrom.Height then begin
      hh := BmpFrom.Height - PointFrom.Y;
      RectTo.Bottom := RectTo.Top + hh;
    end;
    if hh > 0 then begin
      Y1 := PointFrom.Y;
      dec(RectTo.Bottom);
      dec(RectTo.Right);
      if InitLine(BmpFrom, Pointer(S0), DeltaS) and InitLine(BmpTo, Pointer(D0), DeltaD) then
        for Y := RectTo.Top to RectTo.Bottom do begin
          D := Pointer(PAnsiChar(D0) + DeltaD * Y);
          S := Pointer(PAnsiChar(S0) + DeltaS * Y1);
          X1 := PointFrom.X;
          for X := RectTo.Left to RectTo.Right do begin
            with S[X1], D[X] do
              if A <> 0 then begin
                aa := (A * BlendValue) shr 8;
                aa_ := MaxByte - aa;
                DR := (DR * aa_ + R * aa) shr 8;
                DG := (DG * aa_ + G * aa) shr 8;
                DB := (DB * aa_ + B * aa) shr 8;
              end;

            inc(X1);
          end;
          inc(Y1);
        end;
    end;
  end;
end;


procedure AnimShowControl(Ctrl: TWinControl; wTime: word = 0; MaxTransparency: integer = MaxByte; AnimType: TacAnimTypeCtrl = atcFade);
var
  h: hwnd;
  fR: TRect;
  rgn: hrgn;
  lTicks: DWord;
  Flags: Cardinal;
  FBmpSize: TSize;
  SavedDC, DC: hdc;
  BGInfo: TacBGInfo;
  S0, S: PRGBAArray_S;
  FBmpTopLeft: TPoint;
  OldAlphaForm: TacGlowForm;
  sp: TAccessProvider;
  D0, D: PRGBAArray_D;
  FBlend: TBlendFunction;
  bExtendedBorders: Boolean;
  acCtrlRects2: TRects;
  dx, dy, l, t, rr, bb, trans, prc, Percent1, Percent2, p, BlurKef: real;
  NewBmp, BGBmp, BlurBmp, TmpBmp, AnimBmp, acDstBmp, OldAlphaBmp: TBitmap;
  k, j, n, StepCount, cy, cx, Y, X, DeltaS, DeltaD: integer;

  procedure Anim_Init;
  begin
    trans := 0;
    p := MaxTransparency / StepCount;
    case AnimType of
      atcAero: begin
        l  := acDstBmp.Width  / acwDivider;
        t  := acDstBmp.Height / acwDivider;
        dx := l / StepCount;
        dy := t / StepCount;
        rr := acDstBmp.Width  - l;
        bb := acDstBmp.Height - t;
      end

      else begin
        dx := 0;
        dy := 0;
        l  := 0;
        t  := 0;
        rr := 0;
        bb := 0;
      end;
    end
  end;

  procedure Anim_DoNext;
  begin
    trans := min(trans + p, MaxTransparency);
    FBlend.SourceConstantAlpha := Round(trans);
    case AnimType of
      atcAero:
        if (l < 0) or (t < 0) then
          BitBlt(AnimBmp.Canvas.Handle, 0, 0, acDstBmp.Width, acDstBmp.Height, acDstBmp.Canvas.Handle, 0, 0, SRCCOPY)
        else
          StretchBlt(AnimBmp.Canvas.Handle, Round(l), Round(t), Round(rr - l), Round(bb - t), acDstBmp.Canvas.Handle, 0, 0, acDstBmp.Width, acDstBmp.Height, SRCCOPY);

      else
        if l = 0 then begin
          BitBlt(AnimBmp.Canvas.Handle, 0, 0, acDstBmp.Width, acDstBmp.Height, acDstBmp.Canvas.Handle, 0, 0, SRCCOPY);
          l := 1;
        end;
    end
  end;

  procedure Anim_GoToNext;
  begin
    case AnimType of
      atcAero: begin
        l := l - dx;
        t := t - dy;
        rr := rr + dx;
        bb := bb + dy;
      end
    end
  end;

  function DoScale(Value: integer): integer;
  begin
    Result := Value * 7 div 8;
  end;

begin
  if IsWindowVisible(Ctrl.Handle) and not IsIconic(GetRootParent(Ctrl.Handle)) then begin
    InAnimationProcess := True;
    if Ctrl is TCustomForm then
      sp := TAccessProvider(TrySendMessage(Ctrl.Handle, SM_ALPHACMD, AC_GETPROVIDER_HI, 0))
    else
      sp := nil;

    bExtendedBorders := (sp <> nil) and (sp.BorderForm <> nil) and acLayered;

    if acLayered and (Ctrl is TCustomForm) then begin
      if sp.BorderForm <> nil then
        if sp.CoverBmp <> nil then begin
          OldAlphaBmp := sp.CoverBmp;
          sp.CoverBmp := nil;
        end
        else begin
          OldAlphaBmp := CreateBmp32;
          OldAlphaBmp.Assign(sp.SkinData.FCacheBmp);
        end;

      sp.PaintAll;
      if sp.SkinData.FCacheBmp = nil then begin
        InAnimationProcess := False;
        Exit;
      end;
      acDstBmp := CreateBmp32(sp.SkinData.FCacheBmp);
{$IFDEF ACDEBUG}
      acDebugAnimBmp := acDstBmp;
{$ENDIF}
      acDstBmp.Canvas.Lock;
      SkinPaintTo(acDstBmp, Ctrl, 0, 0, sp, True);

      if acDstBmp = nil then
        Exit;

      if sp.BorderForm = nil then
        FillAlphaRect(acDstBmp, MkRect(acDstBmp), MaxByte);

      acDstBmp.Canvas.UnLock;
      StepCount := wTime div acTimerInterval;
      Flags := SWP_NOACTIVATE or SWP_NOREDRAW or SWP_NOCOPYBITS or SWP_NOSIZE or SWP_NOMOVE; // or SWP_NOZORDER;

      FBmpSize := MkSize(acDstBmp);
      FBmpTopLeft := MkPoint;
      if StepCount > 0 then
        InitBlendData(FBlend, 0)
      else
        InitBlendData(FBlend, MaxTransparency);

      if sp.BorderForm <> nil then begin
        if sp.BorderForm.AForm = nil then
          sp.BorderForm.CreateNewForm;

        sp.BorderForm.AForm.WindowProc := sp.BorderForm.OldBorderProc;
        if sp.BorderForm.ParentHandle <> 0 then
          SetWindowLong(sp.BorderForm.AForm.Handle, GWL_HWNDPARENT, LONG_PTR(sp.BorderForm.ParentHandle)); // Patch for ReportBuilder and similar windows

        OldAlphaForm := sp.BorderForm.AForm;
        sp.BorderForm.CreateNewForm;

        if sp.FSysExHeight then
          cy := sp.ShadowSize.Top
        else
          with sp.SkinData.SkinManager.CommonSkinData do
            if IsZoomed(sp.Form.Handle) and (ExMaxHeight <> ExTitleHeight) then
              cy := SkinTitleHeight(sp.BorderForm) + sp.ShadowSize.Top - SysCaptHeight(sp.Form) - SysBorderWidth(sp.Form.Handle, sp.BorderForm, False) + ExTitleHeight - ExMaxHeight
            else
              cy := SkinTitleHeight(sp.BorderForm) + sp.ShadowSize.Top - SysCaptHeight(sp.Form) - SysBorderWidth(sp.Form.Handle, sp.BorderForm, False);

        cx := SkinBorderWidth(sp.BorderForm) - SysBorderWidth(sp.Form.Handle, sp.BorderForm, False) + sp.ShadowSize.Left;

        GetWindowRect(Ctrl.Handle, fR);

        sp.BorderForm.AForm.Left := fR.Left - cx;
        sp.BorderForm.AForm.Top := fr.Top - cy;
        sp.BorderForm.AForm.Width := FBmpSize.cx;
        sp.BorderForm.AForm.Height := FBmpSize.cy;

        h := GetNextWindow(sp.Form.Handle, GW_HWNDPREV);

        SetWindowPos(sp.BorderForm.AForm.Handle, h, fR.Left - cx, fr.Top - cy, FBmpSize.cx, FBmpSize.cy, Flags);

        DC := GetDC(0);
        UpdateLayeredWindow(sp.BorderForm.AForm.Handle, DC, nil, @FBmpSize, acDstBmp.Canvas.Handle, @FBmpTopLeft, clNone, @FBlend, ULW_ALPHA);
        ShowWindow(sp.BorderForm.AForm.Handle, SW_SHOWNOACTIVATE);

        AnimBmp := CreateBmp32(FBmpSize);
        FillDC(AnimBmp.Canvas.Handle, MkRect(AnimBmp), 0);

        if StepCount > 0 then begin
          Anim_Init;
          j := 0;
          while j <= StepCount do begin
            lTicks := GetTickCount;
            Anim_DoNext;
            UpdateLayeredWindow(sp.BorderForm.AForm.Handle, DC, nil, @FBmpSize, AnimBmp.Canvas.Handle, @FBmpTopLeft, clNone, @FBlend, ULW_ALPHA);
            SetFormBlendValue(OldAlphaForm.Handle, OldAlphaBmp, min(MaxByte, max(0, 3 * (MaxTransparency - FBlend.SourceConstantAlpha))));
            Anim_GoToNext;
            inc(j);
            if StepCount > 0 then
              WaitTicks(lTicks);
          end;
          FBlend.SourceConstantAlpha := MaxTransparency;
        end;

        ReleaseDC(0, DC);
        InAnimationProcess := False;
{$IFDEF DELPHI7UP}
        if sp.Form.AlphaBlend then
          sp.Form.AlphaBlendValue := MaxTransparency;
{$ENDIF}
        Ctrl.Perform(WM_SETREDRAW, 1, 0); // Vista

        RedrawWindow(Ctrl.Handle, nil, 0, RDWA_ALLNOW);
        SetWindowRgn(sp.BorderForm.AForm.Handle, sp.BorderForm.MakeRgn, False);
        SetWindowPos(sp.BorderForm.AForm.Handle, 0, fR.Left - cx, fr.Top - cy, FBmpSize.cx, FBmpSize.cy, Flags or SWP_NOZORDER);
        sp.BorderForm.UpdateExBordersPos(False);
        FreeAndNil(OldAlphaBmp);
        FreeAndNil(OldAlphaForm);
      end
      else begin
        GetWindowRect(Ctrl.Handle, fR);
        TacGlowForm(ow) := TacGlowForm.CreateNew(nil);

        ow.Left := Ctrl.Left;
        ow.Top := Ctrl.Top;
        ow.Width := Ctrl.Width;
        ow.Height := Ctrl.Height;
        if GetWindowLong(Ctrl.Handle, GWL_EXSTYLE) and WS_EX_TOPMOST <> 0 then begin
          TForm(ow).FormStyle := fsStayOnTop;
          h := HWND_TOPMOST
        end
        else
          if sp.BorderForm <> nil then
            h := sp.Form.Handle
          else
            h := GetWindow(sp.Form.Handle, GW_HWNDPREV);

        SetWindowPos(ow.Handle, h, Ctrl.Left, Ctrl.Top, FBmpSize.cx, FBmpSize.cy, Flags and not SWP_NOMOVE);
        DC := GetDC(0);
        SetWindowLong(ow.Handle, GWL_EXSTYLE, GetWindowLong(ow.Handle, GWL_EXSTYLE) or WS_EX_LAYERED or WS_EX_NOACTIVATE);
        UpdateLayeredWindow(ow.Handle, DC, nil, @FBmpSize, acDstBmp.Canvas.Handle, @FBmpTopLeft, clNone, @FBlend, ULW_ALPHA);

        FillArOR(sp);
        rgn := GetRgnFromArOR(sp);
        SetWindowRgn(ow.Handle, rgn, False);

        ShowWindow(ow.Handle, SW_SHOWNOACTIVATE);

        AnimBmp := CreateBmp32(FBmpSize);
        FillDC(AnimBmp.Canvas.Handle, MkRect(AnimBmp), 0);

        if StepCount > 0 then begin
          Anim_Init;
          j := 0;
          while j <= StepCount do begin
            lTicks := GetTickCount;
            Anim_DoNext;
            UpdateLayeredWindow(ow.Handle, DC, nil, @FBmpSize, AnimBmp.Canvas.Handle, @FBmpTopLeft, clNone, @FBlend, ULW_ALPHA);
            Anim_GoToNext;
            inc(j);
            if StepCount > 0 then
              WaitTicks(lTicks);
          end;
          FBlend.SourceConstantAlpha := MaxTransparency;
        end;
        ReleaseDC(0, DC);
        InAnimationProcess := False;
    {$IFDEF DELPHI7UP}
        if sp.Form.AlphaBlend then
          sp.Form.AlphaBlendValue := MaxTransparency;
    {$ENDIF}
        Ctrl.Perform(WM_SETREDRAW, 1, 0); // Vista
        RedrawWindow(Ctrl.Handle, nil, 0, RDWA_ALLNOW);
        FreeAndNil(ow);
      end;
      FreeAndNil(AnimBmp);
      FreeAndNil(acDstBmp);
    end
    else begin
      if ow = nil then // If not initialized by developer
        PrepareForAnimation(Ctrl);

      if ow = nil then // Init was failed
        Exit;

      SetChildOrderAfter(ow, Ctrl);
      acDstBmp := CreateBmp32(Ctrl.width, Ctrl.Height);
{$IFDEF ACDEBUG}
      acDebugAnimBmp := acDstBmp;
{$ENDIF}
      // Finish picture
      acDstBmp.Canvas.Lock;
      SavedDC := SaveDC(acDstBmp.Canvas.Handle);
      try
        SkinPaintTo(acDstBmp, Ctrl, 0, 0, nil, True);
        if acDstBmp = nil then
          Exit;
      finally
        RestoreDC(acDstBmp.Canvas.Handle, SavedDC);
        acDstBmp.Canvas.UnLock;
      end;
      NewBmp := CreateBmp32(Ctrl.width, Ctrl.Height);
      StepCount := wTime div (acTimerInterval * 2);
      if not bExtendedBorders and (sp <> nil) then begin
        sSkinProvider.FillArOR(TsSkinProvider(sp));
        if ow = nil then
          Exit;

        SetWindowRgn(ow.Handle, sSkinProvider.GetRgnFromArOR(TsSkinProvider(sp)), False);
      end;
      case AnimType of
        atcBlur2: begin
          BlurKef := 1;
          TmpBmp := CreateBmp32;
          BGBmp := nil;
          BlurBmp := nil;
        end;

        atcBlur: begin
          BlurKef := 1 + 0.0006 * (1000 - min(999, wTime));
          BGBmp := CreateBmpLike(acDstBmp);
          BlurBmp := CreateBmpLike(acDstBmp);
          TmpBmp := CreateBmp32;

          BGInfo.DrawDC := BGBmp.Canvas.Handle;
          BGInfo.R := MkRect(Ctrl);
          BGInfo.Offset := MkPoint;
          BGInfo.BgType := btUnknown;
          BGBmp.Canvas.Lock;
          GetBGInfo(@BGInfo, Ctrl.Handle, True);
          BGBmp.Canvas.Unlock;
          j := BGBmp.Width - 1;

          if InitLine(BGBmp, Pointer(S0), DeltaS) and InitLine(acDstBmp, Pointer(D0), DeltaD) then // Leave not equal pixels only
            for Y := 0 to BGBmp.Height - 1 do begin
              S := Pointer(PAnsiChar(S0) + DeltaS * Y);
              D := Pointer(PAnsiChar(D0) + DeltaD * Y);
              for X := 0 to j do
                with S[X], D[X] do
                  if (SR = DR) and (SG = DG) and (SB = DB) then
                    DA := 0
                  else
                    DA := MaxByte;
            end;

          BlurBmp.Assign(acDstBmp);
        end

        else begin
          BlurKef := 1;
          BGBmp := nil;
          BlurBmp := nil;
          TmpBmp := nil;
        end;
      end;

      DC := GetWindowDC(ow.Handle);
      if StepCount > 0 then begin
        prc := MaxByte / StepCount;
        Percent2 := MaxByte;
        Percent1 := MaxByte;
        j := 0;
        n := Length(acCtrlRects1);
        if AnimType = atcBlur then begin
          SetLength(acCtrlRects2, n);
          for k := 0 to n - 1 do
            acCtrlRects2[k] := acCtrlRects1[k];
        end;
//        b := False;
        while Percent1 > 2 do begin
          lTicks := GetTickCount;
          // Animation
          case AnimType of
            atcBlur2: begin
              TmpBmp.Width := MulDiv(acAnimBmp.Width, 3, 6);
              TmpBmp.Height := MulDiv(acAnimBmp.Height, 3, 6);
              acgpStretchRect(TmpBmp, acAnimBmp, MkRect(TmpBmp.Width - 1, TmpBmp.Height - 1), MkRect(acAnimBmp.Width - 1, acAnimBmp.Height - 1));
              acgpStretchRect(acAnimBmp, TmpBmp, MkRect(acAnimBmp.Width - 1, acAnimBmp.Height - 1), Rect(2, 2, TmpBmp.Width - 3, TmpBmp.Height - 3));
              Percent1 := max(0, Percent1 - prc);
              Percent2 := Percent2 * 3 / 4;
              SumBitmapsByMask(NewBmp, acAnimBmp, acDstBmp, nil, max(0, Round(Percent2)));
            end;

            atcBlur: begin
              // acAnimBmp - bitmap with controls (start)
              // acAnimBmp2 - bitmap with BG (start)
              // acDstBmp - bitmap with controls (finish)
              // BGBmp - bitmap with BG (finish)
              TmpBmp.Width := DoScale(acAnimBmp.Width);
              TmpBmp.Height := DoScale(acAnimBmp.Height);

              acgpStretchRect(TmpBmp, acAnimBmp, MkRect(TmpBmp.Width - 1, TmpBmp.Height - 1), MkRect(acAnimBmp.Width - 1, acAnimBmp.Height - 1));
              acAnimBmp.Assign(TmpBmp);
              for k := 0 to n - 1 do begin
                acCtrlRects1[k].Left   := DoScale(acCtrlRects1[k].Left   );
                acCtrlRects1[k].Top    := DoScale(acCtrlRects1[k].Top    );
                acCtrlRects1[k].Right  := DoScale(acCtrlRects1[k].Right  );
                acCtrlRects1[k].Bottom := DoScale(acCtrlRects1[k].Bottom );
              end;

              NewBmp.Assign(BGBmp);
              // Prepare image for each control
              if Percent1 > 1 then
                for k := 0 to n - 1 do begin
                  fR := acCtrlRects2[k];
                  cx := WidthOf(fR);
                  cy := HeightOf(fR);
                  InflateRect(fR, (j + 1) * cx shr 5, (j + 1) * cy shr 5);
                  TmpBmp.Width := WidthOf(fR, True);
                  TmpBmp.Height := HeightOf(fR, True);
                  acgpStretchRect(TmpBmp, acAnimBmp, MkRect(TmpBmp.Width - 1, TmpBmp.Height - 1), Rect(acCtrlRects1[k].Left, acCtrlRects1[k].Top, acCtrlRects1[k].Right - 1, acCtrlRects1[k].Bottom - 1));
                  // Copy Bmp to NewBmp
                  SumBmpRect32(NewBmp, fR, TmpBmp, MkPoint, Round(Percent1));
                end;

              SumBmpRect32(NewBmp, MkRect(BlurBmp), BlurBmp, MkPoint, MaxByte - Round(Percent2));
              // Blur End controls (prc - blend value)
              Percent1 := max(0, Percent1 / BlurKef);
              Percent2 := max(0, Percent2 - prc);
            end

            else begin
              SumBitmapsByMask(NewBmp, acAnimBmp, acDstBmp, nil, max(0, Round(Percent1)));
              Percent1 := max(0, Percent1 - prc);
            end;
          end;
          BitBlt(DC, 0, 0, Ctrl.Width, Ctrl.Height, NewBmp.Canvas.Handle, 0, 0, SRCCOPY);
          if Assigned(acMagnForm) then
            SendMessage(acMagnForm.Handle, SM_ALPHACMD, AC_REFRESH_HI, 0);

          inc(j);
          WaitTicks(lTicks, acTimerInterval * 2);
        end;
      end;
      BitBlt(DC, 0, 0, Ctrl.width, Ctrl.Height, acDstBmp.Canvas.Handle, 0, 0, SRCCOPY);
      if Assigned(acMagnForm) then
        SendMessage(acMagnForm.Handle, SM_ALPHACMD, AC_REFRESH_HI, 0);

      InAnimationProcess := False;
      if Ctrl.Visible then begin
        FillAlphaRect(acDstBmp, MkRect(acDstBmp), MaxByte);
        Ctrl.Perform(WM_SETREDRAW, 1, 0); // Vista
        SetFormBlendValue(ow.Handle, acDstBmp, MaxByte - 1);
        if Win32MajorVersion >= 6 then
          RedrawWindow(Ctrl.Handle, nil, 0, RDWA_ALLNOW);

        if not (Ctrl is TCustomForm) or (acNativeUInt(GetWindowLong(Ctrl.Handle, GWL_EXSTYLE)) and WS_EX_LAYERED = 0) then
          SetWindowPos(ow.Handle, 0, 0, 0, 0, 0, SWP_NOZORDER or SWP_HIDEWINDOW or SWP_NOREDRAW or SWP_NOACTIVATE);
      end;
      ReleaseDC(ow.Handle, DC);
      FreeAndnil(ow);
      FreeAndNil(NewBmp);
      FreeAndNil(acAnimBmp);
      FreeAndNil(acDstBmp);
      if Assigned(acAnimBmp2) then
        FreeAndNil(acAnimBmp2);

      if Assigned(BGBmp) then
        FreeAndNil(BGBmp);

      if Assigned(TmpBmp) then
        FreeAndNil(TmpBmp);

      if Assigned(BlurBmp) then
        FreeAndNil(BlurBmp);
    end;
  end;
end;


procedure SetParentUpdated(const wc: TWinControl); overload;
var
  i: integer;
begin
  if not (csDesigning in wc.ComponentState) and not InAnimationProcess then
    for i := 0 to wc.ControlCount - 1 do
      if not (csDestroying in wc.Controls[i].ComponentState) then
        if wc.Controls[i] is TWinControl then begin
          if TWinControl(wc.Controls[i]).HandleAllocated and TWinControl(wc.Controls[i]).Showing then
            TrySendMessage(TWinControl(wc.Controls[i]).Handle, SM_ALPHACMD, AC_ENDPARENTUPDATE_HI, 0);
        end
        else
          if wc.Controls[i] is TControl then
            TControl(wc.Controls[i]).Perform(SM_ALPHACMD, AC_ENDPARENTUPDATE_HI, 0);
end;


procedure SetParentUpdated(const pHwnd: hwnd); overload;
var
  hCtrl: THandle;
begin
  hCtrl := GetTopWindow(pHwnd);
  while hCtrl <> 0 do begin
    if GetWindowLong(hCtrl, GWL_STYLE) and WS_CHILD <> 0 then
      SendAMessage(hCtrl, AC_ENDPARENTUPDATE);

    hCtrl := GetNextWindow(hCtrl, GW_HWNDNEXT);
  end;
end;


procedure ChangeControlColors(AControl: TControl; AFontColor, AColor: TColor); // clNone will reset a color to default
var
  SkinData: TsCommonData;
begin
  SkinData := TsCommonData(AControl.Perform(SM_ALPHACMD, AC_GETSKINDATA_HI, 0));
  if SkinData <> nil then begin
    SkinData.CustomFont := AFontColor <> clNone;
    SkinData.CustomColor := AColor <> clNone;
  end;
  if AFontColor <> clNone then
    TacAccessControl(AControl).Font.Color := acColorToRGB(AFontColor);

  if AColor <> clNone then begin
    AControl.Perform(SM_ALPHACMD, AC_SETBGCHANGED_HI + 1, 0);
    TacAccessControl(AControl).Color := acColorToRGB(AColor);
  end;
end;


function GetControlColor(const Control: TControl): TColor;
begin
  Result := clFuchsia;
  if Control <> nil then
    if CtrlIsSkinned(Control) then
      Result := ColorToRGB(Control.Perform(SM_ALPHACMD, AC_GETCONTROLCOLOR_HI, Result))
    else
      Result := ColorToRGB(TacAccessControl(Control).Color); // message is not supported by parent control
end;


function GetControlColor(const Handle: THandle): TColor; overload;
begin
  Result := clFuchsia;
  if Handle <> 0 then
    Result := ColorToRGB(TrySendMessage(Handle, SM_ALPHACMD, AC_GETCONTROLCOLOR_HI, Result));
end;


function GetControlFontColor(const Control: TControl; SkinManager: TObject): TColor;
var
  SkinIndex: integer;
  SM: TsSkinManager;
begin
  if SkinManager = nil then
    SM := DefaultManager
  else
    SM := TsSkinManager(SkinManager);

  SkinIndex := GetFontIndex(Control, -1, SM);
  if SkinIndex >= 0 then
    Result := SM.gd[SkinIndex].Props[0].FontColor.Color
  else
    if SM.CommonSkinData.Active then
      Result := SM.Palette[pcLabelText]
    else
      Result := 0;
end;


function IsCustomFont(Ctrl: TControl; AFont: TFont; SaveColor: boolean = True): boolean;
var
  f: TFont;
begin
  if (Ctrl <> nil) and (TAccessControl(Ctrl).Parent <> nil) then
    if not SaveColor then begin
      F := TFont.Create;
      F.Assign(TAccessControl(Ctrl.Parent).Font);
      F.Color := TAccessControl(Ctrl).Font.Color;
      Result := not FontsEqual(F, AFont);
      F.Free;
    end
    else
      Result := not FontsEqual(TAccessControl(Ctrl.Parent).Font, AFont)
  else begin
    F := TFont.Create;
    if not SaveColor then
      F.Color := AFont.Color;

    Result := not FontsEqual(F, AFont);
    F.Free;
  end;
end;


function AllEditSelected(Ctrl: TCustomEdit): Boolean;
type
  TSelection = record
    StartPos,
    EndPos: Integer;
  end;

var
  Selection: TSelection;
begin
  SendMessage(Ctrl.Handle, EM_GETSEL, WPARAM(@Selection.StartPos), LPARAM(@Selection.EndPos));
  Result := (Selection.EndPos = Ctrl.GetTextLen) and (Ctrl.SelLength = Length(Ctrl.Text));
end;


type
  TAccessGraphicCtrl = class(TGraphicControl);


procedure PaintControls(DC: HDC; OwnerControl: TWinControl; ChangeCache: boolean; Offset: TPoint; AHandle: THandle = 0; CheckVisible: boolean = True);
var
  SaveIndex: hdc;
  R, bRect: TRect;
  tDC, MemDC: HDC;
  BGInfo: TacBGInfo;
  MemDCExists: boolean;
  I, J, d, Count: Integer;
  MemBitmap, OldBitmap: HBITMAP;

  function ControlIsReady(Control: TControl): boolean;
  begin
    with Control do begin
      Result := (Control is TGraphicControl) and (Visible or (csDesigning in ComponentState)) and (Width > 0) and (Height > 0);

      Result := Result and not (csNoDesignVisible in ControlStyle) and not (csDestroying in ComponentState) and
                not ((Control is TToolButton) and (TToolButton(Control).Style in [tbsCheck, tbsButton, tbsDropDown]));

      Result := Result and RectVisible(DC, BoundsRect);
    end;
  end;

begin
  if not acGraphPainting then begin
    acGraphPainting := True;
    MemDCExists := False;
    MemDC := 0;
    MemBitmap := 0;
    OldBitmap := 0;
    if (OwnerControl.Visible or (csDesigning in OwnerControl.ComponentState) or not CheckVisible) and (OwnerControl.ControlCount > 0) then
      try
        if (GetClipBox(DC, R) = NULLREGION) or (R.Left = R.Right) or (R.Top = R.Bottom) then begin
          SendAMessage(OwnerControl.Handle, AC_SETHALFVISIBLE);
          acGraphPainting := False;
          Exit;
        end;
        BGInfo.BgType := btUnknown;
        BGInfo.PleaseDraw := False;
        GetBGInfo(@BGInfo, OwnerControl);
        Count := OwnerControl.ControlCount;
        I := 0;
        while I < Count do begin
          if ControlIsReady(OwnerControl.Controls[I]) then begin
            if OwnerControl is TForm then
              with TForm(OwnerControl) do
                if (FormStyle = fsMDIForm) and (Controls[I].Align <> alNone) and (Controls[I] is TGraphicControl) then
                  Controls[I].Perform(SM_ALPHACMD, AC_INVALIDATE shl 16, 0);

            if not MemDCExists then begin // Make BG image
              tDC := GetDC(0);
              MemBitmap := CreateCompatibleBitmap(tDC, OwnerControl.Width, OwnerControl.Height);
              ReleaseDC(0, tDC);
              MemDC := CreateCompatibleDC(0);
              OldBitmap := SelectObject(MemDC, MemBitmap);
              MemDCExists := True;
              for j := 0 to Count - 1 do // Copy parent BG
                with OwnerControl, Controls[J] do
                  if ControlIsReady(Controls[J]) then
                    if not (csOpaque in ControlStyle) then
                      if (BGInfo.BgType = btCache) and (BGInfo.Bmp <> nil) then // If cache exists
                        BitBlt(MemDC, Left, Top, Width, Height, BGInfo.Bmp.Canvas.Handle, Left + BGInfo.Offset.X, Top + BGInfo.Offset.Y, SRCCOPY)
                      else begin
                        if BGInfo.Bmp <> nil then begin
                          bRect := BoundsRect;
                          FillDC(MemDC, bRect, BGInfo.Color);

                          if bRect.Top < BGInfo.FillRect.Top then
                            BitBlt(MemDC, bRect.Left, bRect.Top, Width, BGInfo.FillRect.Top - bRect.Top, BGInfo.Bmp.Canvas.Handle, bRect.Left + BGInfo.Offset.X, bRect.Top + BGInfo.Offset.Y, SRCCOPY);

                          if bRect.Left < BGInfo.FillRect.Left then
                            BitBlt(MemDC, bRect.Left, bRect.Top, BGInfo.FillRect.Left - bRect.Left, Height, BGInfo.Bmp.Canvas.Handle, bRect.Left + BGInfo.Offset.X, bRect.Top + BGInfo.Offset.Y, SRCCOPY);

                          if (bRect.Bottom + BGInfo.Offset.Y > (BGInfo.Bmp.Height - BGInfo.FillRect.Bottom)) and (BGInfo.Offset.Y >= 0 {Not scrolled}) then begin
                            d := bRect.Bottom - (BGInfo.Bmp.Height - BGInfo.FillRect.Bottom);
                            BitBlt(MemDC, bRect.Left, bRect.Bottom - d, Width, d, BGInfo.Bmp.Canvas.Handle, bRect.Left + BGInfo.Offset.X, bRect.Bottom + BGInfo.Offset.Y - d, SRCCOPY);
                          end;

                          if (bRect.Right + BGInfo.Offset.X > (BGInfo.Bmp.Width - BGInfo.FillRect.Right)) and (BGInfo.Offset.X >= 0 {Not scrolled}) then begin
                            d := bRect.Right - (BGInfo.Bmp.Width - BGInfo.FillRect.Right);
                            BitBlt(MemDC, bRect.Right - d, bRect.Top, d, Height, BGInfo.Bmp.Canvas.Handle, bRect.Right + BGInfo.Offset.X - d, bRect.Top + BGInfo.Offset.Y, SRCCOPY);
                          end;
                        end
                        else
                          FillDC(MemDC, BoundsRect, BGInfo.Color);
                      end
                    else
                      FillDC(MemDC, BoundsRect, GetControlColor(Controls[J]));
            end;
            SaveIndex := SaveDC(MemDC);
            if not RectVisible(DC, OwnerControl.Controls[I].BoundsRect) then
              SendAMessage(OwnerControl.Controls[I], AC_SETHALFVISIBLE);

            MoveWindowOrg(MemDC, OwnerControl.Controls[I].Left, OwnerControl.Controls[I].top);
            IntersectClipRect(MemDC, 0, 0, OwnerControl.Controls[I].Width, OwnerControl.Controls[I].Height);

            if csPaintCopy in OwnerControl.ControlState then
              OwnerControl.Controls[I].ControlState := OwnerControl.Controls[I].ControlState + [csPaintCopy];

            try // Errors in dialogs are possible
              if (csDesigning in OwnerControl.ComponentState) and
{$IFNDEF FIXGRAPHERROR}
                (OwnerControl.Controls[I] is TSpeedButton) then
{$ELSE}
                (OwnerControl.Controls[I] is TGraphicControl) and not (OwnerControl.Controls[I] is TImage) then
{$ENDIF}
                  with TAccessGraphicCtrl(OwnerControl.Controls[I]) do begin
                    Canvas.Lock;
                    Canvas.Handle := MemDC;
                    try
                      Paint;
                    finally
                      Canvas.Handle := 0;
                      Canvas.Unlock;
                    end;
                  end
              else
                OwnerControl.Controls[I].Perform(WM_PAINT, WPARAM(MemDC), 0);
            finally
              if csPaintCopy in OwnerControl.ControlState then
                OwnerControl.Controls[I].ControlState := OwnerControl.Controls[I].ControlState - [csPaintCopy];

              MoveWindowOrg(MemDC, - OwnerControl.Controls[I].Left, - OwnerControl.Controls[I].Top);
              RestoreDC(MemDC, SaveIndex);
            end;
          end;
          inc(i);
        end;
        if MemDCExists then begin
          J := 0;
          while J < Count do begin // Copy graphic controls
            if ControlIsReady(OwnerControl.Controls[J]) then
              with OwnerControl.Controls[J] do
                if GetPixel(MemDC, Left + Offset.X, Top + Offset.Y) <> DWord(clFuchsia) then
                  BitBlt(DC, Left + Offset.X, Top + Offset.Y, Width, Height, MemDC, Left, Top, SRCCOPY);

            inc(J);
          end;
        end;
      finally
        if MemDCExists then begin
          SelectObject(MemDC, OldBitmap);
          DeleteDC(MemDC);
          DeleteObject(MemBitmap);
        end;
      end;

    acGraphPainting := False;
  end;
end;


procedure PaintParentBG(AControl: TControl; ABitmap: TBitmap);
var
  BGInfo: TacBGInfo;
begin
  if (AControl <> nil) and (ABitmap <> nil) and (AControl.Parent <> nil) then begin
    BGInfo.PleaseDraw := False;
    GetBGInfo(@BGInfo, AControl.Parent.Handle);
    ABitmap.PixelFormat := pf32bit;
    ABitmap.Width  := AControl.Width;
    ABitmap.Height := AControl.Height;
    case BGInfo.BgType of
      btCache:
        BitBlt(ABitmap.Canvas.Handle, 0, 0, ABitmap.Width, ABitmap.Height, BGInfo.Bmp.Canvas.Handle, BGInfo.Offset.X + AControl.Left, BGInfo.Offset.Y + AControl.Top, SRCCOPY);

      btFill:
        FillDC(ABitmap.Canvas.Handle, MkRect(ABitmap), BGInfo.Color)

      else
        FillDC(ABitmap.Canvas.Handle, MkRect(ABitmap), GetControlColor(AControl));
    end;
  end;
end;


procedure SetRedraw(Handle: THandle; Value: integer = 0); overload;
begin
  TrySendMessage(Handle, WM_SETREDRAW, Value, 0);
end;


procedure SetRedraw(Ctrl: TGraphicControl; Value: integer = 0); overload;
begin
  Ctrl.Perform(WM_SETREDRAW, Value, 0);
end;


function SendAMessage(const Handle: hwnd; const Cmd: Integer; LParam: LPARAM = 0): LRESULT; overload;
begin
  Result := TrySendMessage(Handle, SM_ALPHACMD, WPARAM(Word(Cmd)) shl 16, LParam);
end;


function SendAMessage(const Control: TControl; const Cmd: Integer; LParam: LPARAM = 0): LRESULT; overload;
begin
  Result := TrySendMessage(Control, SM_ALPHACMD, WPARAM(Word(Cmd)) shl 16, LParam);
end;


function TrySendMessage(const Control: TControl; Message: TMessage): LRESULT; overload;
var
  sd: TsCommonData;
begin
  if Control is TWinControl then
    with TWinControl(Control) do
      if not (csDestroying in ComponentState) and HandleAllocated then begin
        sd := GetCommonData(Handle);
        if (sd <> nil) and Assigned(sd.WndProc) then begin
          sd.WndProc(Message);
          Result := Message.Result;
        end
        else
          Result := SendMessage(Handle, Message.Msg, Message.WParam, Message.LParam);
      end
      else
        Result := 0
  else
    Result := Control.Perform(Message.Msg, Message.WParam, Message.LParam);
end;


function TrySendMessage(const Control: TControl; Msg: Cardinal; WParam: WPARAM; LParam: LPARAM = 0): LRESULT;
var
  M: TMessage;
  sd: TsCommonData;
begin
  if Control is TWinControl then
    with TWinControl(Control) do
      if not (csDestroying in ComponentState) and HandleAllocated then begin
        sd := GetCommonData(Handle);
        if (sd <> nil) and Assigned(sd.WndProc) then begin
          M := MakeMessage(Msg, WPAram, LParam, 0);
          sd.WndProc(M);
          Result := M.Result;
        end
        else
          Result := SendMessage(Handle, Msg, WParam, LParam);
      end
      else
        Result := 0
  else
    Result := Control.Perform(Msg, WParam, LParam);
end;


function TrySendMessage(AHandle: THandle; Msg: Cardinal; WParam: WPARAM; LParam: LPARAM = 0): LRESULT; overload;
var
  M: TMessage;
  sd: TsCommonData;
begin
  sd := GetCommonData(AHandle);
  if (sd <> nil) and Assigned(sd.WndProc) then begin
    M := MakeMessage(Msg, WPAram, LParam, 0);
    sd.WndProc(M);
    Result := M.Result;
  end
  else
    Result := SendMessage(AHandle, Msg, WParam, LParam);
end;


function GetBoolMsg(const Control: TWinControl; const Cmd: Cardinal): boolean;
begin
  Result := boolean(SendAMessage(Control, Cmd));
end;


function GetBoolMsg(const CtrlHandle: hwnd; const Cmd: Cardinal): boolean; overload;
var
  LParam: cardinal;
begin
  LParam := 0;
  if SendAMessage(CtrlHandle, WPARAM(Cmd), LParam) = 1 then
    Result := True
  else
    Result := LParam = 1;
end;


function ControlIsReady(const Control: TControl): boolean;
begin
  if (Control <> nil) and (Control.Parent <> nil) and (not (Control is TWinControl) or TWinControl(Control).HandleAllocated) then
    with Control do
      Result := ([csLoading, csDestroying] * ComponentState = []) and ([csCreating, csReadingState] * ControlState = [])
  else
    Result := False;
end;


function GetOwnerForm(const Component: TComponent): TCustomForm;
var
  c: TComponent;
begin
  Result := nil;
  c := Component;
  while Assigned(c) and not (c is TCustomForm) do
    c := c.Owner;

  if c is TCustomForm then
    Result := TCustomForm(c);
end;


function GetOwnerFrame(const Component: TComponent): TCustomFrame;
var
  c: TComponent;
begin
  Result := nil;
  c := Component;
  while Assigned(c) and not (c is TCustomFrame) do
    c := c.Owner;

  if c is TCustomFrame then
    Result := TCustomFrame(c);
end;


procedure SetControlsEnabled(Parent: TWinControl; Value: boolean; Recursion: boolean = False);
var
   i: integer;
begin
  for i := 0 to Parent.ControlCount - 1 do
    if not (Parent.Controls[i] is TCustomPanel) then begin
      Parent.Controls[i].Enabled := Value;
      if Recursion and (Parent.Controls[i] is TWinControl) then
        SetControlsEnabled(TWinControl(Parent.Controls[i]), Value, True);
    end;
end;


function GetStringFlags(const Control: TControl; const al: TAlignment): Cardinal;
begin
{$IFDEF FPC}
  Result := DT_EXPANDTABS or DT_VCENTER or AlignToInt[al];
{$ELSE}
  Result := Control.DrawTextBiDiModeFlags(DT_EXPANDTABS or DT_VCENTER or AlignToInt[al]);
{$ENDIF}
end;


procedure RepaintsControls(const Owner: TWinControl; const BGChanged: boolean);
var
  i: Integer;
begin
  i := 0;
  while i <= Owner.ControlCount - 1 do begin
    if ControlIsReady(Owner.Controls[i]) then
      if not (Owner.Controls[i] is TGraphicControl) then
        Owner.Controls[i].Invalidate;

    inc(i);
  end;
end;


procedure AlphaBroadCast(const Control: TWinControl; var Message);
var
  i: integer;
begin
  i := 0;
  while i < Control.ControlCount do
    with TMessage(Message) do begin
      if (i >= Control.ControlCount) or (csDestroying in Control.Controls[i].ComponentState) then
        Exit;

      if Control.Controls[i] is TWincontrol then
        with TWinControl(Control.Controls[i]) do
          if not HandleAllocated then
            Perform(Msg, Wparam, LParam)
          else begin
            TrySendMessage(Handle, Msg, WParam, LParam);
            if not WndIsSkinned(TWinControl(Control.Controls[i]).Handle) then
              if not Assigned(CheckDevEx) or not CheckDevEx(Control.Controls[i]) then
                AlphaBroadCast(TWinControl(Control.Controls[i]), Message);
          end
      else
        Control.Controls[i].Perform(Msg, Wparam, LParam);

      inc(i);
    end;
end;


type
  TacMsgInfo = record
    Sender: hwnd;
    Message: TMessage;
  end;

  PacMsgInfo = ^TacMsgInfo;


function SendToChildren(Child: HWND; Data: LParam): BOOL; stdcall;
var
  MsgI: TacMsgInfo;
begin
  MsgI := PacMsgInfo(Data)^;
  if GetParent(Child) = MsgI.Sender then
    TrySendMessage(Child, MsgI.Message.Msg, MsgI.Message.WParam, MsgI.Message.LParam);

  Result := True;
end;


procedure AlphaBroadCast(const Handle: hwnd; var Message); overload;
var
  MsgI: TacMsgInfo;
begin
  MsgI.Sender := Handle;
  MsgI.Message := TMessage(Message);
  EnumChildWindows(Handle, @SendToChildren, LPARAM(@MsgI));
end;


function acClientRect(const Handle: hwnd): TRect;
var
  R: TRect;
  P: TPoint;
begin
  GetWindowRect(Handle, R);
  P := MkPoint;
  ClientToScreen(Handle, P);
  GetClientRect(Handle, Result);
  OffsetRect(Result, P.X - R.Left, P.Y - R.Top);
end;


function acMouseInWnd(const Handle: hwnd; X, Y: integer): TPoint;
var
  R: TRect;
begin
  GetWindowRect(Handle, R);
  Result := Point(X, Y);
  dec(Result.X, R.Left);
  dec(Result.Y, R.Top);
end;


function TrySetSkinSection(const Control: TControl; const SectionName: string): boolean;
var
  si: TacSectionInfo;
  Ndx: integer;
begin
  Result := False;
  if Control <> nil then
    if (Control is TLabel) and (Control.Tag and ExceptTag = 0) and (DefaultManager <> nil) and DefaultManager.Active then
      with DefaultManager do begin
        Ndx := GetSkinIndex(SectionName);
        if Ndx >= 0 then
          TLabel(Control).Font.Color := DefaultManager.gd[Ndx].Props[0].FontColor.Color
        else
          TLabel(Control).Font.Color := DefaultManager.Palette[pcLabelText];

        Result := True;
      end
    else begin
      si.siName := SectionName;
      Result := Control.Perform(SM_ALPHACMD, AC_SETSECTION shl 16, LPARAM(@si)) = 1;
    end;
end;


function GetWndClassName(const Hwnd: THandle): string;
var
  Buf: array [0..128] of char;
begin
  GetClassName(Hwnd, Buf, 128);
  Result := StrPas(Buf);
end;


constructor TOutputWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Caption := 'acOutputWindow';
  Visible := False;
  Color := clFuchsia;
  Tag := ExceptTag;
end;


procedure TOutputWindow.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
    if (Parent = nil) and (ParentWindow = 0) then begin
      Params.Style := WS_POPUP;
      if (Owner is TWinControl) and (ACNativeUInt(GetWindowLong(TWinControl(Owner).Handle, GWL_EXSTYLE)) and WS_EX_TOPMOST <> 0) then
        Params.ExStyle := ExStyle or WS_EX_TOPMOST;

{$IFDEF FPC}
      WndParent := Application.MainFormHandle;
{$ELSE}
      WndParent := Application.Handle;
{$ENDIF}
    end;
end;


procedure TOutputWindow.WMEraseBkgnd(var Message: TWmEraseBkgnd);
begin
  Message.Result := 1;
end;


procedure TOutputWindow.WMNCPaint(var Message: TWmEraseBkgnd);
begin
  Message.Result := 0;
end;


function GetAlignShift(const Ctrl: TWinControl; const Align: TAlign; const GraphCtrlsToo: boolean = False): integer;
var
  i: integer;
begin
  Result := 0;
  with Ctrl do
    for i := 0 to ControlCount - 1 do
      if Controls[i].Visible and (Controls[i].Align = Align) and (GraphCtrlsToo or not (Controls[i] is TGraphicControl)) then
        case Align of
          alLeft, alRight: inc(Result, Controls[i].Width);
          alTop, alBottom: inc(Result, Controls[i].Height);
        end;
end;


function GetParentFormHandle(const CtrlHandle: hwnd): hwnd;
var
  ph: hwnd;
begin
  ph := GetParent(CtrlHandle);
  if ph = 0 then
    Result := CtrlHandle
  else
    Result := GetParentFormHandle(ph);
end;


procedure TacHideTimer.Anim_DoNext;
begin
  Trans := max(Trans - p, 0);
  FBlend.SourceConstantAlpha := Round(trans);
  case AnimType of
    atDropDown:
      if SrcBmp.Height = b then
        BitBlt(DstBmp.Canvas.Handle, 0, 0, DstBmp.Width, DstBmp.Height, SrcBmp.Canvas.Handle, 0, 0, SRCCOPY)
      else begin
        FillRect32(DstBmp, MkRect(DstBmp), 0, 0);
        acgpStretchRect(DstBmp, SrcBmp, Rect(Round(l), Round(t), Round(r - l), Round(b - t)), MkRecT(SrcBmp));
      end;

    atAero:
      if (l < 0) or (t < 0) then
        BitBlt(DstBmp.Canvas.Handle, 0, 0, SrcBmp.Width, SrcBmp.Height, SrcBmp.Canvas.Handle, 0, 0, SRCCOPY)
      else begin
        FillDC(DstBmp.Canvas.Handle, MkRect(DstBmp), 0);
        SetStretchBltMode(DstBmp.Canvas.Handle, COLORONCOLOR);
        StretchBlt(DstBmp.Canvas.Handle, Round(l), Round(t), Round(r - l), Round(b - t), SrcBmp.Canvas.Handle, 0, 0, SrcBmp.Width, SrcBmp.Height, SRCCOPY);
      end

    else
      if l = 0 then begin
        BitBlt(DstBmp.Canvas.Handle, 0, 0, SrcBmp.Width, SrcBmp.Height, SrcBmp.Canvas.Handle, 0, 0, SRCCOPY);
        l := 1;
      end;
  end
end;


function TacHideTimer.Anim_GoToNext: boolean;
begin
  Result := True;
  case AnimType of
    atDropDown: begin
      b := b / 2;
      if b < 2 then
        Result := False;
    end;

    atAero: begin
      l := l - dx;
      t := t - dy;
      r := r + dx;
      b := b + dy;
    end
  end
end;


procedure TacHideTimer.Anim_Init;
begin
  Trans := StartBlendValue;
  p := StartBlendValue / (StepCount);
  case AnimType of
    atDropDown: begin
      l := 0;
      t := 0;
      r := SrcBmp.Width;
      b := SrcBmp.Height;
      dx := 0;
      dy := SrcBmp.Height / acwPopupDiv;
      p := 0;
    end;

    atAero: begin
      dx := -SrcBmp.Width / (StepCount * acwDivider);
      dy := -SrcBmp.Height / (StepCount * acwDivider);
      l := 0;
      t := 0;
      r := SrcBmp.Width;
      b := SrcBmp.Height;
    end

    else begin
      dx := 0;
      dy := 0;
      l := 0;
      t := 0;
      r := 0;
      b := 0;
    end;
  end
end;


procedure TacHideTimer.CallEvent;
var
  sp: TsSkinProvider;
begin
  if not EventCalled and (ParentWnd <> 0) then begin
    sp := TsSkinProvider(SendAMessage(ParentWnd, AC_GETPROVIDER));
    if (sp <> nil) and Assigned(sp.OnAfterAnimation) then begin
      EventCalled := True;
      sp.OnAfterAnimation(aeHiding);
    end;
  end;
end;


constructor TacHideTimer.Create(AOwner: TComponent);
begin
  ParentWnd := 0;
  EventCalled := False;
  inherited;
end;


destructor TacHideTimer.Destroy;
begin
  inherited;
  CallEvent;
  FreeAndNil(SrcBmp);
  FreeAndNil(DstBmp);
  FreeAndNil(Form);
end;


procedure TacHideTimer.OnTimerProc(Sender: TObject);
var
  ToContinue: boolean;
begin
  if Form <> nil then begin
    if i <= StepCount then begin
      Anim_DoNext;
      DC := GetDC(0);
      if (Form <> nil) {and (DstBmp <> nil)} and Form.HandleAllocated then
        UpdateLayeredWindow(Form.Handle, DC, nil, @FBmpSize, DstBmp.Canvas.Handle, @FBmpTopLeft, clNone, @FBlend, ULW_ALPHA);

      ReleaseDC(0, DC);
      ToContinue := Anim_GoToNext;
      inc(i);
    end
    else
      ToContinue := False;

    if not ToContinue then begin
      i := StepCount + 1;
      Enabled := False;
      CallEvent;
      OnTimer := nil;
      Interval := MaxWord;
      FreeAndNil(Form);
      FreeAndNil(SrcBmp);
      FreeAndNil(DstBmp);
    end;
  end;
end;


procedure SetFormBlendValue(FormHandle: THandle; Bmp: TBitmap; Value: integer; NewPos: PPoint = nil);
var
  DC: hdc;
  R: TRect;
  b: boolean;
  FBmpSize: TSize;
  FBmpTopLeft: TPoint;
  FBlend: TBlendFunction;
begin
  if not GetWindowRect(FormHandle, R) then
    CheckLastError;

  if Bmp = nil then begin
    Bmp := CreateBmp32(R);
    b := True;
  end
  else
    b := False;

  InitBlendData(FBlend, Value);
  FBmpSize := MkSize(Bmp);
  FBmpTopLeft := MkPoint;
  DC := GetDC(0);
  try
    if GetWindowLong(FormHandle, GWL_EXSTYLE) and WS_EX_LAYERED = 0 then begin
      if not AeroIsEnabled then
        if SetWindowLong(FormHandle, GWL_STYLE, GetWindowLong(FormHandle, GWL_STYLE) and not WS_VISIBLE) = 0 then
          CheckLastError;

      if SetWindowLong(FormHandle, GWL_EXSTYLE, GetWindowLong(FormHandle, GWL_EXSTYLE) or WS_EX_LAYERED) = 0 then
        CheckLastError;

      if not AeroIsEnabled then begin
        RedrawWindow(0, @R, 0, RDW_INVALIDATE or RDW_UPDATENOW or RDW_ALLCHILDREN);
        if SetWindowLong(FormHandle, GWL_STYLE, GetWindowLong(FormHandle, GWL_STYLE) or WS_VISIBLE) = 0 then
          CheckLastError;
      end;
    end;
    SetWindowLong(FormHandle, GWL_EXSTYLE, GetWindowLong(FormHandle, GWL_EXSTYLE) or WS_EX_LAYERED);
    if not UpdateLayeredWindow(FormHandle, DC, NewPos, @FBmpSize, Bmp.Canvas.Handle, @FBmpTopLeft, clNone, @FBlend, ULW_ALPHA) then
      CheckLastError;
  finally
    ReleaseDC(0, DC);
  end;
  if b and (Bmp <> nil) then
    FreeAndNil(Bmp);
end;


function GetShiftState: TShiftState;
begin
  Result := [];
  if GetAsyncKeyState(VK_SHIFT) < 0 then
    Include(Result, ssShift);

  if GetAsyncKeyState(VK_CONTROL) < 0 then
    Include(Result, ssCtrl);

  if GetAsyncKeyState(VK_MENU) < 0 then
    Include(Result, ssAlt);
end;


procedure CheckLastError;
{$IFDEF ACDEBUG}
var
  lError: Cardinal;
{$ENDIF}
begin
{$IFDEF ACDEBUG}
  lError := GetLastError;
  if not (lError in [0]) then
//    lError := 0;
    ShowError(SysErrorMessage(lError));
{$ENDIF}
end;


procedure ReflectControls(ParentWnd: TWinControl; Recursion: boolean);
var
  i: integer;
  LeftCtrl: TControl;

  procedure ReflectAnchors(Ctrl: TControl);
  begin
    if (akLeft in Ctrl.Anchors) and not (akRight in Ctrl.Anchors) then
      Ctrl.Anchors := Ctrl.Anchors + [akRight] - [akLeft]
    else
      if (akRight in Ctrl.Anchors) and not (akLeft in Ctrl.Anchors) then
        Ctrl.Anchors := Ctrl.Anchors + [akLeft] - [akRight];
  end;

  function LeftMostIndex(Ctrl: TWinControl): integer;
  var
    i, l: integer;
  begin
    Result := -1;
    with ParentWnd do begin
      l := Width + 1;
      for i := 0 to ControlCount - 1 do
        if (Controls[i].Align = alLeft) and (Controls[i].Left < l) and ((LeftCtrl = nil) or (Controls[i].Left < LeftCtrl.Left)) then begin
          l := Controls[i].Left;
          Result := i;
        end;
    end;
  end;

  function RightMostIndex(Ctrl: TWinControl): integer;
  var
    i, l: integer;
  begin
    Result := -1;
    l := -1;
    with ParentWnd do
      for i := 0 to ControlCount - 1 do
        if (Controls[i].Align = alRight) and (Controls[i].Left > l) then begin
          l := Controls[i].Left;
          Result := i;
        end;
  end;

begin
  if not (ParentWnd is TToolBar {Not supported}) then begin
{$IFNDEF ALITE}
    if ParentWnd is TsStatusBar then
      TsStatusBar(ParentWnd).ReflectPanels;
{$ENDIF}
    with ParentWnd do begin
      HandleNeeded;
      LeftCtrl := nil;
      for i := 0 to ControlCount - 1 do begin
        if Controls[i].Align = alNone then begin
          Controls[i].Left := ClientWidth - Controls[i].Left - Controls[i].Width;
          ReflectAnchors(Controls[i]);
        end;
        Controls[i].Perform(SM_ALPHACMD, MakeWParam(0, AC_REFLECTLAYOUT), 0);
      end;
      // Change right aligning to left
      while True do begin
        i := RightMostIndex(ParentWnd);
        if i >= 0 then begin
          Controls[i].Align := alLeft;
          if LeftCtrl = nil then
            LeftCtrl := Controls[i]; // Remember first changed control
        end
        else
          Break;
      end;
      // Change left aligning to right
      while True do begin
        i := LeftMostIndex(ParentWnd);
        if i >= 0 then
          Controls[i].Align := alRight
        else
          Break;
      end;
      // Doing recursion
      if Recursion then
        for i := 0 to ControlCount - 1 do
          if Controls[i] is TWinControl then
            ReflectControls(TWinControl(Controls[i]), True);
    end;
  end;
end;


{$IFDEF DELPHI7UP}
function acThemesEnabled: boolean;
begin
{$IFDEF DELPHI_XE2}
  Result := StyleServices.Enabled;
  acThemeServices := StyleServices;
{$ELSE}
  Result := ThemeServices.ThemesEnabled;
  acThemeServices := ThemeServices;
{$ENDIF}
end;
{$ENDIF}


initialization
{$IFDEF DELPHI_XE2}
  acThemeServices := StyleServices;
{$ELSE}
  {$IFDEF DELPHI7UP}
  acThemeServices := ThemeServices;
  {$ENDIF}
{$ENDIF}
  uxthemeLib := LoadLibrary('UXTHEME');
  if uxthemeLib <= HINSTANCE_ERROR then
    uxthemeLib := 0;

  if uxthemeLib <> 0 then
    @Ac_SetWindowTheme := GetProcAddress(uxthemeLib, 'SetWindowTheme');


finalization
  if uxthemeLib <> 0 then
    FreeLibrary(uxthemeLib);

end.

