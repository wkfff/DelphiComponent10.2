unit acPageScroller;
{$I sDefs.inc}
//{$DEFINE LOGGED}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, ExtCtrls,
  {$IFDEF DELPHI_XE4} ComCtrls, CommCtrl, {$ENDIF}
  {$IFNDEF DELPHI5} Types, {$ENDIF}
  sCommonData, sConst, sPanel, acThdTimer;


type
{$IFNDEF NOTFORHELP}
{$IFNDEF DELPHI_XE4}
  TPageScrollerOrientation = (soHorizontal, soVertical);
  TPageScrollEvent = procedure (Sender: TObject; Shift: TShiftState; X, Y: Integer; Orientation: TPageScrollerOrientation; var Delta: Integer) of object;
{$ENDIF}
  TacScrollPosition = (spDefault, spLeft, spRight, spNone);
{$ENDIF}

{$IFDEF DELPHI_XE3}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]{$ENDIF}
  TsPageScroller = class(TsPanel)
{$IFNDEF NOTFORHELP}
  private
    FMargin,
    FPosition,
    FHoverIndex,
    FButtonSize,
    FPressedIndex,
    FPreferredWidth,
    FPreferredHeight: integer;

    Updating,
    FAnimated,
    FAutoScroll: Boolean;

    ScrollShadow1,
    ScrollShadow2: TBitmap;

    Timer: TTimer;
    FScrollStep: word;
    FControl: TWinControl;
    FOnScroll: TPageScrollEvent;
    FScrollPosition: TacScrollPosition;
    FOrientation: TPageScrollerOrientation;
    procedure MakeScrollShadows;
    procedure SavePreferredSize;
    procedure OnBtnTimer(Sender: TObject);
    procedure DoSetControl(Value: TWinControl);
    procedure WMNCHitTest    (var Message: TWMNcHitTest);     message WM_NCHITTEST;
    procedure WMNCMouseMove  (var Message: TMessage);         message WM_NCMOUSEMOVE;
    procedure CMControlChange(var Message: TCMControlChange); message CM_CONTROLCHANGE;
    procedure Scroll(Shift: TShiftState; X, Y: Integer; Orientation: TPageScrollerOrientation; var Delta: Integer); dynamic;

    procedure SetAutoScroll    (const Value: Boolean);
    procedure SetControl       (const Value: TWinControl);
    procedure SetScrollPosition(const Value: TacScrollPosition);
    procedure SetOrientation   (const Value: TPageScrollerOrientation);

    procedure PaintBtn    (aDC: hdc; Btn: integer; ASkinned: boolean);
    procedure PaintStdBtn (aDC: hdc; Btn: integer);
    procedure PaintSkinBtn(aDC: hdc; Btn: integer);

    procedure SetBtnState(Btn, State: integer);
    procedure ChangeBtn  (Btn, AState, AHover, APressed: integer);

    function BtnVisible  (Btn: integer): boolean;
    function BtnState    (Btn: integer): integer;
    function BtnRect     (Btn: integer; WithMargin: boolean = True): TRect;

    function Range:       integer;
    function OffsetLeft:  integer;
    function OffsetRight: integer;

    procedure DoClick(Btn: integer);
    procedure DoMouseLeave;
    function SmallScroll: boolean;
    procedure TryStartTimer;

    procedure SetInteger(const Index, Value: integer);
  protected
    CanDoClick: boolean;
    AnimTimers: array [0..1] of TacThreadedTimer;
    procedure ChangeScale(M, D: Integer); override;
    procedure CopyCache(DC: hdc); override;
  public
    procedure AlignControls(AControl: TControl; var Rect: TRect); override;
    constructor Create(AOwner: TComponent); override;
    function PrepareCache: boolean; override;
    procedure DoScroll(Delta: integer);
    destructor Destroy; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure RecalcSize(NCUpdate: boolean = True);
    procedure WndProc(var Message: TMessage); override;
  published
    property Align;
    property Anchors;
    property AutoScroll: Boolean read FAutoScroll write SetAutoScroll default False;
    property BevelOuter default bvNone;
    property BorderWidth;
    property Color;
    property Constraints;
    property Control: TWinControl read FControl write SetControl;
    property DockSite;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
{$IFDEF DELPHI7UP}
    property ParentBackground default True;
{$ENDIF}
    property ShowHint;
    property TabOrder;
    property TabStop default True;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseWheel;
    property OnResize;
    property OnScroll: TPageScrollEvent read FOnScroll write FOnScroll;
    property OnStartDock;
    property OnStartDrag;
{$ENDIF}
    property Animated: boolean read FAnimated write FAnimated default True;

    property ButtonSize: integer index 0 read FButtonSize write SetInteger default 14;
    property Margin:     integer index 1 read FMargin     write SetInteger default 0;
    property Position:   integer index 2 read FPosition   write SetInteger default 0;

    property Orientation: TPageScrollerOrientation read FOrientation write SetOrientation default soHorizontal;
    property ScrollPosition: TacScrollPosition read FScrollPosition write SetScrollPosition default spDefault;
    property ScrollStep: word read FScrollStep write FScrollStep default 0;
  end;

implementation

uses
  math, TypInfo,
  {$IFDEF LOGGED} sDebugMsgs, {$ENDIF}
  {$IFDEF DELPHI7UP} Themes, {$ENDIF}
  sGraphUtils, sStyleSimply, sVCLUtils, acntUtils, sSkinProps, sAlphaGraph, sDefaults, sMessages, sFade,
  sSkinManager, acntTypes, acgpUtils, sGradient;

{$IFNDEF DELPHI_XE4}
const
  PGM_FIRST      = $1400;
  PGM_SETCHILD   = PGM_FIRST + 1;   { lParam == hwnd }
  PGM_RECALCSIZE = PGM_FIRST + 2;
  PGM_SETPOS     = PGM_FIRST + 8;
  PGM_GETPOS     = PGM_FIRST + 9;
{$ENDIF}


constructor TsPageScroller.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle - [csSetCaption] + [csAcceptsControls, {$IFDEF DELPHI7UP} csParentBackground, {$ENDIF}csCaptureMouse, csClickEvents];
  FPosition := 0;
  FButtonSize := 14;
  FOrientation := soHorizontal;
  FHoverIndex := -1;
  FPressedIndex := -1;
  FScrollStep := 0;
  FAnimated := True;
  FScrollPosition := spDefault;
  Updating := False;
  CanDoClick := False;
  BevelOuter := bvNone;
  ScrollShadow1 := nil;
  ScrollShadow2 := nil;
  Timer := nil;
end;


procedure TsPageScroller.DoSetControl(Value: TWinControl);
begin
  FControl := Value;
  if not (csDestroying in ComponentState) then begin
    if FControl <> nil then begin
      SavePreferredSize;
      FControl.FreeNotification(Self);
      FControl.Parent := Self;
    end;
    RecalcSize;
  end;
end;


procedure TsPageScroller.MakeScrollShadows;
var
  Bmp: TBitmap;
  Mrgn, ShadowSize: integer;
  C, CG: TsColor;

  procedure UpdatePixels(aBmp: TBitmap);
  var
    S0, S: PRGBAArray_S;
    Y, X, Delta: integer;
  begin
    if InitLine(aBmp, Pointer(S0), Delta) then
      for Y := 0 to aBmp.Height - 1 do begin
        S := Pointer(PAnsiChar(S0) + Delta * Y);
          for X := 0 to aBmp.Width - 1 do
            with S[X] do
              if SA = MaxByte then
                SA := 0;
      end;
  end;

  procedure ChangeContrast(aBmp: TBitmap; Side: TacSide);
  var
    S0, S: PRGBAArray_S;
    Y, X, Delta, Coord: integer;
  begin
    case Side of
      asRight:  Coord := aBmp.Width - 1;
      asBottom: Coord := aBmp.Height - 1
      else      Coord := 0;
    end;
    if InitLine(aBmp, Pointer(S0), Delta) then
      case Side of
        asLeft, asRight: for Y := 0 to aBmp.Height - 1 do begin
          S := Pointer(PAnsiChar(S0) + Delta * Y);
          if S <> nil then
            for X := Coord to Coord do
              with S[X] do
                SA := min(MaxByte, SA shl 2);
        end;
        asTop, asBottom: begin
          S := Pointer(PAnsiChar(S0) + Delta * Coord);
          if S <> nil then
            for X := 0 to aBmp.Width - 1 do
              with S[X] do
                SA := min(MaxByte, SA shl 2);
        end;
      end;
  end;

begin
  if ScrollShadow1 = nil then
    ScrollShadow1 := CreateBmp32;

  if ScrollShadow2 = nil then
    ScrollShadow2 := CreateBmp32;

  CG.C := GetControlColor(Self);
  if CG.C = clWhite then
    CG.C := acColorToRGB(clBtnFace);

  CG.A := Max(16, MaxByte - (CG.R + CG.G + CG.B) div 3);
  CG.R := 0;
  CG.G := 0;
  CG.B := 0;

  ShadowSize := min(48, min(Height, Width) div 2);
  Bmp := MakeShadow(CG.C or 0, ShadowSize, 0, 0, 0, ShadowSize);
  if SkinData.Skinned then
    C.C := SkinData.SkinManager.gd[SkinData.SkinIndex].Props[0].Fontcolor.Color
  else
    C.C := 0;

  C.A := 48;

  if Orientation = soHorizontal then begin
    ScrollShadow1.Width := ShadowSize;
    ScrollShadow1.Height := Height;
    ScrollShadow2.Width := ShadowSize;
    ScrollShadow2.Height := Height;
    Mrgn := Bmp.Height div 6;
    acgpStretchRect(ScrollShadow1, Bmp, MkRect(ScrollShadow1), Rect(Bmp.Width - ShadowSize, Mrgn, Bmp.Width, Bmp.Height - Mrgn - 1));
    acgpStretchRect(ScrollShadow2, Bmp, MkRect(ScrollShadow2), Rect(0, Mrgn, ShadowSize, Bmp.Height - Mrgn - 1));
    UpdatePixels(ScrollShadow1);
    UpdatePixels(ScrollShadow2);
    ChangeContrast(ScrollShadow1, asLeft);
    ChangeContrast(ScrollShadow2, asRight);
  end
  else begin
    ScrollShadow1.Width := Width;
    ScrollShadow1.Height := ShadowSize;
    ScrollShadow2.Width := Width;
    ScrollShadow2.Height := ShadowSize;
    Mrgn := Bmp.Width div 6;
    acgpStretchRect(ScrollShadow1, Bmp, MkRect(ScrollShadow1), Rect(Mrgn, Bmp.Height - ShadowSize, Bmp.Width - Mrgn - 1, Bmp.Height));
    acgpStretchRect(ScrollShadow2, Bmp, MkRect(ScrollShadow2), Rect(Mrgn, 0, Bmp.Width - Mrgn - 1, ShadowSize));
    UpdatePixels(ScrollShadow1);
    UpdatePixels(ScrollShadow2);
    ChangeContrast(ScrollShadow1, asTop);
    ChangeContrast(ScrollShadow2, asBottom);
  end;
  Bmp.Free;
end;


procedure TsPageScroller.RecalcSize;
var
  R: TRect;
begin
  if not Updating then
    if Control <> nil then begin
      Updating := True;
      if NCUpdate then
        SetWindowPos(Handle, 0, 0, 0, 0, 0, SWPA_FRAMECHANGED);

      if Orientation = soHorizontal then begin
        if BiDiMode = bdRightToLeft then
          R.Left := FPosition - Range// + integer(BtnVisible(0)) * ButtonSize
        else
          R.Left := -FPosition;

        if Control.Width > ClientWidth then begin
          if R.Left + Control.Width < ClientWidth then
            R.Left := ClientWidth - Control.Width;
        end
        else
          R.Left := 0;

        Control.SetBounds(R.Left, Margin, Control.Width, Height - 2 * Margin);
      end
      else begin
        R.Top := -FPosition;
        if Control.Height > ClientHeight then begin
          if R.Top + Control.Height < ClientHeight then
            R.Top := ClientHeight - Control.Height;
        end
        else
          R.Top := 0;

        Control.SetBounds(Margin, R.Top, Width - 2 * Margin, Control.Height);
      end;
      Updating := False;
    end
    else
      SetWindowPos(Handle, 0, 0, 0, 0, 0, SWPA_FRAMECHANGED);
end;


procedure TsPageScroller.SetControl(const Value: TWinControl);
var
  PrevControl: TWinControl;
begin
  if Control <> Value then begin
    PrevControl := FControl;
    DoSetControl(Value);
    if (PrevControl <> nil) and not (csDestroying in PrevControl.ComponentState) then
      PrevControl.Parent := Parent;
  end;
end;


procedure TsPageScroller.SetInteger(const Index, Value: integer);
begin
  case Index of
    0: if ButtonSize <> Value then begin
      FButtonSize := Value;
      RecalcSize;
    end;

    1: if Margin <> Value then begin
      FMargin := Value;
      RecalcSize;
    end;

    2: if Position <> Value then
      Perform(PGM_SETPOS, 0, Value);
  end;
end;


procedure TsPageScroller.SetOrientation(const Value: TPageScrollerOrientation);
begin
  if Orientation <> Value then begin
    FOrientation := Value;
    RecalcSize;
  end;
end;


procedure TsPageScroller.SavePreferredSize;
begin
  FPreferredWidth := Control.Width;
  FPreferredHeight := Control.Height;
end;


procedure TsPageScroller.WndProc(var Message: TMessage);
var
  DC: hdc;
  R, DstR: TRect;
  i: integer;
begin
{$IFDEF LOGGED}
  AddToLog(Message);
{$ENDIF}
  case Message.Msg of
    SM_ALPHACMD:
      case Message.WParamHi of
        AC_SETNEWSKIN: begin
          FreeAndNil(ScrollShadow1);
          FreeAndNil(ScrollShadow2);
          for i := 0 to 1 do
            if AnimTimers[i] <> nil then
              FreeAndNil(AnimTimers[i]);
        end;

        AC_GETBG: begin
          inherited;
          if Orientation = soHorizontal then
            inc(PacBGInfo(Message.LParam)^.Offset.X, OffsetLeft)
          else
            inc(PacBGInfo(Message.LParam)^.Offset.Y, OffsetLeft);

          Exit;
        end;

        AC_MOUSELEAVE:
          DoMouseLeave;

        AC_ENDPARENTUPDATE: begin
          if SkinData.FUpdating then begin
            if not InUpdating(SkinData, True) then begin
              Repaint;
              Perform(WM_NCPAINT, 0, 0);
            end;

            SetParentUpdated(Self);
          end;
          Exit;
        end;
      end;

    WM_NCCALCSIZE: begin
      inherited;
      with TNCCalcSizeParams(Pointer(Message.LParam)^).rgrc[0] do
        if Orientation = soHorizontal then begin
          Left := Left + OffsetLeft;
          Right := Right - OffsetRight;
        end
        else begin
          Top := Top + OffsetLeft;
          Bottom := Bottom - OffsetRight;
        end;

      Exit;
    end;

    WM_SIZE: begin
      FreeAndNil(ScrollShadow1);
      FreeAndNil(ScrollShadow2);
    end;

    WM_MOUSELEAVE, CM_MOUSELEAVE:
      DoMouseLeave;

    WM_NCLBUTTONDBLCLK:
      if FHoverIndex >= 0 then begin
        StopTimer(AnimTimers[FHoverIndex]);
        SetBtnState(FHoverIndex, 2);
        TryStartTimer;
      end;

    WM_NCLBUTTONDOWN:
      if FHoverIndex >= 0 then begin
        SetBtnState(FHoverIndex, 2);
        TryStartTimer;
      end;

    WM_NCLBUTTONUP:
      if (FHoverIndex >= 0) and (FPressedIndex >= 0) then begin
        SetBtnState(FHoverIndex, 1);
        if CanDoClick then
          DoClick(FHoverIndex);
      end;

    WM_NCPAINT: begin
      if not InUpdating(SkinData) or not SkinData.Skinned then
        for i := 0 to 1 do
          if BtnVisible(i) then begin
            if SkinData.BGChanged and SkinData.Skinned then
              PrepareCache;

            if FMargin > 0 then begin
              DC := GetWindowDC(Handle);
              try
                R := BtnRect(i);
                DstR := BtnRect(i, False);
                ExcludeClipRect(DC, R.Left, R.Top, R.Right, R.Bottom);
                BitBlt(DC, DstR.Left, DstR.Top, WidthOf(DstR), HeightOf(DstR), SkinData.FCacheBmp.Canvas.Handle, DstR.Left, DstR.Top, SRCCOPY);
              finally
                ReleaseDC(Handle, DC);
              end;
            end;
            PaintBtn(0, i, SkinData.Skinned);
          end;

      Exit;
    end;
  end;
  inherited;
  case Message.Msg of
    WM_SIZE, PGM_RECALCSIZE:
      RecalcSize;

    PGM_GETPOS:
      Message.Result := FPosition;

    PGM_SETPOS: begin
      FPosition := Message.LParam;
      RecalcSize;
    end;

    WM_PRINT: begin
      PaintBtn(TWMPaint(Message).DC, 0, SkinData.Skinned);
      PaintBtn(TWMPaint(Message).DC, 1, SkinData.Skinned);
      R := BtnRect(1);
      if not IsRectEmpty(R) then
        ExcludeClipRect(TWMPaint(Message).DC, R.Left, R.Top, R.Right, R.Bottom);

      R := BtnRect(0);
      if not IsRectEmpty(R) then begin
        ExcludeClipRect(TWMPaint(Message).DC, R.Left, R.Top, R.Right, R.Bottom);
        if Orientation = soHorizontal then
          MoveWindowOrg(TWMPaint(Message).DC, WidthOf(R), 0)
        else
          MoveWindowOrg(TWMPaint(Message).DC, 0, HeightOf(R));
      end;
    end;
  end;
end;


function UpdateBtn_CB(Data: TObject; Iteration: integer): boolean;
var
  DC: HDC;
  R: TRect;
  b: integer;
  Bmp: TBitmap;
begin
  Result := False;
  if Data is TacThreadedTimer then
    with TacThreadedTimer(Data) do
      if Assigned(BmpFrom) and Assigned(BmpTo) and Assigned(AnimControl) then begin
        R := AnimRect;
        Bmp := CreateBmpLike(BmpTo);
        BitBlt(Bmp.Canvas.Handle, 0, 0, Bmp.Width, Bmp.Height, BmpFrom.Canvas.Handle, 0, 0, SRCCOPY);
        case State of
          0, 2: Glow := TacThreadedTimer(Data).Glow - GlowStep;
          1:    Glow := Glow + GlowStep
          else  Glow := MaxByte - (Iteration / Iterations) * MaxByte;
        end;
        b := LimitIt(Round(Glow), 0, MaxByte);
        SumBitmaps(Bmp, BmpTo, iff(State = 1, MaxByte - b, b));
        DC := GetWindowDC(TsPageScroller(AnimControl).Handle);
        try
          BitBlt(DC, R.Left, R.Top, Bmp.Width, Bmp.Height, Bmp.Canvas.Handle, 0, 0, SRCCOPY);
        finally
          ReleaseDC(TsPageScroller(AnimControl).Handle, DC);
          Bmp.Free;
        end;
        if Iteration >= TacThreadedTimer(Data).Iterations then begin
          if State = 0 then
            if b > 0 then begin
              Sleep(acTimerInterval);
              Result := UpdateBtn_CB(Data, Iteration);
            end
            else
              Result := False;
        end
        else
          Result := True;
      end;
end;


procedure TsPageScroller.ChangeBtn(Btn, AState, AHover, APressed: integer);
var
  R: TRect;
  i: integer;
  SavedDC: hdc;
begin
  R := BtnRect(Btn);
  if (AState > 0) and (SkinData.SkinManager <> nil) then
    SkinData.SkinManager.ActiveControl := Handle;

  if Animated and ((SkinData.SkinManager = nil) or SkinData.SkinManager.Effects.AllowAnimation) and (AState <> 2) then begin
    i := GetNewTimer(AnimTimers[Btn], Self, AState);
    AnimTimers[Btn].Tag := Btn;
    AnimTimers[Btn].AnimRect := R;
    AnimTimers[Btn].AnimControl := Self;
    with AnimTimers[Btn] do
      if (State < 0) or (AState <> AnimTimers[Btn].State) then begin // Not started already
        if AnimTimers[Btn].BmpFrom <> nil then
          FreeAndNil(AnimTimers[Btn].BmpFrom);

        if AnimTimers[Btn].BmpTo <> nil then
          AnimTimers[Btn].BmpFrom := AnimTimers[Btn].BmpTo
        else begin
          AnimTimers[Btn].BmpFrom := CreateBmp32(R);
          SavedDC := SaveDC(BmpFrom.Canvas.Handle);
          try
            BmpFrom.Canvas.Lock;
            MoveWindowOrg(BmpFrom.Canvas.Handle, -R.Left, -R.Top);
            PaintSkinBtn(BmpFrom.Canvas.Handle, Btn);
          finally
            BmpFrom.Canvas.UnLock;
            RestoreDC(BmpFrom.Canvas.Handle, SavedDC);
          end;
        end;
        FPressedIndex := APressed;
        FHoverIndex := AHover;
        BmpTo := CreateBmp32(BmpFrom);
        SavedDC := SaveDC(BmpTo.Canvas.Handle);
        try
          BmpTo.Canvas.Lock;
          MoveWindowOrg(BmpTo.Canvas.Handle, -R.Left, -R.Top);
          PaintBtn(BmpTo.Canvas.Handle, Btn, SkinData.Skinned);
        finally
          BmpTo.Canvas.UnLock;
          RestoreDC(BmpTo.Canvas.Handle, SavedDC);
        end;
        InitData(AnimTimers[Btn], i, UpdateBtn_CB, AState, AState = 2);
        TimeHandler;
      end;
  end
  else begin
    if AnimTimers[Btn] <> nil then begin
      AnimTimers[Btn].Enabled := False;
      FreeAndNil(AnimTimers[Btn]);
    end;
    FPressedIndex := APressed;
    FHoverIndex := AHover;
    Perform(WM_NCPAINT, 0, 0);
  end;
end;


procedure TsPageScroller.ChangeScale(M, D: Integer);
var
  i: integer;
begin
  if M <> D then begin
    inherited;
    ButtonSize := MulDiv(FButtonSize, M, D);
    for i := 0 to 1 do
      if AnimTimers[i] <> nil then
        FreeAndNil(AnimTimers[i]);
  end
  else
    inherited;
end;

procedure TsPageScroller.CMControlChange(var Message: TCMControlChange);
begin
  if not (csLoading in ComponentState) and (Message.Control is TWinControl) then
    if Message.Inserting then
      DoSetControl(TWinControl(Message.Control));
end;


procedure TsPageScroller.AlignControls(AControl: TControl; var Rect: TRect);
begin
  if (csDesigning in ComponentState) or (AControl <> nil) and (AControl = Control) then begin
    inherited AlignControls(AControl, Rect);
    if Control <> nil then begin
      SavePreferredSize;
      if (Orientation = soHorizontal) and (Control.Height = ClientHeight) or (Orientation = soVertical) and (Control.Width = ClientWidth) then
        Perform(PGM_RECALCSIZE, 0, 0);
    end;
  end;
  FPosition := Perform(PGM_GETPOS, 0, 0);
end;


procedure TsPageScroller.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = Control) then begin
    Control := nil;
    if not (csDestroying in ComponentState) then begin
      Position := 0;
      RecalcSize;
    end;
  end;
end;


procedure TsPageScroller.Scroll(Shift: TShiftState; X, Y: Integer; Orientation: TPageScrollerOrientation; var Delta: Integer);
begin
  if Assigned(FOnScroll) then
    FOnScroll(Self, Shift, X, Y, Orientation, Delta);
end;


procedure TsPageScroller.SetAutoScroll(const Value: Boolean);
begin
  if AutoScroll <> Value then begin
    FAutoScroll := Value;
    RecalcSize;
  end;
end;


function TsPageScroller.BtnVisible(Btn: integer): boolean;
begin
  case FScrollPosition of
    spDefault:
      if BidiMode = bdRightToLeft then
        Result := iff(Btn = 0, Range - FPosition > 0, Position > 0)
      else
        Result := iff(Btn = 0, Position > 0, Range - FPosition > 0);
    spNone:    Result := False
    else       Result := True;
  end;
end;


function TsPageScroller.Range: integer;
var
  CtrlSize, OwnSize, ClientSize: integer;
begin
  if (Control = nil) or (ClientWidth = 0) then
    Result := 0
  else begin
    if FOrientation = soHorizontal then begin
      CtrlSize := Control.Width;
      OwnSize := Width;
      ClientSize := ClientWidth;
    end
    else begin
      CtrlSize := Control.Height;
      OwnSize := Height;
      ClientSize := ClientHeight;
    end;
    if CtrlSize < OwnSize then
      Result := 0
    else
      if BidiMode = bdRightToLeft then
        if CtrlSize - ClientSize - FPosition > 0 then // If first btn visible
          if FPosition > 0 then // If second btn visible
            Result := CtrlSize - OwnSize + 2 * ButtonSize
          else
            Result := CtrlSize - OwnSize + ButtonSize
        else
          Result := CtrlSize - ClientSize
      else
        if FPosition > 0 then // If first btn visible
          if CtrlSize - FPosition > OwnSize - ButtonSize then // If second btn visible
            Result := CtrlSize - OwnSize + 2 * ButtonSize
          else
            Result := CtrlSize - OwnSize + ButtonSize
        else
          Result := CtrlSize - ClientSize;
  end;
end;


function TsPageScroller.BtnState(Btn: integer): integer;
begin
  if BiDiMode = bdRightToLeft then
    if ((Btn = 0) and (FPosition = Range)) or ((Btn = 1) and (FPosition = 0)) then
      Result := 3
    else
      Result := iff(FPressedIndex = Btn, 2, integer(FHoverIndex = Btn))
  else
    if ((Btn = 0) and (FPosition = 0)) or ((Btn = 1) and (FPosition = Range)) then
      Result := 3
    else
      Result := iff(FPressedIndex = Btn, 2, integer(FHoverIndex = Btn));
end;


function TsPageScroller.BtnRect(Btn: integer; WithMargin: boolean = True): TRect;
var
  Mrgn: integer;
begin
  if not BtnVisible(Btn) then
    Result := MkRect
  else begin
    Mrgn := integer(WithMargin) * Margin;
    case FScrollPosition of
      spDefault:
        if FOrientation = soHorizontal then
          if Btn = 0 then
            Result := Rect(0, Mrgn, ButtonSize, Height - Mrgn)
          else
            Result := Rect(Width - ButtonSize, Mrgn, Width, Height - Mrgn)
        else
          if Btn = 0 then
            Result := Rect(Mrgn, 0, Width - Mrgn, ButtonSize)
          else
            Result := Rect(Mrgn, Height - ButtonSize, Width - Mrgn, Height);

      spLeft:
        if FOrientation = soHorizontal then
          Result := Rect(Btn * FButtonSize, Mrgn, Btn * FButtonSize + ButtonSize, Height - Mrgn)
        else
          Result := Rect(Mrgn, Btn * FButtonSize, Width - Mrgn, Btn * FButtonSize + ButtonSize);

      spRight:
        if FOrientation = soHorizontal then
          Result := Rect(Width - ButtonSize - ((Btn + 1) and 1) * FButtonSize, Mrgn, Width - ((Btn + 1) and 1) * FButtonSize, Height - Mrgn)
        else
          Result := Rect(Mrgn, Height - ButtonSize - ((Btn + 1) and 1) * FButtonSize, Width - Mrgn, Height - ((Btn + 1) and 1) * FButtonSize)
    end;
  end;
end;


procedure TsPageScroller.PaintStdBtn(aDC: hdc; Btn: integer);
const
  ScrollBtns: array [boolean, 0..1] of Cardinal = ((DFCS_SCROLLUP, DFCS_SCROLLDOWN), (DFCS_SCROLLLEFT, DFCS_SCROLLRIGHT));
  ScrollStates: array [0..3] of Cardinal = (0, DFCS_HOT, DFCS_PUSHED, DFCS_INACTIVE);
{$IFDEF DELPHI7UP}
  ThemedScrolls: array [boolean, 0..1, 0..3] of TThemedScrollBar =
    (((tsArrowBtnUpNormal,    tsArrowBtnUpHot,    tsArrowBtnUpPressed,    tsArrowBtnUpDisabled),
      (tsArrowBtnDownNormal,  tsArrowBtnDownHot,  tsArrowBtnDownPressed,  tsArrowBtnDownDisabled)),
     ((tsArrowBtnLeftNormal,  tsArrowBtnLeftHot,  tsArrowBtnLeftPressed,  tsArrowBtnLeftDisabled),
      (tsArrowBtnRightNormal, tsArrowBtnRightHot, tsArrowBtnRightPressed, tsArrowBtnRightDisabled)));
{$ENDIF}
var
  DC: hdc;
  R: TRect;
  State: integer;
begin
  R := BtnRect(Btn);
  if not IsRectEmpty(R) then begin
    if aDC = 0 then
      DC := GetWindowDC(Handle)
    else
      DC := aDC;

    State := BtnState(Btn);
    try
{$IFDEF DELPHI7UP}
      if acThemesEnabled then
        acThemeServices.DrawElement(DC, acThemeServices.GetElementDetails(ThemedScrolls[Orientation = soHorizontal, Btn, State]), R)
      else
{$ENDIF}
        DrawFrameControl(DC, R, DFC_SCROLL, ScrollBtns[Orientation = soHorizontal, Btn] or ScrollStates[State]);
    finally
      if aDC = 0 then
        ReleaseDC(Handle, DC);
    end;
  end;
end;


function TsPageScroller.PrepareCache: boolean;
var
  R: TRect;
  ShadowSize: integer;
begin
  Result := inherited PrepareCache;
  if Result then begin
    if ScrollShadow1 = nil then
      MakeScrollShadows;

    ShadowSize := min(48, min(Height, Width) div 2);
    if BtnVisible(0) then begin
      R := BtnRect(0);
      if Orientation = soHorizontal then begin
        R.Left := R.Right;
        R.Right := R.Left + ShadowSize;
        CopyBmp32(R, MkRect(ScrollShadow1), SkinData.FCacheBmp, ScrollShadow1, EmptyCI, False, clNone, 0, False);
      end
      else begin
        R.Top := R.Bottom;
        R.Bottom := R.Top + ShadowSize;
        CopyBmp32(R, MkRect(ScrollShadow1), SkinData.FCacheBmp, ScrollShadow1, EmptyCI, False, clNone, 0, False);
      end;
    end;
    if BtnVisible(1) then begin
      R := BtnRect(1);
      if Orientation = soHorizontal then begin
        R.Right := R.Left;
        R.Left := R.Right - ShadowSize;
        CopyBmp32(R, MkRect(ScrollShadow2), SkinData.FCacheBmp, ScrollShadow2, EmptyCI, False, clNone, 0, False);
      end
      else begin
        R.Bottom := R.Top;
        R.Top := R.Bottom - ShadowSize;
        CopyBmp32(R, MkRect(ScrollShadow2), SkinData.FCacheBmp, ScrollShadow2, EmptyCI, False, clNone, 0, False);
      end;
    end;
  end;
end;


procedure TsPageScroller.WMNCHitTest(var Message: TWMNcHitTest);
var
  p: TPoint;
  R: TRect;
begin
  p := ScreenToClient(Point(Message.XPos, Message.YPos));
  R := BtnRect(0);
  if Orientation = soHorizontal then
    case ScrollPosition of
      spLeft:    p.X := p.X + R.Right * 2;
      spDefault: p.X := p.X + R.Right;
    end
  else
    case ScrollPosition of
      spLeft:    p.Y := p.Y + R.Bottom * 2;
      spDefault: p.Y := p.Y + R.Bottom;
    end;

  if (PtInRect(R, p) or PtInRect(BtnRect(1), p)) then
    Message.Result := HTOBJECT
  else
    inherited;
end;


procedure TsPageScroller.SetBtnState(Btn, State: integer);
begin
  case State of
    2:
      if FPressedIndex <> Btn then
        ChangeBtn(Btn, State, Btn, Btn);

    1: begin
      if (FHoverIndex <> Btn) or (FPressedIndex = Btn) then begin
        if FHoverIndex >= 0 then
          ChangeBtn(FHoverIndex, 0, -1, -1);

        ChangeBtn(Btn, State, Btn, -1);
      end;
      if (Timer <> nil) and not AutoScroll then
        FreeAndNil(Timer);
    end

    else begin
      if (FHoverIndex >= 0) or (FPressedIndex >= 0) then
        ChangeBtn(Btn, State, -1, -1);

      if (Timer <> nil) and not AutoScroll then
        FreeAndNil(Timer);
    end;
  end;
end;


procedure TsPageScroller.DoClick(Btn: integer);
var
  KeyState: TKeyboardState;
  Delta, iRange, ClientSize: integer;
begin
  if BtnState(Btn) <> 3 then begin
    CanDoClick := False;
    Delta := 0;
    iRange := Range;
    ClientSize := iff(Orientation = soHorizontal, ClientWidth, ClientHeight);
    if (BiDiMode <> bdRightToLeft) or (Orientation = soVertical) then
      case Btn of
        0: begin
          Delta := iff(FScrollStep > 0, -FScrollStep, -ClientSize * 4 div 5);
          Delta := max(Delta, -FPosition);
          Delta := iff(Delta + FPosition <= ButtonSize, -FPosition, Delta);
        end;
        1: Delta := min(iff(FScrollStep > 0, FScrollStep, ClientSize * 4 div 5), iRange - FPosition);
      end
    else
      case Btn of
        1: begin
          Delta := iff(FScrollStep > 0, -FScrollStep, -ClientSize * 4 div 5);
          Delta := max(Delta, -FPosition);
          Delta := iff(Delta + FPosition <= ButtonSize, -FPosition, Delta);
        end;
        0: Delta := min(iff(FScrollStep > 0, FScrollStep, ClientSize * 4 div 5), iRange - FPosition);
      end;

    GetKeyboardState(KeyState);
    Scroll(KeyboardStateToShiftState(KeyState), acMousePos.X, acMousePos.Y, Orientation, Delta);
    DoScroll(Delta);
  end;
end;


procedure TsPageScroller.DoScroll(Delta: integer);
const
  Speed = 3;
  Steps = acMaxIterations * Speed - 1;
var
  CtrlArray: TacCtrlArray;
  NewPos, i, cx, j1, j2: integer;
  Pos, Step, r: real;
  Flags: Cardinal;
  lTicks: DWord;
begin
  if Control <> nil then
    if Animated and ((SkinData.SkinManager = nil) or SkinData.SkinManager.Effects.AllowAnimation) then begin
      j1 := 0;
      j2 := 0;
      for i := 0 to 1 do
        if BtnVisible(i) and (AnimTimers[i] <> nil) and not AnimTimers[i].Destroyed then
          FinishTimer(AnimTimers[i]);

      NewPos := Position + Delta;
      if ScrollPosition = spDefault then begin
        if not BtnVisible(1) and (Delta > 0) then begin
          j1 := ButtonSize;
          if SkinData.Skinned then begin
            Perform(WM_SETREDRAW, 0, 0);
            Position := j1;
            Perform(WM_SETREDRAW, 1, 0);
          end
          else
            Position := j1;
        end;
      end
      else
        j1 := 0;

      if (FPosition + Delta = 0) and (Delta < 0) then
        j2 := ButtonSize;

      cx := Delta - j1 + j2;
      Pos := Position;
      Flags := RDW_INVALIDATE or RDW_UPDATENOW or RDW_ALLCHILDREN or RDW_NOERASE;
      if not SkinData.Skinned then
        Flags := Flags or RDW_ERASE;

      Step := 0;
      SetLength(CtrlArray, 0);
      SaveGraphCtrls(Self, CtrlArray);
      for i := 0 to Steps - 1 do begin
        lTicks := GetTickCount;
        Perform(WM_SETREDRAW, 0, 0);

        r := (cx - Step) / Speed;
        Pos := Pos + r;
        Step := Step + r;
        Position := Round(Pos);

        Perform(WM_SETREDRAW, 1, 0);
        RedrawWindow(Control.Handle, nil, 0, Flags);
        WaitTicks(lTicks);
        if Abs(r) < 0.1 then
          Break;
      end;
      RestoreGraphCtrls(Self, CtrlArray);
      for i := 0 to 1 do
        if (AnimTimers[i] <> nil) and not AnimTimers[i].Destroyed then begin
          AnimTimers[i].Enabled := False;
          AnimTimers[i].State := -1;
          FreeAndNil(AnimTimers[i]);
        end;

      RecalcSize;
      Position := NewPos;
    end
    else begin
      for i := 0 to 1 do
        if BtnVisible(i) and (AnimTimers[i] <> nil) and not AnimTimers[i].Destroyed then
          FinishTimer(AnimTimers[i]);

      Position := FPosition + Delta;
      RecalcSize;
    end
  else begin
    Position := 0;
    RecalcSize;
  end;
end;


procedure TsPageScroller.TryStartTimer;
begin
  if Timer <> nil then
    FreeAndNil(Timer);

  CanDoClick := True;
  Timer := TTimer.Create(Self);
  Timer.Enabled := False;
  Timer.Interval := MaxByte;
  Timer.OnTimer := OnBtnTimer;
  Timer.Enabled := True;
end;


procedure TsPageScroller.OnBtnTimer(Sender: TObject);
begin
  if CanDoClick then begin
    CanDoClick := False;
    Timer.Interval := acTimerInterval;
  end;
  if not SmallScroll and (Timer <> nil) then begin
    FreeAndNil(Timer);
    FHoverIndex := -1;
    FPressedIndex := -1;
    RecalcSize;
  end;
end;


function TsPageScroller.SmallScroll: boolean;
var
  b: boolean;
  iBtnNdx: integer;
begin
  Result := False;
  if BiDiMode <> bdRightToLeft then
    iBtnNdx := FHoverIndex
  else
    iBtnNdx := integer(not boolean(FHoverIndex));

  case iBtnNdx of
    0:
      if FPosition > 0 then begin
        Perform(WM_SETREDRAW, 0, 0);
        if FPosition <= ButtonSize then begin // Repaint when button is hiding
          Position := 0;
          b := True;
        end
        else begin
          Position := FPosition - ButtonSize;
          b := False;
        end;
        Perform(WM_SETREDRAW, 1, 0);
        RedrawWindow(Control.Handle, nil, 0, RDW_INVALIDATE or RDW_UPDATENOW or RDW_ALLCHILDREN);
        if b then
          Perform(WM_NCPAINT, 1, 0);

        if Position <= 0 then begin
          FHoverIndex := -1;
          FPressedIndex := -1;
        end;
        Result := True;
      end;

    1:
      if FPosition < Range then begin
        Perform(WM_SETREDRAW, 0, 0);
        if not BtnVisible(0) then begin // Repaint when button is showing
          Position := FPosition + ButtonSize;
          b := True;
        end
        else begin
          Position := FPosition + ButtonSize;
          b := False;
        end;
        Perform(WM_SETREDRAW, 1, 0);
        RedrawWindow(Control.Handle, nil, 0, RDW_INVALIDATE or RDW_UPDATENOW or RDW_ALLCHILDREN);
        if b then
          Perform(WM_NCPAINT, 1, 0);

        if Position >= Range then begin
          FHoverIndex := -1;
          FPressedIndex := -1;
        end;
        Result := True;
      end;
  end;
end;


procedure TsPageScroller.PaintSkinBtn(aDC: hdc; Btn: integer);
const
  BtnSizes: array [boolean, 0..1] of TacSide = ((asLeft, asRight), (asTop, asBottom));
var
  DC: hdc;
  R: TRect;
  C: TsColor;
  b: boolean;
  Bmp: TBitmap;
  CI: TCacheInfo;
  sm: TsSkinManager;
  BGInfo: TacBGInfo;
  BtnNdx, State, iNdx: integer;
begin
  R := BtnRect(Btn);
  if not IsRectEmpty(R) then begin
    if aDC = 0 then
      DC := GetWindowDC(Handle)
    else
      DC := aDC;

    b := False;
    try
      Bmp := CreateBmp32(R);
      sm := SkinData.SkinManager;
      if sm <> nil then
        BtnNdx := sm.ConstData.Sections[ssToolButton]
      else
        BtnNdx := -1;

      C.I := 0;
      if BtnNdx >= 0 then begin
        GetBGInfo(@BGInfo, Self);
        BGInfo.Offset := MkPoint;
        CI := BGInfoToCI(@BGInfo);
        State := BtnState(Btn);
        if State > 2 then begin
          State := 0;
          b := True;
        end;
        PaintItem(BtnNdx, CI, True, State, MkRect(Bmp), R.TopLeft, Bmp);
        if (State = 0) and (sm.gd[BtnNdx].Props[0].Transparency = 100) then
          iNdx := GetFontIndex(Self, SkinData.SkinIndex, sm)
        else
          iNdx := BtnNdx;

        if iNdx >= 0 then begin
          State := mini(State, ac_MaxPropsIndex);
          C.C := sm.gd[iNdx].Props[State].Fontcolor.Color;
        end;
      end;
      C.A := MaxByte;
      if SkinData.SkinManager <> nil then
        DrawArrow(Bmp, C.C, clNone, MkRect(Bmp), BtnSizes[Orientation <> soHorizontal, Btn], 0, 0, 0, sm.Options.ActualArrowStyle)
      else
        DrawArrow(Bmp, C.C, clNone, MkRect(Bmp), BtnSizes[Orientation <> soHorizontal, Btn], 0, 0, 0, arsSolid1);

      if b and CI.Ready then // If disabled
        BlendTransRectangle(Bmp, 0, 0, CI.Bmp, R, DefBlendDisabled)
      else
        FadeBmp(Bmp, R, DefBlendDisabled, TsColor(CI.FillColor), 0, 0);

      BitBlt(DC, R.Left, R.Top, Bmp.Width, Bmp.Height, Bmp.Canvas.Handle, 0, 0, SRCCOPY);
      Bmp.Free;
    finally
      if aDC = 0 then
        ReleaseDC(Handle, DC);
    end;
  end;
end;


destructor TsPageScroller.Destroy;
begin
  if Timer <> nil then FreeAndNil(Timer);
  if ScrollShadow1 <> nil then ScrollShadow1.Free;
  if ScrollShadow2 <> nil then ScrollShadow2.Free;
  inherited;
end;


procedure TsPageScroller.SetScrollPosition(const Value: TacScrollPosition);
begin
  if FScrollPosition <> Value then begin
    FScrollPosition := Value;
    if not (csLoading in ComponentState) then
      RecalcSize;
  end;
end;


function TsPageScroller.OffsetLeft: integer;
begin
  Result := 0;
  if (Control <> nil) and (Range > 0) then
    case FScrollPosition of
      spLeft: Result := 2 * ButtonSize;
      spDefault:
        if (BiDiMode <> bdRightToLeft) or (Orientation = soVertical) then
          Result := integer(FPosition > 0) * ButtonSize
        else
          Result := integer(FPosition < Range) * ButtonSize;
    end
end;


function TsPageScroller.OffsetRight: integer;
begin
  Result := 0;
  if (Control <> nil) and (Range > 0) then
    case FScrollPosition of
      spRight: Result := 2 * ButtonSize;
      spDefault:
        if (BiDiMode <> bdRightToLeft) or (Orientation = soVertical) then
          Result := integer(FPosition < Range) * ButtonSize
        else
          Result := integer(FPosition > 0) * ButtonSize;
    end
end;


procedure TsPageScroller.CopyCache(DC: hdc);
begin
  CopyWinControlCache(Self, SkinData,
    Rect(OffsetLeft * integer(Orientation = soHorizontal), OffsetLeft * integer(Orientation <> soHorizontal), 0, 0), MkRect(Width, Height), DC, False)
end;


procedure TsPageScroller.PaintBtn(aDC: hdc; Btn: integer; ASkinned: boolean);
begin
  if ASkinned then
    PaintSkinBtn(aDC, Btn)
  else
    PaintStdBtn(aDC, Btn)
end;


procedure TsPageScroller.WMNCMouseMove(var Message: TMessage);
var
  R: TRect;
begin
  if (SkinData.SkinManager <> nil) and (SkinData.SkinManager.ActiveControl <> Handle) then
    SkinData.SkinManager.ActiveControl := Handle;

  GetWindowRect(Handle, R);
  R.Left := acMousePos.X - R.Left;
  R.Top  := acMousePos.Y - R.Top;
  if PtInRect(BtnRect(0), R.TopLeft) then begin
    if FPressedIndex < 0 then
      SetBtnState(0, 1);

    if AutoScroll and (Timer = nil) then
      TryStartTimer;
  end
  else
    if PtInRect(BtnRect(1), R.TopLeft) then begin
      if FPressedIndex < 0 then
        SetBtnState(1, 1);

      if AutoScroll and (Timer = nil) then
        TryStartTimer;
    end
    else
      SetBtnState(FHoverIndex, -1);

  inherited;
end;


procedure TsPageScroller.DoMouseLeave;
begin
  if not AutoScroll then begin
    SetBtnState(FHoverIndex, 0);
    CanDoClick := False;
  end
  else
    if Timer = nil then
      CanDoClick := False
    else
      SetBtnState(FHoverIndex, 0);
end;

end.
