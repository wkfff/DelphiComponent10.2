unit sTrackBar;
{$I sDefs.inc}
//{$DEFINE LOGGED}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, comctrls,
  commctrl, ExtCtrls,
  {$IFNDEF DELPHI5} types,{$ENDIF}
  {$IFDEF LOGGED} sDebugMsgs, {$ENDIF}
  {$IFDEF TNTUNICODE} TntComCtrls, {$ENDIF}
  sConst, acntUtils, sGraphUtils, sDefaults, sCommonData, sFade;


type
  TsTrackBar = class;
{$IFNDEF NOTFORHELP}
  TAPoint = array of TPoint;
{$ENDIF}

{$IFDEF DELPHI_XE3}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]{$ENDIF}
{$IFDEF TNTUNICODE}
  TsTrackBar = class(TTnTTrackBar)
{$ELSE}
  TsTrackBar = class(TTrackBar)
{$ENDIF}
{$IFNDEF NOTFORHELP}
  private
    FOnUserChange,
    FOnUserChanged: TNotifyEvent;

    EventFlag,
    FReversed,
    FShowFocus,
    AppShowHint,
    FShowProgress: boolean;

    Thumb,
    FThumbGlyph: TBitmap;

    FCanvas: TCanvas;
    FBarOffset: TPoint;
    FCommonData: TsCommonData;
    FOnSkinPaint: TPaintEvent;
    FShowProgressFrom: Integer;
    FDisabledKind: TsDisabledKind;
    FAnimatEvents: TacAnimatEvents;
    function GetHBarOffset: Integer;
    function GetVBarOffset: Integer;
    procedure SetHBarOffset      (const Value: Integer);
    procedure SetVBarOffset      (const Value: Integer);
    procedure SetThumbGlyph      (const Value: TBitmap);
    procedure SetShowProgressFrom(const Value: Integer);
    procedure SetDisabledKind    (const Value: TsDisabledKind);
    procedure SetBoolean         (const Index: Integer; const Value: boolean);
    procedure WMNCHitTest        (var Message: TWMNCHitTest);
    procedure WMMouseMsg         (var Message: TWMMouse);
    procedure TBMGetThumbRect    (var Message: TMessage);
    function ActReversed: boolean;
  protected
    TickHeight,
    FTickNdx,
    FThumbNdx,
    FSliderNdx,
    FProgressNdx: integer;

    iStep: real;

    procedure UpdateIndexes(MainNdx: integer);

    procedure StdPaintBG (Bmp: TBitmap);
    procedure StdPaintBar(Bmp: TBitmap);
    procedure StdPaintThumb;

    procedure PaintBody;
    procedure PaintBar;
    procedure PaintTicksHor;
    procedure PaintTicksVer;
    procedure PaintTick    (P: TPoint; Horz: boolean);
    procedure PaintProgress(R: TRect;  Horz: boolean);
    procedure PaintThumb;

    procedure Paint;
    procedure PrepareCache;

    procedure PaintWindow(DC: HDC); override;
    property Canvas: TCanvas read FCanvas;
    procedure WndProc(var Message: TMessage); override;
    procedure ChangeScale(M, D: Integer); override;
    procedure UserChanged(Finished: boolean);
  public
    function ThumbRect: TRect;
    function ChannelRect: TRect;
    function TickPos(i: integer): integer;
    function TickCount: integer;
    function TicksArray(ChRect: TRect): TAPoint;
    constructor Create(AOwner: TComponent); override;
    procedure CreateWnd; override;
    destructor Destroy; override;
    procedure AfterConstruction; override;
    procedure Loaded; override;
    function Mode: integer;
  published
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property ThumbLength default 23;
{$ENDIF} // NOTFORHELP
    property AnimatEvents: TacAnimatEvents read FAnimatEvents write FAnimatEvents   default [aeGlobalDef];
    property DisabledKind: TsDisabledKind  read FDisabledKind write SetDisabledKind default DefDisabledKind;
    property SkinData:     TsCommonData    read FCommonData   write FCommonData;

    property Reversed:     boolean index 0 read FReversed     write SetBoolean default False;
    property ShowFocus:    boolean index 1 read FShowFocus    write SetBoolean default False;
    property ShowProgress: boolean index 2 read FShowProgress write SetBoolean default False;

    property ThumbGlyph:   TBitmap read FThumbGlyph write SetThumbGlyph;
{$IFNDEF NOTFORHELP}
    property OnSkinPaint:  TPaintEvent read FOnSkinPaint write FOnSkinPaint;
    property TickStyle;
    property TickMarks;
{$ENDIF} // NOTFORHELP
    property OnUserChange:  TNotifyEvent read FOnUserChange  write FOnUserChange;
    property OnUserChanged: TNotifyEvent read FOnUserChanged write FOnUserChanged;
    property BarOffsetV: Integer read GetVBarOffset write SetVBarOffset default 0;
    property BarOffsetH: Integer read GetHBarOffset write SetHBarOffset default 0;
    property ShowProgressFrom: Integer read FShowProgressFrom write SetShowProgressFrom default 0;
  end;


{$IFNDEF NOTFORHELP}
  TacChangingMode = (cmDepended, cmUndepended, cmIgnoredSecond);
{$ENDIF} // NOTFORHELP

{$IFDEF DELPHI_XE3}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]{$ENDIF}
  TsRangeSelector = class(TCustomControl)
{$IFNDEF NOTFORHELP}
  private
    FMax,
    FMin,
    FState1,
    FState2,
    FFrequency,
    FPosition2,
    FPosition1: integer;

    FShowFocus,
    FShowRange: boolean;

    FCommonData: TsCommonData;
    FOrientation: TTrackBarOrientation;
    FTickStyle: TTickStyle;
    FTickMarks: TTickMark;
    PrevMousePos: TPoint;
    FChangingMode: TacChangingMode;
    FOnChange: TNotifyEvent;
    FChannelMargin: integer;
    procedure WMMouseMsg (var Message: TWMMouse);
    procedure SetBoolean(const Index: Integer; const Value: boolean);
    procedure SetInteger(const Index, Value: integer);
    procedure SetOrientation(const Value: TTrackBarOrientation);
    procedure SetTickMarks(const Value: TTickMark);
    procedure SetTickStyle(const Value: TTickStyle);
    procedure SetState(const Index, Value: integer);
    procedure SetPosition(const Index, Value: integer);
  protected
    iStep: real;

    TickHeight,
    FTickNdx,
    FThumbNdx,
    FSliderNdx,
    FRangeNdx: integer;

    procedure PrepareCache;
    procedure UpdateIndexes(MainNdx: integer);

    procedure WndProc(var Message: TMessage); override;

    procedure StdPaintBG (Bmp: TBitmap);
    procedure StdPaintBar(Bmp: TBitmap);
    procedure StdPaintThumb(Index: integer);

    procedure Paint; override;
    procedure PaintBar;
    procedure PaintBody;
    procedure PaintTicksHor;
    procedure PaintTicksVer;
    procedure PaintTick(P: TPoint; Horz: boolean);
    procedure PaintRange(R: TRect; Horz: boolean);
    procedure PaintThumb(Index, State: integer);

    function Mode: integer;
    function ThumbSize: TSize;
    function ChannelSize: TSize;
    function TickCount: integer;
    function TickPos(i: integer): integer;
    function Coord(Index: integer): integer;
    function ThumbRect(Coord: integer): TRect;
    function RangeRect(aRect: TRect): TRect;
    function TicksArray(ChRect: TRect): TAPoint;
    property State1: integer index 0 read FState1 write SetState default 0;
    property State2: integer index 1 read FState2 write SetState default 0;
    procedure Changed; dynamic;
    procedure ChangeScale(M, D: Integer); override;
    function ChannelRect: TRect;
  public
    constructor Create(AOwner: TComponent); override;
    procedure AfterConstruction; override;
    procedure CreateWnd; override;
    destructor Destroy; override;
    procedure Loaded; override;
    procedure InvalidateNow;
  published
    property Align;
    property Anchors;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ParentCtl3D;
    property ParentShowHint;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnContextPopup;
    property OnDragDrop;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
{$ENDIF} // NOTFORHELP
    property Max:           integer index 3 read FMax           write SetInteger default 10;
    property Min:           integer index 2 read FMin           write SetInteger default 0;
    property Frequency:     integer index 4 read FFrequency     write SetInteger default 1;
    property ChannelMargin: integer index 5 read FChannelMargin write SetInteger default 8;

    property Position2: integer index 1 read FPosition2 write SetPosition default 10;
    property Position1: integer index 0 read FPosition1 write SetPosition default 0;

    property Orientation: TTrackBarOrientation read FOrientation write SetOrientation default trHorizontal;
    property ChangingMode: TacChangingMode read FChangingMode write FChangingMode default cmDepended;

    property ShowFocus: boolean index 1 read FShowFocus write SetBoolean default False;
    property ShowRange: boolean index 2 read FShowRange write SetBoolean default True;
    property SkinData: TsCommonData read FCommonData write FCommonData;

    property TickMarks: TTickMark read FTickMarks write SetTickMarks default tmBottomRight;
    property TickStyle: TTickStyle read FTickStyle write SetTickStyle default tsAuto;
  end;


implementation

uses
  math,
  {$IFDEF DELPHI7UP}Themes, {$ENDIF}
  sMaskData, sSkinProps, sAlphaGraph, sVCLUtils, sMessages, sSkinManager, sStyleSimply;

const
  iThumbSize = 23;
{$IFNDEF D2007}
  TBS_DOWNISLEFT = 1024;
  TBS_REVERSED   = 512;
{$ENDIF}

{$IFDEF DELPHI7UP}
  ThumbStyles: array [boolean, TTickMark, 0..2] of TThemedTrackBar =
   (((ttbThumbBottomNormal, ttbThumbBottomHot, ttbThumbBottomPressed),
     (ttbThumbTopNormal,    ttbThumbTopHot,    ttbThumbTopPressed),
     (ttbThumbNormal,       ttbThumbHot,       ttbThumbPressed)),

    ((ttbThumbRightNormal, ttbThumbRightHot, ttbThumbRightPressed),
     (ttbThumbLeftNormal,  ttbThumbLeftHot,  ttbThumbLeftPressed),
     (ttbThumbVertNormal,  ttbThumbVertHot,  ttbThumbVertPressed)));
{$ENDIF}

constructor TsTrackBar.Create(AOwner: TComponent);
begin
  FCommonData := TsCommonData.Create(Self, True);
  FCommonData.COC := COC_TsTrackBar;
  inherited Create(AOwner);

  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;

  Thumb := TBitmap.Create;
  Thumb.PixelFormat := pf32Bit;

  FThumbGlyph := TBitmap.Create;

  ControlStyle := ControlStyle - [csOpaque];
  EventFlag := False;

  TickHeight := 4;
  ThumbLength := iThumbSize;
  FShowProgressFrom := 0;
  FBarOffset := MkPoint;

  FDisabledKind := DefDisabledKind;
  FAnimatEvents := [aeGlobalDef];
  FShowProgress := False;
  FReversed := False;
end;


destructor TsTrackBar.Destroy;
begin
  FreeAndNil(Thumb);
  FreeAndNil(FCanvas);
  FreeAndNil(FThumbGlyph);
  FreeAndNil(FCommonData);
  inherited Destroy;
end;


function TsTrackBar.GetHBarOffset: Integer;
begin
  Result := FBarOffset.X;
end;


function TsTrackBar.GetVBarOffset: Integer;
begin
  Result := FBarOffset.Y;
end;


procedure TsTrackBar.WMMouseMsg(var Message: TWMMouse);
var
  State: TKeyboardState;
begin
  if ActReversed then
    if Orientation = trVertical then
      Message.YPos := Height - Message.YPos
    else
      Message.XPos := Width - Message.XPos;

  DefaultHandler(Message);
  GetKeyboardState(State);
  case Message.Msg of
    WM_LBUTTONUP:     if Assigned(OnMouseUp)   then OnMouseUp  (Self, mbLeft, GetShiftState, acMousePos.X, acMousePos.Y);
    WM_LBUTTONDOWN:   if Assigned(OnMouseDown) then OnMouseDown(Self, mbLeft, GetShiftState, acMousePos.X, acMousePos.Y);
  end;
end;


procedure TsTrackBar.WMNCHitTest(var Message: TWMNCHitTest);
var
  R: TRect;
begin
  GetWindowRect(Handle, R);
  if Orientation = trVertical then
    Message.YPos := Height - (Message.YPos - R.Top) + R.Top
  else
    Message.XPos := Width - (Message.XPos - R.Left) + R.Left;
end;


procedure TsTrackBar.WndProc(var Message: TMessage);
var
  PS: TPaintStruct;
  DC, SavedDC: hdc;
{$IFDEF DELPHI7UP}
  ParentForm: TCustomForm;
{$ENDIF}
begin
{$IFDEF LOGGED}
  AddToLog(Message);
{$ENDIF}
  case Message.Msg of
    SM_ALPHACMD:
      case Message.WParamHi of
        AC_CTRLHANDLED: begin
          Message.Result := 1;
          Exit;
        end; // AlphaSkins supported

        AC_REMOVESKIN:
          if ACUInt(Message.LParam) = ACUInt(SkinData.SkinManager) then begin
            CommonMessage(Message, FCommonData);
            RecreateWnd;
            Exit;
          end;

        AC_SETNEWSKIN:
          if ACUInt(Message.LParam) = ACUInt(SkinData.SkinManager) then begin
            CommonMessage(Message, FCommonData);
            UpdateIndexes(SkinData.SkinIndex);
            Exit;
          end;

        AC_REFRESH:
          if ACUInt(Message.LParam) = ACUInt(SkinData.SkinManager) then begin
            CommonMessage(Message, FCommonData);
            UpdateIndexes(SkinData.SkinIndex);
            Repaint;
            Exit;
          end;

        AC_PREPARECACHE: begin
          PrepareCache;
          Exit;
        end;

        AC_DRAWANIMAGE: begin
          Message.Result := 0;
          if Message.LParam <> 0 then
            try
              DC := GetWindowDC(Handle);
              SavedDC := SaveDC(DC);
              try
                BitBlt(DC, BorderWidth + FBarOffset.X, BorderWidth + FBarOffset.Y, Width, Height, TBitmap(Message.LParam).Canvas.Handle, 0, 0, SRCCOPY);
              finally
                RestoreDC(DC, SavedDC);
                ReleaseDC(Handle, DC);
              end;
            finally
              Message.Result := 1;
            end;

          Exit;
        end;

        AC_GETDEFINDEX: begin
          if FCommonData.SkinManager <> nil then
            Message.Result := FCommonData.SkinManager.ConstData.Sections[ssTrackBar] + 1
          else
            Message.Result := 0;

          UpdateIndexes(Message.Result - 1);
          Exit;
        end;

        AC_ENDPARENTUPDATE: begin
          if FCommonData.Updating then begin
            FCommonData.Updating := False;
            Repaint;
          end;
          Exit;
        end;

        AC_GETSKINDATA: begin
          Message.Result := LRESULT(SkinData);
          Exit;
        end;
      end;

      CM_MOUSEENTER:
        if not (csDesigning in ComponentState) and not (csLButtonDown in ControlState) then begin
{$IFDEF DELPHI7UP}
          ParentForm := GetParentForm(Self);
          if (ParentForm = nil) or not TForm(ParentForm).TransparentColor then
{$ENDIF}
          begin
            FCommonData.FMouseAbove := True;
            FCommonData.BGChanged := False;
            if FCommonData.SkinIndex >= 0 then
              DoChangePaint(FCommonData, 1, UpdateWindow_CB, EventEnabled(aeMouseEnter, FAnimatEvents), False)
            else
              RedrawWindow(Handle, nil, 0, RDW_INVALIDATE or RDW_NOERASE or RDW_UPDATENOW);
          end;
        end;

      WM_NCCALCSIZE: begin
        inherited;
        with TNCCalcSizeParams(Pointer(Message.LParam)^).rgrc[0] do begin
          Top := Top + FBarOffset.Y;
          Left := Left + FBarOffset.X;
        end;
        Exit;
      end;

      CM_MOUSELEAVE:
        if not (csDesigning in ComponentState) and not (csLButtonDown in ControlState) then begin
{$IFDEF DELPHI7UP}
          ParentForm := GetParentForm(Self);
          if (ParentForm = nil) or not TForm(ParentForm).TransparentColor then
{$ENDIF}
          begin
            FCommonData.FMouseAbove := False;
            FCommonData.BGChanged := False;
            if FCommonData.SkinIndex >= 0 then
              DoChangePaint(FCommonData, 0, UpdateWindow_CB, EventEnabled(aeMouseLeave, FAnimatEvents), False)
            else
              RedrawWindow(Handle, nil, 0, RDW_INVALIDATE or RDW_NOERASE or RDW_UPDATENOW);
          end;
        end;

      WM_NCHITTEST:
        if not (csDesigning in ComponentState) and ActReversed then begin
          WMNCHitTest(TWMNCHitTest(Message));
          Message.Result := 1;
          Exit;
        end;

      WM_MOUSEMOVE:
        if not (csDesigning in ComponentState) and ActReversed then
          WMMouseMsg(TWMMouse(Message));

      WM_LBUTTONUP, WM_LBUTTONDBLCLK, WM_LBUTTONDOWN:
        if not (csDesigning in ComponentState) and ActReversed then begin
          WMMouseMsg(TWMMouse(Message));
          Exit;
        end;

      TBM_GETTHUMBRECT: begin
        TBMGetThumbRect(Message);
        Exit;
      end;

      WM_PAINT: if not SkinData.Skinned then begin
        ControlState := ControlState + [csCustomPaint];
        inherited;
        ControlState := ControlState - [csCustomPaint];
        Exit;
      end;

{$IFDEF DELPHI_XE4}
      WM_ERASEBKGND:
        if csDesigning in ComponentState then begin
          FCanvas.Lock;
          if TWMPaint(Message).DC <> 0 then
            FCanvas.Handle := TWMPaint(Message).DC
          else
            FCanvas.Handle := GetWindowDC(Handle);

          try
            TControlCanvas(FCanvas).UpdateTextFlags;
            Paint;
          finally
            if TWMPaint(Message).DC = 0 then
              ReleaseDC(Handle, FCanvas.Handle);

            FCanvas.Handle := 0;
            FCanvas.Unlock;
          end;
          Exit;
        end;
{$ENDIF}        
  end;

  if not ControlIsReady(Self) or not FCommonData.Skinned(True) then
    inherited
  else begin
    case Message.Msg of
      WM_PRINT: begin
        SkinData.FUpdating := False;
        PaintWindow(TWMPaint(Message).DC);
        Exit;
      end;

      WM_PAINT: begin
        if TimerIsActive(SkinData) then begin
          BeginPaint(Handle, PS);
          EndPaint(Handle, PS);
          Exit;
        end;
        ControlState := ControlState + [csCustomPaint];
      end;

      WM_ERASEBKGND:
        Exit;

      WM_SETFOCUS, CM_ENTER:
        if not (csDesigning in ComponentState) then begin
          inherited;
          if Enabled and not TimerIsActive(SkinData) then
            Repaint;

          Exit;
        end;

      WM_KILLFOCUS, CM_EXIT:
        if not (csDesigning in ComponentState) then begin
          inherited;
          if Enabled then begin
            StopTimer(SkinData);
            Exit
          end;
        end;

      WM_SIZE:
        StopTimer(SkinData);

      WM_LBUTTONUP:
        if not (csDesigning in ComponentState) and Enabled then begin
          Application.ShowHint := AppShowHint;
          ShowHintStored := False;
          if PtInRect(ThumbRect, SmallPointToPoint(TWMMouse(Message).Pos)) then begin
            ControlState := ControlState - [csLButtonDown];
            DoChangePaint(FCommonData, 1, UpdateWindow_CB, EventEnabled(aeMouseUp, FAnimatEvents), True);
          end
          else
            StopTimer(SkinData);
        end;

      WM_LBUTTONDBLCLK, WM_LBUTTONDOWN:
        if not (csDesigning in ComponentState) and Enabled then begin
          if not ShowHintStored then begin
            AppShowHint := Application.ShowHint;
            Application.ShowHint := False;
            ShowHintStored := True;
          end;
          if PtInRect(ThumbRect, SmallPointToPoint(TWMMouse(Message).Pos)) then begin
            ControlState := ControlState + [csLButtonDown];
            Skindata.BGChanged := False;
            DoChangePaint(FCommonData, 2, UpdateWindow_CB, EventEnabled(aeMouseDown, FAnimatEvents), True);
          end
          else
            StopTimer(SkinData);
        end;

      CN_HSCROLL, CN_VSCROLL: begin
        StopTimer(SkinData);
        PaintWindow(0);
      end;
    end;
    CommonWndProc(Message, FCommonData);
    inherited;
    case Message.Msg of
      TB_INDETERMINATE: {if not ThumbGlyph.Empty then }
        Repaint; // Full control repainting

      WM_MOVE:
        if csDesigning in ComponentState then
          Repaint;

      WM_PAINT:
        ControlState := ControlState - [csCustomPaint];
    end;
  end;
  case Message.Msg of
    CN_HSCROLL, CN_VSCROLL:
      if not EventFlag then begin
        EventFlag := True;
        UserChanged(TWMScroll(Message).ScrollCode in [TB_THUMBPOSITION, TB_LINEUP, TB_LINEDOWN, TB_PAGEUP, TB_PAGEDOWN]);
        EventFlag := False;
      end;
  end;
end;


procedure TsTrackBar.ChangeScale(M, D: Integer);
begin
  inherited;
{$IFNDEF DELPHI_10}
  ThumbLength := MulDiv(ThumbLength, M, D);
{$ENDIF}
end;


procedure TsTrackBar.PaintBody;
var
  R: TRect;
  fColor: TColor;
begin
  R := MkRect(Self);
  if SkinData.Skinned then begin
    PaintItem(FCommonData, GetParentCache(FCommonData), True, integer(ControlIsActive(FCommonData)), R, Point(Left, Top), FCommonData.FCacheBmp, False);
    if SkinData.SkinIndex >= 0 then begin
      PaintBar;
      if Assigned(FOnSkinPaint) then
        FOnSkinPaint(Self, FCommonData.FCacheBMP.Canvas);

      PaintThumb;
    end;
    FColor := SkinData.SkinManager.gd[SkinData.SkinIndex].Props[1].FontColor.Color;
  end
  else begin
    StdPaintBG(FCommonData.FCacheBmp);
    if Assigned(FOnSkinPaint) then
      FOnSkinPaint(Self, FCommonData.FCacheBMP.Canvas);

    StdPaintBar(FCommonData.FCacheBmp);
    StdPaintThumb;
    FColor := clBtnText;
  end;
  if FShowFocus and Focused then begin
    InflateRect(R, 0, -1);
    FocusRect(SkinData.FCacheBMP.Canvas, R, FColor, clNone)
  end;
end;


procedure TsTrackBar.PaintBar;
var
  w, h, i, j, d, pos: integer;
  aRect, sRect: TRect;
  CI: TCacheInfo;
begin
  aRect := ChannelRect;
  if SkinData.SkinManager.IsValidImgIndex(FSliderNdx) then begin
    CI := MakeCacheInfo(FCommonData.FCacheBmp);
    pos := SendMessage(Handle, TBM_GETPOS, 0, 0);
    case Orientation of
      trHorizontal: begin
        h := SkinData.SkinManager.ma[FSliderNdx].Height - 1;
        w := HeightOf(aRect);
        aRect.Top := aRect.Top + (w - h) div 2;
        aRect.Bottom := aRect.Top + h;
        InflateRect(aRect, -1, 0);
        DrawSkinRect(FCommonData.FCacheBmp, aRect, CI, SkinData.SkinManager.ma[FSliderNdx], integer(ControlIsActive(FCommonData)), True);
        if ShowProgress then begin
          sRect := aRect;
          d := math.Max(0, ShowProgressFrom - Min);
          if Max = Min then begin
            i := pos - Min;
            j := d;
          end
          else begin
            i := Round(WidthOf(aRect) * (pos - Min) / (Max - Min));
            j := Round(WidthOf(aRect) * d / (Max - Min));
          end;
          if ActReversed then
            if (pos < ShowProgressFrom) then begin
              sRect.Left := sRect.Right - j;
              sRect.Right := sRect.Right - i;
            end
            else begin
              sRect.Left := sRect.Right - i;
              sRect.Right := sRect.Right - j;
            end
          else
            if pos < ShowProgressFrom then begin
              sRect.Right := sRect.Left + j;
              sRect.Left := sRect.Left + i;
            end
            else begin
              sRect.Right := sRect.Left + i;
              sRect.Left := sRect.Left + j;
            end;

          PaintProgress(sRect, True);
        end;
      end;

      trVertical: begin
        h := SkinData.SkinManager.ma[FSliderNdx].Width - 1;
        w := WidthOf(aRect);
        aRect.Left := aRect.Left + (w - h) div 2;
        aRect.Right := aRect.Left + h;
        InflateRect(aRect, 0, -1);
        DrawSkinRect(FCommonData.FCacheBmp, aRect, CI, SkinData.SkinManager.ma[FSliderNdx], integer(ControlIsActive(FCommonData)), True);
        if ShowProgress then begin
          sRect := aRect;
          d := math.Max(0, ShowProgressFrom - Min);
          if Max = Min then begin
            i := pos - Min;
            j := d;
          end
          else begin
            i := Round(HeightOf(aRect) * (pos - Min) / (Max - Min));
            j := Round(HeightOf(aRect) * d / (Max - Min));
          end;
          if ActReversed then begin
            if pos < ShowProgressFrom then begin
              sRect.Top    := sRect.Bottom - j;
              sRect.Bottom := sRect.Bottom - i;
            end
            else begin
              sRect.Top    := sRect.Bottom - i;
              sRect.Bottom := sRect.Bottom - j;
            end;
          end
          else
            if (pos < ShowProgressFrom) then begin
              sRect.Bottom := sRect.Top + j;
              sRect.Top    := sRect.Top + i;
            end
            else begin
              sRect.Bottom := sRect.Top + i;
              sRect.Top    := sRect.Top + j;
            end;

          PaintProgress(sRect, False);
        end;
      end;
    end;
  end;
  if Orientation = trHorizontal then
    PaintTicksHor
  else
    PaintTicksVer;
end;


const
  SelSize = 3;


procedure TsTrackBar.PaintTicksHor;
var
  dw: real;
  cr: TRect;
  pa: TAPoint;
  ArrowPoints: array of TPoint;
  i, mh, sStart, sEnd: integer;
begin
  pa := nil;
  mh := 0;
  if TickStyle <> tsNone then begin
    cr := ChannelRect;
    pa := TicksArray(cr);
    mh := (HeightOf(ThumbRect) - HeightOf(cr)) div 2 + 2;
    if TickMarks in [tmTopLeft, tmBoth] then
      for i := 0 to High(pa) do
        if ((SelStart = 0) and (SelEnd = 0)) or ((i <> SelStart) and (i <> SelEnd)) then
          PaintTick(Point(pa[i].x, cr.Top - mh - TickHeight), True);

    if TickMarks in [tmBottomRight, tmBoth] then
      for i := 0 to High(pa) do
        if ((SelStart = 0) and (SelEnd = 0)) or ((i <> SelStart) and (i <> SelEnd)) then
          PaintTick(Point(pa[i].x, cr.Bottom + mh), True);
  end;
  if (SelStart <> 0) or (SelEnd <> 0) then begin
    sStart := math.max(SelStart, Min);
    sEnd := math.min(SelEnd, Max);
    dw := (WidthOf(ChannelRect) - WidthOf(ThumbRect)) / (Max - Min);
    SetLength(ArrowPoints, 3);
    FCommonData.FCacheBmp.Canvas.Brush.Style := bsSolid;
    FCommonData.FCacheBmp.Canvas.Brush.Color := FCommonData.SkinManager.GetGlobalFontColor;
    FCommonData.FCacheBmp.Canvas.Pen.Color := FCommonData.SkinManager.GetGlobalFontColor;
    if TickMarks in [tmTopLeft, tmBoth] then begin
      // SelStart
      i := Round(dw * (sStart + 1)) + ChannelRect.Left + 4;
      ArrowPoints[0] := Point(i, cr.Top - mh - TickHeight);
      ArrowPoints[1] := Point(ArrowPoints[0].X, ArrowPoints[0].Y - SelSize);
      ArrowPoints[2] := Point(ArrowPoints[0].X - SelSize, ArrowPoints[0].Y - SelSize);
      FCommonData.FCacheBmp.Canvas.Polygon(ArrowPoints);
      // SelEnd
      i := Round(dw * (sEnd + 1)) + ChannelRect.Left + 4;
      ArrowPoints[0] := Point(i, cr.Top - mh - TickHeight);
      ArrowPoints[1] := Point(ArrowPoints[0].X, ArrowPoints[0].Y - SelSize);
      ArrowPoints[2] := Point(ArrowPoints[0].X + SelSize, ArrowPoints[0].Y - SelSize);
      FCommonData.FCacheBmp.Canvas.Polygon(ArrowPoints);
    end;
    if TickMarks in [tmBottomRight, tmBoth] then begin
      // SelStart
      i := Round(dw * (sStart + 1)) + ChannelRect.Left + 4;
      ArrowPoints[0] := Point(i, cr.Bottom + mh);
      ArrowPoints[1] := Point(ArrowPoints[0].X, ArrowPoints[0].Y + SelSize);
      ArrowPoints[2] := Point(ArrowPoints[0].X - SelSize, ArrowPoints[0].Y + SelSize);
      FCommonData.FCacheBmp.Canvas.Polygon(ArrowPoints);
      // SelEnd
      i := Round(dw * (sEnd + 1)) + ChannelRect.Left + 4;
      ArrowPoints[0] := Point(i, cr.Bottom + mh);
      ArrowPoints[1] := Point(ArrowPoints[0].X, ArrowPoints[0].Y + SelSize);
      ArrowPoints[2] := Point(ArrowPoints[0].X + SelSize, ArrowPoints[0].Y + SelSize);
      FCommonData.FCacheBmp.Canvas.Polygon(ArrowPoints);
    end
  end
end;


procedure RotateBmp180(Bmp: TBitmap; Horz: boolean);
var
  x, y, bSize, bSizeD2: integer;
  c: TColor;
begin
  if not Horz then begin
    bSize := Bmp.Height - 1;
    bSizeD2 := bSize div 2;
    for x := 0 to Bmp.Width - 1 do
      for y := 0 to bSizeD2 do begin
        c := Bmp.Canvas.Pixels[x, y];
        Bmp.Canvas.Pixels[x, y] := Bmp.Canvas.Pixels[x, bSize - y];
        Bmp.Canvas.Pixels[x, bSize - y] := c
      end
  end
  else begin
    bSize := Bmp.Width - 1;
    bSizeD2 := bSize div 2;
    for y := 0 to Bmp.Height - 1 do
      for x := 0 to bSizeD2 do begin
        c := Bmp.Canvas.Pixels[x, y];
        Bmp.Canvas.Pixels[x, y] := Bmp.Canvas.Pixels[bSize - x, y];
        Bmp.Canvas.Pixels[bSize - x, y] := c
      end;
  end;
end;


procedure TsTrackBar.PaintThumb;
var
  Bmp: TBitmap;
  GlyphSize: TSize;
  DrawPoint: TPoint;
  Stretched: boolean;
  aRect, DrawRect: TRect;

  procedure PaintGlyph(R: TRect);
  var
    b: boolean;
    S0, S: PRGBAArray_S;
    bWidth, Y, X, DeltaS: integer;
  begin
    if ThumbGlyph.PixelFormat = pfDevice then begin
      ThumbGlyph.HandleType := bmDIB;
      if (ThumbGlyph.Handle <> 0) and (ThumbGlyph.PixelFormat = pf32bit) then begin // Checking for an empty alpha-channel
        b := False;
        bWidth := ThumbGlyph.Width - 1;
        if InitLine(ThumbGlyph, Pointer(S0), DeltaS) then
          for Y := 0 to ThumbGlyph.Height - 1 do begin
            S := Pointer(PAnsiChar(S0) + DeltaS * Y);
            for X := 0 to bWidth do
              if S[X].SA = MaxByte then begin
                b := True;
                Break;
              end;

            if b then
              Break;
          end;
          
        if not b then
          ThumbGlyph.PixelFormat := pf24bit;
      end;
    end;
    if ThumbGlyph.PixelFormat = pf32bit then // Patch if Png, doesn't work in std. mode
      CopyBmp32(R, MkRect(ThumbGlyph), FCommonData.FCacheBmp, ThumbGlyph, EmptyCI, False, clNone, 0, False)
    else
      BitBlt(FCommonData.FCacheBmp.Canvas.Handle, R.Left, R.Top, ThumbGlyph.Width, ThumbGlyph.Height, ThumbGlyph.Canvas.Handle, 0, 0, SRCCOPY);
  end;

  function PrepareBG: TRect;
  var
    TmpBmp: TBitmap;
  begin
    if Stretched or (TickMarks = tmTopLeft) then begin
      Bmp := CreateBmp32(GlyphSize);
      Result := MkRect(GlyphSize);
      TmpBmp := CreateBmp32(aRect);
      BitBlt(TmpBmp.Canvas.Handle, 0, 0, TmpBmp.Width, TmpBmp.Height, FCommonData.FCacheBmp.Canvas.Handle, aRect.Left, aRect.Top, SRCCOPY);
      Stretch(TmpBmp, Bmp, Bmp.Width, Bmp.Height, ftMitchell);
      if TickMarks = tmTopLeft then
        RotateBmp180(Bmp, Orientation <> trHorizontal);
        
      FreeAndNil(TmpBmp);
    end
    else begin
      Bmp := FCommonData.FCacheBmp;
      Result := aRect;
    end;
  end;

  procedure ReturnToCache;
  var
    TmpBmp: TBitmap;
  begin
    if FCommonData.FCacheBmp <> Bmp then begin
      if TickMarks = tmTopLeft then
        RotateBmp180(Bmp, Orientation <> trHorizontal);

      TmpBmp := CreateBmp32(aRect);
      Stretch(Bmp, TmpBmp, TmpBmp.Width, TmpBmp.Height, ftMitchell);
      BitBlt(FCommonData.FCacheBmp.Canvas.Handle, aRect.Left, aRect.Top, WidthOf(aRect), HeightOf(aRect), TmpBmp.Canvas.Handle, 0, 0, SRCCOPY);
      FreeAndNil(TmpBmp);
    end
  end;

begin
{$IFDEF DELPHI7UP}
  if SliderVisible then
{$ENDIF}
  begin
    aRect := ThumbRect;
    if ThumbGlyph.Empty then begin
      with SkinData.SkinManager do
        if IsValidImgIndex(FThumbNdx) then begin
          GlyphSize := MkSize(ma[FThumbNdx]);
          Stretched := False;

          DrawRect := PrepareBG;
          DrawPoint := Point(DrawRect.Left + (WidthOf(DrawRect) - GlyphSize.cx) div 2, DrawRect.Top + (HeightOf(DrawRect) - GlyphSize.cy) div 2);
          DrawSkinGlyph(Bmp, DrawPoint, Mode, 1, ma[FThumbNdx], MakeCacheInfo(Bmp));
          ReturnToCache;
          if Bmp <> FCommonData.FCacheBmp then
            FreeAndNil(Bmp);
        end;
    end
    else begin
      DrawRect.Left   := aRect.Left    + (WidthOf (aRect) - ThumbGlyph.Width)  div 2;
      DrawRect.Top    := aRect.Top     + (HeightOf(aRect) - ThumbGlyph.Height) div 2;
      DrawRect.Right  := DrawRect.Left + ThumbGlyph.Width;
      DrawRect.Bottom := DrawRect.Top  + ThumbGlyph.Height;
      PaintGlyph(DrawRect);
    end;
  end;
end;


procedure TsTrackBar.TBMGetThumbRect(var Message: TMessage);
var
  pR: PRect;
  M: TMessage;
  Size: integer;
begin
  M := Message;
  DefaultHandler(M);
  if ActReversed then begin
    pR := Pointer(Message.LParam);
    if Orientation = trVertical then begin
      Size := HeightOf(pR^);
      pR^.Top := Height - Size - pR^.Top;
      pR^.Bottom := pR^.Top + Size;
    end
    else begin
      Size := WidthOf(pR^);
      pR^.Left := Width - Size - pR^.Left;
      pR^.Right := pR^.Left + Size;
    end;
  end;
end;


function TsTrackBar.ThumbRect: TRect;
begin
  Result := MkRect(1, 1);
  SendMessage(Handle, TBM_GETTHUMBRECT, 0, LPARAM(@Result));
  OffsetRect(Result, FBarOffset.X, FBarOffset.Y);
end;


function TsTrackBar.ChannelRect: TRect;
begin
  Result := MkRect(1, 1);
  SendMessage(Handle, TBM_GETCHANNELRECT, 0, LPARAM(@Result));
  if Orientation = trVertical then begin
    Changei(Result.Left, Result.Top);
    Changei(Result.Right, Result.Bottom);
  end;
  OffsetRect(Result, FBarOffset.X, FBarOffset.Y);
end;


function TsTrackBar.TickPos(i: integer): integer;
var
  Value: ACNativeInt;
begin
  Value := ACNativeInt(i);
  Result := SendMessage(Handle, TBM_GETTICPOS, Value, 0);
end;


function TsTrackBar.TickCount: integer;
begin
  Result := SendMessage(Handle, TBM_GETNUMTICS, 0, 0);
end;


function TsTrackBar.TicksArray(ChRect: TRect): TAPoint;
var
  i, w, c: integer;
  ThRect: TRect;
begin
  Result := nil;
  ThRect := ThumbRect;
  c := TickCount;
  SetLength(Result, c);
  if Orientation = trVertical then begin
    if SkinData.Skinned and (FTickNdx >= 0) then
      OffsetRect(ChRect, 0, SkinData.SkinManager.ma[FTickNdx].Height div 2 + 1)
    else
      OffsetRect(ChRect, 0, 2);

    w := HeightOf(ThRect) div 2;
    if TickStyle = tsAuto then begin
      iStep := (HeightOf(ChRect) - HeightOf(ThRect)) / (TickCount - 1);
      for i := 0 to c - 1 do
        Result[i] := Point(0, Round(ChRect.Top + i * iStep + w));
    end
    else begin
      Result[0] := Point(0, ChRect.Top + w);
      for i := 0 to c - 3 do
        Result[i + 1] := Point(0, TickPos(i));

      Result[c - 1] := Point(0, ChRect.Bottom - w);
    end
  end
  else begin
    if SkinData.Skinned and (FTickNdx >= 0) then
      OffsetRect(ChRect, SkinData.SkinManager.ma[FTickNdx].Width div 2 + 1, 0)
    else
      OffsetRect(ChRect, 2, 0);

    w := WidthOf(ThRect) div 2;
    if TickStyle = tsAuto then begin
      iStep := (WidthOf(ChRect) - WidthOf(ThRect)) / (TickCount - 1);
      for i := 0 to c - 1 do
        Result[i] := Point(Round(ChRect.Left + i * iStep + w), 0);
    end
    else begin
      Result[0] := Point(ChRect.Left + w, 0);
      for i := 0 to c - 3 do
        Result[i + 1] := Point(TickPos(i), 0);

      Result[c - 1] := Point(ChRect.Right - w, 0);
    end;
  end;
end;


procedure TsTrackBar.PaintTicksVer;
var
  dh: real;
  cr: TRect;
  pa: TAPoint;
  ArrowPoints: array of TPoint;
  i, mh, sStart, sEnd: integer;
begin
  mh := 0;
  if TickStyle <> tsNone then begin
    cr := ChannelRect;
    pa := TicksArray(cr);
    mh := (WidthOf(ThumbRect) - WidthOf(cr)) div 2 + 2;
    if TickMarks in [tmTopLeft, tmBoth] then
      for i := 0 to High(pa) do
        if ((SelStart = 0) and (SelEnd = 0)) or ((i <> SelStart) and (i <> SelEnd)) then
          PaintTick(Point(cr.Left - mh - TickHeight, pa[i].y), False);

    if TickMarks in [tmBottomRight, tmBoth] then
      for i := 0 to High(pa) do
        if ((SelStart = 0) and (SelEnd = 0)) or ((i <> SelStart) and (i <> SelEnd)) then
          PaintTick(Point(cr.Right + mh, pa[i].y), False);
  end
  else
    pa := nil;

  if (SelStart > 0) or (SelEnd > 0) then
    with FCommonData.FCacheBmp.Canvas do begin
      sStart := math.max(SelStart, Min);
      sEnd := math.min(SelEnd, Max);
      dh := (HeightOf(ChannelRect) - HeightOf(ThumbRect)) / (Max - Min);
      SetLength(ArrowPoints, 3);
      Brush.Style := bsSolid;
      Brush.Color := FCommonData.SkinManager.GetGlobalFontColor;
      Pen.Color := FCommonData.SkinManager.GetGlobalFontColor;
      if TickMarks in [tmTopLeft, tmBoth] then begin
        // SelStart
        i := Round(dh * (sStart + 1)) + ChannelRect.Top + 4;
        ArrowPoints[0] := Point(cr.Left - mh - TickHeight, i);
        ArrowPoints[1] := Point(ArrowPoints[0].X - SelSize, ArrowPoints[0].Y - SelSize);
        ArrowPoints[2] := Point(ArrowPoints[0].X - SelSize, ArrowPoints[0].Y);
        Polygon(ArrowPoints);
        // SelEnd
        i := Round(dh * (sEnd + 1)) + ChannelRect.Top + 4;
        ArrowPoints[0] := Point(cr.Left - mh - TickHeight, i);
        ArrowPoints[1] := Point(ArrowPoints[0].X - SelSize, ArrowPoints[0].Y);
        ArrowPoints[2] := Point(ArrowPoints[0].X - SelSize, ArrowPoints[0].Y + SelSize);
        Polygon(ArrowPoints);
      end;
      if TickMarks in [tmBottomRight, tmBoth] then begin
        // SelStart
        i := Round(dh * (sStart + 1)) + ChannelRect.Top + 4;
        ArrowPoints[0] := Point(cr.Right + mh, i);
        ArrowPoints[1] := Point(ArrowPoints[0].X + SelSize, ArrowPoints[0].Y - SelSize);
        ArrowPoints[2] := Point(ArrowPoints[0].X + SelSize, ArrowPoints[0].Y);
        Polygon(ArrowPoints);
        // SelEnd
        i := Round(dh * (sEnd + 1)) + ChannelRect.Top + 4;
        ArrowPoints[0] := Point(cr.Right + mh, i);
        ArrowPoints[1] := Point(ArrowPoints[0].X + SelSize, ArrowPoints[0].Y);
        ArrowPoints[2] := Point(ArrowPoints[0].X + SelSize, ArrowPoints[0].Y + SelSize);
        Polygon(ArrowPoints);
      end;
    end;
end;


procedure TsTrackBar.Paint;
begin
  if not (csDestroying in ComponentState) then
    if not FCommonData.Skinned or not (InUpdating(FCommonData) or TimerIsActive(SkinData)) then begin
      PrepareCache;
      if FCommonData.SkinIndex >= 0 then
        UpdateCorners(FCommonData, 0);

      BitBlt(Canvas.Handle, 0, 0, Width, Height, FCommonData.FCacheBmp.Canvas.Handle, 0, 0, SRCCOPY);
    end;
end;


function TsTrackBar.ActReversed: boolean;
begin
  if (BiDiMode = bdRightToLeft) and (Orientation = trHorizontal) then
    Result := not FReversed
  else
    Result := FReversed;
end;


procedure TsTrackBar.AfterConstruction;
begin
  inherited;
  FCommonData.Loaded;
  UpdateIndexes(SkinData.SkinIndex);
end;


procedure TsTrackBar.Loaded;
begin
  inherited;
  FCommonData.Loaded;
  UpdateIndexes(SkinData.SkinIndex);
end;


procedure TsTrackBar.SetBoolean(const Index: Integer; const Value: boolean);

  procedure ChangeProp(var Prop: boolean; Value: boolean);
  begin
    if Prop <> Value then begin
      Prop := Value;
      if (Index = 0) and HandleAllocated then
        if Value then
          SetWindowLong(Handle, GWL_STYLE, GetWindowLong(Handle, GWL_STYLE) or TBS_DOWNISLEFT or TBS_REVERSED)
        else
          SetWindowLong(Handle, GWL_STYLE, GetWindowLong(Handle, GWL_STYLE) and not TBS_DOWNISLEFT and not TBS_REVERSED);

      FCommonData.Invalidate;
    end;
  end;

begin
  case Index of
    0: ChangeProp(FReversed, Value);
    1: ChangeProp(FShowFocus, Value);
    2: ChangeProp(FShowProgress, Value);
  end;
end;


procedure TsTrackBar.SetDisabledKind(const Value: TsDisabledKind);
begin
  if FDisabledKind <> Value then begin
    FDisabledKind := Value;
    FCommonData.Invalidate;
  end;
end;


procedure TsTrackBar.SetHBarOffset(const Value: Integer);
begin
  if FBarOffset.X <> Value then begin
    FBarOffset.X := Value;
    Repaint;
  end;
end;


procedure TsTrackBar.SetVBarOffset(const Value: Integer);
begin
  if FBarOffset.Y <> Value then begin
    FBarOffset.Y := Value;
    Repaint;
  end;
end;


procedure TsTrackBar.StdPaintBar(Bmp: TBitmap);
var
  i, j, d, pos: integer;
  aRect, sRect: TRect;
{$IFDEF DELPHI7UP}
  Details: TThemedElementDetails;
  te: TThemedTrackBar;
{$ENDIF}
begin
  aRect := ChannelRect;
{$IFDEF DELPHI7UP}
  if acThemesEnabled then begin
    if Orientation = trVertical then
      te := ttbTrack
    else
      te := ttbTrackVert;

    Details := acThemeServices.GetElementDetails(te);
    acThemeServices.DrawElement(Bmp.Canvas.Handle, Details, aRect);
    InflateRect(aRect, -1, -1);
  end
  else
{$ENDIF}
  begin
    Frame3D(Bmp.Canvas, aRect, clBtnShadow, clBtnHighlight, 1);
    Frame3D(Bmp.Canvas, aRect, cl3DDkShadow, cl3DLight, 1);
    FillDC(Bmp.Canvas.Handle, aRect, clWindow);
  end;
  if ShowProgress then begin
    sRect := aRect;
    pos := SendMessage(Handle, TBM_GETPOS, 0, 0);
    d := math.Max(0, ShowProgressFrom - Min);
    if Orientation = trVertical then begin
      if Max = Min then begin
        i := pos - Min;
        j := d;
      end
      else begin
        i := Round(HeightOf(aRect) * (pos - Min) / (Max - Min));
        j := Round(HeightOf(aRect) * d / (Max - Min));
      end;
      if ActReversed then
        if pos < ShowProgressFrom then begin
          sRect.Top := sRect.Bottom - j;
          sRect.Bottom := sRect.Bottom - i;
        end
        else begin
          sRect.Top := sRect.Bottom - i;
          sRect.Bottom := sRect.Bottom - j;
        end
      else
        if pos < ShowProgressFrom then begin
          sRect.Bottom := sRect.Top + j;
          sRect.Top := sRect.Top + i;
        end
        else begin
          sRect.Bottom := sRect.Top + i;
          sRect.Top := sRect.Top + j;
        end;
    end
    else begin
      if Max = Min then begin
        i := pos - Min;
        j := d;
      end
      else begin
        i := Round(WidthOf(aRect) * (pos - Min) / (Max - Min));
        j := Round(WidthOf(aRect) * d / (Max - Min));
      end;
      if ActReversed then
        if pos < ShowProgressFrom then begin
          sRect.Left := sRect.Right - j;
          sRect.Right := sRect.Right - i;
        end
        else begin
          sRect.Left := sRect.Right - i;
          sRect.Right := sRect.Right - j;
        end
      else
        if pos < ShowProgressFrom then begin
          sRect.Right := sRect.Left + j;
          sRect.Left := sRect.Left + i;
        end
        else begin
          sRect.Right := sRect.Left + i;
          sRect.Left := sRect.Left + j;
        end;
    end;
    FillRect32(Bmp, sRect, ColorToRGB(clHighLight));
  end;
  if Orientation = trHorizontal then
    PaintTicksHor
  else
    PaintTicksVer;
end;


procedure TsTrackBar.StdPaintBG(Bmp: TBitmap);
{$IFDEF DELPHI7UP}
var
  Details: TThemedElementDetails;
{$ENDIF}
begin
{$IFDEF DELPHI7UP}
  if acThemesEnabled then begin
    FillDC(Bmp.Canvas.Handle, MkRect(Bmp), clBtnFace);
    Details := acThemeServices.GetElementDetails(ttBody);
    acThemeServices.DrawParentBackground(Handle, Bmp.Canvas.Handle, @Details, False);
  end
  else
{$ENDIF}
    FillDC(Bmp.Canvas.Handle, MkRect(Bmp), clBtnFace);
end;


procedure TsTrackBar.UpdateIndexes(MainNdx: integer);
var
  Horz: boolean;
begin
  Horz := Orientation = trHorizontal;
  if MainNdx >= 0 then
    with SkinData.SkinManager, ConstData, TrackBar[Horz] do
      if SkinIndex = MainNdx then begin
        FTickNdx     := TickIndex;
        FProgressNdx := ProgIndex;
        FSliderNdx   := SlideIndex;
        FThumbNdx    := GlyphIndex;
      end
      else begin
        FTickNdx     := GetMaskIndex(MainNdx, ThickArray[Horz]);
        FProgressNdx := GetMaskIndex(MainNdx, ProgArray[Horz]);
        FSliderNdx   := GetMaskIndex(MainNdx, s_SliderChannelMask);

        if not Horz then
          FThumbNdx := GetMaskIndex(MainNdx, s_SliderVertMask)
        else
          FThumbNdx := -1;

        if FThumbNdx < 0 then
          FThumbNdx := GetMaskIndex(MainNdx, s_SliderHorzMask);
      end
  else begin
    FTickNdx     := -1;
    FProgressNdx := -1;
    FSliderNdx   := -1;
    FThumbNdx    := -1;
  end;
end;


procedure TsTrackBar.UserChanged(Finished: boolean);
begin
  if Assigned(FOnUserChange) then
    FOnUserChange(Self);

  if Finished and Assigned(FOnUserChanged) then
    FOnUserChanged(Self);
end;


procedure TsTrackBar.PaintWindow(DC: HDC);
begin
  if not TimerIsActive(SkinData) then begin
    FCanvas.Lock;
    if SkinData.PrintDC <> 0 then
      FCanvas.Handle := SkinData.PrintDC
    else
      FCanvas.Handle := GetWindowDC(Handle);

    try
      TControlCanvas(FCanvas).UpdateTextFlags;
      Paint;
    finally
      if SkinData.PrintDC = 0 then
        ReleaseDC(Handle, FCanvas.Handle);

      FCanvas.Handle := 0;
      FCanvas.Unlock;
    end;
  end;
end;


procedure TsTrackBar.PrepareCache;
begin
  InitCacheBmp(SkinData);
  PaintBody;
  if (SkinData.SkinIndex >= 0) and not Enabled then
    BmpDisabledKind(FCommonData.FCacheBmp, FDisabledKind, Parent, GetParentCache(FCommonData), Point(Left, Top));
end;


procedure TsTrackBar.PaintTick(P: TPoint; Horz: boolean);
var
  w: integer;
  R: TRect;
begin
  if SkinData.SkinIndex >= 0 then
    with SkinData.SkinManager do begin
      if FTickNdx >= 0 then begin
        if Horz then
          dec(P.x, ma[FTickNdx].Width)
        else
          dec(P.y, ma[FTickNdx].Height);

        DrawSkinGlyph(SkinData.FCacheBmp, P, Mode, 1, ma[FTickNdx], MakeCacheInfo(FCommonData.FCacheBmp))
      end
      else begin
        if Horz then
          R := Rect(P.x, P.y, P.x + 2, P.Y + TickHeight)
        else
          R := Rect(P.x, P.y, P.x + TickHeight, P.Y + 2);

        w := 1;
        DrawRectangleOnDC(FCommonData.FCacheBmp.Canvas.Handle, R, ColorToRGB(clBtnShadow), ColorToRGB(clWhite), w);
      end;
    end
  else begin
    if Horz then
      R := Rect(P.x, P.y, P.x + 1, P.Y + 3)
    else
      R := Rect(P.x, P.y, P.x + 3, P.Y + 1);

    FillDC(FCommonData.FCacheBmp.Canvas.Handle, R, clBtnShadow);
  end;
end;


function TsTrackBar.Mode: integer;
begin
  if csLButtonDown in ControlState then
    Result := 2
  else
    Result := integer(ControlIsActive(FCommonData));
end;


procedure TsTrackBar.SetThumbGlyph(const Value: TBitmap);
begin
  FThumbGlyph.Assign(Value);
  if not (csLoading in ComponentState) and SkinData.Skinned then
    SkinData.Invalidate
end;


procedure TsTrackBar.PaintProgress(R: TRect; Horz: boolean);
begin
  with SkinData.SkinManager do
    if IsValidImgIndex(FProgressNdx) then
      DrawSkinRect(FCommonData.FCacheBmp, R, MakeCacheInfo(FCommonData.FCacheBmp), ma[FProgressNdx], integer(ControlIsActive(FCommonData)), True);
end;


procedure TsTrackBar.StdPaintThumb;
var
  Bmp: TBitmap;
  GlyphSize: TSize;
  DrawPoint: TPoint;
  Stretched: boolean;
  ActThumbSize: integer;
  aRect, DrawRect: TRect;
{$IFDEF DELPHI7UP}
  Details: TThemedElementDetails;
  te: TThemedTrackBar;
{$ENDIF}

  function PrepareBG: TRect;
  var
    TmpBmp: TBitmap;
  begin
    if Stretched or (TickMarks = tmTopLeft) then begin
      Bmp := CreateBmp32(GlyphSize);
      Result := MkRect(GlyphSize);
      TmpBmp := CreateBmp32(aRect);
      BitBlt(TmpBmp.Canvas.Handle, 0, 0, TmpBmp.Width, TmpBmp.Height, FCommonData.FCacheBmp.Canvas.Handle, aRect.Left, aRect.Top, SRCCOPY);
      Stretch(TmpBmp, Bmp, Bmp.Width, Bmp.Height, ftMitchell);
      if TickMarks = tmTopLeft then
        RotateBmp180(Bmp, Orientation <> trHorizontal);

      FreeAndNil(TmpBmp);
    end
    else begin
      Bmp := FCommonData.FCacheBmp;
      Result := aRect;
    end;
  end;

  procedure ReturnToCache;
  var
    TmpBmp: TBitmap;
  begin
    if FCommonData.FCacheBmp <> Bmp then begin
      if TickMarks = tmTopLeft then
        RotateBmp180(Bmp, Orientation <> trHorizontal);

      TmpBmp := CreateBmp32(aRect);
      Stretch(Bmp, TmpBmp, TmpBmp.Width, TmpBmp.Height, ftMitchell);
      BitBlt(FCommonData.FCacheBmp.Canvas.Handle, aRect.Left, aRect.Top, WidthOf(aRect), HeightOf(aRect), TmpBmp.Canvas.Handle, 0, 0, SRCCOPY);
      FreeAndNil(TmpBmp);
    end
  end;

  procedure PaintGlyph(R: TRect);
  var
    b: boolean;
    S0, S: PRGBAArray_S;
    Y, X, DeltaS: integer;
  begin
    if ThumbGlyph.PixelFormat = pfDevice then begin
      ThumbGlyph.HandleType := bmDIB;
      if (ThumbGlyph.Handle <> 0) and (ThumbGlyph.PixelFormat = pf32bit) then begin // Checking for an empty alpha-channel
        b := False;
        if InitLine(ThumbGlyph, Pointer(S0), DeltaS) then
          for Y := 0 to ThumbGlyph.Height - 1 do begin
            S := Pointer(PAnsiChar(S0) + DeltaS * Y);
            for X := 0 to ThumbGlyph.Width - 1 do
              if S[X].SA = MaxByte then begin
                b := True;
                Break;
              end;

            if b then
              Break;
          end;
          
        if not b then
          ThumbGlyph.PixelFormat := pf24bit;
      end;
    end;
    if ThumbGlyph.PixelFormat = pf32bit then  // Patch if Png, don't work in std. mode
      CopyBmp32(R, MkRect(ThumbGlyph), FCommonData.FCacheBmp, ThumbGlyph, EmptyCI, False, clNone, 0, False)
    else
      BitBlt(FCommonData.FCacheBmp.Canvas.Handle, R.Left, R.Top, ThumbGlyph.Width, ThumbGlyph.Height, ThumbGlyph.Canvas.Handle, 0, 0, SRCCOPY);
  end;

begin
{$IFDEF DELPHI7UP}
  if SliderVisible then
{$ENDIF}
  begin
    aRect := ThumbRect;
    if ThumbGlyph.Empty then
      with SkinData do begin
        if (SkinData.SkinManager = nil) or (SkinData.SkinManager.GetScale = 0) then
          ActThumbSize := iThumbSize
        else
          ActThumbSize := SkinData.SkinManager.ScaleInt(iThumbSize) + 1;

        GlyphSize := MkSize(aRect);
        if (Orientation = trHorizontal) and (HeightOf(aRect) = ActThumbSize) or (Orientation = trVertical) and (WidthOf(aRect) = ActThumbSize) then
          Stretched := False
        else
          Stretched := (HeightOf(aRect) <> GlyphSize.cy) or (WidthOf(aRect) <> GlyphSize.cx);

        DrawRect := PrepareBG;
        DrawPoint := Point(DrawRect.Left + (WidthOf(DrawRect) - GlyphSize.cx) div 2, DrawRect.Top + (HeightOf(DrawRect) - GlyphSize.cy) div 2);

{$IFDEF DELPHI7UP}
        if acThemesEnabled then begin
          te := ThumbStyles[Orientation = trVertical, TickMarks, Mode];
          Details := acThemeServices.GetElementDetails(te);
          acThemeServices.DrawElement(Bmp.Canvas.Handle, Details, DrawRect);
        end
        else
{$ENDIF}
        begin
          dec(aRect.Bottom);
          Frame3D(Bmp.Canvas, aRect, cl3DLight, cl3DDkShadow, 1);
          Frame3D(Bmp.Canvas, aRect, clWhite, clBtnShadow, 1);
          FillDC(Bmp.Canvas.Handle, aRect, clBtnFace);
        end;
        ReturnToCache;
        if Bmp <> FCommonData.FCacheBmp then
          FreeAndNil(Bmp);
      end
    else begin
      DrawRect.Left   := aRect.Left    + (WidthOf (aRect) - ThumbGlyph.Width)  div 2;
      DrawRect.Top    := aRect.Top     + (HeightOf(aRect) - ThumbGlyph.Height) div 2;
      DrawRect.Right  := DrawRect.Left + ThumbGlyph.Width;
      DrawRect.Bottom := DrawRect.Top  + ThumbGlyph.Height;
      PaintGlyph(DrawRect);
    end;
  end;
end;


procedure TsTrackBar.SetShowProgressFrom(const Value: Integer);
begin
  if FShowProgressFrom <> Value then begin
    FShowProgressFrom := Value;
    if FShowProgressFrom < Min then
      FShowProgressFrom := Min;

    if FShowProgressFrom > Max then
      FShowProgressFrom := Max;
      
    if not (csLoading in ComponentState) and SkinData.Skinned then
      SkinData.Invalidate
  end;
end;


procedure TsTrackBar.CreateWnd;
begin
  inherited;
  DoubleBuffered := False;
end;


procedure TsRangeSelector.AfterConstruction;
begin
  inherited;
  FCommonData.Loaded;
  UpdateIndexes(SkinData.SkinIndex);
end;


procedure TsRangeSelector.Changed;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;


procedure TsRangeSelector.ChangeScale(M, D: Integer);
begin
  inherited;
  FChannelMargin := MulDiv(FChannelMargin, M, D);
end;


function TsRangeSelector.ChannelRect: TRect;
const
  cHeight = 8;
begin
  if Orientation = trVertical then
    Result := Rect(FChannelMargin, FChannelMargin, FChannelMargin + cHeight, Height - cHeight)
  else
    Result := Rect(FChannelMargin, FChannelMargin, Width - FChannelMargin, FChannelMargin + cHeight);
end;


function TsRangeSelector.ChannelSize: TSize;
var
  R: TRect;
begin
  R := ChannelRect;
  Result.cx := WidthOf(R); 
  Result.cy := HeightOf(R);
  if Orientation = trHorizontal then
    dec(Result.cx, Result.cy)
  else
    dec(Result.cy, Result.cx);
end;


function TsRangeSelector.Coord(Index: integer): integer;
begin
  if Orientation = trHorizontal then
    Result := Round(ChannelRect.Left + iff(Index = 0, Position1, Position2) * (ChannelSize.cx / (Max - Min)))
  else
    Result := Round(ChannelRect.Bottom - iff(Index = 0, Position1, Position2) * (ChannelSize.cy / (Max - Min)));
end;


constructor TsRangeSelector.Create(AOwner: TComponent);
begin
  FCommonData := TsCommonData.Create(Self, True);
  FCommonData.COC := COC_TsTrackBar;
  inherited;
  FMin := 0;
  FMax := 10;
  FPosition1 := 0;
  FPosition2 := 10;
  FFrequency := 1;
  FState1 := 0;
  FState2 := 0;
  FChannelMargin := 8;

  FChangingMode := cmDepended;
  
  FTickMarks := tmBottomRight;
  FTickStyle := tsAuto;
  FOrientation := trHorizontal;
  FShowFocus := False;
  FShowRange := True;
end;


procedure TsRangeSelector.CreateWnd;
begin
  inherited;
end;


destructor TsRangeSelector.Destroy;
begin
  FreeAndNil(FCommonData);
  inherited;
end;


procedure TsRangeSelector.InvalidateNow;
begin
  if [csLoading, csDestroying] * ComponentState = [] then begin
    FCommonData.BGChanged := True;
    Repaint;
  end;
end;


procedure TsRangeSelector.Loaded;
begin
  inherited;
  FCommonData.Loaded;
  UpdateIndexes(SkinData.SkinIndex);
end;


function TsRangeSelector.Mode: integer;
begin
  if csLButtonDown in ControlState then
    Result := 2
  else
    Result := integer(ControlIsActive(FCommonData));
end;


procedure TsRangeSelector.Paint;
begin
  if not (csDestroying in ComponentState) then begin
    InitCacheBmp(SkinData);
    if FCommonData.Skinned then begin
      if not (InUpdating(FCommonData) or TimerIsActive(SkinData)) then begin
        PrepareCache;
        if FCommonData.SkinIndex >= 0 then
          UpdateCorners(FCommonData, 0);
      end;
    end
    else begin
      StdPaintBG(FCommonData.FCacheBmp);
      StdPaintBar(FCommonData.FCacheBmp);
      if State2 > 0 then begin
        StdPaintThumb(0);
        StdPaintThumb(1);
      end
      else begin
        StdPaintThumb(1);
        StdPaintThumb(0);
      end;
    end;
    BitBlt(Canvas.Handle, 0, 0, Width, Height, FCommonData.FCacheBmp.Canvas.Handle, 0, 0, SRCCOPY);
  end;
end;


procedure TsRangeSelector.PaintBar;
var
  w, h: integer;
  aRect: TRect;
  CI: TCacheInfo;
begin
  aRect := ChannelRect;
  if SkinData.SkinManager.IsValidImgIndex(FSliderNdx) then begin
    CI := MakeCacheInfo(FCommonData.FCacheBmp);
    case Orientation of
      trHorizontal: begin
        h := SkinData.SkinManager.ma[FSliderNdx].Height - 1;
        w := HeightOf(aRect);
        aRect.Top := aRect.Top + (w - h) div 2;
        aRect.Bottom := aRect.Top + h;
        InflateRect(aRect, -1, 0);
        DrawSkinRect(FCommonData.FCacheBmp, aRect, CI, SkinData.SkinManager.ma[FSliderNdx], integer(ControlIsActive(FCommonData)), True);
        if FShowRange then
          PaintRange(RangeRect(aRect), True);
      end;

      trVertical: begin
        h := SkinData.SkinManager.ma[FSliderNdx].Width - 1;
        w := WidthOf(aRect);
        aRect.Left := aRect.Left + (w - h) div 2;
        aRect.Right := aRect.Left + h;
        InflateRect(aRect, 0, -1);
        DrawSkinRect(FCommonData.FCacheBmp, aRect, CI, SkinData.SkinManager.ma[FSliderNdx], integer(ControlIsActive(FCommonData)), True);
        if FShowRange then
          PaintRange(RangeRect(aRect), False);
      end;
    end;
  end;
  if Orientation = trHorizontal then
    PaintTicksHor
  else
    PaintTicksVer;
end;


procedure TsRangeSelector.PaintBody;
var
  R: TRect;
  fColor: TColor;
begin
  R := MkRect(Self);
  if SkinData.Skinned then begin
    PaintItem(FCommonData, GetParentCache(FCommonData), True, integer(ControlIsActive(FCommonData)), R, Point(Left, Top), FCommonData.FCacheBmp, False);
    if SkinData.SkinIndex >= 0 then begin
      PaintBar;
      if State2 > 0 then begin
        PaintThumb(0, 0);
        PaintThumb(1, 0);
      end
      else begin
        PaintThumb(1, 0);
        PaintThumb(0, 0);
      end;
    end;
    FColor := SkinData.SkinManager.gd[SkinData.SkinIndex].Props[1].FontColor.Color;
  end
  else begin
    StdPaintBG(FCommonData.FCacheBmp);
    StdPaintBar(FCommonData.FCacheBmp);
    StdPaintThumb(Position1);
    FColor := clBtnText;
  end;
  if FShowFocus and Focused then begin
    InflateRect(R, 0, -1);
    FocusRect(SkinData.FCacheBMP.Canvas, R, FColor, clNone)
  end;
end;


procedure TsRangeSelector.PaintRange(R: TRect; Horz: boolean);
begin
  with SkinData.SkinManager do
    if IsValidImgIndex(FRangeNdx) then
      DrawSkinRect(FCommonData.FCacheBmp, R, MakeCacheInfo(FCommonData.FCacheBmp), ma[FRangeNdx], integer(ControlIsActive(FCommonData)), True);
end;


procedure TsRangeSelector.PaintThumb(Index, State: integer);
var
  Bmp: TBitmap;
  GlyphSize: TSize;
  DrawPoint: TPoint;
  Stretched: boolean;
  aRect, DrawRect: TRect;

  function PrepareBG: TRect;
  var
    TmpBmp: TBitmap;
  begin
    if Stretched or (TickMarks = tmTopLeft) then begin
      Bmp := CreateBmp32(GlyphSize);
      Result := MkRect(GlyphSize);
      TmpBmp := CreateBmp32(aRect);
      BitBlt(TmpBmp.Canvas.Handle, 0, 0, TmpBmp.Width, TmpBmp.Height, FCommonData.FCacheBmp.Canvas.Handle, aRect.Left, aRect.Top, SRCCOPY);
      Stretch(TmpBmp, Bmp, Bmp.Width, Bmp.Height, ftMitchell);
      if TickMarks = tmTopLeft then
        RotateBmp180(Bmp, Orientation <> trHorizontal);
        
      FreeAndNil(TmpBmp);
    end
    else begin
      Bmp := FCommonData.FCacheBmp;
      Result := aRect;
    end;
  end;

  procedure ReturnToCache;
  var
    TmpBmp: TBitmap;
  begin
    if FCommonData.FCacheBmp <> Bmp then begin
      if TickMarks = tmTopLeft then
        RotateBmp180(Bmp, Orientation <> trHorizontal);

      TmpBmp := CreateBmp32(aRect);
      Stretch(Bmp, TmpBmp, TmpBmp.Width, TmpBmp.Height, ftMitchell);
      BitBlt(FCommonData.FCacheBmp.Canvas.Handle, aRect.Left, aRect.Top, WidthOf(aRect), HeightOf(aRect), TmpBmp.Canvas.Handle, 0, 0, SRCCOPY);
      FreeAndNil(TmpBmp);
    end
  end;

begin
  aRect := ThumbRect(Coord(Index));
  with SkinData.SkinManager do
    if IsValidImgIndex(FThumbNdx) then begin
      GlyphSize := MkSize(ma[FThumbNdx]);
      Stretched := False;
      DrawRect := PrepareBG;
      DrawPoint := Point(DrawRect.Left + (WidthOf(DrawRect) - GlyphSize.cx) div 2, DrawRect.Top + (HeightOf(DrawRect) - GlyphSize.cy) div 2);
      DrawSkinGlyph(Bmp, DrawPoint, math.min(iff(Index = 0, State1, State2), ma[FThumbNdx].ImageCount), 1, ma[FThumbNdx], MakeCacheInfo(Bmp));
      ReturnToCache;
      if Bmp <> FCommonData.FCacheBmp then
        FreeAndNil(Bmp);
    end;
end;


procedure TsRangeSelector.PaintTick(P: TPoint; Horz: boolean);
var
  w: integer;
  R: TRect;
begin
  if SkinData.SkinIndex >= 0 then
    with SkinData.SkinManager do begin
      if FTickNdx >= 0 then begin
        if Horz then
          dec(P.x, ma[FTickNdx].Width)
        else
          dec(P.y, ma[FTickNdx].Height);

        DrawSkinGlyph(SkinData.FCacheBmp, P, Mode, 1, ma[FTickNdx], MakeCacheInfo(FCommonData.FCacheBmp))
      end
      else begin
        if Horz then
          R := Rect(P.x, P.y, P.x + 2, P.Y + TickHeight)
        else
          R := Rect(P.x, P.y, P.x + TickHeight, P.Y + 2);

        w := 1;
        DrawRectangleOnDC(FCommonData.FCacheBmp.Canvas.Handle, R, ColorToRGB(clBtnShadow), ColorToRGB(clWhite), w);
      end;
    end
  else begin
    if Horz then
      R := Rect(P.x, P.y, P.x + 1, P.Y + 3)
    else
      R := Rect(P.x, P.y, P.x + 3, P.Y + 1);

    FillDC(FCommonData.FCacheBmp.Canvas.Handle, R, clBtnShadow);
  end;
end;


procedure TsRangeSelector.PaintTicksHor;
var
  cr: TRect;
  pa: TAPoint;
  i, mh: integer;
begin
  pa := nil;
  if TickStyle <> tsNone then begin
    cr := ChannelRect;
    pa := TicksArray(cr);
    mh := (ThumbSize.cy - HeightOf(cr)) div 2 + 2;
    if TickMarks in [tmTopLeft, tmBoth] then
      for i := 0 to High(pa) do
        PaintTick(Point(pa[i].x, cr.Top - mh - TickHeight), True);

    if TickMarks in [tmBottomRight, tmBoth] then
      for i := 0 to High(pa) do
        PaintTick(Point(pa[i].x, cr.Bottom + mh), True);
  end;
end;


procedure TsRangeSelector.PaintTicksVer;
var
  cr: TRect;
  pa: TAPoint;
  i, mh: integer;
begin
  if TickStyle <> tsNone then begin
    cr := ChannelRect;
    pa := TicksArray(cr);
    mh := (ThumbSize.cx - WidthOf(cr)) div 2 + 2;
    if TickMarks in [tmTopLeft, tmBoth] then
      for i := 0 to High(pa) do
        PaintTick(Point(cr.Left - mh - TickHeight, pa[i].y), False);

    if TickMarks in [tmBottomRight, tmBoth] then
      for i := 0 to High(pa) do
        PaintTick(Point(cr.Right + mh, pa[i].y), False);
  end
  else
    pa := nil;
end;


procedure TsRangeSelector.PrepareCache;
begin
  InitCacheBmp(SkinData);
  PaintBody;
  if (SkinData.SkinIndex >= 0) and not Enabled then
    BmpDisabledKind(FCommonData.FCacheBmp, [dkBlended], Parent, GetParentCache(FCommonData), Point(Left, Top));
end;


function TsRangeSelector.RangeRect(aRect: TRect): TRect;
var
  pos1, pos2: integer;
begin
  Result := aRect;
  if FShowRange and (Max - Min > 0) then begin
    pos1 := Coord(0);
    pos2 := Coord(1);
    if Orientation = trHorizontal then
      if pos1 < pos2 then begin
        Result.Right := pos2;
        Result.Left := pos1;
      end
      else begin
        Result.Right := pos1;
        Result.Left := pos2;
      end
    else
      if pos1 > pos2 then begin
        Result.Bottom := pos1;
        Result.Top    := pos2;
      end
      else begin
        Result.Bottom := pos2;
        Result.Top    := pos1;
      end;
  end;
end;


procedure TsRangeSelector.SetBoolean(const Index: Integer; const Value: boolean);

  procedure ChangeProp(var Prop: boolean; Value: boolean);
  begin
    if Prop <> Value then begin
      Prop := Value;
      InvalidateNow;
    end;
  end;

begin
  case Index of
    1: ChangeProp(FShowFocus, Value);
    2: ChangeProp(FShowRange, Value);
  end;
end;


procedure TsRangeSelector.SetInteger(const Index, Value: integer);
begin
  case Index of
    2: if (FMin <> Value) and (Value < FMax) then begin
      FMin := Value;
      Changed;
      InvalidateNow;
    end;

    3: if (FMax <> Value) and (Value > FMin) then begin
      FMax := Value;
      Changed;
      InvalidateNow;
    end;

    4: if FFrequency <> Value then begin
      FFrequency := Value;
      InvalidateNow;
    end;
  end;
end;


procedure TsRangeSelector.SetOrientation(const Value: TTrackBarOrientation);
begin
  if FOrientation <> Value then begin
    FOrientation := Value;
    InvalidateNow;
  end;
end;


procedure TsRangeSelector.SetPosition(const Index, Value: integer);

  procedure DoChanged;
  begin
    Changed;
    InvalidateNow;
  end;

begin
  case Index of
    0: if FPosition1 <> Value then
      if Value >= Min then
        if Value < FPosition2 then begin
          FPosition1 := Value;
          DoChanged;
        end
        else
          if ChangingMode = cmUndepended then begin
            if FPosition2 < FMax then begin
              FPosition2 := Value + 1;
              FPosition1 := Value;
              DoChanged;
            end;
          end
          else
            if ChangingMode = cmIgnoredSecond then begin
              if Value <= FMax then
                FPosition1 := Value
              else
                FPosition1 := FMax;

              DoChanged;
            end;

    1: if FPosition2 <> Value then
      if Value <= Max then
        if Value > FPosition1 then begin
          FPosition2 := Value;
          DoChanged;
        end
        else
          if ChangingMode = cmUndepended then begin
            if FPosition1 > FMin then begin
              FPosition1 := Value - 1;
              FPosition2 := Value;
              DoChanged;
            end
          end
          else
            if ChangingMode = cmIgnoredSecond then begin
              if Value >= FMin then
                FPosition2 := Value
              else
                FPosition2 := FMin;

              DoChanged;
            end;
  end;
end;


procedure TsRangeSelector.SetState(const Index, Value: integer);
begin
  case Index of
    0: if FState1 <> Value then begin
      FState1 := Value;
      PrevMousePos := acMousePos;
      if FState1 > 0 then      
        FState2 := 0;                      
    end;

    1: if FState2 <> Value then begin
      FState2 := Value;
      PrevMousePos := acMousePos;
      if FState2 > 0 then      
        FState1 := 0;                      
    end;
  end;
end;


procedure TsRangeSelector.SetTickMarks(const Value: TTickMark);
begin
  if FTickMarks <> Value then begin
    FTickMarks := Value;
    InvalidateNow;
  end;
end;


procedure TsRangeSelector.SetTickStyle(const Value: TTickStyle);
begin
  if FTickStyle <> Value then begin
    FTickStyle := Value;
    InvalidateNow;
  end;
end;


procedure TsRangeSelector.StdPaintBar(Bmp: TBitmap);
var
  aRect: TRect;
{$IFDEF DELPHI7UP}
  Details: TThemedElementDetails;
  te: TThemedTrackBar;
{$ENDIF}
begin
  aRect := ChannelRect;
{$IFDEF DELPHI7UP}
  if acThemesEnabled then begin
    if Orientation = trVertical then
      te := ttbTrack
    else
      te := ttbTrackVert;

    Details := acThemeServices.GetElementDetails(te);
    acThemeServices.DrawElement(Bmp.Canvas.Handle, Details, aRect);
    InflateRect(aRect, -1, -1);
  end
  else
{$ENDIF}
  begin
    Frame3D(Bmp.Canvas, aRect, clBtnShadow, clBtnHighlight, 1);
    Frame3D(Bmp.Canvas, aRect, cl3DDkShadow, cl3DLight, 1);
    FillDC(Bmp.Canvas.Handle, aRect, clWindow);
  end;
  if ShowRange then
    FillRect32(Bmp, RangeRect(aRect), ColorToRGB(clHighLight));

  if Orientation = trHorizontal then
    PaintTicksHor
  else
    PaintTicksVer;
end;


procedure TsRangeSelector.StdPaintBG(Bmp: TBitmap);
{$IFDEF DELPHI7UP}
var
  Details: TThemedElementDetails;
{$ENDIF}
begin
{$IFDEF DELPHI7UP}
  if acThemesEnabled then begin
    FillDC(Bmp.Canvas.Handle, MkRect(Bmp), clBtnFace);
    Details := acThemeServices.GetElementDetails(ttBody);
    acThemeServices.DrawParentBackground(Handle, Bmp.Canvas.Handle, @Details, False);
  end
  else
{$ENDIF}
    FillDC(Bmp.Canvas.Handle, MkRect(Bmp), clBtnFace);
end;


procedure TsRangeSelector.StdPaintThumb(Index: integer);
var
  Bmp: TBitmap;
  GlyphSize: TSize;
  DrawPoint: TPoint;
  Stretched: boolean;
  ActThumbSize: integer;
  aRect, DrawRect: TRect;
{$IFDEF DELPHI7UP}
  Details: TThemedElementDetails;
  te: TThemedTrackBar;
{$ENDIF}

  function PrepareBG: TRect;
  var
    TmpBmp: TBitmap;
  begin
    if Stretched or (TickMarks = tmTopLeft) then begin
      Bmp := CreateBmp32(GlyphSize);
      Result := MkRect(GlyphSize);
      TmpBmp := CreateBmp32(aRect);
      BitBlt(TmpBmp.Canvas.Handle, 0, 0, TmpBmp.Width, TmpBmp.Height, FCommonData.FCacheBmp.Canvas.Handle, aRect.Left, aRect.Top, SRCCOPY);
      Stretch(TmpBmp, Bmp, Bmp.Width, Bmp.Height, ftMitchell);
      if TickMarks = tmTopLeft then
        RotateBmp180(Bmp, Orientation <> trHorizontal);

      FreeAndNil(TmpBmp);
    end
    else begin
      Bmp := FCommonData.FCacheBmp;
      Result := aRect;
    end;
  end;

  procedure ReturnToCache;
  var
    TmpBmp: TBitmap;
  begin
    if FCommonData.FCacheBmp <> Bmp then begin
      if TickMarks = tmTopLeft then
        RotateBmp180(Bmp, Orientation <> trHorizontal);

      TmpBmp := CreateBmp32(aRect);
      Stretch(Bmp, TmpBmp, TmpBmp.Width, TmpBmp.Height, ftMitchell);
      BitBlt(FCommonData.FCacheBmp.Canvas.Handle, aRect.Left, aRect.Top, WidthOf(aRect), HeightOf(aRect), TmpBmp.Canvas.Handle, 0, 0, SRCCOPY);
      FreeAndNil(TmpBmp);
    end
  end;

begin
  aRect := ThumbRect(Coord(Index));
  with SkinData do begin
    if (SkinData.SkinManager = nil) or (SkinData.SkinManager.GetScale = 0) then
      ActThumbSize := iThumbSize
    else
      ActThumbSize := SkinData.SkinManager.ScaleInt(iThumbSize) + 1;

    GlyphSize := MkSize(aRect);
    if (Orientation = trHorizontal) and (HeightOf(aRect) = ActThumbSize) or (Orientation = trVertical) and (WidthOf(aRect) = ActThumbSize) then
      Stretched := False
    else
      Stretched := (HeightOf(aRect) <> GlyphSize.cy) or (WidthOf(aRect) <> GlyphSize.cx);

    DrawRect := PrepareBG;
    DrawPoint := Point(DrawRect.Left + (WidthOf(DrawRect) - GlyphSize.cx) div 2, DrawRect.Top + (HeightOf(DrawRect) - GlyphSize.cy) div 2);

{$IFDEF DELPHI7UP}
    if acThemesEnabled then begin
      te := ThumbStyles[Orientation = trVertical, TickMarks, iff(Index = 0, State1, State2)];
      Details := acThemeServices.GetElementDetails(te);
      acThemeServices.DrawElement(Bmp.Canvas.Handle, Details, DrawRect);
    end
    else
{$ENDIF}
    begin
      dec(aRect.Bottom);
      Frame3D(Bmp.Canvas, aRect, cl3DLight, cl3DDkShadow, 1);
      Frame3D(Bmp.Canvas, aRect, clWhite, clBtnShadow, 1);
      FillDC(Bmp.Canvas.Handle, aRect, clBtnFace);
    end;
    ReturnToCache;
    if Bmp <> FCommonData.FCacheBmp then
      FreeAndNil(Bmp);
  end;
end;


function TsRangeSelector.ThumbRect(Coord: integer): TRect;
var
  Size: TSize;
  cRect: TRect;
begin
  Size := ThumbSize;
  cRect := ChannelRect;
  if Orientation = trVertical then begin
    Result.Top := Coord + Size.cx div 2 - WidthOf(cRect) div 2;
    Result.Left := ChannelRect.Left - (ThumbSize.cy - HeightOf(ChannelRect)) div 2;    
    Result.Right := Result.Left + ThumbSize.cy;
    Result.Bottom := Result.Top + ThumbSize.cx;
  end
  else begin
    Result.Left := Coord - Size.cx div 2 + HeightOf(cRect) div 2;
    Result.Top := ChannelRect.Top - (ThumbSize.cy - HeightOf(ChannelRect)) div 2;    
    Result.Right := Result.Left + ThumbSize.cx;
    Result.Bottom := Result.Top + ThumbSize.cy;
  end;
end;


const
  SysThumbSize: TSize = (cx:11; cy:21);


function TsRangeSelector.ThumbSize: TSize;
begin
  Result := SysThumbSize;
end;


function TsRangeSelector.TickCount: integer;
begin
  Result := (Max - Min) div FFrequency + 1;
end;


function TsRangeSelector.TickPos(i: integer): integer;
var
  R: TRect;

  function TickWidth: integer;
  begin
    if FTickNdx >= 0 then
      Result := SkinData.SkinManager.ma[FTickNdx].Width
    else
      Result := 2;
  end;

begin
  R := ChannelRect;
  if Orientation = trVertical then
    Result := R.Bottom - Round((HeightOf(R) / (i - 1)) * i) + TickWidth div 2
  else
    Result := R.Left   + Round((WidthOf(R)  / (i - 1)) * i) - TickWidth div 2;
end;


function TsRangeSelector.TicksArray(ChRect: TRect): TAPoint;
var
  i, c: integer;
  ThSize: TSize;
begin
  Result := nil;
  ThSize := ThumbSize;
  c := TickCount;
  SetLength(Result, c);
  if Orientation = trVertical then begin
    if SkinData.Skinned and (FTickNdx >= 0) then
      OffsetRect(ChRect, 0, -SkinData.SkinManager.ma[FTickNdx].Height div 2 - 1)
    else
      OffsetRect(ChRect, 0, 2);

    iStep := ChannelSize.cy / (TickCount - 1);
    for i := 0 to c - 1 do
      Result[i] := Point(0, Round(ChRect.Top + i * iStep + WidthOf(ChRect) div 2));
  end
  else begin
    if SkinData.Skinned and (FTickNdx >= 0) then
      OffsetRect(ChRect, SkinData.SkinManager.ma[FTickNdx].Width div 2 + 1, 0)
    else
      OffsetRect(ChRect, 2, 0);

    iStep := ChannelSize.cx / (TickCount - 1);
    for i := 0 to c - 1 do
      Result[i] := Point(Round(ChRect.Left + i * iStep + HeightOf(ChRect) div 2), 0);
  end;
end;


procedure TsRangeSelector.UpdateIndexes(MainNdx: integer);
var
  Horz: boolean;
begin
  Horz := Orientation = trHorizontal;
  if MainNdx >= 0 then
    with SkinData.SkinManager, ConstData, TrackBar[Horz] do
      if SkinIndex = MainNdx then begin
        FTickNdx   := TickIndex;
        FRangeNdx  := ProgIndex;
        FSliderNdx := SlideIndex;
        FThumbNdx  := GlyphIndex;
      end
      else begin
        FTickNdx   := GetMaskIndex(MainNdx, ThickArray[Horz]);
        FRangeNdx  := GetMaskIndex(MainNdx, ProgArray[Horz]);
        FSliderNdx := GetMaskIndex(MainNdx, s_SliderChannelMask);

        if not Horz then
          FThumbNdx := GetMaskIndex(MainNdx, s_SliderVertMask)
        else
          FThumbNdx := -1;

        if FThumbNdx < 0 then
          FThumbNdx := GetMaskIndex(MainNdx, s_SliderHorzMask);
      end
  else begin
    FTickNdx   := -1;
    FRangeNdx  := -1;
    FSliderNdx := -1;
    FThumbNdx  := -1;
  end;
end;


procedure TsRangeSelector.WMMouseMsg(var Message: TWMMouse);
var
  State: TKeyboardState;
begin
  DefaultHandler(Message);
  GetKeyboardState(State);
  case Message.Msg of
    WM_LBUTTONUP:   if Assigned(OnMouseUp)   then OnMouseUp  (Self, mbLeft, GetShiftState, acMousePos.X, acMousePos.Y);
    WM_LBUTTONDOWN: if Assigned(OnMouseDown) then OnMouseDown(Self, mbLeft, GetShiftState, acMousePos.X, acMousePos.Y);
  end;
end;


procedure TsRangeSelector.WndProc(var Message: TMessage);
var
  b: boolean;
  PS: TPaintStruct;
  DC, SavedDC: hdc;
{$IFDEF DELPHI7UP}
  ParentForm: TCustomForm;
{$ENDIF}

  function PosToValue: integer;
  begin
    if Orientation = trHorizontal then 
      Result := (TWMMouse(Message).XPos - FChannelMargin) * (Max - Min) div ChannelSize.cx
    else 
      Result := (Height - TWMMouse(Message).YPos - FChannelMargin) * (Max - Min) div ChannelSize.cx;
  end;
  
begin
{$IFDEF LOGGED}
  AddToLog(Message);
{$ENDIF}
  case Message.Msg of
    SM_ALPHACMD:
      case Message.WParamHi of
        AC_CTRLHANDLED: begin
          Message.Result := 1;
          Exit;
        end; // AlphaSkins supported

        AC_REMOVESKIN:
          if ACUInt(Message.LParam) = ACUInt(SkinData.SkinManager) then begin
            CommonMessage(Message, FCommonData);
            RecreateWnd;
            Exit;
          end;

        AC_SETNEWSKIN:
          if ACUInt(Message.LParam) = ACUInt(SkinData.SkinManager) then begin
            CommonMessage(Message, FCommonData);
            UpdateIndexes(SkinData.SkinIndex);
            Exit;
          end;

        AC_REFRESH:
          if ACUInt(Message.LParam) = ACUInt(SkinData.SkinManager) then begin
            CommonMessage(Message, FCommonData);
            UpdateIndexes(SkinData.SkinIndex);
            Repaint;
            Exit;
          end;

        AC_PREPARECACHE: begin
          PrepareCache;
          Exit;
        end;

        AC_DRAWANIMAGE: begin
          Message.Result := 0;
          if Message.LParam <> 0 then
            try
              DC := GetWindowDC(Handle);
              SavedDC := SaveDC(DC);
              try
                BitBlt(DC, BorderWidth, BorderWidth, Width, Height, TBitmap(Message.LParam).Canvas.Handle, 0, 0, SRCCOPY);
              finally
                RestoreDC(DC, SavedDC);
                ReleaseDC(Handle, DC);
              end;
            finally
              Message.Result := 1;
            end;

          Exit;
        end;

        AC_GETDEFINDEX: begin
          if FCommonData.SkinManager <> nil then
            Message.Result := FCommonData.SkinManager.ConstData.Sections[ssTrackBar] + 1
          else
            Message.Result := 0;

          UpdateIndexes(Message.Result - 1);
          Exit;
        end;

        AC_ENDPARENTUPDATE: begin
          if FCommonData.Updating then begin
            FCommonData.Updating := False;
            Repaint;
          end;
          Exit;
        end;

        AC_GETSKINDATA: begin
          Message.Result := LRESULT(SkinData);
          Exit;
        end;
      end;

      WM_ERASEBKGND:
        Exit;

    CM_MOUSEENTER:
      if not (csDesigning in ComponentState) and not (csLButtonDown in ControlState) then begin
{$IFDEF DELPHI7UP}
        ParentForm := GetParentForm(Self);
        if (ParentForm = nil) or not TForm(ParentForm).TransparentColor then
{$ENDIF}
        begin
          FCommonData.FMouseAbove := True;
          FCommonData.BGChanged := False;
          if FCommonData.SkinIndex >= 0 then
            DoChangePaint(FCommonData, 1, UpdateWindow_CB, EventEnabled(aeMouseEnter, [aeGlobalDef]), False)
          else
            RedrawWindow(Handle, nil, 0, RDW_INVALIDATE or RDW_NOERASE or RDW_UPDATENOW);
        end;
      end;

    CM_MOUSELEAVE:
      if not (csDesigning in ComponentState) and not (csLButtonDown in ControlState) then begin
{$IFDEF DELPHI7UP}
        ParentForm := GetParentForm(Self);
        if (ParentForm = nil) or not TForm(ParentForm).TransparentColor then
{$ENDIF}
        begin
          FCommonData.FMouseAbove := False;
          FCommonData.BGChanged := False;
          State1 := 0;
          State2 := 0;
          if FCommonData.SkinIndex >= 0 then
            DoChangePaint(FCommonData, 0, UpdateWindow_CB, EventEnabled(aeMouseLeave, [aeGlobalDef]), False)
          else
            RedrawWindow(Handle, nil, 0, RDW_INVALIDATE or RDW_NOERASE or RDW_UPDATENOW);
        end;
      end;

    WM_MOUSEMOVE:
      if not (csDesigning in ComponentState) then begin
        b := False;
        WMMouseMsg(TWMMouse(Message));
        if State1 = 2 then begin // Moving of first thumb
          Position1 := PosToValue;
          Exit;
        end
        else
          if State2 = 2 then begin // Moving of second thumb
            Position2 := PosToValue;
            Exit;
          end
          else
            if PtInRect(ThumbRect(Coord(0)), SmallPointToPoint(TWMMouse(Message).Pos)) then begin
              if State1 = 0 then begin
                b := True;
                State1 := 1;
              end;
            end
            else
              if PtInRect(ThumbRect(Coord(1)), SmallPointToPoint(TWMMouse(Message).Pos)) then begin
                if State2 = 0 then begin
                  b := True;
                  State2 := 1;
                end;
              end
              else
                if (State1 <> 0) or (State2 <> 0) then begin
                  b := True;
                  State1 := 0;
                  State2 := 0;
                end;

        if b then
          if SkinData.Skinned then begin
            Skindata.BGChanged := False;
            DoChangePaint(FCommonData, 2, UpdateWindow_CB, EventEnabled(aeMouseDown, [aeGlobalDef]), True);
          end
          else
            Repaint
        else
          StopTimer(SkinData);
      end;

    WM_LBUTTONUP:
      if not (csDesigning in ComponentState) and Enabled then begin
        Application.ShowHint := AppShowHint;
        ShowHintStored := False;

        b := False;
        if PtInRect(ThumbRect(Coord(0)), SmallPointToPoint(TWMMouse(Message).Pos)) then begin
          b := True;
          State1 := 1;
        end
        else
          if PtInRect(ThumbRect(Coord(1)), SmallPointToPoint(TWMMouse(Message).Pos)) then begin
            b := True;
            State2 := 1;
          end
          else
            if State1 > 0 then begin
              b := True;
              State1 := 0;
            end
            else
              if State2 > 0 then begin
                b := True;
                State2 := 0;
              end;

        if b then begin
          ControlState := ControlState - [csLButtonDown];
          if SkinData.Skinned then
            DoChangePaint(FCommonData, 1, UpdateWindow_CB, EventEnabled(aeMouseUp, [aeGlobalDef]), True)
          else
            Repaint;
        end
        else
          StopTimer(SkinData);
      end;

    WM_LBUTTONDBLCLK, WM_LBUTTONDOWN:
      if not (csDesigning in ComponentState) and Enabled then begin
        if CanFocus then
          SetFocus;

        if not ShowHintStored then begin
          AppShowHint := Application.ShowHint;
          Application.ShowHint := False;
          ShowHintStored := True;
        end;
        b := False;
        if PtInRect(ThumbRect(Coord(0)), SmallPointToPoint(TWMMouse(Message).Pos)) then begin
          b := True;
          State1 := 2;
        end
        else
          if PtInRect(ThumbRect(Coord(1)), SmallPointToPoint(TWMMouse(Message).Pos)) then begin
            b := True;
            State2 := 2;
          end;

        if b then begin
          ControlState := ControlState + [csLButtonDown];
          if SkinData.Skinned then begin
            Skindata.BGChanged := False;
            DoChangePaint(FCommonData, 2, UpdateWindow_CB, EventEnabled(aeMouseDown, [aeGlobalDef]), True);
          end
          else
            Repaint;
        end
        else
          StopTimer(SkinData);
      end;
  end;

  if not ControlIsReady(Self) or not FCommonData.Skinned(True) then
    inherited
  else begin
    case Message.Msg of
      WM_PRINT: begin
        SkinData.FUpdating := False;
        PaintWindow(TWMPaint(Message).DC);
        Exit;
      end;

      WM_PAINT: begin
        if TimerIsActive(SkinData) then begin
          BeginPaint(Handle, PS);
          EndPaint(Handle, PS);
          Exit;
        end;
        ControlState := ControlState + [csCustomPaint];
      end;

      WM_ERASEBKGND:
        Exit;

      WM_SETFOCUS, CM_ENTER:
        if not (csDesigning in ComponentState) then begin
          inherited;
          if Enabled and not TimerIsActive(SkinData) then
            InvalidateNow;

          Exit;
        end;

      WM_KILLFOCUS, CM_EXIT:
        if not (csDesigning in ComponentState) then begin
          inherited;
          if Enabled then
            StopTimer(SkinData);

          InvalidateNow;
          Exit
        end;

      WM_SIZE:
        StopTimer(SkinData);

      CN_HSCROLL, CN_VSCROLL: begin
        StopTimer(SkinData);
        PaintWindow(0);
      end;
    end;
    CommonWndProc(Message, FCommonData);
    inherited;
    case Message.Msg of
//      TB_INDETERMINATE:
//        Repaint; // Full control repainting

      WM_MOVE:
        if csDesigning in ComponentState then
          Repaint;

      WM_PAINT:
        ControlState := ControlState - [csCustomPaint];
    end;
  end;
end;

end.

