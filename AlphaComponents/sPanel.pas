unit sPanel;
{$I sDefs.inc}
//{$DEFINE LOGGED}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, ExtCtrls, Imglist,
  {$IFNDEF DELPHI5} Types, {$ENDIF}
  {$IFDEF TNTUNICODE} TntExtCtrls, {$ENDIF}
  sCommonData, sConst, acntTypes;


type
{$IFDEF TNTUNICODE}
  TsCustomPanel = class(TTntPanel)
{$ELSE}
  TsCustomPanel = class(TCustomPanel)
{$ENDIF}
{$IFNDEF NOTFORHELP}
  private
    FCommonData: TsCtrlSkinData;
    FOldBevel: TPanelBevel;
    FOnPaint: TPaintEvent;
{$IFNDEF D2010}
    FOnMouseLeave,
    FOnMouseEnter: TNotifyEvent;
{$ENDIF}
  protected
    procedure CopyCache(DC: hdc); virtual;
    procedure OurPaint(DC: HDC = 0; SendUpdated: boolean = True); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AfterConstruction; override;
    procedure Loaded; override;
    procedure Paint; override;
    function PrepareCache: boolean; virtual;
    procedure PaintDragPanel(DC: hdc);
    procedure WndProc(var Message: TMessage); override;
    procedure WriteText(R: TRect; aCanvas: TCanvas = nil; aDC: hdc = 0); virtual;
    procedure PaintWindow(DC: HDC); override;
  published
{$ENDIF} // NOTFORHELP
    property SkinData: TsCtrlSkinData read FCommonData write FCommonData;
{:@event}
    property OnPaint: TPaintEvent read FOnPaint write FOnPaint;
{$IFNDEF D2010}
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
{$ENDIF}
  end;

{$IFDEF DELPHI_XE3}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]{$ENDIF}
  TsPanel = class(TsCustomPanel)
  public
    property DockManager;
  published
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property BevelEdges;
    property BevelInner;
    property BevelKind;
    property BevelOuter;
    property BevelWidth;
    property BiDiMode;
    property BorderWidth;
    property BorderStyle;
    property Caption;
    property Color;
    property Constraints;
    property Ctl3D;
    property UseDockManager default True;
    property DockSite;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FullRepaint;
    property Font;
    property Locked;
{$IFDEF D2010}
    property Padding;
    property ParentDoubleBuffered;
    property ShowCaption;
    property Touch;
    property VerticalAlignment;
    {$IFDEF DELPHI_XE3}
    property StyleElements;
    {$ENDIF}
    property OnAlignInsertBefore;

    property OnAlignPosition;
    property OnGesture;
    property OnMouseActivate;
{$ENDIF}
    property ParentBiDiMode;
{$IFDEF DELPHI7UP}    
    property ParentBackground;
{$ENDIF}    
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnCanResize;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;

{$IFDEF DELPHI_XE3}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]{$ENDIF}
  TsDragBar = class(TsPanel)
{$IFNDEF NOTFORHELP}
  private
    FDraggedControl: TControl;
    procedure WMActivateApp(var Message: TWMActivateApp); message WM_ACTIVATEAPP;
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    procedure Loaded; override;
    procedure ReadState(Reader: TReader); override;
    constructor Create(AOwner: TComponent); override;
    procedure WndProc(var Message: TMessage); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  published
    property Alignment;
    property Align default alTop;
    property Color default clActiveCaption;
{$ENDIF} // NOTFORHELP
    property DraggedControl: TControl read FDraggedControl write FDraggedControl;
  end;


{$IFNDEF NOTFORHELP}
  TsContainer = class(TsCustomPanel);


  TsGrip = class(TsPanel)
  public
    Transparent: boolean;
    LinkedControl: TWinControl;
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
  end;


  TsColInfo = record
    ciColor: TColor;
    ciRect: TRect;
  end;


{$IFDEF DELPHI_XE3}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]{$ENDIF}
  TsColorsPanel = class(TsPanel)
  private
    FItemIndex,
    FItemWidth,
    FItemHeight,
    FItemMargin,
    FColCount,
    FRowCount: integer;

    FColors: TStrings;
    FOnChange: TNotifyEvent;
    procedure SetColors (const Value: TStrings);
    procedure SetInteger(const Index, Value: integer);
  protected
    procedure OurPaint(DC: HDC = 0; SendUpdated: boolean = True); override;
  public
    OldSelected: integer;
    ColorsArray: array of TsColInfo;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure GenerateColors;
    procedure AfterConstruction; override;
    procedure ChangeScale(M, D: Integer); override;
    procedure Loaded; override;
    procedure PaintColors(const DC: hdc);
    function Count: integer;
    function GetItemByCoord(p: TPoint): integer;
    procedure WndProc(var Message: TMessage); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    function ColorValue: TColor;
  published
    property Colors: TStrings read FColors write SetColors;
    property ColCount:   integer index 0 read FColCount   write SetInteger default 5;
    property ItemIndex:  integer index 1 read FItemIndex  write SetInteger default -1;
    property ItemHeight: integer index 2 read FItemHeight write SetInteger default 21;
    property ItemWidth:  integer index 3 read FItemWidth  write SetInteger default 21;
    property ItemMargin: integer index 4 read FItemMargin write SetInteger default 6;
    property RowCount:   integer index 5 read FRowCount   write SetInteger default 2;
    property Height default 60;
    property Width default 140;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;


  TsGradientPanel = class;
  TacGradPaintData = class;

  TacColorData1 = class(TPersistent)
  private
    FOwner: TacGradPaintData;
    FUseSkinColor: boolean;
    FColor: TColor;
    procedure SetUseSkinColor(const Value: boolean);
    procedure SetColor(const Value: TColor);
  public
    constructor Create(AOwner: TacGradPaintData);
    procedure Invalidate;
  published
    property Color: TColor read FColor write SetColor default clWhite;
    property UseSkinColor: boolean read FUseSkinColor write SetUseSkinColor default False;
  end;


  TacColorData2 = class(TacColorData1)
  public
    constructor Create(AOwner: TacGradPaintData);
  published
    property Color default clBtnFace;
    property UseSkinColor default True;
  end;
{$ENDIF} // NOTFORHELP


  TacGradPaintData = class(TPersistent)
{$IFNDEF NOTFORHELP}
  private
    FIsVertical: boolean;
    FOwner: TsGradientPanel;
    FColor1: TacColorData1;
    FColor2: TacColorData2;
    FCustomGradient: string;
    procedure SetIsVertical    (const Value: boolean);
    procedure SetCustomGradient(const Value: string);
    procedure SetColor1(const Index: Integer; const Value: TacColorData1);
    procedure SetColor2(const Index: Integer; const Value: TacColorData2);
  public
    constructor Create(AOwner: TsGradientPanel);
    destructor Destroy; override;
    procedure Invalidate;
  published
{$ENDIF} // NOTFORHELP
    property IsVertical: boolean read FIsVertical write SetIsVertical default True;
    property Color1: TacColorData1 Index 0 read FColor1 write SetColor1;
    property Color2: TacColorData2 Index 1 read FColor2 write SetColor2;
    property CustomGradient: string read FCustomGradient write SetCustomGradient;
  end;


{$IFDEF DELPHI_XE3}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]{$ENDIF}
{$IFDEF TNTUNICODE}
  TsGradientPanel = class(TTntPanel)
{$ELSE}
  TsGradientPanel = class(TPanel)
{$ENDIF}
{$IFNDEF NOTFORHELP}
  private
    FOnPaint: TPaintEvent;
    FPaintData: TacGradPaintData;
{$IFNDEF D2010}
    FOnMouseLeave,
    FOnMouseEnter: TNotifyEvent;
{$ENDIF}
  protected
    FCacheBmp: TBitmap;
    procedure CopyCache(DC: hdc); virtual;
    procedure OurPaint(DC: HDC = 0; SendUpdated: boolean = True); virtual;
  public
    BGChanged: boolean;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    procedure Paint; override;
    function PrepareCache: boolean; virtual;
    procedure WndProc(var Message: TMessage); override;
    procedure WriteText(R: TRect; aCanvas: TCanvas = nil; aDC: hdc = 0);
    procedure PaintWindow(DC: HDC); override;
  published
{$ENDIF} // NOTFORHELP
    property PaintData: TacGradPaintData read FPaintData write FPaintData;
{:@event}
    property OnPaint: TPaintEvent read FOnPaint write FOnPaint;
{$IFNDEF D2010}
{$IFNDEF NOTFORHELP}
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
{$ENDIF} // NOTFORHELP
{$ENDIF}
  end;


{$IFNDEF NOTFORHELP}
  TsStdColorsPanel = class(TsColorsPanel);

  TacContentPos = record
    rBtn,
    rText,
    rGlyph,
    rArrow: TRect;
  end;
{$ENDIF} // NOTFORHELP


  TsRollOutPanel = class(TsPanel)
{$IFNDEF NOTFORHELP}
  private
    FAutoHide,
    FAutoShow,
    FAnimated,
    FCollapsed,
    FShowArrow,
    BtnChanged,
    MousePressed,
    FDirectionArrow: boolean;

    FBtnState,
    FGroupIndex,
    FButtonHeight,
    FImageIndexExpanded,
    FImageIndexCollapsed: integer;

    FOnAfterExpand,
    FOnStateChanged,
    FOnBeforeExpand,
    FOnAfterCollapse,
    FOnBeforeCollapse: TNotifyEvent;

    FPlacement: TacSide;
    FTitleCursor: TCursor;
    FTitleSkin: TsSkinSection;
    FImages: TCustomImageList;
    FImageChangeLink: TChangeLink;
    procedure SetInteger(const Index, Value: integer);
    procedure SetPlacement(const Value: TacSide);
    procedure WMNCCaclSize   (var Message: TWMNCCalcSize); message WM_NCCALCSIZE;
    procedure WMNCPaint      (var Message: TMessage);      message WM_NCPAINT;
    procedure WMNCHitText    (var Message: TWMNCHitTest);  message WM_NCHITTEST;
    procedure WMNCLButtonDown(var Message: TWMMouse);      message WM_NCLBUTTONDOWN;
    procedure WMNCLButtonUp  (var Message: TWMMouse);      message WM_NCLBUTTONUP;
    procedure SetImages(const Value: TCustomImageList);
    procedure ImageListChange(Sender: TObject);
    procedure SetBoolean(const Index: Integer; const Value: boolean);
    procedure DoTimer(Sender: TObject);
    procedure SetBtnState(const Value: integer);
    procedure SetTitleSkin(const Value: TsSkinSection);
  protected
    RgnOffset,
    BtnIndex: integer;

    ArrowAngle: real;
    BtnCache: TBitmap;
    ExitTimer: TTimer;
    Arranging: boolean;
    TitleForm: TacGlowForm;
    function BtnRect: TRect;
    function BtnPaintState: integer;
    function PtInButton(X, Y: integer): boolean;
    function PrepareStdCache: TPoint;
    function PrepareSkinCache: TPoint;
    function GlyphExists: boolean;
    function ActualImageIndex: integer;
    function ActualButtonHeight: integer;
    procedure CalcContentPos(var ContentPos: TacContentPos);
    procedure CheckAutoShowing(Value: boolean);
    procedure ShowTitleForm;
    procedure HideTitleForm;
    procedure UpdateAngle;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
    function PrepareCache: boolean; override;
    procedure ChangeState(bCollapsed, bAnimated: boolean);
    procedure WndProc(var Message: TMessage); override;
    destructor Destroy; override;
    procedure Loaded; override;
    procedure Rearrange;
    property BtnState: integer read FBtnState write SetBtnState;
  published
    property GroupIndex:          integer index 0 read FGroupIndex          write SetInteger default 0;
{$ENDIF} // NOTFORHELP
    property ButtonHeight:        integer index 1 read FButtonHeight        write SetInteger default 22;
    property ImageIndexCollapsed: integer index 2 read FImageIndexCollapsed write SetInteger default -1;
    property ImageIndexExpanded:  integer index 3 read FImageIndexExpanded  write SetInteger default -1;
    property Collapsed: boolean index 0 read FCollapsed write SetBoolean default False;
    property ShowArrow: boolean index 1 read FShowArrow write SetBoolean default True;
    property Animated:  boolean index 2 read FAnimated  write SetBoolean default True;
    property DirectionArrow: boolean index 3 read FDirectionArrow write SetBoolean default False;

    property AutoHide: boolean read FAutoHide write FAutoHide default False;
    property AutoShow: boolean read FAutoShow write FAutoShow default False;

    property TitleCursor: TCursor read FTitleCursor write FTitleCursor default crDefault;
    property TitleSkin: TsSkinSection read FTitleSkin write SetTitleSkin;

    property Placement: TacSide read FPlacement write SetPlacement default asTop;
    property Images: TCustomImageList read FImages write SetImages;

    property OnStateChanged:   TNotifyEvent read FOnStateChanged   write FOnStateChanged;
    property OnAfterCollapse:  TNotifyEvent read FOnAfterCollapse  write FOnAfterCollapse;
    property OnAfterExpand:    TNotifyEvent read FOnAfterExpand    write FOnAfterExpand;
    property OnBeforeCollapse: TNotifyEvent read FOnBeforeCollapse write FOnBeforeCollapse;
    property OnBeforeExpand:   TNotifyEvent read FOnBeforeExpand   write FOnBeforeExpand;
  end;


const
  DefSVCompactSize    = 50;
  DefSVOpenedSize     = 200;
  DefSVAnimationDelay = 15;
  DefSVAnimationStep  = 20;


type
  TacSVDisplayMode = (svmaDocked, svmaOverlay);
  TacSVCloseStyle  = (svcaCollapse, svcaCompact);
  TacSVPlacement   = (svpaLeft, svpaRight, svpaTop, svpaBottom);
  TacSVState       = (svsaOpened, svsaOpening, svsaClosed, svsaClosing);
  TacSVBlurMode    = (bmNone, bmSplitView, bmParent);


  TacBlurData = class(TPersistent)
  private
    FOpacity: byte;
    FColor: TColor;
    FOwner: TsCustomPanel;
    FMode: TacSVBlurMode;
    FSize: byte;
    procedure SetColor(const Value: TColor);
    procedure SetBytes(const Index: Integer; const Value: byte);
    procedure SetMode(const Value: TacSVBlurMode);
  public
    constructor Create(AOwner: TsCustomPanel);
    procedure Invalidate;
  published
    property Color: TColor read FColor write SetColor default clBtnFace;
    property Mode: TacSVBlurMode read FMode write SetMode default bmSplitView;
    property Opacity: byte index 0 read FOpacity write SetBytes default 127;
    property Size:    byte index 1 read FSize    write SetBytes default 10;
  end;


  TsSplitView = class(TsCustomPanel)
  private
    FVisibleSize,
    FOpenedSize,
    FCompactSize: Integer;

    FOnOpening,
    FOnOpened,
    FOnClosing,
    FOnClosed: TNotifyEvent;

    BGBmp,
    BluredBmp: TBitmap;

    Printing,
    FAutoHide,
    PosUpdating,
    StateChanging,
    FContentMoved,
    FAnimatedHiding,
    FAnimatedShowing: boolean;


    ParentBlur,
    FOverlayPadding: TsPanel;

    BGOffset:        TPoint;
    FAnimationTimer: TTimer;
    FState:          TacSVState;
    FBlurData:       TacBlurData;
    FPlacement:      TacSVPlacement;
    FCloseStyle:     TacSVCloseStyle;
    FDisplayMode:    TacSVDisplayMode;

    function GetOpened: Boolean;
    function CompSize: integer;

    procedure AnimationTimerHandler(Sender: TObject);

    procedure PrepareBGBmp;
    procedure UpdateBGBmp;
    procedure KillParentBlur;
    function CanAnimate(State: TacSVState): boolean;
    function CanAutoHide: boolean;
    function WindowSize: integer;
    function GetVisibleSize: integer;
    function Position: integer;
    procedure SetOpened(Value: Boolean);
    procedure SetOpenedSize(Value: Integer);
    procedure SetCompactSize(Value: Integer);
    procedure SetPlacement(Value: TacSVPlacement);
    procedure SetVisibleSize(const Value: integer);
    procedure SetCloseStyle(Value: TacSVCloseStyle);
    procedure SetDisplayMode(Value: TacSVDisplayMode);
    procedure SetContentMoved(const Value: boolean);
    procedure SetActBounds(aNewPos, aVisibleSize: integer);
    property VisibleSize: integer read GetVisibleSize write SetVisibleSize;
    procedure WMNCPaint(var Message: TMessage); message WM_NCPAINT;
    procedure PaintBorder(aDC: hdc);
    procedure ParentLock(DoLock: boolean; Redraw: boolean = False);
  protected
    procedure SetParent(AParent: TWinControl); override;
    procedure SetState(Value: TacSVState); virtual;
    procedure UpdatePosition;
    procedure UpdateParentBlur;
    procedure DoClosing;
    procedure DoOpening;
    procedure DoClosed;
    procedure DoOpened;
  public
    procedure WndProc(var Message: TMessage); override;
    constructor Create(AOwner: TComponent); override;
    procedure RefreshBackground(DoRepaint: boolean);
    procedure ChangeScale(M, D: Integer); override;
    function PrepareCache: boolean; override;
    procedure CreateWnd; override;
    destructor Destroy; override;
    procedure Loaded; override;
    procedure Close;
    procedure Open;
    property DockManager;
  published
    property AutoHide: boolean read FAutoHide write FAutoHide default False;
    property AnimatedShowing: boolean read FAnimatedShowing write FAnimatedShowing default True;
    property AnimatedHiding:  boolean read FAnimatedHiding  write FAnimatedHiding  default False;
    property ContentMoved:    boolean read FContentMoved    write SetContentMoved  default False;
    property BlurData: TacBlurData read FBlurData write FBlurData;

    property CloseStyle: TacSVCloseStyle read FCloseStyle write SetCloseStyle default svcaCollapse;
    /// <summary>Specifies the width of the control when closed and CloseStyle is set to svcCompact.</summary>
    property CompactSize: Integer read FCompactSize write SetCompactSize default DefSVCompactSize;
    /// <summary>Specifies how the TSplitView appears when opened.
    /// svmDocked - The control is docked to the left or right edge of the form, and the client area is reduced by the
    ///             width of the control.
    /// svmOverlay - The control is displayed on top of the client area of the form.</summary>
    property DisplayMode: TacSVDisplayMode read FDisplayMode write SetDisplayMode default svmaDocked;
    /// <summary>Specifies whether the TSplitView is currently open.</summary>
    property Opened: Boolean read GetOpened write SetOpened default True;
    /// <summary>Specifies the width of the TSplitView when in the opened state.</summary>
    property OpenedSize: Integer read FOpenedSize write SetOpenedSize nodefault;
    /// <summary>Specifies whether the TSplitView is placed on the left or right side of the form.</summary>
    property Placement: TacSVPlacement read FPlacement write SetPlacement nodefault;

    property OnClosed:  TNotifyEvent read FOnClosed  write FOnClosed;
    property OnClosing: TNotifyEvent read FOnClosing write FOnClosing;
    property OnOpened:  TNotifyEvent read FOnOpened  write FOnOpened;
    property OnOpening: TNotifyEvent read FOnOpening write FOnOpening;

    property BevelOuter; // default bvNone;
    property UseDockManager default True;
    property DoubleBuffered default True;
{$IFDEF DELPHI7UP}
    property ParentBackground default False;
{$ENDIF}
{$IFDEF D2010}
    property ParentDoubleBuffered default False;
{$ENDIF}
    property Alignment;
    property BevelEdges;
    property BevelInner;
    property BevelKind;
    property BevelWidth;
    property BiDiMode;
    property BorderWidth;
    property BorderStyle;
    property Caption;
    property Color;
    property Constraints;
    property Ctl3D;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FullRepaint;
    property Font;
    property Locked;
{$IFDEF D2010}
    property Padding;
    property ShowCaption;
    property Touch;
    property VerticalAlignment;
    {$IFDEF DELPHI_XE3}
    property StyleElements;
    {$ENDIF}
    property OnAlignInsertBefore;
    property OnAlignPosition;
    property OnGesture;
    property OnMouseActivate;
{$ENDIF}
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;

    property OnCanResize;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;


implementation


uses
  math, TypInfo, ComCtrls,
  {$IFDEF LOGGED} sDebugMsgs, {$ENDIF}
  {$IFNDEF ALITE} sSplitter, {$ENDIF}
  sSkinProps, sMessages, sGraphUtils, sVCLUtils, sSkinManager, sBorders, acntUtils, sMaskData, sAlphaGraph, sStyleSimply,
  sGradient, sThirdParty, acAlphaImageList, sSkinProvider, acgpUtils, acGlow;


procedure TsCustomPanel.AfterConstruction;
begin
  inherited;
  FCommonData.Loaded;
end;


procedure TsCustomPanel.CopyCache(DC: hdc);
begin
  CopyWinControlCache(Self, FCommonData, MkRect, MkRect(Self), DC, False);
end;


constructor TsCustomPanel.Create(AOwner: TComponent);
begin
  FCommonData := TsCtrlSkinData.Create(Self, True);
  FCommonData.COC := COC_TsPanel;
  inherited Create(AOwner);
end;


destructor TsCustomPanel.Destroy;
begin
  FreeAndNil(FCommonData);
  inherited Destroy;
end;


procedure TsCustomPanel.Loaded;
begin
  inherited;
  FCommonData.Loaded;
  FOldBevel := BevelOuter;
  if HandleAllocated then
    SetClassLong(Handle, GCL_HBRBACKGROUND, Integer(GetSysColorBrush(COLOR_BTNFACE)));
end;


procedure TsCustomPanel.OurPaint;
var
  R: TRect;
  C: TColor;
  i: integer;
  b: boolean;
  TmpCanvas: TCanvas;
  SavedDC, NewDC: HDC;
  ParentBG: TacBGInfo;
begin
  if (Showing or (FCommonData.PrintDC <> 0)) and FCommonData.Skinned then begin
    if DC <> 0 then
      NewDC := DC
    else
      NewDC := Canvas.Handle;

    if not SkinData.CustomColor or (Self is TsColorsPanel) or InAnimationProcess or Assigned(FOnPaint) then begin
      i := GetClipBox(NewDC, R);
      if i = ERROR {or IsRectEmpty(R) is not redrawn while resizing }then
        Exit;

      if i = NULLREGION then begin
        b := False;
        for I := 0 to ControlCount - 1 do
          if Controls[i].Align = alNone then begin
            b := True;
            Break;
          end;

        if b and RectVisible(NewDC, MkRect(Self)) then begin
          SkinData.FUpdating := True;
          Exit;
        end;
      end;

      if not InUpdating(FCommonData) or (csPaintCopy in ControlState) then begin
        // If transparent and parent is resized
        b := FCommonData.HalfVisible or FCommonData.BGChanged or FCommonData.FCacheBmp.Empty;
        if SkinData.RepaintIfMoved and not (csPaintCopy in ControlState) then
          FCommonData.HalfVisible := (WidthOf(R, True) <> Width) or (HeightOf(R, True) <> Height)
        else
          FCommonData.HalfVisible := False;

        if b and not PrepareCache and FCommonData.BGChanged then
          Exit;

        CopyCache(NewDC);
        sVCLUtils.PaintControls(NewDC, Self, b and SkinData.RepaintIfMoved, MkPoint, 0, FCommonData.PrintDC = 0);
      end;
    end
    else begin
      R := MkRect(Width, Height);
      FCommonData.FUpdating := False;
      i := SkinBorderMaxWidth(FCommonData);
      SavedDC := SaveDC(NewDC);
      ExcludeControls(NewDC, Self, 0, 0);
      if not SkinData.CustomColor then begin
        ParentBG.PleaseDraw := False;
        GetBGInfo(@ParentBG, Self.Parent);
        if ParentBG.BgType = btNotReady then begin
          SkinData.FUpdating := True;
          Exit;
        end;
      end
      else begin
        ParentBG.Color := ColorToRGB(Color);
        ParentBG.BgType := btFill;
      end;
      if (FCommonData.SkinManager.gd[FCommonData.SkinIndex].Props[0].Transparency = 100) and (ParentBG.BgType = btCache) then begin
        if not InUpdating(FCommonData) then
          if i = 0 then
            BitBlt(NewDC, 0, 0, Width, Height, ParentBG.Bmp.Canvas.Handle, ParentBG.Offset.X + Left, ParentBG.Offset.Y + Top, SRCCOPY)
          else begin
            if FCommonData.FCacheBmp = nil then
              FCommonData.FCacheBmp := CreateBmp32(Width, Height);

            R := PaintBorderFast(NewDC, R, i, FCommonData, 0);
            if FCommonData.SkinManager.gd[FCommonData.SkinIndex].Props[0].Transparency = 100 then
              FillDC(NewDC, R, ParentBG.Color)
            else
              FillDC(NewDC, R, GetBGColor(SkinData, 0));

            if i > 0 then
              BitBltBorder(NewDC, 0, 0, Width, Height, FCommonData.FCacheBmp.Canvas.Handle, 0, 0, i);

            BitBlt(NewDC, i, i, Width - 2 * i, Height - 2 * i, ParentBG.Bmp.Canvas.Handle, ParentBG.Offset.X + i, ParentBG.Offset.Y + i, SRCCOPY);
          end;
      end
      else begin
        case FCommonData.SkinManager.gd[FCommonData.SkinIndex].Props[0].Transparency of
          100: C := ParentBG.Color;
          0:   C := iff(FCommonData.CustomColor, Color, FCommonData.SkinManager.gd[FCommonData.SkinIndex].Props[0].Color)
          else C := BlendColors(ParentBG.Color,
                              iff(FCommonData.CustomColor, ColorToRGB(Color), FCommonData.SkinManager.gd[FCommonData.SkinIndex].Props[0].Color),
                              FCommonData.SkinManager.gd[FCommonData.SkinIndex].Props[0].Transparency * MaxByte div 100);
        end;
        if i = 0 then
          FillDC(NewDC, R, C)
        else begin
          if FCommonData.FCacheBmp = nil then
            FCommonData.FCacheBmp := CreateBmp32(Width, Height);

          if (FCommonData.SkinManager.gd[FCommonData.SkinIndex].Props[0].Transparency <> 100) or
               FCommonData.SkinManager.IsValidImgIndex(FCommonData.BorderIndex) and
                 (FCommonData.SkinManager.ma[FCommonData.BorderIndex].DrawMode and BDM_FILL = 0) then begin
            R := PaintBorderFast(NewDC, R, i, FCommonData, 0);
            FillDC(NewDC, R, C);
            BitBltBorder(NewDC, 0, 0, Width, Height, FCommonData.FCacheBmp.Canvas.Handle, 0, 0, i);
          end
          else begin // If filled by BorderMask
            PaintBorderFast(NewDC, R, 0, FCommonData, 0);
            BitBlt(NewDC, 0, 0, Width, Height, FCommonData.FCacheBmp.Canvas.Handle, 0, 0, SRCCOPY);
          end;
        end;
      end;
      R := ClientRect;
      i := BorderWidth + (integer(BevelInner <> bvNone) + integer(BevelOuter <> bvNone)) * BevelWidth;
      InflateRect(R, -i, -i);
      if DC = 0 then
        TmpCanvas := Canvas
      else begin
        TmpCanvas := TCanvas.Create;
        TmpCanvas.Handle := NewDC;
        TmpCanvas.Font.Assign(Font);
        TmpCanvas.Brush.Style := bsClear;
      end;
      WriteText(R, TmpCanvas);
      if Assigned(FOnPaint) then
        FOnPaint(Self, TmpCanvas);

      if DC <> 0 then
        FreeAndNil(TmpCanvas);

      RestoreDC(NewDC, SavedDC);
      sVCLUtils.PaintControls(NewDC, Self, True, MkPoint, 0, FCommonData.PrintDC = 0);
    end;
    if SendUpdated and not (csPaintCopy in ControlState) then
      SetParentUpdated(Self);
  end;
end;


procedure TsCustomPanel.Paint;
begin
  inherited;
  if Showing and Assigned(FOnPaint) then
    FOnPaint(Self, Canvas)
end;


procedure TsCustomPanel.PaintDragPanel(DC: hdc);
var
  DragSize, i, Ndx, dHeight, dWidth: integer;
  Captioned: boolean;
  CI: TCacheInfo;
  dRatio: real;
  Bmp: TBitmap;
  gR, R: TRect;
  s: acString;
begin
  if (DockManager <> nil) and (VisibleDockClientCount > 0) then begin
    Captioned := DefaultDockTreeClass.ClassName = 'TCaptionedDockTree';
    DragSize := iff(Captioned, 23, 12);
    for i := 0 to DockClientCount - 1 do
      if DockClients[i].Visible then begin
        R := DockClients[i].BoundsRect;
        OffsetRect(R, 0, -DragSize);
        R.Bottom := R.Top + DragSize;
        Bmp := CreateBmp32(WidthOf(R), DragSize);
        try
          if Captioned then
            if SkinData.SkinManager.ConstData.Sections[ssDragBar] < 0 then
              Ndx := SkinData.SkinManager.ConstData.Sections[ssPanel]
            else
              Ndx := SkinData.SkinManager.ConstData.Sections[ssDragBar]
          else
            if SkinData.SkinManager.ConstData.Sections[ssGripV] < 0 then
              Ndx := SkinData.SkinManager.ConstData.Sections[ssGripH]
            else
              Ndx := SkinData.SkinManager.ConstData.Sections[ssGripV];

          if Ndx >= 0 then begin
            CI := MakeCacheInfo(SkinData.FCacheBmp);
            PaintItem(Ndx, CI, True, 0, R, MkPoint, SkinData.FCacheBmp);
            if Captioned and HasProperty(DockClients[i], 'Caption') then begin
              s := GetStrProp(DockClients[i], 'Caption');
              gR := R;
              InflateRect(gR, -5, -5);
              dec(gR.Right, 18);
              acWriteTextEx(SkinData.FCacheBmp.Canvas, PAcChar(s), True, gR, DT_LEFT or DT_VCENTER or DT_END_ELLIPSIS, Ndx, False);
            end;
          end;

          Ndx := SkinData.SkinManager.ConstData.TitleGlyphs[tgSmallClose];
          if Ndx >= 0 then begin
            Bmp.Width  := SkinData.SkinManager.ma[Ndx].Width;
            Bmp.Height := SkinData.SkinManager.ma[Ndx].Height;
            if Bmp.Height > DragSize then
              dRatio := DragSize / Bmp.Height
            else
              dRatio := 1;

            dWidth  := Round(Bmp.Width  * dRatio);
            dHeight := Round(Bmp.Height * dRatio);

            gR.Top := R.Top + (DragSize - dHeight) div 2;
            gR.Bottom := gR.Top + dHeight;
            gR.Right := R.Right - (gR.Top - R.Top);
            gR.Left := gR.Right - dWidth;

            CI := MakeCacheInfo(SkinData.FCacheBmp, gR.Left, gR.Top);

            if dRatio = 1 then begin
              BitBlt(Bmp.Canvas.Handle, 0, 0, Bmp.Width, Bmp.Height, SkinData.FCacheBmp.Canvas.Handle, gR.Left, gR.Top, SRCCOPY);
              DrawSkinGlyph(Bmp, MkPoint, 0, 1, SkinData.SkinManager.ma[Ndx], CI);
              BitBlt(DC, gR.Left, gR.Top, Bmp.Width, Bmp.Height, Bmp.Canvas.Handle, 0, 0, SRCCOPY);
            end
            else begin
              StretchBlt(Bmp.Canvas.Handle, 0, 0, Bmp.Width, Bmp.Height, SkinData.FCacheBmp.Canvas.Handle, gR.Left, gR.Top, WidthOf(gR), HeightOf(gR), SRCCOPY);
              DrawSkinGlyph(Bmp, MkPoint, 0, 1, SkinData.SkinManager.ma[Ndx], CI);
              StretchBlt(DC, gR.Left, gR.Top, WidthOf(gR), HeightOf(gR), Bmp.Canvas.Handle, 0, 0, Bmp.Width, Bmp.Height, SRCCOPY);
            end;
          end;
        finally
          Bmp.Free
        end;
      end;
  end;
end;


procedure TsCustomPanel.PaintWindow(DC: HDC);
begin
  if (IsWindowVisible(Handle) or (csDesigning in ComponentState)) and not (csDestroying in ComponentState) then begin
    if not (csPaintCopy in ControlState) or not SkinData.Skinned then
      inherited;

    OurPaint(DC);
  end;
end;


function TsCustomPanel.PrepareCache: boolean;
var
  w: integer;
  R: TRect;
begin
  InitCacheBmp(SkinData);
  if not PaintSkinControl(FCommonData, Parent, True, 0, MkRect(Self), Point(Left, Top), FCommonData.FCacheBMP, True) then begin
    SkinData.FUpdating := True;
    Result := False;
  end
  else begin
    R := ClientRect;
    w := BorderWidth + integer(BevelInner <> bvNone) * BevelWidth + integer(BevelOuter <> bvNone) * BevelWidth;
    InflateRect(R, -w, -w);
    UpdateBmpColors(FCommonData.FCacheBmp, SkinData, True, 0, SkinData.ColorTone);
    SkinData.PaintOuterEffects(Self, MkPoint);
    WriteText(R, FCommonData.FCacheBmp.Canvas);
    if Assigned(FOnPaint) then
      FOnPaint(Self, FCommonData.FCacheBmp.Canvas);

    if DockSite then
      PaintDragPanel(FCommonData.FCacheBmp.Canvas.Handle);

    FCommonData.BGChanged := False;
    Result := True;
  end;
end;


procedure TsCustomPanel.WndProc(var Message: TMessage);
var
  SaveIndex: Integer;
  PS: TPaintStruct;
  DC: HDC;
begin
{$IFDEF LOGGED}
//  if Tag = 5 then
//    AddToLog(Message);
{$ENDIF}
  if Message.Msg = SM_ALPHACMD then
    case Message.WParamHi of
      AC_CTRLHANDLED: begin
        Message.Result := 1;
        Exit;
      end;

      AC_REMOVESKIN: begin
//        ControlStyle := ControlStyle - [csOpaque];
        if ACUInt(Message.LParam) = ACUInt(SkinData.SkinManager) then begin
          CommonMessage(Message, FCommonData);
          Invalidate;
        end;
        AlphaBroadCast(Self, Message);
        Exit;
      end;

      AC_SETNEWSKIN: begin
//        ControlStyle := ControlStyle - [csOpaque];
        AlphaBroadCast(Self, Message);
        if ACUInt(Message.LParam) = ACUInt(SkinData.SkinManager) then
          CommonMessage(Message, FCommonData);

        Exit;
      end;

      AC_REFRESH: begin
        if ACUInt(Message.LParam) = ACUInt(SkinData.SkinManager) then begin
          CommonMessage(Message, FCommonData);
          if HandleAllocated then
            SetClassLong(Handle, GCL_HBRBACKGROUND, Integer(SkinData.SkinManager.Brushes[pcMainColor]));

          if HandleAllocated and not (csLoading in ComponentState) and Visible then begin
            InvalidateRect(Handle, nil, True);
            RedrawWindow(Handle, nil, 0, RDWA_NOCHILDRENNOW);
            AlphaBroadCast(Self, Message);
          end;
        end
        else
          AlphaBroadCast(Self, Message);

        Exit;
      end;

      AC_GETBG:
        if (SkinData.SkinManager <> nil) and SkinData.SkinManager.IsValidSkinIndex(SkinData.SkinIndex) then begin
          InitBGInfo(FCommonData, PacBGInfo(Message.LParam), 0);
          if not (PacBGInfo(Message.LParam)^.BgType in [btNotReady, btFill]) then
            with PacBGInfo(Message.LParam)^ do begin
              // If BG is not ready yet
              if SkinData.BGChanged and ((SkinData.FCacheBmp.Width <> Width) or (SkinData.FCacheBmp.Height <> Height)) and not SkinData.Updating then
                if (Parent = nil) or not Parent.HandleAllocated or GetBoolMsg(Parent.Handle, ac_CtrlHandled) then begin // If parent is skinned
                  BgType := btNotReady;
                  Exit;
                end;

              if (WidthOf(ClientRect) <> Width) and (BgType = btCache) and not PleaseDraw then begin
                SaveIndex := BorderWidth + BevelWidth * (integer(BevelInner <> bvNone) + integer(BevelOuter <> bvNone));
                inc(Offset.X, SaveIndex);
                inc(Offset.Y, SaveIndex);
              end;
            end;

          Exit;
        end;

      AC_GETDEFINDEX: begin
        if FCommonData.SkinManager <> nil then
          with FCommonData.SkinManager.ConstData, Message do
            case BevelOuter of
              bvRaised:  Result := 1 + Sections[ssPanel];
              bvLowered: Result := 1 + Sections[ssPanelLow];
              bvSpace:   Result := 1 + Sections[ssGroupBox];
              bvNone:    Result := 1 + Sections[ssTransparent];
            end;

        Exit;
      end;
    end;

  if not ControlIsReady(Self) or not FCommonData.Skinned then
    case Message.Msg of
      WM_PRINT:
        if Assigned(OnPaint) then begin
          OnPaint(Self, Canvas);
          if TWMPaint(Message).DC <> 0 then
            BitBlt(TWMPaint(Message).DC, 0, 0, Width, Height, Canvas.Handle, 0, 0, SRCCOPY);
        end
        else
          Perform(WM_PAINT, Message.WParam, Message.LParam);

      WM_ERASEBKGND:
        if not Assigned(FOnPaint) then
          inherited;

      else
        inherited;
    end
  else begin
    case Message.Msg of
      SM_ALPHACMD:
        case Message.WParamHi of
          AC_ENDPARENTUPDATE: begin
            if FCommonData.FUpdating then begin
              if not InUpdating(FCommonData, True) then
                if ((Width <> FCommonData.FCacheBmp.Width) or (Height <> FCommonData.FCacheBmp.Height)) and Assigned(OnResize) then begin
                  Repaint;
                  OnResize(Self);
                end
                else
                  Repaint;

              SetParentUpdated(Self);
            end;
            Exit;
          end;

          AC_PREPARECACHE: begin
            if not DockSite then
              if not InUpdating(SkinData) and SkinData.BGChanged then
                if not PrepareCache then
                  SkinData.FUpdating := True;

            Exit;
          end

          else
            if CommonMessage(Message, FCommonData) then
              Exit;
        end;

      WM_PRINT: begin
        FCommonData.FUpdating := False;
        FCommonData.HalfVisible := False;
        if ControlIsReady(Self) then begin
          if SkinData.CustomColor or FCommonData.BGChanged then
            PrepareCache;

          OurPaint(TWMPaint(Message).DC, False);
        end;
        Exit;
      end;

      WM_PAINT:
        if Visible or (csDesigning in ComponentState) then begin
          BeginPaint(Handle, PS);
          if not InUpdating(FCommonData) then begin
            if TWMPAINT(Message).DC = 0 then
              DC := GetDC(Handle)
            else
              DC := TWMPAINT(Message).DC;

            try
              SaveIndex := SaveDC(DC);
              ControlState := ControlState + [csCustomPaint];
              Canvas.Lock;
              Canvas.Handle := DC;
              try
                TControlCanvas(Canvas).UpdateTextFlags;
                IntersectClipRect(DC, PS.rcPaint.Left, PS.rcPaint.Top, PS.rcPaint.Right, PS.rcPaint.Bottom);
                OurPaint(DC);
              finally
                Canvas.Handle := 0;
                Canvas.Unlock;
              end;
              RestoreDC(DC, SaveIndex);
            finally
              ControlState := ControlState - [csCustomPaint];
              if TWMPaint(Message).DC = 0 then
                ReleaseDC(Handle, DC);
            end;
          end;
          EndPaint(Handle, PS);
          Message.Result := 0;
          Exit;
        end;

      CM_UNDOCKCLIENT, CM_CONTROLLISTCHANGE, WM_CAPTURECHANGED:
        if DockSite then
          FCommonData.BGChanged := True;

      CM_TEXTCHANGED: begin
        if Parent <> nil then
          FCommonData.Invalidate;

        Exit;
      end;

      WM_ERASEBKGND: begin
        if not (csPaintCopy in ControlState) and (Message.WParam <> WParam(Message.LParam) {PerformEraseBackground, TntSpeedButtons}) then begin
          if csDesigning in ComponentState then
            inherited;
        end
        else
          if Message.WParam <> 0 then begin // From PaintTo
            if FCommonData.BGChanged then
              PrepareCache;

            if not FCommonData.BGChanged then
              BitBlt(TWMPaint(Message).DC, 0, 0, Width, Height, FCommonData.FCacheBmp.Canvas.Handle, 0, 0, SRCCOPY)
          end;

        Message.Result := 1;
        Exit;
      end;

      CM_VISIBLECHANGED: begin
        FCommonData.BGChanged := True;
        FCommonData.FUpdating := False;
        if Visible then begin
          PrepareCache;
          inherited;
          SetParentUpdated(Self);
        end
        else
          inherited;

        Exit;
      end;

      CM_INVALIDATE:
        if FOldBevel <> BevelOuter then begin
          FCommonData.UpdateIndexes;
          FOldBevel := BevelOuter;
        end;

      CM_COLORCHANGED:
        if SkinData.CustomColor then
          SkinData.BGChanged := True;

      WM_KILLFOCUS, WM_SETFOCUS: begin
        inherited;
        Exit
      end;

      CM_SHOWINGCHANGED:
        if Visible then
          FCommonData.FUpdating := False;

      WM_WINDOWPOSCHANGING:
        if (SkinData.SkinManager.gd[SkinData.SkinIndex].Props[0].Transparency <> 0) then
          if (TWMWindowPosChanging(Message).WindowPos.flags and SWP_NOMOVE = 0) then
            FCommonData.BGChanged := True;

      WM_SIZE: begin
        FCommonData.BGChanged := True;
        if HandleAllocated and IsWindowVisible(Handle) then begin
          CommonWndProc(Message, FCommonData);
          FCommonData.Updating := True;
          inherited;
          FCommonData.Updating := False;
          SetParentUpdated(Self);
          Exit;
        end;
      end;
    end;
    CommonWndProc(Message, FCommonData);
    inherited;
    case Message.Msg of
      WM_WINDOWPOSCHANGED, WM_SIZE:
        if not InUpdating(FCommonData) then
          if Assigned(OnResize) and (Message.Msg = WM_SIZE) then
            OnResize(Self);

      CM_ENABLEDCHANGED:
        FCommonData.Invalidate;

      WM_SETFONT:
        if Caption <> '' then begin
          FCommonData.BGChanged := True;
          Repaint;
        end;
    end;
  end;
{$IFNDEF D2010}
  case Message.Msg of
    CM_MOUSEENTER: if Assigned(FOnMouseEnter) then FOnMouseEnter(Self);
    CM_MOUSELEAVE: if Assigned(FOnMouseLeave) then FOnMouseLeave(Self);
  end;
{$ENDIF}
end;


procedure TsCustomPanel.WriteText(R: TRect; aCanvas: TCanvas = nil; aDC: hdc = 0);
var
  C: TCanvas;
  TmpRect: TRect;
  Flags: Cardinal;
begin
{$IFDEF D2009}
  if not ShowCaption then
    Exit;
{$ENDIF}
  if Caption <> '' then begin
    if aCanvas = nil then
      if aDC <> 0 then begin
        C := TCanvas.Create;
        C.Handle := aDC;
      end
      else
        Exit
    else
      C := aCanvas;

    TmpRect := R;
    C.Font.Assign(Font);
{$IFDEF D2005}
    if VerticalAlignment <> taVerticalCenter then begin
      Flags := GetStringFlags(Self, alignment) or DT_SINGLELINE;
      Flags := Flags and not DT_VCENTER and not DT_WORDBREAK;
      if VerticalAlignment = taAlignBottom then
        Flags := Flags or DT_BOTTOM;

      acWriteTextEx(C, PacChar(Caption), Enabled, R, Flags, FCommonData, False);
    end
    else
{$ENDIF}
    begin
      Flags := GetStringFlags(Self, alignment) or DT_WORDBREAK;
      acDrawText(C.Handle, Caption, TmpRect, Flags or DT_CALCRECT);
      R.Top := R.Top + (HeightOf(R) - HeightOf(TmpRect, True)) div 2 + BorderWidth;
      R.Bottom := R.Top + HeightOf(TmpRect, True);
      acWriteTextEx(C, PacChar(Caption), Enabled, R, Flags, FCommonData, False);
    end;
    if (aCanvas = nil) and (C <> nil) then
      FreeAndNil(C);
  end;
end;


constructor TsDragBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SkinData.COC := COC_TsDragBar;
  Caption := s_Space;
  Align := alTop;
  Height := 20;
  Color := clActiveCaption;
  Cursor := crHandPoint;
{$IFDEF DELPHI7UP}
  ParentBackGround := False;
{$ENDIF}
end;


procedure TsDragBar.Loaded;
begin
  inherited;
  if not ParentFont then begin
    if not SkinData.Skinned then
      Font.Color := clCaptionText;

    Font.Style := [fsBold];
  end;
end;


procedure TsDragBar.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, x, y);
  if (Button = mbLeft) and (FDraggedControl <> nil) then begin
    ReleaseCapture;
    FDraggedControl.Perform(WM_SYSCOMMAND, SC_DRAGMOVE, 0);
    if Assigned(OnClick) then
      OnClick(Self);

    if Assigned(OnMouseUp) then
      OnMouseUp(Self, Button, Shift, X, Y);
  end
end;


procedure TsDragBar.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
    if AComponent = DraggedControl then
      DraggedControl := nil
end;


procedure TsDragBar.ReadState(Reader: TReader);
begin
  if (Reader.Parent <> nil) and (DraggedControl = nil) then
    DraggedControl := GetParentForm(TControl(Reader.Parent));

  inherited ReadState(Reader);
end;


procedure TsDragBar.WMActivateApp(var Message: TWMActivateApp);
begin
  Font.Color := iff(Message.Active, clActiveCaption, clInActiveCaption);
end;


procedure TsDragBar.WndProc(var Message: TMessage);
begin
  inherited;
  if Message.Msg = SM_ALPHACMD then
    case Message.WParamHi of
      AC_REMOVESKIN: begin
        Color := clActiveCaption;
        Font.Color := clCaptionText;
{$IFDEF DELPHI7UP}
        ParentBackGround := False;
{$ENDIF}
      end;
    end;
end;


constructor TsGrip.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Caption := s_Space;
  Transparent := False;
  SkinData.SkinSection := s_Transparent;
  Align := alNone;
  Height := 20;
  Width := 20;
end;


procedure TsGrip.Paint;
var
  CI: TCacheInfo;
  BG: TacBGInfo;
begin
  if ControlIsReady(Self) then begin
    SkinData.BGChanged := False;
    CI.Ready := False;
    if Transparent and (LinkedControl <> nil) then begin
      BG.PleaseDraw := False;
      GetBGInfo(@BG, LinkedControl);
      CI := BGInfoToCI(@BG);
    end;
    if CI.Ready then
      BitBlt(Canvas.Handle, 0, 0, Width, Height, CI.Bmp.Canvas.Handle, CI.Bmp.Width - Width + CI.X, CI.Bmp.Height - Height + CI.Y, SRCCOPY)
    else
      FillDC(Canvas.Handle, MkRect(Width, Height), CI.FillColor);
  end;
end;


procedure TsColorsPanel.AfterConstruction;
begin
  inherited;
  GenerateColors;
end;


procedure TsColorsPanel.ChangeScale(M, D: Integer);
begin
  inherited;
  if M <> D then begin
    FItemWidth  := MulDiv(FItemWidth,  M, D);
    FItemHeight := MulDiv(FItemHeight, M, D);
    ItemMargin  := MulDiv(ItemMargin,  M, D);
  end;
end;


function TsColorsPanel.ColorValue: TColor;
begin
  if FItemIndex < 0 then
    Result := clWhite
  else
    Result := ColorsArray[FItemIndex].ciColor;
end;


function TsColorsPanel.Count: integer;
begin
  Result := FColors.Count;
end;


constructor TsColorsPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Caption := s_Space;
  FColors := TStringList.Create;
  FItemIndex := -1;
  ItemHeight := 21;
  ItemWidth := 21;
  FColCount := 5;
  FRowCount := 2;
  FItemMargin := 6;
  Height := 60;
  Width := 140;
end;


destructor TsColorsPanel.Destroy;
begin
  FreeAndNil(FColors);
  inherited Destroy;
end;


procedure TsColorsPanel.GenerateColors;
var
  l, i, x, y: integer;
  s: string;
begin
  SetLength(ColorsArray, 0);
  i := 0;
  for y := 0 to RowCount - 1 do
    for x := 0 to ColCount - 1 do begin
      SetLength(ColorsArray, i + 1);
      with ColorsArray[i] do begin
        if i < FColors.Count then begin
          s := ExtractWord(1, FColors[i], [#13, #10, s_Space]);
          l := integer(HexToInt(s));
          ciColor := $FFFFFF and SwapRedBlue(l);
        end
        else begin
          ciColor := SwapInteger(ColorToRgb(clWhite));
          FColors.Add('FFFFFF');
        end;
        ciRect.Left   := ItemMargin + x * (ItemWidth + ItemMargin);
        ciRect.Top    := ItemMargin + y * (ItemHeight + ItemMargin);
        ciRect.Right  := ciRect.Left + ItemWidth;
        ciRect.Bottom := ciRect.Top + ItemHeight;
      end;
      inc(i);
    end;
end;


function TsColorsPanel.GetItemByCoord(p: TPoint): integer;
var
  i: integer;
  R: TRect;
begin
  Result := - 1;
  for i := 0 to Count - 1 do begin
    R := ColorsArray[i].ciRect;
    InflateRect(R, ItemMargin, ItemMargin);
    if PtInRect(R, p) then begin
      Result := i;
      Exit;
    end
  end;
end;


procedure TsColorsPanel.Loaded;
begin
  inherited;
  GenerateColors;
end;


procedure TsColorsPanel.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if CanFocus and IsWindowVisible(Handle) then
    SetFocus;

  ItemIndex := GetItemByCoord(Point(x, y));
end;


procedure TsColorsPanel.OurPaint;
var
  R: TRect;
  NewDC: hdc;
  Brush: TBrush;
begin
  if DC <> 0 then
    NewDC := DC
  else
    NewDC := Canvas.Handle;

  if not (csDestroying in ComponentState) and not (csCreating in Parent.ControlState) then begin
    if FCommonData.Skinned then begin
      if not InUpdating(FCommonData) then begin
        InitCacheBmp(SkinData);
        // If transparent and in a form resizing
        PaintItem(FCommonData, GetParentCache(FCommonData), False, 0, MkRect(Self), Point(Left, Top), FCommonData.FCacheBMP, False);
        WriteText(ClientRect);
        FCommonData.BGChanged := False;
        if not Assigned(FOnPaint) then
          PaintColors(FCommonData.FCacheBmp.Canvas.Handle);

        if Assigned(FOnPaint) then
          FOnPaint(Self, FCommonData.FCacheBmp.Canvas);

        CopyWinControlCache(Self, FCommonData, MkRect, MkRect(Self), NewDC, True);
        sVCLUtils.PaintControls(NewDC, Self, True, MkPoint);

        if SendUpdated then
          SetParentUpdated(Self);
      end;
    end
    else begin
      inherited;
      Perform(WM_NCPAINT, 0, 0);
      if not Assigned(FOnPaint) then
        PaintColors(NewDC);
    end;
    // Selected item
    if (FItemIndex >= 0) and not Assigned(FOnPaint) then begin
      R := ColorsArray[FItemIndex].ciRect;
      Brush := TBrush.Create;
      Brush.Style := bsSolid;
      Brush.Color := clWhite;
      InflateRect(R, 1, 1);
      FrameRect(NewDC, R, Brush.Handle);
      InflateRect(R, 1, 1);
      Brush.Color := 0;
      FrameRect(NewDC, R, Brush.Handle);
      if Focused then begin
        Brush.Color := clWhite;
        InflateRect(R, 2, 2);
        DrawFocusRect(NewDC, R);
      end;
      FreeAndNil(Brush);
    end;
  end;
end;


procedure TsColorsPanel.PaintColors(const DC: hdc);
var
  i: integer;
  bordColor: TColor;
begin
  if SkinData.Skinned then
    bordColor := SkinData.SkinManager.Palette[pcBorder]
  else
    bordColor := clBtnShadow;

  for i := 0 to Length(ColorsArray) - 1 do
    with ColorsArray[i] do begin
      FillDC(DC, ciRect, ciColor);
      FillDCBorder(DC, ciRect, 1, 1, 1, 1, bordColor);
    end;
end;


procedure TsColorsPanel.SetColors(const Value: TStrings);
begin
  FColors.Assign(Value);
  GenerateColors;
  SkinData.Invalidate;
end;


procedure TsColorsPanel.SetInteger(const Index, Value: integer);

  procedure ChangeProp(var Prop: integer; Value: integer);
  begin
    if Prop <> Value then begin
      Prop := Value;
      if not (csLoading in ComponentState) then begin
        GenerateColors;
        SkinData.Invalidate;
      end;
    end;
  end;

begin
  case Index of
    0: ChangeProp(FColCount,   Value);
    2: ChangeProp(FItemHeight, Value);
    3: ChangeProp(FItemWidth,  Value);
    4: ChangeProp(FItemMargin, Value);
    5: ChangeProp(FRowCount,   Value);
    1: begin
      if FItemIndex >= Count then
        FItemIndex := - 1;

      if FItemIndex <> Value then begin
        OldSelected := FItemIndex;
        FItemIndex := Value;
        if Assigned(FOnChange) then
          FOnChange(Self);

        Repaint;
      end;
    end;
  end;
end;


procedure TsColorsPanel.WndProc(var Message: TMessage);
begin
  inherited;
  case Message.Msg of
    WM_SETFOCUS, WM_KILLFOCUS:
      if FItemIndex >= 0 then
        Repaint;
  end;
end;


procedure TsGradientPanel.CopyCache(DC: hdc);
var
  i: integer;
  R, cR: TRect;
  SaveIndex: HDC;
  Child: TControl;
begin
  SaveIndex := SaveDC(DC);
  cR := MkRect(Self);
  try
    for i := 0 to ControlCount - 1 do begin
      Child := Controls[i];
      if (Child is TGraphicControl) and ((DefaultManager = nil) or DefaultManager.Options.StdImgTransparency) {$IFNDEF ALITE}or (Child is TsSplitter){$ENDIF} then
        Continue;

      with Child do begin
        R := BoundsRect;
        if Visible and RectIsVisible(cR, R) then
          if (csOpaque in ControlStyle) or (Child is TGraphicControl) then
            ExcludeClipRect(DC, R.Left, R.Top, R.Right, R.Bottom);
      end;
    end;
    BitBlt(DC, 0, 0, Width, Height, FCacheBmp.Canvas.Handle, 0, 0, SRCCOPY);
  finally
    RestoreDC(DC, SaveIndex);
  end;
end;


constructor TsGradientPanel.Create(AOwner: TComponent);
begin
  inherited;
  BGChanged := True;
  FCacheBmp := CreateBmp32;
  FPaintData := TacGradPaintData.Create(Self);
end;


destructor TsGradientPanel.Destroy;
begin
  FCacheBmp.Free;
  FPaintData.Free;
  inherited;
end;


procedure TsGradientPanel.Loaded;
begin
  inherited;
  ControlStyle := ControlStyle - [csOpaque];
end;


procedure TsGradientPanel.OurPaint(DC: HDC; SendUpdated: boolean);
var
  R: TRect;
  i: integer;
  b: boolean;
  NewDC: HDC;
begin
  if Showing then begin
    if DC <> 0 then
      NewDC := DC
    else
      NewDC := Canvas.Handle;

    b := BGChanged;
    if BGChanged and not PrepareCache then
      Exit;

    i := GetClipBox(NewDC, R);
    if i = 0 {do not redraw while resizing }then
      Exit;

    CopyCache(NewDC);
    sVCLUtils.PaintControls(NewDC, Self, b, MkPoint);

    if SendUpdated and not (csPaintCopy in ControlState) then
      SetParentUpdated(Self);
  end;
end;



procedure TsGradientPanel.Paint;
begin
  inherited;
  if Showing and Assigned(FOnPaint) then
    FOnPaint(Self, Canvas)
end;


procedure TsGradientPanel.PaintWindow(DC: HDC);
begin
  if not (csPaintCopy in ControlState) then
    inherited;

  OurPaint(DC);
end;


function TsGradientPanel.PrepareCache: boolean;
var
  R: TRect;
  i: integer;
  Skinned: boolean;
  CI: TCacheInfo;
  s, s1, s2: string;
begin
  FCacheBmp.PixelFormat := pf32bit;
  FCacheBmp.Width := Width;
  FCacheBmp.Height := Height;

  R := MkRect(FCacheBmp);
  Skinned := (DefaultManager <> nil) and DefaultManager.CommonSkinData.Active;
  if Skinned then begin
    CI := GetParentCacheHwnd(Handle);
    PaintItem(DefaultManager.ConstData.Sections[ssTransparent], CI, True, 0, MkRect(Self), Point(Left, Top), FCacheBmp, DefaultManager)
  end
  else
    FillDC(FCacheBmp.Canvas.Handle, MkRect(FCacheBmp), TacAccessControl(Parent).Color);
  // Paint here
  if PaintData.CustomGradient = '' then begin
    if FPaintData.Color1.UseSkinColor and Skinned then
      s1 := IntToStr(DefaultManager.Palette[pcMainColor])
    else
      s1 := IntToStr(Cardinal(FPaintData.Color1.Color));

    if FPaintData.Color2.UseSkinColor and Skinned then
      s2 := IntToStr(DefaultManager.Palette[pcMainColor])
    else
      s2 := IntToStr(Cardinal(FPaintData.Color2.Color));

    i := integer(not PaintData.IsVertical);
    if (FPaintData.Color1.Color and $FF000000 <> 0) or (FPaintData.Color2.Color and $FF000000 <> 0) then begin
      i := i or $10;
      if (DefaultManager <> nil) and DefaultManager.CommonSkinData.Active then
        PaintItem(DefaultManager.ConstData.Sections[ssTransparent], GetParentCacheHwnd(Handle), True, 0, R, Point(Left, Top), FCacheBmp, DefaultManager)
      else
        FillDC(FCacheBmp.Canvas.Handle, MkRect(Self), TacAccessControl(Parent).Color);
    end;

    s := s1 + ';0;' + IntToStr(i) + CharSemicolon;
    s := s + s2 + ';100;' + IntToStr(i) + CharSemicolon + #$42;
    PaintGrad(FCacheBmp, R, s);
  end
  else
    PaintGrad(FCacheBmp, R, PaintData.CustomGradient);

  FCacheBmp.Canvas.Brush.Style := bsClear;
  WriteText(R, FCacheBmp.Canvas);
  if Assigned(FOnPaint) then
    FOnPaint(Self, FCacheBmp.Canvas);

  BGChanged := False;
  Result := True;
end;


procedure TsGradientPanel.WndProc(var Message: TMessage);
var
  SaveIndex: Integer;
  PS: TPaintStruct;
  i: integer;
  DC: HDC;
begin
{$IFDEF LOGGED}
//  AddToLog(Message);
{$ENDIF}
  if Message.Msg = SM_ALPHACMD then
    case Message.WParamHi of
      AC_CTRLHANDLED: begin
        Message.Result := 1;
        Exit;
      end;

      AC_REMOVESKIN: begin
        if ACUInt(Message.LParam) = ACUInt(DefaultManager) then begin
          Invalidate;
        end;
        AlphaBroadCast(Self, Message);
        Exit;
      end;

      AC_SETNEWSKIN: begin
        AlphaBroadCast(Self, Message);
        Exit;
      end;

      AC_REFRESH: begin
        if ACUInt(Message.LParam) = ACUInt(DefaultManager) then begin
          if HandleAllocated and not (csLoading in ComponentState) and Visible then begin
            InvalidateRect(Handle, nil, True);
            RedrawWindow(Handle, nil, 0, RDWA_NOCHILDRENNOW);
            AlphaBroadCast(Self, Message);
          end;
        end
        else
          AlphaBroadCast(Self, Message);

        Exit;
      end;

      AC_GETBG: begin
        if not BGChanged or PrepareCache then begin
          PacBGInfo(Message.LParam)^.Bmp := FCacheBmp;
          PacBGInfo(Message.LParam)^.Offset := MkPoint;
          PacBGInfo(Message.LParam)^.R := MkRect;
          PacBGInfo(Message.LParam)^.BgType := btCache;
        end;
        Exit;
      end;

      AC_SETBGCHANGED:
        BGChanged := True;

      AC_CHILDCHANGED:
        Message.Result := 1;

      AC_GETFONTINDEX:
//        if SkinData.Skinned and (Parent <> nil) then }
        begin
          Message.Result := GetFontIndex(Parent, PacPaintInfo(Message.LParam));
//          PacPaintInfo(Message.LParam)^.FontIndex := SkinData.SkinIndex;
          Message.Result := 1;
          Exit;
        end;

      AC_SETCHANGEDIFNECESSARY:
        if HandleAllocated and Showing then begin
          BGChanged := True;
          if Message.WParamLo = 1 then
            RedrawWindow(Handle, nil, 0, RDWA_ALLCHILDREN);

          for i := 0 to ControlCount - 1 do
            if not (csDestroying in Controls[i].ComponentState) then
              Controls[i].Perform(SM_ALPHACMD, AC_SETCHANGEDIFNECESSARY shl 16 + 1, 0);
        end;
    end;

  if not ControlIsReady(Self) then
    case Message.Msg of
      WM_PRINT:
        if Assigned(OnPaint) then begin
          OnPaint(Self, Canvas);
          if TWMPaint(Message).DC <> 0 then
            BitBlt(TWMPaint(Message).DC, 0, 0, Width, Height, Canvas.Handle, 0, 0, SRCCOPY);
        end
        else
          Perform(WM_PAINT, Message.WParam, Message.LParam);

      WM_ERASEBKGND:
        if not Assigned(FOnPaint) then
          inherited;

      else
        inherited;
    end
  else begin
    case Message.Msg of
      SM_ALPHACMD: begin
        case Message.WParamHi of
          AC_ENDPARENTUPDATE:
            SetParentUpdated(Self);

          AC_PREPARECACHE:
            PrepareCache;
        end;
        Exit;
      end;

      WM_PRINT: begin
        if ControlIsReady(Self) then begin
          DC := TWMPaint(Message).DC;
          PrepareCache;
          OurPaint(DC, False);
        end;
        Exit;
      end;

      WM_PAINT:
        if Visible or (csDesigning in ComponentState) then begin
          BeginPaint(Handle, PS);
          if TWMPAINT(Message).DC = 0 then
            DC := GetDC(Handle)
          else
            DC := TWMPAINT(Message).DC;

          try
            SaveIndex := SaveDC(DC);
            ControlState := ControlState + [csCustomPaint];
            Canvas.Lock;
            Canvas.Handle := DC;
            try
              TControlCanvas(Canvas).UpdateTextFlags;
              OurPaint(DC);
            finally
              Canvas.Handle := 0;
              Canvas.Unlock;
            end;
            RestoreDC(DC, SaveIndex);
          finally
            ControlState := ControlState - [csCustomPaint];
            if TWMPaint(Message).DC = 0 then
              ReleaseDC(Handle, DC);
          end;
          EndPaint(Handle, PS);
          Message.Result := 0;
          Exit;
        end;

      CM_TEXTCHANGED: begin
        BGChanged := True;
        Exit;
      end;

      WM_ERASEBKGND: begin
        if not (csPaintCopy in ControlState) and (Message.WParam <> WParam(Message.LParam) {PerformEraseBackground, TntSpeedButtons}) then begin
          if csDesigning in ComponentState then
            inherited;
        end
        else
          if Message.WParam <> 0 then begin // From PaintTo
            if BGChanged then
              PrepareCache;

            if not BGChanged then
              CopyCache(TWMPaint(Message).DC);
          end;

        Message.Result := 1;
        Exit;
      end;

      CM_VISIBLECHANGED: begin
        BGChanged := True;
        if Visible then begin
          PrepareCache;
          inherited;
          SetParentUpdated(Self);
        end
        else
          inherited;

        Exit;
      end;

      WM_KILLFOCUS, WM_SETFOCUS: begin
        inherited;
        Exit
      end;

      WM_WINDOWPOSCHANGED, WM_SIZE:
        BGChanged := True;
    end;
    inherited;
    case Message.Msg of
      WM_WINDOWPOSCHANGED, WM_SIZE:
        if Assigned(OnResize) and (Message.Msg = WM_SIZE) then
          OnResize(Self);

      WM_SETFONT:
        if Caption <> '' then begin
          BGChanged := True;
          Repaint;
        end;
    end;
  end;
{$IFNDEF D2010}
  case Message.Msg of
    CM_MOUSEENTER: if Assigned(FOnMouseEnter) then FOnMouseEnter(Self);
    CM_MOUSELEAVE: if Assigned(FOnMouseLeave) then FOnMouseLeave(Self);
  end;
{$ENDIF}
end;


procedure TsGradientPanel.WriteText(R: TRect; aCanvas: TCanvas; aDC: hdc);
var
  C: TCanvas;
  TmpRect: TRect;
  Flags: Cardinal;
begin
{$IFDEF D2009}
  if not ShowCaption then
    Exit;
{$ENDIF}
  if Caption <> '' then begin
    if aCanvas = nil then
      if aDC <> 0 then begin
        C := TCanvas.Create;
        C.Handle := aDC;
      end
      else
        Exit
    else
      C := aCanvas;

    TmpRect := R;
    C.Font.Assign(Font);
{$IFDEF D2005}
    if VerticalAlignment <> taVerticalCenter then begin
      Flags := GetStringFlags(Self, alignment) or DT_SINGLELINE;
      Flags := Flags and not DT_VCENTER and not DT_WORDBREAK;
      if VerticalAlignment = taAlignBottom then
        Flags := Flags or DT_BOTTOM;
    end
    else
{$ENDIF}
    begin
      Flags := GetStringFlags(Self, alignment) or DT_WORDBREAK;
      acDrawText(C.Handle, Caption, TmpRect, Flags or DT_CALCRECT);
      R.Top := R.Top + (HeightOf(R) - HeightOf(TmpRect, True)) div 2 + BorderWidth;
      R.Bottom := R.Top + HeightOf(TmpRect, True);
    end;
    acWriteText(C, PacChar(Caption), Enabled, R, Flags);
    if (aCanvas = nil) and (C <> nil) then
      FreeAndNil(C);
  end;
end;


constructor TacGradPaintData.Create(AOwner: TsGradientPanel);
begin
  FOwner := AOwner;
  FIsVertical := True;
  FColor1 := TacColorData1.Create(Self);
  FColor2 := TacColorData2.Create(Self);
end;


destructor TacGradPaintData.Destroy;
begin
  FColor2.Free;
  FColor1.Free;
  inherited;
end;


procedure TacGradPaintData.Invalidate;
begin
  FOwner.BGChanged := True;
  if [csLoading, csDestroying] * FOwner.ComponentState = [] then
    FOwner.Invalidate;
end;


procedure TacGradPaintData.SetColor1(const Index: Integer; const Value: TacColorData1);
begin
  if FColor1 <> Value then begin
    FColor1 := Value;
    Invalidate;
  end;
end;


procedure TacGradPaintData.SetColor2(const Index: Integer; const Value: TacColorData2);
begin
  if FColor2 <> Value then begin
    FColor2 := Value;
    Invalidate;
  end;
end;


procedure TacGradPaintData.SetCustomGradient(const Value: string);
begin
  if FCustomGradient <> Value then begin
    FCustomGradient := Value;
    Invalidate;
  end;
end;


procedure TacGradPaintData.SetIsVertical(const Value: boolean);
begin
  if FIsVertical <> Value then begin
    FIsVertical := Value;
    Invalidate;
  end;
end;


constructor TacColorData1.Create(AOwner: TacGradPaintData);
begin
  FOwner := AOwner;
  FUseSkinColor := False;
  FColor := clWhite;
end;


procedure TacColorData1.Invalidate;
begin
  FOwner.Invalidate;
end;


procedure TacColorData1.SetColor(const Value: TColor);
begin
  if FColor <> Value then begin
    FColor := Value;
    Invalidate;
  end;
end;


procedure TacColorData1.SetUseSkinColor(const Value: boolean);
begin
  if FUseSkinColor <> Value then begin
    FUseSkinColor := Value;
    Invalidate;
  end;
end;


constructor TacColorData2.Create(AOwner: TacGradPaintData);
begin
  FOwner := AOwner;
  FColor := clBtnFace;
  FUseSkinColor := True;
end;


function TsRollOutPanel.ActualButtonHeight: integer;
begin
  if SkinData.SkinManager <> nil then
    Result := SkinData.SkinManager.ScaleInt(ButtonHeight)
  else
    Result := ButtonHeight;
end;


function TsRollOutPanel.ActualImageIndex: integer;
begin
  if FCollapsed then
    if FImageIndexCollapsed >= 0 then
      Result := FImageIndexCollapsed
    else
      Result := FImageIndexExpanded
  else
    if FImageIndexCollapsed >= 0 then
      Result := FImageIndexExpanded
    else
      Result := FImageIndexCollapsed;
end;


function TsRollOutPanel.BtnPaintState: integer;
begin
  if not FCollapsed and (BtnIndex = SkinData.SkinManager.ConstData.Sections[ssBarTitle]) then
    Result := min(SkinData.SkinManager.gd[BtnIndex].States - 1, iff(MousePressed, 3, 1))
  else
    Result := FBtnState;
end;


function TsRollOutPanel.BtnRect: TRect;
begin
  Result := MkRect(Self);
  case Placement of
    asLeft:   Result.Right  := ActualButtonHeight;
    asTop:    Result.Bottom := ActualButtonHeight;
    asRight:  Result.Left   := Width - ActualButtonHeight;
    asBottom: Result.Top    := Height - ActualButtonHeight;
  end;
end;


function ArrowSquareSize: integer;
begin
  Result := (acArrowSize + acSpacing) * 2;
end;


procedure TsRollOutPanel.CalcContentPos(var ContentPos: TacContentPos);
var
  iButtonHeight, i: integer;
begin
  iButtonHeight := ActualButtonHeight;
  ContentPos.rBtn := MkRect(Self);
  case Placement of
    asLeft, asRight: begin
      if BtnCache <> nil then
        i := BtnCache.Canvas.TextHeight(s_Yy)
      else
        i := 0;

      if Placement = asLeft then begin
        ContentPos.rBtn.Left := RgnOffset;
        ContentPos.rBtn.Right := iButtonHeight + RgnOffset
      end
      else begin
        ContentPos.rBtn.Left := Width - iButtonHeight - RgnOffset;
        ContentPos.rBtn.Right := ContentPos.rBtn.Left + iButtonHeight;
      end;
      ContentPos.rGlyph.Bottom := Height - acSpacing;
      if GlyphExists then begin
        ContentPos.rGlyph.Top := ContentPos.rGlyph.Bottom - GetImageWidth(Images);
        ContentPos.rGlyph.Left := (iButtonHeight - GetImageWidth(Images)) div 2;
        ContentPos.rGlyph.Right := ContentPos.rGlyph.Left + GetImageWidth(Images);
        if (BtnState = 2) and CanClickShift(SkinData.SkinIndex, SkinData.SkinManager) then
          OffsetRect(ContentPos.rGlyph, 1, 1);

        ContentPos.rText.Bottom := ContentPos.rGlyph.Top - acSpacing;
      end
      else begin
        ContentPos.rGlyph.Top := ContentPos.rGlyph.Bottom;
        ContentPos.rText.Bottom := ContentPos.rGlyph.Top;
      end;
      ContentPos.rText.Left := (iButtonHeight - i) div 2;
      ContentPos.rText.Right := ContentPos.rText.Left + i;;
      if (BtnState = 2) and CanClickShift(SkinData.SkinIndex, SkinData.SkinManager) then
        OffsetRect(ContentPos.rText, 1, 1);

      if FShowArrow then begin
        ContentPos.rArrow.Top := acSpacing;
        ContentPos.rArrow.Bottom := ContentPos.rArrow.Top + ArrowSquareSize;
        ContentPos.rArrow.Left := (iButtonHeight - ArrowSquareSize) div 2;
        ContentPos.rArrow.Right := ContentPos.rArrow.Left + ArrowSquareSize;
        ContentPos.rText.Top := ContentPos.rArrow.Bottom;
      end
      else
        ContentPos.rText.Top := 0;
    end;

    asTop, asBottom: begin
      if Placement = asTop then begin
        ContentPos.rBtn.Top := RgnOffset;
        ContentPos.rBtn.Bottom := iButtonHeight + RgnOffset
      end
      else begin
        ContentPos.rBtn.Top := Height - iButtonHeight - RgnOffset;
        ContentPos.rBtn.Bottom := ContentPos.rBtn.Top + iButtonHeight;
      end;
      ContentPos.rGlyph.Left := acSpacing;
      if GlyphExists then begin
        ContentPos.rGlyph.Right := ContentPos.rGlyph.Left + GetImageWidth(Images);
        ContentPos.rGlyph.Top := (iButtonHeight - GetImageHeight(Images)) div 2;
        ContentPos.rGlyph.Bottom := ContentPos.rGlyph.Top + GetImageHeight(Images);
        if (BtnState = 2) and CanClickShift(SkinData.SkinIndex, SkinData.SkinManager) then
          OffsetRect(ContentPos.rGlyph, 1, 1);

        ContentPos.rText.Left := ContentPos.rGlyph.Right + acSpacing;
      end
      else begin
        ContentPos.rGlyph.Right := ContentPos.rGlyph.Left;
        ContentPos.rText.Left := ContentPos.rGlyph.Right;
      end;
      ContentPos.rText.Top := 0;
      ContentPos.rText.Bottom := iButtonHeight;
      if (BtnState = 2) and CanClickShift(SkinData.SkinIndex, SkinData.SkinManager) then
        OffsetRect(ContentPos.rText, 1, 1);

      if FShowArrow then begin
        ContentPos.rArrow.Right := Width - acSpacing;
        ContentPos.rArrow.Left := ContentPos.rArrow.Right - ArrowSquareSize;
        ContentPos.rArrow.Top := (iButtonHeight - ArrowSquareSize) div 2;
        ContentPos.rArrow.Bottom := ContentPos.rArrow.Top + ArrowSquareSize;
        ContentPos.rText.Right := ContentPos.rArrow.Left;
      end
      else
        ContentPos.rText.Right := Width;
    end;
  end;
end;


const                                               // asLeft    asTop    asRight    asBottom  /  Collapsed  Expanded
  ArrowAngles:    array [TacSide, boolean] of real = ((-90, 0), (0, 90), (270, 180), (0, -90));
  DirArrowAngles: array [TacSide, boolean] of real = ((0, -180), (90, -90), (-180, 0), (-90, 90));

procedure TsRollOutPanel.ChangeState(bCollapsed, bAnimated: boolean);
const
  Divider = 2;
var
  NewRgn: hrgn;
  lTicks: DWord;
  R, rUpdate: TRect;
  aDelta, CurAngle, NewAngle: real;
  NewPos, SavedPos: TPoint;
  i, iSize, iBtnHeight, Steps: integer;
begin
  if ((FCollapsed <> bCollapsed) or not bAnimated) and not Arranging then begin
    case Collapsed of
      False: if Assigned(FOnBeforeCollapse) then FOnBeforeCollapse(Self);
      True:  if Assigned(FOnBeforeExpand)   then FOnBeforeExpand(Self);
    end;
    Arranging := True;
    FCollapsed := bCollapsed;
    CurAngle := ArrowAngle;
    if FDirectionArrow then
      NewAngle := DirArrowAngles[Placement, not FCollapsed]
    else
      NewAngle := ArrowAngles[Placement, not FCollapsed];

    R := BtnRect;
    iBtnHeight := ActualButtonHeight;
    Steps := acMaxIterations shl 1;
    aDelta := (NewAngle - CurAngle) / (Steps + 1);
    NewRgn := 0;
    case Placement of
      asLeft, asRight: iSize := Width - iBtnHeight
      else             iSize := Height - iBtnHeight;
    end;
    i := 0;
    SavedPos := Point(Left, Top);
    if FCollapsed then begin
      if bAnimated and ((SkinData.SkinManager = nil) or SkinData.SkinManager.Effects.AllowAnimation) and HandleAllocated then begin
        while i < Steps do begin
          SetRedraw(Handle, 0);
          lTicks := GetTickCount;
          iSize := Round(iSize / Divider);
          rUpdate := Rect(Left, Top, Left + Width, Top + Height);
          case Placement of
            asLeft: begin
              RgnOffset := Width - iBtnHeight - iSize;
              NewRgn := CreateRectRgn(RgnOffset, 0, Width, Height);
              NewPos := Point(SavedPos.X - RgnOffset, Top);
              rUpdate.Left := NewPos.X + Width;
            end;

            asTop: begin
              RgnOffset := Height - iBtnHeight - iSize;
              NewRgn := CreateRectRgn(0, RgnOffset, Width, Height);
              NewPos := Point(Left, SavedPos.Y - RgnOffset);
              rUpdate.Top := NewPos.Y + Height;
            end;

            asRight: begin
              RgnOffset := Width - iBtnHeight - iSize;
              NewRgn := CreateRectRgn(0, 0, Width - RgnOffset, Height);
              NewPos := Point(min(SavedPos.X + RgnOffset, SavedPos.X + Width - iBtnHeight), Top);
              rUpdate.Right := min(NewPos.X, SavedPos.X + Width - iBtnHeight);
            end;

            asBottom: begin
              RgnOffset := Height - iBtnHeight - iSize;
              NewRgn := CreateRectRgn(0, 0, Width, Height - RgnOffset);
              NewPos := Point(Left, min(SavedPos.Y + RgnOffset, SavedPos.Y + Height - iBtnHeight));
              rUpdate.Bottom := min(NewPos.Y, SavedPos.Y + Height - iBtnHeight);
            end;
          end;
          SetWindowRgn(Handle, NewRgn, False);
          Top := NewPos.Y;
          Left := NewPos.X;
          DeleteObject(NewRgn);
          BtnChanged := True;
          ArrowAngle := ArrowAngle + aDelta;
          if SkinData.Skinned then begin
            ShowTitleForm;
            RedrawWindow(Handle, @rUpdate, 0, RDWA_ALLNOW);
          end;
          RedrawWindow(Parent.Handle, @rUpdate, 0, RDWA_ALLNOW);
          SetRedraw(Handle, 1);
          case Placement of
            asLeft:   rUpdate := Rect(RgnOffset, 0, Width, Height);
            asTop:    rUpdate := Rect(0, RgnOffset, Width, Height);
            asRight:  rUpdate := Rect(0, 0, Width - RgnOffset - iBtnHeight, Height);
            asBottom: rUpdate := Rect(0, 0, Width, Height - RgnOffset - iBtnHeight);
          end;
          RedrawWindow(Handle, @rUpdate, 0, RDWA_ALLNOW);
          if SkinData.Skinned then begin
            Perform(WM_NCPAINT, 0, 0); // If skinned
            HideTitleForm;
          end;
          inc(i);
          WaitTicks(lTicks);
        end;
        SetRedraw(Handle, 0);
        rUpdate := Rect(Left, Top, Left + Width, Top + Height);
        RgnOffset := 0;
        case Placement of
          asLeft:   NewRgn := CreateRectRgn(0, 0, iBtnHeight, Height);
          asTop:    NewRgn := CreateRectRgn(0, 0, Width, iBtnHeight);
          asRight:  NewRgn := CreateRectRgn(Width - iBtnHeight, 0, Width, Height);
          asBottom: NewRgn := CreateRectRgn(0, Height - iBtnHeight, Width, Height);
        end;
        SetWindowRgn(Handle, NewRgn, False);
        DeleteObject(NewRgn);
        Left := SavedPos.X;
        Top := SavedPos.Y;
        case Placement of
          asLeft:   rUpdate.Left   := Left + iBtnHeight;
          asTop:    rUpdate.Top    := Top + iBtnHeight;
          asRight:  rUpdate.Right  := Left + Width - iBtnHeight;
          asBottom: rUpdate.Bottom := Top + Height - iBtnHeight;
        end;
        InvalidateRect(Parent.Handle, @rUpdate, True);
        BtnChanged := True;
        ArrowAngle := NewAngle;
        SetRedraw(Handle, 1);
        RedrawWindow(Handle, nil, 0, RDWA_ALLNOW);
        Perform(WM_NCPAINT, 0, 0); // If skinned
      end
      else begin
        ArrowAngle := NewAngle;
        if HandleAllocated then begin
          NewRgn := CreateRectRgn(R.Left, R.Top, R.Right, R.Bottom);
          SetWindowRgn(Handle, NewRgn, True);
          DeleteObject(NewRgn);
        end;
      end;
    end
    else begin
      if bAnimated and ((SkinData.SkinManager = nil) or SkinData.SkinManager.Effects.AllowAnimation) and HandleAllocated then begin
        while i < Steps do begin
          SetRedraw(Handle, 0);
          lTicks := GetTickCount;
          iSize := Round(iSize / Divider);
          RgnOffset := iSize;
          case Placement of
            asLeft: begin
              NewRgn := CreateRectRgn(RgnOffset, 0, Width, Height);
              NewPos := Point(SavedPos.X - RgnOffset, Top);
            end;

            asTop: begin
              NewRgn := CreateRectRgn(0, RgnOffset, Width, Height);
              NewPos := Point(Left, SavedPos.Y - RgnOffset);
            end;

            asRight: begin
              NewRgn := CreateRectRgn(0, 0, Width - iSize, Height);
              NewPos := Point(SavedPos.X + RgnOffset, Top);
            end;

            asBottom: begin
              NewRgn := CreateRectRgn(0, 0, Width, Height - iSize);
              NewPos := Point(Left, SavedPos.Y + RgnOffset);
            end;
          end;
          SetWindowRgn(Handle, NewRgn, False);
          Top := NewPos.Y;
          Left := NewPos.X;
          DeleteObject(NewRgn);
          BtnChanged := True;
          ArrowAngle := ArrowAngle + aDelta;
          SetRedraw(Handle, 1);
          case Placement of
            asLeft:   rUpdate := Rect(RgnOffset, 0, Width, Height);
            asTop:    rUpdate := Rect(0, RgnOffset, Width, Height);
            asRight:  rUpdate := Rect(0, 0, Width - RgnOffset - iBtnHeight, Height);
            asBottom: rUpdate := Rect(0, 0, Width, Height - RgnOffset - iBtnHeight);
          end;
          if SkinData.Skinned then begin
            ShowTitleForm;
            RedrawWindow(Handle, @rUpdate, 0, RDWA_ALLNOW);
            Perform(WM_NCPAINT, 0, 0); // If skinned
            HideTitleForm;
          end
          else
            RedrawWindow(Handle, @rUpdate, 0, RDWA_ALLNOW);

          inc(i);
          WaitTicks(lTicks);
        end;
        RgnOffset := 0;
        SetRedraw(Handle, 0);
        NewRgn := CreateRectRgn(0, 0, Width, Height);
        SetWindowRgn(Handle, NewRgn, False);
        DeleteObject(NewRgn);
        Left := SavedPos.X;
        Top := SavedPos.Y;
        BtnChanged := True;
        ArrowAngle := NewAngle;
        if HandleAllocated then begin
          SetWindowRgn(Handle, 0, False);
          SetRedraw(Handle, 1);
          RedrawWindow(Handle, nil, 0, RDWA_ALLNOW);
        end;
      end
      else begin
        ArrowAngle := NewAngle;
        if HandleAllocated then
          SetWindowRgn(Handle, 0, True);
      end;
    end;
    Arranging := False;
    case Collapsed of
      False: if Assigned(FOnAfterExpand)   then FOnAfterExpand(Self);
      True:  if Assigned(FOnAfterCollapse) then FOnAfterCollapse(Self);
    end;
    if Assigned(FOnStateChanged) then
      FOnStateChanged(Self);
  end;
end;


procedure TsRollOutPanel.CheckAutoShowing(Value: boolean);
begin
  BtnState := integer(not Value);
  if not (csDesigning in ComponentState) and (Collapsed <> Value) then
    if not Value then begin
      if AutoShow then
        ChangeState(False, Animated)
    end
    else
      if AutoHide and not acMouseInControl(Self) then
        ChangeState(True, Animated);
end;


constructor TsRollOutPanel.Create(AOwner: TComponent);
begin
  inherited;
  FButtonHeight := 22;
  RgnOffset := 0;
  BtnChanged := True;
  FCollapsed := False;
  FAnimated := True;
  FShowArrow := True;
  FGroupIndex := 0;
  Arranging := False;
  FImageIndexCollapsed := -1;
  FImageIndexExpanded := -1;
  FDirectionArrow := False;
  FPlacement := asTop;
  FAutoHide := False;
  MousePressed := False;
  FTitleCursor := crDefault;
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageListChange;
  BtnCache := CreateBmp32;
end;


destructor TsRollOutPanel.Destroy;
begin
  FreeAndNil(FImageChangeLink);
  BtnCache.Free;
  ExitTimer.Free;
  inherited;
end;


procedure TsRollOutPanel.DoTimer(Sender: TObject);
begin
  if not PtInButton(acMousePos.X, acMousePos.Y) then begin
    CheckAutoShowing(True);
    if Collapsed then
      ExitTimer.Enabled := False;
  end
end;


function TsRollOutPanel.GlyphExists: boolean;
begin
  Result := (Images <> nil) and IsValidIndex(ActualImageIndex, Images.Count);
end;


procedure TsRollOutPanel.HideTitleForm;
begin
  FreeAndNil(TitleForm);
end;


procedure TsRollOutPanel.ImageListChange(Sender: TObject);
begin
  FCommonData.Invalidate;
end;


procedure TsRollOutPanel.Loaded;
begin
  inherited;
  ChangeState(Collapsed, False);
  ControlStyle := ControlStyle - [csOpaque];
end;


procedure TsRollOutPanel.Paint;
var
  R: TRect;
  C: TColor;
  w: integer;
begin
  w := 1;
  R := GetClientRect;
  C := BlendColors(ColorToRGB(Color), 0, 230);
  FillDC(Canvas.Handle, GetClientRect, Color);
  DrawRectangleOnDC(Canvas.Handle, R, C, C, w);
end;


function TsRollOutPanel.PrepareCache: boolean;
var
  R: TRect;
  P: TPoint;
begin
  InitCacheBmp(SkinData);
  P.X := Left;
  P.Y := Top;
  R := MkRect(Self);
  case Placement of
    asLeft: begin
      R.Right := R.Right - ActualButtonHeight;
      inc(P.X, ActualButtonHeight);
    end;
    asTop: begin
      R.Bottom := R.Bottom - ActualButtonHeight;
      inc(P.Y, ActualButtonHeight);
    end;
  end;
  if not PaintSkinControl(FCommonData, Parent, True, 0, R, P, FCommonData.FCacheBMP, True) then begin
    SkinData.FUpdating := True;
    Result := False;
  end
  else begin
    UpdateBmpColors(FCommonData.FCacheBmp, SkinData, True, 0, SkinData.ColorTone);
    if Assigned(FOnPaint) then
      FOnPaint(Self, FCommonData.FCacheBmp.Canvas);

    FCommonData.BGChanged := False;
    Result := True;
  end;
end;


function TsRollOutPanel.PrepareSkinCache: TPoint;
var
  OldFont: HFONT;
  Flags: Cardinal;
  TmpBmp: TBitmap;
  iState, iSquareSize: integer;
  ContentPos: TacContentPos;
begin
  if BtnChanged then begin
    CalcContentPos(ContentPos);
    BtnCache.Width := WidthOf(ContentPos.rBtn);
    BtnCache.Height := HeightOf(ContentPos.rBtn);

    if FTitleSkin <> '' then
      BtnIndex := SkinData.SkinManager.GetSkinIndex(FTitleSkin)
    else
      if Placement = asTop then
        BtnIndex := SkinData.SkinManager.ConstData.Sections[ssBarTitle]
      else
        BtnIndex := SkinData.SkinManager.ConstData.Sections[ssButton];

    iState := BtnPaintState;
    PaintItem(BtnIndex, GetParentCache(SkinData), True, iState, MkRect(BtnCache), Point(Left + ContentPos.rBtn.Left, Top + ContentPos.rBtn.Top), BtnCache, SkinData.SkinManager);

    BtnCache.Canvas.Font.Assign(Font);
    BtnCache.Canvas.Brush.Style := bsClear;
    BtnCache.Canvas.Font.Color := ColorToRGB(GetFontColor(Self, BtnIndex, SkinData.SkinManager, min(2, iState)));
    if GlyphExists then
      if (Images is TsAlphaImageList) and (SkinData.SkinManager <> nil) and SkinData.SkinManager.Effects.DiscoloredGlyphs then
        DrawAlphaImgList(Images, BtnCache, ContentPos.rGlyph.Left, ContentPos.rGlyph.Top, ActualImageIndex, 0, BtnCache.Canvas.Font.Color, 0, 1, False)
      else
        Images.Draw(BtnCache.Canvas, ContentPos.rGlyph.Left, ContentPos.rGlyph.Top, ActualImageIndex, True);

    if Caption <> '' then begin
      Flags := DT_SINGLELINE or DT_VCENTER or DT_END_ELLIPSIS;
      if UseRightToLeftReading then
        Flags := Flags or DT_RTLREADING;

      if Placement in [asTop, asBottom] then
        if not SkinData.CustomFont then
          acWriteTextEx(BtnCache.Canvas, PacChar(Caption), True, ContentPos.rText, Flags, BtnIndex, iState > 0, FCommonData.SkinManager)
        else
          acWriteText(BtnCache.Canvas, PacChar(Caption), True, ContentPos.rText, Flags)
      else begin
        OldFont := MakeAngledFont(BtnCache.Canvas.Handle, BtnCache.Canvas.Font, -2700); // Rotated font initialization
        acTextRect(BtnCache.Canvas, ContentPos.rText, ContentPos.rText.Left, ContentPos.rText.Bottom, Caption);
        SelectObject(BtnCache.Canvas.Handle, OldFont); // Returning prev. font
      end;
    end;

    if FShowArrow then begin
      iSquareSize := ArrowSquareSize;
      TmpBmp := CreateBmp32(iSquareSize, iSquareSize);
      FillDC(TmpBmp.Canvas.Handle, MkRect(TmpBmp), $FFFFFF);
      DrawArrow(TmpBmp, 0, clNone{/$FFFFFF}, MkRect(TmpBmp), asRight, max(2, acLineWidth), Round(ArrowAngle), SkinData.SkinManager.ScaleInt(5), arsLines1); // Paint a mask
      sGraphUtils.ColorizeByMask(BtnCache, TmpBmp, ContentPos.rArrow.TopLeft, BtnCache.Canvas.Font.Color, clNone);
      TmpBmp.Free;
    end;
    FillAlphaRect(BtnCache, MkRect(BtnCache), MaxByte);
    Result := ContentPos.rBtn.TopLeft;
  end
  else
    Result := MkPoint;
end;


function TsRollOutPanel.PrepareStdCache: TPoint;
var
  R: TRect;
  C: TColor;
  OldFont: HFONT;
  Flags: Cardinal;
  TmpBmp: TBitmap;
  iSquareSize: integer;
  ContentPos: TacContentPos;
begin
  if BtnChanged then begin
    CalcContentPos(ContentPos);
    BtnCache.Width := WidthOf(ContentPos.rBtn);
    BtnCache.Height := HeightOf(ContentPos.rBtn);
    C := BlendColors(ColorToRGB(Color), 0, 230);
    if BtnState = 1 then
      C := BlendColors(C, $FFFFFF, 200)
    else
      if BtnState = 2 then
        C := BlendColors(C, 0, 230);

    FillDC(BtnCache.Canvas.Handle, MkRect(BtnCache), C);
    BtnIndex := SkinData.SkinIndex;
    C := GetFontColor(Self, BtnIndex, SkinData.SkinManager, BtnState);
    if GlyphExists then
      if (Images is TsAlphaImageList) and (SkinData.SkinManager <> nil) and SkinData.SkinManager.Effects.DiscoloredGlyphs then
        DrawAlphaImgList(Images, BtnCache, ContentPos.rGlyph.Left, ContentPos.rGlyph.Top, ActualImageIndex, 0, C, 0, 1, False)
      else
        Images.Draw(BtnCache.Canvas, ContentPos.rGlyph.Left, ContentPos.rGlyph.Top, ActualImageIndex, True);

    if Caption <> '' then begin
      BtnCache.Canvas.Font.Assign(Font);

      Flags := DT_SINGLELINE or DT_VCENTER or DT_END_ELLIPSIS;
      if UseRightToLeftReading then
        Flags := Flags or DT_RTLREADING;

      BtnCache.Canvas.Font.Assign(Font);
      BtnCache.Canvas.Brush.Style := bsClear;
      if Placement in [asTop, asBottom] then
        if not SkinData.CustomFont then
          acWriteTextEx(BtnCache.Canvas, PacChar(Caption), True, ContentPos.rText, Flags, BtnIndex, BtnState > 0, FCommonData.SkinManager)
        else
          acWriteText(BtnCache.Canvas, PacChar(Caption), True, ContentPos.rText, Flags)
      else begin
        OldFont := MakeAngledFont(BtnCache.Canvas.Handle, BtnCache.Canvas.Font, -2700); // Rotated font initialization
        acTextRect(BtnCache.Canvas, ContentPos.rText, ContentPos.rText.Left, ContentPos.rText.Bottom, Caption);
        SelectObject(BtnCache.Canvas.Handle, OldFont); // Return prev. font
      end;
    end;

    if FShowArrow then begin
      iSquareSize := ArrowSquareSize;
      TmpBmp := CreateBmp32(iSquareSize, iSquareSize);
      FillDC(TmpBmp.Canvas.Handle, MkRect(TmpBmp), $FFFFFF);

      R.Top := TmpBmp.Height div 2 - acArrowSize;
      R.Left := R.Top;
      R.Bottom := R.Top + 2 * acArrowSize;
      R.Right := R.Bottom;

      // Paint a mask
      if SkinData.SkinManager <> nil then
        DrawArrow(TmpBmp, 0, clNone, R, asRight, max(2, acLineWidth), Round(ArrowAngle), SkinData.SkinManager.ScaleInt(5), arsLines1)
      else
        DrawArrow(TmpBmp, 0, clNone, R, asRight, max(2, acLineWidth), Round(ArrowAngle), 5, arsLines1);

      sGraphUtils.ColorizeByMask(BtnCache, TmpBmp, ContentPos.rArrow.TopLeft, C, clNone);
      TmpBmp.Free;
    end;
    Result := ContentPos.rBtn.TopLeft;
  end
  else
    Result := MkPoint;
end;


function TsRollOutPanel.PtInButton(X, Y: integer): boolean;
var
  R, rBtn: TRect;
begin
  GetWindowRect(Handle, R);
  rBtn := BtnRect;
  OffsetRect(rBtn, R.Left, R.Top);
  Result := PtInRect(rBtn, Point(X, Y));
end;


procedure TsRollOutPanel.Rearrange;
begin
  if FCollapsed then begin
    ChangeState(False, False);
    ChangeState(True, not InAnimationProcess and not(csLoading in ComponentState));
  end
  else begin
    UpdateAngle;
    SetWindowPos(Handle, 0, 0, 0, 0, 0, SWPA_FRAMECHANGED);
  end;
end;


procedure TsRollOutPanel.SetBoolean(const Index: Integer; const Value: boolean);
begin
  case Index of
    2: FAnimated := Value;
    0: if FCollapsed <> Value then begin
      FCollapsed := Value;
      ChangeState(Value, False);
      if (csDesigning in ComponentState) and Parent.HandleAllocated then
        RedrawWindow(Parent.Handle, nil, 0, RDWA_ALLNOW);
    end;
    1: if ShowArrow <> Value then begin
      FShowArrow := Value;
      SkinData.Invalidate(True);
    end;
    3: if FDirectionArrow <> Value then begin
      FDirectionArrow := Value;
      BtnChanged := True;
      UpdateAngle;
      SkinData.Invalidate(True);
    end;
  end;
end;


procedure TsRollOutPanel.SetBtnState(const Value: integer);
begin
  if FBtnState <> Value then begin
    FBtnState := Value;
    BtnChanged := True;
    Perform(WM_NCPAINT, 0, 0);
    if FBtnState <> 0 then begin
      if ExitTimer = nil then begin
        ExitTimer := TTimer.Create(Self);
        ExitTimer.OnTimer := DoTimer;
        ExitTimer.Interval := 100;
      end;
      ExitTimer.Enabled := True;
    end;
  end;
end;


procedure TsRollOutPanel.SetImages(const Value: TCustomImageList);
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


procedure TsRollOutPanel.SetInteger(const Index, Value: integer);

  procedure ChangeRepaint(var Prop: integer; Value: integer);
  begin
    if Prop <> Value then begin
      Prop := Value;
      SkinData.Invalidate(True);
    end;
  end;

begin
  case Index of
    0: FGroupIndex := Value;
    2: ChangeRepaint(FImageIndexCollapsed, Value);
    3: ChangeRepaint(FImageIndexExpanded, Value);
    1: if FButtonHeight <> Value then begin
      FButtonHeight := Value;
      Rearrange;
    end;
  end;
end;


procedure TsRollOutPanel.SetPlacement(const Value: TacSide);
begin
  if FPlacement <> Value then begin
    FPlacement := Value;
    Rearrange;
  end;
end;


procedure TsRollOutPanel.SetTitleSkin(const Value: TsSkinSection);
begin
  if FTitleSkin <> Value then begin
    FTitleSkin := Value;
    SkinData.Invalidate;
  end;
end;


procedure TsRollOutPanel.ShowTitleForm;
var
  R, rBtn: TRect;
begin
  rBtn := BtnRect;
  GetWindowRect(Handle, R);
  OffsetRect(rBtn, R.Left, R.Top);
  case Placement of
    asLeft:   inc(R.Left, RgnOffset);
    asTop:    inc(R.Top, RgnOffset);
    asRight:  inc(R.Left, Width - RgnOffset - ActualButtonHeight);
    asBottom: inc(R.Top, Height - RgnOffset - ActualButtonHeight);
  end;
  TitleForm := TacGlowForm.CreateNew(nil);
  TitleForm.SetBounds(rBtn.Left, rBtn.Top, WidthOf(rBtn), HeightOf(rBtn));
  SetFormBlendValue(TitleForm.Handle, BtnCache, MaxByte);
  SetWindowPos(TitleForm.Handle, HWND_TOPMOST, R.Left, R.Top, 0, 0, SWPA_SHOWZORDER);
end;


procedure TsRollOutPanel.UpdateAngle;
begin
  if FDirectionArrow then
    ArrowAngle := DirArrowAngles[FPlacement, not FCollapsed]
  else
    ArrowAngle := ArrowAngles[FPlacement, not FCollapsed];
end;


procedure TsRollOutPanel.WMNCCaclSize(var Message: TWMNCCalcSize);
begin
  inherited;
  case Placement of
    asLeft:   inc(Message.CalcSize_Params.rgrc[0].Left,   ActualButtonHeight);
    asTop:    inc(Message.CalcSize_Params.rgrc[0].Top,    ActualButtonHeight);
    asRight:  dec(Message.CalcSize_Params.rgrc[0].Right,  ActualButtonHeight);
    asBottom: dec(Message.CalcSize_Params.rgrc[0].Bottom, ActualButtonHeight);
  end;
end;


procedure TsRollOutPanel.WMNCHitText(var Message: TWMNCHitTest);
begin
  inherited;
  if not (csDesigning in ComponentState) then
    if PtInButton(Message.XPos, Message.YPos) then begin
      SetCursor(Screen.Cursors[TitleCursor]);
      Message.Result := HTOBJECT;
      if BtnState = 0 then
        CheckAutoShowing(False);
    end
    else begin
      SetCursor(Screen.Cursors[Cursor]);
      CheckAutoShowing(True);
    end;
end;


procedure TsRollOutPanel.WMNCLButtonDown(var Message: TWMMouse);
begin
  if not (csDesigning in ComponentState) then begin
    MousePressed := True;
    if PtInButton(Message.XPos, Message.YPos) and (BtnState <> 2) then
      BtnState := 2;
  end
  else inherited;
end;


procedure TsRollOutPanel.WMNCLButtonUp(var Message: TWMMouse);
begin
  if not (csDesigning in ComponentState) then begin
    MousePressed := False;
    BtnState := integer(PtInButton(Message.XPos, Message.YPos));
    ChangeState(not Collapsed, FAnimated);
  end
  else inherited;
end;


procedure TsRollOutPanel.WMNCPaint(var Message: TMessage);
var
  DC: hdc;
  p: TPoint;
begin
  if IsWindowVisible(Handle) or (csDesigning in ComponentState) then begin
    DC := GetWindowDC(Handle);
    try
      if SkinData.Skinned then
        p := PrepareSkinCache
      else
        p := PrepareStdCache;

      BitBlt(DC, p.X, p.Y, BtnCache.Width, BtnCache.Height, BtnCache.Canvas.Handle, 0, 0, SRCCOPY);
    finally
      ReleaseDC(Handle, DC);
    end;
  end;
end;


const
  // (asLeft, asTop, asRight, asBottom) (Collapsed, Expanded)
  TabPositions: array [TacSide] of TTabPosition = (tpLeft, tpTop, tpRight, tpBottom);

procedure TsRollOutPanel.WndProc(var Message: TMessage);
begin
{$IFDEF LOGGED}
//  AddToLog(Message);
{$ENDIF}
  case Message.Msg of
    SM_ALPHACMD:
      case Message.WParamHi of
        AC_SETNEWSKIN:
          if SkinData.SkinManager <> nil then
            BtnIndex := SkinData.SkinManager.GetSkinIndex(s_BarTitle)
          else
            BtnIndex := -1;

        AC_REMOVESKIN:
          BtnIndex := -1;

        AC_ISOPAQUE: begin
          Message.Result := 0;
          Exit;
        end;

        AC_GETFONTINDEX:
          if PacPaintInfo(Message.LParam)^.FontIndex = BtnIndex then begin
            if NeedParentFont(SkinData.SkinManager, BtnIndex, PacPaintInfo(Message.LParam)^.State, nil) then
              Message.Result := GetFontIndex(Parent, PacPaintInfo(Message.LParam))
            else
              Message.Result := BtnIndex;

            Exit;
          end;

        AC_GETDEFINDEX: begin
          if FCommonData.SkinManager <> nil then
            if Placement = asTop then
              Message.Result := FCommonData.SkinManager.ConstData.Sections[ssBarPanel] + 1
            else begin
              Message.Result := FCommonData.SkinManager.GetSkinIndex(s_PageControl + sTabPositions[TabPositions[Placement]]) + 1;
              if Message.Result < 0 then
                Message.Result := FCommonData.SkinManager.ConstData.Sections[ssPageControl] + 1;
            end;

          Exit;
        end;
      end;

    WM_PRINT: begin
      FCommonData.FUpdating := False;
      FCommonData.HalfVisible := False;
      if ControlIsReady(Self) then begin
        PrepareCache;
        PrepareSkinCache;

        with BtnRect do
          BitBlt(TWMPaint(Message).DC, Left, Top, BtnCache.Width, BtnCache.Height, BtnCache.Canvas.Handle, 0, 0, SRCCOPY);

        case Placement of
          asLeft: MoveWindowOrg(TWMPaint(Message).DC, ActualButtonHeight, 0);
          asTop:  MoveWindowOrg(TWMPaint(Message).DC, 0, ActualButtonHeight);
        end;
        if not FCollapsed then
          OurPaint(TWMPaint(Message).DC, False)
        else
          Message.Result := NOCHILDRENPRINT;
      end;
      Exit;
    end;
  end;
  inherited;
  case Message.Msg of
    WM_WINDOWPOSCHANGED, WM_SIZE:
      if not (csDestroying in ComponentState) then begin
        BtnChanged := True;
        if FCollapsed then
          ChangeState(True, False);

        Perform(WM_NCPAINT, 0, 0);
      end;
  end;
end;


procedure TsSplitView.CreateWnd;
begin
  inherited;
  UpdatePosition;
end;


procedure TsSplitView.AnimationTimerHandler(Sender: TObject);
const
  iAnimDivider = 1.25;
var
  RestRect: TRect;
  DeltaSize: real;
  NewSize: real;
begin
  BringToFront;
  ClearGlows;
  if FState in [svsaOpened, svsaClosed] then
    FAnimationTimer.Enabled := False
  else begin
    Perform(WM_SETREDRAW, 0, 0);
    if FState = svsaOpening then begin
      RestRect.Right := RestRect.Left;
      if FOpenedSize - VisibleSize < iAnimDivider then
        VisibleSize := FOpenedSize;

      if (DisplayMode = svmaOverlay) and FContentMoved then begin
        if VisibleSize <> FOpenedSize then begin
          DeltaSize := (FOpenedSize - VisibleSize) / iAnimDivider;
          VisibleSize := Min(Round(FVisibleSize + DeltaSize), FOpenedSize);
        end;

        SetActBounds(Position, VisibleSize);
        UpdateBGBmp;
        if (FBlurData.FMode in [bmSplitView]) and (DisplayMode = svmaOverlay) then
          SkinData.BGChanged := True;
      end
      else begin // Size is really changed
        if VisibleSize <> FOpenedSize then begin
          NewSize := VisibleSize + (FOpenedSize - VisibleSize) / iAnimDivider;
          VisibleSize := Round(Min(NewSize, FOpenedSize));
        end;
        if DisplayMode = svmaDocked then
          ParentLock(True);

        SetActBounds(Position, VisibleSize);
        if DisplayMode = svmaDocked then
          ParentLock(False, True);

        SkinData.Updating := False;
        if not InUpdating(SkinData) then
          SetParentUpdated(Self);
      end;
    end
    else begin
      KillParentBlur;
      RestRect := BoundsRect;
      if FVisibleSize < CompSize + iAnimDivider then
        VisibleSize := CompSize;

      if DisplayMode = svmaDocked then
        ParentLock(True);

      if (DisplayMode = svmaOverlay) and FContentMoved then begin
        if VisibleSize <> CompSize then begin
          DeltaSize := (FVisibleSize - CompSize) / iAnimDivider;
          VisibleSize := Max(CompSize, Min(Round(FVisibleSize - DeltaSize), FOpenedSize));
        end;
        SetActBounds(Position, WindowSize);
      end
      else begin
        if VisibleSize <> CompSize then begin
          NewSize := VisibleSize - (VisibleSize - CompSize) / iAnimDivider;
          VisibleSize := Max(CompSize, Min(Round(NewSize), FOpenedSize));
        end;
        SetActBounds(Position, VisibleSize);
      end;
      if DisplayMode = svmaDocked then begin
        ParentLock(False, True);
        RestRect.Right := RestRect.Left;
      end
      else
        case FPlacement of
          svpaLeft:   RestRect.Left   := Left + WindowSize;
          svpaRight:  RestRect.Right  := Left;
          svpaTop:    RestRect.Top    := Top + WindowSize;
          svpaBottom: RestRect.Bottom := Top;
        end;
    end;

    if VisibleSize = FOpenedSize then
      SetState(svsaOpened)
    else
      if VisibleSize = CompSize then
        SetState(svsaClosed);

    Perform(WM_SETREDRAW, 1, 0);
    RedrawWindow(Handle, nil, 0, RDWA_ALLNOW);

    if (FState in [svsaOpened, svsaOpening]) and (DisplayMode = svmaOverlay) then
      UpdateParentBlur;

    if not IsRectEmpty(RestRect) then
      RedrawWindow(Parent.Handle, @RestRect, 0, RDWA_ALLNOW);
  end;
end;


function TsSplitView.CanAnimate(State: TacSVState): boolean;
begin
  if not (csDesigning in COmponentState) then
    Result := ((FAnimatedShowing and (State in [svsaOpened, svsaOpening])) or (FAnimatedHiding and (State in [svsaClosed, svsaClosing]))) and not(csDesigning in ComponentState) and not (csLoading in ComponentState) and ((SkinData.SkinManager = nil) or SkinData.SkinManager.Effects.AllowAnimation)
  else
    Result := False;
end;


function TsSplitView.CanAutoHide: boolean;
begin
  if not (csDesigning in COmponentState) then
    Result := not (csDesigning in ComponentState) and (AutoHide or (BlurData.Mode = bmParent)) and not PosUpdating
  else
    Result := False;
end;


procedure TsSplitView.ChangeScale(M, D: Integer);
begin
  inherited;
  if M <> D then begin
    FOpenedSize  := MulDiv(FOpenedSize,  M, D);
    FCompactSize := MulDiv(FCompactSize, M, D);
  end;
end;


procedure TsSplitView.Close;
begin
  if not (csLoading in ComponentState) and Opened then
    if FAnimatedHiding then
      SetState(svsaClosing)
    else
      SetState(svsaClosed);
end;


function TsSplitView.CompSize: integer;
begin
  if FCloseStyle = svcaCompact then
    Result := FCompactSize
  else
    Result := 0;
end;


constructor TsSplitView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  ControlStyle := ControlStyle - [csSetCaption{$IFDEF DELPHI7UP}, csParentBackground{$ENDIF}] + [csOpaque, csCaptureMouse];
  DoubleBuffered := True;

  FState := svsaOpened;
  FDisplayMode := svmaDocked;
  FCloseStyle := svcaCollapse;

  FBlurData := TacBlurData.Create(Self);
  BGBmp := nil;
  FOverlayPadding := nil;

  FCompactSize := DefSVCompactSize;
  FOpenedSize := DefSVOpenedSize;
  FPlacement := svpaLeft;
  Printing := False;
  FAutoHide := False;
  PosUpdating := False;
  FAnimatedShowing := True;
  FAnimatedHiding := False;
  FContentMoved := False;
  StateChanging := False;

//  BevelOuter := bvNone; // Remove the default border of ancestor

  FAnimationTimer := TTimer.Create(Self);
  FAnimationTimer.Enabled := False;
  FAnimationTimer.Interval := acTimerInterval * 2;
  FAnimationTimer.OnTimer := AnimationTimerHandler;
end;


destructor TsSplitView.Destroy;
begin
  FBlurData.Free;
  inherited;
end;


procedure TsSplitView.DoClosed;
begin
  if Assigned(FOnClosed) then
    FOnClosed(Self);
end;


procedure TsSplitView.DoClosing;
begin
  if Assigned(FOnClosing) then
    FOnClosing(Self);
end;


procedure TsSplitView.DoOpened;
begin
  if Assigned(FOnOpened) then
    FOnOpened(Self);
end;


procedure TsSplitView.DoOpening;
begin
  if Assigned(FOnOpening) then
    FOnOpening(Self);
end;


procedure TsSplitView.PrepareBGBmp;
begin
  UpdateBGBmp;
  if BluredBmp = nil then begin
    if FPlacement in [svpaLeft, svpaRight] then
      BluredBmp := CreateBmp32(FOpenedSize, Height)
    else
      BluredBmp := CreateBmp32(Width, FOpenedSize);

    case Placement of
      svpaLeft:   BitBlt(BluredBmp.Canvas.Handle, 0, 0, OpenedSize, Height, BGBmp.Canvas.Handle, BGOffset.X, Top + BGOffset.Y, SRCCOPY);
      svpaRight:  BitBlt(BluredBmp.Canvas.Handle, 0, 0, OpenedSize, Height, BGBmp.Canvas.Handle, Parent.ClientWidth - FOpenedSize + BGOffset.X, Top + BGOffset.Y, SRCCOPY);
      svpaTop:    BitBlt(BluredBmp.Canvas.Handle, 0, 0, Width,  OpenedSize, BGBmp.Canvas.Handle, Left + BGOffset.X, BGOffset.Y, SRCCOPY);
      svpaBottom: BitBlt(BluredBmp.Canvas.Handle, 0, 0, Width,  OpenedSize, BGBmp.Canvas.Handle, Left + BGOffset.X, Parent.ClientHeight - FOpenedSize + BGOffset.Y, SRCCOPY);
    end;
    acgpBlur(BluredBmp, FBlurData.Size);
    BlendTransBitmap(BluredBmp, MaxByte - FBlurData.Opacity, TsColor(acColorToRGB(FBlurData.Color)));
  end;
end;


function TsSplitView.PrepareCache: boolean;
begin
  if not (csDesigning in ComponentState) and (DisplayMode = svmaOverlay) and (FBlurData.FMode <> bmNone) then begin
    if FCommonData.BGChanged or (SkinData.FCacheBmp.Width <> Width) or (SkinData.FCacheBmp.Height <> Height) then begin
      FCommonData.BGChanged := False;
      InitCacheBmp(SkinData);
      case FBlurData.FMode of
        bmSplitView: begin
          PrepareBGBmp;
          case Placement of
            svpaLeft:
              if ContentMoved then
                BitBlt(SkinData.FCacheBmp.Canvas.Handle, 0, 0, SkinData.FCacheBmp.Width, Height, BluredBmp.Canvas.Handle, VisibleSize - FOpenedSize, Top, SRCCOPY)
              else
                BitBlt(SkinData.FCacheBmp.Canvas.Handle, 0, 0, SkinData.FCacheBmp.Width, Height, BluredBmp.Canvas.Handle, 0, Top, SRCCOPY);

            svpaRight:
              BitBlt(SkinData.FCacheBmp.Canvas.Handle, 0, 0, SkinData.FCacheBmp.Width, Height, BluredBmp.Canvas.Handle, FOpenedSize - VisibleSize{ + 1}, Top, SRCCOPY);

            svpaTop:
              if ContentMoved then
                BitBlt(SkinData.FCacheBmp.Canvas.Handle, 0, 0, Width, SkinData.FCacheBmp.Height, BluredBmp.Canvas.Handle, Left, VisibleSize - FOpenedSize, SRCCOPY)
              else
                BitBlt(SkinData.FCacheBmp.Canvas.Handle, 0, 0, Width, SkinData.FCacheBmp.Height, BluredBmp.Canvas.Handle, Left, 0, SRCCOPY);

            svpaBottom:
              BitBlt(SkinData.FCacheBmp.Canvas.Handle, 0, 0, Width, SkinData.FCacheBmp.Height, BluredBmp.Canvas.Handle, Left, FOpenedSize - VisibleSize{ + 1}, SRCCOPY);
          end;
          FCommonData.BGChanged := False;
        end;

        bmParent: begin
          PaintItemBG(FCommonData, EmptyCI{GetParentCache(SkinData)}, 0, MkRect(Self), Point(Left, Top), FCommonData.FCacheBMP);
          FCommonData.FCacheBMP.Canvas.Pen.Color := acColorToRGB(clActiveBorder, FCommonData.SkinManager);
          FCommonData.FCacheBMP.Canvas.Pen.Width := 1;
          FCommonData.FCacheBMP.Canvas.Brush.Style := bsClear;
          FCommonData.FCacheBMP.Canvas.Rectangle(0, 0, Width, Height);
        end;
      end;
    end;
    Result := True;
  end
  else
    Result := inherited PrepareCache;
end;


type
  TacParentBlur = class(TsPanel)
  protected
    RefreshCount: integer;
    FOwner: TsSplitView;
    PosUpdating, UpdateBG: boolean;
  public
    procedure WndProc(var Message: TMessage); override;
    procedure PaintWindow(DC: HDC); override;
    constructor Create(AOwner: TComponent); override;
    function PrepareCache: boolean; override;
    destructor Destroy; override;
    procedure RefreshImage;
    procedure UpdatePos;
  end;


procedure TsSplitView.RefreshBackground;
var
  M: TMessage;
begin
  if not (csDesigning in ComponentState) and (FBlurData.FMode in [bmSplitView]) and (DisplayMode = svmaOverlay) then begin
    SkinData.BGChanged := True;
    FreeAndNil(BGBmp);
    FreeAndNil(BluredBmp);
    if DoRepaint and HandleAllocated and IsWindowVisible(Handle) then begin
      M := MakeMessage(SM_ALPHACMD, AC_SETBGCHANGED_HI + 1, 0, 0);
      BroadCast(M);
      Repaint;
    end;
  end;
end;


function TsSplitView.GetVisibleSize: integer;
begin
  case FState of
    svsaOpened: FVisibleSize := FOpenedSize;

    svsaClosed: FVisibleSize := CompSize;

    svsaOpening: if not FAnimationTimer.Enabled then
      FVisibleSize := FOpenedSize;

    svsaClosing: if not FAnimationTimer.Enabled then
      FVisibleSize := CompSize;
  end;
  Result := FVisibleSize;
end;


procedure TsSplitView.PaintBorder;
const
  BorderSize = 1;
var
  Pen: HPen;
  DC: hdc;
begin
  if Showing and (FBlurData.Mode = bmSplitView) and (DisplayMode = svmaOverlay) then begin
    if aDC = 0 then
      DC := GetWindowDC(Handle)
    else
      DC := aDC;

    try
      Pen := CreatePen(PS_SOLID, BorderSize, acColorToRGB(clBtnShadow));
      SelectObject(DC, Pen);
      case Placement of
        svpaLeft:   acPaintLine(DC, Width - BorderSize, 0, Width - BorderSize, Height);
        svpaRight:  acPaintLine(DC, 0, 0, 0, Height);
        svpaTop:    acPaintLine(DC, 0, Height - BorderSize, Width, Height - BorderSize);
        svpaBottom: acPaintLine(DC, 0, 0, Width, 0);
      end;
    finally
      if aDC = 0 then
        ReleaseDC(Handle, DC);
    end;
  end;
end;


procedure TsSplitView.ParentLock(DoLock, Redraw: boolean);
begin
  if not (csDesigning in ComponentState) and (Parent <> nil) and Parent.HandleAllocated then begin
    Parent.Perform(WM_SETREDRAW, integer(not DoLock), 0);
    if Redraw then
      RedrawWindow(Parent.Handle, nil, 0, RDWA_ALL);
  end;
end;


function TsSplitView.Position: integer;
begin
  case Placement of
    svpaRight:  Result := Parent.ClientWidth - VisibleSize;
    svpaBottom: Result := Parent.ClientHeight - VisibleSize;
    else // svpaLeft, svpaTop
      if ContentMoved then
        Result := -OpenedSize + VisibleSize
      else
        Result := 0;
  end;
end;


function TsSplitView.GetOpened: Boolean;
begin
  Result := FState = svsaOpened;
end;


procedure TsSplitView.KillParentBlur;
begin
  if ParentBlur <> nil then begin
    FreeAndNil(ParentBlur);
    FreeAndNil(BGBmp);
  end;
end;


procedure TsSplitView.Loaded;
begin
  inherited;
  if (FBlurData.Mode = bmSplitView) and (DisplayMode = svmaOverlay) then
    SetWindowPos(Handle, 0, 0, 0, 0, 0, SWPA_FRAMECHANGED);

  UpdatePosition;
end;


procedure TsSplitView.Open;
begin
  BringToFront;
  if FAnimatedShowing and ((SkinData.SkinManager = nil) or not SkinData.SkinManager.Effects.AllowAnimation) then
    SetState(svsaOpening)
  else
    SetState(svsaOpened);
end;


procedure TsSplitView.SetActBounds(aNewPos, aVisibleSize: integer);
begin
  if Placement in [svpaLeft, svpaRight] then
    SetBounds(aNewPos, 0, aVisibleSize, Parent.ClientHeight)
  else
    SetBounds(0, aNewPos, Parent.ClientWidth, aVisibleSize);
end;


procedure TsSplitView.SetCloseStyle(Value: TacSVCloseStyle);
begin
  if FCloseStyle <> Value then begin
    FCloseStyle := Value;
    UpdatePosition;
    if not (csLoading in ComponentState) then
      RedrawWindow(Handle, nil, 0, RDWA_ALL);
  end;
end;


procedure TsSplitView.SetCompactSize(Value: Integer);
begin
  if FCompactSize <> Value then begin
    FCompactSize := Value;
    if Placement in [svpaLeft, svpaRight] then begin
      if FCompactSize < Constraints.MinWidth then
        FCompactSize := Constraints.MinWidth;
    end
    else
      if FCompactSize < Constraints.MinHeight then
        FCompactSize := Constraints.MinHeight;

    if FCompactSize > FOpenedSize then
      FCompactSize := FOpenedSize;

    if (FCloseStyle = svcaCompact) then begin
      if (FDisplayMode = svmaOverlay) then
        if FOverlayPadding <> nil then
          if Placement in [svpaLeft, svpaRight] then
            FOverlayPadding.Width := FCompactSize
          else
            FOverlayPadding.Height := FCompactSize;

      if (FState in [svsaClosed, svsaClosing]) then
        UpdatePosition;
    end;
  end;
end;


procedure TsSplitView.SetContentMoved(const Value: boolean);
begin
  if FContentMoved <> Value then begin
    FContentMoved := Value;
    if not (csLoading in ComponentState) and not Opened then begin
      UpdatePosition;
    end;
  end;
end;


procedure TsSplitView.SetVisibleSize(const Value: integer);
begin
  FVisibleSize := Value;
end;


const
  acAligns:  array[TacSVPlacement] of TAlign   = (alLeft, alRight, alTop, alBottom);
  acAnchors: array[TacSVPlacement] of TAnchors = ([akLeft, akTop, akBottom], [akTop, akRight, akBottom], [akLeft, akTop, akRight], [akLeft, akBottom, akRight]);
  acStates:  array[boolean{Animated}, boolean{Opened}] of TacSVState = ((svsaClosed, svsaOpened), (svsaClosing, svsaOpening));


procedure TsSplitView.SetDisplayMode(Value: TacSVDisplayMode);
begin
  if FDisplayMode <> Value then begin
    FDisplayMode := Value;
    SkinData.BGChanged := True;
    if not (csLoading in ComponentState) then begin
      UpdatePosition;
      RefreshBackground(True);
      SkinData.Invalidate;
    end;
  end;
end;


procedure TsSplitView.SetOpened(Value: Boolean);
begin
  SetState(acStates[CanAnimate(svsaOpening), Value]);
end;


procedure TsSplitView.SetOpenedSize(Value: Integer);
begin
  if FOpenedSize <> Value then begin
    FOpenedSize := Value;
    if FOpenedSize < FCompactSize then
      FOpenedSize := FCompactSize;

    if Placement in [svpaLeft, svpaRight] then begin
      if (Constraints.MaxWidth > 0) and (FOpenedSize > Constraints.MaxWidth) then
        FOpenedSize := Constraints.MaxWidth;
    end
    else
      if (Constraints.MaxHeight > 0) and (FOpenedSize > Constraints.MaxHeight) then
        FOpenedSize := Constraints.MaxHeight;

    UpdatePosition;
  end;
end;


procedure TsSplitView.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  if FOverlayPadding <> nil then begin
    FOverlayPadding.Parent := AParent;
    FOverlayPadding.SendToBack;
  end;
end;


procedure TsSplitView.SetPlacement(Value: TacSVPlacement);
begin
  if FPlacement <> Value then begin
    FPlacement := Value;
    if not (csLoading in ComponentState) then begin
      ParentLock(True);
      UpdatePosition;
      RefreshBackground(True);
      ParentLock(False, True);
    end;
  end;
end;


procedure TsSplitView.SetState(Value: TacSVState);
var
  RestRect: TRect;
begin
  FState := Value;
  if FState in [svsaClosed, svsaClosing] then
    KillParentBlur;

  if not StateChanging then begin
    StateChanging := True;
    if Showing then
      case FState of
        svsaOpened, svsaOpening:
          if CanFocus and CanAutoHide then
            SetFocus;
      end;

    if Showing and CanAnimate(Value) then begin
      if not FAnimationTimer.Enabled then begin
        FAnimationTimer.Enabled := FState in [svsaOpening, svsaClosing];
        FAnimationTimer.OnTimer(FAnimationTimer);
        case FState of
          svsaOpened:  DoOpened;
          svsaOpening: DoOpening;
          svsaClosed:  DoClosed;
          svsaClosing: DoClosing;
        end;
      end;
    end
    else begin
      if FState in [svsaClosed, svsaClosing] then
        RestRect := BoundsRect;

      Perform(WM_SETREDRAW, 0, 0);
      UpdatePosition;
      Perform(WM_SETREDRAW, 1, 0);
      if FState in [svsaClosed, svsaClosing] then begin
        case FPlacement of
          svpaLeft:   RestRect.Left   := Left + VisibleSize;
          svpaRight:  RestRect.Right  := Left;
          svpaTop:    RestRect.Top    := Top + VisibleSize;
          svpaBottom: RestRect.Bottom := Top;
        end;
        RedrawWindow(Parent.Handle, @RestRect, 0, RDWA_ALLNOW);
      end
      else
        if (BlurData.Mode = bmParent) and (DisplayMode = svmaOverlay) then begin
          UpdateBGBmp;
          UpdateParentBlur;
        end;

      RedrawWindow(Handle, nil, 0, RDWA_ALLNOW);
      if FState = svsaOpened then
        DoOpened
      else
        DoClosed;

      case FState of
        svsaClosing:
          FVisibleSize := CompSize;
      end;
    end;
    case FState of
      svsaClosed: begin
        FVisibleSize := CompSize;
        RefreshBackground(False);
      end;
    end;
    StateChanging := False;
  end;
end;


procedure TacParentBlur.PaintWindow(DC: HDC);
begin
  if IsWindowVisible(Handle) and not (csDestroying in ComponentState) then begin
    if not (csPaintCopy in ControlState) or not SkinData.Skinned then begin
      PrepareCache;
      CopyCache(DC);
    end
    else
      OurPaint(DC);
  end;
end;


function TacParentBlur.PrepareCache: boolean;
var
  R: TRect;
  TmpBmp: TBitmap;
  BGInfo: TacBGInfo;
begin
  Result := True;
  if UpdateBG then begin
    if SkinData.FCacheBmp.Empty then begin
      if FOwner.BGBmp = nil then
        FOwner.UpdateBGBmp;

      InitCacheBmp(SkinData);
      BitBlt(SkinData.FCacheBmp.Canvas.Handle, 0, 0, Width, Height, FOwner.BGBmp.Canvas.Handle, Left + FOwner.BGOffset.X, Top + FOwner.BGOffset.Y, SRCCOPY);
    end;
    if FOwner.CanAnimate(svsaOpening) then begin
      if (SkinData.FCacheBmp.Width > 512) and (SkinData.FCacheBmp.Height > 512) then begin
        TmpBmp := CreateBmp32(SkinData.FCacheBmp.Width div 8, SkinData.FCacheBmp.Height div 8);
        BGInfo.PleaseDraw := False;
        GetBGInfo(@BGInfo, Parent);
        if BGInfo.BgType = btCache then
          BitBlt(TmpBmp.Canvas.Handle, 0, 0, TmpBmp.Width, TmpBmp.Height, BGInfo.Bmp.Canvas.Handle, Left + BGInfo.Offset.X, Top + BGInfo.Offset.Y, SRCCOPY)
        else
          FillDC(TmpBmp.Canvas.Handle, MkRect(TmpBmp), BGInfo.Color);

        R := Rect(0, 0, TmpBmp.Width - 1, TmpBmp.Height - 1);
        acgpStretchRect(TmpBmp, SkinData.FCacheBmp, R, Rect(0, 0, SkinData.FCacheBmp.Width - 1, SkinData.FCacheBmp.Height - 1));

        if BGInfo.BgType = btCache then
          BitBlt(SkinData.FCacheBmp.Canvas.Handle, 0, 0, SkinData.FCacheBmp.Width, SkinData.FCacheBmp.Height, BGInfo.Bmp.Canvas.Handle, Left + BGInfo.Offset.X, Top + BGInfo.Offset.Y, SRCCOPY)
        else
          FillDC(SkinData.FCacheBmp.Canvas.Handle, MkRect(SkinData.FCacheBmp), BGInfo.Color);

        acgpStretchRect(SkinData.FCacheBmp, TmpBmp, Rect(2, 2, SkinData.FCacheBmp.Width + 5, SkinData.FCacheBmp.Height + 5), R);
        TmpBmp.Free;
      end
      else
        acgpBlur(SkinData.FCacheBmp, 8);
    end
    else
      acgpBlur(SkinData.FCacheBmp, 16);

    UpdateBG := False;
  end;
end;


constructor TacParentBlur.Create(AOwner: TComponent);
begin
  inherited;
  Visible := False;
  FOwner := TsSplitView(AOwner);
  BevelOuter := bvNone;
  UpdateBG := False;
  PosUpdating := False;
  RefreshCount := 0;
end;


destructor TacParentBlur.Destroy;
begin
  inherited;
end;


procedure TacParentBlur.RefreshImage;
begin
  UpdateBG := True;
  SkinData.BGChanged := True;
  RedrawWindow(Handle, nil, 0, RDWA_ALLNOW);
  inc(RefreshCount);
end;


procedure TacParentBlur.UpdatePos;
var
  R: TRect;
begin
  if not Visible then begin
    PosUpdating := True;
    R := FOwner.Parent.ClientRect;
    case FOwner.Placement of
      svpaLeft:   R.Left   := FOwner.Left + FOwner.Width;
      svpaRight:  R.Right  := FOwner.Left;
      svpaTop:    R.Top    := FOwner.Top + FOwner.Height;
      svpaBottom: R.Bottom := FOwner.Top;
    end;
    SetBounds(R.Left, R.Top, WidthOf(R), HeightOf(R));
    Anchors := [akLeft, akTop, akBottom, akRight];
    Parent := FOwner.Parent;
    Visible := True;
    PosUpdating := False;
  end;
end;


procedure TacParentBlur.WndProc(var Message: TMessage);
begin
{$IFDEF LOGGED}
  AddToLog(Message);
{$ENDIF}
  case Message.Msg of
    SM_ALPHACMD:
      case Message.WParamHi of
        AC_REMOVESKIN, AC_REFRESH: begin
          UpdateBG := True;
          SkinData.BGChanged := True;
          FreeAndNil(FOwner.BGBmp);
          FreeAndNil(FOwner.BluredBmp);
        end;
      end;

    WM_ERASEBKGND:
      Exit;

    WM_PAINT:
      if FOwner.Printing or not Showing then
        Exit;

    WM_PRINT:
      if FOwner.Printing then begin
        Message.Result := NOCHILDRENPRINT;
        Exit;
      end;

    WM_WINDOWPOSCHANGED, WM_LBUTTONDOWN, WM_RBUTTONDOWN:
      if FOwner.Opened and Visible and not PosUpdating then begin
        Visible := False;
        FOwner.Close;
        Exit;
      end;
  end;
  inherited;
end;


procedure TsSplitView.UpdateParentBlur;
begin
  if not (csDesigning in COmponentState) and (BlurData.Mode = bmParent) and (FState <> svsaClosed) then begin
    if ParentBlur = nil then
      ParentBlur := TacParentBlur.Create(Self);

    with TacParentBlur(ParentBlur) do begin
      UpdatePos;
      RefreshImage;
    end;
  end
  else
    if ParentBlur <> nil then
      FreeAndNil(ParentBlur);
end;


procedure TsSplitView.UpdatePosition;
begin
  if not (csLoading in ComponentState) and not PosUpdating and (Parent <> nil) then begin
    PosUpdating := True;
    Parent.DisableAlign;
    if FDisplayMode = svmaDocked then begin
      Align := acAligns[FPlacement];
{
      case FPlacement of
        svpaLeft: begin
          Width := VisibleSize;
          Left := -1;
        end;
        svpaRight: begin
          Width := VisibleSize;
          Parent.DisableAlign;
          Left := Parent.ClientWidth + 1;
          Parent.EnableAlign;
        end;
        svpaTop: begin
          Height := VisibleSize;
          Top := -1;
        end;
        svpaBottom: begin
          Height := VisibleSize;
          Top := Parent.ClientHeight + 1;
        end;
      end;
}
      case FPlacement of
        svpaLeft, svpaRight: Width := VisibleSize;
        else                Height := VisibleSize;
      end;
      if FOverlayPadding <> nil then
        FreeAndNil(FOverlayPadding);
    end
    else begin
      if FOverlayPadding = nil then begin
        FOverlayPadding := TsPanel.Create(Self);
        FOverlayPadding.BevelOuter := bvNone;
      end;
      FOverlayPadding.Align := acAligns[FPlacement];
      if FPlacement in [svpaLeft, svpaRight] then
        FOverlayPadding.Width := CompSize
      else
        FOverlayPadding.Height := CompSize;

      FOverlayPadding.Parent := Parent;
      FOverlayPadding.SendToBack;
      Align := alNone;
      Anchors := acAnchors[FPlacement];
      SetActBounds(Position, WindowSize);
    end;
    Parent.EnableAlign;
    PosUpdating := False;
  end;
end;


procedure TsSplitView.UpdateBGBmp;
var
  Size: TSize;
  SavedDC: hdc;
  sp: TsSkinProvider;
begin
  if BGBmp = nil then begin
    Printing := True;
    sp := TsSkinProvider(SendAMessage(Parent.Handle, AC_GETPROVIDER));
    if (sp <> nil) and (TsSkinProvider(sp).SkinData.Skinned) then begin
      Size := MkSize(TsSkinProvider(sp).SkinData.FCacheBmp);
      BGOffset := Point(TsSkinProvider(sp).OffsetX, TsSkinProvider(sp).OffsetY);
    end
    else begin
      Size := MkSize(Parent.ClientWidth, Parent.Height);
      BGOffset := MkPoint;
    end;

    if BGBmp = nil then
      BGBmp := CreateBmp32(Size)
    else begin
      BGBmp.Width  := Size.cx;
      BGBmp.Height := Size.cy;
    end;

    SavedDC := SaveDC(BGBmp.Canvas.Handle);
    try
      if SkinData.Skinned then
        SkinPaintTo(BGBmp, Parent, 0, 0, sp)
      else
        StdPaintTo(BGBmp, Parent);
    finally
      RestoreDC(BGBmp.Canvas.Handle, SavedDC);
    end;

    if (BlurData.Mode = bmParent) and (FBlurData.Opacity <> 0) then
      BlendTransBitmap(BGBmp, MaxByte - FBlurData.Opacity, TsColor(acColorToRGB(FBlurData.Color)));

    Printing := False;
  end;
end;


function TsSplitView.WindowSize: integer;
begin
  if (FState in [svsaClosed, svsaClosing]) and ContentMoved and (DisplayMode = svmaOverlay) then 
    Result := FOpenedSize
  else    
    Result := VisibleSize;
end;


procedure TsSplitView.WMNCPaint(var Message: TMessage);
begin
  inherited;
  PaintBorder(0);
end;


procedure TsSplitView.WndProc(var Message: TMessage);
var
  i: integer;
begin
{$IFDEF LOGGED}
//  AddToLog(Message);
{$ENDIF}
  case Message.Msg of
    SM_ALPHACMD:
      case Message.WParamHi of
        AC_GETFONTINDEX:
          if SkinData.Skinned then begin
            PacPaintInfo(Message.LParam)^.FontIndex := SkinData.SkinIndex;
            Message.Result := 1;
            Exit;
          end;

        AC_GETBG: begin
          if not (PacBGInfo(Message.LParam)^.BgType in [btNotReady, btFill]) and not InUpdating(SkinData) then
            with PacBGInfo(Message.LParam)^ do begin
              BgType := btCache;
              PrepareCache;
              Bmp := FCommonData.FCacheBmp;
              Offset.X := 0;
              Offset.Y := 0;
            end;

          Exit;
        end

        else
          CommonMessage(Message, FCommonData);
      end;

    WM_PAINT{, WM_ERASEBKGND}:
      if Printing or not Showing then
        Exit;

    WM_ERASEBKGND: Exit;

    WM_PRINT:
      if Printing then begin
        Message.Result := NOCHILDRENPRINT;
        Exit;
      end;

    WM_SIZE: begin
      if BlurData.Mode = bmSplitView then begin
        if not FAnimationTimer.Enabled then begin
          FreeAndNil(BGBmp);
          FreeAndNil(BluredBmp);
        end;
        SkinData.BGChanged := True;
      end;
      for i := 0 to ControlCount - 1 do
        if not (csDestroying in Controls[i].ComponentState) then
          Controls[i].Perform(SM_ALPHACMD, AC_SETCHANGEDIFNECESSARY shl 16, 0);
    end;

    WM_MOVE: begin
      if BlurData.Mode = bmSplitView then begin
        SkinData.BGChanged := True;
        if not FAnimationTimer.Enabled then
          RefreshBackground(False);
      end;
      for i := 0 to ControlCount - 1 do
        if not (csDestroying in Controls[i].ComponentState) then
          Controls[i].Perform(SM_ALPHACMD, AC_SETCHANGEDIFNECESSARY shl 16, 0);
    end;
  end;
  inherited;
  case Message.Msg of
    SM_ALPHACMD:
      case Message.WParamHi of
        AC_REMOVESKIN, AC_REFRESH:
          RefreshBackground(True);
      end;

    CM_FOCUSCHANGED, WM_KILLFOCUS:
      if CanAutoHide and not ContainsWnd(GetFocus, Handle) then
        Close;

    WM_SIZE:
      if not FAnimationTimer.Enabled and CanAutoHide then
        Close;

    WM_PRINT:
      PaintBorder(hdc(Message.WParam));

    WM_MOVE:
      if not FAnimationTimer.Enabled and CanAutoHide then
        Close
      else
        SetWindowPos(Handle, 0, 0, 0, 0, 0, SWPA_FRAMECHANGED);
{
    WM_WINDOWPOSCHANGING:
      if not FAnimationTimer.Enabled and (Parent <> nil) then
        with TWMWindowPosChanging(Message) do
          case Placement of
            svpaLeft, svpaRight: begin
              WindowPos.x := Position;
              WindowPos.y := 0;
              WindowPos.cx := WindowSize;
              WindowPos.cy := Parent.ClientHeight;
            end
            else begin // svpaTop, svpaBottom:
              WindowPos.y := Position;
              WindowPos.x := 0;
              WindowPos.cy := WindowSize;
              WindowPos.cx := Parent.ClientWidth;
            end;
          end;
}
    WM_NCCALCSIZE:
      if not (csLoading in ComponentState) and (BlurData.Mode = bmSplitView) and (DisplayMode = svmaOverlay) then
        case Placement of
          svpaLeft:   dec(TNCCalcSizeParams(Pointer(Message.LParam)^).rgrc[0].Right);
          svpaRight:  inc(TNCCalcSizeParams(Pointer(Message.LParam)^).rgrc[0].Left);
          svpaTop:    dec(TNCCalcSizeParams(Pointer(Message.LParam)^).rgrc[0].Bottom);
          svpaBottom: inc(TNCCalcSizeParams(Pointer(Message.LParam)^).rgrc[0].Top);
        end;
  end;
end;


constructor TacBlurData.Create(AOwner: TsCustomPanel);
begin
  FOwner := AOwner;
  FOpacity := 127;
  FColor := clBtnFace;
  FMode := bmSplitView;
  FSize := 10;
end;


procedure TacBlurData.Invalidate;
var
  i: integer;
begin
  if not (csLoading in FOwner.ComponentState) then begin
    TsSplitView(FOwner).RefreshBackground(True);
    FOwner.SkinData.BGChanged := True;
    for i := 0 to FOwner.ControlCount - 1 do
      if not (csDestroying in FOwner.Controls[i].ComponentState) then
        FOwner.Controls[i].Perform(SM_ALPHACMD, AC_SETCHANGEDIFNECESSARY shl 16, 0);

    RedrawWindow(FOwner.Handle, nil, 0, RDWA_ALLNOW);
  end;
end;


procedure TacBlurData.SetBytes(const Index: Integer; const Value: byte);

  procedure ChangeProp(var Prop: byte; Value: byte);
  begin
    if Prop <> Value then begin
      Prop := Value;
      Invalidate;
    end;
  end;

begin
  case Index of
    0: ChangeProp(FOpacity, Value);
    1: ChangeProp(FSize, Value);
  end;
end;


procedure TacBlurData.SetColor(const Value: TColor);
begin
  if FColor <> Value then begin
    FColor := Value;
    Invalidate;
  end;
end;


procedure TacBlurData.SetMode(const Value: TacSVBlurMode);
begin
  if FMode <> Value then begin
    FMode := Value;
{
    if FMode = bmSplitView then
      FOwner.BevelOuter := bvNone
    else
      FOwner.BevelOuter := bvRaised;
}
    if not (csLoading in FOwner.ComponentState) then
      SetWindowPos(FOwner.Handle, 0, 0, 0, 0, 0, SWPA_FRAMECHANGED);

    Invalidate;
  end;
end;

end.
