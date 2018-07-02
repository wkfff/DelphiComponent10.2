unit acArcControls;
{$I sDefs.inc}
//{$DEFINE LOGGED}

interface

uses
  {$IFNDEF DELPHI5} Types, {$ENDIF}
  Windows, Graphics, SysUtils, Classes, Controls, Messages, ImgList, Buttons, ActnList,
  sCommonData, acThdTimer, sConst, acntTypes, sSkinManager;

{$IFNDEF NOTFORHELP}
const
  DefaultSize = 82;
  DefaultBadgeSize = 20;
  DefaultBaseBlend = 222;
{$ENDIF}

type
  TsArcControl = class(TWinControl)
{$IFNDEF NOTFORHELP}
  private
    FBaseAngle,
    FGlyphBlend,
    FGlyphIndex: integer;

    FShowText,
    FGlyphGrayed,
    FUseEllipsis,
    FGlyphReflected: boolean;

    FCanvas: TCanvas;
    FImages: TCustomImageList;
    FCommonData: TsCommonData;
    FImageChangeLink: TChangeLink;
    FGlyph: TBitmap;
    FTransparent: boolean;
    procedure ImageListChange(Sender: TObject);
    procedure SetImages (const Value: TCustomImageList);
    procedure SetInteger(const Index: integer; const Value: integer); virtual;
    procedure SetBoolean(const Index: integer; const Value: boolean); virtual;
    procedure WMPaint     (var Message: TWMPaint); message WM_PAINT;
    procedure WMEraseBkGnd(var Message: TWMPaint); message WM_ERASEBKGND;
    function GetGlyphIndex: integer; virtual;
    procedure SetGlyph(const Value: TBitmap);
  protected
    Center: TPoint;
    procedure Init;
    procedure Paint; virtual;
    function CurrentState: integer; virtual;
    procedure PrepareCache; virtual; abstract;
    procedure PaintGlyph(R: TRect; GrayColor: TColor = clNone);
    function Diameter: integer;
    function FontColor: TColor;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    property Canvas: TCanvas read FCanvas;
  public
    constructor Create(AOwner: TComponent); override;
    procedure WndProc(var Message: TMessage); override;
    destructor Destroy; override;
    procedure Loaded; override;
    property GlyphIndex: integer index 0 read FGlyphIndex  write SetInteger default -1;
    property BaseAngle:  integer index 1 read FBaseAngle   write SetInteger default -i90;

    property Transparent:boolean index 4 read FTransparent write SetBoolean default False;
    property ShowText:   boolean index 0 read FShowText    write SetBoolean default True;
  published
    property Align;
    property Anchors;
    property Font;
    property Constraints;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Height default DefaultSize;
    property Width default DefaultSize;
    property Visible;
{$ENDIF}
    property Glyph: TBitmap read FGlyph write SetGlyph;

    property GlyphBlend: integer index 2 read FGlyphBlend write SetInteger default 0;

    property GlyphReflected: boolean Index 1 read FGlyphReflected write SetBoolean default False;
    property GlyphGrayed:    boolean index 2 read FGlyphGrayed    write SetBoolean default False;
    property UseEllipsis:    boolean index 3 read FUseEllipsis    write SetBoolean default True;

    property Images: TCustomImageList read FImages write SetImages;
    property SkinData: TsCommonData read FCommonData write FCommonData;
{$IFNDEF NOTFORHELP}
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
{$ENDIF}
  end;

{$IFNDEF NOTFORHELP}
  TacGaugePaintData = record
    ABaseColor,
    AFontColor,
    AProgressColor: TColor;
    ALineWidth: integer;
    AText: string;
  end;

  TOnGetGaugePaintData = procedure(Sender: TObject; var PaintData: TacGaugePaintData) of object;
{$ENDIF} // NOTFORHELP

{$IFDEF DELPHI_XE3}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]{$ENDIF}
  TsArcGauge = class(TsArcControl)
{$IFNDEF NOTFORHELP}
  private
    FMin,
    FMax,
    FProgress: int64;

    FBaseBlend,
    FLineWidth: integer;

    FBaseColor,
    FProgressColor: TColor;

    FShowTextValue: boolean;
    FOnGetPaintData: TOnGetGaugePaintData;
    procedure SetInteger (const Index: integer; const Value: integer); override;
    procedure SetInt64   (const Index: integer; Value: int64);
    procedure SetShowTextValue(const Value: boolean);
    procedure SetColor(const Index: Integer; const Value: TColor);
  protected
    procedure PrepareCache; override;
    function ShownText: string;
  public
    constructor Create(AOwner: TComponent); override;
    function CanFocus: Boolean; override;
  published
{$ENDIF}
    property Min:      int64 index 10 read FMin      write SetInt64;
    property Max:      int64 index 11 read FMax      write SetInt64 default 100;
    property Progress: int64 index 12 read FProgress write SetInt64 default 47;

    property LineWidth: integer index 10 read FLineWidth write SetInteger default 6;
    property BaseBlend: integer index 12 read FBaseBlend write SetInteger default DefaultBaseBlend;

    property BaseColor:     TColor index 10 read FBaseColor     write SetColor default $00969696;
    property ProgressColor: TColor index 11 read FProgressColor write SetColor default $00FF9122;

    property ShowTextValue: boolean read FShowTextValue write SetShowTextValue default True;
    property ShowText;
    property BaseAngle;
    property GlyphIndex;
    property Text;
    {:@event}
    property OnGetPaintData: TOnGetGaugePaintData read FOnGetPaintData write FOnGetPaintData;
  end;


{$IFNDEF NOTFORHELP}
  TacPreloaderPaintData = record
    ABaseColor,
    ALineColor,
    AFontColor: TColor;
    ALineWidth: integer;
    AText: string;
  end;

  TOnGetPreloaderPaintData = procedure(Sender: TObject; var PaintData: TacPreloaderPaintData) of object;

  TsArcPreloader= class;
  TacPreAnimTimer = class(TacThreadedTimer)
  protected
    FOwner: TsArcPreloader;
  public
    procedure OnTimerProc(Sender: TObject);
    constructor Create(AOwner: TComponent); override;
  end;

  TacMotionType = (mtNormal, mtAcceleration);
  TacImageType = (itLine, itMask1, itMask2, itCustomMask, itCustomImage);
{$ENDIF} // NOTFORHELP

{$IFDEF DELPHI_XE3}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]{$ENDIF}
  TsArcPreloader = class(TsArcControl)
{$IFNDEF NOTFORHELP}
  private
    FSpace,
    FAnimStep,
    FBaseBlend,
    FLineAngle,
    FLineWidth,
    FProgressAngle: integer;

    FBaseColor,
    FLineColor: TColor;

    FStretched,
    FAnimated: boolean;

    FCustomImage,
    StretchBmp: TBitmap;

    ActualProgress: real;
    FMotionType: TacMotionType;
    FOnGetPaintData: TOnGetPreloaderPaintData;
    AnimTimer: TacPreAnimTimer;
    FImageType: TacImageType;
    procedure SetImageType(const Value: TacImageType);
    procedure SetCustomImage(const Value: TBitmap);
    procedure SetInteger(const Index: integer; const Value: integer); override;
    procedure SetColor  (const Index: Integer; const Value: TColor);
    procedure SetBoolean(const Index: Integer; const Value: boolean); override;
  protected
    procedure InitAnimation(Active: boolean);
    procedure PrepareCache; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function CanFocus: Boolean; override;
    procedure DoProgress(ANewText: string = ''; AStep: integer = 0);
    procedure Loaded; override;
  published
{$ENDIF}
    property Animated:  boolean index 10 read FAnimated  write SetBoolean default True;
    property Stretched: boolean index 11 read FStretched write SetBoolean default True;

    property LineWidth: integer index 10 read FLineWidth write SetInteger default 6;
    property Space:     integer index 11 read FSpace     write SetInteger default 4;
    property LineAngle: integer index 12 read FLineAngle write SetInteger default 120;
    property AnimStep:  integer index 13 read FAnimStep  write SetInteger default 4;
    property BaseBlend: integer index 14 read FBaseBlend write SetInteger default DefaultBaseBlend;

    property BaseColor: TColor  index 10 read FBaseColor write SetColor   default $00969696;
    property LineColor: TColor  index 11 read FLineColor write SetColor   default $00FF9122;

    property MotionType: TacMotionType read FMotionType write FMotionType default mtNormal;
    property ImageType: TacImageType read FImageType write SetImageType default itLine;

    property CustomImage: TBitmap read FCustomImage write SetCustomImage;
    property ShowText;
    property BaseAngle;
    property GlyphIndex;
    property Text;

    {:@event}
    property OnGetPaintData: TOnGetPreloaderPaintData read FOnGetPaintData write FOnGetPaintData;
  end;


{$IFNDEF NOTFORHELP}
  TacHandlePaintData = record
    AFontColor,
    ABaseColor,
    ABorderColor: TColor;
    ABorderWidth: integer;
    AText: string;
  end;

  TOnGetButtonPaintData = procedure(Sender: TObject; var PaintData: TacButtonPaintData) of object;

  TacKnobPosition = (kpCircle, kpBevel);
{$ENDIF} // NOTFORHELP

  TsArcKnobControl = class(TsArcControl)
{$IFNDEF NOTFORHELP}
  private
    FMin,
    FMax,
    FValue,
    FKnobSize,
    FBorderWidth: integer;

    FBaseColor,
    FBorderColor: TColor;

    FPressed,
    FShowValue: boolean;

    Multiplier: real;
    FOnChange: TNotifyEvent;
    FKnobPosition: TacKnobPosition;
    FPaintOptions: TacPaintButtonOptions;
    FOnGetHandlePaintData: TOnGetButtonPaintData;
    procedure SetInteger(const Index: integer; const Value: integer); override;
    procedure SetKnobPosition(const Value: TacKnobPosition);
  protected
    RealPosition: real;
    CurrentMouse: TPoint;
    function GlyphSize: TSize;
    function ContentSize: TSize;
    function HandleText: string; virtual; abstract;
    procedure DoInvalidate(Sender: TObject);
    procedure PaintHandle(ARect: TRect);
    function ValueToAngle(AValue: integer): integer; virtual; abstract;
    procedure PaintBG;
    procedure ChangePosition(pSrc, pDst: TPoint);
    function CanChange(var ADelta: real): boolean; virtual;
    function HandleRect: TRect; virtual; abstract;
  public
    function AngleToValue(AAngle: integer): integer;
    procedure WndProc(var Message: TMessage); override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
  published
{$ENDIF}
    property Value:       integer index 10 read FValue       write SetInteger default 0;
    property Min:         integer index 11 read FMin         write SetInteger default 0;
    property Max:         integer index 12 read FMax         write SetInteger default 0;
    property KnobSize:    integer index 14 read FKnobSize    write SetInteger default 5;

    property KnobPosition: TacKnobPosition read FKnobPosition write SetKnobPosition default kpCircle;

    property PaintOptions: TacPaintButtonOptions read FPaintOptions write FPaintOptions;
    property GlyphIndex;
    property ShowText;
    property Text;
    property Transparent;

    {:@event}
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    {:@event}
    property OnGetHandlePaintData: TOnGetButtonPaintData read FOnGetHandlePaintData write FOnGetHandlePaintData;
  end;


{$IFNDEF NOTFORHELP}
  TacTextContent = (tcValue, tcPercent, tcAngle, tcCustom);
{$ENDIF} // NOTFORHELP

{$IFDEF DELPHI_XE3}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]{$ENDIF}
  TsArcHandle = class(TsArcKnobControl)
{$IFNDEF NOTFORHELP}
  private
    FRotationAngle: integer;
    FTextContent: TacTextContent;
    procedure SetTextContent(const Value: TacTextContent);
    procedure SetInteger(const Index: integer; const Value: integer); override;
  protected
    function ValueToAngle(AValue: integer): integer; override;
    function HandleText: string; override;
    procedure PrepareCache; override;
    function HandleRect: TRect; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
{$ENDIF}
    property RotationAngle: integer index 100 read FRotationAngle write SetInteger default 0;
    property TextContent: TacTextContent read FTextContent write SetTextContent default tcAngle;
    property ShowText;
  end;


{$IFNDEF NOTFORHELP}
  TacDialPaintData = record
    APenColor,
    ABaseColor,
    AFontColor: TColor;
    APenWidth: integer;
  end;

  TOnGetDialPaintData = procedure(Sender: TObject; var PaintData: TacDialPaintData) of object;
{$ENDIF} // NOTFORHELP

{$IFDEF DELPHI_XE3}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]{$ENDIF}
  TsArcDial = class(TsArcKnobControl)
{$IFNDEF NOTFORHELP}
  private
    StartAngle,
    TextWidthD2,
    TextHeightD2,
    FTicksBigStep,
    FTickBigLength,
    FTicksSmallStep: integer;

    TickSmallBeginRadius,
    TickBigBeginRadius,
    CenterTextRadius,
    TickEndRadius,
    HandleRadius: integer;

    TextSize: TSize;

    FOnGetDialPaintData: TOnGetDialPaintData;
  protected
    function HandleText: string; override;
    procedure PaintDial;
    procedure ShowValueText;
    procedure PrepareCache; override;
    procedure InitPaintData;
    function ValueToAngle(AValue: integer): integer; override;
    procedure SetInteger(const Index: integer; const Value: integer); override;
    function HandleRect: TRect; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
{$ENDIF}
    property TicksBigStep:   integer index 100 read FTicksBigStep   write SetInteger default 10;
    property TicksSmallStep: integer index 101 read FTicksSmallStep write SetInteger default 2;
    property TickBigLength:  integer index 102 read FTickBigLength  write SetInteger default 6;
    property Enabled;
    property ShowText;
{$IFNDEF NOTFORHELP}
    property Width  default 200;
    property Height default 200;
{$ENDIF} // NOTFORHELP
    property Max    default 100;
    {:@event}
    property OnGetDialPaintData: TOnGetDialPaintData read FOnGetDialPaintData write FOnGetDialPaintData;
  end;


  TacCustomRoundBtn = class(TsArcControl)
  private
{$IFNDEF NOTFORHELP}
    FOnMouseEnter,
    FOnMouseLeave: TNotifyEvent;

    FPressed,
    FWordWrap,
    FReflected,
    FShowCaption: boolean;
    FCaption: acString;
    FPaintOptions: TacPaintButtonOptions;
    FOnGetButtonPaintData: TOnGetButtonPaintData;
    FImageIndex,
    FImageIndexHot,
    FImageIndexPressed,
    FImageIndexSelected,
    FImageIndexDisabled: integer;
    FLayout: TButtonLayout;
    procedure CMMouseEnter   (var Message: TMessage);     message CM_MOUSEENTER;
    procedure CMMouseLeave   (var Message: TMessage);     message CM_MOUSELEAVE;
    procedure WMNCHitTest    (var Message: TWMNCHitTest); message WM_NCHITTEST;
    procedure WMLButtonDown  (var Message: TWMMouse);     message WM_LBUTTONDOWN;
    procedure WMLButtonUp    (var Message: TWMMouse);     message WM_LBUTTONUP;
    procedure WMLButtonDblClk(var Message: TWMMouse);     message WM_LBUTTONDBLCLK;
    procedure WMKeyDown      (var Message: TWMKeyDown);   message WM_KEYDOWN;
    procedure WMKeyUp        (var Message: TWMKeyUp);     message WM_KEYUP;
    procedure SetLayout(const Value: TButtonLayout);
    procedure SetCaption(const Value: acString);
  protected
    procedure PaintBG;
    procedure PaintContent;
    function GlyphSize: TSize;
    function ContentSize: TSize;
    function GetTextSize: TSize;
    procedure PrepareCache; override;
    procedure InitPaintData(State: integer; var dpData: TacButtonPaintData); virtual;
    function CurrentState: integer; override;
    procedure SetInteger(const Index: integer; const Value: integer); override;
    procedure SetBoolean(const Index: integer; const Value: boolean); override;
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
    function GetGlyphIndex: integer; override;
    procedure DoInvalidate(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Click; override;
    procedure WndProc(var Message: TMessage); override;
  published
    property Action;
    property Enabled;
    property Transparent;
    property TabStop default True;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
{$ENDIF} // NOTFORHELP
    property ImageIndex:        integer index 10 read FImageIndex         write SetInteger default -1;
    property ImageIndexHot:     integer index 11 read FImageIndexHot      write SetInteger default -1;
    property ImageIndexPressed: integer index 12 read FImageIndexPressed  write SetInteger default -1;
    property ImageIndexDisabled:integer index 13 read FImageIndexDisabled write SetInteger default -1;
    property ImageIndexSelected:integer index 14 read FImageIndexSelected write SetInteger default -1;

    property ShowCaption: boolean index 10 read FShowCaption write SetBoolean default True;
    property Reflected:   boolean index 11 read FReflected   write SetBoolean default False;
    property WordWrap:    boolean index 12 read FWordWrap    write SetBoolean default True;

    property Caption: acString read FCaption write SetCaption;
    property PaintOptions: TacPaintButtonOptions read FPaintOptions write FPaintOptions;
    property Layout: TButtonLayout read FLayout write SetLayout default blGlyphLeft;
    {:@event}
    property OnGetButtonPaintData: TOnGetButtonPaintData read FOnGetButtonPaintData write FOnGetButtonPaintData;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  end;


{$IFDEF DELPHI_XE3}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]{$ENDIF}
  TsRoundBtn = class(TacCustomRoundBtn);


{$IFNDEF NOTFORHELP}
  TacBadgeAlignTo = (baTopLeft, baTopRight, baBottomLeft, baBottomRight);
{$ENDIF} // NOTFORHELP

{$IFDEF DELPHI_XE3}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]{$ENDIF}
  TsBadgeBtn = class(TacCustomRoundBtn)
{$IFNDEF NOTFORHELP}
  private
    FAttachTo: TControl;
    FPrinting,
    FRealigning: boolean;
    FOldWinProc: TWndMethod;
    FAlignTo: TacBadgeAlignTo;
    FOffsetX,
    FOffsetY: integer;
    procedure NewWinProc(var Message: TMessage);
    procedure SetAttachTo(const Value: TControl);
    procedure SetAlignTo(const Value: TacBadgeAlignTo);
    function NewPos: TPoint;
    procedure UpdateRgn;
  protected
    procedure PrepareCache; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetInteger(const Index: integer; const Value: integer); override;
    procedure InitPaintData(State: integer; var dpData: TacButtonPaintData); override;
  public
    procedure WndProc(var Message: TMessage); override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AdjustPos;
  published
    property Width default DefaultBadgeSize;
    property Height default DefaultBadgeSize;
{$ENDIF} // NOTFORHELP
    property AlignTo: TacBadgeAlignTo read FAlignTo write SetAlignTo default baTopRight;
    property AttachTo: TControl read FAttachTo write SetAttachTo;
    property OffsetX: integer index 100 read FOffsetX write SetInteger default 0;
    property OffsetY: integer index 101 read FOffsetY write SetInteger default 0;
  end;


{$IFNDEF NOTFORHELP}
procedure InitBtnPaintData(SM: TsSkinManager; SkinData: TsCommonData; State: integer; PaintOptions: TacPaintButtonOptions; var dpData: TacButtonPaintData);
procedure DrawRoundButton(DC: hdc; Left, Top, Diameter: integer; dpData: TacButtonPaintData);
{$ENDIF} // NOTFORHELP

implementation

uses Math,
  {$IFDEF LOGGED}sDebugMsgs, {$ENDIF}
  sGraphUtils, sStyleSimply, sVCLUtils, acntUtils, acPng, sAlphaGraph, sMessages, acgpUtils, sDefaults, sButton, sThirdParty,
  sFade, acGlow;

{$R acArcControls.res}

const
  DialDegree = 290;

var
  Mask1, Mask2: TBitmap;


const
  BtnColors: array [0..2, 0..3] of TacPaletteColors = (
    (pcBtnColor1Normal,  pcBtnColor2Normal,  pcBtnBorderNormal,  pcBtnFontNormal),
    (pcBtnColor1Active,  pcBtnColor2Active,  pcBtnBorderActive,  pcBtnFontActive),
    (pcBtnColor1Pressed, pcBtnColor2Pressed, pcBtnBorderPressed, pcBtnFontPressed)
  );


procedure InitBtnPaintData(SM: TsSkinManager; SkinData: TsCommonData; State: integer; PaintOptions: TacPaintButtonOptions; var dpData: TacButtonPaintData);
var
  Data: TacDataNormal;
begin
  dpData.ABorderWidth := PaintOptions.BorderWidth;
  dpData.ABevelWidth := PaintOptions.BevelWidth;
  dpData.ATransparent := False;
  if (SM = nil) or ((SkinData <> nil) and SkinData.CustomColor) or not SM.CommonSkinData.Active then begin
    Data := PaintOptions.GetData(State);
    if SkinData <> nil then
      dpData.AFontColor := Data.FontColor
    else
      dpData.AFontColor := clBtnText;

    case Data.Color1 and $FF000000 of
      0, $FF000000: dpData.AColor1 := TColor($FF000000 or Cardinal(ColorToRGB(Data.Color1)))
      else          dpData.AColor1 := Data.Color1;
    end;
    case Data.Color2 and $FF000000 of
      0, $FF000000: dpData.AColor2 := TColor($FF000000 or Cardinal(ColorToRGB(Data.Color2)))
      else          dpData.AColor2 := Data.Color2;
    end;

//    dpData.AColor2 := TColor($FF000000 or Cardinal(ColorToRGB(Data.Color2)));
    dpData.ABorderColor := Data.BorderColor;
  end
  else begin
    dpData.AFontColor   := SM.Palette[BtnColors[State, 3]];
    dpData.ABorderColor := SM.Palette[BtnColors[State, 2]];
    dpData.AColor1      := TColor(SM.Palette[BtnColors[State, 0]]);
    dpData.AColor2      := TColor(SM.Palette[BtnColors[State, 1]]);
  end;
end;


procedure DrawRoundButton(DC: hdc; Left, Top, Diameter: integer; dpData: TacButtonPaintData);
var
  PenWidth, PenDiv2, PenDiv2_1, PenMod2, BevelDiv2, BevelMod2: integer;
  IntRect, cRect: TRect;
  C_: TsColor_;
begin
  PenWidth := dpData.ABorderWidth;
  PenDiv2 := PenWidth div 2;
  PenDiv2_1 := (PenWidth + 1) mod 2;
  PenMod2 := PenWidth mod 2;
  cRect := Rect(Left + PenDiv2 - 1 + PenMod2,
                Top  + PenDiv2 - 1 + PenMod2,
                Diameter - PenWidth - 1,
                Diameter - PenWidth - 1);

  IntRect := cRect;
  inc(IntRect.Right,  PenMod2);
  inc(IntRect.Bottom, PenMod2);
  with dpData do
    if not ATransparent then
      if ABevelWidth > 0 then begin
        inc(ABevelWidth);
        BevelDiv2 := ABevelWidth div 2;
        BevelMod2 := ABevelWidth mod 2;
        // Bevel
        acgpGradientRing(DC,
                         IntRect.Left + BevelDiv2 + BevelMod2 + PenDiv2,
                         IntRect.Top  + BevelDiv2 + BevelMod2 + PenDiv2,
                         IntRect.Right  - PenWidth - ABevelWidth - BevelMod2 + PenDiv2_1 + 1,
                         IntRect.Bottom - PenWidth - ABevelWidth - BevelMod2 + PenDiv2_1 + 1,
                         AColor1, AColor2, ABevelWidth);

        // Fill
        acgpGradientEllipse(DC,
                            IntRect.Left + ABevelWidth + PenDiv2,
                            IntRect.Top  + ABevelWidth + PenDiv2,
                            IntRect.Right  - PenWidth - 2 * ABevelWidth + PenDiv2_1 + 1,
                            IntRect.Bottom - PenWidth - 2 * ABevelWidth + PenDiv2_1 + 1,
                            AColor2, AColor1);

      end
      else
        acgpGradientEllipse(DC, IntRect.Left + PenDiv2,
                                IntRect.Top + PenDiv2,
                                IntRect.Right  - PenWidth + PenDiv2_1 + 1,
                                IntRect.Bottom - PenWidth + PenDiv2_1 + 1,
                                dpData.AColor1, dpData.AColor2);

  if PenWidth > 0 then begin
    C_.C := ColorToRGB(dpData.ABorderColor);
    C_.A := MaxByte;
    acGPDrawEllipse(DC, cRect.Left + PenDiv2_1, cRect.Top + PenDiv2_1, cRect.Right + PenMod2, cRect.Bottom + PenMod2, C_.C, PenWidth);
  end;
end;


constructor TsArcControl.Create(AOwner: TComponent);
begin
  FCommonData := TsCommonData.Create(Self, True);
  FCommonData.COC := COC_TsCircleControl;
  inherited;
  FGlyph := TBitmap.Create;
  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;
  Height := DefaultSize;
  Width := DefaultSize;
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageListChange;
  FBaseAngle := -i90;
  FGlyphBlend := 0;
  FGlyphIndex := -1;
  FShowText := True;
  FUseEllipsis := True;
  FGlyphGrayed := False;
  ControlStyle := ControlStyle + [csOpaque];
end;


function TsArcControl.CurrentState: integer;
begin
  Result := 0;
end;


destructor TsArcControl.Destroy;
begin
  if GetCaptureControl = Self then
    SetCaptureControl(nil);

  FCanvas.Free;
  FGlyph.Free;
  FreeAndNil(FCommonData);
  FreeAndNil(FImageChangeLink);
  inherited;
end;


function TsArcControl.Diameter: integer;
begin
  Result := math.Min(Width, Height) - 1;
end;


function TsArcControl.FontColor: TColor;

  function GetSkinFontColor: TColor;
  var
    Ndx: integer;
  begin
    Ndx := GetFontIndex(Self, FCommonData.SkinIndex, FCommonData.SkinManager);
    if IsValidIndex(Ndx, Length(FCommonData.SkinManager.gd)) then
      Result := FCommonData.SkinManager.gd[Ndx].Props[0].FontColor.Color
    else
      Result := FCommonData.SkinManager.Palette[pcLabelText];
  end;

begin
  if (FCommonData.SkinManager = nil) or FCommonData.CustomFont or not FCommonData.SkinManager.CommonSkinData.Active then
    Result := Font.Color
  else
    Result := GetSkinFontColor;
end;


function TsArcControl.GetGlyphIndex: integer;
begin
  Result := FGlyphIndex;
end;


procedure TsArcControl.Loaded;
begin
  inherited;
  FCommonData.Loaded;
end;


procedure TsArcControl.Paint;

  procedure FillBG(DC: hdc; R: TRect);
  var
    BGInfo: TacBGInfo;
  begin
    BGInfo.BgType := btUnknown;
    BGInfo.PleaseDraw := False;
    BGInfo.FillRect := MkRect;
    SendMessage(Parent.Handle, SM_ALPHACMD, AC_GETBG_HI, LPARAM(@BGInfo));
    if BGInfo.BgType = btCache then
      BitBlt(DC, R.Left, R.Top, WidthOf(R), HeightOf(R), BGInfo.Bmp.Canvas.Handle, Left + R.Left + BGInfo.Offset.X, Top + R.Top +BGInfo.Offset.Y, SRCCOPY)
    else
      if BGInfo.BgType = btFill then
        FillDC(DC, R, BGInfo.Color)
      else
        SendMessage(Parent.Handle, WM_ERASEBKGND, WParam(DC), 0);
  end;

begin
  Init;
  SkinData.FCacheBmp.Canvas.Lock;
  if SkinData.SkinIndex < 0 then
    FillBG(SkinData.FCacheBmp.Canvas.Handle, MkRect(SkinData.FCacheBmp));

  PrepareCache;
  BitBlt(Canvas.Handle, 0, 0, SkinData.FCacheBmp.Width, SkinData.FCacheBmp.Height, SkinData.FCacheBmp.Canvas.Handle, 0, 0, SRCCOPY);
  SkinData.FCacheBmp.Canvas.Unlock;
end;


procedure TsArcControl.PaintGlyph(R: TRect; GrayColor: TColor = clNone);
var
  DrawData: TacDrawGlyphData;
begin
  if R.Right - R.Left > 0 then begin
    DrawData.Images            := Images;
    DrawData.Glyph             := FGlyph;
    DrawData.ImageIndex        := GetGlyphIndex;
    DrawData.ImgRect           := R;
    DrawData.NumGlyphs         := 1;
    DrawData.Enabled           := Enabled;
    DrawData.Blend             := FGlyphBlend;
    DrawData.Down              := CurrentState = 2;
    if CurrentState <> 0 then
      DrawData.Grayed := False
    else
      DrawData.Grayed := FGlyphGrayed or (SkinData.SkinManager <> nil) and SkinData.SkinManager.Effects.DiscoloredGlyphs;

    DrawData.CurrentState      := CurrentState;
    DrawData.DisabledGlyphKind := DefDisabledGlyphKind;
    DrawData.Reflected         := GlyphReflected;

    DrawData.SkinManager := SkinData.SkinManager;
    DrawData.DstBmp := SkinData.FCacheBmp;
    DrawData.Canvas := DrawData.DstBmp.Canvas;
    DrawData.SkinIndex := -1;

    DrawData.BGColor := GrayColor;

    acDrawGlyphEx(DrawData);
  end;
end;


procedure TsArcControl.SetBoolean(const Index: integer; const Value: boolean);

  procedure ChangeProp(var Prop: boolean; Value: boolean);
  begin
    if Prop <> Value then begin
      Prop := Value;
      Repaint;
    end;
  end;

begin
  case Index of
    0: ChangeProp(FShowText, Value);
    1: ChangeProp(FGlyphReflected, Value);
    2: ChangeProp(FGlyphGrayed, Value);
    3: ChangeProp(FUseEllipsis, Value);
    4: ChangeProp(FTransparent, Value);
  end;
end;


procedure TsArcControl.SetGlyph(const Value: TBitmap);
begin
  FGlyph.Assign(Value);
  Repaint;
end;


procedure TsArcControl.SetInteger(const Index: integer; const Value: integer);

  procedure ChangeProp(var Prop: integer; Value: integer);
  begin
    if Prop <> Value then begin
      Prop := Value;
      Repaint;
    end;
  end;

begin
  case Index of
    0: ChangeProp(FGlyphIndex, Value);
    1: ChangeProp(FBaseAngle, Value);
    2: ChangeProp(FGlyphBlend, Value);
  end;
end;


procedure TsArcControl.SetImages(const Value: TCustomImageList);
begin
  if Images <> Value then begin
    if Images <> nil then
      Images.UnRegisterChanges(FImageChangeLink);

    FImages := Value;
    if Images <> nil then begin
      Images.RegisterChanges(FImageChangeLink);
      Images.FreeNotification(Self);
    end;
    if Visible or ([csDesigning, csLoading] * ComponentState = []) then
      Repaint;
  end;
end;


procedure TsArcControl.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = Images) then
    Images := nil;
end;


procedure TsArcControl.ImageListChange(Sender: TObject);
begin
  Repaint;
end;


procedure TsArcControl.Init;
var
  iSize: integer;
begin
  iSize := min(Width, Height);
  Center := Point(iSize div 2, iSize div 2);
  if SkinData.FCacheBmp = nil then
    SkinData.FCacheBmp := CreateBmp32(Self)
  else begin
    SkinData.FCacheBmp.PixelFormat := pf32bit;
    SkinData.FCacheBmp.Width := Width;
    SkinData.FCacheBmp.Height := Height;
  end;
end;


procedure TsArcControl.WMEraseBkGnd(var Message: TWMPaint);
begin
// Skip
end;


procedure TsArcControl.WMPaint(var Message: TWMPaint);
var
  SavedDC: hdc;
  PS: TPaintStruct;
begin
  if not (csDestroying in ComponentState) then begin
    Canvas.Lock;
    try
      if Message.DC <> 0 then begin
        Canvas.Handle := Message.DC;
        SavedDC := 0;
      end
      else begin
        Canvas.Handle := BeginPaint(Handle, PS);
        SavedDC := SaveDC(Canvas.Handle);
      end;
      try
        Paint;
      finally
        if Message.DC = 0 then begin
          RestoreDC(Canvas.Handle, SavedDC);
          ReleaseDC(Handle, Canvas.Handle);
          EndPaint(Handle, PS);
        end;
        Canvas.Handle := 0;
      end;
    finally
      Canvas.Unlock;
    end;
  end;
end;


procedure TsArcControl.WndProc(var Message: TMessage);
begin
{$IFDEF LOGGED}
//  AddToLog(Message);
{$ENDIF}
  case Message.Msg of
    SM_ALPHACMD:
      case Message.WParamHi of
        AC_GETDEFINDEX: begin
          if FCommonData.SkinManager <> nil then
            Message.Result := 1 + FCommonData.SkinManager.ConstData.Sections[ssTransparent];

          Exit;
        end

        else
          if CommonMessage(Message, SkinData) then
            Exit;
      end;
  end;
  inherited;
  case Message.Msg of
    WM_PRINT:
      WMPaint(TWMPaint(Message));

    CM_TEXTCHANGED:
      if not (csDestroying in ComponentState) then
        Repaint;
  end;
end;


function TsArcGauge.CanFocus: Boolean;
begin
  Result := False;
end;


constructor TsArcGauge.Create(AOwner: TComponent);
begin
  inherited;
  FMax := 100;
  FMin := 0;
  FProgress := 47;
  FShowTextValue := True;
  FBaseColor := $00969696;
  FProgressColor := $00FF9122;
  FBaseBlend := DefaultBaseBlend;
  FLineWidth := 6;
end;


procedure TsArcGauge.PrepareCache;
var
  C_: TsColor;
  cRect: TRect;
  BG: TacBGInfo;
  TmpBmp: TBitmap;
  RText, RGlyph: TRect;
  gpData: TacGaugePaintData;
  iSize, HandlePenWidth, PenWidthDiv2: integer;

  procedure InitData(var RectText, RectGlyph: TRect);
  var
    TextSize: TSize;
  begin
    if ShowText then begin
      acGetTextExtent(SkinData.FCacheBmp.Canvas.Handle, gpData.AText, TextSize);
      RectText.Top    := (iSize - TextSize.cy) div 2;
      RectText.Left   := (iSize - TextSize.cx) div 2;
      RectText.Bottom := RectText.Top + TextSize.cy;
      RectText.Right  := RectText.Left + TextSize.cx;
    end
    else
      TextSize := MkSize;

    if (FImages <> nil) and IsValidIndex(FGlyphIndex, FImages.Count) then begin
      RectGlyph.Left := (iSize - Images.Width) div 2;
      RectGlyph.Right := RectGlyph.Left + Images.Width;
      RectGlyph.Top := (iSize - Images.Height - acSpacing - TextSize.cy) div 2;
      RectGlyph.Bottom := RectGlyph.Top + Images.Height;
      OffsetRect(RectText, 0, Images.Width div 2 + acSpacing);
    end
    else
      RectGlyph := MkRect;
  end;

begin
  SkinData.FCacheBmp.Canvas.Font.Assign(Font);
  gpData.ALineWidth := FLineWidth;
  gpData.AText := ShownText;
  iSize := Diameter;

  gpData.AFontColor := FontColor;

  if (FCommonData.SkinManager = nil) or FCommonData.CustomColor or not FCommonData.SkinManager.CommonSkinData.Active then begin
    gpData.ABaseColor := FBaseColor;
    gpData.AProgressColor := FProgressColor;
  end
  else begin
    gpData.ABaseColor := gpData.AFontColor;
    if FCommonData.SkinManager.ConstData.Sections[ssSelection] >= 0 then
      gpData.AProgressColor := FCommonData.SkinManager.gd[FCommonData.SkinManager.ConstData.Sections[ssSelection]].Props[1].Color
    else
      gpData.AProgressColor := $00FF9122;
  end;

  if Assigned(FOnGetPaintData) then
    FOnGetPaintData(Self, gpData);

  HandlePenWidth := mini(gpData.ALineWidth, iSize div 2 + 1);
  PenWidthDiv2 := HandlePenWidth div 2;
  C_.C := ColorToRGB(gpData.ABaseColor);
  C_.A := MaxByte;

  if FCommonData.Skinned then begin
    BG.DrawDC := 0;
    BG.Offset := MkPoint;
    BG.PleaseDraw := False;
    GetBGInfo(@BG, Parent, False);
    PaintItem(FCommonData.SkinIndex, BGInfoToCI(@BG), True, 0, MkRect(Self), Point(Left, Top), SkinData.FCacheBmp, FCommonData.SkinManager);
  end;

  if HandlePenWidth > 0 then begin
    cRect := Rect(PenWidthDiv2, PenWidthDiv2, iSize - HandlePenWidth - 1, iSize - HandlePenWidth - 1);

    TmpBmp := CreateBmp32(SkinData.FCacheBmp);
    acGPDrawEllipse(TmpBmp.Canvas.Handle, cRect.Left, cRect.Top, cRect.Right, cRect.Bottom, FBaseBlend shl 16 or FBaseBlend shl 8 or FBaseBlend, HandlePenWidth);
    BlendBmpByMask(SkinData.FCacheBmp, TmpBmp, C_);
    TmpBmp.Free;

    if (FProgress > 0) and (Max > Min) then begin
      C_.C := ColorToRGB(gpData.AProgressColor);
      C_.A := MaxByte;
      acGPDrawArc(SkinData.FCacheBmp.Canvas.Handle, cRect.Left, cRect.Top, cRect.Right, cRect.Bottom, FBaseAngle, Progress * i360 / (Max - Min), C_.C, HandlePenWidth);
    end;
  end;
  InitData(RText, RGlyph);
  SkinData.FCacheBmp.Canvas.Font.Color := gpData.AFontColor;
  if RText.Right - RText.Left > 0 then begin
    SkinData.FCacheBmp.Canvas.Brush.Style := bsClear;
    SelectObject(SkinData.FCacheBmp.Canvas.Handle, SkinData.FCacheBmp.Canvas.Font.Handle);
    acDrawText(SkinData.FCacheBmp.Canvas.Handle, gpData.AText, RText, DT_NOCLIP or DT_CENTER or DT_VCENTER or DT_WORDBREAK or TextEllips[UseEllipsis]);
  end;
  PaintGlyph(RGlyph);
end;


procedure TsArcGauge.SetColor(const Index: Integer; const Value: TColor);

  procedure ChangeProp(var Prop: TColor; Value: integer);
  begin
    if Prop <> Value then begin
      Prop := Value;
      Repaint;
    end;
  end;

begin
  case Index of
    10: ChangeProp(FBaseColor, Value);
    11: ChangeProp(FProgressColor, Value);
  end;
end;


procedure TsArcGauge.SetInt64(const Index: integer; Value: int64);

  procedure ChangeProp(var Prop: int64; Value: integer);
  begin
    if Prop <> Value then begin
      Prop := Value;
      Repaint;
    end;
  end;

begin
  case Index of
    10: ChangeProp(FMin, Value);
    11: ChangeProp(FMax, Value);
    12: ChangeProp(FProgress, math.max(math.min(Value, Max), Min));
  end;
end;


procedure TsArcGauge.SetInteger(const Index: integer; const Value: integer);

  procedure ChangeProp(var Prop: integer; Value: integer);
  begin
    if Prop <> Value then begin
      Prop := Value;
      Repaint;
    end;
  end;

begin
  case Index of
    10: ChangeProp(FLineWidth, Value);
    12: ChangeProp(FBaseBlend, Value)
    else inherited;
  end;
end;


procedure TsArcGauge.SetShowTextValue(const Value: boolean);
begin
  if FShowTextValue <> Value then begin
    FShowTextValue := Value;
    Repaint;
  end;
end;


function TsArcGauge.ShownText: string;
begin
  if ShowTextValue then
    Result := IntToStr(Progress)
  else
    Result := Text;
end;


function TsArcPreloader.CanFocus: Boolean;
begin
  Result := False;
end;


constructor TsArcPreloader.Create(AOwner: TComponent);
begin
  inherited;
  FCustomImage := TBitmap.Create;
  FProgressAngle := 0;
  ActualProgress := -1;
  FBaseColor := $00969696;
  FLineColor := $00FF9122;
  FLineWidth := 6;
  FSpace := 4;
  FLineAngle := 120;
  FAnimStep := 4;
  FAnimated := True;
  FBaseBlend := DefaultBaseBlend;
  FMotionType := mtNormal;
  FImageType := itLine;
  FStretched := True;
end;


destructor TsArcPreloader.Destroy;
begin
  FCustomImage.Free;
  inherited;
end;


procedure TsArcPreloader.DoProgress(ANewText: string = ''; AStep: integer = 0);
var
  TmpStep: real;
begin
  while ActualProgress > i360 do
    ActualProgress := ActualProgress - i360;

  if ActualProgress < 0 then
    ActualProgress := 0;

  if MotionType = mtAcceleration then begin
    if ActualProgress < i180 then
      TmpStep := AStep * ActualProgress
    else
      TmpStep := AStep * (i360 - ActualProgress);

    ActualProgress := ActualProgress + TmpStep / i90 + 1;
    FProgressAngle := Round(ActualProgress) - LineAngle div 2;
  end
  else
    FProgressAngle := FProgressAngle + AStep;

  if Text <> ANewText then
    Text := ANewText
  else
    Repaint;
end;


procedure TsArcPreloader.InitAnimation(Active: boolean);
begin
  if Active then begin
    if AnimTimer = nil then
      AnimTimer := TacPreAnimTimer.Create(Self);

    AnimTimer.Interval := acTimerInterval;
    AnimTimer.Enabled := True;
  end
  else
    if AnimTimer <> nil then
      AnimTimer.Enabled := False;
end;


procedure TsArcPreloader.Loaded;
begin
  inherited;
  InitAnimation(FAnimated);
end;


procedure TsArcPreloader.PrepareCache;
var
  C_: TsColor;
  cRect: TRect;
  BG: TacBGInfo;
  RText, RGlyph: TRect;
  gpData: TacPreloaderPaintData;
  Bmp, SrcBmp, TmpBmp, MaskBmp: TBitmap;
  iSize, HandlePenWidth, PenWidthDiv2: integer;

  procedure InitData(var RectText, RectGlyph: TRect);
  var
    TextSize: TSize;
  begin
    if ShowText then begin
      acGetTextExtent(SkinData.FCacheBmp.Canvas.Handle, gpData.AText, TextSize);
      RectText.Top    := (iSize - TextSize.cy) div 2;
      RectText.Left   := (iSize - TextSize.cx) div 2;
      RectText.Bottom := RectText.Top + TextSize.cy;
      RectText.Right  := RectText.Left + TextSize.cx;
    end
    else
      TextSize := MkSize;

    if (FImages <> nil) and IsValidIndex(FGlyphIndex, FImages.Count) then begin
      RectGlyph.Left := (iSize - Images.Width) div 2;
      RectGlyph.Right := RectGlyph.Left + Images.Width;
      RectGlyph.Top := (iSize - Images.Height - Space - TextSize.cy) div 2;
      RectGlyph.Bottom := RectGlyph.Top + Images.Height;
      OffsetRect(RectText, 0, Images.Width div 2 + Space);
    end
    else
      RectGlyph := MkRect;
  end;

begin
  SkinData.FCacheBmp.Canvas.Font.Assign(Font);
  gpData.ALineWidth := FLineWidth;
  gpData.AText := Text;
  iSize := Diameter;
  gpData.AFontColor := FontColor;

  if (FCommonData.SkinManager = nil) or FCommonData.CustomColor or not FCommonData.SkinManager.CommonSkinData.Active then begin
    gpData.ABaseColor := FBaseColor;
    gpData.ALineColor := FLineColor;
  end
  else begin
    gpData.ABaseColor := gpData.AFontColor;
    if FCommonData.SkinManager.ConstData.Sections[ssSelection] >= 0 then
      gpData.ALineColor := FCommonData.SkinManager.gd[FCommonData.SkinManager.ConstData.Sections[ssSelection]].Props[1].Color
    else
      gpData.ALineColor := $00FF9122;
  end;

  if Assigned(FOnGetPaintData) then
    FOnGetPaintData(Self, gpData);

  HandlePenWidth := mini(gpData.ALineWidth, iSize div 2 + 1);
  PenWidthDiv2 := HandlePenWidth div 2;
  C_.C := ColorToRGB(gpData.ABaseColor);
  C_.A := MaxByte;

  if FCommonData.Skinned then begin
    BG.DrawDC := 0;
    BG.Offset := MkPoint;
    BG.PleaseDraw := False;
    GetBGInfo(@BG, Parent, False);
    PaintItem(FCommonData.SkinIndex, BGInfoToCI(@BG), True, 0, MkRect(Self), Point(Left, Top), SkinData.FCacheBmp, FCommonData.SkinManager);
  end;

  case ImageType of
    itLine: begin
      if HandlePenWidth > 0 then begin
        cRect := Rect(PenWidthDiv2, PenWidthDiv2, iSize - HandlePenWidth - 1, iSize - HandlePenWidth - 1);
        TmpBmp := CreateBmp32(SkinData.FCacheBmp);
        acGPDrawEllipse(TmpBmp.Canvas.Handle, cRect.Left, cRect.Top, cRect.Right, cRect.Bottom, FBaseBlend shl 16 or FBaseBlend shl 8 or FBaseBlend, HandlePenWidth);
        BlendBmpByMask(SkinData.FCacheBmp, TmpBmp, C_);
        TmpBmp.Free;
        C_.C := ColorToRGB(gpData.ALineColor);
        C_.A := MaxByte;
        acGPDrawArc(SkinData.FCacheBmp.Canvas.Handle, cRect.Left, cRect.Top, cRect.Right, cRect.Bottom, (FBaseAngle + FProgressAngle) mod i360, (FLineAngle) mod i360, C_.C, HandlePenWidth);
      end;
      InitData(RText, RGlyph);
      SkinData.FCacheBmp.Canvas.Font.Color := gpData.AFontColor;
      if RText.Right - RText.Left > 0 then begin
        SkinData.FCacheBmp.Canvas.Brush.Style := bsClear;
        SelectObject(SkinData.FCacheBmp.Canvas.Handle, SkinData.FCacheBmp.Canvas.Font.Handle);
        acDrawText(SkinData.FCacheBmp.Canvas.Handle, gpData.AText, RText, DT_NOCLIP or DT_CENTER or DT_VCENTER or DT_WORDBREAK or TextEllips[UseEllipsis]);
      end;
      PaintGlyph(RGlyph);
    end;

    itMask1, itMask2, itCustomMask: begin
      if ImageType = itMask1 then
        MaskBmp := Mask1
      else
        if ImageType = itMask2 then
          MaskBmp := Mask2
        else
          if not CustomImage.Empty then
            MaskBmp := CustomImage
          else
            Exit;

      Bmp := CreateBmp32(SkinData.FCacheBmp);
      FillRect32(Bmp, MkRect(Bmp), $FFFFFF, 0);
      if FStretched then begin
        if StretchBmp = nil then
          StretchBmp := CreateBmp32(iSize, iSize)
        else begin
          StretchBmp.Width := iSize;
          StretchBmp.Height := iSize;
        end;
        sGraphUtils.Stretch(MaskBmp, StretchBmp, StretchBmp.Width - 2, StretchBmp.Height - 2, ftMitchell);
        SrcBmp := StretchBmp;
      end
      else
        SrcBmp := MaskBmp;

      BitBltRotated(Bmp.Canvas.Handle, SrcBmp, Point(iSize div 2, iSize div 2), (FBaseAngle + FProgressAngle + i180) mod i360);
      BlendBmpByMask(SkinData.FCacheBmp, Bmp, TsColor(gpData.ALineColor));
      PaintBmp32(SkinData.FCacheBmp, Bmp);
      Bmp.Free;
    end;

    itCustomImage:
      if CustomImage.PixelFormat = pf32bit then begin
        Bmp := CreateBmp32(SkinData.FCacheBmp);
        FillRect32(Bmp, MkRect(Bmp), 0, 0);
        if Stretched then begin
          if StretchBmp = nil then
            StretchBmp := CreateBmp32(iSize, iSize)
          else begin
            StretchBmp.Width := iSize;
            StretchBmp.Height := iSize;
          end;
          sGraphUtils.Stretch(CustomImage, StretchBmp, StretchBmp.Width, StretchBmp.Height, ftMitchell);
          SrcBmp := StretchBmp;
        end
        else
          SrcBmp := CustomImage;

        BitBltRotated(Bmp.Canvas.Handle, SrcBmp, Point(iSize div 2, iSize div 2), (FBaseAngle + FProgressAngle + i180) mod i360);
        PaintBmp32(SkinData.FCacheBmp, Bmp);
        Bmp.Free;
      end
      else begin
        if Stretched then begin
          if StretchBmp = nil then
            StretchBmp := CreateBmp32(iSize, iSize)
          else begin
            StretchBmp.Width := iSize;
            StretchBmp.Height := iSize;
          end;
          sGraphUtils.Stretch(CustomImage, StretchBmp, StretchBmp.Width, StretchBmp.Height, ftMitchell);
          SrcBmp := StretchBmp;
          BitBltRotated(SkinData.FCacheBmp.Canvas.Handle, StretchBmp, Point(iSize div 2, iSize div 2), (FBaseAngle + FProgressAngle + i90) mod i360);
        end
        else
          SrcBmp := CustomImage;

        BitBltRotated(SkinData.FCacheBmp.Canvas.Handle, SrcBmp, Point(iSize div 2, iSize div 2), (FBaseAngle + FProgressAngle + i90) mod i360);
      end;
  end;
end;


procedure TsArcPreloader.SetBoolean(const Index: Integer; const Value: boolean);
begin
  case Index of
    10: if FAnimated <> Value then begin
      FAnimated := Value;
      InitAnimation(Value);
    end;

    11: begin
      if FStretched <> Value then begin
        FStretched := Value;
        if StretchBmp <> nil then
          FreeAndNil(StretchBmp);

        Repaint;
      end
    end

    else
      inherited;
  end;
end;


procedure TsArcPreloader.SetColor(const Index: Integer; const Value: TColor);

  procedure ChangeProp(var Prop: TColor; Value: integer);
  begin
    if Prop <> Value then begin
      Prop := Value;
      Repaint;
    end;
  end;

begin
  case Index of
    10: ChangeProp(FBaseColor, Value);
    11: ChangeProp(FLineColor, Value);
  end;
end;


procedure TsArcPreloader.SetCustomImage(const Value: TBitmap);
begin
  FCustomImage.Assign(Value);
  if StretchBmp <> nil then
    FreeAndNil(StretchBmp);

  Repaint;
end;


procedure TsArcPreloader.SetImageType(const Value: TacImageType);
begin
  if FImageType <> Value then begin
    FImageType := Value;
    if StretchBmp <> nil then
      FreeAndNil(StretchBmp);

    Repaint;
  end;
end;


procedure TsArcPreloader.SetInteger(const Index: integer; const Value: integer);

  procedure ChangeProp(var Prop: integer; Value: integer);
  begin
    if Prop <> Value then begin
      Prop := Value;
      Repaint;
    end;
  end;

begin
  case Index of
    10: ChangeProp(FLineWidth, Value);
    11: ChangeProp(FSpace, Value);
    12: ChangeProp(FLineAngle, Value);
    13: ChangeProp(FAnimStep, Value);
    14: ChangeProp(FBaseBlend, Value)
    else inherited;
  end;
end;


constructor TacPreAnimTimer.Create(AOwner: TComponent);
begin
  inherited;
  FOwner := TsArcPreloader(AOwner);
  OnTimer := OnTimerProc;
end;


procedure TacPreAnimTimer.OnTimerProc(Sender: TObject);
begin
  FOwner.DoProgress(FOwner.Text, FOwner.AnimStep);
end;


function TsArcKnobControl.AngleToValue(AAngle: integer): integer;
begin
  if AAngle < 0 then
    Result := -AAngle
  else
    Result := i360 - AAngle;
end;


function TsArcKnobControl.CanChange(var ADelta: real): boolean;
begin
  Result := True;
end;


procedure TsArcKnobControl.ChangePosition(pSrc, pDst: TPoint);
var
  r: real;

  function CrossProduct(const Point1, Point2: TPoint): Single;
  begin
    Result := Point1.X * Point2.Y - Point1.Y * Point2.X;
  end;

  function AngleBetween(const Point1, Point2: TPoint): real;
  var
    r1, r2, a1, a2, an1, an2: real;
  begin
    r1 := Point1.Y / Point1.X;
    a1 := ArcTan(r1);
    an1 := RadToDeg(a1);
    if Point1.X < 0 then
      an1 := i180 + an1
    else
      if Point1.Y < 0 then
        an1 := i360 + an1;

    r2 := Point2.Y / Point2.X;
    a2 := ArcTan(r2);
    an2 := RadToDeg(a2);
    if Point2.X < 0 then
      an2 := i180 + an2
    else
      if Point2.Y < 0 then
        an2 := i360 + an2;

    if (an2 > 300) and (an1 < i90) then
      an1 := an1 + i360;

    if (an1 > 300) and (an2 < i90) then
      an2 := an2 + i360;

    Result := an1 - an2;
  end;

begin
  if (pSrc.X <> pDst.X) or (pSrc.Y <> pDst.Y) then begin
    pSrc := ScreenToClient(pSrc);
    pDst := ScreenToClient(pDst);
    pSrc.X := pSrc.X - Center.X;
    pDst.X := pDst.X - Center.X;
    pSrc.Y := -(pSrc.Y - Center.Y);
    pDst.Y := -(pDst.Y - Center.Y);
    if (pSrc.X <> 0) and (pDst.X <> 0) then begin
      r := AngleBetween(pSrc, pDst) * Multiplier;
      if CanChange(r) then begin
        RealPosition := RealPosition + r;
        Value := Round(RealPosition);
      end;
    end;
  end;
end;


function TsArcKnobControl.ContentSize: TSize;
var
  TextSize: TSize;
begin
  if ShowText then
    acGetTextExtent(0, Caption, TextSize, Font.Handle)
  else
    TextSize := MkSize;

  with GlyphSize do begin
    Result.cx := math.max(cx, TextSize.cx);
    Result.cy := cy + iff(cy <> 0, acSpacing, 0) + TextSize.cy;
  end;
end;


constructor TsArcKnobControl.Create(AOwner: TComponent);
begin
  inherited;
  FMin := 0;
  FMax := 0;
  FValue := 0;
  FPressed := False;
  FBorderWidth := 1;
  FShowValue := False;
  FBaseColor := cl3DLight;
  FKnobPosition := kpCircle;
  FKnobSize := 5;
  Multiplier := 1;
  FBorderColor := clWindowFrame;
  CurrentMouse := Point(-1, -1);
  FPaintOptions := TacPaintButtonOptions.Create;
  FPaintOptions.OnInvalidate := Self.DoInvalidate;
end;


destructor TsArcKnobControl.Destroy;
begin
  FPaintOptions.Free;
  inherited;
end;


procedure TsArcKnobControl.DoInvalidate(Sender: TObject);
begin
  Invalidate;
end;


function TsArcKnobControl.GlyphSize: TSize;
begin
  if (Images <> nil) and IsValidIndex(GlyphIndex, GetImageCount(Images)) then
    Result := MkSize(GetImageWidth(Images), GetImageHeight(Images))
  else
    Result := MkSize;
end;


procedure TsArcKnobControl.Loaded;
begin
  inherited;
  RealPosition := FValue;
end;


procedure TsArcKnobControl.PaintBG;
var
  BG: TacBGInfo;
begin
  if FCommonData.Skinned then begin
    BG.DrawDC := 0;
    BG.Offset := MkPoint;
    BG.PleaseDraw := False;
    GetBGInfo(@BG, Parent, False);
    PaintItem(FCommonData.SkinIndex, BGInfoToCI(@BG), True, 0, MkRect(Self), Point(Left, Top), SkinData.FCacheBmp, FCommonData.SkinManager);
  end;
end;


procedure TsArcKnobControl.PaintHandle(ARect: TRect);
var
  R, iDiam, State, KnobDiv2, CurrentAngle: integer;
  dpData: TacButtonPaintData;
  RText, RGlyph: TRect;
  CurrentRad: real;
  KnobCoord: TPoint;

  procedure InitData(var RectText, RectGlyph: TRect);
  var
    TextSize, ImgSize, cSize: TSize;
  begin
    ImgSize := GlyphSize;
    cSize := ContentSize;
    if ShowText and (dpData.AText <> '') then begin
      acGetTextExtent(SkinData.FCacheBmp.Canvas.Handle, dpData.AText, TextSize);
      RectText.Left := (Width - TextSize.cx) div 2;
      RectText.Top := (Height - cSize.cy) div 2 + ImgSize.cy + iff(ImgSize.cy <> 0, acSpacing, 0);
      RectText.Bottom := RectText.Top + TextSize.cy;
      RectText.Right  := RectText.Left + TextSize.cx;
    end
    else begin
      RectText := MkRect;
      TextSize := MkSize;
    end;
    if ImgSize.cx <> 0 then begin
      RectGlyph.Left := (Width - ImgSize.cx) div 2;
      RectGlyph.Top  := (Height - cSize.cy)  div 2;
      RectGlyph.Bottom := RectGlyph.Top  + ImgSize.cy;
      RectGlyph.Right  := RectGlyph.Left + ImgSize.cx;
    end
    else
      RectGlyph := MkRect;
  end;

begin
  State := 0;

  iDiam := WidthOf(aRect);
  KnobDiv2 := KnobSize div 2;
  dpData.AText := HandleText;
  SkinData.FCacheBmp.Canvas.Font.Assign(Font);
  dpData.ABorderWidth := FBorderWidth;

  InitBtnPaintData(SkinData.SkinManager, SkinData, State, PaintOptions, dpData);

  dpData.ATransparent := FTransparent;
  if FTransparent then begin
    dpData.ABorderColor := FontColor;
    dpData.AFontColor := dpData.ABorderColor;
  end;

  if Assigned(FOnGetHandlePaintData) then
    FOnGetHandlePaintData(Self, dpData);

  InitData(RText, RGlyph);
  DrawRoundButton(SkinData.FCacheBmp.Canvas.Handle, aRect.Left, aRect.Top, iDiam, dpData);

  if (State = 2) and CanClickShift(SkinData.SkinIndex, SkinData.SkinManager) then begin
    OffsetRect(RText, 1, 1);
    OffsetRect(RGlyph, 1, 1);
  end;

  SkinData.FCacheBmp.Canvas.Font.Color := ColorToRGB(dpData.AFontColor);
  if RText.Right - RText.Left > 0 then begin
    SkinData.FCacheBmp.Canvas.Brush.Style := bsClear;
    SelectObject(SkinData.FCacheBmp.Canvas.Handle, SkinData.FCacheBmp.Canvas.Font.Handle);
    acDrawText(SkinData.FCacheBmp.Canvas.Handle, dpData.AText, RText, DT_NOCLIP or DT_CENTER or DT_VCENTER or DT_WORDBREAK or TextEllips[UseEllipsis]);
  end;
  PaintGlyph(RGlyph);
  CurrentAngle := ValueToAngle(Value);

  if FTransparent then
    R := 4 * iDiam div 10 - dpData.ABorderWidth
  else
    if FKnobPosition = kpCircle then
      R := iDiam div 2 - dpData.ABorderWidth - dpData.ABevelWidth - KnobDiv2 - acSpacing
    else
      R := iDiam div 2 - dpData.ABorderWidth - dpData.ABevelWidth div 2;

  CurrentRad := PI * CurrentAngle / i180;
  KnobCoord.X := aRect.Left + Round(R * cos(CurrentRad)) + iDiam div 2;
  KnobCoord.Y := aRect.Top  + Round(R * sin(CurrentRad)) + HeightOf(aRect) div 2;
  acgpFillEllipse(SkinData.FCacheBmp.Canvas.Handle,
                  KnobCoord.X - KnobDiv2,
                  KnobCoord.Y - KnobDiv2,
                  KnobSize, KnobSize, dpData.AFontColor);

  SkinData.FCacheBmp.Canvas.Font.Color := dpData.AFontColor;
  if RText.Right - RText.Left > 0 then begin
    SkinData.FCacheBmp.Canvas.Brush.Style := bsClear;
    SelectObject(SkinData.FCacheBmp.Canvas.Handle, SkinData.FCacheBmp.Canvas.Font.Handle);
    acDrawText(SkinData.FCacheBmp.Canvas.Handle, dpData.AText, RText, DT_NOCLIP or DT_CENTER or DT_VCENTER or DT_WORDBREAK or TextEllips[UseEllipsis]);
  end;
  PaintGlyph(RGlyph);
end;


procedure TsArcKnobControl.SetInteger(const Index: integer; const Value: integer);

  procedure ChangeProp(var Prop: integer; Value: integer);
  begin
    if Prop <> Value then begin
      Prop := Value;
      Repaint;
    end;
  end;

begin
  case Index of
    10: if FValue <> Value then begin
      FValue := Value;
      RealPosition := FValue;
      if Assigned(OnChange) then
        OnChange(Self);

      Repaint;
    end;
    11: ChangeProp(FMin, Value);
    12: ChangeProp(FMax, Value);
    13: ChangeProp(FBorderWidth, Value);
    14: ChangeProp(FKnobSize, math.min(20, math.max(1, Value)))
    else
      inherited;
  end;
end;


procedure TsArcKnobControl.SetKnobPosition(const Value: TacKnobPosition);
begin
  if FKnobPosition <> Value then begin
    FKnobPosition := Value;
    Invalidate;
  end;
end;


procedure TsArcKnobControl.WndProc(var Message: TMessage);
var
  p: TPoint;
begin
  inherited;
  if not (csDesigning in ComponentState) then
    case Message.Msg of
      WM_LBUTTONDOWN: begin
        FPressed := True;
        CurrentMouse := acMousePos;
      end;

      WM_LBUTTONUP:
        FPressed := False;

      WM_MOUSELEAVE:
        FPressed := False;

      WM_MOUSEMOVE: if FPressed then begin
        p := acMousePos;
        ChangePosition(CurrentMouse, p);
        CurrentMouse := p;
      end;
    end;
end;


function LoadBmpFromRes(Name: string): TBitmap;
var
  rs: TResourceStream;
begin
  rs := TResourceStream.Create(hInstance, Name, RT_RCDATA);
  Result := TPNGGraphic.Create;
  Result.LoadFromStream(rs);
  Result.PixelFormat := pf32bit;
  rs.Free;
end;


constructor TsArcHandle.Create(AOwner: TComponent);
begin
  inherited;
  FRotationAngle := 0;
  FTextContent := tcAngle;
end;


function TsArcHandle.HandleRect: TRect;
begin
  Result := MkRect(Diameter, Diameter);
end;


function TsArcHandle.HandleText: string;
begin
  if ShowText then
    case FTextContent of
      tcValue:
        Result := IntToStr(Value);

      tcAngle:
        Result := IntToStr(FRotationAngle) + #$02DA;

      tcPercent:
        if Max - Min <> 0 then
          Result := IntToStr((Value - Min) * 100 div (Max - Min)) + '%'
        else
          Result := IntToStr(Value * 100 div i360) + '%'

      else {tcCustom}
        Result := Text;
    end
  else
    Result := '';
end;


procedure TsArcHandle.PrepareCache;
begin
  PaintBG;
  PaintHandle(HandleRect);
end;


procedure TsArcHandle.SetInteger(const Index: integer; const Value: integer);
var
  CurrentAngle: integer;
begin
  case Index of
    10: begin // Value
      CurrentAngle := Value mod i360;
      if CurrentAngle > i180 then
        FRotationAngle := i360 - CurrentAngle
      else
        FRotationAngle := -CurrentAngle;

      inherited;
    end;

    100: begin
      if FRotationAngle <> Value then begin
        FRotationAngle := Value;
        Self.Value := AngleToValue(Value);
        Repaint;
      end;
    end
    else
      inherited;
  end;
end;


procedure TsArcHandle.SetTextContent(const Value: TacTextContent);
begin
  if FTextContent <> Value then begin
    FTextContent := Value;
    Repaint;
  end;
end;


function TsArcHandle.ValueToAngle(AValue: integer): integer;
begin
  Result := AValue mod i360;
end;


constructor TsArcDial.Create(AOwner: TComponent);
begin
  inherited;
  FTicksBigStep := 10;
  FTicksSmallStep := 2;
  FTickBigLength := 6;
  Height := 200;
  Width := 200;
  Max := 100;
end;


function TsArcDial.HandleRect: TRect;
var
  iSize, i: integer;
begin
  iSize := Diameter;
  i := iSize div 2 - HandleRadius;
  Result := Rect(i, i, iSize - i, iSize - i);
end;


function TsArcDial.HandleText: string;
begin
  Result := '';
end;


procedure TsArcDial.InitPaintData;
begin
  SkinData.FCacheBmp.Canvas.Font.Assign(Font);
  acGetTextExtent(SkinData.FCacheBmp.Canvas.Handle, IntToStr(Max), TextSize);
  TextWidthD2  := TextSize.cx div 2;
  TextHeightD2 := TextSize.cy div 2;

  CenterTextRadius := Center.X - TextWidthD2;
  TickBigBeginRadius := CenterTextRadius - (TextWidthD2 + acSpacing);
  TickSmallBeginRadius := TickBigBeginRadius - 2 * FTickBigLength div 3;
  TickEndRadius := TickSmallBeginRadius - mini(1, FTickBigLength div 3);
  HandleRadius := TickEndRadius - acSpacing;
end;

type
  TacTicksSection = record
    MinValue,
    MaxValue: integer;
    StartColor,
    EndColor: TColor;
  end;


procedure TsArcDial.PaintDial;
var
  pd: TacDialPaintData;

  function GetLineCoord(aAngle: real; aRadius: real): TPoint;
  begin
    Result.X := Center.X - Round(cos(aAngle) * aRadius);
    Result.Y := Center.Y - Round(sin(aAngle) * aRadius);
  end;

  procedure PaintSmallTicks;
  var
    TickValue: integer;
    CurAngle, AngleStep: real;

    procedure PaintTick(AAngle: integer);
    var
      Rad: real;
      Coord1, Coord2, TextCenterCoord: TPoint;
    begin
      if TickValue mod TicksBigStep <> 0 then begin
        Rad := AAngle * Pi / i180;
        TextCenterCoord := GetLineCoord(Rad, CenterTextRadius);
        Coord1 := GetLineCoord(Rad, TickSmallBeginRadius);
        Coord2 := GetLineCoord(Rad, TickEndRadius);
        acGPDrawLine(SkinData.FCacheBmp.Canvas.Handle, Coord1.X, Coord1.Y, Coord2.X, Coord2.Y, pd.APenColor, 1{ArrowPenWidth});
      end;
    end;

  begin
    CurAngle := 0;
    AngleStep := DialDegree * TicksSmallStep / (Max - Min);
    TickValue := Min;
    while CurAngle <= DialDegree do begin
      PaintTick(Round(CurAngle) + i90 - DialDegree div 2);
      CurAngle := CurAngle + AngleStep;
      inc(TickValue, TicksSmallStep);
    end;
  end;

  procedure PaintBigTicks;
  var
    TickValue: integer;

    CurAngle,
    AngleStep: real;

    ts: TacTicksSection;

    function GetLineCoord(aAngle: real; aRadius: real): TPoint;
    begin
      Result.X := Center.X - Round(cos(aAngle) * aRadius);
      Result.Y := Center.Y - Round(sin(aAngle) * aRadius);
    end;

    procedure PaintBigTick(AAngle: integer);
    const
      TickLength = 6;
    var
      R: TRect;
      Rad: real;

      TextCenterCoord,
      Coord1,
      Coord2: TPoint;
    begin
      Rad := AAngle * Pi / i180;

      TextCenterCoord := GetLineCoord(Rad, CenterTextRadius);
      Coord1 := GetLineCoord(Rad, TickBigBeginRadius);
      Coord2 := GetLineCoord(Rad, TickEndRadius);

      acGPDrawLine(SkinData.FCacheBmp.Canvas.Handle, Coord1.X, Coord1.Y, Coord2.X, Coord2.Y, pd.APenColor, 1);

      if True {Show marks} then begin
        R.Left := TextCenterCoord.X - TextWidthD2;
        R.Top  := TextCenterCoord.Y - TextHeightD2;
        R.Right := R.Left + TextSize.cx;
        R.Bottom := R.Top + TextSize.cy;

        SkinData.FCacheBmp.Canvas.Brush.Style := bsClear;
        SkinData.FCacheBmp.Canvas.Font.Color := pd.AFontColor;
        acDrawText(SkinData.FCacheBmp.Canvas.Handle, IntToStr(TickValue), R, DT_NOCLIP or DT_CENTER);
      end;
    end;

  begin
    CurAngle := 0;
    AngleStep := DialDegree * TicksBigStep / (Max - Min);

    ts.MinValue := Min;
    ts.MaxValue := Max;
    TickValue := Min;
    while CurAngle <= DialDegree do begin
      PaintBigTick(Round(CurAngle) + i90 - DialDegree div 2);
      CurAngle := CurAngle + AngleStep;
      inc(TickValue, TicksBigStep);
    end;
  end;

begin
  pd.AFontColor := FontColor;

  if (FCommonData.SkinManager = nil) or FCommonData.CustomColor or not FCommonData.SkinManager.CommonSkinData.Active then
    pd.ABaseColor := FBaseColor
  else
    pd.ABaseColor := FCommonData.SkinManager.Palette[pcMainColor];

  pd.APenColor := BlendColors(pd.AFontColor, pd.ABaseColor, 127);
  pd.APenWidth := 1;

  if Assigned(FOnGetDialPaintData) then
    FOnGetDialPaintData(Self, pd);

  PaintBigTicks;
  PaintSmallTicks;
  ShowValueText;
end;


procedure TsArcDial.PrepareCache;
begin
  InitPaintData;
  StartAngle := 270 - DialDegree div 2;

  PaintBG;
  PaintHandle(HandleRect);
  PaintDial;
end;


procedure TsArcDial.SetInteger(const Index: integer; const Value: integer);
begin
  case Index of
    100: if TicksBigStep <> Value then begin
      FTicksBigStep := Value;
      Repaint;
    end;

    101: if TicksSmallStep <> Value then begin
      FTicksSmallStep := Value;
      Repaint;
    end;

    11, 12: begin
      inherited;
      Multiplier := (Max - Min) / DialDegree;
    end;

    10:
      inherited SetInteger(Index, math.max(Min, math.min(Max, Value))) // Value

    else
      inherited;
  end;
end;


procedure TsArcDial.ShowValueText;
var
  R: TRect;
begin
  if ShowText then begin
    R.Left := Center.X - TextSize.cx div 2;
    R.Top  := Center.Y + TickEndRadius + acSpacing;
    R.Right := R.Left + TextSize.cx;
    R.Bottom := R.Top + TextSize.cy;
    acDrawText(SkinData.FCacheBmp.Canvas.Handle, IntToStr(Value), R, DT_NOCLIP or DT_CENTER or DT_VCENTER);
  end;
end;


function TsArcDial.ValueToAngle(AValue: integer): integer;
begin
  Result := Round(AValue / Multiplier) + 270 - DialDegree div 2;
end;


procedure TacCustomRoundBtn.ActionChange(Sender: TObject; CheckDefaults: Boolean);
begin
  inherited ActionChange(Sender, CheckDefaults);
  if not (csLoading in ComponentState) then begin
    if (TCustomAction(Sender).ActionList <> nil) and (Images = nil) then
      FImages := TCustomAction(Sender).ActionList.Images;

    if ImageIndex < 0 then
      FImageIndex := TCustomAction(Sender).ImageIndex;

    FCaption := TCustomAction(Sender).Caption;
    if Hint = '' then
      Hint := TCustomAction(Sender).Hint;

    if not (csDestroying in ComponentState) then
      Invalidate;
  end;
end;


procedure TacCustomRoundBtn.Click;
begin
  FPressed := False;
  FCommonData.Invalidate(True);
  inherited;
end;


procedure TacCustomRoundBtn.CMMouseEnter(var Message: TMessage);
begin
  if Enabled and not (csDesigning in ComponentState) then
    if not FCommonData.FMouseAbove and ((SkinData.SkinManager = nil) or not SkinData.SkinManager.Options.NoMouseHover) then begin
      if Assigned(FOnMouseEnter) then
        FOnMouseEnter(Self);

      FCommonData.BGChanged := False;
      FCommonData.FMouseAbove := True;
      DoChangePaint(FCommonData, 1, UpdateWindow_CB, True, False);
    end;
end;


procedure TacCustomRoundBtn.CMMouseLeave(var Message: TMessage);
begin
  if Enabled and not (csDesigning in ComponentState) then begin
    if Assigned(FOnMouseLeave) then
      FOnMouseLeave(Self);

    FCommonData.BGChanged := False;
    FCommonData.FMouseAbove := False;
    DoChangePaint(FCommonData, 0, UpdateWindow_CB, True, False, False);
  end;
end;


function TacCustomRoundBtn.ContentSize: TSize;
var
  TextSize: TSize;
begin
  if ShowCaption and (Caption <> '') then
    TextSize := GetTextSize
  else
    TextSize := MkSize;

  with GlyphSize do
    case Layout of
      blGlyphLeft, blGlyphRight: begin
        Result.cx := cx + iff((cx <> 0) and (TextSize.cx <> 0), acSpacing, 0) + TextSize.cx;
        Result.cy := max(cy, TextSize.cy);
      end;

      blGlyphTop, blGlyphBottom: begin
        Result.cx := max(cx, TextSize.cx);
        Result.cy := cy + iff((cy <> 0) and (TextSize.cy <> 0), acSpacing, 0) + TextSize.cy;
      end;
    end;
end;


constructor TacCustomRoundBtn.Create(AOwner: TComponent);
begin
  inherited;
  FImageIndex := -1;
  FImageIndexHot := -1;
  FImageIndexPressed := -1;
  FImageIndexDisabled := -1;
  FImageIndexSelected := -1;
  FPaintOptions := TacPaintButtonOptions.Create;
  FPaintOptions.OnInvalidate := Self.DoInvalidate;
  FLayout := blGlyphLeft;
  FShowCaption := True;
  FReflected := False;
  FWordWrap := True;
  TabStop := True;
end;


function TacCustomRoundBtn.CurrentState: integer;
begin
  if FPressed then
    Result := 2
  else
    if SkinData.FMouseAbove or Focused then
      Result := 1
    else
      Result := 0;
end;


destructor TacCustomRoundBtn.Destroy;
begin
  FPaintOptions.Free;
  inherited;
end;


procedure TacCustomRoundBtn.DoInvalidate(Sender: TObject);
begin
  Invalidate;
end;


function TacCustomRoundBtn.GetGlyphIndex: integer;
begin
  Result := FImageIndex;
  if not Enabled then
    Result := iff(FImageIndexDisabled >= 0, FImageIndexDisabled, FImageIndex)
  else
    if (FImageIndexPressed >= 0) and FPressed then
      Result := FImageIndexPressed
    else
      if Focused and (FImageIndexSelected >= 0) then
        Result := FImageIndex
      else
        if (FImageIndexHot >= 0) and (SkinData.FMouseAbove or Focused) then
          Result := FImageIndexHot;
end;


function TacCustomRoundBtn.GetTextSize: TSize;
var
  R: TRect;
  ImgSize: TSize;
begin
  SkinData.FCacheBmp.Canvas.Font.Assign(Font);
  R := MkRect(Width - 2 * (BorderWidth + acSpacing));
  ImgSize := GlyphSize;
  if ImgSize.cx > 0 then
    if (Layout in [blGlyphLeft, blGlyphRight]) then
      dec(R.Right, ImgSize.cx + acSpacing)
    else
      dec(R.Bottom, ImgSize.cx + acSpacing);

  acDrawText(SkinData.FCacheBmp.Canvas.Handle, Caption, R, DT_CALCRECT or DT_NOCLIP or TextWrapping[WordWrap] or TextEllips[UseEllipsis]);
  Result.cx := WidthOf(R);
  Result.cy := HeightOf(R);
end;


function TacCustomRoundBtn.GlyphSize: TSize;
begin
  if not FGlyph.Empty then
    Result := MkSize(FGlyph.Width, FGlyph.Height)
  else
    if (Images <> nil) and IsValidIndex(GetGlyphIndex, GetImageCount(Images)) then
      Result := MkSize(GetImageWidth(Images), GetImageHeight(Images))
    else
      Result := MkSize;
end;


procedure TacCustomRoundBtn.InitPaintData(State: integer; var dpData: TacButtonPaintData);
begin
  InitBtnPaintData(SkinData.SkinManager, SkinData, State, PaintOptions, dpData);
end;


procedure TacCustomRoundBtn.PaintBG;
var
  BG: TacBGInfo;
begin
  if FCommonData.Skinned then begin
    BG.DrawDC := 0;
    BG.Offset := MkPoint;
    BG.PleaseDraw := False;
    GetBGInfo(@BG, Parent, False);
    PaintItem(FCommonData.SkinIndex, BGInfoToCI(@BG), True, 0, MkRect(Self), Point(Left, Top), SkinData.FCacheBmp, FCommonData.SkinManager);
  end;
end;


procedure TacCustomRoundBtn.PaintContent;
var
  iDiam, State: integer;
  dpData: TacButtonPaintData;
  aRect, RText, RGlyph: TRect;

  procedure InitData(var RectText, RectGlyph: TRect);
  var
    RText: TRect;
    TextSize, ImgSize, cSize: TSize;
  begin
    ImgSize := GlyphSize;
    cSize := ContentSize;
    if ShowCaption and (Caption <> '') then begin
      SkinData.FCacheBmp.Canvas.Font.Assign(Font);
      RText := MkRect(Width - 2 * (BorderWidth + acSpacing));
      if (ImgSize.cx > 0) and (Layout in [blGlyphLeft, blGlyphRight]) then
        dec(RText.Right, ImgSize.cx + acSpacing);

      acDrawText(SkinData.FCacheBmp.Canvas.Handle, dpData.AText, RText, DT_CALCRECT or DT_NOCLIP or TextWrapping[WordWrap] or TextEllips[UseEllipsis]);
      TextSize.cx := WidthOf(RText);
      TextSize.cy := HeightOf(RText);
      case Layout of
        blGlyphLeft: begin
          RectText.Left := (Width - cSize.cx) div 2 + ImgSize.cx + iff(ImgSize.cx <> 0, acSpacing, 0);
          RectText.Top := (Height - TextSize.cy) div 2;
        end;
        blGlyphRight: begin
          RectText.Left := (Width - cSize.cx) div 2;
          RectText.Top := (Height - TextSize.cy) div 2;
        end;
        blGlyphTop: begin
          RectText.Left := (Width - TextSize.cx) div 2;
          RectText.Top := (Height - cSize.cy) div 2 + ImgSize.cy + iff(ImgSize.cy <> 0, acSpacing, 0);
        end;
        blGlyphBottom: begin
          RectText.Left := (Width - TextSize.cx) div 2;
          RectText.Top := (Height - cSize.cy) div 2;
        end;
      end;
      RectText.Bottom := RectText.Top + TextSize.cy;
      RectText.Right  := RectText.Left + TextSize.cx;
    end
    else begin
      RectText := MkRect;
      TextSize := MkSize;
    end;

    if ImgSize.cx <> 0 then begin
      case Layout of
        blGlyphLeft: begin
          RectGlyph.Left := (Width - cSize.cx) div 2;
          RectGlyph.Top := (Height - ImgSize.cy) div 2;
        end;
        blGlyphRight: begin
          RectGlyph.Left := (Width - cSize.cx) div 2 + TextSize.cx + iff(TextSize.cx <> 0, acSpacing, 0);
          RectGlyph.Top := (Height - ImgSize.cy) div 2;
        end;
        blGlyphTop: begin
          RectGlyph.Left := (Width - ImgSize.cx) div 2;
          RectGlyph.Top := (Height - cSize.cy) div 2;
        end;
        blGlyphBottom: begin
          RectGlyph.Left := (Width - ImgSize.cx) div 2;
          RectGlyph.Top := (Height - cSize.cy) div 2 + TextSize.cy + iff(TextSize.cy <> 0, acSpacing, 0);
        end;
      end;
      RectGlyph.Bottom := RectGlyph.Top + ImgSize.cy;
      RectGlyph.Right  := RectGlyph.Left + ImgSize.cx;
    end
    else
      RectGlyph := MkRect;
  end;

begin
  State := CurrentState;
  iDiam := Diameter;
  aRect := MkRect(iDiam - 2, iDiam - 2);
  OffsetRect(aRect, (Width - iDiam) div 2, (Height - iDiam) div 2);
  dpData.AText := Caption;

  InitPaintData(State, dpData);

  dpData.ATransparent := FTransparent and (CurrentState = 0);
  if Assigned(FOnGetButtonPaintData) then
    FOnGetButtonPaintData(Self, dpData);

  InitData(RText, RGlyph);
  DrawRoundButton(SkinData.FCacheBmp.Canvas.Handle, aRect.Left, aRect.Top, iDiam, dpData);
  if (State = 2) and CanClickShift(SkinData.SkinIndex, SkinData.SkinManager) then begin
    OffsetRect(RText,  1, 1);
    OffsetRect(RGlyph, 1, 1);
  end;
  SkinData.FCacheBmp.Canvas.Font.Color := dpData.AFontColor;
  if RText.Right - RText.Left > 0 then begin
    SkinData.FCacheBmp.Canvas.Brush.Style := bsClear;
    SelectObject(SkinData.FCacheBmp.Canvas.Handle, SkinData.FCacheBmp.Canvas.Font.Handle);
    acDrawText(SkinData.FCacheBmp.Canvas.Handle, dpData.AText, RText, DT_NOCLIP or DT_CENTER or DT_VCENTER or TextWrapping[WordWrap] or TextEllips[UseEllipsis]);
  end;
  if GlyphGrayed then
    PaintGlyph(RGlyph, BlendColors(dpData.AColor1, dpData.AColor2, 127))
  else
    PaintGlyph(RGlyph, clNone);
end;


procedure TacCustomRoundBtn.PrepareCache;
begin
  PaintBG;
  PaintContent;
  if not Enabled then
    BmpDisabledKind(FCommonData.FCacheBmp, DefDisabledKind, Parent, GetParentCacheHwnd(Handle), Point(Left, Top));
end;


procedure TacCustomRoundBtn.SetBoolean(const Index: integer; const Value: boolean);

  procedure ChangeProp(var Prop: boolean; Value: boolean);
  begin
    if Prop <> Value then begin
      Prop := Value;
      Invalidate;
    end;
  end;

begin
  case Index of
    10: ChangeProp(FShowCaption, Value);
    11: ChangeProp(FReflected,   Value);
    12: ChangeProp(FWordWrap,    Value)
    else inherited;
  end;
end;


procedure TacCustomRoundBtn.SetCaption(const Value: acString);
begin
  if FCaption <> Value then begin
    FCaption := Value;
    Repaint;
  end;
end;


procedure TacCustomRoundBtn.SetInteger(const Index, Value: integer);

  procedure ChangeProp(var Prop: integer; Value: integer);
  begin
    if Prop <> Value then begin
      Prop := Value;
      Invalidate;
    end;
  end;

begin
  case Index of
    10: ChangeProp(FImageIndex,         Value);
    11: ChangeProp(FImageIndexHot,      Value);
    12: ChangeProp(FImageIndexPressed,  Value);
    13: ChangeProp(FImageIndexDisabled, Value);
    14: ChangeProp(FImageIndexSelected, Value)
    else inherited;
  end;
end;


procedure TacCustomRoundBtn.SetLayout(const Value: TButtonLayout);
begin
  if FLayout <> Value then begin
    FLayout := Value;
    Invalidate;
  end;
end;


procedure TacCustomRoundBtn.WMKeyDown(var Message: TWMKeyDown);
begin
  if Message.CharCode in [VK_RETURN, VK_SPACE] then begin
    FPressed := True;
    FCommonData.Invalidate(True);
  end;
  inherited;
end;


procedure TacCustomRoundBtn.WMKeyUp(var Message: TWMKeyUp);
begin
  if FPressed and (Message.CharCode in [VK_RETURN, VK_SPACE]) then
    Click;

  inherited;
end;


procedure TacCustomRoundBtn.WMLButtonDblClk(var Message: TWMMouse);
begin
  WMLButtonDown(Message);
end;


procedure TacCustomRoundBtn.WMLButtonDown(var Message: TWMMouse);
begin
  FPressed := True;
  if CanFocus then
    SetFocus;

  inherited;
  Invalidate;
end;


procedure TacCustomRoundBtn.WMLButtonUp(var Message: TWMMouse);
begin
  FPressed := False;
  inherited;
  Invalidate;
end;


procedure TacCustomRoundBtn.WMNCHitTest(var Message: TWMNCHitTest);
var
  R: TRect;
  X, Y, div2: integer;
begin
  if not (csDesigning in ComponentState) then begin
    div2 := Diameter div 2;
    GetWindowRect(Handle, R);
    X := Message.XPos - R.Left;
    Y := Message.YPos - R.Top;
    if sqr(X - Width div 2) + sqr(Y - Height div 2) <= sqr(div2) then
      inherited
    else
      Message.Result := HTTRANSPARENT;
  end
  else
    inherited;
end;


procedure TacCustomRoundBtn.WndProc(var Message: TMessage);
begin
{$IFDEF LOGGED}
  AddToLog(Message);
{$ENDIF}
  case Message.Msg of
    SM_ALPHACMD:
      case Message.WParamHi of
        AC_PREPARECACHE: begin
          PrepareCache;
          SkinData.BGChanged := False;
          Exit;
        end;
      end;

    WM_SETFOCUS, CM_ENTER:
      if not (csDesigning in ComponentState) and Visible then begin
        if Enabled and not (csDestroying in ComponentState) then begin
          inherited;
          if (SkinData.AnimTimer <> nil) and SkinData.AnimTimer.Enabled then begin
            SkinData.BGChanged := True;
            SkinData.AnimTimer.TimeHandler // Fast repaint
          end
          else
            FCommonData.Invalidate(True);
        end
        else
          inherited;

        Exit;
      end;

    WM_KILLFOCUS, CM_EXIT:
      if not (csDesigning in ComponentState) and Visible then begin
        if Enabled and not (csDestroying in ComponentState) then begin
          StopTimer(SkinData);
          inherited;
          FCommonData.FFocused := False;
          FCommonData.Invalidate(True);
          HideGlow(SkinData.GlowID);
        end
        else
          inherited;

        Exit
      end;

    CM_ENABLEDCHANGED:
      Repaint;
  end;
  inherited;
end;


procedure TsBadgeBtn.AdjustPos;
begin
  FRealigning := True;
  if FAttachTo <> nil then
    with NewPos do
      SetBounds(X, Y, Width, Height);

  FRealigning := False;
end;


constructor TsBadgeBtn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  TabStop := False;
  FRealigning := False;
  FAlignTo := baTopRight;
  FOffsetX := 0;
  FOffsetY := 0;
  Width := DefaultBadgeSize;
  Height := DefaultBadgeSize;
  ControlStyle := ControlStyle - [csOpaque];
end;


destructor TsBadgeBtn.Destroy;
begin
  SetAttachTo(nil);
  inherited Destroy;
end;


procedure TsBadgeBtn.InitPaintData(State: integer; var dpData: TacButtonPaintData);
var
  BaseColor: TColor;
begin
  if not SkinData.CustomColor then begin
    if SkinData.Skinned then begin
      BaseColor         := SkinData.SkinManager.Palette[pcSelectionBG_Focused];
      dpData.AFontColor := SkinData.SkinManager.Palette[pcSelectionText_Focused];
    end
    else begin
      BaseColor         := ColorToRGB(clHighlight);
      dpData.AFontColor := ColorToRGB(clHighlightText);
    end;

    case State of
      0: begin
        dpData.AColor1 := BaseColor;
        dpData.AColor2 := BlendColors(BaseColor, 0, 240);
      end;

      1: begin
        dpData.AColor1 := BlendColors(BaseColor, clWhite, 220);
        dpData.AColor2 := BaseColor;
      end;

      2: begin
        dpData.AColor1 := BlendColors(BaseColor, 0, 200);
        dpData.AColor2 := BlendColors(BaseColor, 0, 200);
      end;
    end;

    dpData.AColor1 := TColor($FF000000 or Cardinal(dpData.AColor1));
    dpData.AColor2 := TColor($FF000000 or Cardinal(dpData.AColor2));
    dpData.ABorderColor := {clRed;//}dpData.AColor1;
    dpData.ABorderWidth := 0;
    dpData.ABevelWidth := 0;
    dpData.ATransparent := False;
  end
  else
    inherited;
end;


function TsBadgeBtn.NewPos: TPoint;
begin
  case AlignTo of
    baBottomLeft: begin
      Result.Y :=  FAttachTo.Top + FAttachTo.Height - Height div 2;
      Result.X := FAttachTo.Left - Width div 2;
    end;

    baTopRight: begin
      Result.Y :=  FAttachTo.Top - Height div 2;
      Result.X := FAttachTo.Left + FAttachTo.Width - Width div 2;
    end;

    baTopLeft: begin
      Result.Y :=  FAttachTo.Top - Height div 2;
      Result.X := FAttachTo.Left - Width div 2;
    end;

    else begin
      Result.Y :=  FAttachTo.Top + FAttachTo.Height - Height div 2;
      Result.X := FAttachTo.Left + FAttachTo.Width - Width div 2;
    end;
  end;
  inc(Result.X, OffsetX);
  inc(Result.Y, OffsetY);
end;


procedure TsBadgeBtn.NewWinProc(var Message: TMessage);
begin
  if not (csDestroying in ComponentState) then
    if Assigned(FAttachTo) and not FRealigning then begin
      FRealigning := True;
      try
        case(Message.Msg) of
          CM_ENABLEDCHANGED:
            Enabled := FAttachTo.Enabled;

          CM_VISIBLECHANGED:
            Visible := FAttachTo.Visible;

          WM_SIZE, WM_MOVE, WM_WINDOWPOSCHANGED:
            AdjustPos;
        end;
      finally
        FRealigning := False;
      end;
    end;

  if Assigned(FOldWinProc) then
    FOldWinProc(Message);
end;


procedure TsBadgeBtn.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FAttachTo) then
    SetAttachTo(nil);

  inherited Notification(AComponent, Operation);
end;


procedure TsBadgeBtn.PrepareCache;
var
  Bmp: TBitmap;
  R, cR: TRect;
  i, dx, dy: integer;
begin
  // Parent BG
  if SkinData.Skinned then
    PaintItem(SkinData.SkinManager.ConstData.Sections[ssTransparent], GetParentCache(SkinData), False, 0, MkRect(Self), Point(Left, Top), SkinData.FCacheBmp, SkinData.SkinManager)
  else
    FillDC(SkinData.FCacheBmp.Canvas.Handle, MkRect(Self), TsAccessControl(Parent).Color);

  R := BoundsRect;
  // Child controls BG
  for i := 0 to Parent.ControlCount - 1 do
    if Parent.Controls[i] <> Self then begin
      cR := Parent.Controls[i].BoundsRect;
      if RectInRect(cR, R, False) then begin
        Bmp := CreateBmp32(Parent.Controls[i]);
        SkinPaintTo(Bmp, Parent.Controls[i], 0, 0);
        dx := cR.Left - R.Left;
        dy := cR.Top - R.Top;
        BitBlt(SkinData.FCacheBmp.Canvas.Handle, dx, dy, Bmp.Width, Bmp.Height, Bmp.Canvas.Handle, 0, 0, SRCCOPY);
        Bmp.Free;
      end;
    end;

  PaintContent;
end;


procedure TsBadgeBtn.SetAlignTo(const Value: TacBadgeAlignTo);
begin
  if FAlignTo <> Value then begin
    FAlignTo := Value;
    AdjustPos;
  end;
end;


procedure TsBadgeBtn.SetAttachTo(const Value: TControl);
begin
  if Value <> FAttachTo then begin
    if Assigned(FAttachTo) then
      FAttachTo.WindowProc := FOldWinProc;

    FAttachTo := Value;
    if Assigned(Value) then begin
      AdjustPos;
      Enabled := FAttachTo.Enabled;
      Visible := FAttachTo.Visible;
      FOldWinProc := FAttachTo.WindowProc;
      FAttachTo.WindowProc := NewWinProc;
    end;
  end;
end;


procedure TsBadgeBtn.SetInteger(const Index, Value: integer);

  procedure ChangeProp(var Prop: integer; Value: integer);
  begin
    if Prop <> Value then begin
      Prop := Value;
      AdjustPos;
    end;
  end;

begin
  case Index of
    100: ChangeProp(FOffsetX, Value);
    101: ChangeProp(FOffsetY, Value)
    else inherited;
  end;
end;


procedure TsBadgeBtn.UpdateRgn;
var
  rgn: hrgn;
  l, t, d: integer;
begin
  d := Diameter + 4;
  l := (Width - d) div 2;
  t := (Height - d) div 2;
  rgn := CreateRoundRectRgn(l, t, l + d, t + d, d, d);
  SetWindowRgn(Handle, rgn, False);
end;


procedure TsBadgeBtn.WndProc(var Message: TMessage);
var
  M: TWMWindowPosMsg;
begin
  case Message.Msg of
    SM_ALPHACMD:
      case MEssage.WParamHi of
        AC_ISOPAQUE:
          Message.Result := 0
        else
          inherited;
      end;

    WM_WINDOWPOSCHANGING:
      if not (csDestroying in ComponentState) and Assigned(FAttachTo) and not FRealigning then begin
        FRealigning := True;
        try
          M := TWMWindowPosMsg(Message);
          with NewPos do begin
            M.WindowPos.x := X;
            M.WindowPos.y := Y;
          end;
        finally
          FRealigning := False;
        end;
      end
      else
        inherited;

    WM_PRINT: begin
      FPrinting := True;
      inherited;
      FPrinting := False;
    end;

    WM_SIZE: begin
      inherited;
      AdjustPos;
      UpdateRgn;
    end

    else
      inherited;
  end;
end;


initialization
  Mask1 := LoadBmpFromRes('acMask1');
  Mask2 := LoadBmpFromRes('acMask2');


finalization
  Mask1.Free;
  Mask2.Free;

end.

