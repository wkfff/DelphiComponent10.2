unit acTitleBar;
{$I sDefs.inc}

interface

uses
  Windows, Classes, ImgList, Messages, Graphics, Forms, Controls, Menus, ExtCtrls,
  {$IFNDEF DELPHI5}Types, {$ENDIF}
  {$IFDEF DELPHI_XE2}UITypes, {$ENDIF}
  {$IFDEF TNTUNICODE}TntControls, {$ENDIF}
  acntTypes, sConst;


type
{$IFNDEF NOTFORHELP}
  TacTitleBarItem = class;
  TacInternalDrawEvent = procedure(Item: TacTitleBarItem; R: TRect; Bmp: TBitmap) of object;

  TacTitleItemDrawData = record
    ARect: TRect;
    Bmp: TBitmap;
  end;

  TacDrawItemEvent = procedure(Item: TacTitleBarItem; ADrawData: TacTitleItemDrawData) of object;

  TacItemAnimation = class(TTimer)
  public
    CurrentLevel,
    CurrentState,
    MaxIterations: integer;

    AForm,
    OldForm: TacGlowForm;

    ToShow: boolean;
    NewBmp: TBitmap;
    FormHandle: hwnd;
    Item: TacTitleBarItem;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetFormBounds: TRect;
    function Offset: integer;
    function Step: integer;
    procedure MakeForm;
    procedure UpdateForm(Blend: integer);
    procedure StartAnimation(NewState: integer; Show: boolean);
    procedure ChangeState   (NewState: integer; Show: boolean);
    procedure MakeImg;
    procedure CheckMouseLeave;
    procedure OnAnimTimer(Sender: TObject);
  end;
{$ENDIF}


  TacFontData = class(TPersistent)
{$IFNDEF NOTFORHELP}
  private
    FUseSysSize,
    FUseSysStyle,
    FUseSysColor,
    FUseSysGlowing,
    FUseSysFontName: boolean;

    FFont: TFont;
    WndHandle: hwnd;
    FGlowSize: integer;
    FGlowColor: TColor;
    function GetBool      (const Index: Integer): boolean;
    procedure SetBool     (const Index: Integer; const Value: boolean);
    procedure SetFont     (const Value: TFont);
    procedure SetGlowSize (const Value: integer);
    procedure SetGlowColor(const Value: TColor);
  public
    UsedFont: TFont;
    GlowMask: TBitmap;
    constructor Create;
    destructor Destroy; override;
    procedure Invalidate;
  published
    property Font: TFont read FFont write SetFont;
{$ENDIF}
    property GlowColor: TColor read FGlowColor write SetGlowColor default clGray;
    property GlowSize: integer read FGlowSize write SetGlowSize default 0;
    property UseSysColor    : boolean index 0 read GetBool write SetBool default True;
    property UseSysFontName : boolean index 1 read GetBool write SetBool default True;
    property UseSysGlowing  : boolean index 2 read GetBool write SetBool default True;
    property UseSysSize     : boolean index 3 read GetBool write SetBool default True;
    property UseSysStyle    : boolean index 4 read GetBool write SetBool default True;
  end;


{$IFNDEF NOTFORHELP}
  TacBtnStyle = (bsButton, bsMenu, bsTab, bsSpacing, bsInfo);
  TacItemAlign = (tbaLeft, tbaRight, tbaCenter, tbaCenterInSpace);
{$ENDIF}

  TacTitleBarItem = class(TCollectionItem)
{$IFNDEF NOTFORHELP}
  private
    FGlyph,
    FDefaultMenuBtn: TBitmap;

    FEnabled,
    FVisible,
    FAutoSize,
    FShowHint,
    FDroppedDown: boolean;

    FHint,
    FCaption: acString;

    FOnMouseUp,
    FOnMouseDown: TMouseEvent;

    FOnClick,
    FOnChanged,
    FOnMouseLeave,
    FOnMouseEnter: TNotifyEvent;

    FWidth,
    FHeight,
    FSpacing,
    FNumGlyphs,
    FImageIndex,
    FGroupIndex: integer;

    FName: string;
    FCursor: TCursor;
    FStyle: TacBtnStyle;
    FContentSize: TSize;
    FAlign: TacItemAlign;
    FFontData: TacFontData;
    FDropdownMenu: TPopupMenu;
    FOnInternalDraw: TacInternalDrawEvent;
    FAlignment: TAlignment;
    FTag: integer;
    FOnDrawItem: TacDrawItemEvent;
    FPressedImageIndex: integer;
    FDisabledImageIndex: integer;
    FHotImageIndex: integer;
    FSkinSection: string;
{$IFDEF ALPHASKINS}
    procedure SetSkinSection(const Value: string);
{$ENDIF}
    procedure SetAlignment  (const Value: TAlignment);
    procedure SetGlyph      (const Value: TBitmap);
    procedure SetName       (const Value: string);
    procedure SetAlign      (const Value: TacItemAlign);
    procedure SetCaption    (const Value: acString);
    procedure SetStyle      (const Value: TacBtnStyle);
    procedure SetState      (const Value: integer);
    procedure SetInteger    (const AIndex, Value: integer);
    procedure SetBoolean    (const Index: Integer; const Value: boolean);
    function GetState: integer;
    procedure OnGlyphChange(Sender: TObject);
  protected
    FDown: boolean;
    FState: integer;
  public
{$IFDEF TNTUNICODE}
    HintWnd: TTntHintWindow;
{$ELSE}
    HintWnd: THintWindow;
{$ENDIF}
    FPressed: boolean;
    Rect: TRect;
    Data: ^TObject;
    Timer: TacItemAnimation;
    ExtForm: TacGlowForm;
    procedure AssignTo(Dest: TPersistent); override;
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    procedure ScaleItem(NewValue, OldValue: integer);
    procedure InitFont;

    procedure DoMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure DoMouseUp  (Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure DoClick;
    procedure Changed;
    procedure UpdateSize(ContentOnly: boolean = False);

    procedure Invalidate(RecalcSize: boolean = False);
    function GetDisplayName: string; override;
    function IntXMargin: integer;
    property DefaultMenuBtn: TBitmap read FDefaultMenuBtn;
    property DroppedDown: boolean read FDroppedDown;
    property GroupIndex: integer index 8 read FGroupIndex write SetInteger default 0;
    property ContentSize: TSize read FContentSize;
    property Pressed: boolean read FPressed;
    property State: integer read GetState write SetState;
    property OnInternalDraw: TacInternalDrawEvent read FOnInternalDraw write FOnInternalDraw;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  published
{$ENDIF} // NOTFORHELP
    property Align: TacItemAlign read FAlign write SetAlign default tbaLeft;
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;

    property Caption: acString read FCaption write SetCaption;
    property DropdownMenu: TPopupMenu read FDropdownMenu write FDropdownMenu;
    property FontData: TacFontData read FFontData write FFontData;
    property Glyph: TBitmap read FGlyph write SetGlyph;
    property Cursor: TCursor read FCursor write FCursor default crDefault;

    property AutoSize : boolean index 0 read FAutoSize write SetBoolean default True;
    property Down     : boolean index 1 read FDown     write SetBoolean default False;
    property Enabled  : boolean index 2 read FEnabled  write SetBoolean default True;
    property Visible  : boolean index 3 read FVisible  write SetBoolean default True;

    property Hint: acString read FHint write FHint;

    property DisabledImageIndex : integer index 0 read FDisabledImageIndex write SetInteger default -1;
    property ImageIndex         : integer index 1 read FImageIndex         write SetInteger default -1;
    property HotImageIndex      : integer index 2 read FHotImageIndex      write SetInteger default -1;
    property PressedImageIndex  : integer index 3 read FPressedImageIndex  write SetInteger default -1;

    property NumGlyphs          : integer index 4 read FNumGlyphs          write SetInteger default 1;
    property Spacing            : integer index 5 read FSpacing            write SetInteger default 4;
    property Height             : integer index 6 read FHeight             write SetInteger default 18;
    property Width              : integer index 7 read FWidth              write SetInteger default 22;

    property Index;
    property Name: string read FName write SetName;

    property ShowHint: boolean read FShowHint write FShowHint default False;
{$IFDEF ALPHASKINS}
    property SkinSection: string read FSkinSection write SetSkinSection;
{$ENDIF}
    property Style: TacBtnStyle read FStyle write SetStyle default bsButton;
    property Tag: integer read FTag write FTag default 0;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property OnDrawItem: TacDrawItemEvent read FOnDrawItem   write FOnDrawItem;
    property OnMouseDown:  TMouseEvent    read FOnMouseDown  write FOnMouseDown;
    property OnMouseEnter: TNotifyEvent   read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent   read FOnMouseLeave write FOnMouseLeave;
    property OnMouseUp:    TMouseEvent    read FOnMouseUp    write FOnMouseUp;
  end;

{$IFNDEF NOTFORHELP}
  TsTitleBar = class;

  TacTitleItems = class(TCollection)
  private
    FOwner: TsTitleBar;
    function GetItem (Index: Integer): TacTitleBarItem;
    procedure SetItem(Index: Integer; Value: TacTitleBarItem);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TPersistent);
    function Add: TacTitleBarItem;
    function Insert(Index: Integer): TacTitleBarItem;
    property Items[Index: Integer]: TacTitleBarItem read GetItem write SetItem; default;
  end;
{$ENDIF} // NOTFORHELP


{$IFDEF DELPHI_XE3}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]{$ENDIF}
  TsTitleBar = class(TComponent)
{$IFNDEF NOTFORHELP}
  private
    FLeftWidth,
    FRightWidth,
    FItemsMargin,
    FCenterWidth,
    FCenterWidthS,
    FItemsSpacing: integer;

    Form: TForm;
    FCaptionRect: TRect;
    FItems: TacTitleItems;
    FShowCaption: boolean;
    FRightSpacing: integer;
    FImages: TCustomImageList;
    FImageChangeLink: TChangeLink;
    procedure SetItems       (const Value: TacTitleItems);
    procedure SetImages      (const Value: TCustomImageList);
    procedure SetShowCaption (const Value: boolean);
  protected
    FFullRight,
    FBarVCenter,
    OldScaleValue: integer;

    FBarRect: TRect;
    FUpdating: boolean;
    procedure ImageListChange(Sender: TObject);
  public
    FDefaultMenuBtn: TBitmap;
    procedure CalcSizes;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    procedure UpdateScale(NewValue: integer);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Invalidate;

    property BarRect:     TRect read FBarRect;
    property CaptionRect: TRect read FCaptionRect;
    property LeftWidth:  integer read FLeftWidth;
    property RightWidth: integer read FRightWidth;
  published
{$ENDIF}
    property Images: TCustomImageList read FImages write SetImages;
    property Items: TacTitleItems read FItems write SetItems;
    property ItemsMargin:  integer read FItemsMargin  write FItemsMargin  default 3;
    property ItemsSpacing: integer read FItemsSpacing write FItemsSpacing default 0;
    property RightSpacing: integer read FRightSpacing write FRightSpacing default 0;
    property ShowCaption: boolean read FShowCaption write SetShowCaption default False;
  end;


{$IFNDEF NOTFORHELP}
procedure PaintItemNoAlpha(const DstBmp, SrcBmp: TBitmap; const DstRect, SrcRect, BorderWidths: TRect);
procedure ShowHintWnd(Item: TacTitleBarItem);
procedure HideHintWnd(Item: TacTitleBarItem);
procedure StartItemAnimation(const Item: TacTitleBarItem; const Iterations: integer; const Show: boolean; ExtendedForm: TacGlowForm = nil);


const
  acDownChar = #117;
  acDownFont = 'Marlett';
{$ENDIF}


implementation

uses
  SysUtils, math,
  sMessages, sGraphUtils, sSkinMenus, sVclUtils, sAlphaGraph, acntUtils, sThirdParty, sSkinManager, sSkinProvider;

{$R acTitleBar.res}

procedure StartItemAnimation(const Item: TacTitleBarItem; const Iterations: integer; const Show: boolean; ExtendedForm: TacGlowForm = nil);
begin
  with Item do
    if Timer = nil then begin
      if TacTitleItems(Item.Collection).FOwner.Form <> nil then begin
        Item.Timer := TacItemAnimation.Create(TacTitleItems(Item.Collection).FOwner.Form);
        Item.ExtForm := ExtendedForm;
        Item.Timer.FormHandle := TacTitleItems(Item.Collection).FOwner.Form.Handle;
        if Timer <> nil then begin
          Timer.Enabled := False;
          Timer.Item := Item;
          Timer.MaxIterations := Iterations;
          Timer.Interval := acTimerInterval;
          Timer.StartAnimation(State, True);
        end;
      end;
    end
    else begin
      if (State = 1) and (Item.Timer.CurrentState = 2) or (Item.State = 2) then begin
        Item.Timer.OldForm.Free;
        Item.Timer.OldForm := Item.Timer.AForm;
        Item.Timer.AForm := nil;
        if Timer.NewBmp <> nil then
          FreeAndNil(Timer.NewBmp);
      end;
      if Show then
        Item.Timer.CurrentState := State;

      Item.Timer.MaxIterations := Iterations;
      Timer.ToShow := Show;
      Timer.Enabled := True;
      ExtForm := ExtendedForm;
      Timer.StartAnimation(Item.State, True);
      if State = 2 then
        Timer.CurrentLevel := Iterations;
    end;
end;


procedure ShowHintWnd(Item: TacTitleBarItem);
begin
  with Item do 
    if HintWnd = nil then
      if HintWindowClass = THintWindow then
        HintWnd := acShowHintWnd(Hint,  Point(acMousePos.X, acMousePos.Y))
      else
        acShowHintWnd(Hint, Point(acMousePos.X, acMousePos.Y + 16));
end;


procedure HideHintWnd(Item: TacTitleBarItem);
begin
  if Item.HintWnd <> nil then
    FreeAndNil(Item.HintWnd);
end;


function acGetTitleFont: hFont;
var
  NonClientMetrics: {$IFDEF TNTUNICODE}TNonClientMetricsW{$ELSE}TNonClientMetrics{$ENDIF};
begin
  NonClientMetrics.cbSize := {$IFDEF D2010}TNonClientMetrics.SizeOf{$ELSE}SizeOf(NonClientMetrics){$ENDIF};
{$IFDEF TNTUNICODE}
  if SystemParametersInfoW(SPI_GETNONCLIENTMETRICS, 0, @NonClientMetrics, 0) then
    Result := CreateFontIndirectW(NonClientMetrics.lfCaptionFont)
{$ELSE}
  if SystemParametersInfo(SPI_GETNONCLIENTMETRICS, 0, @NonClientMetrics, 0) then
    Result := CreateFontIndirect(NonClientMetrics.lfCaptionFont)
{$ENDIF}
  else
    Result := 0;
end;


function GetCaptionFontSize: integer;
var
  NonClientMetrics: {$IFDEF TNTUNICODE}TNonClientMetricsW{$ELSE}TNonClientMetrics{$ENDIF};
begin
  NonClientMetrics.cbSize := {$IFDEF D2010}TNonClientMetrics.SizeOf{$ELSE}SizeOf(NonClientMetrics){$ENDIF};
  if {$IFDEF TNTUNICODE}SystemParametersInfoW{$ELSE}SystemParametersInfo{$ENDIF}(SPI_GETNONCLIENTMETRICS, 0, @NonClientMetrics, 0) then
    Result := NonClientMetrics.lfCaptionFont.lfHeight
  else
    Result := 0;
end;


function GetStringSize(hFont: hgdiobj; const Text: acString): TSize;
var
  DC: HDC;
  SaveFont: HGDIOBJ;
begin
  DC := GetDC(0);
  try
    SaveFont := SelectObject(DC, hFont);
{$IFDEF TNTUNICODE}
    if not GetTextExtentPoint32W(DC, PWideChar(Text), Length(Text), Result) then
{$ELSE}
    if not GetTextExtentPoint32(DC, PChar(Text), Length(Text), Result) then
{$ENDIF}
      Result := MkSize;

    SelectObject(DC, SaveFont);
  finally
    ReleaseDC(0, DC);
  end;
end;


procedure UpdateAlphaTitle(Img: TBitmap);
var
  x, y: integer;
  S: PRGBAArray_;
begin
  for Y := 0 to Img.Height - 1 do begin
    S := Img.ScanLine[Y];
    for X := 0 to Img.Width - 1 do
      with S[X] do
        if C <> clFuchsia then begin
          A := MaxByte - A;
          R := (R * A) shr 8;
          G := (G * A) shr 8;
          B := (B * A) shr 8;
        end;
  end;
end;


procedure CopyTransRect(DstBmp, SrcBmp: Graphics.TBitMap; X, Y: integer; SrcRect: TRect);
var
  S0, S: PRGBAArray_S;
  D0, D: PRGBAArray_D;
  DeltaS, DeltaD, sX, sY, DstX, DstY, SrcX, SrcY, h, w, dh: integer;
begin
  // Check ranges
  if SrcRect.Top < 0 then
    SrcRect.Top := 0;

  if SrcRect.Bottom >= SrcBmp.Height then
    SrcRect.Bottom := SrcBmp.Height - 1;

  if SrcRect.Left < 0 then
    SrcRect.Left := 0;

  if SrcRect.Right >= SrcBmp.Width then
    SrcRect.Right := SrcBmp.Width - 1;
  // Calc delta
  h := SrcRect.Bottom - SrcRect.Top;
  w := SrcRect.Right - SrcRect.Left;
  dh := DstBmp.Height - 1;
  DstY := Y;
  SrcY := SrcRect.Top;
  if InitLine(SrcBmp, Pointer(S0), DeltaS) and InitLine(DstBmp, Pointer(D0), DeltaD) then
    for sY := 0 to h do begin
      if (DstY <= dh) and (DstY >= 0) then begin
        S := Pointer(PAnsiChar(S0) + DeltaS * SrcY);
        D := Pointer(PAnsiChar(D0) + DeltaD * DstY);
        DstX := X;
        SrcX := SrcRect.Left;
        for sX := 0 to w do begin
          with S[SrcX] do
            if (SC <> clFuchsia) then
              D[DstX].DC := SC;

          inc(DstX);
          inc(SrcX);
        end
      end;
      inc(DstY);
      inc(SrcY);
    end
end;


procedure PaintItemNoAlpha(const DstBmp, SrcBmp: TBitmap; const DstRect, SrcRect, BorderWidths: TRect);
begin
  with TSrcRect(SrcRect), TDstRect(DstRect), BorderWidths do begin
    DstBmp.Transparent := True;
    DstBmp.TransparentColor := clFuchsia;
    SrcBmp.Transparent := True;
    SrcBmp.TransparentColor := clFuchsia;
    SetStretchBltMode(DstBmp.Canvas.Handle, COLORONCOLOR);
    // Copy corners
    // Left top
    CopyTransRect(DstBmp, SrcBmp, DLeft, DTop, Rect(SLeft, STop, SLeft + Right - 1, STop + Top - 1));
    // Left bottom
    CopyTransRect(DstBmp, SrcBmp, DLeft, DBottom - Bottom, Rect(SLeft, SBottom - Bottom, SLeft + Right - 1, SBottom - 1));
    // Right top
    CopyTransRect(DstBmp, SrcBmp, DRight - Right, DTop, Rect(SRight - Right, STop, SRight - 1, STop + Top - 1));
    // Right bottom
    CopyTransRect(DstBmp, SrcBmp, DRight - Right, DBottom - Bottom, Rect(SRight - Right, SBottom - Bottom, SRight - 1, SBottom - 1));

    if max(0, SBottom - STop - Bottom - Top) <> 0 then begin
      // Left border
      StretchBlt(DstBmp.Canvas.Handle, DLeft, DTop + Top, Left, DBottom - DTop - Top - Bottom,
                 SrcBmp.Canvas.Handle, SLeft, STop + Top, Left, SBottom - STop - Top - Bottom, SRCCOPY);
      // Right border
      StretchBlt(DstBmp.Canvas.Handle, DRight - Right, DTop + Top, Right, DBottom - DTop - Bottom - Top,
                 SrcBmp.Canvas.Handle, SRight - Right, STop + Top, Right, SBottom - STop - Bottom - Top, SRCCOPY);
    end;
    if max(0, SRight - SLeft - Right - Left) <> 0 then begin
      // Top border
      StretchBlt(DstBmp.Canvas.Handle, DLeft + Left, DTop, DRight - DLeft - Left - Right, Top,
                 SrcBmp.Canvas.Handle, SLeft + Left, STop, SRight - SLeft - Left - Right, Top, SRCCOPY);
      // Bottom border
      StretchBlt(DstBmp.Canvas.Handle, DLeft + Left, DBottom - Bottom, DRight - DLeft - Right - Left, Bottom,
                 SrcBmp.Canvas.Handle, SLeft + Left, SBottom - Bottom, SRight - SLeft - Right - Left, Bottom, SRCCOPY);
    end;
    // Center
    StretchBlt(DstBmp.Canvas.Handle, DLeft + Left, DTop + Top, DRight - DLeft - Right - Left, DBottom - DTop - Bottom - Top,
               SrcBmp.Canvas.Handle, SLeft + Left, STop + Top, SRight - SLeft - Right - Left, SBottom - STop - Bottom - Top, SRCCOPY);
  end;
end;


procedure TsTitleBar.CalcSizes;
var
  i, LastLeft, LastCenter, LastCenterS, LastRight: integer;

  function ConvAlign(Align: TacItemAlign): TacItemAlign;
  begin
    Result := FItems[i].Align;
    if Form.BidiMode = bdRightToLeft then
      case FItems[i].Align of
        tbaLeft:  Result := tbaRight;
        tbaRight: Result := tbaLeft;
      end
  end;

  procedure CalcItem(Item: TacTitleBarItem);
  begin
    with TDstRect(Item.Rect) do begin
      case Item.Style of
        bsButton:  DTop := FBarVCenter - Item.Height div 2;
        bsMenu:    DTop := FBarRect.Top;
        bsTab:     DTop := FBarRect.Bottom - Item.Height;
        bsSpacing: DTop := FBarVCenter - Item.Height div 2;
        bsInfo:    DTop := FBarVCenter - Item.Height div 2;
      end;

      DBottom := DTop + Item.Height;
      case ConvAlign(Item.Align) of
        tbaLeft: begin
          DLeft := LastLeft;
          inc(LastLeft, Item.Width);
        end;

        tbaRight: begin
          DLeft := LastRight;
          inc(LastRight, Item.Width);
        end;

        tbaCenter: begin
          DLeft := LastCenter;
          inc(LastCenter, Item.Width);
        end;

        tbaCenterInSpace: begin
          DLeft := LastCenterS;
          inc(LastCenterS, Item.Width);
        end;
      end;
      DRight := DLeft + Item.Width;
    end;
  end;

begin
  // Init
  FLeftWidth    := 0;
  FRightWidth   := FRightSpacing;
  FCenterWidth  := 0;
  FCenterWidthS := 0;
  // Calc main parts sizes
  for i := 0 to FItems.Count - 1 do
    if FItems[i].Visible then
      case ConvAlign(FItems[i].Align) of
        tbaLeft:          inc(FLeftWidth,    FItems[i].Width);
        tbaRight:         inc(FRightWidth,   FItems[i].Width);
        tbaCenter:        inc(FCenterWidth,  FItems[i].Width);
        tbaCenterInSpace: inc(FCenterWidthS, FItems[i].Width);
      end;
  // Items positions
  LastLeft    := FBarRect.Left;
  LastCenter  := FBarRect.Left + (FFullRight - FBarRect.Left - FCenterWidth) div 2;
  LastCenterS := FBarRect.Left + FLeftWidth + (FBarRect.Right - FBarRect.Left - FRightWidth - FLeftWidth - FCenterWidthS) div 2;
  LastRight := FBarRect.Right - FRightWidth;
  if Form.BidiMode = bdRightToLeft then begin
    for i := FItems.Count - 1 downto 0 do
      if FItems[i].Visible then
        CalcItem(FItems[i]);
  end
  else
    for i := 0 to FItems.Count - 1 do
      if FItems[i].Visible then
        CalcItem(FItems[i]);
end;


constructor TsTitleBar.Create(AOwner: TComponent);
begin
  if not (AOwner is TCustomForm) then
    Raise EAbort.Create('TsTitleBar component may be used with forms only!');

  inherited Create(AOwner);
  Form := TForm(AOwner);
  FDefaultMenuBtn := TBitmap.Create;
  FDefaultMenuBtn.Handle := LoadBitmap(hInstance, 'MENUBTN');
  FDefaultMenuBtn.PixelFormat := pf32bit;
  UpdateAlphaTitle(FDefaultMenuBtn);
  FCaptionRect := MkRect;
  FFullRight := 0;
  OldScaleValue := 100;
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageListChange;
  FItems := TacTitleItems.Create(Self);
  FItemsMargin := 3;
  FRightSpacing := 0;
  FShowCaption := False;
  FUpdating := False;
end;


destructor TsTitleBar.Destroy;
begin
  FreeAndNil(FItems);
  FreeAndNil(FDefaultMenuBtn);
  FreeAndNil(FImageChangeLink);
  inherited;
end;


procedure TsTitleBar.ImageListChange(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to Items.Count - 1 do
    if Items[i].AutoSize then
      Items[i].UpdateSize;

  Invalidate;
end;


procedure TsTitleBar.Invalidate;
begin
  if Assigned(Form) and not (csLoading in Form.ComponentState) then
    SendMessage(Form.Handle, SM_ALPHACMD, AC_NCPAINT shl 16, 1);
end;


procedure TsTitleBar.Loaded;
var
  i: integer;
begin
  inherited;
  for i := 0 to Items.Count - 1 do
    if Items[i].AutoSize then
      Items[i].UpdateSize;
end;


procedure TsTitleBar.Notification(AComponent: TComponent; Operation: TOperation);
var
  i: integer;
begin
  inherited Notification(AComponent, Operation);
  if AComponent is TCustomImageList then
    case Operation of
      opRemove:
        if FImages = AComponent then
          FImages := nil;
    end
  else
    if AComponent is TPopupMenu then
      case Operation of
        opRemove:
          for i := 0 to Items.Count - 1 do
            if Items[i].DropdownMenu = AComponent then
              Items[i].DropdownMenu := nil;
      end
end;


procedure TsTitleBar.SetImages(const Value: TCustomImageList);
var
  i: integer;
begin
  if FImages <> Value then begin
    if FImages <> nil then
      FImages.UnRegisterChanges(FImageChangeLink);

    FImages := Value;
    if FImages <> nil then begin
      FImages.RegisterChanges(FImageChangeLink);
      FImages.FreeNotification(Self);
    end;
    for i := 0 to Items.Count - 1 do
      if Items[i].AutoSize then
        Items[i].UpdateSize;

    CalcSizes;
    Invalidate;
  end;
end;


procedure TsTitleBar.SetItems(const Value: TacTitleItems);
begin
  FItems := Value;
end;


procedure TsTitleBar.SetShowCaption(const Value: boolean);
begin
  if FShowCaption <> Value then begin
    FShowCaption := Value;
    Invalidate;
  end;
end;


procedure TsTitleBar.UpdateScale(NewValue: integer);
var
  i: integer;
begin
  for i := 0 to Items.Count - 1 do
    Items[i].ScaleItem(NewValue, OldScaleValue);

  CalcSizes;
  OldScaleValue := NewValue;
end;


procedure TacTitleBarItem.AssignTo(Dest: TPersistent);
begin
  if Dest = nil then
    inherited
  else begin
    TacTitleBarItem(Dest).AutoSize := AutoSize;
    TacTitleBarItem(Dest).Align := Align;
    TacTitleBarItem(Dest).Caption := Caption;
    TacTitleBarItem(Dest).Enabled := Enabled;
    TacTitleBarItem(Dest).Glyph := Glyph;
    TacTitleBarItem(Dest).GroupIndex := GroupIndex;
    TacTitleBarItem(Dest).Hint := Hint;
    TacTitleBarItem(Dest).ImageIndex := ImageIndex;
    TacTitleBarItem(Dest).Name := Name;
    TacTitleBarItem(Dest).NumGlyphs := NumGlyphs;
    TacTitleBarItem(Dest).Style := Style;
    TacTitleBarItem(Dest).Visible := Visible;
    TacTitleBarItem(Dest).OnMouseDown := OnMouseDown;
    TacTitleBarItem(Dest).OnMouseUp := OnMouseUp;
  end;
end;


procedure TacTitleBarItem.Changed;
begin
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;


constructor TacTitleBarItem.Create(ACollection: TCollection);
begin
  FAutoSize := True;
  FFontData := TacFontData.Create;
  FFontData.WndHandle := TacTitleItems(ACollection).FOwner.Form.Handle;
  FGlyph := TBitmap.Create;
  FGlyph.OnChange := OnGlyphChange;
  FEnabled := True;
  ExtForm := nil;
  FVisible := True;
  FAlign := tbaLeft;
  FDown := False;
  FImageIndex := -1;
  FSpacing := 4;
  FNumGlyphs := 1;
  FHeight := 18;
  FWidth := 22;
  FTag := 0;
  FGroupIndex := 0;
  FShowHint := False;
  FCursor := crDefault;

  FDisabledImageIndex := -1;
  FImageIndex := -1;
  FHotImageIndex := -1;
  FPressedImageIndex := -1;
  FAlignment := taLeftJustify;

  HintWnd := nil;
  inherited Create(ACollection);
  if FName = '' then
    FName := ClassName;
end;


destructor TacTitleBarItem.Destroy;
begin
  if Timer <> nil then
    FreeAndNil(Timer);

  FreeAndNil(FGlyph);
  FreeAndNil(FFontData);
  if HintWnd <> nil then
    FreeAndNil(HintWnd);

  if Data <> nil then begin
    Data.Free;
    FreeMem(Data);
  end;
  inherited Destroy;
end;


procedure TacTitleBarItem.DoClick;
begin
  FPressed := False;
  if Assigned(FOnClick) then
    FOnClick(Self);
end;


var
  SkipMenuShowing: boolean = False;


type
  TAccessProvider = class(TsSkinProvider);


procedure TacTitleBarItem.DoMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  p: TPoint;
  i: integer;
  R, bR: TRect;
  sp: TsSkinProvider;
  NewItem: TacTitleBarItem;
begin
  if not FPressed and Enabled then begin
    FPressed := True;
    if Assigned(FOnMouseDown) then
      FOnMouseDown(Self, Button, Shift, X, Y);

    case Style of
      bsMenu, bsButton:
        if (FDropDownMenu <> nil) and (FDropDownMenu.Items.Count > 0) then begin
          sp := GetSkinProvider(TacTitleItems(Collection).FOwner.Form);
          if (sp <> nil) and (sp.BorderForm <> nil) and (sp.BorderForm.AForm <> nil) then
            GetWindowRect(sp.BorderForm.AForm.Handle, R)
          else
            GetWindowRect(TacTitleItems(Collection).FOwner.Form.Handle, R);

          if not SkipMenuShowing then begin
            SkipMenuShowing := True;
            p.X := Rect.Left;
            p.Y := Rect.Bottom;
            if Assigned(ExtForm) then begin
              inc(p.X, ExtForm.Left);
              inc(p.Y, ExtForm.Top);
            end
            else begin
              inc(p.Y, R.Top);
              inc(p.X, R.Left);
            end;
{$IFNDEF FPC}
            FDropDownMenu.WindowHandle := TacTitleItems(Collection).FOwner.Form.Handle;
{$ENDIF}
            FDroppedDown := True;
            FreeAndNil(Timer);
            Invalidate;
            if (sp <> nil) and (sp.Form.BidiMode = bdRightToLeft) then begin
              FDropDownMenu.BiDiMode := sp.Form.BiDiMode;
              FDropDownMenu.ParentBidiMode := False;
              FDropDownMenu.Popup(p.X + WidthOf(Rect), p.Y)
            end
            else
              FDropDownMenu.Popup(p.X, p.Y);

            FDroppedDown := False;
            FPressed := False;

            bR := Rect;
            OffsetRect(bR, R.Left, R.Top);
            p := acMousePos;
            if PtInRect(bR, Point(p.X, p.Y)) then begin
              SkipMenuShowing := True;
              FState := 1;
              Invalidate;
              Exit;
            end;

            FState := 0;
            // Repaint a form title
            Invalidate;
{$IFNDEF FPC}
            FDropDownMenu.WindowHandle := 0;
{$ENDIF}
            DoClick;
          end
          else begin
            FDroppedDown := False;
            FPressed := False;
            FreeAndNil(Timer);
            FState := 0;
            Invalidate;
            if sp <> nil then
              TAccessProvider(sp).SetHotHT(HTUDBTN + Index);
          end;
          SkipMenuShowing := False;
        end;

      bsTab:
        if not Down then begin
          DoClick;
          if GroupIndex >= 0 then begin
            TacTitleItems(Collection).FOwner.FUpdating := True;
            for i := 0 to Collection.Count - 1 do begin
              NewItem := TacTitleBarItem(Collection.Items[i]);
              if NewItem.Visible and (NewItem.GroupIndex = GroupIndex) and NewItem.Down then
                NewItem.Down := False;
            end;
            TacTitleItems(Collection).FOwner.FUpdating := False;
            Down := True;
          end
          else
            Invalidate;
        end;
    end;
  end;
end;


procedure TacTitleBarItem.DoMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Enabled then begin
    if Assigned(FOnMouseUp) then
      FOnMouseUp(Self, Button, Shift, X, Y);

    if Button <> mbRight then begin
      if FPressed and (Style <> bsTab) then
        DoClick;

      FPressed := False;
      FState := 1;
    end;
  end;
end;


function TacTitleBarItem.GetDisplayName: string;
begin
  Result := Name;
end;


function TacTitleBarItem.GetState: integer;
begin
  if FDroppedDown or FDown then
    Result := 2
  else
    Result := FState;
end;


procedure TacTitleBarItem.InitFont;
begin
  FFontData.UsedFont.Handle := acGetTitleFont;
  if not FFontData.FUseSysStyle then
    FFontData.UsedFont.Style := FFontData.Font.Style;

  if not FFontData.UseSysFontName then begin
    FFontData.UsedFont.CharSet := FFontData.Font.CharSet;
    FFontData.UsedFont.Name := FFontData.Font.Name;
  end;

  if not FFontData.FUseSysSize or (DefaultManager = nil) then
    FFontData.UsedFont.Height := FFontData.Font.Height
  else
    FFontData.UsedFont.Height := DefaultManager.ScaleInt(GetCaptionFontSize, DefaultManager.SysFontScale);
end;


function TacTitleBarItem.IntXMargin: integer;
begin
  case Style of
    bsMenu: Result := 4
    else    Result := 0;
  end;
end;


procedure TacTitleBarItem.Invalidate;
begin
  with TacTitleItems(Collection).FOwner do
    if Assigned(Form) and not (csLoading in Form.ComponentState) then begin
      if RecalcSize and FAutoSize then
        UpdateSize;

      if Timer <> nil then begin
        FreeAndNil(Timer.NewBmp);
        FreeAndNil(Timer.AForm);
      end;
      if not FUpdating and (SendAMessage(Form.Handle, AC_PREPARING) = 0) then
        SendMessage(Form.Handle, SM_ALPHACMD, AC_NCPAINT shl 16, 1); // Change cache in AlphaSkins and repaint a non-client area
    end;
end;


procedure TacTitleBarItem.OnGlyphChange(Sender: TObject);
begin
//
end;


procedure TacTitleBarItem.ScaleItem(NewValue, OldValue: integer);
begin
  FFontData.FFont.Height := MulDiv(FFontData.FFont.Height, NewValue, OldValue);
  Spacing := MulDiv(Spacing, NewValue, OldValue);
  UpdateSize(not AutoSize);
  if not AutoSize then begin
    Width  := MulDiv(Width,  NewValue, OldValue);
    Height := MulDiv(Height, NewValue, OldValue);
  end;
end;


procedure TacTitleBarItem.SetAlign(const Value: TacItemAlign);
begin
  if FAlign <> Value then begin
    FAlign := Value;
    Invalidate(True);
  end;
end;


procedure TacTitleBarItem.SetAlignment(const Value: TAlignment);
begin
  if FAlignment <> Value then begin
    FAlignment := Value;
    Invalidate;
  end;
end;


procedure TacTitleBarItem.SetBoolean(const Index: Integer; const Value: boolean);
var
  i: integer;

  procedure ChangeProp(var Prop: boolean; Value: boolean);
  begin
    if Prop <> Value then begin
      Prop := Value;
      Invalidate(True);
    end;
  end;

begin
  case Index of
    1: if FDown <> Value then begin
      FDown := Value;
      State := integer(Value) * 2;
      if FDown and ((FGroupIndex <> 0) or (FGroupIndex >= 0) and (Style = bsTab)) then
        for i := 0 to TacTitleItems(Collection).Count - 1 do
          if (TacTitleItems(Collection).GetItem(i) <> Self) and (TacTitleItems(Collection).GetItem(i).GroupIndex = GroupIndex) then begin
            TacTitleItems(Collection).GetItem(i).FDown := False;
            TacTitleItems(Collection).GetItem(i).FState := 0;
          end;

      Invalidate;
    end;
    0: ChangeProp(FAutoSize, Value);
    2: ChangeProp(FEnabled,  Value);
    3: ChangeProp(FVisible,  Value);
  end;
end;


procedure TacTitleBarItem.SetCaption(const Value: acString);
begin
  if FCaption <> Value then begin
    FCaption := Value;
    Invalidate(True);
  end;
end;


procedure TacTitleBarItem.SetGlyph(const Value: TBitmap);
begin
  FGlyph.Assign(Value);
  Invalidate(True);
end;


procedure TacTitleBarItem.SetInteger(const AIndex, Value: integer);

  procedure ChangeProp(var Prop: integer; Value: integer);
  begin
    if Prop <> Value then begin
      Prop := Value;
      Invalidate(True);
    end;
  end;

begin
  if State = 2 then
    State := 0;

  case AIndex of
    0: ChangeProp(FDisabledImageIndex, Value);
    1: ChangeProp(FImageIndex,         Value);
    2: ChangeProp(FHotImageIndex,      Value);
    3: ChangeProp(FPressedImageIndex,  Value);
    4: ChangeProp(FNumGlyphs,          Value);
    5: ChangeProp(FSpacing,            Value);
    6: ChangeProp(FHeight,             Value);
    7: ChangeProp(FWidth,              Value);
    8: ChangeProp(FGroupIndex,         Value);
  end;
end;


procedure TacTitleBarItem.SetName(const Value: string);
begin
  FName := Value;
end;


{$IFDEF ALPHASKINS}
procedure TacTitleBarItem.SetSkinSection(const Value: string);
begin
  if FSkinSection <> Value then begin
    FSkinSection := Value;
    Changed;
    Invalidate;
  end;
end;
{$ENDIF}


procedure TacTitleBarItem.SetState(const Value: integer);
begin
  if FState <> Value then
    if FDroppedDown then
      FState := 2
    else
      FState := Value;
end;


procedure TacTitleBarItem.SetStyle(const Value: TacBtnStyle);
begin
  if FStyle <> Value then begin
    FStyle := Value;
    Invalidate(True);
  end;
end;


procedure TacTitleBarItem.UpdateSize;
var
  ts: TSize;
  b: boolean;
  s: string;
begin
  if Visible then
    with TacTitleItems(Collection).FOwner do begin
      // Size and pos of icon
      if Assigned(Images) and IsValidIndex(ImageIndex, GetImageCount(Images)) then
        FContentSize := MkSize(Images.Width, Images.Height)
      else // Min size
        if Assigned(FGlyph) and not FGlyph.Empty then
          FContentSize := MkSize(FGlyph.Width div NumGlyphs, FGlyph.Height)
        else
          FContentSize := MkSize;

      // Text size
      if Caption <> '' then begin
        // Init font
        InitFont;
        b := True;
        s := Caption;
        ts := GetStringSize(FFontData.UsedFont.Handle, s);
        if FContentSize.cx <> 0 then
          inc(FContentSize.cx, FSpacing); // If image is defined then add a spacing

        FContentSize := MkSize(FContentSize.cx + ts.cx + acSpacing * integer(Style = bsTab),
                               max(FContentSize.cy, ts.cy));
      end
      else
        b := False;

      if (Style in [bsMenu, bsButton]) and (DropDownMenu <> nil) then begin
        if not b then
          InitFont;

        if Style = bsMenu then
          inc(FContentSize.cx, acSpacing)
        else
          inc(FContentSize.cx, 1); // Additional spacing for arrow

        inc(FContentSize.cx, ac_ArrowWidth + acSpacing);
      end;
      if Style in [bsTab, bsMenu] then
        inc(FContentSize.cy, 1);

      if not ContentOnly and not (Style in [bsSpacing]) then begin
        FWidth  := max(16, FContentSize.cx + 2 * (ItemsMargin + IntXMargin));
        FHeight := max(16, FContentSize.cy + 2 * (ItemsMargin));
      end;
    end;
end;


function TacTitleItems.Add: TacTitleBarItem;
begin
  Result := TacTitleBarItem(inherited Add);
end;


constructor TacTitleItems.Create(AOwner: TPersistent);
begin
  FOwner := TsTitleBar(AOwner);
  inherited Create(TacTitleBarItem);
end;


function TacTitleItems.GetItem(Index: Integer): TacTitleBarItem;
begin
  Result := TacTitleBarItem(inherited GetItem(Index))
end;


function TacTitleItems.GetOwner: TPersistent;
begin
  Result := FOwner;
end;


function TacTitleItems.Insert(Index: Integer): TacTitleBarItem;
var
  sp: TsSkinProvider;
begin
  if FOwner.Form <> nil then begin
    sp := GetSkinProvider(FOwner.Form);
    if sp.biClicked then
      sp.KillAnimations;
  end;
  Result := TacTitleBarItem(inherited Insert(Index));
end;


procedure TacTitleItems.SetItem(Index: Integer; Value: TacTitleBarItem);
begin
  inherited SetItem(Index, Value);
end;


constructor TacFontData.Create;
begin
  FFont := TFont.Create;
  GlowMask := nil;
  FGlowColor := clGray;
  FGlowSize := 0;
  UsedFont := TFont.Create;
  FUseSysColor := True;
  FUseSysFontName := True;
  FUseSysGlowing := True;
  FUseSysSize := True;
  FUseSysStyle := True;
end;


destructor TacFontData.Destroy;
begin
  FreeAndNil(FFont);
  FreeAndNil(UsedFont);
  FreeAndNil(GlowMask);
  inherited;
end;


function TacFontData.GetBool(const Index: Integer): boolean;
begin
  case Index of
    0:   Result := FUseSysColor;
    1:   Result := FUseSysFontName;
    2:   Result := FUseSysGlowing;
    3:   Result := FUseSysSize
    else Result := FUseSysStyle;
  end;
end;


procedure TacFontData.Invalidate;
begin
  if WndHandle <> 0 then
    SendMessage(WndHandle, SM_ALPHACMD, AC_NCPAINT shl 16, 1); // Change cache in AlphaSkins and repaint a non-client area
end;


procedure TacFontData.SetBool(const Index: Integer; const Value: boolean);
begin
  case Index of
    0: FUseSysColor    := Value;
    1: FUseSysFontName := Value;
    2: FUseSysGlowing  := Value;
    3: FUseSysSize     := Value;
    4: FUseSysStyle    := Value;
  end;
  Invalidate;
end;


procedure TacFontData.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
end;


procedure TacFontData.SetGlowColor(const Value: TColor);
begin
  if FGlowColor <> Value then begin
    FGlowColor := Value;
    Invalidate;
  end;
end;


procedure TacFontData.SetGlowSize(const Value: integer);
begin
  if FGlowSize <> Value then begin
    FGlowSize := Value;
    Invalidate;
  end;
end;


procedure TacItemAnimation.ChangeState(NewState: integer; Show: boolean);
begin
  ToShow := Show;
  Enabled := True;
end;


procedure TacItemAnimation.CheckMouseLeave;
var
  P, mPos: TPoint;
  R: TRect;
begin
  mPos := acMousePos;
  if Item.ExtForm <> nil then
    p := Point(mPos.X - Item.ExtForm.Left, mPos.Y - Item.ExtForm.Top)
  else begin
    GetWindowRect(FormHandle, R);
    p := Point(mPos.X - R.Left, mPos.Y - R.Top);
  end;
  if not PtInRect(Item.Rect, P) then begin
    Enabled := False;
    SendMessage(FormHandle, WM_MOUSELEAVE, 0, 0);
  end;
end;


constructor TacItemAnimation.Create(AOwner: TComponent);
begin
  inherited;
  CurrentLevel := 0;
  CurrentState := 0;
  NewBmp := nil;
  AForm := nil;
  OldForm := nil;
  ToShow := False;
  OnTimer := OnAnimTimer;
end;


destructor TacItemAnimation.Destroy;
begin
  Enabled := False;
  if AForm <> nil then
    FreeAndNil(AForm);

  if OldForm <> nil then
    FreeAndNil(OldForm);

  if NewBmp <> nil then
    FreeAndNil(NewBmp);

  Item.Timer := nil;
  inherited;
end;


function TacItemAnimation.GetFormBounds: TRect;
var
  mOffset: integer;
begin
  if Item.ExtForm <> nil then
    GetWindowRect(Item.ExtForm.Handle, Result)
  else
    GetWindowRect(FormHandle, Result);

  OffsetRect(Result, Item.Rect.Left, Item.Rect.Top);
  mOffset := Offset;
  if mOffset = 0 then begin
    Result.Right := Result.Left + Item.Rect.Right - Item.Rect.Left;
    Result.Bottom := Result.Top + Item.Rect.Bottom - Item.Rect.Top;
  end;
end;


procedure TacItemAnimation.MakeForm;
begin
  if AForm = nil then begin
    AForm := TacGlowForm.CreateNew(nil);
    AForm.HandleNeeded;
    if (AForm <> nil) and AForm.HandleAllocated then begin
      SetWindowLong(AForm.Handle, GWL_EXSTYLE, GetWindowLong(AForm.Handle, GWL_EXSTYLE) or {WS_EX_TOOLWINDOW or WS_EX_NOACTIVATE or }WS_EX_TRANSPARENT);
      if (Item.ExtForm <> nil) and (TForm(Item.ExtForm).FormStyle = fsStayOnTop) then
        SetWindowLong(AForm.Handle, GWL_EXSTYLE, GetWindowLong(AForm.Handle, GWL_EXSTYLE) or WS_EX_TOPMOST);
    end;
  end;
end;


procedure TacItemAnimation.MakeImg;
var
  R: TRect;
begin
  if (CurrentState <> 0) or (NewBmp = nil) then begin // If updating is required
    R := GetFormBounds;
    if NewBmp = nil then
      NewBmp := CreateBmp32;

    NewBmp.Width := R.Right - R.Left;
    NewBmp.Height := R.Bottom - R.Top;
    if Assigned(Item.OnInternalDraw) then
      Item.OnInternalDraw(Item, MkRect(NewBmp), NewBmp)
    else
      FillRect32(NewBmp, MkRect(NewBmp), clYellow);
  end;
end;


function TacItemAnimation.Offset: integer;
begin
  Result := 0;
end;


procedure TacItemAnimation.OnAnimTimer(Sender: TObject);
begin
  if Enabled then
    if ToShow then
      if CurrentLevel >= MaxIterations then begin // Showing is finished
        CheckMouseLeave;
        if MaxIterations <> -1 then begin
          MaxIterations := -1;
          UpdateForm(MaxByte);
        end;
        FreeAndNil(OldForm);
      end
      else begin
        UpdateForm(max(min(CurrentLevel * Step, MaxByte), 0));
        inc(CurrentLevel);
      end
    else
      if CurrentLevel <= 0 then begin // Hiding is finished
        CurrentState := -1;
        Enabled := False;
        FreeAndNil(NewBmp);
        FreeAndNil(AForm);
      end
      else begin
        UpdateForm(max(0, min(CurrentLevel * Step, MaxByte)));
        dec(CurrentLevel);
      end;
end;


procedure TacItemAnimation.StartAnimation(NewState: integer; Show: boolean);
begin
  if CurrentState <> NewState then begin
    CurrentState := NewState;
    if NewState <> 0 then begin
      if NewState = 2 then begin
        FreeAndNil(AForm);
        FreeAndNil(NewBmp);
      end;
      CurrentLevel := 1;
      ToShow := Show;
      UpdateForm(min(Step, MaxByte));
      inc(CurrentLevel);
      Enabled := True;
    end
    else begin
      ToShow := False;
      dec(CurrentLevel);
      UpdateForm(max(Step, MaxByte));
      if Maxiterations > 1 then
        Enabled := True;
    end;
  end;
end;


function TacItemAnimation.Step: integer;
begin
  Result := max(MaxByte div MaxIterations, 0);
end;


procedure TacItemAnimation.UpdateForm(Blend: integer);
var
  DC: hdc;
  R: TRect;
  iInsAfter: hwnd;
  Flags: Cardinal;
  FBmpSize: TSize;
  OwnerHandle: THandle;
  FBmpTopLeft: TPoint;
  FBlend: TBlendFunction;
begin
  if NewBmp = nil then
    MakeImg;

  if AForm = nil then
    MakeForm;

  if (NewBmp <> nil) and (AForm <> nil) then begin
    FBmpSize := MkSize(NewBmp);
    R := GetFormBounds;
    if FBmpSize.cx <> R.Right - R.Left then // If image is hiding
      InflateRect(R, (FBmpSize.cx - R.Right + R.Left) div 2, (FBmpSize.cy - R.Bottom + R.Top) div 2);

    if Item.ExtForm <> nil then
      OwnerHandle := Item.ExtForm.Handle
    else
      OwnerHandle := FormHandle;

    if GetWindowLong(OwnerHandle, GWL_EXSTYLE) and WS_EX_TOPMOST <> 0 then
      iInsAfter := HWND_TOPMOST
    else
      iInsAfter := GetNextWindow(OwnerHandle, GW_HWNDPREV);

    Flags := SWP_NOACTIVATE or SWP_NOOWNERZORDER or SWP_NOSENDCHANGING;
    // Replacement of SetWindowPos (for Aero)
    AForm.Left := R.Left;
    AForm.Top := R.Top;
    AForm.Width := FBmpSize.cx;

    AForm.Height := FBmpSize.cy;

    FBmpTopLeft := MkPoint;
    InitBlendData(FBlend, Blend);

    SetWindowPos(AForm.Handle, iInsAfter, 0, 0, 0, 0, Flags or SWP_NOMOVE or SWP_NOSIZE);
    DC := GetDC(0);
    SetWindowLong(AForm.Handle, GWL_EXSTYLE, GetWindowLong(AForm.Handle, GWL_EXSTYLE) or WS_EX_LAYERED or WS_EX_NOACTIVATE or WS_EX_TRANSPARENT);
    UpdateLayeredWindow(AForm.Handle, DC, nil, @FBmpSize, NewBmp.Canvas.Handle, @FBmpTopLeft, clNone, @FBlend, ULW_ALPHA);
    ShowWindow(AForm.Handle, SW_SHOWNOACTIVATE);
    ReleaseDC(0, DC);
  end;
end;

end.
