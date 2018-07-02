unit acFloatCtrls;
{$I sDefs.inc}
//{$DEFINE LOGGED}

interface

uses Classes, SysUtils, Forms, Messages, Controls, Windows, Graphics, ImgList, ExtCtrls, Buttons,
  {$IFDEF TNTUNICODE} TntWideStrUtils, TntMenus, TntStdCtrls, TntControls, {$ENDIF}
  {$IFNDEF DELPHI5} Types, {$ENDIF}
  sSkinProvider, sConst, acntTypes, acThdTimer, acArcControls;


type
{$IFNDEF NOTFORHELP}
  TVertAlignment = (vaAlignTop, vaAlignBottom, vaVerticalCenter, vaFormTitleCenter);

  TacFloatComponent = class(TComponent)
  private
    FImages: TCustomImageList;
    FImageChangeLink: TChangeLink;
    procedure SetImages(const Value: TCustomImageList);
  protected
    Flag,
    Hiding,
    Updating: boolean;

    Form: TCustomForm;
    FHintTimer: TTimer;
    FOwner: TComponent;
    SP: TsSkinProvider;
    FFormPrevBounds: TRect;
    OldWndProc: TWndMethod;

    procedure NewWndProc(var Message: TMessage);

    procedure OnHintTimer(Sender: TObject);
    procedure StartHintTimer(Item: TObject);

    procedure ImageListChange(Sender: TObject);
    procedure CopyItems(Bmp: TBitmap); virtual; abstract;
    function CanCreate: boolean; virtual; abstract;
    function GetTopWnd: HWND; virtual; abstract;
    procedure ChangedPosition; virtual; abstract;
    procedure UpdateBitmaps; virtual; abstract;
    procedure KillTimers; virtual; abstract;
    procedure HideItems; virtual; abstract;
    procedure Invalidate; virtual; abstract;
    procedure SetZOrder(TopWnd: integer); virtual; abstract;
    procedure SetScale(Value: integer); virtual; abstract;
    procedure SetAlpha(AlphaValue: byte); virtual; abstract;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    procedure BeginUpdate;
    procedure EndUpdate;
  published
    property Images: TCustomImageList read FImages write SetImages;
  end;


  TacFloatBtnStyle = (fbsDefault, fbsTransparent, fbsRound);
  TsFloatButtons = class;

  TacCustomPaintData = record
    DestBmp: TBitmap;
  end;

  TCustomPaint = procedure(Sender: TObject; PaintData: TacCustomPaintData) of object;
  TacFloatBtn = class;
{$ENDIF} // NOTFORHELP

  TacFloatBtn = class(TCollectionItem)
{$IFNDEF NOTFORHELP}
  private
    FOnMouseUp,
    FOnMouseDown: TMouseEvent;

    FEnabled,
    FPressed,
    FVisible,
    FShowHint,
    FAutoSize,
    FAllowClick,
    FCustomFont,
    FShowCaption,
    MouseChecking,
    FCustomColors: boolean;

    FHint,
    FCaption: acString;

    FName: string;
    FOwner: TsFloatButtons;

    FTop,
    State,
    FLeft,
    FWidth,
    FHeight,
    FMargin,
    FOffsetX,
    FOffsetY,
    FImageIndex,
    FImageIndexHot,
    FImageIndexPressed: integer;

    FOnMouseEnter,
    FOnMouseLeave: TNotifyEvent;

    AForm: TacGlowForm;
    AlphaBmp: TBitmap;

    FFont: TFont;

    FBlendValue: byte;
    OldWndProc: TWndMethod;
    FOnClick: TNotifyEvent;
    FAlignHorz: TAlignment;
    FOnDblClick: TNotifyEvent;
    FAlignVert: TVertAlignment;
    AnimTimer: TacThreadedTimer;
    FPaintOptions: TacPaintButtonOptions;

    HintWnd: {$IFDEF TNTUNICODE}TTntHintWindow{$ELSE}THintWindow{$ENDIF};
    HintHandle: THandle;

    FCursor: TCursor;
    FLayout: TButtonLayout;
    FOnPaint: TCustomPaint;
    CheckMouseTimer: TTimer;
    FStyle: TacFloatBtnStyle;
    FOnMouseMove: TMouseMoveEvent;
    procedure NewWndProc(var Message: TMessage);

    function GlyphSize: TSize;
    function ContentSize: TSize;
    procedure CheckMouseProc(Sender: TObject);
    procedure SetName   (const Value: string);
    procedure SetBoolean(const Index: integer; const Value: boolean);
    procedure SetCaption(const Value: acString);
    procedure SetInteger(const Index, Value: integer);
    procedure MakeForm;
    function GlyphIndex: integer;
    function CurrentAlpha: integer;

    procedure CreateAlphaBmp;

    function CanShow: boolean;
    function CanMouseClick: boolean;
    procedure KillTimer;
    procedure SetFont(const Value: TFont);
    procedure SetAlphaValue(const Value: byte);
    procedure ChangeState(NewState: integer);
    procedure SetStyle(const Value: TacFloatBtnStyle);
    function CalcBounds: TRect;
    procedure SetEnabled(const Value: boolean);
    procedure SetCursor(const Value: TCursor);
    procedure DoInvalidate(Sender: TObject);
    procedure SetLayout(const Value: TButtonLayout);
    procedure UpdateScale(const Value, OldValue: integer);
  public
    destructor Destroy; override;
    function GetDisplayName: string; override;
    procedure AssignTo(Dest: TPersistent); override;
    constructor Create(ACollection: TCollection); override;
    function IsUnderMouse: boolean;
    procedure UpdatePosition;
    procedure AdjustSize;
    procedure Repaint;
  published
{$ENDIF} // NOTFORHELP
    property Enabled: boolean read FEnabled write SetEnabled default True;
    property Caption: acString read FCaption write SetCaption;
    property Hint: acString read FHint write FHint;
    property Name: string read FName write SetName;
    property Cursor: TCursor read FCursor write SetCursor default crDefault;
    property AutoSize:    boolean index 0 read FAutoSize     write SetBoolean default True;
    property Visible:     boolean index 1 read FVisible      write SetBoolean default True;
    property CustomFont:  boolean index 2 read FCustomFont   write SetBoolean default False;
    property CustomColors:boolean index 3 read FCustomColors write SetBoolean default False;
    property ShowCaption: boolean index 4 read FShowCaption  write SetBoolean default True;

    property AllowClick: boolean read FAllowClick write FAllowClick default True;
    property ShowHint:   boolean read FShowHint write FShowHint default True;

    property AlignVert: TVertAlignment read FAlignVert write FAlignVert default vaFormTitleCenter;
    property AlignHorz: TAlignment     read FAlignHorz write FAlignHorz default taLeftJustify;

    property Left:    integer index 0 read FLeft    write SetInteger default 0;
    property Top:     integer index 1 read FTop     write SetInteger default 0;
    property Width:   integer index 2 read FWidth   write SetInteger default 32;
    property Height:  integer index 3 read FHeight  write SetInteger default 32;

    property OffsetX: integer index 4 read FOffsetX write SetInteger default 0;
    property OffsetY: integer index 5 read FOffsetY write SetInteger default 0;
    property Margin:  integer index 9 read FMargin  write SetInteger default 4;

    property BlendValue: byte read FBlendValue write SetAlphaValue default MaxByte;
    property Style: TacFloatBtnStyle read FStyle write SetStyle default fbsDefault;

    property Font: TFont read FFont write SetFont;
    property PaintOptions: TacPaintButtonOptions read FPaintOptions write FPaintOptions;

    property ImageIndex:        integer index 6 read FImageIndex        write SetInteger default -1;
    property ImageIndexHot:     integer index 7 read FImageIndexHot     write SetInteger default -1;
    property ImageIndexPressed: integer index 8 read FImageIndexPressed write SetInteger default -1;

    property Layout: TButtonLayout read FLayout write SetLayout default blGlyphTop;

    property OnMouseDown: TMouseEvent read FOnMouseDown write FOnMouseDown;
    property OnMouseUp:   TMouseEvent read FOnMouseUp   write FOnMouseUp;
    property OnClick:    TNotifyEvent read FOnClick    write FOnClick;
    property OnDblClick: TNotifyEvent read FOnDblClick write FOnDblClick;
    {:@event}
    property OnPaint: TCustomPaint read FOnPaint write FOnPaint;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnMouseMove: TMouseMoveEvent read FOnMouseMove write FOnMouseMove;
  end;


{$IFNDEF NOTFORHELP}
  TacFloatBtnsList = class(TCollection)
  private
    FOwner: TsFloatButtons;
    function  GetItem(Index: Integer): TacFloatBtn;
    procedure SetItem(Index: Integer; Value: TacFloatBtn);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TsFloatButtons);
    destructor Destroy; override;
    property Items[Index: Integer]: TacFloatBtn read GetItem write SetItem; default;
  end;
{$ENDIF} // NOTFORHELP


{$IFDEF DELPHI_XE3}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]{$ENDIF}
  TsFloatButtons = class(TacFloatComponent)
{$IFNDEF NOTFORHELP}
  private
    ScalePercent: integer;
    FItems: TacFloatBtnsList;
    procedure SetItems(const Value: TacFloatBtnsList);
  protected
    procedure SetAlpha(AlphaValue: byte); override;
    procedure SetZOrder(TopWnd: integer); override;
    procedure SetScale(Value: integer); override;
    procedure CopyItems(Bmp: TBitmap); override;
    function CanCreate: boolean; override;
    procedure ChangedPosition; override;
    function GetTopWnd: HWND; override;
    procedure UpdateBitmaps; override;
    procedure KillTimers; override;
    procedure HideItems; override;
    procedure UpdatePositions;
    procedure RepaintItems;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Invalidate; override;
    destructor Destroy; override;
    procedure Loaded; override;
  published
{$ENDIF} // NOTFORHELP
    property Items: TacFloatBtnsList read FItems write SetItems;
  end;


implementation

uses
  math,
  {$IFDEF LOGGED}sDebugMsgs, {$ENDIF}
  acntUtils, sGraphUtils, sAlphaGraph, sVclUtils, acAlphaImageList, sThirdParty, sMessages, sStyleSimply, sButton, sSkinManager;


procedure DoShowHintWnd(Item: TObject);
begin
  if Item is TacFloatBtn then
    with TacFloatBtn(Item) do
      if HintWnd = nil then begin
        if HintWindowClass = THintWindow then
          HintWnd := acShowHintWnd(Hint, Point(acMousePos.X, acMousePos.Y))
        else
          HintWnd := acShowHintWnd(Hint, Point(acMousePos.X, acMousePos.Y + 16));

        if HintWnd <> nil then
          HintHandle := HintWnd.Handle
        else
          HintHandle := 0;
      end;
end;


procedure DoHideHintWnd(Item: TObject);
begin
  if Item is TacFloatBtn then
    with TacFloatBtn(Item) do begin
      if IsWindow(HintHandle) then
        acHideHintWnd(HintWnd)
      else
        HintWnd := nil;

      HintHandle := 0;
    end;
end;


procedure TacFloatComponent.BeginUpdate;
begin
  Updating := True;
end;


constructor TacFloatComponent.Create(AOwner: TComponent);
begin
  if AOwner is TCustomForm then begin
    FOwner := AOwner;
    Flag := False;
    Form := TForm(AOwner);
    Updating := False;
    Hiding := False;
    if CanCreate then begin
      inherited Create(AOwner);
      FImageChangeLink := TChangeLink.Create;
      FImageChangeLink.OnChange := ImageListChange;
      if not (csDesigning in ComponentState) then begin
        OldWndProc := Form.WindowProc;
        Form.WindowProc := NewWndProc;
      end;
    end;
  end
  else
    Alert(ClassName + ' component may be used with forms only!');
end;


function TsFloatButtons.CanCreate: boolean;
begin
  Result := True;
end;


destructor TacFloatComponent.Destroy;
begin
  FreeAndNil(FHintTimer);
  if not (csDesigning in ComponentState) then
    Form.WindowProc := OldWndProc;

  FreeAndNil(FImageChangeLink);
  inherited;
end;


procedure TacFloatComponent.EndUpdate;
begin
  Updating := False;
  ChangedPosition;
end;


procedure TacFloatComponent.ImageListChange(Sender: TObject);
begin
  Invalidate;
  ChangedPosition;
end;


procedure TacFloatComponent.Loaded;
begin
  inherited;
  FFormPrevBounds := Form.BoundsRect;
end;


procedure TacFloatComponent.NewWndProc(var Message: TMessage);
begin
{$IFDEF LOGGED}
  AddToLog(Message);
{$ENDIF}
  case Message.Msg of
    SM_ALPHACMD:
      case Message.WParamHi of
        AC_UPDATEFLOATITEMS:
          ChangedPosition;

        AC_PAINTFLOATITEMS:
          CopyItems(TBitmap(Message.LPARAM));

        AC_UPDATEITEMSALPHA:
          SetAlpha(Message.LParam);

        AC_SETFLOATZORDER:
          SetZOrder(Message.LParam);

        AC_UPDATEFLOATBITMAPS:
          UpdateBitmaps;

        AC_KILLTIMERS:
          KillTimers;

        AC_GETTOPWND: begin
          Message.Result := GetTopWnd;
          Exit;
        end;

        AC_SETSCALE:
          SetScale(Message.LParam);
      end;

    WM_CLOSE, CM_VISIBLECHANGED, WM_SHOWWINDOW:
      if Message.WParam = 0 then begin
        KillTimers;
        HideItems;
        OldWndProc(Message);
        Exit;
      end;
  end;
  OldWndProc(Message);
  if Form.Showing then
    case Message.Msg of
      WM_WINDOWPOSCHANGED, WM_SIZE, WM_EXITSIZEMOVE:
        if IsIconic(Form.Handle) then
          HideItems
        else
          if (SP <> nil) and (sp.FormState and FS_ANIMRESTORING <> 0) then
            HideItems
          else
            if not Flag then begin
              Flag := True;
              ChangedPosition;
              Flag := False;
            end;
    end;
end;


procedure TacFloatComponent.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = Images) then
    Images := nil;
end;


procedure TacFloatComponent.OnHintTimer(Sender: TObject);
begin
  if (FHintTimer <> nil) and (TObject(FHintTimer.Tag) is TacFloatBtn) then
    with TacFloatBtn(FHintTimer.Tag) do begin
      FHintTimer.Enabled := False;
      if (AForm <> nil) and PtInRect(AForm.BoundsRect, acMousePos) then
        DoShowHintWnd(TObject(FHintTimer.Tag));
    end;
end;


procedure TacFloatComponent.SetImages(const Value: TCustomImageList);
begin
  if Images <> Value then begin
    if Images <> nil then
      Images.UnRegisterChanges(FImageChangeLink);

    FImages := Value;
    if Images <> nil then begin
      Images.RegisterChanges(FImageChangeLink);
      Images.FreeNotification(Self);
    end;
    Invalidate;
  end;
end;


procedure TacFloatComponent.StartHintTimer(Item: TObject);
begin
  if Item is TacFloatBtn then
    with TacFloatBtn(Item) do
      if ShowHint and (Hint <> '') and (not IsWindow(HintHandle) or not IsWindowVisible(HintHandle)) and ((FHintTimer = nil) or not FHintTimer.Enabled) then begin
        HintWnd := nil;
        if FHintTimer = nil then
          FHintTimer := TTimer.Create(nil);

        AForm.BoundsRect;
        FHintTimer.Tag := Integer(Item);
        if Application.HintPause > 0 then begin
          FHintTimer.Interval := Application.HintPause;
          FHintTimer.OnTimer := OnHintTimer;
          FHintTimer.Enabled := True;
        end
        else begin
          if FHintTimer <> nil then
            FHintTimer.Enabled := False;

          DoShowHintWnd(Item);
        end;
      end;
end;


procedure TacFloatBtn.AdjustSize;
begin
  if AutoSize then
    with ContentSize do
      if Style <> fbsRound then begin
        FWidth := cx + (FMargin + PaintOptions.BorderWidth) * 2;
        FHeight := cy + (FMargin + PaintOptions.BorderWidth) * 2;
      end
      else begin
        FWidth := Round(sqrt(sqr(cx) + sqr(cy))) + (FMargin + PaintOptions.BorderWidth) * 2;
        FHeight := FWidth;
      end;
end;


function UpdateFloatBtn(Data: TObject; Iteration: integer): boolean;
begin
  Result := False;
  if Data is TacFloatBtn then
    with TacFloatBtn(Data) do
      if Assigned(AnimTimer.BmpFrom) and Assigned(AnimTimer.BmpTo) then begin
        SumBitmapsToDst(AlphaBmp, AnimTimer.BmpTo, AnimTimer.BmpFrom, MaxByte - Round(Iteration * AnimTimer.ValueStep));
        SetFormBlendValue(AForm.Handle, AlphaBmp, CurrentAlpha);
        if AnimTimer.Iteration >= AnimTimer.Iterations then begin
          if (AnimTimer.State in [0, 3]) and (Round(Iteration * AnimTimer.ValueStep) < MaxByte) then
            Exit;
        end
        else
          Result := True;
      end;
end;


procedure TacFloatBtn.ChangeState(NewState: integer);
var
  i: integer;
  cRect: TRect;
begin
  if (FOwner.SP <> nil) and (FOwner.SP.SkinData.SkinManager <> nil) and FOwner.SP.SkinData.SkinManager.Effects.AllowAnimation and (State <> 2) then begin
    if NewState = 0 then
      DoHideHintWnd(Self);

    cRect := MkRect(AlphaBmp);

    i := GetNewTimer(AnimTimer, AForm, NewState);
    if (AnimTimer.State >= 0) and (NewState = AnimTimer.State) then // Started already
      Exit;

    if AnimTimer.BmpFrom <> nil then
      FreeAndNil(AnimTimer.BmpFrom);

    CreateAlphaBmp;
    AnimTimer.BmpFrom := AlphaBmp;
    AlphaBmp := nil;
    State := NewState;

    if AnimTimer.BmpTo <> nil then
      FreeAndNil(AnimTimer.BmpTo);

    CreateAlphaBmp;
    AnimTimer.BmpTo := AlphaBmp;
    AlphaBmp := nil;

    AlphaBmp := CreateBmp32(AnimTimer.BmpTo);

    AnimTimer.InitData(Self, i, UpdateFloatBtn, State);
    AnimTimer.TimeHandler;
  end
  else begin
    State := NewState;
    Repaint;
  end;
end;


procedure TacFloatBtn.CheckMouseProc(Sender: TObject);
begin
  MouseChecking := True;
  if (AForm = nil) or (WindowFromPoint(acMousePos) <> AForm.Handle) then begin
    CheckMouseTimer.Enabled := False;
    DoHideHintWnd(Self);
    if Assigned(FOnMouseLeave) then
      FOnMouseLeave(Self);
  end;
  MouseChecking := False;
end;


function TacFloatBtn.ContentSize: TSize;
var
  TextSize: TSize;
begin
  if ShowCaption and (Caption <> '') then
    acGetTextExtent(0, Caption, TextSize, Font.Handle)
  else
    TextSize := MkSize;

  with GlyphSize do
    case Layout of
      blGlyphLeft, blGlyphRight: begin
        Result.cx := cx + iff((TextSize.cx <> 0) and (cx <> 0), acSpacing, 0) + TextSize.cx;
        Result.cy := max(cy, TextSize.cy);
      end;

      blGlyphTop, blGlyphBottom: begin
        Result.cx := max(cx, TextSize.cx);
        Result.cy := cy + iff((TextSize.cy <> 0) and (cy <> 0), acSpacing, 0) + TextSize.cy;
      end;
    end;
end;


procedure TacFloatBtn.AssignTo(Dest: TPersistent);
begin
  if Dest = nil then
    inherited
  else begin
    TacFloatBtn(Dest).Enabled := Enabled;
    TacFloatBtn(Dest).Hint := Hint;
    TacFloatBtn(Dest).Caption := Caption;
    TacFloatBtn(Dest).Name := Name;
    TacFloatBtn(Dest).OnMouseDown := OnMouseDown;
    TacFloatBtn(Dest).OnMouseUp := OnMouseUp;
  end;
end;


function TacFloatBtn.CanMouseClick: boolean;
begin
  Result := Enabled and FAllowClick;
end;


function TacFloatBtn.CanShow: boolean;
begin
  if (FOwner.Form <> nil) and FOwner.Form.HandleAllocated then begin
    Result := FVisible and not IsIconic(FOwner.Form.Handle) and
                not (fsCreating in FOwner.Form.FormState) and
                  ((FOwner.SP = nil) or (FOwner.SP.FormState and FS_ANIMSHOWING = 0));

    if Result and (FOwner.SP <> nil) and (FOwner.SP.BorderForm <> nil) then
      Result := FOwner.SP.FormState and FS_BLENDMOVING = 0;
  end
  else
    Result := False;

  if not Result then
    KillTimer;
end;


constructor TacFloatBtn.Create(ACollection: TCollection);
begin
  FEnabled := True;
  FVisible := True;
  FAutoSize := True;
  FAllowClick := True;
  FShowCaption  := True;
  MouseChecking := False;
  AForm := nil;
  AlphaBmp := nil;
  AnimTimer := nil;
  CheckMouseTimer := nil;
  inherited Create(ACollection);
  FOwner := TacFloatBtnsList(ACollection).FOwner;
  State   := 0;
  FLeft   := 0;
  FTop    := 0;
  FWidth  := 32;
  FHeight := 32;
  FMargin := 4;
  FCursor := crDefault;
  FBlendValue := MaxByte;
  FShowHint := True;
  HintHandle := 0;
  FStyle := fbsDefault;
  FPaintOptions := TacPaintButtonOptions.Create;
  FPaintOptions.OnInvalidate := Self.DoInvalidate;
  FLayout := blGlyphTop;

  FFont := TFont.Create;
  FAlignVert := vaFormTitleCenter;
  FAlignHorz := taLeftJustify;

  FImageIndex        := -1;
  FImageIndexHot     := -1;
  FImageIndexPressed := -1;

  if FName = '' then
    FName := ClassName;
end;


procedure MakeHalfAlpha(var Prop: TsColor_; const Param: integer);
begin
  if Param = 1 then begin
    Prop.B := Prop.B shr 1;
    Prop.G := Prop.G shr 1;
    Prop.R := Prop.R shr 1;
  end;
  Prop.A := Prop.A shr 1;
end;


procedure TacFloatBtn.CreateAlphaBmp;
var
  AGlyphIndex, sIndex, i: integer;
  TextRect, GlyphRect: TRect;
  dpData: TacButtonPaintData;
  pd: TacCustomPaintData;
  C: TColor;

  procedure InitData(var RectText, RectGlyph: TRect);
  var
    TextSize, ImgSize, cSize: TSize;
  begin
    ImgSize := GlyphSize;
    cSize := ContentSize;
    if ShowCaption and (Caption <> '') then begin
      dpData.AText := Caption;
      AlphaBmp.Canvas.Font.Assign(Font);
      acGetTextExtent(AlphaBmp.Canvas.Handle, dpData.AText, TextSize);
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

  function PaintBody: integer;
  begin
    case FStyle of
      fbsDefault:
        if (FOwner.SP <> nil) and FOwner.SP.SkinData.Skinned then begin
          Result := FOwner.SP.SkinData.SkinManager.ConstData.Sections[ssButton];
          PaintItem32(Result, True, State, MkRect(AlphaBmp), MkPoint, AlphaBmp, FOwner.SP.SkinData.SkinManager);
        end
        else begin
          FillDC(AlphaBmp.Canvas.Handle, MkRect(AlphaBmp), clActiveCaption);
          acPaintStdBtn(AlphaBmp.Canvas, MkRect(AlphaBmp), Enabled, False, State);
          FillAlphaRect(AlphaBmp, MkRect(AlphaBmp), MaxByte);
          Result := -1;
        end;

      fbsRound: begin
        i := mini(AlphaBmp.Width, AlphaBmp.Height);
        if CustomColors then
          InitBtnPaintData(nil, nil, State, PaintOptions, dpData)
        else
          InitBtnPaintData(FOwner.SP.SkinData.SkinManager, nil, State, PaintOptions, dpData);

        DrawRoundButton(AlphaBmp.Canvas.Handle, 0, 0, i, dpData);
        AlphaBmp.Canvas.Font.Color := dpData.AFontColor;
        Result := -1;
      end

      else begin
        Result := -1;
        if FOwner.SP.SkinData.Skinned and (Font.Color = clWindowText) then
          AlphaBmp.Canvas.Font.Color := FOwner.SP.SkinData.SkinManager.Palette[pcLabelText]
        else
          AlphaBmp.Canvas.Font.Color := Font.Color;
      end;
    end
  end;

begin
  if AlphaBmp = nil then
    AlphaBmp := CreateBmp32(Width, Height)
  else begin
    AlphaBmp.Width := Width;
    AlphaBmp.Height := Height;
  end;
  FillRect32(AlphaBmp, MkRect(AlphaBmp), 0, 0);
  if Assigned(FOnPaint) then begin
    pd.DestBmp := AlphaBmp;
    FOnPaint(Self, pd);
  end
  else begin
    InitData(TextRect, GlyphRect);
    sIndex := PaintBody;
    AGlyphIndex := GlyphIndex;
    if AGlyphIndex >= 0 then begin
      if (State = 0) and (FOwner.SP <> nil) and FOwner.SP.SkinData.Skinned and FOwner.SP.SkinData.SkinManager.Effects.DiscoloredGlyphs then
        if (FStyle = fbsTransparent) or (FOwner.SP.SkinData.SkinManager.ConstData.Sections[ssButton] < 0) then
          C := FOwner.SP.SkinData.SkinManager.GetGlobalColor
        else
          C := FOwner.SP.SkinData.SkinManager.gd[FOwner.SP.SkinData.SkinManager.ConstData.Sections[ssButton]].Props[0].Color
      else
        C := clNone;

      if CustomFont then begin
        AlphaBmp.Canvas.Font.Color := PaintOptions.GetData(State).FontColor;
        AlphaBmp.Canvas.Font.Size := 0;
      end;
      DrawAlphaImgList(FOwner.Images, AlphaBmp, GlyphRect.Left, GlyphRect.Top, AGlyphIndex, 0, C, 0, 1, False);
    end;
    if Caption <> '' then begin
      AlphaBmp.Canvas.Brush.Style := bsClear;
      if (sIndex >= 0) and not FCustomFont then
        WriteText32(AlphaBmp, PacChar(Caption), Enabled, TextRect, DT_EXPANDTABS or DT_VCENTER or DT_SINGLELINE, sIndex, State, FOwner.SP.SkinData.SkinManager)
      else
        WriteText32(AlphaBmp, PacChar(Caption), Enabled, TextRect, DT_EXPANDTABS or DT_VCENTER or DT_SINGLELINE, -1, State, nil);
    end;
  end;
  if not FEnabled then
    if FOwner.SP <> nil then
      ChangeBitmapPixels(AlphaBmp, MakeHalfAlpha, integer(FOwner.SP.FormState and FS_BLENDMOVING = 0), clNone)
    else
      ChangeBitmapPixels(AlphaBmp, MakeHalfAlpha, 1, clNone);
end;


function TacFloatBtn.CurrentAlpha: integer;
begin
  if State = 1 then
    Result := MaxByte
  else begin
    Result := BlendValue;
    if State = 2 then
      Result := Result div 2
    else
      if not Enabled then
        Result := Result div 2;
  end;
end;


destructor TacFloatBtn.Destroy;
begin
  FreeAndNil(CheckMouseTimer);
  FreeAndNil(AlphaBmp);
  FPaintOptions.Free;
  FreeAndNil(AForm);
  FFont.Free;
  inherited Destroy;
end;


procedure TacFloatBtn.DoInvalidate(Sender: TObject);
begin
  UpdatePosition;
end;


function TacFloatBtn.GetDisplayName: string;
begin
  Result := Name;
end;


function TacFloatBtn.GlyphIndex: integer;
begin
  if FOwner.Images <> nil then begin
    if (State > 0) and IsValidIndex(ImageIndexHot, FOwner.Images.Count) then
      Result := ImageIndexHot
    else
      Result := ImageIndex;

    if (State = 2) and IsValidIndex(ImageIndexPressed, FOwner.Images.Count) then
      Result := ImageIndexPressed;
  end
  else
    Result := -1;
end;


function TacFloatBtn.GlyphSize: TSize;
begin
  if (FOwner.Images <> nil) and (ImageIndex >= 0) then
    Result := MkSize(GetImageWidth(FOwner.Images), GetImageHeight(FOwner.Images))
  else
    Result := MkSize;
end;



function TacFloatBtn.IsUnderMouse: boolean;
begin
  if AForm <> nil then
    Result := WindowFromPoint(acMousePos) = AForm.Handle
  else
    Result := False;
end;


procedure TacFloatBtn.KillTimer;
begin
  if AnimTimer <> nil then begin
    AnimTimer.Enabled := False;
    FreeAndNil(AnimTimer);
  end;
end;


procedure TacFloatBtn.MakeForm;
begin
  if not (csDesigning in FOwner.ComponentState) then
    if CanShow then begin
      if AForm = nil then begin
        AForm := TacGlowForm.CreateNew(FOwner);
        AForm.TransparentMouse := False;
        AForm.Cursor := FCursor;
        OldWndProc := AForm.WindowProc;
        AForm.WindowProc := NewWndProc;
      end;
      AdjustSize;
    end
    else
      FreeAndNil(AForm);
end;


procedure TacFloatBtn.NewWndProc(var Message: TMessage);
var
  M: TMessage;
begin
  OldWndProc(Message);
  case Message.Msg of
    WM_NCHITTEST: if not CanMouseClick and not MouseChecking then begin
      Message.Result := HTTRANSPARENT;
      if Assigned(FOnMouseEnter) then
        FOnMouseEnter(Self);

      if FEnabled then
        FOwner.StartHintTimer(Self);

      if CheckMouseTimer = nil then begin
        CheckMouseTimer := TTimer.Create(TacFloatBtnsList(Collection).FOwner);
        CheckMouseTimer.Interval := 100;
      end;
      CheckMouseTimer.OnTimer := CheckMouseProc;
      CheckMouseTimer.Enabled := True;
      Exit;
    end;

    WM_LBUTTONDOWN: if CanMouseClick then begin
      FPressed := True;
      FreeAndNil(FOwner.FHintTimer);
      DoHideHintWnd(Self);
      State := 2;
      Repaint;
      if Assigned(FOnMouseDown) then
        FOnMouseDown(Self, mbLeft, [], TWMLButtonDown(Message).XPos, TWMLButtonDown(Message).YPos);
    end;

    WM_LBUTTONUP: if CanMouseClick then begin
      State := integer(acMouseInControl(AForm));
      Repaint;
      if Assigned(FOnMouseUp) then
        FOnMouseUp(Self, mbLeft, [], TWMLButtonDown(Message).XPos, TWMLButtonDown(Message).YPos);

      if FPressed then begin
        FPressed := False;
        if Assigned(FOnClick) then
          FOnClick(Self);
      end;
    end;

    WM_LBUTTONDBLCLK: if CanMouseClick then begin
      M := MakeMessage(WM_LBUTTONDOWN, Message.WParam, Message.LParam, Message.Result);
      NewWndProc(M);
      if Assigned(FOnDblClick) then
        FOnDblClick(Self);
    end;

    WM_MOUSEMOVE: begin
      if not acMouseInControl(AForm) and FEnabled then begin
        ChangeState(0);
        FPressed := False;
      end;
      if Assigned(FOnMouseMove) then
        FOnMouseMove(Self, GetShiftState, TWMMouse(Message).XPos, TWMMouse(Message).YPos);
    end;

    CM_MOUSEENTER: begin
      if FEnabled then begin
        FOwner.StartHintTimer(Self);
        ChangeState(1 + integer(FPressed));
      end;
      if Assigned(FOnMouseEnter) then
        FOnMouseEnter(Self);
    end;

    CM_MOUSELEAVE: begin
      if FEnabled then
        ChangeState(0);

      if Assigned(FOnMouseLeave) then
        FOnMouseLeave(Self);
    end;

    WM_MOUSEACTIVATE:
      Message.Result := MA_NOACTIVATE;
  end;
end;


procedure TacFloatBtn.Repaint;
begin
  if (FOwner.ComponentState * [csDesigning, csDestroying, csLoading] = []) and (AForm <> nil) then begin
    CreateAlphaBmp;
    if (FOwner.SP = nil) or (FOwner.SP.FormState and FS_BLENDMOVING = 0) then
      if (FStyle = fbsTransparent) and (State = 2) {or not Enabled} then
        SetFormBlendValue(AForm.Handle, AlphaBmp, FBlendValue div 2)
      else
        SetFormBlendValue(AForm.Handle, AlphaBmp, FBlendValue);
  end;
end;


procedure TacFloatBtn.SetAlphaValue(const Value: byte);
begin
  if FBlendValue <> Value then begin
    FBlendValue := Value;
    Repaint;
  end;
end;


procedure TacFloatBtn.SetBoolean(const Index: integer; const Value: boolean);

  procedure ChangeProp(var Prop: boolean; Value: boolean);
  begin
    if Prop <> Value then begin
      Prop := Value;
      UpdatePosition;
    end;
  end;

begin
  case Index of
    0: ChangeProp(FAutoSize,     Value);
    1: ChangeProp(FVisible,      Value);
    2: ChangeProp(FCustomFont,   Value);
    3: ChangeProp(FCustomColors, Value);
    4: ChangeProp(FShowCaption,  Value);
  end;
end;


procedure TacFloatBtn.SetCaption(const Value: acString);
begin
  if FCaption <> Value then begin
    FCaption := Value;
    UpdatePosition;
  end;
end;


procedure TacFloatBtn.SetCursor(const Value: TCursor);
begin
  FCursor := Value;
  if AForm <> nil then
    AForm.Cursor := Value;
end;


procedure TacFloatBtn.SetEnabled(const Value: boolean);
begin
  if FEnabled <> Value then begin
    FEnabled := Value;
    Repaint;
  end;
end;


procedure TacFloatBtn.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
end;


procedure TacFloatBtn.SetInteger(const Index, Value: integer);

  procedure ChangeProp(var Prop: integer; Value: integer);
  begin
    if Prop <> Value then begin
      Prop := Value;
      UpdatePosition;
    end;
  end;

begin
  case Index of
    0: ChangeProp(FLeft, Value);
    1: ChangeProp(FTop, Value);
    2: ChangeProp(FWidth, Value);
    3: ChangeProp(FHeight, Value);
    4: ChangeProp(FOffsetX, Value);
    5: ChangeProp(FOffsetY, Value);
    6: ChangeProp(FImageIndex, Value);
    7: ChangeProp(FImageIndexHot, Value);
    8: ChangeProp(FImageIndexPressed, Value);
    9: ChangeProp(FMargin, Value);
  end;
end;


procedure TacFloatBtn.SetLayout(const Value: TButtonLayout);
begin
  if FLayout <> Value then begin
    FLayout := Value;
    UpdatePosition;
  end;
end;


procedure TacFloatBtn.SetName(const Value: string);
begin
  if FName <> Value then
    FName := Value;
end;


procedure TacFloatBtn.SetStyle(const Value: TacFloatBtnStyle);
begin
  if FStyle <> Value then begin
    FStyle := Value;
    UpdatePosition;
  end;
end;


function TacFloatBtn.CalcBounds: TRect;
var
  CurBounds: TRect;
begin
  AdjustSize;
  GetWindowRect(FOwner.Form.Handle, CurBounds);
  case AlignHorz of
    taLeftJustify:  Result.Left := CurBounds.Left  + OffsetX;
    taRightJustify: Result.Left := CurBounds.Right + OffsetX - Width;
    taCenter:       Result.Left := CurBounds.Left  + OffsetX + (WidthOf(CurBounds) - Width) div 2;
  end;
  case AlignVert of
    vaAlignTop:       Result.Top := CurBounds.Top    + OffsetY;
    vaAlignBottom:    Result.Top := CurBounds.Bottom + OffsetY - Height;
    vaVerticalCenter: Result.Top := CurBounds.Top    + OffsetY + (HeightOf(CurBounds) - Height) div 2;
    vaFormTitleCenter:
      if (FOwner.SP <> nil) and (FOwner.SP.BorderForm <> nil) then
        Result.Top := FOwner.SP.Form.Top - FOwner.SP.BorderForm.OffsetY + FOwner.SP.OffsetY - (SkinTitleHeight(FOwner.SP.BorderForm) + Height) div 2 + FOwner.SP.SkinData.SkinManager.CommonSkinData.ExCenterOffs + OffsetY
      else
        Result.Top := CurBounds.Top + (SysBorderWidth(FOwner.Form.Handle, nil, False) + SysCaptHeight(TForm(FOwner.Form)) - Height) div 2 + OffsetY;
  end;
  Result.Bottom := Height;
  Result.Right := Width;
end;


procedure TacFloatBtn.UpdatePosition;
var
  R: TRect;
  wnd: HWND;
begin
  if not (csLoading in FOwner.ComponentState) and not FOwner.Updating and not FOwner.Hiding and (IsWindowVisible(FOwner.Form.Handle) or InAnimationProcess) then begin
    MakeForm;
    if AForm <> nil then begin
      R := CalcBounds;
      wnd := GetNextWindow(FOwner.Form.Handle, GW_HWNDPREV);
      SetWindowLong(AForm.Handle, GWL_EXSTYLE, GetWindowLong(AForm.Handle, GWL_EXSTYLE) or WS_EX_LAYERED or WS_EX_NOACTIVATE);
      Repaint;
      SetWindowPos(AForm.Handle, wnd, R.Left, R.Top, R.Right, R.Bottom, SWP_NOACTIVATE or SWP_NOREDRAW or SWP_SHOWWINDOW or SWP_NOSENDCHANGING or SWP_NOOWNERZORDER);
      AForm.SetBounds(R.Left, R.Top, R.Right, R.Bottom);
      AForm.Visible := True;
    end;
  end;
end;


procedure TacFloatBtn.UpdateScale(const Value, OldValue: integer);
begin
  FFont.Height := MulDiv(FFont.Height, Value, OldValue);
  Height       := MulDiv(Height,       Value, OldValue);
  Width        := MulDiv(Width,        Value, OldValue);
  Left         := MulDiv(Left,         Value, OldValue);
  Top          := MulDiv(Top,          Value, OldValue);
  OffsetX      := MulDiv(OffsetX,      Value, OldValue);
  OffsetY      := MulDiv(OffsetY,      Value, OldValue);
  Margin       := MulDiv(Margin,       Value, OldValue);
end;


constructor TacFloatBtnsList.Create(AOwner: TsFloatButtons);
begin
  inherited Create(TacFloatBtn);
  FOwner := AOwner;
end;


destructor TacFloatBtnsList.Destroy;
begin
  FOwner := nil;
  inherited;
end;


function TacFloatBtnsList.GetItem(Index: Integer): TacFloatBtn;
begin
  Result := TacFloatBtn(inherited GetItem(Index))
end;


function TacFloatBtnsList.GetOwner: TPersistent;
begin
  Result := FOwner;
end;


procedure TacFloatBtnsList.SetItem(Index: Integer; Value: TacFloatBtn);
begin
  inherited SetItem(Index, Value);
end;


procedure TsFloatButtons.UpdateBitmaps;
var
  i: integer;
begin
  if not (csLoading in ComponentState) then
    for i := 0 to Items.Count - 1 do
      Items[i].Repaint;
end;


procedure TsFloatButtons.UpdatePositions;
var
  i: integer;
begin
  if not InAnimationProcess then begin
    SP := TsSkinProvider(Form.Perform(SM_ALPHACMD, AC_GETPROVIDER_HI, 0));
    if not (csLoading in ComponentState) then
      for i := 0 to Items.Count - 1 do
        Items[i].UpdatePosition;
  end;
end;


procedure TsFloatButtons.ChangedPosition;
begin
  UpdatePositions;
end;


procedure TsFloatButtons.CopyItems(Bmp: TBitmap);
var
  p: TPoint;
  i: integer;
  RBounds, RDst: TRect;

  S0, S: PRGBAArray_S;
  D0, D: PRGBAArray_D;
  X1, Y1, X2, Y2, x, y, DeltaS, DeltaD: integer;
begin
  p := Point(Form.Left, Form.Top);
  if (SP <> nil) and (SP.BorderForm <> nil) then begin
    dec(p.X, SP.BorderForm.OffsetX);
    dec(p.Y, SP.BorderForm.OffsetY);
  end;
  for i := Items.Count - 1 downto 0 do
    if Items[i].Visible then begin
      RBounds := Items[i].CalcBounds;
      if Items[i].AlphaBmp = nil then
        Items[i].CreateAlphaBmp;

      RDst.Left := RBounds.Left - p.x;
      RDst.Top  := RBounds.Top  - p.y;
      RDst.Right := RDst.Left + RBounds.Right;
      RDst.Bottom := RDst.Top + RBounds.Bottom;

      Y1 := 0;
      Y2 := Items[i].AlphaBmp.Height - 1;
      if RDst.Top < 0 then begin
        Y1 := -RDst.Top;
        Y2 := Y2 + Y1;
        if Y2 >= Items[i].AlphaBmp.Height then
          Y2 := Items[i].AlphaBmp.Height - 1;
      end;
      if RDst.Bottom >= Bmp.Height then
        Y2 := Y2 - (RDst.Bottom - Bmp.Height + 1);

      X1 := 0;
      X2 := Items[i].AlphaBmp.Width - 1;
      if RDst.Left < 0 then begin
        X1 := -RDst.Left;
        X2 := X2 + X1;
        if X2 >= Items[i].AlphaBmp.Width then
          X2 := Items[i].AlphaBmp.Width - 1;
      end;
      if RDst.Right >= Bmp.Width then
        X2 := X2 - (RDst.Right - Bmp.Width + 1);

      if InitLine(Items[i].AlphaBmp, Pointer(S0), DeltaS) and InitLine(Bmp, Pointer(D0), DeltaD) then
        for Y := Y1 to Y2 do begin
          S := Pointer(PAnsiChar(S0) + DeltaS * Y);
          D := Pointer(PAnsiChar(D0) + DeltaD * (RDst.Top + Y));
          for X := X1 to X2 do
            with S[X], D[X + RDst.Left] do
              if DA = MaxByte then begin // Back convert
                DR := min(MaxByte, (SR shl 8 - DR * SA + DR shl 8) shr 8);
                DG := min(MaxByte, (SG shl 8 - DG * SA + DG shl 8) shr 8);
                DB := min(MaxByte, (SB shl 8 - DB * SA + DB shl 8) shr 8);
              end
              else begin
                DR := ((SR * SA shr 8 - DR) * SA + DR shl 8) shr 8;
                DG := ((SG * SA shr 8 - DG) * SA + DG shl 8) shr 8;
                DB := ((SB * SA shr 8 - DB) * SA + DB shl 8) shr 8;
              end;
        end;
    end;
end;


constructor TsFloatButtons.Create(AOwner: TComponent);
begin
  ScalePercent := 100;
  inherited;
  FItems := TacFloatBtnsList.Create(Self);
end;


destructor TsFloatButtons.Destroy;
begin
  FItems.Free;
  inherited;
end;


function TsFloatButtons.GetTopWnd: HWND;
begin
  if (Items.Count > 0) and (Items[0].AForm <> nil) then
    Result := Items[0].AForm.Handle
  else
    Result := HWND_TOP;
end;


procedure TsFloatButtons.Invalidate;
begin
  RepaintItems;
end;


procedure TsFloatButtons.KillTimers;
var
  i: integer;
begin
  if FHintTimer <> nil then begin
    FHintTimer.Enabled := False;
    FHintTimer.OnTimer := nil;
    FreeAndNil(FHintTimer);
  end;
  for i := 0 to Items.Count - 1 do
    Items[i].KillTimer;
end;


procedure TsFloatButtons.HideItems;
var
  i: integer;
begin
  Hiding := True;
  for i := 0 to Items.Count - 1 do
    if Items[i].AForm <> nil then begin
      Items[i].AForm.Visible := False;
      FreeAndNil(Items[i].AForm);
    end;

  Hiding := False;
end;


procedure TsFloatButtons.Loaded;
begin
  inherited;
end;


procedure TsFloatButtons.RepaintItems;
var
  i: integer;
  Item: TacFloatBtn;
begin
  if ([csDestroying, csLoading] * ComponentState = []) and IsWindowVisible(Form.Handle) and not (csDestroying in Form.ComponentState) then begin
    SP := TsSkinProvider(Form.Perform(SM_ALPHACMD, AC_GETPROVIDER_HI, 0));
    for i := 0 to Items.Count - 1 do begin
      Item := Items[i];
      Item.Repaint;
    end;
  end;
end;


procedure TsFloatButtons.SetAlpha(AlphaValue: byte);
var
  i: integer;
begin
  for i := 0 to Items.Count - 1 do
    Items[i].BlendValue := AlphaValue;
end;


procedure TsFloatButtons.SetItems(const Value: TacFloatBtnsList);
begin
  FItems.Assign(Value);
end;


procedure TsFloatButtons.SetScale(Value: integer);
var
  i: integer;
begin
  if (SP <> nil) and (SP.SkinData.SkinManager <> nil) then begin
    BeginUpdate;
    for i := 0 to Items.Count - 1 do
      Items[i].UpdateScale(Value, ScalePercent);

    ScalePercent := Value;
    EndUpdate;
  end;
end;


procedure TsFloatButtons.SetZOrder(TopWnd: Integer);
var
  i: integer;
begin
  for i := 0 to Items.Count - 1 do
    if Items[i].AForm <> nil then
      if TopWnd = 0 then
        SetWindowPos(Items[i].AForm.Handle, Form.Handle, 0, 0, 0, 0, SWPA_SHOWZORDERONLY)
      else
        SetWindowPos(Items[i].AForm.Handle, HWND_TOPMOST, 0, 0, 0, 0, SWPA_SHOWZORDERONLY);
end;

end.
