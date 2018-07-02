unit sFrameBar;
{$I sDefs.inc}
//{$DEFINE LOGGED}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, ImgList, Menus, comctrls,
  sSpeedButton, sScrollBox, sConst, acThdTimer;


type
{$IFNDEF NOTFORHELP}
  TsTitleItem = class;
  TsTitles = class;
  TsTitleState = (stClosed, stOpened, stClosing, stOpening);
  TOnFrameChangeEvent   = procedure (Sender: TObject;    TitleItem: TsTitleItem) of object;
  TOnFrameClosingEvent  = procedure (Sender: TObject;    TitleItem: TsTitleItem; var CanClose:  boolean) of object;
  TOnFrameChangingEvent = procedure (Sender: TObject; NewTitleItem: TsTitleItem; var CanChange: boolean) of object;
  TOnChangedStateEvent  = procedure (Sender: TObject;    TitleItem: TsTitleItem; State: TsTitleState)    of object;
  TOnItemMovedEvent     = procedure (Sender: TObject;    TitleItem: TsTitleItem; OldIndex : Integer; NewIndex : Integer)  of object;
{$ENDIF} // NOTFORHELP


{$IFDEF DELPHI_XE3}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]{$ENDIF}
  TsFrameBar = class(TsScrollBox)
{$IFNDEF NOTFORHELP}
  private
    FAnimation,
    FAllowAllOpen,
    FAllowAllClose,
    FAutoFrameSize: boolean;

    FSpacing,
    FTitleHeight,
    FBorderWidth,
    FActiveFrameIndex: integer;

    FItems:      TsTitles;
    FImages:     TCustomImageList;
    FOnItemMoved:TOnItemMovedEvent;
    FOnChange:   TOnFrameChangeEvent;
    FOnClosing:  TOnFrameClosingEvent;
    FOnChanging: TOnFrameChangingEvent;

    FTitleFont: TFont;
    FDragItems: boolean;
    FAnimationSteps: byte;
    DragTimer: TacThreadedTimer;
    FOnChangedState: TOnChangedStateEvent;
    procedure SetTitleFont(Value: TFont);
	  function GetActiveFrame: TCustomFrame;
    procedure ChangedTitleFontNotify(Sender: TObject);
    procedure CMFontChanged      (var Message: TMessage); message CM_FONTCHANGED;
    procedure CMParentFontChanged(var Message: TMessage); message CM_PARENTFONTCHANGED;

    function Offset: integer;
    procedure UpdateWidths;
    function CalcClientRect: TRect;
    function GetAutoHeight: integer;
    function StoredTitleFont: boolean;
    procedure InitDrag(Value: boolean);
    function CreateDefaultFrame: TFrame;
    procedure DoDragTimer(Sender: TObject);
    procedure MoveItem(Index, NewIndex: integer);
    procedure SetDragItems    (const Value: boolean);
    procedure SetAutoFrameSize(const Value: boolean);
    procedure SetAllowAllOpen (const Value: boolean);
    procedure SetItems        (const Value: TsTitles);
    procedure SetInteger      (const Index, Value: integer);
    procedure SetImages       (const Value: TCustomImageList);
    procedure DoDragDrop(Sender, Source: TObject; X, Y: Integer);
    function UpdateFrame(i, y: integer; var h, w: integer): boolean;
    procedure DoDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
  public
    Sizing,
    Arranging: boolean;
    procedure Loaded; override;
    destructor Destroy; override;
    procedure ChangeScale(M, D: Integer); override;
    constructor Create(AOwner: TComponent); override;
    procedure WndProc(var Message: TMessage); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
{$ENDIF} // NOTFORHELP
    procedure Rearrange;
    procedure ArrangeTitles;
    procedure ExpandAll  (AllowAnimation: boolean);
    procedure CollapseAll(AllowAnimation: boolean);
    procedure UpdatePositions(DoRepaint: boolean = True); // Fast update
    procedure OpenItem  (Index: integer; AllowAnimation: boolean);
    procedure CloseItem (Index: integer; AllowAnimation: boolean);
    procedure ChangeSize(Index: integer; AllowAnimation: boolean; Height: integer);
    property DragItems: boolean read FDragItems write SetDragItems default False;
  published
	  property ActiveFrame: TCustomFrame read GetActiveFrame;
    property AnimationSteps: byte read FAnimationSteps write FAnimationSteps default acMaxIterations;
    // TitleFont works only if skinData.CustomFont = true;
    property TitleFont: TFont read fTitleFont write SetTitleFont stored StoredTitleFont;
    {:@event}
    property OnChangedState: TOnChangedStateEvent read fOnChangedState write fOnChangedState;
{$IFNDEF NOTFORHELP}
    property Align default alLeft;
    property BorderStyle;
    property BorderWidth:      integer index 0 read FBorderWidth      write SetInteger default 2;
{$ENDIF} // NOTFORHELP
    property ActiveFrameIndex: integer index 1 read FActiveFrameIndex write SetInteger;
    property TitleHeight:      integer index 2 read FTitleHeight      write SetInteger default 28;
    property Spacing:          integer index 3 read FSpacing          write SetInteger default 2;

    property Animation:     boolean read FAnimation     write FAnimation       default True;
    property AllowAllClose: boolean read FAllowAllClose write FAllowAllClose   default False;
    property AllowAllOpen:  boolean read FAllowAllOpen  write SetAllowAllOpen  default False;
    property AutoFrameSize: boolean read FAutoFrameSize write SetAutoFrameSize default False;

    property Images: TCustomImageList read FImages write SetImages;
    property Items: TsTitles read FItems write SetItems;
    {:@event}
    property OnChange:   TOnFrameChangeEvent   read FOnChange   write FOnChange;
    {:@event}
    property OnChanging: TOnFrameChangingEvent read FOnChanging write FOnChanging;
    {:@event}
    property OnClosing:  TOnFrameClosingEvent  read FOnClosing  write FOnClosing;
    {:@event}
    property OnItemMoved:   TOnItemMovedEvent  read FOnItemMoved write FOnItemMoved;
  end;


{$IFNDEF NOTFORHELP}
  TsTitles = class(TCollection)
  private
    FOwner: TsFrameBar;
  protected
    function GetOwner: TPersistent; override;
    function  GetItem(Index: Integer): TsTitleItem;
    procedure Update(Item: TCollectionItem); override;
    procedure SetItem(Index: Integer; Value: TsTitleItem);
  public
    destructor Destroy; override;
    constructor Create(AOwner: TsFrameBar);
    property Items[Index: Integer]: TsTitleItem read GetItem write SetItem; default;
  end;


  TsTitleButton = class(TsSpeedButton)
  private
    procedure SetCloseBtnState(const Value: integer);
    procedure SetClosePressed(const Value: boolean);
  protected
    Active,
    FClosePressed: boolean;

    Alpha,
    TempState,
    FCloseBtnState: integer;

    ArrowAngle: real;
    SavedBmp: TBitmap;
    FOwner: TsFrameBar;
    function CloseBtnRect: TRect;
    function CloseBtnIndex: integer;
    procedure Ac_CMMouseEnter; override;
    procedure Ac_CMMouseLeave; override;
  public
    TitleItem: TsTitleItem;
    constructor InternalCreate(AOwner: TsFrameBar; Index: integer);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure WndProc(var Message: TMessage); override;
    function PrepareCache: boolean; override;
    procedure StdPaint(PaintButton: boolean = True); override;
    function CurrentState: integer; override;
    property TextAlignment default taLeftJustify;
    property CloseBtnState: integer read FCloseBtnState write SetCloseBtnState;
    property ClosePressed: boolean read FClosePressed write SetClosePressed;
    property OnClick;
  end;

  TacBarItemDrawData = record
    ARect: TRect;
    ACanvas: TCanvas;
  end;
  TItemCloseAction = (icaNone, icaHide, icaFree);

  TCreateFrameEvent   = procedure (Sender: TObject; var Frame: TCustomFrame) of object;
  TFrameDestroyEvent  = procedure (Sender: TObject; var Frame: TCustomFrame; var CanDestroy: boolean) of object;
  TCloseBtnClickEvent = procedure (Sender: TObject; var CloseAction: TItemCloseAction) of object;
  TDrawBarItemEvent   = procedure (Item: TsTitleItem; ADrawData: TacBarItemDrawData) of object;
{$ENDIF} // NOTFORHELP


  TsTitleItem = class(TCollectionItem)
{$IFNDEF NOTFORHELP}
  private
    FTag: Longint;
    FCursor: TCursor;
    FOwner: TsTitles;
    FCaption: acString;

    FVisible,
    FShowArrow: boolean;

    FGroupIndex,
    FImageIndex: integer;

    FOnClick,
    FOnFrameShow,
    FOnFrameClose,
    FOnTitleItemDestroy: TNotifyEvent;

    FOnDrawItem:         TDrawBarItemEvent;
    FOnCreateFrame:      TCreateFrameEvent;
    FOnFrameDestroy:     TFrameDestroyEvent;
    FOnCloseBtnClick:     TCloseBtnClickEvent;
    FShowCloseBtn: boolean;
    function GetPopupMenu: TPopupMenu;
    function GetSkinSection: string;
    procedure TitleButtonClick(Sender: TObject);
    procedure SetCaption      (const Value: acString);
    procedure SetSkinSection  (const Value: string);
    procedure SetPopupMenu    (const Value: TPopupMenu);
    procedure SetCursor       (const Value: TCursor);
    procedure SetInteger      (const Index, Value: integer);
    function GetInteger       (const Index: Integer): integer;
    function GetAlignment     (const Index: Integer): TAlignment;
    procedure SetAlignment    (const Index: Integer; const Value: TAlignment);
    procedure SetBoolean      (const Index: Integer; const Value: boolean);
  protected
    TempHeight: real;
    LastBottom: integer;
    procedure SetIndex(Value: Integer); override;
  public
{$ENDIF} // NOTFORHELP
    TitleButton: TsTitleButton;
    Frame: TCustomFrame;
    State: TsTitleState;
{$IFNDEF NOTFORHELP}
    FrameSize: integer;
    Closing: boolean;
    Obj: TObject;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    constructor Create(Collection: TCollection); override;
  published
    property DisplayName;
{$ENDIF} // NOTFORHELP

    property GroupIndex: integer read FGroupIndex write FGroupIndex   default 0;

    property Margin:     integer index 0 read GetInteger write SetInteger default 5;
    property Spacing:    integer index 1 read GetInteger write SetInteger default 8;
    property ImageIndex: integer index 2 read GetInteger write SetInteger default -1;

    property ShowArrow:    boolean index 0 read FShowArrow      write SetBoolean default False;
    property Visible:      boolean index 1 read FVisible        write SetBoolean default True;
    property ShowCloseBtn: boolean index 2 read FShowCloseBtn   write SetBoolean default False;

    property Alignment:     TAlignment index 0 read GetAlignment write SetAlignment default taCenter;
    property TextAlignment: TAlignment index 1 read GetAlignment write SetAlignment;

    property Caption:       acString   read FCaption         write SetCaption;
    property Cursor:        TCursor    read FCursor          write SetCursor default crDefault;
    property SkinSection:   string     read GetSkinSection   write SetSkinSection;
    property Tag:           Longint    read FTag             write FTag default 0;
    property PopupMenu:     TPopupMenu read GetPopupMenu     write SetPopupMenu;

    {:@event}
    property OnCreateFrame:   TCreateFrameEvent   read FOnCreateFrame      write FOnCreateFrame;
    {:@event}
    property OnFrameDestroy:  TFrameDestroyEvent  read FOnFrameDestroy     write FOnFrameDestroy;
    {:@event}
    property OnDrawItem:      TDrawBarItemEvent   read FOnDrawItem         write FOnDrawItem;
    {:@event}
    property OnClick:         TNotifyEvent        read FOnClick            write FOnClick;
    {:@event}
    property OnFrameShow:     TNotifyEvent        read FOnFrameShow        write FOnFrameShow;
    {:@event}
    property OnFrameClose:    TNotifyEvent        read FOnFrameClose       write FOnFrameClose;
    {:@event}
    property OnDestroy:       TNotifyEvent        read FOnTitleItemDestroy write FOnTitleItemDestroy;

    property OnCloseBtnClick: TCloseBtnClickEvent read FOnCloseBtnClick    write FOnCloseBtnClick;
  end;


implementation

uses
  stdctrls,
  {$IFNDEF DELPHI5}   Types,   {$ENDIF}
  {$IFDEF DELPHI_XE2} UITypes, {$ENDIF}
  {$IFDEF LOGGED} sDebugMsgs, {$ENDIF}
  sMessages, sSkinProps, sVCLUtils, sFrameAdapter, sLabel, acntUtils, acSBUtils, sGraphUtils, sCommonData, sFade, sAlphaGraph,
  sSkinManager, sStyleSimply, acntTypes;


const
  Speed = 3;
  aOpenStates:  array [boolean] of TsTitleState = (stOpened, stOpening);
  aCloseStates: array [boolean] of TsTitleState = (stClosed, stClosing);

var
  bDontAnim:  boolean = False;
  bCanChange: boolean = False;


constructor TsTitles.Create(AOwner: TsFrameBar);
begin
  inherited Create(TsTitleItem);
  FOwner := AOwner;
end;


destructor TsTitles.Destroy;
begin
  inherited Destroy;
  FOwner := nil;
end;


function TsTitles.GetItem(Index: Integer): TsTitleItem;
begin
  Result := TsTitleItem(inherited GetItem(Index));
end;


function TsTitles.GetOwner: TPersistent;
begin
  Result := FOwner;
end;


procedure TsTitles.SetItem(Index: Integer; Value: TsTitleItem);
begin
  inherited SetItem(Index, Value);
  FOwner.UpdatePositions;
end;


procedure TsTitles.Update(Item: TCollectionItem);
begin
  inherited;
  if (UpdateCount = 0) and FOwner.HandleAllocated then
    FOwner.UpdatePositions;
end;


procedure TsFrameBar.ArrangeTitles;
var
  Steps, sDiv, i, ii, sHeight, cWidth, AutoHeight: integer;
  b, CanDestroy, DoAnimation: boolean;
  CtrlArray: TacCtrlArray;
  aDelta, rDelta: real;
  ti: TsTitleItem;
  lTicks: DWord;
  cRect: TRect;

  procedure SetActive(Index: integer; Active: boolean);
  begin
    with Items[Index] do
      if (TitleButton.Active <> Active) and (State in [stClosed, stOpened]) then begin
        TitleButton.Active := Active;
        TitleButton.SkinData.Invalidate;
      end;
  end;

  procedure LockControl(Locked: boolean);
  begin
    Perform(WM_SETREDRAW, integer(not Locked), 0);
    if Locked then
      SkinData.BeginUpdate
    else
      SkinData.EndUpdate;
  end;

  procedure DoOpened(ti: TsTitleItem);
  var
    j: integer;
  begin
    ti.TitleButton.ArrowAngle := i90;
    ti.TitleButton.SkinData.BGChanged := True;
    FActiveFrameIndex := i;
    if AutoHeight >= 0 then begin
      sDiv := AutoHeight;
      ti.FrameSize := AutoHeight;
      if ti.Frame <> nil then
        ti.Frame.Height := AutoHeight
    end;
    j := -1;
    UpdateFrame(i, sHeight - Offset, j, cWidth);
    if (sDiv = 0) and (ti.Frame <> nil) then
      sDiv := ti.Frame.Height;
  end;

  procedure DoClosed(ti: TsTitleItem);
  begin
    ti.TitleButton.ArrowAngle := iff(BiDiMode = bdLeftToRight, 0, i180);
    ti.TitleButton.SkinData.BGChanged := True;
    if FActiveFrameIndex = i then
      FActiveFrameIndex := -1;

    if ti.Frame <> nil then begin
      CanDestroy := True;
      if Assigned(ti.FOnFrameDestroy) then
        ti.FOnFrameDestroy(Items[i], Items[i].Frame, CanDestroy);

      if CanDestroy then
        FreeAndNil(ti.Frame)
      else
        ti.Frame.Visible := False;

      ti.FrameSize := 0;
      sDiv := 0;
      if ti.Frame <> nil then
        UpdateFrame(i, sHeight - Offset, sDiv, cWidth);

      ti.FrameSize := 0;
    end;
  end;

  procedure DoOpening(ti: TsTitleItem);
  begin
//    if BiDiMode = bdLeftToRight then
    ti.TitleButton.ArrowAngle := ti.TitleButton.ArrowAngle + acMinusPlus[BiDiMode = bdLeftToRight] * aDelta;
    if ii = Steps then begin
      if ti.Frame <> nil then
        if AutoHeight <> -1 then begin
          sDiv := AutoHeight;
          ti.FrameSize := AutoHeight;
          if ti.Frame <> nil then
            ti.Frame.Height := AutoHeight
        end
        else
          sDiv := ti.Frame.Height
    end
    else
      if Steps <> 0 then begin
        if ti.Frame <> nil then
          ti.TempHeight := ti.Frame.Height - (ti.Frame.Height - ti.TempHeight) / Speed
        else
          ti.TempHeight := 0;

        sDiv := Round(ti.TempHeight);
      end;

    if (ti.Frame <> nil) and (Steps <> 0) then
      ti.TitleButton.Alpha := (Steps - ii) * MaxByte div Steps
    else
      ti.TitleButton.Alpha := MaxByte;

    ti.TitleButton.SkinData.BGChanged := True;
    lTicks := GetTickCount;
    if UpdateFrame(i, sHeight - Offset, sDiv, cWidth) then begin
      if (ii = Steps) or (ti.Frame <> nil) and (ti.TempHeight = ti.Frame.Height) then
        ti.State := stOpened
      else
        if Steps > 0 then
          WaitTicks(lTicks);
    end;
  end;

  procedure DoClosing(ti: TsTitleItem);
  var
    j: integer;
  begin
    ti.TitleButton.ArrowAngle := ti.TitleButton.ArrowAngle - acMinusPlus[BiDiMode = bdLeftToRight] * aDelta;
    if not ti.TitleButton.SkinData.CustomFont and SkinData.CustomFont then
      ti.TitleButton.SkinData.CustomFont := true;

    ti.TitleButton.Alpha := ii;
    ti.TitleButton.SkinData.BGChanged := True;
    lTicks := GetTickCount;
    if Assigned(FOnClosing) then begin
      b := True;
      FOnClosing(Self, ti, b);
      if not b then begin
        ti.State := stOpened;
        if AutoHeight <> -1 then begin
          ti.FrameSize := AutoHeight;
          if ti.Frame <> nil then
            ti.Frame.Height := AutoHeight
        end;
        j := -1;
        UpdateFrame(i, sHeight - Offset, j, cWidth);
        inc(sHeight, FSpacing);
        Exit;
      end;
    end;
    if Steps = 0 then
      sDiv := 0
    else begin
      rDelta := (ti.Frame.Height - ti.TempHeight) / Speed;
      ti.TempHeight := ti.Frame.Height - rDelta;
      sDiv := ti.Frame.Height - Round(ti.TempHeight);
    end;

    if ti.Frame <> nil then
      ti.TitleButton.Alpha := Round((sDiv / ti.Frame.Height) * MaxByte)
    else
      ti.TitleButton.Alpha := MaxByte;

    if (ii = Steps) or (rDelta < 1) then begin
      ti.Closing := False;
      CanDestroy := True;

      if Assigned(ti.FOnFrameDestroy) then
        ti.FOnFrameDestroy(ti, ti.Frame, CanDestroy);

      if CanDestroy then
        FreeAndNil(ti.Frame)
      else
        ti.Frame.Visible := False;

      ti.FrameSize := 0;
      sDiv := 0;
      ti.State := stClosed;
      SetActive(i, False);
      if ti.Frame <> nil then
        UpdateFrame(i, sHeight - Offset, sDiv, cWidth);

      inc(sHeight, FSpacing);
    end
    else begin
      UpdateFrame(i, sHeight - Offset, sDiv, cWidth);
      if Steps > 0 then
        WaitTicks(lTicks);
    end;
  end;

begin
  if not Visible or Arranging or (csReading in ComponentState) or (Items.Count = 0) then
    FActiveFrameIndex := -1
  else
    if Items.UpdateCount <= 0 then begin
      DoAnimation := FAnimation and ((SkinData.SkinManager = nil) or SkinData.SkinManager.Effects.AllowAnimation) and (FAnimationSteps > 0);
      if not bDontAnim and DoAnimation and ([csDesigning, csLoading] * ComponentState = []) and Visible then begin
        Steps := FAnimationSteps - 1;
        aDelta := i90 / (Steps + 1);
      end
      else begin
        Steps := 0;
        aDelta := i90;
      end;

      cRect := CalcClientRect;
      Arranging := True;
      sHeight := 0;
      AutoHeight := -1;
      bCanChange := True;
      if not ShowHintStored then begin
        AppShowHint := Application.ShowHint;
        Application.ShowHint := False;
        ShowHintStored := True;
      end;

      FadingForbidden := True;
      if AutoFrameSize then begin
        AutoScroll := False;
        AutoHeight := GetAutoHeight;
      end;

      for i := 0 to Items.Count - 1 do
        if Items[i].TitleButton <> nil then
          with Items[i] do begin
            LastBottom := TitleButton.Top + TitleButton.Height;
            TitleButton.SkinData.FMouseAbove := False;
            StopTimer(TitleButton.SkinData);
          end;

      SetLength(CtrlArray, 0);
      SaveGraphCtrls(Self, CtrlArray);
      for ii := 0 to Steps do begin
        LockControl(True);
        sHeight := cRect.Top;
        cWidth := WidthOf(cRect);
        for i := 0 to Items.Count - 1 do begin
          ti := Items[i];
          if ti.Visible and (ti.TitleButton <> nil) and ti.TitleButton.Visible then begin
            ti.TitleButton.SetBounds(cRect.Left, sHeight - Offset, cWidth, FTitleHeight);
            if ti.TitleButton.Parent <> Self then
              ti.TitleButton.Parent := Self;

            inc(sHeight, FTitleHeight);
            if ii = 0 then begin
              ti.TempHeight := 0;
              if ti.Frame <> nil then
                ti.FrameSize := ti.Frame.Height;
            end;
            sDiv := ti.FrameSize;
            if (sDiv = 0) and (ti.State = stOpening) and not DoAnimation then
              ti.State := stOpened;

            case ti.State of
              stOpening: DoOpening(ti);
              stClosing: DoClosing(ti);
              stOpened:  DoOpened (ti);
              stClosed:  DoClosed (ti);
            end;
            inc(sHeight, FSpacing);
            if ti.Frame <> nil then begin
              if ti.State in [stOpened, stOpening, stClosing] then begin
                if ti.Frame.Parent = nil then
                  ti.Frame.Parent := Self;

                inc(sHeight, sDiv);
              end;
              if ti.State = stOpened then
                SetWindowRgn(ti.Frame.Handle, 0, False);
            end
            else
              if ti.State = stOpening then begin
                ti.State := stClosed;
                ti.TitleButton.SkinData.FMouseAbove := True;
              end;

            inc(sHeight, BorderWidth);
            SetActive(i, ti.State in [stOpened, stOpening]);
          end;
        end;
        LockControl(False);
        if Showing then begin
          InvalidateRect(Handle, nil, True);
          RedrawWindow(Handle, nil, 0, RDWA_ALLNOW);
          SetParentUpdated(Self);
        end;
        if Assigned(acMagnForm) then
          acMagnForm.Perform(SM_ALPHACMD, AC_REFRESH_HI, 0);
      end;
      RestoreGraphCtrls(Self, CtrlArray);
      bCanChange := False;
      FadingForbidden := False;
      inc(sHeight, BorderWidth + 2 * integer(BorderStyle = bsSingle));
      if VertScrollBar.Range <> sHeight then
        VertScrollBar.Range := sHeight;

      Arranging := False;
      UpdateWidths;
      if Showing then
        RedrawWindow(Handle, nil, 0, RDWA_ALL);

      if Assigned(acMagnForm) then
        SendMessage(acMagnForm.Handle, SM_ALPHACMD, AC_REFRESH_HI, 0);

      Application.ShowHint := AppShowHint;
      ShowHintStored := False;
    end;
end;


function TsFrameBar.GetAutoHeight: integer;
var
  cRect: TRect;
  i, iHeight: integer;
begin
  cRect := CalcClientRect;
  iHeight := cRect.Top;
  for i := 0 to Items.Count - 1 do
    if Items[i].Visible and (Items[i].TitleButton <> nil) and Items[i].TitleButton.Visible then begin
      inc(iHeight, FTitleHeight);
      if (Items[i].State in [stOpened, stOpening]) then
        inc(iHeight, BorderWidth);

      inc(iHeight, BorderWidth);
    end;

  Result := maxi(0, HeightOf(cRect) - iHeight);
end;


const
  DragModes: array [boolean] of TDragMode = (dmManual, dmAutomatic);

procedure TsFrameBar.InitDrag(Value: boolean);
var
  i: integer;
begin
  if Value then begin
    OnDragOver := DoDragOver;
    CollapseAll(False);
  end
  else
    OnDragOver := nil;

  for i := 0 to Items.Count - 1 do begin
    Items[i].TitleButton.DragMode := DragModes[Value];
    if Value then
      Items[i].TitleButton.OnDragOver := DoDragOver
    else
      Items[i].TitleButton.OnDragOver := nil;
  end;
end;


function TsFrameBar.CalcClientRect: TRect;
begin
  if HandleAllocated then
    Windows.GetClientRect(Handle, Result)
  else
    Result := MkRect(Self);

  InflateRect(Result, -BorderWidth, -BorderWidth);
end;


procedure TsFrameBar.ChangeScale(M, D: Integer);
begin
  inherited;
  if M <> D then
    TitleHeight := MulDiv(TitleHeight, M, D);
end;


procedure TsFrameBar.ChangeSize(Index: integer; AllowAnimation: boolean; Height: integer);
begin
  with Items[Index] do begin
    if Assigned(Frame) then begin
      FrameSize := Height;
      Frame.Height := Height;
    end;
    FrameSize := Height;
    State := aOpenStates[AllowAnimation];
  end;
  bDontAnim := not AllowAnimation;
  ArrangeTitles;
  bDontAnim := False;
end;


procedure TsFrameBar.CloseItem(Index: integer; AllowAnimation: boolean);
begin
  with Items[Index] do begin
    State := aCloseStates[AllowAnimation];
    bDontAnim := not AllowAnimation;
    ArrangeTitles;
    bDontAnim := False;
    if Assigned(FOnFrameClose) then
      FOnFrameClose(Items[Index]);
  end;
end;


procedure TsFrameBar.CollapseAll(AllowAnimation: boolean);
var
  i: integer;
  b: boolean;
begin
  for i := 0 to Items.Count - 1 do
    Items[i].State := aCloseStates[AllowAnimation];

  if AllowAnimation then
    ArrangeTitles
  else begin
    b := FAnimation;
    FAnimation := False;
    ArrangeTitles;
    FAnimation := b;
  end;
end;


constructor TsFrameBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SkinData.COC := COC_TsFrameBar;
  FItems := TsTitles.Create(Self);
  Caption := s_Space;
  Align := alLeft;
  BevelOuter := bvLowered;
  FTitleHeight := 28;
  VertScrollBar.Tracking := True;
  HorzScrollBar.Visible := False;
  FBorderWidth := 2;
  FAnimationSteps := acMaxIterations;
  FAnimation := True;
  FAllowAllClose := False;
  FAllowAllOpen := False;
  FActiveFrameIndex := -1;
  FAutoFrameSize := False;
  FDragItems := False;
  DragTimer := nil;
  FTitleFont := TFont.Create;
  FTitleFont.OnChange := ChangedTitleFontNotify;
end;


type
  TAccessCommonData = class(TsCommonData);

function TsFrameBar.CreateDefaultFrame: TFrame;
var
  lbl: TsLabel;
begin
  Result := TFrame.Create(Self);
  Result.Height := 150;
  with TsFrameAdapter.Create(Result), SkinData do begin
    SkinManager := TAccessCommonData(SkinData).FSkinManager;
    SkinSection := s_BarPanel;
  end;
  lbl := TsLabel.Create(Result);
  with lbl do begin
    Align := alClient;
    Caption := 'The OnCreateFrame event ' + s_0D0A + ' has not been handled.';
    Alignment := taCenter;
    Layout := tlCenter;
    WordWrap := True;
    Parent := Result;
  end;
end;


destructor TsFrameBar.Destroy;
begin
  FreeAndNil(FItems);
  FreeAndNil(DragTimer);
  FreeAndNil(FTitleFont);
  inherited Destroy;
end;


procedure TsFrameBar.DoDragDrop(Sender, Source: TObject; X, Y: Integer);

  function GetNewIndex: integer;
  var
    i: integer;
  begin
    for i := 0 to Items.Count - 1 do
      if Y < Items[i].TitleButton.BoundsRect.Bottom then begin
        Result := i;
        Exit;
      end;

    Result := -1;
  end;

begin
  if Source is TsTitleButton then
    if Sender is TsTitleButton then begin
      if (y > 0) and (y < TsTitleButton(Sender).Height) then
        MoveItem(TsTitleButton(Source).TitleItem.Index, TsTitleButton(Sender).TitleItem.Index)
    end
    else
      MoveItem(TsTitleButton(Source).TitleItem.Index, GetNewIndex);
end;


procedure TsFrameBar.DoDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  if Source is TsTitleButton then begin
    Accept := True;

    if DragTimer = nil then begin
      DragTimer := TacThreadedTimer.Create(Self);
      DragTimer.Enabled := False;
    end;
    if not DragTimer.Enabled then begin
      DragTimer.Interval := 100;
      DragTimer.OnTimer := DoDragTimer;
      DragTimer.Enabled := True;
    end;
    if Sender <> Source then
      DoDragDrop(Sender, Source, X, Y);
  end
  else
    Accept := False;
end;


procedure TsFrameBar.DoDragTimer(Sender: TObject);
var
  i: integer;
begin
  if not acMouseInControl(Self) then begin
    DragTimer.Enabled := False;
    for i := 0 to Items.Count - 1 do
      if Items[i].TitleButton.Dragging then begin
        Items[i].TitleButton.SkinData.BGChanged := True;
        Items[i].TitleButton.GraphRepaint;
        Break;
      end;
  end;
end;


procedure TsFrameBar.ExpandAll(AllowAnimation: boolean);
var
  i: integer;
begin
  for i := 0 to Items.Count - 1 do
    Items[i].State := aOpenStates[AllowAnimation];

  ArrangeTitles;
end;


procedure TsFrameBar.Loaded;
var
  i: integer;
begin
  inherited;
  for i := 0 to Items.Count - 1 do begin
    Items[i].TitleButton.SkinData.SkinManager := TAccessCommonData(SkinData).FSkinManager;
    Items[i].TitleButton.Cursor := Items[i].Cursor;
  end;
  if Visible then
    Rearrange;

  InitDrag(DragItems);
end;


procedure TsFrameBar.MoveItem(Index, NewIndex: integer);
begin
  if NewIndex <> Index then begin
    if IsValidIndex(NewIndex, Items.Count) and IsValidIndex(Index, Items.Count) then begin
      SetRedraw(Handle, 0);
      Items.BeginUpdate;
      Items[Index].SetIndex(NewIndex);
      Items.EndUpdate;
      SetRedraw(Handle, 1);
      RedrawWindow(Handle, nil, 0, RDWA_ALLNOW and not RDW_FRAME);

      if (Assigned(FOnItemMoved)) then FOnItemMoved(Self, Items[NewIndex], Index, NewIndex);
    end;
  end;
end;


procedure TsFrameBar.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = Images) then
    Images := nil;
end;


function TsFrameBar.Offset: integer;
var
  sbi: TScrollInfo;
begin
  if SkinData.Skinned then
    if Assigned(ListSW) and (ListSW.sBarVert <> nil) and ListSW.sBarVert.fScrollVisible then
      Result := ListSW.sBarVert.ScrollInfo.nPos
    else
      Result := 0
  else begin
    sbi.cbSize := SizeOf(TScrollInfo);
    sbi.fMask := SIF_POS;
    if GetScrollInfo(Handle, SB_VERT, sbi) then
      Result := sbi.nPos
    else
      Result := 0;
  end;
end;


procedure TsFrameBar.OpenItem(Index: integer; AllowAnimation: boolean);
var
  l, i: integer;
  ClosedItems: array of integer;
begin
  if not IsValidIndex(Index, Items.Count) then
    raise Exception.Create('Open Index Out of Range: ' + IntToStr(Index));

  FActiveFrameIndex := Index;
  Items[Index].State := aOpenStates[AllowAnimation];
  if not AllowAllOpen then begin
    for i := 0 to Items.Count - 1 do
      if (Items[i].State = stOpened) and (Items[i].GroupIndex = Items[Index].GroupIndex) then begin
        Items[i].State := stClosing;
        if (i <> Index) and Assigned(Items[Index].FOnFrameClose) then begin
          l := Length(ClosedItems);
          SetLength(ClosedItems, l + 1);
          ClosedItems[l] := i;
        end;
      end;

    Items[Index].State := stOpened;
  end;
  bDontAnim := not AllowAnimation;
  ArrangeTitles;
  bDontAnim := False;
  if Assigned(Items[Index].FOnFrameShow) then
    Items[Index].FOnFrameShow(Items[Index]);

  for i := 0 to Length(ClosedItems) - 1 do
    if (ClosedItems[i] < Items.Count) and Assigned(Items[Index].FOnFrameClose) then
      Items[ClosedItems[i]].FOnFrameClose(Items[ClosedItems[i]]);
end;


procedure TsFrameBar.Rearrange;
begin
  bDontAnim := True;
  ArrangeTitles;
  bDontAnim := False;
end;


function TsFrameBar.GetActiveFrame: TCustomFrame;
begin
  if ActiveFrameIndex >= 0 then
    Result := Items[ActiveFrameIndex].Frame
  else
    Result := nil;
end;


procedure TsFrameBar.SetAllowAllOpen(const Value: boolean);
begin
  if FAllowAllOpen <> Value then begin
    if Value and FAutoFrameSize then
      FAutoFrameSize := False;

    FAllowAllOpen := Value;
    if not (csLoading in ComponentState) then
      Rearrange;
  end;
end;


procedure TsFrameBar.SetDragItems(const Value: boolean);
begin
  if FDragItems <> Value then begin
    FDragItems := Value;
    InitDrag(Value);
  end;
end;


procedure TsFrameBar.SetAutoFrameSize(const Value: boolean);
begin
  if FAutoFrameSize <> Value then begin
    if Value then begin
      if AllowAllOpen then
        AllowAllOpen := False;

      AutoScroll := False;
    end;
    FAutoFrameSize := Value;
    if not (csLoading in ComponentState) then
      Rearrange;
  end;
end;


procedure TsFrameBar.SetImages(const Value: TCustomImageList);
var
  i: integer;
begin
  if FImages <> Value then begin
    FImages := Value;
    for i := 0 to Items.Count - 1 do
      if Items[i].TitleButton.Visible then
        Items[i].TitleButton.Images := Images;
  end;
end;


procedure TsFrameBar.SetInteger(const Index, Value: integer);

  procedure ChangeProp(var Prop: integer; Value: integer);
  begin
    if Prop <> Value then begin
      Prop := Value;
      if Index = 0 then
        RecreateWnd;

      if not (csLoading in ComponentState) then
        Rearrange;
    end;
  end;

begin
  case Index of
    0: ChangeProp(FBorderWidth, Value);
    2: ChangeProp(FTitleHeight, Value);
    3: ChangeProp(FSpacing, Value);
    1: begin
      if IsValidIndex(Value, Items.Count) then
        OpenItem(Value, True);

      FActiveFrameIndex := Value;
    end;
  end;
end;


procedure TsFrameBar.SetItems(const Value: TsTitles);
begin
  FItems.Assign(Value);
end;


function TsFrameBar.UpdateFrame(i, y: integer; var h, w: integer): boolean;
var
  rgn: hrgn;

  procedure CallOnChange;
  begin
    if Items[i].Frame <> nil then begin
      if bCanChange and Assigned(FOnChange) then
        FOnChange(Self, Items[i]);

      bCanChange := False;
    end;
  end;

begin
  Result := False;
  if Items.Count > i then
    with Items[i] do begin
      if (Frame = nil) and not (csDesigning in ComponentState) then begin
        if Assigned(OnCreateFrame) then
          OnCreateFrame(Items[i], Frame)
        else
          Frame := CreateDefaultFrame;

        CallOnChange;
      end
      else
        if not Frame.Visible and (h <> 0) then begin
          Frame.Visible := True;
          CallOnChange;
        end;

      if (Frame <> nil) and Frame.Visible then begin
        if FrameSize = 0 then
          if AutoFrameSize then
            FrameSize := GetAutoHeight
          else
            FrameSize := Frame.Height;;

        if h = -1 then begin
          h := FrameSize; // if frame has not been created
          Frame.Height := FrameSize;
        end;
        if h < LastBottom - y then
          h := LastBottom - y
        else
          if h = 0 then begin // Frame was created and will be opened now
            TempHeight := FrameSize - (FrameSize - TempHeight) / Speed;
            h := Round(TempHeight);
          end;

        if h > FrameSize then
          h := FrameSize;

        if h = 0 then
          Frame.Visible := False
        else begin
          if h = Frame.Height then
            SetWindowRgn(Frame.Handle, 0, False)
          else begin
            if Frame.Parent = nil then
              Frame.Parent := Self;

            rgn := CreateRectRgn(0, FrameSize - h, w, Frame.Height);
            SetWindowRgn(Frame.Handle, rgn, False);
          end;
          Frame.Visible := True;
        end;
        Frame.SetBounds(TitleButton.Left, y - (FrameSize - h), w, FrameSize);
        Result := True;
      end;
    end;
end;


procedure TsFrameBar.UpdatePositions;
var
  cRect: TRect;
  CanDestroy: boolean;
  sDiv, i, j, sHeight, cWidth, AutoHeight: integer;
begin
  if not Visible or Arranging or ([csReading,csDestroying] * ComponentState <> []) or (Items.Count = 0) then
    FActiveFrameIndex := -1
  else
    if Items.UpdateCount <= 0 then begin
      cRect := CalcClientRect;
      Arranging := True;
      AutoHeight := -1;
      bCanChange := True;
      if AutoFrameSize then begin
        AutoScroll := False;
        AutoHeight := GetAutoHeight;
      end;
      SkinData.BeginUpdate;
      Perform(WM_SETREDRAW, 0, 0);
      sHeight := cRect.Top;
      cWidth := WidthOf(cRect);
      for i := 0 to Items.Count - 1 do
        with Items[i] do
          if Visible and (TitleButton <> nil) and TitleButton.Visible then begin
            TitleButton.SetBounds(cRect.Left, sHeight - Offset, cWidth, FTitleHeight);
            if TitleButton.Parent <> Self then
              TitleButton.Parent := Self;

            inc(sHeight, FTitleHeight);
            if Frame <> nil then begin
              FrameSize := Frame.Height;
              sDiv := FrameSize;
            end
            else begin
              FrameSize := 0;
              sDiv := 0;
            end;
            if State = stOpening then
              State := stOpened;

            case State of
              stOpened: begin
                FActiveFrameIndex := i;
                if AutoHeight <> -1 then begin
                  FrameSize := AutoHeight;
                  if Frame <> nil then
                    Frame.Height := FrameSize;
                end
                else
                  if Frame <> nil then
                    FrameSize := Frame.Height;

                j := -1;
                UpdateFrame(i, sHeight - Offset, j, cWidth);
                sDiv := Frame.Height;
                inc(sHeight, FSpacing);
              end;

              stClosed: begin
                if FActiveFrameIndex = i then
                  FActiveFrameIndex := -1;

                if Frame <> nil then begin
                  CanDestroy := True;
                  if Assigned(FOnFrameDestroy) then
                    FOnFrameDestroy(Items[i], Frame, CanDestroy);

                  if CanDestroy then
                    FreeAndNil(Frame);

                  FrameSize := 0;
                  sDiv := 0;
                  if Frame <> nil then
                    UpdateFrame(i, sHeight - Offset, sDiv, cWidth);

                  FrameSize := 0;
                end;
                inc(sHeight, FSpacing);
              end;
            end;
            if (Frame <> nil) and (State in [stOpened]) then begin
              if Frame.Parent = nil then
                Frame.Parent := Self;

              inc(sHeight, sDiv + BorderWidth);
            end;
            inc(sHeight, BorderWidth);
          end;

      if Assigned(acMagnForm) then
        acMagnForm.Perform(SM_ALPHACMD, AC_REFRESH_HI, 0);

      bCanChange := False;
      inc(sHeight, BorderWidth + 2 * integer(BorderStyle = bsSingle));
      if VertScrollBar.Range <> sHeight then
        VertScrollBar.Range := sHeight;

      Arranging := False;
      Perform(WM_SETREDRAW, 1, 0);
      SkinData.EndUpdate;
      if Showing and DoRepaint then
        RedrawWindow(Handle, nil, 0, RDWA_ALL);

      if Assigned(acMagnForm) then
        acMagnForm.Perform(SM_ALPHACMD, AC_REFRESH_HI, 0);
    end;
end;


procedure TsFrameBar.UpdateWidths;
var
  i, cWidth: integer;
begin
  if Items.UpdateCount <= 0 then begin
    Arranging := True;
    cWidth := WidthOf(CalcClientRect);
    for i := 0 to Items.Count - 1 do
      with Items[i] do
        if (TitleButton <> nil) and TitleButton.Visible and Visible then begin
          if TitleButton.Width <> cWidth then begin
            TitleButton.SkinData.BGChanged := True;
            TitleButton.Width := cWidth;
          end;
          if (Frame <> nil) and (Frame.Width <> cWidth) then
            Frame.Width := cWidth;
        end;

    Arranging := False;
    if AutoScroll then
      UpdateScrolls(ListSW);
  end;
end;


procedure TsFrameBar.WndProc(var Message: TMessage);
var
  i: integer;
begin
{$IFDEF LOGGED}
  AddToLog(Message);
{$ENDIF}
  case Message.Msg of
    CM_BIDIMODECHANGED:
      for i := 0 to Items.Count - 1 do
        case Items[i].State of
          stClosed: Items[i].TitleButton.ArrowAngle := iff(BiDiMode = bdLeftToRight, 0, i180);
        end;
  end;
  inherited;
  case Message.Msg of
    SM_ALPHACMD:
      case Message.WParamHi of
        AC_REFRESH:
          if (ACUInt(Message.LParam) = ACUInt(SkinData.SkinManager)) and SkinData.Skinned then begin
//            if IsWindowVisible(Handle) then
              SetWindowPos(Handle, 0, 0, 0, 0, 0, SWPA_FRAMECHANGED);

            UpdateWidths;
          end;

        AC_ENDUPDATE: Rearrange;
      end;

    WM_SIZE:
      if AutoFrameSize then
        Rearrange
      else begin
        UpdateWidths;
        if Showing then
          RedrawWindow(Handle, nil, 0, RDW_FRAME or RDW_INVALIDATE);
      end;

    CM_VISIBLECHANGED:
      if Showing then
        Rearrange;

    CM_ENABLEDCHANGED: begin
      for i := 0 to Items.Count - 1 do
        with Items[i] do begin
          TitleButton.Enabled := Enabled;
          if Frame <> nil then
            Frame.Enabled := Enabled;
        end;

      Repaint;
    end;
  end;
end;


procedure TsFrameBar.CMParentFontChanged(var Message: TMessage);
begin
  inherited;
  if ParentFont then
    TitleFont := Font;
end;


procedure TsFrameBar.CMFontChanged(var Message: TMessage);
begin
  inherited;
  //if not SkinData.CustomFont then   // look also at TsTitleButton.InternalCreate
 // if ParentFont then
 //   fTitleFont.Assign(Font);        // default font.
end;


procedure TsFrameBar.ChangedTitleFontNotify(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to Items.Count - 1 do begin
    if Items[i].TitleButton.ParentFont then
      Items[i].TitleButton.SkinData.CustomFont := SkinData.CustomFont;

    if SkinData.CustomFont then
      Items[i].TitleButton.Font := fTitleFont;
  end;
end;


procedure TsFrameBar.SetTitleFont(Value: TFont);
begin
  fTitleFont.Assign(Value);
end;


function TsFrameBar.StoredTitleFont: boolean;
begin
  Result := IsCustomFont(nil, FTitleFont);
end;


procedure TsTitleItem.Assign(Source: TPersistent);
var
  Src: TsTitleItem;
begin
  if Source <> nil then begin
    Src         := TsTitleItem(Source);
    Tag         := Src.Tag;
    Cursor      := Src.Cursor;
    Margin      := Src.Margin;
    Spacing     := Src.Spacing;
    Caption     := Src.Caption;
    Visible     := Src.Visible;
    PopupMenu   := Src.PopupMenu;
    ImageIndex  := Src.ImageIndex;
    SkinSection := Src.SkinSection;
  end
  else
    inherited;
end;


constructor TsTitleItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FTag := 0;
  FOwner := TsTitles(Collection);
  TitleButton := TsTitleButton.InternalCreate(FOwner.FOwner, Index);
  TitleButton.TitleItem := Self;
  TitleButton.OnClick := TitleButtonClick;
  if FOwner.FOwner.DragItems and ([csLoading] * FOwner.FOwner.ComponentState = []) then begin
    TitleButton.DragMode := dmAutomatic;
    TitleButton.OnDragOver := FOwner.FOwner.DoDragOver;
  end;

  FVisible := True;
  FShowArrow := False;
  FShowCloseBtn := False;
  FImageIndex := -1;
  FCursor := crDefault;
  State := stClosed;
  FOwner.FOwner.UpdatePositions;
end;


destructor TsTitleItem.Destroy;
begin
  if Assigned(FOnTitleItemDestroy) then
    FOnTitleItemDestroy(Self);

  if not (csDestroying in FOwner.FOwner.ComponentState) and (TitleButton <> nil) then begin
    TitleButton.Visible := False;
    TitleButton.Free;
    TitleButton := nil;
    FreeAndNil(Frame);
  end;
  if Assigned(FOnTitleItemDestroy) then
    FOnTitleItemDestroy(self);

  inherited Destroy;
  if not (csDestroying in FOwner.FOwner.ComponentState) then
    FOwner.FOwner.UpdatePositions;
end;


function TsTitleItem.GetPopupMenu: TPopupMenu;
begin
  if TitleButton <> nil then
    Result := TitleButton.PopupMenu
  else
    Result := nil
end;


function TsTitleItem.GetSkinSection: string;
begin
  Result := TitleButton.SkinData.SkinSection;
end;


function TsTitleItem.GetAlignment(const Index: Integer): TAlignment;
begin
  case Index of
    0: if TitleButton <> nil then
        Result := TitleButton.Alignment
      else
        Result := taCenter

    else
      if TitleButton <> nil then
        Result := TitleButton.TextAlignment
      else
        Result := taLeftJustify;
  end;
end;


function TsTitleItem.GetInteger(const Index: Integer): integer;
begin
  case Index of
    0: Result := TitleButton.Margin;

    1: begin
      Result := 0;
      if Result <> TitleButton.Spacing then begin
        Result := TitleButton.Spacing;
        if csDesigning in TitleButton.ComponentState then
          TitleButton.SkinData.Invalidate;
      end;
    end

    else
      Result := FImageIndex;
  end;
end;


procedure TsTitleItem.SetCaption(const Value: acString);
begin
  TitleButton.Caption := Value;
  FCaption := Value;
  FOwner.FOwner.UpdatePositions;
end;


procedure TsTitleItem.SetCursor(const Value: TCursor);
begin
  if FCursor <> Value then begin
    FCursor := Value;
    if TitleButton <> nil then
      TitleButton.Cursor := Value;
  end;
end;


procedure TsTitleItem.SetIndex(Value: Integer);
begin
  inherited;
  FOwner.FOwner.ArrangeTitles;
end;


procedure TsTitleItem.SetInteger(const Index, Value: integer);
begin
  case Index of
    0: if TitleButton.Margin <> Value then begin
      TitleButton.Margin := Value;
      if csDesigning in TitleButton.ComponentState then
        TitleButton.SkinData.Invalidate;
    end;

    1: if TitleButton.Spacing <> Value then begin
      TitleButton.Spacing := Value;
      if csDesigning in TitleButton.ComponentState then
        TitleButton.SkinData.Invalidate;
    end;

    2: if FImageIndex <> Value then begin
      FImageIndex := Value;
      TitleButton.ImageIndex := Value;
      if TitleButton.Images <> FOwner.FOwner.Images then
        TitleButton.Images := FOwner.FOwner.Images;

      FOwner.FOwner.UpdatePositions;
    end;
  end;
end;


procedure TsTitleItem.SetPopupMenu(const Value: TPopupMenu);
begin
  if TitleButton <> nil then
    TitleButton.PopupMenu := Value;
end;


procedure TsTitleItem.SetSkinSection(const Value: string);
begin
  TitleButton.SkinData.SkinSection := Value
end;


procedure TsTitleItem.SetAlignment(const Index: Integer; const Value: TAlignment);
begin
  case Index of
    0: if TitleButton <> nil then
      TitleButton.Alignment := Value;

    1: if TitleButton <> nil then
      TitleButton.TextAlignment := Value;
  end;
end;


procedure TsTitleItem.SetBoolean(const Index: Integer; const Value: boolean);
begin
  case Index of
    0: if FShowArrow <> Value then begin
      FShowArrow := Value;
      TitleButton.SkinData.BGChanged := True;
      TitleButton.GraphRepaint;
    end;

    1: if FVisible <> Value then begin
      FVisible := Value;
      if Value then begin
        TitleButton.SkinData.UpdateIndexes;
        TitleButton.Parent := FOwner.FOwner;
      end
      else
        TitleButton.Parent := nil;

      FOwner.FOwner.ArrangeTitles;
    end;

    2: if FShowCloseBtn <> Value then begin
      FShowCloseBtn := Value;
      TitleButton.SkinData.BGChanged := True;
      TitleButton.GraphRepaint;
    end;
  end;
end;


procedure TsTitleItem.TitleButtonClick;
var
  l, i: integer;
  ClosedItems: array of integer;
  b: boolean;
begin
  if not (csDesigning in FOwner.FOwner.ComponentState) then begin
    if Assigned(TitleButton) and Assigned(FOnClick) then
      FOnClick(TitleButton);

    case State of
      stClosed: begin
        TitleButton.SkinData.CustomFont := False;
        State := stOpening;
        if Assigned(FOwner.FOwner.FOnChanging) then begin
          b := True;
          FOwner.FOwner.FOnChanging(FOwner.FOwner, Self, b);
          if not b then begin
            State := stClosed;
            Exit;
          end;
        end;
        if not FOwner.FOwner.AllowAllOpen then
          for i := 0 to FOwner.Count - 1 do
            if (FOwner[i].State = stOpened) and (FOwner[i].GroupIndex = Self.GroupIndex) then begin
              FOwner[i].State := stClosing;
              if (i <> Index) and Assigned(FOwner[i].FOnFrameClose) then begin
                l := Length(ClosedItems);
                SetLength(ClosedItems, l + 1);
                ClosedItems[l] := i;
              end;
            end;
      end;

      stOpened: begin
        if Assigned(FOwner.FOwner.FOnChanging) then begin
          b := True;
          FOwner.FOwner.FOnChanging(FOwner.FOwner, Self, b);
          if not b then
            Exit;
        end;
        if FOwner.FOwner.AllowAllClose then begin
          FOwner[Index].State := stClosing;
          if Assigned(FOwner[Index].FOnFrameClose) then begin
            l := Length(ClosedItems);
            SetLength(ClosedItems, l + 1);
            ClosedItems[l] := Index;
          end;
        end;
      end;
    end;
    FOwner.FOwner.ArrangeTitles;
    if (State = stOpened) and Assigned(FOnFrameShow) then
      FOnFrameShow(Self);

    if Assigned(FOwner.FOwner.fOnChangedState) then
      FOwner.FOwner.fOnChangedState(FOwner.FOwner, Self, State);

    for i := 0 to Length(ClosedItems) - 1 do
      if (ClosedItems[i] < FOwner.Count) and Assigned(FOwner[i].FOnFrameClose) then
        FOwner[ClosedItems[i]].FOnFrameClose(FOwner[ClosedItems[i]]);
  end;
end;


procedure TsTitleButton.Ac_CMMouseLeave;
begin
  if not (csDestroying in Owner.ComponentState) and not (csDestroying in ComponentState)  then begin
    if TitleItem.State <> stOpened then
      if (Owner as TsFrameBar).SkinData.CustomFont then
        SkinData.CustomFont := True;

    SkinData.FMouseAbove := False;
    if SkinData.SkinManager.ActiveGraphControl = Self then
      SkinData.SkinManager.ActiveGraphControl := nil;

    SkinData.BGChanged := False;
    FCloseBtnState := 0;
    FClosePressed := False;
    if not FMenuOwnerMode then
      DoChangePaint(SkinData, 0, UpdateGraphic_CB, EventEnabled(aeMouseLeave, AnimatEvents), False, False)
    else
      SkinData.BGChanged := True;

    if Assigned(OnMouseLeave) then
      OnMouseLeave(Self);
  end;
end;


procedure TsTitleButton.Ac_CMMouseEnter;
var
  p: TPoint;
begin
  if SkinData.CustomFont then
    SkinData.CustomFont := False;

  if Assigned(OnMouseEnter) then
    OnMouseEnter(Self);

  if not SkinData.FMouseAbove and not (ButtonStyle in [tbsDivider, tbsSeparator]) and not SkinData.SkinManager.Options.NoMouseHover then begin
    SkinData.FMouseAbove := True;
    p := ScreenToClient(acMousePos);
    if PtInRect(CloseBtnRect, p) then
      CloseBtnState := 1;

    SkinData.SkinManager.ActiveGraphControl := Self;
    if not FMenuOwnerMode then
      DoChangePaint(SkinData, 1, UpdateGraphic_CB, EventEnabled(aeMouseEnter, AnimatEvents), False, not Active)
    else
      SkinData.BGChanged := True;
  end;
end;


function TsTitleButton.CloseBtnIndex: integer;
begin
  if SkinData.SkinIndex >= 0 then
    with SkinData.SkinManager, ConstData do
      Result := iff(TitleGlyphs[tgSmallClose] >= 0, TitleGlyphs[tgSmallClose], TitleGlyphs[tgClose])
  else
    Result := -1;
end;


function TsTitleButton.CloseBtnRect: TRect;
var
  i: integer;
  Size: TSize;
begin
  i := CloseBtnIndex;
  if CloseBtnIndex >= 0 then
    Size := MkSize(SkinData.SkinManager.ma[i].Width, SkinData.SkinManager.ma[i].Height)
  else
    Size := MkSize(acCloseBtnSize, acCloseBtnSize);

  Result.Left := iff(BiDiMode = bdLeftToRight, Width - acCloseBtnSize, 0);
  Result.Right := Result.Left + Size.cx;
  Result.Top := 0;
  Result.Bottom := Size.cy;
end;


constructor TsTitleButton.Create(AOwner: TComponent);
begin
  inherited;
  SavedBmp := nil;
  Active := False;
  FClosePressed := False;
end;


function TsTitleButton.CurrentState: integer;
begin
  if TempState >= 0 then
    Result := TempState
  else
    Result := inherited CurrentState;

  if Active and SkinData.Skinned then
    with SkinData, SkinManager do
      if IsValidImgIndex(BorderIndex) then
        case Result of
          0:
            Result := iff(ma[BorderIndex].ImageCount > 3, 3, 1);

          else
            if ma[BorderIndex].ImageCount > 3 then
              Result := 3;
        end;
end;


destructor TsTitleButton.Destroy;
begin
  inherited;
  if Visible then
    FOwner.UpdatePositions;
end;


constructor TsTitleButton.InternalCreate(AOwner: TsFrameBar; Index: integer);
const
  sComponentName = 'sTitleButton';
var
  i: Integer;
begin
  inherited Create(AOwner);
  TempState := -1;
  FOwner := AOwner;
  SkinData.COC := COC_TsBarTitle;
  i := aOwner.FItems.Count;
  while AOwner.FindComponent(sComponentName + IntToStr(i)) <> nil do
    inc(i);

  Name := sComponentName + IntToStr(i);
  Alignment := taCenter;
  Spacing := 8;
  Margin := 5;
  FOwner.UpdatePositions;
  TextAlignment := taLeftJustify;

  if (Owner as TsFrameBar).SkinData.CustomFont then begin
    SkinData.CustomFont := True;
    Font.Assign(TsFrameBar(Owner).fTitleFont);
  end;
end;


function TsTitleButton.PrepareCache: boolean;
var
  C: TColor;
  i, l, iNdx: integer;
  TmpBmp: TBitmap;
  ADrawData: TacBarItemDrawData;
begin
  if TitleItem.State in [stClosing, stOpening] then begin // Receive new image
    if not TitleItem.ShowArrow and (SavedBmp = nil) then begin // Save image
      SavedBmp := SkinData.FCacheBmp;
      SkinData.FCacheBmp := nil;
    end;

    Active := TitleItem.State = stOpening;
    if TitleItem.State = stOpening then
      TempState := 4
    else
      TempState := integer(SkinData.FMouseAbove);

    Result := inherited PrepareCache;
    Active := TitleItem.State <> stOpening;
    TempState := -1;
  end
  else begin
    FreeAndNil(SavedBmp);
    Result := inherited PrepareCache;
  end;
  if TitleItem.ShowArrow then begin
    TmpBmp := CreateBmp32((acArrowSize + acSpacing) * 2, (acArrowSize + acSpacing) * 2);
    FillDC(TmpBmp.Canvas.Handle, MkRect(TmpBmp), $FFFFFF);
    DrawArrow(TmpBmp, 0, clNone, MkRect(TmpBmp), asRight, maxi(2, acLineWidth), Round(ArrowAngle), SkinData.SkinManager.ScaleInt(5), arsLines1); // Paint a mask

    if SectionInArray(SkinData.SkinManager.ConstData.Sections, SkinData.SkinIndex, ssMenuItem, ssWebBtn) and (Parent <> nil) then
      iNdx := GetFontIndex(Self, SkinData.SkinIndex, SkinData.SkinManager, CurrentState)
    else
      iNdx := SkinData.SkinIndex;

    i := mini(ac_MaxPropsIndex, CurrentState);
    C := SkinData.SkinManager.gd[iNdx].Props[i].FontColor.Color;
    if BiDiMode = bdLeftToRight then
      l := Width - acSpacing - TmpBmp.Width
    else
      l := acSpacing;

    sGraphUtils.ColorizeByMask(SkinData.FCacheBmp, TmpBmp, Point(l, (Height - TmpBmp.Height) div 2), C, clNone);
    TmpBmp.Free;
  end;
  if SavedBmp <> nil then // Mix images
    SumBitmaps(SkinData.FCacheBmp, SavedBmp, iff(TitleItem.State = stOpening, MaxByte - Alpha, Alpha));

  if Assigned(TitleItem.OnDrawItem) then begin
    ADrawData.ACanvas := SkinData.FCacheBmp.Canvas;
    ADrawData.ARect := MkRect(SkinData.FCacheBmp);
    TitleItem.OnDrawItem(TitleItem, ADrawData);
  end;

  if TitleItem.ShowCloseBtn then
    with SkinData.SkinManager, ConstData do begin
      iNdx := CloseBtnIndex;
      if iNdx >= 0 then
        DrawSkinGlyph(SkinData.FCacheBmp, CloseBtnRect.TopLeft, CloseBtnState, 1, ma[iNdx], MakeCacheInfo(SkinData.FCacheBmp))
      else
        DrawCloseBtn(SkinData.FCacheBmp.Canvas.Handle, CloseBtnRect, CloseBtnState);
    end;

  if Dragging and acMouseInControl(Parent) then
    FadeBmp(SkinData.FCacheBmp, MkRect(SkinData.FCacheBmp), 50, TsColor(SkinData.SkinManager.Palette[pcSelectionBG]), 0, 0);
end;


procedure TsTitleButton.SetCloseBtnState(const Value: integer);
begin
  if FCloseBtnState <> Value then begin
    FCloseBtnState := Value;
    SkinData.BGChanged := True;
    GraphRepaint;
  end;
end;


procedure TsTitleButton.SetClosePressed(const Value: boolean);
begin
  if FClosePressed <> Value then begin
    FClosePressed := Value;
    if Value then
      FCloseBtnState := 2;

    SkinData.BGChanged := True;
    GraphRepaint;
  end;
end;


procedure TsTitleButton.StdPaint(PaintButton: boolean);
var
  R: TRect;
  Bmp: TBitmap;
  ADrawData: TacBarItemDrawData;
  Size, ArrowDim: integer;
begin
  inherited;
  if TitleItem.ShowArrow then begin
    Size := 5;
    if SkinData.SkinManager <> nil then
      Size := SkinData.SkinManager.ScaleInt(Size);

    ArrowDim := (Size + acSpacing) * 2;
    R.TopLeft := Point(Width - acSpacing - ArrowDim, (Height - ArrowDim) div 2);
    R.Right := R.Left + ArrowDim;
    R.Bottom := R.Top + ArrowDim;
    DrawArrow(Canvas.Handle, 0, clNone{ /$FFFFFF}, R, asRight, maxi(2, acLineWidth), Round(ArrowAngle), Size, arsLines1); // Paint a mask
  end;
  if Assigned(TitleItem.OnDrawItem) then begin
    ADrawData.ACanvas := Canvas;
    ADrawData.ARect := MkRect(Self);
    TitleItem.OnDrawItem(TitleItem, ADrawData);
  end;
  if TitleItem.ShowCloseBtn then
    DrawCloseBtn(Canvas.Handle, CloseBtnRect, FCloseBtnState);

  if Dragging and acMouseInControl(Parent) then begin
    Bmp := CreateBmp32(Self);
    BitBlt(Bmp.Canvas.Handle, 0, 0, Bmp.Width, Bmp.Height, Canvas.Handle, 0, 0, SRCCOPY);
    FadeBmp(Bmp, MkRect(Bmp), 50, TsColor(ColorToRGB(clHighlight)), 0, 0);
    BitBlt(Canvas.Handle, 0, 0, Bmp.Width, Bmp.Height, Bmp.Canvas.Handle, 0, 0, SRCCOPY);
    Bmp.Free;
  end;
end;

procedure TsTitleButton.WndProc(var Message: TMessage);
var
  p: TPoint;
  ica: TItemCloseAction;
begin
  case Message.Msg of
    WM_MOUSEMOVE: begin
      inherited;
      if TitleItem.ShowCloseBtn then begin
        p.x := TCMHitTest(Message).XPos;
        p.y := TCMHitTest(Message).YPos;
        if PtInRect(CloseBtnRect, p) then
          CloseBtnState := 1 + integer(FClosePressed)
        else
          CloseBtnState := 0;
      end;
    end;

    WM_LBUTTONUP:
      if TitleItem.ShowCloseBtn and FClosePressed then begin
        p.x := TCMHitTest(Message).XPos;
        p.y := TCMHitTest(Message).YPos;
        FClosePressed := False;
        if PtInRect(CloseBtnRect, p) then begin
          CloseBtnState := 1;
          ica := icaHide;
          if Assigned(TitleItem.FOnCloseBtnClick) then
            TitleItem.FOnCloseBtnClick(TitleItem, ica);

          case ica of
            icaNone: CloseBtnState := 1;
            icaHide: TitleItem.Visible := False;
            icaFree: FreeAndNil(TitleItem);
          end;
        end
        else
          CloseBtnState := 0;
      end
      else
        inherited;

    WM_LBUTTONDBLCLK,
    WM_LBUTTONDOWN:
      if TitleItem.ShowCloseBtn then begin
        p.x := TCMHitTest(Message).XPos;
        p.y := TCMHitTest(Message).YPos;
        if PtInRect(CloseBtnRect, p) then
          ClosePressed := True
        else begin
          FClosePressed := False;
          inherited;
        end;
      end
      else
        inherited;

    else
      inherited;
  end;
end;

end.
