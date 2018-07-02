unit sPageControl;
{$I sDefs.inc}
//{$DEFINE LOGGED}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, ComCtrls, extctrls, menus,
  {$IFNDEF DELPHI5} Types, {$ENDIF}
  {$IFDEF DELPHI_XE2} UITypes, {$ENDIF}
  {$IFDEF TNTUNICODE} TntComCtrls, TntGraphics, {$ENDIF}
  sCommonData, sConst, sSpeedButton, acSBUtils, acThdTimer, acntTypes, sBitBtn;


type
{$IFNDEF NOTFORHELP}
  TacCloseAction = (acaHide, acaFree);
  TacCloseBtnClick  = procedure(Sender: TComponent; TabIndex: integer; var CanClose: boolean; var Action: TacCloseAction) of object;
  TacTabMouseEvent  = procedure(Sender: TComponent; TabIndex: integer) of object;
  TacTabStartDrag   = procedure(Sender: TComponent; TabIndex: integer; var Allowed: boolean) of object;
  TacTabDrop        = procedure(Sender: TComponent; TabIndex, TargetIndex: integer; var Allowed: boolean) of object;


  TsPageControl = class;
  TsTabSheet = class;

  TsTabSkinData = class(TPersistent)
  private
    FCustomColor,
    FCustomFont: boolean;
    FPage: TsTabSheet;
    FSkinSection: string;
    procedure SetCustomFont (const Value: boolean);
    procedure SetSkinSection(const Value: string);
  published
    property CustomColor: boolean read FCustomColor write FCustomColor default False;
    property CustomFont:  boolean read FCustomFont  write SetCustomFont default False;
    property SkinSection: TsSkinSection read FSkinSection write SetSkinSection;
  end;


  TacGlyphLayout = (glLeft, glTop);
  TacTabType = (ttButton, ttMenu, ttTab);

  TacPageMargins = class(TPersistent)
  private
    FOffsetTop,
    FOffsetLeft,
    FOffsetRight,
    FOffsetBottom: integer;
    FOwner: TsPageControl;
    procedure SetMargin(Index: Integer; Value: integer);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    class procedure InitDefaults(Margins: TacPageMargins); virtual;
  public
    constructor Create(Control: TControl); virtual;
  published
    property OffsetLeft:   integer index 0 read FOffsetLeft   write SetMargin default 0;
    property OffsetTop:    integer index 1 read FOffsetTop    write SetMargin default 0;
    property OffsetRight:  integer index 2 read FOffsetRight  write SetMargin default 0;
    property OffsetBottom: integer index 3 read FOffsetBottom write SetMargin default 0;
  end;



{$IFDEF TNTUNICODE}
  TsTabSheet = class(TTntTabSheet)
{$ELSE}
  TsTabSheet = class(TTabSheet)
{$ENDIF}
  private
    FTabSkin,
    FButtonSkin: TsSkinSection;

    FTabMenu: TPopupMenu;
    FTabType: TacTabType;
    FUseCloseBtn: boolean;
    FOnClickBtn: TNotifyEvent;
    FCommonData: TsTabSkinData;
    procedure SetUseCloseBtn(const Value: boolean);
    procedure SetButtonSkin (const Value: TsSkinSection);
    procedure SetTabSkin    (const Value: TsSkinSection);
    procedure SetTabMenu    (const Value: TPopupMenu);
    procedure SetTabType    (const Value: TacTabType);
  protected
    AnimTimer: TacThreadedTimer;
    procedure ChangeScale(M, D: Integer); override;
    function FontStored: boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CreateParams (var Params: TCreateParams); override;
    procedure WMEraseBkGnd (var Message: TWMPaint); message WM_ERASEBKGND;
    procedure WMNCPaint    (var Message: TWMPaint); message WM_NCPAINT;
    procedure WndProc      (var Message: TMessage); override;
  published
    property Font stored FontStored;
    property ParentFont stored FontStored;
    property ButtonSkin:  TsSkinSection read FButtonSkin  write SetButtonSkin;
    property TabType:     TacTabType    read FTabType     write SetTabType default ttTab;
    property TabMenu:     TPopupMenu    read FTabMenu     write SetTabMenu;
    property TabSkin:     TsSkinSection read FTabSkin     write SetTabSkin;
    property SkinData:    TsTabSkinData read FCommonData  write FCommonData;
    property UseCloseBtn: boolean       read FUseCloseBtn write SetUseCloseBtn default True;
    property OnClickBtn:  TNotifyEvent  read FOnClickBtn  write FOnClickBtn;
  end;


  TacTabData = record
    GlyphRect,
    TextRect,
    BtnRect,
    ArrowRect,
    FocusRect:      TRect;
    TextSize:       TSize;
    TextPos:        TPoint;
    ArrowDirection: TacSide;
  end;


  TacTabChangingEvent = procedure(Sender: TObject; NewPage: TsTabSheet; var AllowChange: Boolean) of object;
  TacCloseBtnVisibility = (cvAlways, cvActiveTab, cvUnactiveTabs, cvMouseHovered);
{$ENDIF}

{$IFNDEF DELPHI_10TOKYO}
  {$IFDEF DELPHI_10BERLIN}
    {$DEFINE SKIPSCALE}
  {$ENDIF}
{$ENDIF}

{$IFDEF DELPHI_XE3}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]{$ENDIF}
  {$IFDEF TNTUNICODE}
  TsPageControl = class(TTntPageControl)
  {$ELSE}
  TsPageControl = class(TPageControl)
  {$ENDIF}
{$IFNDEF NOTFORHELP}
  private
    FOnCloseBtnClick: TacCloseBtnClick;
    StoredVisiblePageCount: integer;
    FAnimatEvents: TacAnimatEvents;
    FCloseBtnSkin: TsSkinSection;
    FNewDockSheet: TsTabSheet;
    FCommonData: TsCommonData;

    FWordWrap,
    UpDownChecking,
    FShowCloseBtns,
    FAllowAnimSwitching: boolean;

    DraggedBmp,
    TabsBG: TBitmap;

    DraggedForm,
    DraggedInsForm: TacGlowForm;

    DraggedPos: TPoint;
    DropBtn: TsBitBtn;
    procedure CheckDragState;
    function GetInsTab: integer;
    procedure CheckDragMousePos;
    procedure FinishTabDragging;
    procedure StartTabDrag;
    function IsLeftToRight: boolean;

    procedure CheckUpDown;
    procedure CMHintShow(var Message: TMessage);  message CM_HINTSHOW;
    procedure CNNotify  (var Message: TWMNotify); message CN_NOTIFY;

    procedure StdPaint(var Message: TWMPaint);
    procedure DrawStdTabs(DC: hdc);
    procedure DrawStdTab(PageIndex: Integer; State: integer; DC: hdc);

    procedure InitTabContentData(Canvas: TCanvas; Page: TTabSheet; BmpRect: TRect; State: integer; IsTabMenu: boolean; var Data: TacTabData);

    procedure AcPaint(const Message: TWMPaint);
    procedure DrawSkinTabs(Bmp: TBitmap);
    procedure DrawSkinTab(PageIndex: Integer; State: integer; DstDC: hdc);
    function GetTabSkinIndex(Page: TsTabSheet): integer;

    function GetTabLayout(PageIndex: Integer): TacTabLayout;
    function GetNeighborIndex(PageIndex: Integer; Next: boolean): integer;
    function PageRect: TRect;
    function TabsRect: TRect;
    function TabsBGRect: TRect;
    procedure KillTimers;
    function GetActivePage: TsTabSheet;
    procedure UpdateBtnData;
    procedure UpdatePadding;
    function SpinSection: string;
    procedure CMDockClient(var Message: TCMDockClient); message CM_DOCKCLIENT;
  private
    acDragging,
    FShowFocus,
    FShowUpDown,
    SkipDropDown,
    FActiveIsBold,
    FAllowTabsDrag,
    FRotateCaptions,
    FReflectedGlyphs,
    FActiveTabEnlarged,
    FAccessibleDisabledPages: boolean;

    FTabMargin,
    FTabPadding,
    FTabSpacing,
    FTabsLineIndex,
    FHoveredBtnIndex,
    FPressedBtnIndex: integer;

    FOnTabMouseEnter,
    FOnTabMouseLeave: TacTabMouseEvent;

    FTabAlignment: TAlignment;
    FTabsLineSkin: TsSkinSection;
    FOnPageChanging: TacTabChangingEvent;
    FOnTabStartDrag: TacTabStartDrag;
    FOnTabDrop: TacTabDrop;
    FCloseBtnVisibility: TacCloseBtnVisibility;
    FPageMargins: TacPageMargins;
    FGlyphLayout: TacGlyphLayout;
    function GetActivePageIndex: Integer;
    procedure SetTabAlignment    (const Value: TAlignment);
    procedure SetShowUpDown      (const Value: boolean);
    procedure SetHoveredBtnIndex (const Value: integer);
    procedure SetActivePageIndex (const Value: integer);
    procedure SetCurItem         (const Value: integer);
    procedure SetInt (const Index: Integer; const Value: integer);
    procedure SetBool(const Index: Integer; const Value: boolean);
    procedure SetTabsLineSkin(const Value: TsSkinSection);
    procedure SetCloseBtnSkin(const Value: TsSkinSection);
    procedure SetActivePage(const Value: TsTabSheet);
    function GetPage(Index: Integer): TsTabSheet;
    procedure SetCloseBtnVisibility(const Value: TacCloseBtnVisibility);
    procedure SetPageMargins(const Value: TacPageMargins);
    procedure SetGlyphLayout(const Value: TacGlyphLayout);
  protected
    FCurItem,
    BtnIndex,
    BtnWidth,
    DraggedItem,
    DroppedDownItem,
    BtnHeight: integer;
{$IFDEF SKIPSCALE}
    DontScale: boolean;
{$ENDIF}
    SpinWnd: TacSpinWnd;
    function BtnOffset(TabHeight: integer; Active: boolean): integer;
    procedure DoAddDockClient(Client: TControl; const ARect: TRect); override;
    procedure PaintButton(DC: hdc; TabRect: TRect; State: integer; Page: TTabSheet);
    procedure RepaintTab(Ndx: integer; AllowAnimation: boolean = True);
    function VisibleTabsCount: integer;
    function IsSpecialTab(i: integer; IsMenu: boolean = False): boolean;
    function CheckActiveTab(PageIndex: integer): TTabSheet;
    function PageIndexFromTabIndex(TabIndex: Integer): Integer;
    function SkinTabRect(Index: integer; Active: boolean): TRect;
    function StdTabRect(Index: integer): TRect;
    procedure ChangeScale(M, D: Integer); override;
    procedure TrySetNewPage(aPage: TsTabSheet);
    function FontStored: boolean;
    function PrepareCache: boolean;
{$IFDEF DELPHI7UP}
    procedure SetTabIndex(Value: Integer); override;
{$ENDIF}
  public
    property ActivePageIndex: integer read GetActivePageIndex write SetActivePageIndex;
    property CurItem: integer read FCurItem write SetCurItem default -1;
    property ShowUpDown: boolean read FShowUpDown write SetShowUpDown default True;
    property HoveredBtnIndex: integer read FHoveredBtnIndex write SetHoveredBtnIndex default -1;
    property Pages[Index: Integer]: TsTabSheet read GetPage;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure WndProc(var Message: TMessage); override;
    procedure Loaded; override;
    function GetTabUnderMouse(p: TPoint): integer;
    procedure AfterConstruction; override;
    procedure UpdateActivePage; override;
    procedure CloseClick(Sender: TObject);
    procedure SetPadding(Value: boolean);
  published
    property Font stored FontStored;
    property ParentFont stored FontStored;
    property ActivePage: TsTabSheet read GetActivePage write SetActivePage;
    property AnimatEvents: TacAnimatEvents read FAnimatEvents write FAnimatEvents default [aeGlobalDef];
    property Style;
{$ENDIF}
    property AccessibleDisabledPages: boolean read FAccessibleDisabledPages write FAccessibleDisabledPages default True;

    property CloseBtnSkin: TsSkinSection read FCloseBtnSkin write SetCloseBtnSkin;
    property CloseBtnVisibility: TacCloseBtnVisibility read FCloseBtnVisibility write SetCloseBtnVisibility default cvAlways;
    property GlyphLayout: TacGlyphLayout read FGlyphLayout write SetGlyphLayout default glLeft;

    property ActiveIsBold:       boolean index 0 read FActiveIsBold       write SetBool default False;
    property ReflectedGlyphs:    boolean index 1 read FReflectedGlyphs    write SetBool default False;
    property RotateCaptions:     boolean index 2 read FRotateCaptions     write SetBool default False;
    property ShowCloseBtns:      boolean index 3 read FShowCloseBtns      write SetBool default False;
    property ShowFocus:          boolean index 4 read FShowFocus          write SetBool default True;
    property AllowAnimSwitching: boolean index 5 read FAllowAnimSwitching write SetBool default True;
    property ActiveTabEnlarged:  boolean index 6 read FActiveTabEnlarged  write SetBool default True;
    property AllowTabsDrag:      boolean index 7 read FAllowTabsDrag      write SetBool default False;
    property WordWrap:           boolean index 8 read FWordWrap           write SetBool default False;

    property PageMargins: TacPageMargins read FPageMargins write SetPageMargins;

    property TabMargin:  integer index 0 read FTabMargin  write SetInt default 3;
    property TabPadding: integer index 1 read FTabPadding write SetInt default 0;
    property TabSpacing: integer index 2 read FTabSpacing write SetInt default 6;

    property TabsLineSkin: TsSkinSection read FTabsLineSkin write SetTabsLineSkin;
    property SkinData: TsCommonData read FCommonData write FCommonData;
    property TabAlignment: TAlignment read FTabAlignment write SetTabAlignment default taCenter;


    property OnCloseBtnClick: TacCloseBtnClick    read FOnCloseBtnClick write FOnCloseBtnClick;
    property OnPageChanging:  TacTabChangingEvent read FOnPageChanging  write FOnPageChanging;
    property OnTabMouseEnter: TacTabMouseEvent    read FOnTabMouseEnter write FOnTabMouseEnter;
    property OnTabMouseLeave: TacTabMouseEvent    read FOnTabMouseLeave write FOnTabMouseLeave;

    property OnTabStartDrag:  TacTabStartDrag read FOnTabStartDrag write FOnTabStartDrag;
    property OnTabDrop:       TacTabDrop      read FOnTabDrop      write FOnTabDrop;
{$IFNDEF NOTFORHELP}
    property OnDblClick;
{$ENDIF} // NOTFORHELP
  end;


procedure DeletePage(Page: TsTabSheet); // Page removing without switching to the first page


implementation

uses
  math, Commctrl, Buttons,
  {$IFDEF DELPHI7UP}Themes, {$ENDIF}
  {$IFDEF LOGGED}sDebugMsgs, {$ENDIF}
  sMessages, sVclUtils, acntUtils, sMaskData, sStyleSimply, acAlphaImageList, acgpUtils,
  sSkinProps, acGlow, sAlphaGraph, sDefaults, sGraphUtils, sSkinManager, sFade, sThirdParty;


const
  acTabsDragKey = [ssCtrl];

type
  TacDropBtn = class(TsBitBtn)
  public
    FOwner: TsPageControl;
    Menu: TPopupMenu;
    procedure ItemClick(Sender: TObject);
    procedure PaintGlyph(Sender: TObject; Bmp: Graphics.TBitmap);
    function CanFocus: Boolean; override;
    function GetBtnBounds: TRect;
    procedure UpdatePosition;
    procedure Click; override;
    function CurrentState: integer; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure WndProc(var Message: TMessage); override;
  end;


procedure InitTabData(Timer: TacThreadedTimer; AData: TObject; AIteration: integer; AAnimProc: TacAnimProc; AState: integer; AFast: boolean);
begin
  with Timer do begin
    Enabled := False;
    AnimData := AData;
    Iterations := acMaxIterations;
    if AState = 0 then
      Iterations := acMultipNormal * Iterations;

    if AFast then
      Iterations := Iterations div 2;

    if BmpOut <> nil then
      CopyFrom(BmpFrom, BmpOut, MkRect(BmpOut));

    case AState of
      0:
        case State of
          1:   Iteration := max(0, (acMaxIterations - Iteration) * 2)
          else Iteration := 0;
        end;

      1:
        case State of
          0, 2, 3: Iteration := AIteration;
          else     Iteration := 0
        end

      else Iteration := 0
    end;

    Value := LimitIt(Round(MaxByte * Iteration / Iterations), 0, MaxByte);
    ValueStep := (MaxByte - Value) / Iterations;

    Interval := acTimerInterval;
    AnimProc := AAnimProc;
    OldState := State;
    State    := AState;
    Enabled  := True;
  end;
end;


function UpdateTab_CB(Data: TObject; Iteration: integer): boolean;
var
  b: byte;
  DC: HDC;
  R: TRect;
  Bmp: TBitmap;
  pc: TsPageControl;
begin
  Result := False;
  if Data is TsTabSheet then
    with TsTabSheet(Data) do
      if Assigned(AnimTimer.BmpFrom) and Assigned(AnimTimer.BmpTo) then begin
        pc := TsPageControl(PageControl);
        R := pc.SkinTabRect(TabIndex, TsTabSheet(Data) = pc.ActivePage);
        Bmp := CreateBmpLike(AnimTimer.BmpTo);
        BitBlt(Bmp.Canvas.Handle, 0, 0, Bmp.Width, Bmp.Height, AnimTimer.BmpFrom.Canvas.Handle, 0, 0, SRCCOPY);

        TsTabSheet(Data).AnimTimer.Value := AnimTimer.Value + TsTabSheet(Data).AnimTimer.ValueStep;
        b := LimitIt(Round(AnimTimer.Value), 0, MaxByte);

        SumBitmaps(Bmp, AnimTimer.BmpTo, MaxByte - b);
        DC := GetDC(pc.Handle);
        try
          BitBlt(DC, R.Left, R.Top, Bmp.Width, Bmp.Height, Bmp.Canvas.Handle, 0, 0, SRCCOPY);
        finally
          ReleaseDC(pc.Handle, DC);
        end;
        Bmp.Free;
        if TsTabSheet(Data).AnimTimer.Iteration >= TsTabSheet(Data).AnimTimer.Iterations then begin
          if (TsTabSheet(Data).AnimTimer.State in [0, 3]) and (b < MaxByte) then
            Result := UpdateTab_CB(Data, Iteration);
        end
        else
          Result := True;
      end;
end;


type
  TsTabBtn = class(TsSpeedButton)
  public
    Page: TsTabSheet;
    constructor Create(AOwner:TComponent); override;
    procedure Paint; override;
    procedure UpdateGlyph;
  end;


const
  BtnOffsX = 4; // Offset of the Close button from right border

var
  acBtnPressed: boolean = False;


procedure DeletePage(Page: TsTabSheet);
begin
  if Page <> nil then begin
    if Page.PageIndex > 0 then
      Page.PageIndex := Page.PageIndex - 1;

    Page.Free
  end;
end;


procedure TsPageControl.AfterConstruction;
begin
  inherited;
  SkinData.Loaded;
end;


procedure TsPageControl.CheckUpDown;
var
  Wnd: HWND;
  sp: TacSkinParams;
begin
  if not (csLoading in ComponentState) and ([csCreating{$IFDEF D2006}, csRecreating, csAligning{$ENDIF}] * ControlState = []) and HandleAllocated then
    if not UpDownChecking then begin
      UpDownChecking := True;
      Wnd := FindWindowEx(Handle, 0, UPDOWN_CLASS, nil);
      if Wnd <> 0 then begin
        if FShowUpDown then begin
          if (SpinWnd <> nil) and (SpinWnd.CtrlHandle <> Wnd) then
            FreeAndNil(SpinWnd);

          if SpinWnd = nil then begin
            sp.SkinSection := SpinSection;
            sp.Control := nil;
            SpinWnd := TacSpinWnd.Create(Wnd, nil, SkinData.SkinManager, sp);
            SpinWnd.OwnerWnd := Self;
            if not IsLeftToRight then
              SpinWnd.SetRTL;

            with SpinWnd do
              InitCtrlData(Wnd, ParentWnd, WndRect, ParentRect, WndSize, WndPos);
          end;
          SpinWnd.Skinned := SkinData.Skinned;
        end
        else
          DestroyWindow(Wnd);

        if IsWindowVisible(Wnd) then begin
          if DropBtn = nil then
            DropBtn := TacDropBtn.Create(Self);

          TacDropBtn(DropBtn).UpdatePosition;
        end;
      end
      else begin
        if DropBtn <> nil then
          FreeAndNil(DropBtn);
      end;
      UpDownChecking := False;
    end;
end;


procedure TsPageControl.CMDockClient(var Message: TCMDockClient);
var
  IsVisible: Boolean;
  DockCtl: TControl;
  I: Integer;
begin
  with Message do begin
    Result := 0;
    DockCtl := DockSource.Control;
    for I := 0 to PageCount - 1 do
      if DockCtl.Parent = Pages[I] then begin
        Pages[I].PageIndex := PageCount - 1;
        Exit;
      end;

    FNewDockSheet := TsTabSheet.Create(Self);
    try
      try
        if DockCtl is TCustomForm then
          FNewDockSheet.Caption := TCustomForm(DockCtl).Caption;

        FNewDockSheet.PageControl := Self;
        DockCtl.Dock(Self, DockSource.DockRect);
      except
        FNewDockSheet.Free;
        raise;
      end;
      IsVisible := DockCtl.Visible;
      FNewDockSheet.TabVisible := IsVisible;
      if IsVisible then
        ActivePage := FNewDockSheet;

      DockCtl.Align := alClient;
    finally
      FNewDockSheet := nil;
    end;
  end;
end;


procedure TsPageControl.CMHintShow(var Message: TMessage);
var
  Item: integer;
  P: TPoint;
begin
  with TCMHintShow(Message), HintInfo^ do begin
    Item := GetTabUnderMouse(CursorPos);
    if (Item >= 0) and (Pages[Item].Hint <> '') then begin
      P := ClientToScreen(CursorPos);
      P.X := P.X + GetSystemMetrics(SM_CXCURSOR) div 2;
      P.Y := P.Y + GetSystemMetrics(SM_CYCURSOR) div 2;
      HintPos := P;
      HintStr := Pages[Item].Hint;
      Message.Result := 0;
    end
    else inherited;
  end;
end;


procedure TsPageControl.CNNotify(var Message: TWMNotify);
begin
  if FCommonData.Skinned then
    case Message.NMHdr^.code of
      TCN_SELCHANGE: begin
        acGlow.ClearGlows;
        if not (csDesigning in ComponentState) and FAllowAnimSwitching and FCommonData.SkinManager.AnimEffects.PageChange.Active and SkinData.SkinManager.Effects.AllowAnimation and (ow <> nil) then begin
          KillTimers;
          inherited;
          FCommonData.Updating := True;
          AnimShowControl(Self, FCommonData.SkinManager.AnimEffects.PageChange.Time, MaxByte);
          if ActivePage <> nil then
            RedrawWindow(ActivePage.Handle, nil, 0, RDW_ERASE or RDW_INVALIDATE or RDW_ALLCHILDREN);
        end
        else
          if (ActivePage <> nil) and not InUpdating(SkinData) then begin
            SkinData.Updating := True;
            inherited;
            SkinData.Updating := False;
            FCommonData.BGChanged := True;
            RedrawWindow(Handle, nil, 0, RDWA_ALLNOW);
          end
          else inherited;

        Exit;
      end;

      TCN_SELCHANGING:
        if not (csDesigning in ComponentState) and FAllowAnimSwitching and FCommonData.SkinManager.AnimEffects.PageChange.Active and FCommonData.SkinManager.Effects.AllowAnimation then begin
          PrepareForAnimation(Self);
          FCommonData.Updating := True; // Do not try to paint controls
        end
    end;

  inherited;
  if FCommonData.Skinned then
    case Message.NMHdr^.code of
      TCN_SELCHANGING:
        if (Message.Result = 1) or (ow = nil) {Animation cancelled} then begin
          FCommonData.Updating := False;
          Perform(WM_SETREDRAW, 1, 0);
          if ow <> nil then
            FreeAndNil(ow);
        end;
    end;
end;


constructor TsPageControl.Create(AOwner: TComponent);
begin
  FCommonData := TsCommonData.Create(Self, True);
  FCommonData.COC := COC_TsPageControl;
  inherited Create(AOwner);
  FAnimatEvents := [aeGlobalDef];
  FShowCloseBtns := False;
  FShowUpDown := True;
  FShowFocus := True;
  FHoveredBtnIndex := -1;
  FPressedBtnIndex := -1;
  FTabsLineIndex := -1;
  DraggedItem := -1;
  DraggedBmp := nil;
  DraggedInsForm := nil;
  DraggedForm := nil;
  FRotateCaptions := False;
  FActiveIsBold := False;
  UpDownChecking := False;
  FAccessibleDisabledPages := True;
  FActiveTabEnlarged := True;
  FAllowAnimSwitching := True;
  FTabAlignment := taCenter;
  FCloseBtnVisibility := cvAlways;
  FAllowTabsDrag := False;
  TabsBG := CreateBmp32;
  FTabSpacing := 6;
  FTabMargin := 3;
  FReflectedGlyphs := False;
  DroppedDownItem := -1;
  SpinWnd := nil;
  FCurItem := -1;
  FPageMargins := TacPageMargins.Create(Self);
end;


destructor TsPageControl.Destroy;
begin
  TabsBG.Free;
  DraggedBmp.Free;
  if Assigned(SpinWnd) then
    FreeAndNil(SpinWnd);

  FreeAndNil(FCommonData);
  FreeAndNil(FPageMargins);
  inherited Destroy;
end;


procedure TsPageControl.DoAddDockClient(Client: TControl; const ARect: TRect);
begin
  if FNewDockSheet <> nil then
    Client.Parent := FNewDockSheet;
end;


const
  TabsMargin = 15;
  TabOffsets: array [boolean] of integer = (-2, -1);


procedure TsPageControl.DrawSkinTab(PageIndex, State: integer; DstDC: hdc);
var
  C: TColor;
  CI: TCacheInfo;
  OldFont: hFont;
  Flags: Cardinal;
  SavedDC, DC: hdc;
  Page: TsTabSheet;
  SM: TsSkinManager;
  bTabMenu: boolean;
  lCaption: ACString;
  dContent: TacTabData;
  TempBmp: Graphics.TBitmap;
  aRect, rTab, rTabs, rBmp: TRect;
  FontIndex, TabsCovering, TabIndex, TabState, i: integer;
begin
  if (PageIndex >= 0) and not FCommonData.FUpdating then begin
    bTabMenu := IsSpecialTab(PageIndex);
    Page := TsTabSheet(Pages[PageIndex]);
    if not (csDestroying in Page.ComponentState) and Page.TabVisible then begin
      rTab := SkinTabRect(Page.TabIndex, (State = 2) and not bTabMenu);

      if {(State <> 0) and (rTab.Left < 0) or }IsRectEmpty(rTab) then
        Exit;

      SM := FCommonData.SkinManager;
      if SM.ConstData.Tabs[tlSingle][asTop].SkinIndex >= 0 then begin
        // Tabs drawing
        TabState := State;
        TabIndex := GetTabSkinIndex(TsTabSheet(Page));
        if bTabMenu then
          InflateRect(rTab, TabOffsets[TabPosition in [tpTop, tpBottom]], TabOffsets[TabPosition in [tpLeft, tpRight]]);

        if NeedParentFont(SM, TabIndex, State) then
          FontIndex := GetFontIndex(Parent, TabIndex, SM, State)
        else
          FontIndex := TabIndex;

        aRect := rTab;
{$IFDEF TNTUNICODE}
        if Page is TTntTabSheet then
          lCaption := TTntTabSheet(Page).Caption
        else
{$ENDIF}
          lCaption := Page.Caption;

        TabsCovering := 0;
        if (Style = tsTabs) and (Page.TabType = ttTab) then
          if Page.TabSkin = '' then
            TabsCovering := SM.CommonSkinData.TabsCovering
          else
            if Page.TabSkin = s_RibbonTab then
              TabsCovering := SM.CommonSkinData.RibbonCovering;

        // Draw tab on bitmap
        TempBmp := CreateBmp32(rTab);
        rBmp := MkRect(TempBmp);
        if Page.SkinData.CustomFont or (FontIndex < 0) then
          TempBmp.Canvas.Font.Assign(Page.Font)
        else begin
          TempBmp.Canvas.Font.Assign(Font);
          if NeedParentFont(SM, TabIndex, State) then
            FontIndex := GetFontIndex(Parent, TabIndex, SM, State)
          else
            FontIndex := TabIndex;

          i := mini(State, ac_MaxPropsIndex);
          TempBmp.Canvas.Font.Color := SM.gd[FontIndex].Props[i].FontColor.Color;
        end;
        if ActiveIsBold and (Page = ActivePage) then
          TempBmp.Canvas.Font.Style := TempBmp.Canvas.Font.Style + [fsBold];

        TempBmp.Canvas.Brush.Style := bsClear;
        if (TabIndex >= 0) and (SM.gd[TabIndex].States <= TabState) then
          TabState := SM.gd[TabIndex].States - 1;

        if TabsCovering > 0 then begin
          CI := MakeCacheInfo(FCommonData.FCacheBmp);
          PaintItem(TabIndex, CI, True, TabState, rBmp, rTab.TopLeft, TempBmp, SM);
        end
        else begin
          rTabs := TabsBGRect;
          CI := MakeCacheInfo(TabsBG, - rTabs.Left, - rTabs.Top);
          PaintItem(TabIndex, CI, True, TabState, rBmp, rTab.TopLeft, TempBmp, SM);
        end;

        // End of tabs drawing
        if TabsCovering <> 0 then
          if TabPosition in [tpTop, tpBottom] then begin
            if TabsCovering > 0 then
              InflateRect(rBmp, -TabsCovering * 2, 0);
          end
          else
            if TabsCovering > 0 then
              InflateRect(rBmp, 0, -TabsCovering * 2);

        if not OwnerDraw then begin
          // Tab content drawing
          TempBmp.Canvas.Lock;
          Flags := DT_CENTER or DT_VCENTER or TextWrapping[WordWrap];
          if UseRightToLeftReading then
            Flags := Flags or DT_RTLREADING or DT_NOCLIP;

          OldFont := 0;
          if (TabPosition in [tpLeft, tpRight]) and not RotateCaptions or (TabPosition in [tpTop, tpBottom]) and RotateCaptions then // If vertical text
            OldFont := MakeAngledFont(TempBmp.Canvas.Handle, TempBmp.Canvas.Font, -2700); // Rotated font initialization
          // Get coordinates for tab content
          InitTabContentData(TempBmp.Canvas, Page, rBmp, State, bTabMenu, dContent);
          // Draw glyph if rect is not empty
          if not IsRectEmpty(dContent.GlyphRect) then
            if (Images is TsAlphaImageList) and SM.Effects.DiscoloredGlyphs then begin
              if State = 0 then
                if TabIndex >= 0 then
                  C := SM.gd[TabIndex].Props[0].Color
                else
                  C := clBtnFace
              else
                C := clNone;

              DrawAlphaImgList(Images, TempBmp, dContent.GlyphRect.Left, dContent.GlyphRect.Top, Page.ImageIndex, 0, C, 0, 1, ReflectedGlyphs)
            end
            else
              Images.Draw(TempBmp.Canvas, dContent.GlyphRect.Left, dContent.GlyphRect.Top, Page.ImageIndex, True);

          // Write Text
          if dContent.ArrowRect.Left = dContent.ArrowRect.Right then
            Flags := Flags or DT_END_ELLIPSIS or DT_WORD_ELLIPSIS; // Draw ellipsis if not menu

          if OldFont <> 0 then begin // If font is rotated
            lCaption := CutText(TempBmp.Canvas, lCaption, HeightOf(dContent.TextRect));
            acTextRect(TempBmp.Canvas, dContent.TextRect, dContent.TextPos.X, dContent.TextPos.Y, lCaption);
            SelectObject(TempBmp.Canvas.Handle, OldFont); // Returning prev. font
          end
          else
            if Page.SkinData.CustomFont or (FontIndex < 0) then
              acTextRect(TempBmp.Canvas, dContent.TextRect, dContent.TextPos.X, dContent.TextPos.Y, lCaption)
            else
              WriteText32(TempBmp, PacChar(lCaption), True, dContent.TextRect, Flags, FontIndex, State, SM);
          // Paint focus rect
          if not IsRectEmpty(dContent.FocusRect) then begin
            TempBmp.Canvas.Pen.Color := clWindowFrame;
            TempBmp.Canvas.Brush.Color := clBtnFace;
            TempBmp.Canvas.Brush.Style := bsClear;
            TempBmp.Canvas.DrawFocusRect(dContent.FocusRect);
          end;
          // Paint Close btn
          if not IsRectEmpty(dContent.BtnRect) then
            PaintButton(TempBmp.Canvas.Handle, dContent.BtnRect, integer(FHoveredBtnIndex = PageIndex) + integer(FPressedBtnIndex = PageIndex), Page);
          // Draw Arrow
          if not IsRectEmpty(dContent.ArrowRect) and (FontIndex >= 0) then
            DrawArrow(TempBmp, SkinData.SkinManager.gd[FontIndex].Props[TabState].FontColor.Color, clNone,
            dContent.ArrowRect, dContent.ArrowDirection, 0, 0, 0, SkinData.SkinManager.Options.ActualArrowStyle);

          if (not Page.Enabled or not Enabled) and (ActivePage <> Page) then
            BlendTransRectangle(TempBmp, 0, 0, FCommonData.FCacheBmp, rTab, DefBlendDisabled);

          BitBlt(DstDC, aRect.Left, aRect.Top, TempBmp.Width, TempBmp.Height, TempBmp.Canvas.Handle, 0, 0, SRCCOPY);
          TempBmp.Canvas.Unlock;
        end
        else
          if Assigned(OnDrawTab) then begin
            BitBlt(DstDC, aRect.Left, aRect.Top, TempBmp.Width, TempBmp.Height, TempBmp.Canvas.Handle, 0, 0, SRCCOPY);
            DC := Canvas.Handle;
            Canvas.Handle := DstDC;
            SavedDC := SaveDC(Canvas.Handle);
            OnDrawTab(Self, Page.TabIndex, aRect, State <> 0);
            RestoreDC(Canvas.Handle, SavedDC);
            Canvas.Handle := DC;
          end;

        FreeAndNil(TempBmp);
      end;
    end;
  end;
end;


procedure TsPageControl.DrawSkinTabs(Bmp: TBitmap);
var
  i: integer;
  CI: TCacheInfo;
  R, RTabsBG: TRect;
begin
  if not (csDestroying in ComponentState) then begin
    CI := GetParentCache(FCommonData);
    R := TabsRect;
    if ActivePage <> nil then begin
      RTabsBG := RectsAnd(PageRect, SkinTabRect(ActivePage.TabIndex, True));
      if not IsRectEmpty(RTabsBG) then
        if not ci.Ready then
          FillDC(Bmp.Canvas.Handle, RTabsBG, CI.FillColor)
        else
          BitBlt(Bmp.Canvas.Handle, RTabsBG.Left, RTabsBG.Top, WidthOf(RTabsBG), HeightOf(RTabsBG),
                 ci.Bmp.Canvas.Handle, ci.X + Left + RTabsBG.Left, ci.Y + Top + RTabsBG.Top, SRCCOPY);
    end;
    Bmp.Canvas.Lock;
    if FTabsLineIndex >= 0 then
      PaintItem(FTabsLineIndex, CI, True, 0, R, Point(Left, Top), Bmp, SkinData.SkinManager)
    else
      if not ci.Ready then
        FillDC(Bmp.Canvas.Handle, R, CI.FillColor)
      else
        BitBlt(Bmp.Canvas.Handle, R.Left, R.Top, min(WidthOf(R), ci.Bmp.Width), min(HeightOf(R), ci.Bmp.Height),
               ci.Bmp.Canvas.Handle, ci.X + Left + R.Left, ci.Y + Top + R.Top, SRCCOPY);

    if Bmp = SkinData.FCacheBmp then begin
      RTabsBG := TabsBGRect;
      TabsBG.Width  := WidthOf (RTabsBG);
      TabsBG.Height := HeightOf(RTabsBG);
      BitBlt(TabsBG.Canvas.Handle, 0, 0, TabsBG.Width, TabsBG.Height, Bmp.Canvas.Handle, RTabsBG.Left, RTabsBG.Top, SRCCOPY);
      FillAlphaRect(TabsBG, MkRect(TabsBG), MaxByte);
    end;

    for i := PageCount - 1 downto 0 do
      with TsTabSheet(Pages[i]) do
        if TabVisible and ((Pages[i] <> ActivePage) or (TabType <> ttTab)) then
          DrawSkinTab(i, iff((DroppedDownItem = i) or ((FPressedBtnIndex = i) and (TabType <> ttTab)), 2, integer(CurItem = i)), Bmp.Canvas.Handle);

    // Draw active tab
    if (Tabs.Count > 0) and (ActivePage <> nil) and (ActivePage.TabType = ttTab) then
      DrawSkinTab(ActivePage.PageIndex, 2, Bmp.Canvas.Handle);

    Bmp.Canvas.UnLock;
  end;
end;


function TsPageControl.GetActivePage: TsTabSheet;
begin
  Result := TsTabSheet(inherited ActivePage);
end;


function TsPageControl.GetTabUnderMouse(p: TPoint): integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to PageCount - 1 do
    if Pages[i] = ActivePage then begin
      if PtInRect(SkinTabRect(Pages[i].TabIndex, True), p) then begin
        Result := i;
        Exit;
      end;
    end
    else
      if PtInRect(StdTabRect(Pages[i].TabIndex), p) then begin
        Result := i;
        Exit;
      end;
end;


procedure TsPageControl.Loaded;
begin
  inherited;
  SkinData.Loaded;
  if ActivePage <> nil then begin
    AddToAdapter(ActivePage);
    CheckUpDown;
    inherited ActivePage := CheckActiveTab(ActivePage.PageIndex);
  end;
  if FTabPadding <> 0 then begin
    UpdateBtnData;
    Perform(WM_SIZE, 0, 0);
  end;
  if (SkinData.SkinManager <> nil) and SkinData.SkinManager.CommonSkinData.Active and HandleAllocated then
    SetClassLong(Handle, GCL_HBRBACKGROUND, Integer(SkinData.SkinManager.Brushes[pcMainColor]));
end;


procedure TsPageControl.CheckDragState;
var
  p: TPoint;
  h: THandle;
  b: boolean;
  OldNdx, NewNdx: integer;
begin
  if DraggedItem >= 0 then begin
    p := acMousePos;
    h := GetCapture;
    if h <> Handle then begin // MouseUp has been processed
      NewNdx := GetInsTab;
      OldNdx := DraggedItem;
      FinishTabDragging;
      if NewNdx >= 0 then begin
        b := True;
        if Assigned(FOnTabDrop) then
          OnTabDrop(Self, OldNdx, NewNdx, b);

        if b then begin
          FCommonData.BGChanged := True;
          Pages[OldNdx].PageIndex := NewNdx;
          Perform(WM_NCPAINT, 0, 0);
        end;
      end;
    end
    else
      if (p.X <> DraggedPos.X) or (p.Y <> DraggedPos.Y) then begin
        DraggedForm.SetBounds(DraggedForm.Left + p.X - DraggedPos.X, DraggedForm.Top + p.Y - DraggedPos.Y, DraggedForm.Width, DraggedForm.Height);
        DraggedPos := p;
        CheckDragMousePos;
      end;
  end;
end;


function TsPageControl.PageRect: TRect;
var
  r: TRect;
begin
  Result := MkRect(Width, Height);
  if Tabs.Count > 0 then begin
    AdjustClientRect(r);
    case TabPosition of
      tpTop:    Result.Top    := R.Top    - TopOffset;
      tpBottom: Result.Bottom := R.Bottom + BottomOffset;
      tpLeft:
        if IsLeftToRight then
          Result.Left   := R.Left   - LeftOffset
        else
          Result.Right  := R.Right  + RightOffset;
      else
        if IsLeftToRight then
          Result.Right  := R.Right  + RightOffset
        else
          Result.Left   := R.Left   - LeftOffset
    end;
  end;
end;


procedure TsPageControl.RepaintTab(Ndx: integer; AllowAnimation: boolean = True);
var
  R: TRect;
  Bmp: TBitmap;
  DC, SavedDC: hdc;
  State, i: integer;

  function GetTabState: integer;
  begin
    if (Ndx <> CurItem) or not HotTrack and (State = 1) then
      Result := 0
    else
      if CurItem = FPressedBtnIndex then
        Result := 4
      else
        if CurItem = FHoveredBtnIndex then
          Result := 3
        else
          Result := 1 + integer((CurItem = FPressedBtnIndex) or (CurItem = DroppedDownItem));
  end;

begin
  if not (FCommonData.FUpdating and SkinData.Skinned) and IsValidIndex(Ndx, PageCount) then begin
    if SkinData.Skinned then begin
      if not SkinData.SkinManager.Effects.AllowAnimation then
        AllowAnimation := False;

      R := TsPageControl(Pages[Ndx].PageControl).SkinTabRect(Pages[Ndx].TabIndex, Pages[Ndx] = ActivePage);
      Bmp := CreateBmpLike(SkinData.FCacheBmp);
      DrawSkinTabs(Bmp);
      if AllowAnimation then
        with TsTabSheet(Pages[Ndx]) do begin
          State := GetTabState;
          i := GetNewTimer(AnimTimer, Pages[Ndx], State);
          if AnimTimer.BmpFrom <> nil then
            FreeAndNil(AnimTimer.BmpFrom);

          if AnimTimer.BmpTo <> nil then
            AnimTimer.BmpFrom := AnimTimer.BmpTo;

          if AnimTimer.BmpFrom = nil then begin
            AnimTimer.BmpFrom := CreateBmp32(R);
            BitBlt(AnimTimer.BmpFrom.Canvas.Handle, 0, 0, AnimTimer.BmpFrom.Width, AnimTimer.BmpFrom.Height, TsPageControl(Pages[Ndx].PageControl).SkinData.FCacheBmp.Canvas.Handle, R.Left, R.Top, SRCCOPY);
          end;
          AnimTimer.BmpTo := CreateBmp32(AnimTimer.BmpFrom);
          BitBlt(AnimTimer.BmpTo.Canvas.Handle, 0, 0, AnimTimer.BmpTo.Width, AnimTimer.BmpTo.Height, Bmp.Canvas.Handle, R.Left, R.Top, SRCCOPY);
          if (AnimTimer.State = -1) or (State <> AnimTimer.State) then begin // Not started already
            InitTabData(AnimTimer, Pages[Ndx], i, UpdateTab_CB, State, (State = 2) and (TabType <> ttTab) or (State = 4));
            UpdateTab_CB(Pages[Ndx], i);
          end;
        end
      else begin
        DC := GetDC(Handle);
        SavedDC := SaveDC(DC);
        try
          BitBlt(DC, R.Left, R.Top, WidthOf(R), HeightOf(R), Bmp.Canvas.Handle, R.Left, R.Top, SRCCOPY);
        finally
          RestoreDC(DC, SavedDC);
          ReleaseDC(Handle, DC);
        end;
      end;
      Bmp.Free;
    end
    else begin
      State := GetTabState;
      DC := GetDC(Handle);
      SavedDC := SaveDC(DC);
      try
        R := StdTabRect(Pages[Ndx].TabIndex);
        if (ActivePage <> nil) and (ActivePage.TabType = ttTab) and (Pages[Ndx] <> ActivePage) then begin
          InterSectClipRect(DC, R.Left, R.Top, R.Right, R.Bottom);
          R := StdTabRect(ActivePage.TabIndex);
          InflateRect(R, 2, 2);
          ExcludeClipRect(DC, R.Left, R.Top, R.Right, R.Bottom)
        end;
        if DroppedDownItem = Ndx then
          State := 2;

        if Pages[Ndx] = ActivePage then
          State := 2
        else
          if State > 2 then
            State := 1;

        DrawStdTab(Ndx, State, DC);
      finally
        RestoreDC(DC, SavedDC);
        ReleaseDC(Handle, DC);
      end;
    end;
  end
end;


procedure TsPageControl.SetActivePage(const Value: TsTabSheet);
begin
  if Value <> nil then begin
    inherited ActivePage := CheckActiveTab(Value.PageIndex);
    AddToAdapter(ActivePage);
    if not Value.TabVisible and not SkinData.FUpdating then
      SetParentUpdated(Value); // Update because TCM_SETCURSEL is not received
  end
  else
    inherited ActivePage := nil;
end;


function TsPageControl.SkinTabRect(Index: integer; Active: boolean): TRect;
var
  aTabsCovering: integer;

  function GetTabCovering(aIndex: integer): integer;
  begin
    Result := 0;
    with FCommonData.SkinManager, TsTabSheet(Pages[aIndex]) do
      if TabType = ttTab then
        if TabSkin = '' then
          Result := CommonSkinData.TabsCovering
        else
          if TsTabSheet(Pages[aIndex]).TabSkin = s_RibbonTab then
            Result := CommonSkinData.RibbonCovering;
  end;

begin
  Result := MkRect;
  if IsValidIndex(Index, PageCount) then begin
    Result := StdTabRect(Index);
    if FActiveTabEnlarged then begin
      case Style of
        tsTabs: if (Result.Left <> Result.Right) and Active then begin
          InflateRect(Result, 2, 2);
          if (FCommonData.SkinManager <> nil) and (FCommonData.SkinManager.CommonSkinData.Version >= 13) then
            case TabPosition of
              tpLeft:   dec(Result.Right);
              tpTop:    dec(Result.Bottom);
              tpRight:  inc(Result.Left);
              tpBottom: inc(Result.Top);
            end;
        end;
      end;
      aTabsCovering := 0;
      if (Style = tsTabs) and (FCommonData.SkinManager <> nil) then
        if TsTabSheet(Pages[Index]).TabType = ttTab then
          aTabsCovering := GetTabCovering(Index)
        else
          if Index > 0 then
            Result.Left := Result.Left + (GetTabCovering(Index - 1) + 1)
          else
            if Index < Self.PageCount - 1 then
              Result.Right := Result.Right - (GetTabCovering(Index + 1) + 1);

      if aTabsCovering > 0 then
        if TabPosition in [tpTop, tpBottom] then begin
          OffsetRect (Result, aTabsCovering, 0);
          InflateRect(Result, aTabsCovering, 0);
        end
        else begin
          OffsetRect (Result, 0, aTabsCovering);
          InflateRect(Result, 0, aTabsCovering);
        end;

      case TsTabSheet(Pages[Index]).TabType of
        ttMenu, ttButton:
          if TabPosition in [tpTop, tpBottom] then begin
            dec(Result.Right, 2 + integer(Index <> 0));
            inc(Result.Left, 2);
          end
          else begin
            dec(Result.Bottom, 2 + integer(Index <> 0));
            inc(Result.Top, 2);
          end;
      end;
    end
    else
      case TabPosition of
        tpLeft: begin
          InflateRect(Result, 2, 2 * integer(Active));
          if not Active then
            dec(Result.Right);
        end;

        tpRight: begin
          InflateRect(Result, 2, 2 * integer(Active));
          if not Active then
            inc(Result.Left)
        end

        else begin
          InflateRect(Result, 0, 2);
          if Between(Result.Left, 0, 4) then
            Result.Left := 0;
        end;
      end;
  end;
end;


function TsPageControl.SpinSection: string;
begin
  if SkinData.Skinned then
    if SkinData.SkinManager.ConstData.Sections[ssUpDown] < 0 then
      Result := s_Button
    else
      Result := s_UpDown;
end;


function TsPageControl.TabsRect: TRect;
var
  R: TRect;
begin
  if Tabs.Count > 0 then begin
    Result := MkRect(Width, Height);
    AdjustClientRect(R);
    case TabPosition of
      tpTop:    Result.Bottom := R.Top    - TopOffset;
      tpBottom: Result.Top    := R.Bottom + BottomOffset;
      tpLeft:
        if IsLeftToRight then
          Result.Right  := R.Left   - LeftOffset
        else
          Result.Left   := R.Right  + RightOffset;
      else
        if IsLeftToRight then
          Result.Left   := R.Right  + RightOffset
        else
          Result.Right  := R.Left   - LeftOffset;
    end;
  end
  else
    Result := MkRect;
end;


procedure TsPageControl.TrySetNewPage(aPage: TsTabSheet);
var
  MHdr: TNMHdr;
  Result: HRESULT;
begin
  if aPage <> ActivePage then begin
    MHdr.hwndFrom := Handle;
    MHdr.code := TCN_SELCHANGING;
    Result := Perform(CN_NOTIFY, WParam(Handle), LPAram(@MHdr));
    if Result = 0 then begin
      ActivePage := aPage;
      MHdr.code := TCN_SELCHANGE;
      Perform(CN_NOTIFY, WParam(Handle), LPAram(@MHdr));
    end;
  end;
end;


function TsPageControl.TabsBGRect: TRect;
var
  R: TRect;
begin
  if Tabs.Count > 0 then begin
    Result := MkRect(Width, Height);
    AdjustClientRect(R);
    case TabPosition of
      tpTop:    Result.Bottom := R.Top    - TopOffset    + TabsMargin;
      tpBottom: Result.Top    := R.Bottom + BottomOffset - TabsMargin;
      tpLeft:   Result.Right  := R.Left   - LeftOffset   + TabsMargin
      else      Result.Left   := R.Right  + RightOffset  - TabsMargin;
    end;
  end
  else
    Result := MkRect;
end;


procedure TsPageControl.UpdateActivePage;
var
  DC, SavedDC: hdc;
begin
  inherited;
  if FCommonData.Skinned and not SkinData.SkinManager.AnimEffects.PageChange.Active and FAllowAnimSwitching and FCommonData.SkinManager.Effects.AllowAnimation and not FCommonData.Updating then
    if StoredVisiblePageCount <> VisibleTabsCount then begin
      Perform(WM_PAINT, 0, 0);
      if Assigned(ActivePage) then
        ActivePage.Repaint
    end
    else
      if ActivePage <> nil then begin // Active tab repainting
        DC := GetDC(Handle);
        SavedDC := SaveDC(DC);
        SkinData.BGChanged := True;
        try
          DrawSkinTab(ActivePage.PageIndex, 2, DC)
        finally
          RestoreDC(DC, SavedDC);
          ReleaseDC(Handle, DC);
        end;
      end
      else
        FCommonData.Invalidate;
end;


function TsPageControl.VisibleTabsCount: integer;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to PageCount - 1 do
    if Pages[i].TabVisible then
      inc(Result);
end;


procedure TsPageControl.AcPaint(const Message: TWMPaint);
var
  R: TRect;
  b: boolean;
  DC, SavedDC: hdc;
begin
  if not (csDestroying in Parent.ComponentState) and not (csLoading in ComponentState) then begin
    if not InUpdating(FCommonData, False) then
      FCommonData.FUpdating := GetBoolMsg(Parent, AC_PREPARING); // Transparent BG may be not ready if PageControl haven't cached BG

    if not FCommonData.FUpdating then begin
      if (SkinData.CtrlSkinState and ACS_PRINTING <> 0) and (Message.DC = SkinData.PrintDC) then
        DC := Message.DC
      else
        DC := GetDC(Handle);

      try
        // If transparent and form resizing processed
        b := FCommonData.BGChanged or FCommonData.HalfVisible or GetBoolMsg(Parent, AC_GETHALFVISIBLE);
        GetClipBox(DC, R); // If control is shown partially (remember it)
        FCommonData.HalfVisible := (WidthOf(R) <> Width) or (HeightOf(R) <> Height);
        if b then // If cache is changed
          if not PrepareCache then
            Exit;

        // Output to DC
        if DroppedDownItem >= 0 then begin // If TabMenu is dropped down then don't repaint
          SavedDC := SaveDC(DC);
          R := StdTabRect(DroppedDownItem);
          ExcludeClipRect(DC, R.Left, R.Top, R.Right, R.Bottom);
          CopyWinControlCache(Self, FCommonData,  MkRect, MkRect(Self), DC, True);
          RestoreDC(DC, SavedDC);
        end
        else
          CopyWinControlCache(Self, FCommonData,  MkRect, MkRect(Self), DC, True);

        sVCLUtils.PaintControls(DC, Self, True, MkPoint); // Paint skinned TGraphControls
        if ActivePage <> nil then begin // Draw active tab
{$IFDEF D2005}
          if csDesigning in ComponentState then
            RedrawWindow(ActivePage.Handle, nil, 0, RDW_INVALIDATE or RDW_UPDATENOW or RDW_ERASE);
{$ENDIF}
          if ActivePage.BorderWidth > 0 then
            ActivePage.Perform(WM_NCPAINT, 0, 0);

          SetParentUpdated(Self);
        end;
      finally
        if DC <> Message.DC then
          ReleaseDC(Handle, DC);
      end;
    end;
    StoredVisiblePageCount := VisibleTabsCount;
  end;
end;


function CanBeActive(Page: TsTabSheet; PageControl: TsPageControl): boolean;
begin
  if (Page <> nil) and (Page.TabType = ttTab) and (PageControl.AccessibleDisabledPages or Page.Enabled) then
    if Assigned(PageControl.OnPageChanging) then begin
      Result := True;
      PageControl.OnPageChanging(PageControl, Page, Result);
    end
    else
      Result := True
  else
    Result := False;
end;

{
procedure MirrorInRect(MainRect: TRect; var R: TRect; Horz: boolean);
var
  i: integer;
begin
  if Horz then begin
    i := R.Left;
    R.Left := MainRect.Left + MainRect.Right - R.Right;
    R.Right := MainRect.Left + MainRect.Right - i;
  end
  else begin
    i := R.Top;
    R.Top := MainRect.Top + MainRect.Bottom - R.Bottom;
    R.Bottom := MainRect.Top + MainRect.Bottom - i;
  end;
end;
}

procedure TsPageControl.WndProc(var Message: TMessage);
var
  p: TPoint;
  b: boolean;
  rBtn, R: TRect;
  DC, SavedDC: hdc;
  PS: TPaintStruct;
  Page: TsTabSheet;
  TabData: TacTabData;
  NewItem, i: integer;
  Act: TacCloseAction;
begin
{$IFDEF LOGGED}
//  if (Tag = 7) and Parent.Parent.HandleAllocated and IsWindowVisible(Parent.Parent.Handle) then
    AddToLog(Message);
{$ENDIF}
  case Message.Msg of
    TCM_ADJUSTRECT: begin
      PRect(Message.LParam)^.Left  := FPageMargins.FOffsetLeft;
      PRect(Message.LParam)^.Right := ClientWidth + FPageMargins.FOffsetRight;
      inc(PRect(Message.LParam)^.Top, FPageMargins.FOffsetTop);
      PRect(Message.LParam)^.Bottom := ClientHeight + FPageMargins.FOffsetBottom;
    end;

    WM_PAINT:
      if Visible or (csDesigning in ComponentState) then begin
        BeginPaint(Handle, PS);
        TWMPaint(Message).DC := GetDC(Handle);
        try
          if FCommonData.Skinned then begin
            if not InAnimationProcess or (csDesigning in ComponentState) then begin
              b := False;
              for i := 0 to PageCount - 1 do
                with TsTabSheet(Pages[i]) do
                  if (AnimTimer <> nil) and AnimTimer.Enabled then begin
                    b := True;
                    Break;
                  end;

              if not b then
                AcPaint(TWMPaint(Message));

              Message.Result := 1;
            end;
          end
          else
            StdPaint(TWMPaint(Message));
        finally
          ReleaseDC(Handle, TWMPaint(Message).DC);
          EndPaint(Handle, PS);
        end;
        Message.Result := 0;
        Exit;
      end
      else inherited;

    TCM_SETCURSEL: begin
      KillTimers;
      inherited;
      if FCommonData.Skinned then begin
        FCommonData.BGChanged := True;
        RedrawWindow(Handle, nil, 0, RDW_ERASE or RDW_INVALIDATE or RDW_FRAME);
        if not SkinData.FUpdating then
          SetParentUpdated(Self);
      end
      else
        RedrawWindow(Handle, nil, 0, RDW_INVALIDATE or RDW_FRAME);

      Exit;
    end;

    TCM_SETIMAGELIST: begin
      inherited;
      if HandleAllocated then
        Perform(WM_SIZE, 0, 0);

      CheckUpDown;
      Exit;
    end;

    CM_DIALOGKEY:
      if (TWMKey(Message).CharCode = Ord(#9)) and (ActivePage <> nil) then begin
        if GetKeyState(VK_CONTROL) and $8000 = $8000 then begin
          Page := nil;

          if GetKeyState(VK_SHIFT) and $8000 = 0 then begin
            if ActivePage.PageIndex = PageCount - 1 then
              i := 0
            else
              i := ActivePage.PageIndex + 1;

            while i < PageCount do begin
              if Pages[i].TabVisible then begin
                Page := TsTabSheet(Pages[i]);
                Break;
              end;
              inc(i);
            end;
          end
          else begin
            if ActivePage.PageIndex = 0 then
              i := PageCount - 1
            else
              i := ActivePage.PageIndex - 1;

            while i >= 0 do begin
              if Pages[i].TabVisible then begin
                Page := TsTabSheet(Pages[i]);
                Break;
              end;
              dec(i);
            end;
          end;
          if not CanBeActive(Page, Self) then
            Exit;
        end;
      end;

    WM_KEYDOWN:
      if ActivePage <> nil then
        case Message.WParamLo of
          VK_LEFT: begin
            Page := nil;
            for i := ActivePage.PageIndex - 1 downto 0 do
              if Pages[i].TabVisible then begin
                Page := TsTabSheet(Pages[i]);
                Break;
              end;

            if not CanBeActive(Page, Self) then
              Exit;
          end;

          VK_RIGHT: begin
            Page := nil;
            for i := ActivePage.PageIndex + 1 to PageCount - 1 do
              if Pages[i].TabVisible then begin
                Page := TsTabSheet(Pages[i]);
                Break;
              end;

            if not CanBeActive(Page, Self) then
              Exit;
          end;
        end;

    WM_MOUSELEAVE, CM_MOUSELEAVE:
      if ([csDesigning, csDestroying] * ComponentState = []) and (CurItem >= 0) then begin
        p := ScreeNToClient(acMousePos);
        R := TabsRect;
        if not SkinData.Skinned then
          inherited;

        FPressedBtnIndex := -1;
        FHoveredBtnIndex := -1;
        acBtnPressed := False;
        if IsValidIndex(CurItem, PageCount) and Assigned(FOnTabMouseLeave) then
          FOnTabMouseLeave(Self, CurItem);

        CurItem := -1;
        Exit;
      end;

    WM_NCHITTEST: if PtInRect(TabsRect, ScreenToClient(Point(TWMMouse(Message).XPos, TWMMouse(Message).YPos))) then begin
      if GetTabUnderMouse(ScreenToClient(Point(TWMMouse(Message).XPos, TWMMouse(Message).YPos))) >= 0 then
        Message.Result := HTCLIENT
      else
        Message.Result := HTTRANSPARENT;

      Exit;
    end;

    WM_MOUSEMOVE:
      if [csDesigning, csDestroying] * ComponentState = [] then begin
        if DraggedItem >= 0 then begin
          StartTabDrag;
          CheckDragState;
        end;

        if (DefaultManager <> nil) and not (csDesigning in DefaultManager.ComponentState) then
          DefaultManager.ActiveControl := Handle;

        p.x := TCMHitTest(Message).XPos;
        p.y := TCMHitTest(Message).YPos;
        NewItem := GetTabUnderMouse(p);
        if IsValidIndex(NewItem, PageCount) and TsTabSheet(Pages[NewItem]).Enabled then begin // If tab is hovered
          Page := TsTabSheet(Pages[NewItem]);
          if Assigned(OnMouseMove) then
            OnMouseMove(Self, GetShiftState, TCMHitTest(Message).XPos, TCMHitTest(Message).YPos);
{
          inherited;
          if not SkinData.Skinned or not SkinData.SkinManager.Effects.AllowAnimation then begin
            PeekMessage(aMsg, Handle, WM_PAINT, WM_PAINT, PM_REMOVE); // Prevent an automatic repainting
            Application.ProcessMessages;
          end;
}
          if not IsValidIndex(NewItem, PageCount) then
            Exit;

          if FShowCloseBtns then
            case Page.TabType of
              ttTab: begin // Check if Close button was hovered
                if SkinData.Skinned then
                  R := SkinTabRect(Page.TabIndex, Page = ActivePage)
                else
                  R := StdTabRect(Page.TabIndex);

                InitTabContentData(Canvas, Page, MkRect(WidthOf(R), HeightOf(R)), 1 + integer(Page = ActivePage), False, TabData);
                rBtn := TabData.BtnRect;
                OffsetRect(rBtn, R.Left, R.Top);

                if PtInRect(rBtn, p) then begin
                  if FHoveredBtnIndex <> NewItem then begin
                    i := FHoveredBtnIndex;
                    FHoveredBtnIndex := NewItem;
                    RepaintTab(FHoveredBtnIndex);
                    if i >= 0 then
                      RepaintTab(i);
                  end;
                end
                else
                  if FHoveredBtnIndex >= 0 then begin
                    FHoveredBtnIndex := -1;
                    FPressedBtnIndex := -1;
                    RepaintTab(NewItem);
                  end;
              end
            end;

          if NewItem <> CurItem then begin // If hot item is changed
            if IsValidIndex(CurItem, PageCount) then begin
              if ShowHint and (Page.Hint <> '') then begin
                Application.HideHint;
                Application.ActivateHint(acMousePos);
              end;
              if Assigned(FOnTabMouseLeave) then
                FOnTabMouseLeave(Self, CurItem);

              if (ActivePage = Pages[NewItem]) and ((CurItem <> ActivePageIndex) or ShowCloseBtns) then
                NewItem := -1;
            end;
            if Assigned(FOnTabMouseEnter) then
              FOnTabMouseEnter(Self, NewItem);

            CurItem := NewItem;
          end;
        end
        else begin
          FHoveredBtnIndex := -1;
          FPressedBtnIndex := -1;
          if (CurItem >= 0) and ((CurItem <> ActivePageIndex) or ShowCloseBtns) then begin
            CurItem := -1;
            acBtnPressed := False;
          end;
        end;
        Exit;
      end;

    WM_LBUTTONUP, WM_LBUTTONDOWN:
      if not (csDesigning in ComponentState) and Enabled then begin
        if FAllowTabsDrag and (DraggedItem >= 0) and (WM_LBUTTONUP = Message.Msg) and (GetShiftState = acTabsDragKey) then begin
          inherited;
          ReleaseCapture;
          CheckDragState;
          Exit;
        end;
        p.x := TCMHitTest(Message).XPos;
        p.y := TCMHitTest(Message).YPos;
        NewItem := GetTabUnderMouse(p);
        if NewItem >= 0 then begin // If tab is pressed
          if FAllowTabsDrag and (WM_LBUTTONDOWN = Message.Msg) and (GetShiftState = acTabsDragKey) then begin
            b := True;
            if Assigned(FOnTabStartDrag) then
              OnTabStartDrag(Self, NewItem, b);

            if b then begin
              DraggedPos := p;
              DraggedItem := NewItem;
              Exit;
            end;
          end;
          Page := TsTabSheet(Pages[NewItem]);
          if not FAccessibleDisabledPages and not Page.Enabled then
            Exit;

          case Page.TabType of
            ttMenu: begin
              if (Page.TabMenu <> nil) and (Message.Msg = WM_LBUTTONDOWN) and not SkipDropDown then begin
                SkipDropDown := True;
                DroppedDownItem := NewItem;
                RepaintTab(NewItem, False);
                p := ClientToScreen(MkPoint);
                R := StdTabRect(Page.TabIndex);
                TempControl := pointer(Self);
                Page.TabMenu.PopupComponent := Self;
                if (TabPosition in [tpLeft, tpRight]) and not RotateCaptions or (TabPosition in [tpTop, tpBottom]) and RotateCaptions then
                  Page.TabMenu.Popup(p.X + R.Right, p.Y + R.Top)
                else
                  if BiDiMode = bdRightToLeft then
                    Page.TabMenu.Popup(p.X + R.Right, p.Y + R.Bottom - 2)
                  else
                    Page.TabMenu.Popup(p.X + R.Left, p.Y + R.Bottom - 2);

                Page.TabMenu.PopupComponent := nil;
                TempControl := nil;
                p := Self.ScreenToClient(acMousePos);
                if NewItem <> GetTabUnderMouse(p) then
                  SkipDropDown := False;
              end
              else
                SkipDropDown := False;

              DroppedDownItem := -1;
              CurItem := -1;
              KillTimers;
              FCommonData.Updating := False;
              RedrawWindow(Handle, nil, 0, RDWA_NOCHILDRENNOW);
              Perform(WM_NCPAINT, 0, 0);
              Exit;
            end;

            ttButton:
              if Page.Enabled then begin
                i := FPressedBtnIndex;
                FPressedBtnIndex := iff(Message.Msg = WM_LBUTTONDOWN, NewItem, -1);
                RepaintTab(NewItem);
                if (Message.Msg <> WM_LBUTTONDOWN) and (i = NewItem) and Assigned(Page.OnClickBtn) then
                  Page.OnClickBtn(Pages[NewItem]);

                Exit;
              end;

            else begin
              if FShowCloseBtns and Page.UseCloseBtn then begin
                if SkinData.Skinned then
                  R := SkinTabRect(Page.TabIndex, Page = ActivePage)
                else
                  R := StdTabRect(Page.TabIndex);

                InitTabContentData(Canvas, Page, MkRect(WidthOf(R), HeightOf(R)), 1 + integer(Page = ActivePage), IsSpecialTab(NewItem), TabData);
                rBtn := TabData.BtnRect;
                OffsetRect(rBtn, R.Left, R.Top);

                if PtInRect(rBtn, p) then begin // Close btn pressed
                  FPressedBtnIndex := iff(Message.Msg = WM_LBUTTONDOWN, NewItem, -1);
                  RepaintTab(NewItem);
                  if WM_LBUTTONUP = Message.Msg then begin
                    if not acBtnPressed then
                      Exit;

                    Act := acaFree;
                    b := True;
                    if Assigned(OnCloseBtnClick) then
                      OnCloseBtnClick(Self, NewItem, b, Act);

                    if b and (Pages[NewItem] <> nil) then begin
                      i := ActivePageIndex;
                      Perform(WM_SETREDRAW, 0, 0);
                      KillTimers;
                      SkinData.BeginUpdate;
                      if Act = acaFree then
                        Page.Free
                      else
                        Page.TabVisible := False;

                      if IsValidIndex(i, PageCount) then
                        TrySetNewPage(TsTabSheet(Pages[mini(i, PageCount - 1)]));

                      SkinData.EndUpdate;
                      Perform(WM_SETREDRAW, 1, 0);
                      FCommonData.BGChanged := True;
                      RedrawWindow(Handle, nil, 0, RDWA_NOCHILDRENNOW);
                    end;
                    acBtnPressed := False;
                    FPressedBtnIndex := -1;
                  end
                  else
                    acBtnPressed := True;
                end
                else
                  if (Message.Msg <> WM_LBUTTONDOWN) or CanBeActive(Page, Self) then
                    if WM_LBUTTONUP = Message.Msg then begin
                      FPressedBtnIndex := -1;
                      MouseUp(mbLeft, GetShiftState, TCMHitTest(Message).XPos, TCMHitTest(Message).YPos);
                    end
                    else begin
                      MouseDown(mbLeft, GetShiftState, TCMHitTest(Message).XPos, TCMHitTest(Message).YPos);
                      TrySetNewPage(Page);
                    end;
              end
              else
                if Message.Msg = WM_LBUTTONDOWN then begin
                  MouseDown(mbLeft, GetShiftState, TCMHitTest(Message).XPos, TCMHitTest(Message).YPos);
                  if CanBeActive(Page, Self) then
                    TrySetNewPage(Page);
                end
                else
                  if Message.Msg = WM_LBUTTONUP then
                    MouseUp(mbLeft, GetShiftState, TCMHitTest(Message).XPos, TCMHitTest(Message).YPos);

              Exit;
            end;
          end;
        end;
      end;
  end;
  if Message.Msg = SM_ALPHACMD then
    case Message.WParamHi of
      AC_REMOVESKIN: begin
        if (ACUInt(Message.LParam) = ACUInt(SkinData.SkinManager)) and (SkinData.SkinIndex >= 0) then begin
          CommonWndProc(Message, FCommonData);
          if SpinWnd <> nil then
            FreeAndNil(SpinWnd);

          CheckUpDown;
          UpdateBtnData;
          AlphaBroadcast(Self, Message);
          if HandleAllocated then begin
            SetClassLong(Handle, GCL_HBRBACKGROUND, Integer(GetSysColorBrush(COLOR_BTNFACE)));
            if Showing and ([csLoading, csDestroying] * ComponentState = []) then
              RedrawWindow(Handle, nil, 0, RDWA_NOCHILDRENNOW);
          end;
        end
        else
          AlphaBroadcast(Self, Message);

        Exit;
      end;

      AC_REFRESH:
        if ACUInt(Message.LParam) = ACUInt(SkinData.SkinManager) then begin
          KillTimers;
          CommonWndProc(Message, FCommonData);
          UpdateBtnData;
          if HandleAllocated then begin
            SetClassLong(Handle, GCL_HBRBACKGROUND, Integer(SkinData.SkinManager.Brushes[pcMainColor]));
            if Showing and not (csLoading in ComponentState) then
              RedrawWindow(Handle, nil, 0, RDWA_NOCHILDRENNOW);
          end;
          AddToAdapter(ActivePage);
          CheckUpDown;
          if SpinWnd <> nil then
            SendMessage(SpinWnd.CtrlHandle, Message.Msg, Message.WParam, Message.LParam);

          AlphaBroadcast(Self, Message);
          Exit;
        end;

      AC_SETNEWSKIN: begin
        AlphaBroadcast(Self, Message);
        if ACUInt(Message.LParam) = ACUInt(SkinData.SkinManager) then begin
          if Message.LParam = LPARAM(SkinData.SkinManager) then
            CommonWndProc(Message, FCommonData);

          UpdateBtnData;
          if not (csLoading in ComponentState) and not aSkinChanging and HandleAllocated then
            Perform(WM_SIZE, 0, 0);

          FTabsLineIndex := SkinData.SkinManager.GetSkinIndex(FTabsLineSkin);
          Exit;
        end;
      end;

      AC_PREPARECACHE: begin
        if not InUpdating(SkinData) then
          PrepareCache;

        Exit;
      end;

      AC_GETOUTRGN: begin
        PRect(Message.LParam)^ := PageRect;
        OffsetRect(PRect(Message.LParam)^, Left, Top);
        Exit;
      end;

      AC_ENDPARENTUPDATE: begin
        if FCommonData.FUpdating then
          if not GetBoolMsg(Parent, AC_PREPARING) and not InUpdating(SkinData, True) then
            RedrawWindow(Handle, nil, 0, RDWA_NOCHILDRENNOW)
          else begin
            FCommonData.FUpdating := True;
            SetParentUpdated(Self);
          end;

        Exit;
      end;

      AC_ENDUPDATE:
        if not FCommonData.FUpdating then begin
          RedrawWindow(Handle, nil, 0, RDWA_ALLNOW);
          SetParentUpdated(Self);
        end;

      AC_MOUSELEAVE: begin
        Perform(WM_MOUSELEAVE, 0, 0);
        Exit;
      end;

      AC_GETBG: begin
        CommonMessage(Message, FCommonData);
        PacBGInfo(Message.LParam).FillRect := MkRect;
        Exit;
      end;

      AC_GETDEFINDEX: begin
        if FCommonData.SkinManager <> nil then
          Message.Result := FCommonData.SkinManager.GetSkinIndex(s_PageControl + sTabPositions[TabPosition]) + 1;

        Exit;
      end

      else
        if CommonMessage(Message, FCommonData) then
          Exit;
    end
  else
    if (FCommonData <> nil) and FCommonData.Skinned(True) then
      case Message.Msg of
        4871:
          FCommonData.BGChanged := True; // Items were added

        WM_KILLFOCUS, WM_SETFOCUS:
          if not (csDesigning in ComponentState) then begin
            FCommonData.BGChanged := True;
            inherited;
            if (ActivePage <> nil) and not InUpdating(FCommonData) then
              RepaintTab(ActivePage.PageIndex, False);

            Exit;
          end

        else
          Exit;
      end;

  if (FCommonData <> nil) and FCommonData.Skinned then begin
    if CommonWndProc(Message, FCommonData) then
      Exit;

    case Message.Msg of
      WM_PRINT: begin
        CheckUpDown;
        SkinData.Updating := False;
        AcPaint(TWMPaint(Message));
        if (SpinWnd <> nil) and IsWindowVisible(SpinWnd.CtrlHandle) then begin
          SavedDC := SaveDC(TWMPaint(Message).DC);
          try
            MoveWindowOrg(TWMPaint(Message).DC, SpinWnd.WndPos.X, SpinWnd.WndPos.Y);
            SendMessage(SpinWnd.CtrlHandle, WM_PAINT, Message.WParam, Message.LParam);
          finally
            RestoreDC(TWMPaint(Message).DC, SavedDC);
          end;
        end;
        Exit;
      end;

      WM_NCPAINT:
        if not InAnimationProcess and (DroppedDownItem < 0) then begin
          if ActivePage <> nil then begin
            if InUpdating(FCommonData) then
              Exit;

            DC := GetDC(Handle);
            SavedDC := SaveDC(DC);
            try
              if FCommonData.BGChanged then
                PrepareCache;

              if ActivePage <> nil then begin
                R := ActivePage.BoundsRect;
                ExcludeClipRect(DC, R.Left, R.Top, R.Right, R.Bottom);
              end;
              CopyWinControlCache(Self, FCommonData, MkRect, MkRect(Self), DC, True);
            finally
              RestoreDC(DC, SavedDC);
              ReleaseDC(Handle, DC);
            end;
          end;
          if (SpinWnd <> nil) and not FCommonData.BGChanged then
            RedrawWindow(SpinWnd.CtrlHandle, nil, 0, RDW_INVALIDATE or RDW_FRAME or RDW_UPDATENOW);

          Message.Result := 0;
        end;

      WM_ERASEBKGND: begin
        Message.Result := 1;
        Exit
      end;

      WM_STYLECHANGED, WM_STYLECHANGING:
        if not (csLoading in ComponentState) then begin
          KillTimers;
          FCommonData.BGChanged := True;
        end;

      WM_HSCROLL:
        if not (csLoading in ComponentState) then begin
          KillTimers;
          FCommonData.BGChanged := True;
          inherited;
          RedrawWindow(Handle, nil, 0, RDW_ERASE or RDW_INVALIDATE or RDW_FRAME);
          Exit;
        end;

      WM_PARENTNOTIFY:
        if ([csDesigning, csLoading] * ComponentState = []) and (Message.WParamLo in [WM_CREATE, WM_DESTROY]) then begin
          i := PageCount;
          inherited;
          if Message.WParamLo = WM_CREATE then
            if (srThirdParty in SkinData.SkinManager.SkinningRules) and (i <> PageCount) then
              AddToAdapter(Self);

          CheckUpDown;
          Exit;
        end;

      CM_CONTROLLISTCHANGE: begin
        i := PageCount;
        inherited;
        if i <> PageCount then begin
          CheckUpDown;
          if srThirdParty in SkinData.SkinManager.SkinningRules then
            AddToAdapter(Self);
        end;
        Exit;
      end;
    end;
  end;
  inherited;
  case Message.Msg of
    WM_PRINT: begin
      StdPaint(TWMPaint(Message));
      Exit;
    end;

    WM_STYLECHANGED:
      if not (csLoading in ComponentState) then begin
        FCommonData.UpdateIndexes;
        if FCommonData.Skinned then begin
          UpdateBtnData;
          CheckUpDown;
        end;
      end;

    CM_DIALOGCHAR:
      if FCommonData.Skinned then begin
        FCommonData.BGChanged := True;
        RedrawWindow(Handle, nil, 0, RDW_ERASE or RDW_INVALIDATE or RDW_FRAME);
        SetParentUpdated(Self);
      end;

    TCM_DELETEITEM:
      if FCommonData.Skinned and not SkinData.FUpdating then
        SkinData.BGChanged := True;

    WM_LBUTTONDBLCLK: begin
      if IsValidIndex(CurItem, PageCount) and ((Style <> tsTabs) or (TsTabSheet(Pages[CurItem]).TabType = ttButton)) then
        RepaintTab(CurItem, False); // If button is dblclicked

      if Assigned(OnDblClick) then
        OnDblClick(Self);
    end;

    WM_CREATE:
      if HandleAllocated then begin
        UpdateBtnData;
        Perform(WM_SIZE, 0, 0); // Update tab sizes after creation or style changing
        CheckUpDown;
      end;

    CM_ENABLEDCHANGED:
      if [csDestroying, csLoading] * ComponentState = [] then
        SkinData.Invalidate;

    SM_ALPHACMD:
      case Message.WParamHi of
        AC_UPDATEPAGEBTN:
          CheckUpDown;
      end;

    WM_SIZE:
      if DropBtn <> nil then
        TacDropBtn(DropBtn).UpdatePosition;
  end;
end;


procedure TsPageControl.CloseClick(Sender: TObject);
var
  i: integer;
  ToClose: boolean;
  Act: TacCloseAction;
begin
  ToClose := True;
  Act := acaFree;
  if Assigned(OnCloseBtnClick) then
    OnCloseBtnClick(Self, TsTabBtn(Sender).Page.TabIndex, ToClose, Act);

  if ToClose then begin
    i := ActivePageIndex;
    if Act = acaFree then
      FreeAndNil(TsTabBtn(Sender).Page)
    else
      TsTabBtn(Sender).Page.TabVisible := False;

    if (i < PageCount) and (i <> 0) then
      ActivePageIndex := i;

    TsTabBtn(Sender).Visible := False;
    RedrawWindow(Handle, nil, 0, RDW_ERASE or RDW_INVALIDATE or RDW_UPDATENOW);
  end;
end;


procedure TsPageControl.PaintButton(DC: hdc; TabRect: TRect; State: integer; Page: TTabSheet);
var
  TmpBmp: TBitmap;
begin
  if BtnIndex >= 0 then begin
    TmpBmp := CreateBmp32(BtnWidth, BtnHeight);
    BitBlt(TmpBmp.Canvas.Handle, 0, 0, BtnWidth, BtnHeight, DC, TabRect.Left, TabRect.Top, SRCCOPY);
    if CloseBtnSkin = '' then
      DrawSkinGlyph(TmpBmp, Point(BtnWidth - FCommonData.SkinManager.ma[BtnIndex].Width, 0), State, 1, FCommonData.SkinManager.ma[BtnIndex], MakeCacheInfo(TmpBmp))
    else begin
      PaintItem(BtnIndex, MakeCacheInfo(TmpBmp), True, State, MkRect(TmpBmp), MkPoint, TmpBmp, SkinData.SkinManager);
      SelectObject(TmpBmp.Canvas.Handle, CreatePen(PS_SOLID, 2, clRed));
      acPaintLine(TmpBmp.Canvas.Handle, 4, 4,                 TmpBmp.Width - 5, TmpBmp.Height - 5);
      acPaintLine(TmpBmp.Canvas.Handle, 4, TmpBmp.Height - 5, TmpBmp.Width - 5, 4);
    end;
    BitBlt(DC, TabRect.Left, TabRect.Top, BtnWidth, BtnHeight, TmpBmp.Canvas.Handle, 0, 0, SRCCOPY);
    FreeAndNil(TmpBmp);
  end;
end;


var
  bUpdating: boolean = False;

procedure TsPageControl.UpdateBtnData;
begin
  BtnWidth := acCloseBtnSize;
  BtnHeight := acCloseBtnSize;
  if FCommonData.Skinned then
    with FCommonData.SkinManager do begin
      if CloseBtnSkin <> '' then
        BtnIndex := GetSkinIndex(CloseBtnSkin)
      else
        BtnIndex := ConstData.TitleGlyphs[tgSmallClose];

      if BtnIndex >= 0 then begin
        BtnWidth := max(ma[BtnIndex].Width, 8);
        BtnHeight := ma[BtnIndex].Height;
      end;
    end;

  if not bUpdating then
    UpdatePadding;
end;


procedure TsPageControl.UpdatePadding;
begin
  bUpdating := True;
  SetPadding(FShowCloseBtns);
  bUpdating := False;
end;


procedure TsPageControl.SetCloseBtnVisibility(const Value: TacCloseBtnVisibility);
begin
  if FCloseBtnVisibility <> Value then begin
    FCloseBtnVisibility := Value;
    KillTimers;
    FCommonData.Invalidate;
  end;
end;


procedure TsPageControl.SetCloseBtnSkin(const Value: TsSkinSection);
begin
  if FCloseBtnSkin <> Value then begin
    FCloseBtnSkin := Value;
    FCommonData.Invalidate;
  end;
end;


procedure TsPageControl.SetShowUpDown(const Value: boolean);
var
  Wnd: THandle;
begin
  FShowUpDown := Value;
  if not FShowUpDown then begin
    if SpinWnd <> nil then begin
      Wnd := SpinWnd.CtrlHandle;
      FreeAndNil(SpinWnd);
    end
    else
      Wnd := FindWindowEx(Handle, 0, UPDOWN_CLASS, nil);

    if Wnd <> 0 then
      DestroyWindow(Wnd);
  end
  else
    if not (csLoading in ComponentState) then
      RecreateWnd;
end;


procedure TsPageControl.SetPadding(Value: boolean);
const
  OffsArray: array [boolean, boolean] of integer = ((7, 4), (6, 4));
var
  i: integer;
begin
  if HandleAllocated then begin
    i := OffsArray[Value, Images = nil] + FTabPadding;
    if FCommonData.Skinned then
      i := FCommonData.SkinManager.ScaleInt(i + FCommonData.SkinManager.CommonSkinData.TabsCovering);

    SendMessage(Handle, TCM_SETPADDING, 0, MakeLParam(iff(Value, BtnWidth div 2, 0) + i, 3));
  end;
end;


procedure TsPageControl.SetPageMargins(const Value: TacPageMargins);
begin
  FPageMargins.Assign(Value);
end;


function TsPageControl.BtnOffset(TabHeight: integer; Active: boolean): integer;
begin
  Result := (TabHeight - acCloseBtnSize - 1) div 2 - 2
end;


function TsPageControl.IsLeftToRight: boolean;
begin
  Result := (csDesigning in ComponentState) or (BiDiMode <> bdRightToLeft);
end;


function TsPageControl.IsSpecialTab(i: integer; IsMenu: boolean = False): boolean;
begin
  Result := Pages[i].TabVisible and (TsTabSheet(Pages[i]).TabType <> ttTab);
  if Result and IsMenu then
    Result := TsTabSheet(Pages[i]).TabType = ttMenu;
end;


procedure TsPageControl.KillTimers;
var
  i: integer;
begin
  for i := 0 to PageCount - 1 do
    with TsTabSheet(Pages[i]) do
      if AnimTimer <> nil then
        FreeAndNil(AnimTimer);
end;


function TsPageControl.CheckActiveTab(PageIndex: integer): TTabSheet;
var
  i: integer;
begin
  if IsSpecialTab(PageIndex) or not Pages[PageIndex].TabVisible then begin
    for i := PageIndex to PageCount - 1 do
      if not IsSpecialTab(i) then begin
        Result := Pages[i];
        Exit;
      end;

    for i := PageIndex downto 0 do
      if not IsSpecialTab(i) then begin
        Result := Pages[i];
        Exit;
      end;
  end;
  Result := Pages[PageIndex];
end;


procedure TsPageControl.CheckDragMousePos;
var
  CurNdx: integer;

  procedure ShowInsForm(Ndx: integer);
  var
    AlphaBmp: TBitmap;
    Div2, w: integer;
    C: TColor;
    wR, R: TRect;
  begin
    if DraggedItem < Ndx then
      inc(Ndx);

    GetWindowRect(Handle, wR);
    if Ndx >= PageCount then
      R := StdTabRect(Ndx - 1)
    else
      R := StdTabRect(Ndx);

    if TabPosition in [tpTop, tpBottom] then begin    
      Dec(R.Top, 3);
      Inc(R.Bottom, 3);
    end
    else begin
      Dec(R.Left, 3);
      Inc(R.Right, 3);
    end;
    Div2 := acArrowSize + 1;
    if DraggedInsForm = nil then begin
      w := Div2 * 2;
      if TabPosition in [tpTop, tpBottom] then 
        AlphaBmp := CreateBmp32(w, HeightOf(R) + w)
      else
        AlphaBmp := CreateBmp32(WidthOf(R) + w, w);
        
      FillRect32(AlphaBmp, MkRect(AlphaBmp), 0, 0);

      C := Cardinal(ColorToRGB(clHighLight)) or $FF000000;

      if TabPosition in [tpTop, tpBottom] then begin
        DrawArrow(AlphaBmp, C, clNone, Rect(0, 0, w, Div2), asBottom, acLineWidth + 1, 0, 0, arsSolid1);
        DrawArrow(AlphaBmp, C, clNone, Rect(0, AlphaBmp.Height - Div2 - 1, w, AlphaBmp.Height - 1), asTop, acLineWidth + 1, 0, 0, arsSolid1);
      end
      else begin
        DrawArrow(AlphaBmp, C, clNone, Rect(0, 0, Div2, w), asRight, acLineWidth + 1, 0, 0, arsSolid1);
        DrawArrow(AlphaBmp, C, clNone, Rect(AlphaBmp.Width - Div2, 0, AlphaBmp.Width, w), asLeft, acLineWidth + 1, 0, 0, arsSolid1);
      end;

      DraggedInsForm := TacGlowForm.CreateNew(nil);
      SetWindowLong(DraggedInsForm.Handle, GWL_EXSTYLE, GetWindowLong(DraggedForm.Handle, GWL_EXSTYLE) or WS_EX_TRANSPARENT);

      DraggedInsForm.Width := AlphaBmp.Width;
      DraggedInsForm.Height := AlphaBmp.Height;

      SetFormBlendValue(DraggedInsForm.Handle, AlphaBmp, MaxByte);
      AlphaBmp.Free;
      ShowWindow(DraggedInsForm.Handle, SW_SHOWNA);
    end;
    if Ndx >= PageCount then
      if TabPosition in [tpTop, tpBottom] then 
        SetWindowPos(DraggedInsForm.Handle, HWND_TOPMOST, R.Right - Div2 + wR.Left, R.Top - Div2 + wR.Top, 0, 0, SWPA_SHOWZORDER)
      else
        SetWindowPos(DraggedInsForm.Handle, HWND_TOPMOST, R.Left - Div2 + wR.Left, R.Bottom - Div2 + wR.Top, 0, 0, SWPA_SHOWZORDER)
    else
      SetWindowPos(DraggedInsForm.Handle, HWND_TOPMOST, R.Left - Div2 + wR.Left, R.Top - Div2 + wR.Top, 0, 0, SWPA_SHOWZORDER);
  end;

  procedure HideInsForm;
  begin
    FreeAndNil(DraggedInsForm);
  end;

begin
  CurNdx := GetInsTab;
  if CurNdx >= 0 then
    ShowInsForm(CurNdx)
  else
    HideInsForm;
end;


{$IFDEF DELPHI7UP}
procedure TsPageControl.SetTabIndex(Value: Integer);
var
  PageIndex: integer;
begin
  if IsValidIndex(Value, PageCount) then begin
    PageIndex := PageIndexFromTabIndex(Value);
    if IsValidIndex(PageIndex, PageCount) then
      if TsTabSheet(Pages[PageIndex]).TabType = ttTab then begin
        inherited;
        AddToAdapter(ActivePage);
      end;
  end
  else
    inherited;
end;
{$ENDIF}


procedure TsPageControl.StartTabDrag;
var
  R, wR: TRect;
begin
  if not acDragging then begin
    acDragging := True;
    DraggedPos := acMousePos;
    GetWindowRect(Handle, wR);
    if SkinData.Skinned then begin
      R := SkinTabRect(DraggedItem, ActivePage.TabIndex = DraggedItem);
      DraggedBmp := CreateBmp32(R);
    end
    else begin
      R := StdTabRect(DraggedItem);
      DraggedBmp := CreateBmp32(R);
      MoveWindowOrg(DraggedBmp.Canvas.Handle, -R.Left, -R.Top);
      DraggedBmp.Canvas.Lock;
      DrawStdTab(DraggedItem, 1 + integer(ActivePage.TabIndex = DraggedItem), DraggedBmp.Canvas.Handle);
      DraggedBmp.Canvas.Unlock;
      MoveWindowOrg(DraggedBmp.Canvas.Handle, R.Left, R.Top);
    end;
    BitBlt(DraggedBmp.Canvas.Handle, 0, 0, DraggedBmp.Width, DraggedBmp.Height, SkinData.FCacheBmp.Canvas.Handle, R.Left, R.Top, SRCCOPY);
    OffsetRect(R, wR.Left, wR.Top);
    DraggedForm := TacGlowForm.CreateNew(nil);
    SetWindowLong(DraggedForm.Handle, GWL_EXSTYLE, GetWindowLong(DraggedForm.Handle, GWL_EXSTYLE) or WS_EX_TRANSPARENT);
    FillAlphaRect(DraggedBmp, MkRect(DraggedBmp), 127);
    UpdateAlpha(DraggedBmp);

    DraggedForm.Width := DraggedBmp.Width;
    DraggedForm.Height := DraggedBmp.Height;

    SetFormBlendValue(DraggedForm.Handle, DraggedBmp, MaxByte);
    SetWindowPos(DraggedForm.Handle, HWND_TOPMOST, R.Left, R.Top, 0, 0, SWPA_SHOWZORDER);

    SetCapture(Handle);
  end;
end;


procedure TsPageControl.StdPaint(var Message: TWMPaint);
var
  R: TRect;
  DC, SavedDC: hdc;
  Bmp: TBitmap;
{$IFDEF DELPHI7UP}
  Details: TThemedElementDetails;
{$ENDIF}
begin
  if not (csDestroying in Parent.ComponentState) and not (csLoading in ComponentState) then begin
    Bmp := CreateBmp32(Self);
    DC := Bmp.Canvas.Handle;
    Canvas.Handle := DC;
    Canvas.Lock;
    Bmp.Canvas.Lock;
    R := MkRect(Width, Height);
    try
      if Style = tsTabs then begin
{$IFDEF DELPHI7UP}
        if acThemesEnabled then begin
          // Transparent part drawing
          Details := acThemeServices.GetElementDetails(ttBody);
          acThemeServices.DrawParentBackground(Handle, DC, @Details, True);
          acThemeServices.DrawElement(DC, Details, R);
          // Page painting
          R := PageRect;
          case TabPosition of
            tpTop: inc(R.Right, 2);
{
            tpBottom: Result.Bottom := R.Bottom + BottomOffset;
            tpLeft:   Result.Left   := R.Left   - LeftOffset
            else      Result.Right  := R.Right  + RightOffset;
}
          end;
          Details := acThemeServices.GetElementDetails(ttPane);
          acThemeServices.DrawParentBackground(Handle, DC, @Details, False);
          acThemeServices.DrawElement(DC, Details, R);
        end
        else
{$ENDIF}
        begin
          R := PageRect;
          FillDC(DC, ClientRect, ColorToRGB(Color));
          DrawEdge(DC, R, EDGE_RAISED, BF_RECT);
        end;
      end
      else
        FillDC(DC, R, Color);

      DrawStdTabs(DC);
    finally
      Canvas.UnLock;
    end;
    StoredVisiblePageCount := VisibleTabsCount;

    if Message.DC <> 0 then begin
      SavedDC := 0;
      DC := Message.DC;
    end
    else begin
      DC := GetDC(Handle);
      SavedDC := SaveDC(DC);
    end;

    try
      BitBlt(DC, 0, 0, Width, Height, Bmp.Canvas.Handle, 0, 0, SRCCOPY);
    finally
      if DC <> Message.DC then begin
        RestoreDC(DC, SavedDC);
        ReleaseDC(Handle, DC);
      end;
    end;
    Bmp.Canvas.UnLock;
    Bmp.Free;
    Message.Result := 1;
  end;
end;


function TsPageControl.StdTabRect(Index: integer): TRect;
begin
  Result := TabRect(Index);
  if not IsLeftToRight and (TabPosition in [tpTop, tpBottom]) then
    ReflectRect(Result, Width, Height, TabPosition in [tpTop, tpBottom]);
end;


procedure TsPageControl.DrawStdTab(PageIndex, State: integer; DC: hdc);
const
  AntiPosArray: array [TTabPosition] of integer = (BF_BOTTOM, BF_TOP, BF_RIGHT, BF_LEFT);
  Spacing = 3;
  FixOffset = 6;
var
{$IFDEF DELPHI7UP}
  Details: TThemedElementDetails;
  Tab: TThemedTab;
  ToolBtn: TThemedToolBar;
  Btn: TThemedButton;
  tp: TTabPosition;
{$ENDIF}
  SavedDC: hdc;
  OldFont: hFont;
  Flags: Cardinal;
  Page: TsTabSheet;
  TempBmp: TBitmap;
  bTabMenu: boolean;
  lCaption: ACString;
  dContent: TacTabData;
  aRect, R, rTmp: TRect;
begin
  if PageIndex >= 0 then begin
    bTabMenu := IsSpecialTab(PageIndex);
    Page := TsTabSheet(Pages[PageIndex]);
    R := StdTabRect(Pages[PageIndex].TabIndex);
//    if (State <> 0) and (R.Left < 0) then
//      Exit;

    if bTabMenu then
      if TabPosition in [tpTop, tpBottom] then
        InflateRect(R, -3, -1)
      else
        InflateRect(R, -1, -3)
    else
      if (State = 2) and not bTabMenu and (Style = tsTabs) then
        InflateRect(R, 2, 2);

    if not Page.TabVisible then
      Exit;

    TempBmp := CreateBmp32(R);
    try
      TempBmp.Canvas.Font.Assign(Font);// := Font;
      SelectObject(TempBmp.Canvas.Handle, TempBmp.Canvas.Font.Handle);
      if ActiveIsBold and (Page = ActivePage) then
        TempBmp.Canvas.Font.Style := TempBmp.Canvas.Font.Style + [fsBold];

      TempBmp.Canvas.Brush.Style := bsClear;
      aRect := MkRect(TempBmp);
{$IFDEF DELPHI7UP}
      if acThemesEnabled then begin
        // Tabs drawing
        if bTabMenu then begin
          if State = 0 then
            BitBlt(TempBmp.Canvas.Handle, 0, 0, TempBmp.Width, TempBmp.Height, DC, R.Left, R.Top, SRCCOPY) { Copy background }
          else
            FillDC(TempBmp.Canvas.Handle, MkRect(TempBmp), Color);

          if Page.TabType = ttMenu then begin
            case State of
              0:
                TempBmp.Canvas.Font.Color := clMenuText;

              1, 2: begin
                FillDC(TempBmp.Canvas.Handle, Rect(1, 1, TempBmp.Width - 1, TempBmp.Height - 1), clMenuHighlight);
                TempBmp.Canvas.Font.Color := clHighlightText;
              end;
            end;
            Details.Part := -1;
          end
          else begin
            case State of
              0:   ToolBtn := ttbButtonNormal;
              1:   ToolBtn := ttbButtonHot;
              else ToolBtn := ttbButtonPressed;
            end;
            Details := acThemeServices.GetElementDetails(ToolBtn);
          end;
        end
        else
          case Style of
            tsTabs: begin
              tp := TabPosition;
              if TabPosition <> tpTop then begin // Others tabs are not supported by API
                if not IsLeftToRight then
                  case TabPosition of
                    tpLeft: tp := tpRight;
                    tpRight: tp := tpLeft;
                  end;

                case State of
                  0:
                    if tp = tpBottom then
                      Details := acThemeServices.GetElementDetails(ttTabItemNormal)
                    else
                      Details := acThemeServices.GetElementDetails(ttbButtonNormal);

                  1:
                    if tp = tpBottom then
                      Details := acThemeServices.GetElementDetails(ttTabItemHot)
                    else
                      Details := acThemeServices.GetElementDetails(ttbButtonHot);

                  2:
                    Details := acThemeServices.GetElementDetails(ttPane);
                end
              end
              else begin // Draw buttons with a special offset (tabs emulation)
                case State of
                  0:   Tab := ttTabItemNormal;
                  1:   Tab := ttTabItemHot;
                  else Tab := ttTabItemSelected;
                end;
                Details := acThemeServices.GetElementDetails(Tab);
              end;
              with acThemeServices, TempBmp do // Draw tab
                case tp of
                  tpTop:    DrawElement(Canvas.Handle, Details, aRect);
                  tpBottom: DrawElement(Canvas.Handle, Details, Rect(0, -FixOffset, Width, Height));
                  tpLeft:   DrawElement(Canvas.Handle, Details,  MkRect(Width + FixOffset, Height));
                  tpRight:  DrawElement(Canvas.Handle, Details, Rect(-FixOffset, 0, Width, Height));
                end;

              Details.Part := -1;
            end;

            tsButtons: begin
              case State of
                0:   Btn := tbPushButtonNormal;
                1:   Btn := tbPushButtonHot
                else Btn := tbPushButtonPressed;
              end;
              Details := acThemeServices.GetElementDetails(Btn);
            end

            else begin
              if R.Left > 6 then begin // Draw a separator
                rTmp := Rect(R.Left - 6, R.Top, R.Left, R.Bottom);
                DrawEdge(DC, rTmp, EDGE_ETCHED, BF_LEFT);
              end;
              case State of
                0: begin
                  FillDC(TempBmp.Canvas.Handle, MkRect(TempBmp), Color); { Fill background }
                  ToolBtn := ttbButtonNormal;
                end;

                1: begin
                  FillDC(TempBmp.Canvas.Handle, MkRect(TempBmp), Color); { Fill background }
                  ToolBtn := ttbButtonHot;
                end

                else
                  ToolBtn := ttbButtonPressed;
              end;
              Details := acThemeServices.GetElementDetails(ToolBtn);
            end;
          end;

        if Details.Part <> -1 then
          acThemeServices.DrawElement(TempBmp.Canvas.Handle, Details, aRect); // Draw tab
      end
      else
{$ENDIF}
      begin // Draw without themes (very od style)
        FillDC(TempBmp.Canvas.Handle, aRect, Color);
        if bTabMenu then
          if Page.TabType = ttMenu then
            case State of
              1, 2: begin
                FillDC(TempBmp.Canvas.Handle, Rect(1, 1, TempBmp.Width - 1, TempBmp.Height - 1), {$IFDEF DELPHI7UP}clMenuHighlight{$ELSE}clHighlight{$ENDIF});
                TempBmp.Canvas.Font.Color := clHighlightText;
              end;
            end
          else
            case State of
              1: Frame3D(TempBmp.Canvas, aRect, ColorToRGB(clWhite),     ColorToRGB(clBtnShadow), 1);
              2: Frame3D(TempBmp.Canvas, aRect, ColorToRGB(clBtnShadow), ColorToRGB(clWhite),     1);
            end
        else
          case Style of
            tsTabs:
              DrawEdge(TempBmp.Canvas.Handle, aRect, EDGE_RAISED, BF_RECT and not AntiPosArray[TabPosition]);

            tsButtons:
              case State of
                0, 1: DrawEdge(TempBmp.Canvas.Handle, aRect, EDGE_RAISED, BF_RECT);
                2:    DrawEdge(TempBmp.Canvas.Handle, aRect, EDGE_SUNKEN, BF_RECT);
              end;

            else begin
              if R.Left > 6 then begin // Draw a separator
                rTmp := Rect(R.Left - 6, R.Top, R.Left, R.Bottom);
                DrawEdge(DC, rTmp, EDGE_ETCHED, BF_LEFT);
              end;
              case State of
                1:
                  Frame3D(TempBmp.Canvas, aRect, ColorToRGB(clWhite), ColorToRGB(clBtnShadow), 1);

                2: begin
                  FillDC(TempBmp.Canvas.Handle, aRect, ColorToRGB(cl3DLight));
                  DrawEdge(TempBmp.Canvas.Handle, aRect, EDGE_SUNKEN, BF_RECT);
                end;
              end;
            end;
          end;
      end;
      if not OwnerDraw then begin
        // Drawing of tab content
        Flags := TextWrapping[WordWrap] or DT_CENTER or DT_VCENTER or DT_END_ELLIPSIS or DT_WORD_ELLIPSIS;
        if UseRightToLeftReading then
          Flags := Flags or DT_RTLREADING or DT_NOCLIP;
{$IFDEF TNTUNICODE}
        if Page is TTntTabSheet then
          lCaption := TTntTabSheet(Page).Caption
        else
{$ENDIF}
          lCaption := Page.Caption;

        OldFont := 0;
        if (TabPosition in [tpLeft, tpRight]) and not RotateCaptions or (TabPosition in [tpTop, tpBottom]) and RotateCaptions then // If vertical text
          OldFont := MakeAngledFont(TempBmp.Canvas.Handle, TempBmp.Canvas.Font, -2700); // Rotated font initialization
        // Get coordinates for tab content
        InitTabContentData(TempBmp.Canvas, Page, aRect, State, bTabMenu, dContent);
        // Draw glyph if rect is not empty
        if not IsRectEmpty(dContent.GlyphRect) then
          Images.Draw(TempBmp.Canvas, dContent.GlyphRect.Left, dContent.GlyphRect.Top, Page.ImageIndex, Page.Enabled and Enabled);
        // Write Text
        if Page.SkinData <> nil then
          if OldFont <> 0 then  // If font is rotated
            SetTextColor(TempBmp.Canvas.Handle, ColorToRGB(iff(Page.SkinData.CustomFont, Page.Font.Color, clBtnText)))
          else
            TempBmp.Canvas.Font.Color := ColorToRGB(iff(Page.SkinData.CustomFont, Page.Font.Color, clBtnText));

        if not Page.Enabled or not Enabled then
          TempBmp.Canvas.Font.Color := BlendColors(TempBmp.Canvas.Font.Color, ColorToRGB(clBtnFace), sDefaults.DefBlendDisabled);

        TempBmp.Canvas.Brush.Style := bsClear;
        if OldFont <> 0 then begin // If font is rotated
          lCaption := CutText(TempBmp.Canvas, lCaption, HeightOf(dContent.TextRect));
          acTextRect(TempBmp.Canvas, dContent.TextRect, dContent.TextPos.X, dContent.TextPos.Y, lCaption);
          SelectObject(TempBmp.Canvas.Handle, OldFont); // Returning prev. font
        end
        else
          acWriteText(TempBmp.Canvas, PacChar(lCaption), True, dContent.TextRect, Flags);

        // Paint focus rect
        if not IsRectEmpty(dContent.FocusRect) then begin
          TempBmp.Canvas.Pen.Color := clWindowFrame;
          TempBmp.Canvas.Brush.Color := clBtnFace;
          TempBmp.Canvas.DrawFocusRect(dContent.FocusRect);
        end;
        // Paint Close btn
        if not IsRectEmpty(dContent.BtnRect) then
          DrawCloseBtn(TempBmp.Canvas.Handle, dContent.BtnRect, integer(FHoveredBtnIndex = PageIndex) + integer(FPressedBtnIndex = PageIndex));
        // Draw Arrow
        if not IsRectEmpty(dContent.ArrowRect) then
          DrawArrow(TempBmp, TempBmp.Canvas.Font.Color, clNone, dContent.ArrowRect, dContent.ArrowDirection, 0, 0, 0, arsSolid1);
      end
      else
        if Assigned(OnDrawTab) then begin
          Canvas.Handle := TempBmp.Canvas.Handle;
          SavedDC := SaveDC(Canvas.Handle);
          Canvas.Lock;
          MoveWindowOrg(Canvas.Handle, -R.Left, -R.Top);
          OnDrawTab(Self, Page.TabIndex, R, State <> 0);
          Canvas.UnLock;
          RestoreDC(Canvas.Handle, SavedDC);
          BitBlt(DC, R.Left, R.Top, TempBmp.Width, TempBmp.Height, TempBmp.Canvas.Handle, 0, 0, SRCCOPY);
        end;

      BitBlt(DC, R.Left, R.Top, TempBmp.Width, TempBmp.Height, TempBmp.Canvas.Handle, 0, 0, SRCCOPY); // Copy Tab to DC
    finally
      TempBmp.Free;
    end;
  end;
end;


procedure TsPageControl.DrawStdTabs(DC: hdc);
var
  i: integer;
begin
  for i := 0 to PageCount - 1 do
    if Pages[i].TabVisible and (Pages[i] <> ActivePage) then
      if DroppedDownItem = i then
        DrawStdTab(i, 2, DC)
      else
        DrawStdTab(i, integer(CurItem = i), DC);

  // Draw active tab
  if (Tabs.Count > 0) and (ActivePage <> nil) and (ActivePage.TabType = ttTab) then
    DrawStdTab(ActivePage.PageIndex, 2, DC);
end;


procedure TsPageControl.FinishTabDragging;
begin
  if acDragging then begin
    ReleaseCapture;
    DraggedItem := -1;
    acDragging := False;
    FreeAndNil(DraggedForm);
    FreeAndNil(DraggedInsForm);
    FreeAndNil(DraggedBmp);
  end;
end;


function TsPageControl.FontStored: boolean;
begin
  Result := IsCustomFont(Self, Font, not SkinData.Skinned or SkinData.CustomFont);
end;


procedure TsPageControl.InitTabContentData(Canvas: TCanvas; Page: TTabSheet; BmpRect: TRect; State: integer; IsTabMenu: boolean; var Data: TacTabData);
var
  b, IsEmpty, IsVertical, bArrowVisible: boolean;
  i, ContentSize, iGlyphSpace, iArrSpace, iCloseSpace, imgWidth, imgHeight: integer;
  lCaption: ACString;
  GlyphSize, CloseSize, ArrowSize: TSize;
  ta: TAlignment;
  R: TRect;

  procedure OffsetContent(LeftDX, RightDX, DY: integer);
  begin
    OffsetRect(Data.BtnRect,    RightDX, DY);
    OffsetRect(Data.ArrowRect,  RightDX, DY);
    OffsetRect(Data.GlyphRect,  LeftDX, DY);
    OffsetRect(Data.TextRect,   LeftDX, DY);
  end;

  function GetBtnRect: TRect;
  begin
    if IsVertical then begin
      Result.Top    := (imgHeight - CloseSize.cx) div 2;
      Result.Right  := imgWidth - BtnOffsX;
      Result.Bottom := Result.Top + CloseSize.cx;
      Result.Left   := Result.Right - CloseSize.cy;
    end
    else begin
      Result.Top    := (imgHeight - CloseSize.cy) div 2;
      Result.Right  := imgWidth - BtnOffsX;
      Result.Bottom := Result.Top + CloseSize.cy;
      Result.Left   := Result.Right - CloseSize.cx;
    end;
  end;

  function GetTextSize(aRect: TRect): TSize;
  begin
    if lCaption <> '' then begin
      R := aRect;
      acDrawText(Canvas.Handle, lCaption, R, DT_CALCRECT or TextWrapping[WordWrap]);
      Result := MkSize(R);
    end
    else
      Result := MkSize(0, 0);
  end;

begin
{$IFDEF TNTUNICODE}
  if Page is TTntTabSheet then
    lCaption := TTntTabSheet(Page).Caption
  else
{$ENDIF}
    lCaption := Page.Caption;

  // Size of glyph
  if (Images <> nil) and IsValidIndex(Page.ImageIndex, GetImageCount(Images)) then begin
    GlyphSize.cx := GetImageWidth(Images);
    GlyphSize.cy := GetImageHeight(Images);
  end
  else
    GlyphSize := MkSize;

  // Flags and variables initializing
  IsEmpty := (lCaption = '') and (GlyphSize.cx = 0);
  bArrowVisible := IsTabMenu and (TsTabSheet(Page).TabType = ttMenu);
  iGlyphSpace := integer((lCaption <> '') and (GlyphSize.cx <> 0)) * TabSpacing;
  iArrSpace := integer(not IsEmpty and bArrowVisible) * acSpacing;

  if bArrowVisible then
    ArrowSize := MkSize(acArrowSize * 2, acArrowSize)
  else
    ArrowSize := MkSize;

  if RotateCaptions then
    IsVertical := TabPosition in [tpTop, tpBottom]
  else
    IsVertical := TabPosition in [tpLeft, tpRight];

  if TsTabSheet(Page).TabType = ttTab then
    if Style = tsTabs then
      ta := TabAlignment
    else
      ta := taCenter
  else
    ta := taCenter;

  // Close btn visibility
  if FShowCloseBtns and not IsTabMenu and TsTabSheet(Page).UseCloseBtn then
    case FCloseBtnVisibility of
      cvActiveTab:    b := State = 2;
      cvUnactiveTabs: b := State <> 2;
      cvMouseHovered: b := False;
      else            b := True;
    end
  else
    b := False;

  // CloseBtn size
  if b then
    CloseSize := MkSize(BtnWidth, BtnHeight)
  else
    CloseSize := MkSize;

  // Additional variables variables
  iCloseSpace := integer(b) * acSpacing;
  if IsVertical then begin
    imgWidth  := HeightOf(BmpRect);
    imgHeight := WidthOf (BmpRect);
    ChangeI(GlyphSize.cx, GlyphSize.cy);
  end
  else begin
    imgWidth  := WidthOf (BmpRect);
    imgHeight := HeightOf(BmpRect);
  end;
  if (State = 2) and (FCommonData.SkinManager <> nil) and (FCommonData.SkinManager.CommonSkinData.Version >= 13) then
    inc(imgHeight);

  // Close btn rect
  Data.BtnRect := GetBtnRect;
  // Arrow rect
  Data.ArrowDirection := asBottom;
  Data.ArrowRect.Left := Data.BtnRect.Left - iCloseSpace - ArrowSize.cx - TabMargin;
//  if ta <> taRightJustify then
//    min(Data.ArrowRect.Left, Data.GlyphRect.Left + iGlyphSpace + Data.TextSize.cx);

  Data.ArrowRect.Right  := Data.ArrowRect.Left + ArrowSize.cx;
  Data.ArrowRect.Top    := (imgHeight - acArrowSize) div 2;
  Data.ArrowRect.Bottom := Data.ArrowRect.Top + acArrowSize;
  // Image rect
  case GlyphLayout of
    glTop: begin
      Data.TextSize := GetTextSize(Rect(0, 0, Data.ArrowRect.Left - TabMargin - acSpacing, 0)); // Size of text
      ContentSize := max(GlyphSize.cy + iGlyphSpace + Data.TextSize.cy, ArrowSize.cy);
      Data.GlyphRect.Left := (Data.BtnRect.Left - GlyphSize.cx) div 2;
      Data.GlyphRect.Top := (imgHeight - ContentSize) div 2;
    end;
    glLeft: begin
      Data.TextSize := GetTextSize(Rect(0, 0, Data.ArrowRect.Left - 2 * TabMargin - acSpacing - GlyphSize.cx + iGlyphSpace, 0)); // Size of text
      ContentSize := GlyphSize.cx + iGlyphSpace + Data.TextSize.cx + iArrSpace + ArrowSize.cx + iCloseSpace + CloseSize.cx;
      case ta of
        taLeftJustify:  Data.GlyphRect.Left := TabMargin;
        taRightJustify: Data.GlyphRect.Left := imgWidth - ContentSize - TabMargin
        else            Data.GlyphRect.Left := max(TabMargin, (imgWidth - ContentSize) div 2);
      end;
      Data.GlyphRect.Top := (imgHeight - GlyphSize.cy) div 2;
    end;
  end;
  Data.GlyphRect.Right := Data.GlyphRect.Left + GlyphSize.cx;
  Data.GlyphRect.Bottom := Data.GlyphRect.Top + GlyphSize.cy;
  // Text rect
  case GlyphLayout of
    glTop: begin
      Data.TextRect.Left := (Data.BtnRect.Left - Data.TextSize.cx) div 2;
      Data.TextRect.Top := Data.GlyphRect.Bottom + iGlyphSpace;
    end;
    glLeft: begin
      Data.TextRect.Left := Data.GlyphRect.Right + iGlyphSpace;
      Data.TextRect.Top := (imgHeight - Data.TextSize.cy) div 2 + (imgHeight - Data.TextSize.cy) mod 2;
    end;
  end;
  Data.TextRect.Right := Data.TextRect.Left + Data.TextSize.cx;
  Data.TextRect.Bottom := Data.TextRect.Top + Data.TextSize.cy;
  // Update close btn for cvMouseHovered
  if FShowCloseBtns and not IsTabMenu and TsTabSheet(Page).UseCloseBtn and not b then begin
    if (State = 1) or (State = 2) and (FCurItem = Page.TabIndex) and (FCloseBtnVisibility = cvMouseHovered) then
      CloseSize := MkSize(BtnWidth, BtnHeight);
      Data.BtnRect := GetBtnRect;
    end;
  // Correction of text rect
  if CloseSize.cx <> 0 then
    if Data.TextRect.Right > Data.BtnRect.Left - acSpacing then
      Data.TextRect.Right := Data.BtnRect.Left - acSpacing;

  // Update active tab offsets
  if State = 2 then
    case TsTabSheet(Page).TabType of
      ttTab:
        if FActiveTabEnlarged then begin
          case ta of // Correction of tab rect
            taLeftJustify:  OffsetContent(2, -2, 0);
            taRightJustify: OffsetContent(-2, 2, 0);
            else            OffsetRect(Data.BtnRect, -2, 0);
          end;
          if not RotateCaptions and (Style = tsTabs) then begin
            i := 1 + integer(CanClickShift(GetTabSkinIndex(TsTabSheet(Page)), SkinData.SkinManager));
            if not IsLeftToRight then
              OffsetContent(0, 0, acMinusPlus[TabPosition in [tpBottom,  tpLeft]] * i)
            else
              OffsetContent(0, 0, acMinusPlus[TabPosition in [tpBottom, tpRight]] * i);
          end;
        end;
    end;
  // Mirror rects
  if not IsLeftToRight then
    if not IsVertical then begin
      ReflectRect(Data.BtnRect,   BmpRect, True);
      ReflectRect(Data.GlyphRect, BmpRect, True);
      ReflectRect(Data.TextRect,  BmpRect, True);
      ReflectRect(Data.ArrowRect, BmpRect, True);
    end;
  // Focus rect
  if FShowFocus and (Focused or FCommonData.FFocused) and (State = 2) and (Data.TextSize.cx <> 0) and not IsTabMenu then begin
    Data.FocusRect := Data.TextRect;
    InflateRect(Data.FocusRect, 3, 0);
    inc(Data.FocusRect.Bottom, 3);
  end
  else
    Data.FocusRect := MkRect;
  // Rotate rects
  if IsVertical then begin
    Data.BtnRect   := RotateRect(Data.BtnRect,   False, imgWidth);
    Data.GlyphRect := RotateRect(Data.GlyphRect, False, imgWidth);
    Data.TextRect  := RotateRect(Data.TextRect,  False, imgWidth);
    Data.ArrowRect := RotateRect(Data.ArrowRect, False, imgWidth);
  end;
  // Shift content
  if State = 2 then
    case TsTabSheet(Page).TabType of
      ttButton, ttMenu:
        if CanClickShift(GetTabSkinIndex(TsTabSheet(Page)), SkinData.SkinManager) then
          OffsetContent(1, 1, 1);
    end;

  if IsVertical then begin
    Data.TextPos.X := Data.TextRect.Left + (WidthOf(Data.TextRect) - Data.TextSize.cy) div 2;
    Data.TextPos.Y := Data.TextRect.Bottom;
  end
  else begin
    Data.TextPos.X := Data.TextRect.Left;
    Data.TextPos.Y := Data.TextRect.Top;
  end;
end;


procedure TsPageControl.SetHoveredBtnIndex(const Value: integer);
begin
  FHoveredBtnIndex := Value;
end;


procedure TsPageControl.SetInt(const Index, Value: integer);
begin
  case Index of
    0: if FTabMargin <> Value then begin
      FTabMargin := Value;
      SkinData.Invalidate(True);
    end;

    1: if FTabPadding <> Value then begin
      FTabPadding := Value;
      if not (csLoading in ComponentState) then begin
        UpdateBtnData;
        if HandleAllocated then
          Perform(WM_SIZE, 0, 0);

        FCommonData.Invalidate;
      end;
    end;

    2: if FTabSpacing <> Value then begin
      FTabSpacing := Value;
      SkinData.Invalidate(True);
    end;
  end;
end;


function TsPageControl.GetActivePageIndex: Integer;
begin
  Result := inherited ActivePageIndex;
end;


function TsPageControl.GetInsTab: integer;
var
  R, wR: TRect;
  p: TPoint;
begin
  if DraggedItem >= 0 then begin
    GetWindowRect(Handle, wR);
    p := ScreenToClient(acMousePos);
    Result := GetTabUnderMouse(p);

    if Result >= 0 then begin
      if Result = DraggedItem then
        Result := -1
      else begin
        R := StdTabRect(Result);
        if TabPosition in [tpTop, tpBottom] then begin
          if p.X > R.Left + WidthOf(R) div 2 then begin
            if Result < DraggedItem then
              inc(Result);
          end
          else
            if Result > DraggedItem then
              dec(Result);
        end
        else
          if p.Y > R.Top + HeightOf(R) div 2 then begin
            if Result < DraggedItem then
              inc(Result);
          end
          else
            if Result > DraggedItem then
              dec(Result);

        if (Result = DraggedItem) or (Result >= Self.PageCount) or not Pages[Result].TabVisible then
          Result := -1;
      end;
    end;
  end
  else
    Result := -1;
end;


procedure TsPageControl.SetActivePageIndex(const Value: Integer);
begin
  if IsValidIndex(Value, PageCount) and (TsTabSheet(Pages[Value]).TabType = ttTab) then begin
    inherited ActivePageIndex := Value;
    if not Pages[Value].TabVisible then
      SetParentUpdated(Pages[Value]); // Update because TCM_SETCURSEL is not received
  end;
end;


procedure TsPageControl.SetBool(const Index: Integer; const Value: boolean);
var
  R: TRect;
begin
  case Index of
    0: if FActiveIsBold <> Value then begin
      FActiveIsBold := Value;
      if not (csLoading in ComponentState) and (Visible or (csDesigning in ComponentState)) then begin
        SkinData.BGChanged := True;
        RedrawWindow(Handle, nil, 0, RDWA_REPAINT);
      end;
    end;

    1: if FReflectedGlyphs <> Value then begin
      FReflectedGlyphs := Value;
      SkinData.Invalidate(True);
    end;

    2: if FRotateCaptions <> Value then begin
      FRotateCaptions := Value;
      if not (csLoading in ComponentState) and (Visible or (csDesigning in ComponentState)) then begin
        Perform(WM_SETREDRAW, 0, 0);
        if FRotateCaptions then begin
          if PageCount > 0 then
            R := StdTabRect(0)
          else
            R := MkRect(24, 24);

          if TabPosition in [tpTop, tpBottom] then begin
            TabWidth := HeightOf(R);
            TabHeight := max(WidthOf(R), 140);
          end
          else begin
            TabWidth := WidthOf(R);
            TabHeight := max(HeightOf(R), 140);
          end;
        end
        else begin
          TabHeight := 0;
          TabWidth := 0;
        end;
        SkinData.BGChanged := True;
        Perform(WM_SETREDRAW, 1, 0);
        RedrawWindow(Handle, nil, 0, RDWA_ALL);
      end;
    end;

    3: if FShowCloseBtns <> Value then begin
      FShowCloseBtns := Value;
      UpdateBtnData;
      if HandleAllocated and not (csLoading in ComponentState) then begin
        KillTimers;
        Perform(WM_SIZE, 0, 0);
        if SkinData.Skinned then
          RedrawWindow(Handle, nil, 0, RDW_ERASE or RDW_INVALIDATE or RDW_UPDATENOW);
      end;
    end;

    4: if FShowFocus <> Value then begin
      FShowFocus := Value;
      SkinData.Invalidate;
    end;

    5: FAllowAnimSwitching := Value;

    6: if FActiveTabEnlarged <> Value then begin
      FActiveTabEnlarged := Value;
      SkinData.Invalidate;
    end;

    7: FAllowTabsDrag := Value;

    8: if FWordWrap <> Value then begin
      FWordWrap := Value;
      SkinData.Invalidate;
    end;
  end;
end;


function TsPageControl.PageIndexFromTabIndex(TabIndex: Integer): Integer;
var
  i, j: integer;
begin
  Result := -1;
  j := -1;
  for i := 0 to min(TabIndex, PageCount - 1) do begin
    inc(Result);
    if Pages[i].TabVisible then
      inc(j);

    if TabIndex = j then
      Exit;
  end;
end;


procedure TsPageControl.SetCurItem(const Value: integer);
var
  Old: integer;
begin
  if FCurItem <> Value then begin
    Old := FCurItem;
    FCurItem := Value;
    if DroppedDownItem < 0 then begin
      if IsValidIndex(Value, PageCount) then
        if (Pages[Value] <> ActivePage) or ShowCloseBtns then
          RepaintTab(Value); // Repaint new tab

      if IsValidIndex(Old, PageCount) then
        if (Pages[Old] <> ActivePage) or ShowCloseBtns then
          RepaintTab(Old); // Repaint old tab in normal state
    end;
  end;
end;


procedure TsPageControl.SetGlyphLayout(const Value: TacGlyphLayout);
begin
  if FGlyphLayout <> Value then begin
    FGlyphLayout := Value;
    KillTimers;
    SkinData.Invalidate(True);
  end;
end;


function TsPageControl.PrepareCache: boolean;
var
  R: TRect;
  i: integer;
  CI: TCacheInfo;
begin
  InitCacheBmp(SkinData);
  Result := True;
  CI := GetParentCache(FCommonData);
  R := PageRect;
  PaintItemBG(FCommonData, CI, 0, R, Point(Left + R.Left, Top + R.Top), FCommonData.FCacheBmp);
  with FCommonData.SkinManager.gd[SkinData.SkinIndex], FCommonData.SkinManager do begin
    if IsValidImgIndex(ImgTL) then
      DrawSkinGlyph(FCommonData.FCacheBmp, Point(R.Left, R.Top), 0, 1, ma[ImgTL], MakeCacheInfo(FCommonData.FCacheBmp));

    if IsValidImgIndex(ImgTR) then
      DrawSkinGlyph(FCommonData.FCacheBmp, Point(R.Right - ma[ImgTR].Width, R.Top), 0, 1, ma[ImgTR], MakeCacheInfo(FCommonData.FCacheBmp));

    if IsValidImgIndex(ImgBL) then
      DrawSkinGlyph(FCommonData.FCacheBmp, Point(0, R.Bottom - ma[ImgBL].Height), 0, 1, ma[ImgBL], MakeCacheInfo(FCommonData.FCacheBmp));

    if IsValidImgIndex(ImgBR) then
      DrawSkinGlyph(FCommonData.FCacheBmp, Point(R.Right - ma[ImgBR].Width, R.Bottom - ma[ImgBR].Height), 0, 1, ma[ImgBR], MakeCacheInfo(FCommonData.FCacheBmp));
  end;
  if FCommonData.BorderIndex >= 0 then
    DrawSkinRect(FCommonData.FCacheBmp, R, CI, FCommonData.SkinManager.ma[FCommonData.BorderIndex], 0, True);

  if (ActivePage <> nil) and not (csDestroying in ActivePage.ComponentState) and (ActivePage.SkinData.SkinSection <> '') then begin
    i := FCommonData.SkinManager.GetSkinIndex(ActivePage.SkinData.SkinSection);
    if FCommonData.SkinManager.IsValidSkinIndex(i) then begin
      R := ActivePage.BoundsRect;
      PaintItem(i, MakeCacheInfo(FCommonData.FCacheBmp), True, 0, R, MkPoint, FCommonData.FCacheBmp, FCommonData.SkinManager);
    end;
  end;
  R := PageRect;
  if Tabs.Count > 0 then
    DrawSkinTabs(FCommonData.FCacheBmp);

  if ActivePage <> nil then
    SkinData.PaintOuterEffects(ActivePage, Point(ActivePage.Left + ActivePage.BorderWidth, ActivePage.Top + ActivePage.BorderWidth));

  FCommonData.BGChanged := False;
end;


procedure TsPageControl.SetTabAlignment(const Value: TAlignment);
begin
  if FTabAlignment <> Value then begin
    FTabAlignment := Value;
    KillTimers;
    SkinData.Invalidate(True);
  end;
end;


procedure TsPageControl.SetTabsLineSkin(const Value: TsSkinSection);
begin
  if FTabsLineSkin <> Value then begin
    FTabsLineSkin := Value;
    if SkinData.SkinManager <> nil then
      FTabsLineIndex := SkinData.SkinManager.GetSkinIndex(FTabsLineSkin);

    FCommonData.Invalidate;
  end;
end;


procedure TsTabSheet.ChangeScale(M, D: Integer);
begin
{$IFDEF SKIPSCALE}
  if not TsPageControl(PageControl).DontScale then
{$ENDIF}
    inherited;
end;


constructor TsTabSheet.Create(AOwner: TComponent);
begin
  FCommonData := TsTabSkinData.Create;
  inherited;
  FCommonData.FPage := Self;
  FUseCloseBtn := True;
  FTabType := ttTab;
end;


procedure TsTabSheet.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params.WindowClass do
    Style := Style and not (CS_HREDRAW or CS_VREDRAW);
end;


destructor TsTabSheet.Destroy;
begin
  if AnimTimer <> nil then
    FreeAndNil(AnimTimer);

  FreeAndNil(FCommonData);
  inherited;
end;


function TsTabSheet.FontStored: boolean;
begin
  if (PageControl = nil) then
    Result := IsCustomFont(Self, Font)
  else
    Result := IsCustomFont(Self, Font, not TsPageControl(PageControl).SkinData.Skinned or TsPageControl(PageControl).SkinData.CustomFont);
end;


procedure TsTabSheet.SetButtonSkin(const Value: TsSkinSection);
begin
  if FButtonSkin <> Value then begin
    FButtonSkin := Value;
    if PageControl <> nil then
      TsPageControl(PageControl).SkinData.Invalidate;
  end;
end;


procedure TsTabSheet.SetTabMenu(const Value: TPopupMenu);
begin
  if FTabMenu <> Value then
    FTabMenu := Value;
end;


procedure TsTabSheet.SetTabSkin(const Value: TsSkinSection);
begin
  if FTabSkin <> Value then begin
    FTabSkin := Value;
    if (PageControl <> nil) and not (csLoading in PageControl.ComponentState) and (Visible or (csDesigning in ComponentState)) then begin
      TsPageControl(PageControl).SkinData.BGChanged := True;
      RedrawWindow(PageControl.Handle, nil, 0, RDW_ERASE or RDW_FRAME or RDW_INVALIDATE);
    end;
  end;
end;


procedure TsTabSheet.SetTabType(const Value: TacTabType);
begin
  if FTabType <> Value then begin
    FTabType := Value;
    if TabSkin = '' then
      case Value of
        ttMenu:   TabSkin := s_MenuButton;
        ttButton: TabSkin := s_SpeedButton;
      end
    else
      if Value = ttTab then
        TabSkin := '';

    if PageControl <> nil then begin
      TsPageControl(PageControl).UpdateBtnData;
      Perform(CM_TEXTCHANGED, 0, 0);
      inherited PageControl.ActivePage := TsPageControl(PageControl).CheckActiveTab(PageControl.ActivePage.PageIndex);
    end;
  end;
end;


procedure TsTabSheet.SetUseCloseBtn(const Value: boolean);
begin
  if FUseCloseBtn <> Value then begin
    FUseCloseBtn := Value;
    if (PageControl <> nil) and not (csLoading in PageControl.ComponentState) then
      TsPageControl(PageControl).RepaintTab(TabIndex, False);
  end;
end;


procedure TsTabSheet.WMEraseBkGnd(var Message: TWMPaint);
begin
  if not (csDestroying in ComponentState) and (PageControl <> nil) and not (csDestroying in PageControl.ComponentState) and
       TsPageControl(PageControl).SkinData.Skinned and Showing then
    with TsPageControl(PageControl).SkinData do begin
{$IFDEF D2005}
      if csDesigning in PageControl.ComponentState then
        inherited; // Fixing of designer issue
{$ENDIF}
      FUpdating := Updating;
      if (Message.DC = 0) or (CtrlSkinState and ACS_PRINTING <> 0) and (Message.DC <> PrintDC) then
        Exit;

      if not FUpdating then begin
        if BGChanged then
          TsPageControl(PageControl).PrepareCache;

        CopyWinControlCache(Self, TsPageControl(PageControl).SkinData, Rect(Left + BorderWidth, Top + BorderWidth, 0, 0), MkRect(Self), Message.DC, False);
        sVCLUtils.PaintControls(Message.DC, Self, True, MkPoint);
        SetParentUpdated(Self);
      end;
      Message.Result := 1;
    end
  else inherited;
end;


procedure TsTabSheet.WMNCPaint(var Message: TWMPaint);
var
  DC: hdc;
begin
  if not (csDestroying in ComponentState) and (BorderWidth > 0) and Showing and not InAnimationProcess then begin
    if TsPageControl(PageControl).SkinData.Skinned then begin
      TsPageControl(PageControl).SkinData.Updating := TsPageControl(PageControl).SkinData.Updating;
      if not TsPageControl(PageControl).SkinData.FUpdating then begin
        if (TsPageControl(PageControl).SkinData.CtrlSkinState and ACS_PRINTING <> 0) and (Message.DC = TsPageControl(PageControl).SkinData.PrintDC) then
          DC := Message.DC
        else
          DC := GetWindowDC(Handle);

        BitBltBorder(DC, 0, 0, Width, Height, TsPageControl(PageControl).SkinData.FCacheBmp.Canvas.Handle, Left, Top, BorderWidth);
        if DC <> Message.DC then
          ReleaseDC(Handle, DC);
      end;
    end
    else begin
{$IFDEF DELPHI7UP}
      if acThemesEnabled then
        inherited
      else
{$ENDIF}
      begin
        DC := GetWindowDC(Handle);
        FillDCBorder(DC, MkRect(Self), BorderWidth, BorderWidth, BorderWidth, BorderWidth, Color);
        if DC <> Message.DC then
          ReleaseDC(Handle, DC);
      end;
    end;
    Message.Result := 1;
  end;
end;


procedure TsTabSheet.WndProc(var Message: TMessage);
var
  i: integer;
  b: boolean;
  PS: TPaintStruct;
begin
{$IFDEF LOGGED}
//  AddToLog(Message);
{$ENDIF}
  if PageControl <> nil then
    if Message.Msg = SM_ALPHACMD then
      case Message.WParamHi of
        AC_PRINTING, AC_CTRLHANDLED: begin
          Message.Result := 1;
          Exit
        end;

        AC_GETAPPLICATION: begin
          Message.Result := LRESULT(Application);
          Exit
        end;

        AC_REMOVESKIN: begin
          if Message.LParam = LPARAM(TsPageControl(PageControl).SkinData.SkinManager) then
            Repaint;

          if HandleAllocated then
            SetClassLong(Handle, GCL_HBRBACKGROUND, Integer(GetSysColorBrush(COLOR_BTNFACE)));

          AlphaBroadCast(Self, Message);
        end;

        AC_FONTSCHANGED, AC_SETNEWSKIN:
          AlphaBroadCast(Self, Message);

        AC_REFRESH: begin
          if (Message.LParam = LPARAM(TsPageControl(PageControl).SkinData.SkinManager)) and (Visible or (csDesigning in ComponentState)) then
            Repaint;

          AlphaBroadCast(Self, Message);
    		  if (TsPageControl(PageControl).SkinData.SkinManager <> nil) and TsPageControl(PageControl).SkinData.SkinManager.CommonSkinData.Active and HandleAllocated then
          	SetClassLong(Handle, GCL_HBRBACKGROUND, Integer(TsPageControl(PageControl).SkinData.SkinManager.Brushes[pcMainColor]));
        end;

        AC_GETBG:
          if TsPageControl(PageControl).SkinData.Skinned then
            with PacBGInfo(Message.LParam)^ do begin
              if (SkinData <> nil) and (SkinData.SkinSection <> '') and not TsPageControl(PageControl).SkinData.FCacheBmp.Empty then begin
                BgType := btCache;
                Bmp := TsPageControl(PageControl).SkinData.FCacheBmp;
                Offset := MkPoint;
              end
              else begin
                CommonMessage(Message, TsPageControl(PageControl).SkinData);
                if BgType = btNotReady then begin
                  TsPageControl(PageControl).SkinData.Updating := True;
                  Exit;
                end;
                FillRect := MkRect;
              end;
              if (Bmp <> nil) and not PleaseDraw then begin
                Offset.X := Offset.X + Left + BorderWidth;
                Offset.Y := Offset.Y + Top  + BorderWidth;
                if BgType = btFill then begin
                  FillRect.Left := Offset.X - FillRect.Left;
                  FillRect.Top  := Offset.Y - FillRect.Top;
                end;
              end;
              Exit;
            end;

        AC_GETCONTROLCOLOR: begin
          if TsPageControl(PageControl).SkinData.Skinned then
            Message.Result := PageControl.Perform(SM_ALPHACMD, AC_GETCONTROLCOLOR_HI, 0)
          else
            Message.Result := TacAccessControl(PageControl).Color;

          Exit;
        end;

        AC_PREPARING:
          if TsPageControl(PageControl).SkinData.Skinned then begin
            Message.Result := LRESULT(TsPageControl(PageControl).SkinData.Updating);
            Exit;
          end;

        AC_CHILDCHANGED:
          with TsPageControl(PageControl).SkinData do
            if Skinned then
              with SkinManager.gd[SkinIndex].Props[0] do begin
                Message.Result := LRESULT((GradientPercent + ImagePercent > 0) or RepaintIfMoved);
                Exit;
              end;

        AC_GETSKININDEX:
          with PacSectionInfo(Message.LParam)^, TsPageControl(PageControl) do begin
            if (SkinData <> nil) and (SkinData.SkinSection <> '') then begin
              siSkinIndex := SkinData.SkinManager.GetSkinIndex(SkinData.SkinSection);
              if siSkinIndex < 0 then
                siSkinIndex := SkinData.SkinIndex;
            end
            else
              siSkinIndex := SkinData.SkinIndex;

            Exit
          end;

        AC_ENDPARENTUPDATE: begin
          RedrawWindow(Handle, nil, 0, RDWA_ERASENOW);
          SetParentUpdated(Self);
          Exit;
        end;

        AC_GETDEFINDEX: begin
          if TsPageControl(PageControl).SkinData.SkinManager <> nil then
            Message.Result := TsPageControl(PageControl).SkinData.SkinManager.GetSkinIndex(s_TabSheet) + 1;

          Exit;
        end;

        AC_GETFONTINDEX:
          with TsPageControl(PageControl), PacPaintInfo(Message.LParam)^ do begin
            if SkinData.Skinned then
              with SkinData.SkinManager.gd[SkinData.SkinIndex] do
                if NeedParentFont(SkinData, State) then begin
                  inc(R.Left, SkinData.FOwnerControl.Left);
                  inc(R.Top,  SkinData.FOwnerControl.Top);
                  Message.Result := GetFontIndex(SkinData.FOwnerControl.Parent, PacPaintInfo(Message.LParam))
                end
                else begin
                  FontIndex := TsPageControl(PageControl).SkinData.SkinIndex;
                  Message.Result := 1;
                end;

            Exit;
          end;

          AC_SETCHANGEDIFNECESSARY:
            with TsPageControl(PageControl).SkinData do begin
              b := RepaintIfMoved;
              BGChanged := BGChanged or b;
              if FOwnerControl is TWinControl then begin
                if Message.WParamLo = 1 then
                  RedrawWindow(Handle, nil, 0, RDW_NOERASE or RDW_NOINTERNALPAINT or RDW_INVALIDATE or RDW_ALLCHILDREN);

                if b then
                  for i := 0 to ControlCount - 1 do
                    if (i < ControlCount) and not (csDestroying in Controls[i].ComponentState) then
                      Controls[i].Perform(SM_ALPHACMD, AC_SETCHANGEDIFNECESSARY shl 16 + 1, 0);
              end;
              Exit;
            end;

          AC_SETSCALE: begin
            AlphaBroadCast(TWinControl(Self), Message);
            Exit;
          end;
      end
    else
      if TsPageControl(PageControl).SkinData.Skinned then
        case Message.Msg of
          WM_MOUSEMOVE:
            if not (csDesigning in ComponentState) then
              if (DefaultManager <> nil) and not (csDesigning in DefaultManager.ComponentState) then
                DefaultManager.ActiveControl := Handle;

          WM_PARENTNOTIFY:
            if ([csDesigning, csLoading] * ComponentState = []) and (Message.WParamLo in [WM_CREATE, WM_DESTROY]) then begin
              inherited;
              if Message.WParamLo = WM_CREATE then begin
                AddToAdapter(Self);
              end;
              Exit;
            end;

          WM_PAINT:
            if Visible or (csDesigning in ComponentState) then begin
              if not (csDestroying in ComponentState) and (Parent <> nil) then // Background update
                InvalidateRect(Handle, nil, True); // Background update (for repaint of graphic controls and for tabsheets refreshing)

              BeginPaint(Handle, PS);
              EndPaint(Handle, PS);
              Message.Result := 0;
              Exit
            end;

          WM_PRINT: begin
            WMEraseBkGnd(TWMPaint(Message));
            Message.Result := 0;
            Exit
          end;

          CM_TEXTCHANGED:
            TsPageControl(PageControl).SkinData.BGChanged := True;
        end;

  inherited;
  if (PageControl <> nil) and TsPageControl(PageControl).SkinData.Skinned then
    case Message.Msg of
      CM_VISIBLECHANGED:
        if (TsPageControl(PageControl).SkinData.SkinManager.Options.OptimizingPriority = opMemory) and not (csDestroying in ComponentState) then
          if not Visible then begin
            for i := 0 to ControlCount - 1 do
              if (Controls[i] is TWinControl) and TWinControl(Controls[i]).HandleAllocated then
                SendAMessage(TWinControl(Controls[i]).Handle, AC_CLEARCACHE)
          end
          else begin
            with TsPageControl(PageControl).SkinData.SkinManager do
              if not (AnimEffects.PageChange.Active and TsPageControl(PageControl).AllowAnimSwitching and Effects.AllowAnimation) then
                RedrawWindow(Parent.Handle, nil, 0, RDWA_ALLNOW);

            SetParentUpdated(Handle);
          end;

      WM_SIZE:
        if IsWindowVisible(Handle) then
          CommonWndProc(Message, TsPageControl(PageControl).SkinData);

      CM_TEXTCHANGED:
        TsPageControl(PageControl).KillTimers;

      CM_ENABLEDCHANGED:
        if ([csDestroying, csLoading] * ComponentState = []) and (Parent <> nil) then begin
          TsPageControl(Parent).SkinData.BGChanged := True;
          RedrawWindow(Parent.Handle, nil, 0, RDWA_REPAINT);
        end;
    end;
end;


procedure TsTabSkinData.SetCustomFont(const Value: boolean);
begin
  FCustomFont := Value;
  if (FPage <> nil) and (FPage.PageControl <> nil) and not (csLoading in FPage.PageControl.ComponentState) then begin
    TsPageControl(FPage.PageControl).SkinData.BGChanged := True;
    RedrawWindow(FPage.PageControl.Handle, nil, 0, RDWA_FRAMENOW);
  end;
end;


procedure TsTabSkinData.SetSkinSection(const Value: string);
begin
  if FSkinSection <> Value then begin
    FSkinSection := Value;
    if FPage.PageControl <> nil then
      TsPageControl(FPage.PageControl).SkinData.Invalidate;
  end;
end;


constructor TsTabBtn.Create(AOwner: TComponent);
begin
  inherited;
  Flat := True;
  UpdateGlyph;
end;


procedure TsTabBtn.Paint;
{$IFDEF DELPHI7UP}
var
  PaintRect: TRect;
  Button: TThemedWindow;
  Details: TThemedElementDetails;
{$ENDIF}
begin
{$IFDEF DELPHI7UP}
  if acThemesEnabled then begin
    if FState in [bsDown, bsExclusive] then
      Button := twSmallCloseButtonPushed
    else
      if MouseInControl then
        Button := twSmallCloseButtonHot
      else
        Button := twSmallCloseButtonNormal;

    Details := acThemeServices.GetElementDetails(Button);
    PerformEraseBackground(Self, Canvas.Handle);
    PaintRect := MkRect(Width, Height);
    acThemeServices.DrawElement(Canvas.Handle, Details, PaintRect);
  end
  else
{$ENDIF}
    inherited
end;


procedure TsTabBtn.UpdateGlyph;
begin
  Caption := 'X';
  Font.Style := [fsBold];
  Font.Color := clRed;
end;


function TsPageControl.GetTabLayout(PageIndex: Integer): TacTabLayout;
begin
  if (SkinData.SkinManager <> nil) and (SkinData.SkinManager.ConstData.Tabs[tlFirst][acTabSides[TabPosition]].SkinIndex >= 0) then
    if GetNeighborIndex(PageIndex, False) < 0 then
      Result := tlFirst
    else
      if GetNeighborIndex(PageIndex, True) < 0 then
        Result := tlLast
      else
        Result := tlMiddle
  else
    Result := tlSingle;
end;


function TsPageControl.GetTabSkinIndex(Page: TsTabSheet): integer;
var
  tp: TTabPosition;
  TabLayout: TacTabLayout;
begin
  if SkinData.Skinned then begin
    with SkinData.SkinManager, ConstData do
      if IsSpecialTab(Page.PageIndex) then begin
        if Page.TabSkin <> '' then
          Result := GetSkinIndex(Page.TabSkin)
        else
          Result := Sections[ssMenuButton];

        if Result < 0 then
          if Page.TabType = ttMenu then
            Result := Sections[ssMenuItem]
          else
            Result := Sections[ssSpeedButton];
      end
      else
        if Page.TabSkin <> '' then
          Result := GetSkinIndex(Page.TabSkin)
        else
          case Style of
            tsTabs: begin
              TabLayout := GetTabLayout(Page.PageIndex);
              tp := TabPosition;
              if not IsLeftToRight then
                case TabPosition of
                  tpLeft: tp := tpRight;
                  tpRight: tp := tpLeft;
                end;

              Result := Tabs[TabLayout][acTabSides[tp]].SkinIndex;
              if Result < 0 then
                Result := Tabs[tlSingle][acTabSides[tp]].SkinIndex;
            end;

            tsButtons:
              Result := Sections[ssButton]

            else
              Result := Sections[ssToolButton];
          end;
  end
  else
    Result := -1;
end;


function TsPageControl.GetNeighborIndex(PageIndex: Integer; Next: boolean): integer;
var
  i: integer;
begin
  Result := -1;
  if Next then
    for i := PageIndex + 1 to PageCount - 1 do begin
      if Pages[i].TabVisible and (TsTabSheet(Pages[i]).TabType = ttTab) then begin
        Result := i;
        Exit;
      end;
    end
  else
    for i := PageIndex - 1 downto 0 do
      if Pages[i].TabVisible and (TsTabSheet(Pages[i]).TabType = ttTab) then begin
        Result := i;
        Exit;
      end;
end;


function TsPageControl.GetPage(Index: Integer): TsTabSheet;
begin
  Result := TsTabSheet(inherited Pages[Index]);
end;


procedure TsPageControl.ChangeScale(M, D: Integer);
begin
{$IFDEF SKIPSCALE}
  DontScale := True; // Do not scale twice in the Delph Berlin
{$ENDIF}
  inherited;
{$IFDEF SKIPSCALE}
  DontScale := False;
{$ENDIF}
{$IFNDEF DELPHI_10}
  TabHeight := MulDiv(TabHeight, M, D);
  TabWidth  := MulDiv(TabWidth,  M, D);
{$ENDIF}
  UpdatePadding;
end;


procedure TacPageMargins.AssignTo(Dest: TPersistent);
begin
  if Dest is TacPageMargins then
    with TacPageMargins(Dest) do begin
      FOffsetLeft := Self.FOffsetLeft;
      FOffsetTop := Self.FOffsetTop;
      FOffsetRight := Self.FOffsetRight;
      FOffsetBottom := Self.FOffsetBottom;
    end
  else
    inherited;
end;


constructor TacPageMargins.Create(Control: TControl);
begin
  inherited Create;
  FOwner := TsPageControl(Control);
  InitDefaults(Self);
end;


class procedure TacPageMargins.InitDefaults(Margins: TacPageMargins);
begin
  with Margins do begin
    FOffsetLeft   := 0;
    FOffsetRight  := 0;
    FOffsetTop    := 0;
    FOffsetBottom := 0;
  end;
end;


procedure TacPageMargins.SetMargin(Index, Value: integer);

  procedure ChangeProp(var Prop: integer; Value: integer);
  begin
    if Prop <> Value then begin
      Prop := Value;
      if not (csLoading in FOwner.ComponentState) then
        SendMessage(FOwner.Handle, WM_SIZE, 0, 0);
    end;
  end;

begin
  case Index of
    0: ChangeProp(FOffsetLeft,   Value);
    1: ChangeProp(FOffsetTop,    Value);
    2: ChangeProp(FOffsetRight,  Value);
    3: ChangeProp(FOffsetBottom, Value);
  end;
end;


function TacDropBtn.CanFocus: Boolean;
begin
  Result := False;
end;


procedure TacDropBtn.Click;
var
  R: TRect;
  i: integer;
  Item: TacMenuItem;
begin
  Menu.Items.Clear;
  Menu.Images := FOwner.Images;
  GetWindowRect(Handle, R);
  for i := 0 to FOwner.PageCount - 1 do
    if FOwner.Pages[i].TabVisible and (FOwner.Pages[i].TabType = ttTab) then begin
      Item := TacMenuItem.Create(Menu);
      Item.Caption := FOwner.Pages[i].Caption;
      Item.ImageIndex := FOwner.Pages[i].ImageIndex;
      Item.OnClick := ItemClick;
      Item.RadioItem := True;
      Item.Name := 'DropItem' + IntToStr(i);
      Item.Tag := i;
      Item.GroupIndex := 1;
      if FOwner.Pages[i] = FOwner.ActivePage then begin
        Item.Checked := True;
        Item.Default := True;
      end;
      Item.Enabled := FOwner.Pages[i].Enabled or FOwner.AccessibleDisabledPages;
      Menu.Items.Add(Item);
    end;

  if SkinData.Skinned then
    SkinData.SkinManager.SkinableMenus.HookPopupMenu(Menu, True);

  if (SkinData.SkinManager <> nil) and SkinData.SkinManager.CommonSkinData.Active then
    SkinData.SkinManager.SkinableMenus.HookPopupMenu(Menu, True);

  if FOwner.IsLeftToRight then
    Menu.Popup(R.Left, R.Top + Height)
  else
    Menu.Popup(R.Left + Width, R.Top + Height);

  Windows.SetFocus(0);
end;


constructor TacDropBtn.Create(AOwner: TComponent);
begin
  inherited;
  FOwner := TsPageControl(AOwner);
  SkinData.SkinSection := s_UpDown;
  TabStop := False;
  ShowFocus := False;
  OnPaint := PaintGlyph;
  Menu := TPopupMenu.Create(Application);
end;


function TacDropBtn.CurrentState: integer;
begin
  Result := inherited CurrentState;
  if (Result = 1) and not SkinData.FMouseAbove then
    Result := 0;
end;


destructor TacDropBtn.Destroy;
begin
//  Menu.Free;
  inherited;
end;


function TacDropBtn.GetBtnBounds: TRect;
var
  Wnd: THandle;
  UpDownRect, ParentRect: TRect;
begin
  Wnd := FindWindowEx(FOwner.Handle, 0, UPDOWN_CLASS, nil);
  if (Wnd <> 0) and IsWindowVisible(Wnd) then begin
    GetWindowRect(Wnd, UpDownRect);
    GetWindowRect(Parent.Handle, ParentRect);
    OffsetRect(UpDownRect, -ParentRect.Left, -ParentRect.Top);
    Result := UpDownRect;
    Result.Bottom := HeightOf(UpDownRect);
    Result.Right := Result.Bottom;
    if FOwner.IsLeftToRight then
      Result.Left := UpDownRect.Left - Result.Right
    else
      Result.Left := UpDownRect.Right;
  end
  else begin
    Visible := False;
    Result := MkRect;
  end;
end;


procedure TacDropBtn.ItemClick(Sender: TObject);
begin
//  if FOwner.Pages[TMenuItem(Sender).Tag].TabType then
  FOwner.ActivePageIndex := TMenuItem(Sender).Tag;
end;


procedure TacDropBtn.PaintGlyph(Sender: TObject; Bmp: Graphics.TBitmap);
var
  C: TColor;
begin
  if SkinData.Skinned then begin
    C := SkinData.SkinManager.gd[SkinData.SkinIndex].Props[CurrentState].FontColor.Color;
    DrawArrow(Bmp, C, clNone, MkRect(Bmp), asBottom, 0, 0, 0, SkinData.SkinManager.Options.ActualArrowStyle)
  end
  else
    DrawArrow(Bmp, clBtnFace, clNone, MkRect(Bmp), asBottom, 0, 0, 0, arsSolid1)
end;


procedure TacDropBtn.UpdatePosition;
var
  R: TRect;
begin
  Parent := FOwner;
  R := GetBtnBounds;
  if R.Right <> 0 then begin
    SetBounds(R.Left, R.Top, R.Bottom, R.Bottom);
    Visible := True;
  end
  else
    Visible := False;
end;


procedure TacDropBtn.WndProc(var Message: TMessage);
var
  R: TRect;
begin
{$IFDEF LOGGED}
//  AddToLog(Message);
{$ENDIF}
  inherited;
  case Message.Msg of
    WM_WINDOWPOSCHANGING: begin
      R := GetBtnBounds;
      TWMWindowPosChanging(Message).WindowPos.y := R.Top;
      TWMWindowPosChanging(Message).WindowPos.x := R.Left
    end;
  end;
end;

end.
