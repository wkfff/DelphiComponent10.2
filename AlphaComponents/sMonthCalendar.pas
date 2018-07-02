unit sMonthCalendar;
{$I sDefs.inc}
//{$DEFINE LOGGED}

interface

uses Windows, Classes, Controls, SysUtils, Graphics, buttons, grids, messages, StdCtrls, forms, comctrls, Menus,
  {$IFNDEF DELPHI5} types, {$ENDIF}
  {$IFDEF DELPHI_XE2} UItypes, {$ENDIF}
  acntUtils, sPanel, sGraphUtils, sConst, sMessages, sDateUtils, sSpeedButton, sDefaults;


type
{$IFNDEF NOTFORHELP}
  TGetCellParams = procedure(Sender: TObject; Date: TDatetime; AFont: TFont; var Background: TColor) of object;
  TDateMouseEvent = procedure(Sender: TObject; Date: TDatetime) of object;
  TsMonthCalendar = class;


  TsCalendGrid = class(TDrawGrid)
  private
    FOwner: TsMonthCalendar;
    FMouseDate: TDateTime;
    procedure WMSize         (var Message: TWMSize ); message WM_SIZE;
    procedure WMMouseActivate(var Message: TMessage); message WM_MOUSEACTIVATE;
    procedure SetMouseDate(const Value: TDateTime);
  protected
    MouseCell: TPoint;
    procedure WndProc(var Message: TMessage); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp  (Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    Clicked: boolean;
    procedure Click; override;
    procedure KeyPress(var Key: Char); override;
    function SelectCell(ACol, ARow: Longint): Boolean; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState); override;
    procedure Paint; override;
    procedure CreateWnd; override;
    constructor Create(AOwner: TComponent); override;
    procedure MouseToCell(X, Y: Integer; var ACol, ARow: Longint);
    procedure SetMouseCell(const p: TPoint);
    property MouseDate: TDateTime read FMouseDate write SetMouseDate;
    property GridLineWidth;
    property DefaultColWidth;
    property DefaultRowHeight;
  end;
{$ENDIF} // NOTFORHELP

  TacCalendarStates = set of (csMonthChanging, csCmpLoading);

{$IFDEF DELPHI_XE3}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]{$ENDIF}
  TsMonthCalendar = class(TsPanel)
{$IFNDEF NOTFORHELP}
  private
    FReadOnly,
    FUpdating,
    FShowTitle,
    FShowWeeks,
    FShowTodayBtn,
    FShowYearBtns,
    FShowMonthBtns,
    FUseCurrentDate,
    FShowSelectAlways,
    FAllowChangeMonth,
    FTravellingSelection,
    FShowCurrentDate: boolean;

    FDate,
    FMinDate,
    FMaxDate: TDateTime;

    FOnDrawDay,
    FOnGetCellParams: TGetCellParams;

    FWeekendColor: TColor;
    FWeekends:     TDaysOfWeek;
    FOnChange:     TNotifyEvent;
    FStartOfWeek:  TCalDayOfWeek;
    FAnimated: boolean;
    FOnDateMouseEnter: TDateMouseEvent;
    FOnDateMouseLeave: TDateMouseEvent;
    function GetCellText   (ACol, ARow: Integer): string;
    function GetDayNum     (ACol, ARow: Integer): integer;
    function GetDateElement(Index: Integer): Integer;

    function IsWeekend(ACol, ARow: Integer): Boolean;
    procedure CalendarUpdate(DayOnly: Boolean);
    function StoreCalendarDate: Boolean;
    function FirstDay: integer;
    procedure TopPanelDblClick(Sender: TObject);
    procedure DoChangeAnimated(ForwDirection: boolean; OldBmp: TBitmap);

    procedure SetWeekendColor  (Value: TColor);
    procedure SetWeekends      (Value: TDaysOfWeek);
    procedure SetStartOfWeek   (Value: TCalDayOfWeek);
    procedure SetDateElement   (Index: Integer; Value: Integer);

    procedure SetBoolean(const Index: Integer; const Value: boolean);
    procedure SetDate(const Index: Integer; const Value: TDateTime);
    procedure SetCalendarDate(const Value: TDateTime);
  protected
    PopMenu: TPopupMenu;
    FMonthOffset: Integer;
    ShownDate: TDateTime;
    procedure PrevMonthBtnClick(Sender: TObject);
    procedure NextMonthBtnClick(Sender: TObject);
    procedure PrevYearBtnClick (Sender: TObject);
    procedure NextYearBtnClick (Sender: TObject);
    procedure MenuClick        (Sender: TObject);
    procedure TodayClick       (Sender: TObject);
    procedure TitleClick       (Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure MakePopup;
    procedure UpdateProps;
    procedure UpdateNavBtns;
    procedure Change;
    function DaysThisMonth: Integer;
    function ThisDayNum (ACol, ARow: integer): integer;
    procedure ChangeMonth(Delta: Integer; AllowAnimation: boolean = False);
    procedure ChangeYear (Delta: Integer; AllowAnimation: boolean = False);
    procedure GoToShownDate(ToForward: boolean; AllowAnimation: boolean = False);
  public
    State: TacCalendarStates;
    FGrid: TsCalendGrid;
    FDragBar: TsDragBar;
    FTodayBtn: TsSpeedButton;
    FBtns: array [0..3] of TsTimerSpeedButton;
    constructor Create(AOwner: TComponent); override;
    procedure CreateWnd; override;
    procedure Loaded; override;
    function MousePosToDate(X, Y: Integer): TDateTime;
    function IsValidDate(Date: TDateTime): boolean;
    procedure NextMonth;
    procedure NextYear;
    procedure PrevMonth;
    procedure PrevYear;
    procedure UpdateCalendar;
    procedure WndProc(var Message: TMessage); override;
    function GetCellDate(ACol, ARow: Integer): TDateTime;
    property CellDate[ACol, ARow: Integer]: TDateTime read GetCellDate;
    property CellText[ACol, ARow: Integer]: string read GetCellText;
    property OnDrawDay: TGetCellParams read FOnDrawDay write FOnDrawDay;
    property MaxDate: TDateTime Index 1 read FMaxDate write SetDate;
    property MinDate: TDateTime Index 2 read FMinDate write SetDate;
{$ENDIF} // NOTFORHELP
  published
{$IFNDEF NOTFORHELP}
    property Align;
    property BorderWidth default 0;
    property BevelWidth  default 5;
    property Width  default 178;
    property Height default 139;
{$ENDIF} // NOTFORHELP
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnDateMouseEnter: TDateMouseEvent read FOnDateMouseEnter write FOnDateMouseEnter;
    property OnDateMouseLeave: TDateMouseEvent read FOnDateMouseLeave write FOnDateMouseLeave;
    {:@event}
    property OnGetCellParams: TGetCellParams read FOnGetCellParams write FOnGetCellParams;
    property Animated: boolean read FAnimated write FAnimated default True;
    property ReadOnly: Boolean read FReadOnly write FReadOnly default False;
    property StartOfWeek: TCalDayOfWeek read FStartOfWeek write SetStartOfWeek default dowLocaleDefault;
    property WeekendColor: TColor read FWeekendColor write SetWeekendColor default clRed;
    property Weekends: TDaysOfWeek read FWeekends write SetWeekends default DefWeekends;

    property CalendarDate: TDateTime read FDate write SetCalendarDate stored StoreCalendarDate;

    property Year:  Integer index 1 read GetDateElement write SetDateElement stored False;
    property Day:   Integer index 3 read GetDateElement write SetDateElement stored False;
    property Month: Integer index 2 read GetDateElement write SetDateElement stored False;

    property AllowChangeMonth: boolean index 0 read FAllowChangeMonth write SetBoolean default True;
    property ShowCurrentDate:  boolean index 1 read FShowCurrentDate  write SetBoolean default True;
    property ShowSelectAlways: boolean index 2 read FShowSelectAlways write SetBoolean default True;
    property ShowTitle:        boolean index 3 read FShowTitle        write SetBoolean default True;
    property ShowTodayBtn:     boolean index 4 read FShowTodayBtn     write SetBoolean default False;
    property ShowWeeks:        boolean index 5 read FShowWeeks        write SetBoolean default False;
    property ShowYearBtns:     boolean index 6 read FShowYearBtns     write SetBoolean default True;
    property ShowMonthBtns:    boolean index 7 read FShowMonthBtns    write SetBoolean default True;
    property UseCurrentDate:   boolean index 8 read FUseCurrentDate   write SetBoolean default True;
    property TravellingSelection: boolean read FTravellingSelection write FTravellingSelection default True;
  end;


var
  s_WeeksTitle: acString = '¹/w';
  s_Today: acString;

function acChangeMonth(aDelta: Integer; var aDate: TDateTime): Boolean;
function acChangeYear (aDelta: Integer; var aDate: TDateTime): Boolean;

implementation

uses
  ExtCtrls,
  {$IFDEF DELPHI6UP} DateUtils, {$ENDIF}
  {$IFDEF LOGGED} sDebugMsgs, {$ENDIF}
  sCommonData, sSkinProps, sSkinManager, sPopupClndr, sVclUtils, sToolEdit, sStyleSimply;


function WeekNum(const TDT: TDateTime): Word;
var
  Y, M, D: Word;
  dtTmp: TDateTime;
  A: array [0..1] of char;
begin
  DecodeDate(TDT, Y, M, D);
  GetLocaleInfo(LOCALE_USER_DEFAULT, LOCALE_IFIRSTWEEKOFYEAR, A, SizeOf(A));
{$IFDEF DELPHI6UP}
  if Ord(A[0]) - Ord(ZeroChar) = 0 then begin
{$ENDIF}
    dtTmp := EnCodeDate(Y, 1, 1);
    Result := (Trunc(TDT - dtTmp) + (DayOfWeek(dtTmp) - 1)) div 7 + 1;
{$IFDEF DELPHI6UP}
  end
  else
    DecodeDateWeek(TDT, Y, Result, D);
{$ENDIF}
  if Result <> 0 then
    Result := Result - 1;
end;


const
  CaptArray: array [boolean{RTL}, 0..3] of Char = (('7', '3', '4', '8'), ('8', '4', '3', '7'));

constructor TsMonthCalendar.Create(AOwner: TComponent);
var
  i: integer;
  sp: TsPanel;
//  ds: acString;
begin
  inherited Create(AOwner);
  SkinData.COC := COC_TsMonthCalendar;
  SkinData.FUpdating := False;

  FShowTitle := True;
  FShowTodayBtn := False;
  FShowSelectAlways := True;
  FTravellingSelection := True;
  PopMenu := nil;

  FDragBar := TsDragBar.Create(Self);
  FDragBar.SkinData.SkinSection := s_DragBar;
  FDragBar.Color := clActiveCaption;
  FDragBar.OnDblClick := TopPanelDblClick;
  FDragBar.SkinData.Updating := False;
  if SkinData.SkinManager <> nil then
    FDragBar.Height := SkinData.SkinManager.ScaleInt(FDragBar.Height);

  FDragBar.OnMouseDown := TitleClick;

  FDragBar.Align := alTop;
  FDragBar.Top := -1;
  FAnimated := True;
  FAllowChangeMonth := True;

  sp := TsPanel.Create(Self);
  with sp do begin
    Align := alTop;
    Height := 3;
    sp.Skindata.SkinSection := s_Transparent;
    Caption := '';
    BevelOuter := bvNone;
    Parent := Self;
  end;

  FGrid := TsCalendGrid.Create(Self);
  FGrid.Parent := Self;
  FGrid.ParentColor := True;
  FGrid.OnDblClick := OnDblClick;
  FGrid.OnClick := OnClick;
  BorderWidth := 3;
  BevelWidth := 1;
  Caption := s_Space;
  FShowCurrentDate := True;

  FShowYearBtns := True;
  FShowMonthBtns := True;

  FUseCurrentDate := True;
  FStartOfWeek := dowLocaleDefault;
  FWeekends := DefWeekends;
  FWeekendColor := clRed;
  FDate := Date;
  Width  := 178;
  Height := 139;

  for i := 0 to 3 do begin
    FBtns[i] := TsTimerSpeedButton.Create(Self);
    FBtns[i].Parent := FDragBar;
    FBtns[i].Flat := True;
    FBtns[i].SkinData.SkinSection := s_ToolButton;
    FBtns[i].Font.Name := s_Webdings;
    FBtns[i].Font.Size := 10;
    FBtns[i].Caption := CaptArray[BidiMode = bdRightToLeft, i];
    if SkinData.SkinManager <> nil then
      FBtns[i].Font.Size := SkinData.SkinManager.ScaleInt(FBtns[i].Font.Size);
  end;
  FBtns[0].Align := alLeft;
  FBtns[0].OnClick := PrevYearBtnClick;

  FBtns[1].Align := alLeft;
  FBtns[1].OnClick := PrevMonthBtnClick;

  FBtns[2].Align := alRight;
  FBtns[2].OnClick := NextMonthBtnClick;

  FBtns[3].Align := alRight;
  FBtns[3].OnClick := NextYearBtnClick;

  FBtns[1].Left := 20;
  FBtns[2].Left := 80;
  FBtns[3].Left := 100;

  FDragBar.Parent := Self;

  FTodayBtn := TsSpeedButton.Create(Self);
  FTodayBtn.Visible := False;
  FTodayBtn.Align := alBottom;
  FTodayBtn.Height := 18;
  if SkinData.SkinManager <> nil then
    FTodayBtn.Height := SkinData.SkinManager.ScaleInt(FTodayBtn.Height);

  FTodayBtn.Flat := True;
//  FTodayBtn.Caption := s_Today + FormatDateTime(' mmmm d, yyyy', Date);
//  ds := FormatDateTime(FormatSettings.LongDateFormat, Date );           (*** DB ***)
//  FTodayBtn.Caption := s_Today + Copy(ds, Pos(' ', ds), 9999);          (*** DB ***)
  FTodayBtn.Caption := s_Today + s_Space + FormatDateTime({$IFDEF DELPHI_XE}FormatSettings.{$ENDIF}ShortDateFormat, Date );

  FTodayBtn.Font.Style := [fsBold];
  FTodayBtn.OnClick := TodayClick;
  FTodayBtn.Parent := Self;
end;


procedure TsMonthCalendar.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;


function TsMonthCalendar.DaysThisMonth: Integer;
begin
  Result := DaysPerMonth(Year, Month);
end;


function MakeNewDate(Y, M, D: word): TDateTime;
begin
{$IFDEF DELPHI7UP}
  TryEncodeDate(Y, M, D, Result);
{$ELSE}
  Result := EncodeDate(Y, M, D);
{$ENDIF}
end;


function TsMonthCalendar.GetCellDate(ACol, ARow: Integer): TDateTime;
var
  DayNum, Y, M: integer;
begin
  DayNum := ThisDayNum(ACol, ARow);
  Y := Year;
  M := Month;
  if DayNum < 1 then begin
    if M = 1 then begin // Get previous month
      dec(Y);
      M := 12;
    end
    else
      dec(M);

    Result := MakeNewDate(Y, M, DaysPerMonth(Y, M) + DayNum);
  end
  else
    if DayNum > DaysThisMonth then begin
      inc(M);
      if M > 12 then begin
        inc(Y);
        M := 1;
      end;
      Result := MakeNewDate(Y, M, DayNum - DaysThisMonth);
    end
    else
      Result := MakeNewDate(Y, M, DayNum);
end;


function TsMonthCalendar.GetCellText(ACol, ARow: Integer): string;
var
  DayNum: Integer;
  Y, M: Word;
begin
  if ARow = 0 then
    if ShowWeeks and (ACol = 0) then
      Result := s_WeeksTitle
    else
      Result := {$IFDEF DELPHI_XE}FormatSettings.{$ENDIF}ShortDayNames[(FirstDay + ACol - Integer(ShowWeeks) + 1) mod 7 + 1]
  else
    if ShowWeeks and (ACol = 0) then begin
      if ARow <= FGrid.RowCount - 1 then begin
        Y := Year;
        M := Month;
        if ARow = 1 then
          DayNum := FMonthOffset + 7 - Integer(ShowWeeks)
        else
          DayNum := FMonthOffset + 1 - Integer(ShowWeeks) + (ARow - 1) * 7 + 1;

        if FirstDay = 0 then
          dec(DayNum); // If Monday - first

        if DayNum <= 0 then begin
          if M = 1 then begin // Previous year
            M := 12;
            dec(Y);
          end
          else
            dec(M);

          DayNum := DaysPerMonth(Y, M);
        end
        else
          if DayNum > DaysThisMonth then begin
            DayNum := DayNum - DaysThisMonth;
            inc(M);
            if M > 12 then begin
              inc(Y);
              M := 1;
            end;
          end;

        Result := IntToStr(WeekNum(EncodeDate(Y, M, DayNum)) + 1);
      end;
    end
    else
      Result := IntToStr(GetDayNum(ACol, ARow));
end;


function TsMonthCalendar.StoreCalendarDate: Boolean;
begin
  Result := not FUseCurrentDate;
end;


function TsMonthCalendar.GetDateElement(Index: Integer): Integer;
var
  AYear, AMonth, ADay: Word;
begin
  DecodeDate(ShownDate, AYear, AMonth, ADay);
  case Index of
    1:   Result := AYear;
    2:   Result := AMonth;
    3:   Result := ADay;
    else Result := -1;
  end;
end;


function TsMonthCalendar.GetDayNum(ACol, ARow: Integer): integer;
var
  DayNum, Y, M: integer;
begin
  DayNum := ThisDayNum(ACol, ARow);
  if DayNum < 1 then begin
    Y := Year;
    M := Month;
    if ARow <= 2 then begin
      if M = 1 then begin // Get previous month
        dec(Y);
        M := 12;
      end
      else
        dec(M);

      Result := DaysPerMonth(Y, M) + DayNum;
    end
    else begin
      if M = 12 then begin // Get previous month
        inc(Y);
        M := 1;
      end
      else
        inc(M);

      Result := DaysPerMonth(Y, M) + DayNum;
    end;
  end
  else
    if DayNum > DaysThisMonth then
      Result := DayNum - DaysThisMonth
    else
      Result := DayNum;
end;


procedure TsMonthCalendar.SetDate(const Index: Integer; const Value: TDateTime);
begin
  case Index of
    1: if FMaxDate <> Value then begin
      FMaxDate := Value;
      if Value <> 0 then
        if not (csLoading in ComponentState) and (CalendarDate > Value) then
          CalendarDate := Value;
    end;

    2: if FMinDate <> Value then begin
      FMinDate := Value;
      if Value <> 0 then
        if not (csLoading in ComponentState) and (CalendarDate < Value) then
          CalendarDate := Value;
    end;
  end;
  UpdateCalendar;
end;


procedure TsMonthCalendar.SetDateElement(Index: Integer; Value: Integer);
var
  AYear, AMonth, ADay: Word;
begin
  if Value > 0 then begin
    DecodeDate(ShownDate, AYear, AMonth, ADay);
    case Index of
      1:
        if AYear <> Value then
          AYear := Value
        else
          Exit;

      2:
        if (Value <= 12) and (Value <> AMonth) then begin
          AMonth := Value;
          if ADay > DaysPerMonth(Year, Value) then
            ADay := DaysPerMonth(Year, Value);
        end
        else
          Exit;

      3:
        if (Value <= DaysThisMonth) and (Value <> ADay) then
          ADay := Value
        else
          Exit

      else
        Exit;
    end;
    ShownDate := MakeNewDate(AYear, AMonth, ADay);
    if ShownDate <> 0 then begin
      FUseCurrentDate := False;
      if FTravellingSelection then begin
        FDate := ShownDate;
        Change;
      end;
      CalendarUpdate(Index = 3);
    end
    else
      ShownDate := FDate;
  end;
end;


procedure TsMonthCalendar.SetWeekendColor(Value: TColor);
begin
  if Value <> FWeekendColor then begin
    FWeekendColor := Value;
    SkinData.Invalidate;
  end;
end;


procedure TsMonthCalendar.SetWeekends(Value: sConst.TDaysOfWeek);
begin
  if Value <> FWeekends then begin
    FWeekends := Value;
    UpdateCalendar;
  end;
end;


procedure TsMonthCalendar.GoToShownDate(ToForward: boolean; AllowAnimation: boolean = False);
var
  b: boolean;
{$IFDEF DELPHI6UP}
  OldBmp: TBitmap;
  AYear, AMonth, ADay: word;
{$ENDIF}
begin
  if FTravellingSelection then
    CalendarDate := ShownDate;
{$IFDEF DELPHI6UP}
  b := FAnimated and AllowAnimation and ((SkinData.SkinManager = nil) or SkinData.SkinManager.Effects.AllowAnimation);
  if (MinDate <> 0) and (ShownDate < MinDate) then begin
    ShownDate := MinDate;
    DecodeDate(ShownDate, AYear, AMonth, ADay);
    b := False;
  end
  else
    if (MaxDate <> 0) and (ShownDate > MaxDate) then begin
      ShownDate := MaxDate;
      DecodeDate(ShownDate, AYear, AMonth, ADay);
      b := False;
    end;

  if b then begin
    UpdateCalendar;
    OldBmp := CreateBmp32(FGrid);
    FGrid.PaintTo(OldBmp.Canvas, 0, 0);
    DoChangeAnimated(ToForward, OldBmp);
  end
  else
{$ENDIF} // DELPHI6UP
    UpdateCalendar;
end;


function TsMonthCalendar.IsWeekend(ACol, ARow: Integer): Boolean;
begin
  Result := TCalDayOfWeek((FirstDay + ACol - Integer(ShowWeeks)) mod 7) in FWeekends;
end;


procedure TsMonthCalendar.SetStartOfWeek(Value: TCalDayOfWeek);
begin
  if Value <> FStartOfWeek then begin
    FStartOfWeek := Value;
    UpdateCalendar;
  end;
end;


function acChangeMonth(aDelta: Integer; var aDate: TDateTime): Boolean;
var
  AYear, AMonth, ADay: Word;
  NewDate: TDateTime;
  CurDay: Integer;
begin
  DecodeDate(aDate, AYear, AMonth, ADay);
  CurDay := ADay;
  if aDelta > 0 then
    ADay := DaysPerMonth(AYear, AMonth)
  else
    ADay := 1;

  NewDate := EncodeDate(AYear, AMonth, ADay);
  NewDate := NewDate + aDelta;
  DecodeDate(NewDate, AYear, AMonth, ADay);

  if DaysPerMonth(AYear, AMonth) > CurDay then
    ADay := CurDay
  else
    ADay := DaysPerMonth(AYear, AMonth);

  aDate := MakeNewDate(AYear, AMonth, ADay);
  Result := aDate <> 0;
end;


function acChangeYear(aDelta: Integer; var aDate: TDateTime): Boolean;
var
  AYear, AMonth, ADay: Word;
begin
  DecodeDate(aDate, AYear, AMonth, ADay);

  if IsLeapYear(aYear) and (aMonth = 2) and (aDay = 29) then
    aDay := 28;

  aDate := MakeNewDate(AYear + aDelta, AMonth, ADay);
  Result := aDate <> 0;
end;


procedure TsMonthCalendar.ChangeMonth(Delta: Integer; AllowAnimation: boolean = False);
begin
  if FAllowChangeMonth then
    if acChangeMonth(Delta, ShownDate) then begin
      State := State + [csMonthChanging];
      GoToShownDate(Delta > 0, AllowAnimation);
      State := State - [csMonthChanging];
    end;
end;


procedure TsMonthCalendar.PrevMonth;
begin
  ChangeMonth(-1);
end;


procedure TsMonthCalendar.NextMonth;
begin
  ChangeMonth(1);
end;


procedure TsMonthCalendar.NextYear;
begin
  ChangeYear(1);
end;


procedure TsMonthCalendar.PrevYear;
begin
  ChangeYear(-1);
end;


procedure TsMonthCalendar.CalendarUpdate(DayOnly: Boolean);
var
  AYear, AMonth, ADay: Word;
  FirstDate: TDateTime;
  d1, d2: integer;
begin
  FUpdating := True;
  try
    DecodeDate(ShownDate, AYear, AMonth, ADay);
    FirstDate := MakeNewDate(AYear, AMonth, 1);
    if FirstDate <> 0 then begin
      FMonthOffset := 2 - (DayOfWeek(FirstDate) - (FirstDay + 1) + 7) mod 7;
      if FMonthOffset in [1, 2] then
        FMonthOffset := FMonthOffset - 7;

      d1 := (ADay - FMonthOffset) mod 7 + Integer(ShowWeeks);
      d2 := (ADay - FMonthOffset) div 7 + 1;
      FGrid.MoveColRow(d1, d2, False, False);

      if not DayOnly then begin
        FDragBar.Caption := {$IFDEF DELPHI_XE}FormatSettings.{$ENDIF}LongMonthNames[AMonth] + s_Space + IntToStr(AYear);
        FGrid.Invalidate;
      end;
    end;
  finally
    FUpdating := False;
  end;
end;


procedure TsMonthCalendar.UpdateCalendar;
begin
  CalendarUpdate(False);
end;


function TsMonthCalendar.FirstDay: integer;
var
  A: array [0..1] of char;
begin
  if FStartOfWeek = dowLocaleDefault then begin
    GetLocaleInfo(LOCALE_USER_DEFAULT, LOCALE_IFIRSTDAYOFWEEK, A, SizeOf(A));
    Result := Ord(A[0]) - Ord(ZeroChar);
  end
  else
    Result := Ord(FStartOfWeek);
end;


procedure TsMonthCalendar.PrevYearBtnClick(Sender: TObject);
begin
  ChangeYear(-1, True);
end;


procedure TsMonthCalendar.NextYearBtnClick(Sender: TObject);
begin
  ChangeYear(1, True);
end;


procedure TsMonthCalendar.PrevMonthBtnClick(Sender: TObject);
begin
  ChangeMonth(-1, True);
end;


procedure TsMonthCalendar.NextMonthBtnClick(Sender: TObject);
begin
  ChangeMonth(1, True);
end;


procedure TsMonthCalendar.TopPanelDblClick(Sender: TObject);
begin
  CalendarDate := Date;
end;


procedure TsMonthCalendar.WndProc(var Message: TMessage);
var
  i: integer;
  Size: TSize;
begin
  inherited;
  case Message.Msg of
    SM_ALPHACMD:
      case Message.WParamHi of
        AC_REMOVESKIN, AC_REFRESH:
          if ACUInt(Message.LParam) = ACUInt(SkinData.SkinManager) then begin
            if (PopMenu <> nil) and SkinData.SkinManager.SkinnedPopups then
              Skindata.SkinManager.SkinableMenus.HookPopupMenu(PopMenu, Message.WParamHi = AC_REFRESH);

            UpdateCalendar;
            FGrid.Repaint;
            if ShowTitle then
              SendMessage(FDragBar.Handle, Message.Msg, Message.WParam, Message.LParam);
          end;
      end;

    CM_FONTCHANGED: begin
      FTodayBtn.ParentFont := True;
      FTodayBtn.Font.Style := [fsBold];
      Size := GetStringSize(FTodayBtn.Font.Handle, 'W');
      FTodayBtn.Height := Size.cy + 4;
      FDragBar.ParentFont := True;
      FDragBar.Font.Color := clCaptionText;
      FDragBar.Font.Style := [fsBold];
      FDragBar.Height := Size.cy + 4;
      for i := 0 to 3 do begin
        FBtns[i].Width := Size.cx + 10;
        if Skindata.SkinManager <> nil then
          FBtns[i].Font.Size := Skindata.SkinManager.ScaleInt(10);
      end;
    end;

    CM_BIDIMODECHANGED:
      for i := 0 to 3 do
        FBtns[i].Caption := CaptArray[BidiMode = bdRightToLeft, i];
  end;
end;


procedure TsMonthCalendar.SetBoolean(const Index: Integer; const Value: boolean);
begin
  case Index of
    0: if FAllowChangeMonth <> Value then begin
      FAllowChangeMonth := Value;
      UpdateNavBtns;
      if Value then
        FDragBar.Cursor := crHandPoint
      else
        FDragBar.Cursor := Cursor;

      Invalidate;
    end;

    1: if FShowCurrentDate <> Value then begin
      FShowCurrentDate := Value;
      Invalidate;
    end;

    2: if FShowSelectAlways <> Value then begin
      FShowSelectAlways := Value;
      CalendarUpdate(False);
    end;

    3: if FShowTitle <> Value then begin
      FShowTitle := Value;
      FDragBar.Visible := Value;
      if Value then
        FDragBar.Parent := Self
      else
        FDragBar.Parent := nil;
    end;

    4: if FShowTodayBtn <> Value then begin
      FShowTodayBtn := Value;
      if Value then
        FTodayBtn.Parent := Self
      else
        FTodayBtn.Parent := nil;

      CalendarUpdate(False);
    end;

    5: if FShowWeeks <> Value then begin
      FShowWeeks := Value;
      FGrid.DefaultColWidth := FGrid.Width div (7 + integer(ShowWeeks));
      UpdateProps;
      Invalidate;
    end;

    6: if FShowYearBtns <> Value then begin
      FShowYearBtns := Value;
      UpdateNavBtns;
    end;

    7: if FShowMonthBtns <> Value then begin
      FShowMonthBtns := Value;
      UpdateNavBtns;
    end;

    8: if Value <> FUseCurrentDate then begin
      FUseCurrentDate := Value;
      if Value then begin
        FDate := Date;
        UpdateCalendar;
      end;
    end;
  end;
end;


procedure TsMonthCalendar.SetCalendarDate(const Value: TDateTime);
var
  Y, M, D: Word;
begin
  if (FDate <> Value) and IsValidDate(Value) then begin
    DecodeDate(Value, Y, M, D);
    if not FAllowChangeMonth and ((Y <> Year) or (M <> Month)) then
      Exit;

    FDate := MakeNewDate(Y, M, D);
    if FDate <> 0 then begin
      FDate := Value;
      ShownDate := FDate;
      Change;
      if not (csMonthChanging in State) then
        UpdateCalendar;
    end;
  end;
end;


type
  TAccessCommonData = class(TsCommonData);

procedure TsMonthCalendar.Loaded;
var
  i: integer;
begin
  inherited;
  ShownDate := FDate;
  if Assigned(FDragBar) and FDragBar.Visible then begin
    TAccessCommonData(FDragBar.SkinData).FSkinManager := TAccessCommonData(SkinData).FSkinManager;
    for i := 0 to 3 do
      TAccessCommonData(FBtns[i].SkinData).FSkinManager := TAccessCommonData(SkinData).FSkinManager;
  end;
end;


procedure TsMonthCalendar.CreateWnd;
begin
  inherited;
  if FShowtodayBtn then
    FTodayBtn.Visible := True;

  UpdateProps;
  UpdateNavBtns;
end;


procedure TsMonthCalendar.MakePopup;
var
  i: integer;
  miRoot: TMenuItem;
  yName, mName: string;


  procedure MakeMenusForYear(aYear: integer; Items: TMenuItem);
  var
    i, j: integer;
    mi, si: TMenuItem;
  begin
    for i := aYear to aYear + 9 do begin
      mi := TMenuItem.Create(Self);
      mi.Tag := i;
      mi.Caption := IntToStr(i);
      if Items = PopMenu.Items then
        mi.Caption := mi.Caption;

      if i = Year then begin
        mi.Default := True;
        mi.Checked := True;
      end;
      for j := 1 to 12 do begin
        si := TMenuItem.Create(Self);
        si.Caption := {$IFDEF DELPHI_XE}FormatSettings.{$ENDIF}LongMonthNames[j];
        if mi.Default and (j = Month) then begin
          si.Default := True;
          si.Checked := True;
        end;
        si.OnClick := MenuClick;
        mi.Add(si);
      end;
      Items.Add(mi);
    end;
  end;

  procedure CheckItem(AItem: TMenuItem);
  var
    i: integer;
    b: boolean;
  begin
    if AItem.Count = 0 then begin
      AItem.Checked := (AItem.Parent.Caption = yName) and (AItem.Caption = mName);
      AItem.Default := AItem.Checked;
    end
    else begin
      b := False;
      for i := 0 to AItem.Count - 1 do begin
        CheckItem(AItem.Items[i]);
        if AItem.Items[i].Checked then
          b := True;
      end;
      AItem.Checked := b;
      AItem.Default := b;
    end;
  end;

begin
  mName := {$IFDEF DELPHI_XE}FormatSettings.{$ENDIF}LongMonthNames[Month];
  yName := IntToStr(Year);
  PopMenu.Free;

  PopMenu := TPopupMenu.Create(Self);
  PopMenu.AutoHotkeys := maManual;
  i := (Year - 4) - 10 * 5;
  while i < (Year - 4) do begin
    miRoot := TMenuItem.Create(Self);
    miRoot.Caption := IntToStr(i) + '..' + IntToStr(i + 9);
    MakeMenusForYear(i, miRoot);
    PopMenu.Items.Add(miRoot);
    inc(i, 10);
  end;
  MakeMenusForYear(Year - 4, PopMenu.Items);
  i := Year + 6;
  while i < (Year - 4) + 50 do begin
    miRoot := TMenuItem.Create(Self);
    miRoot.Caption := IntToStr(i) + '..' + IntToStr(i + 9);
    MakeMenusForYear(i, miRoot);
    PopMenu.Items.Add(miRoot);
    inc(i, 10);
  end;
  if SkinData.Skinned and SkinData.SkinManager.SkinnedPopups then
    Skindata.SkinManager.SkinableMenus.HookPopupMenu(PopMenu, True);
end;


procedure TsMonthCalendar.MenuClick(Sender: TObject);
var
  mi: TMenuItem;
begin
  mi := TMenuItem(Sender);
  Year := mi.Parent.Tag;
  Month := mi.MenuIndex + 1;
end;


function TsMonthCalendar.ThisDayNum(ACol, ARow: integer): integer;
begin
  Result := FMonthOffset + ACol - Integer(ShowWeeks) + (ARow - 1) * 7;
end;


procedure TsMonthCalendar.TitleClick(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
begin
  if FAllowChangeMonth then begin
    MakePopup;
    P := acMousePos;
    PopMenu.PopupComponent := Self;
    PopMenu.Popup(P.X, P.Y);
    PopMenu.PopupComponent := nil;
  end;
end;


procedure TsMonthCalendar.TodayClick(Sender: TObject);
begin
  CalendarDate := Date;
  if Parent is TsPopupCalendar then
    TsPopupCalendar(Parent).CalendarClick;
end;


procedure TsMonthCalendar.UpdateProps;
begin
  if FShowTodayBtn then
    FTodayBtn.Parent := Self
  else
    FTodayBtn.Parent := nil;

  if FGrid <> nil then begin
    FGrid.ColCount := 7 + integer(ShowWeeks);
    FGrid.FixedCols := integer(ShowWeeks);
    UpdateCalendar;
  end;
end;


procedure TsMonthCalendar.UpdateNavBtns;
begin
  FBtns[0].Visible := FAllowChangeMonth and ShowYearBtns;
  FBtns[1].Visible := FAllowChangeMonth and ShowMonthBtns;
  FBtns[2].Visible := FAllowChangeMonth and ShowMonthBtns;
  FBtns[3].Visible := FAllowChangeMonth and ShowYearBtns;
end;


function TsMonthCalendar.MousePosToDate(X, Y: Integer): TDateTime;
var
  ACol, ARow: integer;
begin
  if not PtInRect(FGrid.BoundsRect, Point(X, Y)) then
    Result := 0
  else begin
    FGrid.MouseToCell(X - FGrid.Left, Y - FGrid.Top, ACol, ARow);
    Result := GetCellDate(ACol, ARow);
  end;
end;


function TsMonthCalendar.IsValidDate(Date: TDateTime): boolean;
begin
  if ((MinDate <> 0) and (Date < MinDate)) or ((MaxDate <> 0) and (Date > MaxDate)) then
    Result := False
  else
    Result := True;
end;


procedure TsCalendGrid.Click;
var
  DayNum: integer;
  TheCellText: string;
begin
  if not FOwner.FUpdating then begin
    inherited Click;
    if not FOwner.ShowWeeks or (Col > 0) then begin
      TheCellText := FOwner.CellText[Col, Row];
      if TheCellText <> '' then begin
        DayNum := FOwner.ThisDayNum(Col, Row);
        if FOwner.FAllowChangeMonth or ((DayNum >= 1) and (DayNum <= FOwner.DaysThisMonth)) then begin
          if DayNum < 1 then
            FOwner.PrevMonth
          else
            if DayNum > FOwner.DaysThisMonth then
              FOwner.NextMonth;

          FOwner.Day := StrToInt(TheCellText);
          FOwner.CalendarDate := FOwner.ShownDate;
        end
        else
          FOwner.UpdateCalendar;
      end;
    end;
    if Assigned(FOwner.OnClick) then
      FOwner.OnClick(FOwner);
  end;
end;


constructor TsCalendGrid.Create(AOwner: TComponent);
begin
  inherited;
  FOwner := TsMonthCalendar(AOwner);
  Ctl3D := False;
  BorderStyle := bsNone;
  FixedCols := 0;
  FixedRows := 1;
  ColCount := 7;
  RowCount := 8;
  ScrollBars := ssNone;
  MouseCell := Point(-1, -1);
  MouseDate := 0;
  Options := [];
  Align := alClient;
  Tag := ExceptTag;
end;


procedure TsCalendGrid.CreateWnd;
begin
  inherited;
  DefaultDrawing := False;
  Clicked := False;
end;


procedure TsCalendGrid.DrawCell(ACol, ARow: Integer; ARect: TRect; AState: TGridDrawState);
var
  R: TRect;
  c: TColor;
  Y, M, D: word;
  TheText: string;
  aDate: TDateTime;
  BGInfo: TacBGInfo;
  CanAccept: boolean;
  Editor: TsDateEdit;
  pc: TsPopupCalendar;
  RealNum, DayNum: integer;

  function IsToday: boolean;
  begin
    if (ARow > 2) and (RealNum < 10) or (ARow > 4) and (RealNum < 16) or (ARow < 3) and (RealNum > 20) or FOwner.ShowWeeks and (ACol < 1) then
      Result := False
    else
      Result := (ARow <> 0) and (TheText <> '') and (EncodeDate(FOwner.Year, FOwner.Month, StrToInt(TheText)) = Date)
  end;

begin
  if ARow < RowCount - 1 then begin
    TheText := FOwner.CellText[ACol, ARow];
    RealNum := FOwner.GetDayNum(ACol, ARow);
    R := ARect;
    if ACol = 6 + Integer(FOwner.ShowWeeks) then
      R.Right := Width;

    if ARow = 6 then
      R.Bottom := Height;

    BGInfo.PleaseDraw := False;
    BGInfo.Offset := MkPoint;
    GetBGInfo(@BGInfo, FOwner);
    if (BGInfo.BgType = btCache) and not BGInfo.Bmp.Empty then
      BitBlt(Canvas.Handle, R.Left, R.Top, WidthOf(R), HeightOf(R), BGInfo.Bmp.Canvas.Handle, Left + R.Left + BGInfo.Offset.X, Top + R.Top + BGInfo.Offset.Y, SRCCOPY)
    else
      FillDC(Canvas.Handle, R, BGInfo.Color);

    R := ARect;
    Canvas.Font.Assign(Font);
    if gdSelected in AState then begin
      DecodeDate(FOwner.FDate, Y, M, D);
      if (Y <> FOwner.Year) or (M <> FOwner.Month) then
        AState := AState - [gdSelected];

      if not FOwner.ShowSelectAlways and not Focused then
        AState := AState - [gdSelected];
    end;

    if FOwner.ShowCurrentDate and (gdSelected in AState) then begin
      Canvas.Font.Style := [fsBold];
      Canvas.Font.Color := clBlack;
    end
    else
      Canvas.Font.Style := [];

    Canvas.Brush.Style := bsClear;
    Canvas.Font.Style := [];
    if not (FOwner.ShowWeeks and (ACol = 0)) and FOwner.ShowCurrentDate and IsToday and (ACol <> 7 + Integer(FOwner.ShowWeeks)) then begin
      Canvas.Brush.Style := bsSolid;
      Canvas.Pen.Style := psClear;
      if FOwner.SkinData.Skinned then
        Canvas.Brush.Color := BlendColors(FOwner.WeekendColor, FOwner.SkinData.SkinManager.GetGlobalColor, 77)
      else
        Canvas.Brush.Color := BlendColors(FOwner.WeekendColor, ColortoRGB(clBtnFace), 77);

      Canvas.RoundRect(R.Left, R.Top, R.Right + 1, R.Bottom + 1, 5, 5);
      Canvas.Font.Color := clWhite;
      Canvas.Brush.Style := bsClear;
      Canvas.Font.Style := [fsBold];
    end
    else begin
      if FOwner.IsWeekend(ACol, ARow) then
        if FOwner.SkinData.Skinned and (FOwner.WeekendColor = clRed) then
          if FOwner.SkinData.SkinManager.gd[FOwner.SkinData.SkinIndex].Props[0].FontColor.Color = 0 then
            Canvas.Font.Color := BlendColors(clRed, FOwner.SkinData.SkinManager.gd[FOwner.SkinData.SkinIndex].Props[0].FontColor.Color, 196)
          else
            Canvas.Font.Color := BlendColors(clRed, FOwner.SkinData.SkinManager.gd[FOwner.SkinData.SkinIndex].Props[0].FontColor.Color, 127) // acColorToRGB(slBtnRedText)
        else
          Canvas.Font.Color := FOwner.WeekendColor
      else
        with FOwner.SkinData do
          if FOwner.ShowWeeks and (ACol = 0) then begin
            if ARow = 0 then begin
              Canvas.Font.Name := 'Small Fonts';
              Canvas.Font.Size := 6;
            end
            else
              Canvas.Font.Style := [fsBold];

            if Skinned and Assigned(SkinManager) then
              Canvas.Font.Color := BlendColors(SkinManager.GetGlobalFontColor, SkinManager.GetGlobalColor, 77)
            else
              Canvas.Font.Color := clGrayText;

            Canvas.Pen.Style := psSolid;
            Canvas.Pen.Color := Canvas.Font.Color;
            acPaintLine(Canvas.Handle, ARect.Right - 1, ARect.Top, ARect.Right - 1, ARect.Bottom);
            dec(ARect.Right, 3)
          end
          else
            if Skinned and Assigned(SkinManager) and (SkinManager.GetGlobalFontColor <> clFuchsia) then
              with SkinManager do
                if SkinIndex >= 0 then
                  if (BorderIndex < 0) and (gd[SkinIndex].Props[0].Transparency = 100) then
                    Canvas.Font.Color := GetControlFontColor(Parent, SkinManager)
                  else
                    Canvas.Font.Color := gd[SkinIndex].Props[0].FontColor.Color
                else
                  Canvas.Font.Color := GetGlobalFontColor
            else
              Canvas.Font.Color := clWindowText;

      if ARow <> 0 then begin
        DayNum := FOwner.ThisDayNum(ACol, ARow);
        if (DayNum <= 0) or (DayNum > FOwner.DaysThisMonth) then
          if FOwner.SkinData.Skinned then
            Canvas.Font.Color := BlendColors(Canvas.Font.Color, FOwner.SkinData.SkinManager.GetGlobalColor, 127)
          else
            Canvas.Font.Color := BlendColors(ColorToRGB(Canvas.Font.Color), ColorToRGB(FOwner.Color), 127);
      end;
    end;

    if (ARow > 0) and (TheText <> '') and not (FOwner.ShowWeeks and (ACol = 0)) then begin
      aDate := FOwner.GetCellDate(ACol, ARow);
      if FOwner.Parent is TsPopupCalendar then begin
        pc := TsPopupCalendar(FOwner.Parent);
        Editor := TsDateEdit(pc.FEditor);
        if Editor.DimUnacceptedCells and Assigned(Editor.OnAcceptDate) then begin
          CanAccept := True;
          Editor.OnAcceptDate(Editor, aDate, CanAccept);
          if not CanAccept then begin
            Canvas.Font.Style := [];
            Canvas.Font.Color  := SysColorToSkin(clGrayText,  FOwner.SkinData.SkinManager);
            Canvas.Brush.Color := SysColorToSkin(clBtnShadow, FOwner.SkinData.SkinManager);
          end;
        end;
      end;

      if Assigned(FOwner.FOnDrawDay) or Assigned(FOwner.FOnGetCellParams) then begin
        c := clFuchsia;
        if Assigned(FOwner.FOnDrawDay) then
          FOwner.FOnDrawDay(TsPopupCalendar(FOwner.Parent).FEditor, aDate, Canvas.Font, c)
        else
          FOwner.FOnGetCellParams(Self, aDate, Canvas.Font, c);

        if c <> clFuchsia then begin
          Canvas.Brush.Color := c;
          Canvas.Brush.Style := bsSolid;
        end;
      end;

      if not FOwner.IsValidDate(aDate) then
        Canvas.Font.Color := BlendColors(ColorToRGB(Canvas.Font.Color), ColorToRGB(Canvas.Brush.Color), 77);
    end;
    Canvas.TextRect(ARect, ARect.Left + (ARect.Right - ARect.Left - Canvas.TextWidth(TheText)) div 2,
                    ARect.Top + (ARect.Bottom - ARect.Top - Canvas.TextHeight(TheText)) div 2, TheText);

    if gdSelected in AState then begin
      if TheText = '' then
        Exit;

      Canvas.Font.Style := [fsBold];
      InflateRect(R, -2, -2);

      if FOwner.SkinData.Skinned then begin
        PaintItem(FOwner.SkinData.SkinManager.ConstData.Sections[ssSelection], BGInfoToCI(@BGInfo),
                  True, integer(Focused or FOwner.Focused), R, Point(0, 0), Canvas.Handle);

        Canvas.Font.Color := FOwner.SkinData.SkinManager.GetHighLightFontColor(Focused or FOwner.Focused);
      end
      else begin
        FillDC(Canvas.Handle, R, clHighlight);
        Canvas.Font.Color := clHighlightText;
      end;

      Canvas.Brush.Color := Color;
      Canvas.Brush.Style := bsClear;
      Canvas.Pen.Style := psClear;

      Canvas.TextRect(ARect, ARect.Left + (ARect.Right - ARect.Left - Canvas.TextWidth(TheText)) div 2,
                    ARect.Top + (ARect.Bottom - ARect.Top - Canvas.TextHeight(TheText)) div 2, TheText);
    end;

    if not FOwner.SkinData.Skinned then
      if (gdSelected in AState) and (Canvas.Brush.Style <> bsSolid) and Focused then begin
        Canvas.Brush.Style := bsClear;
        Canvas.Pen.Style := psClear;
        DrawFocusRect(Canvas.Handle, R);
      end;
  end;
end;


procedure TsCalendGrid.KeyDown(var Key: Word; Shift: TShiftState);
var
  d: TDateTime;
  CanAccept: boolean;
begin
  case Key of
    VK_UP, VK_DOWN: begin
      Shift := Shift - [ssCtrl];
      if VK_UP = Key then begin
        if FOwner.IsValidDate(FOwner.CalendarDate - 7) then
          FOwner.CalendarDate := FOwner.CalendarDate - 7;
      end
      else
        if FOwner.IsValidDate(FOwner.CalendarDate + 7) then
          FOwner.CalendarDate := FOwner.CalendarDate + 7;

      Exit;
    end;

    VK_LEFT, VK_SUBTRACT:
      if Shift = [] then begin
        if FOwner.IsValidDate(FOwner.CalendarDate - 1) then
          FOwner.CalendarDate := FOwner.CalendarDate - 1;

        Exit;
      end;

    VK_RIGHT, VK_ADD:
      if Shift = [] then begin
        if FOwner.IsValidDate(FOwner.CalendarDate + 1) then
          FOwner.CalendarDate := FOwner.CalendarDate + 1;

        Exit;
      end;

    VK_NEXT: begin
      if ssCtrl in Shift then
        FOwner.NextYear
      else
        FOwner.NextMonth;

      Exit;
    end;

    VK_PRIOR: begin
      if ssCtrl in Shift then
        FOwner.PrevYear
      else
        FOwner.PrevMonth;

      Exit;
    end;

    VK_RETURN:
      if FOwner.Parent is TsPopupCalendar then
        with TsPopupCalendar(FOwner.Parent) do begin
          if FEditor <> nil then begin
            d := sMonthCalendar1.CalendarDate;
            if not TsPopupCalendar(FOwner.Parent).IsValidDate(d) then
              Exit;

            CanAccept := True;
            if Assigned(TsDateEdit(FEditor).OnAcceptDate) then
              TsDateEdit(FEditor).OnAcceptDate(FEditor, d, CanAccept);

            if CanAccept then begin
              TsCustomDateEdit(FEditor).Date := d;
              if Assigned(TsCustomDateEdit(FEditor).OnChange) then
                TsCustomDateEdit(FEditor).OnChange(TsCustomDateEdit(FEditor));
            end;
            FEditor.SetFocus;
            if FEditor.AutoSelect then
              FEditor.SelectAll;
          end;
          Close;
          CloseUp;
        end;

    VK_ESCAPE:
      if FOwner.Parent is TsPopupCalendar then
        with TsPopupCalendar(FOwner.Parent) do begin
          if FEditor.Visible then
            FEditor.SetFocus;

          Close;
          CloseUp;
        end;
  end;
  inherited KeyDown(Key, Shift);
end;


procedure TsCalendGrid.KeyPress(var Key: Char);
begin
  if CharInSet(AnsiChar(Key), ['T', 't']) then begin
    FOwner.CalendarDate := Trunc(Now);
    Key := #0;
  end;
  inherited KeyPress(Key);
end;


procedure TsCalendGrid.MouseToCell(X, Y: Integer; var ACol, ARow: Integer);
var
  Coord: TGridCoord;
begin
  Coord := MouseCoord(X, Y);
  ACol := Coord.X;
  ARow := Coord.Y;
end;


procedure TsCalendGrid.Paint;
begin
  if [csDestroying, csLoading] * ComponentState = [] then
    inherited;
end;


function TsCalendGrid.SelectCell(ACol, ARow: Integer): Boolean;
begin
  if not FOwner.FUpdating and FOwner.FReadOnly or (FOwner.CellText[ACol, ARow] = '') then
    Result := False
  else
    Result := inherited SelectCell(ACol, ARow);
end;


procedure TsCalendGrid.SetMouseCell(const p: TPoint);
begin
  if (MouseCell.X <> p.X) or (MouseCell.Y <> p.Y) then begin
    MouseCell := p;
    if (FOwner.ShowWeeks and (p.X = 0)) or (p.Y = 0) then
      MouseDate := 0
    else
      MouseDate := FOwner.GetCellDate(MouseCell.X, MouseCell.Y);
  end;
end;


procedure TsCalendGrid.SetMouseDate(const Value: TDateTime);
begin
  if Value <> MouseDate then begin
    if (FMouseDate <> 0) and Assigned(FOwner.FOnDateMouseLeave) then
      FOwner.FOnDateMouseLeave(FOwner, FMouseDate);

    FMouseDate := Value;
    if Assigned(FOwner.FOnDateMouseEnter) then
      FOwner.FOnDateMouseEnter(FOwner, FMouseDate);
  end;
end;


procedure TsCalendGrid.WMMouseActivate(var Message: TMessage);
begin
  if FOwner.Parent is TsPopupCalendar then
    Message.Result := MA_NOACTIVATE
  else
    inherited;
end;


procedure TsCalendGrid.WMSize(var Message: TWMSize);
begin
  DefaultColWidth  := Message.Width  div (7 + integer(FOwner.ShowWeeks));
  DefaultRowHeight := Message.Height div 7;
end;

{$IFNDEF DELPHI6UP}
  {$HINTS OFF}
type
  TAccessStdGrid = class(TCustomControl)
  private
    FAnchor:        TGridCoord;
    FBorderStyle:   TBorderStyle;
    FCanEditModify: Boolean;
    FColCount:      Longint;
    FColWidths,
    FTabStops:      Pointer;
    FCurrent:       TGridCoord;
  end;
{$ENDIF}


procedure TsCalendGrid.WndProc(var Message: TMessage);
var
  DC: HDC;
  R: TRect;
  BGInfo: TacBGInfo;
  SaveIndex, X, Y: integer;
begin
{$IFDEF LOGGED}
  AddToLog(Message);
{$ENDIF}
  case Message.Msg of
    WM_ERASEBKGND: ;
    CM_MOUSEENTER: ;
    CM_MOUSELEAVE: MouseDate := 0;
    WM_MOUSEMOVE: begin
      MouseToCell(TWMMouse(Message).XPos, TWMMouse(Message).YPos, X, Y);
      SetMouseCell(Point(X, Y));
    end;

    WM_LBUTTONDBLCLK: begin
      inherited;
      if Assigned(FOwner.OnDblClick) and (csDoubleClicks in FOwner.ControlStyle) then
        if FOwner.Parent is TsPopupCalendar then begin
          if TsPopupCalendar(FOwner.Parent).FEditor = nil then
            FOwner.OnDblClick(FOwner)
        end
        else
          FOwner.OnDblClick(FOwner);
    end;

    WM_PAINT: begin
      inherited;
      // Filling BG vert line
      if TWMPAINT(Message).DC = 0 then
        DC := GetDC(Handle)
      else
        DC := TWMPAINT(Message).DC;

      SaveIndex := SaveDC(DC);
      try
        BGInfo.PleaseDraw := False;
        GetBGInfo(@BGInfo, FOwner);
        if BiDiMode = bdLeftToRight then
          R := Rect(ColCount * DefaultColWidth, 0, Width, Height) // Right BG line filling
        else
          R := Rect(0, 0, ClientWidth - ColCount * DefaultColWidth, Height); // Left BG line filling

        if BGInfo.BgType = btCache then
          BitBlt(DC, R.Left, R.Top, WidthOf(R), HeightOf(R), BGInfo.Bmp.Canvas.Handle, Left + R.Left + BGInfo.Offset.X, Top + R.Top + BGInfo.Offset.Y, SRCCOPY)
        else
          FillDC(DC, R, BGInfo.Color);
      finally
        RestoreDC(DC, SaveIndex);
        if TWMPAINT(Message).DC = 0 then
          ReleaseDC(Handle, DC);
      end;
    end;

    WM_LBUTTONDOWN: begin
      if not (FOwner.Parent is TsPopupCalendar) then
        inherited
      else
        if FOwner.Parent.Visible then
          if not Clicked then begin
            Clicked := True;
            MouseToCell(TWMMouse(Message).XPos, TWMMouse(Message).YPos, X, Y);
            if not FOwner.ShowWeeks or (X > 0) then
              if (Y > 0) and FOwner.IsValidDate(FOwner.GetCellDate(X, Y)) then begin // If not a day name
{$IFDEF DELPHI6UP}
                FocusCell(X, Y, False);
{$ELSE}
                TAccessStdGrid(Self).FCurrent.X := X;
                TAccessStdGrid(Self).FCurrent.Y := Y;
                Click;
{$ENDIF}
                Invalidate;
              end;

            Clicked := False;
          end;
      end;

    WM_LBUTTONUP: begin
      inherited;
      if FOwner.Parent is TsPopupCalendar then
        if FOwner.Parent.Visible and not Clicked then begin
          Clicked := True;
          TsPopupCalendar(FOwner.Parent).CalendarClick;
          Clicked := False;
        end;
    end

    else inherited;
  end;
end;


procedure TsCalendGrid.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if Assigned(FOwner.OnMouseDown) then
    FOwner.OnMouseDown(FOwner, Button, Shift, X + Left, Y + Top);
end;


procedure TsCalendGrid.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if Assigned(FOwner.OnMouseUp) then
    FOwner.OnMouseUp(FOwner, Button, Shift, X + Left, Y + Top);
end;


procedure TsMonthCalendar.DoChangeAnimated(ForwDirection: boolean; OldBmp: TBitmap);
var
  CacheBmp, NewBmp: TBitmap;
  Position: Integer;
  lTicks: DWord;
  FPos: real;
  DC: hdc;
begin
  FDragBar.Repaint;
  NewBmp := CreateBmp32(FGrid);
  FGrid.PaintTo({$IFDEF DELPHI6UP}NewBmp.Canvas{$ELSE}NewBmp.Canvas.Handle{$ENDIF}, 0, 0);
  CacheBmp := CreateBmp32(FGrid);
  FPos := 0;
  while Round(FPos) < FGrid.Width do begin
    lTicks := GetTickCount;
    FPos := FPos + (FGrid.Width - FPos) / 5;
    Position := Round(FPos);
    BitBlt(CacheBmp.Canvas.Handle, -Position * acMinusPlus[ForwDirection], 0, CacheBmp.Width, CacheBmp.Height, OldBmp.Canvas.Handle, 0, 0, SRCCOPY);
    BitBlt(CacheBmp.Canvas.Handle, (FGrid.Width - Position) * acMinusPlus[ForwDirection], 0, CacheBmp.Width, CacheBmp.Height, NewBmp.Canvas.Handle, 0, 0, SRCCOPY);
    DC := GetDC(FGrid.Handle);
    try
      BitBlt(DC, 0, 0, CacheBmp.Width, CacheBmp.Height, CacheBmp.Canvas.Handle, 0, 0, SRCCOPY);
    finally
      ReleaseDC(FGrid.Handle, DC);
    end;
    WaitTicks(lTicks);
  end;
  OldBmp.Free;
  NewBmp.Free;
  CacheBmp.Free;
end;


procedure TsMonthCalendar.ChangeYear(Delta: Integer; AllowAnimation: boolean = False);
begin
  if FAllowChangeMonth then begin
    if TravellingSelection and IsLeapYear(Year) and (Month = 2) and (Day = 29) then
      Day := 28;

    ShownDate := MakeNewDate(Year + Delta, Month, Day);
    if ShownDate <> 0 then begin
      State := State + [csMonthChanging];
      GoToShownDate(Delta > 0, AllowAnimation);
      State := State - [csMonthChanging];
    end
    else
      ShownDate := FDate;
  end;
end;


{$IFNDEF WIN64}
var
  Lib: HModule = 0;
  ResStringRec: TResStringRec;


initialization
  Lib := LoadLibrary(comctl32);
  if Lib <> 0 then begin
  {$IFDEF DELPHI5}
    ResStringRec.Module := @ACNativeInt(Lib);
  {$ELSE}
    ResStringRec.Module := @Lib;
  {$ENDIF}
    // Search a localization of "Today:"
    ResStringRec.Identifier := {$IFDEF DELPHI_XE2} 4432 {$ELSE} 4163 {$ENDIF};
    s_Today := LoadResString(@ResStringRec);
    FreeLibrary(Lib);
    if s_Today = '' then
      s_Today := 'Today: ';
  end;

finalization
{$ENDIF} // WIN64

end.
