unit sToolEdit;
{$I sDefs.inc}
//{$DEFINE LOGGED}

interface

uses
  Windows, Classes, sConst, StdCtrls, Controls, Messages, SysUtils, Forms, Graphics, Buttons, Dialogs, Mask, FileCtrl, comctrls,
  {$IFDEF DELPHI6UP} Variants, {$ENDIF}
  sDialogs, sDateUtils, sCustomComboEdit, sPopupClndr, sMonthCalendar, acntUtils, sDefaults;


type
{$IFNDEF NOTFORHELP}
  TExecOpenDialogEvent = procedure(Sender: TObject; var Name: string; var Action: boolean) of object;
  TGetPopupFont = procedure(Sender: TObject; PopupFont: TFont) of object;
{$ENDIF}

  TsFileDirEdit = class(TsCustomComboEdit)
{$IFNDEF NOTFORHELP}
  private
    FOnBeforeDialog,
    FOnAfterDialog: TExecOpenDialogEvent;

    FAcceptFiles: boolean;
    FOnDropFiles: TNotifyEvent;
    procedure SetDragAccept (Value: boolean);
    procedure SetAcceptFiles(Value: boolean);
    procedure WMDropFiles(var Msg: TWMDropFiles); message WM_DROPFILES;
  protected
    FMultipleDirs: boolean;
    procedure CreateHandle; override;
    procedure DestroyWindowHandle; override;
    function GetLongName:  string; virtual; abstract;
    function GetShortName: string; virtual; abstract;
    procedure DoAfterDialog (var FileName: string; var Action: boolean);
    procedure DoBeforeDialog(var FileName: string; var Action: boolean);
    procedure ReceptFileDir(const AFileName: acString); virtual; abstract;
    procedure ClearFileList; virtual;
    property MaxLength default MaxByte;
  public
    constructor Create(AOwner: TComponent); override;
{$ENDIF} // NOTFORHELP
    property LongName:  string read GetLongName;
    property ShortName: string read GetShortName;
    property AcceptFiles: boolean read FAcceptFiles write SetAcceptFiles default False;
  published
    property OnBeforeDialog: TExecOpenDialogEvent read FOnBeforeDialog write FOnBeforeDialog;
    property OnAfterDialog:  TExecOpenDialogEvent read FOnAfterDialog  write FOnAfterDialog;
    property OnDropFiles:    TNotifyEvent         read FOnDropFiles    write FOnDropFiles;
  end;


  TFileDialogKind = (dkOpen, dkSave, dkOpenPicture, dkSavePicture);

{$IFDEF DELPHI_XE3}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]{$ENDIF}
  TsFilenameEdit = class(TsFileDirEdit)
{$IFNDEF NOTFORHELP}
  private
    FDialog: TOpenDialog;
    FDialogKind: TFileDialogKind;
    procedure CreateEditDialog;
    function GetFileName:    string;
    function GetFilter:      string;
    function GetDialogTitle: string;
    function GetFileEditStyle: TFileEditStyle;
    function GetOptions:       TOpenOptions;
    function GetDialogFiles:   TStrings;
    function GetHistoryList:   TStrings;
    function GetFilterIndex:   Integer;

    procedure SetFileName   (const Value: string);
    procedure SetFilter     (const Value: string);
    procedure SetDialogTitle(const Value: string);
    procedure SetFileEditStyle(Value: TFileEditStyle);
    procedure SetDialogKind   (Value: TFileDialogKind);
    procedure SetOptions      (Value: TOpenOptions);
    procedure SetHistoryList  (Value: TStrings);
    procedure SetFilterIndex  (Value: Integer);

    function IsCustomTitle:  boolean;
    function IsCustomFilter: boolean;
    function GetString(const Index: Integer): string;
    procedure SetString(const Index: Integer; const Value: string);
  protected
    procedure ReceptFileDir(const AFileName: acString); override;
    procedure ButtonClick; override;
    procedure ClearFileList; override;
    function GetLongName:  string; override;
    function GetShortName: string; override;
    property FileEditStyle: TFileEditStyle read GetFileEditStyle write SetFileEditStyle default fsEdit;
  public
    constructor Create(AOwner: TComponent); override;
    property Dialog: TOpenDialog read FDialog;
{$ENDIF} // NOTFORHELP
    property DialogFiles: TStrings read GetDialogFiles;
    property DialogTitle: string read GetDialogTitle write SetDialogTitle stored IsCustomTitle;
  published
    property AcceptFiles;
{$IFNDEF NOTFORHELP}
    property FilterIndex: Integer read GetFilterIndex write SetFilterIndex default 1;
    property DefaultExt: string index 0 read GetString write SetString;
    property InitialDir: string index 1 read GetString write SetString;
{$ENDIF} // NOTFORHELP
    property FileName:   string read GetFileName write SetFileName stored False;
    property Filter:     string read GetFilter write SetFilter stored IsCustomFilter;
    property DialogKind: TFileDialogKind read FDialogKind    write SetDialogKind default dkOpen;
    property HistoryList: TStrings       read GetHistoryList write SetHistoryList;
    property DialogOptions: TOpenOptions read GetOptions     write SetOptions default [ofHideReadOnly, ofEnableSizing];
  end;


{$IFDEF DELPHI_XE3}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]{$ENDIF}
  TsDirectoryEdit = class(TsFileDirEdit)
{$IFNDEF NOTFORHELP}
  private
    FInitialDir,
    FDialogText: string;

    FNoChangeDir,
    FShowRootBtns: boolean;

    FRoot: TacRoot;
    FOptions: TSelectDirOpts;
  protected
    procedure ButtonClick; override;
    procedure ReceptFileDir(const AFileName: acString); override;
    function GetLongName:  string; override;
    function GetShortName: string; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property AcceptFiles;
    property InitialDir: string read FInitialDir write FInitialDir;
{$ENDIF} // NOTFORHELP
    property DialogOptions: TSelectDirOpts read FOptions write FOptions default [sdAllowCreate, sdPerformCreate, sdPrompt];
    property DialogText:   string  read FDialogText   write FDialogText;
    property Root:         TacRoot read FRoot         write FRoot;
    property MultipleDirs: boolean read FMultipleDirs write FMultipleDirs default False;
    property NoChangeDir:  boolean read FNoChangeDir  write FNoChangeDir  default False;
    property ShowRootBtns: boolean read FShowRootBtns write FShowRootBtns default False;
  end;


  TYearDigits = (dyDefault, dyFour, dyTwo);
  TacDatePortion = (dvYear, dvMonth, dvDay);
  TOnAcceptDate = procedure(Sender: TObject; var aDate: TDateTime; var CanAccept: boolean) of object;

  TsCustomDateEdit = class(TsCustomComboEdit)
{$IFNDEF NOTFORHELP}
  private
    FHooked,
    FShowWeeks,
    FFormatting,
    FCheckOnExit,
    FShowTodayBtn,
    FDefaultToday,
    FShowCurrentDate,
    FDimUnacceptedCells,
    FTravellingSelection: boolean;

    FMinDate,
    FMaxDate,
    FIntDate: TDateTime;

    FOnDrawDay,
    FOnGetCellParams: TGetCellParams;

    FTitle: string;
    FBlanksChar: Char;
    FCalendarHints: TStrings;
    FStartOfWeek: TCalDayOfWeek;
    FWeekends: sConst.TDaysOfWeek;
    FWeekendColor: TColor;
    FYearDigits: TYearDigits;
    FDateFormat: string[10];
    FCloseUp: TNotifyEvent;

    FOnGetPopupFont:  TGetPopupFont;
    FOnAcceptDate:    TOnAcceptDate;
    FAnimated,
    FTextChanging: boolean;
    procedure SetYearDigits(Value: TYearDigits);
    procedure SetCalendarHints(Value: TStrings);
    procedure CalendarHintsChanged(Sender: TObject);
    procedure SetWeekendColor(Value: TColor);
    procedure SetWeekends(Value: sConst.TDaysOfWeek);
    procedure SetStartOfWeek(Value: TCalDayOfWeek);
    procedure SetBlanksChar(Value: Char);
    function TextStored: boolean;
    function FourDigitYear: boolean;
    function FormatSettingsChange(var Message: TMessage): boolean;
    procedure CMExit       (var Message: TCMExit);        message CM_EXIT;
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure CMMouseWheel (var Message: TCMMouseWheel);  message CM_MOUSEWHEEL;
    procedure SetDate(const Index: Integer; const Value: TDateTime);
    function GetDate(const Index: Integer): TDateTime;
    function Portion: TacDatePortion;
  protected
    function CaretPos(X: integer): integer;
    procedure Change; override;
    procedure ClickUpDown(Up: boolean); virtual;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure CreateWindowHandle(const Params: TCreateParams); override;
    procedure DestroyWindowHandle; override;
    function GetDateFormat: string;
    function DateIsStored: boolean;
    procedure ApplyDate(Value: TDateTime); virtual;
    procedure UpdateFormat;
    procedure UpdatePopup;
    procedure PopupWindowShow; override;
    property Formatting: boolean read FFormatting;
    property EditMask stored False;
{$ENDIF} // NOTFORHELP
    property BlanksChar: Char read FBlanksChar write SetBlanksChar default s_Space;
    property CalendarHints: TStrings read FCalendarHints write SetCalendarHints;

    property CheckOnExit: boolean read FCheckOnExit write FCheckOnExit default True;
    property DefaultToday: boolean read FDefaultToday write FDefaultToday default False;

    property MaxLength stored False;
    property StartOfWeek: TCalDayOfWeek read FStartOfWeek write SetStartOfWeek default dowLocaleDefault;
    property Weekends: sConst.TDaysOfWeek read FWeekends write SetWeekends default DefWeekends;
    property WeekendColor: TColor read FWeekendColor write SetWeekendColor default clRed;
    property YearDigits: TYearDigits read FYearDigits write SetYearDigits default dyFour;
    property OnAcceptDate: TOnAcceptDate read FOnAcceptDate write FOnAcceptDate;
{$IFNDEF NOTFORHELP}
  public
    procedure Loaded; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function IsEmpty: boolean;
    function CheckValidDate(CreateRaise: boolean = True): boolean;
    function GetDateMask: string;
    procedure WndProc (var Message: TMessage); override;
    procedure UpdateMask(ChangeDate: boolean = True);
{$ENDIF} // NOTFORHELP
    property Text stored DateIsStored;
    property Date:    TDateTime index 2 read GetDate  write SetDate;
  published
    property MinDate: TDateTime index 0 read FMinDate write SetDate;
    property MaxDate: TDateTime index 1 read FMaxDate write SetDate;

    property Animated:        boolean read FAnimated        write FAnimated        default True;
    property ShowCurrentDate: boolean read FShowCurrentDate write FShowCurrentDate default True;
    property ShowWeeks:       boolean read FShowWeeks       write FShowWeeks       default False;
    property ShowTodayBtn:    boolean read FShowTodayBtn    write FShowTodayBtn    default True;
    property TravellingSelection: boolean read FTravellingSelection write FTravellingSelection default False;
    property DimUnacceptedCells:  boolean read FDimUnacceptedCells  write FDimUnacceptedCells  default True;

    property OnGetCellParams: TGetCellParams read FOnGetCellParams write FOnGetCellParams;
    property OnGetPopupFont:  TGetPopupFont  read FOnGetPopupFont  write FOnGetPopupFont;
    property OnCloseUp:       TNotifyEvent   read FCloseUp         write FCloseUp;
  end;


{$IFDEF DELPHI_XE3}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]{$ENDIF}
  TsDateEdit = class(TsCustomDateEdit)
{$IFNDEF NOTFORHELP}
  public
    constructor Create(AOwner: TComponent); override;
    property EditMask;
  published
    property BlanksChar;
    property CalendarHints;
    property CheckOnExit;
    property ClickKey;
    property Date;
    property DefaultToday;
    property MaxDate;
    property MinDate;
    property PopupAlign;
    property PopupHeight;
    property PopupWidth;
    property StartOfWeek;
    property Text;
    property Weekends;
    property WeekendColor;
    property YearDigits;
    property OnAcceptDate;
    property OnButtonClick;
    property OnChange;
    property OnContextPopup;
{$ENDIF} // NOTFORHELP
  end;


{$IFNDEF NOTFORHELP}
procedure DateFormatChanged;
function StrToDateFmt(const DateFormat, S: string): TDateTime;
{$ENDIF} // NOTFORHELP

implementation

uses
  ShellAPI, {$IFDEF DELPHI_10}Vcl.Consts{$ELSE}Consts{$ENDIF}, ExtDlgs, math,
  {$IFDEF LOGGED}sDebugMsgs, {$ENDIF}
  sMessages, acPathDialog, acShellCtrls, sSkinManager, sGlyphUtils, sGraphUtils;


procedure TsFileDirEdit.DoBeforeDialog(var FileName: string; var Action: boolean);
begin
  if Assigned(FOnBeforeDialog) then
    FOnBeforeDialog(Self, FileName, Action);
end;


procedure TsFileDirEdit.DoAfterDialog(var FileName: string; var Action: boolean);
begin
  if Assigned(FOnAfterDialog) then
    FOnAfterDialog(Self, FileName, Action);
end;


constructor TsFileDirEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SkinData.COC := COC_TsFileDirEdit;
  MaxLength := MaxByte;
end;


procedure TsFileDirEdit.CreateHandle;
begin
  inherited CreateHandle;
  if FAcceptFiles then
    SetDragAccept(True);
end;


procedure TsFileDirEdit.DestroyWindowHandle;
begin
  SetDragAccept(False);
  inherited DestroyWindowHandle;
end;


procedure TsFileDirEdit.SetDragAccept(Value: boolean);
begin
  if not (csDesigning in ComponentState) and (Handle <> 0) then
    DragAcceptFiles(Handle, Value);
end;


procedure TsFileDirEdit.SetAcceptFiles(Value: boolean);
begin
  if FAcceptFiles <> Value then begin
    SetDragAccept(Value);
    FAcceptFiles := Value;
  end;
end;


procedure TsFileDirEdit.WMDropFiles(var Msg: TWMDropFiles);
const
  maxlen = 254;
var
  i, Num: Cardinal;
  FileName: acString;
  pchr: array [0..maxlen] of acChar;
begin
  Msg.Result := 0;
  Num := DragQueryFile(Msg.Drop, $FFFFFFFF, nil, 0);
  if Num > 0 then begin
    ClearFileList;
    for i := 0 to Num - 1 do begin
      {$IFDEF TNTUNICODE}DragQueryFileW{$ELSE}DragQueryFile{$ENDIF}(Msg.Drop, i, pchr, maxlen);
      FileName := acString(pchr);
      ReceptFileDir(FileName);
      if not FMultipleDirs then
        Break;
    end;
    if Assigned(FOnDropFiles) then
      FOnDropFiles(Self);
  end;
  DragFinish(Msg.Drop);
end;


procedure TsFileDirEdit.ClearFileList;
begin
//
end;


constructor TsFilenameEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SkinData.COC := COC_TsFilenameEdit;
  FDefBmpID := iBTN_OPENFILE;
  CreateEditDialog;
end;


procedure TsFilenameEdit.CreateEditDialog;
var
  NewDialog: TOpenDialog;
begin
  case FDialogKind of
    dkOpen: begin
      NewDialog := TsOpenDialog.Create(Self);
      TsOpenDialog(NewDialog).ZipShowing := zsAsFile;
    end;
    dkOpenPicture: NewDialog := TOpenPictureDialog.Create(Self);
    dkSave:        NewDialog := TsSaveDialog.Create(Self);
    dkSavePicture: NewDialog := TSavePictureDialog.Create(Self)
    else           NewDialog := nil;
  end;
  if FDialog <> nil then begin
    NewDialog.DefaultExt    := FDialog.DefaultExt;
    NewDialog.FileEditStyle := FDialog.FileEditStyle;
    NewDialog.FileName      := FDialog.FileName;
    NewDialog.Filter        := FDialog.Filter;
    NewDialog.FilterIndex   := FDialog.FilterIndex;
    NewDialog.InitialDir    := FDialog.InitialDir;
    NewDialog.HistoryList   := FDialog.HistoryList;
    NewDialog.Options       := FDialog.Options;
    NewDialog.Files.Assign(FDialog.Files);
    FreeAndNil(FDialog);
  end
  else begin
    NewDialog.Filter := SDefaultFilter;
    NewDialog.Options := [ofHideReadOnly, ofEnableSizing];
  end;
  FDialog := NewDialog;
end;


function TsFilenameEdit.IsCustomTitle: boolean;
begin
  Result := CompareStr(acs_FileOpen, FDialog.Title) <> 0;
end;


function TsFilenameEdit.IsCustomFilter: boolean;
begin
  Result := CompareStr(sDefaultFilter, FDialog.Filter) <> 0;
end;


procedure TsFilenameEdit.ButtonClick;
var
  Temp: string;
  Flag: boolean;
begin
  inherited;
  Temp := inherited Text;
  Flag := True;
  DoBeforeDialog(Temp, Flag);
  if Flag then begin
    if ValidFileName(Temp) then begin
      if acDirExists(ExtractFilePath(Temp)) then
        InitialDir := ExtractFilePath(Temp);

      if (ExtractFileName(Temp) = '') or not ValidFileName(ExtractFileName(Temp)) then
        Temp := '';

      FDialog.FileName := Temp;
    end
    else
      FDialog.FileName := '';

    if acDirExists(InitialDir) then
      FDialog.InitialDir := InitialDir;

    FDialog.HelpContext := Self.HelpContext;
    Flag := FDialog.Execute;
    if Flag then
      Temp := FDialog.FileName;

    if CanFocus then
      SetFocus;

    DoAfterDialog(Temp, Flag);
    if Flag then begin
      inherited Text := Temp;
      InitialDir := ExtractFilePath(FDialog.FileName);
    end;
  end;
end;


function TsFilenameEdit.GetLongName: string;
begin
  Result := FileName;
end;


function TsFilenameEdit.GetShortName: string;
begin
  Result := FileName;
end;


function TsFilenameEdit.GetString(const Index: Integer): string;
begin
  case Index of
    0: Result := FDialog.DefaultExt;
    1: Result := FDialog.InitialDir;
  end;
end;


procedure TsFilenameEdit.ClearFileList;
begin
  FDialog.Files.Clear;
end;


procedure TsFilenameEdit.ReceptFileDir(const AFileName: acString);
begin
  if FMultipleDirs then begin
    if FDialog.Files.Count = 0 then
      SetFileName(AFileName);

    FDialog.Files.Add(AFileName);
  end
  else
    SetFileName(AFileName);
end;


function TsFilenameEdit.GetDialogFiles: TStrings;
begin
  Result := FDialog.Files;
end;


function TsFilenameEdit.GetFileEditStyle: TFileEditStyle;
begin
  Result := FDialog.FileEditStyle;
end;


function TsFilenameEdit.GetFileName: string;
begin
  Result := inherited Text;
end;


function TsFilenameEdit.GetFilter: string;
begin
  Result := FDialog.Filter;
end;


function TsFilenameEdit.GetFilterIndex: Integer;
begin
  Result := FDialog.FilterIndex;
end;


function TsFilenameEdit.GetHistoryList: TStrings;
begin
  Result := FDialog.HistoryList;
end;


function TsFilenameEdit.GetOptions: TOpenOptions;
begin
  Result := FDialog.Options;
end;


function TsFilenameEdit.GetDialogTitle: string;
begin
  Result := FDialog.Title;
end;


procedure TsFilenameEdit.SetDialogKind(Value: TFileDialogKind);
begin
  if FDialogKind <> Value then begin
    FDialogKind := Value;
    CreateEditDialog;
  end;
end;


procedure TsFilenameEdit.SetFileEditStyle(Value: TFileEditStyle);
begin
  FDialog.FileEditStyle := Value;
end;


procedure TsFilenameEdit.SetFileName(const Value: string);
begin
  if (Value = '') or ValidFileName(Value) then begin
    inherited Text := Value;
    ClearFileList;
  end
  else
    raise Exception.CreateFmt('Invalid file name', [Value]);
end;


procedure TsFilenameEdit.SetFilter(const Value: string);
begin
  FDialog.Filter := Value;
end;


procedure TsFilenameEdit.SetFilterIndex(Value: Integer);
begin
  FDialog.FilterIndex := Value;
end;


procedure TsFilenameEdit.SetHistoryList(Value: TStrings);
begin
  FDialog.HistoryList := Value;
end;


procedure TsFilenameEdit.SetOptions(Value: TOpenOptions);
begin
  if Value <> FDialog.Options then begin
    FMultipleDirs := ofAllowMultiSelect in Value;
    FDialog.Options := Value;
    if not FMultipleDirs then
      ClearFileList;
  end;
end;


procedure TsFilenameEdit.SetString(const Index: Integer; const Value: string);
begin
  case Index of
    0: FDialog.DefaultExt := Value;
    1: FDialog.InitialDir := Value;
  end;
end;


procedure TsFilenameEdit.SetDialogTitle(const Value: string);
begin
  FDialog.Title := Value;
end;


constructor TsDirectoryEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SkinData.COC := COC_TsDirectoryEdit;
  FDefBmpID := iBTN_OPENFOLDER;
  FOptions := [sdAllowCreate, sdPerformCreate, sdPrompt];
  FRoot := SRFDesktop;
  FShowRootBtns := False;
end;


procedure TsDirectoryEdit.ButtonClick;
var
  s: string;
  Flag: boolean;
  bw: integer;
begin
  inherited;
  s := Text;
  Flag := True;
  DoBeforeDialog(s, Flag);
  if Flag then begin
    if (s = '') and (InitialDir <> '') then
      s := InitialDir;

    if acDirExists(s) then begin
      if not NoChangeDir then
        ChDir(s);
    end
    else
      s := '';

    PathDialogForm := TPathDialogForm.Create(Application);
    with PathDialogForm do begin
      InitLngCaptions;
      sBitBtn3.Visible := sdAllowCreate in DialogOptions;
      if ShowRootBtns then begin
        sScrollBox1.Visible := True;
        sLabel1.Visible := True;
        bw := GetSystemMetrics(SM_CXSIZEFRAME);
        sShellTreeView1.Left := sShellTreeView1.Left + sScrollBox1.Width + 4;
        sBitBtn1.Left := sBitBtn1.Left + sScrollBox1.Width + 4;
        sBitBtn2.Left := sBitBtn2.Left + sScrollBox1.Width + 4;
        Width := Width + sScrollBox1.Width + 4 + bw;
        GenerateButtons;
      end
      else
        sLabel1.Visible := False;

      UpdateAnchors;
      try
        sShellTreeView1.BoundLabel.Caption := DialogText;
        sShellTreeView1.Root := FRoot;
        if (s <> '') and acDirExists(s) then
          sShellTreeView1.Path := s;

        if ShowModal = mrOk then begin
          s := sShellTreeView1.Path;
          if (s <> '') and acDirExists(s) then
            InitialDir := s;
        end
        else begin
          s := Text;
          Flag := False;
        end;

      finally
        FreeAndNil(PathDialogForm);
      end;
    end;
    DoAfterDialog(s, Flag);
    if Flag then
      Text := s;
  end;
end;


procedure TsDirectoryEdit.ReceptFileDir(const AFileName: acString);
var
  s: string;
begin
  if FileExists(AFileName) then
    s := ExtractFilePath(AFileName)
  else
    s := AFileName;

  if (Text = '') or not MultipleDirs then
    Text := s
  else
    Text := Text + ';' + s;
end;


function TsDirectoryEdit.GetLongName: string;
var
  s: string;
  Pos: Integer;
begin
  if not MultipleDirs then
    Result := ShortToLongPath(Text)
  else begin
    Result := '';
    Pos := 1;
    while Pos <= Length(Text) do begin
      s := ShortToLongPath(ExtractSubstr(Text, Pos, [';']));
      if (Result <> '') and (s <> '') then
        Result := Result + ';';

      Result := Result + s;
    end;
  end;
end;


function TsDirectoryEdit.GetShortName: string;
var
  s: string;
  Pos: Integer;
begin
  if not MultipleDirs then
    Result := LongToShortPath(Text)
  else begin
    Result := '';
    Pos := 1;
    while Pos <= Length(Text) do begin
      s := LongToShortPath(ExtractSubstr(Text, Pos, [';']));
      if (Result <> '') and (s <> '') then
        Result := Result + ';';

      Result := Result + s;
    end;
  end;
end;


constructor TsCustomDateEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SkinData.COC := COC_TsCustomDateEdit;

  FBlanksChar := s_Space;
  FTitle := 'Date select';
  FStartOfWeek := dowLocaleDefault;
  FWeekends := DefWeekends;
  FWeekendColor := clRed;
  FYearDigits := dyFour;
  FCalendarHints := TStringList.Create;
  TStringList(FCalendarHints).OnChange := CalendarHintsChanged;
  FDefBmpID := iBTN_DATE;
  FShowCurrentDate := True;
  FTravellingSelection := False;
  FDimUnacceptedCells := True;
  FShowWeeks := False;
  FShowTodayBtn := True;
  FDefaultToday := False;
  FAnimated := True;
  FTextChanging := False;
  FCheckOnExit := True;

  ControlState := ControlState + [csCreating];
  Width := 86;
  try
    UpdateFormat;
  finally
    ControlState := ControlState - [csCreating];
  end;
end;


destructor TsCustomDateEdit.Destroy;
begin
  if FHooked then begin
    Application.UnhookMainWindow(FormatSettingsChange);
    FHooked := False;
  end;
  if Assigned(FPopupwindow) then
    FreeAndNil(FPopupWindow);

  TStringList(FCalendarHints).OnChange := nil;
  FreeAndNil(FCalendarHints);
  inherited Destroy;
end;


procedure TsCustomDateEdit.CreateWindowHandle(const Params: TCreateParams);
begin
  inherited CreateWindowHandle(Params);
  if Handle <> 0 then begin
    UpdateMask;
    if not (csDesigning in ComponentState) and not (IsLibrary or FHooked) then begin
      Application.HookMainWindow(FormatSettingsChange);
      FHooked := True;
    end;
  end;
end;


procedure TsCustomDateEdit.DestroyWindowHandle;
begin
  if FHooked then begin
    Application.UnhookMainWindow(FormatSettingsChange);
    FHooked := False;
  end;
  inherited DestroyWindowHandle;
end;


procedure TsCustomDateEdit.UpdateFormat;
begin
  FDateFormat := DefDateFormat(FourDigitYear);
end;


function TsCustomDateEdit.GetDate(const Index: Integer): TDateTime;
begin
  if (Pos(s_Space, Text) > 0) or (NullDate = FIntDate) then
    if DefaultToday and not (csDesigning in ComponentState) then
      Result := SysUtils.Date
    else
      Result := NullDate
  else
    Result := Trunc(FIntDate);
end;


function TsCustomDateEdit.GetDateFormat: string; // Unused
begin
  Result := FDateFormat;
end;


function TsCustomDateEdit.TextStored: boolean;
begin
  Result := not IsEmpty;
end;


function TsCustomDateEdit.CheckValidDate(CreateRaise: boolean = True): boolean;
var
  i, l: integer;
  s, sub: string;
{$IFDEF DELPHI7UP}
  D: TDateTime;
{$ENDIF}
begin
  Result := False;
  if TextStored then
    try
      FFormatting := True;
      try
        i := iff(FourDigitYear, 4, 2);
        l := Length(Text) - i + 1;
        if (Copy(Text, l, 1) = s_Space) or (Copy(Text, l + 1, 1) = s_Space) then begin
          s := Text;
          Delete(s, l, i);
          l := ExtractYear(SysUtils.Date);
          Text := s + IntToStr(l);
        end
        else
          if FourDigitYear then
            if (Copy(Text, l + 2, 1) = s_Space) or (Copy(Text, l + 3, 1) = s_Space) then begin
              s := Text;
              sub := Copy(IntToStr(ExtractYear(SysUtils.Date)), 1, 2) + Copy(Text, l, 2);
              Delete(s, l, i);
              Text := s + sub;
            end;

{$IFDEF DELPHI7UP}
        Result := TryStrToDate(Text, D);
        if Result then
          SetDate(2, D)
        else begin
          s := acs_InvalidDate + ': ' + Text;
          if Assigned(OnValidateError) then
            OnValidateError(Self, s)
          else
            if CreateRaise then
              Raise EConvertError.CreateFmt(acs_InvalidDate, [Text]);
        end;
{$ELSE}
        SetDate(2, StrToDateFmt(FDateFormat, Text));
        Result := True;
{$ENDIF}
      finally
        FFormatting := False;
      end;
    except
      if CanFocus then
        SetFocus;

      raise;
    end;
end;


const
  aTextLength: array [boolean] of integer = (8,  10);
  aIncrement:  array [boolean] of integer = (-1, 1);
  acPositions: array [TDateOrder, TacDatePortion] of integer = (
    (6, 0, 3), // doMDY
    (6, 3, 0), // doDMY
    (0, 3, 6)  // doYMD
  );

procedure TsCustomDateEdit.ClickUpDown(Up: boolean);
var
  cPortion: TacDatePortion;
  Increment: integer;
  NewDate: TDateTime;

  procedure MakeSelection;
  var
    x, l: integer;
    dOrder: TDateOrder;
  begin
    l := 2;
    dOrder := GetDateOrder({$IFDEF DELPHI_XE}FormatSettings.{$ENDIF}ShortDateFormat);
    x := acPositions[dOrder, cPortion];
    if FourDigitYear then
      if cPortion = dvYear then
        inc(l, 2)
      else
        if dOrder = doYMD then
          inc(x, 2);

    SelStart := x;
    SelLength := l;
  end;

begin
  if not ReadOnly then
    if length(Text) = aTextLength[FourDigitYear] then begin
      cPortion := Portion;
      Increment := aIncrement[Up];
      case cPortion of
        dvYear: begin
          NewDate := Date;
          acChangeYear(Increment, NewDate);
          Date := NewDate;
        end;

        dvMonth: begin
          NewDate := Date;
          acChangeMonth(Increment, NewDate);
          Date := NewDate;
        end;

        dvDay:
          Date := Date + Increment;
      end;
      MakeSelection;
    end;
end;


procedure TsCustomDateEdit.Change;
begin
  if not FFormatting then
    inherited Change;
end;


function StrToDateFmtDef(const DateFormat, S: string; Default: TDateTime): TDateTime;
begin
  if not InternalStrToDate(DateFormat, S, Result) then
    Result := Trunc(Default);
end;


procedure TsCustomDateEdit.CMExit(var Message: TCMExit);
begin
  if not (csDesigning in ComponentState) and CheckOnExit and CheckValidDate then
    if (FMaxDate <> 0) and (Date > FMaxDate) then
      Date := FMaxDate
    else
      if (FMinDate <> 0) and (Date < FMinDate) then
        Date := FMinDate
      else
        Date := StrToDateFmtDef(FDateFormat, Text, Date);

  inherited;
end;


procedure TsCustomDateEdit.CMMouseWheel(var Message: TCMMouseWheel);
begin
  inherited;
  if not ReadOnly and (Message.Result = 0) then
    if Message.WheelDelta < 0 then
      ClickUpDown(False)
    else
      if Message.WheelDelta > 0 then
        ClickUpDown(True);
end;


procedure TsCustomDateEdit.SetBlanksChar(Value: Char);
begin
  if Value <> FBlanksChar then begin
    if Value < s_Space then
      Value := s_Space;

    FBlanksChar := Value;
    UpdateMask;
  end;
end;


procedure TsCustomDateEdit.UpdateMask(ChangeDate: boolean = True);
var
  DateValue: TDateTime;
  OldFormat: string[10];
begin
  DateValue := GetDate(2);
  OldFormat := FDateFormat;
  UpdateFormat;
  if (GetDateMask <> EditMask) or (OldFormat <> FDateFormat) then begin { force update }
    EditMask := '';
    EditMask := GetDateMask;
  end;
  UpdatePopup;
  if ChangeDate then
    SetDate(2, DateValue);
end;


function TsCustomDateEdit.FormatSettingsChange(var Message: TMessage): boolean;
begin
  Result := False;
  if (Message.Msg = WM_WININICHANGE) and Application.UpdateFormatSettings then
    UpdateMask;
end;


function TsCustomDateEdit.FourDigitYear: boolean;
begin
  Result := FYearDigits = dyFour;
  Result := Result or ((FYearDigits = dyDefault) and sDateUtils.NormalYears);
end;


function TsCustomDateEdit.GetDateMask: string;
begin
  Result := DefDateMask(FBlanksChar, FourDigitYear);
end;


procedure TsCustomDateEdit.ApplyDate(Value: TDateTime);
begin
  SetDate(2, Value);
  SelectAll;
end;

{
function TsCustomDateEdit.GetDialogTitle: string;
begin
  Result := FTitle;
end;
}

procedure TsCustomDateEdit.SetDate(const Index: Integer; const Value: TDateTime);
var
  D: TDateTime;
begin
  case Index of
    0:
      if (FMaxDate = NullDate) or (Value <= FMaxDate) then
        if FMinDate <> Value then begin
          FMinDate := Value;
          if Date < FMinDate then
            Date := FMinDate;
        end;

    1:
      if (FMaxDate <> Value) and (Value >= FMinDate) then begin
        FMaxDate := Value;
        if Date > FMaxDate then
          Date := FMaxDate;
      end;

    2: begin
      if not ValidDate(Value) or (Value = NullDate) then
        if DefaultToday and not (csDesigning in ComponentState) then
          FIntDate := SysUtils.Date
        else
          FIntDate := NullDate
      else
        FIntDate := Value;

      D := Self.Date;
      if not FTextChanging then begin
        FTextChanging := True;
        if FIntDate = NullDate then
          Text := ''
        else
          TEdit(Self).Text := FormatDateTime(FDateFormat, FIntDate);

        FTextChanging := False;
      end;
      Modified := D <> Date;
      SkinData.BGChanged := SkinData.BGChanged or Modified;
    end;
  end;
end;

{
procedure TsCustomDateEdit.SetDialogTitle(const Value: string);
begin
  FTitle := Value;
end;


function TsCustomDateEdit.IsCustomTitle: boolean;
begin
  Result := (CompareStr('Date select', DialogTitle) <> 0) and (FTitle <> '');
end;
}

function TsCustomDateEdit.IsEmpty: boolean;
begin
  Result := IsEmptyStr(Text, [#0, s_Space, {$IFDEF DELPHI_XE}FormatSettings.{$ENDIF}DateSeparator, FBlanksChar]);
end;


procedure TsCustomDateEdit.UpdatePopup;
var
  i: integer;
begin
  if (FPopupWindow <> nil) and (TsPopupCalendar(FPopupWindow).FCalendar <> nil) then
    with TsPopupCalendar(FPopupWindow) do begin
      BiDiMode := Self.BiDiMode;
      FCalendar.StartOfWeek := FStartOfWeek;
      FCalendar.Weekends := FWeekends;
      FCalendar.WeekendColor := FWeekendColor;
      sMonthCalendar1.ShowWeeks := FShowWeeks;
      sMonthCalendar1.ShowTodayBtn := FShowTodayBtn;
      sMonthCalendar1.TravellingSelection := FTravellingSelection;
//      sMonthCalendar1.ShowSelectAlways := not HideSelectedCell;
      FPopupWindow.Height := FormHeight + Integer(FShowTodayBtn) * 16;

      if Assigned(FOnGetCellParams) then
        sMonthCalendar1.OnGetCellParams := FOnGetCellParams
      else
        sMonthCalendar1.OnGetCellParams := nil;

      if Assigned(FOnDrawDay) then
        sMonthCalendar1.OnDrawDay := FOnDrawDay
      else
        sMonthCalendar1.OnDrawDay := nil;

      if Assigned(FCloseUp) then
        TsPopupCalendar(FPopupWindow).OnCloseUp := FCloseUp
      else
        TsPopupCalendar(FPopupWindow).OnCloseUp := nil;

      if SkinData.Skinned and (DefaultManager <> nil) then
        Color := DefaultManager.GetGlobalColor
      else
        Color := clBtnFace;

      for i := 0 to min(CalendarHints.Count - 1, 3) do begin
        FCalendar.FBtns[i].Hint := CalendarHints[i];
        FCalendar.FBtns[i].ShowHint := ShowHint;
        if i = 3 then
          break;
      end;
    end;
end;


procedure TsCustomDateEdit.SetYearDigits(Value: TYearDigits);
begin
  if FYearDigits <> Value then begin
    FYearDigits := Value;
    UpdateMask;
  end;
end;


procedure TsCustomDateEdit.SetCalendarHints(Value: TStrings);
begin
  FCalendarHints.Assign(Value);
end;


procedure TsCustomDateEdit.CalendarHintsChanged(Sender: TObject);
begin
  TStringList(FCalendarHints).OnChange := nil;
  try
    while FCalendarHints.Count > 4 do
      FCalendarHints.Delete(FCalendarHints.Count - 1);
  finally
    TStringList(FCalendarHints).OnChange := CalendarHintsChanged;
  end;
  if not (csDesigning in ComponentState) then
    UpdatePopup;
end;


procedure TsCustomDateEdit.SetWeekendColor(Value: TColor);
begin
  if Value <> FWeekendColor then begin
    FWeekendColor := Value;
    UpdatePopup;
  end;
end;


procedure TsCustomDateEdit.SetWeekends(Value: sConst.TDaysOfWeek);
begin
  if Value <> FWeekends then begin
    FWeekends := Value;
    UpdatePopup;
  end;
end;


procedure TsCustomDateEdit.SetStartOfWeek(Value: TCalDayOfWeek);
begin
  if Value <> FStartOfWeek then begin
    FStartOfWeek := Value;
    UpdatePopup;
  end;
end;


procedure TsCustomDateEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if (Key in [VK_PRIOR, VK_NEXT, VK_LEFT, VK_UP, VK_RIGHT, VK_DOWN, VK_ADD, VK_SUBTRACT, VK_RETURN, VK_ESCAPE]) and DroppedDown then begin
    TsPopupCalendar(FPopupWindow).FCalendar.FGrid.KeyDown(Key, Shift);
    Key := 0;
  end
  else
    if (Shift = []) and DirectInput and not ReadOnly then
      case Key of
        VK_UP, VK_ADD: begin
          if Date = NullDate then
            ApplyDate(Now + 1)
          else
            ClickUpDown(True);

          Key := 0;
        end;

        VK_DOWN, VK_SUBTRACT: begin
          if Date = NullDate then
            ApplyDate(Now - 1)
          else
            ClickUpDown(False);

          Key := 0;
        end;
      end;

  inherited KeyDown(Key, Shift);
end;


procedure TsCustomDateEdit.KeyPress(var Key: Char);
begin
  if CharInSet(Key, ['T', 't', CharPlus, CharMinus]) and DroppedDown then begin
    TsPopupCalendar(FPopupWindow).FCalendar.FGrid.KeyPress(Key);
    Key := #0;
  end
  else
    if DirectInput and not ReadOnly then
      case Key of
        'T', 't': begin
          ApplyDate(Trunc(Now));
          Key := #0;
        end;

        CharPlus, CharMinus:
          Key := #0;
      end;

  inherited KeyPress(Key);
end;


procedure TsCustomDateEdit.PopupWindowShow;
begin
  FreeAndNil(FPopupWindow);
  if sPopupCalendar <> nil then
    sPopupCalendar.Close;

  FPopupWindow := TsPopupCalendar.Create(Self);
  sPopupCalendar := TForm(FPopupWindow);
  with TsPopupCalendar(FPopupWindow) do begin
    Font.Assign(Self.Font);
    if SkinData.SkinManager <> nil then
      Font.Height := Font.Height * 100 div aScalePercents[Self.SkinData.SkinManager.GetScale];

    if Assigned(OnGetPopupFont) then
      OnGetPopupFont(Self, Font);

    FCalendar.MaxDate := MaxDate;
    FCalendar.MinDate := MinDate;
    FCalendar.Animated := FAnimated;
    if Self.Date <> NullDate then
      FCalendar.CalendarDate := Self.Date
    else
      FCalendar.CalendarDate := SysUtils.Date;

    FCalendar.ControlStyle := FCalendar.ControlStyle - [csDoubleClicks];
    FEditor := Self;
  end;
  UpdatePopup;
  inherited;
end;


const
//  TDateOrder = (doMDY, doDMY, doYMD);
  acPortArray: array[TDateOrder, 0..9] of TacDatePortion =
    ((dvMonth, dvMonth, dvDay, dvDay, dvDay, dvYear, dvYear, dvYear, dvYear, dvYear),
     (dvDay, dvDay, dvMonth, dvMonth, dvMonth, dvYear, dvYear, dvYear, dvYear, dvYear),
     (dvYear, dvYear, dvYear, dvYear, dvMonth, dvMonth, dvMonth, dvDay, dvDay, dvDay));

function TsCustomDateEdit.Portion: TacDatePortion;
var
  FCurPos, l: DWord;
  dOrder: TDateOrder;
begin
  FCurPos := ACNativeUInt(SendMessage(Handle, EM_GETSEL, 0, 0)) mod $10000;
  dOrder := GetDateOrder({$IFDEF DELPHI_XE}FormatSettings.{$ENDIF}ShortDateFormat);
  if (dOrder = doYMD) and not FourDigitYear then
    inc(FCurPos);

  l := Length(Text);
  if not AutoSelect or (FCurPos <= l) then
    Result := acPortArray[dOrder, FCurPos]
  else
    Result := dvDay;
end;


procedure TsCustomDateEdit.Loaded;
begin
  inherited;
  UpdateMask;
end;


procedure TsCustomDateEdit.WndProc(var Message: TMessage);
begin
{$IFDEF LOGGED}
  AddToLog(Message);
{$ENDIF}
  case Message.Msg of
    SM_ALPHACMD:
      case Message.WParamHi of
        AC_REMOVESKIN:
          if ACUInt(Message.LParam) = ACUInt(SkinData.SkinManager) then begin
            if Assigned(FPopupWindow) then
              FPopupWindow.BroadCast(Message);

            UpdateFormat;
            UpdateMask;
          end;

        AC_SETNEWSKIN, AC_REFRESH:
          if ACUInt(Message.LParam) = ACUInt(SkinData.SkinManager) then begin
            if Assigned(FPopupWindow) then
              FPopupWindow.BroadCast(Message);

            UpdateFormat;
            UpdateMask(False);
          end;
      end;
  end;
  inherited;
  case Message.Msg of
    CM_CHANGED, CM_TEXTCHANGED:
      if not (csLoading in ComponentState) and not FTextChanging then begin
        FTextChanging := True;
        if (Text = '') or (Pos(s_Space, Text) > 0) or not TextStored then
          Date := 0
        else
          SetDate(2, StrToDateFmtDef(FDateFormat, Text, Date));

        FTextChanging := False;
     end;
  end;
end;


function TsCustomDateEdit.DateIsStored: boolean;
begin
  Result := not DefaultToday
end;


procedure TsCustomDateEdit.WMLButtonDown(var Message: TWMLButtonDown);
var
  l, Pos, YearOffset: integer;
begin
  inherited;
  Pos := CaretPos(Message.XPos);
  l := Length(Text);
  if not AutoSelect or (Pos <= l) then begin
    YearOffset := 2 * integer(FourDigitYear and (UpperCase(FDateFormat[1]) = 'Y'));
    Pos := CaretPos(Message.XPos);
    if Between(Pos, 1, 3 + YearOffset) then begin
      SelStart  := 0;
      SelLength := 2 + YearOffset;
    end
    else
      if Between(Pos, 4 + YearOffset, 6 + YearOffset) then begin
        SelStart  := 3 + YearOffset;
        SelLength := 2;
      end
      else begin
        SelStart  := 6 + YearOffset;
        SelLength := 2 + 2 * integer(FourDigitYear and (UpperCase(FDateFormat[1]) <> 'Y'));
      end;
  end;
end;


function TsCustomDateEdit.CaretPos(X: integer): integer;
var
  i, l, pos, w: integer;
begin
  Result := 0;
  pos := 0;
  l := iff(FourDigitYear, 10, 8);
  for i := 1 to l do begin
    case i of
      3, 6: w := GetStringSize(Font.Handle, s_Comma).cx
      else  w := GetStringSize(Font.Handle, ZeroChar).cx;
    end;
    if X < pos + w then begin
      Result := i;
      Exit;
    end
    else
      inc(pos, w);
  end;
end;


constructor TsDateEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SkinData.COC := COC_TsDateEdit;
  EditMask := '!90/90/0000;1; ';
  UpdateMask;
end;


procedure DateFormatChanged;

  procedure IterateControls(AControl: TWinControl);
  var
    I: Integer;
  begin
    with AControl do
      for I := 0 to ControlCount - 1 do
        if Controls[I] is TsCustomDateEdit then
          TsCustomDateEdit(Controls[I]).UpdateMask
        else
          if Controls[I] is TWinControl then
            IterateControls(TWinControl(Controls[I]));
  end;

var
  I: Integer;
begin
  if Screen <> nil then
    for I := 0 to Screen.FormCount - 1 do
      IterateControls(Screen.Forms[I]);
end;


function StrToDateFmt(const DateFormat, S: string): TDateTime;
begin
  if not InternalStrToDate(DateFormat, S, Result) then
    Raise EConvertError.CreateFmt(acs_InvalidDate, [S]);
end;

end.
