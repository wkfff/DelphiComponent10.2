unit sSpinEdit;
{$I sDefs.inc}
//{$DEFINE LOGGED}

interface

uses
  Windows, Classes, StdCtrls, ExtCtrls, Controls, Messages, SysUtils, Forms, Graphics, buttons,
  {$IFDEF DELPHI7UP} Types, {$ENDIF}
  sEdit, acntUtils, sConst, sGraphUtils, sSpeedButton;

type

  TsBaseSpinEdit = class(TsEdit)
{$IFNDEF NOTFORHELP}
  private
    FBtn1State,
    FBtn2State,
    BtnSkinNdx: integer;

    FCreating,
    MousePressed,
    FEditorEnabled,
    FAllowNegative,
    FShowSpinButtons,
    FFlatSpinButtons: Boolean;

    FPrevTabControl,
    FNextTabControl: TWinControl;

    FOnUpClick,
    FOnDownClick: TNotifyEvent;

    FRepeatTimer: TTimer;

    FAlignment: TAlignment;
    function GetMinHeight: Integer;
    procedure WMSize       (var Message: TWMSize);       message WM_SIZE;
    procedure WMPaste      (var Message: TWMPaste);      message WM_PASTE;
    procedure WMCut        (var Message: TWMCut);        message WM_CUT;
    procedure WMGetDlgCode (var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure CMTextChanged(var Message: TMessage);      message CM_TEXTCHANGED;
    procedure WMNCCalcSize (var Message: TWMNCCalcSize); message WM_NCCALCSIZE;
    procedure WMNCHitTest  (var Message: TWMMouse); message WM_NCHITTEST;
    procedure WMNCMouseMove(var Message: TWMMouse); message WM_NCMOUSEMOVE;
    procedure WMNCMouseDown(var Message: TWMMouse); message WM_NCLBUTTONDOWN;
    procedure WMNCMouseUp  (var Message: TWMMouse); message WM_NCLBUTTONUP;
    procedure SetAlignment      (const Value: TAlignment);
    procedure SetFlatSpinButtons(const Value: Boolean);
    procedure SetShowSpinButtons(const Value: Boolean); virtual;

    procedure TimerExpired(Sender: TObject);
    procedure CreateRepeatTimer;
    function BtnRect(IsUp: boolean): TRect;
    procedure PaintBtns(aBmp: TBitmap);
    procedure PaintBtn(aBmp: TBitmap; R: TRect; IsUp: boolean);
    procedure PaintBtnStd(aBmp: TBitmap; R: TRect; IsUp: boolean);
    function FontStored: boolean;
    function ColorStored: boolean;
    procedure SetBtn1State(const Value: integer);
    procedure SetBtn2State(const Value: integer);
  protected
    procedure SetEditRect; virtual;
    procedure UpClick  (Sender: TObject); virtual; abstract;
    procedure DownClick(Sender: TObject); virtual; abstract;
    procedure KeyDown   (var Key: Word; Shift: TShiftState); override;
    procedure KeyPress  (var Key: Char); override;
    function IsValidChar(var Key: Char): Boolean; virtual;
    function TextRect: TRect;
    procedure PaintText; override;
    procedure PrepareStd;
    function PrepareCache: boolean; override;
    procedure UpdateBtnIndex(aSkinned: boolean);
    property Btn1State: integer read FBtn1State write SetBtn1State;
    property Btn2State: integer read FBtn2State write SetBtn2State;
  public
    constructor Create(AOwner: TComponent); override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    destructor Destroy; override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    property CharCase;
    procedure Loaded; override;
    procedure WndProc(var Message: TMessage); override;
  published
    property Color stored ColorStored;
    property Font stored FontStored;
    property ParentFont stored FontStored;
    property NextTabControl:  TWinControl  read FNextTabControl  write FNextTabControl;
    property PrevTabControl:  TWinControl  read FPrevTabControl  write FPrevTabControl;
{$ENDIF} // NOTFORHELP
    property Alignment:       TAlignment   read FAlignment       write SetAlignment       default taLeftJustify;
    property AllowNegative:   Boolean      read FAllowNegative   write FAllowNegative     default True;
    property EditorEnabled:   Boolean      read FEditorEnabled   write FEditorEnabled     default True;

    property FlatSpinButtons: Boolean      read FFlatSpinButtons write SetFlatSpinButtons default True;
    property ShowSpinButtons: Boolean      read FShowSpinButtons write SetShowSpinButtons default True;

    property OnDownClick:     TNotifyEvent read FOnDownClick     write FOnDownClick;
    property OnUpClick:       TNotifyEvent read FOnUpClick       write FOnUpClick;
end;


{$IFDEF DELPHI_XE3}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]{$ENDIF}
  TsSpinEdit = class(TsBaseSpinEdit)
{$IFNDEF NOTFORHELP}
  private
    FMinValue,
    FMaxValue,
    FIncrement: Int64;
    function GetValue: Int64;
    function StoreValue: boolean;
    function CheckValue(NewValue: Int64): Int64;
    procedure SetValue (NewValue: Int64);
    procedure CMExit      (var Message: TCMExit);       message CM_EXIT;
    procedure CMMouseWheel(var Message: TCMMouseWheel); message CM_MOUSEWHEEL;
    procedure WMPaste     (var Message: TWMPaste);      message WM_PASTE;
  protected
    function IsValidChar(var Key: Char): Boolean; override;
    procedure UpClick  (Sender: TObject); override;
    procedure DownClick(Sender: TObject); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure AfterConstruction; override;
    procedure Loaded; override;
  published
{$ENDIF} // NOTFORHELP
    property Increment: Int64 read FIncrement write FIncrement default 1;
    property MaxValue:  Int64 read FMaxValue  write FMaxValue;
    property MinValue:  Int64 read FMinValue  write FMinValue;
    property Value:     Int64 read GetValue   write SetValue stored StoreValue;
    property Text stored StoreValue;
  end;


{$IFDEF DELPHI_XE3}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]{$ENDIF}
  TsDecimalSpinEdit = class(TsBaseSpinEdit)
{$IFNDEF NOTFORHELP}
  private
    FValue,
    FMinValue,
    FMaxValue,
    FIncrement: Extended;
    fDecimalPlaces: Integer;
    FUseSystemDecSeparator: boolean;
    FHideExcessZeros: boolean;
    FShowThousandSeparator: boolean;
    function GetFormattedText: string;
    procedure FormatText;
    function GetValue: Extended;
    procedure SetValue (NewValue: Extended);
    function CheckValue(NewValue: Extended): Extended;
    procedure CMExit      (var Message: TCMExit);       message CM_EXIT;
    procedure WMSetText   (var Message: TMessage);      message WM_SETTEXT;
    procedure CMChanged   (var Message: TMessage);      message CM_CHANGED;
    procedure CMMouseWheel(var Message: TCMMouseWheel); message CM_MOUSEWHEEL;
    procedure WMPaste     (var Message: TWMPaste);      message WM_PASTE;
    procedure SetHideExcessZeros(const Value: boolean);
    procedure SetShowThousandSeparator(const Value: boolean);
    function StoreValue: Boolean;
  protected
    TextChanging,
    ValueChanging: boolean;
    function IsValidChar(var Key: Char): Boolean; override;
    procedure UpClick   (Sender: TObject); override;
    procedure DownClick (Sender: TObject); override;
    procedure SetDecimalPlaces(New: Integer);
  public
    procedure AfterConstruction; override;
    constructor Create(AOwner: TComponent); override;
    procedure Loaded; override;
    procedure UpdateText;
  published
{$ENDIF} // NOTFORHELP
    property HideExcessZeros: boolean read FHideExcessZeros write SetHideExcessZeros default False;
    property Increment: Extended read FIncrement write FIncrement;
    property MaxValue: Extended read FMaxValue write FMaxValue;
    property MinValue: Extended read FMinValue write FMinValue;
    property Value: Extended read GetValue write SetValue stored StoreValue;
    property DecimalPlaces: Integer read fDecimalPlaces write SetDecimalPlaces default 2;
    property ShowThousandSeparator: boolean read FShowThousandSeparator write SetShowThousandSeparator default False;
    property UseSystemDecSeparator: boolean read FUseSystemDecSeparator write FUseSystemDecSeparator default True;
    property Text stored StoreValue;
  end;


{$IFNDEF NOTFORHELP}

  TacTimePanel = class(TWinControl)
  private
    FOwner: TsSpinEdit;
    function ActualWidth: integer;
  protected
    procedure Loaded; override;
    procedure WndProc(var Message: TMessage); override;
  public
    PM: boolean;
    procedure SetMode(aPM: boolean);
    procedure UpdatePosition;
    procedure PaintWindow(aDC: HDC); override;
    constructor Create(AOwner: TComponent); override;
  end;

  TacTimePortion = (tvHours, tvMinutes, tvSeconds);


{$IFDEF DELPHI_XE3}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]{$ENDIF}
  TsTimePicker = class(TsBaseSpinEdit)
  private
    fHour,
    fMin,
    fSec: word;

    FDoBeep,
    FUse12Hour,
    TextChanging,
    ValueChanging,
    FShowSeconds: boolean;
    function GetValue: TDateTime;
    procedure SetValue (NewValue: TDateTime);
    function CheckValue(NewValue: TDateTime): TDateTime;
    procedure SetShowSeconds(const Value: boolean);
    procedure SetUse12Hour  (const Value: boolean);
    procedure CMChanged   (var Message: TMessage);      message CM_CHANGED;
    procedure CMExit      (var Message: TCMExit);       message CM_EXIT;
    procedure CMMouseWheel(var Message: TCMMouseWheel); message CM_MOUSEWHEEL;
    procedure SetShowSpinButtons(const Value: Boolean); override;
  protected
    FPos: integer;
    TimePanel: TacTimePanel;
    procedure UpdateTimePanel;
    function IsValidChar(var Key: Char): Boolean; override;
    function ActualHour: integer;
    procedure ClickUpDown(Up: boolean);
    procedure ChangeScale(M, D: Integer); override;
    procedure UpClick  (Sender: TObject); override;
    procedure DownClick(Sender: TObject); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure SetHour(NewHour: integer);
    procedure SetMin (NewMin: integer);
    procedure SetSec (NewSec: integer);
    procedure DecodeValue;
    function Portion: TacTimePortion;
    procedure SetPos(NewPos: integer; Highlight: boolean = True);
    procedure IncPos;
    procedure ReplaceAtPos(APos: integer; AChar: Char);
    procedure KeyDown (var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure HighlightPos(APos: integer);
    function Sec: word;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Loaded; override;
    procedure WndProc(var Message: TMessage); override;
  published
    property Date:  TDateTime read GetValue write SetValue;
    property Value: TDateTime read GetValue write SetValue;
    property Time:  TDateTime read GetValue write SetValue;
    property DoBeep:      boolean read FDoBeep      write FDoBeep        default False;
    property ShowSeconds: boolean read FShowSeconds write SetShowSeconds default True;
    property Use12Hour:   boolean read FUse12Hour   write SetUse12Hour   default False;
  end;
{$ENDIF} // NOTFORHELP


implementation

uses
  Math, Menus,
  {$IFDEF TNTUNICODE}TntStdCtrls, {$ENDIF}
  {$IFDEF DELPHI7UP} Themes, {$ENDIF}
  {$IFDEF LOGGED}sDebugMsgs, {$ENDIF}
  sCommonData, sSkinProps, sDefaults, sMessages, sSkinManager, sVCLUtils, sAlphaGraph, sStyleSimply, acntTypes;


{$IFNDEF NOTFORHELP}
const
  InitRepeatPause = 300; // pause before first repeat timer (ms)
  RepeatPause     =  80; // pause before repeat timer (ms)
  iMaxHour        =  24;
  iMaxMin         =  60;
  aIncrement:  array [boolean] of integer = (-1, 1);
  aTextLength: array [boolean] of integer = (5,  8);
  aTimeText:   array [boolean] of string = ('AM',          'PM');
  aEmptyText:  array [boolean] of string = ('00:00',       '00:00:00');
  aFormat:     array [boolean] of string = ('%0.2d:%0.2d', '%0.2d:%0.2d:%0.2d');
{$ENDIF} // NOTFORHELP


function ActualSpinWidth: integer;
begin
  Result := acScrollBtnLength + acSpacing;
end;


function TsBaseSpinEdit.ColorStored: boolean;
begin
  Result := not SkinData.Skinned or SkinData.CustomColor;
end;


constructor TsBaseSpinEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  MousePressed := False;
  Btn1State := 0;
  Btn2State := 0;
  BtnSkinNdx := -1;
  FAlignment := taLeftJustify;
  FEditorEnabled := True;
  FShowSpinButtons := True;
  FFlatSpinButtons := True;
  FAllowNegative := True;
end;


destructor TsBaseSpinEdit.Destroy;
begin
  inherited Destroy;
  FRepeatTimer.Free;
end;


procedure TsBaseSpinEdit.GetChildren(Proc: TGetChildProc; Root: TComponent);
begin
//
end;


procedure TsBaseSpinEdit.KeyDown(var Key: Word; Shift: TShiftState);
var
  myForm: TCustomForm;
  CtlDir: integer;
  M: tagMsg;
  C: Char;
begin
  inherited KeyDown(Key, Shift);
  case Key of
    VK_UP:
      UpClick(Self);

    VK_DOWN:
      DownClick(Self);

    VK_TAB, VK_RETURN: begin
      if ssShift in Shift then begin
        CtlDir := 1;
        if Assigned(FPrevTabControl) then begin
          Key := 0;
          FPrevTabControl.SetFocus;
          Exit;
        end;
      end
      else begin
        if Assigned(FNextTabControl) then begin
          Key := 0;
          FNextTabControl.SetFocus;
          Exit;
        end;
        CtlDir := 0;
      end;
      if VK_TAB = Key then begin
        myForm := GetParentForm(Self);
        if not (MYForm = nil) then
          SendMessage(myForm.Handle, WM_NEXTDLGCTL, CtlDir, 0);

        Exit;
      end
      else
        if Assigned(OnKeyPress) then begin
          C := #13;
          OnKeyPress(Self, C);
        end;
    end;

    ord('A'):
      if (ShortCut(Key, Shift) = scCtrl + ord('A')) then begin
        SelectAll;
        Key := 0;
        PeekMessage(M, Handle, WM_CHAR, WM_CHAR, PM_REMOVE);
      end;
  end;
end;


procedure TsBaseSpinEdit.KeyPress(var Key: Char);
var
  err: boolean;
  C: Char;
begin
  C := Key;
  err := not IsValidChar(C);
  if err or (C = #0) then begin
    if C = #0 then
      Key := #0;

    if err then
      MessageBeep(0);
  end
  else
    inherited KeyPress(Key);
end;


function TsBaseSpinEdit.IsValidChar(var Key: Char): Boolean;
begin
  Result := CharInSet(Key, [{$IFDEF DELPHI_XE}FormatSettings.{$ENDIF}DecimalSeparator, CharPlus, CharMinus, ZeroChar..'9']) or
            (Key < #32);

  if Result then
    Result := FAllowNegative or (Key <> CharMinus);

  if not FEditorEnabled and Result and ((Key >= #32) or CharInSet(Key, [Char(VK_BACK), Char(VK_DELETE)])) then
    Result := False;

  if Result then begin
    Result := False;
    case Key of
      Chr(VK_RETURN), Chr(VK_TAB): begin
        Result := True;
        Key := #0;
      end;

      Char(VK_BACK), Chr(VK_ESCAPE):
        Result := True;

      CharMinus:
        if not fAllowNegative then
          Result := False
        else
          if Length(Text) > 0 then begin
            if SelLength = 0 then
              if Text[1] = CharMinus then
                Result := False
              else begin
                SelStart := 0;
                if Text[1] = CharPlus then
                  SelLength := 1;

                Result := True;
              end
            else
              if ACNativeUInt(SendMessage(Handle, EM_GETSEL, 0, 0)) mod $10000 = 0 then
                Result := True;
          end
          else
            Result := True;

      CharPlus:
        if Length(Text) > 0 then begin
          if SelLength = 0 then
            if Text[1] = CharPlus then
              Result := False
            else begin
              SelStart := 0;
              if Text[1] = CharMinus then
                SelLength := 1;

              Result := True;
            end
          else
            if ACNativeUInt(SendMessage(Handle, EM_GETSEL, 0, 0)) mod $10000 = 0 then
              Result := True;
        end
        else
          Result := True;

      else
        Result := True;
    end;
  end;
end;


procedure TsBaseSpinEdit.CreateParams(var Params: TCreateParams);
const
  Alignments: array[TAlignment] of Longword = (ES_LEFT, ES_RIGHT, ES_CENTER);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or ES_MULTILINE or WS_CLIPCHILDREN or Alignments[FAlignment] and not WS_BORDER;
end;


procedure TsBaseSpinEdit.CreateRepeatTimer;
begin
  if FRepeatTimer = nil then begin
    FRepeatTimer := TTimer.Create(Self);
    FRepeatTimer.OnTimer := TimerExpired;
    FRepeatTimer.Interval := InitRepeatPause;
  end;
  FRepeatTimer.Enabled  := True;
end;


procedure TsBaseSpinEdit.CreateWnd;
begin
  FCreating := True;
  inherited CreateWnd;
  FCreating := False;
  SetEditRect;
end;


procedure TsBaseSpinEdit.SetEditRect;
var
  Loc: TRect;
begin
  SendMessage(Handle, EM_GETRECT, 0, LPARAM(@Loc));
  Loc.Top := 0;
  Loc.Bottom := ClientHeight + 1; {+1 is workaround for windows paint bug}
  if IsRightToLeft then begin
    Loc.Left := 1;
    Loc.Right := ClientWidth;
  end
  else begin
    Loc.Left := 0;
    Loc.Right := ClientWidth + 1;
  end;
  SendMessage(Handle, EM_SETRECTNP, 0, LPARAM(@Loc));
end;


procedure TsBaseSpinEdit.SetFlatSpinButtons(const Value: Boolean);
begin
  if FFlatSpinButtons <> Value then begin
    FFlatSpinButtons := Value;
    UpdateBtnIndex(SkinData.Skinned);
    SkinData.Invalidate;
  end;
end;


procedure TsBaseSpinEdit.WMSize(var Message: TWMSize);
var
  MinHeight: Integer;
begin
  inherited;
  MinHeight := GetMinHeight;
  if Height < MinHeight then
    Height := MinHeight
end;


function TsBaseSpinEdit.GetMinHeight: Integer;
begin
  Result := 0;
end;


procedure TsBaseSpinEdit.WMPaste(var Message: TWMPaste);
begin
  if FEditorEnabled and not ReadOnly then
    inherited;
end;


procedure TsBaseSpinEdit.WMCut(var Message: TWMPaste);
begin
  if FEditorEnabled and not ReadOnly then
    inherited;
end;


procedure TsBaseSpinEdit.Loaded;
begin
  inherited;
  UpdateBtnIndex(SkinData.Skinned);
end;


constructor tsSpinEdit.Create(AOwner:TComponent);
begin
  inherited create(AOwner);
  Text := '';
  FIncrement := 1;
end;


procedure TsSpinEdit.CMExit(var Message: TCMExit);
begin
  inherited;
  if (Text = '-0') or (Text = CharMinus) or (Text = '+0') or (Text = CharPlus) then
    Text := ZeroChar;

  if CheckValue(Value) <> Value then
    SetValue(Value);
end;


procedure TsSpinEdit.UpClick(Sender: TObject);
begin
  if ReadOnly then
    MessageBeep(0)
  else
    if Assigned(FOnUpClick) then
      FOnUpClick(Self)
    else
      Value := Value + FIncrement;
end;


procedure TsSpinEdit.DownClick(Sender: TObject);
begin
  if ReadOnly then
    MessageBeep(0)
  else
    if Assigned(FOnDownClick) then
      FOnDownClick(Self)
    else
      Value := Value - FIncrement;
end;


function TsSpinEdit.GetValue: Int64;
begin
  if Text <> '' then
    try
      Result := StrToInt64(Text); // shark
    except
      Result := FMinValue;
    end
  else
    Result := 0;
end;


procedure TsSpinEdit.SetValue(NewValue: Int64);
begin
  if (NewValue < 0) and not FAllowNegative then
    NewValue := max(0, FMinValue);

  if not (csLoading in ComponentState) then
    Text := IntToStr(CheckValue(NewValue));
end;


function TsSpinEdit.StoreValue: boolean;
begin
  Result := Value <> 0;
end;


procedure TsSpinEdit.AfterConstruction;
begin
  inherited;
  if csDesigning in ComponentState then
    if Text = '' then
      Text := ZeroChar
    else
      Text := IntToStr(Value);
end;


function TsSpinEdit.CheckValue(NewValue: Int64): Int64;
begin
  if FMinValue <> FMaxValue then begin
    Result := NewValue;
    if FMinValue <> 0 then
      if NewValue < FMinValue then
        Result := FMinValue
      else
        if (NewValue > FMaxValue) and (FMaxValue <> 0) then
          Result := FMaxValue;

    if FMaxValue <> 0 then
      if NewValue > FMaxValue then
        Result := FMaxValue
      else
        if (NewValue < FMinValue) and (FMinValue <> 0) then
          Result := FMinValue
  end
  else
    if (NewValue < 0) and not FAllowNegative then
      Result := max(0, FMinValue)
    else
      Result := NewValue;
end;


function TsSpinEdit.IsValidChar(var Key: Char): Boolean;
begin
  Result := inherited IsValidChar(Key);
  if Result then
    if Key = CharMinus then
      if not fAllowNegative then
        Result := False
      else
        Result := (MinValue <= 0);

  if not Result then
    Key := #0;
end;


procedure TsSpinEdit.Loaded;
var
  SavedOnChange: TNotifyEvent;
begin
  inherited;
  SavedOnChange := OnChange;
  OnChange := nil;
  if Text = '' then
    Text := ZeroChar
  else
    Text := IntToStr(Value);

  OnChange := SavedOnChange;
end;


procedure TsSpinEdit.CMMouseWheel(var Message: TCMMouseWheel);
begin
  inherited;
  if not ReadOnly and (Message.Result = 0) then begin
    Value := Value + Increment * (Message.WheelDelta div 120);
    Message.Result := 1
  end;
end;


procedure TsSpinEdit.WMPaste(var Message: TWMPaste);
{$IFDEF DELPHI6UP}
var
  OldValue, NewValue: integer;
{$ENDIF}
begin
  if FEditorEnabled and not ReadOnly then begin
{$IFDEF DELPHI6UP}
    OldValue := Value;
{$ENDIF}
    inherited;
{$IFDEF DELPHI6UP}
    if not TryStrToInt(Text, NewValue) then
      Text := IntToStr(OldValue);
{$ENDIF}
  end;
end;


constructor TsDecimalSpinEdit.Create(AOwner:TComponent);
begin
  inherited create(AOwner);
  ValueChanging := False;
  TextChanging := False;
  FUseSystemDecSeparator := True;
  FShowThousandSeparator := False;
  FHideExcessZeros := False;
  FIncrement := 1;
  FDecimalPlaces := 2;
end;


procedure TsDecimalSpinEdit.CMExit(var Message: TCMExit);
begin
  inherited;
  if CheckValue(Value) <> Value then
    SetValue(Value);

  FormatText;
end;


procedure TsDecimalSpinEdit.UpClick(Sender: TObject);
var
  CurValue: real;
begin
  if ReadOnly then
    MessageBeep(0)
  else begin
    CurValue := Value;
    if Assigned(FOnUpClick) then
      FOnUpClick(Self)
    else
      Value := CurValue + FIncrement;
  end;
end;


procedure TsDecimalSpinEdit.UpdateText;
begin
  if HandleAllocated then
    SendMessage(Handle, WM_SETTEXT, 0, LPARAM(PChar(GetFormattedText)))
  else
    Text := GetFormattedText;
end;


procedure TsDecimalSpinEdit.DownClick(Sender: TObject);
var
  CurValue: real;
begin
  if ReadOnly then
    MessageBeep(0)
  else begin
    CurValue := Value;
    if Assigned(FOnDownClick) then
      FOnDownClick(Self)
    else
      Value := CurValue - FIncrement;
  end;
end;


procedure TsDecimalSpinEdit.SetDecimalPlaces(New: Integer);
var
  ActValue: Extended;
begin
  if fDecimalPlaces <> New then begin
    fDecimalPlaces := New;
    ActValue := FValue;
    FValue := 0;
    Value := CheckValue(ActValue);
  end;
end;


procedure TsDecimalSpinEdit.SetHideExcessZeros(const Value: boolean);
begin
  if FHideExcessZeros <> Value then begin
    FHideExcessZeros := Value;
    if not (csLoading in ComponentState) then
      Text := GetFormattedText;
  end;
end;


procedure TsDecimalSpinEdit.SetShowThousandSeparator(const Value: boolean);
begin
  if FShowThousandSeparator <> Value then begin
    FShowThousandSeparator := Value;
    UpdateText;
  end;
end;


procedure TsDecimalSpinEdit.SetValue(NewValue: Extended);
begin
  if FValue <> NewValue then begin
    if (NewValue < 0) and not FAllowNegative then
      NewValue := max(0, FMinValue);

    if MaxValue > MinValue then
      FValue := max(min(NewValue, MaxValue), MinValue)
    else
      FValue := NewValue;

    if not TextChanging then begin
      ValueChanging := True;
      UpdateText;
      if not (csLoading in ComponentState) and Assigned(OnChange) then
        OnChange(Self);

      ValueChanging := False;
    end;
  end;
end;


function TsDecimalSpinEdit.StoreValue: Boolean;
begin
  Result := Value <> 0;
end;


procedure TsDecimalSpinEdit.AfterConstruction;
begin
  inherited;
  if not TextChanging then begin
    ValueChanging := True;
    UpdateText;
    ValueChanging := False;
  end;
end;


function TsDecimalSpinEdit.CheckValue(NewValue: Extended): Extended;
begin
  if (NewValue < 0) and not FAllowNegative then
    Result := max(0, FMinValue)
  else
    Result := NewValue;

  if (FMinValue <> 0) and (NewValue < FMinValue) then
    Result := FMinValue
  else
    if (FMaxValue <> 0) and (NewValue > FMaxValue) then
      Result := FMaxValue;
end;


function TsDecimalSpinEdit.IsValidChar(var Key: Char): Boolean;
begin
  Result := inherited IsValidChar(Key);
  if Result then
    case Key of
      CharMinus:
        if not fAllowNegative then
          Result := False
        else
          Result := (MinValue <= 0);
    end;

  if not Result then
    Key := #0;
end;


procedure TsDecimalSpinEdit.Loaded;
begin
  inherited;
  if not TextChanging then begin
    ValueChanging := True;
    UpdateText;
    ValueChanging := False;
  end;
end;


procedure TsDecimalSpinEdit.CMMouseWheel(var Message: TCMMouseWheel);
begin
  inherited;
  if not ReadOnly and (Message.Result = 0) then begin
    Value := Value + Increment * (Message.WheelDelta div 120);
    Message.Result := 1
  end;
end;


function TsDecimalSpinEdit.GetFormattedText: string;
var
  l: integer;
begin
  if FShowThousandSeparator then
    Result := FloatToStrF(CheckValue(FValue), ffNumber, 18, FDecimalPlaces)
  else
    Result := FloatToStrF(CheckValue(FValue), ffFixed, 18, FDecimalPlaces);

  if FHideExcessZeros then
    while True do begin
      l := Length(Result);
      if l > 0 then
        case Result[l] of
          ',', '.': begin
            Delete(Result, l, 1);
            Exit;
          end;

          '0':
            Delete(Result, l, 1)

          else
            Exit;
        end
      else
        Exit;
    end;
end;


function TsDecimalSpinEdit.GetValue: Extended;
{$IFDEF DELPHI6UP}
var
  v: Extended;
{$ENDIF}
begin
  if not TextChanging then
    Result := FValue
  else
{$IFDEF DELPHI6UP}
    if TryStrToFloat(Text, V) then
      Result := V
    else
      Result := 0;
{$ELSE}
  try
    if Text = '' then
      Result := 0
    else
      Result := StrToFloat(Text);
  except
    Result := 0;
  end;
{$ENDIF}
end;


procedure TsDecimalSpinEdit.CMChanged(var Message: TMessage);
var
  dSep: Char;
  s: string;
begin
  inherited;
  if {not (csLoading in ComponentState) and }not ValueChanging then begin
    TextChanging := True;
    if (Text = '') or (Text = CharMinus) or (Text = CharPlus) then
      Value := 0
    else begin
      dSep := {$IFDEF DELPHI_XE}FormatSettings.{$ENDIF}DecimalSeparator;
      if (dSep <> s_Dot) and (Pos(s_Dot, Text) > 0) then
        s := ReplaceStr(Text, s_Dot, dSep)
      else
        if (dSep <> s_Comma) and (Pos(s_Comma, Text) > 0) then
          s := ReplaceStr(Text, s_Comma, dSep)
        else
          s := Text;

{$IFDEF DELPHI6UP}
      Value := StrToFloatDef(s, 0);
{$ELSE}
      Value := StrToFloat(s);
{$ENDIF}
    end;

    TextChanging := False;
  end;
end;


procedure TsDecimalSpinEdit.FormatText;
begin
  ValueChanging := True;
  if not TextChanging then
    SendMessage(Handle, WM_SETTEXT, 0, LPARAM(PChar(GetFormattedText)));

  ValueChanging := False;
end;


procedure TsDecimalSpinEdit.WMPaste(var Message: TWMPaste);
{$IFDEF DELPHI6UP}
var
  OldValue, NewValue: extended;
{$ENDIF}
begin
  if FEditorEnabled and not ReadOnly then begin
{$IFDEF DELPHI6UP}
    OldValue := Value;
{$ENDIF}
    inherited;
{$IFDEF DELPHI6UP}
    if not TryStrToFloat(Text, NewValue) then
      Text := FloatToStr(OldValue);
{$ENDIF}
  end;
end;


procedure TsDecimalSpinEdit.WMSetText(var Message: TMessage);
begin
  inherited;
  if Text = '' then
    FValue := 0;
end;


procedure TsBaseSpinEdit.WndProc(var Message: TMessage);
var
  DC, SavedDC: HDC;
  bw: integer;
  R: TRect;
begin
{$IFDEF LOGGED}
  AddToLog(Message);
{$ENDIF}
  case Message.Msg of
    SM_ALPHACMD:
      case Message.WParamHi of
        AC_REMOVESKIN:
          if ACUInt(Message.LParam) = ACUInt(SkinData.SkinManager) then
            UpdateBtnIndex(False);

        AC_SETNEWSKIN:
          if ACUInt(Message.LParam) = ACUInt(SkinData.SkinManager) then
            UpdateBtnIndex(True);

        AC_MOUSELEAVE:
          Perform(CM_MOUSELEAVE, 0, 0);
      end;

    WM_NCLBUTTONDBLCLK: begin
      Perform(WM_NCLBUTTONDOWN, Message.WParam, Message.LParam);
      Exit;
    end;

    WM_KILLFOCUS:
      StopTimer(SkinData);
  end;
  inherited;
  case Message.Msg of
    SM_ALPHACMD:
      case Message.WParamHi of
        AC_GETBG:
          InitBGInfo(SkinData, PacBGInfo(Message.LParam), 0);

        AC_GETCONTROLCOLOR:
          Message.Result := GetBGColor(SkinData, 0);

        AC_PREPARECACHE:
          PrepareCache;
      end;

    WM_NCPAINT: if (not InUpdating(SkinData) or (SkinData.SkinIndex < 0)) and (not SkinData.BGChanged or not SkinData.Skinned) then begin
      DC := GetWindowDC(Handle);
      SavedDC := SaveDC(DC);
      try
        if ShowSpinButtons then begin
          R := BtnRect(True);
          R.Bottom := Height - R.Top - 1;
          if ShowSpinButtons and not SkinData.Skinned then
            PrepareStd;

          BitBlt(DC, R.Left, R.Top, R.Right, R.Bottom, SkinData.FCacheBmp.Canvas.Handle, R.Left, R.Top, SRCCOPY);
        end;
      finally
        RestoreDC(DC, SavedDC);
        ReleaseDC(Handle, DC);
      end;
      PaintAddedGlyphStd;
    end;

    WM_PRINT: begin
      bw := EditBorderWidth({$IFDEF TNTUNICODE}TTntEdit{$ELSE}TEdit{$ENDIF}(Self));
      MoveWindowOrg(TWMPaint(Message).DC, bw, bw);
      if IsRightToLeft and ShowSpinButtons then
        MoveWindowOrg(TWMPaint(Message).DC, acScrollBtnLength, 0);
    end;

    WM_SIZE, CM_FONTCHANGED: begin
      SetEditRect;
      if not SkinData.Skinned then
        Perform(WM_NCPAINT, 0, 0);
    end;

    WM_SETFOCUS:
      if AutoSelect then
        SelectAll;

    CM_MOUSELEAVE: begin
      if Btn1State > 0 then
        Btn1State := 0;

      if Btn2State > 0 then
        Btn2State := 0;
    end;

    CM_COLORCHANGED:
      if SkinData.CustomColor then begin
        PrepareCache;
        RedrawWindow(Handle, nil, 0, RDW_FRAME or RDW_INVALIDATE);
      end;
  end;
end;

const
  VertDirections: array [boolean] of TacSide = (asBottom, asTop);


constructor TsTimePicker.Create(AOwner:TComponent);
begin
  inherited create(AOwner);
  ValueChanging := False;
  TextChanging := False;
  FShowSeconds := True;
  FDoBeep := False;
  fHour := 0;
  fMin := 0;
  fSec := 0;
end;


procedure TsTimePicker.CMExit(var Message: TCMExit);
begin
  inherited;
  if CheckValue(Value) <> Value then
    SetValue(Value);
end;


procedure TsTimePicker.ClickUpDown(Up: boolean);
var
  cPortion: TacTimePortion;
  Increment: integer;
begin
  cPortion := Portion;
  if ReadOnly then begin
    if FDoBeep then
      MessageBeep(0);
  end
  else
    if length(Text) = aTextLength[ShowSeconds] then begin
      Increment := aIncrement[Up];
      case Portion of
        tvHours:   SetHour(FHour + Increment);
        tvMinutes: SetMin (FMin  + Increment);
        tvSeconds: SetSec (FSec  + Increment);
      end;
      if ShowSeconds then
        Text := Format(aFormat[True], [ActualHour, fMin, fSec])
      else
        Text := Format(aFormat[False], [ActualHour, fMin]);

      if (not (csLoading in ComponentState)) then begin
        case cPortion of
          tvHours:   SelStart := 0;
          tvMinutes: SelStart := 3;
          tvSeconds: SelStart := 6;
        end;
        FPos := SelStart + 2;
        SelLength := 2;
      end;
    end;
end;


procedure TsTimePicker.UpClick(Sender: TObject);
begin
  ClickUpDown(True);
end;


procedure TsTimePicker.UpdateTimePanel;
begin
  if FUse12Hour then begin
    ControlStyle := ControlStyle - [csSetCaption] + [csAcceptsControls];
    if TimePanel = nil then
      TimePanel := TacTimePanel.Create(Self);

    TimePanel.UpdatePosition;
    TimePanel.Parent := Self;
    TimePanel.Visible := True;
    TimePanel.Color := clRed;
    InvalidateRect(TimePanel.Handle, nil, True);
    ControlStyle := ControlStyle - [csAcceptsControls];
  end
  else
    FreeAndNil(TimePanel);
end;


procedure TsTimePicker.WndProc(var Message: TMessage);
begin
  inherited;
  case Message.Msg of
    SM_ALPHACMD:
      case Message.WParamHi of
        AC_ENDPARENTUPDATE:
          if not InUpdating(SkinData) and (TimePanel <> nil) and TimePanel.HandleAllocated then
            RedrawWindow(TimePanel.Handle, nil, 0, RDWA_ALLNOW);
      end;

    CM_ENABLEDCHANGED:
      if TimePanel <> nil then
        TimePanel.Repaint;

    WM_PAINT:
      if SkinData.Skinned and (TimePanel <> nil) then
        if not InUpdating(SkinData) then
          TimePanel.Repaint;

    WM_SETTEXT:
      if TimePanel <> nil then
        TimePanel.SetMode(Value >= EncodeTime(12, 0, 0, 0));

    WM_SIZE:
      if TimePanel <> nil then
        TimePanel.UpdatePosition;

    CM_EXIT:
      Value := Value;
  end;
end;


procedure TsTimePicker.DownClick(Sender: TObject);
begin
  ClickUpDown(False);
end;


function TsTimePicker.GetValue: TDateTime;
begin
  try
    Result := EncodeTime(FHour, FMin, Sec, 0)
  except
    Result := 0;
  end;
end;


procedure TsTimePicker.SetValue(NewValue: TDateTime);
var
  NewText: String;
  dMSec: Word;
begin
  DecodeTime(NewValue, FHour, FMin, FSec, dMSec);
  if ShowSeconds then
    NewText := Format(aFormat[True], [ActualHour, FMin, FSec])
  else
    NewText := Format(aFormat[False], [ActualHour, FMin]);

  if not (csLoading in ComponentState) and not TextChanging then begin
    ValueChanging := True;
    Text := NewText;
    ValueChanging := False;
  end;
end;


function TsTimePicker.ActualHour: integer;
begin
  if FUse12Hour then
    Result := FHour mod 12
  else
    Result := FHour;
end;


procedure TsTimePicker.ChangeScale(M, D: Integer);
begin
  inherited;
  UpdateTimePanel;
end;


function TsTimePicker.CheckValue (NewValue: TDateTime): TDateTime;
begin
  Result := integer(NewValue >= 0) * NewValue;
end;


function TsTimePicker.IsValidChar(var Key: Char): Boolean;
var
  i: integer;
{$IFDEF TNTUNICODE}
  c: PWideChar;
{$ENDIF}
  s: string;
begin
  Result := False;
  if not FEditorEnabled or ReadOnly or CharInSet(Key, [Chr(VK_ESCAPE), Chr(VK_RETURN), #0]) then
    Key := #0
  else begin
    Result := CharInSet(Key, [ZeroChar..'9']);
    if Result then begin
{$IFDEF TNTUNICODE}
      c := PWideChar(Text);
      s := WideCharToString(c);
{$ELSE}
      s := Text;
{$ENDIF}
      case FPos of
        1: i := StrToInt(Key  + s[2]);
        2: i := StrToInt(s[1] + Key);
        4: i := StrToInt(Key  + s[4]);
        5: i := StrToInt(s[4] + Key);
        7: i := StrToInt(Key  + s[8]);
        8: i := StrToInt(s[7] + Key)
        else begin // If selected all
          FPos := 1;
          i := StrToInt(Key + s[2]);
        end
      end;
      if FPos in [1, 2] then
        if (FPos = 1) and (StrToInt(Key) > 2) then
          Result := False
        else begin
          if i > 23 then
            if AllEditSelected(Self) then begin
              s[2] := '3';
              Text := s;
            end
            else
              Result := False;
        end
      else
        if i > 59 then
          Result := False;
    end;
    if not Result then
      Key := #0;
  end;
end;


function TsBaseSpinEdit.PrepareCache: boolean;
var
  bw: integer;
  BGInfo: TacBGInfo;
begin
  Result := False;
  BGInfo.BgType := btUnknown;
  GetBGInfo(@BGInfo, Parent);
  if BGInfo.BgType <> btNotReady then begin
    InitCacheBmp(SkinData);
    SkinData.BGChanged := False;
    if BorderStyle = bsSingle then
      PaintItem(SkinData, BGInfoToCI(@BGInfo), True, integer(ControlIsActive(SkinData)), MkRect(Self), Point(Left, top), SkinData.FCacheBmp, False)
    else
      PaintItemBG(SkinData, BGInfoToCI(@BGInfo), integer(ControlIsActive(SkinData)), MkRect(Self), Point(Left, Top), SkinData.FCacheBmp);

    if SkinData.CustomColor then begin
      bw := EditBorderWidth({$IFDEF TNTUNICODE}TTntEdit{$ELSE}TEdit{$ENDIF}(Self));
      FillDC(SkinData.FCacheBmp.Canvas.Handle, Rect(bw, bw, Width - bw, Height - bw), Color);
    end;
    PaintText;
    if ShowSpinButtons then
      PaintBtns(SkinData.FCacheBmp);

    if not Enabled then
      BmpDisabledKind(SkinData.FCacheBmp, DisabledKind, Parent, GetParentCache(SkinData), Point(Left, Top));

    Result := True;
  end;
  if AddedGlyphVisible then
    PaintAddedGlyph;
end;


procedure TsBaseSpinEdit.PrepareStd;
begin
  InitCacheBmp(SkinData);
  FillDC(SkinData.FCacheBmp.Canvas.Handle, MkRect(Self), Color);
  PaintBtns(SkinData.FCacheBmp);
end;


procedure TsTimePicker.KeyDown(var Key: Word; Shift: TShiftState);
var
  M: tagMsg;
begin
  case Key of
    VK_UP:
      UpClick(Self);

    VK_DOWN:
      DownClick(Self);

    VK_RIGHT:
      if Shift = [] then
        IncPos
      else begin
        FPos := min(aTextLength[ShowSeconds], FPos + 1);
        inherited;
        Exit;
      end;

    VK_LEFT:
      if Shift = [] then
        SetPos(max(1, FPos - 1 - integer(FPos in [4, 7])), (Shift = []))
      else begin
        FPos := max(1, FPos - 1);
        inherited;
        exit;
      end;

    VK_BACK, VK_DELETE:
      if not AllEditSelected(Self) then begin
        ReplaceAtPos(FPos, ZeroChar);
        if Key = VK_BACK then begin
          Key := VK_LEFT;
          KeyDown(Key, Shift);
        end
        else begin
          HighlightPos(FPos);
          Key := 0;
        end
      end
      else
        if not ReadOnly then begin
          if ([csLoading, csDesigning] * ComponentState = []) and Visible then begin
            SelStart := 0;
            SelLength := 0;
          end;
          Text := aEmptyText[ShowSeconds];
          SetPos(1);
        end;

    ord('A'):
      if ShortCut(Key, Shift) = scCtrl + ord('A') then begin
        Key := 0;
        SelectAll;
        PeekMessage(M, Handle, WM_CHAR, WM_CHAR, PM_REMOVE);
      end;
  end;
  if Key in [VK_BACK, VK_SPACE, VK_LEFT..VK_DOWN, VK_DELETE] then
    Key := 0;

  inherited;
  case Key of
    VK_END: begin
      FPos := aTextLength[ShowSeconds];
      if Shift = [] then begin
        if ([csLoading, csDesigning] * ComponentState = []) and Visible then begin
          SelStart := aTextLength[ShowSeconds] - 1;
          SelLength := 1;
        end;
        Key := 0;
      end;
    end;

    VK_HOME: begin
      if Shift = [] then begin
        if ([csLoading, csDesigning] * ComponentState = []) and Visible then begin
          SelStart := 0;
          SelLength := 1;
        end;
        Key := 0;
      end
      else
        SelStart := FPos;

      FPos := 1;
    end;
  end
end;


procedure TsTimePicker.KeyPress(var Key: Char);
var
  C: Char;
begin
  C := Key;
  if not IsValidChar(C) then
    if C = #0 then begin
      Key := #0;
      if FDoBeep then
        MessageBeep(0);
    end
    else
      inherited
  else begin
    if AllEditSelected(Self) then
      SetPos(1);

    inherited;
    if Key <> #0 then
      ReplaceAtPos(FPos, Key);

    Key := #0;
    IncPos;
    DecodeValue;
  end;
end;


procedure TsTimePicker.HighlightPos(APos: integer);
begin
  if ([csLoading, csDesigning] * ComponentState = []) and Visible then begin
    SelStart := APos - 1;
    SelLength := 1;
  end
end;


procedure TsTimePicker.SetPos(NewPos: integer; Highlight: boolean);
begin
  FPos := NewPos;
  if FPos in [3, 6] then
    dec(FPos);

  if Highlight then
    HighlightPos(FPos);
end;


procedure TsTimePicker.ReplaceAtPos(APos: integer; AChar: Char);
var
  s: string;
begin
  if FEditorEnabled and (APos <= Length(Text)) then begin
    s := Text;
    s[APos] := AChar;
    Text := s;
  end;
end;


procedure TsTimePicker.IncPos;
begin
  SetPos(min(aTextLength[ShowSeconds], FPos + 1 + integer(FPos in [2, 5])));
end;


function TsTimePicker.Portion: TacTimePortion;
var
  FCurPos: DWord;
begin
  FCurPos := ACNativeUInt(SendMessage(Handle, EM_GETSEL, 0, 0)) mod $10000;
  case FCurPos of
    0..2: Result := tvHours;
    3..5: Result := tvMinutes
    else  Result := tvSeconds;
  end
end;


procedure TsTimePicker.DecodeValue;
var
  s: string;
begin
  s := Text;
  FHour := StrToInt(copy(s, 1, 2));
  FMin  := StrToInt(copy(s, 4, 2));
  if (aTextLength[ShowSeconds] <= Length(Text)) and ShowSeconds then
    FSec := StrToInt(copy(s, 7, 2))
  else
    FSec := 0;
end;


procedure TsTimePicker.SetHour(NewHour: integer);
begin
  if NewHour >= iMaxHour then
    SetHour(NewHour - iMaxHour)
  else
    if NewHour < 0 then
      SetHour(NewHour + iMaxHour)
    else
      FHour := NewHour;
end;


procedure TsTimePicker.SetMin(NewMin: integer);
begin
  if NewMin >= iMaxMin then begin
    SetHour(FHour + 1);
    SetMin(NewMin - iMaxMin);
  end
  else
    if NewMin < 0 then begin
      SetHour(FHour - 1);
      SetMin(NewMin + iMaxMin);
    end
    else
      FMin := NewMin
end;


procedure TsTimePicker.SetSec(NewSec: integer);
begin
  if NewSec >= iMaxMin then begin
    SetMin(FMin + 1);
    SetSec(NewSec - iMaxMin);
  end
  else
    if NewSec < 0 then begin
      SetMin(FMin - 1);
      SetSec(NewSec + iMaxMin);
    end
    else
      FSec := NewSec
end;


procedure TsTimePicker.Loaded;
var
  SavedOnChange: TNotifyEvent;
begin
  inherited;
  SavedOnChange := OnChange;
  OnChange := nil;
  Value := Value;

  OnChange := SavedOnChange;

  if AllEditSelected(Self) then
    FPos := aTextLength[ShowSeconds] + 1
  else
    SetPos(1);

  UpdateTimePanel;
end;


procedure TsTimePicker.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if SelLength = 0 then begin
    FPos := ACNativeUInt(SendMessage(Handle, EM_GETSEL, 0, 0)) mod $10000;
    SetPos(min(aTextLength[ShowSeconds], FPos) + 1)
  end
  else
    FPos := SelStart + 1;
end;


procedure TsTimePicker.SetShowSeconds(const Value: boolean);
var
  CurValue: TDateTime;
begin
  if FShowSeconds <> Value then begin
    CurValue := Self.Value;
    FShowSeconds := Value;
    SetValue(CurValue);
    if not (csLoading in ComponentState) and Visible then
      Repaint;
  end;
end;


procedure TsTimePicker.SetShowSpinButtons(const Value: Boolean);
begin
  inherited;
  if not (csLoading in COmponentState) and (TimePanel <> nil) then
    TimePanel.UpdatePosition;
end;


function TsTimePicker.Sec: word;
begin
  Result := integer(FShowSeconds) * FSec;
end;


procedure TsTimePicker.SetUse12Hour(const Value: boolean);
begin
  FUse12Hour := Value;
  if not (csLoading in ComponentState) then
    UpdateTimePanel;
end;


procedure TsBaseSpinEdit.SetAlignment(const Value: TAlignment);
begin
  if FAlignment <> Value then begin
    FAlignment := Value;
    RecreateWnd;
  end;
end;


procedure TsBaseSpinEdit.SetBtn1State;
begin
  if FBtn1State <> Value then begin
    FBtn2State := 0;
    FBtn1State := Value;
    if [csLoading] * ComponentState = [] then begin
      if SkinData.Skinned then
        PrepareCache;

      SendMessage(Handle, WM_NCPAINT, 0, 0);
    end;
  end;
end;


procedure TsBaseSpinEdit.SetBtn2State;
begin
  if FBtn2State <> Value then begin
    FBtn1State := 0;
    FBtn2State := Value;
    if [csLoading] * ComponentState = [] then begin
      if SkinData.Skinned then
        PrepareCache;

      SendMessage(Handle, WM_NCPAINT, 0, 0);
    end;
  end;
end;


procedure TsBaseSpinEdit.PaintBtn(aBmp: TBitmap; R: TRect; IsUp: boolean);
var
  sm: TsSkinManager;
  C, BtnState: integer;
begin
  BtnState := iff(IsUp, Btn1State, Btn2State);
  sm := SkinData.SkinManager;
  if BtnSkinNdx >= 0 then begin
    PaintItem(BtnSkinNdx, MakeCacheInfo(SkinData.FCacheBmp), True, BtnState, R, R.TopLeft, SkinData.FCacheBmp.Canvas.Handle, SkinData.SkinManager);
    with SkinData, sm.gd[BtnSkinNdx] do
      if BtnState = 0 then
        if FlatSpinButtons or NeedParentFont(sm, BtnSkinNdx, 0) then
          C := ColorToRGB(Font.Color)
        else
          C := ColorToRGB(Props[BtnState].FontColor.Color)
      else
        if not SkinData.CustomFont then
          if NeedParentFont(sm, BtnSkinNdx, BtnState) then
            C := ColorToRGB(Font.Color)
          else
            C := ColorToRGB(Props[BtnState].FontColor.Color)
        else
          C := ColorToRGB(iff(Enabled, ColorToRGB(sm.gd[SkinData.SkinIndex].Props[0].FontColor.Color), clGrayText))
  end
  else
    C := Font.Color;

  DrawArrow(SkinData.FCacheBmp, C, clNone, R, VertDirections[IsUp], 0, 0, acArrowSize - 1, sm.Options.ActualArrowStyle)
end;


procedure TsBaseSpinEdit.PaintBtns(aBmp: TBitmap);
begin
  if SkinData.Skinned then begin
    PaintBtn(aBmp, BtnRect(True), True);
    PaintBtn(aBmp, BtnRect(False), False);
  end
  else begin
    PaintBtnStd(aBmp, BtnRect(True), True);
    PaintBtnStd(aBmp, BtnRect(False), False);
  end;
end;


procedure TsBaseSpinEdit.PaintBtnStd(aBmp: TBitmap; R: TRect; IsUp: boolean);
const
  DownStyles: array[Boolean] of Integer = (BDR_RAISEDINNER, BDR_SUNKENOUTER);
var
  BtnState: integer;
  DrawFlags: Integer;
{$IFDEF DELPHI7UP}
  Button: TThemedButton;
  ToolButton: TThemedToolBar;
  Details: TThemedElementDetails;
{$ENDIF}
begin
  if not FFlatSpinButtons then
    InflateRect(R, 1, 1);

  if Enabled and (iff(IsUp, Btn1State, Btn2State) = 2) then
    BtnState := 2
  else
    BtnState := 0;

{$IFDEF DELPHI7UP}
  if acThemesEnabled then begin
    if not Enabled then
      Button := tbPushButtonDisabled
    else
      if BtnState = 2 then
        Button := tbPushButtonPressed
      else
        if BtnState = 1 then
          Button := tbPushButtonHot
        else
          Button := tbPushButtonNormal;

    ToolButton := ttbToolbarDontCare;
    if FFlatSpinButtons then
      case Button of
        tbPushButtonDisabled: Toolbutton := ttbButtonDisabled;
        tbPushButtonPressed : Toolbutton := ttbButtonPressed;
        tbPushButtonHot     : Toolbutton := ttbButtonHot;
        tbPushButtonNormal  : Toolbutton := ttbButtonNormal;
      end;

    if ToolButton = ttbToolbarDontCare then
      Details := acThemeServices.GetElementDetails(Button)
    else
      Details := acThemeServices.GetElementDetails(ToolButton);

    acThemeServices.DrawElement(aBmp.Canvas.Handle, Details, R);
  end
  else
{$ENDIF}
  begin
    if not FFlatSpinButtons then begin
      DrawFlags := DFCS_BUTTONPUSH or DFCS_ADJUSTRECT;
      if BtnState = 2 then
        DrawFlags := DrawFlags or DFCS_PUSHED;

      DrawFrameControl(aBmp.Canvas.Handle, R, DFC_BUTTON, DrawFlags);
    end
    else
      if (BtnState > 0) or (csDesigning in ComponentState) then
        DrawEdge(aBmp.Canvas.Handle, R, DownStyles[BtnState > 1], BF_RECT)
  end;
  DrawArrow(aBmp, Font.Color, clNone, R, VertDirections[IsUp], 0, 0, acArrowSize - 1, arsSolid1);
end;


procedure TsBaseSpinEdit.PaintText;
var
  R: TRect;
  s: acString;
  i: integer;
  Flags: Cardinal;
begin
  SkinData.FCacheBMP.Canvas.Font.Assign(Font);
  Flags := DT_TOP or DT_NOPREFIX or DT_SINGLELINE;
  R := TextRect;
  if PasswordChar <> #0 then
    for i := 1 to Length(Text) do
      s := s + PasswordChar
  else
    s := Text;

  acWriteTextEx(SkinData.FCacheBMP.Canvas, PacChar(s), True, R, Flags or Cardinal(GetStringFlags(Self, Alignment)) and not DT_VCENTER, SkinData, ControlIsActive(SkinData));
end;


procedure TsBaseSpinEdit.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;
  Message.Result := Message.Result and not DLGC_WANTALLKEYS;
end;


procedure TsBaseSpinEdit.WMNCCalcSize(var Message: TWMNCCalcSize);
begin
  inherited;
  if ShowSpinButtons then
    with Message.CalcSize_Params.rgrc[0] do
      if BidiMode = bdLeftToRight then
        dec(Right, acScrollBtnLength)
      else
        inc(Left, acScrollBtnLength);
end;


procedure TsBaseSpinEdit.WMNCHitTest(var Message: TWMMouse);
var
  p: TPoint;
begin
  if not (csDesigning in COmponentState) then begin
    p := acMouseInWnd(Handle, Message.XPos, Message.YPos);
    if PtInRect(BtnRect(True), p) then
      Message.Result := HTMINBUTTON
    else
      if PtInRect(BtnRect(False), p) then
        Message.Result := HTMAXBUTTON
      else
        inherited
  end
  else
    inherited;
end;


procedure TsBaseSpinEdit.WMNCMouseDown(var Message: TWMMouse);
var
  p: TPoint;
begin
  if not (csDesigning in COmponentState) then begin
    if CanFocus then
      SetFocus;

    p := acMouseInWnd(Handle, Message.XPos, Message.YPos);
    if PtInRect(BtnRect(True), p) then begin
      MousePressed := True;
      Btn1State := 2;
      CreateRepeatTimer;
    end
    else
      if PtInRect(BtnRect(False), p) then begin
        MousePressed := True;
        Btn2State := 2;
        CreateRepeatTimer;
      end
      else begin
        MousePressed := False;
        inherited;
      end;
  end;
end;


procedure TsBaseSpinEdit.WMNCMouseMove(var Message: TWMMouse);
var
  p: TPoint;
begin
  if not SkinData.FMouseAbove then
    Perform(CM_MOUSEENTER, 0, 0);

  if SkinData.Skinned then begin
    p := acMouseInWnd(Handle, Message.XPos, Message.YPos);
    if PtInRect(BtnRect(True), p) then
      Btn1State := 1 + integer(MousePressed)
    else
      if PtInRect(BtnRect(False), p) then
        Btn2State := 1 + integer(MousePressed)
      else begin
        if Btn1State <> 0 then
          Btn1State := 0;

        if Btn2State <> 0 then
          Btn2State := 0;

        inherited;
      end;
  end
  else
    inherited;
end;


procedure TsBaseSpinEdit.WMNCMouseUp(var Message: TWMMouse);
var
  p: TPoint;
begin
  FreeAndNil(FRepeatTimer);
  p := acMouseInWnd(Handle, Message.XPos, Message.YPos);
  MousePressed := False;
  if PtInRect(BtnRect(True), p) then begin
    if Btn1State = 2 then begin
      Btn1State := 1;
      UpClick(Self);
    end;
  end
  else
    if PtInRect(BtnRect(False), p) then begin
      if Btn2State = 2 then begin
        Btn2State := 1;
        DownClick(Self);
      end;
    end
    else begin
      if Btn1State <> 0 then
        Btn1State := 0;

      if Btn2State <> 0 then
        Btn2State := 0;

      inherited;
    end;
end;


function TsBaseSpinEdit.BtnRect(IsUp: boolean): TRect;
var
  bw: integer;
begin
  bw := EditBorderWidth({$IFDEF TNTUNICODE}TTntEdit{$ELSE}TEdit{$ENDIF}(Self));
  Result := Rect(bw, bw, Width - bw, Height - bw);
  if IsRightToLeft then
    Result.Right := Result.Left + acScrollBtnLength
  else
    Result.Left := Result.Right - acScrollBtnLength;

  if IsUp then
    Result.Bottom := Height div 2
  else
    Result.Top := Height div 2 + Height mod 2;
end;


procedure TsBaseSpinEdit.CMTextChanged(var Message: TMessage);
begin
  if not FCreating then
    inherited;
end;


procedure TsTimePicker.CMMouseWheel(var Message: TCMMouseWheel);
begin
  inherited;
  if not ReadOnly and (Message.Result = 0) then
    if Message.WheelDelta < 0 then
      DownClick(Self)
    else
      if Message.WheelDelta > 0 then
        UpClick(Self);
end;


procedure TsBaseSpinEdit.SetShowSpinButtons(const Value: Boolean);
begin
  if FShowSpinButtons <> Value then begin
    FShowSpinButtons := Value;
    if not (csLoading in ComponentState) then begin
      SkinData.BGChanged := True;
      RedrawWindow(Handle, nil, 0, RDWA_ALLNOW);
    end;
  end;
end;


function TsBaseSpinEdit.TextRect: TRect;
var
  bw, iSingle: integer;
begin
  if BorderStyle <> bsNone then
    bw := EditBorderWidth({$IFDEF TNTUNICODE}TTntEdit{$ELSE}TEdit{$ENDIF}(Self)) {$IFDEF DELPHI7UP} + integer(BevelKind <> bkNone) * (integer(BevelOuter <> bvNone) + integer(BevelInner <> bvNone)) {$ENDIF}
  else
    bw := 0;

  iSingle := integer(BorderStyle = bsSingle);
  if IsRightToLeft then
    Result := Rect(ActualSpinWidth * integer(FShowSpinButtons) + bw + 2 * integer(not Ctl3D), bw, Width - bw - AddedGlyphSpace - acSpacing div 2, Height - bw)
  else
    Result := Rect(AddedGlyphSpace + bw + iSingle, bw + iSingle, Width - bw - iSingle - ActualSpinWidth * integer(FShowSpinButtons), Height - bw);
end;


procedure TsBaseSpinEdit.TimerExpired;
begin
  FRepeatTimer.Interval := RepeatPause;
  try
    if Btn1State = 2 then
      UpClick(Self)
    else
      if Btn2State = 2 then
        DownClick(Self);
  except
    FRepeatTimer.Enabled := False;
    raise;
  end;
end;


procedure TsBaseSpinEdit.UpdateBtnIndex(aSkinned: boolean);
begin
  if aSkinned then
    if FlatSpinButtons then
      BtnSkinNdx := SkinData.SkinManager.ConstData.Sections[ssToolButton]
    else
      BtnSkinNdx := SkinData.SkinManager.ConstData.Sections[ssUpDown]
  else
    BtnSkinNdx := -1;
end;


function TsBaseSpinEdit.FontStored: boolean;
begin
  Result := IsCustomFont(Self, Font, not SkinData.Skinned or SkinData.CustomFont);
end;


function TacTimePanel.ActualWidth: integer;
begin
  FOwner.SkinData.FCacheBmp.Canvas.Font.Assign(FOwner.Font);
  Result := FOwner.SkinData.FCacheBmp.Canvas.TextExtent('AM').cx + 6;
end;


constructor TacTimePanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csAcceptsControls, csSetCaption];
  FOwner := TsSpinEdit(AOwner);
  Width := 22;
end;


procedure TacTimePanel.Loaded;
begin
  inherited Loaded;
  if FOwner.SkinData.FCacheBmp <> nil then
    FOwner.SkinData.FCacheBmp.Canvas.Font.Assign(Font);
end;


procedure TacTimePanel.PaintWindow(aDC: HDC);
var
  DC: hdc;
  R: TRect;
  bWidth: integer;
  Bmp: TBitmap;
  Flags: Cardinal;
begin
  if aDC = 0 then
    DC := GetWindowDC(Handle)
  else
    DC := aDC;

  Bmp := CreateBmp32(Self);
  R := MkRect(Bmp);
  Flags := DT_TOP or DT_NOPREFIX or DT_SINGLELINE;
  Bmp.Canvas.Font.Assign(FOwner.Font);
  if FOwner.SkinData.SkinIndex < 0 then
    FillDC(Bmp.Canvas.Handle, R, ColortoRGB(FOwner.Color))
  else begin
    bWidth := EditBorderWidth({$IFDEF TNTUNICODE}TTntEdit{$ELSE}TEdit{$ENDIF}(FOwner));
    if IsRightToLeft then
      BitBlt(Bmp.Canvas.Handle, 0, 0, Width, Height, FOwner.SkinData.FCacheBmp.Canvas.Handle, bWidth + Left + acScrollBtnLength, bWidth, SRCCOPY)
    else
      BitBlt(Bmp.Canvas.Handle, 0, 0, Width, Height, FOwner.SkinData.FCacheBmp.Canvas.Handle, bWidth + Left, bWidth, SRCCOPY);
  end;
  inc(R.Top);
  inc(R.Left, 3);
  Bmp.Canvas.Brush.Style := bsClear;
  acWriteText(Bmp.Canvas, PacChar(aTimeText[PM]), True, R, Flags);
  BitBlt(DC, 0, 0, Width, Height, Bmp.Canvas.Handle, 0, 0, SRCCOPY);
  Bmp.Free;
end;


procedure TacTimePanel.SetMode(aPM: boolean);
begin
  if PM <> aPM then begin
    PM := aPM;
    Repaint;
  end;
end;


procedure TacTimePanel.UpdatePosition;
var
  bWidth: integer;
begin
  bWidth := EditBorderWidth({$IFDEF TNTUNICODE}TTntEdit{$ELSE}TEdit{$ENDIF}(FOwner));
  Width := ActualWidth;
  Height := FOwner.Height - 2 * bWidth;
  Top := 0;
  if IsRightToLeft then
    Left := bWidth
  else
    Left := bWidth + FOwner.SkinData.FCacheBmp.Canvas.TextExtent(FOwner.Text).cx + 2;

  Cursor := FOwner.Cursor;
end;


procedure TacTimePanel.WndProc(var Message: TMessage);
var
  PS: TPaintStruct;
  BordWidth: integer;
begin
  case Message.Msg of
    WM_LBUTTONDOWN: if FOwner.CanFocus then
      FOwner.SetFocus;

    WM_PAINT: begin
      TWMPaint(Message).DC := BeginPaint(Handle, PS);
      PaintWindow(TWMPaint(Message).DC);
      EndPaint(Handle, PS);
      Exit;
    end;

    WM_ERASEBKGND: begin
      PaintWindow(TWMPaint(Message).DC);
      Exit;
    end;

    WM_WINDOWPOSCHANGING: begin
      inherited;
      BordWidth := EditBorderWidth({$IFDEF TNTUNICODE}TTntEdit{$ELSE}TEdit{$ENDIF}(FOwner));
      with TWMWindowPosChanging(Message).WindowPos^ do begin
        cx := ActualWidth;
        cy := FOwner.Height - 2 * BordWidth;
        Y := 0;
        if IsRightToLeft then
          X := BordWidth
        else
          X := BordWidth + FOwner.SkinData.FCacheBmp.Canvas.TextExtent(FOwner.Text).cx + 2;
      end;
      Exit;
    end;
  end;
  inherited;
end;


procedure TsTimePicker.CMChanged(var Message: TMessage);

  procedure DecodeText(AText: string);
  var
    l, i: integer;
    s: string;
  begin
    l := WordCount(AText, [':']);
    if l > 0 then begin
      s := ExtractWord(1, AText, [':']);
{$IFDEF DELPHI6UP}
      if TryStrToInt(s, i) then begin
{$ELSE}
      i := StrToInt(s);
      begin
{$ENDIF}
        SetHour(i);
        s := ExtractWord(2, AText, [':']);
{$IFDEF DELPHI6UP}
        if TryStrToInt(s, i) then begin
{$ELSE}
        i := StrToInt(s);
        begin
{$ENDIF}
          SetMin(i);
          if l > 2 then begin
            s := ExtractWord(3, AText, [':']);
{$IFDEF DELPHI6UP}
            if TryStrToInt(s, i) then
{$ELSE}
            i := StrToInt(s);
{$ENDIF}
              SetSec(i);
          end;
        end;
      end;
    end;
  end;

begin
  inherited;
  if not (csLoading in ComponentState) and not ValueChanging then begin
    TextChanging := True;
    if Text = '' then
      Value := 0
    else
      DecodeText(Text);

    TextChanging := False;
  end;
end;

end.
