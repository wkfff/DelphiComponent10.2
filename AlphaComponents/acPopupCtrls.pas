unit acPopupCtrls;
{$I sDefs.inc}
//{$DEFINE LOGGED}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, Buttons, ComCtrls,
  sMemo, sEdit, sComboEdit;


type
{$IFDEF DELPHI_XE3}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]{$ENDIF}
  TsPopupBox = class(TsComboEdit)
{$IFNDEF NOTFORHELP}
  private
    FOnBeforePopup: TNotifyEvent;
    FPopupForm: TCustomForm;
    procedure SetPopupForm(const Value: TCustomForm);
  protected
    procedure PopupWindowShow; override;
    procedure PopupWindowClose; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    procedure Loaded; override;
    constructor Create(AOwner: TComponent); override;
    procedure WndProc(var Message: TMessage); override;
  published
{$ENDIF} // NOTFORHELP
    property PopupForm: TCustomForm read FPopupForm write SetPopupForm;
    property OnBeforePopup: TNotifyEvent read FOnBeforePopup write FOnBeforePopup;
  end;


{$IFNDEF NOTFORHELP}
  TOnSetTextFromValue = procedure(Sender: TObject; var AText: string) of object;
  TOnSetValueFromText = procedure(Sender: TObject; var AValue: Extended) of object;
{$ENDIF} // NOTFORHELP

{$IFDEF DELPHI_XE3}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]{$ENDIF}
  TsTrackEdit = class(TsEdit)
{$IFNDEF NOTFORHELP}
  private
    FValue,
    FMinValue,
    FMaxValue,
    FIncrement: Extended;

    FOnCloseUp,
    FOnDropDown: TNotifyEvent;

    FAutoPopup,
    TrackShowing,
    FShowProgress,
    FEditorEnabled,
    FAllowNegative,
    FHideExcessZeros,
    FUseSystemDecSeparator: boolean;

    FNextTabControl,
    FPrevTabControl: TWinControl;

    FDecimalPlaces,
    FMinTrackWidth: integer;

    FPopupKey: TShortCut;

    FTrackAlphaBlend: byte;
    FOnSetValueFromText: TOnSetValueFromText;
    FOnSetTextFromValue: TOnSetTextFromValue;
    procedure FormatText;
    function GetValue: Extended;
    function GetAsInteger: Longint;
    function GetFormattedText: string;
    procedure SetValue (NewValue: Extended);
    function CheckValue(NewValue: Extended): Extended;
    procedure SetHideExcessZeros(const Value: boolean);
    procedure WMCut       (var Message: TWMCut);        message WM_CUT;
    procedure CMExit      (var Message: TCMExit);       message CM_EXIT;
    procedure WMPaste     (var Message: TWMPaste);      message WM_PASTE;
    procedure CMChanged   (var Message: TMessage);      message CM_CHANGED;
    procedure CMMouseWheel(var Message: TCMMouseWheel); message CM_MOUSEWHEEL;
  protected
    TrackForm: TForm;
    SkipShowing,
    TextChanging,
    ValueChanging: boolean;
    procedure SetDecimalPlaces(New: Integer);
    procedure Change; override;
    procedure KeyDown   (var Key: Word; Shift: TShiftState); override;
    procedure KeyPress  (var Key: Char); override;
    function IsValidChar(var Key: Char): Boolean;
    procedure ShowTrackForm(CheckAuto: boolean);
    function MaxValueStored: boolean;
  public
    constructor Create(AOwner: TComponent); override;
    function DroppedDown: boolean;
    procedure WndProc(var Message: TMessage); override;
    procedure Loaded; override;
    property UseSystemDecSeparator: boolean read FUseSystemDecSeparator write FUseSystemDecSeparator default True;
  published
    property NextTabControl:  TWinControl read FNextTabControl write FNextTabControl;
    property PrevTabControl:  TWinControl read FPrevTabControl write FPrevTabControl;
{$ENDIF} // NOTFORHELP
    property AutoPopup:       Boolean read FAutoPopup write FAutoPopup default True;
    property AllowNegative:   Boolean read FAllowNegative write FAllowNegative default True;
    property EditorEnabled:   Boolean read FEditorEnabled write FEditorEnabled default True;

    property HideExcessZeros: boolean read FHideExcessZeros write SetHideExcessZeros default True;
    property AsInteger: Longint read GetAsInteger;
    property Increment: Extended read FIncrement write FIncrement;
    property MaxValue: Extended read FMaxValue write FMaxValue stored MaxValueStored;
    property MinValue: Extended read FMinValue write FMinValue;
    property MinTrackWidth: integer read FMinTrackWidth write FMinTrackWidth default 140;
    property PopupKey: TShortCut read FPopupKey write FPopupKey default scAlt + vk_Down;
    property ShowProgress: boolean read FShowProgress write FShowProgress default False;
    property TrackAlphaBlend: byte read FTrackAlphaBlend write FTrackAlphaBlend default MaxByte;
    property Value: Extended read GetValue write SetValue;
    property DecimalPlaces: Integer read fDecimalPlaces write SetDecimalPlaces default 2;

    property OnCloseUp:  TNotifyEvent read FOnCloseUp  write FOnCloseUp;
    property OnDropDown: TNotifyEvent read FOnDropDown write FOnDropDown;
    property OnSetTextFromValue: TOnSetTextFromValue read FOnSetTextFromValue write FOnSetTextFromValue;
    property OnSetValueFromText: TOnSetValueFromText read FOnSetValueFromText write FOnSetValueFromText;
  end;

implementation


uses
  math, Menus, sVCLUtils,
  {$IFDEF LOGGED}sDebugMsgs, {$ENDIF}
  sConst, sGlyphUtils, sMessages, sStyleSimply, acPopupController, sCommonData, acntUtils, sTrackBar,
  sThirdParty, sSkinProps, acAlphaImageList;


constructor TsPopupBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SkinData.COC := COC_TsComboBox;
  FDefBmpID := iBTN_ARROW;
end;


procedure TsPopupBox.Loaded;
begin
  inherited;
  if csDesigning in ComponentState then
    AdjustSize;
end;


procedure TsPopupBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if (Key = VK_ESCAPE) and DroppedDown and (FPopupForm <> nil) then begin
    Key := 0;
    FPopupForm.Close;
  end;
end;


procedure TsPopupBox.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
    if AComponent = PopupForm then
      PopupForm := nil;
end;


procedure TsPopupBox.PopupWindowClose;
var
  p: TPoint;
begin
  if FPopupForm.Visible then
    TForm(FPopupForm).Close;

  FPopupWindow := nil;
  FDroppedDown := False;
  p := ScreenToClient(acMousePos);
  BtnState := integer(PtInRect(BtnRect, p));
end;


procedure TsPopupBox.PopupWindowShow;
begin
  if FPopupForm <> nil then begin
    FPopupWindow := FPopupForm;
    if Assigned(FOnBeforePopup) then
      FOnBeforePopup(Self);

    FDroppedDown := True;
    if FPopupForm.BiDiMode <> BiDiMode then
      FPopupForm.BiDiMode := BiDiMode;

    ShowPopupForm(TForm(FPopupForm), Self);
  end
  else
    BtnState := 1;
end;


procedure TsPopupBox.SetPopupForm(const Value: TCustomForm);
begin
  GetIntController.KillAllForms(nil);
  FPopupForm := Value;
end;


procedure TsPopupBox.WndProc(var Message: TMessage);
begin
  case Message.Msg of
    SM_ALPHACMD:
      case Message.WParamHi of
        AC_GETDEFINDEX: begin
          if SkinData.SkinManager <> nil then
            Message.Result := SkinData.SkinManager.ConstData.Sections[ssComboBox] + 1;

          Exit;
        end;
      end;
  end;
  inherited;
  case Message.Msg of
    CM_TEXTCHANGED: begin
      SkinData.BGChanged := True;
      RedrawWindow(Handle, nil, 0, RDWA_ALLNOW);
    end;
  end;
end;


type
  TacTrackForm = class(TForm)
  protected
    MulValue: integer;
    FOwner: TsTrackEdit;
    TrackBar: TsTrackBar;
    OwnerFocused: boolean;
    function SetValue(Value: integer): Extended;
    function GetValue(Value: Extended): integer;
    procedure OnTrackChange(Sender: TObject);
    procedure DoHide; override;
    procedure ac_CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
  public
    constructor CreateNew(AOwner: TComponent; Dummy: Integer  = 0); override;
  end;


procedure TsTrackEdit.Change;
begin
  if [csLoading] * ComponentState = [] then
    inherited;
end;


function TsTrackEdit.CheckValue(NewValue: Extended): Extended;
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


procedure TsTrackEdit.CMChanged(var Message: TMessage);
var
  s: string;
  e: Extended;
begin
  inherited;
  if not (csLoading in ComponentState) and not ValueChanging then begin
    TextChanging := True;
    if (Text = '') or (Text = CharMinus) or (Text = CharPlus) then
      E := 0
    else begin
      s := DelChars(Text, {$IFDEF DELPHI_XE}FormatSettings.{$ENDIF}ThousandSeparator);
{$IFDEF DELPHI6UP}
      E := StrToFloatDef(s, 0);
{$ELSE}
      E := StrToFloat(s);
{$ENDIF}
    end;
    if Assigned(FOnSetValueFromText) then
      FOnSetValueFromText(Self, e);

    Value := E;
    TextChanging := False;
  end;
end;


procedure TsTrackEdit.CMExit(var Message: TCMExit);
begin
  inherited;
  if CheckValue(Value) <> Value then
    SetValue(Value);

  FormatText;
end;


procedure TsTrackEdit.CMMouseWheel(var Message: TCMMouseWheel);
begin
  inherited;
  if not ReadOnly and (Message.Result = 0) then begin
    Value := Value + Increment * (Message.WheelDelta div 120);
    Message.Result := 1
  end;
end;


constructor TsTrackEdit.Create(AOwner: TComponent);
begin
  inherited create(AOwner);
  ValueChanging := False;
  TextChanging := False;
  FAutoPopup := True;
  FUseSystemDecSeparator := True;
  FHideExcessZeros := True;
  FIncrement := 1;
  FShowProgress := False;
  FDecimalPlaces := 2;
  FEditorEnabled := True;
  FAllowNegative := True;
  FMinTrackWidth := 140;
  FPopupKey := scAlt + vk_Down;
  FMaxValue := 100;
  FTrackAlphaBlend := MaxByte;
  TrackShowing := False;
end;


function TsTrackEdit.DroppedDown: boolean;
begin
  Result := (TrackForm <> nil) and IsWindowVisible(TrackForm.Handle);
end;


procedure TsTrackEdit.FormatText;
begin
  ValueChanging := True;
  if not TextChanging then
    SendMessage(Handle, WM_SETTEXT, 0, LPARAM(PChar(GetFormattedText)));

  ValueChanging := False;
end;


function TsTrackEdit.GetFormattedText: string;
var
  l: integer;
begin
  Result := FloatToStrF(CheckValue(FValue), ffFixed, 18, FDecimalPlaces);
  if FHideExcessZeros and (DecimalPlaces > 0) then
    if Value = 0 then
      Result := '0'
    else
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


function TsTrackEdit.GetAsInteger: Longint;
begin
  Result := Round(Value);
end;


function TsTrackEdit.GetValue: Extended;
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


function TsTrackEdit.IsValidChar(var Key: Char): Boolean;
begin
  Result := CharInSet(Key, [{$IFDEF DELPHI_XE}FormatSettings.{$ENDIF}DecimalSeparator, CharPlus, CharMinus, ZeroChar..'9']) or
            (Key < #32) and (Key <> Chr(VK_RETURN));

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
        else begin
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
        end;

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


procedure TsTrackEdit.KeyDown(var Key: Word; Shift: TShiftState);
var
  myForm: TCustomForm;
  CtlDir: integer;
  M: tagMsg;
  sc: TShortCut;
begin
  inherited KeyDown(Key, Shift);
  sc := ShortCut(Key, Shift);
  if sc = FPopupKey then
    ShowTrackForm(False)
  else
    case Key of
      VK_ESCAPE:
        if DroppedDown then begin
          Key := 0;
          TrackForm.Close;
        end;

      VK_LEFT:
        if DroppedDown then
          Value := Value - FIncrement;

      VK_RIGHT:
        if DroppedDown then
          Value := Value + FIncrement;

      VK_UP:
        if ReadOnly then
          MessageBeep(0)
        else
          Value := Value + FIncrement;

      VK_DOWN:
        if ReadOnly then
          MessageBeep(0)
        else
          Value := Value - FIncrement;

      VK_TAB, VK_RETURN: begin
        if ssShift in Shift then begin
          CtlDir := 1;
          if Assigned(FPrevTabControl) then begin
            Key := 0;
            if FPrevTabControl.CanFocus then
              FPrevTabControl.SetFocus;

            Exit;
          end;
        end
        else begin
          if Assigned(FNextTabControl) then begin
            Key := 0;
            if FNextTabControl.CanFocus then
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


procedure TsTrackEdit.KeyPress(var Key: Char);
var
  err: boolean;
  C: Char;
begin
  C := Key;
  err := not IsValidChar(C);
  if (Key = #27) and DroppedDown then begin
    Key := #0;
    TrackForm.Close;
  end
  else
    if err or (C = #0) then begin
      if (C = #0) then
        Key := #0;

      if err then
        MessageBeep(0);
    end
    else
      inherited KeyPress(Key);
end;


procedure TsTrackEdit.Loaded;
begin
  inherited;
  if (MaxValue = 0) and (MinValue = 0) then
    MaxValue := 100; // Default MaxValue is 100

  if Value = 0 then
    Text := '0';
end;


function TsTrackEdit.MaxValueStored: boolean;
begin
  Result := FMaxValue <> 100;
end;


procedure TsTrackEdit.SetDecimalPlaces(New: Integer);
begin
  if fDecimalPlaces <> New then begin
    fDecimalPlaces := New;
    Value := CheckValue(Value);
  end;
end;


procedure TsTrackEdit.SetHideExcessZeros(const Value: boolean);
begin
  if FHideExcessZeros <> Value then begin
    FHideExcessZeros := Value;
    if not (csLoading in ComponentState) then
      Text := GetFormattedText;
  end;
end;


procedure TsTrackEdit.SetValue(NewValue: Extended);
var
  s: string;
begin
  if (NewValue < 0) and not FAllowNegative then
    NewValue := max(0, FMinValue);

  if MaxValue > MinValue then
    FValue := max(min(NewValue, MaxValue), MinValue)
  else
    FValue := NewValue;

  ValueChanging := True;
  if not TextChanging then begin
    s := GetFormattedText;
    if Assigned(FOnSetTextFromValue) then
      FOnSetTextFromValue(Self, s);

    if HandleAllocated then begin
      SendMessage(Handle, WM_SETTEXT, 0, LPARAM(PChar(s)));
      if not (csLoading in ComponentState) and Assigned(OnChange) then
        OnChange(Self);
    end
    else
      Text := s;
  end;

  if DroppedDown then
    TacTrackForm(TrackForm).TrackBar.Position := TacTrackForm(TrackForm).GetValue(FValue);

  ValueChanging := False;
end;


procedure TsTrackEdit.ShowTrackForm;
begin
  if not (csDesigning in ComponentState) then
    if (MaxValue <> MinValue) and not DroppedDown and not TrackShowing then
      if not CheckAuto or AutoPopup then begin
        if acIntController <> nil then
          if acIntController.PopupCount(TForm(GetOwnerForm(Self))) > 0 then
            Exit;

        TrackShowing := True;
        if TrackForm <> nil then
          TrackForm.Free;

        TrackForm := TacTrackForm.CreateNew(Self);
        if Assigned(FOnDropDown) then
          FOnDropDown(Self);

        TrackForm.BiDiMode := BiDiMode;
        if BiDiMode = bdRightToLeft then
          ReflectControls(TrackForm, True);

        ShowPopupForm(TrackForm, Self);
        TrackShowing := False;
      end;
end;


procedure TsTrackEdit.WMCut(var Message: TWMCut);
begin
  if FEditorEnabled and not ReadOnly then
    inherited;
end;


procedure TsTrackEdit.WMPaste(var Message: TWMPaste);
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


procedure TsTrackEdit.WndProc(var Message: TMessage);
begin
{$IFDEF LOGGED}
  AddToLog(Message);
{$ENDIF}
  case Message.Msg of
    WM_SETFOCUS: begin
      if not TrackShowing and not DroppedDown then begin
        inherited;
        ShowTrackForm(True);
      end
      else
        inherited;

      if AutoSelect then
        Self.SelectAll;

      Exit;
    end;
  end;
  inherited;
  case Message.Msg of
    WM_LBUTTONUP:
      if not Focused and Canfocus then begin
        TrackShowing := True;
        if CanFocus and not (csDesigning in ComponentState) then
          SetFocus;
          
        TrackShowing := False;
      end;

    CM_MOUSEENTER:
      if Application.Active then
        ShowTrackForm(True);

    WM_LBUTTONDBLCLK:
      ShowTrackForm(False);

    CM_MOUSELEAVE:
      if DroppedDown and not acMouseInControl(TrackForm) then
        TrackForm.Close;
  end;
end;


function TacTrackForm.GetValue(Value: Extended): integer;
begin
  Result := Round(Value * MulValue);
end;


procedure TacTrackForm.OnTrackChange(Sender: TObject);
begin
  if FOwner.ComponentState * [csLoading, csDestroying] = [] then begin
    FOwner.Perform(WM_SETREDRAW, 0, 0);
    FOwner.Value := SetValue(TrackBar.Position);
    FOwner.Perform(WM_SETREDRAW, 1, 0);
    RedrawWindow(FOwner.Handle, nil, 0, RDW_INVALIDATE or RDW_NOERASE or RDW_UPDATENOW);
  end;
end;


function TacTrackForm.SetValue(Value: integer): Extended;
var
  r: Extended;
  i: int64;
begin
  r := MulValue * FOwner.Increment;
  i := int64(Value) * int64(MulValue);
  r := Round(i / r) div MulValue;
  Result := r * FOwner.Increment;
//  Result := Round(Value * MulValue / (MulValue * FOwner.Increment)) div MulValue * FOwner.Increment;
end;


procedure TacTrackForm.ac_CMMouseLeave(var Message: TMessage);
begin
  if not OwnerFocused then // If was hovered by mouse
    if not acMouseInControl(FOwner) and (GetCapture = 0) then
      Close;
end;


constructor TacTrackForm.CreateNew(AOwner: TComponent; Dummy: Integer);
begin
  inherited;
  OwnerFocused := TWinControl(AOwner).Focused;
  BorderStyle := bsNone;
  FOwner := TsTrackEdit(AOwner);
{$IFDEF DELPHI7UP}
  AlphaBlend := True;
  AlphaBlendValue := FOwner.TrackAlphaBlend;
{$ENDIF}
  Width := max(FOwner.MinTrackWidth, FOwner.Width);
  Height := 32;
  MulValue := Round(IntPower(10, min(3, FOwner.DecimalPlaces)));

  TrackBar := TsTrackBar.Create(Self);
  TrackBar.Align := alClient;
  TrackBar.Max := GetValue(FOwner.MaxValue);
  TrackBar.Min := GetValue(FOwner.MinValue);
  TrackBar.LineSize := GetValue(FOwner.Increment);
  TrackBar.Position := GetValue(FOwner.Value);
  TrackBar.ShowProgress := FOwner.ShowProgress;
  TrackBar.TickStyle := tsNone;
  TrackBar.SkinData.SkinSection := s_MainMenu;
  TrackBar.OnChange := OnTrackChange;
  TrackBar.Parent := Self;
end;


procedure TacTrackForm.DoHide;
begin
  inherited;
  if Assigned(FOwner.OnCloseUp) then
    FOwner.OnCloseUp(Self);
end;

end.
