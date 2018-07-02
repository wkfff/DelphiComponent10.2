unit sColorSelect;
{$I sDefs.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Dialogs, Buttons,
  sSpeedButton;


type
{$IFDEF DELPHI_XE3}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]{$ENDIF}
  TsColorSelect = class(TsSpeedButton)
{$IFNDEF NOTFORHELP}
  private
    FColorValue: TColor;
    FCustomColors: TStrings;
    FOnChange: TNotifyEvent;
    FImgWidth: integer;
    FImgHeight: integer;
    FStandardDlg: boolean;
    procedure SetCustomColors(const Value: TStrings);
    procedure SetColorValue(const Value: TColor);
    procedure SetInteger(const Index, Value: integer);
  public
    procedure AfterConstruction; override;
    constructor Create(AOwner: TComponent); override;
    procedure DrawGlyph; override;
    destructor Destroy; override;
    procedure Click; override;
    procedure Loaded; override;
    procedure WndProc(var Message: TMessage); override;
    procedure UpdateGlyph(Repaint: boolean = True);
  published
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
{$ENDIF} // NOTFORHELP
    property CustomColors: TStrings read FCustomColors write SetCustomColors;
    property ColorValue: TColor read FColorValue write SetColorValue;
    property ImgWidth:  integer Index 0 read FImgWidth  write SetInteger default 15;
    property ImgHeight: integer Index 1 read FImgHeight write SetInteger default 15;
    property StandardDlg: boolean read FStandardDlg write FStandardDlg default False;
  end;

implementation

uses sGraphUtils, acntUtils, sDialogs, sCommonData, sMessages, sAlphaGraph, sConst;


procedure TsColorSelect.AfterConstruction;
begin
  inherited;
  UpdateGlyph(False);
end;


procedure TsColorSelect.Click;
var
  ColorDialog: TColorDialog;
begin
  if FStandardDlg then
    ColorDialog := TColorDialog.Create(Self)
  else
    ColorDialog := TsColorDialog.Create(Self);

  ColorDialog.Color := ColorValue;
  ColorDialog.CustomColors.Assign(FCustomColors);
  if ColorDialog.Execute then begin
    ColorValue := ColorDialog.Color;
    CustomColors.Assign(ColorDialog.CustomColors);
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
  FreeAndNil(ColorDialog);
  inherited Click;
end;


constructor TsColorSelect.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCustomColors := TStringList.Create;
  FStandardDlg := False;
  FImgHeight := 15;
  FImgWidth := 15;
end;


destructor TsColorSelect.Destroy;
begin
  FCustomColors.Free;
  inherited;
end;


procedure TsColorSelect.DrawGlyph;
begin
  FillDC(SkinData.FCacheBmp.Canvas.Handle, ImgRect, ColorValue)
end;


procedure TsColorSelect.Loaded;
begin
  inherited;
  UpdateGlyph(False);
end;


procedure TsColorSelect.SetColorValue(const Value: TColor);
begin
  if FColorValue <> Value then begin
    FColorValue := Value;
    Glyph.Assign(nil);
    UpdateGlyph;
  end;
end;


procedure TsColorSelect.SetInteger(const Index, Value: integer);

  procedure ChangeProp(var Prop: integer; Value: integer);
  begin
    if Prop <> Value then begin
      Prop := Value;
      Glyph.Assign(nil);
      UpdateGlyph;
    end;
  end;

begin
  case Index of
    0: ChangeProp(FImgWidth, Value);
    1: ChangeProp(FImgHeight, Value);
  end;
end;


procedure TsColorSelect.UpdateGlyph;
begin
  if not (csLoading in ComponentState) then
    if (FImgHeight <> 0) and (FImgWidth <> 0) then begin
      Glyph.OnChange := nil;
      OldOnChange := nil;
      Glyph.PixelFormat := pf32bit;
      if SkinData.SkinManager <> nil then begin
        Glyph.Width := SkinData.SkinManager.ScaleInt(FImgWidth);
        Glyph.Height := SkinData.SkinManager.ScaleInt(FImgHeight);
      end
      else begin
        Glyph.Width := FImgWidth;
        Glyph.Height := FImgHeight;
      end;
      if Repaint then begin
        SkinData.BGChanged := True;
        GraphRepaint;
      end;
    end;
end;


procedure TsColorSelect.SetCustomColors(const Value: TStrings);
begin
  FCustomColors.Assign(Value);
end;


procedure TsColorSelect.WndProc(var Message: TMessage);
begin
  inherited;
  if Message.Msg = SM_ALPHACMD then
    case Message.WParamHi of
      AC_REFRESH:
        if ACUInt(Message.LParam) = ACUInt(SkinData.SkinManager) then
          UpdateGlyph;
    end;
end;

end.
