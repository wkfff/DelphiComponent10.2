unit sGradBuilder;
{$I sDefs.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, ExtCtrls, Menus, StdCtrls, Buttons, ComCtrls,
  {$IFNDEF DELPHI5} Types, {$ENDIF}

  sGradient, sSkinProvider, sPanel, sLabel, sButton, sComboBox, sRadioButton, sDialogs, sPageControl, sColorSelect,
  sSpeedButton, sGroupBox, sConst, sMessages, sComboBoxes, sEdit, acPopupCtrls,
  sBitBtn;


// Gradient format: <GradientPoint>PointsCount(byte)
// GradientPoint: <Value(uint);Position in percents(byte);Mode(word);>

// Mode:                                                                       | Mask |
//    Lower byte 1-2 bite: 0 - Horizontal; 1 - Vertical; 2 - Diagonal          (and $3)
//               3-4 bits; 0 - Color value; 4 - Opacity value; 8 - Both values (and $C)
//    High byte 1 bit: 0 - not transparent; $10 - Transparent                  (and $10)

type
  TGradPointData = class(TObject)
    Position: integer;
    Value: byte;
  end;

  TPointState = (psMoving, psSubMoving, psColorChoosing, psControlChanging);
  TPointStates = set of TPointState;

  TGradPoints = array of TGradPointData;

  TMaskCheckProc = function(Mask: Cardinal): boolean;

  TGradBuilder = class(TForm)
    PopupMenu1: TPopupMenu;
    Changecolor1: TMenuItem;
    Delete1: TMenuItem;
    sSkinProvider1: TsSkinProvider;
    BitBtn1: TsBitBtn;
    sButton1: TsBitBtn;
    sButton2: TsBitBtn;
    sRadioButton1: TsRadioButton;
    sRadioButton2: TsRadioButton;
    sRadioButton3: TsRadioButton;
    sGroupBox1: TsPanel;
    PaintPanel: TsPanel;
    PaintBox1: TPaintBox;
    sGroupBox2: TsPanel;
    PanelDiag: TsPanel;
    PaintBox2: TPaintBox;
    sColorSelect5: TsColorSelect;
    sColorSelect3: TsColorSelect;
    sColorSelect4: TsColorSelect;
    sColorSelect2: TsColorSelect;
    sColorSelect1: TsColorSelect;
    sGroupBox3: TsGroupBox;
    sColorBox1: TsColorBox;
    sTrackEdit1: TsTrackEdit;
    sLabel1: TsLabel;
    sLabel2: TsLabel;
    procedure PaintBox1Paint(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure sButton1Click(Sender: TObject);
    procedure sButton2Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure sRadioButton1Click(Sender: TObject);
    procedure sColorSelect1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure sColorBox1Change(Sender: TObject);
    procedure sTrackEdit1Change(Sender: TObject);
  private
    NoMouse: boolean;
    UniqWord: word;
    NoControlsUpdate: boolean;
    g: TsGradArray;
    OffsetY: integer;
    PressedPos: TPoint;
    PressedCoord: real;
    PanelPoints: TsPanel;
    PanelOpacity: TsPanel;
    PressedIndex,
    CurrentIndex: integer;
    DeletedPoint: TsGradPoint;
    CurrentArray: TsGradArray;
    State: TPointStates;

    function GetDirection: integer;
    procedure SetDirection(const Value: integer);
    procedure UpdateControlsColor(Value: TColor; Percent: integer; ItsColor: boolean; Active: boolean);
    procedure InitDiagControls;

    procedure ReorderPoints;
    procedure CopyArray(Src: TsGradArray; var Dst: TsGradArray);
    procedure RepaintPanels;
    procedure SetNewColor(C: TColor);
    procedure ChooseColor(AIndex: integer);
    procedure DeletePoint(Index: integer; DoRepaint: boolean = True);
    function InsertGradPoint(Item: TsGradPoint; PrevIndex, NextIndex: integer): integer;
    function GetPointPos(AIndex: integer): integer;
    function GetPointByMouse(X, Y: Integer; CheckProc: TMaskCheckProc): integer;
    procedure PaintArrow(Bmp: TBitmap; ARect: TRect; ATop: boolean; AColor: TColor);

    procedure MovePoint(PointIndex, NewX: integer);
    function AddNewPoint(X: Integer; DoRepaint: boolean = True): integer;
    function GetPointRect(AIndex: integer): TRect;
    procedure PanelPointsPaint(Sender: TObject; Canvas: TCanvas);
    procedure PanelPointsMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure PanelPointsMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure PanelPointsMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

    procedure MoveOpacity(PointIndex, NewX: integer);
    function AddNewOpacity(X: Integer; DoRepaint: boolean = True): integer;
    function GetOpacityRect(AIndex: integer): TRect;
    procedure PanelOpacityPaint(Sender: TObject; Canvas: TCanvas);
    procedure PanelOpacityMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure PanelOpacityMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure PanelOpacityMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    function PointHeight: integer;
    function PointWidth: integer;
  public
    ModalResult: TModalResult;
    ColorDialog1: TColorDialog;
    function AsString: string;
    procedure LoadFromArray(NewArray: TsGradArray);
    property Direction: integer read GetDirection write SetDirection;
  end;


  TPointPanel = class(TsPanel)
  public
    procedure WndProc(var Message: TMessage); override;
  end;


  TOpacityPanel = class(TsPanel)
  public
    procedure WndProc(var Message: TMessage); override;
  end;


const
  PointAmount = 5;
  DelOffset = 15;


var
  GradBuilder: TGradBuilder;
  ColorDialog: TColorDialog;
  ColSelects: array[0..PointAmount - 1] of TsColorSelect;


procedure CreateEditorForm; overload;
procedure CreateEditorForm(CustomDlg: TColorDialog); overload;
procedure KillForm;


implementation

uses math,
  acntUtils, sAlphaGraph, sStyleSimply, sGraphUtils, sColorDialog, acPopupController, sVCLUtils, sSkinManager, acntTypes;


{$R *.DFM}


function TGradBuilder.PointWidth: integer;
begin
  if sSkinProvider1.SkinData.SkinManager <> nil then
    Result := sSkinProvider1.SkinData.SkinManager.ScaleInt(5) * 2 - 1
  else
    Result := 9;
end;


procedure CreateEditorForm;
begin
  Application.CreateForm(TGradBuilder, GradBuilder);
  GradBuilder.BiDiMode := Application.BidiMode;
  if GradBuilder.BiDiMode = bdRightToLeft then
    ReflectControls(GradBuilder, True);
end;


procedure CreateEditorForm(CustomDlg: TColorDialog); overload;
begin
  CreateEditorForm;
  ColorDialog := CustomDlg;
  with GradBuilder do begin
    ColorDialog1 := CustomDlg;
    ColSelects[0] := sColorSelect1;
    ColSelects[1] := sColorSelect5;
    ColSelects[2] := sColorSelect2;
    ColSelects[3] := sColorSelect4;
    ColSelects[4] := sColorSelect3;
  end;
end;


procedure KillForm;
begin
  FreeAndNil(GradBuilder);
end;


procedure TGradBuilder.sRadioButton1Click(Sender: TObject);
begin
  PressedIndex := -1;
  CurrentIndex := -1;
  Direction := TsRadioButton(Sender).Tag;
  sTrackEdit1.Enabled := False;
end;


procedure TGradBuilder.sTrackEdit1Change(Sender: TObject);
begin
  if not (psControlChanging in State) and (CurrentIndex >= 0) then begin
    g[CurrentIndex].Color.A := Byte(Round(sTrackEdit1.Value));
    if g[CurrentIndex].Color.A <> MaxByte then
      g[CurrentIndex].Mode := g[CurrentIndex].Mode or PM_TRANSPARENT
    else
      g[CurrentIndex].Mode := g[CurrentIndex].Mode and not PM_TRANSPARENT;

    if sRadioButton3.Checked then
      PanelDiag.SkinData.Invalidate(True)
    else begin
      ReorderPoints;
      RepaintPanels;
    end;
  end;
end;


procedure TGradBuilder.UpdateControlsColor(Value: TColor; Percent: integer; ItsColor: boolean; Active: boolean);
begin
  if Active then begin
    sLabel2.Caption := IntToStr(Percent) + '%';
    sLabel1.Visible := True;
    sLabel2.Visible := True;
    sColorBox1.Enabled := ItsColor;
    sTrackEdit1.Enabled := not ItsColor;
    State := State + [psControlChanging];
    if ItsColor then begin
      Cardinal(Value) := Cardinal(Value) and $FFFFFF;
      if Cardinal(sColorBox1.Selected) <> Cardinal(Value) then
        sColorBox1.Selected := Value
    end;
    if sTrackEdit1.Value <> Cardinal(Value) shr 24 then
      sTrackEdit1.Value := Cardinal(Value) shr 24;

    State := State - [psControlChanging];
  end
  else begin
    sLabel1.Visible := False;
    sLabel2.Visible := False;
    if ItsColor then
      sColorBox1.Enabled := False
    else
      sTrackEdit1.Enabled := False;
  end
end;


procedure TGradBuilder.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if (ColorDialog = nil) and (ColorDialog1 <> nil) then
    FreeAndNil(ColorDialog1)
  else
    ColorDialog1 := nil;
end;


procedure TGradBuilder.FormCreate(Sender: TObject);
begin
  inherited;
  PressedIndex := -1;
  NoMouse := False;
  NoControlsUpdate := False;
  DeletedPoint.Mode := -1;
  PanelPoints := TPointPanel.Create(sGroupBox1);
  PanelPoints.DoubleBuffered := True;
  PanelPoints.OnPaint := PanelPointsPaint;
  PanelPoints.OnMouseDown := PanelPointsMouseDown;
  PanelPoints.OnMouseUp := PanelPointsMouseUp;
  PanelPoints.OnMouseMove := PanelPointsMouseMove;
  PanelPoints.Left := PaintPanel.Left - PointWidth div 2 - 1;
  PanelPoints.Width := PaintPanel.Width + PointWidth + 2;
  PanelPoints.Top := PaintPanel.Top + PaintPanel.Height + 1;
  PanelPoints.Height := PointHeight + 1;
  PanelPoints.BevelOuter := bvNone;
  PanelPoints.Parent := sGroupBox1;

  PanelOpacity := TOpacityPanel.Create(sGroupBox1);
  PanelOpacity.DoubleBuffered := True;
  PanelOpacity.OnPaint := PanelOpacityPaint;
  PanelOpacity.OnMouseDown := PanelOpacityMouseDown;
  PanelOpacity.OnMouseUp := PanelOpacityMouseUp;
  PanelOpacity.OnMouseMove := PanelOpacityMouseMove;
  PanelOpacity.Left := PaintPanel.Left - (PointWidth div 2 - 1)   - 1;
  PanelOpacity.Width := PaintPanel.Width + PointWidth + 2;
  PanelOpacity.Top := PaintPanel.Top - PointHeight - 2;
  PanelOpacity.Height := PaintPanel.Top + 1;
  PanelOpacity.BevelOuter := bvNone;
  PanelOpacity.Parent := sGroupBox1;
end;


function CheckColorProc(Mask: Cardinal): boolean;
begin
  Result := Mask and MASK_VALUES <> PM_OPACITY;
end;


function CheckOpacityProc(Mask: Cardinal): boolean;
begin
  Result := Mask and PM_OPACITY <> 0;
end;


function TGradBuilder.GetPointByMouse(X, Y: Integer; CheckProc: TMaskCheckProc): integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to Length(g) - 1 do
    if PtInRect(GetPointRect(i), Point(X, PointWidth)) and CheckProc(g[i].Mode) then begin
      Result := i;
      Exit;
    end;
end;


function TGradBuilder.GetPointPos(AIndex: integer): integer;
begin
  Result := PaintPanel.Width * g[AIndex].Percent div 100;
end;


function TGradBuilder.GetPointRect(AIndex: integer): TRect;
begin
  Result.Top := 1;
  Result.Bottom := Result.Top + PointHeight;

  Result.Left := GetPointPos(AIndex) + 1;
  Result.Right := Result.Left + PointWidth;

  if (AIndex = PressedIndex) and (OffsetY > 8) then
    OffsetRect(Result, 0, OffsetY - 8);
end;


function TGradBuilder.AddNewOpacity(X: Integer; DoRepaint: boolean): integer;
var
  Perc, i, PrevIndex, NextIndex: integer;
  NewItem: TsGradPoint;
  B: byte;
begin
  NoMouse := True;
  PrevIndex := 0;
  NextIndex := Length(g) - 1;

  Perc := X * 100 div PaintPanel.Width;
  for i := 1 to Length(g) - 1 do begin
    if g[i].Percent > Perc then begin
      NextIndex := i;
      Break;
    end;
    PrevIndex := i;
  end;
  NewItem.Mode := g[0].Mode and MASK_DIRECTION or PM_OPACITY;
  NewItem.Percent := X * 100 div PaintPanel.Width;
  if CurrentIndex > PrevIndex then
    inc(CurrentIndex);

  Result := InsertGradPoint(NewItem, PrevIndex, NextIndex);

  PrevIndex := 0;
  for i := Result - 1 downto 0 do
    if g[i].Mode and PM_OPACITY <> 0 then begin
      PrevIndex := i;
      Break;
    end;

  NextIndex := 0;
  for i := Result + 1 to Length(g) - 1 do
    if g[i].Mode and PM_OPACITY <> 0 then begin
      NextIndex := i;
      Break;
    end;

  if g[NextIndex].Percent = g[PrevIndex].Percent then
    b := 0
  else
    b := (g[Result].Percent - g[PrevIndex].Percent) * MaxByte div (g[NextIndex].Percent - g[PrevIndex].Percent);

  g[Result].Color.A := (g[PrevIndex].Color.A * (MaxByte - b) + g[NextIndex].Color.A * b) div MaxByte;
  if g[Result].Color.A <> MaxByte then
    g[Result].Mode := g[Result].Mode or PM_TRANSPARENT;

  CurrentIndex := Result;
  if DoRepaint then begin
    UpdateControlsColor(g[Result].Color.C, g[Result].Percent, False, True);
    ReorderPoints;
    RepaintPanels;
  end
  else
    ReorderPoints;

  NoMouse := False;
end;


function TGradBuilder.AddNewPoint(X: Integer; DoRepaint: boolean = True): integer;
var
  Perc, i, PrevIndex, NextIndex: integer;
  NewItem: TsGradPoint;
  C: TsColor;
begin
  NoMouse := True;
  PrevIndex := 0;
  NextIndex := Length(g) - 1;

  Perc := X * 100 div PaintPanel.Width;
  for i := 1 to Length(g) - 1 do begin
    if g[i].Percent > Perc then begin
      NextIndex := i;
      Break;
    end;
    PrevIndex := i;
  end;
  if CurrentIndex >= 0 then begin
    NewItem.Color := g[CurrentIndex].Color;
    NewItem.Color.A := 0;
  end
  else
    NewItem.Color.I := 0;

  NewItem.Mode := g[0].Mode and MASK_DIRECTION;
  NewItem.Percent := X * 100 div PaintPanel.Width;
  if CurrentIndex > PrevIndex then
    inc(CurrentIndex);

  Result := InsertGradPoint(NewItem, PrevIndex, NextIndex);
  CurrentIndex := Result;
  if DoRepaint then begin
    C := g[Result].Color;
    C.A := 0;
    UpdateControlsColor(C.C, g[Result].Percent, True, True);
    ReorderPoints;
    RepaintPanels;
  end
  else
    ReorderPoints;

  NoMouse := False;
end;


procedure TGradBuilder.PanelOpacityMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  i: integer;
begin
  i := GetPointByMouse(X, Y, CheckOpacityProc);
  if i >= 0 then begin
    CurrentIndex := i;
    UpdateControlsColor(g[i].Color.C, g[i].Percent, False, True);
  end
  else
    if mbLeft = Button then
      i := AddNewOpacity(X);

  if IsValidIndex(i, Length(g)) then begin
    PressedIndex := i;
    PressedPos := acMousePos;
    PressedCoord := GetPointPos(i);
  end
  else
    PressedIndex := -1;
end;


procedure TGradBuilder.PanelOpacityMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  NewP: TPoint;
  i: integer;
begin
  if State * [psMoving, psColorChoosing] = [] then begin
    State := State + [psMoving];
    if (PressedIndex > 0) and (PressedIndex < Length(g) - 1) then begin
      MoveOpacity(PressedIndex, min(PaintPanel.Width - 4, max(0, X)));
      if (OffsetY > DelOffset) and not acMouseInControl(PanelOpacity) and (PressedIndex > 0) then begin
        DeletedPoint := g[PressedIndex];
        i := PressedIndex;
        PressedIndex := -2;
        DeletePoint(i);
        CurrentIndex := -1;
        sTrackEdit1.Enabled := False;
      end
    end
    else
      if PressedIndex = -2 then begin
        NewP := acMousePos;
        OffsetY := max(0, OffsetY - NewP.Y + PressedPos.Y);
        PressedPos := NewP;
        if OffsetY < DelOffset then begin
          i := AddNewPoint(X, False);
          PressedIndex := i;
          g[i].Color := DeletedPoint.Color;
          g[i].Mode := DeletedPoint.Mode;
          g[i].Index := DeletedPoint.Index;

          UpdateControlsColor(DeletedPoint.Color.C, g[i].Percent, False, True);
          PressedCoord := GetPointPos(i);
          RepaintPanels;
        end;
      end;

    State := State - [psMoving];
  end;
end;


procedure TGradBuilder.PanelOpacityMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  bRemoved: boolean;
begin
  bRemoved := False;;
  if State * [psColorChoosing] = [] then
    if not acMouseInControl(PanelOpacity) then
      if (PressedIndex > 0) and (PressedIndex < Length(g) - 1) then begin
        DeletePoint(PressedIndex);
        bRemoved := True;
      end;

  OffsetY := 0;
  PressedIndex := -1;
  if not bRemoved and sTrackEdit1.Enabled then begin
    PanelOpacity.SetFocus;
    sTrackEdit1.SetFocus;
    sTrackEdit1.SelectAll;
  end
  else begin
    CurrentIndex := -1;
    sTrackEdit1.Enabled := False;
  end;
end;


procedure TGradBuilder.PanelOpacityPaint(Sender: TObject; Canvas: TCanvas);
var
  i: integer;

  procedure PaintPoint(AIndex: integer);
  var
    R, RBmp: TRect;
    Bmp: TBitmap;
    b: byte;
  begin
    R := GetOpacityRect(AIndex);
    RBmp := R;
    InflateRect(RBmp, 1, 1);
    Bmp := CreateBmp32(R);
    b := MaxByte - g[AIndex].Color.A;
    BitBlt(Bmp.Canvas.Handle, 0, 0, Bmp.Width, Bmp.Height, Canvas.Handle, RBmp.Left, RBmp.Top, SRCCOPY);

    if (sSkinProvider1.SkinData.SkinManager <> nil) and sSkinProvider1.SkinData.SkinManager.CommonSkinData.Active then
      PaintArrow(Bmp, MkRect(Bmp), False, sSkinProvider1.SkinData.SkinManager.Palette[pcBorder])
    else
      PaintArrow(Bmp, MkRect(Bmp), False, clBtnShadow);

    PaintArrow(Bmp, Rect(1, 1, Bmp.Width - 1, Bmp.Height - 1), False, b shl 16 or b shl 8 or b);
    BitBlt(Canvas.Handle, RBmp.Left, RBmp.Top, Bmp.Width, Bmp.Height, Bmp.Canvas.Handle, 0, 0, SRCCOPY);
    Bmp.Free;
  end;

begin
  if not PanelOpacity.SkinData.Skinned then
    FillDC(Canvas.Handle, MkRect(PanelOpacity), PanelOpacity.Color);

  for i := 0 to Length(g) - 1 do
    if g[i].Mode and PM_OPACITY <> 0 then
      PaintPoint(i);
end;


procedure TGradBuilder.PanelPointsMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  i: integer;
begin
  i := GetPointByMouse(X, Y, CheckColorProc);
  if i >= 0 then begin
    CurrentIndex := i;
    sColorBox1.Enabled := True;
    UpdateControlsColor(g[i].Color.C, g[i].Percent, True, True);
  end
  else
    if mbLeft = Button then
      i := AddNewPoint(X);

  if IsValidIndex(i, Length(g)) then begin
    PressedIndex := i;
    PressedPos := acMousePos;
    PressedCoord := GetPointPos(i);
  end
  else
    PressedIndex := -1;
end;


procedure TGradBuilder.PanelPointsMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  NewP: TPoint;
  i: integer;
begin
  if State * [psMoving, psColorChoosing] = [] then begin
    State := State + [psMoving];
    if (PressedIndex > 0) and (PressedIndex < Length(g) - 1) then begin
      MovePoint(PressedIndex, min(PaintPanel.Width - 4, max(0, X)));
      if (OffsetY > DelOffset) and not acMouseInControl(PanelPoints) and (PressedIndex > 0) then begin
        DeletedPoint := g[PressedIndex];
        i := PressedIndex;
        PressedIndex := -2;
        DeletePoint(i);
      end;
    end
    else
      if PressedIndex = -2 then begin
        NewP := acMousePos;
        OffsetY := max(0, OffsetY + NewP.Y - PressedPos.Y);
        PressedPos := NewP;
        if OffsetY < DelOffset then begin
          i := AddNewPoint(X, False);
          PressedIndex := i;
          g[i].Color := DeletedPoint.Color;
          g[i].Mode := DeletedPoint.Mode;
          g[i].Index := DeletedPoint.Index;

          UpdateControlsColor(DeletedPoint.Color.C, g[i].Percent, True, True);
          PressedCoord := GetPointPos(i);
          RepaintPanels;
        end;
      end;

    State := State - [psMoving];
  end;
end;


procedure TGradBuilder.PanelPointsMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  i: integer;
begin
  if State * [psColorChoosing] = [] then
    if not acMouseInControl(PanelPoints) then begin
      if (PressedIndex > 0) and (PressedIndex < Length(g) - 1) then
        DeletePoint(PressedIndex)
    end
    else
      if mbRight = Button then begin
        i := GetPointByMouse(X, Y, CheckColorProc);
        if i >= 0 then
          ChooseColor(i);
      end;

  PressedIndex := -1;
  OffsetY := 0;
end;


procedure TGradBuilder.PanelPointsPaint(Sender: TObject; Canvas: TCanvas);
var
  i: integer;

  procedure PaintPoint(AIndex: integer);
  var
    R, RBmp: TRect;
    Bmp: TBitmap;
  begin
    R := GetPointRect(AIndex);
    RBmp := R;
    InflateRect(RBmp, 1, 1);
    Bmp := CreateBmp32(R);

    BitBlt(Bmp.Canvas.Handle, 0, 0, Bmp.Width, Bmp.Height, Canvas.Handle, RBmp.Left, RBmp.Top, SRCCOPY);
    if (sSkinProvider1.SkinData.SkinManager <> nil) and sSkinProvider1.SkinData.SkinManager.CommonSkinData.Active then
      PaintArrow(Bmp, MkRect(Bmp), True, sSkinProvider1.SkinData.SkinManager.Palette[pcBorder])
    else
      PaintArrow(Bmp, MkRect(Bmp), True, clBtnShadow);

    PaintArrow(Bmp, Rect(1, 1, Bmp.Width - 1, Bmp.Height - 1), True, g[AIndex].Color.C);
    BitBlt(Canvas.Handle, RBmp.Left, RBmp.Top, Bmp.Width, Bmp.Height, Bmp.Canvas.Handle, 0, 0, SRCCOPY);
    Bmp.Free;
  end;

begin
  if not PanelPoints.SkinData.Skinned then
    FillDC(Canvas.Handle, MkRect(PanelPoints), PanelPoints.Color);

  for i := 0 to Length(g) - 1 do
    if g[i].Mode and MASK_VALUES <> PM_OPACITY then
      PaintPoint(i);
end;


function TGradBuilder.PointHeight: integer;
begin
  Result := 15;
  if sSkinProvider1.SkinData.SkinManager <> nil then
    Result := sSkinProvider1.SkinData.SkinManager.ScaleInt(Result);
end;


procedure TGradBuilder.ChooseColor(AIndex: integer);
begin
  State := State + [psColorChoosing];
  CurrentIndex := AIndex;
  if sColorDialogForm <> nil then
    FreeAndNil(sColorDialogForm);

  sColorDialogForm := TsColorDialogForm.Create(Application);

  sColorDialogForm.InitControls(True, False, g[AIndex].Color.C, bsNone);
  sColorDialogForm.SetCurrentColor(g[AIndex].Color.C, False);
  ShowPopupForm(sColorDialogForm, PanelPoints, acMousePos.X, acMousePos.Y);
  GetIntController.FormHandlers[GetIntController.GetFormIndex(sColorDialogForm)].PopupCtrl := PanelPoints;
  // Allow showing of Dlg when screen is captured
  SetWindowLong(sColorDialogForm.Handle, GWL_EXSTYLE, GetWindowLong(sColorDialogForm.Handle, GWL_EXSTYLE) and not WS_EX_LAYERED);
end;


procedure TGradBuilder.CopyArray(Src: TsGradArray; var Dst: TsGradArray);
var
  i, l: integer;
begin
  l := Length(Src);
  SetLength(Dst, l);
  for i := 0 to l - 1 do
    Dst[i] := Src[i];
end;


procedure TGradBuilder.DeletePoint(Index: integer; DoRepaint: boolean = True);
var
  i, l: integer;
begin
  l := Length(g);
  if IsValidIndex(Index, l) then begin
    for i := Index to l - 2 do
      g[i] := g[i + 1];

    SetLength(g, Length(g) - 1);

    if DoRepaint then begin
      ReorderPoints;
      UpdateControlsColor(clNone, 0, True, False);
      RepaintPanels;
    end;
  end;
end;


procedure TGradBuilder.PaintBox1Paint(Sender: TObject);
var
  i: integer;
  Bmp: TBitMap;
  TmpArrag: TsGradArray;
begin
  Bmp := CreateBmp32(TPaintBox(Sender).Width, TPaintBox(Sender).Height);
  try
    PaintTransBG(Bmp, MkRect(Bmp), clWhite, clSilver, acArrowSize);
    if g[0].Mode and MASK_DIRECTION = 0 then begin
      SetLength(TmpArrag, Length(g));
      for i := 0 to Length(TmpArrag) - 1 do begin
        TmpArrag[i].Color := g[i].Color;
        TmpArrag[i].Mode := g[i].Mode or 1;
        TmpArrag[i].Percent := g[i].Percent;
        TmpArrag[i].Index := g[i].Index;
      end;
      PaintGrad(Bmp, MkRect(Bmp), TmpArrag);
    end
    else
      PaintGrad(Bmp, MkRect(Bmp), g);

    TPaintBox(Sender).Canvas.Draw(0, 0, Bmp);
  finally
    FreeAndNil(Bmp);
  end;
end;


procedure TGradBuilder.PaintArrow(Bmp: TBitmap; ARect: TRect; ATop: boolean; AColor: TColor);
const
  ArrowPos: array [boolean] of TacSide = (asBottom, asTop);
var
  NewRect, ArrowRect: TRect;
  ArrowSize: integer;
begin
  AColor := AColor and $FFFFFF;
  NewRect := ARect;
  ArrowRect := ARect;
  ArrowSize := WidthOf(ARect) div 2;
  if ATop then begin
    inc(NewRect.Top, ArrowSize);
    ArrowRect.Bottom := ArrowRect.Top + ArrowSize;
  end
  else begin
    dec(NewRect.Bottom, ArrowSize);
    ArrowRect.Top := ArrowRect.Bottom - ArrowSize;
  end;
  FillDC(Bmp.Canvas.Handle, NewRect, ColorToRGB(AColor));
  DrawArrow(Bmp, AColor, AColor, ArrowRect, ArrowPos[ATop], 1, 0, ArrowSize, arsSolid1);
end;


procedure TGradBuilder.FormShow(Sender: TObject);
begin
  CurrentIndex := -1;
{$IFDEF DELPHI7UP}
  sColorBox1.PopupMode := pmPickColor;
{$ENDIF}  
  if (CurrentArray <> nil) and (Length(CurrentArray) > 0) then
    Direction := max(CurrentArray[0].Mode and MASK_DIRECTION, 0)
  else
    Direction := 0;
end;


procedure TGradBuilder.BitBtn1Click(Sender: TObject);
begin
  ModalResult := mrCancel;
  Close;
end;


function TGradBuilder.AsString: string;
var
  i, l: integer;
begin
  Result := '';
  l := Length(g);
  for i := 0 to l - 1 do
    with g[i] do
      Result := Result + IntToStr(Color.I) + CharSemicolon + IntToStr(Percent) + CharSemicolon + IntToStr(Mode) + CharSemicolon;

  Result := Result + Char(l + $40);
end;


procedure TGradBuilder.LoadFromArray(NewArray: TsGradArray);
var
  i, c: integer;
begin
  CurrentArray := NewArray;
  c := Length(CurrentArray) - 1;
  if c < 0 then begin
    c := 1;
    SetLength(CurrentArray, c + 1);
    Cardinal(CurrentArray[0].Color.I) := $FF000000;
    CurrentArray[0].Mode := PM_ALL or PM_OPACITY;
    CurrentArray[0].Percent := 0;
    Cardinal(CurrentArray[1].Color.I) := 0;
    CurrentArray[1].Mode := PM_ALL or PM_OPACITY or PM_TRANSPARENT;
    CurrentArray[1].Percent := 100;
  end;
  SetLength(g, c + 1);
  for i := 0 to c do
    with CurrentArray[i] do begin
      g[i].Color   := Color;
      g[i].Percent := Percent;
      g[i].Mode    := Mode;
      g[i].Index   := i;
    end;

  UniqWord := c + 1;
  case g[0].Mode and MASK_DIRECTION of
    PM_VERTICAL: sRadioButton1.Checked := True;
    PM_DIAGONAL: sRadioButton3.Checked := True
    else         sRadioButton2.Checked := True
  end;
  if not sRadioButton2.Checked then
    ReorderPoints;

  Direction := CurrentArray[0].Mode and MASK_DIRECTION;
  g[0].Mode := g[0].Mode or PM_ALL or PM_OPACITY;
  g[c].Mode := g[c].Mode or PM_ALL or PM_OPACITY;
end;


procedure TGradBuilder.MoveOpacity(PointIndex, NewX: integer);
var
  i: integer;
  NewP: TPoint;
begin
  if State * [psSubMoving, psColorChoosing] = [] then begin
    State := State + [psSubMoving];
    DeletedPoint := g[PressedIndex];
    i := PressedIndex;
    PressedIndex := -2;
    CurrentIndex := -1;
    DeletePoint(i, False);
    ReorderPoints;

    NewP := acMousePos;
    OffsetY := max(0, OffsetY - NewP.Y + PressedPos.Y);
    PressedPos := NewP;

    i := AddNewPoint(NewX, False);

    PressedIndex := i;
    CurrentIndex := i;
    PressedCoord := GetPointPos(i);
    g[i].Color := DeletedPoint.Color;
    g[i].Mode := DeletedPoint.Mode;
    g[i].Index := DeletedPoint.Index;
    UpdateControlsColor(DeletedPoint.Color.C, g[i].Percent, False, True);
    ReorderPoints;
    RepaintPanels;
    State := State - [psSubMoving];
  end;
end;


procedure TGradBuilder.MovePoint(PointIndex, NewX: integer);
var
  i: integer;
  NewP: TPoint;
begin
  if State * [psSubMoving, psColorChoosing] = [] then begin
    State := State + [psSubMoving];
    DeletedPoint := g[PressedIndex];
    i := PressedIndex;
    PressedIndex := -2;
    CurrentIndex := -1;

    DeletePoint(i, False);
    ReorderPoints;

    NewP := acMousePos;
    OffsetY := max(0, OffsetY + NewP.Y - PressedPos.Y);
    PressedPos := NewP;

    i := AddNewPoint(NewX, False);

    PressedIndex := i;
    CurrentIndex := i;
    PressedCoord := GetPointPos(i);
    g[i].Color := DeletedPoint.Color;
    g[i].Mode := DeletedPoint.Mode;
    g[i].Index := DeletedPoint.Index;
    UpdateControlsColor(DeletedPoint.Color.C, g[i].Percent, True, True);
    ReorderPoints;
    RepaintPanels;
    State := State - [psSubMoving];
  end;
end;


procedure TGradBuilder.sButton1Click(Sender: TObject);
begin
  ModalResult := mrOk;
  Close;
end;


procedure TGradBuilder.sButton2Click(Sender: TObject);
const
  sReally = 'Do you really wanna clear a current gradient information?';
begin
{$IFNDEF ALITE}
  if Customrequest(sReally) then begin
{$ELSE}
  if MessageDlg(sReally, mtConfirmation, [mbYes, mbNo], 0) = mrYes then begin
{$ENDIF}
    ModalResult := mrNone;
    Close;
  end;
end;


function TGradBuilder.GetDirection: integer;
begin
  if sRadioButton1.Checked then
    Result := 0
  else
    Result := 1 + integer(not sRadioButton2.Checked)
end;


function TGradBuilder.GetOpacityRect(AIndex: integer): TRect;
begin
  Result.Bottom := PanelOpacity.Height - 1;
  Result.Top := Result.Bottom - PointHeight;
  Result.Left := GetPointPos(AIndex) + 1;
  Result.Right := Result.Left + PointWidth;
  if (AIndex = PressedIndex) and (OffsetY > 8) then
    OffsetRect(Result, 0, -(OffsetY - 8));
end;


procedure TGradBuilder.SetDirection(const Value: integer);
var
  i: integer;
begin
  case Value of
    0: begin
      sRadioButton1.Checked := True;
      sGroupBox1.BringToFront;
    end;

    1: begin
      sRadioButton2.Checked := True;
      sGroupBox1.BringToFront;
    end;

    2: begin
      InitDiagControls;
      for i := 0 to 4 do
        ColSelects[i].ColorValue := Cardinal(g[i].Color) and $FFFFFF;

      sRadioButton3.Checked := True;
      sGroupBox2.BringToFront;
    end;
  end;

  for i := 0 to Length(g) - 1 do begin
    g[i].Mode := g[i].Mode and not MASK_DIRECTION;
    g[i].Mode := g[i].Mode or Value;
  end;
end;


procedure TGradBuilder.SetNewColor(C: TColor);
begin
  if CurrentIndex >= 0 then begin
    sColorBox1.Selected := C;
    sColorBox1.OnChange(sColorBox1);
  end;
end;


procedure TGradBuilder.sColorBox1Change(Sender: TObject);
var
  b: byte;
begin
  if not (psControlChanging in State) and (CurrentIndex >= 0) then begin
    b := g[CurrentIndex].Color.A;
    g[CurrentIndex].Color.C := sColorBox1.Selected;
    g[CurrentIndex].Color.A := b;
    RepaintPanels;
  end;
end;


procedure TGradBuilder.sColorSelect1Change(Sender: TObject);
begin
  InitDiagControls;
  CurrentIndex := TsColorSelect(Sender).Tag;
  State := State + [psControlChanging];
  sTrackEdit1.Value := g[CurrentIndex].Color.A;
  State := State - [psControlChanging];
  g[CurrentIndex].Color.C := TsColorSelect(Sender).ColorValue;
  g[CurrentIndex].Color.A := byte(Round(sTrackEdit1.Value));
  sTrackEdit1.Enabled := True;
  PanelDiag.SkinData.Invalidate(True);
end;


procedure TGradBuilder.ReorderPoints;
var
  TmpArray: TsGradArray;
  i, j, l: integer;

  function SmallerPercentPoint(a: TsGradArray; PercentFrom: integer; CurIndex: integer): integer;
  var
    i, MinPercent: integer;
  begin
    Result := -1;
    MinPercent := PercentFrom;
    for i := CurIndex to Length(a) - 1 do
      if a[i].Percent < MinPercent then begin
        MinPercent := a[i].Percent;
        Result := i;
      end;
  end;

  procedure UpdateColor(Index: integer);
  var
    b, i, IndexPrev, IndexNext: integer;
    tmp: byte;
  begin
    IndexPrev := 0;
    for i := Index - 1 downto 0 do
      if g[i].Mode and MASK_VALUES <> PM_OPACITY then begin
        IndexPrev := i;
        Break;
      end;

    IndexNext := 0;
    for i := Index + 1 to Length(g) - 1 do
      if g[i].Mode and MASK_VALUES <> PM_OPACITY then begin
        IndexNext := i;
        Break;
      end;

    tmp := g[Index].Color.A;
    if g[IndexNext].Percent - g[IndexPrev].Percent <> 0 then
      b := (g[Index].Percent - g[IndexPrev].Percent) * MaxByte div (g[IndexNext].Percent - g[IndexPrev].Percent)
    else
      b := 0;

    g[Index].Color.C := BlendColors(g[IndexPrev].Color.C, g[IndexNext].Color.C, MaxByte - b);
    g[Index].Color.A := tmp;
  end;

  procedure UpdateOpacity(Index: integer);
  var
    b: byte;
    i, IndexPrev, IndexNext: integer;
  begin
    IndexPrev := 0;
    for i := Index - 1 downto 0 do
      if g[i].Mode and PM_OPACITY <> 0 then begin
        IndexPrev := i;
        Break;
      end;

    IndexNext := 0;
    for i := Index + 1 to Length(g) - 1 do
      if g[i].Mode and PM_OPACITY <> 0 then begin
        IndexNext := i;
        Break;
      end;

    b := (g[Index].Percent - g[IndexPrev].Percent) * MaxByte div (g[IndexNext].Percent - g[IndexPrev].Percent);
    g[Index].Color.A := (g[IndexPrev].Color.A * (MaxByte - b) + g[IndexNext].Color.A * b) div MaxByte;
    if g[Index].Color.A <> MaxByte then
      g[Index].Mode := g[Index].Mode or PM_TRANSPARENT;
  end;

begin
  CopyArray(g, TmpArray);
  l := Length(g);
  // Set default points
  g[0].Mode := g[0].Mode or $C;
  g[l - 1].Mode := g[l - 1].Mode or $C;
  // Sorting
  for i := 1 to l - 1 do begin
    j := SmallerPercentPoint(TmpArray, g[i - 1].Percent, i);
    if IsValidIndex(j, l) then
      g[i] := TmpArray[j];
  end;
  // Check colors in opacity points
  for i := 1 to l - 2 do
    if g[i].Mode and MASK_VALUES = PM_OPACITY then
      UpdateColor(i);

  // Check opacity in colors points
  for i := 1 to l - 2 do
    if g[i].Mode and MASK_VALUES = 0 then
      UpdateOpacity(i);
end;


procedure TGradBuilder.RepaintPanels;
begin
  PanelPoints. SkinData.Invalidate(True);
  PanelOpacity.SkinData.Invalidate(True);
  PaintPanel.  SkinData.Invalidate(True);
end;


procedure TGradBuilder.InitDiagControls;
begin
  UniqWord := Length(g);
  while Length(g) < 5 do begin
    SetLength(g, Length(g) + 1);
    g[Length(g) - 1] := g[Length(g) - 2];
    g[Length(g) - 1].Index := UniqWord;
    inc(UniqWord);
  end;
end;


function TGradBuilder.InsertGradPoint(Item: TsGradPoint; PrevIndex, NextIndex: integer): integer;
var
  i, l: integer;
begin
  l := Length(g) + 1;
  SetLength(g, l);
  for i := l - 1 downto NextIndex + 1 do
    g[i] := g[i - 1];

  g[NextIndex] := Item;
  g[NextIndex].Index := UniqWord;
  inc(UniqWord);
  Result := NextIndex;
end;


procedure TPointPanel.WndProc(var Message: TMessage);
var
  i: integer;
begin
  inherited;
  case Message.Msg of
    SM_ALPHACMD:
      case Message.WParamHi of
        AC_POPUPCLOSED: begin
          if (Message.LParam <> 0) and (TsColorDialogForm(Message.LParam).ModalResult = mrOk) then
            GradBuilder.SetNewColor(ColorToRGB(TsColorDialogForm(Message.LParam).sColor.C));

          GradBuilder.State := GradBuilder.State - [psColorChoosing];
        end;
      end;

    WM_LBUTTONDBLCLK, WM_NCLBUTTONDBLCLK: begin
      i := GradBuilder.GetPointByMouse(TWMMouse(Message).XPos, TWMMouse(Message).YPos, CheckColorProc);
      if i >= 0 then
        GradBuilder.ChooseColor(i);
    end;

    WM_NCHITTEST:
      with ScreenToClient(Point(TWMMouse(Message).XPos, TWMMouse(Message).YPos)) do
        if GradBuilder.GetPointByMouse(X, Y, CheckColorProc) >= 0 then
          Cursor := crDefault
        else
          Cursor := crHandPoint;
  end;
end;


procedure TOpacityPanel.WndProc(var Message: TMessage);
begin
  inherited;
  case Message.Msg of
    WM_NCHITTEST:
      with ScreenToClient(Point(TWMMouse(Message).XPos, TWMMouse(Message).YPos)) do
        if GradBuilder.GetPointByMouse(X, Y, CheckOpacityProc) >= 0 then
          Cursor := crDefault
        else
          Cursor := crHandPoint;
  end;
end;

end.

