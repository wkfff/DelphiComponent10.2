unit acCharMap;
{$I sDefs.inc}
//{$DEFINE LOGGED}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, ComCtrls, Grids, ExtCtrls, StdCtrls,
  {$IFDEF DELPHI7UP} Types, {$ENDIF}
  sListView, sSpeedButton, sPanel, sStatusBar, sSkinProvider, sComboBox, sLabel, sConst;


const
  iColCount = 11;
  iCharSize = 28;
  iCellSize = 48;

type
  TacCharGrid = class(TDrawGrid)
  protected
    Clicked: boolean;
    procedure ChangeScale(M, D: Integer{$IFDEF DELPHI_10TOKYO}; isDpiChange: Boolean{$ENDIF}); override;
  public
    HotCell: TPoint;
    procedure InitCtrl;
    procedure WndProc(var Message: TMessage); override;
  end;


  TacCharRow = array [0 .. iColCount - 1] of Word;

  TFormCharMap = class(TForm)
    sPanel1: TsPanel;
    sPanel2: TsPanel;
    sLabel1: TsLabel;
    sLabel2: TsLabel;
    sComboBox1: TsComboBox;
    sComboBox2: TsComboBox;
    sSkinProvider1: TsSkinProvider;
    procedure sComboBox1Change(Sender: TObject);
    procedure sComboBox2Change(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure sSkinProvider1SkinItemEx(Item: TComponent; var CanBeAdded: Boolean; SkinParams: PacSkinParams);
    procedure DrawGrid1DrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
    procedure DrawGrid1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure DrawGrid1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  public
    Grid: TacCharGrid;
    Loading: boolean;
    PressedButton: TsSpeedButton;
    CharArray: array of TacCharRow;
    procedure AfterConstruction; override;
    procedure FillList;
    procedure WndProc(var Message: TMessage); override;
  end;

var
  FormCharMap: TFormCharMap;

implementation

uses
  {$IFDEF LOGGED} sDebugMsgs, {$ENDIF}
  acCharListEditor, sSkinManager, sGraphUtils, acntUtils, sVCLUtils, sSkinProps, sStyleSimply;

{$R *.dfm}


procedure TFormCharMap.AfterConstruction;
begin
  inherited;
  Loading := True;
  Grid := TacCharGrid.Create(FormCharMap);
  Grid.InitCtrl;
  Grid.Parent := sPanel1;
  if sSkinProvider1.SkinData.Skinned then
    sSkinProvider1.Adapter.AddNewItem(Grid);
end;


procedure TFormCharMap.DrawGrid1DrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
var
  R: TRect;
  c: WideChar;
  p: PWideChar;
  Bmp: TBitmap;
  sm: TsSkinManager;
  Ndx, aState: integer;
begin
  if (FormCharMap <> nil) and (ACol >= 0) and (ARow >= 0) then
    with FormCharMap do begin
      if (Grid.HotCell.X = ACol) and (Grid.HotCell.Y = ARow) then
        aState := iff(Grid.Clicked, 2, 1)
      else
        aState := 0;

      c := WideChar(CharArray[ARow, ACol]);
      if sSkinProvider1.SkinData.SkinManager <> nil then
        FormCharMap.Grid.Font.Size := sSkinProvider1.SkinData.SkinManager.ScaleInt(iCharSize)
      else
        FormCharMap.Grid.Font.Size := iCharSize;

      if aState = 0 then begin
        FillDC(Grid.Canvas.Handle, Rect, Grid.Color);
        if sSkinProvider1.SkinData.Skinned then
          Grid.Canvas.Font.Color := sSkinProvider1.SkinData.SkinManager.Palette[pcEditText]
        else
          Grid.Canvas.Font.Color := clWindowText;
      end
      else begin
        if sSkinProvider1.SkinData.Skinned then
          Ndx := sSkinProvider1.SkinData.SkinManager.ConstData.Sections[ssSelection]
        else
          Ndx := -1;

        if Ndx >= 0 then begin
          sm := sSkinProvider1.SkinData.SkinManager;
          Bmp := CreateBmp32(WidthOf(Rect), HeightOf(Rect));
          PaintItemBGFast(Ndx, sm.gd[Ndx].Props[0].TextureIndex, sm.gd[Ndx].Props[1].TextureIndex, EmptyCI, aState, MkRect(Bmp), MkPoint, Bmp, sm);
          BitBlt(Grid.Canvas.Handle, Rect.Left, Rect.Top, Bmp.Width, Bmp.Height, Bmp.Canvas.Handle, 0, 0, SRCCOPY);
          Bmp.Free;
          Grid.Canvas.Font.Color := sm.gd[Ndx].Props[1].FontColor.Color;
        end
        else begin
          FillDC(Grid.Canvas.Handle, Rect, clHighlight);
          Grid.Canvas.Font.Color := clHighlightText;
        end;
      end;
      p := @c;
      SelectObject(Grid.Canvas.Handle, Grid.Canvas.Font.Handle);
      Grid.Canvas.Brush.Style := bsClear;
      DrawTextW(Grid.Canvas.Handle, p, 1, R, DT_NOPREFIX or DT_CALCRECT or DT_EXTERNALLEADING);

      Rect.Top := Rect.Top + (HeightOf(Rect) - HeightOf(R)) div 2;
      Rect.Bottom := Rect.Top + abs(Grid.Canvas.Font.Height);
      DrawTextW(Grid.Canvas.Handle, p, 1, Rect, DT_NOPREFIX or DT_CENTER or DT_NOCLIP or DT_VCENTER);
    end;
end;


procedure TFormCharMap.DrawGrid1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  p: TPoint;
  r, c: integer;
begin
  with FormCharMap do begin
    p := ScreenToClient(acMousePos);
    Grid.MouseToCell(p.X, p.Y, c, r);
    Grid.Repaint;
    Delay(100);
    if IsValidIndex(c, iColCount) and IsValidIndex(r, Grid.RowCount) and (ord(CharArray[r][c]) <> 0) then begin
      if PressedButton = FormCharListEditor.sSpeedButton1 then
        FormCharListEditor.AddNewGlyph(CharArray[r][c], sComboBox1.ItemIndex, sComboBox2.ItemIndex)
      else begin
        FormCharListEditor.sCharImageList1.Items[FormCharListEditor.sListView1.Selected.Index].Char := CharArray[r][c];
        FormCharListEditor.sStatusBar1.Panels[3].Text := '0x' + IntToHex(ord(CharArray[r][c]), 4);
      end;
      FormCharListEditor.sCharImageList1.GenerateStdList;
      Close;
    end;
  end;
end;


procedure TFormCharMap.DrawGrid1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  cR: TRect;
  OldCell: TPoint;
  r, c: integer;
begin
  with FormCharMap do begin
    OldCell := Grid.HotCell;
    Grid.MouseToCell(x, y, c, r);
    if (FormCharMap.Grid.HotCell.X <> c) or (Grid.HotCell.Y <> r) then begin
      Grid.HotCell := Point(c, r);
      if OldCell.Y >= 0 then begin
        cR := Grid.CellRect(OldCell.X, OldCell.Y);
        if FormCharMap.Grid.DefaultRowHeight = HeightOf(cR) then
          DrawGrid1DrawCell(Grid, OldCell.X, OldCell.Y, cR, []);
      end;
      if (r >= 0) and (c >= 0) then begin
        sLabel2.Caption := '0x' + IntToHex(ord(CharArray[r, c]), 4);
        cR := Grid.CellRect(c, r);
        if FormCharMap.Grid.DefaultRowHeight = HeightOf(cR) then
          DrawGrid1DrawCell(Grid, Grid.HotCell.X, Grid.HotCell.Y, cR, []);
      end
      else begin
        Grid.HotCell := Point(-1, -1);
        sLabel2.Caption := '';
      end;
    end;
  end;
end;


procedure TFormCharMap.FillList;
var
  k, x, y, i: integer;
  gs: PGlyphSet;
  R: PWCRange;
  wc: WChar;
begin
  Grid.ColCount := iColCount;
  Grid.RowCount := 0;
  SetLength(CharArray, 0);
  Grid.Canvas.Font.Assign(Grid.Font);
  k := GetFontUnicodeRanges(Grid.Canvas.Handle, nil);
  GetMem(Pointer(gs), k);
  try
    gs.cbThis := k;
    gs.flAccel := 0;
    gs.cGlyphsSupported := 0;
    gs.cRanges := 0;
    if GetFontUnicodeRanges(Grid.Canvas.Handle, gs) > 0 then begin
      k := 0;
      R := @GS.ranges[0];
      for i := 0 to gs^.cRanges - 1 do begin
        inc(k, R^.cGlyphs);
        inc(R);
      end;

      i := k div iColCount + 1;
      SetLength(CharArray, i);
      Grid.RowCount := k div iColCount + 1;

      y := 0;
      x := 0;
      R := @GS.ranges[0];
      for i := 0 to gs.cRanges - 1 do begin
        wc := R^.wcLow;
        for k := 0 to R^.cGlyphs - 1 do begin
          if ord(wc) > 32 then begin
            if x = iColCount then begin
              x := 0;
              inc(y);
            end;
            CharArray[y][x] := ord(wc);
            inc(x);
          end;
          inc(wc);
        end;
        inc(R);
      end;
    end;
  finally
    FreeMem(Pointer(gs));
  end;
  if IsWindowVisible(Grid.Handle) then
    Grid.Repaint;
end;


procedure TFormCharMap.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FormCharMap := nil;
  PressedButton.SkinData.FMouseAbove := False;
  PressedButton.SkinData.BGChanged := True;
  PressedButton.Down := False;
  PressedButton.Repaint;
  Action := caFree;
end;


procedure TFormCharMap.sComboBox1Change(Sender: TObject);
var
  bInstalled: boolean;
  sl: TStringList;
  lf: TLogFont;
  i: integer;
  DC: hdc;
begin
  sComboBox2.Items.BeginUpdate;
  sComboBox2.Clear;
  bInstalled := sComboBox1.ItemIndex < sComboBox1.Items.Count - 1; // Not last item
  if bInstalled then begin
    ZeroMemory(@lf, SizeOf(TLogFont));
    lf.lfCharSet := arCharSets[sComboBox1.ItemIndex];
    DC := GetDC(0);
    try
      EnumFontFamiliesEx(DC, lf, @EnumFontCallback, {$IFDEF DELPHI7UP}NativeInt{$ELSE}LParam{$ENDIF}(sComboBox2.Items), 0);
    finally
      ReleaseDC(0, DC);
    end;
  end
  else
    for i := 0 to FormCharListEditor.sCharImageList1.EmbeddedFonts.Count - 1 do
      sComboBox2.Items.Add(FormCharListEditor.sCharImageList1.EmbeddedFonts[i].FontName);

  sComboBox2.Items.EndUpdate;
  sl := TStringList.Create;
  sl.Assign(sComboBox2.Items);
  if bInstalled then
    sl.Sort;

  sComboBox2.Items.Assign(sl);
  sl.Free;
  if not Loading then begin
    sComboBox2.ItemIndex := 0;
    sComboBox2Change(sComboBox2);
  end
  else
    if bInstalled then
      sComboBox2.ItemIndex := sComboBox2.Items.IndexOf(FormCharListEditor.sListBox1.Items[FormCharListEditor.sListBox1.ItemIndex]);
end;


procedure TFormCharMap.sComboBox2Change(Sender: TObject);
begin
  if not Loading then begin
    if sComboBox1.ItemIndex = iEMBEDDED then
      Grid.Font.Charset := DEFAULT_CHARSET
    else
      Grid.Font.Charset := arCharSets[sComboBox1.ItemIndex];

    Grid.Font.Name := sComboBox2.Text;
    FillList;
  end;
end;


procedure TFormCharMap.sSkinProvider1SkinItemEx(Item: TComponent; var CanBeAdded: Boolean; SkinParams: PacSkinParams);
begin
  if Item = Grid then begin
    SkinParams.VertScrollBtnSize := 0;
    SkinParams.VertScrollSize := acSpacing * 3;
    SkinParams.SkinSection := s_Transparent;
  end;
end;


procedure TFormCharMap.WndProc(var Message: TMessage);
begin
{$IFDEF LOGGED}
  AddToLog(Message);
{$ENDIF}
  inherited;
end;


type
  TAccessControl = class(TWinControl);


procedure TacCharGrid.ChangeScale(M, D: Integer{$IFDEF DELPHI_10TOKYO}; isDpiChange: Boolean{$ENDIF});
begin
  DefaultColWidth := MulDiv(DefaultColWidth, M, D);
  DefaultRowHeight := MulDiv(DefaultRowHeight, M, D);
  Width := MulDiv(Width, M, D);
  Height := MulDiv(Height, M, D);
end;


procedure TacCharGrid.InitCtrl;
begin
  Clicked := False;
  ColCount := iColCount;
  FixedCols := 0;
  FixedRows := 0;
  BorderStyle := bsNone;
  DefaultColWidth := iCellSize;
  DefaultRowHeight := iCellSize;
  ScrollBars := ssVertical;
  OnDrawCell := FormCharMap.DrawGrid1DrawCell;
  OnMouseMove := FormCharMap.DrawGrid1MouseMove;
  OnMouseDown := FormCharMap.DrawGrid1MouseDown;
  Options := Options - [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect] + [goThumbTracking];
  Align := alClient;
  HotCell := Point(-1, -1);
{$IFDEF DELPHI6UP}
  InvalidateGrid;
{$ELSE}
  Repaint;
{$ENDIF}
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


procedure TacCharGrid.WndProc(var Message: TMessage);
var
  X, Y: integer;
begin
{$IFDEF LOGGED}
//  AddToLog(Message);
{$ENDIF}
  case Message.Msg of
    WM_LBUTTONDOWN:
      if not Clicked then begin
        Clicked := True;
        MouseToCell(TWMMouse(Message).XPos, TWMMouse(Message).YPos, X, Y);
        if IsValidIndex(X, ColCount) and IsValidIndex(Y, RowCount) then begin
{$IFDEF DELPHI6UP}
          FocusCell(X, Y, False);
{$ELSE}
          TAccessStdGrid(Self).FCurrent.X := X;
          TAccessStdGrid(Self).FCurrent.Y := Y;
          Click;
{$ENDIF}
          OnMouseDown(Self, mbLeft, [], X, Y);
        end;
        Clicked := False;
      end;

    else inherited;
  end;
end;

end.
