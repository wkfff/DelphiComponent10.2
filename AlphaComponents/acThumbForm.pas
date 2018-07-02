unit acThumbForm;
{$WARNINGS OFF}
{$i sDefs.inc}

interface

uses
  {$IFDEF DELPHI6UP} Variants, {$ENDIF}
  {$IFNDEF DELPHI5}Types,{$ENDIF}
  Windows, Messages, SysUtils, Classes, Controls, Forms;


const
  sMagnificationDll = 'Magnification.dll';


type
  TMagTransform = packed record
    v: packed array [0..2, 0..2] of Single;
  end;

  THWNDArray = packed array [0..1] of HWND;
  PHWNDArray = ^THWNDArray;


type
  TMagnifierOwner = class;
  TMagnifierWindow = class
  private
    FTop,
    FLeft,
    FWidth,
    FHeight: Integer;
    FHandle: HWND;
    FMagFactor: Byte;
    FVisible: Boolean;
    FWndStyle: Cardinal;
    FParent: TMagnifierOwner;
    procedure SetMagFactor(const Value: Byte);
    procedure SetVisible(const Value: Boolean);
  public
    PosUpdating: boolean;
    property Handle: HWND read FHandle;
    property MagFactor: Byte read FMagFactor write SetMagFactor;
    property Visible: Boolean read FVisible write SetVisible;
    constructor Create(Parent: TMagnifierOwner);
    destructor Destroy; override;
    function BlackWnd: boolean;
    procedure Refresh;
    procedure UpdateSource;
  end;


  TMagnifierOwner = class(TForm)
    procedure WMMove(var m: TMessage); message WM_MOVE;
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormDblClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormActivate(Sender: TObject);
  public
    cL,
    cT,
    cR,
    cB,
    inds: integer;
    Dragging: boolean;
    ParentForm: TForm;
    MagnWnd: TMagnifierWindow;
    procedure UpdateRgn;
    procedure UpdateParentPos;
    procedure CreateSysMagnifier;
    destructor Destroy; override;
    procedure UpdatePosition(Full: boolean = True);
    constructor Create(AOwner: TComponent); override;
    procedure CreateParams(var Params: TCreateParams); override;
  end;


var
  MagnifierOwner: TMagnifierOwner;
  acMagInitialize:          function: BOOL; stdcall;
  acMagUninitialize:        function: BOOL; stdcall;
  acMagSetWindowSource:     function (hwnd: HWND; rect: TRect): BOOL; stdcall;
  acMagSetWindowTransform:  function (hwnd: HWND; out Transform: TMagTransform): BOOL; stdcall;
  acMagSetWindowFilterList: function (hwnd: HWND; dwFilterMode: DWORD; count: Integer; pHWND: PHWNDArray): BOOL; stdcall;


implementation

uses
  math, Graphics,
  sConst, sSkinProvider, acMagn, sGraphUtils, acntUtils;

{$R *.dfm}

const
  WC_MAGNIFIER = 'Magnifier';


constructor TMagnifierOwner.Create(AOwner: TComponent);
begin
  inherited;
  ParentForm := TForm(AOwner);
  with TacMagnForm(AOwner).ContentMargins do begin
    cL := Left;
    cR := Right;
    cT := Top;
    cB := Bottom;
  end;

  Left := ParentForm.Left + cL;
  Top  := ParentForm.Top  + cT;
  Width  := TacMagnForm(AOwner).MagnSize.cx - cL - cR;
  Height := TacMagnForm(AOwner).MagnSize.cy - cT - cB;

  CreateSysMagnifier;
  if TacMagnForm(AOwner).Caller.ShowPopupMenu then
    PopupMenu := TacMagnForm(AOwner).PopupMenu;

  OnKeyUp := TacMagnForm(AOwner).Caller.OnKeyUp;
  OnKeyDown := TacMagnForm(AOwner).Caller.OnKeyDown;
end;


procedure TMagnifierOwner.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.ExStyle := Params.ExStyle or WS_EX_TOOLWINDOW or WS_EX_NOACTIVATE or WS_EX_TRANSPARENT or WS_EX_TOPMOST;
end;


procedure TMagnifierOwner.CreateSysMagnifier;
begin
  if HandleAllocated then begin
    MagnWnd := TMagnifierWindow.Create(Self);
    MagnWnd.MagFactor := TacMagnForm(ParentForm).Caller.Scaling;
    UpdatePosition(True);
  end;
end;


destructor TMagnifierOwner.Destroy;
begin
  inherited;
  FreeAndNil(MagnWnd);
end;


procedure TMagnifierOwner.WMMove(var m: TMessage);
begin
  if not Application.Terminated then begin
    if Assigned(MagnWnd) and not MagnWnd.PosUpdating then
      MagnWnd.UpdateSource;

    UpdateParentPos;
  end;
end;


constructor TMagnifierWindow.Create(Parent: TMagnifierOwner);
begin
  if Assigned(acMagInitialize) then begin
    if not acMagInitialize then
      {$IFDEF DELPHI6UP} RaiseLastOSError{$ENDIF};

    FVisible := False;
    PosUpdating := False;
    FParent := Parent;
    FLeft := 0;
    FTop := 0;

    FWidth := Parent.ClientWidth;
    FHeight := Parent.ClientHeight;
    SetLayeredWindowAttributes(Handle, 0, MaxByte, ULW_ALPHA);
    FWndStyle := WS_CHILD or WS_VISIBLE or WS_CLIPSIBLINGS;
    FHandle := CreateWindow(WC_MAGNIFIER, 'acMagWnd', FWndStyle, FLeft, FTop, FWidth, FHeight, FParent.Handle, 0, HInstance, nil);
    if FHandle = 0 then
      {$IFDEF DELPHI6UP} RaiseLastOSError{$ENDIF};
  end;
end;


destructor TMagnifierWindow.Destroy;
begin
  DestroyWindow(FHandle);
  if not acMagUninitialize then
    {$IFDEF DELPHI6UP} RaiseLastOSError{$ENDIF};

  inherited;
end;


procedure TMagnifierWindow.SetMagFactor(const Value: Byte);
var
  matrix: TMagTransform;
begin
  FMagFactor := Value;
  FillMemory(@matrix, SizeOf(matrix), 0);
  matrix.v[0][0] := FMagFactor;
  matrix.v[1][1] := FMagFactor;
  matrix.v[2][2] := 1;
  if not acMagSetWindowTransform(FHandle, matrix) then
    {$IFDEF DELPHI6UP} RaiseLastOSError{$ENDIF};
end;


procedure TMagnifierWindow.SetVisible(const Value: Boolean);
const
  ShowCmd: array[Boolean] of Cardinal = (SW_HIDE, SW_SHOW);
begin
  FVisible := Value;
  ShowWindow(FHandle, ShowCmd[FVisible]);
  if FVisible then
    UpdateSource;
end;


function TMagnifierWindow.BlackWnd: boolean;
var
  DC: hdc;
  R: TRect;
  p: TPoint;
begin
  DC := GetDC(0);
  try
    GetWindowRect(FHandle, R);
    p := Point(R.Left + WidthOf(R) div 2, R.Top + HeightOf(R) div 2);
    Result := (GetPixel(DC, p.X, p.Y) = 0) and (GetPixel(DC, p.X + 10, p.Y) = 0) and (GetPixel(DC, p.X, p.Y + 10) = 0);
  finally
    ReleaseDC(0, DC);
  end;
end;


procedure TMagnifierWindow.Refresh;
{$IFDEF DELPHI6UP}
var
  R: TRect;
  Monitor: TMonitor;
{$ENDIF}
begin
{$IFDEF DELPHI6UP}
  Monitor := Screen.MonitorFromWindow(FHandle);
  GetWindowRect(FHandle, R);
  if not PosUpdating and ((Monitor.Left > R.Left) or (Monitor.Top > R.Top) or (Monitor.Left + Monitor.Width < R.Right) or (Monitor.Top + Monitor.Height < R.Bottom) {or BlackWnd}) then
    UpdateSource
  else
{$ENDIF}
    InvalidateRect(FHandle, nil, True);
end;


procedure TMagnifierWindow.UpdateSource;
var
  p: TPoint;
  SourceRect: TRect;
  warray: THWNDArray;
begin
  if FParent.ParentForm <> nil then
    if not TacMagnForm(FParent.ParentForm).Closing then
      if not PosUpdating then begin
        PosUpdating := True;
        SourceRect := Rect(FLeft, FTop, FLeft + FWidth, FTop + FHeight);
        InflateRect(SourceRect, ((FWidth div FMagFactor) - FWidth) div 2, ((FHeight div FMagFactor) - FHeight) div 2);
        SourceRect.TopLeft := FParent.ClientToScreen(SourceRect.TopLeft);
        SourceRect.BottomRight := FParent.ClientToScreen(SourceRect.BottomRight);
        if Assigned(TacMagnForm(FParent.ParentForm).Caller.OnGetSourceCoords) then begin
          p := SourceRect.TopLeft;
          TacMagnForm(FParent.ParentForm).Caller.OnGetSourceCoords(p);
          SourceRect := Rect(p.x, p.y, WidthOf(SourceRect), HeightOf(SourceRect));
        end;
        SetWindowPos(FHandle, HWND_TOP, 0, 0, FWidth, FHeight, SWP_NOREDRAW or SWP_NOMOVE or SWP_NOACTIVATE or SWP_NOREDRAW or SWP_NOSENDCHANGING or SWP_NOOWNERZORDER or SWP_NOZORDER);
        warray[0] := FParent.ParentForm.Handle;
        warray[1] := FParent.Handle;
        acMagSetWindowFilterList(FHandle, 0, 2, @warray);
        if not acMagSetWindowSource(FHandle, SourceRect) then
          {$IFDEF DELPHI6UP} RaiseLastOSError{$ENDIF};

        Visible := True;
        Refresh;
        PosUpdating := False;
      end;
end;


procedure TMagnifierOwner.FormActivate(Sender: TObject);
begin
  UpdatePosition(True);
end;


procedure TMagnifierOwner.FormDblClick(Sender: TObject);
begin
  if Assigned(TacMagnForm(ParentForm).Caller.OnDblClick) then
    TsMagnifier(TacMagnForm(ParentForm).Caller).OnDblClick(TacMagnForm(ParentForm).Caller);
end;


procedure TMagnifierOwner.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = 27) or (ssAlt in Shift) then
    if ParentForm <> nil then
      TacMagnForm(ParentForm).Close;
end;


procedure TMagnifierOwner.FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ParentForm.OnMouseDown(Sender, Button, Shift, X, Y);
end;


procedure TMagnifierOwner.FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  ParentForm.OnMouseMove(Sender, Shift, X, Y);
end;


procedure TMagnifierOwner.FormMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ParentForm.OnMouseUp(Sender, Button, Shift, X, Y);
end;


procedure TMagnifierOwner.UpdateParentPos;
begin
  if ParentForm <> nil then
    if not TacMagnForm(ParentForm).Closing and (GetWindowLong(Handle, GWL_STYLE) and WS_VISIBLE <> 0) and not TacMagnForm(ParentForm).PosUpdating then begin
      TacMagnForm(ParentForm).PosUpdating := True;
      ParentForm.SetBounds(Left - TacMagnForm(ParentForm).ContentMargins.Left, Top - TacMagnForm(ParentForm).ContentMargins.Top, TacMagnForm(ParentForm).Width, TacMagnForm(ParentForm).Height);
      SetWindowPos(Handle, ParentForm.Handle, 0, 0, 0, 0, SWPA_SHOWZORDERONLY);
      TacMagnForm(ParentForm).PosUpdating := False;
    end;
end;


procedure TMagnifierOwner.UpdatePosition(Full: boolean = True);
const
  TestOffset = 0;
begin
  if ParentForm <> nil then
    if not TacMagnForm(ParentForm).Closing then
      if Full then begin
        InitDwm(Handle, True);
        with MagnWnd do begin
          FWidth  := min(max(TacMagnForm(ParentForm).MinSize, ParentForm.Width),  amMaxSize) - cL - cR;
          FHeight := min(max(TacMagnForm(ParentForm).MinSize, ParentForm.Height), amMaxSize) - cT - cB;
          if TacMagnForm(ParentForm).AeroUpdating then
            inc(FHeight, inds);

          inds := integer(not boolean(inds));
        end;
        UpdateRgn;
        SetWindowPos(Handle, ParentForm.Handle, ParentForm.Left + cL + TestOffset, ParentForm.Top + cT, MagnWnd.FWidth, MagnWnd.FHeight, SWP_NOACTIVATE or SWP_NOREDRAW or SWP_SHOWWINDOW or SWP_NOSENDCHANGING or SWP_NOOWNERZORDER);
        if not TacMagnForm(ParentForm).PosUpdating then
          MagnWnd.UpdateSource;
      end
      else
        SetWindowPos(Handle, 0, ParentForm.Left + cL + TestOffset, ParentForm.Top + cT, 0, 0, SWP_NOSIZE or SWP_NOACTIVATE or SWP_NOREDRAW or SWP_NOZORDER or SWP_NOSENDCHANGING or SWP_NOOWNERZORDER);
end;


procedure TMagnifierOwner.UpdateRgn;
begin
  if TacMagnForm(ParentForm).Caller.Style = amsLens then
    SetWindowRgn(Handle, CreateRoundRectRgn(0, 0, Width, Height, 262, 262), False);
end;

end.
