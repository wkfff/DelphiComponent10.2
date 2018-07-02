unit Led;

interface

uses SysUtils, Messages, Classes, Types, Controls, ExtCtrls, Graphics;

type
  TLedColor=(lcBlu,lcRosso,lcVerde);

  TLed=class(TGraphicControl)
  private
    FBmp:Array[1..6] of TBitmap;
    FLedColor: TLedColor;
    FAcceso: Boolean;
    FCaption3D: Boolean;
    FShadowColor: TColor;
    FBmpIndex: byte;
    procedure SelectBitmap;
    procedure FontChange(Sender:Tobject);
    procedure SetLedColor(const Value: TLedColor);
    procedure SetAcceso(const Value: Boolean);
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure SetCaption3D(const Value: Boolean);
    procedure SetShadowColor(const Value: TColor);
  protected
    procedure Paint; Override;
  public
    Constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;
  published
    property Height;
    property Width;
    property OnClick;
    property ShowHint;
    property Hint;
    property Font;
    property Caption;
    property LedColor:TLedColor read FLedColor write SetLedColor Default lcBlu;
    property Acceso:Boolean read FAcceso write SetAcceso Default False;
    property Caption3D:Boolean read FCaption3D write SetCaption3D;
    property ShadowColor:TColor read FShadowColor write SetShadowColor;
  end;

procedure Register;

{$R Led.res}

implementation

procedure Register;
begin
RegisterComponents('Termo', [TLed]);
end;

{ TLed }

procedure TLed.CMTextChanged(var Message: TMessage);
begin
inherited;
Invalidate;
end;

constructor TLed.Create(AOwner: TComponent);
Var i:Integer;
begin
inherited;
Height:=16;
Width:=16;
for i:=low(FBmp) to High(FBmp) do
    begin
     FBmp[i]:=TBitmap.Create;
     FBmp[i].Transparent:=True;
     try
      FBmp[i].LoadFromResourceID(hInstance,i);
     finally
     end;
    end;
FLedColor:=lcBlu;
FAcceso:=False;
FBmpIndex:=2;
//Font.OnChange:=FontChange;
SelectBitmap;
end;

destructor TLed.Destroy;
Var i:integer;
begin
try
 for i:=low(FBmp) to High(FBmp) do
     begin
      if FBmp[i]<>NIL then FBmp[i].Destroy;
      FBmp[i]:=NIL;
     end;
finally
 inherited;
end;
end;

procedure TLed.FontChange(Sender: Tobject);
begin
//invalidate;
end;

procedure TLed.Paint;
var ts:TSize; c:TColor;
begin
canvas.Font:=Font;
with canvas do
     try
      lock;
      StretchDraw(Rect(0,0,Height,Height),FBmp[FBmpIndex]);
      ts:=TextExtent(caption);
      Width:=Height+ts.cx+3;
      brush.Style:=bsClear;
      if FCaption3D then
         begin
          c:=Font.Color; Font.Color:=FShadowColor;
          textOut(Height,(Height-ts.cy) DIV 2-1,caption);
          Font.Color:=c;
         end;
      textOut(Height+1,(Height-ts.cy) DIV 2,caption);
     finally
      unlock;
     end;
end;

procedure TLed.SelectBitmap;
begin
case FLedColor of
 lcBlu:if FAcceso then FBmpIndex:=1
                  else FBmpIndex:=2;
 lcRosso:if FAcceso then FBmpIndex:=3
                    else FBmpIndex:=4;
 lcVerde:if FAcceso then FBmpIndex:=5
                    else FBmpIndex:=6;
end;
end;

procedure TLed.SetAcceso(const Value: Boolean);
begin
if Value=FAcceso then exit;
FAcceso:=Value;
SelectBitmap;
Invalidate;
end;

procedure TLed.SetCaption3D(const Value: Boolean);
begin
if FCaption3D=Value then exit;
FCaption3D:=Value;
Invalidate;
end;

procedure TLed.SetLedColor(const Value: TLedColor);
begin
if Value=FLedColor then exit;
FLedColor:=Value;
SelectBitmap;
Invalidate;
end;

procedure TLed.SetShadowColor(const Value: TColor);
begin
if FShadowColor=Value then exit;
FShadowColor:=Value;
Invalidate;
end;

end.
