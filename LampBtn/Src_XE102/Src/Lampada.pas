unit Lampada;

interface

uses
  Windows, SysUtils, Classes, Controls, ExtCtrls, ButtonLed, PngImage, graphics,
  CommonUnit;

type
  TLampada = class(TGraphicControl)
  private
   {$IF CompilerVersion >= 21}
   FPng:Array[TLedColor,Boolean] of TPngImage;
   {$ELSE}
   FPng:Array[TLedColor,Boolean] of TPngObject;
   {$IFEND}
   FLedColor: TLedColor;
   FSelected: Boolean;
   FBlink: Boolean;
   FBlinkSpeed: Cardinal;
   FBlinkThr: TBlinkThr;
   FOnON: TNotifyEvent;
   FOnOFF: TNotifyEvent;
   FLongText: Boolean;
   Procedure LoadPNG;
   procedure SetLedColor(const Value: TLedColor);
   procedure SetSelected(const Value: Boolean);
   procedure SetBlink(const Value: Boolean);
   procedure SetBlinkSpeed(const Value: Cardinal);
   procedure SetOnON(const Value: TNotifyEvent);
   procedure SetOnOFF(const Value: TNotifyEvent);
   procedure SetLongText(const Value: Boolean);
  protected
   procedure Paint; Override;
  public
   Constructor Create(AOwner: TComponent); Override;
   Destructor Destroy; override;
  published
   property Align;
   property Height;
   property Width;
   property Caption;
   property Font;
   property ParentFont;   
   property OnClick;
   property ShowHint;
   property Hint;
   property Enabled;
   property Visible;
   property LedColor:TLedColor read FLedColor write SetLedColor default lcBlue;
   property Selected:Boolean read FSelected write SetSelected default False;
   property Blink:Boolean read FBlink write SetBlink;
   property BlinkSpeed:Cardinal read FBlinkSpeed write SetBlinkSpeed default 700;
   property OnON:TNotifyEvent read FOnON write SetOnON;
   property OnOFF:TNotifyEvent read FOnOFF write SetOnOFF;
   property LongText:Boolean read FLongText write SetLongText;
  end;

procedure Register;

{$R Lampada.res}

implementation

procedure Register;
begin
  RegisterComponents('Termo', [TLampada]);
end;

{ TLampada }

constructor TLampada.Create(AOwner: TComponent);
Var lc:TLedColor;
begin
inherited;
{$IF CompilerVersion >= 21}
for lc:=Low(TLedColor) to High(TLedColor) do
    begin
     FPng[lc,False]:=TPngImage.Create;
     FPng[lc,True]:=TPngImage.Create;
    end;
{$ELSE}
for lc:=Low(TLedColor) to High(TLedColor) do
    begin
     FPng[lc,False]:=TPngObject.Create;
     FPng[lc,True]:=TPngObject.Create;
    end;
{$IFEND}
Height:=90;
Width:=90;
FLedColor:=lcBlue;
FBlinkSpeed:=700;
try LoadPNG; finally end;
FBlinkThr:=TBlinkThr.Create(Self);
end;

destructor TLampada.Destroy;
Var lc:TLedColor;
begin
for lc:=Low(TLedColor) to High(TLedColor) do
    begin
     FPng[lc,False].Destroy;
     FPng[lc,True].Destroy;
    end;
if FBlinkThr<>NIL then
   begin
    FBlinkThr.stop:=True;
    FBlinkThr.Terminate;
   end;
sleep(100); 
try FBlinkThr.Destroy; except end;
inherited;
end;

procedure TLampada.LoadPNG;
 {$IF CompilerVersion >= 21}
 procedure Load(CONST PNG:TPngImage;S:String);
 {$ELSE}
 procedure Load(CONST PNG:TPngObject;S:String);
 {$IFEND}
 VAR Stream: TCustomMemoryStream;
 begin
  try
   Stream:=TResourceStream.Create(hInstance,PChar(s),PChar('PNG'));
   PNG.LoadFromStream(Stream);
  finally
   Stream.Destroy;
  end;
 end;
begin
Load(FPNG[lcBlue,False],'BLU');
Load(FPNG[lcBlue,True],'BLUL');
Load(FPNG[lcGreen,False],'GREEN');
Load(FPNG[lcGreen,True],'GREENL');
Load(FPNG[lcYellow,False],'YELLOW');
Load(FPNG[lcYellow,True],'YELLOWL');
Load(FPNG[lcOrange,False],'ORANGE');
Load(FPNG[lcOrange,True],'ORANGEL');
Load(FPNG[lcRed,False],'RED');
Load(FPNG[lcRed,True],'REDL');
end;

procedure TLampada.Paint;
var TFontFlags:LongInt; rc:Trect;
begin
canvas.Font:=Font;
TFontFlags:=DT_CENTER+DT_TOP;
with canvas do
     begin
      lock;
      if FLongText then
         begin
          StretchDraw(Rect(0,0,Width,Height-10),FPNG[FLedColor,FSelected]);
          StretchDraw(Rect(0,10,Width,Height),FPNG[FLedColor,FSelected]);
         end
      else
         StretchDraw(Rect(0,0,Width,Height),FPNG[FLedColor,FSelected]);
      brush.Style:=bsClear;
      rc:=Rect(0,0,Width,30);
      DrawText(Handle,PChar(Caption),Length(Caption),rc,TFontFlags+DT_CALCRECT);
      rc.Left:=(Width-rc.Right) DIV 2;
      inc(rc.Right,rc.Left);
      //DrawText(Handle,PChar(Caption+#0),Length(Caption+#0),rc,TFontFlags);7
      DrawText(Handle,PChar(Caption),Length(Caption),rc,TFontFlags);
      unlock;
     end;
end;

procedure TLampada.SetBlink(const Value: Boolean);
begin
FBlink:=Value;
if FBlinkThr<>NIL then FBlinkThr.Active:=FBlink;
end;

procedure TLampada.SetBlinkSpeed(const Value: Cardinal);
begin
FBlinkSpeed:=Value;
if FBlinkThr<>NIL then FBlinkThr.BlinkInterval:=FBlinkSpeed;
end;

procedure TLampada.SetLedColor(const Value: TLedColor);
begin
if Value=FLedColor then exit;
FLedColor:=Value;
Invalidate;
end;

procedure TLampada.SetLongText(const Value: Boolean);
begin
if Value=FLongText then exit;
FLongText:=Value;
Invalidate;
end;

procedure TLampada.SetOnOFF(const Value: TNotifyEvent);
begin
FOnOFF:=Value;
end;

procedure TLampada.SetOnON(const Value: TNotifyEvent);
begin
FOnON:=Value;
end;

procedure TLampada.SetSelected(const Value: Boolean);
begin
FSelected:=Value;
if (NOT FBlink) AND (FSelected) AND Assigned(FOnON) then FOnON(self);
if (NOT FBlink) AND (NOT FSelected) AND Assigned(FOnON) then FOnOFF(self);
Invalidate;
end;

end.
