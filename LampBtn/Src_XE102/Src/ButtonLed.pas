unit ButtonLed;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, ExtCtrls, Graphics, PngImage,
  CommonUnit;

CONST NComb=21;

type
  TLedColor=(lcBlue,lcGreen,lcYellow,lcOrange,lcRed);
  TButtState=(bsON,bsPressed);
  TButtonState=Set of TButtState;
  TButtonMode=(bmAstable,bmBistable);

  TButtonLed = class(TGraphicControl)
  private
   {$IF CompilerVersion >= 21} FPng:Array[0..NComb-1] of TPngImage;
   {$ELSE} FPng:Array[0..NComb-1] of TPngObject;
   {$IFEND}
   FButtonState: TButtonState;
   FLedColor: TLedColor;
   FCaption: TCaption;
   FLedOnOnPressed: Boolean;
   FBlink: Boolean;
   FBlinkSpeed: Cardinal;
   FBlinkThr: TBlinkThr;
   FButtonMode: TButtonMode;
   FAstableTime: Cardinal;
   FPressTime: Cardinal;
   FAstThr: TAstThr;
   FPngIndex: Byte;
   procedure SetButtonState(const Value: TButtonState);
   procedure SetLedColor(const Value: TLedColor);
   Procedure LoadPNG;
   Procedure SetPNG;
   procedure SetCaption(const Value: TCaption);
   procedure SetLedOnOnPressed(const Value: Boolean);
   procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
   procedure SetBlink(const Value: Boolean);
   procedure SetBlinkSpeed(const Value: Cardinal);
   procedure SetButtonMode(const Value: TButtonMode);
   procedure LBUTTONDOWN(Var Message:TWMLBUTTONDOWN); message WM_LBUTTONDOWN;
   procedure LBUTTONUP(Var Message:TWMLBUTTONUP); message WM_LBUTTONUP;
   procedure SetAstableTime(const Value: Cardinal);
  protected
   procedure Paint; Override;
   procedure Click; override;
  public
   Constructor Create(AOwner: TComponent); Override;
   Destructor Destroy; override;
  published
   property Align;
   property Height;
   property Width;
   property OnClick;
   property ShowHint;
   property Hint;
   property Enabled;
   property Font;
   property ParentFont;
   property Cursor default crHandPoint;
   property LedColor:TLedColor read FLedColor write SetLedColor default lcBlue;
   property ButtonState:TButtonState read FButtonState write SetButtonState;
   property Caption:TCaption read FCaption write SetCaption;
   property LedOnOnPressed:Boolean read FLedOnOnPressed write SetLedOnOnPressed;
   property Blink:Boolean read FBlink write SetBlink;
   property BlinkSpeed:Cardinal read FBlinkSpeed write SetBlinkSpeed default 700;
   property ButtonMode:TButtonMode read FButtonMode write SetButtonMode default bmAstable;
   property AstableTime:Cardinal read FAstableTime write SetAstableTime default 500;
  end;

procedure Register;

{$R ButtonLed.res}

implementation

procedure Register;
begin
RegisterComponents('Termo',[TButtonLed]);
end;

{ TButtonLed }

constructor TButtonLed.Create(AOwner: TComponent);
Var n:Integer;
begin
inherited;
{$IF CompilerVersion >= 21}
for n:=0 to NComb-1 do FPng[n]:=TPngImage.Create;
{$ELSE}
for n:=0 to NComb-1 do FPng[n]:=TPngObject.Create;
{$IFEND}
Height:=90;
Width:=90;
FLedColor:=lcBlue;
Cursor:=crHandPoint;
FBlinkSpeed:=700;
FButtonMode:=bmAstable;
FPngIndex:=0;
try LoadPNG; finally end;
SetPng;
FAstableTime:=500;
FBlinkThr:=TBlinkThr.Create(Self);
end;

procedure TButtonLed.CMEnabledChanged(var Message: TMessage);
begin
inherited;
try SetPNG; finally end;
end;

destructor TButtonLed.Destroy;
Var n:Integer;
begin
for n:=0 to NComb-1 do FPng[n].Destroy;
if FBlinkThr<>NIL then
   begin
    FBlinkThr.stop:=True;
    FBlinkThr.Terminate;
   end;
sleep(100);
try FBlinkThr.Destroy; except end;
if FAstThr<>NIL then
   begin
    FAstThr.Destroy;
    FAstThr.Terminate;
   end;
inherited;
end;

procedure TButtonLed.LoadPNG;
VAR Stream: TCustomMemoryStream; n:Integer;
    s:Array[0..NComb-1] of String;
begin
s[0]:='1-PBLULB'; s[1]:='1-PBLULA'; s[2]:='1-PBLUB'; s[3]:='1-PBLUA';
s[4]:='2-PGREENLB'; s[5]:='2-PGREENLA'; s[6]:='2-PGREENB'; s[7]:='2-PGREENA';
s[8]:='3-PYELLOWLB'; s[9]:='3-PYELLOWLA'; s[10]:='3-PYELLOWB'; s[11]:='3-PYELLOWA';
s[12]:='4-PORANGELB'; s[13]:='4-PORANGELA'; s[14]:='4-PORANGEB'; s[15]:='4-PORANGEA';
s[16]:='5-PREDLB'; s[17]:='5-PREDLA'; s[18]:='5-PREDB'; s[19]:='5-PREDA';
s[20]:='PDISABLED';
for n:=0 to NComb-1 do
    begin
     Stream:=TResourceStream.Create(hInstance,PChar(s[n]),PChar('PNG'));
     FPng[n].LoadFromStream(Stream);
     Stream.Destroy;
    end;
end;

procedure TButtonLed.SetPNG;
begin
if (FLedOnOnPressed) AND (NOT FBlink) then
   if (bsPressed in FButtonState) then FButtonState:=FButtonState+[bsON]
                                  else FButtonState:=FButtonState-[bsON];
case FLedColor of
 lcBlue:if (bsON in FButtonState) then
            Begin
             if (bsPressed in FButtonState) then FPngIndex:=0
                                            else FPngIndex:=1;
            end
         else
            if (bsPressed in FButtonState) then FPngIndex:=2
                                           else FPngIndex:=3;
 lcGreen:if (bsON in FButtonState) then
            Begin
             if (bsPressed in FButtonState) then FPngIndex:=4
                                            else FPngIndex:=5;
            end
         else
            if (bsPressed in FButtonState) then FPngIndex:=6
                                           else FPngIndex:=7;
 lcYellow:if (bsON in FButtonState) then
            Begin
             if (bsPressed in FButtonState) then FPngIndex:=8
                                            else FPngIndex:=9;
            end
         else
            if (bsPressed in FButtonState) then FPngIndex:=10
                                           else FPngIndex:=11;
 lcOrange:if (bsON in FButtonState) then
            Begin
             if (bsPressed in FButtonState) then FPngIndex:=12
                                            else FPngIndex:=13;
            end
         else
            if (bsPressed in FButtonState) then FPngIndex:=14
                                           else FPngIndex:=15;
 lcRed:if (bsON in FButtonState) then
            Begin
             if (bsPressed in FButtonState) then FPngIndex:=16
                                            else FPngIndex:=17;
            end
         else
            if (bsPressed in FButtonState) then FPngIndex:=18
                                           else FPngIndex:=19;
end;
if NOT Enabled then FPngIndex:=20;
end;

procedure TButtonLed.Paint;
var tw:Integer;
begin
Canvas.font.Assign(Font);
with canvas do
     begin
      lock;
      StretchDraw(Rect(0,0,Width,Height),FPNG[FPngIndex]);
      brush.Style:=bsClear;
      tw:=TextWidth(FCaption);
      textOut((width-tw) DIV 2,0,FCaption);
      unlock;
     end;
end;

procedure TButtonLed.SetBlink(const Value: Boolean);
begin
FBlink:=Value;
if FBlinkThr<>NIL then FBlinkThr.Active:=FBlink;
end;

procedure TButtonLed.SetBlinkSpeed(const Value: Cardinal);
begin
FBlinkSpeed:=Value;
if FBlinkThr<>NIL then FBlinkThr.BlinkInterval:=FBlinkSpeed;
end;

procedure TButtonLed.SetButtonState(const Value: TButtonState);
begin
if Value=FButtonState then exit;
FButtonState:=Value;
try SetPNG; finally end;
Invalidate;
end;

procedure TButtonLed.SetCaption(const Value: TCaption);
begin
FCaption:=Value;
Invalidate;
end;

procedure TButtonLed.SetLedColor(const Value: TLedColor);
begin
if Value=FLedColor then exit;
FLedColor:=Value;
try SetPNG; finally end;
Invalidate;
end;

procedure TButtonLed.SetLedOnOnPressed(const Value: Boolean);
begin
if Value=FLedOnOnPressed then exit;
FLedOnOnPressed:=Value;
try SetPNG; finally end;
Invalidate;
end;

procedure TButtonLed.SetButtonMode(const Value: TButtonMode);
begin
if Value=FButtonMode then exit;
FButtonMode:=Value;
case FButtonMode of
 bmAstable:ButtonState:=FButtonState-[bsPressed];
end;
end;

procedure TButtonLed.Click;
begin
case FButtonMode of
 bmAstable:;
 bmBistable:if (bsPressed in FButtonState) then ButtonState:=FButtonState-[bsPressed]
                                           else ButtonState:=FButtonState+[bsPressed];
end;
if Assigned(OnClick) then OnClick(Self);
end;

procedure TButtonLed.LBUTTONDOWN(var Message: TWMLBUTTONDOWN);
begin
case FButtonMode of
 bmAstable:ButtonState:=FButtonState+[bsPressed];
end;
FPressTime:=GetTickCount;
inherited;
end;

procedure TButtonLed.LBUTTONUP(var Message: TWMLBUTTONUP);
var c:Cardinal;
begin
case FButtonMode of
 bmAstable:begin
            c:=GetTickCount-FPressTime;
            if c<FAstableTime then begin FAstThr:=TAstThr.create(self,c); FAstThr.AstInterval:=FAstableTime; end 
                              else ButtonState:=FButtonState-[bsPressed];
           end;
end;
inherited;
end;

procedure TButtonLed.SetAstableTime(const Value: Cardinal);
begin
FAstableTime:=Value;
end;

end.
