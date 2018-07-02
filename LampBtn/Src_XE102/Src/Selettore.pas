unit Selettore;

interface

uses
  Messages, SysUtils, Classes, Controls, ExtCtrls, PngImage, Graphics, Lampada,
  types, CommonUnit;

type
  TSelType=(s2pos,s3pos);
  TSelColor=(scBlue,scGreen,scYellow,scOrange,scRed);

  TSelettore = class(TGraphicControl)
  private
   Fx:Integer;
   {$IF CompilerVersion >= 21}
   FPng:Array[TSelType,TSelColor,byte,boolean,boolean] of TPngImage;
   {$ELSE}
   FPng:Array[TSelType,TSelColor,byte,boolean,boolean] of TPngObject;
   {$IFEND}
   FSelType: TSelType;
   FLight: Boolean;
   FPosition: Byte;
   FSelColor: TSelColor;
   FLightedOnON: Boolean;
   FCaption: TCaption;
   FLampPOS1: TLampada;
   FLampPOS0: TLampada;
   FLampPOS2: TLampada;
   FBlink: Boolean;
   FBlinkSpeed: Cardinal;
   FBlinkThr: TBlinkThr;
   FOnChange: TNotifyEvent;
   procedure SetLamp;
   procedure SetSelType(const Value: TSelType);
   procedure SetLight(const Value: Boolean);
   procedure SetPosition(const Value: Byte);
   procedure SetSelColor(const Value: TSelColor);
   Procedure LoadPNG;
   procedure SetLightedOnON(const Value: Boolean);
   procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
   procedure CMHITTEST(var Message: TMessage); message CM_HITTEST;
   procedure SetCaption(const Value: TCaption);
   procedure SetBlink(const Value: Boolean);
   procedure SetBlinkSpeed(const Value: Cardinal);
   procedure SetOnChange(const Value: TNotifyEvent);
  protected
   procedure Paint; Override;
   procedure Click; Override;
  public
   Constructor Create(AOwner: TComponent); Override;
   Destructor Destroy; override;
  published
   property Align;
   property Height;
   property Width;
   property Font;
   property ParentFont;
   property SelColor:TSelColor read FSelColor write SetSelColor default scBlue;
   property SelType:TSelType read FSelType write SetSelType default s2pos;
   property Position:Byte read FPosition write SetPosition default 0;
   property Light:Boolean read FLight write SetLight default False;
   property LightedOnON:Boolean read FLightedOnON write SetLightedOnON default False;
   property Caption:TCaption read FCaption write SetCaption;
   property LampPOS0:TLampada read FLampPOS0 write FLampPOS0;
   property LampPOS1:TLampada read FLampPOS1 write FLampPOS1;
   property LampPOS2:TLampada read FLampPOS2 write FLampPOS2;
   property Blink:Boolean read FBlink write SetBlink;
   property BlinkSpeed:Cardinal read FBlinkSpeed write SetBlinkSpeed default 700;
   property OnClick;
   property OnChange:TNotifyEvent read FOnChange write SetOnChange;
   property Hint;
   property ShowHint;
   property PopUpMenu;
   property Enabled;
  end;

procedure Register;

{$R Selettore2.res}
{$R Selettore3.res}

implementation

procedure Register;
begin
RegisterComponents('Termo', [TSelettore]);
end;

{ TSelettore }

constructor TSelettore.Create(AOwner: TComponent);
Var st:TSelType; sc:TSelColor; Pos:Byte;
begin
inherited;
{$IF CompilerVersion >= 21}
for st:=Low(TSelType) to High(TSelType) do
    for sc:=Low(TSelColor) to High(TSelColor) do
        for pos:=0 to 2 do
            begin
             FPng[st,sc,pos,False,True]:=TPngImage.Create;
             FPng[st,sc,pos,True,True]:=TPngImage.Create;
             FPng[st,sc,pos,False,False]:=TPngImage.Create; //Disabled
            end;
{$ELSE}
for st:=Low(TSelType) to High(TSelType) do
    for sc:=Low(TSelColor) to High(TSelColor) do
        for pos:=0 to 2 do
            begin
             FPng[st,sc,pos,False,True]:=TPngObject.Create;
             FPng[st,sc,pos,True,True]:=TPngObject.Create;
             FPng[st,sc,pos,False,False]:=TPngObject.Create; //Disabled
            end;
{$IFEND}
Height:=90;
Width:=90;
FSelType:=s2pos;
FSelColor:=scBlue;
FPosition:=0;
FLight:=False;
FLightedOnON:=False;
try LoadPNG; finally end;
FBlinkSpeed:=700;
FBlinkThr:=TBlinkThr.Create(Self);
end;

destructor TSelettore.Destroy;
Var st:TSelType; sc:TSelColor; Pos:Byte;
begin
for st:=Low(TSelType) to High(TSelType) do
    for sc:=Low(TSelColor) to High(TSelColor) do
        for pos:=0 to 2 do
            begin
             FPng[st,sc,pos,False,True].destroy;
             FPng[st,sc,pos,True,True].destroy;
             FPng[st,sc,pos,False,False].destroy;
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

procedure TSelettore.LoadPNG;
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
Load(FPng[s2pos,scBlue,0,False,False],'2DISABLED');
Load(FPng[s3pos,scBlue,0,False,False],'3DISABLED');

Load(FPng[s2pos,scBlue,0,False,True],'2BLU0');
Load(FPng[s2pos,scBlue,0,True,True],'2BLU0L');
Load(FPng[s2pos,scBlue,1,False,True],'2BLU1');
Load(FPng[s2pos,scBlue,1,True,True],'2BLU1L');

Load(FPng[s3pos,scBlue,0,False,True],'3BLU0');
Load(FPng[s3pos,scBlue,0,True,True],'3BLU0L');
Load(FPng[s3pos,scBlue,1,False,True],'3BLU1');
Load(FPng[s3pos,scBlue,1,True,True],'3BLU1L');
Load(FPng[s3pos,scBlue,2,False,True],'3BLU2');
Load(FPng[s3pos,scBlue,2,True,True],'3BLU2L');


Load(FPng[s2pos,scGreen,0,False,True],'2GREEN0');
Load(FPng[s2pos,scGreen,0,True,True],'2GREEN0L');
Load(FPng[s2pos,scGreen,1,False,True],'2GREEN1');
Load(FPng[s2pos,scGreen,1,True,True],'2GREEN1L');

Load(FPng[s3pos,scGreen,0,False,True],'3GREEN0');
Load(FPng[s3pos,scGreen,0,True,True],'3GREEN0L');
Load(FPng[s3pos,scGreen,1,False,True],'3GREEN1');
Load(FPng[s3pos,scGreen,1,True,True],'3GREEN1L');
Load(FPng[s3pos,scGreen,2,False,True],'3GREEN2');
Load(FPng[s3pos,scGreen,2,True,True],'3GREEN2L');

Load(FPng[s2pos,scYellow,0,False,True],'2YELLOW0');
Load(FPng[s2pos,scYellow,0,True,True],'2YELLOW0L');
Load(FPng[s2pos,scYellow,1,False,True],'2YELLOW1');
Load(FPng[s2pos,scYellow,1,True,True],'2YELLOW1L');

Load(FPng[s3pos,scYellow,0,False,True],'3YELLOW0');
Load(FPng[s3pos,scYellow,0,True,True],'3YELLOW0L');
Load(FPng[s3pos,scYellow,1,False,True],'3YELLOW1');
Load(FPng[s3pos,scYellow,1,True,True],'3YELLOW1L');
Load(FPng[s3pos,scYellow,2,False,True],'3YELLOW2');
Load(FPng[s3pos,scYellow,2,True,True],'3YELLOW2L');

Load(FPng[s2pos,scOrange,0,False,True],'2ORANGE0');
Load(FPng[s2pos,scOrange,0,True,True],'2ORANGE0L');
Load(FPng[s2pos,scOrange,1,False,True],'2ORANGE1');
Load(FPng[s2pos,scOrange,1,True,True],'2ORANGE1L');

Load(FPng[s3pos,scOrange,0,False,True],'3ORANGE0');
Load(FPng[s3pos,scOrange,0,True,True],'3ORANGE0L');
Load(FPng[s3pos,scOrange,1,False,True],'3ORANGE1');
Load(FPng[s3pos,scOrange,1,True,True],'3ORANGE1L');
Load(FPng[s3pos,scOrange,2,False,True],'3ORANGE2');
Load(FPng[s3pos,scOrange,2,True,True],'3ORANGE2L');

Load(FPng[s2pos,scRed,0,False,True],'2RED0');
Load(FPng[s2pos,scRed,0,True,True],'2RED0L');
Load(FPng[s2pos,scRed,1,False,True],'2RED1');
Load(FPng[s2pos,scRed,1,True,True],'2RED1L');

Load(FPng[s3pos,scRed,0,False,True],'3RED0');
Load(FPng[s3pos,scRed,0,True,True],'3RED0L');
Load(FPng[s3pos,scRed,1,False,True],'3RED1');
Load(FPng[s3pos,scRed,1,True,True],'3RED1L');
Load(FPng[s3pos,scRed,2,False,True],'3RED2');
Load(FPng[s3pos,scRed,2,True,True],'3RED2L');
end;

procedure TSelettore.Paint;
var tw:Integer;
begin
Canvas.Font.Assign(Font);
with canvas do
     begin
      lock;
      if Enabled then
         StretchDraw(Rect(0,0,Width,Height),FPNG[FSelType,FSelColor,FPosition,FLight,True])
      else
         StretchDraw(Rect(0,0,Width,Height),FPNG[FSelType,scBlue,0,False,False]);
      brush.Style:=bsClear;
      tw:=TextWidth(FCaption);
      textOut((width-tw) DIV 2,0,FCaption);
      unlock;
     end;
end;

procedure TSelettore.SetCaption(const Value: TCaption);
begin
FCaption:=Value;
Invalidate;
end;

procedure TSelettore.SetLight(const Value: Boolean);
begin
if Value=FLight then exit;
FLight:=Value;
Invalidate;
end;

procedure TSelettore.SetLightedOnON(const Value: Boolean);
begin
if Value=FLightedOnON then exit;
FLightedOnON:=Value;
Invalidate;
end;

procedure TSelettore.SetPosition(const Value: Byte);
Var p:byte;
begin
p:=FPosition;
if FPosition=Value then exit;
FPosition:=Value;
case FSelType of
 s2pos:if FPosition>1 then FPosition:=1;
 s3pos:if FPosition>2 then FPosition:=2;
end;
if p<>FPosition then if Assigned(OnChange) then OnChange(self);
SetLamp;
Invalidate;
end;

procedure TSelettore.SetSelColor(const Value: TSelColor);
begin
if Value=FSelColor then exit;
FSelColor:=Value;
Invalidate;
end;

procedure TSelettore.SetSelType(const Value: TSelType);
begin
FSelType:=Value;
SetPosition(FPosition); //Se cambio tipo e la posizione non è ammissibile viene corretta.
end;

procedure TSelettore.CMEnabledChanged(var Message: TMessage);
begin
inherited;
end;

procedure TSelettore.SetLamp;
begin
if Assigned(FLampPOS0) then FLampPOS0.Selected:=FPosition=0;
if Assigned(FLampPOS1) then FLampPOS1.Selected:=FPosition=1;
if Assigned(FLampPOS2) then FLampPOS2.Selected:=FPosition=2;
end;

procedure TSelettore.CMHITTEST(var Message: TMessage);
begin
inherited;
Fx:=TWMNCHitTest(Message).XPos;
end;

procedure TSelettore.Click;
Var W3:Integer;
begin
W3:=Width DIV 3;
case FSelType of
 s2pos:begin
        if Fx<2*W3 then Position:=0
                   else Position:=1;
       end;
 s3pos:begin
        if Fx<W3 then Position:=1
        else if Fx<2*W3 then Position:=0
                        else Position:=2;
       end;
end;
if Assigned(OnClick) then Onclick(Self);
end;

procedure TSelettore.SetBlink(const Value: Boolean);
begin
FBlink:=Value;
if FBlinkThr<>NIL then FBlinkThr.Active:=FBlink;
end;

procedure TSelettore.SetBlinkSpeed(const Value: Cardinal);
begin
FBlinkSpeed:=Value;
if FBlinkThr<>NIL then FBlinkThr.BlinkInterval:=FBlinkSpeed;
end;

procedure TSelettore.SetOnChange(const Value: TNotifyEvent);
begin
FOnChange:=Value;
end;

end.
