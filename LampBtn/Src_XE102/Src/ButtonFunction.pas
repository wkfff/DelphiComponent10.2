unit ButtonFunction;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, ExtCtrls, Graphics, PngImage,
  CommonUnit;

type
  TFuncImg=(fBlank,fUP,fDN,fSX,fDX,fUP2,fDN2,fX,fV,fPlus,fMinus);

  TButtonFunction = class(TGraphicControl)
  private
   {$IF CompilerVersion >= 21}
   FPng:TPngImage;
   {$ELSE}
   FPng:TPngObject;
   {$IFEND}
   FPressTime: Cardinal;
   FAstThr: TAstThr;
   FSelected: Boolean;
   FFuncImg: TFuncImg;
   FAstableTime: Cardinal;
   Procedure LoadPNG;
   procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
   procedure LBUTTONDOWN(Var Message:TWMLBUTTONDOWN); message WM_LBUTTONDOWN;
   procedure LBUTTONUP(Var Message:TWMLBUTTONUP); message WM_LBUTTONUP;
   procedure SetFuncImg(const Value: TFuncImg);
   procedure SetAstableTime(const Value: Cardinal);
   procedure SetSelected(const Value: Boolean);
  protected
   procedure Paint; Override;
  public
   Constructor Create(AOwner: TComponent); override;
   Destructor Destroy; override;
  published
   property Align;
   property Height;
   property Width;
   property OnClick;
   property ShowHint;
   property Hint;
   property Enabled;
   property Cursor default crHandPoint;
   property Selected: Boolean read FSelected write SetSelected;
   property FuncImg:TFuncImg read FFuncImg write SetFuncImg default fBlank;
   property AstableTime:Cardinal read FAstableTime write SetAstableTime default 300;
  end;

procedure Register;

{$R ButtonFunction.res}

implementation

procedure Register;
begin
  RegisterComponents('Termo', [TButtonFunction]);
end;

{ TButtonFunction }

constructor TButtonFunction.Create(AOwner: TComponent);
begin
inherited;
{$IF CompilerVersion >= 21}
FPng:=TPngImage.Create;
{$ELSE}
FPng:=TPngObject.Create;
{$IFEND}
Height:=60;
Width:=60;
Cursor:=crHandPoint;
FFuncImg:=fBlank;
FAstableTime:=300;
try LoadPNG; finally end;
end;

destructor TButtonFunction.Destroy;
begin
FPng.Destroy;
if FAstThr<>NIL then FAstThr.Destroy;
inherited;
end;

procedure TButtonFunction.CMEnabledChanged(var Message: TMessage);
begin
inherited;
try LoadPNG; finally end;
end;

procedure TButtonFunction.LoadPNG;
VAR Stream: TCustomMemoryStream;
    s:String;
begin
case FFuncImg of
 fBlank:s:='VUOTO';
 fUP:s:='SU';
 fDN:s:='GIU';
 fSX:s:='SX';
 fDX:s:='DX';
 fUP2:s:='SU2';
 fDN2:s:='GIU2';
 fX:s:='X';
 fV:s:='V';
 fPlus:s:='+';
 fMinus:s:='-';
end;
if FSelected then s:=s+'_P';
if NOT Enabled then s:='VUOTO';
Stream:=TResourceStream.Create(hInstance,PChar(S),PChar('PNG'));
FPng.LoadFromStream(Stream); Stream.Destroy;
end;

procedure TButtonFunction.Paint;
begin
with canvas do
     begin
      lock;
      StretchDraw(Rect(0,0,Width,Height),FPNG);
      unlock;
     end;
end;

procedure TButtonFunction.LBUTTONDOWN(var Message: TWMLBUTTONDOWN);
begin
Selected:=True;
FPressTime:=GetTickCount;
inherited;
end;

procedure TButtonFunction.LBUTTONUP(var Message: TWMLBUTTONUP);
var c:Cardinal;
begin
c:=GetTickCount-FPressTime;
if c<FAstableTime then
   begin
    FAstThr:=TAstThr.create(self,c);
    FAstThr.AstInterval:=FAstableTime;
   end
else Selected:=False;
inherited;
end;


procedure TButtonFunction.SetFuncImg(const Value: TFuncImg);
begin
FFuncImg:=Value;
try LoadPNG; finally end;
Invalidate;
end;

procedure TButtonFunction.SetAstableTime(const Value: Cardinal);
begin
FAstableTime:=Value;
end;

procedure TButtonFunction.SetSelected(const Value: Boolean);
begin
FSelected:=Value;
try LoadPNG; finally end;
Invalidate;
end;

end.
