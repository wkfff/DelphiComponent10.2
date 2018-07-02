unit CommonUnit;

interface

Uses SysUtils,Classes,Forms;

Type
  TBlinkThr=Class(TThread)
  private
    FParent:TObject;
    FActive: Boolean;
    FPreviusState:Boolean;
    FBlinkInterval: Integer;
    procedure SetActive(const Value: Boolean);
    procedure SetBlinkInterval(const Value: Integer);
  protected
    procedure Execute; override;
  public
    stop:Boolean;
    constructor Create(Parent:TObject); reintroduce;
    property Active:Boolean read FActive write SetActive default False;
    property BlinkInterval:Integer read FBlinkInterval write SetBlinkInterval default 600;
  end;

  TAstThr=Class(TThread)
  private
    FParent:TObject;
    FAstInterval: Integer;
    FStartTick: Cardinal;
    procedure SetAstInterval(const Value: Integer);
  protected
    procedure Execute; override;
  public
    constructor Create(Parent:TObject;StartTick:Cardinal); reintroduce;
    property AstInterval:Integer read FAstInterval write SetAstInterval default 500;
  end;

implementation

Uses ButtonLed,Lampada,Selettore,ButtonFunction;

{ TBlinkThr }

constructor TBlinkThr.Create(Parent:TObject);
begin
FBlinkInterval:=600;
FActive:=False;
FParent:=Parent;
inherited Create(False);
end;

procedure TBlinkThr.Execute;
Var i:integer;
begin
stop:=False;
while (not stop) OR Terminated OR (Application.Terminated) do
      begin
       i:=0;
       while i<FBlinkInterval do
             begin
              sleep(100);
              if stop then exit;
              inc(i,100);
             end;
       if FActive then
          begin
           if (FParent) is TButtonLed then
              begin
               if (bsON in TButtonLed(FParent).ButtonState) then TButtonLed(FParent).ButtonState:=TButtonLed(FParent).ButtonState-[bsON]
                                                            else TButtonLed(FParent).ButtonState:=TButtonLed(FParent).ButtonState+[bsON];
              end;
           if (FParent) is TLampada then TLampada(FParent).Selected:=NOT TLampada(FParent).Selected;
           if (FParent) is TSelettore then TSelettore(FParent).Light:=NOT TSelettore(FParent).Light;
          end;
      end;
end;

procedure TBlinkThr.SetActive(const Value: Boolean);
begin
if Value then
   begin
    if (FParent) is TButtonLed then FPreviusState:=(bsON in TButtonLed(FParent).ButtonState);
    if (FParent) is TLampada then FPreviusState:=TLampada(FParent).Selected;
    if (FParent) is TSelettore then FPreviusState:=TSelettore(FParent).Light;
   end
else
   begin
    if (FParent) is TButtonLed then
       begin
        if FPreviusState then TButtonLed(FParent).ButtonState:=TButtonLed(FParent).ButtonState+[bsON]
                         else TButtonLed(FParent).ButtonState:=TButtonLed(FParent).ButtonState-[bsON]
       end;
    if (FParent) is TLampada then TLampada(FParent).Selected:=FPreviusState;
    if (FParent) is TSelettore then TSelettore(FParent).Light:=FPreviusState;
   end;
FActive:=Value;
end;

procedure TBlinkThr.SetBlinkInterval(const Value: Integer);
begin
if (value<100) or (value>2000) then exit;
FBlinkInterval:=Value;
end;

{ TAstThr }

constructor TAstThr.Create(Parent: TObject;StartTick:Cardinal);
begin
FAstInterval:=500;
FStartTick:=StartTick;
FParent:=Parent;
inherited Create(False);
end;

procedure TAstThr.Execute;
begin
dec(FAstInterval,FStartTick);
while FAstInterval>0 do
      begin
       dec(FAstInterval,20);
       sleep(20);
      end;
if (FParent is TButtonLed) then TButtonLed(FParent).ButtonState:=TButtonLed(FParent).ButtonState-[bsPressed];
if (FParent is TButtonFunction) then  TButtonFunction(FParent).Selected:=False;
end;

procedure TAstThr.SetAstInterval(const Value: Integer);
begin
FAstInterval:=Value;
end;

end.
