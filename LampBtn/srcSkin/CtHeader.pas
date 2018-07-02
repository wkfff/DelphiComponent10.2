unit CtHeader;

interface

uses Windows, messages, SysUtils, stdctrls, Classes, Controls, ExtCtrls,
     PngImage, Graphics, TExDef, sLabel, sPanel, Led, Forms;

type

   TClockThr=class(TThread)
   public
    stop:Boolean;
    FParent:TCustomPanel;
    constructor Create(CONST Parent:TsPanel); reintroduce;
   protected
    procedure Execute; override;
    procedure ClockUpdate;
   end;

   TNFunc=0..19;

   TCtHeader=class(TsPanel)
   private
    FLedALTxt,FLedOpTxt,FLedSupTxt,FLedPlcTxt:TsLabelFx;
    FClockLabel:TsLabelFx;
    FTotTimeLabel:TsLabelFx;
    FTotTime:TsPanel;
    FStepTimeLabel:TsLabelFx;
    FStepTime:TsPanel;
    FFuncTextLabel:TsLabelFx;
    FLedAL,FLedOp,FLedSup,FLedPlc:TLed;
    FBatchIDPanel:TsPanel;
    FBatchIDTxtLabel:TsLabelFx;
    FPasso:TsPanel;
    FPassoTxtLabel:TsLabelFx;
    FFunctionPanel:Array[TNFunc] of TsPanel;
    FProgramTxtLabel:TsLabelFx;
    FProgramName:TsLabelFx;
    FProgrammaPanel:TsPanel;   
    FAutoDim: Boolean;
    FClockThr:TClockThr;
    FModoMacchina: TModoMacchina;
    FHeaderRes: THeaderRes;
    procedure InitLed;
    procedure InitLedPos;
    Procedure SomethingChange(Sender:TObject);
    procedure SetClockFont(const Value: TFont);
    function ReadClockFont: TFont;
    function ReadTotTimeFont: TFont;
    procedure SetTotTimeFont(const Value: TFont);
    procedure SetAutoDim(const Value: Boolean);
    function GetFunction1: String;
    function GetFunction10: String;
    function GetFunction2: String;
    function GetFunction3: String;
    function GetFunction4: String;
    function GetFunction5: String;
    function GetFunction6: String;
    function GetFunction7: String;
    function GetFunction8: String;
    function GetFunction9: String;
    procedure SetFunction1(const Value: String);
    procedure SetFunction10(const Value: String);
    procedure SetFunction2(const Value: String);
    procedure SetFunction3(const Value: String);
    procedure SetFunction4(const Value: String);
    procedure SetFunction5(const Value: String);
    procedure SetFunction6(const Value: String);
    procedure SetFunction7(const Value: String);
    procedure SetFunction8(const Value: String);
    procedure SetFunction9(const Value: String);
    procedure SetLedAlAcceso(const Value: Boolean);
    procedure SetLedAlCol(const Value: TLedColor);
    procedure SetLedOpAcceso(const Value: Boolean);
    procedure SetLedOpCol(const Value: TLedColor);
    procedure SetLedPlcAcceso(const Value: Boolean);
    procedure SetLedPlcCol(const Value: TLedColor);
    procedure SetLedSupAcceso(const Value: Boolean);
    procedure SetLedSupCol(const Value: TLedColor);
    function GetLedAlAcceso: Boolean;
    function GetLedAlCol: TLedColor;
    function GetLedOpAcceso: Boolean;
    function GetLedOpCol: TLedColor;
    function GetLedPlcAcceso: Boolean;
    function GetLedPlcCol: TLedColor;
    function GetLedSupAcceso: Boolean;
    function GetLedSupCol: TLedColor;
    procedure SetBatchID(const Value: String);
    procedure SetBatchIDTxt(const Value: String);
    procedure SetProgramma(const Value: String);
    procedure SetProgramTxt(const Value: String);
    function GetProgramma: String;
    function GetProgramTxt: String;
    function GetBatchIDTxt: String;
    function GetBatchID: String;
    procedure SetHeaderRes(const Value: THeaderRes);
    function GetPasso: String;
    function GetPassoTxt: String;
    procedure SetPasso(const Value: String);
    procedure SetPassoTxt(const Value: String);
    function GetProgramName: String;
    procedure SetProgramName(const Value: String);
    function GetStepTime: String;
    function GetStepTimeTxt: String;
    function GetTotTimeTxt: String;
    procedure SetStepTime(const Value: String);
    procedure SetStepTimeTxt(const Value: String);
    procedure SetTotTimeTxt(const Value: String);
    function GetTotTime: String;
    procedure SetTotTime(const Value: String);
   public
    Constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;
    Procedure SetFunctionNo(id,fNo:byte); overload;
    Procedure SetFunctionNo(id:byte;fNo:String); overload;
   published
    property AutoDim:Boolean read FAutoDim write SetAutoDim default True;
    property ClockFont:TFont read ReadClockFont write SetClockFont;
    property TotTimeFont:TFont read ReadTotTimeFont write SetTotTimeFont;
    property TotTime:String read GetTotTime write SetTotTime;
    property TotTimeTxt:String read GetTotTimeTxt write SetTotTimeTxt;
    property StepTime:String read GetStepTime write SetStepTime;
    property StepTimeTxt:String read GetStepTimeTxt write SetStepTimeTxt;
    property Function1:String read GetFunction1 write SetFunction1;
    property Function2:String read GetFunction2 write SetFunction2;
    property Function3:String read GetFunction3 write SetFunction3;
    property Function4:String read GetFunction4 write SetFunction4;
    property Function5:String read GetFunction5 write SetFunction5;
    property Function6:String read GetFunction6 write SetFunction6;
    property Function7:String read GetFunction7 write SetFunction7;
    property Function8:String read GetFunction8 write SetFunction8;
    property Function9:String read GetFunction9 write SetFunction9;
    property Function10:String read GetFunction10 write SetFunction10;
    property LedAlCol:TLedColor read GetLedAlCol write SetLedAlCol default lcRosso;
    property LedOpCol:TLedColor read GetLedOpCol write SetLedOpCol default lcBlu;
    property LedSupCol:TLedColor read GetLedSupCol write SetLedSupCol default lcRosso;
    property LedPlcCol:TLedColor read GetLedPlcCol write SetLedPlcCol default lcRosso;
    property LedAlAcceso:Boolean read GetLedAlAcceso write SetLedAlAcceso;
    property LedOpAcceso:Boolean read GetLedOpAcceso write SetLedOpAcceso;
    property LedSupAcceso:Boolean read GetLedSupAcceso write SetLedSupAcceso;
    property LedPlcAcceso:Boolean read GetLedPlcAcceso write SetLedPlcAcceso;
    property ProgramTxt:String read GetProgramTxt write SetProgramTxt;
    property ProgramName:String read GetProgramName write SetProgramName;
    property Programma:String read GetProgramma write SetProgramma;
    property BatchIDTxt:String read GetBatchIDTxt write SetBatchIDTxt;
    property BatchID:String read GetBatchID write SetBatchID;
    property HeaderRes:THeaderRes read FHeaderRes write SetHeaderRes;
    property PassoTxt:String read GetPassoTxt write SetPassoTxt;
    property Passo:String read GetPasso write SetPasso;
   end;

procedure Register;

implementation

procedure Register;
begin
RegisterComponents('Termo', [TCtHeader]);
end;

{ TClockThr }

procedure TClockThr.ClockUpdate;
begin
if Application.Terminated then exit;
try
 if FParent<>NIL then
    TCtHeader(FParent).FClockLabel.Caption:=TimeToStr(now);
finally
end;
end;

constructor TClockThr.Create(const Parent: TsPanel);
begin
FParent:=Parent;
inherited create(False);
end;

procedure TClockThr.Execute;
Var t:Integer;
begin
Stop:=False;
while NOT Stop do
      begin
       t:=0;
       while t<300 do
             begin
              if Stop then exit;
              sleep(50);
              inc(t,50);
             end;
       Synchronize(ClockUpdate);
      end;
end;

{ TTEx2Header }

constructor TCtHeader.Create(AOwner: TComponent);
CONST PL=10; PT=5;
Var p:Integer;
begin
inherited;
Width:=640; Height:=55;
Caption:=''; FAutoDim:=True;
FClockLabel:=TsLabelFx.Create(self); FClockLabel.Parent:=self;
FClockLabel.Top:=PT; FClockLabel.Left:=10;
FClockThr:=TClockThr.Create(self);
{$IF CompilerVersion >= 21}
ShowCaption:=False;
{$ELSE}
{$IFEND}
for p:=0 to High(FFunctionPanel) do
    begin
     FFunctionPanel[p]:=TsPanel.Create(self);
     FFunctionPanel[p].Parent:=self;
     FFunctionPanel[p].Width:=37;
     FFunctionPanel[p].Height:=20;
     FFunctionPanel[p].Left:=50+(FFunctionPanel[p].Width+4)*p;
     FFunctionPanel[p].Top:=Height-FFunctionPanel[p].Height-PT;
     FFunctionPanel[p].Visible:=(FHeaderRes=hr1024) OR (p<10);
     FFunctionPanel[p].SkinData.SkinSection:='EDIT';
     FFunctionPanel[p].SkinData.CustomFont:=True;
     FFunctionPanel[p].Font:=Font;
     FFunctionPanel[p].Font.Size:=10;
     FFunctionPanel[p].Font.Style:=[fsBold];
     FFunctionPanel[p].Font.Color:=clRed;
     FFunctionPanel[p].Caption:='';
    end;
FFuncTextLabel:=TsLabelFx.Create(self);
with FFuncTextLabel do
     begin
      Parent:=self; Top:=PT; left:=PL;
      Alignment:=taRightJustify;
      Autosize:=False;
      Height:=20; Layout:=tlCenter;
      width:=FFunctionPanel[0].Width+3;
      Caption:='Funct:';
     end;
FBatchIDTxtLabel:=TsLabelFx.Create(self);
with FBatchIDTxtLabel do
     begin
      Parent:=self; Top:=PT;
      Alignment:=taRightJustify;
      Autosize:=False;
      Height:=20; Layout:=tlCenter;
      width:=FFunctionPanel[0].Width+3;
      Caption:='Batch:';
     end;
FBatchIDPanel:=TsPanel.Create(self);
with FBatchIDPanel do
     begin
      Parent:=self; Top:=PT;
      Height:=20; Width:=FFunctionPanel[6].Width+FFunctionPanel[7].Width+4;
      SkinData.SkinSection:='EDIT';
      SkinData.CustomFont:=True;
      Font.Style:=[fsBold];
      Font.Color:=$0000AA00;
     end;
FProgramTxtLabel:=TsLabelFx.Create(self);
with FProgramTxtLabel do
     begin
      Parent:=self; Top:=PT;
      Autosize:=False;
      Height:=20; Layout:=tlCenter;
      width:=FFunctionPanel[0].Width;
      Alignment:=taRightJustify;
      Caption:='Prg: ';
     end;
FProgramName:=TsLabelFx.Create(self);
with FProgramName do
     begin
      Parent:=self; Top:=PT; left:=FFunctionPanel[16].Left;
     end;
FProgrammaPanel:=TsPanel.Create(self);
with FProgrammaPanel do
     begin
      Parent:=self; Top:=PT;
      Height:=20; Width:=FFunctionPanel[9].Width;
      SkinData.SkinSection:='EDIT';
      SkinData.CustomFont:=True;
      Font.Style:=[fsBold];
      Font.Color:=$00AA0000;
     end;
FPasso:=TsPanel.Create(Self);
with FPasso do
     begin
      Parent:=self; Top:=PT;
      Height:=20; Width:=FFunctionPanel[1].Width;
      SkinData.SkinSection:='EDIT';
      SkinData.CustomFont:=True;
      Font.Style:=[fsBold];
      Font.Color:=$00AA0000;
     end;
FPassoTxtLabel:=TsLabelFx.Create(self);
with FPassoTxtLabel do
     begin
      Parent:=self; Top:=PT;
      Alignment:=taRightJustify;
      Autosize:=False;
      Height:=20; Layout:=tlCenter;
      width:=FFunctionPanel[0].Width;
      Caption:='P: ';
     end;
FTotTimeLabel:=TsLabelFx.Create(self);
with FTotTimeLabel do
     begin
      Parent:=self; Top:=PT;
      Alignment:=taRightJustify;
      Autosize:=False;
      Height:=20; Layout:=tlCenter;
      width:=FFunctionPanel[0].Width;
      Caption:='TT: ';
     end;
FTotTime:=TsPanel.Create(Self);
with FTotTime do
     begin
      Parent:=self; Top:=PT;
      Height:=20; Width:=FFunctionPanel[1].Width;
      SkinData.SkinSection:='EDIT';
      Font.Style:=[fsBold];
      SkinData.CustomFont:=True;
      Font.Color:=$00AA0000;
     end;
FStepTimeLabel:=TsLabelFx.Create(self);
with FStepTimeLabel do
     begin
      Parent:=self; Top:=PT; Left:=FFunctionPanel[4].Left;
      Alignment:=taRightJustify;
      Autosize:=False;
      Height:=20; Layout:=tlCenter;
      width:=FFunctionPanel[0].Width;
      Caption:='ST: ';
     end;
FStepTime:=TsPanel.Create(Self);
with FStepTime do
     begin
      Parent:=self; Top:=PT; left:=FFunctionPanel[5].Left;
      Height:=20; Width:=FFunctionPanel[1].Width;
      SkinData.SkinSection:='EDIT';
      SkinData.CustomFont:=True;
      Font.Style:=[fsBold];
      Font.Color:=$00AA0000;
     end;
InitLed;
SomethingChange(Nil);
Font.OnChange:=SomethingChange;
ClockFont.OnChange:=SomethingChange;
OnResize:=SomethingChange;
end;

destructor TCtHeader.Destroy;
begin
try
 if FClockThr<>NIL then
    begin
     FClockThr.stop:=True;
     FClockThr.Terminate;
    end;
 FLedAL.Destroy; FLedOp.Destroy; FLedSup.Destroy; FLedPlc.Destroy;
finally
 inherited;
end;
end;

procedure TCtHeader.SomethingChange(Sender: TObject);
begin
FLedALTxt.Font.Assign(Font);
FLedOpTxt.Font.Assign(Font);
FLedSupTxt.Font.Assign(Font);
FLedPlcTxt.Font.Assign(Font);
FFuncTextLabel.Font.Assign(Font);
FClockLabel.Font.Assign(ClockFont);
FTotTimeLabel.Font.Assign(Font);
FStepTimeLabel.Font.Assign(Font);
FPassoTxtLabel.Font.Assign(Font);
FBatchIDTxtLabel.Font.Assign(Font);
FProgramTxtLabel.Font.Assign(Font);
FFuncTextLabel.Top:=Height-FFuncTextLabel.Height-3;
end;

procedure TCtHeader.InitLed;
begin
FLedAL:=TLed.Create(Self); FLedOp:=TLed.Create(Self);
FLedSup:=TLed.Create(Self); FLedPlc:=TLed.Create(Self);
FLedAL.Parent:=Self; FLedOp.Parent:=Self; FLedSup.Parent:=Self; FLedPlc.Parent:=Self;
FLedAL.LedColor:=lcRosso; FLedOp.LedColor:=lcBlu;
FLedSup.LedColor:=lcRosso; FLedPlc.LedColor:=lcRosso;
FLedAL.Caption:=' '; FLedOp.Caption:=' ';
FLedSup.Caption:=' '; FLedPlc.Caption:=' ';
FLedALTxt:=TsLabelFx.Create(self); FLedALTxt.Parent:=self;  FLedALTxt.Caption:='RUN';
FLedOpTxt:=TsLabelFx.Create(self); FLedOpTxt.Parent:=self;  FLedOpTxt.Caption:='Op';
FLedSupTxt:=TsLabelFx.Create(self);FLedSupTxt.Parent:=self; FLedSupTxt.Caption:='Sup';
FLedPlcTxt:=TsLabelFx.Create(self);FLedPlcTxt.Parent:=self; FLedPlcTxt.Caption:='CPU';
InitLedPos;
end;

procedure TCtHeader.InitLedPos;
Var La:Integer;
begin
case FHeaderRes of
 hr640:LA:=545;
 hr800:LA:=545;
 hr1024:LA:=920;
 else LA:=545;
end;
FLedAL.Left:=LA-47; FLedAL.top:=10;
FLedOp.Left:=LA-47; FLedOp.top:=30;
FLedSup.Left:=LA; FLedSup.top:=10;
FLedPlc.Left:=LA; FLedPlc.top:=30;
FLedALTxt.Top:=7; FLedALTxt.left:=FLedAL.Left+18;
FLedOpTxt.Top:=27; FLedOpTxt.left:=FLedOp.Left+18;
FLedSupTxt.Top:=7; FLedSupTxt.left:=FLedSup.Left+18;
FLedPlcTxt.Top:=27; FLedPlcTxt.left:=FLedPlc.Left+18;
end;

function TCtHeader.GetBatchID: String;
begin
Result:=FBatchIDPanel.Caption;
end;

function TCtHeader.GetBatchIDTxt: String;
begin
Result:=FBatchIDTxtLabel.Caption;
end;

function TCtHeader.GetFunction1: String;
begin
Result:=FFunctionPanel[0].Caption;
end;

function TCtHeader.GetFunction10: String;
begin
Result:=FFunctionPanel[9].Caption;
end;

function TCtHeader.GetFunction2: String;
begin
Result:=FFunctionPanel[1].Caption;
end;

function TCtHeader.GetFunction3: String;
begin
Result:=FFunctionPanel[2].Caption;
end;

function TCtHeader.GetFunction4: String;
begin
Result:=FFunctionPanel[3].Caption;
end;

function TCtHeader.GetFunction5: String;
begin
Result:=FFunctionPanel[4].Caption;
end;

function TCtHeader.GetFunction6: String;
begin
Result:=FFunctionPanel[5].Caption;
end;

function TCtHeader.GetFunction7: String;
begin
Result:=FFunctionPanel[6].Caption;
end;

function TCtHeader.GetFunction8: String;
begin
Result:=FFunctionPanel[7].Caption;
end;

function TCtHeader.GetFunction9: String;
begin
Result:=FFunctionPanel[8].Caption;
end;

function TCtHeader.GetLedAlAcceso: Boolean;
begin
Result:=FLedAL.Acceso;
end;

function TCtHeader.GetLedAlCol: TLedColor;
begin
Result:=FLedAL.LedColor;
end;

function TCtHeader.GetLedOpAcceso: Boolean;
begin
Result:=FLedOp.Acceso;
end;

function TCtHeader.GetLedOpCol: TLedColor;
begin
Result:=FLedOp.LedColor;
end;

function TCtHeader.GetLedPlcAcceso: Boolean;
begin
Result:=FLedPlc.Acceso;
end;

function TCtHeader.GetLedPlcCol: TLedColor;
begin
Result:=FLedPlc.LedColor;
end;

function TCtHeader.GetLedSupAcceso: Boolean;
begin
Result:=FLedSup.Acceso;
end;

function TCtHeader.GetLedSupCol: TLedColor;
begin
Result:=FLedSup.LedColor;
end;

function TCtHeader.GetProgramma: String;
begin
result:=FProgrammaPanel.Caption;
end;

function TCtHeader.GetProgramTxt: String;
begin
Result:=FProgramTxtLabel.Caption;
end;

function TCtHeader.GetTotTime: String;
begin
Result:=FTotTime.Caption;
end;

function TCtHeader.ReadClockFont: TFont;
begin
Result:=FClockLabel.Font;
end;

function TCtHeader.ReadTotTimeFont: TFont;
begin
Result:=FTotTimeLabel.Font;
end;

procedure TCtHeader.SetAutoDim(const Value: Boolean);
begin
FAutoDim:=Value;
if FAutoDim then
   begin Width:=640; Height:=95; end;
end;

procedure TCtHeader.SetBatchID(const Value: String);
begin
FBatchIDPanel.Caption:=Value;
end;

procedure TCtHeader.SetBatchIDTxt(const Value: String);
begin
FBatchIDTxtLabel.Caption:=Value;
end;

procedure TCtHeader.SetClockFont(const Value: TFont);
begin
FClockLabel.Font:=Value;
end;

procedure TCtHeader.SetFunction1(const Value: String);
begin
FFunctionPanel[0].Caption:=Value;
end;

procedure TCtHeader.SetFunction10(const Value: String);
begin
FFunctionPanel[9].Caption:=Value;
end;

procedure TCtHeader.SetFunction2(const Value: String);
begin
FFunctionPanel[1].Caption:=Value;
end;

procedure TCtHeader.SetFunction3(const Value: String);
begin
FFunctionPanel[2].Caption:=Value;
end;

procedure TCtHeader.SetFunction4(const Value: String);
begin
FFunctionPanel[3].Caption:=Value;
end;

procedure TCtHeader.SetFunction5(const Value: String);
begin
FFunctionPanel[4].Caption:=Value;
end;

procedure TCtHeader.SetFunction6(const Value: String);
begin
FFunctionPanel[5].Caption:=Value;
end;

procedure TCtHeader.SetFunction7(const Value: String);
begin
FFunctionPanel[6].Caption:=Value;
end;

procedure TCtHeader.SetFunction8(const Value: String);
begin
FFunctionPanel[7].Caption:=Value;
end;

procedure TCtHeader.SetFunction9(const Value: String);
begin
FFunctionPanel[8].Caption:=Value;
end;

procedure TCtHeader.SetFunctionNo(id: byte; fNo: String);
begin
if id>High(FFunctionPanel) then exit;
FFunctionPanel[id].Caption:=fNo;
end;

procedure TCtHeader.SetFunctionNo(id,fNo: byte);
begin
if id>High(FFunctionPanel) then exit;
FFunctionPanel[id].Caption:=IntToStr(fNo);
end;

procedure TCtHeader.SetLedAlAcceso(const Value: Boolean);
begin
FLedAL.Acceso:=Value;
end;

procedure TCtHeader.SetLedAlCol(const Value: TLedColor);
begin
FLedAL.LedColor:=Value;
end;

procedure TCtHeader.SetLedOpAcceso(const Value: Boolean);
begin
FLedOp.Acceso:= Value;
end;

procedure TCtHeader.SetLedOpCol(const Value: TLedColor);
begin
FLedOp.LedColor:=Value;
end;

procedure TCtHeader.SetLedPlcAcceso(const Value: Boolean);
begin
FLedPlc.Acceso:= Value;
end;

procedure TCtHeader.SetLedPlcCol(const Value: TLedColor);
begin
FLedPlc.LedColor:=Value;
end;

procedure TCtHeader.SetLedSupAcceso(const Value: Boolean);
begin
FLedSup.Acceso:= Value;
end;

procedure TCtHeader.SetLedSupCol(const Value: TLedColor);
begin
FLedSup.LedColor:=Value;
end;

procedure TCtHeader.SetProgramma(const Value: String);
begin
FProgrammaPanel.Caption:=Value;
end;

procedure TCtHeader.SetProgramTxt(const Value: String);
begin
FProgramTxtLabel.Caption:=Value;
end;

procedure TCtHeader.SetTotTime(const Value: String);
begin
FTotTime.Caption:=Value;
end;

procedure TCtHeader.SetTotTimeFont(const Value: TFont);
begin
FTotTimeLabel.Font:=Value;
end;

procedure TCtHeader.SetHeaderRes(const Value: THeaderRes);
Var p:integer;
begin
FHeaderRes:=Value;
for p:=0 to High(FFunctionPanel) do
    FFunctionPanel[p].Visible:=(FHeaderRes=hr1024) OR (p<11);
case FHeaderRes of
 hr640,
 hr800:begin
        FProgrammaPanel.left:=FFunctionPanel[10].Left;
        FProgramTxtLabel.left:=FFunctionPanel[9].Left;
        FBatchIDPanel.left:=FFunctionPanel[7].Left;
        FBatchIDPanel.Width:=FFunctionPanel[7].Width+FFunctionPanel[8].Width+4;
        FBatchIDTxtLabel.left:=FFunctionPanel[6].Left;
        FPasso.left:=FFunctionPanel[1].Left;
        FPassoTxtLabel.left:=FFunctionPanel[0].Left;
        FProgramName.Visible:=False;
        FTotTimeLabel.left:=FFunctionPanel[2].Left;
        FTotTime.left:=FFunctionPanel[3].Left;
        FTotTime.Width:=FFunctionPanel[1].Width;
        FStepTimeLabel.Left:=FFunctionPanel[4].Left;
        FStepTime.left:=FFunctionPanel[5].Left;
        FStepTime.Width:=FFunctionPanel[1].Width;
       end;
 hr1024:Begin
         FProgrammaPanel.left:=FFunctionPanel[15].Left;
         FProgramTxtLabel.left:=FFunctionPanel[14].Left;
         FBatchIDPanel.left:=FFunctionPanel[11].Left;
         FBatchIDPanel.Width:=FFunctionPanel[11].Width+FFunctionPanel[12].Width+FFunctionPanel[13].Width+8;
         FBatchIDTxtLabel.left:=FFunctionPanel[10].Left;
         FPasso.left:=FFunctionPanel[2].Left;
         FPassoTxtLabel.left:=FFunctionPanel[1].Left;
         FProgramName.Visible:=True;
         FTotTimeLabel.left:=FFunctionPanel[3].Left;
         FTotTime.left:=FFunctionPanel[4].Left;
         FTotTime.Width:=FFunctionPanel[4].Width+FFunctionPanel[5].Width+4;
         FStepTimeLabel.Left:=FFunctionPanel[6].Left;
         FStepTime.left:=FFunctionPanel[7].Left;
         FStepTime.Width:=FFunctionPanel[7].Width+FFunctionPanel[8].Width+4;
        end;
end;
InitLedPos;
end;

function TCtHeader.GetPasso: String;
begin
Result:=FPasso.Caption;
end;

function TCtHeader.GetPassoTxt: String;
begin
Result:=FPassoTxtLabel.Caption;
end;

procedure TCtHeader.SetPasso(const Value: String);
begin
FPasso.Caption:=Value;
end;

procedure TCtHeader.SetPassoTxt(const Value: String);
begin
FPassoTxtLabel.Caption:=Value;
end;

function TCtHeader.GetProgramName: String;
begin
Result:=FProgramName.Caption;
end;

procedure TCtHeader.SetProgramName(const Value: String);
begin
FProgramName.Caption:=Value;
end;

function TCtHeader.GetStepTime: String;
begin
Result:=FStepTime.Caption;
end;

function TCtHeader.GetStepTimeTxt: String;
begin
Result:=FStepTimeLabel.caption;
end;

function TCtHeader.GetTotTimeTxt: String;
begin
Result:=FTotTimeLabel.Caption;
end;

procedure TCtHeader.SetStepTime(const Value: String);
begin
FStepTime.Caption:=Value;
end;

procedure TCtHeader.SetStepTimeTxt(const Value: String);
begin
FStepTimeLabel.Caption:=Value;
end;

procedure TCtHeader.SetTotTimeTxt(const Value: String);
begin
FTotTimeLabel.Caption:=Value;
end;

end.
