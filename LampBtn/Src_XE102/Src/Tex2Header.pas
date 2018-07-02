unit TEx2Header;

interface

uses Windows, messages, SysUtils, stdctrls, Classes, Controls, ExtCtrls, PngImage,
     Graphics, TExDef, Led;

type

   TClockThr=class(TThread)
   public
    stop:Boolean;
    FParent:TCustomPanel;
    constructor Create(CONST Parent:TCustomPanel); reintroduce;
   protected
    procedure Execute; override;
    procedure ClockUpdate;
   end;

   TTEx2Header=class(TCustomPanel)
   private
    {$IF CompilerVersion >= 21}FSxPng,FCPng,FCCPng,FDxPng,FDxRPng,FDxFPng:TPngImage;
    {$ELSE}FSxPng,FCPng,FCCPng,FDxPng,FDxRPng,FDxFPng:TPngObject;
    {$IFEND}
    FLedAL,FLedOp,FLedSup,FLedPlc:TLed;
    FClock: String;
    FHeaderRes: THeaderRes;
    FClockFont: TFont;
    FAutoDim: Boolean;
    FClockThr:TClockThr;
    FDateFont: TFont;
    FDate: String;
    FPassoTxt: String;
    FGeneralFont: TFont;
    FPasso: Integer;
    FPassiTot: Integer;
    FProgramTxt: String;
    FBatchIDTxt: String;
    FFunzioniTxt: String;
    FFunzione6: String;
    FFunzione7: String;
    FFunzione9: String;
    FFunzione3: String;
    FFunzione1: String;
    FFunzione2: String;
    FFunzione4: String;
    FFunzione8: String;
    FFunzione5: String;
    FFunzione10: String;
    FAllarmi: String;
    FProgramma: String;
    FBatchID: String;
    FPrgVer: String;
    FModoMacchina: TModoMacchina;
    FLedOpCol: TLedColor;
    FLedPlcCol: TLedColor;
    FLedSupCol: TLedColor;
    FLedAlCol: TLedColor;
    FLedSupAcceso: Boolean;
    FLedPlcAcceso: Boolean;
    FLedOpAcceso: Boolean;
    FLedAlAcceso: Boolean;
    FMasterSlaveStatus: byte;
    MasterSlaveBmp:array[1..6] of TBitmap;
    FShowSector: TShowSector;
    FContinuousMachine: Boolean;
    FFunzione19: String;
    FFunzione16: String;
    FFunzione11: String;
    FFunzione17: String;
    FFunzione18: String;
    FFunzione15: String;
    FFunzione14: String;
    FFunzione12: String;
    FFunzione20: String;
    FFunzione13: String;
    FFunzione26: String;
    FFunzione21: String;
    FFunzione24: String;
    FFunzione27: String;
    FFunzione28: String;
    FFunzione25: String;
    FFunzione23: String;
    FFunzione22: String;
    FFunzione29: String;
    FFunzione30: String;
    FFunzione31: String;
    FFunzione32: String;
    FFunzione33: String;
    FFunzione34: String;
    Procedure LoadPNG;
    Procedure SomethingChange(Sender:TObject);
    procedure SetHeaderRes(const Value: THeaderRes);
    procedure SetClockFont(const Value: TFont);
    procedure SetAutoDim(const Value: Boolean);
    procedure DoAutoDim;
    procedure SetDateFont(const Value: TFont);
    procedure SetPassoTxt(const Value: String);
    procedure SetGeneralFont(const Value: TFont);
    procedure SetPassiTot(const Value: Integer);
    procedure SetPasso(const Value: Integer);
    procedure SetBatchIDTxt(const Value: String);
    procedure SetFunzioniTxt(const Value: String);
    procedure SetProgramTxt(const Value: String);
    procedure SetFunzione1(const Value: String);
    procedure SetFunzione2(const Value: String);
    procedure SetFunzione3(const Value: String);
    procedure SetFunzione4(const Value: String);
    procedure SetFunzione5(const Value: String);
    procedure SetFunzione6(const Value: String);
    procedure SetFunzione7(const Value: String);
    procedure SetFunzione8(const Value: String);
    procedure SetFunzione9(const Value: String);
    procedure SetFunzione10(const Value: String);
    procedure SetAllarmi(const Value: String);
    procedure SetBatchID(const Value: String);
    procedure SetProgramma(const Value: String);
    procedure SetPrgVer(const Value: String);
    procedure SetModoMacchina(const Value: TModoMacchina);
    procedure SetLedAlCol(const Value: TLedColor);
    procedure SetLedOpCol(const Value: TLedColor);
    procedure SetLedPlcCol(const Value: TLedColor);
    procedure SetLedSupCol(const Value: TLedColor);
    procedure InitLed;
    procedure SetLedPos;
    procedure SetLedAlAcceso(const Value: Boolean);
    procedure SetLedOpAcceso(const Value: Boolean);
    procedure SetLedPlcAcceso(const Value: Boolean);
    procedure SetLedSupAcceso(const Value: Boolean);
    procedure SetMasterSlaveStatus(const Value: byte);
    procedure UpdateClock;
    procedure SetShowSector(const Value: TShowSector);
    procedure SetContinuousMachine(const Value: Boolean);
    procedure SetFunzione11(const Value: String);
    procedure SetFunzione12(const Value: String);
    procedure SetFunzione13(const Value: String);
    procedure SetFunzione14(const Value: String);
    procedure SetFunzione15(const Value: String);
    procedure SetFunzione16(const Value: String);
    procedure SetFunzione17(const Value: String);
    procedure SetFunzione18(const Value: String);
    procedure SetFunzione19(const Value: String);
    procedure SetFunzione20(const Value: String);
    procedure SetFunzione21(const Value: String);
    procedure SetFunzione22(const Value: String);
    procedure SetFunzione23(const Value: String);
    procedure SetFunzione24(const Value: String);
    procedure SetFunzione25(const Value: String);
    procedure SetFunzione26(const Value: String);
    procedure SetFunzione27(const Value: String);
    procedure SetFunzione28(const Value: String);
    procedure SetFunzione29(const Value: String);
    procedure SetFunzione30(const Value: String);
    procedure SetFunzione31(const Value: String);
    procedure SetFunzione32(const Value: String);
    procedure SetFunzione33(const Value: String);
    procedure SetFunzione34(const Value: String);
    procedure PaintFuncNO(X,Y:Integer;ft:String);
   protected
    procedure PaintSx;
    procedure PaintC;
    procedure PaintDx;
    procedure PaintFuncAtt;
    procedure PaintAlarms;
    procedure PaintMS;
    procedure InvalidateLed;
    procedure Paint; Override;
   public
    Constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;
   published
    property Align;
    property Height;
    property Width;
    property PopUpMenu;
    property HeaderRes:THeaderRes read FHeaderRes write SetHeaderRes default hr1024;
    property Clock:String read FClock;
    property ClockFont:TFont read FClockFont write SetClockFont;
    property Date:String read FDate;
    property DateFont:TFont read FDateFont write SetDateFont;
    property AutoDim:Boolean read FAutoDim write SetAutoDim default False;
    property GeneralFont:TFont read FGeneralFont write SetGeneralFont;
    property PassoTxt:String read FPassoTxt write SetPassoTxt;
    property Passo:Integer read FPasso write SetPasso;
    property PassiTot:Integer read FPassiTot write SetPassiTot;
    property ProgramTxt:String read FProgramTxt write SetProgramTxt;
    property Programma:String read FProgramma write SetProgramma;
    property BatchIDTxt:String read FBatchIDTxt write SetBatchIDTxt;
    property BatchID:String read FBatchID write SetBatchID;
    property Allarmi:String read FAllarmi write SetAllarmi;
    property FunzioniTxt:String read FFunzioniTxt write SetFunzioniTxt;
    property Funzione1:String read FFunzione1 write SetFunzione1;
    property Funzione2:String read FFunzione2 write SetFunzione2;
    property Funzione3:String read FFunzione3 write SetFunzione3;
    property Funzione4:String read FFunzione4 write SetFunzione4;
    property Funzione5:String read FFunzione5 write SetFunzione5;
    property Funzione6:String read FFunzione6 write SetFunzione6;
    property Funzione7:String read FFunzione7 write SetFunzione7;
    property Funzione8:String read FFunzione8 write SetFunzione8;
    property Funzione9:String read FFunzione9 write SetFunzione9;
    property Funzione10:String read FFunzione10 write SetFunzione10;
    property Funzione11:String read FFunzione11 write SetFunzione11;
    property Funzione12:String read FFunzione12 write SetFunzione12;
    property Funzione13:String read FFunzione13 write SetFunzione13;
    property Funzione14:String read FFunzione14 write SetFunzione14;
    property Funzione15:String read FFunzione15 write SetFunzione15;
    property Funzione16:String read FFunzione16 write SetFunzione16;
    property Funzione17:String read FFunzione17 write SetFunzione17;
    property Funzione18:String read FFunzione18 write SetFunzione18;
    property Funzione19:String read FFunzione19 write SetFunzione19;
    property Funzione20:String read FFunzione20 write SetFunzione20;
    property Funzione21:String read FFunzione21 write SetFunzione21;
    property Funzione22:String read FFunzione22 write SetFunzione22;
    property Funzione23:String read FFunzione23 write SetFunzione23;
    property Funzione24:String read FFunzione24 write SetFunzione24;
    property Funzione25:String read FFunzione25 write SetFunzione25;
    property Funzione26:String read FFunzione26 write SetFunzione26;
    property Funzione27:String read FFunzione27 write SetFunzione27;
    property Funzione28:String read FFunzione28 write SetFunzione28;
    property Funzione29:String read FFunzione29 write SetFunzione29;
    property Funzione30:String read FFunzione30 write SetFunzione30;
    property Funzione31:String read FFunzione31 write SetFunzione31;
    property Funzione32:String read FFunzione32 write SetFunzione32;
    property Funzione33:String read FFunzione33 write SetFunzione33;
    property Funzione34:String read FFunzione34 write SetFunzione34;
    property ModoMacchina:TModoMacchina read FModoMacchina write SetModoMacchina;
    property PrgVer:String read FPrgVer write SetPrgVer;
    procedure SetFunction(nFunc:Integer;ValFunc:String);
    property LedAlCol:TLedColor read FLedAlCol write SetLedAlCol default lcRosso;
    property LedOpCol:TLedColor read FLedOpCol write SetLedOpCol default lcBlu;
    property LedSupCol:TLedColor read FLedSupCol write SetLedSupCol default lcRosso;
    property LedPlcCol:TLedColor read FLedPlcCol write SetLedPlcCol default lcRosso;
    property LedAlAcceso:Boolean read FLedAlAcceso write SetLedAlAcceso;
    property LedOpAcceso:Boolean read FLedOpAcceso write SetLedOpAcceso;
    property LedSupAcceso:Boolean read FLedSupAcceso write SetLedSupAcceso;
    property LedPlcAcceso:Boolean read FLedPlcAcceso write SetLedPlcAcceso;
    property MasterSlaveStatus:byte read FMasterSlaveStatus write SetMasterSlaveStatus default 1;
    property ShowSector:TShowSector read FShowSector write SetShowSector default [SectSx,SectC,SectDx];
    property ContinuousMachine:Boolean read FContinuousMachine write SetContinuousMachine;
   end;

procedure Register;

{$R TEx2Header.res}
{$R masterslave.res}

implementation

procedure Register;
begin
RegisterComponents('Termo', [TTEx2Header]);
end;

{ TClockThr }

procedure TClockThr.ClockUpdate;
begin
TTEx2Header(FParent).UpdateClock;
end;

constructor TClockThr.Create(const Parent: TCustomPanel);
begin
FParent:=Parent;
inherited create(False);
end;

procedure TClockThr.Execute;
Var t:Integer;
begin
Stop:=False;
while NOT (Stop OR Terminated) do
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

constructor TTEx2Header.Create(AOwner: TComponent);
Var i:Integer;
begin
inherited;
{$IF CompilerVersion >= 21}
FSxPng:=TPngImage.Create; FCPng:=TPngImage.Create;
FDxPng:=TPngImage.Create; FCCPng:=TPngImage.Create;
FDxRPng:=TPngImage.Create; FDxFPng:=TPngImage.Create;
{$ELSE}
FSxPng:=TPngObject.Create; FCPng:=TPngObject.Create;
FDxPng:=TPngObject.Create; FCCPng:=TPngObject.Create;
FDxRPng:=TPngObject.Create; FDxFPng:=TPngObject.Create;
{$IFEND}
FLedAL:=TLed.Create(Self); FLedOp:=TLed.Create(Self);
FLedSup:=TLed.Create(Self); FLedPlc:=TLed.Create(Self);
FLedAL.Parent:=Self; FLedOp.Parent:=Self; FLedSup.Parent:=Self; FLedPlc.Parent:=Self;
for i:=1 to 6 do
    begin
     MasterSlaveBmp[i]:=TBitmap.Create;
     try
      MasterSlaveBmp[i].LoadFromResourceID(hInstance,10+i);
     finally
     end;
    end;
ParentColor:=True;
FHeaderRes:=hr1024;
FShowSector:=[SectSx,SectC,SectDx];
try LoadPNG; finally end;
Height:=FSxPng.Height;
Width:=FSxPng.Width+FCPng.Width+FDxPng.Width;
FClockFont:=TFont.Create;
FClockFont.Color:=clTExMenu3;
FClockFont.OnChange:=SomethingChange;
FClockFont.Name:='arial';
FDateFont:=TFont.Create;
FDateFont.Color:=clLime;
FDateFont.OnChange:=SomethingChange;
FDateFont.Name:='arial';
FGeneralFont:=TFont.Create;
FGeneralFont.Color:=clTExMenu3;
FGeneralFont.Size:=12;
FGeneralFont.Style:=[fsBold];
FGeneralFont.OnChange:=SomethingChange;
FGeneralFont.Name:='arial';
FAutoDim:=False;
FPassoTxt:='Step'; FProgramTxt:='Progr'; FBatchIDTxt:='Plan'; FFunzioniTxt:='Funct';
if NOT (csDesigning in ComponentState) then
   FClockThr:=TClockThr.Create(Self);
InitLed;
SetLedPos;
FMasterSlaveStatus:=1;
end;

procedure TTEx2Header.InitLed;
begin
FLedAlCol:=lcRosso; FLedOpCol:=lcBlu;
FLedSupCol:=lcRosso; FLedPlcCol:=lcRosso;
FLedAL.LedColor:=lcRosso; FLedOp.LedColor:=lcBlu;
FLedSup.LedColor:=lcRosso; FLedPlc.LedColor:=lcRosso;
FLedAL.Caption3D:=True; FLedOp.Caption3D:=True;
FLedSup.Caption3D:=True; FLedPlc.Caption3D:=True;
FLedAL.ShadowColor:=0; FLedOp.ShadowColor:=0;
FLedSup.ShadowColor:=0; FLedPlc.ShadowColor:=0;
FLedAL.Caption:='Al'; FLedOp.Caption:='Op';
FLedSup.Caption:='Sup'; FLedPlc.Caption:='Plc';
FLedAL.Font:=FGeneralFont; FLedOp.Font:=FGeneralFont;
FLedSup.Font:=FGeneralFont; FLedPlc.Font:=FGeneralFont;
end;

destructor TTEx2Header.Destroy;
Var i:integer;
begin
if FClockThr<>NIL then FClockThr.stop:=True;
FLedAL.Destroy; FLedOp.Destroy; FLedSup.Destroy; FLedPlc.Destroy;
FClockFont.Destroy; FDateFont.Destroy; FGeneralFont.Destroy;
for i:=6 downto 1 do
    MasterSlaveBmp[i].Destroy;
FSxPng.destroy; FCPng.Destroy; FCCPng.Destroy; FDxPng.Destroy; FDxRPng.Destroy; FDxFPng.Destroy;
inherited;
end;

procedure TTEx2Header.DoAutoDim;
begin
if Align=alClient then exit;
if NOT FAutoDim then exit;
case FHeaderRes of
 hr640:if (Align<>alTop) AND (Align<>alBottom) then width:=640;
 hr800:if (Align<>alTop) AND (Align<>alBottom) then width:=800;
 hr1024:if (Align<>alTop) AND (Align<>alBottom) then width:=1024;
 hr1280:if (Align<>alTop) AND (Align<>alBottom) then width:=1280;
end;
if (Align<>alLeft) AND (Align<>alRight) then height:=FSxPng.Height;
end;

procedure TTEx2Header.LoadPNG;
VAR Stream: TCustomMemoryStream;
begin
case FHeaderRes of
 hr640:begin
        Stream:=TResourceStream.Create(hInstance,PChar('ANGOLODESTRO800_2'),PChar('PNG'));
        FDxPng.LoadFromStream(Stream); Stream.Destroy;
        Stream:=TResourceStream.Create(hInstance,PChar('TESTATA640'),PChar('PNG'));
        FCPng.LoadFromStream(Stream); Stream.Destroy;
        Stream:=TResourceStream.Create(hInstance,PChar('ANGOLOSINISTRO800'),PChar('PNG'));
        FSxPng.LoadFromStream(Stream); Stream.Destroy;
       end;
 hr800:begin
        Stream:=TResourceStream.Create(hInstance,PChar('ANGOLODESTRO800_2'),PChar('PNG'));
        FDxPng.LoadFromStream(Stream); Stream.Destroy;
        Stream:=TResourceStream.Create(hInstance,PChar('TESTATA800'),PChar('PNG'));
        FCPng.LoadFromStream(Stream); Stream.Destroy;
        Stream:=TResourceStream.Create(hInstance,PChar('ANGOLOSINISTRO800'),PChar('PNG'));
        FSxPng.LoadFromStream(Stream); Stream.Destroy;
       end;
 hr1024,
 hr1280:Begin
         Stream:=TResourceStream.Create(hInstance,PChar('ANGOLODESTRO1024_3'),PChar('PNG'));
         FDxPng.LoadFromStream(Stream); Stream.Destroy;
         Stream:=TResourceStream.Create(hInstance,PChar('TESTATA1024'),PChar('PNG'));
         FCPng.LoadFromStream(Stream); Stream.Destroy;
         case FHeaderRes of
          hr1024:Stream:=TResourceStream.Create(hInstance,PChar('TESTATAC1024'),PChar('PNG'));
          hr1280:Stream:=TResourceStream.Create(hInstance,PChar('TESTATAC1280'),PChar('PNG'));
         end;
         FCCPng.LoadFromStream(Stream); Stream.Destroy;
         Stream:=TResourceStream.Create(hInstance,PChar('ANGOLOSINISTRO1024'),PChar('PNG'));
         FSxPng.LoadFromStream(Stream); Stream.Destroy;
         Stream:=TResourceStream.Create(hInstance,PChar('ANGOLODESTRO1024_RIEMP'),PChar('PNG'));
         FDxRPng.LoadFromStream(Stream); Stream.Destroy;
         Stream:=TResourceStream.Create(hInstance,PChar('ANGOLODESTRO1024_FINALE'),PChar('PNG'));
         FDxFPng.LoadFromStream(Stream); Stream.Destroy;
        end;
End;
end;

procedure TTEx2Header.Paint;
begin
with canvas do
     begin
      PaintSx;
      PaintC;
      PaintDx;
      DoAutoDim;
     end;
end;

procedure TTEx2Header.PaintSx;
Var ts:TSize; s:String; i:integer;
begin
if NOT (csDesigning in ComponentState) then
   begin
    if FClockThr=NIL then exit;
    if FClockThr.stop then exit;
   end;
if NOT (SectSx in FShowSector) then exit;
FClock:=TimeToStr(Now);
FDate:=DateToStr(Now);
with canvas do
     begin
      lock;
      brush.Style:=bsClear;
      Draw(0,0,FSxPng);
      case FHeaderRes of
       hr640,
       hr800:begin
              //--------------------------------------------------------OROLOGIO
              font:=FClockFont;
              font.Size:=font.Size-FS800;
              ts:=TextExtent(FClock);
              TextOut((79-ts.cx) DIV 2,(22-ts.cy) DIV 2,FClock); //6,6 96,22
              //--------------------------------------------------------DATA
              font:=FDateFont;
              font.Size:=font.Size-FS800;
              ts:=TextExtent(FDate);
              TextOut(78+(79-ts.cx) DIV 2,(22-ts.cy) DIV 2,FDate); //106,6 196,22
              //--------------------------------------------------------STEP
              font:=FGeneralFont;
              font.Size:=font.Size-FS800;
              font.Color:=0;
              ts:=TextExtent(FPassoTxt);
              TextOut(7-1,20+((50-ts.cy) DIV 2)-1,FPassoTxt);
              font.Color:=FGeneralFont.Color;
              TextOut(7,20+(50-ts.cy) DIV 2,FPassoTxt);
              font.Size:=15; font.Style:=[fsBold];
              s:='/'+IntToStr(FPassiTot);
              ts:=TextExtent(s); i:=153-ts.cx;
              font.Color:=$444444;
              TextOut(i+1,65-ts.cy+1,s);
              font.Color:=0;
              TextOut(i,65-ts.cy,s);
              font.Size:=28;
              s:=IntToStr(FPasso);
              ts:=TextExtent(s);
              font.Color:=$444444;
              TextOut(i-ts.cx+2,68-ts.cy+2,s);
              font.Color:=clBlue;
              TextOut(i-ts.cx,68-ts.cy,s);
             end;
       hr1024,
       hr1280:begin
               //--------------------------------------------------------OROLOGIO
               font:=FClockFont;
               ts:=TextExtent(FClock);
               TextOut((101-ts.cx) DIV 2,(28-ts.cy) DIV 2,FClock); //6,6 96,22
               //--------------------------------------------------------DATA
               font:=FDateFont;
               ts:=TextExtent(FDate);
               TextOut(100+(101-ts.cx) DIV 2,(28-ts.cy) DIV 2,FDate); //106,6 196,22
               //--------------------------------------------------------STEP
               font:=FGeneralFont;
               font.Color:=0;
               ts:=TextExtent(FPassoTxt);
               TextOut(10-1,25+((65-ts.cy) DIV 2)-1,FPassoTxt);
               font.Color:=FGeneralFont.Color;
               TextOut(10,25+(65-ts.cy) DIV 2,FPassoTxt);
               font.Size:=18; font.Style:=[fsBold];
               s:='/'+IntToStr(FPassiTot);
               ts:=TextExtent(s); i:=190-ts.cx;
               font.Color:=$444444;
               TextOut(i+1,85-ts.cy+1,s);
               font.Color:=0;
               TextOut(i,85-ts.cy,s);
               font.Size:=34;
               s:=IntToStr(FPasso);
               ts:=TextExtent(s);
               font.Color:=$444444;
               TextOut(i-ts.cx+2,88-ts.cy+2,s);
               font.Color:=$FF0000;
               TextOut(i-ts.cx,88-ts.cy,s);
              end;
      end;
      Unlock;
     end;
end;

procedure TTEx2Header.PaintC;
Var ts:TSize; i,sxOff,amoff:integer; s:String;
begin
sxOff:=FSxPng.Width;
if NOT (SectC in FShowSector) then exit;
if NOT (SectSx in FShowSector) then sxOff:=0;
with canvas do
     begin
      Lock;
      brush.Style:=bsClear;
      if FContinuousMachine then Draw(sxOff,0,FCCPng)
                            else Draw(sxOff,0,FCPng);
      case ModoMacchina of
       mmMan:s:='MAN';
       mmAuto:s:='AUT';
       mmDemo:s:='DEM';
       mmSuspend:s:='SUS';
      end;
      case FHeaderRes of
       hr640:begin
              //--------------------------------------------------------Desc1
              font:=FGeneralFont;
              font.Size:=font.Size-FS800;
              ts:=TextExtent(FProgramTxt);
              font.Color:=0;
              i:=sxOff+42;
              TextOut(i-ts.cx-1,6+(16-ts.cy) DIV 2-1,FProgramTxt);
              font.Color:=FGeneralFont.Color;
              TextOut(i-ts.cx,6+(16-ts.cy) DIV 2,FProgramTxt);
              ts:=TextExtent(FBatchIDTxt);
              font.Color:=0;
              TextOut(i-ts.cx-1,27+(16-ts.cy) DIV 2-1,FBatchIDTxt);
              font.Color:=FGeneralFont.Color;
              TextOut(i-ts.cx,27+(16-ts.cy) DIV 2,FBatchIDTxt);
              ts:=TextExtent(FFunzioniTxt);
              font.Color:=0;
              TextOut(i-ts.cx-1,48+(16-ts.cy) DIV 2-1,FFunzioniTxt);
              font.Color:=FGeneralFont.Color;
              TextOut(i-ts.cx,48+(16-ts.cy) DIV 2,FFunzioniTxt);
              //----------------------------------------------Prg e plan
              font.Color:=clwhite;
              i:=sxOff+58;
              ts:=TextExtent(FProgramma);
              TextOut(i+1,6+(16-ts.cy) DIV 2+1,FProgramma);
              font.Color:=clTExMenu2;
              TextOut(i,6+(16-ts.cy) DIV 2,FProgramma);
              font.Color:=clwhite;
              i:=sxOff+58;
              ts:=TextExtent(FBatchID);
              TextOut(i+1,27+(16-ts.cy) DIV 2+1,FBatchID);
              font.Color:=clTExMenu2;
              TextOut(i,27+(16-ts.cy) DIV 2,FBatchID);
              //----------------------------------------------Modo macchina
              font.Size:=32-FS800-2;
              ts:=TextExtent(s);
              i:=sxOff+368+(99-ts.cx) div 2;
              font.Color:=$444444;
              TextOut(i+2,30+(31-ts.cy) DIV 2+2,s);
              case ModoMacchina of
               mmMan:font.Color:=$0000BB00;
               mmAuto:font.Color:=$000000DF;
               mmDemo:font.Color:=clBlue;
               mmSuspend:font.Color:=$000080FF;
              end;
              TextOut(i,30+(31-ts.cy) DIV 2,s);
              //---------------------------------------se non frame dx prog ver
              if NOT (SectSx in FShowSector) then
              font.Size:=9-FS800;
              font.Color:=clTExMenu4;
              ts:=TextExtent(FPrgVer);
              i:=sxOff+FCPng.Width-ts.cx-60;
              TextOut(i,Height-ts.cy-6,FPrgVer);
             end;
       hr800:begin
              //--------------------------------------------------------Desc1
              font:=FGeneralFont;
              font.Size:=font.Size-FS800;
              ts:=TextExtent(FProgramTxt);
              font.Color:=0;
              i:=sxOff+42;
              TextOut(i-ts.cx-1,6+(16-ts.cy) DIV 2-1,FProgramTxt);
              font.Color:=FGeneralFont.Color;
              TextOut(i-ts.cx,6+(16-ts.cy) DIV 2,FProgramTxt);
              ts:=TextExtent(FBatchIDTxt);
              font.Color:=0;
              TextOut(i-ts.cx-1,27+(16-ts.cy) DIV 2-1,FBatchIDTxt);
              font.Color:=FGeneralFont.Color;
              TextOut(i-ts.cx,27+(16-ts.cy) DIV 2,FBatchIDTxt);
              ts:=TextExtent(FFunzioniTxt);
              font.Color:=0;
              TextOut(i-ts.cx-1,48+(16-ts.cy) DIV 2-1,FFunzioniTxt);
              font.Color:=FGeneralFont.Color;
              TextOut(i-ts.cx,48+(16-ts.cy) DIV 2,FFunzioniTxt);
              //----------------------------------------------Prg e plan
              font.Color:=clwhite;
              i:=sxOff+50;
              ts:=TextExtent(FProgramma);
              TextOut(i+1,6+(16-ts.cy) DIV 2+1,FProgramma);
              font.Color:=clTExMenu2;
              TextOut(i,6+(16-ts.cy) DIV 2,FProgramma);
              font.Color:=clwhite;
              i:=sxOff+50;
              ts:=TextExtent(FBatchID);
              TextOut(i+1,27+(16-ts.cy) DIV 2+1,FBatchID);
              font.Color:=clTExMenu2;
              TextOut(i,27+(16-ts.cy) DIV 2,FBatchID);
              //----------------------------------------------Modo macchina
              font.Size:=32-FS800-2;
              ts:=TextExtent(s);
              i:=sxOff+315+(85-ts.cx) div 2;
              font.Color:=$444444;
              TextOut(i+2,30+(31-ts.cy) DIV 2+2,s);
              case ModoMacchina of
               mmMan:font.Color:=$0000BB00;
               mmAuto:font.Color:=$000000DF;
               mmDemo:font.Color:=clBlue;
               mmSuspend:font.Color:=$000080FF;
              end;
              TextOut(i,30+(31-ts.cy) DIV 2,s);
             end;
       hr1024:begin
               //--------------------------------------------------------Desc1
               font:=FGeneralFont;
               ts:=TextExtent(FProgramTxt);
               font.Color:=0;
               i:=sxOff+52;
               TextOut(i-ts.cx-1,10+(20-ts.cy) DIV 2-1,FProgramTxt);
               font.Color:=FGeneralFont.Color;
               TextOut(i-ts.cx,10+(20-ts.cy) DIV 2,FProgramTxt);
               if NOT FContinuousMachine then
                  begin
                   ts:=TextExtent(FBatchIDTxt);
                   font.Color:=0;
                   TextOut(i-ts.cx-1,37+(20-ts.cy) DIV 2-1,FBatchIDTxt);
                   font.Color:=FGeneralFont.Color;
                   TextOut(i-ts.cx,37+(20-ts.cy) DIV 2,FBatchIDTxt);
                   ts:=TextExtent(FFunzioniTxt);
                   font.Color:=0;
                   TextOut(i-ts.cx-1,61+(20-ts.cy) DIV 2-1,FFunzioniTxt);
                   font.Color:=FGeneralFont.Color;
                   TextOut(i-ts.cx,61+(20-ts.cy) DIV 2,FFunzioniTxt);
                  end
               else
                  begin
                   ts:=TextExtent(FFunzioniTxt);
                   font.Color:=0;
                   TextOut(i-ts.cx-1,37+(20-ts.cy) DIV 2-1,FFunzioniTxt);
                   font.Color:=FGeneralFont.Color;
                   TextOut(i-ts.cx,37+(20-ts.cy) DIV 2,FFunzioniTxt);
                  end;
               //----------------------------------------------Prg e plan
               font.Color:=clwhite;
               i:=sxOff+65;
               ts:=TextExtent(FProgramma);
               TextOut(i+1,10+(20-ts.cy) DIV 2+1,FProgramma);
               font.Color:=clTExMenu2;
               TextOut(i,10+(20-ts.cy) DIV 2,FProgramma);
               font.Color:=clwhite;
               i:=sxOff+65;
               if NOT ContinuousMachine then
                  begin
                   ts:=TextExtent(FBatchID);
                   TextOut(i+1,37+(20-ts.cy) DIV 2+1,FBatchID);
                   font.Color:=clTExMenu2;
                   TextOut(i,37+(20-ts.cy) DIV 2,FBatchID);
                  end;
               //----------------------------------------------Modo macchina
               if FContinuousMachine then amoff:=130
                                     else amoff:=0;
               font.Size:=32;
               ts:=TextExtent(s);
               i:=sxOff+402+(110-ts.cx) div 2;
               font.Color:=$444444;
               TextOut(i+2+amoff,37+(43-ts.cy) DIV 2+2,s);
               case ModoMacchina of
                mmMan:font.Color:=$0000BB00;
                mmAuto:font.Color:=$000000DF;
                mmDemo:font.Color:=clBlue;
                mmSuspend:font.Color:=$000080FF;
               end;
               TextOut(i+amoff,37+(43-ts.cy) DIV 2,s);
              end;
       hr1280:begin
               //--------------------------------------------------------Desc1
               font:=FGeneralFont;
               ts:=TextExtent(FProgramTxt);
               font.Color:=0;
               i:=sxOff+52;
               TextOut(i-ts.cx-1,10+(20-ts.cy) DIV 2-1,FProgramTxt);
               font.Color:=FGeneralFont.Color;
               TextOut(i-ts.cx,10+(20-ts.cy) DIV 2,FProgramTxt);
               if NOT FContinuousMachine then
                  begin
                   ts:=TextExtent(FBatchIDTxt);
                   font.Color:=0;
                   TextOut(i-ts.cx-1,37+(20-ts.cy) DIV 2-1,FBatchIDTxt);
                   font.Color:=FGeneralFont.Color;
                   TextOut(i-ts.cx,37+(20-ts.cy) DIV 2,FBatchIDTxt);
                   ts:=TextExtent(FFunzioniTxt);
                   font.Color:=0;
                   TextOut(i-ts.cx-1,61+(20-ts.cy) DIV 2-1,FFunzioniTxt);
                   font.Color:=FGeneralFont.Color;
                   TextOut(i-ts.cx,61+(20-ts.cy) DIV 2,FFunzioniTxt);
                  end
               else
                  begin
                   ts:=TextExtent(FFunzioniTxt);
                   font.Color:=0;
                   TextOut(i-ts.cx-1,37+(20-ts.cy) DIV 2-1,FFunzioniTxt);
                   font.Color:=FGeneralFont.Color;
                   TextOut(i-ts.cx,37+(20-ts.cy) DIV 2,FFunzioniTxt);
                  end;
               //----------------------------------------------Prg e plan
               font.Color:=clwhite;
               i:=sxOff+65;
               ts:=TextExtent(FProgramma);
               TextOut(i+1,10+(20-ts.cy) DIV 2+1,FProgramma);
               font.Color:=clTExMenu2;
               TextOut(i,10+(20-ts.cy) DIV 2,FProgramma);
               font.Color:=clwhite;
               i:=sxOff+65;
               if NOT ContinuousMachine then
                  begin
                   ts:=TextExtent(FBatchID);
                   TextOut(i+1,37+(20-ts.cy) DIV 2+1,FBatchID);
                   font.Color:=clTExMenu2;
                   TextOut(i,37+(20-ts.cy) DIV 2,FBatchID);
                  end;
               //----------------------------------------------Modo macchina
               font.Size:=32;
               ts:=TextExtent(s);
               i:=sxOff+642+(110-ts.cx) div 2;
               font.Color:=$444444;
               TextOut(i+2,37+(43-ts.cy) DIV 2+2,s);
               case ModoMacchina of
                mmMan:font.Color:=$0000BB00;
                mmAuto:font.Color:=$000000DF;
                mmDemo:font.Color:=clBlue;
                mmSuspend:font.Color:=$000080FF;
               end;
               TextOut(i,37+(43-ts.cy) DIV 2,s);
              end;
      end;
      Unlock;
     end;
PaintFuncAtt;
PaintAlarms;
PaintMS;
SetLedPos;
end;

procedure TTEx2Header.PaintFuncNO(X,Y:Integer;ft:String);
begin
with canvas do
     begin
      brush.Style:=bsSolid; brush.Color:=clTExMenu3;
      font.Color:=$444444;
      TextRect(Rect(x,y,x+Trunc(font.Size*1.8),y+Trunc(font.Size*1.5)),x,y-2,ft);
      font.Color:=clRed;
      brush.Style:=bsClear;
      TextOut(x+1,y-3,ft);
     end;
end;

procedure TTEx2Header.PaintFuncAtt;
CONST FOFF1024=34; FOFF800=26.5; FOFF640=31;
Var i,sxOff,Off:integer;
begin
sxOff:=FSxPng.Width;
if NOT (SectC in FShowSector) then exit;
if NOT (SectSx in FShowSector) then sxOff:=0;
with canvas do
     begin
      Lock;
      case FHeaderRes of
       hr640:Begin
              font:=FGeneralFont;
              font.Size:=font.Size-1-FS800;
              //Func1-------
              i:=sxOff+63;
              PaintFuncNO(i,51,FFunzione1);
              //Func2-------
              i:=Round(sxOff+63+FOFF640*1);
              PaintFuncNO(i,51,FFunzione2);
              //Func3-------
              i:=Round(sxOff+63+FOFF640*2);
              PaintFuncNO(i,51,FFunzione3);
              //Func4-------
              i:=Round(sxOff+63+FOFF640*3);
              PaintFuncNO(i,51,FFunzione4);
              //Func5-------
              i:=Round(sxOff+63+FOFF640*4);
              PaintFuncNO(i,51,FFunzione5);
              //Func6-------
              i:=Round(sxOff+63+FOFF640*5);
              PaintFuncNO(i,51,FFunzione6);
              //Func7-------
              i:=Round(sxOff+63+FOFF640*6);
              PaintFuncNO(i,51,FFunzione7);
              //Func8-------
              i:=Round(sxOff+63+FOFF640*7);
              PaintFuncNO(i,51,FFunzione8);
              //Func9-------
              i:=Round(sxOff+63+FOFF640*8);
              PaintFuncNO(i,51,FFunzione9);
              //Func10-------
              i:=Round(sxOff+63+FOFF640*9);
              PaintFuncNO(i,51,FFunzione10);
             end;
       hr800:begin
              font:=FGeneralFont;
              font.Size:=font.Size-1-FS800;
              //Func1-------
              i:=sxOff+52;
              PaintFuncNO(i,51,FFunzione1);
              //Func2-------
              i:=Round(sxOff+52+FOFF800*1);
              PaintFuncNO(i,51,FFunzione2);
              //Func3-------
              i:=Round(sxOff+52+FOFF800*2);
              PaintFuncNO(i,51,FFunzione3);
              //Func4-------
              i:=Round(sxOff+52+FOFF800*3);
              PaintFuncNO(i,51,FFunzione4);
              //Func5-------
              i:=Round(sxOff+52+FOFF800*4);
              PaintFuncNO(i,51,FFunzione5);
              //Func6-------
              i:=Round(sxOff+52+FOFF800*5);
              PaintFuncNO(i,51,FFunzione6);
              //Func7-------
              i:=Round(sxOff+52+FOFF800*6);
              PaintFuncNO(i,51,FFunzione7);
              //Func8-------
              i:=Round(sxOff+52+FOFF800*7);
              PaintFuncNO(i,51,FFunzione8);
              //Func9-------
              i:=Round(sxOff+52+FOFF800*8);
              PaintFuncNO(i,51,FFunzione9);
              //Func10-------
              i:=Round(sxOff+52+FOFF800*9);
              PaintFuncNO(i,51,FFunzione10);
             end;
       hr1024,
       hr1280:begin
               if FContinuousMachine then Off:=-27
                                     else Off:=0;
               font:=FGeneralFont;
               font.Size:=font.Size-1;
               //Func1-------
               i:=sxOff+66;
               PaintFuncNO(i,66+Off,FFunzione1);
               //Func2-------
               i:=sxOff+66+FOFF1024*1;
               PaintFuncNO(i,66+Off,FFunzione2);
               //Func3-------
               i:=sxOff+66+FOFF1024*2;
               PaintFuncNO(i,66+Off,FFunzione3);
               //Func4-------
               i:=sxOff+66+FOFF1024*3;
               PaintFuncNO(i,66+Off,FFunzione4);
               //Func5-------
               i:=sxOff+66+FOFF1024*4;
               PaintFuncNO(i,66+Off,FFunzione5);
               //Func6-------
               i:=sxOff+66+FOFF1024*5;
               PaintFuncNO(i,66+Off,FFunzione6);
               //Func7-------
               i:=sxOff+66+FOFF1024*6;
               PaintFuncNO(i,66+Off,FFunzione7);
               //Func8-------
               i:=sxOff+66+FOFF1024*7;
               PaintFuncNO(i,66+Off,FFunzione8);
               //Func9-------
               i:=sxOff+66+FOFF1024*8;
               PaintFuncNO(i,66+Off,FFunzione9);
               //Func10-------
               i:=sxOff+66+FOFF1024*9;
               PaintFuncNO(i,66+Off,FFunzione10);
               if FContinuousMachine then
                  begin //da 11 a 28
                   //Func11-------
                   i:=sxOff+66+FOFF1024*10;
                   PaintFuncNO(i,66+Off,FFunzione11);
                   //Func12-------
                   i:=sxOff+66+FOFF1024*11;
                   PaintFuncNO(i,66+Off,FFunzione12);
                   //Func13-------
                   i:=sxOff+66+FOFF1024*12;
                   PaintFuncNO(i,66+Off,FFunzione13);
                   //Func14-------
                   i:=sxOff+66+FOFF1024*13;
                   PaintFuncNO(i,66+Off,FFunzione14);
                   case  FHeaderRes of
                    hr1024:Begin
                           //Func15-------
                           i:=sxOff+66;
                           PaintFuncNO(i,66,FFunzione15);
                           //Func16-------
                           i:=sxOff+66+FOFF1024*1;
                           PaintFuncNO(i,66,FFunzione16);
                           //Func17-------
                           i:=sxOff+66+FOFF1024*2;
                           PaintFuncNO(i,66,FFunzione17);
                           //Func18-------
                           i:=sxOff+66+FOFF1024*3;
                           PaintFuncNO(i,66,FFunzione18);
                           //Func19-------
                           i:=sxOff+66+FOFF1024*4;
                           PaintFuncNO(i,66,FFunzione19);
                           //Func20-------
                           i:=sxOff+66+FOFF1024*5;
                           PaintFuncNO(i,66,FFunzione20);
                           //Func21-------
                           i:=sxOff+66+FOFF1024*6;
                           PaintFuncNO(i,66,FFunzione21);
                           //Func22-------
                           i:=sxOff+66+FOFF1024*7;
                           PaintFuncNO(i,66,FFunzione22);
                           //Func23-------
                           i:=sxOff+66+FOFF1024*8;
                           PaintFuncNO(i,66,FFunzione23);
                           //Func24-------
                           i:=sxOff+66+FOFF1024*9;
                           PaintFuncNO(i,66,FFunzione24);
                           //Func25-------
                           i:=sxOff+66+FOFF1024*10;
                           PaintFuncNO(i,66,FFunzione25);
                           //Func26-------
                           i:=sxOff+66+FOFF1024*11;
                           PaintFuncNO(i,66,FFunzione26);
                           //Func27-------
                           i:=sxOff+66+FOFF1024*12;
                           PaintFuncNO(i,66,FFunzione27);
                           //Func28-------
                           i:=sxOff+66+FOFF1024*13;
                           PaintFuncNO(i,66,FFunzione28);
                          end;
                    hr1280:Begin
                           //Func15-------
                           i:=sxOff+66+FOFF1024*14;
                           PaintFuncNO(i,66+Off,FFunzione15);
                           //Func16-------
                           i:=sxOff+66+FOFF1024*15;
                           PaintFuncNO(i,66+Off,FFunzione16);
                           //Func17-------
                           i:=sxOff+66+FOFF1024*16;
                           PaintFuncNO(i,66+Off,FFunzione17);
                           //Func18-------
                           i:=sxOff+66+FOFF1024*0;
                           PaintFuncNO(i,66,FFunzione18);
                           //Func19-------
                           i:=sxOff+66+FOFF1024*1;
                           PaintFuncNO(i,66,FFunzione19);
                           //Func20-------
                           i:=sxOff+66+FOFF1024*2;
                           PaintFuncNO(i,66,FFunzione20);
                           //Func21-------
                           i:=sxOff+66+FOFF1024*3;
                           PaintFuncNO(i,66,FFunzione21);
                           //Func22-------
                           i:=sxOff+66+FOFF1024*4;
                           PaintFuncNO(i,66,FFunzione22);
                           //Func23-------
                           i:=sxOff+66+FOFF1024*5;
                           PaintFuncNO(i,66,FFunzione23);
                           //Func24-------
                           i:=sxOff+66+FOFF1024*6;
                           PaintFuncNO(i,66,FFunzione24);
                           //Func25-------
                           i:=sxOff+66+FOFF1024*7;
                           PaintFuncNO(i,66,FFunzione25);
                           //Func26-------
                           i:=sxOff+66+FOFF1024*8;
                           PaintFuncNO(i,66,FFunzione26);
                           //Func27-------
                           i:=sxOff+66+FOFF1024*9;
                           PaintFuncNO(i,66,FFunzione27);
                           //Func28-------
                           i:=sxOff+66+FOFF1024*10;
                           PaintFuncNO(i,66,FFunzione28);
                           //Func29-------
                           i:=sxOff+66+FOFF1024*11;
                           PaintFuncNO(i,66,FFunzione29);
                           //Func30-------
                           i:=sxOff+66+FOFF1024*12;
                           PaintFuncNO(i,66,FFunzione30);
                           //Func31-------
                           i:=sxOff+66+FOFF1024*13;
                           PaintFuncNO(i,66,FFunzione31);
                           //Func32-------
                           i:=sxOff+66+FOFF1024*14;
                           PaintFuncNO(i,66,FFunzione32);
                           //Func33-------
                           i:=sxOff+66+FOFF1024*15;
                           PaintFuncNO(i,66,FFunzione33);
                           //Func34-------
                           i:=sxOff+66+FOFF1024*16;
                           PaintFuncNO(i,66,FFunzione34);
                          end;
                   end;
              end;
      end;
      end;
      Unlock;
     end;
end;

procedure TTEx2Header.PaintAlarms;
Var i,sxOff:integer;
begin
sxOff:=FSxPng.Width;
if NOT (SectC in FShowSector) then exit;
if NOT (SectSx in FShowSector) then sxOff:=0;
with canvas do
     begin
      Lock;
      font:=FGeneralFont;
      font.Color:=clRED;
      brush.Style:=bsSolid;
      brush.Color:=clTExMenu3;
      case FHeaderRes of
       hr640:begin
              font.Size:=font.Size-FS800;
              i:=sxOff+368;
              TextRect(Rect(i,8,i+210,22),i+1,8-2,FAllarmi);
             end;
       hr800:begin
              font.Size:=font.Size-FS800;
              i:=sxOff+315;
              TextRect(Rect(i,8,i+180,22),i+1,8-2,FAllarmi);
             end;
       hr1024:begin
               i:=sxOff+404;
               TextRect(Rect(i,11,i+225,27),i+1,11-2,FAllarmi);
              end;
       hr1280:begin
               i:=sxOff+472;
               TextRect(Rect(i,11,i+420,27),i+1,11-2,FAllarmi);
              end;
      end;
      Unlock;
     end;
end;

procedure TTEx2Header.PaintDx;
Var ts:TSize; i,sxOff,cOff:integer;
begin
if NOT (SectDx in FShowSector) then exit;
sxOff:=FSxPng.Width;
if FContinuousMachine then cOff:=FCCPng.Width
                      else cOff:=FCPng.Width;
if NOT (SectSx in FShowSector) then sxOff:=0;
if NOT (SectC in FShowSector) then cOff:=0;
with canvas do
     begin
      Lock;
      brush.Style:=bsClear;
      Draw(sxOff+cOff,0,FDxPng);
      font:=FGeneralFont;
      font.Color:=clYellow;
      i:=sxOff+cOff+FDxPng.Width;
      case FHeaderRes of
       hr640,
       hr800:begin
              font.Size:=9-FS800;
              ts:=TextExtent(FPrgVer);
              TextOut(i-ts.cx-1,30,FPrgVer);
             end;
       hr1024:begin
              Draw(width-FDxFPng.Width,0,FDxFPng);
              if width>1024 then
                 StretchDraw(Rect(sxOff+cOff+FDxPng.Width,0,width-FDxFPng.Width,height),FDxRPng);
              font.Size:=9;
              ts:=TextExtent(FPrgVer);
              TextOut(i-ts.cx-1,38,FPrgVer);
              end;
       hr1280:begin
              Draw(width-FDxFPng.Width,0,FDxFPng);
              if width>1280 then
                 StretchDraw(Rect(sxOff+cOff+FDxPng.Width,0,width-FDxFPng.Width,height),FDxRPng);
              font.Size:=9;
              ts:=TextExtent(FPrgVer);
              TextOut(i-ts.cx-1,38,FPrgVer);
              end;
      end;
      Unlock;
     end;
end;

procedure TTEx2Header.SetAutoDim(const Value: Boolean);
begin
if FAutoDim=Value then exit;
FAutoDim:=Value;
if (csDesigning in ComponentState) then invalidate;
end;

procedure TTEx2Header.SetBatchIDTxt(const Value: String);
begin
if Value=FBatchIDTxt then exit;
FBatchIDTxt:=Value;
PaintC;
InvalidateLed;
end;

procedure TTEx2Header.SetClockFont(const Value: TFont);
begin
FClockFont.Assign(Value);
end;

procedure TTEx2Header.SetDateFont(const Value: TFont);
begin
FDateFont.Assign(Value);
end;

procedure TTEx2Header.SetGeneralFont(const Value: TFont);
begin
FGeneralFont.Assign(Value);
FLedAL.Font.Assign(Value); FLedOp.Font.Assign(Value);
FLedSup.Font.Assign(Value); FLedPlc.Font.Assign(Value);
end;

procedure TTEx2Header.SetFunzioniTxt(const Value: String);
begin
if Value=FFunzioniTxt then exit;
FFunzioniTxt:=Value;
PaintC;
InvalidateLed;
end;

procedure TTEx2Header.SetHeaderRes(const Value: THeaderRes);
begin
if FHeaderRes=Value then exit;
FHeaderRes:=Value;
try LoadPNG; finally end;
FLedAL.Font.Size:=FGeneralFont.Size-FS800;
FLedOp.Font.Size:=FGeneralFont.Size-FS800;
FLedSup.Font.Size:=FGeneralFont.Size-FS800;
FLedPlc.Font.Size:=FGeneralFont.Size-FS800;
DoAutoDim;
Invalidate;
end;

procedure TTEx2Header.SetPassiTot(const Value: Integer);
begin
if Value=FPassiTot then exit;
FPassiTot:=Value;
PaintSx;
end;

procedure TTEx2Header.SetPasso(const Value: Integer);
begin
if Value=FPasso then exit;
FPasso:=Value;
PaintSx;
end;

procedure TTEx2Header.SetPassoTxt(const Value: String);
begin
if Value=FPassoTxt then exit;
FPassoTxt:=Value;
PaintSx;
end;

procedure TTEx2Header.SetProgramTxt(const Value: String);
begin
if Value=FProgramTxt then exit;
FProgramTxt:=Value;
PaintC;
InvalidateLed;
end;

procedure TTEx2Header.SomethingChange(Sender: TObject);
begin
if FGeneralFont<>NIL then
   begin
    FLedAL.Font.assign(FGeneralFont); FLedOp.Font.assign(FGeneralFont);
    FLedSup.Font.assign(FGeneralFont); FLedPlc.Font.assign(FGeneralFont);
   end;
Invalidate;
end;

procedure TTEx2Header.SetFunzione1(const Value: String);
begin
if Value=FFunzione1 then exit;
FFunzione1:=Value;
PaintFuncAtt;
end;

procedure TTEx2Header.SetFunzione2(const Value: String);
begin
if Value=FFunzione2 then exit;
FFunzione2:=Value;
PaintFuncAtt;
end;

procedure TTEx2Header.SetFunzione3(const Value: String);
begin
if Value=FFunzione3 then exit;
FFunzione3:=Value;
PaintFuncAtt;
end;

procedure TTEx2Header.SetFunzione4(const Value: String);
begin
if Value=FFunzione4 then exit;
FFunzione4:=Value;
PaintFuncAtt;
end;

procedure TTEx2Header.SetFunzione5(const Value: String);
begin
if Value=FFunzione5 then exit;
FFunzione5:=Value;
PaintFuncAtt;
end;

procedure TTEx2Header.SetFunzione6(const Value: String);
begin
if Value=FFunzione6 then exit;
FFunzione6:=Value;
PaintFuncAtt;
end;

procedure TTEx2Header.SetFunzione7(const Value: String);
begin
if Value=FFunzione7 then exit;
FFunzione7:=Value;
PaintFuncAtt;
end;

procedure TTEx2Header.SetFunzione8(const Value: String);
begin
if Value=FFunzione8 then exit;
FFunzione8:=Value;
PaintFuncAtt;
end;

procedure TTEx2Header.SetFunzione9(const Value: String);
begin
if Value=FFunzione9 then exit;
FFunzione9:=Value;
PaintFuncAtt;
end;

procedure TTEx2Header.SetFunzione10(const Value: String);
begin
if Value=FFunzione10 then exit;
FFunzione10:=Value;
PaintFuncAtt;
end;

procedure TTEx2Header.SetFunzione11(const Value: String);
begin
if Value=FFunzione11 then exit;
FFunzione11:=Value;
PaintFuncAtt;
end;

procedure TTEx2Header.SetFunzione12(const Value: String);
begin
if Value=FFunzione12 then exit;
FFunzione12:=Value;
PaintFuncAtt;
end;

procedure TTEx2Header.SetFunzione13(const Value: String);
begin
if Value=FFunzione13 then exit;
FFunzione13:=Value;
PaintFuncAtt;
end;

procedure TTEx2Header.SetFunzione14(const Value: String);
begin
if Value=FFunzione14 then exit;
FFunzione14:=Value;
PaintFuncAtt;
end;

procedure TTEx2Header.SetFunzione15(const Value: String);
begin
if Value=FFunzione15 then exit;
FFunzione15:=Value;
PaintFuncAtt;
end;

procedure TTEx2Header.SetFunzione16(const Value: String);
begin
if Value=FFunzione16 then exit;
FFunzione16:=Value;
PaintFuncAtt;
end;

procedure TTEx2Header.SetFunzione17(const Value: String);
begin
if Value=FFunzione17 then exit;
FFunzione17:=Value;
PaintFuncAtt;
end;

procedure TTEx2Header.SetFunzione18(const Value: String);
begin
if Value=FFunzione18 then exit;
FFunzione18:=Value;
PaintFuncAtt;
end;

procedure TTEx2Header.SetFunzione19(const Value: String);
begin
if Value=FFunzione19 then exit;
FFunzione19:=Value;
PaintFuncAtt;
end;

procedure TTEx2Header.SetFunzione20(const Value: String);
begin
if Value=FFunzione20 then exit;
FFunzione20:=Value;
PaintFuncAtt;
end;

procedure TTEx2Header.SetFunzione21(const Value: String);
begin
if Value=FFunzione21 then exit;
FFunzione21:=Value;
PaintFuncAtt;
end;

procedure TTEx2Header.SetFunzione22(const Value: String);
begin
if Value=FFunzione22 then exit;
FFunzione22:=Value;
PaintFuncAtt;
end;

procedure TTEx2Header.SetFunzione23(const Value: String);
begin
if Value=FFunzione23 then exit;
FFunzione23:=Value;
PaintFuncAtt;
end;

procedure TTEx2Header.SetFunzione24(const Value: String);
begin
if Value=FFunzione24 then exit;
FFunzione24:=Value;
PaintFuncAtt;
end;

procedure TTEx2Header.SetFunzione25(const Value: String);
begin
if Value=FFunzione25 then exit;
FFunzione25:=Value;
PaintFuncAtt;
end;

procedure TTEx2Header.SetFunzione26(const Value: String);
begin
if Value=FFunzione26 then exit;
FFunzione26:=Value;
PaintFuncAtt;
end;

procedure TTEx2Header.SetFunzione27(const Value: String);
begin
if Value=FFunzione27 then exit;
FFunzione27:=Value;
PaintFuncAtt;
end;

procedure TTEx2Header.SetFunzione28(const Value: String);
begin
if Value=FFunzione28 then exit;
FFunzione28:=Value;
PaintFuncAtt;
end;

procedure TTEx2Header.SetFunzione29(const Value: String);
begin
if Value=FFunzione29 then exit;
FFunzione29:=Value;
PaintFuncAtt;
end;

procedure TTEx2Header.SetFunzione30(const Value: String);
begin
if Value=FFunzione30 then exit;
FFunzione30:=Value;
PaintFuncAtt;
end;

procedure TTEx2Header.SetFunzione31(const Value: String);
begin
if Value=FFunzione31 then exit;
FFunzione31:=Value;
PaintFuncAtt;
end;

procedure TTEx2Header.SetFunzione32(const Value: String);
begin
if Value=FFunzione32 then exit;
FFunzione32:=Value;
PaintFuncAtt;
end;

procedure TTEx2Header.SetFunzione33(const Value: String);
begin
if Value=FFunzione33 then exit;
FFunzione33:=Value;
PaintFuncAtt;
end;

procedure TTEx2Header.SetFunzione34(const Value: String);
begin
if Value=FFunzione34 then exit;
FFunzione34:=Value;
PaintFuncAtt;
end;

procedure TTEx2Header.SetAllarmi(const Value: String);
begin
if Value=FAllarmi then exit;
FAllarmi:=Value;
PaintAlarms;
end;

procedure TTEx2Header.SetBatchID(const Value: String);
begin
if Value=FBatchID then exit;
FBatchID:=Value;
PaintC;
InvalidateLed;
end;

procedure TTEx2Header.SetProgramma(const Value: String);
begin
if Value=FProgramma then exit;
FProgramma:=Value;
PaintC;
InvalidateLed;
end;

procedure TTEx2Header.SetPrgVer(const Value: String);
begin
if Value=FPrgVer then exit;
FPrgVer:=Value;
PaintDx;
end;

procedure TTEx2Header.SetModoMacchina(const Value: TModoMacchina);
begin
if Value=FModoMacchina then exit;
FModoMacchina:=Value;
PaintC;
InvalidateLed;
end;

procedure TTEx2Header.SetFunction(nFunc: Integer; ValFunc: String);
begin
case nFunc of
  1:Funzione1 :=ValFunc;
  2:Funzione2 :=ValFunc;
  3:Funzione3 :=ValFunc;
  4:Funzione4 :=ValFunc;
  5:Funzione5 :=ValFunc;
  6:Funzione6 :=ValFunc;
  7:Funzione7 :=ValFunc;
  8:Funzione8 :=ValFunc;
  9:Funzione9 :=ValFunc;
 10:Funzione10:=ValFunc;
 11:Funzione11:=ValFunc;
 12:Funzione12:=ValFunc;
 13:Funzione13:=ValFunc;
 14:Funzione14:=ValFunc;
 15:Funzione15:=ValFunc;
 16:Funzione16:=ValFunc;
 17:Funzione17:=ValFunc;
 18:Funzione18:=ValFunc;
 19:Funzione19:=ValFunc;
 20:Funzione20:=ValFunc;
 21:Funzione21:=ValFunc;
 22:Funzione22:=ValFunc;
 23:Funzione23:=ValFunc;
 24:Funzione24:=ValFunc;
 25:Funzione25:=ValFunc;
 26:Funzione26:=ValFunc;
 27:Funzione27:=ValFunc;
 28:Funzione28:=ValFunc;
 29:Funzione29:=ValFunc;
 30:Funzione30:=ValFunc;
 31:Funzione31:=ValFunc;
 32:Funzione32:=ValFunc;
 33:Funzione33:=ValFunc;
 34:Funzione34:=ValFunc;
end;
end;

procedure TTEx2Header.SetLedAlCol(const Value: TLedColor);
begin
FLedAlCol:=Value;
FLedAl.LedColor:=FLedAlCol;
end;

procedure TTEx2Header.SetLedOpCol(const Value: TLedColor);
begin
FLedOpCol:=Value;
FLedOp.LedColor:=FLedOpCol;
end;

procedure TTEx2Header.SetLedPlcCol(const Value: TLedColor);
begin
FLedPlcCol:=Value;
FLedPlc.LedColor:=FLedPlcCol;
end;

procedure TTEx2Header.SetLedSupCol(const Value: TLedColor);
begin
FLedSupCol:=Value;
FLedSup.LedColor:=FLedSupCol;
end;

procedure TTEx2Header.SetLedPos;
Var sxOff:Integer;
begin
sxOff:=FSxPng.Width;
if NOT (SectSx in FShowSector) then sxOff:=0;
case FHeaderRes of
 hr640:begin
        FLedAL.Left:=sxOff+586; FLedAL.top:=4;
        FLedOp.Left:=sxOff+586; FLedOp.top:=20;
        FLedSup.Left:=sxOff+586; FLedSup.top:=36;
        FLedPlc.Left:=sxOff+586; FLedPlc.top:=52;
       end;
 hr800:begin
        FLedAL.Left:=sxOff+502; FLedAL.top:=4;
        FLedOp.Left:=sxOff+502; FLedOp.top:=20;
        FLedSup.Left:=sxOff+502; FLedSup.top:=36;
        FLedPlc.Left:=sxOff+502; FLedPlc.top:=52;
       end;
 hr1024:begin
         FLedAL.Left:=sxOff+643; FLedAL.top:=7;
         FLedOp.Left:=sxOff+643; FLedOp.top:=27;
         FLedSup.Left:=sxOff+643; FLedSup.top:=47;
         FLedPlc.Left:=sxOff+643; FLedPlc.top:=67;
        end;
 hr1280:begin
         FLedAL.Left:=sxOff+901; FLedAL.top:=7;
         FLedOp.Left:=sxOff+901; FLedOp.top:=27;
         FLedSup.Left:=sxOff+901; FLedSup.top:=47;
         FLedPlc.Left:=sxOff+901; FLedPlc.top:=67;
        end;
end;
end;

procedure TTEx2Header.SetLedAlAcceso(const Value: Boolean);
begin
FLedAlAcceso:=Value;
FLedAl.Acceso:=Value;
end;

procedure TTEx2Header.SetLedOpAcceso(const Value: Boolean);
begin
FLedOpAcceso:=Value;
FLedOp.Acceso:=Value;
end;

procedure TTEx2Header.SetLedPlcAcceso(const Value: Boolean);
begin
FLedPlcAcceso:=Value;
FLedPlc.Acceso:=Value;
end;

procedure TTEx2Header.SetLedSupAcceso(const Value: Boolean);
begin
FLedSupAcceso:=Value;
FLedSup.Acceso:=Value;
end;

procedure TTEx2Header.SetMasterSlaveStatus(const Value: byte);
begin
if Value=FMasterSlaveStatus then exit;
if (Value<1) OR (Value>6) then exit;
FMasterSlaveStatus:=Value;
PaintMS;
end;

procedure TTEx2Header.PaintMS;
Var ts:TSize; s:String; i,sxOff:integer;
begin
sxOff:=FSxPng.Width;
if NOT (SectC in FShowSector) then exit;
if NOT (SectSx in FShowSector) then sxOff:=0;
case FMasterSlaveStatus of
 1:s:='SINGLE';
 2,4:s:='MASTER';
 3,5:s:='SLAVE';
 6:s:='COM ERR';
end;
with canvas do
     begin
      Lock;
      brush.Style:=bsSolid;
      brush.Color:=clTExMenu3;
      font:=FGeneralFont;
      font.Color:=0;
      case FHeaderRes of
       hr640:begin
              font.Size:=9-FS800;
              i:=sxOff+479;
              StretchDraw(Rect(i,30,
                               i+Round(MasterSlaveBmp[1].Width*0.76),
                               30+Round(MasterSlaveBmp[1].Height*0.76)),MasterSlaveBmp[FMasterSlaveStatus]);
              ts:=TextExtent(s);
              font.Color:=clWhite;
              TextOut(i+MasterSlaveBmp[1].width+2+1,21+(34-ts.cy) DIV 2+1,s);
              font.Color:=clTExMenu2;
              brush.Style:=bsClear;
              TextOut(i+MasterSlaveBmp[1].width+2,21+(34-ts.cy) DIV 2,s);
             end;
       hr800:begin
              font.Size:=9-FS800;
              i:=sxOff+410;
              StretchDraw(Rect(i,30,
                               i+Round(MasterSlaveBmp[1].Width*0.76),
                               30+Round(MasterSlaveBmp[1].Height*0.76)),MasterSlaveBmp[FMasterSlaveStatus]);
              ts:=TextExtent(s);
              font.Color:=clWhite;
              TextOut(i+MasterSlaveBmp[1].width+2+1,30+(34-ts.cy) DIV 2+1,s);
              font.Color:=clTExMenu2;
              brush.Style:=bsClear;
              TextOut(i+MasterSlaveBmp[1].width+2,30+(34-ts.cy) DIV 2,s);
             end;
       hr1024:begin
               if FContinuousMachine then begin unlock; exit; end;
               font.Size:=Trunc(font.Size*66/canvas.TextWidth(s));
               i:=sxOff+526;
               Draw(i,38,MasterSlaveBmp[FMasterSlaveStatus]);
               ts:=TextExtent(s);
               font.Color:=clWhite;
               TextOut(i+MasterSlaveBmp[1].width+2+1,38+(42-ts.cy) DIV 2+1,s);
               font.Color:=clTExMenu2;
               brush.Style:=bsClear;
               TextOut(i+MasterSlaveBmp[1].width+2,38+(42-ts.cy) DIV 2,s);
              end;
       hr1280:begin
               {font.Size:=Trunc(font.Size*66/canvas.TextWidth(s));
               i:=sxOff+772;
               Draw(i,38,MasterSlaveBmp[FMasterSlaveStatus]);
               ts:=TextExtent(s);
               font.Color:=clWhite;
               TextOut(i+MasterSlaveBmp[1].width+2+1,38+(42-ts.cy) DIV 2+1,s);
               font.Color:=clTExMenu2;
               brush.Style:=bsClear;
               TextOut(i+MasterSlaveBmp[1].width+2,38+(42-ts.cy) DIV 2,s);}
              end;
      end;
      Unlock;
     end;
end;

procedure TTEx2Header.UpdateClock;
Var ts:TSize; r:Trect;
begin
FClock:=TimeToStr(Now);
FDate:=DateToStr(Now);
if FHeaderRes<>hr640 then
   if NOT (SectSx in FShowSector) then exit;
if NOT visible then exit;
try
with canvas do
     begin
      lock;
      brush.Style:=bsSolid;
      brush.Color:=clTExMenu1;
      case FHeaderRes of
       hr640:begin
              //--------------------------------------------------------OROLOGIO
              font.Size:=8;
              font.Color:=clTExMenu4;
              brush.Style:=bsSolid;
              brush.Color:=clTExMenu3;
              ts:=TextExtent(FClock);
              r.Left:=491+MasterSlaveBmp[1].width+2;
              r.Top:=13+(74-ts.cy) DIV 2;
              r.Right:=r.Left+ts.cx;
              r.Bottom:=r.Top+ts.cy-3;
              TextRect(r,r.Left,r.Top,FClock);
             end;
       hr800:begin
              //--------------------------------------------------------OROLOGIO
              font:=FClockFont;
              font.Size:=font.Size-FS800;
              ts:=TextExtent(FClock);
              TextOut((79-ts.cx) DIV 2,(22-ts.cy) DIV 2,FClock); //6,6 96,22
              //--------------------------------------------------------DATA
              font:=FDateFont;
              font.Size:=font.Size-FS800;
              ts:=TextExtent(FDate);
              TextOut(78+(79-ts.cx) DIV 2,(22-ts.cy) DIV 2,FDate); //106,6 196,22
             end;
       hr1024,
       hr1280:begin
               //--------------------------------------------------------OROLOGIO
               font:=FClockFont;
               ts:=TextExtent(FClock);
               TextOut((101-ts.cx) DIV 2,(28-ts.cy) DIV 2,FClock); //6,6 96,22
               //--------------------------------------------------------DATA
               font:=FDateFont;
               ts:=TextExtent(FDate);
               TextOut(100+(101-ts.cx) DIV 2,(28-ts.cy) DIV 2,FDate); //106,6 196,22
              end;
      end;
      Unlock;
     end;
finally
end;
end;

procedure TTEx2Header.InvalidateLed;
begin
FLedAL.Invalidate;
FLedOP.Invalidate;
FLedSUP.Invalidate;
FLedPLC.Invalidate;
end;

procedure TTEx2Header.SetShowSector(const Value: TShowSector);
begin
FShowSector:=Value;
FLedAL.Visible:=SectC in FShowSector;
FLedOP.Visible:=SectC in FShowSector;
FLedSUP.Visible:=SectC in FShowSector;
FLedPLC.Visible:=SectC in FShowSector;
if visible then
   with canvas do
        begin
         brush.Color:=clTExMenu3;
         pen.Style:=psClear;
         rectangle(cliprect);
        end;
Invalidate;
end;

procedure TTEx2Header.SetContinuousMachine(const Value: Boolean);
begin
FContinuousMachine:=Value;
Invalidate;
end;

end.
