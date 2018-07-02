unit ComEx;

interface

uses Windows, SysUtils, Classes, StdCtrls, SocketUnit;

type
  TSTOPBIT=(ONESTOPBIT,ONE5STOPBITS,TWOSTOPBITS);
  TNetMode=(nmNone,nmMaster,nmSlave,nmMTCPIP,nmSTCPIP);

  TLineEnd=Procedure(Line:AnsiString) of object;

  TComEx = class(TComponent)
  private
    ParamCom:TDCB;
    ComH:THandle;
    FLastString:String;
    //-----------------------
    FByteSize: Integer;
    FPorta: Byte;
    FStopBits: TSTOPBIT;
    FParity: Integer;
    FBaudRate: Integer;
    FIsOpen: Boolean;
    FTimeOuts: TCommTimeouts;
    FMemo:TMemo;
    FLogOnMemo: Boolean;
    FRaiseErrors: Boolean;
    FUseCrypt: Boolean;
    FUseNet: TNetMode;
    FNetFile,FNetFileTx,FNetFileRx: String;
    FBufferToHexString: Boolean;
    FUseOverlap: Boolean;
    FLogOnFile: Boolean;
    FLogFn: TFileName;
    FMaxLogSize: Integer;
    FLogFnSl:TStringList;
    FByteValueToLabelArray1: Integer;
    FByteValueToLabelArray2: Integer;
    FByteValueToLabelArray3: Integer;
    FSlaveIndex: Integer;
    FIgnoreIPonNetFile: Boolean;
    FNetPort: Word;
    FSocketModule: TSocketModule;
    FFlags: DWord;
    FOnLineEnd: TThreadMethod;
    FLineend: AnsiString;
    FLineRead: AnsiString;
    FLineReadTmp: AnsiString;
    FOnLineEndGet: TLineEnd;
    FReadBusy: Boolean;
    FDTRControl: Boolean;
    FLastErr: String;
    FContinuosLog: Boolean;
    function BufferToString(CONST Buffer;Count:integer):String; overload;
    function BufferToString(CONST Buffer;Count,Written:integer):String; overload;
    procedure SetBaudRate(const Value: Integer);
    procedure SetByteSize(const Value: Integer);
    procedure SetParity(const Value: Integer);
    procedure SetPorta(const Value: Byte);
    procedure SetStopBits(const Value: TSTOPBIT);
    procedure SetTimeOuts(const Value: TCommTimeouts);
    procedure SetLogOnMemo(const Value: Boolean);
    procedure SetRaiseErrors(const Value: Boolean);
    procedure SetUseCrypt(const Value: Boolean);
    procedure SetNetFile(const Value: String);
    procedure SetUseNet(const Value: TNetMode);
    procedure SetBufferToHexString(const Value: Boolean);
//    procedure DecodeFlags(F:Integer);
    Function EncodeFlags:DWord;
    procedure SetFlags(const Value: DWord);
    procedure SetUseOverlap(const Value: Boolean);
    procedure SetLogOnFile(const Value: Boolean);
    procedure SetLogFn(const Value: TFileName);
    procedure SetMaxLogSize(const Value: Integer);
    procedure SaveFileLog(s:string);
    procedure SetByteValueToLabelArray1(const Value: Integer);
    procedure SetByteValueToLabelArray2(const Value: Integer);
    procedure SetByteValueToLabelArray3(const Value: Integer);
    procedure SetIgnoreIPonNetFile(const Value: Boolean);
    procedure SetSlaveIndex(const Value: Integer);
    procedure SetNetPort(const Value: Word);
    procedure SetDTRControl(const Value: Boolean);
  protected
  public
    LabelArray1:Array[0..255] of String;
    LabelArray2:Array[0..255] of String;
    LabelArray3:Array[0..255] of String;
    Constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;
    function Open: Boolean;
    procedure Close;
    function Write(Linea: String): Boolean; Overload;
    function Write(CONST Buffer;Count:Integer): Boolean; Overload;
    function Read(var Buffer;Count:Integer):Integer;  Overload;
    procedure Read; Overload; //Questo metto legge e quando ha il carattere/i specificato in LineEnd genera OnLineEnd e leggo da FLineRead
    function ResendLast:Boolean;
    function ResetCOM:Boolean;
    procedure Purge;
    procedure PurgeRx;
    procedure PurgeTx;
    procedure Flush;
  published
    property Porta: Byte read FPorta write SetPorta default 1;
    property BaudRate: Integer read FBaudRate write SetBaudRate default CBR_9600;
    property ByteSize: Integer read FByteSize write SetByteSize default 8;
    property Parity: Integer read FParity write SetParity default NOPARITY;
    property StopBits: TSTOPBIT read FStopBits write SetStopBits default ONESTOPBIT;
    property Flags: DWord read FFlags write SetFlags default 0;
    property TimeOuts: TCommTimeouts read FTimeOuts write SetTimeOuts;
    property Memo: TMemo read FMemo write FMemo;
    property LogOnMemo: Boolean read FLogOnMemo write SetLogOnMemo default False;
    property LogOnFile: Boolean read FLogOnFile write SetLogOnFile default False;
    property LogFn: TFileName read FLogFn write SetLogFn;
    property MaxLogSize: Integer read FMaxLogSize write SetMaxLogSize;
    property RaiseErrors: Boolean read FRaiseErrors write SetRaiseErrors;
    property ByteValueToLabelArray1: Integer read FByteValueToLabelArray1 write SetByteValueToLabelArray1 default -1;
    property ByteValueToLabelArray2: Integer read FByteValueToLabelArray2 write SetByteValueToLabelArray2 default -1;
    property ByteValueToLabelArray3: Integer read FByteValueToLabelArray3 write SetByteValueToLabelArray3 default -1;
    property LineEnd: AnsiString read FLineend write FLineEnd;
    property LineRead:AnsiString read FLineRead;

    property DTRControl:Boolean read FDTRControl write SetDTRControl;
{    property LineEndms: Integer read FLineendms write FLineEndms;
    property LineEndCarAfter: Integer read FLineEndCarAfter write FLineEndCarAfter;
    property NumCaracteres: Integer read FComNumCaracteres write FComNumCaracteres;
    property Ack: String read FAck write FAck;
    property NAck: String read FNAck write FNAck;
    property Busy: String read FBusy write FBusy;
    property LineStart: String read FLineStart write FLineStart;
    property TimeOut: Integer read FTimeOut write FTimeOut;
    property Text: String read FComText write FComText;
    property OnLineStart: TNotifyEvent read FOnLineStart write FOnLineStart;
    property OnTimeout: TNotifyEvent read FOnTimeOut write FOnTimeOut;
    property OnAck: TNotifyEvent read FOnAck write FOnAck;
    property OnNAck: TNotifyEvent read FOnNAck write FOnNAck;
    property OnBusy: TNotifyEvent read FOnBusy write FOnBusy;         }
    property IsOpen:Boolean read FIsOpen;
    property UseCrypt:Boolean read FUseCrypt write SetUseCrypt;
    property UseNet:TNetMode read FUseNet write SetUseNet Default nmNONE;
    property NetFile:String read FNetFile write SetNetFile;
    property BufferToHexString:Boolean read FBufferToHexString write SetBufferToHexString;
    property UseOverlap:Boolean read FUseOverlap write SetUseOverlap Default True;
    //nel file di scambio di rete c'è l'IP di chi ha scritto il file, se sono in esecuzione sullo stesso pc non comunica
    property IgnoreIPonNetFile:Boolean read FIgnoreIPonNetFile write SetIgnoreIPonNetFile;
    //da impostare anche sul master, è la periferica da cui aspetto la risposta
    property SlaveIndex:Integer read FSlaveIndex write SetSlaveIndex;
    property NetPort:Word read FNetPort write SetNetPort default 5050;
    property OnLineEnd: TThreadMethod read FOnLineEnd write FOnLineEnd;
    property OnLineEndGet: TLineEnd read FOnLineEndGet write FOnLineEndGet;
    property LastErr: String Read FLastErr;
    property ContinuosLog:Boolean read FContinuosLog write FContinuosLog;
  end;

Var GenBuffer:Array[0..2048] of byte;

{CBR_110 CBR_300 CBR_600 CBR_1200 CBR_2400 CBR_4800 CBR_9600 CBR_14400
 CBR_19200 CBR_38400 CBR_56000 CBR_57600 CBR_115200 CBR_128000 CBR_256000

 NOPARITY = 0;    ODDPARITY = 1;  EVENPARITY = 2;  MARKPARITY = 3;  SPACEPARITY = 4;
}

procedure Register;

implementation

uses CryptExUnit;

const // auxilary constants used not defined in windows.pas
      //qui definisco posizione bit poi sono attive con 1 e disattive con 0
      //tranne dcb_DTRControl e dcb_RTSControl che sono due bit
      dcb_Binary           = $0001;  //0000 0000 0000 0001
      dcb_Parity           = $0002;  //0000 0000 0000 0010
      dcb_OutxCTSFlow      = $0004;  //0000 0000 0000 0100
      dcb_OutxDSRFlow      = $0008;  //0000 0000 0000 1000
      dcb_DTRControl_enab  = $0010;  //0000 0000 0001 0000
      dcb_DTRControl_hands = $0020;  //0000 0000 0010 0000
      dcb_DSRSensivity     = $0040;  //0000 0000 0100 0000
      dcb_TxContinueOnXoff = $0080;  //0000 0000 1000 0000
      dcb_OutX             = $0100;  //0000 0001 0000 0000
      dcb_InX              = $0200;  //0000 0010 0000 0000
      dcb_ErrorChar        = $0400;  //0000 0100 0000 0000
      dcb_Null             = $0800;  //0000 1000 0000 0000
      dcb_RTSControl_enab  = $1000;  //0001 0000 0000 0000
      dcb_RTSControl_hands = $2000;  //0010 0000 0000 0000
      dcb_RTSControl_toggle= $3000;  //0011 0000 0000 0000
      dcb_AbortOnError     = $4000;  //0100 0000 0000 0000


procedure Register;
begin
  RegisterComponents('Termo', [TComEx]);
end;

{ TComEx }

Constructor TComEx.Create(AOwner: TComponent);
Var i:Integer;
begin
inherited;
FPorta:=1;
FBaudRate:=CBR_9600;
FByteSize:=8;
FParity:=NOPARITY;
FStopBits:=ONESTOPBIT; //0
//FFlags:=12305;
FFlags:=0;
UseOverlap:=True;
{Specifies the maximum time, in milliseconds, allowed to elapse between the arrival
 of two characters on the communications line. During a ReadFile operation,
 the time period begins when the first character is received.
 If the interval between the arrival of any two characters exceeds this amount,
 the ReadFile operation is completed and any buffered data is returned.
 A value of zero indicates that interval time-outs are not used.}
FTimeOuts.ReadIntervalTimeout:=10;

{Specifies the multiplier, in milliseconds, used to calculate the total time-out
 period for read operations. For each read operation, this value is multiplied by
 the requested number of bytes to be read. }
FTimeOuts.ReadTotalTimeoutMultiplier:=1;

{Specifies the constant, in milliseconds, used to calculate the total time-out
 period for read operations. For each read operation, this value is added to the
 product of the ReadTotalTimeoutMultiplier member and the requested number of bytes.
 A value of zero for both the ReadTotalTimeoutMultiplier and ReadTotalTimeoutConstant
 members indicates that total time-outs are not used for read operations. }
FTimeouts.ReadTotalTimeoutConstant:=30;

{Specifies the multiplier, in milliseconds, used to calculate the total time-out
 period for write operations. For each write operation, this value is multiplied by
 the number of bytes to be written. }
FTimeouts.WriteTotalTimeoutMultiplier:=1;

{Specifies the constant, in milliseconds, used to calculate the total time-out
 period for write operations. For each write operation, this value is added to the
 product of the WriteTotalTimeoutMultiplier member and the number of bytes to be written.
 A value of zero for both the WriteTotalTimeoutMultiplier and WriteTotalTimeoutConstant
 members indicates that total time-outs are not used for write operations.}
FTimeouts.WriteTotalTimeoutConstant:=10;

FLogOnMemo:=False;
FLogOnFile:=False;
FLogFn:='c:\comlog.txt';
FUseNet:=nmNONE;
FNetFile:='';
FLogFnSl:=TStringList.Create;
ByteValueToLabelArray1:=-1;
for i:=0 to High(LabelArray1) do
    begin
     LabelArray1[i]:=IntToHex(i,2);
     LabelArray2[i]:=LabelArray1[i];
     LabelArray3[i]:=LabelArray1[i];
    end;
FNetPort:=5050;
FSocketModule:=TSocketModule.Create(self); //AOwner
FSocketModule.IdTCPClient1.Port:=FNetPort;
FSocketModule.IdTCPServer1.Active:=False;
FSocketModule.IdTCPServer1.DefaultPort:=FNetPort;
end;

destructor TComEx.Destroy;
begin
FLogOnMemo:=False;
Memo:=NIL;
//purge;
close;
try
 if FSocketModule<>NIL then
    try
     FSocketModule.IdTCPClient1.Disconnect;
     FSocketModule.IdTCPServer1.Active:=False;
    finally
     FSocketModule.Destroy;
    end;
finally FSocketModule:=NIL; end;
try if FLogFnSl<>NIl then FLogFnSl.Destroy; finally FLogFnSl:=NIL; end;
inherited;
end;

procedure TComEx.SaveFileLog(s: string);
Var fs:TfileStream;
begin
try
 while FLogFnSl.Count>MaxLogSize do
       FLogFnSl.Delete(0);
 FLogFnSl.Add(s);
 fs:=TfileStream.Create(FLogFn,fmCreate OR fmShareDenyWrite);  //+fmShareDenyWrite
 FLogFnSl.SaveToStream(fs);
// FLogFnSl.SaveToFile(FLogFn);
finally
  try //se non l'ha creato non serve chuderlo
   fs.Destroy;
  finally

  end;
end;
end;

procedure TComEx.SetBaudRate(const Value: Integer);
begin
FBaudRate:=Value;
end;

procedure TComEx.SetByteSize(const Value: Integer);
begin
FByteSize:=Value;
end;

procedure TComEx.SetByteValueToLabelArray1(const Value: Integer);
begin
FByteValueToLabelArray1:=Value;
end;

procedure TComEx.SetByteValueToLabelArray2(const Value: Integer);
begin
FByteValueToLabelArray2:=Value;
end;

procedure TComEx.SetByteValueToLabelArray3(const Value: Integer);
begin
FByteValueToLabelArray3:=Value;
end;

procedure TComEx.SetDTRControl(const Value: Boolean);
begin
FDTRControl:=Value;
end;

procedure TComEx.SetParity(const Value: Integer);
begin
FParity:=Value;
end;

procedure TComEx.SetPorta(const Value: Byte);
begin
FPorta:=Value;
end;

procedure TComEx.SetSlaveIndex(const Value: Integer);
begin
FSlaveIndex:=Value;
SetNetFile(FNetFile);
end;

procedure TComEx.SetStopBits(const Value: TSTOPBIT);
begin
FStopBits:=Value;
end;

function TComEx.Open: Boolean;
Var err,ovl:Cardinal;
    pac:PChar; //config:String;
begin
if (ComH>0) AND (ComH<>INVALID_HANDLE_VALUE) then Close;
case FUseNet of
 nmMASTER,
 nmSLAVE:begin
          Result:=DirectoryExists(ExtractFilePath(FNetFile));
          if Result then ComH:=1;
         end;
 nmMTCPIP:begin
           FSocketModule.IdTCPServer1.Active:=True;
           Result:=True;
           ComH:=1;
          end;
 nmSTCPIP:begin
           Result:=False; ComH:=0;
          end;
 nmNONE:begin                 //'\\.\COM'
// CreateFile('COM2',
         if FUseOverlap then ovl:=FILE_FLAG_OVERLAPPED
                        else ovl:=FILE_ATTRIBUTE_NORMAL;
         ComH:=CreateFile(PChar('\\.\COM'+inttostr(FPorta)),
//         ComH:=CreateFile(PChar('COM'+inttostr(FPorta)), //se maggiore di non so mi fa problemi
                          GENERIC_READ+GENERIC_WRITE,
                          0, // Not shared
                          nil, // No security attributes
                          OPEN_EXISTING,
                          ovl,
                          0); //No template
         Result:=(ComH>0) AND (ComH<INVALID_HANDLE_VALUE);
         if NOT Result Then
            begin
             CloseHandle(ComH);
             err:=GetLastError;
            // pac:=NIL;
             getMem(pac,512);
             FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM OR FORMAT_MESSAGE_ARGUMENT_ARRAY,
                           nil,err,LANG_NEUTRAL,pac,512,nil);
             FLastErr:=Format('[%d] %s',[err,pac])+' In COM'+Inttostr(FPorta);
             if FRaiseErrors then
                raise EWriteError.Create(FLastErr);
             FreeMem(pac,512);
             if FLogOnMemo AND (FMemo<>NIL) then
                try
                 FMemo.Lines.Add('COM '+inttostr(FPorta)+' NOT connected, INVALID handle '+Inttostr(ComH));
                 FMemo.Lines.Add(Format('[%d] %s',[err,pac]));
             finally
             end;
             exit;
            end;
         GetCommState(ComH,ParamCom);
         (* Config:=[baud=b][parity={N|E|O|M|S}][data={4|5|6|7|8}][stop={1|2}][to={on|off}][xon={on|off}][odsr={on|off}][octs={on|off}][dtr={on|off|hs}][rts={on|off|hs|tg}][idsr={on|off}]
            Config:='baud=9600 parity=n data=8 stop=1'; *)
 (* ---------------------------------------------------------------------------------
    25-01-2013 Abilito DTR e RTS per ridurre problemi di comunicazione per i CT
    la configurazione qui è molto controversa, con BuildCommDCB faccio preparare la struttura ParamCom alle API di windows
    26-01-2013 ...ma fallisce spesso la creazione... torno a impostazione manuale
    ---------------------------------------------------------------------------------
         Config:=Format('baud=%d parity=N data=%d stop=%d to=off xon=off odsr=off octs=off dtr=off rts=off idsr=off', *)
{         Config:=Format('baud=%d parity=N data=%d stop=%d to=off xon=off odsr=off octs=off dtr=on rts=tg idsr=off',
                        [     FBaudRate,
     //                                 FParity,
                                                FByteSize,
                                                        Ord(FStopBits)+1]);
         if not BuildCommDCB(PChar(Config), ParamCom) then
            begin
             err:=GetLastError;
            // pac:=NIL;
             getMem(pac,512);
             FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM OR FORMAT_MESSAGE_ARGUMENT_ARRAY,
                           nil,err,LANG_NEUTRAL,pac,512,nil);
             if FRaiseErrors then
                raise EWriteError.Create(Format('[%d] %s',[err,pac])+' In COM'+Inttostr(FPorta));
             FreeMem(pac,512);
             Result:=False;
             exit;
            end;
         //DecodeFlags(ParamCom.Flags); da finire di sviluppare }
         ParamCom.BaudRate:=FBaudRate;
         ParamCom.ByteSize:=FByteSize;
         ParamCom.Parity:=FParity;
         ParamCom.StopBits:=Ord(FStopBits);
         ParamCom.XONLim:=2048;
         ParamCom.XOFFLim:=512;
         ParamCom.XONChar:=#17; // XON ASCII char - DC1, Ctrl-Q, ASCII 17
         ParamCom.XOFFChar:=#19; // XOFF ASCII char - DC3, Ctrl-S, ASCII 19
{        ParamCom.Flags:=12305;} //20615
         if FFlags<>0 then ParamCom.Flags:=FFlags
                      else ParamCom.Flags:=EncodeFlags;
         Result:=SetCommState(ComH,ParamCom);
         if NOT Result then
            begin
             err:=GetLastError;
            // pac:=NIL;
             getMem(pac,512);
             FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM OR FORMAT_MESSAGE_ARGUMENT_ARRAY,
                           nil,err,LANG_NEUTRAL,pac,512,nil);
             FLastErr:=Format('[%d] %s',[err,pac])+' In COM'+Inttostr(FPorta);
             if FRaiseErrors then
                raise EWriteError.Create(FLastErr);
             FreeMem(pac,512);
            end;
         Result:=Result AND SetCommTimeouts(ComH,FTimeouts);
        end;
 else ComH:=0; Result:=False;
end;
FIsOpen:=Result;
if FLogOnMemo AND (FMemo<>NIL) then
   try
    FMemo.Lines.Add('COM '+inttostr(FPorta)+' connected, handle '+Inttostr(ComH));
   finally
   end;
end;

Procedure TComEx.Close;
begin               //4294967295
FIsOpen:=False;
if (ComH>0) AND (ComH<>INVALID_HANDLE_VALUE) then
   try
    case FUseNet of
     nmMASTER,
     nmSLAVE:;
     nmMTCPIP:FSocketModule.IdTCPServer1.Active:=False;
     nmSTCPIP:;
     nmNONE:FileClose(ComH);
    end;
   finally
    ComH:=0;
    if FLogOnMemo AND (FMemo<>NIL) then
       try
        FMemo.Lines.Add('COM '+inttostr(FPorta)+' closed.');
       finally
       end;
   end;
end;

function TComEx.ResetCOM: Boolean;
begin
Purge;
FIsOpen:=False;
if (ComH>0) AND (ComH<>INVALID_HANDLE_VALUE) then
   case FUseNet of
    nmMASTER,
    nmSLAVE:begin
             DeleteFile(FNetFileTx);
             DeleteFile(FNetFileRx);
            end;
    nmMTCPIP:;
    nmSTCPIP:;
    nmNONE:FileClose(ComH);
   end;
Result:=Open;
end;

procedure TComEx.Read;
Var c:AnsiChar; {Overlapped:TOverlapped; r:Boolean; ReadBytes:Cardinal;}
begin
if FReadBusy then exit;
FReadBusy:=True;
try
 While Read(c,1)=1 do
       begin
        FLineReadTmp:=FLineReadTmp+c;
//        Application.
        if pos(FLineend,FLineReadTmp)<>0 then
           begin
            FLineRead:=FLineReadTmp;
            FLineReadTmp:='';
            if Assigned(FOnLineEnd) then try FOnLineEnd except end;
            if Assigned(FOnLineEndGet) then try FOnLineEndGet(FLineRead) except end;
           end;
       end;
finally
 FReadBusy:=False;
end;
{if (ComH=0) OR (ComH=INVALID_HANDLE_VALUE) then exit;
    if NOT FUseOverlap then
       r:=ReadFile(ComH,c,1,ReadBytes,NIL)
    else
       begin
        FillChar(Overlapped,SizeOf(Overlapped),0);
        Overlapped.hEvent:=CreateEvent(nil,True,True,nil);
        ReadFile(ComH,c,1,ReadBytes,@Overlapped);
        WaitForSingleObject(Overlapped.hEvent,TimeOuts.ReadIntervalTimeout);
        r:=GetOverlappedResult(ComH,Overlapped,ReadBytes,False);
        CloseHandle(Overlapped.hEvent);
       end;
if ReadBytes=1 then
   begin
    FLineReadTmp:=FLineReadTmp+c;
    if pos(FLineend,FLineReadTmp)<>0 then
       begin
        FLineRead:=FLineReadTmp;
        FLineReadTmp:='';
        if Assigned(FOnLineEnd) then try FOnLineEnd except end;
        if Assigned(FOnLineEndGet) then try FOnLineEndGet(FLineRead) except end;
       end;
    Read;
   end; //ricorsivo}
end;

function TComEx.Read(var Buffer; Count: Integer): Integer;
var Overlapped:TOverlapped; r:Boolean;
    ReadBytes:Cardinal; ls:TFormatSettings;
    s:String; //pac:pAnsiChar;
begin
Result:=0; r:=False;
if (ComH=0) OR (ComH=INVALID_HANDLE_VALUE) then exit;
case FUseNet of
 nmMaster,
 nmSlave:r:=ReadFromNet(FNetFileRx,Buffer,Count,true,FIgnoreIPonNetFile,ReadBytes);
 nmNone:
   begin
    if NOT FUseOverlap then
       r:=ReadFile(ComH,Buffer,Count,ReadBytes,NIL)
    else
       begin
        FillChar(Overlapped,SizeOf(Overlapped),0);
        Overlapped.hEvent:=CreateEvent(nil,True,True,nil);
        ReadFile(ComH,Buffer,Count,ReadBytes,@Overlapped);
        WaitForSingleObject(Overlapped.hEvent,TimeOuts.ReadIntervalTimeout);
        {r:=}GetOverlappedResult(ComH,Overlapped,ReadBytes,False);
        //durante le operazioni in thr fallisce
        {if NOT ReadFile(ComH,Buffer,Count,ReadBytes,@Overlapped) then
           begin
            CloseHandle(Overlapped.hEvent);
            err:=GetLastError;
            pac:=NIL;
            getMem(pac,512);
            FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM OR FORMAT_MESSAGE_ARGUMENT_ARRAY,
                          nil,
                          err,
                          LANG_NEUTRAL,
                          pac,
                      512,
                      nil);
           end; //}
        WaitForSingleObject(Overlapped.hEvent,TimeOuts.ReadIntervalTimeout);
        r:=GetOverlappedResult(ComH,Overlapped,ReadBytes,False);
        if not r then
           begin
            ReadBytes:=0;
            if FRaiseErrors then
               begin
                raise EWriteError.Create('Unable to read to port. Errore nella lettura da Seriale : '+IntToStr(GetLastError)+' In COM'+Inttostr(FPorta));
                CloseHandle(Overlapped.hEvent);
               end;

           end;
        CloseHandle(Overlapped.hEvent);
       end;
   end;
end;
if r then
   begin
    if FUseCrypt then DeCrypt(@Buffer,Count);
    Result:=ReadBytes;
    if (FLogOnMemo AND (FMemo<>NIL) AND (ReadBytes<>0)) OR
       (FLogOnFile AND (ReadBytes<>0)) then
    begin
     s:=BufferToString(Buffer,ReadBytes);
     {$IF CompilerVersion<22}

     {$ELSE}
      ls:=TFormatSettings.Create();
     {$ENDIF}
     ls.LongTimeFormat:='hh:mm:ss.zzz';
    end;
    if FLogOnMemo AND (FMemo<>NIL) AND (ReadBytes<>0) then
       try
        FMemo.Lines.Add(TimeToStr(time,ls)+Format(': Rx(%d): %s [B read %d]',[FPorta,s,ReadBytes]));
       finally
       end;
    if FLogOnFile AND (ReadBytes<>0) then
        if ContinuosLog then
           try SaveFileLog(s); finally end
        else
           try SaveFileLog(TimeToStr(time,ls)+Format(': Rx: %s [B read %d]',[s,ReadBytes])); finally end;
   end;
end;

function TComEx.Write(Linea: String): Boolean;
var PLinea:PChar; ls:TFormatSettings;
    Overlapped	: TOverlapped;
    Count:Integer; WrittenBytes:Cardinal;
begin
Result:=False;
if (ComH<1) OR (ComH=INVALID_HANDLE_VALUE) then exit;
FLastString:=Linea;
Count:=length(Linea);
if Count>64000 then exit;
if FUseCrypt then Crypt(@Linea[1],Count);
PLinea:=PChar(Linea);
case FUseNet of
 nmMaster,
 nmSlave:begin
          //FlushRXCom;
          PurgeRx;
          Result:=WriteToNet(FNetFileTx,PLinea^,Count,FIgnoreIPonNetFile);
          WrittenBytes:=Count;
         end;
 nmNone:
   begin
    if NOT FUseOverlap then
       Result:=WriteFile(ComH,PLinea^,Count,WrittenBytes,NIL)
    else
       begin
        FillChar(Overlapped,SizeOf(Overlapped),0);
        Overlapped.hEvent:=CreateEvent(nil,True,True,nil);
        Result:=WriteFile(ComH,PLinea^,Count,WrittenBytes,@overlapped);
        IF not Result Then exit;
        WaitForSingleObject(Overlapped.hEvent,TimeOuts.WriteTotalTimeoutConstant); //Aspetto per una risposta...(2000 milisecondi)
        Result:=GetOverlappedResult(ComH,Overlapped,WrittenBytes,False);
        CloseHandle(Overlapped.hEvent);
       end;
   end;
end;
{$IF CompilerVersion<22}
{$ELSE}
 ls:=TFormatSettings.Create();
{$ENDIF}
ls.LongTimeFormat:='hh:mm:ss.zzz';
if FLogOnMemo AND (FMemo<>NIL) then
   try
    FMemo.Lines.Add(TimeToStr(time,ls)+' Tx: '+Linea);
   finally
   end;
if FLogOnFile then
   try SaveFileLog(TimeToStr(time,ls)+' Tx: '+Linea); finally end;
end;

function TComEx.Write(CONST Buffer; Count: Integer): Boolean;
var Overlapped:TOverlapped;
    ls:TFormatSettings;
    WrittenBytes:Cardinal;
begin
Result:=False;
if (ComH<1) OR (ComH=INVALID_HANDLE_VALUE) then exit;
//FLastString:=Linea;
//Count:=length(Linea);
if Count>64000 then exit;
if FUseCrypt then Crypt(@Buffer,Count);
case FUseNet of
 nmMaster,
 nmSlave:begin
          PurgeRx;
          Result:=WriteToNet(FNetFileTx,Buffer,Count,FIgnoreIPonNetFile);
          WrittenBytes:=Count;
         end;
 nmNone:
   begin
    if NOT FUseOverlap then
       Result:=WriteFile(ComH,Buffer,Count,WrittenBytes,NIL)
    else
       begin
        FillChar(Overlapped,SizeOf(Overlapped),0);
        Overlapped.hEvent:=CreateEvent(nil,True,True,nil);
        WriteFile(ComH,Buffer,Count,WrittenBytes,@overlapped);
        WaitForSingleObject(Overlapped.hEvent,TimeOuts.WriteTotalTimeoutConstant); //Aspetto per una risposta...(x milisecondi)
        Result:=GetOverlappedResult(ComH,Overlapped,WrittenBytes,False);
        CloseHandle(Overlapped.hEvent);
       end;
   end;
end;
try
 {$IF CompilerVersion<22}
 {$ELSE}
  ls:=TFormatSettings.Create();
 {$ENDIF}
 ls.LongTimeFormat:='hh:mm:ss.zzz';
 if FLogOnMemo AND (FMemo<>NIL) then
    try
     FMemo.Lines.Add(TimeToStr(time,ls)+': Tx: '+BufferToString(Buffer,Count,WrittenBytes));
    finally
    end;
 if FLogOnFile then
    try SaveFileLog(TimeToStr(time,ls)+': Tx: '+BufferToString(Buffer,Count,WrittenBytes)); finally end;
finally
end;
end;

function TComEx.ResendLast: Boolean;
begin
Result:=Write(FLastString);
end;

procedure TComEx.SetTimeOuts(const Value: TCommTimeouts);
begin
if (Value.ReadIntervalTimeout<1) OR (Value.ReadTotalTimeoutConstant<1) OR
   (Value.WriteTotalTimeoutConstant<1) then exit; //Tutti i time out devono essere sensati
FTimeOuts:=Value;
if (ComH>0) AND (ComH<>INVALID_HANDLE_VALUE) then SetCommTimeouts(ComH,FTimeouts);
end;

procedure TComEx.SetLogFn(const Value: TFileName);
Var ex:String;
begin
FLogFn:=Value;
ex:=ExtractFileExt(FLogFn);
FLogFn:=ChangeFileExt(FLogFn,'_'+intToStr(Porta)+ex);
if FileExists(FLogFn) then
   FLogFnSl.LoadFromFile(FLogFn);
end;

procedure TComEx.SetLogOnFile(const Value: Boolean);
begin
FLogOnFile:=Value;
end;

procedure TComEx.SetLogOnMemo(const Value: Boolean);
begin
FLogOnMemo:=Value;
end;

procedure TComEx.SetMaxLogSize(const Value: Integer);
begin
FMaxLogSize:=Value;
end;

function TComEx.BufferToString(const Buffer; Count: integer): String;
begin
Result:=BufferToString(Buffer,Count,-1);
end;

function TComEx.BufferToString(const Buffer; Count,Written: integer): String;
Var pB:pByte; c:integer;
begin
pB:=PByte(@Buffer);
Result:='';
for c:=0 to Count-1 do
    begin
     if c=ByteValueToLabelArray1 then
        Result:=Result+' '+LabelArray1[pB^]
     else if c=ByteValueToLabelArray2 then
             Result:=Result+' '+LabelArray2[pB^]
          else if c=ByteValueToLabelArray3 then
                  Result:=Result+' '+LabelArray3[pB^]
               else
                  if FBufferToHexString then Result:=Result+' '+IntToHex(pB^,2)
                                        else Result:=Result+' '+IntToStr(pB^);
     Inc(pB);
    end;
if NOT ContinuosLog then
   if Written<0 then Result:=Result+' [L:'+Inttostr(Count)+']'
                else Result:=Result+' [L:'+Inttostr(Count)+' W: '+Inttostr(Written)+']'
end;

procedure TComEx.SetRaiseErrors(const Value: Boolean);
begin
FRaiseErrors:=Value;
end;

procedure TComEx.Purge;
begin
case FUseNet of
 nmMASTER,
 nmSLAVE:begin
          DeleteFile(FNetFileTx);
          DeleteFile(FNetFileRx);
         end;
 nmMTCPIP:;
 nmSTCPIP:;
 nmNONE:if (ComH>0) AND (ComH<>INVALID_HANDLE_VALUE) then
           PurgeComm(ComH,PURGE_RXABORT or PURGE_TXABORT or PURGE_TXCLEAR or PURGE_RXCLEAR);
end;
end;

procedure TComEx.PurgeRx;
begin
case FUseNet of
 nmMASTER,
 nmSLAVE:DeleteFile(FNetFileRx);
 nmMTCPIP:;
 nmSTCPIP:;
 nmNONE:if (ComH>0) AND (ComH<>INVALID_HANDLE_VALUE) then
           PurgeComm(ComH,PURGE_RXABORT or PURGE_RXCLEAR);
end;
end;

procedure TComEx.PurgeTx;
begin
case FUseNet of
 nmMASTER,
 nmSLAVE:DeleteFile(FNetFileTx);
 nmMTCPIP:;
 nmSTCPIP:;
 nmNONE:if (ComH>0) AND (ComH<>INVALID_HANDLE_VALUE) then
           PurgeComm(ComH,PURGE_TXABORT or PURGE_TXCLEAR);
end;
end;

procedure TComEx.Flush;
begin
case FUseNet of
 nmMASTER,
 nmSLAVE:Begin
          DeleteFile(FNetFileTx);
          DeleteFile(FNetFileRx);
         end;
 nmMTCPIP:;
 nmSTCPIP:;
 nmNONE:if (ComH>0) AND (ComH<>INVALID_HANDLE_VALUE) then
           FlushFileBuffers(ComH);
end;
end;

procedure TComEx.SetUseCrypt(const Value: Boolean);
begin
FUseCrypt:=Value;
end;

procedure TComEx.SetNetFile(const Value: String);
begin
FNetFile:=Value;
case FUseNet of
 nmMaster:Begin
           FNetFileTx:=FNetFile+IntToStr(FSlaveIndex)+'.tx';
           FNetFileRx:=FNetFile+IntToStr(FSlaveIndex)+'.rx';
          end;
 nmSlave:Begin
           FNetFileTx:=FNetFile+IntToStr(FSlaveIndex)+'.rx';
           FNetFileRx:=FNetFile+IntToStr(FSlaveIndex)+'.tx';
         end;
end;
end;

procedure TComEx.SetUseNet(const Value: TNetMode);
begin
if Value<>nmNONE then FPorta:=0;
FUseNet:=Value;
SetNetFile(FNetFile);
end;

procedure TComEx.SetBufferToHexString(const Value: Boolean);
begin
FBufferToHexString:=Value;
end;

{procedure TComEx.DecodeFlags(F: Integer);
begin
F_Binary:=(F AND dcb_Binary)=dcb_Binary;
F_Parity:=(F AND dcb_Parity)=dcb_Parity;
F_OutxCTSFlow:=(F AND dcb_OutxCTSFlow)=dcb_OutxCTSFlow;
F_OutxDSRFlow:=(F AND dcb_OutxDSRFlow)=dcb_OutxDSRFlow;
F_DTRControl:=(F AND dcb_DTRControl)=dcb_DTRControl;

end;}

(**function TComEx.EncodeFlags: Integer;
begin
Result:=dcb_Binary;
//    if FDiscardNull then
//Result:=Result or dcb_Null;
if F_DTRControl then Result:=Result or dcb_DtrControl;
{  case FHwFlow of
    // No hw flow control
    hfNONE:;
    // No hw flow control but set RTS high and leave it high
    hfNONERTSON:
      dcb.Flags := dcb.Flags or dcb_RtsControlEnable;
    // RTS/CTS (request-to-send/clear-to-send) flow control
    hfRTSCTS:
      dcb.Flags := dcb.Flags or dcb_OutxCtsFlow or dcb_RtsControlHandshake;
  end;}
  {case FSwFlow of
    // No sw flow control
    sfNONE:;
    // XON/XOFF sw flow control
    sfXONXOFF:
      dcb.Flags := dcb.Flags or dcb_OutX or dcb_InX;
  end;}
end;**)

function TComEx.EncodeFlags: DWord;
begin
Result:=dcb_Binary; //windows lo richiede sempre
//con ct su seriel vera meglio abilitare entrambi
if FDTRControl then Result:=Result OR dcb_DTRControl_enab;
//Result:=Result OR dcb_RTSControl_toggle;
end;

procedure TComEx.SetFlags(const Value: DWord);
begin
FFlags:=Value;
end;

procedure TComEx.SetIgnoreIPonNetFile(const Value: Boolean);
begin
FIgnoreIPonNetFile:=Value;
end;

procedure TComEx.SetUseOverlap(const Value: Boolean);
begin
if FUseOverlap=Value then exit;
FUseOverlap:=Value;
if NOT IsOpen then exit;
Close;
Open;
end;

procedure TComEx.SetNetPort(const Value: Word);
begin
FNetPort:=Value;
FSocketModule.IdTCPClient1.Port:=FNetPort;
FSocketModule.IdTCPServer1.Active:=False;
FSocketModule.IdTCPServer1.DefaultPort:=FNetPort;
end;

end.

