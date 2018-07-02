unit CryptExUnit;

interface

// {$IF CompilerVersion < 21.0} Windows, {$IFEND}
Uses Windows, SysUtils, Winsock;

procedure Crypt(pb:pByte; Count: Integer);
procedure DeCrypt(pb:pByte; Count: Integer);
Function DummyCrypt(s:String):String;
Function GetPCIp:Int64;
Function ReadFromNet(NetFile:String; var Buffer;Count:Integer;AutoDelete,FIgnoreIPon:Boolean;Var pBytesIn:Cardinal):Boolean;
function WriteToNet(NetFile:String;CONST Buffer;Count:Integer;FIgnoreIPon:Boolean):Boolean;

implementation

Var FLastReadByte:Int64;

procedure Crypt(pb:pByte; Count: Integer);
var i,j:Integer;
begin
i:=0;
while i<Count do
      begin
       j:=(pB^+128);
       if j>255 then j:=255-j;
       pB^:=j;
       inc(pB);
       inc(i);
      end;
end;

procedure DeCrypt(pb:pByte; Count: Integer);
var i,j:Integer;
begin
i:=0;
while i<Count do
      begin
       j:=(pB^-128);
       if j<0 then j:=255+j;
       pB^:=j;
       inc(pB);
       inc(i);
      end;
end;

Function DummyCrypt(s:String):String;
var i:integer;
begin
Result:='';
for i:=1 to Length(s) do
    case i MOD 3 of
     0:Result:=Result+Chr(255-Ord(s[i])+i);
     1:Result:=Result+Chr(255-Ord(s[i])-i);
     2:Result:=Result+Chr(255+Ord(s[i])+i);
    end;
end;

Function GetPCIp:Int64;
type TName=array[0..100] of AnsiChar;
var HEnt: pHostEnt; HostName:AnsiString; IPaddr:String;
    HName:PAnsiChar; WSAData:TWSAData; i:Integer;
begin
Result:=0;
if WSAStartup($0101,WSAData)<>0 then Exit;
IPaddr:='';
New(HName);
if GetHostName(HName,SizeOf(TName))=0 then
   begin
    HostName:=StrPas(HName);
    HEnt:=GetHostByName(HName);
    for i:=0 to HEnt^.h_length-1 do
        IPaddr:=Concat(IPaddr,IntToStr(Ord(HEnt^.h_addr_list^[i])));
    TryStrToInt64(IPaddr,Result);
  end
else
   begin
    case WSAGetLastError of
     WSANOTINITIALISED:Result:=-3; //WSAErr:='WSANotInitialised';
     WSAENETDOWN      :Result:=-1; //'WSAENetDown';
     WSAEINPROGRESS   :Result:=-2; //WSAErr:='WSAEInProgress';
    end;
  end;
Dispose(HName);
WSACleanup;
end;

Function ReadFromNet(NetFile:String; var Buffer;Count:Integer;AutoDelete,FIgnoreIPon:Boolean;Var pBytesIn:Cardinal):Boolean;
Var h:Integer; ip,pip:Int64;
begin
Result:=False;
pBytesIn:=0;
h:=FileOpen(NetFile,fmOpenRead OR fmShareDenyNone);
//h:=FileOpen(NetFile,fmOpenRead);
if H<1 then
   begin FLastReadByte:=0; exit; end;
FileRead(h,ip,SizeOf(Int64));
try
 pip:=0;
 if NOT FIgnoreIPon then pip:=GetPCIp;
 if (ip<>pip) OR FIgnoreIPon then
    begin
     fileSeek(h,FLastReadByte+SizeOf(Int64),0);
     pBytesIn:=FileRead(h,buffer,count);
     inc(FLastReadByte,pBytesIn);
     if (pBytesIn=0) OR (Count>1) then FLastReadByte:=0;
    end;
finally
 fileClose(H);
end;
//if pBytesIn<=count then pBytesIn:=0;
if AutoDelete then
   if (FLastReadByte=0) AND ((ip<>pip) OR FIgnoreIPon) then
      DeleteFile(NetFile);
Result:=True;
end;

function WriteToNet(NetFile:String;CONST Buffer;Count:Integer;FIgnoreIPon:Boolean):Boolean;
Var h:Integer; ip:Int64; ms:Integer;
begin
Result:=False;
DeleteFile(NetFile+'.tmp');
h:=FileCreate(NetFile+'.tmp');
if H<1 then exit;
ip:=0;
if NOT FIgnoreIPon then ip:=GetPCIp;
try
 FileWrite(h,ip,SizeOf(Int64));
 FileWrite(h,buffer,Count);
finally
 fileClose(h);
end;
ms:=0;
repeat
// DeleteFile(NetFile); //lo cancella chi legge
 Result:=RenameFile(NetFile+'.tmp',NetFile);
 sleep(10);
 inc(ms);
until Result OR (ms>100);  //aspetto un secondo
if not result then  //se time out cancello io
   if DeleteFile(NetFile) then
      Result:=RenameFile(NetFile+'.tmp',NetFile);
end;

end.
