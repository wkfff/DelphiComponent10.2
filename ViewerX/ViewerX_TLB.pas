unit ViewerX_TLB;

// ************************************************************************ //
// WARNING                                                                    
// -------                                                                    
// The types declared in this file were generated from data read from a       
// Type Library. If this type library is explicitly or indirectly (via        
// another type library referring to this type library) re-imported, or the   
// 'Refresh' command of the Type Library Editor activated while editing the   
// Type Library, the contents of this file will be regenerated and all        
// manual modifications will be lost.                                         
// ************************************************************************ //

// $Rev: 52393 $
// File generated on 18/07/2015 01:38:22 from Type Library described below.

// ************************************************************************  //
// Type Lib: \\vmware-host\Shared Folders\Software Develop\delphicomponents\VncX\newVer\scvncctrl.dll (1)
// LIBID: {2864468C-765F-4EF3-B8CF-42666C92B545}
// LCID: 0
// Helpfile: 
// HelpString: SmartCode ViewerX VNC Control v3.6.1.0
// DepndLst: 
//   (1) v2.0 stdole, (C:\Windows\SysWOW64\stdole2.tlb)
// SYS_KIND: SYS_WIN32
// ************************************************************************ //
{$TYPEDADDRESS OFF} // Unit must be compiled without type-checked pointers. 
{$WARN SYMBOL_PLATFORM OFF}
{$WRITEABLECONST ON}
{$VARPROPSETTER ON}
{$ALIGN 4}

interface

uses Winapi.Windows, System.Classes, System.Variants, System.Win.StdVCL, Vcl.Graphics, Vcl.OleCtrls, Vcl.OleServer, Winapi.ActiveX;
  


// *********************************************************************//
// GUIDS declared in the TypeLibrary. Following prefixes are used:        
//   Type Libraries     : LIBID_xxxx                                      
//   CoClasses          : CLASS_xxxx                                      
//   DISPInterfaces     : DIID_xxxx                                       
//   Non-DISP interfaces: IID_xxxx                                        
// *********************************************************************//
const
  // TypeLibrary Major and minor versions
  ViewerXMajorVersion = 1;
  ViewerXMinorVersion = 0;

  LIBID_ViewerX: TGUID = '{2864468C-765F-4EF3-B8CF-42666C92B545}';

  DIID__ISmartCodeVNCViewerEvents: TGUID = '{301C38CE-584C-4E08-A545-140A08892FB0}';
  IID_ISmartCodeVNCViewer: TGUID = '{37A0E3B1-1AFD-4125-AB2E-2DE8F69DED39}';
  IID_ISmartCodeVNCViewer2: TGUID = '{82B3A16E-4E01-467E-AED4-198EA092D3AF}';
  IID_ISmartCodeVNCViewer3: TGUID = '{C35468F3-A938-4CB4-ADBB-D0BB37F504CA}';
  IID_IScVxAdvancedSettings: TGUID = '{200722C4-0B36-48F7-BACF-A9E7AD3D0A7E}';
  IID_IScVxCapabilities: TGUID = '{CC783088-B1D2-4B2F-88CA-51B8ACC638B9}';
  IID_IScVxUltraSecurity_MSRC4: TGUID = '{FE105750-449E-4220-ADF5-AB8EF7D6906A}';
  IID_IScVxUltraSecurity_SecureVNC: TGUID = '{5D2B76F9-B1F4-43B5-A207-F23EA608BA49}';
  CLASS_CSC_ViewerXControl: TGUID = '{62FA83F7-20EC-4D62-AC86-BAB705EE1CCD}';

// *********************************************************************//
// Declaration of Enumerations defined in Type Library                    
// *********************************************************************//
// Constants for enum AuthenticationStage
type
  AuthenticationStage = TOleEnum;
const
  AS_PROXY = $00000000;
  AS_SERVER = $00000001;

// Constants for enum AutoReconnectContinueState
type
  AutoReconnectContinueState = TOleEnum;
const
  ARCS_CONTINUE = $00000000;
  ARCS_STOP = $00000001;

// Constants for enum VNCEncoding
type
  VNCEncoding = TOleEnum;
const
  RFB_RAW = $00000000;
  RFB_RRE = $00000002;
  RFB_CORRE = $00000004;
  RFB_HEXTILE = $00000005;
  RFB_ZLIB = $00000006;
  RFB_TIGHT = $00000007;
  RFB_ZLIBHEX = $00000008;
  RFB_ULTRA = $00000009;
  RFB_ZRLE = $00000010;
  RFB_ZYWRLE = $00000011;

// Constants for enum CursorTrackingMode
type
  CursorTrackingMode = TOleEnum;
const
  VCT_NO_CURSOR = $00000000;
  VCT_DOT_CURSOR = $00000001;
  VCT_NORMAL_CURSOR = $00000002;
  VCT_SMALL_CURSOR = $00000003;

// Constants for enum VncConnectionState
type
  VncConnectionState = TOleEnum;
const
  VCS_DISCONNECTED = $00000000;
  VCS_CONNECTED = $00000001;
  VCS_CONNECTING = $00000002;
  VCS_LISTENING = $00000003;
  VCS_AWAITINGRECONNECT = $00000004;

// Constants for enum ViewerLoginType
type
  ViewerLoginType = TOleEnum;
const
  VLT_VNC = $00000000;
  VLT_MSWIN = $00000001;

// Constants for enum ConnectionProxyType
type
  ConnectionProxyType = TOleEnum;
const
  VPT_NONE = $00000000;
  VPT_SOCKS5 = $00000001;
  VPT_HTTP = $00000002;
  VPT_ULTRA_REPEATER = $00000003;

// Constants for enum ConnectionBarMode
type
  ConnectionBarMode = TOleEnum;
const
  CBM_NONE = $00000000;
  CBM_PINNED = $00000001;
  CBM_UNPINNED = $00000002;
  CBM_HIDDEN = $00000003;

// Constants for enum ScreenStretchMode
type
  ScreenStretchMode = TOleEnum;
const
  SSM_NONE = $00000000;
  SSM_FREE = $00000001;
  SSM_ASPECT = $00000002;

// Constants for enum ConnectionStage
type
  ConnectionStage = TOleEnum;
const
  CS_SOCKETCONNECT = $00000000;
  CS_PROTOCOLNEGOTIATION = $00000001;

// Constants for enum CursorMode
type
  CursorMode = TOleEnum;
const
  CM_TRACK_LOCALLY = $00000000;
  CM_REMOTE_DEAL = $00000001;
  CM_DONT_SHOW_REM = $00000002;

// Constants for enum ColorDepth
type
  ColorDepth = TOleEnum;
const
  COLOR_FULL = $00000000;
  COLOR_256 = $00000001;
  COLOR_64 = $00000002;
  COLOR_8 = $00000003;

// Constants for enum EncryptionPluginType
type
  EncryptionPluginType = TOleEnum;
const
  EPT_NONE = $00000000;
  EPT_MSRC4 = $00000001;
  EPT_SECUREVNC = $00000002;

// Constants for enum DsmKeyStorage
type
  DsmKeyStorage = TOleEnum;
const
  DKS_FILE = $00000000;
  DKS_MEMORY = $00000001;

// Constants for enum ScreenStretchRenderingQuality
type
  ScreenStretchRenderingQuality = TOleEnum;
const
  SSQ_GDI = $00000000;
  SSQ_GDIPLUS_HIGH = $00000001;
  SSQ_GDIPLUS_LOW = $00000002;

type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                    
// *********************************************************************//
  _ISmartCodeVNCViewerEvents = dispinterface;
  ISmartCodeVNCViewer = interface;
  ISmartCodeVNCViewerDisp = dispinterface;
  ISmartCodeVNCViewer2 = interface;
  ISmartCodeVNCViewer2Disp = dispinterface;
  ISmartCodeVNCViewer3 = interface;
  ISmartCodeVNCViewer3Disp = dispinterface;
  IScVxAdvancedSettings = interface;
  IScVxAdvancedSettingsDisp = dispinterface;
  IScVxCapabilities = interface;
  IScVxCapabilitiesDisp = dispinterface;
  IScVxUltraSecurity_MSRC4 = interface;
  IScVxUltraSecurity_MSRC4Disp = dispinterface;
  IScVxUltraSecurity_SecureVNC = interface;
  IScVxUltraSecurity_SecureVNCDisp = dispinterface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  CSC_ViewerXControl = ISmartCodeVNCViewer3;


// *********************************************************************//
// DispIntf:  _ISmartCodeVNCViewerEvents
// Flags:     (4096) Dispatchable
// GUID:      {301C38CE-584C-4E08-A545-140A08892FB0}
// *********************************************************************//
  _ISmartCodeVNCViewerEvents = dispinterface
    ['{301C38CE-584C-4E08-A545-140A08892FB0}']
    function Connected: HResult; dispid 1;
    function Disconnected: HResult; dispid 2;
    function ServerDimension(nWidth: Integer; nHeight: Integer): HResult; dispid 3;
    function ScreenUpdated: HResult; dispid 4;
    function Connecting: HResult; dispid 5;
    function RequestShow: HResult; dispid 6;
    function RequestHide: HResult; dispid 7;
    function ConnectionAccepted(const bstrServerAddress: WideString): HResult; dispid 8;
    function AuthenticationFailed(stage: AuthenticationStage; 
                                  out pbCancelAndDontPromptForPassword: WordBool): HResult; dispid 9;
    function OnChatSessionStarted: HResult; dispid 10;
    function OnChatSessionEnded: HResult; dispid 11;
    function OnChatMessageSend(const messageText: WideString): HResult; dispid 12;
    function OnChatMessageReceived(const messageText: WideString): HResult; dispid 13;
    function OnDisableRemoteInputChanged(remoteInputEnabled: WordBool): HResult; dispid 14;
    function OnAutoReconnecting(attemptCount: Integer; 
                                out pArcContinueStatus: AutoReconnectContinueState): HResult; dispid 15;
    function OnMouseMove(x: Integer; y: Integer): HResult; dispid 16;
  end;

// *********************************************************************//
// Interface: ISmartCodeVNCViewer
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {37A0E3B1-1AFD-4125-AB2E-2DE8F69DED39}
// *********************************************************************//
  ISmartCodeVNCViewer = interface(IDispatch)
    ['{37A0E3B1-1AFD-4125-AB2E-2DE8F69DED39}']
    procedure Set_HostIP(const pstrHostIP: WideString); safecall;
    function Get_HostIP: WideString; safecall;
    procedure Set_Port(pport: Integer); safecall;
    function Get_Port: Integer; safecall;
    procedure Set_Password(const pstrPassword: WideString); safecall;
    function Get_Password: WideString; safecall;
    procedure Set_CustomCompression(pbAllow: WordBool); safecall;
    function Get_CustomCompression: WordBool; safecall;
    procedure Set_CustomCompressionLevel(pnLevel: Integer); safecall;
    function Get_CustomCompressionLevel: Integer; safecall;
    procedure Set_JPEGCompression(pbAllow: WordBool); safecall;
    function Get_JPEGCompression: WordBool; safecall;
    procedure Set_JPEGCompressionLevel(pnLevel: Integer); safecall;
    function Get_JPEGCompressionLevel: Integer; safecall;
    procedure Set_CopyRect(pbAllow: WordBool); safecall;
    function Get_CopyRect: WordBool; safecall;
    procedure Set_EmulateThreeButton(pbEmulate: WordBool); safecall;
    function Get_EmulateThreeButton: WordBool; safecall;
    procedure Set_SwapMouseButtons(pbSwap: WordBool); safecall;
    function Get_SwapMouseButtons: WordBool; safecall;
    procedure Set_Encoding(pnEncoding: VNCEncoding); safecall;
    function Get_Encoding: VNCEncoding; safecall;
    procedure Set_ViewOnly(pbViewOnly: WordBool); safecall;
    function Get_ViewOnly: WordBool; safecall;
    procedure Set_RestrictPixel(pbRestrict: WordBool); safecall;
    function Get_RestrictPixel: WordBool; safecall;
    procedure Set_ScaleNum(pnScale: Integer); safecall;
    function Get_ScaleNum: Integer; safecall;
    procedure Set_ScaleDen(pnScale: Integer); safecall;
    function Get_ScaleDen: Integer; safecall;
    procedure Set_ScaleEnable(pbScale: WordBool); safecall;
    function Get_ScaleEnable: WordBool; safecall;
    function Get_Connected: WordBool; safecall;
    procedure Set_FullScreen(pbFull: WordBool); safecall;
    function Get_FullScreen: WordBool; safecall;
    procedure Set_LocalCursor(pnTack: CursorTrackingMode); safecall;
    function Get_LocalCursor: CursorTrackingMode; safecall;
    procedure Set_MessageBoxes(pbMsg: WordBool); safecall;
    function Get_MessageBoxes: WordBool; safecall;
    procedure Set_DisableClipboard(pbClip: WordBool); safecall;
    function Get_DisableClipboard: WordBool; safecall;
    procedure Connect; safecall;
    procedure ConnectEx(const strIP: WideString; nPort: Integer; const strPassword: WideString); safecall;
    procedure Disconnect; safecall;
    procedure SendCAD; safecall;
    procedure RequestRefresh; safecall;
    procedure ShowConnectionInfo; safecall;
    function Get_ScreenBitmap: Integer; safecall;
    function Get_ScreenWidth: Integer; safecall;
    function Get_ScreenHeight: Integer; safecall;
    procedure Set_RequestSharedSession(pbShared: WordBool); safecall;
    function Get_RequestSharedSession: WordBool; safecall;
    function GetScreenBitmapScaled(nWidth: Integer; nHeight: Integer): Integer; safecall;
    procedure Set_ThumbnailMode(pbThumbnail: WordBool); safecall;
    function Get_ThumbnailMode: WordBool; safecall;
    procedure Set_TopLevelParent(hParent: Integer); safecall;
    function Get_TopLevelParent: Integer; safecall;
    procedure ConnectAsync; safecall;
    procedure ConnectAsyncEx(const strIP: WideString; nPort: Integer; const strPassword: WideString); safecall;
    function GetConnectionState: VncConnectionState; safecall;
    procedure SetDormant(bDormant: WordBool); safecall;
    procedure Set_LoginType(ptype: ViewerLoginType); safecall;
    function Get_LoginType: ViewerLoginType; safecall;
    procedure Set_MsUser(const pstrUser: WideString); safecall;
    function Get_MsUser: WideString; safecall;
    procedure Set_MsDomain(const pstrDomain: WideString); safecall;
    function Get_MsDomain: WideString; safecall;
    procedure Set_MsPassword(const pstrPassword: WideString); safecall;
    function Get_MsPassword: WideString; safecall;
    procedure Set_ProxyIP(const pstrFirewallIP: WideString); safecall;
    function Get_ProxyIP: WideString; safecall;
    procedure Set_ProxyPort(pport: Integer); safecall;
    function Get_ProxyPort: Integer; safecall;
    procedure Set_ProxyUser(const pstrUser: WideString); safecall;
    function Get_ProxyUser: WideString; safecall;
    procedure Set_ProxyPassword(const pstrPassword: WideString); safecall;
    function Get_ProxyPassword: WideString; safecall;
    procedure Set_ProxyType(ptype: ConnectionProxyType); safecall;
    function Get_ProxyType: ConnectionProxyType; safecall;
    procedure Set_ConnectionBar(pmode: ConnectionBarMode); safecall;
    function Get_ConnectionBar: ConnectionBarMode; safecall;
    procedure Set_ConnectionBarHeight(Param1: Integer); safecall;
    procedure Set_StretchMode(pmode: ScreenStretchMode); safecall;
    function Get_StretchMode: ScreenStretchMode; safecall;
    procedure Set_ListenPort(pport: Integer); safecall;
    function Get_ListenPort: Integer; safecall;
    function Get_Listening: WordBool; safecall;
    procedure Listen; safecall;
    procedure ListenEx(Port: Integer); safecall;
    procedure StopListen; safecall;
    function GetBytesSent: Largeuint; safecall;
    function GetBytesReceived: Largeuint; safecall;
    procedure Set_DisconnectedText(const pstrText: WideString); safecall;
    function Get_DisconnectedText: WideString; safecall;
    procedure Set_ConnectingText(const pstrText: WideString); safecall;
    function Get_ConnectingText: WideString; safecall;
    procedure Set_ListeningText(const pstrText: WideString); safecall;
    function Get_ListeningText: WideString; safecall;
    function Get_AdvancedSettings: IScVxAdvancedSettings; safecall;
    property HostIP: WideString read Get_HostIP write Set_HostIP;
    property Port: Integer read Get_Port write Set_Port;
    property Password: WideString read Get_Password write Set_Password;
    property CustomCompression: WordBool read Get_CustomCompression write Set_CustomCompression;
    property CustomCompressionLevel: Integer read Get_CustomCompressionLevel write Set_CustomCompressionLevel;
    property JPEGCompression: WordBool read Get_JPEGCompression write Set_JPEGCompression;
    property JPEGCompressionLevel: Integer read Get_JPEGCompressionLevel write Set_JPEGCompressionLevel;
    property CopyRect: WordBool read Get_CopyRect write Set_CopyRect;
    property EmulateThreeButton: WordBool read Get_EmulateThreeButton write Set_EmulateThreeButton;
    property SwapMouseButtons: WordBool read Get_SwapMouseButtons write Set_SwapMouseButtons;
    property Encoding: VNCEncoding read Get_Encoding write Set_Encoding;
    property ViewOnly: WordBool read Get_ViewOnly write Set_ViewOnly;
    property RestrictPixel: WordBool read Get_RestrictPixel write Set_RestrictPixel;
    property ScaleNum: Integer read Get_ScaleNum write Set_ScaleNum;
    property ScaleDen: Integer read Get_ScaleDen write Set_ScaleDen;
    property ScaleEnable: WordBool read Get_ScaleEnable write Set_ScaleEnable;
    property Connected: WordBool read Get_Connected;
    property FullScreen: WordBool read Get_FullScreen write Set_FullScreen;
    property LocalCursor: CursorTrackingMode read Get_LocalCursor write Set_LocalCursor;
    property MessageBoxes: WordBool read Get_MessageBoxes write Set_MessageBoxes;
    property DisableClipboard: WordBool read Get_DisableClipboard write Set_DisableClipboard;
    property ScreenBitmap: Integer read Get_ScreenBitmap;
    property ScreenWidth: Integer read Get_ScreenWidth;
    property ScreenHeight: Integer read Get_ScreenHeight;
    property RequestSharedSession: WordBool read Get_RequestSharedSession write Set_RequestSharedSession;
    property ThumbnailMode: WordBool read Get_ThumbnailMode write Set_ThumbnailMode;
    property TopLevelParent: Integer read Get_TopLevelParent write Set_TopLevelParent;
    property LoginType: ViewerLoginType read Get_LoginType write Set_LoginType;
    property MsUser: WideString read Get_MsUser write Set_MsUser;
    property MsDomain: WideString read Get_MsDomain write Set_MsDomain;
    property MsPassword: WideString read Get_MsPassword write Set_MsPassword;
    property ProxyIP: WideString read Get_ProxyIP write Set_ProxyIP;
    property ProxyPort: Integer read Get_ProxyPort write Set_ProxyPort;
    property ProxyUser: WideString read Get_ProxyUser write Set_ProxyUser;
    property ProxyPassword: WideString read Get_ProxyPassword write Set_ProxyPassword;
    property ProxyType: ConnectionProxyType read Get_ProxyType write Set_ProxyType;
    property ConnectionBar: ConnectionBarMode read Get_ConnectionBar write Set_ConnectionBar;
    property ConnectionBarHeight: Integer write Set_ConnectionBarHeight;
    property StretchMode: ScreenStretchMode read Get_StretchMode write Set_StretchMode;
    property ListenPort: Integer read Get_ListenPort write Set_ListenPort;
    property Listening: WordBool read Get_Listening;
    property DisconnectedText: WideString read Get_DisconnectedText write Set_DisconnectedText;
    property ConnectingText: WideString read Get_ConnectingText write Set_ConnectingText;
    property ListeningText: WideString read Get_ListeningText write Set_ListeningText;
    property AdvancedSettings: IScVxAdvancedSettings read Get_AdvancedSettings;
  end;

// *********************************************************************//
// DispIntf:  ISmartCodeVNCViewerDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {37A0E3B1-1AFD-4125-AB2E-2DE8F69DED39}
// *********************************************************************//
  ISmartCodeVNCViewerDisp = dispinterface
    ['{37A0E3B1-1AFD-4125-AB2E-2DE8F69DED39}']
    property HostIP: WideString dispid 1;
    property Port: Integer dispid 2;
    property Password: WideString dispid 3;
    property CustomCompression: WordBool dispid 4;
    property CustomCompressionLevel: Integer dispid 5;
    property JPEGCompression: WordBool dispid 6;
    property JPEGCompressionLevel: Integer dispid 7;
    property CopyRect: WordBool dispid 8;
    property EmulateThreeButton: WordBool dispid 9;
    property SwapMouseButtons: WordBool dispid 10;
    property Encoding: VNCEncoding dispid 11;
    property ViewOnly: WordBool dispid 12;
    property RestrictPixel: WordBool dispid 13;
    property ScaleNum: Integer dispid 14;
    property ScaleDen: Integer dispid 15;
    property ScaleEnable: WordBool dispid 16;
    property Connected: WordBool readonly dispid 17;
    property FullScreen: WordBool dispid 18;
    property LocalCursor: CursorTrackingMode dispid 19;
    property MessageBoxes: WordBool dispid 20;
    property DisableClipboard: WordBool dispid 21;
    procedure Connect; dispid 22;
    procedure ConnectEx(const strIP: WideString; nPort: Integer; const strPassword: WideString); dispid 23;
    procedure Disconnect; dispid 24;
    procedure SendCAD; dispid 25;
    procedure RequestRefresh; dispid 26;
    procedure ShowConnectionInfo; dispid 27;
    property ScreenBitmap: Integer readonly dispid 28;
    property ScreenWidth: Integer readonly dispid 29;
    property ScreenHeight: Integer readonly dispid 30;
    property RequestSharedSession: WordBool dispid 31;
    function GetScreenBitmapScaled(nWidth: Integer; nHeight: Integer): Integer; dispid 32;
    property ThumbnailMode: WordBool dispid 33;
    property TopLevelParent: Integer dispid 34;
    procedure ConnectAsync; dispid 35;
    procedure ConnectAsyncEx(const strIP: WideString; nPort: Integer; const strPassword: WideString); dispid 36;
    function GetConnectionState: VncConnectionState; dispid 37;
    procedure SetDormant(bDormant: WordBool); dispid 38;
    property LoginType: ViewerLoginType dispid 39;
    property MsUser: WideString dispid 40;
    property MsDomain: WideString dispid 41;
    property MsPassword: WideString dispid 42;
    property ProxyIP: WideString dispid 100;
    property ProxyPort: Integer dispid 101;
    property ProxyUser: WideString dispid 102;
    property ProxyPassword: WideString dispid 103;
    property ProxyType: ConnectionProxyType dispid 104;
    property ConnectionBar: ConnectionBarMode dispid 200;
    property ConnectionBarHeight: Integer writeonly dispid 201;
    property StretchMode: ScreenStretchMode dispid 202;
    property ListenPort: Integer dispid 203;
    property Listening: WordBool readonly dispid 204;
    procedure Listen; dispid 205;
    procedure ListenEx(Port: Integer); dispid 206;
    procedure StopListen; dispid 207;
    function GetBytesSent: Largeuint; dispid 208;
    function GetBytesReceived: Largeuint; dispid 209;
    property DisconnectedText: WideString dispid 210;
    property ConnectingText: WideString dispid 211;
    property ListeningText: WideString dispid 212;
    property AdvancedSettings: IScVxAdvancedSettings readonly dispid 213;
  end;

// *********************************************************************//
// Interface: ISmartCodeVNCViewer2
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {82B3A16E-4E01-467E-AED4-198EA092D3AF}
// *********************************************************************//
  ISmartCodeVNCViewer2 = interface(ISmartCodeVNCViewer)
    ['{82B3A16E-4E01-467E-AED4-198EA092D3AF}']
    procedure Set_CtrlKeyPressed(pbPressed: WordBool); safecall;
    function Get_CtrlKeyPressed: WordBool; safecall;
    procedure Set_AltKeyPressed(pbPressed: WordBool); safecall;
    function Get_AltKeyPressed: WordBool; safecall;
    procedure OpenFileTransfer; safecall;
    procedure SendCtrlEsq; safecall;
    function Get_Capabilities: IScVxCapabilities; safecall;
    procedure OpenChat; safecall;
    function Get_SrvCap_FileTransfer: WordBool; safecall;
    function Get_SrvCap_Chat: WordBool; safecall;
    procedure SendCustomKey(keyCode: Integer); safecall;
    function IsDormant: WordBool; safecall;
    procedure Set_MouseCursorMode(pnMode: CursorMode); safecall;
    function Get_MouseCursorMode: CursorMode; safecall;
    procedure Set_remoteInputEnabled(pbEnable: WordBool); safecall;
    function Get_remoteInputEnabled: WordBool; safecall;
    procedure SelectSingleWindow(bSelect: WordBool); safecall;
    procedure SwitchMultiMonitor; safecall;
    procedure SendCustomKeyEx(keyCode: Integer; bKeyDownEvent: WordBool); safecall;
    property CtrlKeyPressed: WordBool read Get_CtrlKeyPressed write Set_CtrlKeyPressed;
    property AltKeyPressed: WordBool read Get_AltKeyPressed write Set_AltKeyPressed;
    property Capabilities: IScVxCapabilities read Get_Capabilities;
    property SrvCap_FileTransfer: WordBool read Get_SrvCap_FileTransfer;
    property SrvCap_Chat: WordBool read Get_SrvCap_Chat;
    property MouseCursorMode: CursorMode read Get_MouseCursorMode write Set_MouseCursorMode;
    property remoteInputEnabled: WordBool read Get_remoteInputEnabled write Set_remoteInputEnabled;
  end;

// *********************************************************************//
// DispIntf:  ISmartCodeVNCViewer2Disp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {82B3A16E-4E01-467E-AED4-198EA092D3AF}
// *********************************************************************//
  ISmartCodeVNCViewer2Disp = dispinterface
    ['{82B3A16E-4E01-467E-AED4-198EA092D3AF}']
    property CtrlKeyPressed: WordBool dispid 250;
    property AltKeyPressed: WordBool dispid 251;
    procedure OpenFileTransfer; dispid 252;
    procedure SendCtrlEsq; dispid 253;
    property Capabilities: IScVxCapabilities readonly dispid 254;
    procedure OpenChat; dispid 255;
    property SrvCap_FileTransfer: WordBool readonly dispid 256;
    property SrvCap_Chat: WordBool readonly dispid 257;
    procedure SendCustomKey(keyCode: Integer); dispid 258;
    function IsDormant: WordBool; dispid 259;
    property MouseCursorMode: CursorMode dispid 260;
    property remoteInputEnabled: WordBool dispid 270;
    procedure SelectSingleWindow(bSelect: WordBool); dispid 271;
    procedure SwitchMultiMonitor; dispid 275;
    procedure SendCustomKeyEx(keyCode: Integer; bKeyDownEvent: WordBool); dispid 276;
    property HostIP: WideString dispid 1;
    property Port: Integer dispid 2;
    property Password: WideString dispid 3;
    property CustomCompression: WordBool dispid 4;
    property CustomCompressionLevel: Integer dispid 5;
    property JPEGCompression: WordBool dispid 6;
    property JPEGCompressionLevel: Integer dispid 7;
    property CopyRect: WordBool dispid 8;
    property EmulateThreeButton: WordBool dispid 9;
    property SwapMouseButtons: WordBool dispid 10;
    property Encoding: VNCEncoding dispid 11;
    property ViewOnly: WordBool dispid 12;
    property RestrictPixel: WordBool dispid 13;
    property ScaleNum: Integer dispid 14;
    property ScaleDen: Integer dispid 15;
    property ScaleEnable: WordBool dispid 16;
    property Connected: WordBool readonly dispid 17;
    property FullScreen: WordBool dispid 18;
    property LocalCursor: CursorTrackingMode dispid 19;
    property MessageBoxes: WordBool dispid 20;
    property DisableClipboard: WordBool dispid 21;
    procedure Connect; dispid 22;
    procedure ConnectEx(const strIP: WideString; nPort: Integer; const strPassword: WideString); dispid 23;
    procedure Disconnect; dispid 24;
    procedure SendCAD; dispid 25;
    procedure RequestRefresh; dispid 26;
    procedure ShowConnectionInfo; dispid 27;
    property ScreenBitmap: Integer readonly dispid 28;
    property ScreenWidth: Integer readonly dispid 29;
    property ScreenHeight: Integer readonly dispid 30;
    property RequestSharedSession: WordBool dispid 31;
    function GetScreenBitmapScaled(nWidth: Integer; nHeight: Integer): Integer; dispid 32;
    property ThumbnailMode: WordBool dispid 33;
    property TopLevelParent: Integer dispid 34;
    procedure ConnectAsync; dispid 35;
    procedure ConnectAsyncEx(const strIP: WideString; nPort: Integer; const strPassword: WideString); dispid 36;
    function GetConnectionState: VncConnectionState; dispid 37;
    procedure SetDormant(bDormant: WordBool); dispid 38;
    property LoginType: ViewerLoginType dispid 39;
    property MsUser: WideString dispid 40;
    property MsDomain: WideString dispid 41;
    property MsPassword: WideString dispid 42;
    property ProxyIP: WideString dispid 100;
    property ProxyPort: Integer dispid 101;
    property ProxyUser: WideString dispid 102;
    property ProxyPassword: WideString dispid 103;
    property ProxyType: ConnectionProxyType dispid 104;
    property ConnectionBar: ConnectionBarMode dispid 200;
    property ConnectionBarHeight: Integer writeonly dispid 201;
    property StretchMode: ScreenStretchMode dispid 202;
    property ListenPort: Integer dispid 203;
    property Listening: WordBool readonly dispid 204;
    procedure Listen; dispid 205;
    procedure ListenEx(Port: Integer); dispid 206;
    procedure StopListen; dispid 207;
    function GetBytesSent: Largeuint; dispid 208;
    function GetBytesReceived: Largeuint; dispid 209;
    property DisconnectedText: WideString dispid 210;
    property ConnectingText: WideString dispid 211;
    property ListeningText: WideString dispid 212;
    property AdvancedSettings: IScVxAdvancedSettings readonly dispid 213;
  end;

// *********************************************************************//
// Interface: ISmartCodeVNCViewer3
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {C35468F3-A938-4CB4-ADBB-D0BB37F504CA}
// *********************************************************************//
  ISmartCodeVNCViewer3 = interface(ISmartCodeVNCViewer2)
    ['{C35468F3-A938-4CB4-ADBB-D0BB37F504CA}']
    procedure Set_CacheEncoding(pbAllow: WordBool); safecall;
    function Get_CacheEncoding: WordBool; safecall;
    procedure Set_ColorDepth(pcolorDepth: ColorDepth); safecall;
    function Get_ColorDepth: ColorDepth; safecall;
    procedure SelectSingleWindowAt(x: SYSINT; y: SYSINT); safecall;
    procedure Set_OuterBackgroundColor(pcrColor: Integer); safecall;
    function Get_OuterBackgroundColor: Integer; safecall;
    procedure SendMousePointerEvent(x: SYSINT; y: SYSINT; buttonMask: SYSINT); safecall;
    procedure Set_EnableAutoReconnect(pbEnableAutoReconnect: WordBool); safecall;
    function Get_EnableAutoReconnect: WordBool; safecall;
    procedure Set_EncryptionPlugin(pluginType: EncryptionPluginType); safecall;
    function Get_EncryptionPlugin: EncryptionPluginType; safecall;
    function Get_UltraVNCSecurity_MSRC4: IScVxUltraSecurity_MSRC4; safecall;
    function Get_UltraVNCSecurity_SecureVNC: IScVxUltraSecurity_SecureVNC; safecall;
    procedure Set_ScreenStretchRenderingQuality(pmode: ScreenStretchRenderingQuality); safecall;
    function Get_ScreenStretchRenderingQuality: ScreenStretchRenderingQuality; safecall;
    property CacheEncoding: WordBool read Get_CacheEncoding write Set_CacheEncoding;
    property ColorDepth: ColorDepth read Get_ColorDepth write Set_ColorDepth;
    property OuterBackgroundColor: Integer read Get_OuterBackgroundColor write Set_OuterBackgroundColor;
    property EnableAutoReconnect: WordBool read Get_EnableAutoReconnect write Set_EnableAutoReconnect;
    property EncryptionPlugin: EncryptionPluginType read Get_EncryptionPlugin write Set_EncryptionPlugin;
    property UltraVNCSecurity_MSRC4: IScVxUltraSecurity_MSRC4 read Get_UltraVNCSecurity_MSRC4;
    property UltraVNCSecurity_SecureVNC: IScVxUltraSecurity_SecureVNC read Get_UltraVNCSecurity_SecureVNC;
    property ScreenStretchRenderingQuality: ScreenStretchRenderingQuality read Get_ScreenStretchRenderingQuality write Set_ScreenStretchRenderingQuality;
  end;

// *********************************************************************//
// DispIntf:  ISmartCodeVNCViewer3Disp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {C35468F3-A938-4CB4-ADBB-D0BB37F504CA}
// *********************************************************************//
  ISmartCodeVNCViewer3Disp = dispinterface
    ['{C35468F3-A938-4CB4-ADBB-D0BB37F504CA}']
    property CacheEncoding: WordBool dispid 277;
    property ColorDepth: ColorDepth dispid 278;
    procedure SelectSingleWindowAt(x: SYSINT; y: SYSINT); dispid 279;
    property OuterBackgroundColor: Integer dispid 280;
    procedure SendMousePointerEvent(x: SYSINT; y: SYSINT; buttonMask: SYSINT); dispid 281;
    property EnableAutoReconnect: WordBool dispid 282;
    property EncryptionPlugin: EncryptionPluginType dispid 283;
    property UltraVNCSecurity_MSRC4: IScVxUltraSecurity_MSRC4 readonly dispid 284;
    property UltraVNCSecurity_SecureVNC: IScVxUltraSecurity_SecureVNC readonly dispid 285;
    property ScreenStretchRenderingQuality: ScreenStretchRenderingQuality dispid 286;
    property CtrlKeyPressed: WordBool dispid 250;
    property AltKeyPressed: WordBool dispid 251;
    procedure OpenFileTransfer; dispid 252;
    procedure SendCtrlEsq; dispid 253;
    property Capabilities: IScVxCapabilities readonly dispid 254;
    procedure OpenChat; dispid 255;
    property SrvCap_FileTransfer: WordBool readonly dispid 256;
    property SrvCap_Chat: WordBool readonly dispid 257;
    procedure SendCustomKey(keyCode: Integer); dispid 258;
    function IsDormant: WordBool; dispid 259;
    property MouseCursorMode: CursorMode dispid 260;
    property remoteInputEnabled: WordBool dispid 270;
    procedure SelectSingleWindow(bSelect: WordBool); dispid 271;
    procedure SwitchMultiMonitor; dispid 275;
    procedure SendCustomKeyEx(keyCode: Integer; bKeyDownEvent: WordBool); dispid 276;
    property HostIP: WideString dispid 1;
    property Port: Integer dispid 2;
    property Password: WideString dispid 3;
    property CustomCompression: WordBool dispid 4;
    property CustomCompressionLevel: Integer dispid 5;
    property JPEGCompression: WordBool dispid 6;
    property JPEGCompressionLevel: Integer dispid 7;
    property CopyRect: WordBool dispid 8;
    property EmulateThreeButton: WordBool dispid 9;
    property SwapMouseButtons: WordBool dispid 10;
    property Encoding: VNCEncoding dispid 11;
    property ViewOnly: WordBool dispid 12;
    property RestrictPixel: WordBool dispid 13;
    property ScaleNum: Integer dispid 14;
    property ScaleDen: Integer dispid 15;
    property ScaleEnable: WordBool dispid 16;
    property Connected: WordBool readonly dispid 17;
    property FullScreen: WordBool dispid 18;
    property LocalCursor: CursorTrackingMode dispid 19;
    property MessageBoxes: WordBool dispid 20;
    property DisableClipboard: WordBool dispid 21;
    procedure Connect; dispid 22;
    procedure ConnectEx(const strIP: WideString; nPort: Integer; const strPassword: WideString); dispid 23;
    procedure Disconnect; dispid 24;
    procedure SendCAD; dispid 25;
    procedure RequestRefresh; dispid 26;
    procedure ShowConnectionInfo; dispid 27;
    property ScreenBitmap: Integer readonly dispid 28;
    property ScreenWidth: Integer readonly dispid 29;
    property ScreenHeight: Integer readonly dispid 30;
    property RequestSharedSession: WordBool dispid 31;
    function GetScreenBitmapScaled(nWidth: Integer; nHeight: Integer): Integer; dispid 32;
    property ThumbnailMode: WordBool dispid 33;
    property TopLevelParent: Integer dispid 34;
    procedure ConnectAsync; dispid 35;
    procedure ConnectAsyncEx(const strIP: WideString; nPort: Integer; const strPassword: WideString); dispid 36;
    function GetConnectionState: VncConnectionState; dispid 37;
    procedure SetDormant(bDormant: WordBool); dispid 38;
    property LoginType: ViewerLoginType dispid 39;
    property MsUser: WideString dispid 40;
    property MsDomain: WideString dispid 41;
    property MsPassword: WideString dispid 42;
    property ProxyIP: WideString dispid 100;
    property ProxyPort: Integer dispid 101;
    property ProxyUser: WideString dispid 102;
    property ProxyPassword: WideString dispid 103;
    property ProxyType: ConnectionProxyType dispid 104;
    property ConnectionBar: ConnectionBarMode dispid 200;
    property ConnectionBarHeight: Integer writeonly dispid 201;
    property StretchMode: ScreenStretchMode dispid 202;
    property ListenPort: Integer dispid 203;
    property Listening: WordBool readonly dispid 204;
    procedure Listen; dispid 205;
    procedure ListenEx(Port: Integer); dispid 206;
    procedure StopListen; dispid 207;
    function GetBytesSent: Largeuint; dispid 208;
    function GetBytesReceived: Largeuint; dispid 209;
    property DisconnectedText: WideString dispid 210;
    property ConnectingText: WideString dispid 211;
    property ListeningText: WideString dispid 212;
    property AdvancedSettings: IScVxAdvancedSettings readonly dispid 213;
  end;

// *********************************************************************//
// Interface: IScVxAdvancedSettings
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {200722C4-0B36-48F7-BACF-A9E7AD3D0A7E}
// *********************************************************************//
  IScVxAdvancedSettings = interface(IDispatch)
    ['{200722C4-0B36-48F7-BACF-A9E7AD3D0A7E}']
    procedure SetRawSendData(stage: ConnectionStage; const strText: WideString); safecall;
    function GetRawSendData(stage: ConnectionStage): WideString; safecall;
    procedure Set_EnableScreenUpdatedEvent(pbEnable: WordBool); safecall;
    function Get_EnableScreenUpdatedEvent: WordBool; safecall;
    procedure Set_ShowServerAddress(pbShow: WordBool); safecall;
    function Get_ShowServerAddress: WordBool; safecall;
    procedure Set_UpdatesRequestInterval(pnInterval: Integer); safecall;
    function Get_UpdatesRequestInterval: Integer; safecall;
    procedure Set_ShowRetryButton(pbShow: WordBool); safecall;
    function Get_ShowRetryButton: WordBool; safecall;
    procedure Set_EnableMouseMoveEvent(pbEnable: WordBool); safecall;
    function Get_EnableMouseMoveEvent: WordBool; safecall;
    procedure Set_HotKeyCtrlAltDel(photKeyCtrlAltDel: Integer); safecall;
    function Get_HotKeyCtrlAltDel: Integer; safecall;
    procedure Set_EnableHotKeyCtrlAltDel(penableHotKeyCtrlAltDel: WordBool); safecall;
    function Get_EnableHotKeyCtrlAltDel: WordBool; safecall;
    property EnableScreenUpdatedEvent: WordBool read Get_EnableScreenUpdatedEvent write Set_EnableScreenUpdatedEvent;
    property ShowServerAddress: WordBool read Get_ShowServerAddress write Set_ShowServerAddress;
    property UpdatesRequestInterval: Integer read Get_UpdatesRequestInterval write Set_UpdatesRequestInterval;
    property ShowRetryButton: WordBool read Get_ShowRetryButton write Set_ShowRetryButton;
    property EnableMouseMoveEvent: WordBool read Get_EnableMouseMoveEvent write Set_EnableMouseMoveEvent;
    property HotKeyCtrlAltDel: Integer read Get_HotKeyCtrlAltDel write Set_HotKeyCtrlAltDel;
    property EnableHotKeyCtrlAltDel: WordBool read Get_EnableHotKeyCtrlAltDel write Set_EnableHotKeyCtrlAltDel;
  end;

// *********************************************************************//
// DispIntf:  IScVxAdvancedSettingsDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {200722C4-0B36-48F7-BACF-A9E7AD3D0A7E}
// *********************************************************************//
  IScVxAdvancedSettingsDisp = dispinterface
    ['{200722C4-0B36-48F7-BACF-A9E7AD3D0A7E}']
    procedure SetRawSendData(stage: ConnectionStage; const strText: WideString); dispid 400;
    function GetRawSendData(stage: ConnectionStage): WideString; dispid 401;
    property EnableScreenUpdatedEvent: WordBool dispid 402;
    property ShowServerAddress: WordBool dispid 403;
    property UpdatesRequestInterval: Integer dispid 404;
    property ShowRetryButton: WordBool dispid 405;
    property EnableMouseMoveEvent: WordBool dispid 406;
    property HotKeyCtrlAltDel: Integer dispid 407;
    property EnableHotKeyCtrlAltDel: WordBool dispid 408;
  end;

// *********************************************************************//
// Interface: IScVxCapabilities
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {CC783088-B1D2-4B2F-88CA-51B8ACC638B9}
// *********************************************************************//
  IScVxCapabilities = interface(IDispatch)
    ['{CC783088-B1D2-4B2F-88CA-51B8ACC638B9}']
    function Get_FileTransfer: WordBool; safecall;
    function Get_Chat: WordBool; safecall;
    function Get_EnableRemoteInput: WordBool; safecall;
    function Get_SelectSingleWindow: WordBool; safecall;
    function Get_SwitchMultiMonitor: WordBool; safecall;
    function Get_RfbProtocolVersionMajor: SYSINT; safecall;
    function Get_RfbProtocolVersionMinor: SYSINT; safecall;
    property FileTransfer: WordBool read Get_FileTransfer;
    property Chat: WordBool read Get_Chat;
    property EnableRemoteInput: WordBool read Get_EnableRemoteInput;
    property SelectSingleWindow: WordBool read Get_SelectSingleWindow;
    property SwitchMultiMonitor: WordBool read Get_SwitchMultiMonitor;
    property RfbProtocolVersionMajor: SYSINT read Get_RfbProtocolVersionMajor;
    property RfbProtocolVersionMinor: SYSINT read Get_RfbProtocolVersionMinor;
  end;

// *********************************************************************//
// DispIntf:  IScVxCapabilitiesDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {CC783088-B1D2-4B2F-88CA-51B8ACC638B9}
// *********************************************************************//
  IScVxCapabilitiesDisp = dispinterface
    ['{CC783088-B1D2-4B2F-88CA-51B8ACC638B9}']
    property FileTransfer: WordBool readonly dispid 600;
    property Chat: WordBool readonly dispid 601;
    property EnableRemoteInput: WordBool readonly dispid 602;
    property SelectSingleWindow: WordBool readonly dispid 603;
    property SwitchMultiMonitor: WordBool readonly dispid 604;
    property RfbProtocolVersionMajor: SYSINT readonly dispid 605;
    property RfbProtocolVersionMinor: SYSINT readonly dispid 606;
  end;

// *********************************************************************//
// Interface: IScVxUltraSecurity_MSRC4
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {FE105750-449E-4220-ADF5-AB8EF7D6906A}
// *********************************************************************//
  IScVxUltraSecurity_MSRC4 = interface(IDispatch)
    ['{FE105750-449E-4220-ADF5-AB8EF7D6906A}']
    procedure Set_KeyFilePath(const pstrKeyPath: WideString); safecall;
    function Get_KeyFilePath: WideString; safecall;
    procedure Set_KeyStorage(pStorage: DsmKeyStorage); safecall;
    function Get_KeyStorage: DsmKeyStorage; safecall;
    procedure Set_KeyData(Param1: OleVariant); safecall;
    procedure Set_KeyDataAsHexStr(const Param1: WideString); safecall;
    property KeyFilePath: WideString read Get_KeyFilePath write Set_KeyFilePath;
    property KeyStorage: DsmKeyStorage read Get_KeyStorage write Set_KeyStorage;
    property KeyData: OleVariant write Set_KeyData;
    property KeyDataAsHexStr: WideString write Set_KeyDataAsHexStr;
  end;

// *********************************************************************//
// DispIntf:  IScVxUltraSecurity_MSRC4Disp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {FE105750-449E-4220-ADF5-AB8EF7D6906A}
// *********************************************************************//
  IScVxUltraSecurity_MSRC4Disp = dispinterface
    ['{FE105750-449E-4220-ADF5-AB8EF7D6906A}']
    property KeyFilePath: WideString dispid 701;
    property KeyStorage: DsmKeyStorage dispid 703;
    property KeyData: OleVariant writeonly dispid 704;
    property KeyDataAsHexStr: WideString writeonly dispid 705;
  end;

// *********************************************************************//
// Interface: IScVxUltraSecurity_SecureVNC
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {5D2B76F9-B1F4-43B5-A207-F23EA608BA49}
// *********************************************************************//
  IScVxUltraSecurity_SecureVNC = interface(IDispatch)
    ['{5D2B76F9-B1F4-43B5-A207-F23EA608BA49}']
    procedure Set_PrivateKeyFilePath(const pstrKeyPath: WideString); safecall;
    function Get_PrivateKeyFilePath: WideString; safecall;
    procedure Set_KeyStorage(pStorage: DsmKeyStorage); safecall;
    function Get_KeyStorage: DsmKeyStorage; safecall;
    procedure Set_PrivateKeyData(Param1: OleVariant); safecall;
    procedure Set_PrivateKeyDataAsHexStr(const Param1: WideString); safecall;
    procedure Set_Passphrase(const Param1: WideString); safecall;
    property PrivateKeyFilePath: WideString read Get_PrivateKeyFilePath write Set_PrivateKeyFilePath;
    property KeyStorage: DsmKeyStorage read Get_KeyStorage write Set_KeyStorage;
    property PrivateKeyData: OleVariant write Set_PrivateKeyData;
    property PrivateKeyDataAsHexStr: WideString write Set_PrivateKeyDataAsHexStr;
    property Passphrase: WideString write Set_Passphrase;
  end;

// *********************************************************************//
// DispIntf:  IScVxUltraSecurity_SecureVNCDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {5D2B76F9-B1F4-43B5-A207-F23EA608BA49}
// *********************************************************************//
  IScVxUltraSecurity_SecureVNCDisp = dispinterface
    ['{5D2B76F9-B1F4-43B5-A207-F23EA608BA49}']
    property PrivateKeyFilePath: WideString dispid 720;
    property KeyStorage: DsmKeyStorage dispid 722;
    property PrivateKeyData: OleVariant writeonly dispid 723;
    property PrivateKeyDataAsHexStr: WideString writeonly dispid 724;
    property Passphrase: WideString writeonly dispid 725;
  end;


// *********************************************************************//
// OLE Control Proxy class declaration
// Control Name     : TCSC_ViewerXControl
// Help String      : SmartCode VNC Viewer Control
// Default Interface: ISmartCodeVNCViewer3
// Def. Intf. DISP? : No
// Event   Interface: _ISmartCodeVNCViewerEvents
// TypeFlags        : (2) CanCreate
// *********************************************************************//
  TCSC_ViewerXControlServerDimension = procedure(ASender: TObject; nWidth: Integer; nHeight: Integer) of object;
  TCSC_ViewerXControlConnectionAccepted = procedure(ASender: TObject; const bstrServerAddress: WideString) of object;
  TCSC_ViewerXControlAuthenticationFailed = procedure(ASender: TObject; stage: AuthenticationStage; 
                                                                        out pbCancelAndDontPromptForPassword: WordBool) of object;
  TCSC_ViewerXControlOnChatMessageSend = procedure(ASender: TObject; const messageText: WideString) of object;
  TCSC_ViewerXControlOnChatMessageReceived = procedure(ASender: TObject; const messageText: WideString) of object;
  TCSC_ViewerXControlOnDisableRemoteInputChanged = procedure(ASender: TObject; remoteInputEnabled: WordBool) of object;
  TCSC_ViewerXControlOnAutoReconnecting = procedure(ASender: TObject; attemptCount: Integer; 
                                                                      out pArcContinueStatus: AutoReconnectContinueState) of object;
  TCSC_ViewerXControlOnMouseMove = procedure(ASender: TObject; x: Integer; y: Integer) of object;

  TCSC_ViewerXControl = class(TOleControl)
  private
    FOnConnected: TNotifyEvent;
    FOnDisconnected: TNotifyEvent;
    FOnServerDimension: TCSC_ViewerXControlServerDimension;
    FOnScreenUpdated: TNotifyEvent;
    FOnConnecting: TNotifyEvent;
    FOnRequestShow: TNotifyEvent;
    FOnRequestHide: TNotifyEvent;
    FOnConnectionAccepted: TCSC_ViewerXControlConnectionAccepted;
    FOnAuthenticationFailed: TCSC_ViewerXControlAuthenticationFailed;
    FOnChatSessionStarted: TNotifyEvent;
    FOnChatSessionEnded: TNotifyEvent;
    FOnChatMessageSend: TCSC_ViewerXControlOnChatMessageSend;
    FOnChatMessageReceived: TCSC_ViewerXControlOnChatMessageReceived;
    FOnDisableRemoteInputChanged: TCSC_ViewerXControlOnDisableRemoteInputChanged;
    FOnAutoReconnecting: TCSC_ViewerXControlOnAutoReconnecting;
    FOnMouseMove: TCSC_ViewerXControlOnMouseMove;
    FIntf: ISmartCodeVNCViewer3;
    function  GetControlInterface: ISmartCodeVNCViewer3;
  protected
    procedure CreateControl;
    procedure InitControlData; override;
    function Get_AdvancedSettings: IScVxAdvancedSettings;
    function Get_Capabilities: IScVxCapabilities;
    function Get_UltraVNCSecurity_MSRC4: IScVxUltraSecurity_MSRC4;
    function Get_UltraVNCSecurity_SecureVNC: IScVxUltraSecurity_SecureVNC;
  public
    procedure Connect;
    procedure ConnectEx(const strIP: WideString; nPort: Integer; const strPassword: WideString);
    procedure Disconnect;
    procedure SendCAD;
    procedure RequestRefresh;
    procedure ShowConnectionInfo;
    function GetScreenBitmapScaled(nWidth: Integer; nHeight: Integer): Integer;
    procedure ConnectAsync;
    procedure ConnectAsyncEx(const strIP: WideString; nPort: Integer; const strPassword: WideString);
    function GetConnectionState: VncConnectionState;
    procedure SetDormant(bDormant: WordBool);
    procedure Listen;
    procedure ListenEx(Port: Integer);
    procedure StopListen;
    function GetBytesSent: Largeuint;
    function GetBytesReceived: Largeuint;
    procedure OpenFileTransfer;
    procedure SendCtrlEsq;
    procedure OpenChat;
    procedure SendCustomKey(keyCode: Integer);
    function IsDormant: WordBool;
    procedure SelectSingleWindow(bSelect: WordBool);
    procedure SwitchMultiMonitor;
    procedure SendCustomKeyEx(keyCode: Integer; bKeyDownEvent: WordBool);
    procedure SelectSingleWindowAt(x: SYSINT; y: SYSINT);
    procedure SendMousePointerEvent(x: SYSINT; y: SYSINT; buttonMask: SYSINT);
    property  ControlInterface: ISmartCodeVNCViewer3 read GetControlInterface;
    property  DefaultInterface: ISmartCodeVNCViewer3 read GetControlInterface;
    property Connected: WordBool index 17 read GetWordBoolProp;
    property ScreenBitmap: Integer index 28 read GetIntegerProp;
    property ScreenWidth: Integer index 29 read GetIntegerProp;
    property ScreenHeight: Integer index 30 read GetIntegerProp;
    property LoginType: TOleEnum index 39 read GetTOleEnumProp write SetTOleEnumProp;
    property ConnectionBar: TOleEnum index 200 read GetTOleEnumProp write SetTOleEnumProp;
    property ConnectionBarHeight: Integer index 201 write SetIntegerProp;
    property Listening: WordBool index 204 read GetWordBoolProp;
    property AdvancedSettings: IScVxAdvancedSettings read Get_AdvancedSettings;
    property Capabilities: IScVxCapabilities read Get_Capabilities;
    property SrvCap_FileTransfer: WordBool index 256 read GetWordBoolProp;
    property SrvCap_Chat: WordBool index 257 read GetWordBoolProp;
    property UltraVNCSecurity_MSRC4: IScVxUltraSecurity_MSRC4 read Get_UltraVNCSecurity_MSRC4;
    property UltraVNCSecurity_SecureVNC: IScVxUltraSecurity_SecureVNC read Get_UltraVNCSecurity_SecureVNC;
  published
    property Anchors;
    property  TabStop;
    property  Align;
    property  DragCursor;
    property  DragMode;
    property  ParentShowHint;
    property  PopupMenu;
    property  ShowHint;
    property  TabOrder;
    property  Visible;
    property  OnDragDrop;
    property  OnDragOver;
    property  OnEndDrag;
    property  OnEnter;
    property  OnExit;
    property  OnStartDrag;
    property HostIP: WideString index 1 read GetWideStringProp write SetWideStringProp stored False;
    property Port: Integer index 2 read GetIntegerProp write SetIntegerProp stored False;
    property Password: WideString index 3 read GetWideStringProp write SetWideStringProp stored False;
    property CustomCompression: WordBool index 4 read GetWordBoolProp write SetWordBoolProp stored False;
    property CustomCompressionLevel: Integer index 5 read GetIntegerProp write SetIntegerProp stored False;
    property JPEGCompression: WordBool index 6 read GetWordBoolProp write SetWordBoolProp stored False;
    property JPEGCompressionLevel: Integer index 7 read GetIntegerProp write SetIntegerProp stored False;
    property CopyRect: WordBool index 8 read GetWordBoolProp write SetWordBoolProp stored False;
    property EmulateThreeButton: WordBool index 9 read GetWordBoolProp write SetWordBoolProp stored False;
    property SwapMouseButtons: WordBool index 10 read GetWordBoolProp write SetWordBoolProp stored False;
    property Encoding: TOleEnum index 11 read GetTOleEnumProp write SetTOleEnumProp stored False;
    property ViewOnly: WordBool index 12 read GetWordBoolProp write SetWordBoolProp stored False;
    property RestrictPixel: WordBool index 13 read GetWordBoolProp write SetWordBoolProp stored False;
    property ScaleNum: Integer index 14 read GetIntegerProp write SetIntegerProp stored False;
    property ScaleDen: Integer index 15 read GetIntegerProp write SetIntegerProp stored False;
    property ScaleEnable: WordBool index 16 read GetWordBoolProp write SetWordBoolProp stored False;
    property FullScreen: WordBool index 18 read GetWordBoolProp write SetWordBoolProp stored False;
    property LocalCursor: TOleEnum index 19 read GetTOleEnumProp write SetTOleEnumProp stored False;
    property MessageBoxes: WordBool index 20 read GetWordBoolProp write SetWordBoolProp stored False;
    property DisableClipboard: WordBool index 21 read GetWordBoolProp write SetWordBoolProp stored False;
    property RequestSharedSession: WordBool index 31 read GetWordBoolProp write SetWordBoolProp stored False;
    property ThumbnailMode: WordBool index 33 read GetWordBoolProp write SetWordBoolProp stored False;
    property TopLevelParent: Integer index 34 read GetIntegerProp write SetIntegerProp stored False;
    property MsUser: WideString index 40 read GetWideStringProp write SetWideStringProp stored False;
    property MsDomain: WideString index 41 read GetWideStringProp write SetWideStringProp stored False;
    property MsPassword: WideString index 42 read GetWideStringProp write SetWideStringProp stored False;
    property ProxyIP: WideString index 100 read GetWideStringProp write SetWideStringProp stored False;
    property ProxyPort: Integer index 101 read GetIntegerProp write SetIntegerProp stored False;
    property ProxyUser: WideString index 102 read GetWideStringProp write SetWideStringProp stored False;
    property ProxyPassword: WideString index 103 read GetWideStringProp write SetWideStringProp stored False;
    property ProxyType: TOleEnum index 104 read GetTOleEnumProp write SetTOleEnumProp stored False;
    property StretchMode: TOleEnum index 202 read GetTOleEnumProp write SetTOleEnumProp stored False;
    property ListenPort: Integer index 203 read GetIntegerProp write SetIntegerProp stored False;
    property DisconnectedText: WideString index 210 read GetWideStringProp write SetWideStringProp stored False;
    property ConnectingText: WideString index 211 read GetWideStringProp write SetWideStringProp stored False;
    property ListeningText: WideString index 212 read GetWideStringProp write SetWideStringProp stored False;
    property CtrlKeyPressed: WordBool index 250 read GetWordBoolProp write SetWordBoolProp stored False;
    property AltKeyPressed: WordBool index 251 read GetWordBoolProp write SetWordBoolProp stored False;
    property MouseCursorMode: TOleEnum index 260 read GetTOleEnumProp write SetTOleEnumProp stored False;
    property remoteInputEnabled: WordBool index 270 read GetWordBoolProp write SetWordBoolProp stored False;
    property CacheEncoding: WordBool index 277 read GetWordBoolProp write SetWordBoolProp stored False;
    property ColorDepth: TOleEnum index 278 read GetTOleEnumProp write SetTOleEnumProp stored False;
    property OuterBackgroundColor: Integer index 280 read GetIntegerProp write SetIntegerProp stored False;
    property EnableAutoReconnect: WordBool index 282 read GetWordBoolProp write SetWordBoolProp stored False;
    property EncryptionPlugin: TOleEnum index 283 read GetTOleEnumProp write SetTOleEnumProp stored False;
    property ScreenStretchRenderingQuality: TOleEnum index 286 read GetTOleEnumProp write SetTOleEnumProp stored False;
    property OnConnected: TNotifyEvent read FOnConnected write FOnConnected;
    property OnDisconnected: TNotifyEvent read FOnDisconnected write FOnDisconnected;
    property OnServerDimension: TCSC_ViewerXControlServerDimension read FOnServerDimension write FOnServerDimension;
    property OnScreenUpdated: TNotifyEvent read FOnScreenUpdated write FOnScreenUpdated;
    property OnConnecting: TNotifyEvent read FOnConnecting write FOnConnecting;
    property OnRequestShow: TNotifyEvent read FOnRequestShow write FOnRequestShow;
    property OnRequestHide: TNotifyEvent read FOnRequestHide write FOnRequestHide;
    property OnConnectionAccepted: TCSC_ViewerXControlConnectionAccepted read FOnConnectionAccepted write FOnConnectionAccepted;
    property OnAuthenticationFailed: TCSC_ViewerXControlAuthenticationFailed read FOnAuthenticationFailed write FOnAuthenticationFailed;
    property OnChatSessionStarted: TNotifyEvent read FOnChatSessionStarted write FOnChatSessionStarted;
    property OnChatSessionEnded: TNotifyEvent read FOnChatSessionEnded write FOnChatSessionEnded;
    property OnChatMessageSend: TCSC_ViewerXControlOnChatMessageSend read FOnChatMessageSend write FOnChatMessageSend;
    property OnChatMessageReceived: TCSC_ViewerXControlOnChatMessageReceived read FOnChatMessageReceived write FOnChatMessageReceived;
    property OnDisableRemoteInputChanged: TCSC_ViewerXControlOnDisableRemoteInputChanged read FOnDisableRemoteInputChanged write FOnDisableRemoteInputChanged;
    property OnAutoReconnecting: TCSC_ViewerXControlOnAutoReconnecting read FOnAutoReconnecting write FOnAutoReconnecting;
    property OnMouseMove: TCSC_ViewerXControlOnMouseMove read FOnMouseMove write FOnMouseMove;
  end;

procedure Register;

resourcestring
  dtlServerPage = 'ActiveX';

  dtlOcxPage = 'ActiveX';

implementation

uses System.Win.ComObj;

procedure TCSC_ViewerXControl.InitControlData;
const
  CEventDispIDs: array [0..15] of DWORD = (
    $00000001, $00000002, $00000003, $00000004, $00000005, $00000006,
    $00000007, $00000008, $00000009, $0000000A, $0000000B, $0000000C,
    $0000000D, $0000000E, $0000000F, $00000010);
  CLicenseKey: array[0..13] of Word = ( $0073, $0063, $0076, $0069, $0065, $0077, $0078, $006C, $0069, $0063, $006B
    , $0065, $0079, $0000);
  CControlData: TControlData2 = (
    ClassID:      '{62FA83F7-20EC-4D62-AC86-BAB705EE1CCD}';
    EventIID:     '{301C38CE-584C-4E08-A545-140A08892FB0}';
    EventCount:   16;
    EventDispIDs: @CEventDispIDs;
    LicenseKey:   @CLicenseKey;
    Flags:        $00000000;
    Version:      500);
begin
  ControlData := @CControlData;
  TControlData2(CControlData).FirstEventOfs := UIntPtr(@@FOnConnected) - UIntPtr(Self);
end;

procedure TCSC_ViewerXControl.CreateControl;

  procedure DoCreate;
  begin
    FIntf := IUnknown(OleObject) as ISmartCodeVNCViewer3;
  end;

begin
  if FIntf = nil then DoCreate;
end;

function TCSC_ViewerXControl.GetControlInterface: ISmartCodeVNCViewer3;
begin
  CreateControl;
  Result := FIntf;
end;

function TCSC_ViewerXControl.Get_AdvancedSettings: IScVxAdvancedSettings;
begin
  Result := DefaultInterface.AdvancedSettings;
end;

function TCSC_ViewerXControl.Get_Capabilities: IScVxCapabilities;
begin
  Result := DefaultInterface.Capabilities;
end;

function TCSC_ViewerXControl.Get_UltraVNCSecurity_MSRC4: IScVxUltraSecurity_MSRC4;
begin
  Result := DefaultInterface.UltraVNCSecurity_MSRC4;
end;

function TCSC_ViewerXControl.Get_UltraVNCSecurity_SecureVNC: IScVxUltraSecurity_SecureVNC;
begin
  Result := DefaultInterface.UltraVNCSecurity_SecureVNC;
end;

procedure TCSC_ViewerXControl.Connect;
begin
  DefaultInterface.Connect;
end;

procedure TCSC_ViewerXControl.ConnectEx(const strIP: WideString; nPort: Integer; 
                                        const strPassword: WideString);
begin
  DefaultInterface.ConnectEx(strIP, nPort, strPassword);
end;

procedure TCSC_ViewerXControl.Disconnect;
begin
  DefaultInterface.Disconnect;
end;

procedure TCSC_ViewerXControl.SendCAD;
begin
  DefaultInterface.SendCAD;
end;

procedure TCSC_ViewerXControl.RequestRefresh;
begin
  DefaultInterface.RequestRefresh;
end;

procedure TCSC_ViewerXControl.ShowConnectionInfo;
begin
  DefaultInterface.ShowConnectionInfo;
end;

function TCSC_ViewerXControl.GetScreenBitmapScaled(nWidth: Integer; nHeight: Integer): Integer;
begin
  Result := DefaultInterface.GetScreenBitmapScaled(nWidth, nHeight);
end;

procedure TCSC_ViewerXControl.ConnectAsync;
begin
  DefaultInterface.ConnectAsync;
end;

procedure TCSC_ViewerXControl.ConnectAsyncEx(const strIP: WideString; nPort: Integer; 
                                             const strPassword: WideString);
begin
  DefaultInterface.ConnectAsyncEx(strIP, nPort, strPassword);
end;

function TCSC_ViewerXControl.GetConnectionState: VncConnectionState;
begin
  Result := DefaultInterface.GetConnectionState;
end;

procedure TCSC_ViewerXControl.SetDormant(bDormant: WordBool);
begin
  DefaultInterface.SetDormant(bDormant);
end;

procedure TCSC_ViewerXControl.Listen;
begin
  DefaultInterface.Listen;
end;

procedure TCSC_ViewerXControl.ListenEx(Port: Integer);
begin
  DefaultInterface.ListenEx(Port);
end;

procedure TCSC_ViewerXControl.StopListen;
begin
  DefaultInterface.StopListen;
end;

function TCSC_ViewerXControl.GetBytesSent: Largeuint;
begin
  Result := DefaultInterface.GetBytesSent;
end;

function TCSC_ViewerXControl.GetBytesReceived: Largeuint;
begin
  Result := DefaultInterface.GetBytesReceived;
end;

procedure TCSC_ViewerXControl.OpenFileTransfer;
begin
  DefaultInterface.OpenFileTransfer;
end;

procedure TCSC_ViewerXControl.SendCtrlEsq;
begin
  DefaultInterface.SendCtrlEsq;
end;

procedure TCSC_ViewerXControl.OpenChat;
begin
  DefaultInterface.OpenChat;
end;

procedure TCSC_ViewerXControl.SendCustomKey(keyCode: Integer);
begin
  DefaultInterface.SendCustomKey(keyCode);
end;

function TCSC_ViewerXControl.IsDormant: WordBool;
begin
  Result := DefaultInterface.IsDormant;
end;

procedure TCSC_ViewerXControl.SelectSingleWindow(bSelect: WordBool);
begin
  DefaultInterface.SelectSingleWindow(bSelect);
end;

procedure TCSC_ViewerXControl.SwitchMultiMonitor;
begin
  DefaultInterface.SwitchMultiMonitor;
end;

procedure TCSC_ViewerXControl.SendCustomKeyEx(keyCode: Integer; bKeyDownEvent: WordBool);
begin
  DefaultInterface.SendCustomKeyEx(keyCode, bKeyDownEvent);
end;

procedure TCSC_ViewerXControl.SelectSingleWindowAt(x: SYSINT; y: SYSINT);
begin
  DefaultInterface.SelectSingleWindowAt(x, y);
end;

procedure TCSC_ViewerXControl.SendMousePointerEvent(x: SYSINT; y: SYSINT; buttonMask: SYSINT);
begin
  DefaultInterface.SendMousePointerEvent(x, y, buttonMask);
end;

procedure Register;
begin
  RegisterComponents(dtlOcxPage, [TCSC_ViewerXControl]);
end;

end.
