// ViewerXDlg.cpp : implementation file
//

#include "stdafx.h"
#include "ViewerX.h"
#include "ViewerXDlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CViewerXDlg dialog

CViewerXDlg::CViewerXDlg(CWnd* pParent /*=NULL*/)
	: CDialog(CViewerXDlg::IDD, pParent)
{
	//{{AFX_DATA_INIT(CViewerXDlg)
		// NOTE: the ClassWizard will add member initialization here
	//}}AFX_DATA_INIT
	// Note that LoadIcon does not require a subsequent DestroyIcon in Win32
	m_hIcon = AfxGetApp()->LoadIcon(IDR_MAINFRAME);
}

void CViewerXDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CViewerXDlg)
	DDX_Control(pDX, IDOK, m_ctlConnect);
	DDX_Control(pDX, IDC_IP, m_ctlIP);
	DDX_Control(pDX, IDC_PORT, m_ctlPort);
	DDX_Control(pDX, IDC_VIEWERX, m_viewer);
	DDX_Control(pDX, IDC_COMBO_STRETCH, m_ctrlStretchCombo);
	//}}AFX_DATA_MAP
}

BEGIN_MESSAGE_MAP(CViewerXDlg, CDialog)
	//{{AFX_MSG_MAP(CViewerXDlg)
	ON_WM_PAINT()
	ON_WM_QUERYDRAGICON()
	ON_BN_CLICKED(IDOK, OnConnectButton)
	//}}AFX_MSG_MAP
	ON_BN_CLICKED(IDC_CAD_BUTTON, &CViewerXDlg::OnBnClickedCadButton)
	ON_BN_CLICKED(IDC_FT_BUTTON, &CViewerXDlg::OnBnClickedFtButton)
	ON_CBN_SELCHANGE(IDC_COMBO_STRETCH, &CViewerXDlg::OnCbnSelchangeComboStretch)
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CViewerXDlg message handlers

BOOL CViewerXDlg::OnInitDialog()
{
	CDialog::OnInitDialog();

	//set to default VNC port
	m_ctlPort.SetWindowText(_T("5900"));

	//disable stretching by default
	m_ctrlStretchCombo.SetCurSel(0);

	//use this window handle as a parent window for ViewerX error message boxes
	m_viewer.put_TopLevelParent(reinterpret_cast<long>(m_hWnd));

	// Set the icon for this dialog.  The framework does this automatically
	//  when the application's main window is not a dialog
	SetIcon(m_hIcon, TRUE);			// Set big icon
	SetIcon(m_hIcon, FALSE);		// Set small icon
	
	return TRUE;  // return TRUE  unless you set the focus to a control
}

// If you add a minimize button to your dialog, you will need the code below
//  to draw the icon.  For MFC applications using the document/view model,
//  this is automatically done for you by the framework.

void CViewerXDlg::OnPaint() 
{
	if (IsIconic())
	{
		CPaintDC dc(this); // device context for painting

		SendMessage(WM_ICONERASEBKGND, (WPARAM) dc.GetSafeHdc(), 0);

		// Center icon in client rectangle
		int cxIcon = GetSystemMetrics(SM_CXICON);
		int cyIcon = GetSystemMetrics(SM_CYICON);
		CRect rect;
		GetClientRect(&rect);
		int x = (rect.Width() - cxIcon + 1) / 2;
		int y = (rect.Height() - cyIcon + 1) / 2;

		// Draw the icon
		dc.DrawIcon(x, y, m_hIcon);
	}
	else
	{
		CDialog::OnPaint();
	}
}

// The system calls this to obtain the cursor to display while the user drags
//  the minimized window.
HCURSOR CViewerXDlg::OnQueryDragIcon()
{
	return (HCURSOR) m_hIcon;
}

void CViewerXDlg::OnConnectButton() 
{
	if (m_viewer.get_Connected())
	{
		m_viewer.Disconnect();
	}
	else
	{
		CString strText;
		strText.LoadString(IDS_DISCONNECT);
		SetDlgItemText(IDOK, strText);

		CString strIP;
		m_ctlIP.GetWindowText(strIP);

		long nPort = GetDlgItemInt(IDC_PORT);

		m_viewer.put_HostIP(strIP);
		m_viewer.put_Port(nPort);
		m_viewer.put_StretchMode(m_ctrlStretchCombo.GetCurSel());

		m_viewer.ConnectAsync();
	}
}

BEGIN_EVENTSINK_MAP(CViewerXDlg, CDialog)
    //{{AFX_EVENTSINK_MAP(CViewerXDlg)
	ON_EVENT(CViewerXDlg, IDC_VIEWERX, 1 /* Connected */, OnVNCConnected, VTS_NONE)
	ON_EVENT(CViewerXDlg, IDC_VIEWERX, 2 /* Disconnected */, OnVNCDisconnected, VTS_NONE)
	//}}AFX_EVENTSINK_MAP
END_EVENTSINK_MAP()

void CViewerXDlg::OnVNCConnected() 
{
	GetDlgItem(IDC_CAD_BUTTON)->EnableWindow(TRUE);
	GetDlgItem(IDC_FT_BUTTON)->EnableWindow(m_viewer.get_SrvCap_FileTransfer());
}

void CViewerXDlg::OnVNCDisconnected() 
{
	CString strText;
	strText.LoadString(IDS_CONNECT);
	SetDlgItemText(IDOK, strText);

	GetDlgItem(IDC_CAD_BUTTON)->EnableWindow(FALSE);
	GetDlgItem(IDC_FT_BUTTON)->EnableWindow(FALSE);
}

void CViewerXDlg::OnBnClickedCadButton()
{
	m_viewer.SendCAD();
}

void CViewerXDlg::OnBnClickedFtButton()
{
	if (m_viewer.get_SrvCap_FileTransfer())
	m_viewer.OpenFileTransfer();
}

void CViewerXDlg::OnCbnSelchangeComboStretch()
{
	m_viewer.put_StretchMode(m_ctrlStretchCombo.GetCurSel());
}

