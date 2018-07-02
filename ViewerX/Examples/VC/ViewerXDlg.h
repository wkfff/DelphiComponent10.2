// ViewerXDlg.h : header file
//
//{{AFX_INCLUDES()
#include "smartcodevncviewer.h"
#include "afxwin.h"
//}}AFX_INCLUDES

#if !defined(AFX_VIEWERXDLG_H__087FD9FC_9685_4D10_AC58_4F1C497B37D2__INCLUDED_)
#define AFX_VIEWERXDLG_H__087FD9FC_9685_4D10_AC58_4F1C497B37D2__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

/////////////////////////////////////////////////////////////////////////////
// CViewerXDlg dialog

class CViewerXDlg : public CDialog
{
// Construction
public:
	CViewerXDlg(CWnd* pParent = NULL);	// standard constructor

// Dialog Data
	//{{AFX_DATA(CViewerXDlg)
	enum { IDD = IDD_VIEWERX_DIALOG };
	CButton	m_ctlConnect;
	CEdit	m_ctlIP;
	CEdit	m_ctlPort;
	CComboBox m_ctrlStretchCombo;
	CSmartCodeVNCViewer	m_viewer;
	//}}AFX_DATA

	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CViewerXDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);	// DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:
	HICON m_hIcon;

	// Generated message map functions
	//{{AFX_MSG(CViewerXDlg)
	virtual BOOL OnInitDialog();
	afx_msg void OnPaint();
	afx_msg HCURSOR OnQueryDragIcon();
	afx_msg void OnConnectButton();
	afx_msg void OnVNCConnected();
	afx_msg void OnVNCDisconnected();
	DECLARE_EVENTSINK_MAP()
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
public:
	afx_msg void OnBnClickedCadButton();
	afx_msg void OnBnClickedFtButton();
	afx_msg void OnCbnSelchangeComboStretch();
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_VIEWERXDLG_H__087FD9FC_9685_4D10_AC58_4F1C497B37D2__INCLUDED_)
