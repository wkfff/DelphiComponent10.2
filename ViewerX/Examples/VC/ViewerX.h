// ViewerX.h : main header file for the VIEWERX application
//

#if !defined(AFX_VIEWERX_H__B5509DD4_9B31_444C_84E7_5A4014E47698__INCLUDED_)
#define AFX_VIEWERX_H__B5509DD4_9B31_444C_84E7_5A4014E47698__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#ifndef __AFXWIN_H__
	#error include 'stdafx.h' before including this file for PCH
#endif

#include "resource.h"		// main symbols

/////////////////////////////////////////////////////////////////////////////
// CViewerXApp:
// See ViewerX.cpp for the implementation of this class
//

class CViewerXApp : public CWinApp
{
public:
	CViewerXApp();

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CViewerXApp)
	public:
	virtual BOOL InitInstance();
	//}}AFX_VIRTUAL

// Implementation

	//{{AFX_MSG(CViewerXApp)
		// NOTE - the ClassWizard will add and remove member functions here.
		//    DO NOT EDIT what you see in these blocks of generated code !
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};


/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_VIEWERX_H__B5509DD4_9B31_444C_84E7_5A4014E47698__INCLUDED_)
