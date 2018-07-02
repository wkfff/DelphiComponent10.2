using System;
using System.Diagnostics;
using System.Drawing;
using System.IO;
using System.Windows.Forms;
using System.Runtime.InteropServices;

namespace ViewerX
{
	/// <summary>
	/// Summary description for Form1.
	/// </summary>
	public partial class Form1 : Form
	{
		public Form1()
		{
			//
			// Required for Windows Form Designer support
			//
			InitializeComponent();

			ConnectViewerXEvents();

			PopulateEncodings();
			PopulateColors();

			cbColorDepth.SelectedIndexChanged += cbColorDepth_SelectedIndexChanged;
			cbEncodings.SelectedIndexChanged += cbEncodings_SelectedIndexChanged;

			cbStretch.SelectedIndex = 0;
			cbScaleEngine.SelectedIndex = (int)viewerX.ScreenStretchRenderingQuality;
			cbScaleEngine.SelectedIndexChanged += cbScaleEngine_SelectedIndexChanged;

			cbDsm.SelectedIndex = 0;

			btnConnect.Font = new Font(btnConnect.Font, FontStyle.Bold);
		}

		/// <summary>
		/// The main entry point for the application.
		/// </summary>
		[STAThread]
		static void Main() 
		{
			Application.EnableVisualStyles();
			Application.SetCompatibleTextRenderingDefault(false);
			
			try
			{
				Application.Run(new Form1());
			}
			catch (COMException ex)
			{
				//0x80040154 class not registered error. 
				//running the sample without registering ViewerX ActiveX dll.
				if (ex.ErrorCode == -2147221164)
				{
					MessageBox.Show(Properties.Resources.controlIsNotRegistered,
						Properties.Resources.demoTitle, MessageBoxButtons.OK,
						MessageBoxIcon.Exclamation);
					return;
				}

				throw;
			}
		}

		private ScreenStretchMode StretchMode
		{
			get
			{
				switch (cbStretch.SelectedIndex)
				{
					case 0: return ScreenStretchMode.SSM_NONE;
					case 1: return ScreenStretchMode.SSM_FREE;
					case 2: return ScreenStretchMode.SSM_ASPECT;
				}

				Debug.Assert(false);

				return ScreenStretchMode.SSM_NONE;
			}
		}

		private void UpdateButtons()
		{
			var conState = viewerX.GetConnectionState();
			bool connected = conState == VncConnectionState.VCS_CONNECTED;

			btnConnect.Text = conState == VncConnectionState.VCS_CONNECTING || connected ? "Disconnect" : "Connect";

			btnFileTransfer.Enabled = btnSendCad.Enabled = btnChat.Enabled =
				btnGetScaled.Enabled = connected;

			tableLayoutPanelDsm.Enabled = !connected;
		}

		private void ConnectViewerXEvents()
		{
			viewerX.Disconnected += OnViewerX_Disconnected;
			viewerX.ConnectedEvent += OnViewerX_Connected;

			viewerX.OnChatSessionStarted += viewerX_OnChatSessionStarted;
			viewerX.OnChatSessionEnded += viewerX_OnChatSessionEnded;
			viewerX.OnChatMessageReceived += viewerX_OnChatMessageReceived;
			viewerX.OnChatMessageSend += viewerX_OnChatMessageSend;
		}

		#region Chat event handlers

		private void viewerX_OnChatSessionStarted(object sender, EventArgs e)
		{
			txtChatLog.AppendText("Chat Started");
			txtChatLog.AppendText(Environment.NewLine + "-------------------" + Environment.NewLine);
		}

		private void viewerX_OnChatSessionEnded(object sender, EventArgs e)
		{
			txtChatLog.AppendText("Chat Ended");
			txtChatLog.AppendText(Environment.NewLine + "-------------------" + Environment.NewLine);
		}

		private void viewerX_OnChatMessageSend(object sender, AxViewerX._ISmartCodeVNCViewerEvents_OnChatMessageSendEvent e)
		{
			txtChatLog.AppendText("Send: "  + e.messageText);
		}

		private void viewerX_OnChatMessageReceived(object sender, AxViewerX._ISmartCodeVNCViewerEvents_OnChatMessageReceivedEvent e)
		{
			txtChatLog.AppendText("Received: " + e.messageText);
		}

		#endregion

		private void OnViewerX_Connected(object sender, EventArgs e)
		{
			UpdateButtons();

			lbRfbProtocol.Text = String.Format("RFB protocol in use: {0}.{1}",
				viewerX.Capabilities.RfbProtocolVersionMajor,
				viewerX.Capabilities.RfbProtocolVersionMinor);
		}

		private void OnViewerX_Disconnected(object sender, EventArgs e)
		{
			UpdateButtons();

			lbRfbProtocol.Text = "";
		}

		private void PopulateEncodings()
		{
			cbEncodings.Items.Add(VNCEncoding.RFB_RAW);	
			cbEncodings.Items.Add(VNCEncoding.RFB_RRE);	
			cbEncodings.Items.Add(VNCEncoding.RFB_CORRE);	
			cbEncodings.Items.Add(VNCEncoding.RFB_HEXTILE);	
			cbEncodings.Items.Add(VNCEncoding.RFB_ZLIB);	
			cbEncodings.Items.Add(VNCEncoding.RFB_TIGHT);	
			cbEncodings.Items.Add(VNCEncoding.RFB_ZLIBHEX);	
			cbEncodings.Items.Add(VNCEncoding.RFB_ULTRA);
			cbEncodings.SelectedIndex = cbEncodings.Items.Add(VNCEncoding.RFB_ZRLE);	
			cbEncodings.Items.Add(VNCEncoding.RFB_ZYWRLE);
		}

		private void PopulateColors()
		{
			cbColorDepth.Items.Add(ColorDepth.COLOR_FULL);
			cbColorDepth.Items.Add(ColorDepth.COLOR_256);
			cbColorDepth.Items.Add(ColorDepth.COLOR_64);
			cbColorDepth.Items.Add(ColorDepth.COLOR_8);

			cbColorDepth.SelectedIndex = 0;
		}

		private void cbStretch_SelectedIndexChanged(object sender, EventArgs e)
		{
			viewerX.StretchMode = StretchMode;

			lbScaleEngine.Enabled = cbScaleEngine.Enabled = StretchMode != ScreenStretchMode.SSM_NONE;
		}

		private void cbEncodings_SelectedIndexChanged(object sender, EventArgs e)
		{
			viewerX.Encoding = (VNCEncoding)cbEncodings.SelectedItem;
		}

		void cbScaleEngine_SelectedIndexChanged(object sender, EventArgs e)
		{
			viewerX.ScreenStretchRenderingQuality = (ScreenStretchRenderingQuality)cbScaleEngine.SelectedIndex;
		}

		private void cbColorDepth_SelectedIndexChanged(object sender, EventArgs e)
		{
			viewerX.ColorDepth = (ViewerX.ColorDepth)cbColorDepth.SelectedItem;
		}

		private void cbEncCopyRect_CheckedChanged(object sender, EventArgs e)
		{
			viewerX.CopyRect = cbEncCopyRect.Checked;
		}

		private void cbEncCache_CheckedChanged(object sender, EventArgs e)
		{
			viewerX.CacheEncoding = cbEncCache.Checked;
		}

		private void btnConnect_Click(object sender, EventArgs e)
		{
			try
			{
				if (viewerX.Connected)
				{
					viewerX.Disconnect();
				}
				else
				{
					if (txtIP.Text.Trim().Length == 0)
					{
						MessageBox.Show(this, "You must enter VNC server address first.", 
							Text, MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
						return;
					}

					viewerX.StretchMode = StretchMode;
					viewerX.ScreenStretchRenderingQuality = (ScreenStretchRenderingQuality)cbScaleEngine.SelectedIndex;
					viewerX.ColorDepth = (ColorDepth)cbColorDepth.SelectedItem;
					viewerX.Encoding = (VNCEncoding)cbEncodings.SelectedItem;
					viewerX.CopyRect = cbEncCopyRect.Checked;
					viewerX.CacheEncoding = cbEncCache.Checked;


					viewerX.EncryptionPlugin = (EncryptionPluginType)cbDsm.SelectedIndex;
					viewerX.UltraVNCSecurity_MSRC4.KeyStorage =
						viewerX.UltraVNCSecurity_SecureVNC.KeyStorage = DsmKeyStorage.DKS_FILE;
					viewerX.UltraVNCSecurity_MSRC4.KeyFilePath = txtMsrc4KeyPath.Text.Trim();
					viewerX.UltraVNCSecurity_SecureVNC.PrivateKeyFilePath = txtSecureVncKeyPath.Text.Trim();

					viewerX.ConnectAsyncEx(txtIP.Text, Convert.ToInt32(udPort.Value),
						txtPassword.Text);
				}
			}
			catch (COMException ex)
			{
				//this exception can be ignored for demonstration purposes
				//in a real application some error handling code will be required
				Trace.WriteLine(ex.Message);
			}

			UpdateButtons();
		}

		private void btnSendCAD_Click(object sender, EventArgs e)
		{
			try
			{
				viewerX.SendCAD();
			}
			catch (COMException ex)
			{
				// If ViewerX is disconnected, it will return E_FAIL, which will 
				// result in COMException being raised.
				Trace.WriteLine(ex.Message);
			}
		}

		private void buttonFileTransfer_Click(object sender, EventArgs e)
		{
			if (viewerX.Capabilities.FileTransfer)
			{
				try
				{
					viewerX.OpenFileTransfer();
				}
				catch (COMException ex)
				{
					// If ViewerX is disconnected, it will return E_FAIL, which will 
					// result in COMException being raised.
					Trace.WriteLine(ex.Message);
				}
			}
			else
				MessageBox.Show("File Transfer feature is not supported by the VNC server at " + viewerX.HostIP);
		}

		private void btnChat_Click(object sender, EventArgs e)
		{
			if (viewerX.Capabilities.Chat)
				try
				{
					viewerX.OpenChat();
				}
				catch (COMException ex)
				{
					// If ViewerX is disconnected, it will return E_FAIL, which will 
					// result in COMException being raised.
					Trace.WriteLine(ex.Message);
				}
			else
				MessageBox.Show("Chat feature is not supported by the VNC server at " + viewerX.HostIP);
		}

		private void btnClose_Click(object sender, EventArgs e)
		{
			Close();
		}

		private void linkLabel1_LinkClicked(object sender, LinkLabelLinkClickedEventArgs e)
		{
			Process.Start("http://www.s-code.com/products/viewerx/");
		}

		[System.Runtime.InteropServices.DllImport("gdi32.dll")]
		static public extern bool DeleteObject(IntPtr hObject);

		private void btnGetScaled_Click(object sender, EventArgs e)
		{
			IntPtr hBitmapHandle;

			try
			{
				hBitmapHandle = new IntPtr(
					viewerX.GetScreenBitmapScaled(Convert.ToInt32(udScaledW.Value), Convert.ToInt32(udScaledH.Value)));
			}
			catch (COMException ex)
			{
				// If ViewerX is disconnected, it will return E_FAIL, which will 
				// result in COMException being raised.
				Trace.WriteLine(ex.Message);

				return;
			}

			new ScaledBitmapForm(Image.FromHbitmap(hBitmapHandle)).ShowDialog(this);

			//you must delete the handle returned by GetScreenBitmapScaled
			DeleteObject(hBitmapHandle);
		}

		private void btnSendMouseEvent_Click(object sender, EventArgs e)
		{
			int buttonsFlags = 0;
			if (cbMouseLeft.Checked)
				buttonsFlags |= 1;
			if (cbMouseRight.Checked)
				buttonsFlags |= 4;

			try
			{
				viewerX.SendMousePointerEvent(Convert.ToInt32(udMouseX.Value),
					Convert.ToInt32(udMouseY.Value), (int)buttonsFlags);
			}
			catch (COMException ex)
			{
				// If ViewerX is disconnected, it will return E_FAIL, which will 
				// result in COMException being raised.
				Trace.WriteLine(ex.Message);
			}
		}

		private void btnMsrcBrowse_Click(object sender, EventArgs e)
		{
			openFileDialog.Filter = "MSRC4 Key Files|*.key|All Files (*.*)|*.*";

			if (openFileDialog.ShowDialog(this) == DialogResult.OK)
				txtMsrc4KeyPath.Text = openFileDialog.FileName;
		}

		private void btnSecureVncBrowse_Click(object sender, EventArgs e)
		{
			openFileDialog.Filter = "SecureVNC Client Key Files|*.pkey|All Files (*.*)|*.*";

			if (openFileDialog.ShowDialog(this) == DialogResult.OK)
				txtSecureVncKeyPath.Text = openFileDialog.FileName;
		}

		private void txtMsrc4KeyPath_TextChanged(object sender, EventArgs e)
		{
			btnMsrcShowHex.Enabled = txtMsrc4KeyPath.Text.Trim().Length > 0;
		}

		private void txtSecureVncKeyPath_TextChanged(object sender, EventArgs e)
		{
			btnSecureShowHex.Enabled = txtSecureVncKeyPath.Text.Trim().Length > 0;
		}

		private void btnMsrcShowHex_Click(object sender, EventArgs e)
		{
			ShowDsmKeyHex(txtMsrc4KeyPath.Text.Trim());
		}

		private void btnSecureShowHex_Click(object sender, EventArgs e)
		{
			ShowDsmKeyHex(txtSecureVncKeyPath.Text.Trim());
		}

		private string ConvertBinaryKeyToHexString(string dsmKeyFilePath)
		{
			try
			{
				using (var reader = new FileStream(dsmKeyFilePath, FileMode.Open, FileAccess.Read))
				{
					var arr = new byte[reader.Length];

					reader.Read(arr, 0, arr.Length);

					var sb = new System.Text.StringBuilder();

					foreach (var byteChar in arr)
						sb.Append(byteChar.ToString("x2"));

					return sb.ToString();
				}
			}
			catch (Exception ex)
			{
				MessageBox.Show(ex.Message);
				return null;
			}
		}

		private void ShowDsmKeyHex(string dsmKeyFilePath)
		{
			string hexString = ConvertBinaryKeyToHexString(dsmKeyFilePath);
			if (hexString != null)
				new ShowDsmKeyHex(hexString).ShowDialog(this);
		}
	}
}