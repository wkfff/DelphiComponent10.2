using System.Windows.Forms;

namespace ViewerX
{
	/// <summary>
	/// Summary description for Form1.
	/// </summary>
	public partial class Form1
	{
		/// <summary>
		/// Required designer variable.
		/// </summary>
		private System.ComponentModel.Container components = null;

		/// <summary>
		/// Clean up any resources being used.
		/// </summary>
		protected override void Dispose(bool disposing)
		{
			if (disposing)
			{
				if (components != null)
				{
					components.Dispose();
				}
			}
			base.Dispose(disposing);
		}

		#region Windows Form Designer generated code
		/// <summary>
		/// Required method for Designer support - do not modify
		/// the contents of this method with the code editor.
		/// </summary>
		private void InitializeComponent()
		{
			System.Windows.Forms.GroupBox groupBox1;
			System.Windows.Forms.Label label3;
			System.Windows.Forms.Label label2;
			System.Windows.Forms.Label label1;
			System.Windows.Forms.Label label4;
			System.Windows.Forms.LinkLabel linkLabel1;
			System.Windows.Forms.Label label5;
			System.Windows.Forms.Label label6;
			System.Windows.Forms.Label label7;
			System.Windows.Forms.Label label10;
			System.Windows.Forms.Label label11;
			System.Windows.Forms.Label label12;
			System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(Form1));
			this.txtPassword = new System.Windows.Forms.TextBox();
			this.udPort = new System.Windows.Forms.NumericUpDown();
			this.txtIP = new System.Windows.Forms.TextBox();
			this.btnConnect = new System.Windows.Forms.Button();
			this.cbStretch = new System.Windows.Forms.ComboBox();
			this.btnSendCad = new System.Windows.Forms.Button();
			this.cbEncodings = new System.Windows.Forms.ComboBox();
			this.cbEncCopyRect = new System.Windows.Forms.CheckBox();
			this.cbEncCache = new System.Windows.Forms.CheckBox();
			this.cbColorDepth = new System.Windows.Forms.ComboBox();
			this.btnFileTransfer = new System.Windows.Forms.Button();
			this.btnChat = new System.Windows.Forms.Button();
			this.txtChatLog = new System.Windows.Forms.TextBox();
			this.udScaledW = new System.Windows.Forms.NumericUpDown();
			this.udScaledH = new System.Windows.Forms.NumericUpDown();
			this.btnGetScaled = new System.Windows.Forms.Button();
			this.label8 = new System.Windows.Forms.Label();
			this.udMouseY = new System.Windows.Forms.NumericUpDown();
			this.udMouseX = new System.Windows.Forms.NumericUpDown();
			this.label9 = new System.Windows.Forms.Label();
			this.cbMouseRight = new System.Windows.Forms.CheckBox();
			this.cbMouseLeft = new System.Windows.Forms.CheckBox();
			this.btnSendMouseEvent = new System.Windows.Forms.Button();
			this.cbDsm = new System.Windows.Forms.ComboBox();
			this.txtMsrc4KeyPath = new System.Windows.Forms.TextBox();
			this.txtSecureVncKeyPath = new System.Windows.Forms.TextBox();
			this.btnMsrcBrowse = new System.Windows.Forms.Button();
			this.btnSecureVncBrowse = new System.Windows.Forms.Button();
			this.openFileDialog = new System.Windows.Forms.OpenFileDialog();
			this.tableLayoutPanelDsm = new System.Windows.Forms.TableLayoutPanel();
			this.btnSecureShowHex = new System.Windows.Forms.Button();
			this.btnMsrcShowHex = new System.Windows.Forms.Button();
			this.lbRfbProtocol = new System.Windows.Forms.Label();
			this.lbScaleEngine = new System.Windows.Forms.Label();
			this.cbScaleEngine = new System.Windows.Forms.ComboBox();
			this.viewerX = new AxViewerX.AxCSC_ViewerXControl();
			groupBox1 = new System.Windows.Forms.GroupBox();
			label3 = new System.Windows.Forms.Label();
			label2 = new System.Windows.Forms.Label();
			label1 = new System.Windows.Forms.Label();
			label4 = new System.Windows.Forms.Label();
			linkLabel1 = new System.Windows.Forms.LinkLabel();
			label5 = new System.Windows.Forms.Label();
			label6 = new System.Windows.Forms.Label();
			label7 = new System.Windows.Forms.Label();
			label10 = new System.Windows.Forms.Label();
			label11 = new System.Windows.Forms.Label();
			label12 = new System.Windows.Forms.Label();
			groupBox1.SuspendLayout();
			((System.ComponentModel.ISupportInitialize)(this.udPort)).BeginInit();
			((System.ComponentModel.ISupportInitialize)(this.udScaledW)).BeginInit();
			((System.ComponentModel.ISupportInitialize)(this.udScaledH)).BeginInit();
			((System.ComponentModel.ISupportInitialize)(this.udMouseY)).BeginInit();
			((System.ComponentModel.ISupportInitialize)(this.udMouseX)).BeginInit();
			this.tableLayoutPanelDsm.SuspendLayout();
			((System.ComponentModel.ISupportInitialize)(this.viewerX)).BeginInit();
			this.SuspendLayout();
			// 
			// groupBox1
			// 
			groupBox1.Controls.Add(this.txtPassword);
			groupBox1.Controls.Add(label3);
			groupBox1.Controls.Add(this.udPort);
			groupBox1.Controls.Add(label2);
			groupBox1.Controls.Add(this.txtIP);
			groupBox1.Controls.Add(label1);
			groupBox1.Controls.Add(this.btnConnect);
			groupBox1.FlatStyle = System.Windows.Forms.FlatStyle.System;
			groupBox1.Location = new System.Drawing.Point(12, 12);
			groupBox1.Name = "groupBox1";
			groupBox1.Size = new System.Drawing.Size(245, 120);
			groupBox1.TabIndex = 0;
			groupBox1.TabStop = false;
			groupBox1.Text = "Connect to";
			// 
			// txtPassword
			// 
			this.txtPassword.Location = new System.Drawing.Point(96, 64);
			this.txtPassword.Name = "txtPassword";
			this.txtPassword.PasswordChar = '*';
			this.txtPassword.Size = new System.Drawing.Size(136, 20);
			this.txtPassword.TabIndex = 5;
			this.txtPassword.UseSystemPasswordChar = true;
			// 
			// label3
			// 
			label3.AutoSize = true;
			label3.FlatStyle = System.Windows.Forms.FlatStyle.System;
			label3.Location = new System.Drawing.Point(12, 67);
			label3.Name = "label3";
			label3.Size = new System.Drawing.Size(56, 13);
			label3.TabIndex = 4;
			label3.Text = "Password:";
			// 
			// udPort
			// 
			this.udPort.Location = new System.Drawing.Point(96, 40);
			this.udPort.Maximum = new decimal(new int[] {
            65535,
            0,
            0,
            0});
			this.udPort.Minimum = new decimal(new int[] {
            1,
            0,
            0,
            0});
			this.udPort.Name = "udPort";
			this.udPort.Size = new System.Drawing.Size(80, 20);
			this.udPort.TabIndex = 3;
			this.udPort.Value = new decimal(new int[] {
            5900,
            0,
            0,
            0});
			// 
			// label2
			// 
			label2.AutoSize = true;
			label2.FlatStyle = System.Windows.Forms.FlatStyle.System;
			label2.Location = new System.Drawing.Point(12, 42);
			label2.Name = "label2";
			label2.Size = new System.Drawing.Size(62, 13);
			label2.TabIndex = 2;
			label2.Text = "Server port:";
			// 
			// txtIP
			// 
			this.txtIP.Location = new System.Drawing.Point(96, 16);
			this.txtIP.Name = "txtIP";
			this.txtIP.Size = new System.Drawing.Size(136, 20);
			this.txtIP.TabIndex = 1;
			// 
			// label1
			// 
			label1.AutoSize = true;
			label1.FlatStyle = System.Windows.Forms.FlatStyle.System;
			label1.Location = new System.Drawing.Point(10, 19);
			label1.Name = "label1";
			label1.Size = new System.Drawing.Size(81, 13);
			label1.TabIndex = 0;
			label1.Text = "Server address:";
			// 
			// btnConnect
			// 
			this.btnConnect.DialogResult = System.Windows.Forms.DialogResult.OK;
			this.btnConnect.FlatStyle = System.Windows.Forms.FlatStyle.System;
			this.btnConnect.Location = new System.Drawing.Point(123, 90);
			this.btnConnect.Name = "btnConnect";
			this.btnConnect.Size = new System.Drawing.Size(109, 23);
			this.btnConnect.TabIndex = 6;
			this.btnConnect.Text = "Connect";
			this.btnConnect.Click += new System.EventHandler(this.btnConnect_Click);
			// 
			// label4
			// 
			label4.AutoSize = true;
			label4.Location = new System.Drawing.Point(9, 237);
			label4.Name = "label4";
			label4.Size = new System.Drawing.Size(93, 13);
			label4.TabIndex = 7;
			label4.Text = "Screen stretching:";
			// 
			// linkLabel1
			// 
			linkLabel1.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
			linkLabel1.AutoSize = true;
			linkLabel1.Location = new System.Drawing.Point(571, 4);
			linkLabel1.Name = "linkLabel1";
			linkLabel1.Size = new System.Drawing.Size(169, 13);
			linkLabel1.TabIndex = 19;
			linkLabel1.TabStop = true;
			linkLabel1.Text = "Visit SmartCode Solutions web site";
			linkLabel1.LinkClicked += new System.Windows.Forms.LinkLabelLinkClickedEventHandler(this.linkLabel1_LinkClicked);
			// 
			// label5
			// 
			label5.AutoSize = true;
			label5.Location = new System.Drawing.Point(9, 148);
			label5.Name = "label5";
			label5.Size = new System.Drawing.Size(55, 13);
			label5.TabIndex = 1;
			label5.Text = "Encoding:";
			// 
			// label6
			// 
			label6.AutoSize = true;
			label6.Location = new System.Drawing.Point(9, 210);
			label6.Name = "label6";
			label6.Size = new System.Drawing.Size(64, 13);
			label6.TabIndex = 5;
			label6.Text = "Color depth:";
			// 
			// label7
			// 
			label7.AutoSize = true;
			label7.Location = new System.Drawing.Point(9, 334);
			label7.Name = "label7";
			label7.Size = new System.Drawing.Size(67, 13);
			label7.TabIndex = 11;
			label7.Text = "Chat events:";
			// 
			// label10
			// 
			label10.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Left | System.Windows.Forms.AnchorStyles.Right)));
			label10.AutoSize = true;
			label10.Location = new System.Drawing.Point(3, 7);
			label10.Name = "label10";
			label10.Size = new System.Drawing.Size(135, 13);
			label10.TabIndex = 26;
			label10.Text = "DSM plug-in:";
			// 
			// label11
			// 
			label11.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Left | System.Windows.Forms.AnchorStyles.Right)));
			label11.AutoSize = true;
			label11.Location = new System.Drawing.Point(3, 35);
			label11.Name = "label11";
			label11.Size = new System.Drawing.Size(135, 13);
			label11.TabIndex = 28;
			label11.Text = "MSRC4 key path:";
			// 
			// label12
			// 
			label12.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Left | System.Windows.Forms.AnchorStyles.Right)));
			label12.AutoSize = true;
			label12.Location = new System.Drawing.Point(3, 64);
			label12.Margin = new System.Windows.Forms.Padding(3, 0, 0, 0);
			label12.Name = "label12";
			label12.Size = new System.Drawing.Size(138, 13);
			label12.TabIndex = 29;
			label12.Text = "SecureVNC client key path:";
			// 
			// cbStretch
			// 
			this.cbStretch.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
			this.cbStretch.FormattingEnabled = true;
			this.cbStretch.Items.AddRange(new object[] {
            "None",
            "Free",
            "Aspect"});
			this.cbStretch.Location = new System.Drawing.Point(105, 234);
			this.cbStretch.Name = "cbStretch";
			this.cbStretch.Size = new System.Drawing.Size(123, 21);
			this.cbStretch.TabIndex = 8;
			this.cbStretch.SelectedIndexChanged += new System.EventHandler(this.cbStretch_SelectedIndexChanged);
			// 
			// btnSendCad
			// 
			this.btnSendCad.Enabled = false;
			this.btnSendCad.Location = new System.Drawing.Point(12, 292);
			this.btnSendCad.Name = "btnSendCad";
			this.btnSendCad.Size = new System.Drawing.Size(109, 23);
			this.btnSendCad.TabIndex = 9;
			this.btnSendCad.Text = "Send Ctrl+Alt+Del";
			this.btnSendCad.UseVisualStyleBackColor = true;
			this.btnSendCad.Click += new System.EventHandler(this.btnSendCAD_Click);
			// 
			// cbEncodings
			// 
			this.cbEncodings.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
			this.cbEncodings.FormattingEnabled = true;
			this.cbEncodings.Location = new System.Drawing.Point(105, 145);
			this.cbEncodings.Name = "cbEncodings";
			this.cbEncodings.Size = new System.Drawing.Size(123, 21);
			this.cbEncodings.TabIndex = 2;
			// 
			// cbEncCopyRect
			// 
			this.cbEncCopyRect.AutoSize = true;
			this.cbEncCopyRect.Checked = true;
			this.cbEncCopyRect.CheckState = System.Windows.Forms.CheckState.Checked;
			this.cbEncCopyRect.Location = new System.Drawing.Point(12, 170);
			this.cbEncCopyRect.Name = "cbEncCopyRect";
			this.cbEncCopyRect.Size = new System.Drawing.Size(142, 17);
			this.cbEncCopyRect.TabIndex = 3;
			this.cbEncCopyRect.Text = "Use CopyRect encoding";
			this.cbEncCopyRect.UseVisualStyleBackColor = true;
			this.cbEncCopyRect.CheckedChanged += new System.EventHandler(this.cbEncCopyRect_CheckedChanged);
			// 
			// cbEncCache
			// 
			this.cbEncCache.AutoSize = true;
			this.cbEncCache.Location = new System.Drawing.Point(12, 188);
			this.cbEncCache.Name = "cbEncCache";
			this.cbEncCache.Size = new System.Drawing.Size(126, 17);
			this.cbEncCache.TabIndex = 4;
			this.cbEncCache.Text = "Use Cache encoding";
			this.cbEncCache.UseVisualStyleBackColor = true;
			this.cbEncCache.CheckedChanged += new System.EventHandler(this.cbEncCache_CheckedChanged);
			// 
			// cbColorDepth
			// 
			this.cbColorDepth.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
			this.cbColorDepth.FormattingEnabled = true;
			this.cbColorDepth.Location = new System.Drawing.Point(105, 207);
			this.cbColorDepth.Name = "cbColorDepth";
			this.cbColorDepth.Size = new System.Drawing.Size(123, 21);
			this.cbColorDepth.TabIndex = 6;
			// 
			// btnFileTransfer
			// 
			this.btnFileTransfer.Enabled = false;
			this.btnFileTransfer.Location = new System.Drawing.Point(127, 292);
			this.btnFileTransfer.Name = "btnFileTransfer";
			this.btnFileTransfer.Size = new System.Drawing.Size(109, 23);
			this.btnFileTransfer.TabIndex = 10;
			this.btnFileTransfer.Text = "Open File Transfer";
			this.btnFileTransfer.UseVisualStyleBackColor = true;
			this.btnFileTransfer.Click += new System.EventHandler(this.buttonFileTransfer_Click);
			// 
			// btnChat
			// 
			this.btnChat.Enabled = false;
			this.btnChat.Location = new System.Drawing.Point(127, 324);
			this.btnChat.Name = "btnChat";
			this.btnChat.Size = new System.Drawing.Size(109, 23);
			this.btnChat.TabIndex = 12;
			this.btnChat.Text = "Open Chat";
			this.btnChat.UseVisualStyleBackColor = true;
			this.btnChat.Click += new System.EventHandler(this.btnChat_Click);
			// 
			// txtChatLog
			// 
			this.txtChatLog.BackColor = System.Drawing.SystemColors.Window;
			this.txtChatLog.Location = new System.Drawing.Point(12, 353);
			this.txtChatLog.Multiline = true;
			this.txtChatLog.Name = "txtChatLog";
			this.txtChatLog.ReadOnly = true;
			this.txtChatLog.Size = new System.Drawing.Size(245, 79);
			this.txtChatLog.TabIndex = 13;
			// 
			// udScaledW
			// 
			this.udScaledW.Location = new System.Drawing.Point(12, 449);
			this.udScaledW.Maximum = new decimal(new int[] {
            2000,
            0,
            0,
            0});
			this.udScaledW.Minimum = new decimal(new int[] {
            50,
            0,
            0,
            0});
			this.udScaledW.Name = "udScaledW";
			this.udScaledW.Size = new System.Drawing.Size(51, 20);
			this.udScaledW.TabIndex = 14;
			this.udScaledW.Value = new decimal(new int[] {
            640,
            0,
            0,
            0});
			// 
			// udScaledH
			// 
			this.udScaledH.Location = new System.Drawing.Point(74, 449);
			this.udScaledH.Maximum = new decimal(new int[] {
            2000,
            0,
            0,
            0});
			this.udScaledH.Minimum = new decimal(new int[] {
            50,
            0,
            0,
            0});
			this.udScaledH.Name = "udScaledH";
			this.udScaledH.Size = new System.Drawing.Size(51, 20);
			this.udScaledH.TabIndex = 16;
			this.udScaledH.Value = new decimal(new int[] {
            480,
            0,
            0,
            0});
			// 
			// btnGetScaled
			// 
			this.btnGetScaled.Enabled = false;
			this.btnGetScaled.Location = new System.Drawing.Point(127, 448);
			this.btnGetScaled.Name = "btnGetScaled";
			this.btnGetScaled.Size = new System.Drawing.Size(138, 23);
			this.btnGetScaled.TabIndex = 17;
			this.btnGetScaled.Text = "GetScreenBitmapScaled ";
			this.btnGetScaled.UseVisualStyleBackColor = true;
			this.btnGetScaled.Click += new System.EventHandler(this.btnGetScaled_Click);
			// 
			// label8
			// 
			this.label8.AutoSize = true;
			this.label8.Location = new System.Drawing.Point(63, 453);
			this.label8.Name = "label8";
			this.label8.Size = new System.Drawing.Size(12, 13);
			this.label8.TabIndex = 15;
			this.label8.Text = "x";
			// 
			// udMouseY
			// 
			this.udMouseY.Location = new System.Drawing.Point(74, 491);
			this.udMouseY.Maximum = new decimal(new int[] {
            3000,
            0,
            0,
            0});
			this.udMouseY.Name = "udMouseY";
			this.udMouseY.Size = new System.Drawing.Size(51, 20);
			this.udMouseY.TabIndex = 21;
			this.udMouseY.Value = new decimal(new int[] {
            300,
            0,
            0,
            0});
			// 
			// udMouseX
			// 
			this.udMouseX.Location = new System.Drawing.Point(12, 491);
			this.udMouseX.Maximum = new decimal(new int[] {
            3000,
            0,
            0,
            0});
			this.udMouseX.Name = "udMouseX";
			this.udMouseX.Size = new System.Drawing.Size(51, 20);
			this.udMouseX.TabIndex = 20;
			this.udMouseX.Value = new decimal(new int[] {
            300,
            0,
            0,
            0});
			// 
			// label9
			// 
			this.label9.AutoSize = true;
			this.label9.Location = new System.Drawing.Point(63, 494);
			this.label9.Name = "label9";
			this.label9.Size = new System.Drawing.Size(12, 13);
			this.label9.TabIndex = 22;
			this.label9.Text = "x";
			// 
			// cbMouseRight
			// 
			this.cbMouseRight.AutoSize = true;
			this.cbMouseRight.Location = new System.Drawing.Point(12, 535);
			this.cbMouseRight.Name = "cbMouseRight";
			this.cbMouseRight.Size = new System.Drawing.Size(109, 17);
			this.cbMouseRight.TabIndex = 24;
			this.cbMouseRight.Text = "Right button click";
			this.cbMouseRight.UseVisualStyleBackColor = true;
			// 
			// cbMouseLeft
			// 
			this.cbMouseLeft.AutoSize = true;
			this.cbMouseLeft.Location = new System.Drawing.Point(12, 517);
			this.cbMouseLeft.Name = "cbMouseLeft";
			this.cbMouseLeft.Size = new System.Drawing.Size(102, 17);
			this.cbMouseLeft.TabIndex = 23;
			this.cbMouseLeft.Text = "Left button click";
			this.cbMouseLeft.UseVisualStyleBackColor = true;
			// 
			// btnSendMouseEvent
			// 
			this.btnSendMouseEvent.Location = new System.Drawing.Point(127, 489);
			this.btnSendMouseEvent.Name = "btnSendMouseEvent";
			this.btnSendMouseEvent.Size = new System.Drawing.Size(138, 23);
			this.btnSendMouseEvent.TabIndex = 25;
			this.btnSendMouseEvent.Text = "SendMousePointerEvent";
			this.btnSendMouseEvent.UseVisualStyleBackColor = true;
			this.btnSendMouseEvent.Click += new System.EventHandler(this.btnSendMouseEvent_Click);
			// 
			// cbDsm
			// 
			this.cbDsm.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
			this.cbDsm.FormattingEnabled = true;
			this.cbDsm.Items.AddRange(new object[] {
            "None",
            "MSRC4 v1.2",
            "SecureVNC v2.3"});
			this.cbDsm.Location = new System.Drawing.Point(144, 3);
			this.cbDsm.Name = "cbDsm";
			this.cbDsm.Size = new System.Drawing.Size(175, 21);
			this.cbDsm.TabIndex = 27;
			// 
			// txtMsrc4KeyPath
			// 
			this.txtMsrc4KeyPath.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Left | System.Windows.Forms.AnchorStyles.Right)));
			this.txtMsrc4KeyPath.Location = new System.Drawing.Point(144, 31);
			this.txtMsrc4KeyPath.Name = "txtMsrc4KeyPath";
			this.txtMsrc4KeyPath.Size = new System.Drawing.Size(175, 20);
			this.txtMsrc4KeyPath.TabIndex = 7;
			this.txtMsrc4KeyPath.TextChanged += new System.EventHandler(this.txtMsrc4KeyPath_TextChanged);
			// 
			// txtSecureVncKeyPath
			// 
			this.txtSecureVncKeyPath.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Left | System.Windows.Forms.AnchorStyles.Right)));
			this.txtSecureVncKeyPath.Location = new System.Drawing.Point(144, 60);
			this.txtSecureVncKeyPath.Name = "txtSecureVncKeyPath";
			this.txtSecureVncKeyPath.Size = new System.Drawing.Size(175, 20);
			this.txtSecureVncKeyPath.TabIndex = 30;
			this.txtSecureVncKeyPath.TextChanged += new System.EventHandler(this.txtSecureVncKeyPath_TextChanged);
			// 
			// btnMsrcBrowse
			// 
			this.btnMsrcBrowse.AutoSize = true;
			this.btnMsrcBrowse.DialogResult = System.Windows.Forms.DialogResult.OK;
			this.btnMsrcBrowse.Location = new System.Drawing.Point(322, 30);
			this.btnMsrcBrowse.Margin = new System.Windows.Forms.Padding(0, 3, 3, 3);
			this.btnMsrcBrowse.Name = "btnMsrcBrowse";
			this.btnMsrcBrowse.Size = new System.Drawing.Size(61, 23);
			this.btnMsrcBrowse.TabIndex = 7;
			this.btnMsrcBrowse.Text = "Browse...";
			this.btnMsrcBrowse.Click += new System.EventHandler(this.btnMsrcBrowse_Click);
			// 
			// btnSecureVncBrowse
			// 
			this.btnSecureVncBrowse.AutoSize = true;
			this.btnSecureVncBrowse.DialogResult = System.Windows.Forms.DialogResult.OK;
			this.btnSecureVncBrowse.Location = new System.Drawing.Point(322, 59);
			this.btnSecureVncBrowse.Margin = new System.Windows.Forms.Padding(0, 3, 3, 3);
			this.btnSecureVncBrowse.Name = "btnSecureVncBrowse";
			this.btnSecureVncBrowse.Size = new System.Drawing.Size(61, 23);
			this.btnSecureVncBrowse.TabIndex = 31;
			this.btnSecureVncBrowse.Text = "Browse...";
			this.btnSecureVncBrowse.Click += new System.EventHandler(this.btnSecureVncBrowse_Click);
			// 
			// openFileDialog
			// 
			this.openFileDialog.AddExtension = false;
			// 
			// tableLayoutPanelDsm
			// 
			this.tableLayoutPanelDsm.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)));
			this.tableLayoutPanelDsm.ColumnCount = 4;
			this.tableLayoutPanelDsm.ColumnStyles.Add(new System.Windows.Forms.ColumnStyle());
			this.tableLayoutPanelDsm.ColumnStyles.Add(new System.Windows.Forms.ColumnStyle(System.Windows.Forms.SizeType.Percent, 100F));
			this.tableLayoutPanelDsm.ColumnStyles.Add(new System.Windows.Forms.ColumnStyle());
			this.tableLayoutPanelDsm.ColumnStyles.Add(new System.Windows.Forms.ColumnStyle());
			this.tableLayoutPanelDsm.Controls.Add(this.btnSecureShowHex, 3, 2);
			this.tableLayoutPanelDsm.Controls.Add(this.btnMsrcShowHex, 3, 1);
			this.tableLayoutPanelDsm.Controls.Add(label10, 0, 0);
			this.tableLayoutPanelDsm.Controls.Add(label12, 0, 2);
			this.tableLayoutPanelDsm.Controls.Add(this.txtSecureVncKeyPath, 1, 2);
			this.tableLayoutPanelDsm.Controls.Add(this.btnMsrcBrowse, 2, 1);
			this.tableLayoutPanelDsm.Controls.Add(this.btnSecureVncBrowse, 2, 2);
			this.tableLayoutPanelDsm.Controls.Add(this.cbDsm, 1, 0);
			this.tableLayoutPanelDsm.Controls.Add(label11, 0, 1);
			this.tableLayoutPanelDsm.Controls.Add(this.txtMsrc4KeyPath, 1, 1);
			this.tableLayoutPanelDsm.Location = new System.Drawing.Point(271, 465);
			this.tableLayoutPanelDsm.Name = "tableLayoutPanelDsm";
			this.tableLayoutPanelDsm.RowCount = 3;
			this.tableLayoutPanelDsm.RowStyles.Add(new System.Windows.Forms.RowStyle());
			this.tableLayoutPanelDsm.RowStyles.Add(new System.Windows.Forms.RowStyle());
			this.tableLayoutPanelDsm.RowStyles.Add(new System.Windows.Forms.RowStyle());
			this.tableLayoutPanelDsm.Size = new System.Drawing.Size(469, 85);
			this.tableLayoutPanelDsm.TabIndex = 32;
			// 
			// btnSecureShowHex
			// 
			this.btnSecureShowHex.AutoSize = true;
			this.btnSecureShowHex.DialogResult = System.Windows.Forms.DialogResult.OK;
			this.btnSecureShowHex.Enabled = false;
			this.btnSecureShowHex.Location = new System.Drawing.Point(389, 59);
			this.btnSecureShowHex.Margin = new System.Windows.Forms.Padding(3, 3, 0, 3);
			this.btnSecureShowHex.Name = "btnSecureShowHex";
			this.btnSecureShowHex.Size = new System.Drawing.Size(80, 23);
			this.btnSecureShowHex.TabIndex = 34;
			this.btnSecureShowHex.Text = "Show as Hex";
			this.btnSecureShowHex.Click += new System.EventHandler(this.btnSecureShowHex_Click);
			// 
			// btnMsrcShowHex
			// 
			this.btnMsrcShowHex.AutoSize = true;
			this.btnMsrcShowHex.DialogResult = System.Windows.Forms.DialogResult.OK;
			this.btnMsrcShowHex.Enabled = false;
			this.btnMsrcShowHex.Location = new System.Drawing.Point(389, 30);
			this.btnMsrcShowHex.Margin = new System.Windows.Forms.Padding(3, 3, 0, 3);
			this.btnMsrcShowHex.Name = "btnMsrcShowHex";
			this.btnMsrcShowHex.Size = new System.Drawing.Size(80, 23);
			this.btnMsrcShowHex.TabIndex = 33;
			this.btnMsrcShowHex.Text = "Show as Hex";
			this.btnMsrcShowHex.Click += new System.EventHandler(this.btnMsrcShowHex_Click);
			// 
			// lbRfbProtocol
			// 
			this.lbRfbProtocol.AutoSize = true;
			this.lbRfbProtocol.Location = new System.Drawing.Point(271, 4);
			this.lbRfbProtocol.Name = "lbRfbProtocol";
			this.lbRfbProtocol.Size = new System.Drawing.Size(0, 13);
			this.lbRfbProtocol.TabIndex = 33;
			// 
			// lbScaleEngine
			// 
			this.lbScaleEngine.AutoSize = true;
			this.lbScaleEngine.Location = new System.Drawing.Point(9, 264);
			this.lbScaleEngine.Name = "lbScaleEngine";
			this.lbScaleEngine.Size = new System.Drawing.Size(80, 13);
			this.lbScaleEngine.TabIndex = 34;
			this.lbScaleEngine.Text = "Scaling engine:";
			// 
			// cbScaleEngine
			// 
			this.cbScaleEngine.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
			this.cbScaleEngine.Items.AddRange(new object[] {
            "GDI",
            "GDI+ (High Quality)",
            "GDI+ (Low Quality)"});
			this.cbScaleEngine.Location = new System.Drawing.Point(105, 261);
			this.cbScaleEngine.Name = "cbScaleEngine";
			this.cbScaleEngine.Size = new System.Drawing.Size(123, 21);
			this.cbScaleEngine.TabIndex = 35;
			// 
			// viewerX
			// 
			this.viewerX.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom) 
            | System.Windows.Forms.AnchorStyles.Left) 
            | System.Windows.Forms.AnchorStyles.Right)));
			this.viewerX.Enabled = true;
			this.viewerX.Location = new System.Drawing.Point(269, 21);
			this.viewerX.Name = "viewerX";
			this.viewerX.OcxState = ((System.Windows.Forms.AxHost.State)(resources.GetObject("viewerX.OcxState")));
			this.viewerX.Size = new System.Drawing.Size(471, 438);
			this.viewerX.TabIndex = 18;
			// 
			// Form1
			// 
			this.AcceptButton = this.btnConnect;
			this.AutoScaleBaseSize = new System.Drawing.Size(5, 13);
			this.ClientSize = new System.Drawing.Size(752, 556);
			this.Controls.Add(this.cbScaleEngine);
			this.Controls.Add(this.lbScaleEngine);
			this.Controls.Add(this.lbRfbProtocol);
			this.Controls.Add(this.tableLayoutPanelDsm);
			this.Controls.Add(this.btnSendMouseEvent);
			this.Controls.Add(this.cbMouseRight);
			this.Controls.Add(this.cbMouseLeft);
			this.Controls.Add(this.label9);
			this.Controls.Add(this.udMouseY);
			this.Controls.Add(this.udMouseX);
			this.Controls.Add(this.btnGetScaled);
			this.Controls.Add(this.udScaledH);
			this.Controls.Add(this.udScaledW);
			this.Controls.Add(this.txtChatLog);
			this.Controls.Add(label7);
			this.Controls.Add(this.btnChat);
			this.Controls.Add(this.btnFileTransfer);
			this.Controls.Add(this.cbColorDepth);
			this.Controls.Add(label6);
			this.Controls.Add(this.cbEncCache);
			this.Controls.Add(this.cbEncCopyRect);
			this.Controls.Add(this.cbEncodings);
			this.Controls.Add(label5);
			this.Controls.Add(this.cbStretch);
			this.Controls.Add(this.btnSendCad);
			this.Controls.Add(label4);
			this.Controls.Add(linkLabel1);
			this.Controls.Add(groupBox1);
			this.Controls.Add(this.viewerX);
			this.Controls.Add(this.label8);
			this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
			this.MinimumSize = new System.Drawing.Size(400, 400);
			this.Name = "Form1";
			this.SizeGripStyle = System.Windows.Forms.SizeGripStyle.Show;
			this.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
			this.Text = "SmartCode ViewerX VNC ActiveX Control Demo";
			groupBox1.ResumeLayout(false);
			groupBox1.PerformLayout();
			((System.ComponentModel.ISupportInitialize)(this.udPort)).EndInit();
			((System.ComponentModel.ISupportInitialize)(this.udScaledW)).EndInit();
			((System.ComponentModel.ISupportInitialize)(this.udScaledH)).EndInit();
			((System.ComponentModel.ISupportInitialize)(this.udMouseY)).EndInit();
			((System.ComponentModel.ISupportInitialize)(this.udMouseX)).EndInit();
			this.tableLayoutPanelDsm.ResumeLayout(false);
			this.tableLayoutPanelDsm.PerformLayout();
			((System.ComponentModel.ISupportInitialize)(this.viewerX)).EndInit();
			this.ResumeLayout(false);
			this.PerformLayout();

		}
		#endregion

		private TextBox txtIP;
		private NumericUpDown udPort;
		private TextBox txtPassword;
		private Button btnConnect;
		private Button btnSendCad;
		private ComboBox cbStretch;
		private AxViewerX.AxCSC_ViewerXControl viewerX;
		private ComboBox cbEncodings;
		private CheckBox cbEncCopyRect;
		private CheckBox cbEncCache;
		private ComboBox cbColorDepth;
		private Button btnFileTransfer;
		private Button btnChat;
		private TextBox txtChatLog;
		private NumericUpDown udScaledW;
		private NumericUpDown udScaledH;
		private Button btnGetScaled;
		private Label label8;
		private NumericUpDown udMouseY;
		private NumericUpDown udMouseX;
		private Label label9;
		private CheckBox cbMouseRight;
		private CheckBox cbMouseLeft;
		private Button btnSendMouseEvent;
		private ComboBox cbDsm;
		private TextBox txtMsrc4KeyPath;
		private TextBox txtSecureVncKeyPath;
		private Button btnMsrcBrowse;
		private Button btnSecureVncBrowse;
		private OpenFileDialog openFileDialog;
		private TableLayoutPanel tableLayoutPanelDsm;
		private Button btnSecureShowHex;
		private Button btnMsrcShowHex;
		private Label lbRfbProtocol;
		private Label lbScaleEngine;
		private ComboBox cbScaleEngine;
	}
}