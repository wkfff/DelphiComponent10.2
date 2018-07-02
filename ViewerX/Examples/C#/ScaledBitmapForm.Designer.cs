namespace ViewerX
{
	partial class ScaledBitmapForm
	{
		/// <summary>
		/// Required designer variable.
		/// </summary>
		private System.ComponentModel.IContainer components = null;

		/// <summary>
		/// Clean up any resources being used.
		/// </summary>
		/// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
		protected override void Dispose(bool disposing)
		{
			if (disposing && (components != null))
			{
				components.Dispose();
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
			this.plScaled = new System.Windows.Forms.Panel();
			this.pbScaled = new System.Windows.Forms.PictureBox();
			this.button1 = new System.Windows.Forms.Button();
			this.plScaled.SuspendLayout();
			((System.ComponentModel.ISupportInitialize)(this.pbScaled)).BeginInit();
			this.SuspendLayout();
			// 
			// plScaled
			// 
			this.plScaled.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
						| System.Windows.Forms.AnchorStyles.Left)
						| System.Windows.Forms.AnchorStyles.Right)));
			this.plScaled.AutoScroll = true;
			this.plScaled.Controls.Add(this.pbScaled);
			this.plScaled.Location = new System.Drawing.Point(12, 12);
			this.plScaled.Name = "plScaled";
			this.plScaled.Size = new System.Drawing.Size(454, 304);
			this.plScaled.TabIndex = 22;
			// 
			// pbScaled
			// 
			this.pbScaled.Location = new System.Drawing.Point(0, 0);
			this.pbScaled.Name = "pbScaled";
			this.pbScaled.Size = new System.Drawing.Size(127, 96);
			this.pbScaled.SizeMode = System.Windows.Forms.PictureBoxSizeMode.AutoSize;
			this.pbScaled.TabIndex = 0;
			this.pbScaled.TabStop = false;
			// 
			// button1
			// 
			this.button1.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
			this.button1.DialogResult = System.Windows.Forms.DialogResult.Cancel;
			this.button1.Location = new System.Drawing.Point(391, 331);
			this.button1.Name = "button1";
			this.button1.Size = new System.Drawing.Size(75, 23);
			this.button1.TabIndex = 23;
			this.button1.Text = "Close";
			this.button1.UseVisualStyleBackColor = true;
			// 
			// ScaledBitmapForm
			// 
			this.AcceptButton = this.button1;
			this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
			this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
			this.CancelButton = this.button1;
			this.ClientSize = new System.Drawing.Size(478, 366);
			this.Controls.Add(this.button1);
			this.Controls.Add(this.plScaled);
			this.MaximizeBox = false;
			this.MinimizeBox = false;
			this.Name = "ScaledBitmapForm";
			this.ShowIcon = false;
			this.ShowInTaskbar = false;
			this.SizeGripStyle = System.Windows.Forms.SizeGripStyle.Show;
			this.Text = "GetScreenBitmapScaled";
			this.plScaled.ResumeLayout(false);
			this.plScaled.PerformLayout();
			((System.ComponentModel.ISupportInitialize)(this.pbScaled)).EndInit();
			this.ResumeLayout(false);

		}

		#endregion

		private System.Windows.Forms.Panel plScaled;
		private System.Windows.Forms.PictureBox pbScaled;
		private System.Windows.Forms.Button button1;
	}
}