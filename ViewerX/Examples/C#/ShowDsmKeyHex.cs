using System;
using System.Windows.Forms;

namespace ViewerX
{
	public partial class ShowDsmKeyHex : Form
	{
		public ShowDsmKeyHex(string hexString)
		{
			InitializeComponent();

			txtHex.Text = hexString;
		}

		private void btnCopy_Click(object sender, EventArgs e)
		{
			Clipboard.SetText(txtHex.Text);
		}
	}
}
