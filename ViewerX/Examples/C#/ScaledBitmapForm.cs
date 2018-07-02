using System.Drawing;
using System.Windows.Forms;

namespace ViewerX
{
	public partial class ScaledBitmapForm : Form
	{
		public ScaledBitmapForm(Bitmap scaledBitmap)
		{
			InitializeComponent();

			pbScaled.Image = scaledBitmap;
		}
	}
}
