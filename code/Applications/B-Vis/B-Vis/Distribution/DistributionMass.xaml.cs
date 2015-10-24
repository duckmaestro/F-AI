
using Bevisuali.Util;
using System.Windows.Controls;
using System.Windows.Media;

namespace Bevisuali.UX.Distribution
{
    public partial class DistributionMass : UserControl
    {
        public string ValueLabel
        {
            get
            {
                return xValue.Text;
            }
            set
            {
                xValue.Text = value.Truncate(10);
            }
        }

        public Color ValueColor
        {
            get
            {
                SolidColorBrush solid = xColor.Fill as SolidColorBrush;
                return solid.Color;
            }
            set
            {
                xColor.Fill = new SolidColorBrush(value);
            }
        }

        public double ValueMass
        {
            get
            {
                return _mass;
            }
            set
            {
                _mass = value;
                xMass.Text = value.ToString("F4");
            }
        }

        public DistributionMass()
        {
            InitializeComponent();
        }

        private double _mass;
    }
}
