
using Bevisuali.Util;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Media;

namespace Bevisuali.UX.Observation
{
    public partial class ObservationNameValue : UserControl
    {
        public string VariableName
        {
            get
            {
                return _variableName;
            }
            set
            {
                _variableName = value;
                if (string.IsNullOrWhiteSpace(value))
                {
                    xAbbreviationTextBlock.Visibility = Visibility.Collapsed;
                    xRoot.Margin = new Thickness(
                        -xColor.Margin.Left,
                        xRoot.Margin.Top,
                        xRoot.Margin.Right,
                        xRoot.Margin.Bottom);
                }
                else
                {
                    char letter;
                    string subscript;
                    Utils.ParseVariableName(value, out letter, out subscript);

                    xNameLetter.Text = letter.ToString();
                    xNameSubscript.Text = subscript;

                    xAbbreviationTextBlock.Visibility = Visibility.Visible;
                    xRoot.Margin = new Thickness(0);
                }
            }
        }

        public Color VariableNameColor
        {
            get
            {
                var fgBrush = (SolidColorBrush)xNameLetter.Foreground;
                return fgBrush.Color;
            }
            set
            {
                xNameLetter.Foreground = new SolidColorBrush(value);
                xNameSubscript.Foreground = new SolidColorBrush(value);
            }
        }

        public string VariableValueLabel
        {
            get
            {
                return xValue.Text;
            }
            set
            {
                xValue.Text = value;
            }
        }

        public Color VariableValueColor
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

        public ObservationNameValue()
        {
            InitializeComponent();
        }

        private string _variableName = "?";
    }
}
