
using Microsoft.Win32;
using System;
using System.ComponentModel;
using System.Linq;
using System.Text;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Media;
using FObservation = FAI.Bayesian.Observation;

namespace Bevisuali.UX.Observation
{
    public partial class ObservationSetInspector : UserControl
    {
        public ObservationSetInspector()
        {
            InitializeComponent();
        }


        // hack?: opening files should be up-leveled to main window?
        public void OpenFileBrowser()
        {
            xButtonOpen_Click(null, null);
        }

        private void UserControl_Initialized(object sender, EventArgs e)
        {
            xSetName.Text = "(empty)";
        }

        private void xButtonOpen_Click(object sender, RoutedEventArgs e)
        {
            OpenFileDialog dialog = new OpenFileDialog();
            dialog.Multiselect = false;
            dialog.FileOk += delegate(object s2, CancelEventArgs e2)
            {
                string fileName = dialog.FileName;
                App.Current.MainWindow.RequestLoadUnknownFile(fileName);
            };
            dialog.ShowDialog();
        }

        public void SetObservationSet(FAI.Bayesian.IObservationSet observations)
        {
            if (observations == null)
            {
                xSetName.Text = "(none)";
            }
            else
            {
                xSetName.Text = observations.Name;
            }

            // Update the list of observations.
            const int maxToShow = 100;
            xStackPanelObservations.Children.Clear();

            if (observations != null)
            {
                foreach (var observation in
                    observations.Take(maxToShow))
                {
                    TextBlock tb = new TextBlock();
                    tb.Text = ObservationToString(observation);
                    xStackPanelObservations.Children.Add(tb);
                    tb.FontFamily = new FontFamily("Consolas");
                    tb.FontSize = 10;
                }

                if (observations.Size.Value > maxToShow)
                {
                    TextBlock tb = new TextBlock();
                    tb.Text = "...";
                    xStackPanelObservations.Children.Add(tb);
                    tb.FontFamily = new FontFamily("Consolas");
                    tb.FontSize = 10;
                }
            }
        }

        private string ObservationToString(FObservation observation)
        {
            StringBuilder sb = new StringBuilder();
            sb.Append("{ ");
            foreach (var kvp in observation)
            {
                sb.AppendFormat("{0}:{1}; ", kvp.Key, kvp.Value);
            }
            sb.Append("}");
            return sb.ToString();
        }
    }
}
