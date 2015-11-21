
using Bevisuali.Model;
using System;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Input;

namespace Bevisuali.UX
{
    public partial class LayoutInspector : UserControl
    {
        public LayoutInspector()
        {
            InitializeComponent();
        }

        public void SetNodeSeparationTarget(float value)
        {
            this.xNodeSeparationTargetTextBox.Text = value.ToString();
        }

        private void xButtonApply_Click(object sender, RoutedEventArgs e)
        {
            var algorithm = NetworkLayoutOptions.AlgorithmEnum.SugiyamaEfficient;
            float nodeSeparationTarget = float.Parse(this.xNodeSeparationTargetTextBox.Text);
            int epochs = (int)Math.Round(float.Parse(this.xEpochsTextBox.Text));
            float nodeSize = float.Parse(this.xNodeSizeTextBox.Text);
            float edgeThickness = float.Parse(this.xEdgeThickness.Text);

            var layoutOptions
                = new NetworkLayoutOptions(
                    algorithm,
                    nodeSeparationTarget,
                    epochs,
                    nodeSize,
                    edgeThickness);

            App.Current.MainWindow.RequestLayoutOptions(layoutOptions);
        }

        private void OnNumericalOnlyTextBoxPreviewTextInput(
            object sender,
            TextCompositionEventArgs e)
        {
            float v;
            if (!float.TryParse(this.xNodeSeparationTargetTextBox.Text, out v))
            {
                e.Handled = true;
            }
        }
    }
}
