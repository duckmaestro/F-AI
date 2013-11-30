
using Bevisuali.Model;
using Bevisuali.Util;
using FAI.Bayesian;
using Microsoft.FSharp.Collections;
using Microsoft.FSharp.Core;
using System;
using System.Linq;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Input;
using System.Windows.Media;

namespace Bevisuali.UX
{
    public partial class LearningInspector : UserControl
    {
        private IObservationSet _trainingSet;
        private double _dirichletAlpha;
        private int _parentLimit;

        public LearningInspector()
        {
            InitializeComponent();
        }

        private void UserControl_Initialized(object sender, EventArgs e)
        {
            if (!double.TryParse(xTxtDirichletAlpha.Text, out _dirichletAlpha))
            {
                _dirichletAlpha = 1.0;
            }

            xTxtDirichletAlpha.Text = _dirichletAlpha.ToString();

            if (!int.TryParse(xTxtParentLimit.Text, out _parentLimit))
            {
                _parentLimit = 2;
            }

            xTxtParentLimit.Text = _parentLimit.ToString();
        }

        public void SetIsLearning(bool isLearning)
        {
            var radioButtons = LogicalTreeHelper.GetChildren(xGridOptions).OfType<RadioButton>().ToList();

            if (isLearning)
            {
                xButtonLearn.IsEnabled = false;

                radioButtons
                    .Where(rb => rb.GroupName == "Structure")
                    .ForAll(rb =>
                    {
                        rb.IsEnabled = false;
                    });

                xTxtDirichletAlpha.IsEnabled = false;
                xTxtParentLimit.IsEnabled = false;
            }
            else
            {
                xButtonLearn.IsEnabled = true;

                radioButtons
                    .Where(rb => rb.GroupName == "Structure")
                    .ForAll(rb =>
                    {
                        rb.IsEnabled = true;
                    });

                xTxtDirichletAlpha.IsEnabled = true;
                xTxtParentLimit.IsEnabled = true;
            }
        }

        public void SetTrainingSet(IObservationSet set)
        {
            _trainingSet = set;

            if (_trainingSet == null)
            {
                xTrainingSetSource.Text = "(none)";
                xButtonLearn.IsEnabled = false;
            }
            else
            {
                xTrainingSetSource.Text
                    = string.Format("{0} ({1})", set.Name, set.SourceUri);
                xButtonLearn.IsEnabled = true;
            }
        }

        private void xButtonLearn_Click(object sender, RoutedEventArgs e)
        {
            var trainingSet = _trainingSet;
            if (trainingSet == null)
            {
                return;
            }

            LearningOptions options = new LearningOptions();

            // Prior
            options.DistributionDirichletAlpha = _dirichletAlpha;

            // Structure
            if (xRadStructureDisconnected.IsChecked == true)
            {
                options.Structure = LearningOptions.StructureEnum.DisconnectedStructure;
            }
            else if (xRadStructureRandom.IsChecked == true)
            {
                options.Structure = LearningOptions.StructureEnum.RandomStructure;
                options.StructureSeed =
                    (int)
                    DateTime.UtcNow
                    .Subtract(DateTime.UtcNow.Date)
                    .TotalMilliseconds;
            }
            else if (xRadStructureTree.IsChecked == true)
            {
                options.Structure = LearningOptions.StructureEnum.TreeStructure;
            }
            else if (xRadStructureGeneral.IsChecked == true)
            {
                options.Structure = LearningOptions.StructureEnum.GeneralStructure;
            }
            else
            {
                return;
            }

            // Parent limit
            options.StructureParentLimit = _parentLimit;

            App.Current.MainWindow.RequestTraining(trainingSet, options);
        }

        private void xTxtDirichletAlpha_TextChanged(object sender, TextChangedEventArgs e)
        {
            double newValue;
            if (double.TryParse(xTxtDirichletAlpha.Text, out newValue))
            {
                _dirichletAlpha = newValue;
            }
        }

        private void xTxtParentLimit_TextChanged(object sender, TextChangedEventArgs e)
        {
            int newValue;
            if (int.TryParse(xTxtParentLimit.Text, out newValue))
            {
                _parentLimit = newValue;
            }
        }
    }
}
