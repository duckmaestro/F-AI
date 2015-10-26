
using Bevisuali.Model;
using Bevisuali.Util;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Windows;
using System.Windows.Controls;
using FObservation = FAI.Bayesian.Observation;
using FRandomVariable = FAI.Bayesian.RandomVariable;

namespace Bevisuali.UX
{

    public partial class EvidenceInspector : UserControl
    {
        public EvidenceInspector()
        {
            InitializeComponent();
        }

        internal void SetEvidence(
            IDictionary<string, Tuple<FObservation, FRandomVariable[]>> evidences,
            IDictionary<string, string> abbreviations)
        {
            var evidencesSorted
                = evidences.OrderBy(kvp => kvp.Key);

            xEvidenceList.Children.Clear();

            foreach (var kvp in evidencesSorted)
            {
                // Scenario label.
                TextBlock scenarioLabel = new TextBlock();
                scenarioLabel.Text = kvp.Key;
                xEvidenceList.Children.Add(scenarioLabel);

                // Observation values.
                Observation.Observation obsControl = new Observation.Observation();
                var observation = kvp.Value.Item1;
                var observationVariables = kvp.Value.Item2;
                obsControl.SetData(observation, observationVariables, abbreviations);
                xEvidenceList.Children.Add(obsControl);
                obsControl.Margin = new Thickness(0, 0, 0, 12);
            }
        }

        internal void SetComparisonRelevantVariables(IScenarioComparison comparison, IDictionary<string, string> abbreviations)
        {
            this.xRelevantVariablesList.Children.Clear();

            if (comparison == null)
            {
                return;
            }

            foreach (var v in comparison.SignificantVariables)
            {
                this.xRelevantVariablesList.Children.Add(
                    new Label()
                    {
                        Content = string.Format("{0} ({1}): {2}", v.Item1.Truncate(20), abbreviations[v.Item1], v.Item2)
                    }
                );
            }
        }

        internal void SetComparisonMetric(ComparisonMetric comparisonMetric)
        {
            if (comparisonMetric == ComparisonMetric.ErrorSum)
            {
                _noReentry = true;
                this.xRadioButtonComparisonMetricES.IsChecked = true;
                _noReentry = false;
            }
            else if (comparisonMetric == ComparisonMetric.SymmetricKLDivergence)
            {
                _noReentry = true;
                this.xRadioButtonComparisonMetricKL.IsChecked = true;
                _noReentry = false;
            }
        }

        private void xButtonReset_Click(object sender, RoutedEventArgs e)
        {
            App.Current.MainWindow.RequestResetEvidence(null);
        }

        private void OnComparisonMetricChecked(object sender, RoutedEventArgs e)
        {
            if (_noReentry)
            {
                return;
            }

            if (e.Source == xRadioButtonComparisonMetricES)
            {
                App.Current.MainWindow.RequestSetComparisonMetric(ComparisonMetric.ErrorSum);
            }
            else if (e.Source == xRadioButtonComparisonMetricKL)
            {
                App.Current.MainWindow.RequestSetComparisonMetric(ComparisonMetric.SymmetricKLDivergence);
            }
        }

        private bool _noReentry;
    }
}
