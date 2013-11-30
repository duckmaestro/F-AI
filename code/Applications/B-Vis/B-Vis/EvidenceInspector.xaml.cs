
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

        public void SetEvidence(
            IDictionary<string, Tuple<FObservation,FRandomVariable[]>> evidences,
            IDictionary<string,string> abbreviations)
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

        private void xButtonReset_Click(object sender, RoutedEventArgs e)
        {
            App.Current.MainWindow.RequestResetEvidence(null);
        }
    }
}
