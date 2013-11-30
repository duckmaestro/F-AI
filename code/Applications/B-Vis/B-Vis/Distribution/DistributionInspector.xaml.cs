
using Bevisuali.Util;
using FAI.Bayesian;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Documents;
using FObservation = FAI.Bayesian.Observation;

namespace Bevisuali.UX.Distribution
{
    public partial class DistributionInspector : UserControl
    {
        public string InspectorTitle
        {
            get
            {
                return xTitle.Text;
            }
            set
            {
                xTitle.Text = value;
            }
        }

        public DistributionInspector()
        {
            InitializeComponent();
        }

        private void UserControl_Initialized(object sender, EventArgs e)
        {
            xVariableName.Text = "(None)";
        }

        public void SetDistribution(
            RandomVariable variable, 
            IDictionary<string, string> variableAbbreviations,
            IEnumerable<RandomVariable> variableParents,
            DiscreteDistribution distribution)
        {
            _variable = variable;
            _variableParents = variableParents;
            _variableAbbreviations = variableAbbreviations;

            if (variable != null)
            {
                if (distribution != null)
                {
                    _distributions = new DistributionSet(distribution);
                }
                else
                {
                    _distributions = variable.Distributions;
                }
            }
            else
            {
                _distributions = new DistributionSet();
            }

            RefreshUI();
        }

        private void RefreshUI()
        {
            // Update variable name label.
            if (_variable == null)
            {
                xVariableName.Text = "";
            }
            else
            {
                xVariableName.Text = _variable.Name;
            }

            // Gather distributions.
            List<FObservation> parentConfigurations = new List<FObservation>();
            List<DiscreteDistribution> distributions = new List<DiscreteDistribution>();

            // TODO: Better ordering so that parent values change most on right, least on left.
            // TODO: Deal with limits better.
            const int maxTables = 100;
            foreach (var d in _distributions.EnumerateDistributions().Take(maxTables))
            {
                var parentConfig = d.Item1;
                var distribution = d.Item2;

                parentConfigurations.Add(parentConfig);
                distributions.Add(distribution);
            }

            // Create distribution displays.
            // TODO: Reuse existing if possible.
            xStack.Children.Clear();

            parentConfigurations
                .Zip(distributions, (p, d) => new { p, d })
                .Select(record =>
                    {
                        var dt = new DistributionTable();
                        dt.SetData(_variable, _variableAbbreviations, record.d, record.p, _variableParents);
                        return dt;
                    }
                )
                .ForAll(dt =>
                {
                    xStack.Children.Add(dt);
                    dt.Margin = new Thickness(0, 0, 0, 24);
                });
        }

        private void xScrollViewer_LayoutUpdated(object sender, EventArgs e)
        {
            xStack.Width = xScrollViewer.ActualWidth;
        }


        private IDictionary<string, string> _variableAbbreviations;
        private RandomVariable _variable;
        private IEnumerable<RandomVariable> _variableParents;
        private DistributionSet _distributions;
    }
}
