
using Bevisuali.Util;
using FAI.Bayesian;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Windows.Controls;
using FDiscreteDistribution = FAI.Bayesian.DiscreteDistribution;
using FObservation = FAI.Bayesian.Observation;

namespace Bevisuali.UX.Distribution
{
    public partial class DistributionTable : UserControl
    {
        public DistributionTable()
        {
            InitializeComponent();

            this.LayoutUpdated += OnLayoutUpdated;
        }

        public void SetData(
            RandomVariable variable,
            IDictionary<string,string> variableAbbreviations,
            FDiscreteDistribution distribution,
            FObservation conditionedOn,
            IEnumerable<RandomVariable> parents)
        {
            _variable = variable;
            _distribution = distribution;
            _conditionedOn = conditionedOn;
            _variableAbbreviations = variableAbbreviations;
            _parents 
                = parents
                .Where(p => conditionedOn.Any(kvp => kvp.Key == p.Name))
                .ToArray();

            RefreshUI();
        }

        public RandomVariable Variable
        {
            get
            {
                return _variable;
            }
        }

        public FDiscreteDistribution Distribution
        {
            get
            {
                return _distribution;
            }
        }

        public FObservation ConditionedOn
        {
            get
            {
                return _conditionedOn;
            }
        }

        private void RefreshUI()
        {
            if (_conditionedOn == null
                || _variable == null
                || _distribution == null
                || _parents == null)
            {
                return;
            }

            // Setup variable name.
            {
                char letter;
                string subscript;
                Utils.ParseVariableName(_variableAbbreviations[_variable.Name], out letter, out subscript);

                xNameLetter.Text = letter.ToString();
                xNameSubscript.Text = subscript;
            }

            // Setup parent values region.
            xConditionedOn.SetData(_conditionedOn, _parents, _variableAbbreviations);

            // Add masses.
            xMasses.Children.Clear();
            var space = _variable.Space;
            var values = space.Values.ToList();
            var spaceSize = values.Count();

            for (int i = 0; i < spaceSize; ++i)
            {
                var value = values[i];
                var label = space.GetLabel(value);
                var color = _variable.Space.GetColor(value);
                var mass = (double)_distribution.GetMass(value).Value;

                var row = new DistributionMass();
                row.ValueColor = color;
                row.ValueLabel = label;
                row.ValueMass = mass;
                row.HorizontalAlignment = System.Windows.HorizontalAlignment.Left;

                xMasses.Children.Add(row);
            }
        }

        private void OnLayoutUpdated(object sender, System.EventArgs e)
        {
            double newWidth 
                = this.ActualWidth
                - xConditionedOn.Margin.Left
                - xConditionedOn.Margin.Right;

            xConditionedOn.Width = Math.Max(newWidth, 0);
        }


        private RandomVariable _variable;
        private IDictionary<string, string> _variableAbbreviations;
        private FDiscreteDistribution _distribution;
        private FObservation _conditionedOn;
        private RandomVariable[] _parents;
    }
}
