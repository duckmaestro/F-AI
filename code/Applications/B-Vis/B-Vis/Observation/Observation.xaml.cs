
using Bevisuali.Util;
using System.Collections.Generic;
using System.Windows.Controls;
using System.Windows.Media;
using FObservation = FAI.Bayesian.Observation;
using FRandomVariable = FAI.Bayesian.RandomVariable;

namespace Bevisuali.UX.Observation
{
    /// <summary>
    /// Shows the values of an observation. Supports full and partial 
    /// observatations.
    /// </summary>
    public partial class Observation : UserControl
    {
        public Observation()
        {
            InitializeComponent();
        }

        public void SetData(FObservation values, FRandomVariable[] variables, IDictionary<string,string> abbreviations)
        {
            _values = values;
            _variables = variables ?? new FRandomVariable[0];
            _variableAbbreviations = abbreviations;
            RefreshUI();
        }

        public Color VariableNamesColor
        {
            get
            {
                return _variableNameColor;
            }
            set
            {
                _variableNameColor = value;
                RefreshUI();
            }
        }

        private void RefreshUI()
        {
            xFlow.Children.Clear();

            if (_variables == null || _values == null)
            {
                return;
            }

            foreach (var rv in _variables)
            {
                var variableValueMaybe = _values.TryValueForVariable(rv.Name);
                if (variableValueMaybe == null)
                {
                    continue;
                }
                var variableValue = variableValueMaybe.Value;

                ObservationNameValue onv = new ObservationNameValue();
                onv.VariableName = _variableAbbreviations[rv.Name];
                if (_variableNameColor != default(Color))
                {
                    onv.VariableNameColor = _variableNameColor;
                }
                onv.VariableValueLabel = rv.Space.GetLabel(variableValue);
                onv.VariableValueColor = rv.Space.GetColor(variableValue);

                xFlow.Children.Add(onv);
            }
        }

        private Color _variableNameColor;
        private FObservation _values;
        private FRandomVariable[] _variables;
        private IDictionary<string, string> _variableAbbreviations;
    }
}
