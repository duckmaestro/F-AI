
using Bevisuali.Util;
using Bevisuali.UX.Observation;
using System.Collections.Generic;
using System.Windows.Controls;
using FRandomVariable = FAI.Bayesian.RandomVariable;

namespace Bevisuali.UX
{
    public partial class VariablesInspectorVariable : UserControl
    {
        public VariablesInspectorVariable()
        {
            InitializeComponent();
        }

        public FRandomVariable GetVariable()
        {
            return _variable;
        }

        public void SetVariable(FRandomVariable variable, IDictionary<string,string> abbreviations)
        {
            _variable = variable;

            // Short name.
            char letter;
            string subscript;
            Utils.ParseVariableName(abbreviations[variable.Name], out letter, out subscript);

            xNameLetter.Text = letter.ToString();
            xNameSubscript.Text = subscript;

            // Long name.
            xFullName.Text = variable.Name;

            // Space.
            xSpace.Children.Clear();
            foreach (var value in variable.Space.Values)
            {
                ObservationNameValue onv = new ObservationNameValue();
                onv.VariableName = "";
                onv.VariableValueLabel = variable.Space.GetLabel(value);
                onv.VariableValueColor = variable.Space.GetColor(value);
                onv.HorizontalAlignment = System.Windows.HorizontalAlignment.Left;

                xSpace.Children.Add(onv);
            }
        }

        private FRandomVariable _variable;
    }
}
