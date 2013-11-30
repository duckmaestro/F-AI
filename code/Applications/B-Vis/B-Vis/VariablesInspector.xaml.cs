
using Microsoft.Win32;
using System.Collections.Generic;
using System.ComponentModel;
using System.Linq;
using System.Windows;
using System.Windows.Controls;
using FRandomVariable = FAI.Bayesian.RandomVariable;

namespace Bevisuali.UX
{
    public partial class VariablesInspector : UserControl
    {
        public VariablesInspector()
        {
            InitializeComponent();
        }

        public void SetVariables(IList<FRandomVariable> variables, IDictionary<string,string> abbreviations)
        {
            var uichildren = xVariableList.Children;
            int i = 0; 
            foreach (var rv in variables)
            {
                if (i >= uichildren.Count)
                {
                    VariablesInspectorVariable niv = new VariablesInspectorVariable();
                    niv.SetVariable(rv, abbreviations);
                    xVariableList.Children.Add(niv);
                }
                else
                {
                    FRandomVariable rv_i = (uichildren[i] as VariablesInspectorVariable).GetVariable();
                    if (rv_i != rv)
                    {
                        VariablesInspectorVariable niv_rv
                            = uichildren
                            .OfType<VariablesInspectorVariable>()
                            .Where(niv => niv.GetVariable() == rv)
                            .FirstOrDefault();
                        if (niv_rv != null)
                        {
                            uichildren.Remove(niv_rv);
                        }
                        else
                        {
                            niv_rv = new VariablesInspectorVariable();
                            niv_rv.SetVariable(rv, abbreviations);
                        }

                        uichildren.Insert(i, niv_rv);
                    }
                }
                ++i;
            }

            if (uichildren.Count >= variables.Count)
            {
                uichildren.RemoveRange(variables.Count, uichildren.Count - variables.Count);
            }
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

        private void xButtonSave_Click(object sender, RoutedEventArgs e)
        {
            SaveFileDialog dialog = new SaveFileDialog();
            dialog.AddExtension = true;
            dialog.DefaultExt = ".bn";
            dialog.FileOk += delegate(object s2, CancelEventArgs e2)
            {
                string fileName = dialog.FileName;
                App.Current.MainWindow.RequestSaveBayesianNetwork(fileName);
            };
            dialog.ShowDialog();
        }
    }
}
