
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Bevisuali.Model
{
    internal class ScenarioComparison : IScenarioComparison
    {
        public IScenario Scenario1
        {
            get;
            set;
        }

        public IScenario Scenario2
        {
            get;
            set;
        }

        public IEnumerable<Tuple<string, double>> SignificantVariables
        {
            get;
            set;
        }

        public ComparisonMetric ComparisonMetric
        {
            get;
            set;
        }
    }
}
