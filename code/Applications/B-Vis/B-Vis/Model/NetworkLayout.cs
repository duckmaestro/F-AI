using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Bevisuali.Model
{
    internal class NetworkLayout : INetworkLayout
    {
        public IDictionary<string, System.Windows.Point> Positions
        {
            get;
            set;
        }

        public ComputationState ComputationState
        {
            get;
            set;
        }
    }
}
