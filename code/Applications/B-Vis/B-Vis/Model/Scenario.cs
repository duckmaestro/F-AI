
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using FObservation = FAI.Bayesian.Observation;
using FDiscreteDistribution = FAI.Bayesian.DiscreteDistribution;
using FAI.Bayesian;

namespace Bevisuali.Model
{
    class Scenario : IScenario
    {
        public Scenario(string id, FObservation evidence, BayesianNetwork network)
        {
            _posteriorMarginals = new Dictionary<string, FDiscreteDistribution>();
            _bayesianNetwork = network;
            Id = id;
            Evidence = evidence;
        }

        public string Id
        {
            get;
            protected set;
        }

        public IWorkbench Workbench
        {
            get;
            set;
        }

        public FObservation Evidence
        {
            get;
            set;
        }

        protected IDictionary<string, FDiscreteDistribution> _posteriorMarginals;
        /// <summary>
        /// Stores the posterior marginal distributions for this scenario.
        /// Assigning a new value to this property raises the InferencedUpdated 
        /// event.
        /// </summary>
        public IDictionary<string, FDiscreteDistribution> PosteriorMarginals
        {
            get
            {
                return _posteriorMarginals;
            }
            set
            {
                if (value == null)
                {
                    throw new ArgumentNullException();
                }
                _posteriorMarginals = value;
                if (InferenceUpdated != null)
                {
                    InferenceUpdated(this);
                }
            }
        }

        public event Action<IScenario> InferenceUpdated;
        public event Action<IScenario> InferenceFinished;


        protected ComputationState _inferenceState;
        public ComputationState InferenceState
        {
            get
            {
                return _inferenceState;
            }
            set
            {
                _inferenceState = value;
                if (_inferenceState == Model.ComputationState.Done
                    && InferenceFinished != null)
                {
                    InferenceFinished(this);
                }
            }
        }

        protected BayesianNetwork _bayesianNetwork;
        public BayesianNetwork BayesianNetwork
        {
            get
            {
                return _bayesianNetwork;
            }
        }
    }
}
