
using FAI.Bayesian;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Bevisuali.Model
{
    internal class LearningTask : ILearningTask
    {
        public LearningTask(string id, IObservationSet trainingSet, LearningOptions options)
        {
            Id = id;
            TrainingSet = trainingSet;
            Options = options;
        }

        public string Id
        {
            get;
            set;
        }

        public IWorkbench Workbench
        {
            get;
            set;
        }

        public IObservationSet TrainingSet
        {
            get;
            protected set;
        }

        public LearningOptions Options
        {
            get;
            protected set;
        }

        protected BayesianNetwork _bayesianNetwork;
        public BayesianNetwork BayesianNetwork
        {
            get
            {
                return _bayesianNetwork;
            }
            set
            {
                if (value == null)
                {
                    throw new ArgumentNullException();
                }

                _bayesianNetwork = value;
            }
        }

        protected ComputationState _learningState;
        public ComputationState LearningState
        {
            get
            {
                return _learningState;
            }
            set
            {
                var @new = value;
                var @old = _learningState;
                _learningState = @new;

                if (@new != @old)
                {
                    if (@new == ComputationState.Computing
                        && BayesianNetworkStarted != null)
                    {
                        BayesianNetworkStarted(this);
                    }
                    else if (@new == ComputationState.Done
                        && BayesianNetworkFinished != null)
                    {
                        BayesianNetworkFinished(this);
                    }
                }
            }
        }

        public event Action<ILearningTask> BayesianNetworkStarted;
        public event Action<ILearningTask> BayesianNetworkFinished;
    }
}
