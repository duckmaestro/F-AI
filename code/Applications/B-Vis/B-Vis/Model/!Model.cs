
using FAI.Bayesian;
using System;
using System.Collections.Generic;
using System.Windows;
using FBayesianNetwork = FAI.Bayesian.BayesianNetwork;
using FObservation = FAI.Bayesian.Observation;
using FObservationSet = FAI.Bayesian.IObservationSet;

namespace Bevisuali.Model
{
    internal enum ComputationState
    {
        Initialized,
        Computing,
        Done,
    }

    internal enum Mode
    {
        Inspecting,
        Editing,
    }

    public enum NetworkLayoutAlgorithm
    {
        KK,
        SugiyamaEfficient,
        Sugiyama,
        CompoundFDP,
    }

    internal class LearningOptions
    {
        public enum StructureEnum
        {
            DisconnectedStructure = 0x00,
            RandomStructure = 0x01,
            TreeStructure = 0x02,
            GeneralStructure = 0x03,
        }

        /// <summary>
        /// The graph structure type to learn.
        /// </summary>
        public StructureEnum Structure = StructureEnum.DisconnectedStructure;

        /// <summary>
        /// The structure seed for random structures.
        /// </summary>
        public int StructureSeed = 0;

        /// <summary>
        /// The limit on the number of parents a variable can have.
        /// </summary>
        public int StructureParentLimit = int.MaxValue;

        /// <summary>
        /// The alpha value for the Dirichlet prior over the conditional
        /// distributions being learned.
        /// </summary>
        public double DistributionDirichletAlpha = 1.0;
    }


    internal interface IScenario
    {
        /// <summary>
        /// A unique id for this scenario.
        /// </summary>
        string Id { get; }

        /// <summary>
        /// A reference to the workbench.
        /// </summary>
        IWorkbench Workbench { get; }

        /// <summary>
        /// The Bayesin network associated with this scenario.
        /// </summary>
        FBayesianNetwork BayesianNetwork { get; }

        /// <summary>
        /// The evidence for this scenario.
        /// </summary>
        FObservation Evidence { get; }

        /// <summary>
        /// The resulting posterior marginal distributions.
        /// </summary>
        IDictionary<string, DiscreteDistribution> PosteriorMarginals { get; }

        /// <summary>
        /// The state of the inference computation.
        /// </summary>
        ComputationState InferenceState { get; }

        event Action<IScenario> InferenceUpdated;
        event Action<IScenario> InferenceFinished;
    }

    internal interface ILearningTask
    {
        /// <summary>
        /// A unique id for this learning task.
        /// </summary>
        string Id { get; }

        /// <summary>
        /// A reference to the workbench.
        /// </summary>
        IWorkbench Workbench { get; }

        /// <summary>
        /// The training set for this learning task.
        /// </summary>
        FObservationSet TrainingSet { get; }

        /// <summary>
        /// The options to use for training.
        /// </summary>
        LearningOptions Options { get; }

        /// <summary>
        /// The resulting Bayesian network.
        /// </summary>
        BayesianNetwork BayesianNetwork { get; }

        /// <summary>
        /// The state of the learning computation.
        /// </summary>
        ComputationState LearningState { get; }

        event Action<ILearningTask> BayesianNetworkStarted;
        event Action<ILearningTask> BayesianNetworkFinished;
    }

    internal interface INetworkLayout
    {
        IDictionary<string, Point> Positions { get; }

        /// <summary>
        /// The state of the layout data.
        /// </summary>
        ComputationState ComputationState { get; }
    }

    internal interface IScenarioComparison
    {
        /// <summary>
        /// The base scenario.
        /// </summary>
        IScenario Scenario1 { get; }

        /// <summary>
        /// The other scenario.
        /// </summary>
        IScenario Scenario2 { get; }

        /// <summary>
        /// The variables with most change between the two scenarios,
        /// in order from greatest change to least change.
        /// </summary>
        IEnumerable<Tuple<String, Double>> SignificantVariables { get; }
    }

    internal interface IWorkbench : IDisposable
    {
        /// <summary>
        /// A unique id for this workbench.
        /// </summary>
        string Id { get; set; }

        /// <summary>
        /// The primary data set loaded.
        /// </summary>
        FObservationSet DataSet { get; set; }

        /// <summary>
        /// The name of the selected variable.
        /// </summary>
        string SelectedVariable { get; set; }
        Mode SelectedVariableMode { get; set; }
        event Action<IWorkbench> SelectedVariableUpdated;
        event Action<IWorkbench> SelectedVariableModeUpdated;

        /// <summary>
        /// The loaded bayesian network.
        /// </summary>
        FBayesianNetwork BayesianNetwork { get; set; }
        event Action<IWorkbench> BayesianNetworkReplaced;

        /// <summary>
        /// Abbreviations for each variable in the network, such as
        /// "Apple" -> "A_3".
        /// </summary>
        IDictionary<string, string> BayesianNetworkVariableAbbreviations { get; }

        /// <summary>
        /// The desired network layout algorithm.
        /// </summary>
        NetworkLayoutAlgorithm NetworkLayoutAlgorithm { get; set; }
        /// <summary>
        /// The graph layout for the Bayesian network. This is automatically
        /// kept up-to-date with changes to the current Bayesian network.
        /// </summary>
        INetworkLayout NetworkLayout { get; }
        event Action<IWorkbench> NetworkLayoutUpdated;
       
        /// <summary>
        /// The active scenarios.
        /// </summary>
        IList<IScenario> Scenarios { get; }

        /// <summary>
        /// The list of important variables.
        /// </summary>
        IScenarioComparison ComparisonResults { get; }

        /// <summary>
        /// A value between 0 and 1, where 0 hides all variables,
        /// 1 shows all variables, and intermediate values show a percentage
        /// of all variables, chosen by those which have greatest difference.
        /// </summary>
        double ComparisonResultsLevel { get; set; }

        /// <summary>
        /// Raised when the comparison results are updated.
        /// </summary>
        event Action<IScenarioComparison> ComparisonResultsUpdated;

        /// <summary>
        /// Learning tasks.
        /// </summary>
        IList<ILearningTask> LearningTasks { get; }
    }
}
