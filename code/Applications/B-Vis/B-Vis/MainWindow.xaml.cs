
using Bevisuali.Model;
using FAI.Bayesian;
using Newtonsoft.Json.Linq;
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Threading.Tasks;
using System.Windows;
using FObservation = FAI.Bayesian.Observation;
using FRandomVariable = FAI.Bayesian.RandomVariable;

namespace Bevisuali.UX
{
    public partial class MainWindow : Window
    {
        private const double AutoHideOpacityVisible = 1.0;
        private const double AutoHideOpacityHidden = 0.3;

        private IWorkbench Model { get; set; }

        public MainWindow()
        {
            InitializeComponent();
        }


        #region Model Events

        void OnModelBayesianNetworkReplaced(IWorkbench workbench)
        {
            // Update graph inspector.
            xGraphInspector.SetGraph(workbench.BayesianNetwork, workbench.BayesianNetworkVariableAbbreviations);
            xGraphInspector.SetInterestVariables(null);

            // Setup evidence slots for two scenarios, and trigger inference for
            // empty evidence set.
            Model.Scenarios.Clear();

            // Reset inference.
            xGraphInspector.SetInferenceResults(null, 1, null);
            xGraphInspector.SetInferenceResults(null, 2, null);
            if (workbench.BayesianNetwork.VariablesOrdered.All(rv => rv.Distributions.EnumerateDistributions().Any()))
            {
                Scenario scenario = new Scenario("1", new FObservation(), workbench.BayesianNetwork);
                scenario.InferenceUpdated += OnScenarioInferenceUpdated;
                scenario.InferenceFinished += OnScenarioInferenceFinished;
                Model.Scenarios.Add(scenario);
            }

            WriteMessage("network loaded");
        }

        void OnModelSelectedVariableUpdated(IWorkbench workbench)
        {
            // Lookup variable.
            string selectedName;
            RandomVariable selectedVariable;
            List<FRandomVariable> selectedVariableParents;
            IDictionary<string, string> abbreviations;

            if (workbench.SelectedVariable != null)
            {
                selectedName = workbench.SelectedVariable;
                selectedVariable =
                    workbench
                    .BayesianNetwork
                    .GetVariable(workbench.SelectedVariable);
                selectedVariableParents =
                    selectedVariable
                    .Parents
                    .Select(p => workbench.BayesianNetwork.GetVariable(p))
                    .ToList();
                abbreviations = workbench.BayesianNetworkVariableAbbreviations;
            }
            else
            {
                selectedName = null;
                selectedVariable = null;
                selectedVariableParents = null;
                abbreviations = null;
            }

            Dispatcher.Invoke(delegate
            {
                // Update the CPT inspector.
                xConditionalDistributions.SetDistribution(
                    selectedVariable,
                    abbreviations,
                    selectedVariableParents,
                    null);

                // Update the posterior marginal inspector.
                {
                    DiscreteDistribution posteriorDistribution;
                    IScenario scenario
                        = Model
                        .Scenarios
                        .FirstOrDefault(s => s.Id == "1");
                    if (scenario != null)
                    {
                        if (selectedName == null)
                        {
                            posteriorDistribution = null;
                        }
                        else
                        {
                            scenario
                                .PosteriorMarginals
                                .TryGetValue(selectedName, out posteriorDistribution);
                        }
                    }
                    else
                    {
                        posteriorDistribution = null;
                    }

                    xMarginalPosteriorDistributions.SetDistribution(
                        selectedVariable,
                        abbreviations,
                        selectedVariableParents,
                        posteriorDistribution);
                }

                // Update node selected property.
                OnModelSelectedVariableModeUpdated(workbench);
            });
        }

        void OnModelSelectedVariableModeUpdated(IWorkbench workbench)
        {
            RandomVariable variable;
            if (workbench.SelectedVariable != null)
            {
                variable = workbench
                    .BayesianNetwork
                    .GetVariable(workbench.SelectedVariable);
            }
            else
            {
                variable = null;
            }

            Dispatcher.Invoke(delegate
            {
                if (variable == null)
                {
                    xGraphInspector.SetSelectedVariable(null);
                }
                else
                {
                    switch (workbench.SelectedVariableMode)
                    {
                        case Mode.Editing:
                            xGraphInspector.SetConfiguringVariable(variable.Name);
                            break;
                        case Mode.Inspecting:
                            xGraphInspector.SetSelectedVariable(variable.Name);
                            break;
                    }
                }
            });
        }

        void OnScenarioInferenceUpdated(IScenario scenario)
        {
            if (scenario.BayesianNetwork != Model.BayesianNetwork)
            {
                // Old inference result.
                return;
            }

            // Grab a copy of the results.
            var marginals = scenario.PosteriorMarginals;
            FObservation evidence = scenario.Evidence;
            var abbreviations = scenario.Workbench.BayesianNetworkVariableAbbreviations;

            Dispatcher.Invoke(delegate
            {
                // Make sure the scenario still exists by the time this is invoked.
                if (!Model.Scenarios.Any(s => s.Id == scenario.Id))
                {
                    return;
                }

                // Recover scenario id.
                int scenarioId = Convert.ToInt32(scenario.Id);

                // Update graph inspector with posteriors.
                xGraphInspector.SetInferenceResults(
                    scenario.PosteriorMarginals,
                    scenarioId,
                    evidence);

                // Update distribution inspectors.
                if (Model.SelectedVariable != null && scenario.Id == "1")
                {
                    var selectedVariable
                        = Model
                        .BayesianNetwork
                        .GetVariable(Model.SelectedVariable);
                    var selectedVariableParents
                        = selectedVariable
                        .Parents
                        .Select(p => Model.BayesianNetwork.GetVariable(p))
                        .ToList();

                    DiscreteDistribution distribution;
                    scenario.PosteriorMarginals.TryGetValue(
                        Model.SelectedVariable,
                        out distribution);

                    xMarginalPosteriorDistributions.SetDistribution(
                        selectedVariable,
                        abbreviations,
                        selectedVariableParents,
                        distribution);
                }

                WriteMessage("updating posteriors...");
            });
        }

        void OnScenarioInferenceFinished(IScenario obj)
        {
            WriteMessage("finished posteriors");
        }

        void OnModelGraphLayoutUpdated(IWorkbench workbench)
        {
            // Grab values.
            var network = workbench.BayesianNetwork;
            var positions = workbench.NetworkLayout.Positions;
            var abbreviations = workbench.BayesianNetworkVariableAbbreviations;
            var options = workbench.NetworkLayoutOptions;

            // Init graph inspector.
            Dispatcher.Invoke(delegate
            {
                // Update graph inspector.
                xGraphInspector.SetGraphLayout(positions, options.NodeSize, options.EdgeThickness);

                // Update network inspector.
                {
                    var variablesOrderedVertically
                        = positions
                        .OrderBy(kvp => kvp.Value.Y)
                        .ThenBy(kvp => kvp.Value.X)
                        .Select(kvp => network.GetVariable(kvp.Key))
                        .ToList();
                    xVariablesInspector.SetVariables(variablesOrderedVertically, abbreviations);
                }
            });
        }

        void OnLearningStarted(ILearningTask learningTask)
        {
            Dispatcher.Invoke(delegate
            {
                SetBayesianNetwork(learningTask.BayesianNetwork, "");
                WriteMessage("learning...");
            });
        }

        void OnLearningFinished(ILearningTask learningTask)
        {
            Dispatcher.Invoke(delegate
            {
                if (Model.BayesianNetwork != learningTask.BayesianNetwork)
                {
                    return;
                }

                // If this learning finished for the network we're currently
                // looking at.

                xLearningInspector.SetIsLearning(false);
                SetBayesianNetwork(learningTask.BayesianNetwork, "");

                xGraphInspector.SetInferenceResults(null, 1, null);
                xGraphInspector.SetInferenceResults(null, 2, null);
                if (Model.BayesianNetwork.VariablesOrdered.All(rv => rv.Distributions.EnumerateDistributions().Any()))
                {
                    Scenario scenario = new Scenario("1", new FObservation(), learningTask.BayesianNetwork);
                    scenario.InferenceUpdated += OnScenarioInferenceUpdated;
                    scenario.InferenceFinished += OnScenarioInferenceFinished;
                    Model.Scenarios.Add(scenario);
                }

                WriteMessage("learning completed");
            });
        }

        void OnModelComparisonResultsUpdated(IScenarioComparison comparison)
        {
            Dispatcher.Invoke(delegate
            {
                // Update graph inspector with "relevant" nodes.
                if (comparison != null)
                {
                    // Gather evidence variable names.
                    var evidenceVariables = comparison.Scenario1.Evidence.Select(x => x.Key).Union(comparison.Scenario2.Evidence.Select(x => x.Key));

                    // Show only interesting variables.
                    xGraphInspector.SetInterestVariables(
                        comparison
                        .SignificantVariables
                        .Select(kvp => kvp.Item1)
                        .Union(evidenceVariables)
                    );

                    // Update UI state for comparison metric.
                    xEvidenceInspector.SetComparisonMetric(comparison.ComparisonMetric);
                    xEvidenceInspector.SetComparisonRelevantVariables(comparison, this.Model.BayesianNetworkVariableAbbreviations);
                }
                else
                {
                    xGraphInspector.SetInterestVariables(null);

                    // Update UI state for comparison metric.
                    xEvidenceInspector.SetComparisonMetric(default(ComparisonMetric));
                    xEvidenceInspector.SetComparisonRelevantVariables(null, null);
                }
            });
        }

        #endregion


        #region UI Events

        private void Window_Initialized(object sender, EventArgs e)
        {
            WriteMessage("welcome to bevisuali");

            Model = new Workbench();

            Model.BayesianNetworkReplaced += OnModelBayesianNetworkReplaced;
            Model.SelectedVariableUpdated += OnModelSelectedVariableUpdated;
            Model.SelectedVariableModeUpdated += OnModelSelectedVariableModeUpdated;
            Model.NetworkLayoutUpdated += OnModelGraphLayoutUpdated;
            Model.ComparisonResultsUpdated += OnModelComparisonResultsUpdated;

            Model.ComparisonResultsLevel = xSliderDetail.Value;

            xGraphInspector.SetGraph(null, null);
            xLearningInspector.SetTrainingSet(null);
            xConditionalDistributions.SetDistribution(null, null, null, null);
            xMarginalPosteriorDistributions.SetDistribution(null, null, null, null);
            xObservationSetInspector.SetObservationSet(null);
        }

        private void Window_Loaded(object sender, RoutedEventArgs e)
        {

        }

        private void Window_Closing(object sender, System.ComponentModel.CancelEventArgs e)
        {
            var model = this.Model;
            model.Dispose();
        }

        private void Window_CommandOpen(object sender, System.Windows.Input.ExecutedRoutedEventArgs e)
        {
            xObservationSetInspector.OpenFileBrowser();
        }

        private void LeftTabs_MouseEnter(object sender, System.Windows.Input.MouseEventArgs e)
        {
            xLeftTabs.Opacity = AutoHideOpacityVisible;
        }

        private void LeftTabs_MouseLeave(object sender, System.Windows.Input.MouseEventArgs e)
        {
            xLeftTabs.Opacity = AutoHideOpacityHidden;
        }

        private void RightTabs_MouseEnter(object sender, System.Windows.Input.MouseEventArgs e)
        {
            xRightTabs.Opacity = AutoHideOpacityVisible;
        }

        private void RightTabs_MouseLeave(object sender, System.Windows.Input.MouseEventArgs e)
        {
            xRightTabs.Opacity = AutoHideOpacityHidden;
        }

        private void xSliderDetail_MouseEnter(object sender, System.Windows.Input.MouseEventArgs e)
        {
            xSliderDetail.Opacity = AutoHideOpacityVisible;
        }

        private void xSliderDetail_MouseLeave(object sender, System.Windows.Input.MouseEventArgs e)
        {
            xSliderDetail.Opacity = AutoHideOpacityHidden;
        }

        private void xSliderZoom_MouseEnter(object sender, System.Windows.Input.MouseEventArgs e)
        {
            xSliderZoom.Opacity = AutoHideOpacityVisible;
        }

        private void xSliderZoom_MouseLeave(object sender, System.Windows.Input.MouseEventArgs e)
        {
            xSliderZoom.Opacity = AutoHideOpacityHidden;
        }

        private void SliderZoom_Changed(object sender, RoutedPropertyChangedEventArgs<double> e)
        {
            // Transform [0,1] into zoom level bounds of [1/2, 2]

            double shifted = (e.NewValue - 0.5) * 2;
            double zoom = Math.Pow(2, shifted);

            xGraphInspector.SetZoom(zoom);
        }

        private void SliderDetail_Changed(object sender, RoutedPropertyChangedEventArgs<double> e)
        {
            if (this.Model != null)
            {
                this.Model.ComparisonResultsLevel = e.NewValue;
            }
        }

        #endregion


        #region Helpers

        protected void WriteMessage(string statusMessage)
        {
            Dispatcher.Invoke(delegate
            {
                string networkName;
                if (Model == null || Model.BayesianNetwork == null)
                {
                    networkName = null;
                }
                else
                {
                    networkName = Model.BayesianNetwork.Name;
                }

                string prefix;
                if (networkName != null)
                {
                    prefix = networkName;
                }
                else if (networkName != null)
                {
                    prefix = networkName;
                }
                else
                {
                    prefix = null;
                }

                if (string.IsNullOrWhiteSpace(statusMessage))
                {
                    xStatus.Text = prefix;
                }
                else
                {
                    xStatus.Text = string.Format(@"{0} ({1})", prefix, statusMessage);
                }
            });
        }

        private BayesianNetwork LoadNetwork(string uri)
        {
            string data = App.Current.LoadData(uri);
            if (string.IsNullOrWhiteSpace(data))
            {
                return null;
            }

            JObject data_j = JObject.Parse(data);

            BayesianNetwork bn = data_j.ToBayesianNetwork();
            return bn;
        }

        protected void SetBayesianNetwork(BayesianNetwork network, string sourceUri)
        {
            // Store in model.
            Model.BayesianNetwork = network;
        }

        #endregion


        #region Request Handling

        internal void RequestLoadUnknownFile(string path)
        {
            string extension = Path.GetExtension(path).ToLower();
            if (extension == "")
            {
                return;
            }

            if (extension == ".bn")
            {
                RequestLoadBayesianNetwork(path);
            }
            else if (extension == ".txt")
            {
                RequestLoadObservationSet(path);
            }
        }

        internal void RequestTraining(IObservationSet trainingSet, LearningOptions options)
        {
            if (trainingSet == null)
            {
                throw new ArgumentNullException();
            }

            ILearningTask learningTask
                = new LearningTask(
                    Guid.NewGuid().ToString(),
                    trainingSet,
                    options);

            learningTask.BayesianNetworkStarted += OnLearningStarted;
            learningTask.BayesianNetworkFinished += OnLearningFinished;

            Model.LearningTasks.Clear();
            Model.LearningTasks.Add(learningTask);

            xLearningInspector.SetIsLearning(true);
        }

        internal void RequestSaveBayesianNetwork(string uri)
        {
            var network = Model.BayesianNetwork;
            if (network == null)
            {
                App.Current.SaveData(uri, string.Empty);
            }
            else
            {
                JObject network_j = network.ToJObject();
                App.Current.SaveData(uri, network_j.ToString());
            }
        }

        internal void RequestLoadBayesianNetwork(string uri)
        {
            Task.Factory.StartNew(delegate
            {
                WriteMessage("loading network file...");

                string name = Path.GetFileNameWithoutExtension(uri);
                string pathToNetworkFile = uri;

                BayesianNetwork network = LoadNetwork(pathToNetworkFile);

                if (network == null)
                {
                    WriteMessage("error loading network file");
                }
                else
                {
                    Dispatcher.BeginInvoke(new Action(delegate
                    {
                        SetBayesianNetwork(network, uri);
                        xRightTabs.SelectedItem = xTabVariables;
                    }));
                }
            });
        }

        internal void RequestLoadObservationSet(string path)
        {
            // Parse path.
            Task.Factory.StartNew(delegate
            {
                string dataParentFolder = Path.GetDirectoryName(path);
                string dataSimpleFileName = Path.GetFileNameWithoutExtension(path);
                string dataSimpleFileNameLower = dataSimpleFileName.ToLower();

                // Load data.
                WriteMessage("loading observation set...");
                IObservationSet observations;

                // Decide loader.
                if (dataSimpleFileNameLower.Contains("traffic"))
                {
                    observations = FAI.Loaders.TrafficLoader.LoadFromFile(path) as IObservationSet;
                }
                else if (dataSimpleFileNameLower.Contains("census"))
                {
                    observations = FAI.Loaders.USCensus1990.LoadFromFile(path) as IObservationSet;
                }
                else
                {
                    observations = null;
                }

                Model.DataSet = observations;

                // Update UI.
                Dispatcher.BeginInvoke(new Action(delegate
                {
                    xObservationSetInspector.SetObservationSet(observations);
                    xLearningInspector.SetTrainingSet(observations);
                    WriteMessage("observation set loaded");
                }));
            });
        }

        internal void RequestSelectVariable(RandomVariable rv)
        {
            if (rv != null)
            {
                Model.SelectedVariable = rv.Name;
                Model.SelectedVariableMode = Mode.Inspecting;
            }
            else
            {
                Model.SelectedVariable = null;
                Model.SelectedVariableMode = Mode.Inspecting;
            }
        }

        internal void RequestConfigureVariable(RandomVariable rv)
        {
            Model.SelectedVariable = rv.Name;
            Model.SelectedVariableMode = Mode.Editing;
        }

        internal void RequestConfigureVariableWithEvidence(RandomVariable rv, int scenarioId, float? value)
        {
            if (scenarioId <= 0)
            {
                throw new ArgumentOutOfRangeException("ScenarioId");
            }


            // Find existing scenario.
            IScenario oldScenario;
            oldScenario = Model.Scenarios.FirstOrDefault(s => s.Id == scenarioId.ToString());
            if (oldScenario == null)
            {
                oldScenario = new Scenario(scenarioId.ToString(), new FObservation(), Model.BayesianNetwork);
            }
            else
            {
                oldScenario.InferenceUpdated -= OnScenarioInferenceUpdated;
                oldScenario.InferenceFinished -= OnScenarioInferenceFinished;
            }

            FObservation oldEvidence = oldScenario.Evidence;
            FObservation newEvidence;

            // Build new evidence.
            if (value == null)
            {
                newEvidence = FObservation.op_DotMinusDot(oldEvidence, rv.Name);
            }
            else
            {
                newEvidence
                    = FObservation.op_DotPlusDot(
                        oldEvidence,
                        new Tuple<string, float>(rv.Name, value.Value)
                    );
            }

            if (object.Equals(newEvidence, oldEvidence))
            {
                return;
            }

            // Create new scenario.
            IScenario newScenario = new Scenario(scenarioId.ToString(), newEvidence, Model.BayesianNetwork);
            newScenario.InferenceUpdated += OnScenarioInferenceUpdated;
            newScenario.InferenceFinished += OnScenarioInferenceFinished;
            Model.Scenarios.Remove(oldScenario);
            Model.Scenarios.Add(newScenario);

            // Update UI immediately.
            {
                xGraphInspector.SetInferenceResults(null, scenarioId, newEvidence);

                var evidencesForEvidenceInspector =
                    Model.Scenarios.ToDictionary(
                        e => string.Format("Scenario {0}", e.Id),
                        e => new Tuple<FObservation, FRandomVariable[]>(e.Evidence, e.Workbench.BayesianNetwork.VariablesOrdered.ToArray())
                    );
                xEvidenceInspector.SetEvidence(evidencesForEvidenceInspector, this.Model.BayesianNetworkVariableAbbreviations);
            }

            // Reset to simple node selection state.
            Model.SelectedVariable = rv.Name;
            Model.SelectedVariableMode = Mode.Inspecting;
        }

        internal void RequestResetEvidence(int? scenarioId)
        {
            if (scenarioId == null)
            {
                foreach (IScenario scenario in Model.Scenarios)
                {
                    scenario.InferenceUpdated -= OnScenarioInferenceUpdated;
                    scenario.InferenceFinished -= OnScenarioInferenceFinished;
                    xGraphInspector.SetInferenceResults(null, int.Parse(scenario.Id), new FObservation());
                }
                Model.Scenarios.Clear();
            }
            else
            {
                string scenarioIdAsString = scenarioId.Value.ToString();
                var scenario = Model.Scenarios.Where(s => s.Id == scenarioIdAsString).FirstOrDefault();
                if (scenario != null)
                {
                    Model.Scenarios.Remove(scenario);
                }
            }

            // Restore basic scenario 1.
            if (scenarioId == null || scenarioId.Value == 1)
            {
                Scenario scenario = new Scenario("1", new FObservation(), Model.BayesianNetwork);
                scenario.InferenceUpdated += OnScenarioInferenceUpdated;
                scenario.InferenceFinished += OnScenarioInferenceFinished;
                Model.Scenarios.Add(scenario);
            }

            // Update UI immediately.
            {
                var evidencesForEvidenceInspector =
                    Model.Scenarios.ToDictionary(
                        e => string.Format("Scenario {0}", e.Id),
                        e => new Tuple<FObservation, FRandomVariable[]>(e.Evidence, e.Workbench.BayesianNetwork.VariablesOrdered.ToArray())
                    );
                xEvidenceInspector.SetEvidence(evidencesForEvidenceInspector, Model.BayesianNetworkVariableAbbreviations);
                xGraphInspector.SetInterestVariables(null);
            }

            // Reset to simple node selection state.
            Model.SelectedVariableMode = Mode.Inspecting;
        }

        internal void RequestSetComparisonMetric(ComparisonMetric comparisonMetric)
        {
            this.Model.ComparisonMetric = comparisonMetric;
        }

        internal void RequestLayoutOptions(NetworkLayoutOptions options)
        {
            Debug.Assert(options != null);
            this.Model.NetworkLayoutOptions = options;
        }

        #endregion
    }
}
