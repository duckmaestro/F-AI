
using Bevisuali.Util;
using FAI.Bayesian;
using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.Diagnostics;
using System.Linq;
using System.Threading;
using FObservation = FAI.Bayesian.Observation;

namespace Bevisuali.Model
{
    internal partial class Workbench : IWorkbench
    {
        protected class ScenarioRecord
        {
            public Scenario Scenario { get; set; }
            public InferenceQuery Query { get; set; }

            public ScenarioRecord(IScenario scenario, InferenceQuery query)
            {
                this.Scenario = (Scenario)scenario;
                this.Query = query;
            }
        }

        protected readonly int InferenceWarmupSize = 250;
        protected readonly int InferenceTargetRefinement = 16 * 1024;
        protected readonly int InferenceParticleSeparation = 3;
        protected readonly int InferenceStepBatchSizeVariable = 3 * 1024;
        protected readonly int InferenceStepBatchSizeMinimum = 32;

        protected ObservableCollection<IScenario> _scenarios;
        protected List<ScenarioRecord> _scenariosInternal;
        protected ScenarioComparison _scenariosComparison;
        protected double _comparisonResultsLevel;
        protected ComparisonMetric _comparisonMetric;
        protected volatile bool _comparisonOptionsDirty;
        protected Thread _scenariosThread;
        protected volatile bool _scenariosThreadCancel;

        public IList<IScenario> Scenarios
        {
            get
            {
                return _scenarios;
            }
        }
        public IScenarioComparison ComparisonResults
        {
            get
            {
                return _scenariosComparison;
            }
        }
        public double ComparisonResultsLevel
        {
            get
            {
                return _comparisonResultsLevel;
            }
            set
            {
                if (value < 0 || value > 1)
                {
                    throw new ArgumentOutOfRangeException();
                }

                _comparisonResultsLevel = value;
                _comparisonOptionsDirty = true;
            }
        }
        public ComparisonMetric ComparisonMetric
        {
            get
            {
                return this._comparisonMetric;
            }
            set
            {
                this._comparisonMetric = value;
                this._comparisonOptionsDirty = true;
            }
        }
        public event Action<IScenarioComparison> ComparisonResultsUpdated;

        protected void ScenariosChanged(object sender, NotifyCollectionChangedEventArgs e)
        {
            var scenariosNew = (e.NewItems ?? new object[0]).Cast<Scenario>();
            var scenariosOld = (e.OldItems ?? new object[0]).Cast<Scenario>();

            lock (_scenariosInternal)
            {
                // Remove all.
                if (e.Action == NotifyCollectionChangedAction.Reset)
                {
                    _scenariosInternal.Clear();
                }
                // Remove individuals.
                else if (e.Action == NotifyCollectionChangedAction.Remove
                    || e.Action == NotifyCollectionChangedAction.Replace)
                {
                    foreach (var old in scenariosOld)
                    {
                        var oldRecord =
                            _scenariosInternal
                            .FirstOrDefault(o => o.Scenario == old);
                        _scenariosInternal.Remove(oldRecord);
                    }
                }
                // Add new.
                else if (e.Action == NotifyCollectionChangedAction.Add
                    || e.Action == NotifyCollectionChangedAction.Replace)
                {
                    foreach (var @new in scenariosNew)
                    {
                        @new.Workbench = this;

                        FObservation evidence = @new.Evidence;

                        InferenceQuery query = new InferenceQuery(
                            @new.BayesianNetwork, evidence);
                        query.WarmupSize = this.InferenceWarmupSize;
                        query.ParticleSeparation = this.InferenceParticleSeparation;

                        ScenarioRecord record = new ScenarioRecord(
                            @new,
                            query);

                        _scenariosInternal.Add(record);
                    }
                }
            }
        }
        protected void ThreadMainScenariosInference()
        {
            while (true)
            {
                if (_scenariosThreadCancel)
                {
                    break;
                }

                bool didWork = false;

                List<ScenarioRecord> scenarios;
                lock (_scenariosInternal)
                {
                    scenarios = _scenariosInternal.ToList();
                }

                // For each scenario, refine the results.
                foreach (var scenarioRecord in scenarios)
                {
                    InferenceQuery inferenceQuery = scenarioRecord.Query;

                    // If more refinement is needed.
                    if (inferenceQuery.RefinementCount < this.InferenceTargetRefinement)
                    {
                        int nvariables = inferenceQuery.Network.Variables.Count;
                        int batchSize = InferenceStepBatchSizeVariable / nvariables;
                        if (batchSize < InferenceStepBatchSizeMinimum)
                        {
                            batchSize = InferenceStepBatchSizeMinimum;
                        }
                        inferenceQuery.RefineResults(batchSize);
                        didWork = true;

                        var resultsCopy = inferenceQuery.Results.ToDictionary(
                            kvp => kvp.Key,
                            kvp => kvp.Value);
                        scenarioRecord.Scenario.PosteriorMarginals = resultsCopy;
                    }

                    // If no more refinement is needed.
                    if (inferenceQuery.RefinementCount >= this.InferenceTargetRefinement
                        && scenarioRecord.Scenario.InferenceState != ComputationState.Done)
                    {
                        scenarioRecord.Scenario.InferenceState = ComputationState.Done;
                    }
                }

                // Update comparison.
                if (scenarios.Count == 2)
                {
                    if (didWork || _comparisonOptionsDirty)
                    {
                        _comparisonOptionsDirty = false;

                        var comparisonMetric = this._comparisonMetric;

                        ScenarioComparison oldComparison = _scenariosComparison;

                        ScenarioComparison comparison = new ScenarioComparison();
                        comparison.Scenario1 = scenarios[0].Scenario;
                        comparison.Scenario2 = scenarios[1].Scenario;
                        comparison.ComparisonMetric = comparisonMetric;

                        var posteriors1 = comparison.Scenario1.PosteriorMarginals;
                        var posteriors2 = comparison.Scenario2.PosteriorMarginals;
                        List<Tuple<String, Double>> similarities = new List<Tuple<string, double>>();
                        foreach (string variableName in posteriors1.Keys)
                        {
                            var variableDist1 = posteriors1[variableName];
                            var variableDist2 = posteriors2[variableName];
                            double dissimilarity;
                            if (comparisonMetric == Model.ComparisonMetric.SymmetricKLDivergence)
                            {
                                dissimilarity = this.MeasureDissimilarityKL(variableDist1, variableDist2);
                            }
                            else if (comparisonMetric == Model.ComparisonMetric.ErrorSum)
                            {
                                dissimilarity = this.MeasureDissimilarityES(variableDist1, variableDist2);
                            }
                            else
                            {
                                Debug.Fail("Unexpected state.");
                                dissimilarity = 0;
                            }
                            similarities.Add(new Tuple<string, double>(variableName, dissimilarity));
                        }

                        // Put most different variables first in the list.
                        comparison.SignificantVariables
                            = similarities
                            .OrderByDescending(s => s.Item2)
                            .TakeFraction(this._comparisonResultsLevel)
                            .ToArray();

                        // Store in model.
                        this._scenariosComparison = comparison;
                        if (this.ComparisonResultsUpdated != null)
                        {
                            this.ComparisonResultsUpdated(comparison);
                        }

                        // Restart layout process.
                        IList<string> significantVariables
                            = comparison
                            .SignificantVariables
                            .Select(x => x.Item1)
                            .OrderBy(x => x)
                            .ToList();
                        if (oldComparison == null ||
                                oldComparison
                                .SignificantVariables
                                .Select(x => x.Item1)
                                .OrderBy(x => x)
                                .SequenceEqual(significantVariables) == false)
                        {
                            _networkLayoutInternal = new NetworkLayoutRecord(
                                _bayesianNetwork,
                                _networkLayout,
                                significantVariables,
                                NetworkLayoutAlgorithm
                            );
                        }

                        // Done.
                        didWork = true;
                    }
                }
                else
                {
                    _scenariosComparison = null;
                }

                if (_scenariosThreadCancel)
                {
                    break;
                }

                if (!didWork)
                {
                    Thread.Sleep(200);
                }
                else
                {
                    // HACK: Gives UI some time to breath if we're updating inference results
                    //       too often.
                    Thread.Sleep(50);
                }
            }
        }
        protected double MeasureDissimilarityKL(DiscreteDistribution d1, DiscreteDistribution d2)
        {
            // Symmetric KL divergence.
            var kl1 = d1.KLDivergence(d2);
            var kl2 = d2.KLDivergence(d1);
            return kl1 + kl2;
        }
        protected double MeasureDissimilarityES(DiscreteDistribution d1, DiscreteDistribution d2)
        {
            // Error sum.
            var errorSum = d1.ErrorSum(d2);
            return errorSum;
        }
    }
}
