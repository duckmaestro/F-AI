
using Bevisuali.Util;
using FAI.Bayesian;
using Microsoft.FSharp.Core;
using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.Linq;
using System.Threading;

namespace Bevisuali.Model
{
    internal partial class Workbench : IWorkbench
    {
        protected class LearningTaskRecord
        {
            public LearningTask LearningTask { get; set; }
        }

        protected ObservableCollection<ILearningTask> _learningTasks;
        protected List<LearningTaskRecord> _learningTasksInternal;
        protected Thread _learningTasksThread;
        protected volatile bool _learningTasksThreadCancel;

        public IList<ILearningTask> LearningTasks
        {
            get
            {
                return _learningTasks;
            }
        }

        protected void LearningTasksChanged(object sender, NotifyCollectionChangedEventArgs e)
        {
            var tasksNew = (e.NewItems ?? new object[0]).Cast<LearningTask>();
            var tasksOld = (e.OldItems ?? new object[0]).Cast<LearningTask>();

            lock (_learningTasksInternal)
            {
                // Remove all.
                if (e.Action == NotifyCollectionChangedAction.Reset)
                {
                    _learningTasksInternal.Clear();
                }
                // Remove individuals.
                else if (e.Action == NotifyCollectionChangedAction.Remove
                    || e.Action == NotifyCollectionChangedAction.Replace)
                {
                    foreach (var old in tasksOld)
                    {
                        var oldRecord =
                            _learningTasksInternal
                            .FirstOrDefault(o => o.LearningTask == old);
                        _learningTasksInternal.Remove(oldRecord);
                    }
                }
                // Add new.
                else if (e.Action == NotifyCollectionChangedAction.Add
                    || e.Action == NotifyCollectionChangedAction.Replace)
                {
                    foreach (var @new in tasksNew)
                    {
                        @new.Workbench = this;

                        LearningTaskRecord record = new LearningTaskRecord();
                        record.LearningTask = (LearningTask)@new;

                        _learningTasksInternal.Add(record);
                    }
                }
            }
        }
        protected void ThreadMainLearningTasks()
        {
            while (true)
            {
                if (_learningTasksThreadCancel)
                {
                    break;
                }

                bool didWork = false;

                List<LearningTaskRecord> learningTasks;
                lock (_learningTasksInternal)
                {
                    learningTasks = _learningTasksInternal.ToList();
                }

                foreach (var learningTaskRecord in learningTasks)
                {
                    if (learningTaskRecord.LearningTask.LearningState == ComputationState.Initialized)
                    {
                        DoLearning(learningTaskRecord.LearningTask);
                        
                        didWork = true;
                    }
                }

                if (_learningTasksThreadCancel)
                {
                    break;
                }

                if (!didWork)
                {
                    Thread.Sleep(200);
                }
            }
        }
        protected void DoLearning(LearningTask learningTask)
        {
            if (learningTask.LearningState == ComputationState.Done)
            {
                return;
            }

            // Grab options.
            var options = learningTask.Options;

            // Grab training set.
            var trainingSet = learningTask.TrainingSet;

            // Grab variable names.
            var firstObservation = learningTask.TrainingSet.First();
            var variableNames = firstObservation.VariableNames;

            // Build a starter Bayesian network.
            var bn = new BayesianNetwork(learningTask.TrainingSet.Name);

            // Build variables for each variable in the training set.
            foreach (var variableName in variableNames)
            {
                var space = trainingSet.Variables.TryFind(variableName).Value;              
                var rv = new RandomVariable(variableName, space);
                bn.AddVariable(rv);
            }

            // Store in task so that task can send updates to its listeners.
            learningTask.BayesianNetwork = bn;
            learningTask.LearningState = ComputationState.Computing;

            // Build Dirichlet parameters
            IDictionary<string, DirichletDistribution> priors = new Dictionary<string, DirichletDistribution>();
            foreach (var variableName in variableNames)
            {
                var space = trainingSet.Variables.TryFind(variableName).Value;              

                DirichletDistribution prior;
                if (options.DistributionDirichletAlpha > 0)
                {
                    prior = new DirichletDistribution();
                    foreach (var value in space.Values)
                    {
                        prior.SetParameter(value, options.DistributionDirichletAlpha);
                    }
                    priors[variableName] = prior;
                }
            }

            // Initialize a sufficient statistics.
            SufficientStatistics sufficientStatistics 
                = new SufficientStatistics(trainingSet, Utils.Some(priors));

            // Learn structure.
            switch (options.Structure)
            {
                case LearningOptions.StructureEnum.DisconnectedStructure:
                    break;
                case LearningOptions.StructureEnum.RandomStructure:
                    bn.GenerateStructure(
                        StructureClass.Random, 
                        Utils.Some(options.StructureSeed), 
                        Utils.Some(options.StructureParentLimit));
                    break;
                case LearningOptions.StructureEnum.TreeStructure:
                    bn.LearnStructure(
                        sufficientStatistics,
                        StructureClass.Tree,
                        Utils.None<int>(),
                        Utils.None<int>());
                    break;
                case LearningOptions.StructureEnum.GeneralStructure:
                    bn.LearnStructure(
                        sufficientStatistics,
                        StructureClass.General,
                        Utils.None<int>(),
                        Utils.Some(options.StructureParentLimit));
                    break;
            }
            
            // Learn distributions.
            bn.LearnDistributions(sufficientStatistics);

            // Done.
            learningTask.LearningState = ComputationState.Done;
        }
    }
}
