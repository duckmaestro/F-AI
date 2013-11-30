
using FAI.Bayesian;
using GraphSharp.Algorithms.Layout;
using GraphSharp.Algorithms.Layout.Simple.FDP;
using GraphSharp.Algorithms.Layout.Simple.Hierarchical;
using GraphSharp.Algorithms.Layout.Compound.FDP;
using QuickGraph;
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Threading;
using System.Windows;

namespace Bevisuali.Model
{
    internal partial class Workbench : IWorkbench
    {
        protected class NetworkLayoutRecord
        {
            public NetworkLayout NetworkLayout { get; protected set; }
            public LayoutAlgorithm AlgorithmState { get; protected set; }

            public NetworkLayoutRecord(BayesianNetwork network, NetworkLayout layout, NetworkLayoutAlgorithm algorithm)
            {
                NetworkLayout = layout;

                // Manually specify sizes.
                Dictionary<string, float> sizes = new Dictionary<string, float>();
                foreach (var v in network.Variables)
                {
                    sizes[v.Key] = Workbench.NetworkLayoutVertexSizeNormal;
                }

                // Instantiate algorithm.
                AlgorithmState = new LayoutAlgorithm(network.Clone(), sizes, algorithm);

                // Copy existing positions over.
                if (layout != null && layout.Positions != null && layout.Positions.Count > 0)
                {
                    foreach (var kvp in layout.Positions)
                    {
                        AlgorithmState.Positions[kvp.Key] = kvp.Value;
                    }
                }
            }

            public NetworkLayoutRecord(BayesianNetwork network, NetworkLayout existingLayout, IEnumerable<string> interestVertices, NetworkLayoutAlgorithm algorithm)
            {
                NetworkLayout = existingLayout;

                // Manually specify sizes.
                Dictionary<string, float> sizes = new Dictionary<string, float>();
                foreach (var v in network.Variables)
                {
                    if (!interestVertices.Contains(v.Key))
                    {
                        sizes[v.Key] = Workbench.NetworkLayoutVertexSizeMinimized;
                    }
                    else
                    {
                        sizes[v.Key] = Workbench.NetworkLayoutVertexSizeNormal;
                    }
                }

                AlgorithmState = new LayoutAlgorithm(network.Clone(), sizes, algorithm);

                // Copy existing positions over.
                if (existingLayout != null && existingLayout.Positions != null && existingLayout.Positions.Count > 0)
                {
                    foreach (var kvp in existingLayout.Positions)
                    {
                        AlgorithmState.Positions[kvp.Key] = kvp.Value;
                    }
                }
            }
        }

        protected class LayoutAlgorithm
        {
            public int IterationCount { get; set; }
            public BayesianNetwork BayesianNetwork { get; set; }
            public IDictionary<string, Point> Positions { get; set; }
            
            private Dictionary<string, Size> _sizes;
            private NetworkLayoutAlgorithm _algorithm;

            public LayoutAlgorithm(BayesianNetwork network, IDictionary<string, float> sizes, NetworkLayoutAlgorithm algorithm)
            {
                this.BayesianNetwork = network;
                this.Positions = new Dictionary<string, Point>();
                
                this._sizes = new Dictionary<string, Size>();
                this._algorithm = algorithm;

                foreach (var kvp in sizes)
                {
                    this._sizes[kvp.Key] = new Size(kvp.Value, kvp.Value);
                }
            }

            public void RefineLayout(int iterations)
            {
                // Prepare state for algorithm.
                BidirectionalGraph<string, IEdge<string>> graph
                    = new BidirectionalGraph<string, IEdge<string>>(false);
                Dictionary<string, Point> positions
                    = new Dictionary<string, Point>();

                // Anything to do?
                if (BayesianNetwork.VariablesOrdered.Any() == false)
                {
                    this.Positions = new Dictionary<string, Point>();
                    IterationCount += iterations;
                    return;
                }

                Random random = new Random(0);
                foreach (var rv in BayesianNetwork.VariablesOrdered)
                {
                    graph.AddVertex(rv.Name);

                    foreach (var child
                        in rv.Children.Select(c => BayesianNetwork.GetVariable(c)))
                    {
                        graph.AddVertex(child.Name);
                        graph.AddEdge(new Edge<string>(rv.Name, child.Name));
                    }

                    // If we have no existing layout yet, lets try to prime the
                    // alg by putting pure parents at top and pure children at 
                    // bottom.
                    if (Positions.Count != 0)
                    {                    
                        // We have existing layout. Start with it but add slight
                        // randomness.

                        Point positionNoised;
                        if (Positions.ContainsKey(rv.Name))
                        {
                            positionNoised = Positions[rv.Name];
                        }
                        else
                        {
                            positionNoised = new Point();
                        }
                        positionNoised.X += (random.NextDouble() - 0.5) * 0.01;
                        positionNoised.Y += (random.NextDouble() - 0.5) * 0.01;
                        positions[rv.Name] = positionNoised;
                    }
                }

                // Initialize algorithm.
                var layoutAlgorithms
                    = new StandardLayoutAlgorithmFactory<string, IEdge<string>, IBidirectionalGraph<string, IEdge<string>>>();

                var layoutContext = new LayoutContext<string, IEdge<string>, IBidirectionalGraph<string, IEdge<string>>>(
                    graph,
                    positions,
                    _sizes,
                    LayoutMode.Simple);

                ILayoutAlgorithm<string, IEdge<string>, IBidirectionalGraph<string, IEdge<string>>> layoutAlgorithm;


                // Hack: SugiyamaEfficient breaks if no edges.
                if (_algorithm == NetworkLayoutAlgorithm.KK || graph.Edges.Count() == 0)
                {
                    var layoutParameters = new KKLayoutParameters();
                    layoutParameters.Height = 1000;
                    layoutParameters.Width = 1000;
                    layoutParameters.MaxIterations = iterations;
                    layoutParameters.LengthFactor = 1.35;
                    layoutParameters.K *= 10.1;
                    layoutParameters.AdjustForGravity = false;

                    layoutAlgorithm = layoutAlgorithms.CreateAlgorithm("KK", layoutContext, layoutParameters);
                }
                else if(_algorithm == NetworkLayoutAlgorithm.SugiyamaEfficient)
                {
                    var layoutParameters = new EfficientSugiyamaLayoutParameters();
                    layoutParameters.MinimizeEdgeLength = true;
                    layoutParameters.OptimizeWidth = true;
                    layoutParameters.WidthPerHeight = 1.6;
                    layoutParameters.VertexDistance = 10.0;
                    layoutParameters.LayerDistance = 1.0;

                    layoutAlgorithm = layoutAlgorithms.CreateAlgorithm("EfficientSugiyama", layoutContext, layoutParameters);
                }
                else if (_algorithm == NetworkLayoutAlgorithm.Sugiyama)
                {
                    var layoutParameters = new SugiyamaLayoutParameters();
                    layoutParameters.MaxWidth = 1024;
                    layoutParameters.VerticalGap = 1.0f;
                    layoutParameters.DirtyRound = true;

                    layoutAlgorithm = layoutAlgorithms.CreateAlgorithm("Sugiyama", layoutContext, layoutParameters);
                }
                else if(_algorithm == NetworkLayoutAlgorithm.CompoundFDP)
                {
                    var layoutParameters = new CompoundFDPLayoutParameters();
                    layoutParameters.GravitationFactor = 0.8;
                    layoutParameters.IdealEdgeLength = 30;
                    layoutParameters.RepulsionConstant = 300;
                    layoutAlgorithm = layoutAlgorithms.CreateAlgorithm("CompoundFDP", layoutContext, layoutParameters);
                }
                else
                {
                    throw new InvalidOperationException("Unknown layout.");
                }

                // Compute.
                layoutAlgorithm.Compute();

                // Store Results.
                this.Positions
                    = layoutAlgorithm.VertexPositions.ToDictionary(
                        (kvp) => kvp.Key,
                        (kvp) => kvp.Value
                    );

                // Done.
                IterationCount += iterations;
            }
        }

        protected readonly int NetworkLayoutIterationChunkSize = 256;
        protected readonly int NetworkLayoutIterationLimit = 1;
        protected const float NetworkLayoutVertexSizeMinimized = 24.0f;
        protected const float NetworkLayoutVertexSizeNormal = 125.0f;

        protected NetworkLayout _networkLayout;
        protected NetworkLayoutRecord _networkLayoutInternal;
        protected Thread _networkLayoutThread;
        protected volatile bool _networkLayoutThreadCancel;

        public NetworkLayoutAlgorithm NetworkLayoutAlgorithm { get; set; }
        public INetworkLayout NetworkLayout
        {
            get
            {
                return _networkLayout;
            }
        }
        public event Action<IWorkbench> NetworkLayoutUpdated;

        protected void ThreadMainNetworkLayout()
        {
            while (true)
            {
                if (_networkLayoutThreadCancel)
                {
                    break;
                }

                bool didWork = false;

                // Grab a reference to the record for our operation.
                NetworkLayoutRecord record = this._networkLayoutInternal;
                var algorithm = record.AlgorithmState;

                // If there is work to do.
                if (record != null
                    && record.AlgorithmState.IterationCount < NetworkLayoutIterationLimit)
                {
                    // Mark as computing.
                    record.NetworkLayout.ComputationState = ComputationState.Computing;

                    // Compute.
                    algorithm.RefineLayout(NetworkLayoutIterationChunkSize);

                    // Publish results.
                    record.NetworkLayout.Positions = algorithm.Positions;

                    // Raise event for one large step finished.
                    if (NetworkLayoutUpdated != null)
                    {
                        NetworkLayoutUpdated(this);
                    }

                    // Update record step count.
                    didWork = true;
                }
                // We're finished. Mark done.
                else if (record.AlgorithmState.IterationCount == NetworkLayoutIterationLimit
                    && record.NetworkLayout.ComputationState != ComputationState.Done)
                {
                    record.NetworkLayout.ComputationState = ComputationState.Done;
                }

                if (_networkLayoutThreadCancel)
                {
                    break;
                }

                if (!didWork)
                {
                    Thread.Sleep(200);
                }
            }
        }
    }
}
