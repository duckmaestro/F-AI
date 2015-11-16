
using Bevisuali.Util;
using FAI.Bayesian;
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Input;
using System.Windows.Media;
using FRandomVariable = FAI.Bayesian.RandomVariable;

namespace Bevisuali.UX.Graph
{
    public partial class GraphInspector : UserControl
    {
        const int LayerUnimportantEdges = 0;
        const int LayerUnimportantNodes = 1;
        const int LayerSelectedNodes = 2;
        const int LayerSelectedNodesEdges = 3;
        const double NodeScale = 1.4f;

        public GraphInspector()
        {
            InitializeComponent();

            this._network = null;
            this._nodes = new List<GraphNode>();
            this._edges = new List<GraphEdge>();
        }

        private void xRoot_MouseUp(object sender, MouseButtonEventArgs e)
        {
            App.Current.MainWindow.RequestSelectVariable(null);
        }

        /// <summary>
        /// Assigns a network to this graph inspector. The graph inspector uses
        /// this network's local probability descriptions and structure. Layout
        /// must be updated separately.
        /// </summary>
        /// <param name="network"></param>
        public void SetGraph(BayesianNetwork network, IDictionary<string, string> variableAbbreviations)
        {
            bool isNewNetwork = _network != network;

            // Remove nodes and edges.
            foreach (var node in _nodes.ToList())
            {
                RemoveNode(node);
            }
            _nodes.Clear();

            // Remember.
            _network = network;

            if (network == null)
            {
                return;
            }

            // Generate new nodes.
            foreach (var variable in network.VariablesOrdered)
            {
                GraphNode node = new GraphNode();
                node.Label = variableAbbreviations[variable.Name];
                node.Tag = variable;
                node.ColorSpace = variable.Space.Values.Select(v => variable.Space.GetColor(v)).ToArray();
                node.RenderTransform = new ScaleTransform(NodeScale, NodeScale);

                var cpt = variable.Distributions;
                AddNode(node);
            }

            UpdateEdges();

            network.StructureChanged += OnBayesianNetworkStructureChanged;
        }

        /// <summary>
        /// Configures the graph to emphasize the nodes listed.
        /// </summary>
        public void SetInterestVariables(IEnumerable<string> variableNames)
        {
            if (variableNames == null)
            {
                // Reset.

                foreach (var node in _nodes)
                {
                    if (node.State == GraphNode.StateEnum.Minimized)
                    {
                        node.State = GraphNode.StateEnum.Idling;
                    }
                }

                foreach (var edge in _edges)
                {
                    edge.State = GraphEdge.StateEnum.Normal;
                }

                _interestVariables = null;
            }
            else
            {
                // Minimize non-interest nodes. Reset interest nodes.
                List<GraphNode> nodesNotMinimized = new List<GraphNode>();
                foreach (var node in _nodes)
                {
                    FRandomVariable rv = (FRandomVariable)node.Tag;
                    if (variableNames.Contains(rv.Name))
                    {
                        nodesNotMinimized.Add(node);
                        if (node.State == GraphNode.StateEnum.Minimized)
                        {
                            node.State = GraphNode.StateEnum.Idling;
                        }
                    }
                    else
                    {
                        if (node.State != GraphNode.StateEnum.Minimized
                            && node.State == GraphNode.StateEnum.Idling)
                        {
                            node.State = GraphNode.StateEnum.Minimized;

                            _edges
                            .Where(e => e.To == node || e.From == node)
                            .ForAll(n => n.State = GraphEdge.StateEnum.Minimized);
                        }
                    }
                }

                // Ensure edges that are between non-minimized nodes are set
                // to normal state.
                foreach (var edge in _edges)
                {
                    if (nodesNotMinimized.Contains(edge.To)
                        && nodesNotMinimized.Contains(edge.From))
                    {
                        edge.State = GraphEdge.StateEnum.Normal;
                    }
                }

                // Store.
                _interestVariables = variableNames.ToList();
            }
        }

        /// <summary>
        /// Provides a graph layout for this inspector to use.
        /// </summary>
        /// <param name="layout"></param>
        public void SetGraphLayout(IDictionary<string, Point> layout)
        {
            // Shift layout into view.
            Point vertexShift;
            Size canvasSize;
            if (layout.Any())
            {
                double minX = double.MaxValue;
                double minY = double.MaxValue;
                double maxX = double.MinValue;
                double maxY = double.MinValue;
                foreach (var point in layout.Values)
                {
                    minX = Math.Min(point.X, minX);
                    minY = Math.Min(point.Y, minY);
                    maxX = Math.Max(point.X, maxX);
                    maxY = Math.Max(point.Y, maxY);
                }

                vertexShift = new Point(minX, minY).Multiply(-1);
                canvasSize = new Size(maxX - minX, maxY - minY);
            }
            else
            {
                vertexShift = new Point();
                canvasSize = new Size();
            }

            // Pad position and canvas size.
            const float Padding = 300;
            vertexShift = vertexShift.Add(new Point(Padding * 0.5, Padding * 0.5));
            canvasSize.Height += Padding;
            canvasSize.Width += Padding;

            // Update positions.
            foreach (var node in _nodes)
            {
                FRandomVariable variable = node.Tag as FRandomVariable;
                Point position;
                if (layout.TryGetValue(variable.Name, out position))
                {
                    node.Position = position.Add(vertexShift);
                }
                else
                {
                    node.Position = new Point();
                }
            }

            // Update canvas.
            xRoot.Width = canvasSize.Width * xRootScaleTransform.ScaleX;
            xRoot.Height = canvasSize.Height * xRootScaleTransform.ScaleY;
        }

        public void SetZoom(double zoom)
        {
            if (zoom <= 0.00)
            {
                throw new ArgumentOutOfRangeException("zoom");
            }

            double oldZoom = xRootScaleTransform.ScaleX;
            //double oldScrollX = xScrollViewer.HorizontalOffset / xScrollViewer.ScrollableWidth;
            //double oldScrollY = xScrollViewer.VerticalOffset / xScrollViewer.ScrollableHeight;

            xRootScaleTransform.ScaleX = zoom;
            xRootScaleTransform.ScaleY = zoom;

            xRoot.Width = xRoot.Width * zoom / oldZoom;
            xRoot.Height = xRoot.Height * zoom / oldZoom;

            // TODO: maintain center point of view.
        }

        public void SetInferenceResults(IDictionary<string, DiscreteDistribution> results, int scenarioId, FAI.Bayesian.Observation evidence)
        {
            if (scenarioId <= 0)
            {
                throw new ArgumentOutOfRangeException("ScenarioId");
            }

            if (results != null && results.Count != 0)
            {
                foreach (var node in _nodes)
                {
                    FRandomVariable variable = (FRandomVariable)node.Tag;
                    DiscreteDistribution distribution;

                    // See if this variable has a result in the incoming results.
                    if (results.TryGetValue(variable.Name, out distribution))
                    {
                        bool isEvidence = evidence.TryValueForVariable(variable.Name) != null;

                        var slices
                            = distribution
                            .Masses
                            .OrderBy(p => p.Key)
                            .Select(p => p.Value)
                            .ToArray();

                        node.SetSlices(slices, scenarioId, isEvidence);
                    }
                }
            }
            else
            {
                foreach (var node in _nodes)
                {
                    FRandomVariable variable = (FRandomVariable)node.Tag;
                    bool isEvidence
                        = evidence != null
                        && evidence.TryValueForVariable(variable.Name) != null;
                    node.SetSlices(new double[0], scenarioId, isEvidence);
                }
            }
        }

        public void SetSelectedVariable(string variableName)
        {
            foreach (var edge in _edges)
            {
                edge.Opacity = 1.0;
                edge.SetValue(Canvas.ZIndexProperty, LayerUnimportantEdges);
            }

            foreach (var node in _nodes)
            {
                node.Opacity = 1.0;

                RandomVariable nodeRV = (RandomVariable)node.Tag;
                if (nodeRV.Name == variableName)
                {
                    node.State = GraphNode.StateEnum.Selecting;
                    node.SetValue(Canvas.ZIndexProperty, LayerSelectedNodes);

                    foreach (var edge in
                        _edges.Where(e => e.To == node || e.From == node))
                    {
                        edge.SetValue(Canvas.ZIndexProperty, LayerSelectedNodesEdges);
                    }
                }
                else
                {
                    node.SetValue(Canvas.ZIndexProperty, LayerUnimportantNodes);
                    if (_interestVariables != null)
                    {
                        if (_interestVariables.Contains(nodeRV.Name))
                        {

                            node.State = GraphNode.StateEnum.Idling;
                        }
                        else
                        {
                            node.State = GraphNode.StateEnum.Minimized;
                        }
                    }
                    else
                    {
                        node.State = GraphNode.StateEnum.Idling;
                    }
                }
            }

            _selectedVariableName = variableName;
        }

        public void SetConfiguringVariable(string variableName)
        {
            foreach (var node in _nodes)
            {
                RandomVariable nodeRV = (RandomVariable)node.Tag;
                if (nodeRV.Name == variableName)
                {
                    node.State = GraphNode.StateEnum.Configuring;
                    node.SetValue(Canvas.ZIndexProperty, LayerSelectedNodes);
                    node.Opacity = 1.0;
                }
                else
                {
                    if (node.State != GraphNode.StateEnum.Minimized)
                    {
                        node.State = GraphNode.StateEnum.Idling;
                        node.SetValue(Canvas.ZIndexProperty, LayerUnimportantNodes);
                        node.Opacity = 0.2;
                    }
                }
            }

            foreach (var edge in _edges)
            {
                edge.Opacity = 0.2;
            }
        }

        void OnBayesianNetworkStructureChanged(object sender, BayesianNetwork args)
        {
            Dispatcher.Invoke(delegate
            {
                // If structure changed, random variable instances were shed.
                // Find the latest instances.
                foreach (GraphNode node in _nodes)
                {
                    RandomVariable oldVariable = (RandomVariable)node.Tag;
                    if (_network.HasVariable(oldVariable.Name))
                    {
                        RandomVariable newVariable = _network.GetVariable(oldVariable.Name);
                        node.Tag = newVariable;
                    }
                }

                // Update edges.
                UpdateEdges();
            });
        }


        private void AddNode(GraphNode graphNode)
        {
            // Add to visual tree.
            xRoot.Children.Add(graphNode);
            graphNode.SetValue(Canvas.ZIndexProperty, LayerUnimportantNodes);

            // Add to internal list.
            _nodes.Add(graphNode);

            RandomVariable variable = (RandomVariable)graphNode.Tag;
            variable.UserData = graphNode;

            // Ensure canvas is large enough.
            {
                const double padding = 100;
                double maxX = _nodes.Max(n => n.Position.X) + padding;
                double maxY = _nodes.Max(n => n.Position.Y) + padding;
                xRoot.Width = Math.Max(xRoot.Width, maxX);
                xRoot.Height = Math.Max(xRoot.Height, maxY);
            }

            // Events.
            graphNode.MouseUp += delegate(object sender, MouseButtonEventArgs e)
            {
                if (graphNode.State == GraphNode.StateEnum.Selecting)
                {
                    App.Current.MainWindow.RequestConfigureVariable(variable);
                }
                else
                {
                    App.Current.MainWindow.RequestSelectVariable(variable);
                }

                e.Handled = true;
            };
            graphNode.SliceChosen += delegate(int sliceIndex, int scenarioIndex)
            {
                Debug.Assert(scenarioIndex == 1 || scenarioIndex == 2);

                if (sliceIndex == -1)
                {
                    App.Current.MainWindow.RequestConfigureVariableWithEvidence(variable, scenarioIndex, null);
                }
                else
                {
                    float evidenceValue = variable.Space.Values.ElementAt(sliceIndex);
                    App.Current.MainWindow.RequestConfigureVariableWithEvidence(variable, scenarioIndex, evidenceValue);
                }
            };
        }

        private void RemoveNode(GraphNode node)
        {
            RandomVariable variable = node.Tag as RandomVariable;
            variable.UserData = null;

            xRoot.Children.Remove(node);
            _nodes.Remove(node);

            var edgesWithNode = _edges.Where(e => e.To == node || e.From == node);
            foreach (var edge in edgesWithNode)
            {
                if (edge.To.Parent == null
                    && edge.From.Parent == null)
                {
                    xRoot.Children.Remove(edge);
                    edge.To = null;
                    edge.From = null;
                }
            }
        }

        private void UpdateEdges(GraphNode nodeOfInterest = null)
        {
            // Clear edges.
            _edges.ForAll(edge =>
            {
                xRoot.Children.Remove(edge);
            });
            _edges.Clear();

            // For each node already in this inspector.
            foreach (var graphNode in this._nodes)
            {
                RandomVariable variable = (RandomVariable)graphNode.Tag;

                // Check if this node connects to another node already in the graph.
                foreach (var parentVariable in variable.Parents.Select(p => _network.GetVariable(p)))
                {
                    var parentNode = parentVariable.UserData as GraphNode;
                    Debug.Assert(parentNode != null);

                    // Create edge.
                    GraphEdge edge = new GraphEdge();
                    xRoot.Children.Add(edge);
                    _edges.Add(edge);
                    edge.From = parentNode;
                    edge.To = graphNode;
                    edge.SetValue(Canvas.ZIndexProperty, LayerUnimportantEdges);
                }
            }
        }


        BayesianNetwork _network;
        List<GraphNode> _nodes;
        List<GraphEdge> _edges;
        List<string> _interestVariables;
        string _selectedVariableName;
    }
}
