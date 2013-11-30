
using Bevisuali.Util;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Input;
using System.Windows.Media;

namespace Bevisuali.UX.Graph
{
    public partial class GraphNode : UserControl
    {
        public enum StateEnum
        {
            Idling,
            Selecting,
            Configuring,
            Minimized,
        }

        public delegate void SliceChosenHandler(int sliceIndex, int targetLevel);

        public event SliceChosenHandler SliceChosen;

        public GraphNode()
        {
            InitializeComponent();

            this.Loaded += OnLoaded;

            this.xCircleGroup.MouseDown += OnCircleGroupMouseDown;
            this.xCircleGroup.MouseUp += OnCircleGroupMouseUp;
            this.xCircleGroup.Drop += OnCircleGroupDrop;
            this.xCircleGroup.MouseMove += OnCircleGroupMouseMove;
            this.xCircleGroup.MouseLeave += OnCircleGroupMouseLeave;

            this.xChoiceRing.Drop += OnChoiceRingDrop;
            this.xScenarioRing2.Drop += OnScenarioRing2Drop;
        }

        public Color[] ColorSpace
        {
            get
            {
                return _colorSpace;
            }
            set
            {
                if (value == null)
                {
                    throw new ArgumentNullException();
                }
                _colorSpace = value;

                xScenarioRing2.ColorSpace = value;
                xSlices.ColorSpace = value;

                xChoiceRing.ColorSpace = value;
                xChoiceRing.SliceWeights
                    = value.Select(x => 1.0 / value.Length).ToArray();
            }
        }

        public void SetSlices(double[] slices, int scenarioNumber, bool isEvidence)
        {
            if (slices == null)
            {
                throw new ArgumentNullException("Slices");
            }

            // Record if evidence or not.
            _scenarioIsEvidence[scenarioNumber] = isEvidence;


            //
            // Posterior slices.
            //

            // Scenario 1.
            if (scenarioNumber == 1)
            {
                xSlices.SliceWeights = slices;
            }
            // Scenario 2.
            else if (scenarioNumber == 2)
            {
                // If we're configuring, configuration already owns the decision
                // on scenario 2 ring visibility.
                if (this.State != StateEnum.Configuring 
                    && this.State != StateEnum.Minimized)
                {
                    if (slices.Length != 0)
                    {
                        xScenarioRing2.Visibility 
                            = Visibility.Visible;
                    }
                    else
                    {
                        xScenarioRing2.Visibility 
                            = Visibility.Collapsed;
                    }
                }

                xScenarioRing2.SliceWeights = slices;
            }


            // Update evidence display.
            xInnerShadow.StrokeThickness = _scenarioIsEvidence.ContainsKey(1) && _scenarioIsEvidence[1] ? 2.0 : 0.0;
            xScenarioRing2.StrokeOutsideEnabled = _scenarioIsEvidence.ContainsKey(2) && _scenarioIsEvidence[2];
            xScenarioRing2.StrokeInsideEnabled = false;

            bool anyEvidence = _scenarioIsEvidence.ContainsValue(true);

            _hasEvidence = anyEvidence;
        }

        /// <summary>
        /// Returns the current radius of the node.
        /// </summary>
        public double Radius
        {
            get
            {
                if (xScenarioRing2.Visibility == Visibility.Visible)
                {
                    return xScenarioRing2.OuterRadius;
                }
                else if (xCircleGroupSmallMode.Visibility == Visibility.Visible)
                {
                    return xCircleGroupSmallMode.ActualWidth * 0.5;
                }
                else
                {
                    return xCircleGroup.ActualWidth * 0.5 - 1.0;
                }
            }
        }

        /// <summary>
        /// Specifices whether a 'selected' visual state is on or off.
        /// </summary>
        public StateEnum State
        {
            get
            {
                return _state;
            }
            set
            {
                switch (value)
                {
                    case StateEnum.Idling:
                        xCircleGroup.Visibility = Visibility.Visible;
                        xChoiceRing.Visibility = Visibility.Collapsed;
                        xSelectionIndicator.Visibility = Visibility.Collapsed;
                        xOuterShadow.Visibility = Visibility.Visible;
                        xCircleGroupSmallMode.Visibility = Visibility.Collapsed;

                        if (xScenarioRing2.SliceWeights.Length == 0)
                        {
                            xScenarioRing2.Visibility = Visibility.Collapsed;
                        }
                        else
                        {
                            xScenarioRing2.Visibility = Visibility.Visible;
                            xScenarioRing2.Label = "";
                        }

                        break;

                    case StateEnum.Selecting:
                        xCircleGroup.Visibility = Visibility.Visible;
                        xChoiceRing.Visibility = Visibility.Collapsed;
                        xSelectionIndicator.Visibility = Visibility.Visible;
                        xSelectionIndicator.Width = 114;
                        xSelectionIndicator.Height = 114;
                        xOuterShadow.Visibility = Visibility.Collapsed;
                        xCircleGroupSmallMode.Visibility = Visibility.Collapsed;

                        if (xScenarioRing2.SliceWeights.Length == 0)
                        {
                            xScenarioRing2.Visibility = Visibility.Collapsed;
                        }
                        else
                        {
                            xScenarioRing2.Visibility = Visibility.Visible;
                            xScenarioRing2.Label = "";
                        }
                        break;

                    case StateEnum.Configuring:
                        xCircleGroup.Visibility = Visibility.Visible;
                        xChoiceRing.Visibility = Visibility.Visible;
                        xSelectionIndicator.Visibility = Visibility.Collapsed;
                        xOuterShadow.Visibility = Visibility.Collapsed;
                        xCircleGroupSmallMode.Visibility = Visibility.Collapsed;

                        xScenarioRing2.Visibility = Visibility.Visible;
                        xScenarioRing2.Background = new SolidColorBrush(Colors.LightGray);
                        break;

                    case StateEnum.Minimized:
                        xChoiceRing.Visibility = Visibility.Collapsed;
                        xSelectionIndicator.Visibility = Visibility.Collapsed;
                        xOuterShadow.Visibility = Visibility.Collapsed;
                        xScenarioRing2.Visibility = Visibility.Collapsed;
                        xCircleGroup.Visibility = Visibility.Collapsed;

                        xCircleGroupSmallMode.Visibility = Visibility.Visible;

                        break;
                }

                _state = value;
            }
        }

        /// <summary>
        /// Positions the center of this node within relative to its parent
        /// canvas.
        /// </summary>
        public Point Position
        {
            get
            {
                double left = Canvas.GetLeft(this);
                double top = Canvas.GetTop(this);
                return new Point(left, top);
            }
            set
            {
                Canvas.SetLeft(this, value.X);
                Canvas.SetTop(this, value.Y);
            }
        }

        /// <summary>
        /// The display name for this name.
        /// </summary>
        public string Label
        {
            get
            {
                return _label;
            }
            set
            {
                _label = value;

                char letter;
                string subscript;
                Utils.ParseVariableName(value, out letter, out subscript);
                xLabel.Text = letter.ToString();
                xLabelSubscript.Text = subscript;
            }
        }
       

        private void OnLoaded(object sender, RoutedEventArgs e)
        {
            this.State = StateEnum.Idling;
        }

        private void OnScenarioRing2Drop(object sender, DragEventArgs e)
        {
            if (e.Data.GetDataPresent(typeof(int)))
            {
                int sliceIndex = (int)e.Data.GetData(typeof(int));
                if (SliceChosen != null)
                {
                    SliceChosen(sliceIndex, 2);
                }
            }
        }

        private void OnCircleGroupMouseLeave(object sender, MouseEventArgs e)
        {
            _mouseDownInside = false;
        }

        private void OnCircleGroupMouseUp(object sender, MouseButtonEventArgs e)
        {
            _mouseDownInside = false;
        }

        private void OnCircleGroupMouseDown(object sender, MouseButtonEventArgs e)
        {
            _mouseDownInside = true;
        }

        private void OnChoiceRingDrop(object sender, DragEventArgs e)
        {
            if (this.State != StateEnum.Configuring)
            {
                return;
            }

            if (!e.Data.GetDataPresent(typeof(Object)))
            {
                return;
            }

            if (e.Data.GetData(typeof(Object)) != DropTagClearEvidence)
            {
                return;
            }

            if (SliceChosen != null)
            {
                SliceChosen(-1, 1);
            }
        }

        private void OnCircleGroupMouseMove(object sender, MouseEventArgs e)
        {
            if (!_mouseDownInside)
            {
                return;
            }
            if (this.State != StateEnum.Configuring)
            {
                return;
            }

            DragDrop.DoDragDrop(
                this,
                DropTagClearEvidence,
                DragDropEffects.Move);
        }

        private void OnCircleGroupDrop(object sender, DragEventArgs e)
        {
            if (e.Data.GetDataPresent(typeof(int)))
            {
                int sliceIndex = (int)e.Data.GetData(typeof(int));
                if (SliceChosen != null)
                {
                    SliceChosen(sliceIndex, 1);
                }
            }
        }


        private readonly object DropTagClearEvidence = new object();
        
        private StateEnum _state;
        private bool _hasEvidence;
        private string _label;
        private bool _mouseDownInside;
        private Dictionary<int, bool> _scenarioIsEvidence = new Dictionary<int, bool>();

        private Color[] _colorSpace = new Color[0];
    }
}
