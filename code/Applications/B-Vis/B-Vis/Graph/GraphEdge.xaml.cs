
using System;
using System.Diagnostics;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Media;

namespace Bevisuali.UX.Graph
{
    public partial class GraphEdge : UserControl
    {
        public enum StateEnum
        {
            Normal,
            Minimized,
        }

        public GraphEdge()
        {
            InitializeComponent();
        }

        private void UserControl_Initialized(object sender, EventArgs e)
        {
            this.State = StateEnum.Normal;
        }

        public StateEnum State
        {
            get
            {
                return _state;
            }
            set
            {
                _state = value;
                switch (value)
                {
                    case StateEnum.Normal:
                        xStateMinimized.Visibility = Visibility.Collapsed;
                        xStateNormal.Visibility = Visibility.Visible;
                        break;
                    case StateEnum.Minimized:
                        xStateNormal.Visibility = Visibility.Collapsed;
                        xStateMinimized.Visibility = Visibility.Visible;
                        break;
                }
            }
        }

        public double Length
        {
            get
            {
                return xLine.Y2;
            }
            set
            {
                xLine.Y2 = value;
                xLineMinimized.Y2 = value;
                this.Height = value;
            }
        }

        public GraphNode To
        {
            get
            {
                return _to;
            }
            set
            {
                if (value == null && _to != null)
                {
                    _to.LayoutUpdated -= UpdateEdgeDirection;
                }

                if (value != null && _to != value)
                {
                    value.LayoutUpdated += UpdateEdgeDirection;
                }

                _to = value;
            }
        }

        public GraphNode From
        {
            get
            {
                return _from;
            }
            set
            {
                if (value == null && _from != null)
                {
                    _from.LayoutUpdated -= UpdateEdgeDirection;
                }

                if (value != null && _from != value)
                {
                    value.LayoutUpdated += UpdateEdgeDirection;
                }

                _from = value;
            }
        }

        protected void UpdateEdgeDirection(object sender, EventArgs e)
        {
            // Parent coordinates.
            var to = _to.Position;
            var from = _from.Position;
            if (to.IsNaN() || from.IsNaN())
            {
                this.Visibility = System.Windows.Visibility.Collapsed;
                return;
            }

            var direction = to.Subtract(from);
            var length = direction.Magnitude();

            Debug.Assert(!double.IsNaN(length));

            if (length == 0)
            {
                this.Visibility = System.Windows.Visibility.Collapsed;
                return;
            }
            this.Visibility = System.Windows.Visibility.Visible;


            // Trim according to radii.
            var directionUnit = direction.Multiply(1.0 / length);
            var fromRadius = _from.Radius * _from.GetRenderScale().Item1;
            var toRadius = _to.Radius * _from.GetRenderScale().Item1;
            from = from.Add(directionUnit.Multiply(fromRadius));
            to = to.Subtract(directionUnit.Multiply(toRadius));
            length = Math.Max(length - fromRadius - toRadius, 0.0);

            // Resize this arrow.
            this.Length = length;

            // Position tip.
            Canvas.SetLeft(this, to.X);
            Canvas.SetTop(this, to.Y);

            // Rotate arrow.
            double angle = Math.Atan2(direction.Y, direction.X);
            RotateTransform transform = (RotateTransform)this.RenderTransform;
            transform.Angle = angle * 180.0 / (Math.PI) + 90;
        }


        private GraphNode _from;
        private GraphNode _to;
        private StateEnum _state;

    }
}
