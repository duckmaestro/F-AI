
using System;
using System.Diagnostics;
using System.Linq;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Media;

namespace Bevisuali.UX.General
{
    public partial class PieSlice : UserControl
    {
        public PieSlice()
        {
            InitializeComponent();
        }

        public Color Color
        {
            get
            {
                return ((SolidColorBrush)xSlice.Fill).Color;
            }
            set
            {
                xSlice.Fill = new SolidColorBrush(value);
            }
        }

        public double StartDegree
        {
            get
            {
                return _startDegree;
            }
            set
            {
                _startDegree = Math.Max(value, 0.0);
                RefreshUIAngles();
            }
        }

        public double EndDegree
        {
            get
            {
                return _endDegree;
            }
            set
            {
                _endDegree = Math.Min(value, 360.0);
                RefreshUIAngles();
            }
        }

        public UIElement ChildContent
        {
            get
            {
                return xChildContainer.Children.OfType<UIElement>().FirstOrDefault();
            }
            set
            {
                xChildContainer.Children.Clear();
                if (value != null)
                {
                    xChildContainer.Children.Add(value);
                }
            }
        }

        private void RefreshUIAngles()
        {
            if (double.IsNaN(_startDegree)
                || double.IsNaN(_endDegree))
            {
                return;
            }

            Debug.Assert(_startDegree >= 0 && _startDegree <= 360.0);
            Debug.Assert(_endDegree >= 0 && _endDegree <= 360.0);
            Debug.Assert(_startDegree <= _endDegree);

            xRotation.Angle = _startDegree;

            double arcAngle = _endDegree - _startDegree;

            double arcAngleRadians = arcAngle * Math.PI / 180.0;


            double arcAngleRemapped = (arcAngleRadians - Math.PI * 0.5) * -1.0;
            double endX = Math.Cos(arcAngleRemapped) * 50 + 50;
            double endY = Math.Sin(arcAngleRemapped) * -50 + 50;

            xArc.Point = new Point(endX, endY);

            if (arcAngle >= 180.0)
            {
                xArc.IsLargeArc = true;
            }
            else
            {
                xArc.IsLargeArc = false;
            }

            // Update child content placement.
            {
                double angleMidpoint = (_startDegree + _endDegree) * 0.5;
                xChildRotation.Angle = angleMidpoint;
            }
        }

        private double _startDegree = double.NaN;
        private double _endDegree = double.NaN;
    }
}
