
using System;
using System.Collections.Generic;
using System.Linq;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Media;

namespace Bevisuali.UX.Graph
{
    public partial class GraphNodeRing : UserControl
    {
        public GraphNodeRing()
        {
            InitializeComponent();

            LayoutUpdated += OnLayoutUpdated;
        }
        
        private void OnLayoutUpdated(object sender, EventArgs e)
        {
            xLabel.Margin =
                new Thickness(
                    0,
                    0,
                    0,
                    xInnerBorder.Height + xLabel.ActualHeight
                );
        }

        public new Brush Background
        {
            get
            {
                return xGrid.Background;
            }
            set
            {
                xGrid.Background = value;
            }
        }

        public Brush Stroke
        {
            get
            {
                return xOuterBorder.Stroke;
            }
            set
            {
                xOuterBorder.Stroke = value;
                xInnerBorder.Stroke = value;
            }
        }

        public double StrokeThickness
        {
            get
            {
                return xOuterBorder.StrokeThickness;
            }
            set
            {
                xOuterBorder.StrokeThickness = value;
                xInnerBorder.StrokeThickness = value;
            }
        }

        public bool StrokeOutsideEnabled
        {
            get
            {
                return xOuterBorder.Visibility == Visibility.Visible;
            }
            set
            {
                xOuterBorder.Visibility = value ? Visibility.Visible : Visibility.Collapsed;
            }
        }

        public bool StrokeInsideEnabled
        {
            get
            {
                return xInnerBorder.Visibility == Visibility.Visible;
            }
            set
            {
                xInnerBorder.Visibility = value ? Visibility.Visible : Visibility.Collapsed;
            }
        }

        public bool DashedStroke
        {
            get
            {
                return xOuterBorder.StrokeDashArray.Count != 0;
            }
            set
            {
                if (value)
                {
                    xOuterBorder.StrokeDashArray = new DoubleCollection();
                    xOuterBorder.StrokeDashArray.Add(4);
                    xOuterBorder.StrokeDashArray.Add(4);

                    xInnerBorder.StrokeDashArray = new DoubleCollection();
                    xInnerBorder.StrokeDashArray.Add(4);
                    xInnerBorder.StrokeDashArray.Add(4);
                }
                else
                {
                    xOuterBorder.StrokeDashArray = new DoubleCollection();
                    xInnerBorder.StrokeDashArray = new DoubleCollection();
                }
            }
        }

        public bool Arrows
        {
            get
            {
                return xSlices.EnableArrows;
            }
            set
            {
                xSlices.EnableArrows = value;
            }
        }

        public Color[] ColorSpace
        {
            get
            {
                return xSlices.ColorSpace;
            }
            set
            {
                xSlices.ColorSpace = value;
            }
        }

        public double[] SliceWeights
        {
            get
            {
                return xSlices.SliceWeights;
            }
            set
            {
                xSlices.SliceWeights = value;
            }
        }

        public string Label
        {
            get
            {
                return xLabel.Text;
            }
            set
            {
                xLabel.Text = value;
            }
        }

        public double InnerRadius
        {
            get
            {
                return xInnerHole.RadiusX;
            }
            set
            {
                xInnerHole.RadiusX = value;
                xInnerHole.RadiusY = value;
                xInnerBorder.Width = (value + xInnerBorder.StrokeThickness) * 2.0;
                xInnerBorder.Height = (value + xInnerBorder.StrokeThickness) * 2.0;
            }
        }

        public double OuterRadius
        {
            get
            {
                return xOuterBorder.Width * 0.5;
            }
            set
            {
                xOuterHole.RadiusX = value;
                xOuterHole.RadiusY = value;
                xOuterBorder.Width = value * 2.0;
                xOuterBorder.Height = value * 2.0;
                xSlices.Width = value * 2.0;
                xSlices.Height = value * 2.0;
            }
        }
    }
}
