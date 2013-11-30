
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Input;
using System.Windows.Media;

namespace Bevisuali.UX.General
{
    public partial class Pie : UserControl
    {
        public bool AllowDrag { get; set; }

        public bool EnableArrows
        {
            get
            {
                return _enableArrows;
            }
            set
            {
                _enableArrows = true;
                RefreshUI();
            }
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
            }
        }

        public Pie()
        {
            InitializeComponent();
        }

        public double[] SliceWeights
        {
            get
            {
                return _sliceWeights;
            }
            set
            {
                if (value == null)
                {
                    throw new ArgumentNullException();
                }

#if DEBUG
                if (value.Any(x => x < 0))
                {
                    Debug.Fail("Weights must be non-negative.");
                }
                if (value.Length > 0)
                {
                    double sum = value.Sum();
                    if (!Util.Utils.ApproximatelyEqual(sum, 1.0))
                    {
                        Debug.Fail("Weights must sum to 1.");
                    }
                }
#endif

                _sliceWeights = value;



                RefreshUI();
            }
        }

        private void RefreshUI()
        {
            foreach (PieSlice sliceControl in xSlices.Children.OfType<PieSlice>())
            {
                sliceControl.MouseMove -= OnSliceMouseMove;
            }

            xSlices.Children.Clear();

            double numFractions = _sliceWeights.Length;
            double acc = 0.0;

            for (int i = 0; i < _sliceWeights.Length; ++i)
            {
                double sliceWeight = _sliceWeights[i];
                Color sliceColor = _colorSpace[i];

                PieSlice sliceControl = new PieSlice();
                sliceControl.StartDegree = acc;
                sliceControl.EndDegree = Math.Min(acc + sliceWeight * 360.0, 359.9);
                sliceControl.Color = sliceColor;
                sliceControl.MouseMove += OnSliceMouseMove;
                sliceControl.MouseDown += OnSliceMouseDown;
                sliceControl.MouseLeave += OnSliceMouseLeave;
                sliceControl.MouseUp += OnSliceMouseUp;

                xSlices.Children.Add(sliceControl);

                acc = sliceControl.EndDegree;
            }


            foreach (PieSlice slice in this.xSlices.Children)
            {
                if (_enableArrows && slice.ChildContent == null)
                {
                    Arrow arrow = new Arrow();
                    arrow.Width = 5;
                    arrow.Height = 5;
                    arrow.RenderTransform = new RotateTransform(180);
                    arrow.RenderTransformOrigin = new Point(0.5, 0.5);
                    arrow.HorizontalAlignment = HorizontalAlignment.Center;
                    arrow.VerticalAlignment = VerticalAlignment.Center;
                    arrow.Margin = new Thickness(0, -2, 0, 0);
                    arrow.IsHitTestVisible = false;
                    slice.ChildContent = arrow;
                }
                else if (!_enableArrows && slice.ChildContent != null)
                {
                    slice.ChildContent = null;
                }
            }
        }

        private void OnSliceMouseUp(object sender, MouseButtonEventArgs e)
        {
            _mouseDownInside = false;
        }

        private void OnSliceMouseLeave(object sender, MouseEventArgs e)
        {
            _mouseDownInside = false;
        }

        private void OnSliceMouseDown(object sender, MouseButtonEventArgs e)
        {
            _mouseDownInside = true;
        }

        private void OnSliceMouseMove(object sender, MouseEventArgs e)
        {
            if (!_mouseDownInside)
            {
                return;
            }
            if (!AllowDrag)
            {
                return;
            }

            PieSlice sliceControl = (PieSlice)e.Source;
            int sliceIndex = xSlices.Children.IndexOf(sliceControl);
            BeginDrag(sliceIndex);
        }

        private void BeginDrag(int sliceIndex)
        {
            DragDrop.DoDragDrop(
                this,
                sliceIndex,
                DragDropEffects.Copy);
        }


        private Color[] _colorSpace = new Color[0];
        private double[] _sliceWeights = new double[0];
        private bool _mouseDownInside;
        private bool _enableArrows;


    }
}
