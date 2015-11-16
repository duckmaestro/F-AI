
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Windows;
using System.Windows.Media;
using System.Windows.Threading;

namespace Bevisuali.UX
{
    static class ExtWPF
    {
        #region Point/Vector/Size
        public static Point Add(this Point a, Point b)
        {
            return new Point(a.X + b.X, a.Y + b.Y);
        }

        public static Point Multiply(this Point a, double b)
        {
            return new Point(a.X * b, a.Y * b);
        }

        public static Point Subtract(this Point a, Point b)
        {
            return new Point(a.X - b.X, a.Y - b.Y);
        }

        public static Point Normalized(this Point a)
        {
            return a.Multiply(1.0 / a.Magnitude());
        }

        public static Point Positive(this Point a)
        {
            return new Point(Math.Abs(a.X), Math.Abs(a.Y));
        }

        public static double Magnitude(this Point a)
        {
            return Math.Sqrt(a.X * a.X + a.Y * a.Y);
        }

        public static bool IsNaN(this Point a)
        {
            return double.IsNaN(a.X) || double.IsNaN(a.Y);
        }
        #endregion

        #region Misc
        public static void BeginInvoke(this Dispatcher dispatcher, Action action)
        {
            Delegate actionAsDelegate = action as Delegate;
            dispatcher.BeginInvoke(actionAsDelegate);
        }
        public static void Invoke(this Dispatcher dispatcher, Action action)
        {
            Delegate actionAsDelegate = action as Delegate;
            dispatcher.Invoke(actionAsDelegate);
        }
        #endregion

        #region Transforms
        public static Tuple<double, double> GetRenderScale(this FrameworkElement source)
        {
            ScaleTransform t = source.RenderTransform as ScaleTransform;
            if (t == null)
            {
                return new Tuple<double, double>(1, 1);
            }

            return new Tuple<double, double>(t.ScaleX, t.ScaleY);
        }
        #endregion

    }
}
