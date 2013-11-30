
using System.Windows.Controls;
using System.Windows.Media;

namespace Bevisuali.UX
{
    /// <summary>
    /// Ensures the grid is always centered at (0,0) in its
    /// parent, by apply a render transform once this grid's
    /// size is known.
    /// </summary>
    public class CenteredGrid : Grid
    {
        protected override void OnRender(System.Windows.Media.DrawingContext drawingContext)
        {
            TranslateTransform translation = this.RenderTransform as TranslateTransform;
            if( translation == null)
            {
                translation = new TranslateTransform();
                this.RenderTransform = translation;
            }

            double width = this.Width;
            double height = this.Height;

            if (double.IsNaN(width))
            {
                width = this.ActualWidth;
            }
            if (double.IsNaN(height))
            {
                height = this.ActualHeight;
            }

            translation.X = -width * 0.5;
            translation.Y = -height * 0.5;

            base.OnRender(drawingContext);
        }
    }
}
