
using FAI.Bayesian;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Media;

namespace Bevisuali.Util
{
    static class Extensions
    {
        public static void ForAll<T>(this IEnumerable<T> source, Action<T> performer)
        {
            foreach (var s in source)
            {
                performer(s);
            }
        }

        public static IEnumerable<T> TakeFraction<T>(this IEnumerable<T> source, double fraction, int knownSourceCount = 0)
        {
            if (fraction < 0 || fraction > 1)
            {
                throw new ArgumentOutOfRangeException();
            }

            if (knownSourceCount == 0)
            {
                knownSourceCount = source.Count();
            }

            int amountToTake = (int)Math.Round(fraction * (double)knownSourceCount);
            return source.Take(amountToTake);
        }

        public static int? IndexOf<T>(this IEnumerable<T> source, T obj)
        {
            return source
                .Select((e, i) => new { e, i })
                .Where(p => p.e.Equals(obj))
                .Select(p => (int?)p.i)
                .FirstOrDefault();
        }

        public static int? IndexOf<T>(this IEnumerable<T> source, Func<T, bool> predicate)
        {
            return source
                .Select((e, i) => new { e, i })
                .Where(p => predicate(p.e))
                .Select(p => (int?)p.i)
                .FirstOrDefault();
        }

        public static Color GetColor(this Space space, double value)
        {
            Color[] colors = 
            {
                Colors.LightBlue,
                Colors.LightPink,
                Colors.LightGreen,
                Colors.PeachPuff,
                Color.FromRgb(0xFF, 0xAA, 0xFF),
                Color.FromRgb(0xFF, 0xEE, 0xAA),
            };

            var ds = space.Values;
            var index = ds.Select((v, i) => new { v, i }).Where(p => p.v == value).Select(p => p.i).First();
            return colors[index % colors.Length];
        }
    }
}
