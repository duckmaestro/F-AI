
using Microsoft.FSharp.Core;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;

namespace Bevisuali.Util
{
    public static partial class Utils
    {
        public static void ParseVariableName(string input, out char letter, out string subscript)
        {
            if (string.IsNullOrWhiteSpace(input))
            {
                throw new ArgumentException("Input cannot be empty.");
            }
            // Name_Number style
            else if (input.Contains('_'))
            {
                string[] split = input.Split('_');
                if (split.Length == 2)
                {
                    letter = split[0][0];
                    subscript = split[1].ToString();
                }
                else if (split.Length == 1)
                {
                    letter = split[0][0];
                    subscript = "";
                }
                else
                {
                    throw new ArgumentException();
                }
            }
            // 'NameNumber' style
            else if (Regex.IsMatch(input, @"^[a-zA-Z][0-9]+$"))
            {
                letter = input[0];
                subscript = Regex.Match(input, @"([0-9]+)").Captures[0].Value;
            }
            else if(input.Length == 1)
            {
                letter = input[0];
                subscript = "";
            }
            else
            {
                throw new ArgumentException();
            }

            letter = Char.ToUpper(letter);
        }

        public static T Try<T>(Func<T> tryThis, T defaultValue)
        {
            try
            {
                return tryThis();
            }
            catch
            {
                return defaultValue;
            }
        }

        public static FSharpOption<T> None<T>()
        {
            return FSharpOption<T>.None;
        }

        public static FSharpOption<T> Some<T>(T obj)
        {
            return FSharpOption<T>.Some(obj);
        }

        public static bool ApproximatelyEqual(double x, double y)
        {
            double v = x - y;
            if (v < 0.0001 && v > -0.0001)
            {
                return true;
            }
            else
            {
                return false;
            }
        }
    }
}
