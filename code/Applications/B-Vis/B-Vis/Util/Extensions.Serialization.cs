
using Bevisuali.Util;
using FAI.Bayesian;
using Microsoft.FSharp.Collections;
using Microsoft.FSharp.Core;
using Newtonsoft.Json.Linq;
using System;
using System.Collections.Generic;
using System.Linq;
using FObservation = FAI.Bayesian.Observation;

namespace Bevisuali.UX
{
    public static class ExtSerialization
    {
        public static BayesianNetwork ToBayesianNetwork(this JObject json)
        {
            string networkName = json["name"].Value<string>();

            var bn = new BayesianNetwork(networkName);

            // Load variables and note parents.
            var variableList = new List<string>();
            var parentsList = new Dictionary<string, string[]>();
            foreach (var jrv in json.Property("variables").Value)
            {
                var rv = ToRandomVariable(jrv);
                bn.AddVariable(rv);

                variableList.Add(rv.Name);
                parentsList.Add(rv.Name, jrv["parents"].Values<string>().ToArray());
            }

            // Reparent.
            foreach (var rv in variableList)
            {
                string[] parents = parentsList[rv];
                foreach (var parentName in parents)
                {
                    bn.ConnectVariables(parentName, rv);
                }
            }

            return bn;
        }

        public static RandomVariable ToRandomVariable(this JToken json0)
        {
            JObject json = (JObject)json0;
            return new RandomVariable(
                json["name"].Value<string>(),
                json["space"].ToRandomVariableSpace(),
                json["distributionSet"].ToDistributionSet());
        }

        public static Space ToRandomVariableSpace(this JToken json0)
        {
            JObject json = (JObject)json0;
            if (json.Property("discrete") != null)
            {
                float[] values = json.Property("discrete").Value.Values<float>().ToArray();
                string[] labels =
                    Utils.Try(() => json.Property("labels").Value.Values<string>().ToArray(), null);
                var zipped = 
                    labels != null 
                    ? values.Zip(labels, (v,l) => new Tuple<float, string>(v, l))
                    : values.Select(v => new Tuple<float, string>(v, ""));
                return Space.NewDiscrete(new FSharpMap<float, string>(zipped));
            }
            else
            {
                throw new Exception();
            }
        }

        public static DistributionSet ToDistributionSet(this JToken json0)
        {
            JArray json = (JArray)json0;
            List<Tuple<FObservation, DiscreteDistribution>> distributions 
                = new List<Tuple<FObservation, DiscreteDistribution>>();

            foreach (var d_j in json.Values<JObject>())
            {
                FObservation o = d_j["key"].ToObservation();
                DiscreteDistribution d = d_j["distribution"].ToDiscreteDistribution();
                distributions.Add(new Tuple<FObservation, DiscreteDistribution>(o, d));
            }

            DistributionSet set = new DistributionSet(distributions);
            return set;
        }

        public static DiscreteDistribution ToDiscreteDistribution(this JToken json0)
        {
            JArray json = (JArray)json0;
            List<Tuple<float, double>> masses = new List<Tuple<float, double>>();

            foreach (JObject j in json)
            {
                masses.Add(new Tuple<float, double>(
                    j.Property("value").Value.Value<float>(),
                    j.Property("mass").Value.Value<double>()));
            }

            DiscreteDistribution distribution = new DiscreteDistribution(masses);
            return distribution;
        }

        public static FObservation ToObservation(this JToken json0)
        {
            JObject json = (JObject)json0;
            return new FObservation(
                new FSharpMap<string, float>(
                    json
                    .Properties()
                    .Select(
                        p => new Tuple<string, float>(p.Name, p.Value.Value<float>())
                    )
                )
            );
        }

        public static JObject ToJObject(this BayesianNetwork bn)
        {
            return new JObject(
                new JProperty("name", bn.Name),
                new JProperty("variables",
                    new JArray(bn.VariablesOrdered.Select(v => v.ToJObject()))
                )
            );
        }

        public static JObject ToJObject(this RandomVariable rv)
        {
            return new JObject(
                new JProperty("name", rv.Name),
                new JProperty("space", rv.Space.ToJObject()),
                new JProperty("distributionSet", rv.Distributions.ToJObject()),
                new JProperty("parents",
                    new JArray(rv.Parents)
                )
            );
        }

        public static JObject ToJObject(this Space s)
        {
            var s1 = s;
            return new JObject(
                new JProperty("discrete",
                    new JArray(s1.Values)
                ),
                new JProperty("labels",
                    new JArray(s1.Values.Select(v => s1.GetLabel(v)))
                )
            );
        }

        public static JArray ToJObject(this FAI.Bayesian.DistributionSet ds)
        {
            return new JArray(
                ds
                .EnumerateDistributions()
                .Select(p => new JObject(
                    new JProperty("key", p.Item1.ToJObject()),
                    new JProperty("distribution", p.Item2.ToJObject()))
                )
            );
        }

        public static JArray ToJObject(this DiscreteDistribution d)
        {
            return new JArray(
                d
                .Masses
                .Select(p => new JObject(
                    new JProperty("value", p.Key),
                    new JProperty("mass", p.Value))
                )
            );
        }

        public static JObject ToJObject(this FAI.Bayesian.Observation o)
        {
            return new JObject(
                o.Select(p => new JProperty(p.Key, p.Value))
            );
        }
    }
}
