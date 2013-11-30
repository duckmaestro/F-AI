
using System;
using System.Collections.Generic;
using System.Configuration;
using System.Data;
using System.IO;
using System.Linq;
using System.Text;
using System.Windows;

namespace Bevisuali.UX
{
    public partial class App : Application
    {
        public static new App Current
        {
            get
            {
                return (App)Application.Current;
            }
        }

        public new MainWindow MainWindow
        {
            get
            {
                return (MainWindow)base.MainWindow;
            }
        }

        public string LoadData(string locator)
        {
            try
            {
                using (StreamReader reader = new StreamReader(locator, Encoding.UTF8))
                {
                    return reader.ReadToEnd();
                }
            }
            catch (FileNotFoundException)
            {
                return null;
            }
        }

        public void SaveData(string locator, string data)
        {
            using (StreamWriter writer = new StreamWriter(locator, false, Encoding.UTF8))
            {
                if (data != null)
                {
                    writer.Write(data);
                }
            }
        }
    }
}
