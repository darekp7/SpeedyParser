using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Windows.Forms;
using SpeedyTools;

namespace TestApp
{
    public partial class Form1 : Form
    {
        public Form1()
        {
            InitializeComponent();
        }

        private void btnParseSQL_Click(object sender, EventArgs e)
        {
            var options = new SpeedyParser.ParserOptions
            {
                IsCaseSensitive = false,
                DoubleQuoteSensitivity = SpeedyParser.QuoteSensitivity.Insensitive,
                SingleQuoteSensitivity = SpeedyParser.QuoteSensitivity.SqlLike,
                Comments = new[] { new Tuple<string, string>("--", "") }
            };

            var parser = new SpeedyParser(options, p =>
                p.If("SELECT",
                    p.Span("_columns"),
                    p.If("FROM",
                        p.Span("tablename"),
                        p.While("JOIN",
                            p.Span("tablename"),
                            p.If("ON",
                                p.Span("_condition"))),
                        p.If("WHERE",
                                p.Span("_condition")))
                )
                && p.Eof);

            string query = "select * from employee";
            var matchResult = parser.TryMatch(query);
            if (matchResult.Success)
            {
                MessageBox.Show(string.Join(", ", from x in matchResult.SelectValues("tablename") select x.Item1));
            }

        }

    }
}
