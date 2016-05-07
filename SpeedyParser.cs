using System;
using System.Collections.Generic;
using System.Text;

/*
 * Simple parser.
 * Author: Dariusz Pilarczyk (dpilarcz@gmail.com)
 * Licence: public domain / WTFPL (http://www.wtfpl.net/about/)
 * 
 * Main features:
 * - simple parser for extracting same information(s) from string
 * - easy to learn regular expressions (not like Regex!)
 * - good performance
 * - requires no precompilation/preprocessing/time consuming initialization. Nothing like this. Just put pattern and use.
 * - small amount of source code, everything is contained in one simple struct.
 */

namespace ImmutableList
{
    struct SpeedyParser
    {
        public const uint FLG_IS_CASE_SENSITIVE = 1;
        public const uint FLG_IS_BRACKET_SENSITIVE = 2;
        public const uint FLG_DOUBLE_QUOTE_CPP = 4;
        public const uint FLG_DOUBLE_QUOTE_SQL = 8;
        public const uint FLG_SINGLE_QUOTE_CPP = 16;
        public const uint FLG_SINGLE_QUOTE_SQL = 32;

        private uint FSensitivity;

        private string[] SpeedyExpression;
        private int PatternsEnd;

        private string ParsedStr;
        private string UnboundVar;
        private int UnbndVPttrnPos;
        private int UnbndVarStrPos;

        private Dictionary<string, List<string>> OutTable;
        private Func<string, string, bool> OutFunc;
        private Func<string, int, string, int, int, bool> OutFunc2;

        public SpeedyParser(string[] speedyExpr)
        {
            FSensitivity = FLG_IS_CASE_SENSITIVE | FLG_IS_BRACKET_SENSITIVE | FLG_DOUBLE_QUOTE_CPP | FLG_SINGLE_QUOTE_CPP;
            SpeedyExpression = speedyExpr ?? new[] { "" };
            PatternsEnd = SpeedyExpression.Length;
            while (PatternsEnd > 0 && IsNullOrTrimIsEmpty(SpeedyExpression[PatternsEnd - 1]))
                PatternsEnd--;

            UnboundVar = null;
            UnbndVPttrnPos = 0;
            UnbndVarStrPos = 0;

            OutTable = null;
            OutFunc = null;
            OutFunc2 = null;
            ParsedStr = null;
        }

        public bool TryMatch(string str, Dictionary<string, List<string>> res)
        {
            int str_pos = 0;
            int patt_pos = 0;
            SpeedyParser sp = this;
            sp.ParsedStr = str = str ?? "";
            sp.OutTable = res;
            try
            {
                return sp.TryMatchSinglePass(ref str_pos, ref patt_pos, PatternsEnd)
                    && sp.GotoPrintChar(str_pos) >= sp.ParsedStr.Length;
            }
            catch (Exception ex)
            {
                if (ex.Message == "")
                    return false;
                else
                    throw;
            }
        }

        private bool TryMatchSinglePass(ref int str_pos, ref int patt_pos, int patt_end)
        {
            for (; patt_pos < patt_end; patt_pos++)
            {
                string pattern = SpeedyExpression[patt_pos];
                if (IsNullOrTrimIsEmpty(pattern))
                    continue;
                if (!MatchSingleLine_Safe(ref str_pos, pattern))
                    return false;
            }
            return true;
        }

        private bool MatchSingleLine_Safe(ref int str_pos, string pattern)
        {
            string saveVar = UnboundVar;
            int savePosMSL = str_pos = GotoPrintChar(str_pos);
            if (MatchSingleLine_Unsafe(ref str_pos, pattern))
                return true;
            if (saveVar == null)
            {
                UnboundVar = saveVar;
                str_pos = savePosMSL;
                return false;
            }
            while ((str_pos = FindNextMatchingPos(str_pos, FSensitivity)) < ParsedStr.Length)
            {
                int savePos2 = str_pos;
                if (MatchSingleLine_Unsafe(ref str_pos, pattern))
                    return true;
                UnboundVar = saveVar;
                str_pos = savePos2;
            }
            UnboundVar = saveVar;
            str_pos = savePosMSL;
            return false;
        }

        private bool MatchSingleLine_Unsafe(ref int str_pos, string pattern)
        {
            int p = 0;
            while (p < pattern.Length)
            {
                if (char.IsWhiteSpace(pattern[p]))
                {
                    p++;
                    continue;
                }
                switch (pattern[p])
                {
                    case '$':
                        if (++p < pattern.Length && (pattern[p] == '$' || pattern[p] == '[' || pattern[p] == ']'))
                            goto default;
                        if(p >= pattern.Length || !char.IsLetterOrDigit(pattern[p]))
                        {
                            UnboundVar = "_";
                            UnbndVPttrnPos = 0;
                        }
                        else
                        {
                            UnboundVar = pattern;
                            UnbndVPttrnPos = p;
                        }
                        UnbndVarStrPos = str_pos;
                        // since unbound variable must be the last item of line, we can just return true
                        return true;
                    default:
                        int savePos2M = str_pos;
                        if (!MatchToken(ref str_pos, (FSensitivity & FLG_IS_CASE_SENSITIVE) != 0, pattern, ref p))
                            return false;
                        if (UnboundVar != null)
                        {
                            SaveVariable(UnboundVar, UnbndVPttrnPos, UnbndVarStrPos, savePos2M);
                            UnboundVar = null;
                        }
                        break;
                }
            }
            return true;
        }

        private bool MatchToken(ref int str_pos, bool caseSensitive, string pattern, ref int pattern_pos)
        {
            if ((str_pos = GotoPrintChar(str_pos)) >= ParsedStr.Length)
                return false;
            if (IsIdentChar(pattern[pattern_pos]) && str_pos > 0 && IsIdentChar(ParsedStr[str_pos - 1]))
                return false;
            if (caseSensitive)
                do
                {
                    if (str_pos >= ParsedStr.Length || pattern[pattern_pos] != ParsedStr[str_pos++])
                        return false;
                }
                while (++pattern_pos < pattern.Length && !char.IsWhiteSpace(pattern[pattern_pos]));
            else
                do
                {
                    if (str_pos >= ParsedStr.Length || char.ToUpper(pattern[pattern_pos]) != char.ToUpper(ParsedStr[str_pos++]))
                        return false;
                }
                while (++pattern_pos < pattern.Length && !char.IsWhiteSpace(pattern[pattern_pos]));
            return str_pos >= ParsedStr.Length || !IsIdentChar(ParsedStr[str_pos]) || !IsIdentChar(pattern[pattern_pos - 1]);
        }

        private void SaveVariable(string varNamePattern, int unbndVPttrnPos, int unbndVarStrPos, int str_pos_end)
        {
            if (varNamePattern != null && varNamePattern[0] != '_')
            {
                if (OutTable != null || OutFunc != null)
                {
                    string varName = varNamePattern.Substring(unbndVPttrnPos, GetIdentEnd(varNamePattern, unbndVPttrnPos) - unbndVPttrnPos);
                    string varValue = ParsedStr.Substring(unbndVarStrPos, str_pos_end - unbndVarStrPos).Trim();
                    if (OutTable != null)
                    {
                        List<string> ll;
                        if (!OutTable.TryGetValue(varName, out ll))
                            ll = new List<string>();
                        ll.Add((varValue ?? "").Trim());
                    }
                    if (OutFunc != null && !OutFunc(varName, varValue))
                        throw new Exception("");                           
                }
                if (OutFunc2 != null && !OutFunc2(varNamePattern, unbndVPttrnPos, ParsedStr, unbndVarStrPos, str_pos_end))
                        throw new Exception("");                           
            }
        }

        public static bool IsIdentChar(char c)
        {
            return c == '_' || char.IsLetterOrDigit(c);
        }

        public static bool IsNullOrTrimIsEmpty(string str)
        {
            if (str == null)
                return true;
            for (int i = 0; i < str.Length; i++)
                if (!char.IsWhiteSpace(str[i]))
                    return false;
            return true;
        }

        public static int GetIdentEnd(string str, int pos)
        {
            while (pos < str.Length && IsIdentChar(str[pos]))
                pos++;
            return pos;
        }

        public int FindNextMatchingPos(int pos, uint sensitivity)
        {
            int TODO = 1;
            pos = FindEndPos(pos, sensitivity);
            return GotoPrintChar(pos);
        }

        public int FindEndPos(int pos, uint sensitivity)
        {
            string str = ParsedStr;
            char c = str[pos];
            if (char.IsWhiteSpace(c))
                return GotoPrintChar(pos);
            if (IsIdentChar(c))
                return GetIdentEnd(str, pos);
            switch (c)
            {
                case '(':
                case '[':
                case '{':
                    return ((sensitivity & FLG_IS_BRACKET_SENSITIVE) != 0) 
                        ? GotoAfterClosingBracket(str, pos + 1, sensitivity) : pos + 1;
                case '"':
                    return ((sensitivity & (FLG_DOUBLE_QUOTE_CPP|FLG_DOUBLE_QUOTE_SQL)) != 0) 
                        ? GotoAfterClosingQuote(str, pos, sensitivity & FLG_DOUBLE_QUOTE_CPP, sensitivity & FLG_DOUBLE_QUOTE_SQL) 
                        : pos + 1;
                case '\'':
                    return ((sensitivity & (FLG_SINGLE_QUOTE_CPP | FLG_SINGLE_QUOTE_SQL)) != 0) 
                        ? GotoAfterClosingQuote(str, pos, sensitivity & FLG_SINGLE_QUOTE_CPP, sensitivity & FLG_SINGLE_QUOTE_SQL) 
                        : pos + 1;
                default:
                    return pos + 1;
            }
        }

        private static int GotoAfterClosingBracket(string str, int pos, uint sensitivity)
        {
            int n = 1;
            for (pos++; pos < str.Length; pos++)
                switch (str[pos])
                {
                    case '"':
                        if ((sensitivity & (FLG_DOUBLE_QUOTE_CPP | FLG_DOUBLE_QUOTE_SQL)) != 0)
                            pos = GotoAfterClosingQuote(str, pos, sensitivity & FLG_DOUBLE_QUOTE_CPP, sensitivity & FLG_DOUBLE_QUOTE_SQL) - 1;
                        break;
                    case '\'':
                        if ((sensitivity & (FLG_SINGLE_QUOTE_CPP | FLG_SINGLE_QUOTE_SQL)) != 0)
                            pos = GotoAfterClosingQuote(str, pos, sensitivity & FLG_SINGLE_QUOTE_CPP, sensitivity & FLG_SINGLE_QUOTE_SQL) - 1;
                        break;
                    case '(':
                    case '[':
                    case '{':
                        n++;
                        break;
                    case ')':
                    case ']':
                    case '}':
                        if (--n <= 0)
                            return pos + 1;
                        break;
                }
            return pos;
        }

        public static int GotoAfterClosingQuote(string str, int pos, uint n_styleCpp, uint n_styleSql)
        {
            bool styleCpp = n_styleCpp != 0;
            bool styleSql = n_styleSql != 0;
            for (char quoteChar = str[pos++]; pos < str.Length; pos++)
            {
                char c = str[pos];
                if (c == '\\' && styleCpp && !styleSql)
                    pos++;
                else if (c == quoteChar)
                    if (styleSql)
                    {
                        if (styleCpp || pos + 1 >= str.Length || str[pos + 1] != quoteChar)
                            return pos + 1;
                        pos++;
                    }
                    else
                        return pos + 1;
            }
            return pos;
        }

        private int GotoPrintChar(int pos)
        {
            while (pos < ParsedStr.Length && Char.IsWhiteSpace(ParsedStr[pos]))
                pos++;
            return pos;
        }
    }
}
