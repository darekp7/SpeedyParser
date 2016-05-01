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

        public SpeedyParser(string[] speedyExpr)
        {
            FSensitivity = FLG_IS_CASE_SENSITIVE | FLG_IS_BRACKET_SENSITIVE | FLG_DOUBLE_QUOTE_CPP | FLG_SINGLE_QUOTE_CPP;
            SpeedyExpression = speedyExpr ?? new[] { "" };
            PatternsEnd = SpeedyExpression.Length;
            while (PatternsEnd > 0 && IsNullOrTrimIsEmpty(SpeedyExpression[PatternsEnd - 1]))
                PatternsEnd--;
        }

        public bool TryMatch(string str, Dictionary<string, List<string>> outTable, Action<string, string> outFunc)
        {
            string unboundVar = null;
            int str_pos = 0;
            int patt_pos = 0;
            return TryMatchPart(str, ref str_pos, ref patt_pos, PatternsEnd, ref unboundVar, outTable, outFunc)
                && GotoPrintChar(str, str_pos) >= str.Length;
        }

        private bool TryMatchPart(string str, ref int str_pos, ref int patt_pos, int patt_end,
            ref string unboundVar, Dictionary<string, List<string>> outTable, Action<string, string> outFunc)
        {
            for (; patt_pos < patt_end; patt_pos++)
            {
                string pattern = SpeedyExpression[patt_pos];
                if (IsNullOrTrimIsEmpty(pattern))
                    continue;
                if (!MatchBasicPattern(str, ref str_pos, pattern, ref unboundVar, outTable, outFunc))
                    return false;
            }
            return true;
        }

        private bool MatchBasicPattern(string str, ref int str_pos, string pattern, 
            ref string unboundVar, Dictionary<string, List<string>> outTable, Action<string, string> outFunc)
        {
            int p = 0;
            while (p < pattern.Length)
            {
                if ((p = GotoPrintChar(pattern, p)) >= pattern.Length)
                    return true;
                switch (pattern[p])
                {
                    case '$':
                        if (++p < pattern.Length && pattern[p] == '$')
                            goto default;
                        unboundVar = (p >= pattern.Length || !IsIdentChar(pattern[p]) || pattern[p] == '_' 
                            || outTable == null && outFunc == null)? "_" : str.Substring(p, GetIdentEnd(str, p) - p);
                        break;
                    default:
                        if (!MatchToken(str, ref str_pos, (FSensitivity & FLG_IS_CASE_SENSITIVE) != 0, pattern, ref p))
                            return false;
                        //GotoNextMatchPos(str, ref str_pos, str_end);
                        str_pos = GotoPrintChar(str, str_pos);
                        break;
                }
            }
            return true;
        }

        private static bool MatchToken(string str, ref int str_pos, bool caseSensitive, string pattern, ref int pattern_pos)
        {
            if ((str_pos = GotoPrintChar(str, str_pos)) >= str.Length)
                return false;
            char c = pattern[pattern_pos];
            if (IsIdentChar(c) && str_pos > 0 && IsIdentChar(str[str_pos - 1]))
                return false;
            if (caseSensitive)
                for (; pattern_pos < pattern.Length && !char.IsWhiteSpace(c=pattern[pattern_pos]); pattern_pos++)
                {
                    if (str_pos >= str.Length || c != str[str_pos++])
                        return false;
                }
            else
                for (; pattern_pos < pattern.Length && !char.IsWhiteSpace(c=pattern[pattern_pos]); pattern_pos++)
                {
                    if (str_pos >= str.Length || char.ToUpper(c) != char.ToUpper(str[str_pos++]))
                        return false;
                }
            return !IsIdentChar(c) || str_pos >= str.Length || !IsIdentChar(str[str_pos]);
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

        public static int FindEndPos(string str, int pos, uint sensitivity)
        {
            char c = str[pos];
            if (char.IsWhiteSpace(c))
                return GotoPrintChar(str, pos);
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

        private static int GotoPrintChar(string str, int pos)
        {
            while (pos < str.Length && Char.IsWhiteSpace(str[pos]))
                pos++;
            return pos;
        }
    }
}
