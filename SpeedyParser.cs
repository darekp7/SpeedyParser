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

        private Dictionary<string, List<string>> OutTable;
        private Action<string, string> OutFunc;

        public SpeedyParser(string[] speedyExpr)
        {
            FSensitivity = FLG_IS_CASE_SENSITIVE | FLG_IS_BRACKET_SENSITIVE | FLG_DOUBLE_QUOTE_CPP | FLG_SINGLE_QUOTE_CPP;
            SpeedyExpression = speedyExpr ?? new[] { "" };
            PatternsEnd = SpeedyExpression.Length;
            while (PatternsEnd > 0 && IsNullOrTrimIsEmpty(SpeedyExpression[PatternsEnd - 1]))
                PatternsEnd--;
            OutTable = null;
            OutFunc = null;
        }

        public bool TryMatch(string str)
        {
            string unboundVar = null;
            int str_pos = 0;
            int patt_pos = 0;
            return TryMatchSinglePass(str, ref str_pos, ref patt_pos, PatternsEnd, ref unboundVar)
                && GotoPrintChar(str, str_pos) >= str.Length;
        }

        private bool TryMatchSinglePass(string str, ref int str_pos, ref int patt_pos, int patt_end, ref string unboundVar)
        {
            for (; patt_pos < patt_end; patt_pos++)
            {
                string pattern = SpeedyExpression[patt_pos];
                if (IsNullOrTrimIsEmpty(pattern))
                    continue;
                if (!MatchSingleLine_Safe(str, ref str_pos, pattern, ref unboundVar))
                    return false;
            }
            return true;
        }

        private bool MatchSingleLine_Safe(string str, ref int str_pos, string pattern, ref string unboundVar)
        {
            string saveVar = unboundVar;
            int savePos = str_pos = GotoPrintChar(str, str_pos);
            if (MatchSingleLine_Unsafe(str, ref str_pos, pattern, ref unboundVar))
                return true;
            if (saveVar == null)
            {
                unboundVar = saveVar;
                str_pos = savePos;
                return false;
            }
            while ((str_pos = FindNextMatchingPos(str, str_pos, FSensitivity)) < str.Length)
            {
                int savePos2 = str_pos;
                if (MatchSingleLine_Unsafe(str, ref str_pos, pattern, ref unboundVar))
                    return true;
                unboundVar = saveVar;
                str_pos = savePos2;
            }
            unboundVar = saveVar;
            str_pos = savePos;
            return false;
        }

        private bool MatchSingleLine_Unsafe(string str, ref int str_pos, string pattern, ref string unboundVar)
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
                        int savePos = p;
                        unboundVar = (p >= pattern.Length || !char.IsLetterOrDigit(pattern[p]) || OutTable == null && OutFunc == null)
                            ? "_"
                            : str.Substring(savePos, (p = GetIdentEnd(str, p)) - savePos);
                        // since unbound variable must be the last item of line, we can just return true
                        return true;
                    default:
                        if (!MatchToken(str, ref str_pos, (FSensitivity & FLG_IS_CASE_SENSITIVE) != 0, pattern, ref p))
                            return false;
                        break;
                }
            }
            return true;
        }

        private static bool MatchToken(string str, ref int str_pos, bool caseSensitive, string pattern, ref int pattern_pos)
        {
            if ((str_pos = GotoPrintChar(str, str_pos)) >= str.Length)
                return false;
            if (IsIdentChar(pattern[pattern_pos]) && str_pos > 0 && IsIdentChar(str[str_pos - 1]))
                return false;
            if (caseSensitive)
                do
                {
                    if (str_pos >= str.Length || pattern[pattern_pos] != str[str_pos++])
                        return false;
                }
                while (++pattern_pos < pattern.Length && !char.IsWhiteSpace(pattern[pattern_pos]));
            else
                do
                {
                    if (str_pos >= str.Length || char.ToUpper(pattern[pattern_pos]) != char.ToUpper(str[str_pos++]))
                        return false;
                }
                while (++pattern_pos < pattern.Length && !char.IsWhiteSpace(pattern[pattern_pos]));
            return str_pos >= str.Length || !IsIdentChar(str[str_pos]) || !IsIdentChar(pattern[pattern_pos - 1]);
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

        public static int FindNextMatchingPos(string str, int pos, uint sensitivity)
        {
            int TODO = 1;
            pos = FindEndPos(str, pos, sensitivity);
            return GotoPrintChar(str, pos);
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
