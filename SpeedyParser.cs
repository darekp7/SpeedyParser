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
        private bool FIsCaseSensitive;
        private bool FBracketSensitive;
        private bool FDoubleQuoteIsCpp;
        private bool FSingleQuoteIsCpp;
        private bool FDoubleQuoteIsSql;
        private bool FSingleQuoteIsSql;

        private string[] SpeedyExpression;
        private int PatternsEnd;

        public SpeedyParser(string[] speedyExpr)
        {
            FIsCaseSensitive = true;
            FBracketSensitive = true;
            FDoubleQuoteIsCpp = true;
            FSingleQuoteIsCpp = true;
            FDoubleQuoteIsSql = false;
            FSingleQuoteIsSql = false;

            SpeedyExpression = speedyExpr ?? new[] { "" };
            PatternsEnd = SpeedyExpression.Length;
            while (PatternsEnd > 0 && IsNullOrTrimIsEmpty(SpeedyExpression[PatternsEnd - 1]))
                PatternsEnd--;
        }

        public bool TryMatch(string str)
        {
            ulong unboundVar = 0;
            int str_pos = 0;
            int patt_pos = 0;
            return TryMatchPart(str, ref str_pos, str.Length, ref patt_pos, PatternsEnd, ref unboundVar)
                && GotoPrintChar(str, ref str_pos, str.Length) == '\0';
        }

        private bool TryMatchPart(string str, ref int str_pos, int str_end, ref int patt_pos, int patt_end, ref ulong unboundVar)
        {
            for (; patt_pos < patt_end; patt_pos++)
            {
                string pattern = SpeedyExpression[patt_pos];
                if (IsNullOrTrimIsEmpty(pattern))
                    continue;
                if (!MatchBasicPattern(str, ref str_pos, str_end, pattern, ref unboundVar))
                    return false;
            }
            return true;
        }

        private bool MatchBasicPattern(string str, ref int str_pos, int str_end, string pattern, ref ulong unboundVar)
        {
            int p = 0;
            while (p < pattern.Length)
                switch (GotoPrintChar(pattern, ref p, pattern.Length))
                {
                    case '\0':
                        return true;
                    case '$':
                        if (p + 1 < pattern.Length && pattern[++p] == '$')
                            goto default;
                        unboundVar = T2Make((ulong)p, (ulong)GetIdentLen(str, p + 1));
                        break;
                    default:
                        if (!MatchToken(str, ref str_pos, str_end, FIsCaseSensitive, pattern, ref p, pattern.Length))
                            return false;
                        //GotoNextMatchPos(str, ref str_pos, str_end);
                        GotoPrintChar(str, ref str_pos, str_end);
                        break;
                }
            return true;
        }

        private const ulong T2MASK_LOW = 0xFFFFFFFFFFFFFFFF;

        private static ulong T2Make(ulong pos, ulong len)
        {
            return pos | (len << 32);
        }

        private static int GetIdentLen(string str, int pos)
        {
            for (int n = 0; pos + n < str.Length; n++)
                if (!IsIdentChar(str[pos + n]))
                    return n;
            return 0;
        }

        private static bool MatchToken(string str, ref int str_pos, int str_end, bool caseSensitive, string pattern, ref int pattern_pos, int pattern_end)
        {
            GotoPrintChar(str, ref str_pos, str_end);
            char c = pattern[pattern_pos];
            if (IsIdentChar(c) && str_pos > 0 && IsIdentChar(str[str_pos - 1]))
                return false;
            if (caseSensitive)
                for (; pattern_pos < pattern_end && !char.IsWhiteSpace(c=pattern[pattern_pos]); pattern_pos++)
                {
                    if (str_pos >= str_end || c != str[str_pos++])
                        return false;
                }
            else
                for (; pattern_pos < pattern_end && !char.IsWhiteSpace(c=pattern[pattern_pos]); pattern_pos++)
                {
                    if (str_pos >= str_end || char.ToUpper(c) != char.ToUpper(str[str_pos++]))
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

        private bool GotoNextMatchPos(string str, ref int pos, int end)
        {
            char c = str[pos];
            if (IsIdentChar(c))
            {
                while (IsIdentChar(str[++pos]))
                    ;
                return GotoPrintChar(str, ref pos, end) != '\0';
            }
            switch (c)
            {
                case '(':
                case '[':
                case '{':
                    if (FBracketSensitive)
                        return GotoAfterClosingBracket(str, ref pos, end);
                    else
                    {
                        pos++;
                        return GotoPrintChar(str, ref pos, end) != '\0';
                    }
                case '"':
                    if (FDoubleQuoteIsCpp || FDoubleQuoteIsSql)
                        return GotoAfterClosingQuote(FDoubleQuoteIsCpp, FDoubleQuoteIsSql, str, ref pos, end);
                    else
                    {
                        pos++;
                        return GotoPrintChar(str, ref pos, end) != '\0';
                    }
                case '\'':
                    if (FSingleQuoteIsCpp || FSingleQuoteIsSql)
                        return GotoAfterClosingQuote(FSingleQuoteIsCpp, FSingleQuoteIsSql, str, ref pos, end);
                    else
                    {
                        pos++;
                        return GotoPrintChar(str, ref pos, end) != '\0';
                    }
                default:
                    pos++;
                    return GotoPrintChar(str, ref pos, end) != '\0';
            }
        }

        private bool GotoAfterClosingBracket(string str, ref int pos, int end)
        {
            int n = 1;
            for (pos++; pos < end; pos++)
                switch (str[pos])
                {
                    case '"':
                        if ((FDoubleQuoteIsCpp || FDoubleQuoteIsSql)  && !GotoAfterClosingQuote(
                                FDoubleQuoteIsCpp, FDoubleQuoteIsSql, str, ref pos, end))
                        {
                            return false;
                        }
                        break;
                    case '\'':
                        if ((FSingleQuoteIsCpp || FSingleQuoteIsSql) && !GotoAfterClosingQuote(
                                FSingleQuoteIsCpp, FSingleQuoteIsSql, str, ref pos, end))
                        {
                            return false;
                        }
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
                        {
                            pos++;
                            return true;
                        }
                        break;
                }
            return false;
        }

        public static bool GotoAfterClosingQuote(bool styleCpp, bool styleSql, string str, ref int pos, int end)
        {
            char quoteChar = str[pos++];
            for (; pos < end; pos++)
            {
                char c = str[pos];
                if (c == quoteChar && styleSql && pos + 1 < end && str[pos + 1] == quoteChar)
                    pos++;
                else if (c == quoteChar)
                {
                    pos++;
                    return true;
                }
                else if (styleCpp && c == '\\')
                    pos++;
            }
            return false;
        }

        private static char GotoPrintChar(string str, ref int pos, int end)
        {
            while (pos < end && Char.IsWhiteSpace(str[pos]))
                pos++;
            return (pos < end) ? str[pos] : '\0';
        }
    }
}
