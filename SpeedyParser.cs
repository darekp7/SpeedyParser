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
        private struct PreprocessedLine
        {
            public string Line;
            public int SentinelStartPos; // sentinel means more or less guard (in polish wartownik)
            public int NextSentinel; // next guard 
            public int MinRepeat;
            public int MaxRepeat;
        }

        private enum MatchResult
        {
            True,
            False,
            TotalFailure
        }

        private bool FIsCaseSensitive;
        private bool FIsBracketSensitive;
        private bool FDoubleQuoteIsCpp;
        private bool FDoubleQuoteIsSql;
        private bool FSingleQuoteIsCpp;
        private bool FSingleQuoteIsSql;

        private PreprocessedLine[] MyPreprocessed;
        private int PatternsEnd;

        private string ParsedStr;
        private int FirstSentinel;

        private Dictionary<string, List<string>> OutTable;
        private Func<string, string, bool> OutFunc;
        private Func<string, int, int, string, int, bool> OutFunc2;

        private const int PATTERN_IS_LINE = -1;
        private static readonly string[] FEmptyLines = new string[0];

        public SpeedyParser(string[] speedyExpr)
        {
            speedyExpr = speedyExpr ?? FEmptyLines;

            FIsCaseSensitive = true;
            FIsBracketSensitive = true;
            FDoubleQuoteIsCpp = true;
            FDoubleQuoteIsSql = false;
            FSingleQuoteIsCpp = true;
            FSingleQuoteIsSql = false;

            OutTable = null;
            OutFunc = null;
            OutFunc2 = null;
            ParsedStr = null;
            MyPreprocessed = new PreprocessedLine[speedyExpr.Length];
            PatternsEnd = 0;
            FirstSentinel = -1;
            Preprocess(speedyExpr);
        }

        private void Preprocess(string[] speedyExpr)
        {
            PatternsEnd = 0;
            for (int i_pattern_line = 0; i_pattern_line < speedyExpr.Length; i_pattern_line++)
            {
                string s = speedyExpr[i_pattern_line] ?? "";
                int i = 0;
                if (GotoPrintChar(s, ref i, s.Length) == '\0')
                    continue;
                switch (s[i])
                {
                    case '$':
                        if (++i < s.Length && (s[i] == '$' || s[i] == '[' || s[i] == '|' || s[i] == ']'))
                        {
                            i--;
                            goto default;
                        }
                        CheckLineSyntax(s, i_pattern_line);
                        MyPreprocessed[PatternsEnd++] = new PreprocessedLine
                        {
                            Line = s,
                            SentinelStartPos = -1,
                            NextSentinel = -1,
                            MinRepeat = -1,
                            MaxRepeat = -1
                        };
                        break;
                    case '[':
                    case '|':
                        i++;
                        if (GotoPrintChar(s, ref i, s.Length) != '\0')
                            throw new Exception(string.Format("Row {0}: '[' and '|' must be placed in separate line.", i_pattern_line + 1));
                        MyPreprocessed[PatternsEnd++] = new PreprocessedLine
                        {
                            Line = s,
                            SentinelStartPos = -1,
                            NextSentinel = -1,
                            MinRepeat = -1,
                            MaxRepeat = -1
                        };
                        break;
                    case ']':
                        int min_repeat = 0;
                        int max_repeat = 1;
                        i++;
                        switch (GotoPrintChar(s, ref i, s.Length))
                        {
                            case '\0':
                            case '?':
                                break;
                            case '!':
                                min_repeat = 1;
                                break;
                            case '*':
                                max_repeat = int.MaxValue;
                                break;
                            case '+':
                                min_repeat = 1;
                                max_repeat = int.MaxValue;
                                break;
                            case '[':
                                i++;
                                if (!Atoi(s, ref i, out min_repeat))
                                    throw new Exception(string.Format("Row {0}: syntax error, expected syntax: [number..number].", i_pattern_line + 1));
                                if (TestString(s, ref i, "]"))
                                    max_repeat = min_repeat;
                                else if (!TestString(s, ref i, "..") || !Atoi(s, ref i, out max_repeat) || !TestString(s, ref i, "]") || GotoPrintChar(s, ref i, s.Length) != '\0')
                                    throw new Exception(string.Format("Row {0}: syntax error, expected syntax: [number..number].", i_pattern_line + 1));
                                break;

                        }
                        MyPreprocessed[PatternsEnd++] = new PreprocessedLine
                        {
                            Line = s,
                            SentinelStartPos = -1,
                            NextSentinel = -1,
                            MinRepeat = min_repeat,
                            MaxRepeat = max_repeat
                        };
                        break;
                    default:
                        CheckLineSyntax(s, i_pattern_line);
                        MyPreprocessed[PatternsEnd++] = new PreprocessedLine
                        {
                            Line = s,
                            SentinelStartPos = i,
                            NextSentinel = -1,
                            MinRepeat = -1,
                            MaxRepeat = -1
                        };
                        if (FirstSentinel < 0)
                            FirstSentinel = i_pattern_line;
                        for (int j = i_pattern_line - 1; j >= 0 && MyPreprocessed[j].NextSentinel < 0; j--)
                            MyPreprocessed[j].NextSentinel = i_pattern_line;
                        break;
                }
            }
        }

        private void CheckLineSyntax(string str, int i_pattern_line)
        {
            for (int i=0; i < str.Length; i++)
                if (!char.IsWhiteSpace(str[i]))
                    switch (str[i])
                    {
                        case '$':
                            if (++i >= str.Length || char.IsWhiteSpace(str[i]))
                                throw new Exception(string.Format("Row {0}: expected identifier after '$'.", i_pattern_line + 1));
                            if (str[i] == '$' || str[i] == '[' || str[i] == '|' || str[i] == ']')
                                goto default;
                            if (!IsIdentChar(str[i]))
                                throw new Exception(string.Format("Row {0}: expected identifier or '_' after '$'.", i_pattern_line + 1));
                            if ((i = FindIdentEnd(str, i)) + 1 < str.Length && str[i] == ':')
                                CheckTypeName(str, ref i, i_pattern_line);
                            break;
                        default:
                            while (i < str.Length && !char.IsWhiteSpace(str[i]))
                                i++;
                            i--;
                            break;
                    }
        }

        private void CheckTypeName(string str, ref int i, int i_pattern_line)
        {
            if (++i >= str.Length)
                throw new Exception(string.Format("Row {0}: expected type after ':'.", i_pattern_line + 1));
            if (str[i] == '*')
            {
                if (i + 1 < str.Length && !char.IsWhiteSpace(str[i + 1]))
                    throw new Exception(string.Format("Row {0}: invalid type (after ':').", i_pattern_line + 1));
            }
            else
            {
                int TODO = 1;
            }

        }

        public bool TryMatch(string str, Dictionary<string, List<string>> res)
        {
            int str_pos = 0;
            int patt_pos = 0;
            SpeedyParser sp = this;
            sp.ParsedStr = str = str ?? "";
            sp.OutTable = res;
            if (sp.PatternsEnd <= 0)
                return sp.GotoPrintChar(str_pos) >= sp.ParsedStr.Length;
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
            while (patt_pos < patt_end)
                if (MatchSingleLine_Safe(ref str_pos, MyPreprocessed[patt_pos].Line) != MatchResult.True)
                    return false;
            return true;
        }

        private MatchResult MatchSingleLine_Safe(ref int str_pos, string pattern)
        {
            int savePosMSL = str_pos = GotoPrintChar(str_pos);
            MatchResult res = MatchSingleLine_Unsafe(ref str_pos, pattern);
            if(res != MatchResult.True)
                str_pos = savePosMSL;
            return res;
        }

        private MatchResult MatchSingleLine_Unsafe(ref int str_pos, string pattern)
        {
            MatchResult resFalse = MatchResult.False;
            for (int p = 0; p < pattern.Length; p++)
            {
                if (char.IsWhiteSpace(pattern[p]))
                    continue;
                switch (pattern[p])
                {
                    case '$':
                        if (pattern[++p] == '$' || pattern[p] == '[' || pattern[p] == ']')
                            goto default;
                        resFalse = MatchResult.TotalFailure;
                        int str_begin = str_pos = GotoPrintChar(str_pos);
                        int p_end = FindIdentEnd(pattern, p);
                        if (p_end >= pattern.Length || pattern[p_end] != ':')
                        {
                            char c_after_var = GotoPrintChar(pattern, ref p_end, pattern.Length);
                            while (str_pos < ParsedStr.Length)
                            {
                                if (ParsedStr[str_pos] == c_after_var
                                    && MatchToken_Safe(ref str_pos, FIsCaseSensitive, pattern, ref p_end))
                                {
                                    SaveVariable(str_begin, str_pos, pattern, p);
                                    p = p_end - 1;
                                    break;
                                }

                                bool match_found = false;
                                for (int sent = FirstSentinel; sent >= 0; sent = MyPreprocessed[sent].NextSentinel)
                                {
                                    int sent_patt_pos = MyPreprocessed[sent].SentinelStartPos;
                                    string sent_patt = MyPreprocessed[sent].Line;
                                    if (ParsedStr[str_pos] == sent_patt[sent_patt_pos]
                                        && MatchToken_Safe(ref str_pos, FIsCaseSensitive, pattern, ref sent_patt_pos))
                                    {
                                        SaveVariable(str_begin, str_pos, sent_patt, sent_patt_pos);
                                        p = p_end;
                                        match_found = true;
                                        break;
                                    }
                                }
                                if (match_found)
                                    break;
                                str_pos = GotoPrintChar(FindEndPos(str_pos));
                            }
                            if (str_pos >= ParsedStr.Length)
                            {
                                SaveVariable(str_begin, str_pos, pattern, p);
                                return GotoPrintChar(pattern, ref p_end, pattern.Length) == '\0'? MatchResult.True : MatchResult.TotalFailure;
                            }
                        }
                        else if (pattern[++p_end] == '*')
                        {
                            SaveVariable(str_begin, str_pos, pattern, p);
                            return GotoPrintChar(pattern, ref p_end, pattern.Length) == '\0' ? MatchResult.True : MatchResult.TotalFailure;
                        }
                        else
                        {
                            int p_type = ++p_end;
                            p_end = FindIdentEnd(pattern, p_type);
                            GotoPrintChar(pattern, ref p_end, pattern.Length);
                        }
                        break;
                    default:
                        if (!MatchToken(ref str_pos, FIsCaseSensitive, pattern, ref p))
                            return resFalse;
                        break;
                }
            }
            return MatchResult.True;
        }

        private bool MatchToken_Safe(ref int str_pos, bool caseSensitive, string pattern, ref int pattern_pos)
        {
            int ps = str_pos;
            int pp = pattern_pos;
            if (MatchToken(ref ps, caseSensitive, pattern, ref pp))
            {
                str_pos = ps;
                pattern_pos = pp;
                return true;
            }
            return false;
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

        private void SaveVariable(int str_pos_start, int str_pos_end, string varNamePattern, int varPos)
        {
            if (varNamePattern != null && varNamePattern[0] != '_')
            {
                if (OutTable != null || OutFunc != null)
                {
                    string varName = varNamePattern.Substring(varPos, FindIdentEnd(varNamePattern, varPos) - varPos);
                    string varValue = ParsedStr.Substring(str_pos_start, str_pos_end - str_pos_start).Trim();
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
                if (OutFunc2 != null && !OutFunc2(ParsedStr, str_pos_start, str_pos_end, varNamePattern, varPos))
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

        public static int FindIdentEnd(string str, int pos)
        {
            while (pos < str.Length && IsIdentChar(str[pos]))
                pos++;
            return pos;
        }

        public int FindEndPos(int pos)
        {
            string str = ParsedStr;
            char c = str[pos];
            if (char.IsWhiteSpace(c))
                return GotoPrintChar(pos);
            if (IsIdentChar(c))
                return FindIdentEnd(str, pos);
            switch (c)
            {
                case '(':
                case '[':
                case '{':
                    return FIsBracketSensitive? GotoAfterClosingBracket(str, pos + 1) : pos + 1;
                case '"':
                    return (FDoubleQuoteIsCpp || FDoubleQuoteIsSql) ? GotoAfterClosingQuote(str, pos, FDoubleQuoteIsCpp, FDoubleQuoteIsSql) : pos + 1;
                case '\'':
                    return (FSingleQuoteIsCpp || FSingleQuoteIsSql) ? GotoAfterClosingQuote(str, pos, FSingleQuoteIsCpp, FSingleQuoteIsSql) : pos + 1;
                default:
                    return pos + 1;
            }
        }

        private int GotoAfterClosingBracket(string str, int pos)
        {
            int n = 1;
            for (pos++; pos < str.Length; pos++)
                switch (str[pos])
                {
                    case '"':
                        if (FDoubleQuoteIsCpp || FDoubleQuoteIsSql)
                            pos = GotoAfterClosingQuote(str, pos, FDoubleQuoteIsCpp, FDoubleQuoteIsSql) - 1;
                        break;
                    case '\'':
                        if (FSingleQuoteIsCpp || FSingleQuoteIsSql)
                            pos = GotoAfterClosingQuote(str, pos, FSingleQuoteIsCpp, FSingleQuoteIsSql) - 1;
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

        public static int GotoAfterClosingQuote(string str, int pos, bool styleCpp, bool styleSql)
        {
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

        private static char GotoPrintChar(string str, ref int pos, int end)
        {
            while (pos < end && Char.IsWhiteSpace(str[pos]))
                pos++;
            return (pos < end) ? str[pos] : '\0';
        }

        private static bool Atoi(string str, ref int pos, out int n)
        {
            bool res = false;
            n = 0;
            if (GotoPrintChar(str, ref pos, str.Length) == '\0')
                return false;
            while (pos < str.Length && char.IsDigit(str[pos]))
            {
                n = n * 10 + ((int)str[pos] - (int)'0');
                res = true;
            }
            return res;
        }

        private static bool TestString(string str, ref int pos, string strTest)
        {
            if (GotoPrintChar(str, ref pos, str.Length) == '\0' || pos + strTest.Length < str.Length)
                return false;
            for (int pos2 = 0; pos2 < strTest.Length; pos2++)
                if (str[pos++] != strTest[pos2])
                    return false;
            return true;
        }
    }
}
