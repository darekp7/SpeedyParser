using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace TiramisuParser
{
    class TiramisuParser
    {
        private bool FCaseSensitive;
        private bool FBracketSensitive;
        private bool FQuotesSensitive;
        private bool FDoubleQuoteIsCpp;
        private bool FSingleQuoteIsCpp;
        private Dictionary<string, List<string>> FOutput;
        private string FUnboundVariableName = null;
        private int FUnboundVarStart = 0;

        private struct TrmPattern
        {
            public string StrPattern;
            public int TokensStart;
            public int TokensEnd;
            public string[] VarNames;
            public string[] VarTypes;

            public TrmPattern(string pattern)
            {
                StrPattern = pattern = pattern ?? "";
                TokensEnd = pattern.LastIndexOf("->");
                if (TokensEnd < 0)
                    TokensEnd = pattern.Length;
                TokensStart = 0;
                GotoPrintChar(pattern, ref TokensStart, TokensEnd);
            }

            public bool MatchTokensAtPos(string str, ref int str_pos, int str_end, bool caseSensitive)
            {
                string dummy;
                return MatchTokensAtPos(str, ref str_pos, str_end, caseSensitive, false, out dummy);
            }

            public bool MatchTokensAtPos(string str, ref int str_pos, int str_end, bool caseSensitive, out string matchedStr)
            {
                return MatchTokensAtPos(str, ref str_pos, str_end, caseSensitive, true, out matchedStr);
            }

            private bool MatchTokensAtPos(string str, ref int str_pos, int str_end, bool caseSensitive, 
                bool returnMatched, out string matchedStr)
            {
                int pattern_pos = TokensStart;
                int saveStart = str_pos;
                while (GotoPrintChar(StrPattern, ref pattern_pos, TokensEnd) != '\0')
                    if (!MatchToken(str, ref str_pos, str_end, caseSensitive, StrPattern, ref pattern_pos, TokensEnd))
                    {
                        matchedStr = "";
                        str_pos = saveStart;
                        return false;
                    }
                matchedStr = returnMatched ? str.Substring(saveStart, str_pos - saveStart).Trim() : "";
                return true;
            }
        }

        private bool TryMatchStart(string str, ref int str_pos, int str_end, TrmPattern[] patterns, int startPos, int endPos)
        {
            for (int i_patt = startPos; i_patt < endPos; i_patt++)
            {
                var pattern = patterns[i_patt];
                if (pattern.TokensStart < pattern.TokensEnd)
                    if (FUnboundVariableName == null)
                    {
                        if (!pattern.MatchTokensAtPos(str, ref str_pos, str_end, FCaseSensitive))
                            return false;
                    }
                    else
                    {
                        while (!pattern.MatchTokensAtPos(str, ref str_pos, str_end, FCaseSensitive))
                            if (!GotoNextMatchPos(str, ref str_pos, str_end))
                                return false;
                        SaveVariable(FUnboundVariableName, str.Substring(FUnboundVarStart, str_pos - FUnboundVarStart));
                        FUnboundVariableName = null;
                        FUnboundVarStart = str_pos;
                    }
                if (pattern.VarNames != null)
                {
                    for(int i_var=0; i_var < pattern.VarNames.Length; i_var++)
                        switch (pattern.VarTypes[i_var])
                        {
                            case "unbound":
                                if (FUnboundVariableName != null)
                                    SaveVariable(FUnboundVariableName, str.Substring(FUnboundVarStart, str_pos - FUnboundVarStart));
                                GotoPrintChar(str, ref str_pos, str_end);
                                FUnboundVariableName = pattern.VarNames[i_var];
                                FUnboundVarStart = str_pos;
                                break;
                            case "digit":
                            case "digits":
                                if (!MatchBoundVariable(str, ref str_pos, str_end, pattern.VarNames[i_var], MatchDigits))
                                    return false;
                                break;
                            case "alnum":
                                if (!MatchBoundVariable(str, ref str_pos, str_end, pattern.VarNames[i_var], MatchAlnum))
                                    return false;
                                break;
                            case "alpha":
                                if (!MatchBoundVariable(str, ref str_pos, str_end, pattern.VarNames[i_var], MatchAlpha))
                                    return false;
                                break;
                            case "ident":
                                if (!MatchBoundVariable(str, ref str_pos, str_end, pattern.VarNames[i_var], MatchIdent))
                                    return false;
                                break;
                        }
                }
            }
            return true;
        }

        private void SaveVariable(string varName, string varValue)
        {
            if (varName != null && varName[0] != '_' && FOutput != null)
                FOutput[varName].Add((varValue ?? "").Trim());
        }

        private static bool MatchToken(string str, ref int str_pos, int str_end, bool caseSensitive, string pattern, ref int pattern_pos, int pattern_end)
        {
            if (GotoPrintChar(str, ref str_pos, str_end) == '\0')
                return GotoPrintChar(pattern, ref pattern_pos, pattern_end) == '\0';
            char c = GotoPrintChar(pattern, ref pattern_pos, pattern_end);
            if (IsIdentChar(c) && str_pos > 0 && IsIdentChar(str[str_pos - 1]))
                return false;
            while (pattern_pos < pattern_end && !char.IsWhiteSpace(pattern[pattern_pos]))
            {
                c = pattern[pattern_pos];
                bool eq = str_pos < str_end && (c == str[str_pos] || !caseSensitive && char.ToUpper(c) == char.ToUpper(str[str_pos]));
                if (!eq)
                    return false;
            }
            if (IsIdentChar(c) && str_pos < str.Length && IsIdentChar(str[str_pos]))
                return false;
            return true;
        }

        public static bool IsIdentChar(char c)
        {
            return c == '_' || char.IsLetterOrDigit(c);
        }

        private bool GotoNextMatchPos(string str, ref int pos, int end)
        {
            char c = GotoPrintChar(str, ref pos, end);
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
                    {
                        if (!FindClosingBracket(str, ref pos, end))
                            return false;
                        pos++;
                    }
                    return GotoPrintChar(str, ref pos, end) != '\0';
                case '"':
                case '\'':
                    if (FQuotesSensitive)
                    {
                        if (!FindClosingQuote((c == '"')? FDoubleQuoteIsCpp : FSingleQuoteIsCpp, str, ref pos, end))
                            return false;
                        pos++;
                    }
                    return GotoPrintChar(str, ref pos, end) != '\0';
                default:
                    pos++;
                    return GotoPrintChar(str, ref pos, end) != '\0';
            }
        }

        private bool FindClosingBracket(string str, ref int pos, int end)
        {
            char bracket = str[pos++];
            char closing_bracket;
            switch (bracket)
            {
                case '(':
                    closing_bracket = ')';
                    break;
                case '[':
                    closing_bracket = ']';
                    break;
                case '{':
                    closing_bracket = '}';
                    break;
                default:
                    return false;
            }
            int n = 1;
            for(; pos < end; pos++)
                switch (str[pos])
                {
                    case '"':
                    case '\'':
                        if (FQuotesSensitive &&
                            !FindClosingQuote((str[pos] == '"') ? FDoubleQuoteIsCpp : FSingleQuoteIsCpp, str, ref pos, end))
                        {
                            return false;
                        }
                        break;
                    default:
                        if (str[pos] == bracket)
                            n++;
                        else if (str[pos] == closing_bracket && --n <= 0)
                            return true;
                        break;
                }
            return false;
        }

        public static bool FindClosingQuote(bool styleCpp, string str, ref int pos, int end)
        {
            char quoteChar = str[pos++];
            for (; pos < end; pos++)
            {
                char c = str[pos];
                if (c == quoteChar)
                    return true;
                else if (styleCpp && c == '\\')
                    pos++;
            }
            return false;
        }

        private delegate bool MatchBoundFun(string str, ref int pos, int end);

        private bool MatchBoundVariable(string str, ref int str_pos, int str_end, string varName, MatchBoundFun matchFun)
        {
            GotoPrintChar(str, ref str_pos, str_end);
            if (FUnboundVariableName == null)
            {
                int start_pos = str_pos;
                if (!matchFun(str, ref str_pos, str_end))
                    return false;
                SaveVariable(varName, str.Substring(start_pos, str_pos - start_pos));
                return true;
            }
            else
            {
                int savePos = str_pos;
                while (!matchFun(str, ref str_pos, str_end))
                {
                    str_pos = savePos;
                    if (!GotoNextMatchPos(str, ref str_pos, str_end))
                        return false;
                    savePos = str_pos;
                }
                SaveVariable(FUnboundVariableName, str.Substring(FUnboundVarStart, savePos - FUnboundVarStart));
                FUnboundVariableName = null;
                FUnboundVarStart = savePos;
                SaveVariable(varName, str.Substring(savePos, str_pos - savePos));
                return true;
            }
        }

        private static bool MatchDigits(string str, ref int pos, int end)
        {
            char c = GotoPrintChar(str, ref pos, end);
            if (c == '\0' || !char.IsDigit(c) || pos > 0 && IsIdentChar(str[pos-1]))
                return false;
            while (pos < end && char.IsDigit(str[pos]))
                pos++;
            return pos >= end || !IsIdentChar(str[pos]);
        }

        private static bool MatchAlnum(string str, ref int pos, int end)
        {
            char c = GotoPrintChar(str, ref pos, end);
            if (c == '\0' || !char.IsLetterOrDigit(c) || pos > 0 && IsIdentChar(str[pos - 1]))
                return false;
            while (pos < end && char.IsLetterOrDigit(str[pos]))
                pos++;
            return pos >= end || !IsIdentChar(str[pos]);
        }

        private static bool MatchAlpha(string str, ref int pos, int end)
        {
            char c = GotoPrintChar(str, ref pos, end);
            if (c == '\0' || !char.IsLetter(c) || pos > 0 && IsIdentChar(str[pos - 1]))
                return false;
            while (pos < end && char.IsLetter(str[pos]))
                pos++;
            return pos >= end || !IsIdentChar(str[pos]);
        }

        private static bool MatchIdent(string str, ref int pos, int end)
        {
            char c = GotoPrintChar(str, ref pos, end);
            if (c == '\0' || !IsIdentChar(c) || pos > 0 && IsIdentChar(str[pos - 1]))
                return false;
            while (pos < end && IsIdentChar(str[pos]))
                pos++;
            return true;
        }

        private static char GotoPrintChar(string str, ref int pos, int end)
        {
            while (pos < end && Char.IsWhiteSpace(str[pos]))
                pos++;
            return (pos < end) ? str[pos] : '\0';
        }
    }
}
