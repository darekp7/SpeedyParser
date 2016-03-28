using System;
using System.Collections.Generic;
using System.Text;

namespace T102
{
    enum T102TokenKind
    {
        TOKEN_SINGLE_QUOTED_STRING,
        TOKEN_DOUBLE_QUOTED_STRING,
        TOKEN_NUMBER,
        TOKEN_IDENT,
        TOKEN_OPERATOR,
        TOKEN_BRACKET, // https://en.wikipedia.org/wiki/Bracket
        TOKEN_EOF
    }

    /*struct T102Value
    {
        public T102TokenKind Token;
        public string Text;
        public T102Value[] SubItems;
        private T102SrcLine[] Source;
        private long PosInSrc;
    }*/

    class T102SourceLine
    {
        public string Text;
        public string FileName;
        public int StartLine;
        public int EndLine;

        public T102SourceLine(string txt, string fname, int line_no)
        {
            Text = (txt ?? "").TrimEnd();
            FileName = fname ?? "";
            StartLine = line_no;
            EndLine = line_no + 1;
        }

        public T102SourceLine Clone()
        {
            var res = new T102SourceLine(Text, FileName, StartLine);
            res.EndLine = EndLine;
            return res;
        }

        public T102SourceLine CloneSetText(string txt)
        {
            var res = Clone();
            res.Text = txt;
            return res;
        }

        public ExceptionAtLn CreateException(string msg)
        {
            string msg2 = (FileName != null && FileName.Trim() != "")
                ? string.Format("File {0}, line {1}: {2}", FileName, GetLineDescr(), msg)
                : string.Format("Line {0}: {1}", GetLineDescr(), msg);
            return new ExceptionAtLn(msg2, this);
        }

        public string GetLineDescr()
        {
            return (EndLine > StartLine + 1)
                ? string.Format("{0}..{1}", StartLine + 1, EndLine + 1)
                : string.Format("{0}", StartLine + 1);
        }

        public class ExceptionAtLn : Exception
        {
            public readonly T102SourceLine SourceLine;

            public ExceptionAtLn(string msg, T102SourceLine src)
                : base(msg)
            {
                SourceLine = src;
            }
        }
    }

    class T102SimpleLexer // lexer is not actually a parser, it only converts string to list of tokens
    {
        private string InternalString;
        private int Pos = 0;

        public string Text = "";
        public T102TokenKind Token = T102TokenKind.TOKEN_EOF;

        public bool SkipNL = true;
        public char SingleQuoteEscChar = '\\';
        public char DoubleQuoteEscChar = '\\';
        public bool AllowPeriodForNumbers = false;
        public bool AllowPipeBracket = false;
        public bool CanThrow = true;

        public T102SimpleLexer(string ln)
        {
            InternalString = ln;
        }

        public bool ReadToken(string extraIdentChars, string[] knownOperators)
        {
            Token = T102BasicLexerAPI.GetToken(InternalString, ref Pos, extraIdentChars, SingleQuoteEscChar, DoubleQuoteEscChar, knownOperators,
                    skipNL: SkipNL, allowPeriodForNumbers: AllowPeriodForNumbers, allowPipeBracket: AllowPipeBracket, canThrow: CanThrow,
                    text: out Text);
            return Token != T102TokenKind.TOKEN_EOF;
        }

        public bool TestToken(string testString, string extraIdentChars, string[] knownOperators)
        {
            if (T102BasicLexerAPI.TestToken(InternalString, ref Pos, testString, extraIdentChars, SingleQuoteEscChar, DoubleQuoteEscChar, knownOperators,
                        skipNL: SkipNL, allowPeriodForNumbers: AllowPeriodForNumbers, allowPipeBracket: AllowPipeBracket,
                        canThrow: CanThrow, tokenKind: out Token))
            {
                Text = testString;
                return true;
            }
            return false;
        }

        public string ReadToDelimeter(char delimeter, bool eatDelimeter)
        {
            return T102BasicLexerAPI.ReadToDelimeter(InternalString, ref Pos, delimeter, eatDelimeter);
        }

        public string ReadToEnd()
        {
            return T102BasicLexerAPI.ReadToEnd(InternalString, ref Pos);
        }

        public char GotoPrintChar()
        {
            return T102BasicLexerAPI.GotoPrintChar(InternalString, ref Pos, skipNL: SkipNL);
        }

        public string GetPrintableSequence()
        {
            Text = T102BasicLexerAPI.GetPrintableSequence(InternalString, ref Pos, skipNL: SkipNL);
            Token = (Text == "") ? T102TokenKind.TOKEN_EOF : T102TokenKind.TOKEN_IDENT;
            return Text;
        }

        public string GetPrintableSequenceOrQuotedStr()
        {
            char c = GotoPrintChar();
            if (c == '\'' || c == '"')
            {
                ReadToken("_", null);
                return Text;
            }
            return GetPrintableSequence();
        }

        public bool TestString(string strTest)
        {
            return T102BasicLexerAPI.TestString(strTest, InternalString, ref Pos, skipNL: SkipNL);
        }
    }

    static class T102BasicLexerAPI
    {
        public static bool TestToken(string str, ref int pos, string strTest, string extraIdentChars,
            char singleQuoteEscChar, char doubleQuoteEscChar, string[] knownOperators,
            bool skipNL, bool allowPeriodForNumbers, bool allowPipeBracket, bool canThrow, out T102TokenKind tokenKind)
        {
            extraIdentChars = extraIdentChars ?? "";
            strTest = strTest ?? "";
            str = str ?? "";
            GotoPrintChar(str, ref pos, skipNL);
            int savePos = pos;
            string token;
            if ((tokenKind = GetToken(str, ref pos, extraIdentChars, singleQuoteEscChar, doubleQuoteEscChar, knownOperators,
                        skipNL, allowPeriodForNumbers, allowPipeBracket, canThrow, out token)) != T102TokenKind.TOKEN_EOF
               && token == strTest)
            {
                return true;
            }
            else
            {
                tokenKind = T102TokenKind.TOKEN_EOF;
                pos = savePos;
                return false;
            }
        }

        public static T102TokenKind GetToken(string str, ref int pos, string extraIdentChars,
            char singleQuoteEscChar, char doubleQuoteEscChar, string[] knownOperators,
            bool skipNL, bool allowPeriodForNumbers, bool allowPipeBracket, bool canThrow,
            out string text)
        {
            extraIdentChars = extraIdentChars ?? "";
            str = str ?? "";
            char c = GotoPrintChar(str, ref pos, skipNL);
            switch (c)
            {
                case '\0':
                    if (canThrow)
                        throw new Exception("Unexpected end of line");
                    text = "";
                    return T102TokenKind.TOKEN_EOF;
                case '\'':
                    bool dummy1;
                    pos++;
                    text = GetStringLiteralBody(str, ref pos, '\'', singleQuoteEscChar, canThrow, true, out dummy1);
                    return T102TokenKind.TOKEN_SINGLE_QUOTED_STRING;
                case '"':
                    bool dummy2;
                    pos++;
                    text = GetStringLiteralBody(str, ref pos, '"', doubleQuoteEscChar, canThrow, true, out dummy2);
                    return T102TokenKind.TOKEN_DOUBLE_QUOTED_STRING;
                case '(':
                case ')':
                case '[':
                case ']':
                case '{':
                case '}':
                    text = str[pos++].ToString();
                    return T102TokenKind.TOKEN_BRACKET;
                default:
                    if (Char.IsDigit(c))
                    {
                        int startPos = pos;
                        string ident = TryParseNumber(str, ref pos, allowPeriodForNumbers, skipNL);
                        if (ident == "")
                        {
                            if (canThrow)
                                throw new Exception("Invalid number format for a number starting with '" + c + "'");
                            pos = startPos;
                        }
                        text = str.Substring(startPos, pos - startPos);
                        return (text.Trim() == "") ? T102TokenKind.TOKEN_EOF : T102TokenKind.TOKEN_NUMBER;
                    }
                    else if (IsIdentChar(c, extraIdentChars))
                    {
                        text = GetIdent(str, ref pos, extraIdentChars, skipNL, true);
                        return T102TokenKind.TOKEN_IDENT;
                    }
                    else
                    {
                        if (knownOperators != null)
                            for (int i = 0; i < knownOperators.Length; i++)
                                if (TestString(knownOperators[i], str, ref pos, skipNL))
                                {
                                    text = knownOperators[i];
                                    return T102TokenKind.TOKEN_OPERATOR;
                                }
                        text = str[pos++].ToString();
                        return (c == '|' && allowPipeBracket) ? T102TokenKind.TOKEN_BRACKET : T102TokenKind.TOKEN_OPERATOR;
                    }
            }
        }

        public static string TryParseNumber(string str, ref int pos, bool allowPeriodForNumbers, bool skipNL)
        {
            char c = GotoPrintChar(str, ref pos, skipNL);
            if (c == '\0' || !char.IsDigit(c))
                return "";
            if (++pos >= str.Length)
                return c.ToString();
            StringBuilder sb = new StringBuilder(c);
            int savePos = pos - 1;
            if (str[pos] == 'x' || str[pos] == 'X')
            {
                sb.Append(str[pos++]);
                while (pos < str.Length && (str[pos] == '_' || char.IsDigit(str[pos]) || "abcdefABCDEF".IndexOf(str[pos]) >= 0))
                    sb.Append(str[pos++]);
                if (pos < str.Length && char.IsLetter(str[pos]))
                {
                    pos = savePos;
                    return "";
                }
                return sb.ToString().Replace("_", "");
            }
            else
            {
                while (pos < str.Length && (str[pos] == '_' || char.IsDigit(str[pos])))
                    sb.Append(str[pos++]);
                if (pos < str.Length && str[pos] == '.')
                {
                    sb.Append(str[pos++]);
                    while (pos < str.Length && (str[pos] == '_' || char.IsDigit(str[pos])))
                        sb.Append(str[pos++]);
                    if (allowPeriodForNumbers && pos < str.Length - 1 && str[pos] == '(' && char.IsDigit(str[pos + 1]))
                    {
                        sb.Append(str[pos++]);
                        while (pos < str.Length && (str[pos] == '_' || char.IsDigit(str[pos])))
                            sb.Append(str[pos++]);
                        if (pos >= str.Length || str[pos] != ')')
                        {
                            pos = savePos;
                            return "";
                        }
                    }
                }
                if (pos < str.Length && (str[pos] == 'e' || str[pos] == 'E'))
                {
                    sb.Append(str[pos++]);
                    if (pos < str.Length && (str[pos] == '+' || str[pos] == '-'))
                    {
                        sb.Append(str[pos++]);
                        if (pos >= str.Length || !char.IsDigit(str[pos + 1]))
                        {
                            pos = savePos;
                            return "";
                        }
                    }
                    while (pos < str.Length && (str[pos] == '_' || char.IsDigit(str[pos])))
                        sb.Append(str[pos++]);
                    if (pos < str.Length && (char.IsLetter(str[pos]) || str[pos] == '_'))
                    {
                        pos = savePos;
                        return "";
                    }
                }
                return sb.ToString().Replace("_", "");
            }
        }

        public static bool IsWhiteSpaceOrEmpty(string str)
        {
            if (str == null)
                return true;
            for (int i = 0; i < str.Length; i++)
                if (!Char.IsWhiteSpace(str[i]))
                    return false;
            return true;
        }

        public static bool IsIdentifier(string str, string extraIdentChars)
        {
            if (str == null)
                return false;
            for (int i = 0; i < str.Length; i++)
                if (!IsIdentChar(str[i], extraIdentChars))
                    return false;
            return true;
        }

        public static string GetPrintableSequence(string str, ref int pos, bool skipNL)
        {
            if (GotoPrintChar(str, ref pos, skipNL) == '\0')
                return "";
            int startPos = pos;
            while (pos < str.Length && !char.IsWhiteSpace(str[pos]))
                pos++;
            return str.Substring(startPos, pos - startPos);
        }

        public static string TestToDelimeter(string str, ref int pos, char delimeter, bool eatDelimeter)
        {
            int savePos = pos;
            bool delimeterFound;
            string res = ReadToDelimeter(str, ref pos, delimeter, eatDelimeter, out delimeterFound);
            if (delimeterFound)
                return res;
            else
            {
                pos = savePos;
                return "";
            }
        }

        public static string ReadToDelimeter(string str, ref int pos, char delimeter, bool eatDelimeter)
        {
            bool delimeterFound;
            return ReadToDelimeter(str, ref pos, delimeter, eatDelimeter, out delimeterFound);
        }

        public static string ReadToDelimeter(string str, ref int pos, char delimeter, bool eatDelimeter, out bool delimeterFound)
        {
            StringBuilder res = new StringBuilder();
            while (pos < str.Length && str[pos] != delimeter)
                res.Append(str[pos++]);
            delimeterFound = false;
            if (pos < str.Length && str[pos] == delimeter)
            {
                delimeterFound = true;
                if (eatDelimeter)
                    pos++;
            }
            return res.ToString();
        }

        public static string ReadToEnd(string str, ref int pos)
        {
            str = str ?? "";
            if (pos >= str.Length)
                return "";
            string res = str.Substring(pos).Trim();
            pos = str.Length;
            return res;
        }

        public static string GetIdent(string str, ref int pos, string extraIdentChars, bool skipNL, bool canThrow)
        {
            if (!IsIdentChar(GotoPrintChar(str, ref pos, skipNL), extraIdentChars))
            {
                if (canThrow)
                    throw new Exception("Expected identifier" + ((pos < str.Length) ? " near '" + str[pos] + "'" : " at the end of line"));
                return "";
            }
            StringBuilder res = new StringBuilder();
            while (pos < str.Length && IsIdentChar(str[pos], extraIdentChars))
                res.Append(str[pos++]);
            return res.ToString();
        }

        private static string GetStringLiteralBody(string str, ref int pos, char quoteChar, char escapeChar, bool canThrow, bool autoUnescape, out bool missingQuouteAtEnd)
        {
            StringBuilder res = new StringBuilder();
            missingQuouteAtEnd = false;
            if (quoteChar == escapeChar)
            {
                for (; pos < str.Length; pos++)
                    if (str[pos] == quoteChar && pos < str.Length - 1 && str[pos + 1] == quoteChar)
                        res.Append(str[pos++]);
                    else if (str[pos] == quoteChar)
                    {
                        pos++;
                        return res.ToString();
                    }
                    else
                        res.Append(str[pos]);
                missingQuouteAtEnd = true;
                if (canThrow)
                    throw new Exception("Missing " + quoteChar + " at the end of string literal");
                return res.ToString();
            }
            else
            {
                for (; pos < str.Length; pos++)
                    if (str[pos] == escapeChar)
                        res.Append(str[pos++]);
                    else if (str[pos] == quoteChar)
                    {
                        pos++;
                        return (autoUnescape && escapeChar == '\\') ? Unescape(res.ToString()) : res.ToString();
                    }
                    else
                        res.Append(str[pos]);
                missingQuouteAtEnd = true;
                if (canThrow)
                    throw new Exception("Missing " + quoteChar + " at the end of string literal");
                return res.ToString();
            }
        }

        public static string Unescape(string str /*, bool extendedUnescape*/)
        {
            StringBuilder res = new StringBuilder();
            for (int i = 0; i < str.Length; i++)
                if (str[i] != '\\')
                    res.Append(str[i]);
                else
                {
                    if (i == str.Length - 1)
                        return res.Append('\\').ToString();
                    switch (str[++i])
                    {
                        case '\'':
                            res.Append('\'');
                            break;
                        case '"':
                            res.Append('"');
                            break;
                        case '\\':
                            res.Append('\\');
                            break;
                        case '0':
                            res.Append('\0');
                            break;
                        case 'a':
                            res.Append('\a');
                            break;
                        case 'b':
                            res.Append('\b');
                            break;
                        case 'f':
                            res.Append('\f');
                            break;
                        case 'n':
                            res.Append('\n');
                            break;
                        case 'r':
                            res.Append('\r');
                            break;
                        case 't':
                            res.Append('\t');
                            break;
                        /*case 'u':
                        case 'U':
                        case 'x':
                            break; */
                        case 'v':
                            res.Append('\v');
                            break;
                        /*case 'g':
                            res.Append(extendedUnescape ? '[' : str[i]);
                            break;
                        case 'h':
                            res.Append(extendedUnescape ? ']' : str[i]);
                            break;
                        case 'i':
                            res.Append(extendedUnescape ? '{' : str[i]);
                            break;
                        case 'j':
                            res.Append(extendedUnescape ? '}' : str[i]);
                            break;
                        case 'k':
                            res.Append(extendedUnescape ? '$' : str[i]);
                            break;
                        case 'l':
                            res.Append(extendedUnescape ? '|' : str[i]);
                            break;
                        case 'm':
                            res.Append(extendedUnescape ? '=' : str[i]);
                            break;*/
                        default:
                            res.Append(str[i]);
                            break;
                    }
                }
            return res.ToString();
        }

        public static string Escape(string str, char quoteChar)
        {
            StringBuilder res = new StringBuilder();
            if(quoteChar == '\'' || quoteChar == '"')
                res.Append(quoteChar);
            if(str != null)
                for(int i=0; i < str.Length; i++)
                    switch (str[i])
                    {
                        case '\0':
                            res.Append("\\0");
                            break;
                        case '\a':
                            res.Append("\\a");
                            break;
                        case '\b':
                            res.Append("\\b");
                            break;
                        case '\f':
                            res.Append("\\f");
                            break;
                        case '\n':
                            res.Append("\\n");
                            break;
                        case '\r':
                            res.Append("\\r");
                            break;
                        case '\t':
                            res.Append("\\t");
                            break;
                        case '\v':
                            res.Append("\\v");
                            break;
                        case '"':
                            res.Append((quoteChar != '\'')? "\\\"" : "\"");
                            break;
                        case '\'':
                            res.Append((quoteChar != '"')? "\\'" : "'");
                            break;
                        case '\\':
                            res.Append("\\\\");
                            break;
                        default:
                            res.Append(str[i]);
                            break;
                    }
            if(quoteChar == '\'' || quoteChar == '"')
                res.Append(quoteChar);
            return res.ToString();
        }

        public static bool TestString(string strTest, string str, ref int pos, bool skipNL)
        {
            strTest = strTest ?? "";
            str = str ?? "";
            if (GotoPrintChar(str, ref pos, skipNL) == '\0')
                return false;
            if (pos + strTest.Length > str.Length)
                return false;
            for (int i = 0; i < strTest.Length; i++)
                if (str[pos + i] != strTest[i])
                    return false;
            pos += strTest.Length;
            return true;
        }

        /// <summary>
        /// Returns index of first char beeing not element of a list passed as second parameter. Or (-1) if such index doesn't exists.
        /// Space in second parameter denotes all whitespaces.
        /// </summary>
        public static int FirstIndexNot(string str, string excludeList)
        {
            for(int i=0; i < str.Length; i++)
                if(!(excludeList.IndexOf(str[i]) >= 0 || excludeList.IndexOf(' ') >= 0 && char.IsWhiteSpace(str[i])))
                    return i;
            return -1;
        }

        /// <summary>
        /// Returns listItemBegin || fieldName != ""
        /// </summary>
        public static bool YamlRecognizeLine(string strLine, bool skipNL, out bool listItemBegin, out string fieldName, out string value)
        {
            int pos = 0;
            if (GotoPrintChar(strLine, ref pos, skipNL) == '\0')
            {
                fieldName = value = "";
                return listItemBegin = false;
            }
            listItemBegin = TestString("-", strLine, ref pos, skipNL);
            if (listItemBegin)
            {
                if (TestString(":", strLine, ref pos, skipNL))
                {
                    fieldName = "";
                    value = strLine.Substring(pos).Trim();
                    return true;
                }
                if (GotoPrintChar(strLine, ref pos, skipNL) == '\0')
                {
                    fieldName = value = "";
                    return true;
                }
            }
            char c = GotoPrintChar(strLine, ref pos, skipNL);
            switch (c)
            {
                case '\'':
                case '"':
                    bool missingQuoteAtEnd;
                    int savePos = ++pos;
                    string fld1 = GetStringLiteralBody(strLine, ref pos, c, (c == '\'') ? c : '\\', canThrow: false, autoUnescape: true, missingQuouteAtEnd: out missingQuoteAtEnd);
                    if (!missingQuoteAtEnd && TestString(":", strLine, ref pos, skipNL))
                    {
                        fieldName = fld1;
                        value = strLine.Substring(pos).Trim();
                        return true;
                    }
                    else
                    {
                        fieldName = "";
                        value = listItemBegin ? strLine.Substring(savePos).Trim() : "";
                        return listItemBegin;
                    }
                default:
                    if (char.IsLetter(c) || c == '_')
                    {
                        GetToken(strLine, ref pos, extraIdentChars: "_", singleQuoteEscChar: '\'', doubleQuoteEscChar: '"', knownOperators: null,
                                skipNL: skipNL, allowPeriodForNumbers: false, allowPipeBracket: false, canThrow: false,
                                text: out fieldName);
                        if (TestString(":", strLine, ref pos, skipNL))
                        {
                            value = strLine.Substring(pos).Trim();
                            return true;
                        }
                    }
                    fieldName = "";
                    value = listItemBegin ? strLine.Substring(pos).Trim() : "";
                    return listItemBegin;
            }
        }

        public static bool IsOperatorChar(char c, string extraIdentChars)
        {
            return "`~!@#$%^&*()-=+{}[]\\|;:'\",<.>/?".IndexOf(c) >= 0 && extraIdentChars.IndexOf(c) < 0;
        }

        public static bool IsIdentChar(char c, string extraIdentChars)
        {
            if (extraIdentChars.IndexOf(c) >= 0 || Char.IsDigit(c))
                return true;
            if (Char.IsWhiteSpace(c) || c == '\0')
                return false;
            bool isLetter = !IsOperatorChar(c, extraIdentChars);
            return isLetter;
        }

        public static char GotoPrintChar(string str, ref int pos, bool skipNL)
        {
            while (pos < str.Length && Char.IsWhiteSpace(str[pos]))
            {
                if (str[pos] == '\n' && !skipNL)
                    return '\n';
                pos++;
            }
            return (pos < str.Length) ? str[pos] : '\0';
        }

        /// <summary>
        /// String needs to be quoted if is difficult to write in single line, i.e.
        /// - is empty
        /// - starts or ends with whitespaces
        /// - contains newlines or other escape sequences
        /// </summary>
        public static bool NeedsQuotation(string str)
        {
            if (str == null || str == "" || str != str.Trim())
                return true;
            for (int i = 0; i < str.Length; i++)
                if (str[i] != ' ' && (Char.IsControl(str[i]) || Char.IsWhiteSpace(str[i])))
                    return true;
            return false;
        }
    }

#if DLA_NET_40_LUB_WYZEJ
    struct T102Number
    {
        private T102Integer Numerator;  // licznik
        private T102Integer Denominator;  // mianownik

        public enum NumberFormat
        {
            Fractional,
            Mixed,
            DecimalWithotPeriod,
            WithPeriod
        }

        public T102Number(T102Integer num, T102Integer denum, bool simplify)
        {
            if (denum.Spaceship(0) >= 0)
            {
                Numerator = num;
                Denominator = denum;
            }
            else
            {
                Numerator = num.UnaryMinus();
                Denominator = denum.UnaryMinus();
            }
            if (simplify)
                InternalSimplify();
        }

        public override int GetHashCode()
        {
            return Numerator.GetHashCode() ^ Denominator.GetHashCode();
        }

        public bool IsEqual(long n)
        {
            return Numerator.IsEqual(n) && Denominator.IsEqual(1);
        }

        public override string ToString()
        {
            return ToString(NumberFormat.Fractional, 6);
        }

        public string ToString(NumberFormat format, int afterDot)
        {
            if (Denominator.IsEqual(0))
                return (Numerator.Spaceship(0) >= 0) ? "infinity" : "-infinity";
            if (Denominator.IsEqual(1))
                return Numerator.ToString();
            if (Numerator.Spaceship(0) < 0)
                return "-" + UnaryMinus().ToString(format, afterDot);
            if(afterDot < 0)
                afterDot = 0;
            T102Integer TEN = new T102Integer(10);
            switch (format)
            {
                case NumberFormat.Fractional:
                default:
                    return Numerator.ToString() + "/" + Denominator.ToString();
                case NumberFormat.Mixed:
                    T102Integer mf_rem;
                    T102Integer mf_div = Numerator.DivRem(Denominator, out mf_rem);
                    return (mf_div.Equals(0)) ? Numerator.ToString() + "/" + Denominator.ToString() : string.Format("({0}+{1}/{2})", mf_div.ToString(), mf_rem.ToString(), Denominator.ToString());
                case NumberFormat.DecimalWithotPeriod:
                    T102Integer FIVE = new T102Integer(5);
                    T102Integer dummy1, dummy2;
                    T102Integer dn_div = Numerator.Multiply(TEN.Power(afterDot + 1)).DivRem(Denominator, out dummy1).Add(FIVE).DivRem(TEN, out dummy2);
                    if (afterDot <= 0)
                        return dn_div.ToString();
                    string str = AddZerosBefore(dn_div.ToString(), afterDot + 1);
                    string str_before = str.Substring(0, str.Length - afterDot);
                    string str_after = str.Substring(str.Length - afterDot);
                    return EatFinalZerosAfterDot(str_before + "." + str_after);
                case NumberFormat.WithPeriod:
                    T102Integer rem;
                    T102Integer before = Numerator.DivRem(Denominator, out rem);
                    if (rem.IsEqual(0))
                        return before.ToString();
                    T102Number num_after = new T102Number(rem, Denominator, true);
                    string res = before.ToString() + ".";
                    int digits_after = Denom2PowerOfTen(num_after.Denominator);
                    if (digits_after > 0)
                    {
                        num_after = new T102Number(num_after.Numerator.Multiply(TEN.Power(digits_after)), num_after.Denominator, true);
                        T102Integer x_after = num_after.Numerator.DivRem(num_after.Denominator, out rem);
                        res += AddZerosBefore(x_after.ToString(), digits_after);
                        if (rem.IsEqual(0))
                            return EatFinalZerosAfterDot(res);
                        num_after = new T102Number(rem, num_after.Denominator, true);
                    }

                    
                    // todo
                    return res;
            }
    


            /*T102Number num_rem = new T102Number(n_rem, Denominator, true);
            T102Number TEN = new T102Number(new T102Integer(10), new T102Integer(1), false);
            StringBuilder sb = new StringBuilder(n_div.ToString()).Append(".");
            for (int i = 0; i < 6; i++)
            {
                num_rem = num_rem.Multiply(TEN);
                T102Integer n1 = num_rem.Numerator.DivRem(num_rem.Denominator, out n_rem);
                sb.Append(n1.ToString());

            }*/
        }

        private static string AddZerosBefore(string str, int minlen)
        {
            return (str.Length >= minlen) ? str : new string('0', minlen - str.Length) + str;
        }

        private static string EatFinalZerosAfterDot(string str)
        {
            int i_after = str.Length - 1;
            for (; i_after >= 0; i_after--)
                if (str[i_after] != '0')
                    break;
            if (i_after < 0)
                return "0";
            return (str[i_after] == '.')? str.Substring(0, i_after) : str.Substring(0, i_after + 1);
        }

        private static int Denom2PowerOfTen(T102Integer denom)
        {
            // http://www.matematyka.wroc.pl/ciekawieomatematyce/sie-powtarza-co-sie-powtarza-co
            //
            // Twierdzenie 1. Niech mianownik q ułamka (nieskracalnego) p/q nie dzieli się ani przez 2, ani przez 5. 
            // Wtedy rozwinięcie dziesiętne tego ułamka jest okresowe o okresie zaczynającym się bezpośrednio po przecinku i ma długość mniejszą niż q.
            //
            // Twierdzenie 2. Niech mianownik q ułamka nieskracalnego p/q będzie postaci q = 2^k * 5^n * s, gdzie s nie dzieli się ani przez 2, ani przez 5. 
            // Wtedy rozwinięcie dziesiętne tego ułamka jest okresowe o okresie zaczynającym się od m = 1 + max(k,n) miejsca po przecinku i ma długość mniejszą niż s.

            T102Integer FIVE = new T102Integer(5);
            T102Integer TWO = new T102Integer(2);
            T102Integer rem, d2;
            int n5 = 0;
            while ((d2 = denom.DivRem(FIVE, out rem)).Spaceship(0) > 0 && rem.IsEqual(0))
            {
                denom = d2;
                n5++;
            }
            int n2 = 0;
            while ((d2 = denom.DivRem(TWO, out rem)).Spaceship(0) > 0 && rem.IsEqual(0))
            {
                denom = d2;
                n2++;
            }
            return (n5 > n2) ? n5 : n2;
        }

        public bool IsEqual(T102Number n)
        {
            return Numerator.IsEqual(n.Numerator) && Denominator.IsEqual(n.Denominator);
        }

        private T102Number Simplify()
        {
            T102Number res = this;
            res.InternalSimplify();
            return res;
        }

        private void InternalSimplify()
        {
            if (Numerator.IsEqual(0))
                Denominator = new T102Integer(1);
            else if (Denominator.IsEqual(0))
            {
                switch (Numerator.Spaceship(0))
                {
                    case 0:
                        Numerator = new T102Integer(0);
                        Denominator = new T102Integer(1);
                        break;
                    case 1:
                        Numerator = new T102Integer(1);
                        break;
                    default:
                        Numerator = new T102Integer(-1);
                        break;
                }
            }
            else if (Denominator.Spaceship(1) > 0)
            {
                if (Numerator.IsLong() && Denominator.IsLong())
                {
                    long n_n = Numerator.ToLong();
                    long n_d = Denominator.ToLong();
                    var n_gcd = T102Integer.GcdLong(n_n, n_d);
                    if (n_gcd > 1)
                    {
                        Numerator = new T102Integer(n_n / n_gcd);
                        Denominator = new T102Integer(n_d / n_gcd);
                    }
                }
                else
                {
                    var n = Numerator.ToBigInteger();
                    var d = Denominator.ToBigInteger();
                    var gcd = System.Numerics.BigInteger.GreatestCommonDivisor(n, d);
                    if (gcd > 1)
                    {
                        Numerator = T102Integer.FromBigInteger(n / gcd);
                        Denominator = T102Integer.FromBigInteger(d / gcd);
                    }
                }
            }
        }

        public static bool TryParseNumber(string str, bool allowPeriod, out T102Number res)
        {
            int pos = 0;
            return TryParseNumber(str, ref pos, allowPeriod, out res);
        }

        public static bool TryParseNumber(string str, ref int pos, bool allowPeriod, out T102Number res)
        {
            bool dummy;
            return TryParseNumber(str, ref pos, allowPeriod, out res, out dummy);
        }

        public static bool TryParseNumber(string str, ref int pos, bool allowPeriod, out T102Number res, out bool read_to_end)
        {
            str = str ?? "";
            if (str.IndexOf("0x") >= 0 || str.IndexOf("0X") >= 0)
            {
                T102Integer n = new T102Integer(0);
                if (!T102Integer.TryParseHexInteger(str, ref pos, out n))
                {
                    res = new T102Number(new T102Integer(0), new T102Integer(1), false);
                    read_to_end = pos >= str.Length;
                    return false;
                }
                else
                {
                    res = new T102Number(n, new T102Integer(1), false);
                    read_to_end = pos >= str.Length;
                    return true;
                }
            }
            else
            {
                bool ret = TryParseDecimalNumber(str, ref pos, allowPeriod, out res);
                read_to_end = pos >= str.Length;
                return ret;
            }
        }

        private static bool TryParseDecimalNumber(string str, ref int pos, bool allowPeriod, out T102Number res)
        {
            // unary minus or plus before number is optional
            if (pos < str.Length - 1 && (str[pos] == '-' || str[pos] == '+') && str[pos + 1] != '-' && str[pos + 1] != '+')
            {
                bool b_minus = str[pos++] == '-';
                if (!TryParseDecimalNumber(str, ref pos, allowPeriod, out res))
                    return false;
                if(b_minus)
                    res = res.UnaryMinus();
                return true;
            }

            res = new T102Number(new T102Integer(0), new T102Integer(1), false);

            if (pos >= str.Length)
                return false;
            else
            {
                T102Integer n = new T102Integer(0);
                int dummy = 0;
                if (str[pos] != '.' && !T102Integer.QuickTryParseInt(str, ref pos, out n, out dummy))
                    return false;
                res = new T102Number(n, new T102Integer(1), false);
            }

            int n_digits_after_dot = 0;
            if (pos < str.Length && str[pos] == '.')
            {
                T102Integer num = new T102Integer(0);
                pos++;
                if (T102Integer.QuickTryParseInt(str, ref pos, out num, out n_digits_after_dot))
                {
                    T102Integer denom = (new T102Integer(10)).Power(n_digits_after_dot);
                    res = res.Add(new T102Number(num, denom, true));
                }
            }

            if (allowPeriod && pos < str.Length && str[pos] == '(')
            {
                T102Integer n_repetend = new T102Integer(0);
                int n_digits_repetend = 0;
                pos++;
                if (!T102Integer.QuickTryParseInt(str, ref pos, out n_repetend, out n_digits_repetend) || pos >= str.Length || str[pos] != ')')
                    return false;
                pos++;
                if (!n_repetend.IsEqual(0))
                {
                    T102Number true_repetend = new T102Number(n_repetend, new T102Integer(10).Power(n_digits_repetend), true);
                    T102Number a1 = new T102Number(n_repetend, new T102Integer(10).Power(n_digits_after_dot + n_digits_repetend), true);

                    // suma szeregu geometrycznego - sum of geometric series: a1 / (1 - r) (here r = 1/10)
                    // https://en.wikipedia.org/wiki/Geometric_series
                    // https://en.wikipedia.org/wiki/Repeating_decimal
                    T102Number the_sum = a1.Divide(new T102Number(new T102Integer(1), new T102Integer(1), false).Subtract(Pow10(-n_digits_repetend)));
                    res = res.Add(the_sum);                                                            
                }
            }

            if (pos < str.Length && (str[pos] == 'e' || str[pos] == 'E'))
            {
                T102Integer exponent = new T102Integer(0);
                int dummy;
                pos++;
                if (!T102Integer.QuickTryParseInt(str, ref pos, out exponent, out dummy) || !exponent.IsLong())
                    return false;
                res = res.Multiply(Pow10(exponent.ToLong()));
            }

            return true;
        }

        public static T102Number Pow10(long n)
        {
            T102Integer exponent = new T102Integer(10).Power((n < 0)? -n : n);
            return (n >= 0) ? new T102Number(exponent, new T102Integer(1), false) : new T102Number(new T102Integer(1), exponent, false);
        }

        public T102Number UnaryMinus()
        {
            if (Numerator.IsEqual(0))
                return this;
            return new T102Number(Numerator.UnaryMinus(), Denominator, false);
        }

        public T102Number Abs(out bool modified)
        {
            return new T102Number(Numerator.Abs(out modified), Denominator, false);
        }

        public T102Number Reciprocal()  // aka https://en.wikipedia.org/wiki/Multiplicative_inverse
        {
            if (Denominator.IsEqual(0))
                return new T102Number(new T102Integer(0), new T102Integer(1), false);
            return new T102Number(Denominator, Numerator, false);
        }

        public T102Number Add(T102Number n)
        {
            if (Denominator.IsEqual(n.Denominator))
                return new T102Number(Numerator.Add(n.Numerator), Denominator, true);
            if (Denominator.IsEqual(0))
                return this;
            if (n.Denominator.IsEqual(0))
                return n;
            if (Denominator.IsLong() && n.Denominator.IsLong())
            {
                long n1 = Denominator.ToLong();
                long n2 = n.Denominator.ToLong();
                long gcd = T102Integer.GcdLong(n1, n2);
                if (gcd > 1)
                {
                    long mult1 = n2 / gcd;
                    long mult2 = n1 / gcd;
                    T102Integer denom = Denominator.Multiply(new T102Integer(mult1));
                    return new T102Number(Numerator.Multiply(new T102Integer(mult1)).Add(n.Numerator.Multiply(new T102Integer(mult2))), denom, true);
                }
            }
            return new T102Number(Numerator.Multiply(n.Denominator).Add(n.Numerator.Multiply(Denominator)), Denominator.Multiply(n.Denominator), true);
        }

        public T102Number Subtract(T102Number n)
        {
            return Add(n.UnaryMinus());
        }

        public T102Number Multiply(T102Number n)
        {
            if (Denominator.IsEqual(0) && n.Numerator.IsEqual(0) || n.Denominator.IsEqual(0) && Numerator.IsEqual(0))
                return new T102Number(new T102Integer(0), new T102Integer(1), false);
            if (Denominator.IsEqual(0))
                return (n.Numerator.Spaceship(0) > 0) ? this : new T102Number(Numerator.UnaryMinus(), Denominator, false);
            if (n.Denominator.IsEqual(0))
                return (Numerator.Spaceship(0) > 0) ? n : new T102Number(n.Numerator.UnaryMinus(), n.Denominator, false);
            T102Integer n1 = Numerator;
            T102Integer n2 = n.Numerator;
            T102Integer d1 = Denominator;
            T102Integer d2 = n.Denominator;
            if (n1.IsLong() && d2.IsLong())
            {
                long nn1 = n1.ToLong();
                long nd2 = d2.ToLong();
                long gcd = T102Integer.GcdLong(nn1, nd2);
                if (gcd > 1)
                {
                    n1 = new T102Integer(nn1 / gcd);
                    d2 = new T102Integer(nd2 / gcd);
                }
            }
            if (n2.IsLong() && d1.IsLong())
            {
                long nn2 = n2.ToLong();
                long nd1 = d1.ToLong();
                long gcd = T102Integer.GcdLong(nn2, nd1);
                if (gcd > 1)
                {
                    n2 = new T102Integer(nn2 / gcd);
                    d1 = new T102Integer(nd1 / gcd);
                }
            }
            return new T102Number(n1.Multiply(n2), d1.Multiply(d2), true);
        }

        public T102Number Divide(T102Number n)
        {
            return Multiply(n.Reciprocal());
        }
    }

    struct T102Integer
    {
        private const long MAX_LONG_CALC = long.MaxValue / 2 - 2;
        private long MyLong;
        private System.Numerics.BigInteger[] MyBig;

        public T102Integer(T102Integer n)
        {
            MyLong = n.MyLong;
            MyBig = (n.MyBig == null) ? null : n.MyBig;
        }

        public static T102Integer FromBigInteger(System.Numerics.BigInteger n)
        {
            if (n >= long.MinValue && n <= long.MaxValue)
                return new T102Integer((long)n);
            T102Integer res = new T102Integer(0);
            res.MyBig = new System.Numerics.BigInteger[] { n };
            return res.Simplify();
        }

        public T102Integer(long n)
        {
            MyLong = n;
            MyBig = null;
        }

        public override string ToString()
        {
            return (MyBig != null) ? MyBig[0].ToString() : MyLong.ToString();
        }

        public override int GetHashCode()
        {
            return (MyBig != null) ? MyBig[0].GetHashCode() : MyLong.GetHashCode();
        }

        /*public string ToHexString()
        {
            return (LongValue == long.MaxValue)? LongValue.ToString("X") : BigValue.ToString("X");
        }*/

        public bool IsEqual(long n)
        {
            return MyBig == null && MyLong == n;
        }

        public int Spaceship(long n)
        {
            if(MyBig == null)
                return (MyLong == n)? 0 : (MyLong > n)? 1 : -1;
            return (MyBig[0] > n) ? 1 : -1;
        }

        public bool IsEqual(T102Integer n)
        {
            if(MyBig == null && n.MyBig == null)
                return MyLong == n.MyLong;
            if (MyBig != null && n.MyBig != null)
                return MyBig[0] == n.MyBig[0];
            return false;
        }

        public bool IsLong()
        {
            return MyBig == null;
        }

        public long ToLong()
        {
            return MyLong;
        }


        public static bool TryParseHexInteger(string str, ref int pos, out T102Integer res)
        {
            // unary minus or plus before number is optional
            if (pos < str.Length - 1 && (str[pos] == '-' || str[pos] == '+') && str[pos + 1] != '-' && str[pos + 1] != '+')
            {
                bool b_minus = str[pos++] == '-';
                if (!TryParseHexInteger(str, ref pos, out res))
                    return false;
                if (b_minus)
                    res = res.UnaryMinus();
                return true;
            }

            // the number (in positive version) must start with "0x" or "0X"
            if (pos < str.Length - 1 && str[pos] == '0' && (str[pos + 1] == 'x' || str[pos + 1] == 'X'))
                pos += 2;
            else
            {
                res = new T102Integer(0);
                return false;
            }

            res = new T102Integer(0);
            if (pos >= str.Length)
                return false;

            T102Integer SIXTEEN = new T102Integer(16);
            const string HEX_DIGITS_L = "0123456789abcdef";
            const string HEX_DIGITS_U = "0123456789ABCDEF";
            bool digits_found = false;
            for (; pos < str.Length; pos++)
            {
                if (str[pos] == '_')
                    continue;
                int n = HEX_DIGITS_U.IndexOf(str[pos]);
                if (n < 0 && (n = HEX_DIGITS_L.IndexOf(str[pos])) < 0)
                    return false;
                res = res.Multiply(SIXTEEN).Add(new T102Integer(n));
                digits_found = true;
            }
            return digits_found;
        }

        public static bool QuickTryParseInt(string str, ref int pos, out T102Integer res, out int n_digits_read)
        {
            // unary minus or plus before number is optional
            if (pos < str.Length - 1 && (str[pos] == '-' || str[pos] == '+') && str[pos + 1] != '-' && str[pos + 1] != '+')
            {
                bool b_minus = str[pos++] == '-';
                if (!QuickTryParseInt(str, ref pos, out res, out n_digits_read))
                    return false;
                if (b_minus)
                    res = res.UnaryMinus();
                return true;
            }
            T102Integer TEN = new T102Integer(10);
            const string TEN_DIGITS = "0123456789";
            res = new T102Integer(0);
            n_digits_read = 0;
            for (; pos < str.Length; pos++)
            {
                if (str[pos] == '_')
                    continue;
                int n = TEN_DIGITS.IndexOf(Char.ToUpper(str[pos]));
                if (n < 0)
                    return n_digits_read > 0;
                res = res.Multiply(TEN).Add(new T102Integer(n));
                n_digits_read++;
            }
            return n_digits_read > 0;
        }

        public T102Integer Power(long n)  // https://en.wikipedia.org/wiki/Exponentiation_by_squaring
        {
            if (n < 0 || IsEqual(0))
                return new T102Integer(0);
            else if (n == 0 || IsEqual(1))
                return new T102Integer(1);
            T102Integer res = new T102Integer(1);
            T102Integer x = this;
            while (n > 0)
            {
                if (n % 2 == 1)
                    res = res.Multiply(x);
                x = x.Multiply(x);
                n /= 2;
            }
            return res;
        }

        public T102Integer UnaryMinus()
        {
            if (MyBig == null)
            {
                if (MyLong == long.MaxValue || MyLong == long.MinValue)
                    return T102Integer.FromBigInteger(-ToBigInteger());
                return new T102Integer(-MyLong);
            }
            return T102Integer.FromBigInteger(-MyBig[0]);
        }

        public T102Integer Abs(out bool modified)
        {
            modified = false;
            if (MyLong == long.MinValue)
            {
                modified = true;
                return T102Integer.FromBigInteger(-ToBigInteger());
            }
            if (MyBig == null) 
            {
                if (MyLong < 0)
                {
                    modified = true;
                    return new T102Integer(-MyLong);
                }
            }
            else
            {
                if(MyBig[0] < 0)
                {
                    modified = true;
                    return T102Integer.FromBigInteger(- MyBig[0]);
                }
            }
            return this;
        }

        public T102Integer Add(T102Integer n)
        {
            if (MyBig == null && n.MyBig == null && MyLong <= MAX_LONG_CALC && MyLong >= -MAX_LONG_CALC && n.MyLong <= MAX_LONG_CALC && n.MyLong >= -MAX_LONG_CALC)
                return new T102Integer(MyLong + n.MyLong);
            return T102Integer.FromBigInteger(ToBigInteger() + n.ToBigInteger());
        }

        public T102Integer Subtract(T102Integer n)
        {
            if (MyBig == null && n.MyBig == null && MyLong <= MAX_LONG_CALC && MyLong >= -MAX_LONG_CALC && n.MyLong <= MAX_LONG_CALC && n.MyLong >= -MAX_LONG_CALC)
                return new T102Integer(MyLong - n.MyLong);
            return T102Integer.FromBigInteger(ToBigInteger() - n.ToBigInteger());
        }

        public T102Integer Multiply(T102Integer n)
        {
            const int MAX_FOR_MULT = Int32.MaxValue / 2 - 2;
            if (MyBig == null && n.MyBig == null && MyLong <= MAX_FOR_MULT && MyLong >= -MAX_FOR_MULT && n.MyLong <= MAX_FOR_MULT && n.MyLong >= -MAX_FOR_MULT)
                return new T102Integer(MyLong * n.MyLong);
            return T102Integer.FromBigInteger(ToBigInteger() * n.ToBigInteger());
        }

        public T102Integer DivRem(T102Integer n, out T102Integer rem)
        {
            if (MyBig == null && n.MyBig == null)
                if (n.MyLong == 0)
                {
                    rem = new T102Integer(0);
                    return new T102Integer(0);
                }
                else
                {
                    rem = new T102Integer(MyLong % n.MyLong);
                    return new T102Integer(MyLong / n.MyLong);
                }
            System.Numerics.BigInteger rem2;
            var res = System.Numerics.BigInteger.DivRem(ToBigInteger(), n.ToBigInteger(), out rem2);
            rem = T102Integer.FromBigInteger(rem2);
            return T102Integer.FromBigInteger(res);
        }

        public T102Integer GreatestCommonDivisor(T102Integer n)
        {
            return T102Integer.FromBigInteger(System.Numerics.BigInteger.GreatestCommonDivisor(ToBigInteger(), n.ToBigInteger()));
        }

        private T102Integer Simplify()
        {
            if (MyBig == null)
                return this;
            var big = MyBig[0];
            if (big <= long.MaxValue && big >= long.MinValue)
            {
                long n = (long)big;
                return new T102Integer(n);
            }
            return this;
        }

        public System.Numerics.BigInteger ToBigInteger()
        {
            return (MyBig != null) ? MyBig[0] : new System.Numerics.BigInteger(MyLong);
        }

        public static long GcdLong(long a, long b)
        {
            // https://msdn.microsoft.com/en-us/library/system.numerics.biginteger.greatestcommondivisor(v=vs.110).aspx
            // If the left and right parameters are non-zero numbers, the method always returns at least a value of 1 because all numbers can be divided by 1. 
            // If either parameter is zero, the method returns the absolute value of the non-zero parameter. If both values are zero, the method returns zero.
            // 
            if(a < 0)
                a = -a;
            if(b < 0)
                b = -b;
            if (b > a)
            {
                long t = b;
                b = a;
                a = t;
            }
            while(b != 0)
            {
                long t = b;
                b = a % b;
                a = t;
            }
            return a;
        }
    }
#endif

    class T102Preprocessor
    {
        private readonly string PREPRO_IDENT_CHARS = "_?";
        private readonly string DEFINE_STRONG_DIRECTIVE = null;
        private readonly string DEFINE_WEAK_DIRECTIVE = null;
        private readonly string INCLUDE_DIRECTIVE = null;

        /// <summary>
        /// Constructor.
        /// </summary>
        /// <param name="defineStrongDirective">for example "%%define", if this parameter is null or empty, the directive is not active</param>
        /// <param name="defineWeakDirective">for example "%%define?", if this parameter is null or empty, the directive is not active</param>
        /// <param name="includeDirective">for example "%%include", if this parameter is null or empty, the directive is not active</param>
        public T102Preprocessor(string defineStrongDirective, string defineWeakDirective, string includeDirective)
        {
            DEFINE_STRONG_DIRECTIVE = ((defineStrongDirective ?? "").Trim() == "") ? null : defineStrongDirective.Trim();
            DEFINE_WEAK_DIRECTIVE = ((defineWeakDirective ?? "").Trim() == "") ? null : defineWeakDirective.Trim();
            INCLUDE_DIRECTIVE = ((includeDirective ?? "").Trim() == "") ? null : includeDirective.Trim();

            string strFrom = (defineStrongDirective ?? "") + (defineWeakDirective ?? "")  + (includeDirective ?? "");
            string identChars = "_";
            for (int i = 0; i < strFrom.Length; i++)
            {
                char c = strFrom[i];
                if (!char.IsLetterOrDigit(c) && !char.IsWhiteSpace(c) && identChars.IndexOf(c) < 0)
                    identChars += c;
            }
            PREPRO_IDENT_CHARS = identChars;
        }

        public T102SrcSlice PreprocessFile(string fname, Encoding fileEncoding, bool canThrow)
        {
            try
            {
                var src = T102SrcSlice.ReadAllLines(fname, fileEncoding);
                return PreprocessSlice(src, fileEncoding, YamlValue.MakeNull(), null);
            }
            catch (T102SourceLine.ExceptionAtLn ex)
            {
                if (canThrow)
                    throw;
                else
                {
                    string errStr = string.Format("*** PARSER_ERROR: {0}", ex.ToString());
                    return new T102SrcSlice(new[] { ex.SourceLine.CloneSetText(errStr) }, 0, 1, ex.SourceLine.FileName);
                }
            }
            catch (Exception ex)
            {
                if (canThrow)
                    throw;
                else
                {
                    string errStr = string.Format("*** PARSER_ERROR: {0}", ex.ToString());
                    var srcln = new T102SourceLine(errStr, fname, 0);
                    return new T102SrcSlice(new[] { srcln }, 0, 1, fname);
                }
            }
        }

        public T102SrcSlice PreprocessSlice(T102SrcSlice src, Encoding fileEncoding)
        {
            return PreprocessSlice(src, fileEncoding, YamlValue.MakeNull(), null);
        }

        private class IncludeTraceItem
        {
            public readonly T102SrcSlice Source;
            public readonly int Hash;
            public readonly IncludeTraceItem Previous;

            private IncludeTraceItem(T102SrcSlice src, IncludeTraceItem parent)
            {
                Source = src;
                Hash = 0;
                for (int i = 0; i < src.Count; i++)
                    Hash ^= src.LineAt(i).GetHashCode();
                Previous = parent;
            }

            public static bool TryAppend(T102SrcSlice src, IncludeTraceItem list, out IncludeTraceItem res)
            {
                res = new IncludeTraceItem(src, list);
                for (; list != null; list = list.Previous)
                    if (list.Hash == res.Hash && list.Source.Count == res.Source.Count)
                    {
                        bool equal = true;
                        for (int i = 0; i < res.Source.Count; i++)
                            if (res.Source.LineAt(i) != list.Source.LineAt(i))
                            {
                                equal = false;
                                break;
                            }
                        if (equal)
                        {
                            res = null;
                            return false;
                        }
                    }
                return true;
            }
        }

        private T102SrcSlice PreprocessSlice(T102SrcSlice src, Encoding fileEncoding, YamlValue defines_par, IncludeTraceItem trace_list)
        {
            YamlValue defines = (defines_par.MyType == YamlValue.ValueType.Dict)? defines_par : YamlValue.MakeDict();
            List<T102SourceLine> res = new List<T102SourceLine>();
            for (int i_ln = 0; i_ln < src.Count; i_ln++)
            {
                string ln = src.LineAt(i_ln);
                if (T102BasicLexerAPI.IsWhiteSpaceOrEmpty(ln))
                    res.Add(src.SourceAt(i_ln));
                else
                {
                    int indent = src.GetIndentAt(i_ln);
                    var reader = CreatePreproLexer(ln);
                    bool done = false;
                    if (DEFINE_STRONG_DIRECTIVE != null && reader.TestToken(DEFINE_STRONG_DIRECTIVE ?? "%%define", PREPRO_IDENT_CHARS, null))
                    {
                        string key = reader.GetPrintableSequenceOrQuotedStr();
                        defines.SetStringAtKey(key, reader.ReadToEnd().Trim());
                        done = true;
                    }
                    else if (DEFINE_WEAK_DIRECTIVE != null && reader.TestToken(DEFINE_WEAK_DIRECTIVE ?? "%%define?", PREPRO_IDENT_CHARS, null))
                    {
                        string key = reader.GetPrintableSequenceOrQuotedStr();
                        if (!defines.ContainsKey(key))
                            defines.SetStringAtKey(key, reader.ReadToEnd().Trim());
                        done = true;
                    }
                    else if (INCLUDE_DIRECTIVE != null && reader.TestToken(INCLUDE_DIRECTIVE ?? "%%include", PREPRO_IDENT_CHARS, null))
                    {
                        if (reader.GetPrintableSequenceOrQuotedStr() != "")
                        {
                            string templ_name = reader.Text.Replace('/', '\\');
                            string templ_path = System.IO.Path.Combine(System.IO.Path.GetDirectoryName(src.SourceFileName), templ_name);
                            string str_at = reader.ReadToEnd().Trim();
                            if (!System.IO.File.Exists(templ_path))
                                throw src.SourceAt(i_ln).CreateException(string.Format("File {0} (beeing a parameter of %%include) does not exist", templ_path));
                            res.AddRange(GetInclSource(templ_path, str_at, fileEncoding, defines, trace_list, src.SourceAt(i_ln)).RebaseIndent(indent).RemoveWhiteSpaceTail().ToList());
                            done = true;
                        }
                    }
                    if (!done)
                        res.Add(src.SourceAt(i_ln));
                }
            }
            if (defines_par.IsNull) // finally, if this is top-level call of PreprocessSlice() do StringReplace()
                defines.ForEachKeyValueString((key, inx, val) =>
                {
                    for (int i = 0; i < res.Count; i++)
                        res[i] = res[i].CloneSetText(res[i].Text.Replace(key, val));
                });
            return new T102SrcSlice(res.ToArray(), 0, res.Count, src.SourceFileName);
        }

        private static T102SimpleLexer CreatePreproLexer(string ln)
        {
            T102SimpleLexer res = new T102SimpleLexer(ln);
            res.SkipNL = true;
            res.SingleQuoteEscChar = '\'';
            res.DoubleQuoteEscChar = '"';
            res.AllowPeriodForNumbers = false;
            res.AllowPipeBracket = false;
            res.CanThrow = false;
            return res;
        }

        private T102SrcSlice GetInclSource(string fname, string str_at, Encoding fileEncoding, 
            YamlValue defines, IncludeTraceItem trace_list, T102SourceLine src_line)
        {
            str_at = (str_at ?? "").Trim();

            var file_content = T102SrcSlice.ReadAllLines(fname, fileEncoding);
            file_content.ThrowIfTabs();
            if (str_at != "")
            {
                bool found = false;
                for (int i = 0; i < file_content.Count; i++)
                    if (file_content.LineAt(i).IndexOf(str_at) >= 0)
                    {
                        file_content = file_content.GetIndentedBlock(i, includeStartLn: false);
                        found = true;
                        break;
                    }
                if (!found)
                {
                    throw src_line.CreateException(string.Format("Label {0} not found in file {1}", str_at, fname));
                    return new T102SrcSlice(new T102SourceLine[] { }, 0, 0, fname);
                }
            }

            IncludeTraceItem list2;
            if (!IncludeTraceItem.TryAppend(file_content, trace_list, out list2))
                return new T102SrcSlice(new T102SourceLine[] { }, 0, 0, fname);
            return PreprocessSlice(file_content, fileEncoding, defines, list2);
        }
    }

    class T102SrcSlice
    {
        private string FSourceFileName;
        private T102SourceLine[] Source;
        private int StartLine;
        private int EndLine;

        public T102SrcSlice(T102SourceLine[] src, int start, int end, string fname)
        {
            FSourceFileName = fname ?? "";
            Source = src ?? new T102SourceLine[] { };
            StartLine = (start < 0) ? 0 : start;
            EndLine = (end >= Source.Length) ? Source.Length : (end < start) ? start : end;
        }

        public override string ToString()
        {
            StringBuilder res = new StringBuilder();
            for (int i = 0; i < Count; i++)
                res.Append(LineAt(i)).Append("\n");
            return res.ToString();
        }

        public string SourceFileName
        {
            get
            {
                return FSourceFileName;
            }
        }

        public int Count
        {
            get
            {
                return EndLine - StartLine;
            }
        }

        public string LineAt(int i)
        {
            int inx = i + StartLine;
            return (i >= 0 && inx < EndLine) ? Source[inx].Text : "";
        }

        public T102SourceLine SourceAt(int i)
        {
            int inx = i + StartLine;
            return (i >= 0 && inx < EndLine) ? Source[inx] : null;
        }

        public List<T102SourceLine> ToList()
        {
            List<T102SourceLine> res = new List<T102SourceLine>();
            for (int i = 0; i < Count; i++)
                res.Add(SourceAt(i));
            return res;
        }

        public List<string> ToStringList()
        {
            List<string> res = new List<string>();
            for (int i = 0; i < Count; i++)
                res.Add(LineAt(i).TrimEnd());
            return res;
        }

        public int GetIndentAt(int i)
        {
            while (i < EndLine)
            {
                string line = LineAt(i++);
                if (!T102BasicLexerAPI.IsWhiteSpaceOrEmpty(line))
                    return GetIndentOf_Unsafe(line);
            }
            return 0;
        }

        private static int GetIndentOf_Unsafe(string line)
        {
            for (int i = 0; i < line.Length; i++)
                if (line[i] != ' ')
                    return i;
            return line.Length;
        }

        public static T102SrcSlice ReadAllLines(string fileName, Encoding enc)
        {
            return ReadAllLines(fileName, enc, canThrow: true);
        }

        public static T102SrcSlice ReadSafeAllLines(string fileName, Encoding enc)
        {
            return ReadAllLines(fileName, enc, canThrow: false);
        }

        private static T102SrcSlice ReadAllLines(string fileName, Encoding enc, bool canThrow)
        {
            string[] lines_txt = null;
            if (canThrow)
                lines_txt = System.IO.File.ReadAllLines(fileName, enc);
            else
            {
                if (!System.IO.File.Exists(fileName))
                    return new T102SrcSlice(new T102SourceLine[] { }, 0, 0, fileName);
                try
                {
                    lines_txt = System.IO.File.ReadAllLines(fileName, enc);
                }
                catch (Exception)
                {
                    return new T102SrcSlice(new T102SourceLine[] { }, 0, 0, fileName);
                }
            }

            T102SourceLine[] lines = new T102SourceLine[lines_txt.Length];
            for (int i = 0; i < lines.Length; i++)
                lines[i] = new T102SourceLine(lines_txt[i].Replace('\0', ' ').Replace('\n', ' ').Replace('\r', ' ').TrimEnd(), fileName, i);
            return new T102SrcSlice(lines, 0, lines.Length, fileName);
        }

        public void ThrowIfTabs()
        {
            for (int i = StartLine; i < EndLine; i++)
                if (Source[i].Text.IndexOf('\t') >= 0)
                    throw new Exception(string.Format("Tabs '\\t' not allowed in file {0} (line: {1})", SourceFileName, i + 1));
        }
        public T102SrcSlice GetIndentedBlock(int startLn, bool includeStartLn)
        {
            int startIndent = GetIndentAt(startLn);
            int end = startLn;
            for (int i = startLn + 1; i < Count; i++)
                if (!T102BasicLexerAPI.IsWhiteSpaceOrEmpty(LineAt(i)))
                {
                    if (GetIndentAt(i) <= startIndent)
                        break;
                    end = i + 1;
                }
            return new T102SrcSlice(Source, startLn + StartLine + (includeStartLn ? 0 : 1), end + StartLine, SourceFileName);
        }

        public T102SrcSlice RebaseIndent(int indent)
        {
            int min_ind = MinIndent();
            if (min_ind == indent)
                return this;
            List<T102SourceLine> res = new List<T102SourceLine>();
            string spc = new string(' ', indent);
            for (int i = 0; i < Count; i++)
            {
                string line = LineAt(i);
                var ll = SourceAt(i);
                if (line.Length > indent)
                {
                    ll = ll.Clone();
                    ll.Text = spc + line.Substring(min_ind);
                }
                res.Add(ll);
            }
            return new T102SrcSlice(res.ToArray(), 0, res.Count, SourceFileName);
        }

        public T102SrcSlice Separate()
        {
            List<T102SourceLine> res = new List<T102SourceLine>();
            for (int i = 0; i < Count; i++)
                res.Add(SourceAt(i));
            return new T102SrcSlice(res.ToArray(), 0, res.Count, SourceFileName);
        }

        public T102SrcSlice RemoveWhiteSpaceHead()
        {
            if (Count <= 0 || !T102BasicLexerAPI.IsWhiteSpaceOrEmpty(LineAt(0)))
                return this;
            for (int i = 0; i < Count; i++)
                if (!T102BasicLexerAPI.IsWhiteSpaceOrEmpty(LineAt(i)))
                    return new T102SrcSlice(Source, i, EndLine, SourceFileName);
            return new T102SrcSlice(Source, StartLine, StartLine, SourceFileName);
        }

        public T102SrcSlice RemoveWhiteSpaceTail()
        {
            for (int i = Count; i > 0; i--)
                if (!T102BasicLexerAPI.IsWhiteSpaceOrEmpty(LineAt(i - 1)))
                    return new T102SrcSlice(Source, StartLine, StartLine + i, SourceFileName);
            return new T102SrcSlice(Source, StartLine, StartLine, SourceFileName);
        }

        public int MinIndent()
        {
            int res = (Count > 0) ? LineAt(0).Length : 0;
            for (int i = 0; i < Count; i++)
                if (!T102BasicLexerAPI.IsWhiteSpaceOrEmpty(LineAt(i)) && res > GetIndentAt(i))
                    res = GetIndentAt(i);
            return res;
        }
    }

    /// <summary>
    /// Unlike System.Collections.Generic.Dictionary, this struct preserves keys order
    /// and allows parallel reading (including iterations) by different threads
    /// at the same time.
    /// </summary>
    struct YamlValue
    {
        public enum ValueType
        {
            Null,
            String,
            List,
            Dict
        }

        private string MyText;
        private List<YamlValue> MyList;
        private Dictionary<string, YamlValue> MyDict;
        private int IndexInDictKeys;

        public static YamlValue MakeNull()
        {
            return new YamlValue
            {
                MyText = null,
                MyList = null,
                MyDict = null,
                IndexInDictKeys = 0
            };
        }

        public static YamlValue MakeString(string str)
        {
            return new YamlValue
            {
                MyText = str ?? "",
                MyList = null,
                MyDict = null,
                IndexInDictKeys = 0
            };
        }

        public static YamlValue MakeList()
        {
            return new YamlValue
            {
                MyText = null,
                MyList = new List<YamlValue>(),
                MyDict = null,
                IndexInDictKeys = 0
            };
        }

        public static YamlValue MakeDict()
        {
            return new YamlValue
            {
                MyText = null,
                MyList = new List<YamlValue>(),
                MyDict = new Dictionary<string, YamlValue>(),
                IndexInDictKeys = 0
            };
        }

        public bool IsNull
        {
            get
            {
                return MyText == null && MyList == null && MyDict == null;
            }
        }

        public ValueType MyType
        {
            get
            {
                if (MyText != null)
                    return ValueType.String;
                if (MyDict != null)
                    return ValueType.Dict;
                if (MyList != null)
                    return ValueType.List;
                return ValueType.Null;
            }
        }

        public int Count
        {
            get
            {
                if (MyText != null)
                    return MyText.Length;
                if (MyDict != null)
                    return MyDict.Count;
                if (MyList != null)
                    return MyList.Count;
                return 0;
            }
        }

        public override string ToString()
        {
            switch (MyType)
            {
                case ValueType.Null:
                    return "";
                case ValueType.String:
                    return MyText;
                default:
                    return QuotedString();
            }
        }

        private string QuotedStringIfNeed(bool isYaml)
        {
            return (isYaml && MyType == ValueType.String && !T102BasicLexerAPI.NeedsQuotation(MyText)) ? MyText : QuotedString();
        }

        /// <summary>
        /// Returns single-line expression equivalent this value.
        /// </summary>
        public string QuotedString()
        {
            switch (MyType)
            {
                case ValueType.Null:
                    return "null";
                case ValueType.String:
                    return T102BasicLexerAPI.Escape(MyText, '"');
                case ValueType.List:
                    StringBuilder sbl = new StringBuilder("[");
                    for (int i = 0; i < MyList.Count; i++)
                    {
                        if (i > 0)
                            sbl.Append(", ");
                        sbl.Append(MyList[i].QuotedString());
                    }
                    return sbl.Append("]").ToString();
                case ValueType.Dict:
                    StringBuilder sbd = new StringBuilder("{");
                    bool first = true;
                    foreach (KeyValuePair<string, YamlValue> entry in MyDict)
                    {
                        if (!first)
                            sbd.Append(", ");
                        sbd.Append(T102BasicLexerAPI.IsIdentifier(entry.Key, "_") ? entry.Key : T102BasicLexerAPI.Escape(entry.Key, '"')).Append(": ");
                        sbd.Append(entry.Value.QuotedString());
                        first = false;
                    }
                    return sbd.Append("}").ToString();
                default:
                    throw new Exception(string.Format("QuotedString(): invalid value type {0}", MyType));
            }
        }

        public List<string> ToLines(int indent)
        {
            List<string> res = new List<string>();
            ToLines(indent, isYaml: false, res: res, fromListItem: false);
            return res;
        }

        public List<string> ToYaml()
        {
            List<string> res = new List<string>();
            ToLines(0, isYaml: true, res: res, fromListItem: false);
            return res;
        }

        private void ToLines(int indent, bool isYaml, List<string> res, bool fromListItem)
        {
            switch (MyType)
            {
                case ValueType.Null:
                    res.Add(string.Format("{0}{1}", new string(' ', indent), QuotedString()));
                    break;
                case ValueType.String:
                    res.Add(string.Format("{0}{1}", new string(' ', indent), QuotedStringIfNeed(isYaml)));
                    break;
                case ValueType.List:
                    if (Count <= 0)
                        res.Add(new string(' ', indent) + "[]");
                    else
                    {
                        if (indent > 2)
                            indent -= 2;
                        ForEach((key, i, val) =>
                        {
                            if (val.FmtIsShortValue())
                                res.Add(string.Format("{0}-{1} {2}", new string(' ', indent),
                                    (val.MyType != ValueType.List && val.MyType != ValueType.Dict) ? ":" : "",
                                    val.QuotedStringIfNeed(isYaml)));
                            else
                            {
                                res.Add(string.Format("{0}-", new string(' ', indent)));
                                val.ToLines(indent + 2, isYaml, res, true);
                            }
                        });
                    }
                    break;
                case ValueType.Dict:
                    if (Count <= 0)
                    {
                        if (fromListItem && res.Count > 0)
                            res[res.Count - 1] += " {}";
                        else
                            res.Add(new string(' ', indent) + "{}");
                    }
                    else
                    {
                        int max_key_len = MaxKeyLength();
                        ForEach((key, i, val) =>
                        {
                            string keyStr = ((T102BasicLexerAPI.IsIdentifier(key, "_")) ? key : T102BasicLexerAPI.Escape(key, '"')) + ":";
                            if (max_key_len > keyStr.Length - 1)
                                keyStr += new string(' ', max_key_len - keyStr.Length + 1);
                            if (val.FmtIsShortValue())
                            {
                                if (fromListItem)
                                    res[res.Count - 1] += string.Format(" {0} {1}", keyStr, val.QuotedStringIfNeed(isYaml));
                                else
                                    res.Add(string.Format("{0}{1} {2}", new string(' ', indent), keyStr, val.QuotedStringIfNeed(isYaml)));
                            }
                            else
                            {
                                if (fromListItem && res.Count > 0)
                                    res[res.Count - 1] += string.Format(" {0}", keyStr);
                                else
                                    res.Add(string.Format("{0}{1}", new string(' ', indent), keyStr));
                                val.ToLines(indent + 4, isYaml, res, false);
                            }
                            fromListItem = false;
                        });
                    }
                    break;
            }
        }

        private bool FmtIsShortValue()
        {
            switch (MyType)
            {
                case ValueType.Null:
                case ValueType.String:
                    return true;
                case ValueType.List:
                case ValueType.Dict:
                    return Count <= 0;
                default:
                    return false;
            }
        }

        public YamlValue ItemAtInx(int i)
        {
            if (MyText != null)
                return MakeString((i >= 0 && i < MyText.Length) ? MyText.Substring(i, 1) : "");
            if (MyList == null || MyDict != null)
                throw new Exception("Invalid use of ItemAtInx (operator[]): value must be a list or a string");
            return (i >= 0 && i < MyList.Count) ? MyList[i] : MakeNull();
        }

        public void SetAtInx(int i, YamlValue val)
        {
            if (MyList == null || MyDict != null)
                throw new Exception("Invalid use of SetAtInx (operator[]): value must be a list");
            if (i >= 0 && i < MyList.Count)
                MyList[i] = val;
            else
                MyList.Add(val);
        }

        public void InsertAtInx(int i, YamlValue val)
        {
            if (MyList == null || MyDict != null)
                throw new Exception("Invalid use of InsertAtInx (operator[]): value must be a list");
            if (i >= 0 && i < MyList.Count)
                MyList.Insert(i, val);
            else
                MyList.Add(val);
        }

        public void RemoveAtInx(int i)
        {
            if (MyList == null || MyDict != null)
                throw new Exception("Invalid use of RemoveAtInx (operator[]): value must be a list");
            if (i >= 0 && i < MyList.Count)
                MyList.RemoveAt(i);
        }

        public bool ContainsKey(string key)
        {
            return MyDict != null && MyDict.ContainsKey(key);
        }

        public YamlValue ItemAtKey(string key)
        {
            if (MyDict == null)
                throw new Exception("Invalid use of ItemAtKey (operator[]): value must be a ditionary");
            YamlValue res;
            return (MyDict.TryGetValue(key, out res)) ? res : MakeNull();
        }

        public void SetStringAtKey(string key, string val)
        {
            SetAtKey(key, MakeString(val));
        }

        public void SetAtKey(string key, YamlValue val)
        {
            key = key ?? "";
            if (MyDict == null)
                throw new Exception("Invalid use of SetAtKey (operator[]): value must be a ditionary");
            YamlValue old;
            if (MyDict.TryGetValue(key, out old))
            {
                val.IndexInDictKeys = old.IndexInDictKeys;
                MyDict[key] = val;
            }
            else
            {
                // RebuildKeys(); <-- not necessary when adding item
                val.IndexInDictKeys = MyDict.Count;
                MyDict[key] = val;
                MyList.Add(MakeString(key));
            }
        }

        public void RemoveAtKey(string key)
        {
            if (MyDict == null)
                throw new Exception("Invalid use of RemoveAtKey (operator[]): value must be a ditionary");
            YamlValue old;
            if (MyDict.TryGetValue(key, out old))
            {
                MyDict.Remove(key);
                MyList[old.IndexInDictKeys] = MakeNull();
                RebuildKeys();
            }
        }

        public YamlValue YamlClone()
        {
            switch (MyType)
            {
                case ValueType.Null:
                case ValueType.String:
                    return this;
                case ValueType.List:
                    YamlValue resList = MakeList();
                    resList.MyList.AddRange(MyList);
                    return resList;
                case ValueType.Dict:
                    YamlValue resDict = MakeDict();
                    ForEach((k, i, val) =>
                    {
                        resDict.SetAtKey(k, val);
                    });
                    return resDict;
                default:
                    throw new Exception(string.Format("YamlValue.YamlClone: invalid type {0}", MyType));
            }
        }

        public YamlValue OperatorPlus(YamlValue ym2, bool overwrite)
        {
            if(ym2.IsNull)
                return YamlClone();
            switch (MyType)
            {
                case ValueType.Null:
                    return ym2.YamlClone();
                case ValueType.String:
                    return MakeString(MyText + ym2.ToString());
                case ValueType.List:
                    if(ym2.MyType != ValueType.List)
                        throw new Exception(string.Format("Type mismatch: cannot append {0} to {1}", ym2.MyType, MyType));
                    YamlValue resList = MakeList();
                    resList.MyList.AddRange(MyList);
                    resList.MyList.AddRange(ym2.MyList);
                    return resList;
                case ValueType.Dict:
                    if(ym2.MyType != ValueType.Dict)
                        throw new Exception(string.Format("Type mismatch: cannot append {0} to {1}", ym2.MyType, MyType));
                    YamlValue resDict = MakeDict();
                    ForEach((k, i, val) =>
                    {
                        resDict.SetAtKey(k, val);
                    });
                    ym2.ForEach((k, i, val) =>
                    {
                        if(overwrite || !resDict.MyDict.ContainsKey(k))
                            resDict.SetAtKey(k, val);
                    });
                    return resDict;
                default:
                    throw new Exception(string.Format("YamlValue.AddRange: invalid type {0}", MyType));
            }
        }

        private void RebuildKeys()
        {
            const int DELTA = 0;  /* //!!!
            const int DELTA = 8; 
            */
            var keys = MyList;
            if (keys.Count > MyDict.Count * 2 + DELTA)
            {
                int n_res = 0;
                for (int i = 0; i < keys.Count; i++)
                    if (!keys[i].IsNull)
                        keys[n_res++] = keys[i];
                while (keys.Count > n_res)
                    keys.RemoveAt(keys.Count - 1);
            }
        }

        public void ForEachKeyValueString(Action<string, int, string> body)
        {
            ForEach((key, i, val) =>
            {
                body(key, i, val.ToString());
            });
        }

        public struct KvItem
        {
            public string Key;
            public int Index;
            public YamlValue Value;
        };

        public IEnumerable<KvItem> KeyValues()
        {
            switch (MyType)
            {
                case ValueType.String:
                    for (int i = 0; i < MyText.Length; i++)
                        yield return new KvItem { Key = "", Index = i, Value = MakeString(MyText[i].ToString()) };
                    break;
                case ValueType.List:
                    for (int i = 0; i < MyList.Count; i++)
                        yield return new KvItem { Key = "", Index = i, Value = MyList[i] };
                    break;
                case ValueType.Dict:
                    var keys = MyList;
                    int iter = 0;
                    for (int i = 0; i < keys.Count; i++)
                        if (!keys[i].IsNull)
                        {
                            string key = keys[i].MyText;
                            YamlValue val;
                            if (MyDict.TryGetValue(key, out val))
                                yield return new KvItem { Key = key, Index = iter++, Value = val };
                        }
                    break;
            }
        }

        public void ForEach(Action<string, int, YamlValue> body)
        {
            switch (MyType)
            {
                case ValueType.String:
                    for (int i = 0; i < MyText.Length; i++)
                        body("", i, MakeString(MyText[i].ToString()));
                    break;
                case ValueType.List:
                    for (int i = 0; i < MyList.Count; i++)
                        body("", i, MyList[i]);
                    break;
                case ValueType.Dict:
                    var keys = MyList;
                    int iter = 0;
                    for (int i = 0; i < keys.Count; i++)
                        if (!keys[i].IsNull)
                        {
                            string key = keys[i].MyText;
                            YamlValue val;
                            if (MyDict.TryGetValue(key, out val))
                                body(key, iter++, val);
                        }
                    break;
            }
        }

        public YamlValue DeleteWhere(Func<string, int, YamlValue, bool> whereDel)
        {
            switch (MyType)
            {
                case ValueType.Null:
                    return this;
                case ValueType.String:
                    StringBuilder resSb = new StringBuilder();
                    bool charDeleted = false;
                    for (int i = 0; i < MyText.Length; i++)
                        if (!whereDel("", i, MakeString(MyText[i].ToString())))
                        {
                            resSb.Append(MyText[i]);
                            charDeleted = true;
                        }
                    return (charDeleted) ? YamlValue.MakeString(resSb.ToString()) : this;
                case ValueType.List:
                    for (int i = 0; i < MyList.Count; i++)
                        if (whereDel("", i, MyList[i]))
                            MyList.RemoveAt(i--);
                    return this;
                case ValueType.Dict:
                    var keys = MyList;
                    int iter = 0;
                    for (int i = 0; i < keys.Count; i++)
                        if (!keys[i].IsNull)
                        {
                            string key = keys[i].MyText;
                            YamlValue val;
                            if (MyDict.TryGetValue(key, out val))
                                if (whereDel(key, iter++, val))
                                    this.RemoveAtKey(key);
                        }
                    return this;
                default:
                    throw new Exception(string.Format("DeleteWhere: invalid type {0}", MyType));
            }
        }

        public int MaxKeyLength()
        {
            switch (MyType)
            {
                case ValueType.Dict:
                    int maxLen = 0;
                    for(int i=0; i < MyList.Count; i++)
                        if(MyList[i].MyText != null && MyList[i].MyText.Length > maxLen)
                            maxLen = MyList[i].MyText.Length;
                    return maxLen;
                default:
                    return 0;
            }
        }

        public static YamlValue Parse(T102SrcSlice src, bool isYaml)
        {
            if (src.Count <= 0)
                return YamlValue.MakeNull();
            src.ThrowIfTabs();

            List<YamlValue> list = new List<YamlValue>();
            List<string> objectFieldNames = new List<string>();
            List<YamlValue> objectFieldValues = new List<YamlValue>();
            List<YamlValue> others = new List<YamlValue>();
            int indent = src.GetIndentAt(0);
            for (int i = 0; i < src.Count; i++)
            {
                if (T102BasicLexerAPI.IsWhiteSpaceOrEmpty(src.LineAt(i)))
                    continue;
                if (src.GetIndentAt(i) != indent)
                    throw src.SourceAt(i).CreateException("Invalid indendation");
                bool listItemBegin;
                string fieldName;
                string value;
                if (T102BasicLexerAPI.YamlRecognizeLine(src.LineAt(i), true, out listItemBegin, out fieldName, out value))
                {
                    if (listItemBegin && fieldName != "") // member of object beeing list item
                    {
                        int fieldNameIndent = T102BasicLexerAPI.FirstIndexNot(src.LineAt(i), " -");
                        int indent2 = src.GetIndentAt(i+1);
                        if (indent2 == indent + 1 || indent2 == indent - 1 || indent2 == fieldNameIndent + 1 || indent2 == fieldNameIndent - 1)
                            throw src.SourceAt(i + 1).CreateException("Invalid indentation, the indents cannot differ by 1");
                        if (indent2 > indent) // with indent
                        {
                            var innerBlock = src.GetIndentedBlock(i, false);
                            YamlValue indented = Parse(innerBlock, isYaml);
                            i += innerBlock.Count;
                            if (indented.MyType == ValueType.Dict && indent2 == fieldNameIndent)
                            {
                                YamlValue obj = YamlValue.MakeDict();
                                obj.SetAtKey(fieldName, YamlValue.ParseExpression(value, isYaml));
                                list.Add(obj.OperatorPlus(indented, true));
                            }
                            else
                            {
                                YamlValue obj = YamlValue.MakeDict();
                                obj.SetAtKey(fieldName, YamlValue.ParseExpressionWithIndentedBlock(value, indented, innerBlock, isYaml));
                                list.Add(obj);
                            }
                        }
                        else
                        {
                            YamlValue obj = YamlValue.MakeDict();
                            obj.SetAtKey(fieldName, YamlValue.ParseExpression(value, isYaml));
                            list.Add(obj);
                        }
                    }
                    else if(fieldName != "") // simple object member
                    {
                        int indent2 = src.GetIndentAt(i+1);
                        if (indent2 == indent + 1 || indent2 == indent - 1)
                            throw src.SourceAt(i + 1).CreateException("Invalid indentation, the indents cannot differ by 1");
                        if (indent2 > indent) // with indent
                        {
                            var innerBlock = src.GetIndentedBlock(i, false);
                            YamlValue indented = Parse(innerBlock, isYaml);
                            i += innerBlock.Count;
                            objectFieldNames.Add(fieldName);
                            objectFieldValues.Add(YamlValue.ParseExpressionWithIndentedBlock(value, indented, innerBlock, isYaml));
                        }
                        else
                        {
                            objectFieldNames.Add(fieldName);
                            objectFieldValues.Add(YamlValue.ParseExpression(value, isYaml));
                        }
                    }
                    else  // list item
                    {
                        if (!listItemBegin)
                            throw src.SourceAt(i).CreateException("Internal error, YamlRecognizeLine returned true for line describing neither object nor list item");
                        int indent2 = src.GetIndentAt(i+1);
                        if (indent2 == indent + 1 || indent2 == indent - 1)
                            throw src.SourceAt(i + 1).CreateException("Invalid indentation, the indents cannot differ by 1");
                        else if (indent2 <= indent && value.Trim() == "")
                            throw src.SourceAt(i).CreateException("Missing list item value");
                        else if (indent2 <= indent)
                            list.Add(YamlValue.ParseExpression(value, isYaml));
                        else
                        {
                            var innerBlock = src.GetIndentedBlock(i, false);
                            YamlValue indented = Parse(innerBlock, isYaml);
                            i += innerBlock.Count;
                            list.Add((value.Trim() == "")? indented : YamlValue.ParseExpressionWithIndentedBlock(value, indented, innerBlock, isYaml));
                        }
                    }
                }
                else // expression, not list item neither object
                {
                    int indent2 = src.GetIndentAt(i+1);
                    if (indent2 == indent + 1 || indent2 == indent - 1)
                        throw src.SourceAt(i + 1).CreateException("Invalid indentation, the indents cannot differ by 1");
                    else if (indent2 <= indent)
                        others.Add(YamlValue.ParseExpression(value, isYaml));
                    else
                    {
                        var innerBlock = src.GetIndentedBlock(i, false);
                        YamlValue indented = Parse(innerBlock, isYaml);
                        i += innerBlock.Count;
                        others.Add(YamlValue.ParseExpressionWithIndentedBlock(value, indented, innerBlock, isYaml));
                    }
                }
            }

            if (list.Count > 0 && objectFieldNames.Count <= 0 && others.Count <= 0)
            {
                YamlValue res = YamlValue.MakeList();
                res.MyList.AddRange(list);
                return res;
            }
            if (objectFieldNames.Count > 0 && list.Count <= 0 && others.Count <= 0)
            {
                YamlValue res = YamlValue.MakeDict();
                for (int i = 0; i < objectFieldNames.Count; i++)
                    res.SetAtKey(objectFieldNames[i], objectFieldValues[i]);
                return res;
            }
            if (others.Count >= 0 && list.Count <= 0 && objectFieldNames.Count <= 0)
            {
                YamlValue res = YamlValue.MakeList();
                for (int i = 0; i < others.Count; i++)
                    res.SetAtInx(i, others[i]);
                return res.ConvertToBlock();
            }

            if (list.Count > 0 && objectFieldNames.Count > 0)
                throw src.SourceAt(0).CreateException("Invalid systax: list syntax mixed with object syntax");
            if (list.Count > 0)
                throw src.SourceAt(0).CreateException("Invalid systax: list items mixed with expression(s)");
            throw src.SourceAt(0).CreateException("Invalid systax: object syntax mixed with expression(s)");
        }

        public YamlValue ConvertToBlock()
        {
            // todo
            return YamlValue.MakeNull();
        }

        public static YamlValue ParseExpression(string str, bool isYaml)
        {
            str = (str ?? "").Trim();
            if (isYaml)
            {
                if (str == "")
                    return MakeNull();
                if (str[0] == '|' || str[0] == '>')
                {
                    str = str.Substring(1).Trim();
                    return YamlValue.MakeString(str);
                }
                if (str[0] == '"' || str[0] == '\'')
                {
                    str = str.Substring(1);
                    if(str != "" && (str[str.Length-1] == '"' || str[str.Length-1] == '\''))
                        str = str.Substring(0, str.Length-1);
                    return YamlValue.MakeString(T102BasicLexerAPI.Unescape(str));
                }
                return YamlValue.MakeString(str.Trim());
            }
            // todo
            return YamlValue.MakeNull();
        }

        public static YamlValue ParseExpressionWithIndentedBlock(string str, YamlValue indentedBlock, T102SrcSlice slcIndentedBlock, bool isYaml)
        {
            if (isYaml)
            {
                string str1 = str.Trim();
                switch (str1)
                {
                    case "":
                        return indentedBlock;
                    case ">":
                    case "|":
                        string separator = (str1 == ">") ? " " : "\n";
                        StringBuilder res = new StringBuilder();
                        for (int i = 0; i < slcIndentedBlock.Count; i++)
                        {
                            if (i > 0)
                                res.Append(separator);
                            res.Append(slcIndentedBlock.LineAt(i).Trim());
                        }
                        return YamlValue.MakeString(res.ToString());
                }
            }

            // todo
            return YamlValue.MakeNull();
        }
    }

    /*
    struct YamlValue
    {
        public enum ValueType
        {
            Null,
            Bool,
            Number,
            String,
            List,
            DictOrObject,
            Block,
            Expression
        }
 
        private class UniqueIdentifier
        {
            private long Num1;
            private long Num2;
            private long Num3;
            private long Num4;
 
            public bool IsZero_ThreadSafeAtomicityGuarantedByDotNetFramework;
 
            public UniqueIdentifier(long n1)
            {
                IsZero_ThreadSafeAtomicityGuarantedByDotNetFramework = n1 == 0;
                Num1 = n1;
                Num2 = Num3 = Num4 = 0;
            }
 
            public override int GetHashCode()
            {
                return Num1.GetHashCode() ^ Num2.GetHashCode() ^ Num3.GetHashCode() ^ Num4.GetHashCode();
            }
 
            public bool IsZero()
            {
                return Num1 == 0 && Num2 == 0 && Num3 == 0 && Num4 == 0;
           }
 
            public int Spaceship(UniqueIdentifier uid2)
            {
                if (Num4 != uid2.Num4)
                    return (Num4 > uid2.Num4) ? 1 : -1;
                if (Num3 != uid2.Num3)
                    return (Num3 > uid2.Num3) ? 1 : -1;
                if (Num2 != uid2.Num2)
                    return (Num2 > uid2.Num2) ? 1 : -1;
                if (Num2 != uid2.Num2)
                    return (Num2 > uid2.Num2) ? 1 : -1;
                return 0;
            }
 
            public void Increment(UniqueIdentifier saveCopy)
            {
                lock (this)
                {
                    if (Num1 < long.MaxValue)
                        Num1++;
                    else
                    {
                        Num1 = 0;
                        if (Num2 < long.MaxValue)
                            Num2++;
                        else
                        {
                            Num2 = 0;
                            if (Num3 < long.MaxValue)
                                Num3++;
                            else
                            {
                                Num3 = 0;
                                if (Num4 < long.MaxValue)
                                    Num4++;
                                else
                                {
                                    Num4 = 0;
                                    Num1 = 1;
                                }
                            }
                        }
                   }
                    if (saveCopy != null)
                    {
                        saveCopy.IsZero_ThreadSafeAtomicityGuarantedByDotNetFramework = false;
                        saveCopy.Num1 = Num1;
                        saveCopy.Num2 = Num2;
                        saveCopy.Num3 = Num3;
                        saveCopy.Num4 = Num4;
                    }
                }
            }
        }
 
        private static readonly UniqueIdentifier UnqIdentSeed = new UniqueIdentifier(1);
 
        public ValueType Type;
        private string Text;
        private long Num;
        private UniqueIdentifier MyUniqueId;
        private List<YamlValue> Items;
        private Dictionary<string, YamlValue> Fields;
 
        public static YamlValue CreateNull()
        {
            return new YamlValue
            {
                Type = ValueType.Null,
                Text = null,
                Num = 0,
                MyUniqueId = null,
                Items = null,
                Fields = null
            };
        }
 
        public static YamlValue CreateBool(bool b)
        {
            return new YamlValue
            {
                Type = ValueType.Bool,
                Text = null,
                Num = 0,
                MyUniqueId = null,
                Items = null,
                Fields = null
            };
        }
 
        public static YamlValue CreateNum(long n)
        {
            return new YamlValue
            {
                Type = ValueType.Number,
                Text = null,
                Num = n,
                MyUniqueId = null,
                Items = null,
                Fields = null
            };
        }
 
        public static YamlValue CreateString(string str)
        {
            return new YamlValue
            {
                Type = ValueType.String,
                Text = str ?? "",
                Num = 0,
                MyUniqueId = null,
                Items = null,
                Fields = null
            };
        }
 
        public static YamlValue CreateList()
        {
            return new YamlValue
            {
                Type = ValueType.List,
                Text = null,
                Num = 0,
                MyUniqueId = new UniqueIdentifier(0),
                Items = new List<YamlValue>(),
                Fields = null
            };
        }
 
        public static YamlValue CreateDict()
        {
            return new YamlValue
            {
                Type = ValueType.DictOrObject,
                Text = null,
                Num = 0,
                MyUniqueId = new UniqueIdentifier(0),
                Items = new List<YamlValue>(),
                Fields = new Dictionary<string,YamlValue>()
            };
        }
 
        public override bool Equals(object obj)
        {
            return (obj is YamlValue)? Spaceship((YamlValue)obj, true) == 0 : false;
        }
 
        public override int GetHashCode()
        {
            switch (Type)
            {
                case ValueType.Null:
                    return 1;
                case ValueType.Bool:
                    return (Num != 0).GetHashCode();
                case ValueType.Number:
                    return Num.GetHashCode();
                case ValueType.String:
                    return Text.GetHashCode();
                case ValueType.List:
                case ValueType.DictOrObject:
                case ValueType.Block:
                case ValueType.Expression:
                    return GetUniqueId().GetHashCode();
                default:
                    throw new Exception("YamlValue.GetHashCode() - internal error: invalid type " + Type);
            }
        }
 
        public override string ToString()
        {
            switch (Type)
            {
                case ValueType.Null:
                    return "";
                case ValueType.Bool:
                    return (Num != 0) ? "true" : "false";
                case ValueType.Number:
                    return Num.ToString();
                case ValueType.String:
                    return Text ?? "";
                default:
                    return string.Join("\n", ToLines());
            }
        }
 
        private UniqueIdentifier GetUniqueId()
        {
            if (!MyUniqueId.IsZero_ThreadSafeAtomicityGuarantedByDotNetFramework)
                return MyUniqueId;
            lock (UnqIdentSeed)
            {
                if (MyUniqueId.IsZero())
                    UnqIdentSeed.Increment(MyUniqueId);
                return MyUniqueId;
            }
        }
 
        public string[] ToLines()
        {
            switch (Type)
            {
                case ValueType.Null:
                    return new[] { "null" };
                case ValueType.Bool:
                    return new string[] { (Num != 0) ? "true" : "false" };
                case ValueType.Number:
                    return new[] { Num.ToString() };
                case ValueType.String:
                    return new[] { Escape(Text, wrapInQuotes: true) };
                case ValueType.List:
                case ValueType.DictOrObject:
                case ValueType.Block:
                case ValueType.Expression:
                default:
                    throw new Exception("YamlValue.ToLines() - internal error: invalid type " + Type);
            }
        }
 
        public static string Escape(string str, bool wrapInQuotes)
        {
            str = str ?? "";
            StringBuilder res = new StringBuilder(wrapInQuotes? "\"" : "");
            for(int i=0; i < str.Length; i++)
                switch (str[i])
                {
                    case '\0':
                        res.Append("\\0");
                        break;
                    case '\a':
                        res.Append("\\a");
                        break;
                    case '\b':
                        res.Append("\\b");
                        break;
                    case '\f':
                        res.Append("\\f");
                        break;
                    case '\n':
                        res.Append("\\n");
                        break;
                    case '\r':
                        res.Append("\\r");
                        break;
                    case '\t':
                        res.Append("\\t");
                        break;
                    case '\v':
                        res.Append("\\v");
                        break;
                    case '"':
                        res.Append("\\\"");
                        break;
                    default:
                        res.Append(str[i]);
                        break;
                }
            return res.Append(wrapInQuotes? "\"" : "").ToString();
        }
       
        public int Spaceship(YamlValue val, bool forEquality)
        {
            bool is_eq = Type == val.Type && Text == val.Text && Num == val.Num && object.ReferenceEquals(Items, val.Items) && object.ReferenceEquals(Fields, val.Fields);
            if(is_eq)
                return 0;
            if(forEquality)
                return -1;
            if (Type != val.Type)
                return (Type < val.Type) ? -1 : 1;
            switch (Type)
            {
                case ValueType.Null:
                    return -1;  // null is less than anything else
                case ValueType.Bool:
                case ValueType.Number:
                    return (Num < val.Num) ? -1 : 1;
                case ValueType.String:
                    return string.Compare(Text, val.Text);
                case ValueType.List:
                case ValueType.DictOrObject:
                    return GetUniqueId().Spaceship(val.GetUniqueId());
                case ValueType.Block:
                case ValueType.Expression:
                default:
                    throw new Exception("YamlValue.Spaceship() - internal error: invalid type " + Type);
            }
        }
 
        public bool IsValuesOnly()
        {
            switch (Type)
            {
                case ValueType.Null:
                case ValueType.Bool:
                case ValueType.Number:
                case ValueType.String:
                    return true;
                case ValueType.List:
                case ValueType.DictOrObject:
                case ValueType.Block:
                case ValueType.Expression:
                default:
                    throw new Exception("YamlValue.IsValuesOnly() - internal error: invalid type " + Type);
            }
        }
    }
    */
}

