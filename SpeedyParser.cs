using System;
using System.Collections.Generic;
using System.Text;
using System.Linq.Expressions;

/*
 * Simple parser.
 * Author: Dariusz Pilarczyk (dpilarcz@gmail.com)
 * Licence: public domain / WTFPL (http://www.wtfpl.net/about/)
 * 
 * Main features:
 */

namespace ImmutableList
{
    class SpeedyParser
    {
        /// <summary>
        /// How to treat quotation marks (apostrophe and/or double quote).
        /// </summary>
        public enum QuoteSensitivity
        {
            /// <summary>
            /// Treat as ordinary character with no special meaning.
            /// </summary>
            Insensitive,

            /// <summary>
            /// Text enclosed in quotation marks is treated as single literal.
            /// </summary>
            Sensitive,

            /// <summary>
            /// Text enclosed in quotation marks is treated as single literal, escape character are allowed (like in C, C++, C#, Java, ...).
            /// </summary>
            CSharpLike,

            /// <summary>
            /// Text enclosed in quotation marks is treated as single literal, duplicating quotation mark is allowed (like in SQL, Delphi etc.)
            /// </summary>
            SqlLike
        }

        /// <summary>
        /// Parser options.
        /// </summary>
        public class ParserOptions
        {
            /// <summary>
            /// When true, the upper case and lower case in the input are treated as different characters.
            /// </summary>
            public bool IsCaseSensitive = true;

            /// <summary>
            /// Additional characters identifiers are made of (if null or empty, identifiers can contain only letters and/or digits).
            /// </summary>
            public string IdentCharsEx = "_";

            /// <summary>
            /// When true, the text contained in bracket is indivisible for parser.
            /// </summary>
            public bool IsBracketSensitive = true;

            /// <summary>
            /// How to treat double qoutation mark (").
            /// </summary>
            public QuoteSensitivity DoubleQuoteSensitivity = QuoteSensitivity.CSharpLike;

            /// <summary>
            /// How to treat single qoutation mark (').
            /// </summary>
            public QuoteSensitivity SingleQuoteSensitivity = QuoteSensitivity.CSharpLike;

            /// <summary>
            /// Exception to be thrown when opening bracket without corresponding closing bracket is found.
            /// If this field is set to null, the parser ignores the error.
            /// </summary>
            public Func<SpeedyParser, Exception> MissingClosingBracketError = null;

            /// <summary>
            /// Exception to be thrown when closing quote is missing.
            /// If this field is set to null, the parser ignores the error.
            /// </summary>
            public Func<SpeedyParser, Exception> MissingClosingQuoteError = null;

            /// <summary>
            /// List of comments, if the second string in tuple is null or empty, the tuple denotes
            /// single line comment (to the end of line).
            /// 
            /// Example (C# comments):
            ///     new Tuple<string,string>[] { new Tuple<string,string>("//", ""), new Tuple<string,string>("/*", "*/") }
            /// </summary>
            public Tuple<string, string>[] Comments = null;
        }

        private static readonly ParserOptions DefaultParserOptions = new ParserOptions();

        public ParserOptions Options = DefaultParserOptions;
        protected Expression<Func<SpeedyParser, bool>> CompiledExpression;
        protected Func<SpeedyParser, bool> Body = null;

        public Dictionary<string, List<string>> Result = null;
        protected List<string> Sentinels;

        protected string MyString = "";
        protected long CurrentPos = 0;

        protected SpeedyParser()
        {
        }

        public SpeedyParser(Expression<Func<SpeedyParser, bool>> parserBody)
        {
            Sentinels = new List<string>();
            CompiledExpression = (Expression<Func<SpeedyParser, bool>>)CompileExpression(parserBody);
            Body = CompiledExpression.Compile();
        }

        public SpeedyParser Clone()
        {
            return new SpeedyParser()
            {
                Body = this.Body,
                Options = this.Options,
                Result = null,
                Sentinels = Sentinels,
                MyString = "",
                CurrentPos = 0
            };
        }

        public bool TryMatch(string str)
        {
            MyString = str;
            CurrentPos = 0;

            return Body(this);
        }

        protected virtual Expression CompileExpression(Expression expr)
        {
            // https://msdn.microsoft.com/pl-pl/library/bb352032(v=vs.110).aspx
            // good examples:
            // http://stackoverflow.com/questions/3716492/what-does-expression-quote-do-that-expression-constant-can-t-already-do

            BinaryExpression be = expr as BinaryExpression;
            UnaryExpression une = expr as UnaryExpression;
            switch (expr.NodeType)
            {
                case ExpressionType.Assign:         // a = b
                    return (be != null) ? Expression.Assign(be.Left, CompileExpression(be.Right)) : expr;
                case ExpressionType.AddAssign:      // a += b
                    return (be != null) ? Expression.AddAssign(be.Left, CompileExpression(be.Right)) : expr;
                case ExpressionType.AddAssignChecked: // a += b with overflow checking
                    return (be != null) ? Expression.AddAssignChecked(be.Left, CompileExpression(be.Right)) : expr;
                case ExpressionType.AndAssign:      // a &= b
                    return (be != null) ? Expression.AndAssign(be.Left, CompileExpression(be.Right)) : expr;
                case ExpressionType.DivideAssign:	// a /= b
                    return (be != null) ? Expression.DivideAssign(be.Left, CompileExpression(be.Right)) : expr;
                case ExpressionType.ExclusiveOrAssign:  // a ^= b
                    return (be != null) ? Expression.ExclusiveOrAssign(be.Left, CompileExpression(be.Right)) : expr;
                case ExpressionType.LeftShiftAssign:	// a <<= b
                    return (be != null) ? Expression.LeftShiftAssign(be.Left, CompileExpression(be.Right)) : expr;
                case ExpressionType.ModuloAssign:	    // a %= b
                    return (be != null) ? Expression.ModuloAssign(be.Left, CompileExpression(be.Right)) : expr;
                case ExpressionType.MultiplyAssign:	    // a *= b
                    return (be != null) ? Expression.MultiplyAssign(be.Left, CompileExpression(be.Right)) : expr;
                case ExpressionType.MultiplyAssignChecked:  // a *= b
                    return (be != null) ? Expression.MultiplyAssignChecked(be.Left, CompileExpression(be.Right)) : expr;
                case ExpressionType.OrAssign:	        // a |= b
                    return (be != null) ? Expression.OrAssign(be.Left, CompileExpression(be.Right)) : expr;
                case ExpressionType.PowerAssign:	    // A compound assignment operation that raises a number to a power, such as (a ^= b) in Visual Basic.
                    return (be != null) ? Expression.PowerAssign(be.Left, CompileExpression(be.Right)) : expr;
                case ExpressionType.RightShiftAssign:   // a >>= b
                    return (be != null) ? Expression.RightShiftAssign(be.Left, CompileExpression(be.Right)) : expr;
                case ExpressionType.SubtractAssign:	    // a -= b
                    return (be != null) ? Expression.SubtractAssign(be.Left, CompileExpression(be.Right)) : expr;
                case ExpressionType.SubtractAssignChecked:  // a -= b
                    return (be != null) ? Expression.SubtractAssignChecked(be.Left, CompileExpression(be.Right)) : expr;

                case ExpressionType.Conditional:    // a > b ? a : b
                    ConditionalExpression ce_ex = expr as ConditionalExpression;
                    return (ce_ex != null) ? Expression.Condition(CompileExpression(ce_ex.Test), CompileExpression(ce_ex.IfTrue), CompileExpression(ce_ex.IfFalse)) : expr;
                case ExpressionType.Coalesce:       // a ?? b
                    return (be != null) ? Expression.Coalesce(CompileExpression(be.Left), CompileExpression(be.Right)) : expr;
                case ExpressionType.OrElse:	        // a || b
                    return (be != null) ? Expression.OrElse(CompileExpression(be.Left), CompileExpression(be.Right)) : expr;
                case ExpressionType.AndAlso:        // a && b
                    return (be != null) ? Expression.AndAlso(CompileExpression(be.Left), CompileExpression(be.Right)) : expr;
                case ExpressionType.Or:	            // a | b
                    return (be != null) ? Expression.Or(CompileExpression(be.Left), CompileExpression(be.Right)) : expr;
                case ExpressionType.ExclusiveOr:	// a ^ b
                    return (be != null) ? Expression.ExclusiveOr(CompileExpression(be.Left), CompileExpression(be.Right)) : expr;
                case ExpressionType.And:            // a & b
                    return (be != null) ? Expression.And(CompileExpression(be.Left), CompileExpression(be.Right)) : expr;

                case ExpressionType.Equal:	            // a == b
                    return (be != null) ? Expression.Equal(CompileExpression(be.Left), CompileExpression(be.Right)) : expr;
                case ExpressionType.NotEqual:	        // a != b
                    return (be != null) ? Expression.NotEqual(CompileExpression(be.Left), CompileExpression(be.Right)) : expr;
                case ExpressionType.GreaterThan:	    // a > b
                    return (be != null) ? Expression.GreaterThan(CompileExpression(be.Left), CompileExpression(be.Right)) : expr;
                case ExpressionType.GreaterThanOrEqual:	// a >= b
                    return (be != null) ? Expression.GreaterThanOrEqual(CompileExpression(be.Left), CompileExpression(be.Right)) : expr;
                case ExpressionType.LessThan:	        // a < b
                    return (be != null) ? Expression.LessThan(CompileExpression(be.Left), CompileExpression(be.Right)) : expr;
                case ExpressionType.LessThanOrEqual:	// a <= b
                    return (be != null) ? Expression.LessThanOrEqual(CompileExpression(be.Left), CompileExpression(be.Right)) : expr;
                case ExpressionType.TypeAs:	            // obj as SampleType
                case ExpressionType.TypeIs:	            // obj is SampleType
                    return expr;
                case ExpressionType.LeftShift:	  // a << b
                    return (be != null) ? Expression.LeftShift(CompileExpression(be.Left), CompileExpression(be.Right)) : expr;
                case ExpressionType.RightShift:	  // a >> b
                    return (be != null) ? Expression.RightShift(CompileExpression(be.Left), CompileExpression(be.Right)) : expr;

                case ExpressionType.Add:            // a + b
                    return (be != null) ? Expression.Add(CompileExpression(be.Left), CompileExpression(be.Right)) : expr;
                case ExpressionType.AddChecked:     // a + b with overflow checking
                    return (be != null) ? Expression.AddChecked(CompileExpression(be.Left), CompileExpression(be.Right)) : expr;
                case ExpressionType.Decrement:	    // a - 1
                    return (une != null) ? Expression.Decrement(CompileExpression(une.Operand)) : expr;
                case ExpressionType.Increment:	    // a + 1
                    return (une != null) ? Expression.Increment(CompileExpression(une.Operand)) : expr;
                case ExpressionType.Subtract:	    // a - b
                    return (be != null) ? Expression.Subtract(CompileExpression(be.Left), CompileExpression(be.Right)) : expr;
                case ExpressionType.SubtractChecked:    // a - b
                    return (be != null) ? Expression.SubtractChecked(CompileExpression(be.Left), CompileExpression(be.Right)) : expr;

                case ExpressionType.Multiply:	        // a * b
                    return (be != null) ? Expression.Multiply(CompileExpression(be.Left), CompileExpression(be.Right)) : expr;
                case ExpressionType.MultiplyChecked:    // a * b
                    return (be != null) ? Expression.MultiplyChecked(CompileExpression(be.Left), CompileExpression(be.Right)) : expr;
                case ExpressionType.Divide:	            // a / b
                    return (be != null) ? Expression.Divide(CompileExpression(be.Left), CompileExpression(be.Right)) : expr;
                case ExpressionType.Modulo:	            // a % b
                    return (be != null) ? Expression.Modulo(CompileExpression(be.Left), CompileExpression(be.Right)) : expr;
                case ExpressionType.Power:	  // raises a number to a power, such as (a ^ b) in Visual Basic.
                    return (be != null) ? Expression.Power(CompileExpression(be.Left), CompileExpression(be.Right)) : expr;

                case ExpressionType.PreDecrementAssign:     // --a
                    return (une != null) ? Expression.PreDecrementAssign(CompileExpression(une.Operand)) : expr;
                case ExpressionType.PreIncrementAssign:     // ++a
                    return (une != null) ? Expression.PreIncrementAssign(CompileExpression(une.Operand)) : expr;
                case ExpressionType.PostDecrementAssign:    // a--
                    return (une != null) ? Expression.PostDecrementAssign(CompileExpression(une.Operand)) : expr;
                case ExpressionType.PostIncrementAssign:	// a++
                    return (une != null) ? Expression.PostIncrementAssign(CompileExpression(une.Operand)) : expr;
                case ExpressionType.Negate:	        // -a
                    return (une != null) ? Expression.Negate(CompileExpression(une.Operand)) : expr;
                case ExpressionType.NegateChecked:	// -a
                    return (une != null) ? Expression.NegateChecked(CompileExpression(une.Operand)) : expr;
                case ExpressionType.Not:	        // ~a
                    return (une != null) ? Expression.Not(CompileExpression(une.Operand)) : expr;
                case ExpressionType.OnesComplement:	// ~a in C#
                    return (une != null) ? Expression.OnesComplement(CompileExpression(une.Operand)) : expr;
                case ExpressionType.UnaryPlus:	    // +a
                    return (une != null) ? Expression.UnaryPlus(CompileExpression(une.Operand)) : expr;
                case ExpressionType.ArrayIndex:     // a[b]
                    return (be != null) ? Expression.ArrayIndex(be.Left, CompileExpression(be.Right)) : expr;
                case ExpressionType.ArrayLength:    // a.Length
                    return (une != null) ? Expression.ArrayLength(CompileExpression(une.Operand)) : expr;

                case ExpressionType.Index:	        // An index operation or an operation that accesses a property that takes arguments.
                case ExpressionType.Block:          // block expression
                    return expr;
                case ExpressionType.Convert:        // (SampleType)obj
                case ExpressionType.ConvertChecked: // (SampleType)obj
                case ExpressionType.DebugInfo:	    // Debugging information.
                    return expr;
                case ExpressionType.Call:       // obj.sampleMethod()
                    var call_e = expr as MethodCallExpression;
                    return (call_e != null) ? CompileCall(call_e) : expr;
                case ExpressionType.Constant:   // a constant value
                    var const_e = expr as ConstantExpression;
                    if (const_e != null && const_e.Value != null && (const_e.Value is string))
                        Sentinels.Add(const_e.Value as string);
                    return expr;
                case ExpressionType.Default:	// A default value.
                case ExpressionType.IsFalse:    // A false condition value.
                    return (une != null) ? Expression.IsFalse(CompileExpression(une.Operand)) : expr;
                case ExpressionType.IsTrue:	    // A true condition value.
                    return (une != null) ? Expression.IsTrue(CompileExpression(une.Operand)) : expr;
                case ExpressionType.Lambda:	    // a => a + a
                    var lambda_e = expr as LambdaExpression;
                    return (lambda_e == null) ? expr : Expression.Lambda(CompileExpression(lambda_e.Body), lambda_e.Parameters);
                case ExpressionType.Quote:	    // An expression that has a constant value of type Expression. A Quote node can contain references to parameters that are defined in the context of the expression it represents.
                    return (une != null) ? Expression.Quote(CompileExpression(une.Operand)) : expr;
                case ExpressionType.Dynamic:    // A dynamic operation.
                case ExpressionType.Extension:	// An extension expression.
                case ExpressionType.Goto:	    // A "go to" expression, such as goto Label in C# or GoTo Label in Visual Basic.
                case ExpressionType.Invoke:	    // An operation that invokes a delegate or lambda expression, such as sampleDelegate.Invoke().
                case ExpressionType.Label:	    // A label.
                case ExpressionType.Loop:	    // A loop, such as for or while.
                case ExpressionType.MemberAccess:	// An operation that reads from a field or property, such as obj.SampleProperty.
                case ExpressionType.ListInit:	// An operation that creates a new IEnumerable object and initializes it from a list of elements, such as new List<SampleType>(){ a, b, c } in C# 
                case ExpressionType.MemberInit:	// An operation that creates a new object and initializes one or more of its members, such as new Point { X = 1, Y = 2 } in C#
                case ExpressionType.New:	    // new SampleType().
                case ExpressionType.NewArrayBounds:	// An operation that creates a new array, in which the bounds for each dimension are specified, such as new SampleType[dim1, dim2]
                case ExpressionType.NewArrayInit:   // An operation that creates a new one-dimensional array and initializes it from a list of elements, such as new SampleType[]{a, b, c}
                case ExpressionType.Parameter:	// A reference to a parameter or variable that is defined in the context of the expression. For more information, see ParameterExpression.
                case ExpressionType.Switch:	    // A switch operation, such as switch in C#
                case ExpressionType.Throw:	    // throw new Exception().
                case ExpressionType.Try:	    // A try-catch expression.
                case ExpressionType.TypeEqual:	// An exact type test.
                case ExpressionType.RuntimeVariables:   // A list of run-time variables. For more information, see RuntimeVariablesExpression.
                case ExpressionType.Unbox:	    // An unbox value type operation, such as unbox and unbox.any instructions in MSIL.                
                default:
                    return expr;
            }
        }

        protected virtual Expression CompileCall(MethodCallExpression expr)
        {
            if (expr.Object.Type == typeof(SpeedyParser))
            {
                var pars = expr.Method.GetParameters() ?? new System.Reflection.ParameterInfo[] { };
                switch (expr.Method.Name)
                {
                    case "If":
                    case "While":
                    case "IfOneOf":
                    case "WhileOneOf":
                        if (pars.Length > 0 && expr.Arguments.Count == pars.Length && (pars[0].Name == "sentinel" || pars[0].Name == "sentinels"))
                        {
                            TryAddSentinels(expr.Arguments[0], expr.Method.Name, 0);
                            Expression[] args = ConvertArgumentsToLambdas(expr.Arguments, 1);
                            return Expression.Call(expr.Object, GetImplementationMethod(expr.Object.Type, expr.Method.Name), args);
                        }
                        break;
                }
            }
            return expr;
        }

        public class ECompilationError : Exception
        {
            public ECompilationError(string msg)
                : base(msg)
            {
            }
        }

        public static System.Reflection.MethodInfo GetImplementationMethod(Type objectType, string methodName)
        {
            string searchMethod = methodName + "_implementation";
            var res = objectType.GetMethod(searchMethod, System.Reflection.BindingFlags.NonPublic | System.Reflection.BindingFlags.Instance);
            if (res == null)
                throw new ECompilationError(string.Format("Method {0} not found", searchMethod));
            return res;
        }

        private void TryAddSentinels(Expression expr, string callingFunction, int paramInx)
        {
            object obj = Evaluate(expr, callingFunction, paramInx);
            if (obj == null)
                throw new ECompilationError(string.Format("Parameter {0} of function {1} should not be null", paramInx + 1, callingFunction));
            if (obj is string)
            {
                string sents = ((obj as string) ?? "").Trim();
                if (sents == "")
                    throw new ECompilationError(string.Format("Parameter {0} of function {1} should not be an empty string", paramInx + 1, callingFunction));
                Sentinels.Add(sents);
            }
            if (obj is string[])
            {
                string[] sents = obj as string[];
                foreach (string sentinel in sents)
                    Sentinels.Add(sentinel);
            }
        }

        public Expression[] ConvertArgumentsToLambdas(System.Collections.ObjectModel.ReadOnlyCollection<Expression> arguments, int startInx)
        {
            Expression[] res = new Expression[arguments.Count];
            for (int i = 0; i < startInx; i++)
                res[i] = arguments[i];
            for (int i_res = startInx; i_res < arguments.Count; i_res++)
            {
                Expression expr = arguments[i_res];
                if (expr.NodeType != ExpressionType.NewArrayInit)
                    res[i_res] = expr;
                else
                {
                    var na_e = expr as NewArrayExpression;
                    if (na_e == null)
                        res[i_res] = expr;
                    else
                    {
                        var lambdas = new Expression[na_e.Expressions.Count];
                        for (int i = 0; i < na_e.Expressions.Count; i++)
                            lambdas[i] = Expression.Lambda(CompileExpression(na_e.Expressions[i]));
                        res[i_res] = Expression.NewArrayInit(typeof(Func<bool>), lambdas);
                    }
                }
            }
            return res;
        }

        protected static object Evaluate(Expression expr, string callingFunction, int paramInx)
        {
            //A little optimization for constant expressions
            if (expr.NodeType == ExpressionType.Constant)
                return ((ConstantExpression)expr).Value;
            try
            {
                return Expression.Lambda(expr).Compile().DynamicInvoke();
            }
            catch
            {
                throw new ECompilationError(string.Format("Cannot evaluate parameter {0} of function {1}", paramInx + 1, callingFunction));
            }
        }

        public bool SetOptions(ParserOptions opt)
        {
            Options = opt ?? DefaultParserOptions;
            return true;
        }

        public bool If(string sentinel, params bool[] body)
        {
            return true;
        }

        protected bool If_implementation(string str, Func<bool>[] body)
        {
            if (!Test(str, consumeInput: true))
                return true;
            if (body != null)
                foreach (var f in body)
                    if (!f())
                        return false;
            return true;
        }

        public bool IfOneOf(string[] sentinels, params bool[] body)
        {
            return true;
        }

        protected bool IfOneOf_implementation(string[] sentinels, Func<bool>[] body)
        {
            if (!TestOneOf(sentinels))
                return true;
            if (body != null)
                foreach (var f in body)
                    if (!f())
                        return false;
            return true;
        }

        public bool While(string sentinel, params bool[] body)
        {
            return true;
        }

        protected bool While_implementation(string str, Func<bool>[] body)
        {
            while (Test(str, consumeInput: true))
                if (body != null)
                    foreach (var f in body)
                        if (!f())
                            return false;
            return true;
        }

        public bool WhileOneOf(string[] sentinels, params bool[] body)
        {
            return true;
        }

        protected bool WhileOneOf_implementation(string[] sentinels, Func<bool>[] body)
        {
            while (TestOneOf(sentinels))
                if (body != null)
                    foreach (var f in body)
                        if (!f())
                            return false;
            return true;
        }

        public bool Eat(string varName)
        {
            GotoPrintChar();
            long startPos = CurrentPos;
            while (GotoPrintChar() != '\0' && !PointsAtSentinel())
                GotoNextMatchingPos();
            if (varName != null && (varName = varName.Trim()) != "" && varName[0] != '_')
                Add2Result(varName, GetInputSubstring(startPos, CurrentPos).Trim());
            return true;
        }

        private void GotoNextMatchingPos()
        {
            switch (GetCharAt(CurrentPos))
            {
                case '\'':
                    GotoClosingQuote('\'');
                    break;
                case '"':
                    GotoClosingQuote('"');
                    break;
                case '(':
                    if (Options.IsBracketSensitive)
                        GotoClosingBracket(')');
                    break;
                case '[':
                    if (Options.IsBracketSensitive)
                        GotoClosingBracket(']');
                    break;
                case '{':
                    if (Options.IsBracketSensitive)
                        GotoClosingBracket('}');
                    break;
            }
            if (!IsIdentChar(GetCharAt(CurrentPos)))
                CurrentPos++;
            else
                while (IsIdentChar(GetCharAt(CurrentPos)))
                    CurrentPos++;
        }

        private void GotoClosingBracket(char closing_bracket)
        {
            CurrentPos++;
            int n_brackets = 1;
            char c;
            while ((c = GetCharAt(++CurrentPos)) != '\0')
                switch (c)
                {
                    case '\'':
                        GotoClosingQuote('\'');
                        break;
                    case '"':
                        GotoClosingQuote('"');
                        break;
                    case '(':
                    case '[':
                    case '{':
                        if (Options.MissingClosingBracketError != null)
                            GotoClosingBracket((c == '(') ? ')' : (c == '[') ? ']' : '}');
                        else
                            n_brackets++;
                        break;
                    case ')':
                    case ']':
                    case '}':
                        if (c == closing_bracket)
                            return;
                        else if (Options.MissingClosingBracketError != null)
                            throw Options.MissingClosingBracketError(this);
                        else if (--n_brackets <= 0)
                            return;
                        break;
                }
            if (Options.MissingClosingBracketError != null)
                throw Options.MissingClosingBracketError(this);
        }

        private void GotoClosingQuote(char closing_quote)
        {
            char c;
            switch ((closing_quote == '\'') ? Options.SingleQuoteSensitivity : Options.DoubleQuoteSensitivity)
            {
                case QuoteSensitivity.Insensitive:
                    return;
                case QuoteSensitivity.Sensitive:
                    while ((c = GetCharAt(++CurrentPos)) != closing_quote)
                        if (c == '\0')
                        {
                            if (Options.MissingClosingQuoteError != null)
                                throw Options.MissingClosingQuoteError(this);
                            return;
                        }
                    return;
                case QuoteSensitivity.CSharpLike:
                    while ((c = GetCharAt(++CurrentPos)) != closing_quote)
                        switch (c)
                        {
                            case '\0':
                                if (Options.MissingClosingQuoteError != null)
                                    throw Options.MissingClosingQuoteError(this);
                                return;
                            case '\\':
                                CurrentPos++;
                                break;
                        }
                    return;
                case QuoteSensitivity.SqlLike:
                    while ((c = GetCharAt(++CurrentPos)) != '\0')
                        if (c == closing_quote)
                            if (GetCharAt(CurrentPos + 1) == closing_quote)
                                CurrentPos++;
                            else
                                return;
                    if (Options.MissingClosingQuoteError != null)
                        throw Options.MissingClosingQuoteError(this);
                    return;
            }
        }

        private string GetInputSubstring(long startPos, long endPos)
        {
            return MyString.Substring((int)startPos, (int)(endPos - startPos));
        }

        private bool PointsAtSentinel()
        {
            long startPos = CurrentPos;
            for (int i = Sentinels.Count - 1; i >= 0; i--)
            {
                CurrentPos = startPos;
                if (Test(Sentinels[i], consumeInput: false))
                {
                    CurrentPos = startPos;
                    return true;
                }
            }
            CurrentPos = startPos;
            return false;
        }

        public bool Eof
        {
            get
            {
                return GotoPrintChar() == '\0';
            }
        }

        public bool Add2Result(string varName, string varValue)
        {
            varName = varName ?? "";
            varValue = varValue ?? "";
            if (Result == null)
                Result = new Dictionary<string, List<string>>();
            if (!Result.ContainsKey(varName))
                Result[varName] = new List<string>();
            Result[varName].Add(varValue);
            return true;
        }

        public virtual bool IsIdentChar(char c)
        {
            return char.IsLetterOrDigit(c) || Options.IdentCharsEx != null && Options.IdentCharsEx.IndexOf(c) >= 0;
        }

        protected bool TestOneOf(string[] sentinels)
        {
            GotoPrintChar();
            foreach (string sent in sentinels)
                if (Test(sent, consumeInput: true))
                    return true;
            return false;
        }

        public bool Test(string pattern, bool consumeInput)
        {
            int endPos = pattern.LastIndexOf("->");
            if (endPos < 0)
                endPos = pattern.Length;
            GotoPrintChar();
            long savePos = CurrentPos;
            int pattPos = 0;
            while ((pattPos = PatternGotoPrintChar(pattern, pattPos)) < endPos)
            {
                CurrentPos = savePos;
                if (!TestSingleItem(pattern, ref pattPos))
                {
                    CurrentPos = savePos;
                    return false;
                }
            }
            if (!consumeInput)
                CurrentPos = savePos;
            else if (endPos + 2 < pattern.Length)
            {
                string varName = pattern.Substring(endPos + 2).Trim();
                if (varName != "" && varName[0] != '_')
                {
                    string varValue = pattern.Substring(0, endPos).Trim();
                    Add2Result(varName, varValue);
                }
            }
            return true;
        }

        protected bool TestSingleItem(string str, ref int pos)
        {
            pos = PatternGotoPrintChar(str, pos);
            GotoPrintChar();
            if (pos >= str.Length)
                return true;
            if (CurrentPos > 0 && IsIdentChar(str[pos]) && IsIdentChar(GetCharAt(CurrentPos - 1)))
                return false;
            int itemBegin = pos;
            while (pos < str.Length && !char.IsWhiteSpace(str[pos]))
            {
                char c = GetCharAt(CurrentPos);
                if (c != str[pos] && (Options.IsCaseSensitive || char.ToUpper(c) != char.ToUpper(str[pos])))
                    return false;
                CurrentPos++;
                pos++;
            }
            if (CurrentPos < MyString.Length && IsIdentChar(str[pos - 1]) && IsIdentChar(GetCharAt(CurrentPos)))
                return false;
            return true;
        }

        public static int PatternGotoPrintChar(string pattern, int pos)
        {
            while (pos < pattern.Length && char.IsWhiteSpace(pattern[pos]))
                pos++;
            return pos;
        }

        public char GotoPrintChar()
        {
            if (Options.Comments == null)
                return GotoPrintChar_Basic();
            char c;
            while ((c = GotoPrintChar_Basic()) != '\0')
            {
                Tuple<string, string> comm = FindMatchingComment();
                if (comm == null)
                    return c;
                CurrentPos += comm.Item1.Length;
                if (comm.Item2 == null || comm.Item2.Length <= 0)
                    while ((c = GetCharAt(CurrentPos)) != '\0' && c != '\n' && c != '\r')
                        CurrentPos++;
                else
                {
                }
            }
            return c;
        }

        private Tuple<string, string> FindMatchingComment()
        {
            foreach (var comm in Options.Comments)
                if (comm.Item1 != null && comm.Item1.Length > 0 && TestSubstring(comm.Item1))
                    return comm;
            return null;
        }

        private bool TestSubstring(string str)
        {
            for (int i = 0; i < str.Length; i++)
                if (GetCharAt(CurrentPos + i) != str[i])
                    return false;
            CurrentPos += str.Length;
            return true;
        }

        public char GotoPrintChar_Basic()
        {
            char c;
            while ((c = GetCharAt(CurrentPos)) != '\0' && char.IsWhiteSpace(c))
                CurrentPos++;
            return c;
        }

        public char GetCharAt(long pos)
        {
            return (pos < 0 || pos >= MyString.Length) ? '\0' : MyString[(int)pos];
        }
    }
}
