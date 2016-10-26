using System;
using System.Collections.Generic;
using System.Text;
using System.Linq.Expressions;

/*
 * Simple parser.
 * Author: Dariusz Pilarczyk (dpilarcz@gmail.com)
 * github: https://github.com/darekp7/SpeedyParser
 * 
 * Licence: public domain / unlicenced / WTFPL
 * 
 * This is free and unencumbered software released into the public domain.
 * 
 * Anyone is free to copy, modify, publish, use, compile, sell, or
 * distribute this software, either in source code form or as a compiled
 * binary, for any purpose, commercial or non-commercial, and by any
 * means.
 * 
 * In jurisdictions that recognize copyright laws, the author or authors
 * of this software dedicate any and all copyright interest in the
 * software to the public domain. We make this dedication for the benefit
 * of the public at large and to the detriment of our heirs and
 * successors. We intend this dedication to be an overt act of
 * relinquishment in perpetuity of all present and future rights to this
 * software under copyright law.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR
 * OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
 * ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
 * OTHER DEALINGS IN THE SOFTWARE.
 * 
 * For more information, please refer to <http://unlicense.org/>
 * 
 * 
 * 
 * Main features:
 */

namespace SpeedyTools
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
            /// List of comments, if the second string in tuple is null or empty, the tuple denotes
            /// single line comment (to the end of line).
            /// 
            /// Example (C#-like comments):
            ///     new Tuple<string,string>[] { new Tuple<string,string>("//", ""), new Tuple<string,string>("/*", "*/") }
            /// </summary>
            public Tuple<string, string>[] Comments = null;

            /// <summary>
            /// Action to execute when opening bracket without corresponding closing bracket is found. 
            /// Optionally may return exception to be thrown.
            /// If this field is set to null or returns null, the parser ignores the error.
            /// </summary>
            public Func<SpeedyParser, Exception> OnMissingClosingBracket = null;

            /// <summary>
            /// Action to execute when closing quote is missing. 
            /// Optionally may return exception to be thrown.
            /// If this field is set to null or returns null, the parser ignores the error.
            /// </summary>
            public Func<SpeedyParser, Exception> OnMissingClosingQuote = null;

            /// <summary>
            /// Exception to be thrown when end of comment is missing.
            /// Optionally may return exception to be thrown.
            /// If this field is set to null or returns null, the parser ignores the error.
            /// </summary>
            public Func<SpeedyParser, Exception> OnMissingEndOfComment = null;
        }

        public enum SentinelState
        {
            /// <summary>
            /// Active sentinel occurring in at least one unconditional test.
            /// </summary>
            Active,

            /// <summary>
            /// Passive sentinel consumed by parser.
            /// </summary>
            Consumeable,

            /// <summary>
            /// Inactive sentinel treated as ordinary text.
            /// </summary>
            Disabled
        }

        protected static readonly ParserOptions DefaultParserOptions = new ParserOptions();

        public ParserOptions Options = DefaultParserOptions;
        protected Func<SpeedyParser, bool> Body = null;
        protected Dictionary<string, Func<SpeedyParser, bool>> SubExpressions = null;
        protected SentinelsList Sentinels;
        protected long FCounter = 0;
        protected long FLastLoopCounter = 0;
        protected bool FIf_status = false;

        public ParserInput Input;
        protected Dictionary<string, string> LocalVariables = null;
        protected SpeedyParserResult Result = new SpeedyParserResult();
        protected System.Collections.Concurrent.BlockingCollection<SpeedyParserResult> OutputCollection = null;
        protected List<SpeedyParserResult> OutputList = null;

        protected SpeedyParser()
        {
        }

        /// <summary>
        /// Constructor. Constructor may be time-consuming, because it compiles the expression passed as a second parameter.
        /// </summary>
        /// <param name="parserOptions">parsing options,</param>
        /// <param name="parserBody">expression describing how to parse the input.</param>
        public SpeedyParser(ParserOptions parserOptions, Expression<Func<SpeedyParser, bool>> parserBody)
            : this(parserOptions, parserBody, null)
        {
        }

        /// <summary>
        /// Constructor. Constructor may be time-consuming, because it compiles the expressions passed as 
        /// a second and third parameter.
        /// </summary>
        /// <param name="parserOptions">parsing options,</param>
        /// <param name="parserBody">expression describing how to parse the input,</param>
        /// <param name="subExpressions">subexpressions.</param>
        public SpeedyParser(ParserOptions parserOptions, Expression<Func<SpeedyParser, bool>> parserBody, 
            Dictionary<string, Expression<Func<SpeedyParser, bool>>> subExpressions)
        {
            Options = parserOptions ?? DefaultParserOptions;
            var compiledExpression = (Expression<Func<SpeedyParser, bool>>)CompileExpression(parserBody);
            Body = compiledExpression.Compile();
            if (subExpressions != null && subExpressions.Count > 0)
            {
                SubExpressions = new Dictionary<string, Func<SpeedyParser, bool>>();
                foreach(var kvp in subExpressions)
                    SubExpressions[kvp.Key] = ((Expression<Func<SpeedyParser, bool>>)CompileExpression(kvp.Value)).Compile();
            }
        }

        /// <summary>
        /// Makes copy of the current object. This copy is used during matching. 
        /// </summary>
        /// <returns>copy of the current object.</returns>
        protected virtual SpeedyParser CreateRunInstance(string str, Func<string> readNextLine, int pos)
        {
            var res = new SpeedyParser();
            res.Options = Options;
            res.Body = Body;
            res.SubExpressions = SubExpressions;
            res.Sentinels = Sentinels.Clone();
            res.Sentinels.StartMatching();

            res.Input = new ParserInput(res, str, readNextLine, pos);

            res.Result.Clear();
            res.FCounter = 0;
            res.FLastLoopCounter = 0;
            res.FIf_status = false;
            res.LocalVariables = null;
            res.OutputCollection = null;
            res.OutputList = null;
            return res;
        }

        /// <summary>
        /// Returns information if last act of parsing succeeded
        /// </summary>
        public bool Success
        {
            get
            {
                return Result.Success;
            }
        }

        /// <summary>
        /// The method parses the input. If parsing fails, the method returns false, otherwise returns true.
        /// The result of parsing is returned in Result or handled by user-defined function(s).
        /// If the parsing expression contains Yield(), the result may be incomplete i.e. the result may contain
        /// (and typically will do) only the last yielded set of variables.
        /// </summary>
        /// <param name="input">string to be parsed.</param>
        /// <returns>a SpeedyParserResult value with the Success member set to true if input was succesfully parsed, 
        ///          otherwise set to false.</returns>
        public SpeedyParserResult TryMatch(string input)
        {
            var runner = CreateRunInstance(input, null, 0);
            runner.Result.Success = runner.Body(runner);
            return runner.Result;
        }

        /// <summary>
        /// The method parses the input. If parsing fails, the method returns false, otherwise returns true.
        /// The result of parsing is returned in Result or handled by user-defined function(s).
        /// If the parsing expression contains Yield(), the result may be incomplete i.e. the result may contain
        /// (and typically will do) only the last yielded set of variables.
        /// </summary>
        /// <param name="input">lines to be parsed.</param>
        /// <returns>a SpeedyParserResult value with the Success member set to true if input was succesfully parsed, 
        ///          otherwise set to false.</returns>
        public SpeedyParserResult TryMatch(string[] inputLines)
        {
            int i_ln = 0;
            Func<string> readLine = null;
            if (inputLines != null && inputLines.Length > 0)
                readLine = () =>
                    (inputLines != null && i_ln < inputLines.Length) ? inputLines[i_ln++] ?? "" : null;
            return TryMatch(readLine);
        }

        /// <summary>
        /// The method parses the input. If parsing fails, the method returns false, otherwise returns true.
        /// The result of parsing is returned in Result or handled by user-defined function(s).
        /// If the parsing expression contains Yield(), the result may be incomplete i.e. the result may contain
        /// (and typically will do) only the last yielded set of variables.
        /// </summary>
        /// <param name="input">lines to be parsed.</param>
        /// <returns>a SpeedyParserResult value with the Success member set to true if input was succesfully parsed, 
        ///          otherwise set to false.</returns>
        public SpeedyParserResult TryMatch(IEnumerable<string> lines)
        {
            Func<string> readLine = null;
            if (lines != null)
            {
                IEnumerator<string> enumerator = lines.GetEnumerator();
                readLine = () =>
                    (enumerator.MoveNext()) ? enumerator.Current ?? "" : null;
            }
            return TryMatch(readLine);
        }

        /// <summary>
        /// The method parses the input. If parsing fails, the method returns false, otherwise returns true.
        /// The result of parsing is returned in Result or handled by user-defined function(s).
        /// If the parsing expression contains Yield(), the result may be incomplete i.e. the result may contain
        /// (and typically will do) only the last yielded set of variables.
        /// </summary>
        /// <param name="input">lines to be parsed.</param>
        /// <returns>a SpeedyParserResult value with the Success member set to true if input was succesfully parsed, 
        ///          otherwise set to false.</returns>
        public SpeedyParserResult TryMatch(Func<string> readLine)
        {
            var runner = CreateRunInstance("", readLine, 0);
            runner.Result.Success = runner.Body(runner);
            return runner.Result;
        }

        /// <summary>
        /// The enumerator, which parses the input and returns all results (i.e. returns all collections stored in the
        /// Result class member). 
        /// If parsing fails, method stores false in Succeeded property, otherwise stores true.
        /// </summary>
        /// <param name="input">lines to be parsed.</param>
        /// <param name="succeeded">true if parser expression returned true, otherwise false</param>
        /// <returns>the enumerator</returns>
        public IEnumerable<SpeedyParserResult> TryMatchLoop(IEnumerable<string> lines)
        {
            Func<string> readLine = null;
            if (lines != null)
            {
                IEnumerator<string> enumerator = lines.GetEnumerator();
                readLine = () =>
                    (enumerator.MoveNext()) ? enumerator.Current ?? "" : null;
            }

            return TryMatchLoop(readLine);
        }

        /// <summary>
        /// The enumerator, which parses the input and returns all results (i.e. returns all collections stored in the
        /// Result class member). 
        /// If parsing fails, method stores false in Succeeded property, otherwise stores true.
        /// </summary>
        /// <param name="input">input string</param>
        /// <returns>the enumerator</returns>
        public IEnumerable<SpeedyParserResult> TryMatchLoop(string input)
        {
            var runner = CreateRunInstance(input, null, 0);
            runner.Result.Success = runner.Body(this);
            if (runner.Result.Success)
                if (runner.OutputList != null)
                    foreach (var item in runner.OutputList)
                        yield return item;
                else
                    yield return runner.Result;
        }

        /// <summary>
        /// The enumerator, which parses the input and returns all results (i.e. returns all collections stored in the
        /// Result class member). 
        /// If parsing fails, method stores false in Succeeded property, otherwise stores true.
        /// </summary>
        /// <param name="readLine">function returning subsequent line(s)</param>
        /// <returns>the enumerator</returns>
        public IEnumerable<SpeedyParserResult> TryMatchLoop(Func<string> readLine)
        {
            // https://msdn.microsoft.com/en-us/library/dd267312.aspx
            // http://www.codethinked.com/blockingcollection-and-iproducerconsumercollection

            var runner = CreateRunInstance("", readLine, 0);
            using (var bc = new System.Collections.Concurrent.BlockingCollection<SpeedyParserResult>())
            {
                runner.OutputCollection = bc;
                using (var task = System.Threading.Tasks.Task.Factory.StartNew(() =>
                {
                    runner.Result.Success = runner.Body(runner);
                    bc.CompleteAdding();
                }))
                {
                    foreach (var item in bc.GetConsumingEnumerable())
                        yield return item;
                    System.Threading.Tasks.Task.WaitAll(task);
                }
            }
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
                case ExpressionType.Assign:             // a = b
                    return (be != null) ? Expression.Assign(be.Left, CompileExpression(be.Right)) : expr;
                case ExpressionType.AddAssign:          // a += b
                    return (be != null) ? Expression.AddAssign(be.Left, CompileExpression(be.Right)) : expr;
                case ExpressionType.AddAssignChecked:   // a += b with overflow checking
                    return (be != null) ? Expression.AddAssignChecked(be.Left, CompileExpression(be.Right)) : expr;
                case ExpressionType.AndAssign:          // a &= b
                    return (be != null) ? Expression.AndAssign(be.Left, CompileExpression(be.Right)) : expr;
                case ExpressionType.DivideAssign:	    // a /= b
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

                case ExpressionType.Conditional:        // a > b ? a : b
                    ConditionalExpression ce_ex = expr as ConditionalExpression;
                    return (ce_ex != null) ? Expression.Condition(CompileExpression(ce_ex.Test), CompileExpression(ce_ex.IfTrue), CompileExpression(ce_ex.IfFalse)) : expr;
                case ExpressionType.Coalesce:           // a ?? b
                    return (be != null) ? Expression.Coalesce(CompileExpression(be.Left), CompileExpression(be.Right)) : expr;
                case ExpressionType.OrElse:	            // a || b
                    return (be != null) ? Expression.OrElse(CompileExpression(be.Left), CompileExpression(be.Right)) : expr;
                case ExpressionType.AndAlso:            // a && b
                    return (be != null) ? Expression.AndAlso(CompileExpression(be.Left), CompileExpression(be.Right)) : expr;
                case ExpressionType.Or:	                // a | b
                    return (be != null) ? Expression.Or(CompileExpression(be.Left), CompileExpression(be.Right)) : expr;
                case ExpressionType.ExclusiveOr:	    // a ^ b
                    return (be != null) ? Expression.ExclusiveOr(CompileExpression(be.Left), CompileExpression(be.Right)) : expr;
                case ExpressionType.And:                // a & b
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
                case ExpressionType.LeftShift:	        // a << b
                    return (be != null) ? Expression.LeftShift(CompileExpression(be.Left), CompileExpression(be.Right)) : expr;
                case ExpressionType.RightShift:	        // a >> b
                    return (be != null) ? Expression.RightShift(CompileExpression(be.Left), CompileExpression(be.Right)) : expr;

                case ExpressionType.Add:                // a + b
                    return (be != null) ? Expression.Add(CompileExpression(be.Left), CompileExpression(be.Right)) : expr;
                case ExpressionType.AddChecked:         // a + b with overflow checking
                    return (be != null) ? Expression.AddChecked(CompileExpression(be.Left), CompileExpression(be.Right)) : expr;
                case ExpressionType.Decrement:	        // a - 1
                    return (une != null) ? Expression.Decrement(CompileExpression(une.Operand)) : expr;
                case ExpressionType.Increment:	        // a + 1
                    return (une != null) ? Expression.Increment(CompileExpression(une.Operand)) : expr;
                case ExpressionType.Subtract:	        // a - b
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
                case ExpressionType.Negate:	            // -a
                    return (une != null) ? Expression.Negate(CompileExpression(une.Operand)) : expr;
                case ExpressionType.NegateChecked:	    // -a
                    return (une != null) ? Expression.NegateChecked(CompileExpression(une.Operand)) : expr;
                case ExpressionType.Not:	            // ~a
                    return (une != null) ? Expression.Not(CompileExpression(une.Operand)) : expr;
                case ExpressionType.OnesComplement:	    // ~a in C#
                    return (une != null) ? Expression.OnesComplement(CompileExpression(une.Operand)) : expr;
                case ExpressionType.UnaryPlus:	        // +a
                    return (une != null) ? Expression.UnaryPlus(CompileExpression(une.Operand)) : expr;
                case ExpressionType.ArrayIndex:         // a[b]
                    return (be != null) ? Expression.ArrayIndex(be.Left, CompileExpression(be.Right)) : expr;
                case ExpressionType.ArrayLength:        // a.Length
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
                    /*var const_e = expr as ConstantExpression;
                    if (const_e != null && const_e.Value != null && (const_e.Value is string))
                        Sentinels.Add(const_e.Value as string, this);*/
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
            var pars = expr.Method.GetParameters() ?? new System.Reflection.ParameterInfo[] { };
            if (expr.Object.Type == typeof(SpeedyParser))
                switch (expr.Method.Name)
                {
                    case "If":
                    case "ElseIf":
                    case "While":
                    case "IfOneOf":
                    case "ElseIfOneOf":
                    case "WhileOneOf":
                    case "IfAtLeastOneOf":
                    case "ElseIfAtLeastOneOf":
                    case "WhileAtLeastOneOf":
                    case "SpanUntilConditionWithBacktracking":
                        if ((expr.Method.Name == "If" || expr.Method.Name == "ElseIf" || expr.Method.Name == "While")
                            && pars.Length > 0 && expr.Arguments.Count == pars.Length && pars[0].Name == "condition")
                        {
                            Expression first_parameter = Expression.Lambda(CompileExpression(expr.Arguments[0]));
                            Expression[] args = ConvertArgumentsToLambdas(expr.Arguments, 1, first_parameter);
                            return Expression.Call(expr.Object, GetImplementationMethod(expr.Object.Type, expr.Method.Name + "_bool"), args);
                        }
                        if (pars.Length > 0 && expr.Arguments.Count == pars.Length && (pars[0].Name == "sentinel" || pars[0].Name == "sentinels"))
                        {
                            Expression[] s_sentinels = null;
                            string s_sentinel = TryAddSentinels(expr.Arguments[0], expr.Method.Name, 0, out s_sentinels);
                            Expression first_parameter = null;
                            if (s_sentinel != null)
                                first_parameter = Expression.Constant(s_sentinel);
                            else if (s_sentinels != null)
                                first_parameter = Expression.NewArrayInit(typeof(string), s_sentinels);
                            Expression[] args = ConvertArgumentsToLambdas(expr.Arguments, 1, first_parameter);
                            return Expression.Call(expr.Object, GetImplementationMethod(expr.Object.Type, expr.Method.Name), args);
                        }
                        break;
                    case "Else":
                    case "Do":
                    case "Throw":
                    case "ThrowIf":
                    case "ChoiceWithBacktracking":
                        if (pars.Length > 0 && expr.Arguments.Count == pars.Length)
                        {
                            Expression[] args = ConvertArgumentsToLambdas(expr.Arguments, 0, null);
                            return Expression.Call(expr.Object, GetImplementationMethod(expr.Object.Type, expr.Method.Name), args);
                        }
                        break;
                    case "Test":
                    case "TestOneOf":
                    case "TestAtLeastOneOf":
                    case "TestSomeOf":
                    case "SetSentinel":
                        if (pars.Length > 0 && expr.Arguments.Count == pars.Length && (pars[0].Name == "sentinel" || pars[0].Name == "sentinels"))
                        {
                            Expression[] s_sentinels = null;
                            TryAddSentinels(expr.Arguments[0], expr.Method.Name, 0, out s_sentinels);
                        }
                        break;
                }
            if (pars.Length > 0 && expr.Arguments.Count == pars.Length)
            {
                Expression[] args = new Expression[expr.Arguments.Count];
                bool diff = false;
                for (int i = 0; i < args.Length; i++)
                {
                    Expression expr1 = expr.Arguments[i];
                    Expression expr2 = CompileExpression(expr1);
                    if (!object.ReferenceEquals(expr1, expr2))
                        diff = true;
                    args[i] = expr2;
                }
                return diff ? Expression.Call(expr.Object, expr.Method, args) : expr;
            }
            return expr;
        }

        /// <summary>
        /// Exception thrown if compilation fails.
        /// </summary>
        public class ECompilationError : Exception
        {
            public ECompilationError(string msg)
                : base(msg)
            {
            }
        }

        protected static System.Reflection.MethodInfo GetImplementationMethod(Type objectType, string methodName)
        {
            string searchMethod = methodName + "_implementation";
            var res = objectType.GetMethod(searchMethod, System.Reflection.BindingFlags.NonPublic | System.Reflection.BindingFlags.Instance);
            if (res == null)
                throw new ECompilationError(string.Format("Method {0} not found", searchMethod));
            return res;
        }

        protected string TryAddSentinels(Expression expr, string callingFunction, int paramInx, out Expression[] out_sentinels)
        {
            object obj = Evaluate(expr, callingFunction, paramInx);
            if (obj != null)
                if (obj is string)
                {
                    string s_sentinel = NormalizeSpaces((obj as string) ?? "");
                    if (s_sentinel == "")
                        throw new ECompilationError(string.Format("Parameter {0} of function {1} should not be an empty string", paramInx + 1, callingFunction));
                    Sentinels.Add(s_sentinel, this);
                    out_sentinels = null;
                    return s_sentinel;
                }
                else if (obj is string[])
                {
                    string[] sents = obj as string[];
                    Expression[] res = new Expression[sents.Length];
                    int i = 0;
                    foreach (string i_sentinel in sents)
                    {
                        string sentinel = NormalizeSpaces(i_sentinel);
                        Sentinels.Add(sentinel, this);
                        res[i++] = Expression.Constant(sentinel);
                    }
                    out_sentinels = res;
                    return null;
                }
            throw new ECompilationError(string.Format("Parameter {0} of function {1} should not be null", paramInx + 1, callingFunction));
        }

        protected Expression[] ConvertArgumentsToLambdas(
            System.Collections.ObjectModel.ReadOnlyCollection<Expression> arguments,
            int startInx, Expression first_parameter)
        {
            Expression[] res = new Expression[arguments.Count];
            for (int i = 0; i < startInx; i++)
                res[i] = arguments[i];
            if (first_parameter != null)
                res[0] = first_parameter;
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
            // A little optimization for constant expressions
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

        /// <summary>
        /// If there is sentinel at the current input positon, the parser consumes the sentinel and evaluates
        /// body parameters. Otherwise returns true.
        /// </summary>
        /// <param name="sentinel">sentinel,</param>
        /// <param name="body">expression(s) to be evaluated if sentinel is met.</param>
        /// <returns>false if any evaluated body expression returned false, otherwise true.</returns>
        public bool If(string sentinel, params bool[] body)
        {
            return false;
        }

        protected bool If_implementation(string str, Func<bool>[] body)
        {
            SentinelsList saveSent = Sentinels.Clone();
            try
            {
                if (!(FIf_status = Test(str, consumeInput: true)))
                    return true;
                return ExecuteBody(body);
            }
            finally
            {
                Sentinels = saveSent;
            }
        }

        protected bool ExecuteBody(Func<bool>[] body)
        {
            if (body != null)
            {
                bool saveSucc = FIf_status;
                try
                {
                    FIf_status = false;
                    foreach (var f in body)
                        if (!f())
                            return false;
                }
                finally
                {
                    FIf_status = saveSucc;
                }
            }
            return true;
        }

        /// <summary>
        /// If bodies of preceding If's and ElseIf's were not executed, ElseIf works like 
        /// an ordinary If. Otherwise ElseIf immediately returns true.
        /// </summary>
        /// <param name="sentinel">sentinel,</param>
        /// <param name="body">expression(s) to be evaluated if sentinel is met.</param>
        /// <returns>false if any evaluated body expression returned false, otherwise true.</returns>
        public bool ElseIf(string sentinel, params bool[] body)
        {
            return false;
        }

        protected bool ElseIf_implementation(string str, Func<bool>[] body)
        {
            return FIf_status || If_implementation(str, body);
        }

        /// <summary>
        /// If condition is true, the parser consumes the input related with condition and evaluates body parameters. 
        /// Otherwise returns true.
        /// </summary>
        /// <param name="condition">condition,</param>
        /// <param name="body">expression(s) to be evaluated.</param>
        /// <returns>false if any evaluated body expression returned false, otherwise true.</returns>
        public bool If(bool condition, params bool[] body)
        {
            return false;
        }

        protected bool If_bool_implementation(Func<bool> condition, Func<bool>[] body)
        {
            SentinelsList saveSent = Sentinels.Clone();
            try
            {
                if (!(FIf_status = If_bool_TestCondition_Safe(condition)))
                    return true;
                return ExecuteBody(body);
            }
            finally
            {
                Sentinels = saveSent;
            }
        }

        protected bool If_bool_TestCondition_Safe(Func<bool> condition)
        {
            Input.BeginRecord();
            try
            {
                long savePos = Input.CurrentPos;
                if (!condition())
                {
                    Input.GoToPos_Unsafe(savePos);
                    return false;
                }
            }
            finally
            {
                Input.EndRecord();
            }
            return true;
        }

        /// <summary>
        /// If bodies of preceding If's and ElseIf's were not executed, ElseIf works like 
        /// an ordinary If. Otherwise ElseIf immediately returns true.
        /// </summary>
        /// <param name="condition">condition,</param>
        /// <param name="body">expression(s) to be evaluated.</param>
        /// <returns>false if any evaluated body expression returned false, otherwise true.</returns>
        public bool ElseIf(bool condition, params bool[] body)
        {
            return false;
        }

        protected bool ElseIf_bool_implementation(Func<bool> condition, Func<bool>[] body)
        {
            return FIf_status || If_bool_implementation(condition, body);
        }

        /// <summary>
        /// If there is one of sentinels at the current input positon, the parser consumes the sentinel and evaluates
        /// body parameters. Otherwise returns true.
        /// </summary>
        /// <param name="sentinels">list of sentinels,</param>
        /// <param name="body">expression(s) to be evaluated if sentinel is met.</param>
        /// <returns>false if any evaluated body expression returned false, otherwise true.</returns>
        public bool IfOneOf(string[] sentinels, params bool[] body)
        {
            return false;
        }

        protected bool IfOneOf_implementation(string[] sentinels, Func<bool>[] body)
        {
            SentinelsList saveSent = Sentinels.Clone();
            try
            {
                if (!(FIf_status = TestOneOf(sentinels, consumeInput: true)))
                    return true;
                return ExecuteBody(body);
            }
            finally
            {
                Sentinels = saveSent;
            }
        }

        /// <summary>
        /// If bodies of preceding If's and ElseIf's were not executed, ElseIf works like 
        /// an ordinary If. Otherwise ElseIf immediately returns true.
        /// </summary>
        /// <param name="sentinels">list of sentinels,</param>
        /// <param name="body">expression(s) to be evaluated if sentinel is met.</param>
        /// <returns>false if any evaluated body expression returned false, otherwise true.</returns>
        public bool ElseIfOneOf(string[] sentinels, params bool[] body)
        {
            return false;
        }

        protected bool ElseIfOneOf_implementation(string[] sentinels, Func<bool>[] body)
        {
            return FIf_status || IfOneOf_implementation(sentinels, body);
        }

        /// <summary>
        /// If there is a sequence of one or more sentinels at the current input positon, the parser consumes the sentinels 
        /// and evaluates body parameters. Otherwise returns true.
        /// </summary>
        /// <param name="sentinels">list of sentinels,</param>
        /// <param name="body">expression(s) to be evaluated if sentinels are met.</param>
        /// <returns>false if any evaluated body expression returned false, otherwise true.</returns>
        public bool IfAtLeastOneOf(string[] sentinels, params bool[] body)
        {
            return false;
        }

        protected bool IfAtLeastOneOf_implementation(string[] sentinels, Func<bool>[] body)
        {
            SentinelsList saveSent = Sentinels.Clone();
            try
            {
                if (!(FIf_status = TestAtLeastOneOf(sentinels)))
                    return true;
                return ExecuteBody(body);
            }
            finally
            {
                Sentinels = saveSent;
            }
        }

        /// <summary>
        /// If bodies of preceding If's and ElseIf's were not executed, ElseIf works like 
        /// an ordinary If. Otherwise ElseIf immediately returns true.
        /// </summary>
        /// <param name="sentinels">list of sentinels,</param>
        /// <param name="body">expression(s) to be evaluated if sentinels are met.</param>
        /// <returns>false if any evaluated body expression returned false, otherwise true.</returns>
        public bool ElseIfAtLeastOneOf(string[] sentinels, params bool[] body)
        {
            return false;
        }

        protected bool ElseIfAtLeastOneOf_implementation(string[] sentinels, Func<bool>[] body)
        {
            return FIf_status || IfAtLeastOneOf_implementation(sentinels, body);
        }

        /// <summary>
        /// If bodies of preceding If's and ElseIf's were not executed, Else executes its own body.
        /// Otherwise Else immediately returns true.
        /// </summary>
        /// <param name="body">expression(s) to be evaluated</param>
        /// <returns>false if any evaluated body expression returned false, otherwise true.</returns>
        public bool Else(params bool[] body)
        {
            return false;
        }

        protected bool Else_implementation(Func<bool>[] body)
        {
            if (!FIf_status)
            {
                bool res = Do_implementation(body);
                FIf_status = true;
                return res;
            }
            return true;
        }

        /// <summary>
        /// Zero-based loop counter for WhileXXX loops
        /// </summary>
        public long Counter
        {
            get
            {
                return FCounter;
            }
        }

        /// <summary>
        /// Number of iterations for the recent loop
        /// </summary>
        public long LastLoopCounter
        {
            get
            {
                return FLastLoopCounter;
            }
        }

        /// <summary>
        /// While there is sentinel at the current input positon, the parser consumes the sentinel and evaluates
        /// body parameters. Otherwise returns true.
        /// </summary>
        /// <param name="sentinel">sentinel,</param>
        /// <param name="body">expression(s) to be evaluated if sentinel is met.</param>
        /// <returns>false if any evaluated body expression returned false, otherwise true.</returns>
        public bool While(string sentinel, params bool[] body)
        {
            return false;
        }

        protected void StartCounter()
        {
            FCounter = FLastLoopCounter = 0;
        }

        protected long IncCounter()
        {
            return FCounter = FLastLoopCounter = (FCounter == long.MaxValue) ? 0 : FCounter + 1;
        }

        protected bool While_implementation(string str, Func<bool>[] body)
        {
            SentinelsList saveSent = Sentinels.Clone();
            long saveCounter = FCounter;
            try
            {
                for (StartCounter(); Test(str, consumeInput: true); IncCounter())
                    if (!ExecuteBody(body))
                        return false;
                return true;
            }
            finally
            {
                Sentinels = saveSent;
                FCounter = saveCounter;
            }
        }

        /// <summary>
        /// While the condition is true, the parser evaluates body parameters. Otherwise returns true.
        /// </summary>
        /// <param name="condition">condition,</param>
        /// <param name="body">expression(s) to be evaluated.</param>
        /// <returns>false if any evaluated body expression returned false, otherwise true.</returns>
        public bool While(bool condition, params bool[] body)
        {
            return false;
        }

        protected bool While_bool_implementation(Func<bool> condition, Func<bool>[] body)
        {
            SentinelsList saveSent = Sentinels.Clone();
            long saveCounter = FCounter;
            try
            {
                for (StartCounter(); If_bool_TestCondition_Safe(condition); IncCounter())
                    if (!ExecuteBody(body))
                        return false;
                return true;
            }
            finally
            {
                Sentinels = saveSent;
                FCounter = saveCounter;
            }
        }

        /// <summary>
        /// While there is one of sentinels at the current input positon, the parser consumes the sentinel and evaluates
        /// body parameters. Otherwise returns true.
        /// </summary>
        /// <param name="sentinels">list of sentinels,</param>
        /// <param name="body">expression(s) to be evaluated if sentinel is met.</param>
        /// <returns>false if any evaluated body expression returned false, otherwise true.</returns>
        public bool WhileOneOf(string[] sentinels, params bool[] body)
        {
            return false;
        }

        protected bool WhileOneOf_implementation(string[] sentinels, Func<bool>[] body)
        {
            SentinelsList saveSent = Sentinels.Clone();
            long saveCounter = FCounter;
            try
            {
                for (StartCounter(); TestOneOf(sentinels, consumeInput: true); IncCounter())
                    if (!ExecuteBody(body))
                        return false;
                return true;
            }
            finally
            {
                Sentinels = saveSent;
                FCounter = saveCounter;
            }
        }

        /// <summary>
        /// While there is a sequence of one or more sentinels at the current input positon, the parser consumes the sentinels 
        /// and evaluates body parameters. Otherwise returns true.
        /// </summary>
        /// <param name="sentinels">list of sentinels,</param>
        /// <param name="body">expression(s) to be evaluated if sentinels are met.</param>
        /// <returns>false if any evaluated body expression returned false, otherwise true.</returns>
        public bool WhileAtLeastOneOf(string[] sentinels, params bool[] body)
        {
            return false;
        }

        protected bool WhileAtLeastOneOf_implementation(string[] sentinels, Func<bool>[] body)
        {
            SentinelsList saveSent = Sentinels.Clone();
            long saveCounter = FCounter;
            try
            {
                for (StartCounter(); TestAtLeastOneOf(sentinels); IncCounter())
                    if (!ExecuteBody(body))
                        return false;
                return true;
            }
            finally
            {
                Sentinels = saveSent;
                FCounter = saveCounter;
            }
        }

        /// <summary>
        /// The method evaluates its own parameters starting from the first to the last. Returns true if all parameters
        /// return true, otherwise stops evaluation and returns false. So, this method doeas exactly the same 
        /// operations as the && operator.
        /// </summary>
        /// <param name="body">expression(s) to be evaluated</param>
        /// <returns>false if any evaluated body expression returned false, otherwise true.</returns>
        public bool Do(params bool[] body)
        {
            return false;
        }

        protected bool Do_implementation(Func<bool>[] body)
        {
            SentinelsList saveSent = Sentinels.Clone();
            try
            {
                return ExecuteBody(body);
            }
            finally
            {
                Sentinels = saveSent;
            }
        }

        /// <summary>
        /// Tries match to the input the first expression passed as a parameter, called alternative, then second one, 
        /// and so on until it finds the first correctly matching. Before every try, the input position and the Result 
        /// class member are restored to values they had at the beginning of the method. 
        /// 
        /// NOTE1: Although the parser restores input position and the value of the Result class member, unfortunatelly, 
        /// it cannot undo other side-effects occurred during evaluation of alternatives. Therefore, using Yield() 
        /// and/or other having side-effects expression in alternatives seems to be in most cases a bad idea.
        /// 
        /// NOTE2: This method is as a kind of backtracking (https://en.wikipedia.org/wiki/Backtracking) and it may 
        /// havenegative influence on the code performance. 
        /// </summary>
        /// <param name="alternatives">expression(s) aka alternative(s) to be evaluated.</param>
        /// <returns>false if all evaluated alternatives returned false, otherwise true.</returns>
        public bool ChoiceWithBacktracking(params bool[] alternatives)
        {
            return false;
        }

        protected bool ChoiceWithBacktracking_implementation(Func<bool>[] alternatives)
        {
            Input.GotoPrintChar();
            if (alternatives == null || alternatives.Length <= 0)
                return true;
            long savePos = Input.CurrentPos;
            bool saveIf_status = FIf_status;
            var saveSent = Sentinels.Clone();
            var saveRes = Result.Clone();
            Input.BeginRecord();
            try
            {
                for (int i = 0; i < alternatives.Length; i++)
                {
                    if (alternatives[i]())
                        return true;
                    Sentinels = saveSent;
                    Result = saveRes;
                    Input.GoToPos_Unsafe(savePos);
                    if (i < alternatives.Length - 1)
                    {
                        saveSent = Sentinels.Clone();
                        saveRes = Result.Clone();
                    }
                }
                return false;
            }
            catch (Exception)
            {
                Sentinels = saveSent;
                Result = saveRes;
                throw;
            }
            finally
            {
                Input.EndRecord();
                FIf_status = saveIf_status;
            }
        }

        protected bool ConditionAndWithBacktrackingImplementation(Func<bool>[] conditions)
        {
            Input.GotoPrintChar();
            if (conditions == null || conditions.Length <= 0)
                return true;
            long savePos = Input.CurrentPos;
            bool saveIf_status = FIf_status;
            var saveSent = Sentinels.Clone();
            var saveRes = Result.Clone();
            Input.BeginRecord();
            try
            {
                for (int i = 0; i < conditions.Length; i++)
                    if (!conditions[i]())
                    {
                        Sentinels = saveSent;
                        Result = saveRes;
                        Input.GoToPos_Unsafe(savePos);
                        return false;
                    }
                return true;
            }
            catch (Exception)
            {
                Sentinels = saveSent;
                Result = saveRes;
                throw;
            }
            finally
            {
                Input.EndRecord();
                FIf_status = saveIf_status;
            }
        }

        /// <summary>
        /// Throws an exception.
        /// </summary>
        /// <param name="body">exception to be thrown.</param>
        /// <returns>true.</returns>
        public bool Throw(Exception exception)
        {
            return false;
        }

        protected bool Throw_implementation(Func<Exception> exception)
        {
            throw exception();
        }

        /// <summary>
        /// If condition passed in first parameter is true, the method throws an exception passed in second parameter. 
        /// Otherwise returns true.
        /// </summary>
        /// <param name="condition">condition</param>
        /// <param name="body">exception to be thrown.</param>
        /// <returns>true.</returns>
        public bool ThrowIf(bool condition, Exception exception)
        {
            return false;
        }

        protected bool ThrowIf_implementation(Func<bool> condition, Func<Exception> exception)
        {
            if (condition())
                throw exception();
            return true;
        }

        protected static bool VarNameIsSkipable(string varName)
        {
            return varName == null || (varName = varName.Trim()) == "" || varName[0] == '_';
        }

        /// <summary>
        /// If varName starts with $ sign, sets the value of the local variable. Otherwise adds it into 
        /// Result class member.
        /// </summary>
        /// <param name="varName">variable name</param>
        /// <param name="varValue">variable value</param>
        public void SetVariable(string varName, string varValue)
        {
            if (varName != null && varName != "" && varName[0] == '$')
                (LocalVariables = LocalVariables ?? new Dictionary<string, string>())[varName] = varValue;
            else if(!VarNameIsSkipable(varName))
                Result.Add(varName, varValue);
        }

        /// <summary>
        /// If varName starts with $ sign, the method returns the value of the local variable. Otherwise returns 
        /// last value for varName in Result class member.
        /// </summary>
        /// <param name="varName">variable name</param>
        /// <returns>variable value or empty string if the value wasn't found</returns>
        public string GetVariable(string varName)
        {
            if (varName != null && varName != "" && varName[0] == '$')
                return (LocalVariables != null && LocalVariables.ContainsKey(varName))? LocalVariables[varName] : "";
            else 
                return Result.GetValue(varName, -1);
        }

        /// <summary>
        /// Consumes the input up to the first active sentinel or input's eof and stores it in variable.
        /// </summary>
        /// <param name="varName">name of the variable: if the name is empty or starts with underscore ('_'), 
        ///                     the value is not stored in Result, if the name starts with dollar sign ('$'),
        ///                     the value is stored in the local variable.</param>
        /// <returns>true.</returns>
        public bool Span(string varName)
        {
            Input.GotoPrintChar();
            bool recording = !VarNameIsSkipable(varName);
            if (recording)
                Input.BeginRecord();
            try
            {
                long startPos = Input.CurrentPos;
                while (Input.GotoPrintChar() != '\0' && !PointsAtSentinel())
                    GotoNextMatchingPos();
                if (recording)
                    SetVariable(varName, Input.GetInputSubstring_Unsafe(startPos, Input.CurrentPos).Trim());
            }
            finally
            {
                if (recording)
                    Input.EndRecord();
            }
            return true;
        }

        /// <summary>
        /// Consumes the input up to the first active sentinel or input's eof and puts it as the argument of 
        /// the function passed as parameter.
        /// </summary>
        /// <param name="consumingAction">the action which takes consumed input as a parameter.</param>
        /// <returns>the value returned by consumingAction.</returns>
        public bool Span(Func<string, bool> consumingAction)
        {
            Input.GotoPrintChar();
            bool recording = consumingAction != null;
            bool res = true;
            if (recording)
                Input.BeginRecord();
            try
            {
                long startPos = Input.CurrentPos;
                while (Input.GotoPrintChar() != '\0' && !PointsAtSentinel())
                    GotoNextMatchingPos();
                if (recording)
                    res = consumingAction(Input.GetInputSubstring_Unsafe(startPos, Input.CurrentPos).Trim());
            }
            finally
            {
                if (recording)
                    Input.EndRecord();
            }
            return res;
        }

        /// <summary>
        /// Consumes the identifier and stores it in variable. 
        /// </summary>
        /// <param name="mayStartWithDigit">if true, the method can consume indentifier starting with digit(s),
        ///                     otherwise does not.</param>
        /// <param name="varName">name of the variable: if the name is empty or starts with underscore ('_'), 
        ///                     the value is not stored in Result, if the name starts with dollar sign ('$'),
        ///                     the value is stored in the local variable.</param>
        /// <returns>true, if non-empty identifier was found.</returns>
        public bool Identifier(bool mayStartWithDigit, string varName)
        {
            char c = Input.GotoPrintChar();
            if (!IsIdentChar(c) || (!mayStartWithDigit && char.IsDigit(c)))
                return false;
            StringBuilder value = VarNameIsSkipable(varName) ? null : new StringBuilder(c);
            while (IsIdentChar(c = Input.Advance()))
                if (value != null)
                    value.Append(c);
            if (value != null)
                SetVariable(varName, value.ToString());
            return true;
        }

        /// <summary>
        /// Consumes the identifier and puts it as the argument of the function passed as parameter.
        /// </summary>
        /// <param name="mayStartWithDigit">if true, the method can consume indentifier starting with digit(s),
        ///                     otherwise does not,</param>
        /// <param name="consumingAction">the action which takes consumed input as a parameter.</param>
        /// <returns>the value returned by consumingAction.</returns>
        public bool Identifier(bool mayStartWithDigit, Func<string, bool> consumingAction)
        {
            char c = Input.GotoPrintChar();
            if (!IsIdentChar(c) || (!mayStartWithDigit && char.IsDigit(c)))
                return false;
            StringBuilder value = (consumingAction != null) ? new StringBuilder(c) : null;
            while (IsIdentChar(c = Input.Advance()))
                if (value != null)
                    value.Append(c);
            return consumingAction == null || consumingAction(value.ToString());
        }

        /// <summary>
        /// Consumes the sequence of printable (i.e. non-whitespace) characters and stores it in variable. 
        /// </summary>
        /// <param name="varName">name of the variable: if the name is empty or starts with underscore ('_'), 
        ///                     the value is not stored in Result, if the name starts with dollar sign ('$'),
        ///                     the value is stored in the local variable.</param>
        /// <returns>true, if the sequence of printable characters is non-empty.</returns>
        public bool Printable(string varName)
        {
            return PrintableNotContaining("", varName);
        }

        /// <summary>
        /// Consumes the the sequence of printable (i.e. non-whitespace) characters and puts it 
        /// as the argument of the function passed as parameter.
        /// </summary>
        /// <param name="consumingAction">the action which takes consumed input as a parameter.</param>
        /// <returns>the value returned by consumingAction.</returns>
        public bool Printable(Func<string, bool> consumingAction)
        {
            return PrintableNotContaining("", consumingAction);
        }

        /// <summary>
        /// Consumes the sequence of printable (i.e. non-whitespace) characters and stores it in variable. 
        /// The consumming is stopped if the parser meets one of characters listed in charsetNot parameter.
        /// </summary>
        /// <param name="charsetNot">list of non-consumeable characters,</param>
        /// <param name="varName">name of the variable: if the name is empty or starts with underscore ('_'), 
        ///                     the value is not stored in Result, if the name starts with dollar sign ('$'),
        ///                     the value is stored in the local variable.</param>
        /// <returns>true, if the sequence of printable characters is non-empty.</returns>
        public bool PrintableNotContaining(string charsetNot, string varName)
        {
            StringBuilder value = VarNameIsSkipable(varName) ? null : new StringBuilder();
            charsetNot = charsetNot ?? "";
            for (char c = Input.GotoPrintChar(); c != '\0' && !char.IsWhiteSpace(c) && charsetNot.IndexOf(c) < 0; c = Input.Advance())
                if (value != null)
                    value.Append(c);
            if (value == null || value.Length <= 0)
                return false;
            SetVariable(varName, value.ToString());
            return true;
        }

        /// <summary>
        /// Consumes the the sequence of printable (i.e. non-whitespace) characters and puts it as the argument 
        /// of the function passed as parameter.
        /// The consumming is stopped if the parser meets one of characters listed in charsetNot parameter.
        /// </summary>
        /// <param name="charsetNot">list of non-consumeable characters,</param>
        /// <param name="consumingAction">the action which takes consumed input as a parameter.</param>
        /// <returns>the value returned by consumingAction.</returns>
        public bool PrintableNotContaining(string charsetNot, Func<string, bool> consumingAction)
        {
            StringBuilder value = (consumingAction != null) ? new StringBuilder() : null;
            charsetNot = charsetNot ?? "";
            for (char c = Input.GotoPrintChar(); c != '\0' && !char.IsWhiteSpace(c) && charsetNot.IndexOf(c) < 0; c = Input.Advance())
                if (value != null)
                    value.Append(c);
            return consumingAction == null || consumingAction(value.ToString());
        }

        /// <summary>
        /// Consumes the sequence of characters listed in the first parameter and stores it in variable. 
        /// </summary>
        /// <param name="characterList">the list of characters to be consumed,</param>
        /// <param name="varName">name of the variable: if the name is empty or starts with underscore ('_'), 
        ///                     the value is not stored in Result, if the name starts with dollar sign ('$'),
        ///                     the value is stored in the local variable.</param>
        /// <returns>true, if the sequence of printable characters is non-empty.</returns>
        public bool Characters(string characterList, string varName)
        {
            StringBuilder value = VarNameIsSkipable(varName) ? null : new StringBuilder();
            characterList = characterList ?? "";
            for (char c = Input.GotoPrintChar(); c != '\0' && characterList.IndexOf(c) >= 0; c = Input.Advance())
            {
                if (value != null)
                    value.Append(c);
            }
            if (value == null || value.Length <= 0)
                return false;
            SetVariable(varName, value.ToString());
            return true;
        }

        /// <summary>
        /// Consumes the the sequence of characters listed in the first parameter and puts it as the argument 
        /// of the function passed as the second parameter.
        /// </summary>
        /// <param name="characterList">the list of characters to be consumed,</param>
        /// <param name="consumingAction">the action which takes consumed input as a parameter.</param>
        /// <returns>the value returned by consumingAction.</returns>
        public bool Characters(string characterList, Func<string, bool> consumingAction)
        {
            StringBuilder value = (consumingAction != null) ? new StringBuilder() : null;
            characterList = characterList ?? "";
            for (char c = Input.GotoPrintChar(); c != '\0' && characterList.IndexOf(c) >= 0; c = Input.Advance())
                if (value != null)
                    value.Append(c);
            return consumingAction == null || consumingAction(value.ToString());
        }

        /// <summary>
        /// Consumes the the sequence of decimal letters and (optionally) other characters passed in the first parameter 
        /// and stores it in variable. 
        /// </summary>
        /// <param name="extraChars">the list of characters to be consumed together with letters,</param>
        /// <param name="varName">name of the variable: if the name is empty or starts with underscore ('_'), 
        ///                     the value is not stored in Result, if the name starts with dollar sign ('$'),
        ///                     the value is stored in the local variable.</param>
        /// <returns>true, if the sequence of printable characters is non-empty.</returns>
        public bool Letters(string extraChars, string varName)
        {
            StringBuilder value = VarNameIsSkipable(varName) ? null : new StringBuilder();
            extraChars = extraChars ?? "";
            for (char c = Input.GotoPrintChar(); c != '\0' && (char.IsLetter(c) || extraChars.IndexOf(c) >= 0); c = Input.Advance())
                if (value != null)
                    value.Append(c);
            if (value == null || value.Length <= 0)
                return false;
            SetVariable(varName, value.ToString());
            return true;
        }

        /// <summary>
        /// Consumes the the sequence of decimal letters and (optionally) other characters passed in the first parameter 
        /// and puts it as the argument of the function passed as the second parameter.
        /// </summary>
        /// <param name="extraChars">the list of characters to be consumed together with letters,</param>
        /// <param name="consumingAction">the action which takes consumed input as a parameter.</param>
        /// <returns>the value returned by consumingAction.</returns>
        public bool Letters(string extraChars, Func<string, bool> consumingAction)
        {
            StringBuilder value = (consumingAction != null) ? new StringBuilder() : null;
            extraChars = extraChars ?? "";
            for (char c = Input.GotoPrintChar(); c != '\0' && (char.IsLetter(c) || extraChars.IndexOf(c) >= 0); c = Input.Advance())
                if (value != null)
                    value.Append(c);
            return consumingAction == null || consumingAction(value.ToString());
        }

        /// <summary>
        /// Consumes the input until it hits the end of the input.
        /// </summary>
        /// <param name="varName">name of the variable: if the name is empty or starts with underscore ('_'), 
        ///                     the value is not stored in Result, if the name starts with dollar sign ('$'),
        ///                     the value is stored in the local variable.</param>
        /// <returns>true.</returns>
        public bool SpanUntilEof(string varName)
        {
            StringBuilder value = VarNameIsSkipable(varName) ? null : new StringBuilder();
            for (char c = Input.GotoPrintChar(); c != '\0'; c = Input.Advance())
                if (value != null)
                    value.Append(c);
            if (value != null)
                SetVariable(varName, value.ToString().Trim());
            return true;
        }

        /// <summary>
        /// Consumes the input until it hits the end of the input.
        /// </summary>
        /// <param name="consumingAction">the action which takes consumed input as a parameter.</param>
        /// <returns>true.</returns>
        public bool SpanUntilEof(Func<string, bool> consumingAction)
        {
            StringBuilder value = (consumingAction != null) ? new StringBuilder() : null;
            for (char c = Input.GotoPrintChar(); c != '\0'; c = Input.Advance())
                if (value != null)
                    value.Append(c);
            return consumingAction == null || consumingAction(value.ToString().Trim());
        }

        /// <summary>
        /// Consumes the input until it hits the end of the line.
        /// </summary>
        /// <param name="varName">name of the variable: if the name is empty or starts with underscore ('_'), 
        ///                     the value is not stored in Result, if the name starts with dollar sign ('$'),
        ///                     the value is stored in the local variable.</param>
        /// <returns>true.</returns>
        public bool SpanUntilEoln(string varName)
        {
            StringBuilder value = VarNameIsSkipable(varName) ? null : new StringBuilder();
            for (char c = Input.GotoPrintCharInCurrentLine(); c != '\0' && c!= '\n'; c = Input.Advance())
                if (value != null)
                    value.Append(c);
            if (value != null)
                SetVariable(varName, value.ToString().Trim());
            return true;
        }

        /// <summary>
        /// Consumes the input until it hits the end of the line.
        /// </summary>
        /// <param name="consumingAction">the action which takes consumed input as a parameter.</param>
        /// <returns>true.</returns>
        public bool SpanUntilEoln(Func<string, bool> consumingAction)
        {
            StringBuilder value = (consumingAction != null) ? new StringBuilder() : null;
            for (char c = Input.GotoPrintCharInCurrentLine(); c != '\0' && c != '\n'; c = Input.Advance())
                if (value != null)
                    value.Append(c);
            return consumingAction == null || consumingAction(value.ToString().Trim());
        }

        /// <summary>
        /// Consumes the input until all conditions (passed in 2nd, 3rd, etc. parameters) become true.
        /// 
        /// When the parser evaluates conditions and some condition returns false, the parser restores 
        /// input position and the value of the Result class member ando moves input position to the next
        /// matching position (a matching position is the begin of identifier and/or any other non-identifier 
        /// printable char). 
        /// 
        /// NOTE1: Although the parser restores input position, local v and the value of the Result class member, unfortunatelly, 
        /// it cannot undo other side-effects occurred during evaluation of condition(s). Therefore, using Yield() 
        /// and/or other having side-effects conditions seems to be in most cases a bad idea.
        /// 
        /// NOTE2: This method is a kind of backtracking (https://en.wikipedia.org/wiki/Backtracking) and it may
        /// have negative influence on the code performance. 
        /// </summary>
        /// <param name="varName">name of variable, if the name is empty or starts with underscore ('_'), 
        ///                       the value is not stored in Result</param>
        /// <param name="body">expression(s) to be evaluated.</param>
        /// <returns>true</returns>
        public bool SpanUntilConditionWithBacktracking(string varName, params bool[] conditions)
        {
            return false;
        }

        protected static Dictionary<string, string> CloneDict(Dictionary<string, string> dict)
        {
            if (dict == null)
                return null;
            var res = new Dictionary<string, string>();
            foreach (var kv in dict)
                res[kv.Key] = kv.Value;
            return res;
        }

        protected bool SpanUntilConditionWithBacktracking_implementation(string varName, Func<bool>[] conditions)
        {
            Input.GotoPrintChar();
            if (conditions == null || conditions.Length <= 0)
                return true;

            bool recording = !VarNameIsSkipable(varName);
            var saveLocalVariables = (!recording)? null : CloneDict(LocalVariables);
            var saveRes = new SpeedyParserResult();
            long startPos = Input.CurrentPos;

            if (recording)
            {
                saveRes = Result.Clone();
                SetVariable(varName, Input.GetInputSubstring_Unsafe(startPos, Input.CurrentPos).Trim());
            }
            while (!ConditionAndWithBacktrackingImplementation(conditions: conditions))
            {
                if (recording)
                {
                    Result = saveRes.Clone();
                    LocalVariables = CloneDict(saveLocalVariables);
                }
                if (Input.GotoPrintChar() == '\0')
                    return false;
                GotoNextMatchingPos();
                if (recording)
                    SetVariable(varName, Input.GetInputSubstring_Unsafe(startPos, Input.CurrentPos).Trim());
            }
            return true;
        }

        /// <summary>
        /// Calls a subexpression.
        /// </summary>
        /// <param name="subExpressionName">name of the subexpression to be called,</param>
        /// <returns>the value returned by the subexpression.</returns>
        public bool Call(string subExpressionName)
        {
            bool saveIf_status = FIf_status;
            try
            {
                Func<SpeedyParser, bool> val;
                return (SubExpressions != null && SubExpressions.TryGetValue(subExpressionName, out val)) ? val(this) : false;
            }
            finally
            {
                FIf_status = saveIf_status;
            }
        }

        /// <summary>
        /// Calls a subexpression.
        /// </summary>
        /// <param name="subExpressionName">name of the subexpression to be called,</param>
        /// <param name="fold">folding function, which takes the last folding result in the first parameter (or null if
        ///         no folding occured before) and </param>
        /// <returns>the value returned by the subexpression.</returns>
        public bool Call(string subExpressionName, Func<object, object, object> fold)
        {
            object saveRes = Result.FoldResult;
            bool saveIf_status = FIf_status;
            try
            {
                Result.FoldResult = null;
                if (Call(subExpressionName) && fold != null)
                {
                    Result.FoldResult = fold(saveRes, Result.FoldResult);
                    return true;
                }
                return false;
            }
            catch (Exception)
            {
                Result.FoldResult = null;
                throw;
            }
            finally
            {
                FIf_status = saveIf_status;
            }
        }

        /// <summary>
        /// Sets sentinel state. If sentinel does not exist in sentinel's list, it is added to the list.
        /// </summary>
        /// <param name="sentinel">sentinel,</param>
        /// <param name="state">sentinel state.</param>
        /// <returns>true.</returns>
        public bool SetSentinel(string sentinel, SentinelState state)
        {
            Sentinels.SetSentinelState(sentinel, state);
            return true;
        }

        public SentinelState GetSentinelState(string sentinel)
        {
            return Sentinels.GetSentinelState(sentinel);
        }

        /// <summary>
        /// Sets all sentinels' state. 
        /// </summary>
        /// <param name="state">sentinel state</param>
        /// <returns>true</returns>
        public bool SetAllSentinels(SentinelState state)
        {
            Sentinels.SetAllSentinelsState(state);
            return true;
        }

        protected void GotoNextMatchingPos()
        {
            switch (Input.GotoPrintChar())
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
            if (!IsIdentChar(Input.CurrentChar()))
                Input.Advance();
            else
                while (IsIdentChar(Input.CurrentChar()))
                    Input.Advance();
        }

        protected void GotoClosingBracket(char closing_bracket)
        {
            int n_brackets = 1;
            char c;
            while ((c = Input.Advance()) != '\0')
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
                        if (Options.OnMissingClosingBracket != null)
                            GotoClosingBracket((c == '(') ? ')' : (c == '[') ? ']' : '}');
                        else
                            n_brackets++;
                        break;
                    case ')':
                    case ']':
                    case '}':
                        Exception exc1 = null;
                        if (c == closing_bracket)
                            return;
                        else if (Options.OnMissingClosingBracket != null && (exc1 = Options.OnMissingClosingBracket(this)) != null)
                            throw exc1;
                        else if (--n_brackets <= 0)
                            return;
                        break;
                }
            Exception exc2 = null;
            if (Options.OnMissingClosingBracket != null && (exc2 = Options.OnMissingClosingBracket(this)) != null)
                throw exc2;
        }

        protected void GotoClosingQuote(char closing_quote)
        {
            char c;
            switch ((closing_quote == '\'') ? Options.SingleQuoteSensitivity : Options.DoubleQuoteSensitivity)
            {
                case QuoteSensitivity.Insensitive:
                    return;
                case QuoteSensitivity.Sensitive:
                    while ((c = Input.Advance()) != closing_quote)
                        if (c == '\0')
                        {
                            Exception exc;
                            if (Options.OnMissingClosingQuote != null && (exc = Options.OnMissingClosingQuote(this)) != null)
                                throw exc;
                            return;
                        }
                    return;
                case QuoteSensitivity.CSharpLike:
                    while ((c = Input.Advance()) != closing_quote)
                        switch (c)
                        {
                            case '\0':
                                Exception exc;
                                if (Options.OnMissingClosingQuote != null && (exc = Options.OnMissingClosingQuote(this)) != null)
                                    throw exc;
                                return;
                            case '\\':
                                Input.Advance();
                                break;
                        }
                    return;
                case QuoteSensitivity.SqlLike:
                    while ((c = Input.Advance()) != '\0')
                        if (c == closing_quote)
                            if (Input.NextCharIs(closing_quote))
                                Input.Advance();
                            else
                                return;
                    Exception exc2;
                    if (Options.OnMissingClosingQuote != null && (exc2 = Options.OnMissingClosingQuote(this)) != null)
                        throw exc2;
                    return;
            }
        }

        protected bool PointsAtSentinel()
        {
            if (Sentinels.Sentinels == null || !Sentinels.BloomFilter.ContainsHash(Input.HashAtCurrentPos()))
                return false;
            for (int i = 0; i < Sentinels.Sentinels.Count; i++)
            {
                string sentinel = Sentinels.Sentinels[i];
                switch (Sentinels.GetSentinelState(i))
                {
                    case SentinelState.Active:
                        if (Test(sentinel, consumeInput: false))
                            return true;
                        break;
                    case SentinelState.Consumeable:
                        if (Test(SentinelsList.NormalizeSentinel(sentinel), consumeInput: true))
                            return false;
                        break;
                    case SentinelState.Disabled:
                    default:
                        break;
                }
            }
            return false;
        }

        /// <summary>
        /// Returs true if end of the input is reached.
        /// </summary>
        public bool Eof
        {
            get
            {
                return Input.GotoPrintChar() == '\0';
            }
        }

        /// <summary>
        /// Returs true if character can be used in identifier.
        /// </summary>
        /// <param name="c">character</param>
        /// <returns>true if character can be used in identifier, otherwise false.</returns>
        public virtual bool IsIdentChar(char c)
        {
            return c != '\0' && (char.IsLetterOrDigit(c) || Options.IdentCharsEx != null && Options.IdentCharsEx.IndexOf(c) >= 0);
        }

        /// <summary>
        /// Returns true if there is a sentinel at the current input's position. This method consumes the sentinel, 
        /// if sentinel is met.
        /// </summary>
        /// <param name="sentinels">list of sentinels to be tested,</param>
        /// <returns>true if one of sentinels is met, otherwise false.</returns>
        public bool TestOneOf(string[] sentinels)
        {
            return TestOneOf(sentinels, consumeInput: true);
        }

        /// <summary>
        /// Returns true if there is a sentinel at the current input's position.
        /// </summary>
        /// <param name="sentinels">list of sentinels to be tested,</param>
        /// <param name="consumeInput">if the parameter is true and there is the sentinel at the current input's position, 
        /// the parser moves current position pointer after the sentinel.</param>
        /// <returns>true if one of sentinels is met, otherwise false.</returns>
        public bool TestOneOf(string[] sentinels, bool consumeInput)
        {
            Input.GotoPrintChar();
            foreach (string sent in sentinels)
                if (Test(sent, consumeInput: consumeInput))
                    return true;
            return false;
        }

        /// <summary>
        /// Returns true if there is nonepmty sequence of sentinels (one or more) at the input 
        /// starting from current input's position.
        /// </summary>
        /// <param name="sentinels">list of sentinels to be tested,</param>
        /// <returns>true if one of sentinels is met, otherwise false.</returns>
        public bool TestAtLeastOneOf(string[] sentinels)
        {
            if (!TestOneOf(sentinels, consumeInput: true))
                return false;
            while (TestOneOf(sentinels, consumeInput: true))
                ;
            return true;
        }

        /// <summary>
        /// Returns true if there is - possibly empty - sequence of sentinels (zero or more) at the input 
        /// starting from current input's position.
        /// </summary>
        /// <param name="sentinels">list of sentinels to be tested,</param>
        /// <returns>true.</returns>
        public bool TestSomeOf(string[] sentinels)
        {
            while (TestOneOf(sentinels, consumeInput: true))
                ;
            return true;
        }

        /// <summary>
        /// Returns true if there is a sentinel at the current input's position. This method consumes the sentinel, 
        /// if sentinel is met.
        /// </summary>
        /// <param name="sentinel">sentinel to be tested,</param>
        /// <returns>true if sentinel is met, otherwise false.</returns>
        public bool Test(string sentinel)
        {
            return Test(sentinel, consumeInput: true);
        }

        /// <summary>
        /// Returns true if there is a sentinel at the current input's position.
        /// </summary>
        /// <param name="sentinel">sentinel to be tested,</param>
        /// <param name="consumeInput">if the parameter is true and there is the sentinel at the current input's position, 
        /// the parser moves current position pointer after the sentinel.</param>
        /// <returns>true if sentinel is met, otherwise false.</returns>
        public bool Test(string sentinel, bool consumeInput)
        {
            int endPos = sentinel.LastIndexOf("->");
            if (endPos < 0)
                endPos = sentinel.Length;
            Input.GotoPrintChar();
            Input.BeginRecord();
            try
            {
                long savePos = Input.CurrentPos;
                int pattPos = 0;
                while ((pattPos = PatternGotoPrintChar(sentinel, pattPos)) < endPos)
                    if (!TestSingleItem(sentinel, endPos, ref pattPos))
                    {
                        Input.GoToPos_Unsafe(savePos);
                        return false;
                    }
                if (!consumeInput)
                    Input.GoToPos_Unsafe(savePos);
            }
            finally
            {
                Input.EndRecord();
            }
            if (consumeInput && endPos + 2 < sentinel.Length)
            {
                string varName = sentinel.Substring(endPos + 2).Trim();
                if (!VarNameIsSkipable(varName))
                {
                    string varValue = sentinel.Substring(0, endPos).Trim();
                    SetVariable(varName, varValue);
                }
            }
            return true;
        }

        protected bool TestSingleItem(string pattern, int patternEnd, ref int patternPos)
        {
            Input.GotoPrintChar();
            if (IsIdentChar(pattern[patternPos]) && Input.CharBeforeCurrentPosIsIdentChar())
                return false;
            while (patternPos < patternEnd && !char.IsWhiteSpace(pattern[patternPos]))
            {
                char c = Input.CurrentChar();
                if (c != pattern[patternPos] && (Options.IsCaseSensitive || char.ToUpper(c) != char.ToUpper(pattern[patternPos])))
                    return false;
                Input.Advance();
                patternPos++;
            }
            if (IsIdentChar(pattern[patternPos - 1]) && IsIdentChar(Input.CurrentChar()))
                return false;
            return true;
        }

        protected static int PatternGotoPrintChar(string pattern, int pos)
        {
            while (pos < pattern.Length && char.IsWhiteSpace(pattern[pos]))
                pos++;
            return pos;
        }

        /// <summary>
        /// Sets the FoldResult member to value passed in the parameter. 
        /// </summary>
        /// <param name="val">value.</param>
        /// <returns>true.</returns>
        public bool SetFoldResult(object val)
        {
            Result.FoldResult = val;
            return true;
        }

        /// <summary>
        /// Trims the string and replaces whitespace sequences inside string by single spaces.
        /// </summary>
        public static string NormalizeSpaces(string str)
        {
            if ((str = (str ?? "").Trim()) == "")
                return str;
            bool needs_normalization = false;
            for (int i = 1; i < str.Length; i++)
                if (char.IsWhiteSpace(str[i]) && (str[i] != ' ' || char.IsWhiteSpace(str[i - 1])))
                {
                    needs_normalization = true;
                    break;
                }
            if (needs_normalization)
            {
                StringBuilder sb = new StringBuilder(str[0]);
                for (int i = 1; i < str.Length; i++)
                    if (!char.IsWhiteSpace(str[i]))
                        sb.Append(str[i]);
                    else if (!char.IsWhiteSpace(str[i - 1]))
                        sb.Append(' ');
                str = sb.ToString();
            }
            return str;
        }

        protected int HashChar(char c)
        {
            return (int)(Options.IsCaseSensitive ? c : char.ToUpper(c));
        }

        protected int HashString(string str, int startPos)
        {
            if (str == null || startPos >= str.Length)
                return 0;
            if (!IsIdentChar(str[startPos]))
                return (int)(Options.IsCaseSensitive ? str[startPos] : char.ToUpper(str[startPos]));
            int hash = 0;
            for (int i = 0; i < 3 && startPos + i < str.Length && IsIdentChar(str[startPos + i]); i++)
                hash = (hash << 2) ^ (int)(Options.IsCaseSensitive ? str[startPos + i] : char.ToUpper(str[startPos + i]));
            return hash;
        }

        /// <summary>
        /// Parser's input.
        /// </summary>
        public struct ParserInput
        {
            public SpeedyParser Owner;
            private string FCurrentLine;
            private long FCurrentLineStart;
            private long FCurrentPos;
            private Func<string> FReadNextLineFun;
            private bool FIsMultiline;
            private int FRecordingLevel;
            private List<BufferedLine> FBufferedLines;
            private List<CommentedChars> FCommentsList;
            private long FLastGetCharPos;
            private char FLastGetChar;
            private SimplifiedBloomFilter CommentsFilter;

            private struct BufferedLine
            {
                public string Line;
                public long StartPos;
            }

            private struct CommentedChars
            {
                public long StartPos;
                public long EndPos;
            }

            public ParserInput(SpeedyParser sp_owner, string str, Func<string> readNextLine, int pos)
            {
                Owner = sp_owner;
                FCurrentLine = str = str ?? "";
                FCurrentLineStart = 0;
                FCurrentPos = Math.Max(0, Math.Min(pos, str.Length));
                FReadNextLineFun = readNextLine;
                FIsMultiline = readNextLine != null;
                FRecordingLevel = 0;
                FBufferedLines = null;
                FCommentsList = null;
                FLastGetCharPos = -1;
                FLastGetChar = '\0';
                CommentsFilter = new SimplifiedBloomFilter(0);
                if (FReadNextLineFun != null)
                    LoadNextLine(fromConstructor: true);
                NotifyCommentsChanged();
            }

            public long CurrentPos
            {
                get
                {
                    return FCurrentPos;
                }
            }

            public ParserOptions Options
            {
                get
                {
                    return Owner.Options;
                }
            }

            public void BeginRecord()
            {
                if (FRecordingLevel == 0)
                {
                    TrimBuffer();
                    TrimComments();
                }
                FRecordingLevel++;
            }

            public void EndRecord()
            {
                if (FRecordingLevel > 0)
                    FRecordingLevel--;
            }

            public char GotoPrintChar()
            {
                if (Options.Comments == null || Options.Comments.Length <= 0)
                    return GotoPrintChar_Basic();
                char c;
                while ((c = GotoPrintChar_Basic()) != '\0')
                {
                    Tuple<string, string> comm = FindMatchingComment(c);
                    if (comm == null)
                        return c;
                    CommentedChars commItem = new CommentedChars();
                    commItem.StartPos = FCurrentPos - comm.Item1.Length;
                    if (comm.Item2 == null || comm.Item2.Length <= 0)
                        while ((c = CurrentChar()) != '\0' && c != '\n' && c != '\r')
                            FCurrentPos++;
                    else
                    {
                        bool end_of_comment_found = false;
                        for (; (c = CurrentChar()) != '\0'; FCurrentPos++)
                            if (TestCommentSubstring(comm.Item2))
                            {
                                end_of_comment_found = true;
                                break;
                            }
                        if (!end_of_comment_found)
                        {
                            Exception exc;
                            if (Options.OnMissingEndOfComment != null && (exc = Options.OnMissingEndOfComment(Owner)) != null)
                                throw exc;
                        }
                    }
                    commItem.EndPos = FCurrentPos;
                    AddComment(commItem);
                }
                return c;
            }

            /// <summary>
            /// Returns true and moves input pointer after the prefix if the input starts with a string passed as a parameter. 
            /// Otherwise returns false.
            /// </summary>
            /// <param name="prefix">the string to be tested.</param>
            /// <returns>true/false.</returns>
            public bool TestPrefix(string prefix)
            {
                GotoPrintChar();
                long savePos = CurrentPos;
                BeginRecord();
                try
                {
                    for (int i = 0; i < prefix.Length; i++, Advance())
                        if (CurrentChar() != prefix[i])
                        {
                            GoToPos_Unsafe(savePos);
                            return false;
                        }
                    return true;
                }
                finally
                {
                    EndRecord();
                }
            }

            private char GotoPrintChar_Basic()
            {
                char c;
                while ((c = CurrentChar()) != '\0' && char.IsWhiteSpace(c))
                    FCurrentPos++;
                return c;
            }

            public char GotoPrintCharInCurrentLine()
            {
                char c;
                while ((c = CurrentChar()) != '\0' && char.IsWhiteSpace(c))
                {
                    if (c == '\n')
                        return c;
                    FCurrentPos++;
                }
                return c;
            }

            public bool CharBeforeCurrentPosIsIdentChar()
            {
                return (FCurrentPos <= FCurrentLineStart) ? false : Owner.IsIdentChar(GetCharAt(FCurrentPos - 1));
            }

            public char CurrentChar() // NOTE: this should NOT BE a property because of unwanted side-effect during debugging
            {
                return GetCharAt(FCurrentPos);
            }

            public bool NextCharIs(char c)
            {
                return GetCharAt(CurrentPos + 1) == c;
            }

            public char GetCharAt(long pos)
            {
                if (pos == FLastGetCharPos)
                    return FLastGetChar;
                return FLastGetChar = GetCharAt_Basic(FLastGetCharPos = pos);
            }

            private char GetCharAt_Basic(long pos)
            {
                int inx = (int)(pos - FCurrentLineStart);
                if (inx >= 0 && inx < FCurrentLine.Length)
                    return FCurrentLine[inx];
                if (inx == FCurrentLine.Length && NeedsEolnAtLineEnd(FCurrentLine))
                    return '\n';
                if (FBufferedLines != null && FBufferedLines.Count > 0)
                {
                    char c = FindCharInBuffer(pos);
                    if (c != '\0')
                        return c;
                }
                if (pos < FCurrentLineStart)
                    return '\0';
                while (FReadNextLineFun != null && LoadNextLine(fromConstructor: false))
                {
                    inx = (int)(pos - FCurrentLineStart);
                    if (inx >= 0 && inx < FCurrentLine.Length)
                        return FCurrentLine[inx];
                    if (inx == FCurrentLine.Length && NeedsEolnAtLineEnd(FCurrentLine))
                        return '\n';
                }
                return '\0';
            }

            private char FindCharInBuffer(long pos)
            {
                int mid = FindPosInBuffer(pos);
                if (mid < 0)
                    return '\0';
                FCurrentLine = FBufferedLines[mid].Line;
                FCurrentLineStart = FBufferedLines[mid].StartPos;
                int inx = (int)(pos - FCurrentLineStart);
                return (inx < FCurrentLine.Length) ? FCurrentLine[inx] : '\n';
            }

            private int FindPosInBuffer(long pos)
            {
                int a = 0;
                int b = FBufferedLines.Count - 1;
                while (a <= b)
                {
                    int mid = (a + b) / 2;
                    int inx = (int)(pos - FBufferedLines[mid].StartPos);
                    string line = FBufferedLines[mid].Line;
                    if (inx >= 0 && inx < line.Length + (NeedsEolnAtLineEnd(line) ? 1 : 0))
                        return mid;
                    else if (pos < FBufferedLines[mid].StartPos)
                        b = mid - 1;
                    else
                        a = mid + 1;
                }
                return -1;
            }

            private bool LoadNextLine(bool fromConstructor)
            {
                string ln = FReadNextLineFun();
                if (ln != null)
                {
                    long nextStart = FCurrentLineStart + FCurrentLine.Length + (NeedsEolnAtLineEnd(FCurrentLine) && !fromConstructor ? 1 : 0);
                    if (FRecordingLevel > 0)
                    {
                        if (FBufferedLines == null)
                            FBufferedLines = new List<BufferedLine>();
                        TrimBuffer();
                        TrimComments();
                        if (FBufferedLines.Count <= 0)
                            FBufferedLines.Add(new BufferedLine
                            {
                                Line = FCurrentLine,
                                StartPos = FCurrentLineStart,
                            });
                        FBufferedLines.Add(new BufferedLine
                        {
                            Line = ln,
                            StartPos = nextStart,
                        });
                    }
                    FCurrentLine = ln;
                    FCurrentLineStart = nextStart;
                    return true;
                }
                else
                {
                    FReadNextLineFun = null;
                    return false;
                }
            }

            private void TrimBuffer()
            {
                if (FRecordingLevel <= 0 && FBufferedLines != null)
                {
                    int mid = 0;
                    while (mid < FBufferedLines.Count && FBufferedLines[mid].StartPos < FCurrentLineStart)
                        mid++;
                    if (mid > 0)
                    {
                        int n = FBufferedLines.Count - mid;
                        for (int i = 0; i < n; i++)
                            FBufferedLines[i] = FBufferedLines[i + mid];
                        FBufferedLines.RemoveRange(n, mid);
                    }
                }
            }

            private bool NeedsEolnAtLineEnd(string strLine)
            {
                if (!FIsMultiline)
                    return false;
                char c;
                return strLine.Length <= 0 || (c = strLine[strLine.Length - 1]) != '\n' && c != '\r';
            }

            public char Advance()
            {
                return GetCharAt(++FCurrentPos);
            }

            public bool GoToPos_Unsafe(long pos)
            {
                FCurrentPos = pos;
                return true;
            }

            private bool TestCommentSubstring(string str)
            {
                for (int i = 0; i < str.Length; i++)
                    if (GetCharAt(FCurrentPos + i) != str[i])
                        return false;
                FCurrentPos += str.Length;
                return true;
            }

            private Tuple<string, string> FindMatchingComment(char c)
            {
                if (!CommentsFilter.ContainsHash(Owner.HashChar(c)))
                    return null;
                foreach (var comm in Options.Comments)
                    if (comm.Item1 != null && comm.Item1.Length > 0 && TestCommentSubstring(comm.Item1))
                        return comm;
                return null;
            }

            public void NotifyCommentsChanged()
            {
                SimplifiedBloomFilter cf = new SimplifiedBloomFilter(0);
                if (Options.Comments != null)
                    foreach (var comm in Options.Comments)
                        if (comm.Item1 != null && comm.Item1.Length > 0)
                            cf = cf.AddHash(Owner.HashChar(comm.Item1[0]));
                CommentsFilter = cf;
            }

            public string GetInputSubstring_Unsafe(long startPos, long endPos)
            {
                StringBuilder res = new StringBuilder();
                for (long i = startPos; i < endPos; i++)
                {
                    char c = (FindCommentInx(i) < 0) ? GetCharAt(i) : ' ';
                    if (c == '\0')
                        break;
                    res.Append(c);
                }
                return res.ToString();
            }

            public int HashAtCurrentPos()
            {
                int i = (int)(FCurrentPos - FCurrentLineStart);
                return Owner.HashString(FCurrentLine, i);
            }

            public override string ToString() // for debugging
            {
                StringBuilder res = new StringBuilder("input: ");
                for (int i = Math.Max((int)(FCurrentPos - FCurrentLineStart), 0); i < FCurrentLine.Length; i++)
                {
                    res.Append(FCurrentLine[i]);
                    if (i >= 20)
                        return res.Append((i + 1 < FCurrentLine.Length) ? "(...)" : "").ToString();
                }
                return res.ToString();
            }

            private void AddComment(CommentedChars commItem)
            {
                if (FindCommentInx(commItem.StartPos) < 0)
                {
                    if (FCommentsList == null)
                        FCommentsList = new List<CommentedChars>();
                    FCommentsList.Add(commItem);
                }
            }

            private int FindCommentInx(long pos)
            {
                if (FCommentsList == null)
                    return -1;
                int a = 0;
                int b = FCommentsList.Count - 1;
                while (a <= b)
                {
                    int mid = (a + b) / 2;
                    if (pos >= FCommentsList[mid].StartPos && pos < FCommentsList[mid].EndPos)
                        return mid;
                    else if (pos < FCommentsList[mid].StartPos)
                        b = mid - 1;
                    else if (pos >= FCommentsList[mid].EndPos)
                        a = mid + 1;
                    else
                        break;
                }
                return -1;
            }

            private void TrimComments()
            {
                if (FCommentsList != null)
                {
                    int mid = 0;
                    while (mid < FCommentsList.Count && FCommentsList[mid].EndPos <= FCurrentLineStart)
                        mid++;
                    if (mid > 0)
                    {
                        int n = FCommentsList.Count - mid;
                        for (int i = 0; i < n; i++)
                            FCommentsList[i] = FCommentsList[i + mid];
                        FCommentsList.RemoveRange(n, mid);
                    }
                }
            }
        }

        public struct SimplifiedBloomFilter
        {
            private ulong BitArray;

            public SimplifiedBloomFilter(ulong bits)
            {
                BitArray = bits;
            }

            public bool IsEmpty()
            {
                return BitArray == 0;
            }

            public SimplifiedBloomFilter AddHash(int i)
            {
                return new SimplifiedBloomFilter(BitArray | ToBit(i));
            }

            public bool ContainsHash(int hash)
            {
                return (BitArray & ToBit(hash)) != 0;
            }

            private static ulong ToBit(int i)
            {
                return 1UL << (((i < 0) ? -i : i) % 61); // 61 is a prime number (recommended)
            }

            public SimplifiedBloomFilter UnionWith(SimplifiedBloomFilter sbf)
            {
                return new SimplifiedBloomFilter(BitArray | sbf.BitArray);
            }

            public SimplifiedBloomFilter IntersectionWith(SimplifiedBloomFilter sbf)
            {
                return new SimplifiedBloomFilter(BitArray & sbf.BitArray);
            }
        }

        public struct SentinelsList
        {
            public List<string> Sentinels;
            public BitArray Enabled;
            public BitArray Consumeable;
            public SimplifiedBloomFilter BloomFilter;

            public void Add(string sentinel, SpeedyParser p)
            {
                sentinel = NormalizeSentinel(sentinel);
                if (Sentinels == null)
                    Sentinels = new List<string>();
                for (int i = 0; i < Sentinels.Count; i++)
                {
                    if (Sentinels[i] == sentinel)
                        return;
                    if (Sentinels[i].Length < sentinel.Length)
                    {
                        Sentinels.Insert(i, sentinel);
                        BloomFilter = BloomFilter.AddHash(p.HashString(sentinel, 0));
                        return;
                    }
                }
                BloomFilter = BloomFilter.AddHash(p.HashString(sentinel, 0));
                Sentinels.Add(sentinel);
            }

            public SentinelsList Clone()
            {
                SentinelsList res = this;
                res.Enabled = Enabled.Clone();
                res.Consumeable = Consumeable.Clone();
                return res;
            }

            public void StartMatching()
            {
                if (Sentinels != null)
                    for (int i = 0; i < Sentinels.Count; i++)
                    {
                        Enabled = Enabled.SetAt(i, true);
                        Consumeable = Consumeable.SetAt(i, false);
                    }
            }

            public static string NormalizeSentinel(string sentinel)
            {
                sentinel = NormalizeSpaces(sentinel);
                int endInx = sentinel.LastIndexOf("->");
                return (endInx >= 0) ? sentinel.Substring(0, endInx).Trim() : sentinel;
            }

            private int FindSentinel(string sentinel)
            {
                if (Sentinels == null)
                    return -1;
                sentinel = NormalizeSentinel(sentinel);
                for (int i = 0; i < Sentinels.Count; i++)
                    if (Sentinels[i] == sentinel)
                        return i;
                return -1;
            }

            public SentinelState GetSentinelState(string sentinel)
            {
                return GetSentinelState(FindSentinel(sentinel));
            }

            public SentinelState GetSentinelState(int inx)
            {
                if (inx < 0)
                    return SentinelState.Disabled;
                if (Enabled.GetAt(inx))
                    return SentinelState.Active;
                else
                    return Consumeable.GetAt(inx) ? SentinelState.Consumeable : SentinelState.Disabled;
            }

            public void SetSentinelState(string sentinel, SentinelState st)
            {
                int inx = FindSentinel(sentinel);
                if (inx >= 0)
                {
                    Enabled = Enabled.SetAt(inx, st == SentinelState.Active);
                    Consumeable = Consumeable.SetAt(inx, st == SentinelState.Disabled || st == SentinelState.Consumeable);
                }
            }

            public void SetAllSentinelsState(SentinelState st)
            {
                for (int i = 0; i < Sentinels.Count; i++)
                {
                    Enabled = Enabled.SetAt(i, st == SentinelState.Active);
                    Consumeable = Consumeable.SetAt(i, st == SentinelState.Disabled || st == SentinelState.Consumeable);
                }
            }
        }

        public struct BitArray
        {
            private ulong Bits64;
            private ulong Bits128;
            private ulong[] MoreBits;

            public BitArray Clone()
            {
                var res = this;
                if (MoreBits != null)
                {
                    res.MoreBits = new ulong[MoreBits.Length];
                    for (int i = 0; i < MoreBits.Length; i++)
                        res.MoreBits[i] = MoreBits[i];
                }
                return res;
            }

            public bool GetAt(int i)
            {
                if (i < 0)
                    return false;
                else if (i < 64)
                    return (Bits64 & (1UL << i)) != 0;
                else if (i < 128)
                    return (Bits128 & (1UL << (i - 64))) != 0;
                else
                {
                    i -= 128;
                    int inx = i / 64;
                    if (MoreBits == null || inx >= MoreBits.Length)
                        return false;
                    return (MoreBits[inx] & (1UL << (i % 64))) != 0;
                }
            }

            private static ulong SetBit(ulong n, int i, bool b)
            {
                ulong flag = 1UL << i;
                return (b) ? n | flag : n & ~flag;
            }

            public BitArray SetAt(int i, bool b)
            {
                BitArray res = this;
                if (i < 0 || GetAt(i) == b)
                    return res;
                else if (i < 64)
                    res.Bits64 = SetBit(res.Bits64, i, b);
                else if (i < 128)
                    res.Bits128 = SetBit(res.Bits128, i - 64, b);
                else
                {
                    i -= 128;
                    int inx = i / 64;
                    int n1 = (MoreBits == null) ? 0 : MoreBits.Length;
                    int n = Math.Max(inx + 1, n1);
                    res.MoreBits = new ulong[n];
                    for (int ii = 0; ii < n; ii++)
                        res.MoreBits[ii] = (ii < n1) ? MoreBits[ii] : 0UL;
                    res.MoreBits[inx] = SetBit(res.MoreBits[inx], i % 64, b);
                }
                return res;
            }
        }

        /// <summary>
        /// Result of parsing
        /// </summary>
        public struct SpeedyParserResult
        {
            /// <summary>
            /// True if the match was successful, otherwise false.
            /// </summary>
            public bool Success;

            /// <summary>
            /// List of variables and values used internally by the class. 
            /// </summary>
            public Dictionary<string, List<string>> RawData;

            /// <summary>
            /// Result of folding (folding is an action executed after the calling an subexpression).
            /// </summary>
            public object FoldResult;

            public SpeedyParserResult Clone()
            {
                var rawdataCopy = (RawData == null) ? new Dictionary<string, List<string>>() : new Dictionary<string, List<string>>(RawData);
                var fr = FoldResult;
                return new SpeedyParserResult { Success = Success,  RawData = rawdataCopy, FoldResult = FoldResult };
            }

            /// <summary>
            /// Appends value to variable to the list of values for specified variable.
            /// </summary>
            /// <param name="varName">name of the variable,</param>
            /// <param name="value">value.</param>
            /// <returns>true.</returns>
            public bool Add(string varName, string value)
            {
                varName = varName ?? "";
                value = value ?? "";
                if (RawData == null)
                    RawData = new Dictionary<string, List<string>>();
                List<string> listOfValues;
                if (!RawData.TryGetValue(varName, out listOfValues))
                    RawData[varName] = listOfValues = new List<string>();
                listOfValues.Add(value);
                return true;
            }

            /// <summary>
            /// Removes all values.
            /// </summary>
            /// <returns>true.</returns>
            public bool Clear()
            {
                Success = false;
                if (RawData != null)
                    RawData.Clear();
                FoldResult = null;
                return true;
            }

            /// <summary>
            /// Returns variable value. If variable has more than one value, the first value is returned.
            /// If variable has no value or variable doesn't exist, empty string is returned.
            /// </summary>
            /// <param name="varName">variable name</param>
            /// <returns>variable value or empty string</returns>
            public string GetValue(string varName)
            {
                return GetValue(varName, 0);
            }

            /// <summary>
            /// Returns variable value at specified index. If value index is invalid or variable doesn't exist, 
            /// empty string is returned.
            /// </summary>
            /// <param name="varName">variable name</param>
            /// <param name="inx">value index (the index is Python-style, i.e. -1 is the index for the last element etc.)</param>
            /// <returns>variable value or empty string</returns>
            public string GetValue(string varName, int inx)
            {
                if (RawData == null)
                    return "";
                List<string> listOfValues;
                if (!RawData.TryGetValue(varName, out listOfValues) || listOfValues.Count <= 0)
                    return "";
                return listOfValues[((inx %= listOfValues.Count) < 0) ? inx + listOfValues.Count : inx];
            }

            /// <summary>
            /// Clears all values for specified variable name.
            /// </summary>
            /// <param name="varName">variable name.</param>
            /// <returns>true.</returns>
            public bool ClearValues(string varName)
            {
                if (RawData != null)
                {
                    List<string> listOfValues;
                    if (RawData.TryGetValue(varName, out listOfValues))
                        listOfValues.Clear();
                }
                return true;
            }

            /// <summary>
            /// Returns number of values assigned to specified variable
            /// </summary>
            /// <param name="varName">name of the variable</param>
            /// <returns>number of values</returns>
            public int ValuesCount(string varName)
            {
                if (RawData == null)
                    return 0;
                List<string> listOfValues;
                return (RawData.TryGetValue(varName, out listOfValues)) ? listOfValues.Count : 0;
            }

            /// <summary>
            /// Returns total count of all values (the sum for all variables)
            /// </summary>
            public int TotalCount
            {
                get
                {
                    if (RawData == null)
                        return 0;
                    int res = 0;
                    foreach (var pair in RawData)
                        res += pair.Value.Count;
                    return res;
                }
            }

            /// <summary>
            /// Returns list of values for the variable specified in the method's parameter formatted as the table
            /// with number of rows equal to the number of values and containing "i-th" value in each row. 
            /// Moreover, in the second column of the table the index "i" is returned.
            /// </summary>
            /// <param name="varName2">name of the variable</param>
            /// <returns>table of values</returns>
            public IEnumerable<Tuple<string, int>> SelectValues(string varName)
            {
                if (RawData != null && RawData.ContainsKey(varName))
                {
                    for (int i = 0; i < RawData[varName].Count; i++)
                        yield return new Tuple<string, int>(RawData[varName][i], i);
                }
            }

            /// <summary>
            /// Returns list of values of the variables specified in the method's parameters formatted as the table
            /// with number of rows equal to the maximal number of values for variables specified in parameters 
            /// and containing "i-th" values in each row. Moreover, in the last column of the table the index "i" 
            /// is returned.
            /// </summary>
            /// <param name="varName1">name of the first variable</param>
            /// <param name="varName2">name of the second variable</param>
            /// <returns>table of values</returns>
            public IEnumerable<Tuple<string, string, int>> SelectValues(string varName1, string varName2)
            {
                int n = Math.Max(ValuesCount(varName1), ValuesCount(varName2));
                for (int i = 0; i < n; i++)
                    yield return new Tuple<string, string, int>(GetValue(varName1, i), GetValue(varName2, i), i);
            }

            /// <summary>
            /// Returns list of values of the variables specified in the method's parameters formatted as the table
            /// with number of rows equal to the maximal number of values for variables specified in parameters 
            /// and containing "i-th" values in each row. Moreover, in the last column of the table the index "i" 
            /// is returned.
            /// </summary>
            /// <param name="varName1">name of the first variable</param>
            /// <param name="varName2">name of the second variable</param>
            /// <param name="varName3">name of the third variable</param>
            /// <returns>table of values</returns>
            public IEnumerable<Tuple<string, string, string, int>> SelectValues(string varName1, string varName2, string varName3)
            {
                int n = Math.Max(Math.Max(ValuesCount(varName1), ValuesCount(varName2)), ValuesCount(varName3));
                for (int i = 0; i < n; i++)
                    yield return new Tuple<string, string, string, int>(
                        GetValue(varName1, i), GetValue(varName2, i), GetValue(varName3, i), i);
            }

            /// <summary>
            /// Returns list of values of the variables specified in the method's parameters formatted as the table
            /// with number of rows equal to the maximal number of values for variables specified in parameters 
            /// and containing "i-th" values in each row. Moreover, in the last column of the table the index "i" 
            /// is returned.
            /// </summary>
            /// <param name="varName1">name of the first variable</param>
            /// <param name="varName2">name of the second variable</param>
            /// <param name="varName3">name of the third variable</param>
            /// <param name="varName4">name of the fourth variable</param>
            /// <returns>table of values</returns>
            public IEnumerable<Tuple<string, string, string, string, int>> SelectValues(string varName1, string varName2, 
                string varName3, string varName4)
            {
                int n = Math.Max(Math.Max(ValuesCount(varName1), ValuesCount(varName2)), 
                                 Math.Max(ValuesCount(varName3), ValuesCount(varName4)));
                for (int i = 0; i < n; i++)
                    yield return new Tuple<string, string, string, string, int>(
                        GetValue(varName1, i), GetValue(varName2, i), GetValue(varName3, i), GetValue(varName4, i), i);
            }

            /// <summary>
            /// Returns list of values of the variables specified in the method's parameters formatted as the table
            /// with number of rows equal to the maximal number of values for variables specified in parameters 
            /// and containing "i-th" values in each row. Moreover, in the last column of the table the index "i" 
            /// is returned.
            /// </summary>
            /// <param name="varName1">name of the first variable</param>
            /// <param name="varName2">name of the second variable</param>
            /// <param name="varName3">name of the third variable</param>
            /// <param name="varName4">name of the fourth variable</param>
            /// <param name="varName5">name of the fifth variable</param>
            /// <returns>table of values</returns>
            public IEnumerable<Tuple<string, string, string, string, string, int>> SelectValues(string varName1, string varName2,
                string varName3, string varName4, string varName5)
            {
                int n = Math.Max(Math.Max(Math.Max(ValuesCount(varName1), ValuesCount(varName2)),
                                 Math.Max(ValuesCount(varName3), ValuesCount(varName4))),
                                 ValuesCount(varName5));
                for (int i = 0; i < n; i++)
                    yield return new Tuple<string, string, string, string, string, int>(
                        GetValue(varName1, i), GetValue(varName2, i), GetValue(varName3, i), GetValue(varName4, i), 
                        GetValue(varName5, i), i);
            }

            /// <summary>
            /// Returns list of values of the variables specified in the method's parameters formatted as the table
            /// with number of rows equal to the maximal number of values for variables specified in parameters 
            /// and containing "i-th" values in each row. Moreover, in the last column of the table the index "i" 
            /// is returned.
            /// </summary>
            /// <param name="varName1">name of the first variable</param>
            /// <param name="varName2">name of the second variable</param>
            /// <param name="varName3">name of the third variable</param>
            /// <param name="varName4">name of the fourth variable</param>
            /// <param name="varName5">name of the fifth variable</param>
            /// <param name="varName6">name of the sixth variable</param>
            /// <returns>table of values</returns>
            public IEnumerable<Tuple<string, string, string, string, string, string, int>> SelectValues(string varName1, string varName2,
                string varName3, string varName4, string varName5, string varName6)
            {
                int n = Math.Max(Math.Max(Math.Max(ValuesCount(varName1), ValuesCount(varName2)),
                                 Math.Max(ValuesCount(varName3), ValuesCount(varName4))),
                                 Math.Max(ValuesCount(varName5), ValuesCount(varName6)));
                for (int i = 0; i < n; i++)
                    yield return new Tuple<string, string, string, string, string, string, int>(
                        GetValue(varName1, i), GetValue(varName2, i), GetValue(varName3, i), GetValue(varName4, i),
                        GetValue(varName5, i), GetValue(varName6, i), i);
            }

            /// <summary>
            /// Returns list of values of the variables specified in the method's parameters formatted as the table
            /// with number of rows equal to the maximal number of values for variables specified in parameters 
            /// and containing "i-th" values in each row. Moreover, in the last column of the table the index "i" 
            /// is returned.
            /// </summary>
            /// <param name="varName1">name of the first variable</param>
            /// <param name="varName2">name of the second variable</param>
            /// <param name="varName3">name of the third variable</param>
            /// <param name="varName4">name of the fourth variable</param>
            /// <param name="varName5">name of the fifth variable</param>
            /// <param name="varName6">name of the sixth variable</param>
            /// <param name="varName7">name of the seventh variable</param>
            /// <returns>table of values</returns>
            public IEnumerable<Tuple<string, string, string, string, string, string, string, int>> SelectValues(string varName1, string varName2,
                string varName3, string varName4, string varName5, string varName6, string varName7)
            {
                int n = Math.Max(Math.Max(Math.Max(ValuesCount(varName1), ValuesCount(varName2)),
                                          Math.Max(ValuesCount(varName3), ValuesCount(varName4))),
                                 Math.Max(Math.Max(ValuesCount(varName5), ValuesCount(varName6)), 
                                          ValuesCount(varName7)));
                for (int i = 0; i < n; i++)
                    yield return new Tuple<string, string, string, string, string, string, string, int>(
                        GetValue(varName1, i), GetValue(varName2, i), GetValue(varName3, i), GetValue(varName4, i),
                        GetValue(varName5, i), GetValue(varName6, i), GetValue(varName7, i), i);
            }

        }
    }
}
