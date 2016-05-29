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
        public bool IsCaseSensitive;
        public bool IsBracketSensitive;
        private bool FDoubleQuoteIsCpp;
        private bool FDoubleQuoteIsSql;
        private bool FSingleQuoteIsCpp;
        private bool FSingleQuoteIsSql;

        private Func<SpeedyParser, bool> Body = null;
        private Dictionary<string, List<string>> OutTable;
        private string[] Sentinels;

        public SpeedyParser(Expression<Func<SpeedyParser, bool>> parserBody)
        {
        }

        public static void Test()
        {
            Expression<Func<SpeedyParser, bool>> parserBody = (p) =>
                p.If("select", "_columns",
                    p.If("from", "table",
                        p.While("join", "table",
                            p.If("on", "_")),
                        p.If("where", "_")));

            /*Expression<Func<SpeedyParser, bool>> parserBody2 = (p) =>
                p.If("select _columns",
                    p.If("from table",
                        p.While("join", (str) => p.If(str),
                            p.If("on _")),
                        p.If("where _")));*/

            var k = parserBody.Body.NodeType;
            int i = 0;
        }

        private static Expression CompileExpression(Expression expr)
        {
            switch (expr.NodeType)
            {
                case ExpressionType.Assign:         // a = b
                case ExpressionType.AddAssign:      // a += b
                case ExpressionType.AddAssignChecked: // a += b with overflow checking
                case ExpressionType.AndAssign:      // a &= b
                case ExpressionType.DivideAssign:	// a /= b
                case ExpressionType.ExclusiveOrAssign:  // a ^= b
                case ExpressionType.LeftShiftAssign:	// a <<= b
                case ExpressionType.ModuloAssign:	    // a %= b
                case ExpressionType.MultiplyAssign:	    // a *= b
                case ExpressionType.MultiplyAssignChecked:  // a *= b
                case ExpressionType.OrAssign:	        // a |= b
                case ExpressionType.PowerAssign:	    // A compound assignment operation that raises a number to a power, such as (a ^= b) in Visual Basic.
                case ExpressionType.RightShiftAssign:   // a >>= b
                case ExpressionType.SubtractAssign:	    // a -= b
                case ExpressionType.SubtractAssignChecked:  // a -= b
                    return null;
                case ExpressionType.Conditional:    // a > b ? a : b
                case ExpressionType.Coalesce:       // a ?? b
                case ExpressionType.OrElse:	        // a || b
                case ExpressionType.AndAlso:        // a && b
                    return null;
                case ExpressionType.Or:	            // a | b
                case ExpressionType.ExclusiveOr:	// a ^ b
                case ExpressionType.And:            // a & b
                    return null;
                case ExpressionType.Equal:	            // a == b
                case ExpressionType.NotEqual:	        // a != b
                case ExpressionType.GreaterThan:	    // a > b
                case ExpressionType.GreaterThanOrEqual:	// a >= b
                case ExpressionType.LessThan:	        // a < b
                case ExpressionType.LessThanOrEqual:	// a <= b
                case ExpressionType.TypeAs:	            // obj as SampleType
                case ExpressionType.TypeIs:	            // obj is SampleType
                    return null;
                case ExpressionType.LeftShift:	  // a << b
                case ExpressionType.RightShift:	  // a >> b
                    return null;
                case ExpressionType.Add:            // a + b
                case ExpressionType.AddChecked:     // a + b with overflow checking
                case ExpressionType.Decrement:	    // a - 1
                case ExpressionType.Increment:	    // a + 1
                case ExpressionType.Subtract:	    // a - b
                case ExpressionType.SubtractChecked:    // a - b
                    return null;
                case ExpressionType.Multiply:	        // a * b
                case ExpressionType.MultiplyChecked:    // a * b
                case ExpressionType.Divide:	            // a / b
                case ExpressionType.Modulo:	            // a % b
                case ExpressionType.Power:	  // raises a number to a power, such as (a ^ b) in Visual Basic.
                    return null;
                case ExpressionType.PreDecrementAssign:     // --a
                case ExpressionType.PreIncrementAssign:     // ++a
                case ExpressionType.PostDecrementAssign:    // a--
                case ExpressionType.PostIncrementAssign:	// a++
                    return null;
                case ExpressionType.Negate:	        // -a
                case ExpressionType.NegateChecked:	// -a
                case ExpressionType.Not:	        // ~a
                case ExpressionType.OnesComplement:	// ~a in C#.
                case ExpressionType.UnaryPlus:	    // +a
                    return null;
                case ExpressionType.ArrayIndex:     // a[b]
                case ExpressionType.ArrayLength:    // a.Length
                case ExpressionType.Index:	        // An index operation or an operation that accesses a property that takes arguments.
                case ExpressionType.Block:          // block expression
                    return null;
                case ExpressionType.Convert:        // (SampleType)obj
                case ExpressionType.ConvertChecked: // (SampleType)obj
                case ExpressionType.DebugInfo:	    // Debugging information.
                    return null;
                case ExpressionType.Call:       // obj.sampleMethod()
                case ExpressionType.Constant:   // a constant value
                    return null;
                case ExpressionType.Default:	// A default value.
                case ExpressionType.IsFalse:    // A false condition value.
                case ExpressionType.IsTrue:	    // A true condition value.
                case ExpressionType.Lambda:	    // a => a + a
                    return null;
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
                case ExpressionType.Quote:	    // An expression that has a constant value of type Expression. A Quote node can contain references to parameters that are defined in the context of the expression it represents.
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

        public bool If(string str, string strVar, params bool[] body)
        {
            return true;
        }

        public bool While(string str, string strVar, params bool[] body)
        {
            return true;
        }

        public bool If(string str, Func<string, bool> setVar, params bool[] body)
        {
            return true;
        }

        public bool While(string str, Func<string, bool> setVar, params bool[] body)
        {
            return true;
        }

        public bool Eof
        {
            get
            {
                int TODO = 1;
                return true;
            }
        }

        public bool Test(string str)
        {
            int TODO = 1;
            return true;
        }

        public void Emit(string strVar)
        {
            int TODO = 1;
        }
    }
}
