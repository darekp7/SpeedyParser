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

            Expression<Func<int, int>> x = n => n + i;
            i++;
        }

        private static Expression CompileExpression(Expression expr)
        {
            // https://msdn.microsoft.com/pl-pl/library/bb352032(v=vs.110).aspx
            // good examples:
            // http://stackoverflow.com/questions/3716492/what-does-expression-quote-do-that-expression-constant-can-t-already-do

            BinaryExpression be = expr as BinaryExpression;
            UnaryExpression une = expr as UnaryExpression;
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
                    return expr;

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
                case ExpressionType.Increment:	    // a + 1
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
                case ExpressionType.PreIncrementAssign:     // ++a
                case ExpressionType.PostDecrementAssign:    // a--
                case ExpressionType.PostIncrementAssign:	// a++
                    return null;
                case ExpressionType.Negate:	        // -a
                    return (une != null) ? Expression.Negate(une.Operand) : expr;
                case ExpressionType.NegateChecked:	// -a
                    return (une != null) ? Expression.NegateChecked(une.Operand) : expr;
                case ExpressionType.Not:	        // ~a
                    return (une != null) ? Expression.Not(une.Operand) : expr;
                case ExpressionType.OnesComplement:	// ~a in C#
                    return (une != null) ? Expression.OnesComplement(une.Operand) : expr;
                case ExpressionType.UnaryPlus:	    // +a
                    return (une != null) ? Expression.UnaryPlus(une.Operand) : expr;
                case ExpressionType.ArrayIndex:     // a[b]
                    return (be != null) ? Expression.ArrayIndex(be.Left, CompileExpression(be.Right)) : expr;
                case ExpressionType.ArrayLength:    // a.Length
                    return (une != null) ? Expression.ArrayLength(une.Operand) : expr;
                
                case ExpressionType.Index:	        // An index operation or an operation that accesses a property that takes arguments.
                case ExpressionType.Block:          // block expression
                    return expr;
                case ExpressionType.Convert:        // (SampleType)obj
                case ExpressionType.ConvertChecked: // (SampleType)obj
                case ExpressionType.DebugInfo:	    // Debugging information.
                    return expr;
                case ExpressionType.Call:       // obj.sampleMethod()
                case ExpressionType.Constant:   // a constant value
                    return null;
                case ExpressionType.Default:	// A default value.
                case ExpressionType.IsFalse:    // A false condition value.
                case ExpressionType.IsTrue:	    // A true condition value.
                case ExpressionType.Lambda:	    // a => a + a
                    return expr;
                case ExpressionType.Quote:	    // An expression that has a constant value of type Expression. A Quote node can contain references to parameters that are defined in the context of the expression it represents.
                    return (une != null) ? Expression.Quote(une.Operand) : expr;
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
