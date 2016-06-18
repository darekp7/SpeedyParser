﻿using System;
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
        private List<string> Sentinels;

        public SpeedyParser(Expression<Func<SpeedyParser, bool>> parserBody)
        {
        }

        public static void Test()
        {
            Expression<Func<SpeedyParser, bool>> parserBody = (p) =>
                p.If("select", 
                    p.If("from", 
                        p.While("join", 
                            p.If("on")),
                        p.If("where")));

            /*Expression<Func<SpeedyParser, bool>> parserBody2 = (p) =>
                p.If("select _columns",
                    p.If("from table",
                        p.While("join", (str) => p.If(str),
                            p.If("on _")),
                        p.If("where _")));*/

            var k = parserBody.Body.NodeType;

            int i = 0;

            Expression<Func<int, int>> x = n => n + i;
            SpeedyParser spp = new SpeedyParser(null);
            Expression<Func<bool>> bf = () => spp.If("");
            var a = bf.Body;
            if (a is MethodCallExpression)
            {
                var a1 = a as MethodCallExpression;
                if (a1.Method.Name == "If" && a1.Object.Type == typeof(SpeedyParser))
                {
                    System.Windows.Forms.MessageBox.Show(a1.Method.Name);
                    i++;
                }
            }
            i++;
        }

        private Expression CompileExpression(Expression expr, bool sentinelExpected)
        {
            // https://msdn.microsoft.com/pl-pl/library/bb352032(v=vs.110).aspx
            // good examples:
            // http://stackoverflow.com/questions/3716492/what-does-expression-quote-do-that-expression-constant-can-t-already-do

            BinaryExpression be = expr as BinaryExpression;
            UnaryExpression une = expr as UnaryExpression;
            switch (expr.NodeType)
            {
                case ExpressionType.Assign:         // a = b
                    return (be != null) ? Expression.Assign(be.Left, CompileExpression(be.Right, sentinelExpected)) : expr;
                case ExpressionType.AddAssign:      // a += b
                    return (be != null) ? Expression.AddAssign(be.Left, CompileExpression(be.Right, sentinelExpected)) : expr;
                case ExpressionType.AddAssignChecked: // a += b with overflow checking
                    return (be != null) ? Expression.AddAssignChecked(be.Left, CompileExpression(be.Right, sentinelExpected)) : expr;
                case ExpressionType.AndAssign:      // a &= b
                    return (be != null) ? Expression.AndAssign(be.Left, CompileExpression(be.Right, sentinelExpected)) : expr;
                case ExpressionType.DivideAssign:	// a /= b
                    return (be != null) ? Expression.DivideAssign(be.Left, CompileExpression(be.Right, sentinelExpected)) : expr;
                case ExpressionType.ExclusiveOrAssign:  // a ^= b
                    return (be != null) ? Expression.ExclusiveOrAssign(be.Left, CompileExpression(be.Right, sentinelExpected)) : expr;
                case ExpressionType.LeftShiftAssign:	// a <<= b
                    return (be != null) ? Expression.LeftShiftAssign(be.Left, CompileExpression(be.Right, sentinelExpected)) : expr;
                case ExpressionType.ModuloAssign:	    // a %= b
                    return (be != null) ? Expression.ModuloAssign(be.Left, CompileExpression(be.Right, sentinelExpected)) : expr;
                case ExpressionType.MultiplyAssign:	    // a *= b
                    return (be != null) ? Expression.MultiplyAssign(be.Left, CompileExpression(be.Right, sentinelExpected)) : expr;
                case ExpressionType.MultiplyAssignChecked:  // a *= b
                    return (be != null) ? Expression.MultiplyAssignChecked(be.Left, CompileExpression(be.Right, sentinelExpected)) : expr;
                case ExpressionType.OrAssign:	        // a |= b
                    return (be != null) ? Expression.OrAssign(be.Left, CompileExpression(be.Right, sentinelExpected)) : expr;
                case ExpressionType.PowerAssign:	    // A compound assignment operation that raises a number to a power, such as (a ^= b) in Visual Basic.
                    return (be != null) ? Expression.PowerAssign(be.Left, CompileExpression(be.Right, sentinelExpected)) : expr;
                case ExpressionType.RightShiftAssign:   // a >>= b
                    return (be != null) ? Expression.RightShiftAssign(be.Left, CompileExpression(be.Right, sentinelExpected)) : expr;
                case ExpressionType.SubtractAssign:	    // a -= b
                    return (be != null) ? Expression.SubtractAssign(be.Left, CompileExpression(be.Right, sentinelExpected)) : expr;
                case ExpressionType.SubtractAssignChecked:  // a -= b
                    return (be != null) ? Expression.SubtractAssignChecked(be.Left, CompileExpression(be.Right, sentinelExpected)) : expr;

                case ExpressionType.Conditional:    // a > b ? a : b
                    ConditionalExpression ce_ex = expr as ConditionalExpression;
                    return (ce_ex != null) ? Expression.Condition(CompileExpression(ce_ex.Test, sentinelExpected), CompileExpression(ce_ex.IfTrue, sentinelExpected), CompileExpression(ce_ex.IfFalse, sentinelExpected)) : expr;
                case ExpressionType.Coalesce:       // a ?? b
                    return (be != null) ? Expression.Coalesce(CompileExpression(be.Left, sentinelExpected), CompileExpression(be.Right, sentinelExpected)) : expr;
                case ExpressionType.OrElse:	        // a || b
                    return (be != null) ? Expression.OrElse(CompileExpression(be.Left, sentinelExpected), CompileExpression(be.Right, sentinelExpected)) : expr;
                case ExpressionType.AndAlso:        // a && b
                    return (be != null) ? Expression.AndAlso(CompileExpression(be.Left, sentinelExpected), CompileExpression(be.Right, sentinelExpected)) : expr;
                case ExpressionType.Or:	            // a | b
                    return (be != null) ? Expression.Or(CompileExpression(be.Left, sentinelExpected), CompileExpression(be.Right, sentinelExpected)) : expr;
                case ExpressionType.ExclusiveOr:	// a ^ b
                    return (be != null) ? Expression.ExclusiveOr(CompileExpression(be.Left, sentinelExpected), CompileExpression(be.Right, sentinelExpected)) : expr;
                case ExpressionType.And:            // a & b
                    return (be != null) ? Expression.And(CompileExpression(be.Left, sentinelExpected), CompileExpression(be.Right, sentinelExpected)) : expr;

                case ExpressionType.Equal:	            // a == b
                    return (be != null) ? Expression.Equal(CompileExpression(be.Left, sentinelExpected), CompileExpression(be.Right, sentinelExpected)) : expr;
                case ExpressionType.NotEqual:	        // a != b
                    return (be != null) ? Expression.NotEqual(CompileExpression(be.Left, sentinelExpected), CompileExpression(be.Right, sentinelExpected)) : expr;
                case ExpressionType.GreaterThan:	    // a > b
                    return (be != null) ? Expression.GreaterThan(CompileExpression(be.Left, sentinelExpected), CompileExpression(be.Right, sentinelExpected)) : expr;
                case ExpressionType.GreaterThanOrEqual:	// a >= b
                    return (be != null) ? Expression.GreaterThanOrEqual(CompileExpression(be.Left, sentinelExpected), CompileExpression(be.Right, sentinelExpected)) : expr;
                case ExpressionType.LessThan:	        // a < b
                    return (be != null) ? Expression.LessThan(CompileExpression(be.Left, sentinelExpected), CompileExpression(be.Right, sentinelExpected)) : expr;
                case ExpressionType.LessThanOrEqual:	// a <= b
                    return (be != null) ? Expression.LessThanOrEqual(CompileExpression(be.Left, sentinelExpected), CompileExpression(be.Right, sentinelExpected)) : expr;
                case ExpressionType.TypeAs:	            // obj as SampleType
                case ExpressionType.TypeIs:	            // obj is SampleType
                    return expr;
                case ExpressionType.LeftShift:	  // a << b
                    return (be != null) ? Expression.LeftShift(CompileExpression(be.Left, sentinelExpected), CompileExpression(be.Right, sentinelExpected)) : expr;
                case ExpressionType.RightShift:	  // a >> b
                    return (be != null) ? Expression.RightShift(CompileExpression(be.Left, sentinelExpected), CompileExpression(be.Right, sentinelExpected)) : expr;

                case ExpressionType.Add:            // a + b
                    return (be != null) ? Expression.Add(CompileExpression(be.Left, sentinelExpected), CompileExpression(be.Right, sentinelExpected)) : expr;
                case ExpressionType.AddChecked:     // a + b with overflow checking
                    return (be != null) ? Expression.AddChecked(CompileExpression(be.Left, sentinelExpected), CompileExpression(be.Right, sentinelExpected)) : expr;
                case ExpressionType.Decrement:	    // a - 1
                    return (une != null) ? Expression.Decrement(CompileExpression(une.Operand, sentinelExpected)) : expr;
                case ExpressionType.Increment:	    // a + 1
                    return (une != null) ? Expression.Increment(CompileExpression(une.Operand, sentinelExpected)) : expr;
                case ExpressionType.Subtract:	    // a - b
                    return (be != null) ? Expression.Subtract(CompileExpression(be.Left, sentinelExpected), CompileExpression(be.Right, sentinelExpected)) : expr;
                case ExpressionType.SubtractChecked:    // a - b
                    return (be != null) ? Expression.SubtractChecked(CompileExpression(be.Left, sentinelExpected), CompileExpression(be.Right, sentinelExpected)) : expr;

                case ExpressionType.Multiply:	        // a * b
                    return (be != null) ? Expression.Multiply(CompileExpression(be.Left, sentinelExpected), CompileExpression(be.Right, sentinelExpected)) : expr;
                case ExpressionType.MultiplyChecked:    // a * b
                    return (be != null) ? Expression.MultiplyChecked(CompileExpression(be.Left, sentinelExpected), CompileExpression(be.Right, sentinelExpected)) : expr;
                case ExpressionType.Divide:	            // a / b
                    return (be != null) ? Expression.Divide(CompileExpression(be.Left, sentinelExpected), CompileExpression(be.Right, sentinelExpected)) : expr;
                case ExpressionType.Modulo:	            // a % b
                    return (be != null) ? Expression.Modulo(CompileExpression(be.Left, sentinelExpected), CompileExpression(be.Right, sentinelExpected)) : expr;
                case ExpressionType.Power:	  // raises a number to a power, such as (a ^ b) in Visual Basic.
                    return (be != null) ? Expression.Power(CompileExpression(be.Left, sentinelExpected), CompileExpression(be.Right, sentinelExpected)) : expr;

                case ExpressionType.PreDecrementAssign:     // --a
                    return (une != null) ? Expression.PreDecrementAssign(CompileExpression(une.Operand, sentinelExpected)) : expr;
                case ExpressionType.PreIncrementAssign:     // ++a
                    return (une != null) ? Expression.PreIncrementAssign(CompileExpression(une.Operand, sentinelExpected)) : expr;
                case ExpressionType.PostDecrementAssign:    // a--
                    return (une != null) ? Expression.PostDecrementAssign(CompileExpression(une.Operand, sentinelExpected)) : expr;
                case ExpressionType.PostIncrementAssign:	// a++
                    return (une != null) ? Expression.PostIncrementAssign(CompileExpression(une.Operand, sentinelExpected)) : expr;
                case ExpressionType.Negate:	        // -a
                    return (une != null) ? Expression.Negate(CompileExpression(une.Operand, sentinelExpected)) : expr;
                case ExpressionType.NegateChecked:	// -a
                    return (une != null) ? Expression.NegateChecked(CompileExpression(une.Operand, sentinelExpected)) : expr;
                case ExpressionType.Not:	        // ~a
                    return (une != null) ? Expression.Not(CompileExpression(une.Operand, sentinelExpected)) : expr;
                case ExpressionType.OnesComplement:	// ~a in C#
                    return (une != null) ? Expression.OnesComplement(CompileExpression(une.Operand, sentinelExpected)) : expr;
                case ExpressionType.UnaryPlus:	    // +a
                    return (une != null) ? Expression.UnaryPlus(CompileExpression(une.Operand, sentinelExpected)) : expr;
                case ExpressionType.ArrayIndex:     // a[b]
                    return (be != null) ? Expression.ArrayIndex(be.Left, CompileExpression(be.Right, sentinelExpected)) : expr;
                case ExpressionType.ArrayLength:    // a.Length
                    return (une != null) ? Expression.ArrayLength(CompileExpression(une.Operand, sentinelExpected)) : expr;
                
                case ExpressionType.Index:	        // An index operation or an operation that accesses a property that takes arguments.
                case ExpressionType.Block:          // block expression
                    return expr;
                case ExpressionType.Convert:        // (SampleType)obj
                case ExpressionType.ConvertChecked: // (SampleType)obj
                case ExpressionType.DebugInfo:	    // Debugging information.
                    return expr;
                case ExpressionType.Call:       // obj.sampleMethod()
                    var call_e = expr as MethodCallExpression;
                    return (call_e != null)? CompileCall(call_e) : expr;
                case ExpressionType.Constant:   // a constant value
                    var const_e = expr as ConstantExpression;
                    if (const_e != null && const_e.Value != null && (const_e.Value is string))
                        Sentinels.Add(const_e.Value as string);
                    return expr;
                case ExpressionType.Default:	// A default value.
                case ExpressionType.IsFalse:    // A false condition value.
                    return (une != null) ? Expression.IsFalse(CompileExpression(une.Operand, sentinelExpected)) : expr;
                case ExpressionType.IsTrue:	    // A true condition value.
                    return (une != null) ? Expression.IsTrue(CompileExpression(une.Operand, sentinelExpected)) : expr;
                case ExpressionType.Lambda:	    // a => a + a
                    return expr;
                case ExpressionType.Quote:	    // An expression that has a constant value of type Expression. A Quote node can contain references to parameters that are defined in the context of the expression it represents.
                    return (une != null) ? Expression.Quote(CompileExpression(une.Operand, sentinelExpected)) : expr;
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

        private Expression CompileCall(MethodCallExpression expr)
        {
            if (expr.Object.Type == typeof(SpeedyParser))
                switch (expr.Method.Name)
                {
                    case "If":
                        break;
                }
            var method = expr.Method;
            //method.
            foreach (var p in method.GetParameters())
            {
            }
            return expr;
        }

        public bool If(string str, params bool[] body)
        {
            return true;
        }

        public bool While(string str, params bool[] body)
        {
            return true;
        }

        public bool Switch(bool firstInstruction, params bool[] moreInstructions)
        {
            return true;
        }

        public bool SwitchLoop(bool firstInstruction, params bool[] moreInstructions)
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
