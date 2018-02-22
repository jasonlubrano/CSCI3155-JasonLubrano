package jsy.student

import jsy.lab3.Lab3Like
import jsy.util.JsyApplication

object Lab3 extends JsyApplication with Lab3Like {
  import jsy.lab3.ast._

  /*
   * CSCI 3155: Lab 3 
   * Jason Lubrano
   *
   * Partner: Abiel Fattore
   * Collaborators: Vy
   */

  /*
   * Fill in the appropriate portions above by replacing things delimited
   * by '<'... '>'.
   *
   * Replace the '???' expression with your code in each function.
   *
   * Do not make other modifications to this template, such as
   * - adding "extends App" or "extends Application" to your Lab object,
   * - adding a "main" method, and
   * - leaving any failing asserts.
   *
   * Your lab will not be graded if it does not compile.
   *
   * This template compiles without error. Before you submit comment out any
   * code that does not compile or causes a failing assert. Simply put in a
   * '???' as needed to get something  that compiles without error. The '???'
   * is a Scala expression that throws the exception scala.NotImplementedError.
   */

  /*
   * The implementations of these helper functions for conversions can come
   * Lab 2. The definitions for the new value type for Function are given.
   */

  def toNumber(v: Expr): Double = {
    require(isValue(v))
    (v: @unchecked) match {
      case N(n) => n
      case B(b) => if(b)  1 else 0
      case Undefined => Double.NaN
      case S(s) => s match {
        case "undefined" => Double.NaN
        case "" => 0
        case str => try str.toDouble catch {case e: java.lang.NumberFormatException => Double.NaN}
      }
      case Function(_, _, _) => Double.NaN
      case _ => throw new UnsupportedOperationException
    }
  }

  def toBoolean(v: Expr): Boolean = {
    require(isValue(v))
    (v: @unchecked) match {
      case B(b) => b
      case N(n) => n match {
        case 0.0 => false
        case -0.0 => false //n.isNaN, 0.0, -0.0
        //case => false //<- want to make n.isNaN
        case _ => true
      }
      case S(s) => if(s == "") false else true
      case Undefined => false
      case Function(_, _, _) => true
      case _ => throw new UnsupportedOperationException
    }
  }

  def toStr(v: Expr): String = {
    require(isValue(v))
    (v: @unchecked) match {
      case S(s) => s
      case N(n) => if(n.isWhole()) n.toInt.toString else n.toString
      case B(b) => if(b) "true" else "false"
      case Undefined => "undefined"
      // Here in toStr(Function(_, _, _)), we will deviate from Node.js that returns the concrete syntax
      // of the function (from the input program).
      case Function(_, _, _) => "function"
      case _ => throw new UnsupportedOperationException // delete this line when done
    }
  }

  /*
   * Helper function that implements the semantics of inequality
   * operators Lt, Le, Gt, and Ge on values.
   *
   * We suggest a refactoring of code from Lab 2 to be able to
   * use this helper function in eval and step.
   */
  def inequalityVal(bop: Bop, v1: Expr, v2: Expr): Boolean = {
    require(isValue(v1))
    require(isValue(v2))
    require(bop == Lt || bop == Le || bop == Gt || bop == Ge)
    (v1, v2) match { // strings, numbers, no?
      case (S(str1), S(str2)) =>
        (bop: @unchecked) match {
          case Lt => str1 < str2
          case Gt => str1 > str2
          case Le => str1 <= str2
          case Ge => str1 >= str2
          case _ => throw new UnsupportedOperationException
        }
      case _ =>
        val (num1, num2) = (toNumber(v1), toNumber(v2))
        (bop: @unchecked) match {
          case Lt => num1 < num2
          case Gt => num1 > num2
          case Le => num1 <= num2
          case Ge => num1 >= num2
          case _ => throw new UnsupportedOperationException
        }
    }
  }


  /* Big-Step Interpreter with Dynamic Scoping */

  /*
   * Start by copying your code from Lab 2 here.
   */
  def eval(env: Env, e: Expr): Expr = {
    e match {
      /* Base Cases */
      case N(_) | B(_) | S(_) | Undefined | Function(_, _, _) => e
      case Print(e1) => println(pretty(eval(env, e1))); Undefined
      case Unary(uop, e) => uop match {
        case Neg => N(-toNumber(eval(env, e)))
        case Not => B(!toBoolean(eval(env, e)))
      }
      /* end Unary */
      /* begin binary */
      case Binary(bop, e1, e2) => bop match {
        case Plus => (eval(env, e1),eval(env, e2)) match {
          case (S(s), e2) => S(s + toStr(eval(env, e2)))
          case (e1, S(s)) => S(toStr(eval(env, e1)) + s)
          case (e1, e2) => N(toNumber(eval(env, e1)) + toNumber(eval(env, e2)))
          case (e1, undefined) => N(Double.NaN)
          case (undefined, e2) => N(Double.NaN)
          case _ => throw new UnsupportedOperationException
        }

        case Minus => (eval(env, e1), eval(env, e2)) match {
          case (e1, e2) => N(toNumber(eval(env, e1)) - toNumber(eval(env, e2)))
          case _ => throw new UnsupportedOperationException
        }

        case Times => (eval(env, e1), eval(env, e2)) match {
          case (e1, e2) => N(toNumber(eval(env, e1)) * toNumber(eval(env, e2)))
          case _ => throw new UnsupportedOperationException
        }

        case Div => (eval(env, e1), eval(env, e2)) match {
          case (e1, e2) => N(toNumber(eval(env, e1)) / toNumber(eval(env, e2)))
          case _ => throw new UnsupportedOperationException
        }

        /* === type checking */
        case Eq => {
          val exp1 = eval(env, e1)
          exp1 match {
            case N(n) => if(n == toNumber(eval(env, e2))) B(true) else B(false)
            case B(b) => if(b == toBoolean(eval(env, e2))) B(true) else B(false)
            case S(s) => if(s == toStr(eval(env, e2))) B(true) else B(false)
            case _ => throw new UnsupportedOperationException
          }
        }
        case Ne => {
          val exp1 = eval(env, e1)
          exp1 match {
            case N(n) => if(n != toNumber(eval(env, e2))) B(true) else B(false)
            case B(b) => if(b != toBoolean(eval(env, e2))) B(true) else B(false)
            case S(s) => if(s != toStr(eval(env, e2))) B(true) else B(false)
            case _ => throw new UnsupportedOperationException
          }
        }

        /* comps */
        case ( Lt | Le | Gt | Ge ) => B(inequalityVal(bop, eval(env, e1), eval(env, e2)))

        /* and and ors */
        case And => {
          val v1 = eval(env, e1)
          val v2 = eval(env, e2)
          if(toBoolean(v1)) v2 else v1
        }
        case Or => {
          val v1 = eval(env,e1)
          val v2 = eval(env,e2)
          if(toBoolean(v1)) v1 else v2
        }

        case Seq => {
          eval(env, e1)
          eval(env, e2)
        }
      }
      /* end binary */

      case If(e1,e2,e3) => {
        val v1 = eval(env, e1)
        val v2 = eval(env, e2)
        val v3 = eval(env, e3)
        val b1 = toBoolean(v1)
        if (b1) v2 else v3
      }

      case Var(x) => try {
        lookup(env, x)
      } catch {
        case e: java.util.NoSuchElementException => {
          val variable = x
          eval(env, Print(S("ReferenceError: $variable not defined")))
        }
      }

      /* Inductive Cases */
      case Print(e1) => println(pretty(eval(env, e1))); Undefined

      // ****** Your cases here

      case Call(e1, e2) =>
        val v1 = eval(env, e1)
        val v2 = eval(env, e2)
        v1 match {
          case Function(None, varName, body) => {
            val newEnv_a = extend(env, varName, v2)
            eval(newEnv_a, body) //-> v'
          }
          case Function(Some(x), varName, body) => {
            val newEnv_a = extend(env, x, v1)
            val newEnv_b = extend(newEnv_a, varName, v2)
            eval(newEnv_b, body) // -> v'
          }
        }
      case _ => throw new DynamicTypeError(e)
    }
  }


  /* Small-Step Interpreter with Static Scoping */

  def iterate(e0: Expr)(next: (Expr, Int) => Option[Expr]): Expr = {
    def loop(e: Expr, n: Int): Expr = next(e,n) match {
      case Some(ep) => loop(ep,n+1)
      case None => e
    }
    loop(e0, 0)
  }

  def substitute(e: Expr, v: Expr, x: String): Expr = {
    require(isValue(v))
    e match {
      case N(_) | B(_) | Undefined | S(_) => e
      case Print(e1) => Print(substitute(e1, v, x))
      case Unary(uop, e1) => Unary(uop, substitute(e1, v, x))
      case Binary(bop, e1, e2) => Binary(bop, substitute(e1, v, x), substitute(e2, v, x))
      case If(e1, e2, e3) => If(substitute(e1, v, x), substitute(e2, v, x), substitute(e3, v, x))
      case Call(e1, e2) => Call(substitute(e1, v, x), substitute(e2, v, x))
      case Var(y) => if (x == y) v else Var(y)
      case Function(None, name_var, body) => if(x == name_var) Function(None, name_var, body) else Function(None, name_var, substitute(body, v, x))
      case Function(Some(foo), name_var, body) =>{
        if((x == foo ) || (x == name_var)) Function(Some(foo), name_var, body)
        else Function(Some(foo), name_var, substitute(body, v, x))
      }
      case ConstDecl(y, e1, e2) => if (x == y) ConstDecl(y,substitute(e1,v,x), e2) else ConstDecl(y,substitute(e1,v,x), substitute(e2,v,x))
      case _ => throw new UnsupportedOperationException
    }
  }

  def step(e: Expr): Expr = {
    e match {
      /* Base Cases: Do Rules */
      case Print(v1) if isValue(v1) => println(pretty(v1)); Undefined
      // ****** Your cases here

      /* Inductive Cases: Search Rules */
      /* UOPS */
      case Unary(Neg, e1) => e1 match {
        case N(n) => N(-n)
        case _ => Unary(Neg, step(e1))
      }
      case Unary(Not, e1) => e1 match {
        case B(n) => B(!n)
        case _ => Unary(Not, step(e1))
      }
      /* BOPS */
        /* Arithmatic */
      case Binary(Plus, e1, e2) => if (isValue(e1) && isValue(e2)) (e1, e2) match {
        case (S(_), _) => S(toStr(e1) + toStr(e2))
        case (_, S(_)) => S(toStr(e1) + toStr(e2))
        case (_, _) => N(toNumber(e1) + toNumber(e2))
      } else if(!isValue(e1)) {
        Binary(Plus, step(e1), e2)
      } else {
        Binary(Plus, e1, step(e2))
      }

      case Binary(Minus, e1, e2) => if (isValue(e1) && isValue(e2)) {
        N(toNumber(e1) - toNumber(e2))
      } else if (!isValue(e1)) {
        Binary(Minus, step(e1), e2)
      } else {
        Binary(Minus, e1, step(e2))
      }

      case Binary(Times, e1, e2) => if (isValue(e1) && isValue(e2)) {
        N(toNumber(e1) * toNumber(e2))
      } else if (!isValue(e1)) {
        Binary(Times, step(e1), e2)
      } else {
        Binary(Times, e1, step(e2))
      }

      case Binary(Div, e1, e2) => if (isValue(e1) && isValue(e2)) {
        N(toNumber(e1) / toNumber(e2))
      } else if (!isValue(e1)) {
        Binary(Div, step(e1), e2)
      } else {
        Binary(Div, e1, step(e2))
      }
        /* conditionals */
      case Binary(Eq, e1, e2) => ???
      case Binary(Ne, e1, e2) => ???
      case Binary(Lt, e1, e2) => ???
      case Binary(Gt, e1, e2) => ???
      case Binary(Le, e1, e2) => ???
      case Binary(Ge, e1, e2) => ???
        /* logic */
      case Binary(And, e1, e2) => ???
      case Binary(Or, e1, e2) => ???
        /* seq */
      case Binary(Seq, e1, e2) => ???
        /* if */
      case If(e1, e2, e3) => ???
        /*const decl*/
      case ConstDecl(x, e1, e2) => ???
        /* call */
      case Call(e1, e2) => ???
      case Call(v1, v2) => ???
      case Call(e1, v2) => ???
      case Call(v1, e2) => ???
      case Call(e1@Function(_,_,_), e2) => ???


      /* Cases that should never match. Your cases above should ensure this. */
      case Var(_) => throw new AssertionError("Gremlins: internal error, not closed expression.")
      case N(_) | B(_) | Undefined | S(_) | Function(_, _, _) => throw new AssertionError("Gremlins: internal error, step should not be called on values.");
    }
  }


  /* External Interfaces */

  //this.debug = true // uncomment this if you want to print debugging information
  this.keepGoing = true // comment this out if you want to stop at first exception when processing a file

}
