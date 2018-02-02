package jsy.student

import jsy.lab2.{Lab2Like, ast}

object Lab2 extends jsy.util.JsyApplication with Lab2Like {
  import jsy.lab2.Parser
  import jsy.lab2.ast._

  /*
   * CSCI 3155: Lab 2
   * <Lucas Sward>
   * 
   * Partner: <Your Partner's Name>
   * Collaborators: <Any Collaborators>
   */

  /*
   * Fill in the appropriate portions above by replacing things delimited
   * by '<'... '>'.
   * 
   * Replace the '???' expression with  your code in each function.
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
   *
   */

  /* We represent a variable environment as a map from a string of the
   * variable name to the value to which it is bound.
   * 
   * You may use the following provided helper functions to manipulate
   * environments, which are just thin wrappers around the Map type
   * in the Scala standard library.  You can use the Scala standard
   * library directly, but these are the only interfaces that you
   * need.
   */



  /* Some useful Scala methods for working with Scala values include:
   * - Double.NaN
   * - s.toDouble (for s: String)
   * - n.isNaN (for n: Double)
   * - n.isWhole (for n: Double)
   * - s (for n: Double)
   * - s format n (for s: String [a format string like for printf], n: Double)
   *
   * You can catch an exception in Scala using:
   * try ... catch { case ... => ... }
   */

  def toNumber(v: Expr): Double = {
    require(isValue(v))
    (v: @unchecked) match {
      case N(n) => n
      case B(false) => 0
      case B(true) => 1
      case S("") => 0
      case S("Undefined") => Double.NaN
      case S("undefined") => Double.NaN
      case S(str) => {try str.toDouble catch {case _: Throwable => Double.NaN}}
      case _ => ???
    }
  }

  def toBoolean(v: Expr): Boolean = {
    require(isValue(v))
    (v: @unchecked) match {
      case Undefined => false
      case B(b) => b
      case N(0) => false
      case N(_) => true
      case S("") => false
      case S(_) => true
      case _ => ???
    }
  }

  def toStr(v: Expr): String = {
    require(isValue(v))
    (v: @unchecked) match {
      case S(s) => s
      case Undefined => "undefined"
      case N(n) => n.toString
      case B(b) => b.toString
      case _ => ???
    }
  }

  def eval(env: Env, e: Expr): Expr = {
    e match {
      /* Base Cases */
      case Var(x) => lookup(env, x)
      case N(_)|S(_)|B(_)|Undefined => e

      /* Inductive Cases */
      case ConstDecl(x, e1, e2) => {
        val e1p = eval(env, e1);
        val envp = extend(env, x, e1p);
        eval(envp, e2)
      }
      case Binary(And, e1, e2 )=> {
        val v1 = eval(env, e1)
        if (toBoolean(v1) == true) return eval(env, e2) else eval(env, e1)
      }
      case Binary(Or, e1, e2) => {
        val v1 = eval(e1)
        if (toBoolean(v1) == true) return e1 else eval(e2)
      }

      case Binary(Eq, e1, e2) => eval(B(toNumber(eval(e1)) == toNumber(eval(e2))))


      case Binary(Plus, e1, e2) => ???

      case Binary(Ge, e1, e2) => eval(B(toNumber(eval(e1)) >= toNumber(eval(e2))))

      case Binary(Gt, e1, e2) => eval(B(toNumber(eval(e1)) > toNumber(eval(e2))))

      case Binary(Le, e1, e2) => eval(B(toNumber(eval(e1)) <= toNumber(eval(e2))))


      case Binary(Minus, e1, e2) => eval(N(toNumber(eval(e1))-toNumber(eval(e2))))

      case Binary(Times, e1, e2) => eval(N(toNumber(eval(e1)) * toNumber(eval(e2))))

      case Binary(Div, e1, e2) => eval(N(toNumber(eval(e1)) / toNumber(eval(e2))))

      case Binary(Ne, e1, e2) => eval(B(toNumber(eval(e1)) != toNumber(eval(e2))))

      case Binary(Lt, e1, e2) => eval(B(toNumber(eval(e1)) < toNumber(eval(e2))))

      case Unary(Neg, e1) => eval(N(-toNumber(eval(e1))))

      case Unary(Not, e1) => eval(B(!toBoolean(eval(e1))))

      case If(e1, e2, e3) => {
        val b1 = eval(e1)
        if(toBoolean(b1) == true) eval(e2) else eval(e3)
      }

      case Binary(Seq, e1, e2) => eval(e1);eval(e2)


      case Print(e1) => println(pretty(eval(env, e1))); Undefined

      case _ => ???
    }
  }



  /* Interface to run your interpreter from the command-line.  You can ignore what's below. */
  def processFile(file: java.io.File) {
    if (debug) { println("Parsing ...") }

    val expr = Parser.parseFile(file)

    if (debug) {
      println("\nExpression AST:\n  " + expr)
      println("------------------------------------------------------------")
    }

    if (debug) { println("Evaluating ...") }

    val v = eval(expr)

     println(pretty(v))
  }

}
