package jsy.student

import jsy.lab2.Lab2Like

object Lab2 extends jsy.util.JsyApplication with Lab2Like {
  import jsy.lab2.Parser
	import jsy.lab2.ast._

  /*
   * CSCI 3155: Lab 2
   * Jason Lubrano
   * 
   * Partner: Lucas Sward
   * Collaborators: Abiel Fattore
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
   * - Double.NaN <- undefined
   * - s.toDouble (for s: String)
   * - n.isNaN (for n: Double)
   * - n.isWhole (for n: Double)
   * - s (for n: Double)
   * - s format n (for s: String [a format string like for printf], n: Double)
   *
   * You can catch an exception in Scala using:
   * try ... catch { case ... => ... }
   */
  //Number function
  def toNumber(v: Expr): Double = {
    require(isValue(v))
    (v: @unchecked) match {
      // Number
      case N(n) => n
      // Boolean
      case B(b) => if(b) return 1 else 0
      // string
      case S(s) => s match {
        case "undefined" => Double.NaN
        case "Undefined" => Double.NaN
        case "" => Double.NaN
        case str => try str.toDouble catch {case e : java.lang.NumberFormatException => Double.NaN}
      }
      // undefined
      case undefined => Double.NaN
      // if not those types
      case _ => throw new UnsupportedOperationException
    }
  }
  //Boolean function
  def toBoolean(v: Expr): Boolean = {
    require(isValue(v))
    (v: @unchecked) match {
      // Boolean
      case B(b) => b
      // Number
      case N(0) => false
      case N(_) => true
      // String
      case S("") => false
      case S(_) => true
      // Undefined
      case undefined => false
      case _ => throw new UnsupportedOperationException
    }
  }
  //String
  def toStr(v: Expr): String = {
    require(isValue(v))
    (v: @unchecked) match {
      case S(s) => s
      case Undefined => "undefined"
      case N(n) => n.toString
      case B(b) => b.toString
    	//case _ => Throw new UnsupportedOperationException
    }
  }

  def eval(env: Env, e: Expr): Expr = {
    
    e match {
      /* Base Cases */
      case Var(x) => try {
      	lookup(env, x)
      } catch {
      	case e: java.util.NoSuchElementException => {
      		val variable = x
      		eval(env, Print(S("ReferenceError: $variable not defined")))
      	}
      }
      
      case N(_)|B(_)|S(_)|Undefined => e
      
      /* Inductive Cases */
      case ConstDecl(x, e1, e2) => {
        val e1_new = eval(env, e1) //find e1 with the same environment
        val env_new = extend(env, x, e1_new) //update the environmnt with x = 5 constantly
        eval(env_new, e2) // evaluate the next expression with the updated environment
      }
      
      /* UOPS */
      case Unary(uop, e1) => uop match {
        case Neg => eval(N(-toNumber(eval(e1))))
        case Not => eval(B(!toBoolean(eval(e1))))
      }
      /* BOPS */
      case Binary(bop, e1, e2) => bop match {
        case Plus => (eval(env, e1), eval(env, e2)) match {
        	case (S(str), e2) => S(str + toStr(e2))
        	case (e1, S(str)) => S(toStr(e2) + str)
        	case (e1, e2) => N(toNumber(eval(e1)) + toNumber(eval(e2)))
        }
        case Minus => (eval(env, e1), eval(env, e2)) match {
        	case (e1, e2) => N(toNumber(eval(e1)) - toNumber(eval(e2)))
        }
        case Times => (eval(env, e1), eval(env, e2)) match {
        	case (e1, e2) => N(toNumber(e1) * toNumber(e2))
        }
        case Div => (eval(env, e1), eval(env, e2)) match {
        	case (e1, e2) => N(toNumber(e1) / toNumber(e2))
        }
        /* === type checking */
        case Eq => {
          val exp1 = eval(env, e1)
          val exp2 = eval(env, e2)
          if(exp1 == exp2) B(true) else B(false)
        }
        case Ne => {
          val exp1 = eval(env, e1)
          val exp2 = eval(env, e2)
          if(exp1 == exp2) B(false) else B(true)
        }
        /* comps */
        case Lt => eval(env, B(toNumber(eval(env, e1)) < toNumber(eval(env, e2))))
        case Le => eval(env, B(toNumber(eval(env, e1)) <= toNumber(eval(env, e2))))
        case Ge => eval(env, B(toNumber(eval(env, e1)) >= toNumber(eval(env, e2))))
        case Gt => eval(env, B(toNumber(eval(env, e1)) > toNumber(eval(env, e2))))
        case And => {
        	val v1 = eval(env, e1)
        	val v2 = eval(env, e2)
        	if (toBoolean(v1) == true) return v2 else v1
        }
        case Or => {
					val v1 = eval(env, e1)
					if (toBoolean(v1) == true) return e1 else eval(env, e2)
				} 
        case Seq => eval(env, e1);eval(env, e2)
      }
   		case If(e1, e2, e3) => {
        val bool1 = eval(env, e1)
        if(toBoolean(bool1) == true) eval(env, e2) else eval(env, e3)
			}

    	case Print(e1) => println(pretty(eval(env, e1))); Undefined

    	case _ => throw new UnsupportedOperationException
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