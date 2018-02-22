package jsy.student

import jsy.lab3.Lab3Like
import jsy.util.JsyApplication

object Lab3 extends JsyApplication with Lab3Like {
  import jsy.lab3.ast._
  
  /*
   * CSCI 3155: Lab 3 
   * Abiel Fattore
   * 
   * Partner: Jason Lubrano
   * Collaborators: <Any Collaborators>
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
      case B(false) => 0.0
      case B(true) => 1.0
      case Undefined => Double.NaN
      case S(s) => try s.toDouble catch { case _: Throwable => Double.NaN }
      case Function(_, _, _) => Double.NaN
    }
  }

  def toBoolean(v: Expr): Boolean = {
    require(isValue(v))
    (v: @unchecked) match {
      case B(b) => b
      case N(n) => if(n==0) false else true
      case S(s) => if(s=="") false else true
      case Undefined => false
      case _ => throw new UnsupportedOperationException
    }
  }

  def toStr(v: Expr): String = {
    require(isValue(v))
    (v: @unchecked) match {
      case S(s) => s
      case N(n) => n.toString
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
    (v1, v2) match {
      case _ => ??? // delete this line when done
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
      case Unary(uop, e) => uop match {
        case Neg => N(-toNumber(eval(env,e)))
        case Not => B(!toBoolean(eval(env,e)))
      }
      case Binary(bop,e1,e2) => bop match {
        case Plus => (eval(env,e1),eval(env,e2)) match {
          case (S(s), v2) => S(s + toStr(v2))
          case (v1, S(s)) => S(toStr(v1) + s)
          case (v1, v2) => N(toNumber(v1) + toNumber(v2))
        }
        case Minus => (eval(env,e1),eval(env,e2)) match {
          case (v1, v2) => N(toNumber(v1) - toNumber(v2))
        }
        case Times => (eval(env,e1),eval(env,e2)) match {
          case (v1, v2) => N(toNumber(v1) * toNumber(v2))
        }
        case Div => (eval(env,e1),eval(env,e2)) match {
          case (v1, v2) => N(toNumber(v1) / toNumber(v2))
        }
        case And => {
          val v1 = eval(env,e1)
          val v2 = eval(env,e2)

          val b1 = toBoolean(v1)

          if(b1 == true) v2 else v1
        }
        case Or => {
          val v1 = eval(env,e1)
          val v2 = eval(env,e2)

          val b2 = toBoolean(v2)

          if(b2 == true) v2 else v1
        }
        case Eq => {
          val v1 = eval(env, e1)
          val v2 = eval(env,e2)
          (v1,v2) match {
            case (Function(_,_,_),_) => throw DynamicTypeError(e) //which expression should I throw
            case (_,Function(_,_,_)) => throw DynamicTypeError(e)
            case _ => if(v1 == v2) B(true) else B(false) //we don't want to evaluate all the way because it might not be comparing 2 doubles but two strings
          }
        }
        case Ne => {
          val v1 = eval(env, e1)
          val v2 = eval(env, e2)
          (v1, v2) match {
            case (Function(_, _, _), _) => throw DynamicTypeError(e) //which expression should I throw
            case (_, Function(_, _, _)) => throw DynamicTypeError(e)
            case _ => if (v1 == v2) B(false) else B(true
            )
          }
        }
        case Lt => {
          val v1 = eval(env, e1)
          val v2 = eval(env,e2)

          val b1 = toNumber(v1)
          val b2 = toNumber(v2)

          if(b1 < b2) B(true) else B(false)
        }
        case Le => {
          val v1 = eval(env, e1)
          val v2 = eval(env,e2)

          val b1 = toNumber(v1)
          val b2 = toNumber(v2)

          if(b1 < b2) B(true) else if(b1 > b2) B(false) else B(true)
        }
        case Gt => {
          val v1 = eval(env,e1)
          val v2 = eval(env,e2)
          (v1,v2) match {
            case (S(s1),S(s2)) => B(s1 > s2)
            case _ => {
              val b1 = toNumber(v1)
              val b2 = toNumber(v2)
              if(b1 > b2) B(true) else B(false)
            }
          }

        }
        case Ge => {
          val v1 = eval(env, e1)
          val v2 = eval(env,e2)

          val b1 = toNumber(v1)
          val b2 = toNumber(v2)

          if(b1 > b2) B(true) else if(b1 < b2) B(false) else B(true)
        }
        case Seq => {
          eval(env, e1)
          eval(env,e2)
        }
      }
      case If(e1,e2,e3) => {
        val v1 = eval(env,e1)
        val b1 = toBoolean(v1)
        if(b1 == true) eval(env,e2) else eval(env,e3)
      }
      case Var(s) => try {
        lookup(env,s)
      } catch {
        case e: java.util.NoSuchElementException => {
          val variable = s
          eval(env, Print(S("ReferenceError: $variable not defined")))
        }
      }
      case ConstDecl(s,e1,e2) => {
        val v1 = eval(env, e1)
        val newEnv = extend(env, s, v1)
        eval(newEnv, e2)
      }

      /* Inductive Cases */
      case Print(e1) => println(pretty(eval(env, e1))); Undefined
      
      /* Inductive Cases */
      case Print(e1) => println(pretty(eval(env, e1))); Undefined

        // ****** Your cases here

      case Call(e1, e2) =>
        val v1 = eval(env,e1)
        val v2 = eval(env, e2)
        v1 match {
        case Function(None,p,body) => {
          val newEnv = extend(env, p, v2)
          eval(newEnv, body)
        }
        case Function(Some(x),p,body) => {
          val newEnv = extend(env,x,v1)
          val newEnv1 = extend(newEnv,p,v2)
          eval(newEnv1, body)

        }
      }
      case _ => throw new UnsupportedOperationException
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
      case Unary(uop, e1) => ???
      case Binary(bop, e1, e2) => ???
      case If(e1, e2, e3) => ???
      case Call(e1, e2) => ???
      case Var(y) => ???
      case Function(None, y, e1) => ???
      case Function(Some(y1), y2, e1) => ???
      case ConstDecl(y, e1, e2) => ???
    }
  }
    
  def step(e: Expr): Expr = {
    e match {
      /* Base Cases: Do Rules */
      case Unary(Neg, e1) if isValue(e1) => N(-toNumber(e1))
      case Print(v1) if isValue(v1) => println(pretty(v1)); Undefined
      
        // ****** Your cases here
      
      /* Inductive Cases: Search Rules */
      case Print(e1) => Print(step(e1))
      
        // ****** Your cases here

      /* Cases that should never match. Your cases above should ensure this. */
      case Var(_) => throw new AssertionError("Gremlins: internal error, not closed expression.")
      case N(_) | B(_) | Undefined | S(_) | Function(_, _, _) => throw new AssertionError("Gremlins: internal error, step should not be called on values.");
    }
  }


  /* External Interfaces */
  
  //this.debug = true // uncomment this if you want to print debugging information
  this.keepGoing = true // comment this out if you want to stop at first exception when processing a file

}