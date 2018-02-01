package jsy.student

import javax.swing.text.DefaultStyledDocument.AttributeUndoableEdit

import jsy.lab1
import jsy.lab1._

object Lab1 extends jsy.util.JsyApplication with jsy.lab1.Lab1Like {
  import jsy.lab1.Parser
  import jsy.lab1.ast._

  /*
   * CSCI 3155: Lab 1
   * Jason Lubrano
   *
   * Partner: Brad
   * Collaborators: <Any Collaborators>
   */

  /*
   * Fill in the appropriate portions above by replacing things delimited
   * by '<'... '>'.
   *
   * Replace the '???' expression with your code in each function. The
   * '???' expression is a Scala expression that throws a NotImplementedError
   * exception.
   *
   * Do not make other modifications to this template, such as
   * - adding "extends App" or "extends Application" to your Lab object,
   * - adding a "main" method, and
   * - leaving any failing asserts.
   *
   * Your lab will not be graded if it does not compile.
   *
   * This template compiles without error. Before you submit comment out any
   * code that does not compile or causes a failing assert.  Simply put in a
   * '???' as needed to get something that compiles without error.
   */

  /*
   * Example: Test-driven development of plus
   *
   * A convenient, quick-and-dirty way to experiment, especially with small code
   * fragments, is to use the interactive Scala interpreter. The simplest way
   * to use the interactive Scala interpreter in IntelliJ is through a worksheet,
   * such as Lab1Worksheet.sc. A Scala Worksheet (e.g., Lab1Worksheet.sc) is code
   * evaluated in the context of the project with results for each expression
   * shown inline.
   *
   * Step 0: Sketch an implementation in Lab1.scala using ??? for unimmplemented things.
   * Step 1: Do some experimentation in Lab1Worksheet.sc.
   * Step 2: Write a test in Lab1Spec.scala, which should initially fail because of the ???.
   * Step 3: Fill in the ??? to finish the implementation to make your test pass.
   */

  //def plus(x: Int, y: Int): Int = ???
  def plus(x: Int, y: Int): Int = x + y

  /* Exercises */

  def abs(n: Double): Double = if(n < 0) (-n) else n

  def xor(a: Boolean, b: Boolean): Boolean = {
    if (a == true) {
      if (b == true) {
        return false
      } else {
        return true
      }
    } else {
      if (b == true) {
        return true
      } else {
        return false
      }
    }
  }

  def repeat(s: String, n: Int): String = {
    require(n >= 0)
    if(n > 0) return s + repeat(s, (n-1))
    else return ""
  }

  def sqrtStep(c: Double, xn: Double): Double = {
    require(c >= 0)
    require(xn >= 0)
    return (xn - ((xn*xn - c)/(xn+xn)))
    //plug n chug
  }

  def sqrtN(c: Double, x0: Double, n: Int): Double = {
    require(n >= 0)
    require(x0 >= 0)
    if(n == 0) (return x0) else (return sqrtN(c, sqrtStep(c, x0), n-1))
    //scala is ugly
    //we will need to check and see if n is eq to 0. if it is then we just return our guess
    //else were going to recursively call sqrtN with n-1, and a more educated guess
    //reference newtons method paper i wrote
  }

  def sqrtErr(c: Double, x0: Double, epsilon: Double): Double = {
    require(epsilon > 0) //infinite loop whoops lol, check for c >= 0 in sqrt step
    if(abs(x0*x0 - c) < epsilon)(return x0) else (return sqrtErr(c, sqrtStep(c, x0), epsilon))
    //if less than eps, return x0. Else were going to recursively call sqrtStep w/ guess so it within error bounds
    //to be completely honest i have no idea how to use a for loop in this language
    //so i just beat out the recursive loop haha
  }
  def sqrt(c: Double): Double = {
    require(c >= 0)
    if (c == 0) 0 else sqrtErr(c, 1.0, 0.0001)
  }

  /* Search Tree */

  // Defined in Lab1Like.scala:
  //
  // sealed abstract class SearchTree
  // case object Empty extends SearchTree
  // case class Node(l: SearchTree, d: Int, r: SearchTree) extends SearchTree

  def repOk(t: SearchTree): Boolean = {
    def check(t: SearchTree, min: Int, max: Int): Boolean = t match {
      case Empty => true
      case Node(l, d, r) =>
        if((d <= max) && (d >= min)){
          check(l, min, d) && check(r, d, max)
        } else {
          false
        } //making sure d is within the bound
        //then we checking the left child and the right child to see if the value fits recursiverly with their nodes
        //it continues on until its empty, if all pass it is valid, if one of them fail then they all fail
        //There is an issue with this though, d could be equal to max, but when i put d <= max it fails. Grading error perhaps?
    }
    check(t, Int.MinValue, Int.MaxValue)
  }

  def insert(t: SearchTree, n: Int): SearchTree = {
    //if n < d -> l
    //else -> r
    //if there is NOTHING THEN CREATE THE NODE TREE!
    t match {
      case Empty => return Node(Empty, n, Empty)
      case Node(l, d, r) =>{ //is there a way i can check the value of just seeing if there is a node without needing a left or right child
        if(n < d) return Node(insert(l, n), d, r) else return Node(l, d, insert(r, n))
      }
    }
  }


  //Ask about this one pls, so confuse
  def deleteMin(t: SearchTree): (SearchTree, Int) = {
    require(t != Empty)
    (t: @unchecked) match {
      case Node(Empty, d, r) => return (r, d)
      case Node(l, d, r) => {
        val (l1, m) = deleteMin(l) // so then our node will just have the parent node?
        return (Node(l1, d, r), m)
      }
    }
  }

  def delete(t: SearchTree, n: Int): SearchTree = t match{
    //So we have five types of trees: Empty, Root, Left, Right, Full
      //The one he did in class seemed way to complex
    // I can see using recursion to come back to this to find if d is greater than or equal to
    case Empty => Empty //First case, Empty
    case Node(Empty, d, Empty) => if(n != d) Node(Empty, d, Empty) else Empty //second case, only the root. if not equal leave alone else delte
    case Node(l, d, Empty) => if (n == d) l else if (n < d) Node(delete(l,n),d,Empty) else Node(l, d, Empty)
        // if n == d -> we found it, link the child tree to the grandparnet
        // if n < d -> traverse the left child to find n and we can leave the rest how it is
        // if n > d -> we didnt find it, so just return like normal
        // right tree follows the similar logic to this
    case Node(Empty, d, r) =>  if (n == d) r else if(n < d) Node(Empty, d, r) else Node(Empty, d, delete(r, n))
        //So again, if d ==n we will link child to grandparent
        // if n is < d then we dont do anything becuase we didnt find it
       // and if it is more than one thne we just have to get it to traverse right calling this function recursively
    case Node(l, d, r) =>
      if (n == d) {
        val(right_child, mid) = deleteMin(r)
        Node(l, mid, right_child) //delete min is the helper func, going to replace d with r(mid)
      } else if(n < d){
        Node(delete(l,n),d,r) //go left if n < d
      } else{
        Node(l, d, delete(r, n)) //go right if n > d
      }
  }

  /* JavaScripty */

  def eval(e: Expr): Double = e match {
    //eval takes in a string and parses through it
    //depending on the context and the structre depends on the class
    //this is why there is two classes for Unary and Binary
    case N(n) => n
    //UOP CLASS
    //Single expressions
    //some functions are noted out. I tried to have fun and create a JavaScripty++
    case Unary(uop, rhs) => uop match {
      case Neg => -1 * eval(rhs) //Returns negative value
      //case Uinc => ++rhs //Unary Increment
      //case Udec => --rhs //Unary Decrement
      //case Unot => !rhs //Unary not for boolean values
      case _ => throw new UnsupportedOperationException //we dont know any other unary expressions ;)
    }
    //BOP CLASS
    //Eval parses through the two expressions
    //recursively runs through each one to repeat as needed
    //the expressions is self explanitory
    case Binary(bop, lhs, rhs) => bop match {
      case Plus => eval(lhs) + eval(rhs) //plus
      case Minus => eval(lhs) - eval(rhs) //Minus
      case Times => eval(lhs) * eval(rhs) //mult
      case Div => eval(lhs) / eval(rhs) //Div
      //case Exp => Math.pow(lhs, rhs) //exponent
      case _ => throw new UnsupportedOperationException
    }
    //and then if it isnt any of the two classes
    case _ => throw new UnsupportedOperationException
  }

 // Interface to run your interpreter from a string.  This is convenient
 // for unit testing.
 def eval(s: String): Double = eval(Parser.parse(s))



 /* Interface to run your interpreter from the command-line.  You can ignore the code below. */

 def processFile(file: java.io.File) {
    if (debug) { println("Parsing ...") }

    val expr = Parser.parseFile(file)

    if (debug) {
      println("\nExpression AST:\n  " + expr)
      println("------------------------------------------------------------")
    }

    if (debug) { println("Evaluating ...") }

    val v = eval(expr)

    println(prettyNumber(v))
  }

}
