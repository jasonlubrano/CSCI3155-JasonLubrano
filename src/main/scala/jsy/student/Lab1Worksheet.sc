/*
 * CSCI 3155: Lab 1 Worksheet
 *
 * This worksheet demonstrates how you could experiment
 * interactively with your implementations in Lab1.scala.
 */

/*
 * Example: Test-driven development of plus
 */

/*
def g(x: Int) = {
  val (a, b) = (1, (x, 3))
  if (x == 0) (b, 1) else (b, a + 2)
}
g(1)
g(0)
*/



def abs(n: Double): Double = if(n < 0) (-n) else n
abs(n = -3)
abs(n = 0)
abs(n = 3)


/*
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
xor(a = true, b = true)
xor(a = true, b = false)
xor(a = false, b = true)
xor(a = false ,b = false)
*/

/*
def repeat(s: String, n: Int): String = {
  if(n > 0) return s + repeat(s, n-1)
  else return ""
}
println("%s", repeat("hi", 3))
*/


def sqrtStep(c: Double, xn: Double): Double = {
  require(c >= 0)
  require(xn >= 0)
  return (xn - ((xn*xn - c)/(xn+xn)))
  //plug n chug
}

sqrtStep(0.001, 16)

def sqrtN(c: Double, x0: Double, n: Int): Double = {
  require(n >= 0)
  require(x0 >= 0)
  if(n == 0) (return x0) else (return sqrtN(c, sqrtStep(c, x0), n-1))
  //scala is ugly
  //we will need to check and see if n is eq to 0. if it is then we just return our guess
  //else were going to recursively call sqrtN with n-1, and a more educated guess
  //reference newtons method paper i wrote
}

sqrtN(0.001, 16, 2)

def sqrtErr(c: Double, x0: Double, epsilon: Double): Double = {
  require(epsilon > 0) //infinite loop whoops lol, check for c >= 0 in sqrt step
  if(abs(x0*x0 - c) < epsilon)(return x0) else (return sqrtErr(c, sqrtStep(c, x0), epsilon))
  //if less than eps, return x0. Else were going to recursively call sqrtStep w/ guess so it within error bounds
}

sqrtErr(0.001, 16, 0.1)

def sqrt(c: Double): Double = {
  require(c >= 0)
  if (c == 0) 0 else sqrtErr(c, 1.0, 0.0001)
}




/*
def sqrtStep(c: Double, xn: Double): Double = ???

def sqrtN(c: Double, x0: Double, n: Int): Double = ???

def sqrtErr(c: Double, x0: Double, epsilon: Double): Double = ???

def sqrt(c: Double): Double = {
  require(c >= 0)
  if (c == 0) 0 else sqrtErr(c, 1.0, 0.0001)
}
*/
/*
// Here we can write expressions to experiment with how we might implement
// something. The expression is evaluated interactively.
1 + 1
val n = 1 + 1
n + 3

// The worksheet is built with all of the project files, so we can call
// a function from your jsy.student.Lab1 object (in Lab1.scala).
jsy.student.Lab1.plus(3, 4)

// We can imports all of the functions from jsy.student.Lab1
import jsy.student.Lab1._
plus(3, 4)

// We can check the implementation here, though it better to write tests
// in Lab1Spec.scala.
assert(plus(1, 1) == 2)

// Braces {} can be used wherever parentheses () can be (but not the other
// way around). Braces {} introduce scope, while () do not.
assert {
  val two = 2
  plus(1, 1) == two
}

/* Exercises */

// Call jsy.student.Lab1.abs
//abs(-3) // Will fail until implemented in Lab1.scala.

// Call the JavaScripty parser (from the provided library) on a string
jsy.lab1.Parser.parse("-4")

// We can import the parse function from jsy.lab1.Parser to experiment
// with the provided parser.
import jsy.lab1.Parser.parse
val negFourAST = parse("-4")

// We also want to import the AST nodes for convenience.
import jsy.lab1.ast._
assert {
  negFourAST match {
    case Unary(_, _) => true
    case _ => false
  }
}

// Evaluate that JavaScripty expression.
//eval(negFourAST)

// For convenience, we also have an eval function that takes a string,
// which calls the parser and then delegates to your eval function.
//eval("1 + 1")
*/