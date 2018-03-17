package jsy.student

import jsy.lab4.{Lab4Like, ast}

object Lab4 extends jsy.util.JsyApplication with Lab4Like {
  import jsy.lab4.ast._
  import jsy.lab4.Parser
  
  /*
   * CSCI 3155: Lab 4
   * Jason Lubrano
   * 
   * Partner: Vy Le
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
   * '???' as needed to get something that compiles without error. The '???'
   * is a Scala expression that throws the exception scala.NotImplementedError.
   */
  
  /* Collections and Higher-Order Functions */
  
  /* Lists */
  
  def compressRec[A](l: List[A]): List[A] = l match {
    case Nil | _ :: Nil => return l
    case h1 :: (t1 @ (h2 :: _)) => if (h1 == h2) compressRec(t1) else h1::compressRec(t1)
  }
  
  def compressFold[A](l: List[A]): List[A] = l.foldRight(Nil: List[A]){
    (h, acc) => acc match {
      case Nil => h :: Nil
      case h1 :: tail => if(h == h1) acc else h :: acc
    }
  }
  
  def mapFirst[A](l: List[A])(f: A => Option[A]): List[A] = l match {
    case Nil => Nil
    case h :: t => f(h) match {
      case None => h :: mapFirst(t)(f)
      case Some(foo) => foo :: t
    }
  }
  
  /* Trees */

  def foldLeft[A](t: Tree)(z: A)(f: (A, Int) => A): A = {
    def loop(acc: A, t: Tree): A = t match {
      case Empty => acc /* empty tree */
      case Node(l, d, r) => loop(f(loop(acc, l), d), r) /* apply to d, loop through left tree, then right */
    }
    loop(z, t)
  }

  // An example use of foldLeft
  def sum(t: Tree): Int = foldLeft(t)(0){ (acc, d) => acc + d }

  // Create a tree from a list. An example use of the
  // List.foldLeft method.
  def treeFromList(l: List[Int]): Tree =
    l.foldLeft(Empty: Tree){ (acc, i) => acc insert i }

  def strictlyOrdered(t: Tree): Boolean = {
    val (b, _) = foldLeft(t)((true, None: Option[Int])){
      (acc, d) => (acc, d) match {
        case ((false, _), i) => (false, Some(i))
        case ((true, None), i) => (true, Some(i))
        case ((true, Some(x)), i) => if (x < i) (true, Some(i)) else (false, Some(i))
      }
    }
    b
  }

  /* Type Inference */

  // While this helper function is completely given, this function is
  // worth studying to see how library methods are used.
  def hasFunctionTyp(t: Typ): Boolean = t match {
    case TFunction(_, _) => true
    case TObj(fields) if (fields exists { case (_, t) => hasFunctionTyp(t) }) => true
    case _ => false
  }
  
  def typeof(env: TEnv, e: Expr): Typ = {
    def err[T](tgot: Typ, e1: Expr): T = throw StaticTypeError(tgot, e1, e)

    e match {
      case Print(e1) => typeof(env, e1); TUndefined
      case N(_) => TNumber
      case B(_) => TBool
      case Undefined => TUndefined
      case S(_) => TString
      case Var(x) => lookup(env, x)
      case Decl(mode, x, e1, e2) => {
        val env2 = extend(env, x, typeof(env, e1))
        typeof(env2, e2)
      }

      case Unary(Neg, e1) => typeof(env, e1) match {
        case TNumber => TNumber
        case tgot => err(tgot, e1)
      }

      case Unary(Not, e1) => typeof(env, e1) match {
        case TBool => TBool
        case tsomeothertype => err(tsomeothertype, e1)
      }

      case Binary(Plus, e1, e2) => (typeof(env, e1), typeof(env, e2)) match {
        case (TNumber, TNumber) => TNumber
        case (TString, TString) => TString
        case (tsomeothertype: Typ, _) if(tsomeothertype != TNumber && tsomeothertype != TString) => err(tsomeothertype, e1)
        case (_, tsomeothertype: Typ) => err(tsomeothertype, e2)
      }

      case Binary(Minus|Times|Div, e1, e2) => (typeof(env, e1), typeof(env, e2)) match {
        case (TNumber, TNumber) => TNumber
        case (tsomeothertype: Typ, _) => err(tsomeothertype, e1)
        case (_, tsomeothertype: Typ) => err(tsomeothertype, e2)
      }

      case Binary(Eq|Ne, e1, e2) => (typeof(env, e1)) match {
        case tsomeothertype if (hasFunctionTyp(tsomeothertype)) => err(tsomeothertype, e1)
        case _ => typeof(env, e2) match {
          case tsomeothertype if (hasFunctionTyp(tsomeothertype)) => err(tsomeothertype, e2)
          case _ => if (typeof(env, e1) == typeof(env, e2)) TBool else err(typeof(env, e2), e2)
        }

      }

      case Binary(Lt|Le|Gt|Ge, e1, e2) => (typeof(env, e1), typeof(env, e2)) match {
        case (TNumber, TNumber) => TBool
        case (TString, TString) => TBool
        case (tsomeothertype: Typ, _) if (tsomeothertype != TNumber && tsomeothertype != TString) => err(tsomeothertype, e1)
        case (_, tsomeothertype: Typ) if (tsomeothertype != TNumber && tsomeothertype != TString) => err(tsomeothertype, e2)
      }

      case Binary(And|Or, e1, e2) => (typeof(env, e1), typeof(env, e2)) match {
        case (TBool, TBool) => TBool
        case (tsomeothertype: Typ, _) if (tsomeothertype != TBool) => err(tsomeothertype, e1)
        case (_, tsomeothertype: Typ) if (tsomeothertype != TBool) => err(tsomeothertype, e2)
      }

      case Binary(Seq, e1, e2) => typeof(env, e1); typeof(env, e2)

      case If(e1, e2, e3) => (typeof(env, e1), typeof(env, e2), typeof(env, e3)) match {
        case (TBool, typ1, typ2) => if (typ1 == typ2) typ1 else typ2
        case (tsomeothertype, _, _) => err(tsomeothertype, e1)
      }

      case Function(p, params, tann, e1) => {
        // Bind to env1 an environment that extends env with an appropriate binding if
        // the function is potentially recursive.
        val env1 = (p, tann) match {
          case(Some(foo), Some(typret)) => val funtyp = TFunction(params, typret); env + (foo -> funtyp)
          case (None, _) => env
          case _ => err(TUndefined, e1)
        }
        // Bind to env2 an environment that extends env1 with bindings for params.
        val env2 = params.foldLeft(env1:TEnv){
          case(env1, (s: String, MTyp(_, t))) => extend(env1, s, t)
        } /* ask vy about this one */
        // Infer the type of the function body
        val t1 = typeof(env2, e1)
        // Check with the possibly annotated return type
        tann match {
          case None => {
            val ttyp1 = inferType(e1)
            val ttyp2 = TFunction(params, ttyp1)
            ttyp2
          }
          case Some(trettyp) => {
            val ttyp1 = inferType(e1)
            val ttyp2 = TFunction(params, ttyp1)
            if (ttyp2 != TFunction(params, trettyp)) err(ttyp1, e1) else TFunction(params, trettyp)
          }
        }
      }

      case Call(e1, args) => typeof(env, e1) match {
        case TFunction(params, tret) if (params.length == args.length) =>
          (params zip args).foreach {
            case ((_, MTyp(_, t)), arg) => {
              /* have to be equal types */
              if(t != typeof(env, arg)) err(typeof(env, arg), e1)
            }
          };
          tret
        case tgot => err(tgot, e1)
      }

        /* map expr to type, keep same field name */
      case Obj(fields) => TObj(fields mapValues { (e1) => typeof(env, e1)} )

      case GetField(e1, f) => val type1 = typeof(env, e1); type1 match {
        case TObj(fields) => fields.get(f) match {
            /* e1 must be an object of that field or error */
          case Some(v) => v
          case None => err(type1, e1)
        }
          /* not object type */
        case _ => err(type1, e1)
      }
    }
  }
  
  
  /* Small-Step Interpreter */

  /*
   * Helper function that implements the semantics of inequality
   * operators Lt, Le, Gt, and Ge on values.
   *
   * We suggest a refactoring of code from Lab 2 to be able to
   * use this helper function in eval and step.
   *
   * This should the same code as from Lab 3.
   */
  def inequalityVal(bop: Bop, v1: Expr, v2: Expr): Boolean = {
    require(isValue(v1), s"inequalityVal: v1 ${v1} is not a value")
    require(isValue(v2), s"inequalityVal: v2 ${v2} is not a value")
    require(bop == Lt || bop == Le || bop == Gt || bop == Ge)
    (v1, v2) match {
      case (N(n1), N(n2)) => (bop) match {
        case Lt => n1 < n2
        case Gt => n1 > n2
        case Le => n1 <= n2
        case Ge => n1 >= n2
      }// delete this line when done
      case (S(s1), S(s2)) => (bop) match {
        case Lt => s1 < s2
        case Gt => s1 > s2
        case Le => s1 <= s2
        case Ge => s1 >= s2
      }
    }
  }

  /* This should be the same code as from Lab 3 */
  def iterate(e0: Expr)(next: (Expr, Int) => Option[Expr]): Expr = {
    def loop(e: Expr, n: Int): Expr = next(e, n) match {
      case Some(e1) => loop(e1, n + 1)
      case None => e
    }
    loop(e0, 0)
  }

  /* Capture-avoiding substitution in e replacing variables x with esub. */
  def substitute(e: Expr, esub: Expr, x: String): Expr = {
    def subst(e: Expr): Expr = e match {
      case N(_) | B(_) | Undefined | S(_) => e
      case Print(e1) => Print(substitute(e1, esub, x))

      /** *** Cases from Lab 3 */
      case Unary(uop, e1) => Unary(uop, substitute(e1, esub, x))
      case Binary(bop, e1, e2) => Binary(bop, substitute(e1, esub, x), substitute(e2, esub, x))
      case If(e1, e2, e3) => If(substitute(e1, esub, x), substitute(e2, esub, x), substitute(e3, esub, x))
      case Var(y) => if (x == y) esub else e
      case Decl(mode, y, e1, e2) => {
        val e2sub = substitute(e2, esub, x)
        val e1sub = substitute(e1, esub, x)
        if (x == y) e2 else e2sub
        Decl(mode, y, e1sub, e2sub)

      }

      /** *** Cases needing adapting from Lab 3 */
      case Function(p, params, tann, e1) => {
        if ((params exists ((parm) => parm._1 == x)) || (p == Some(x))) e
        else Function(p, params, tann, substitute(e1, esub, x))
      }

      case Call(e1, args) => Call(substitute(e1, esub, x), args map { e1 => substitute(e1, esub, x) })

      /** *** New cases for Lab 4 */

      case Obj(fields) => Obj(fields mapValues { (exp) => substitute(exp, esub, x) })

      case GetField(e1, foo) => if (x == foo) e else GetField(substitute(e1, esub, x), foo)
    }

      val freshvs = freeVars(esub)
      def fresh(x: String): String = if (freshvs contains x) fresh(x + "$") else x // will use this later with rename
      subst(rename(e)(fresh))
  }

  /* Rename bound variables in e */
  def rename(e: Expr)(fresh: String => String): Expr = {
    def ren(env: Map[String,String], e: Expr): Expr = {
      e match {
        case N(_) | B(_) | Undefined | S(_) => e
        case Print(e1) => Print(ren(env, e1))

        case Unary(uop, e1) => Unary(uop, ren(env, e1))
        case Binary(bop, e1, e2) => Binary(bop, ren(env, e1), ren(env, e2))
        case If(e1, e2, e3) => If(ren(env, e1), ren(env, e2), ren(env, e3))

        case Var(y) => {
        /* ask austin */
          if(env contains y) Var(lookup(env, y))
          else Var(y)
          /* are we just seeing if the var gets renamed? */
        }

        case Decl(mode, y, e1, e2) => /* still dont understand why we changed constdecl to just decl */
          val yp = fresh(y)
          //Decl(mode, yp, ren(env, e1), ren(env, e2)) /* have to extend our env with y as yp */
          Decl(mode, yp, ren(env, e1), ren(extend(env, y, yp), e2))

        case Function(p, params, retty, e1) => {
          val (pp, envp): (Option[String], Map[String,String]) = p match {
            case None => (None, env)
            case Some(x) => val foo = fresh(x);(Some(foo), env + (x -> foo))
          }
          val (paramsp, envpp) = params.foldRight( (Nil: List[(String,MTyp)], envp) ) {
            case ((styp, mtyp @ MTyp(_,_)), (prevList , envprev)) =>
              val sptyp = fresh(styp)
              ((sptyp, mtyp) :: prevList, envprev + (styp -> sptyp))
          }
          Function(pp, paramsp, retty, ren(envpp,e1))
        }

        case Call(e1, args) => Call(ren(env,e1), args map {case (e11) => ren(env, e11)} )

        case Obj(fields) => Obj(fields mapValues((e11) => ren(env, e11)))
        case GetField(e1, f) => GetField(ren(env, e1), f)
      }
    }
    ren(empty, e)
  }

  /* Check whether or not an expression is reduced enough to be applied given a mode. */
  def isRedex(mode: Mode, e: Expr): Boolean = mode match {
    case MConst => if(!isValue(e)) true else false
    case MName => false
  }

  def step(e: Expr): Expr = {
    require(!isValue(e), s"step: e ${e} to step is a value")
    e match {
      /* Base Cases: Do Rules */
      case Print(v1) if isValue(v1) => println(pretty(v1)); Undefined
        /***** Cases needing adapting from Lab 3. */
      case Unary(Neg, v1) if isValue(v1) => v1 match {
        case N(n1) => N(-n1)
        case _ => throw StuckError(v1)
      }
      case Unary(Not, e1) if isValue(e1) => e1 match {
        case B(b1) => B(!b1)
        case _ => throw StuckError(e1)
      }
      case Binary(bop, e1, e2) if isValue(e1) => bop match {
        /* seq */
        case Seq => e2
        /* arithmatic */
        case Plus if isValue(e2) => (e1, e2) match {
          case (N(n1), N(n2)) => N(n1 + n2)
          case (S(s1), S(s2)) => S(s1 + s2)
          case _ => throw StuckError(e)
        }
        case Minus if isValue(e2) => (e1, e2) match {
          case (N(n1), N(n2)) => N(n1 - n2)
          case _ => throw StuckError(e)
        }
        case Times if isValue(e2) => (e1, e2) match {
          case (N(n1), N(n2)) => N(n1 * n2)
          case _ => throw StuckError(e)
        }
        case Div if isValue(e2) => (e1, e2) match {
          case (N(n1), N(n2)) => N(n1/n2)
          case _ => throw StuckError(e)
        }
        /* logical ops */
        case Lt | Gt | Le | Ge => B(inequalityVal(bop, e1, e2))
        case Eq => B(e1 == e2)
        case And => e1 match {
          case B(b) => if (b) e2 else B(false)
          case _ => throw StuckError(e)
        }
        case Or => e1 match {
          case B(b) => if (b) e1 else B(false)
          case _ => throw StuckError(e)
        }

      }

      case If(e1, e2, e3) if isValue(e1) => e1 match {
        case B(b) => if(b) e2 else e3
        case _ => throw StuckError(e)
      }

      case GetField(e1, foo) if isValue(e1) => e1 match {
        case Obj(fields) => fields.get(foo) match {
          case None => throw StuckError(e)
          case Some(ee) => ee
        }
        case _ => throw StuckError(e)
      }

        /* anywhere there is an e2, replace it witht the x e1 val */
      case Decl(mode, x, e1, e2) if !isRedex(mode, e1) => substitute(e2, e1, x)
        /***** More cases here */
      case Call(v1, args) if isValue(v1) =>
        v1 match {
          case Function(p, params, _, e1) => {
            val pazip = params zip args
            if (pazip forall{ case ((_, MTyp(m,_)), arg) => !isRedex(m, arg) }) {
              val e1p = pazip.foldRight(e1) {
                case (((x, _), arg_value), acc) => substitute(acc, arg_value, x)
              }

              p match {
                case None => e1p
                case Some(x1) => substitute(e1p, v1, x1)
              }

            } else {
              val pazipp = mapFirst(pazip) {
                case (param@(_: String, MTyp(m, _)), arg: Expr) if isRedex(m, arg) => Some((param, step(arg)))
                case _ => None
              }
              Call(v1, pazipp.unzip._2)
            }
          }
          case _ => throw StuckError(e)
        }
        /***** New cases for Lab 4. */

      /* Inductive Cases: Search Rules */
      case Print(e1) => Print(step(e1))
        /***** Cases from Lab 3. */
      case Unary(uop, e1) => Unary(uop, step(e1))
        /***** More cases here */
      case Binary(bop, e1, e2) => if (!isValue(e1)) { Binary(bop, step(e1), e2) } else { Binary(bop, e1, step(e2)) }
      case If(e1, e2, e3) => If(step(e1), e2, e3)
      case Decl(mode, x, e1, e2) => Decl(mode, x, step(e1), e2)
      case Obj(fields) if !isValue(e) => fields find {(foo) => !isValue(foo._2)} match { // finds first key that doesn't map to value
        case None => throw StuckError(e) // we shouldn't reach this
        case Some((bar, e1)) => Obj(fields + (bar -> step(e1))) // update this key to map to stepped e
      }

      case GetField(e1, foo) => e1 match {
        case Obj(_) => GetField(step(e1), foo)
        case _ => throw StuckError(e1)
      }
        /***** Cases needing adapting from Lab 3 */
      case Call(v1 @ Function(_, _, _, _), args) => ???
      case Call(e1, args) => Call(step(e1), args)
        /***** New cases for Lab 4. */

      /* Everything else is a stuck error. Should not happen if e is well-typed.
       *
       * Tip: you might want to first develop by comment out the following line to see which
       * cases you have missing. You then uncomment this line when you are sure all the cases
       * that you have left the ones that should be stuck.
       */
      case _ => throw StuckError(e)
    }
  }
  
  
  /* External Interfaces */
  
  //this.debug = true // uncomment this if you want to print debugging information
  this.keepGoing = true // comment this out if you want to stop at first exception when processing a file
}

