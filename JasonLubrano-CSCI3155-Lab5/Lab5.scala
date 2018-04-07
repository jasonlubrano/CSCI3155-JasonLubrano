package jsy.student

import jsy.lab5.Lab5Like

object Lab5 extends jsy.util.JsyApplication with Lab5Like {
  import jsy.lab5.ast._
  import jsy.util.DoWith
  import jsy.util.DoWith._

  /*
   * CSCI 3155: Lab 5
   * Jason Lubrano
   *
   * Partner: Jesse
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

  /*** Exercise with DoWith ***/

  def rename[W](env: Map[String,String], e: Expr)(fresh: String => DoWith[W,String]): DoWith[W,Expr] = {
    def ren(env: Map[String,String], e: Expr): DoWith[W,Expr] = e match {
      case N(_) | B(_) | Undefined | S(_) | Null | A(_) => doreturn(e)
      case Print(e1) => ren(env,e1) map { e1p => Print(e1p) }

      case Unary(uop, e1) => ren(env,e1) map(e1ren => Unary(uop, e1ren))
      case Binary(bop, e1, e2) => ren(env, e1) flatMap(e1ren => ren(env, e2) map(e2ren => Binary(bop, e1ren, e2ren)))
      case If(e1, e2, e3) => ren(env, e1) flatMap(e1ren => ren(env, e2) flatMap(e2ren => ren(env, e3) map(e3ren => If(e1ren, e2ren, e3ren))))

      case Var(x) => doreturn(if(env contains x) Var(lookup(env, x)) else Var(x) )

      case Decl(m, x, e1, e2) => fresh(x) flatMap { xp =>
        ren(env, e1) flatMap(e1ren => ren(extend(env, x, xp), e2) map(e2ren => Decl(m, xp, e1ren, e2ren)))
      }

      case Function(p, params, retty, e1) => {
        val w: DoWith[W,(Option[String], Map[String,String])] = p match {
          case None => ???
          case Some(x) => ???
        }
        w flatMap { case (pp, envp) =>
          params.foldRight[DoWith[W,(List[(String,MTyp)],Map[String,String])]]( doreturn((Nil, envp)) ) {
            case ((x,mty), acc) => acc flatMap {
              ???
            }
          } flatMap {
            ???
          }
        }
      }

      case Call(e1, args) => ren(env, e1) map(e1ren => Call(e1ren, args))

      case Obj(fields) => ???
      case GetField(e1, f) => ???

      case Assign(e1, e2) => ???

      /* Should not match: should have been removed */
      case InterfaceDecl(_, _, _) => throw new IllegalArgumentException("Gremlins: Encountered unexpected expression %s.".format(e))
    }
    ren(env, e)
  }

  def myuniquify(e: Expr): Expr = {
    val fresh: String => DoWith[Int,String] = { _ =>
      /* ignroe s completely, s + str(i) */
      /* (w) => (w, w) map w => w.toString() -> (w, s) */
      doget[Int].map((i:Int) => "a" + i.toString())

    }
    /* the third parenthesis is the initial state */
    val (_, r) = rename(empty, e)(fresh)(0)
    r
  }

  /*** Helper: mapFirst to DoWith ***/

  // List map with an operator returning a DoWith
  def mapWith[W,A,B](l: List[A])(f: A => DoWith[W,B]): DoWith[W,List[B]] = {
    l.foldRight[DoWith[W,List[B]]]( doreturn(Nil) ) { //
      case (a,dwbs) => dwbs flatMap((bs:List[B]) => (f(a):DoWith[W,B]).map((b) => b :: bs))
    }
  }

  // Map map with an operator returning a DoWith
  def mapWith[W,A,B,C,D](m: Map[A,B])(f: ((A,B)) => DoWith[W,(C,D)]): DoWith[W,Map[C,D]] = {
    m.foldRight[DoWith[W,Map[C,D]]]( doreturn(Map())) {
      case(a, dwbs) => dwbs flatMap(currmap => f(a) map(cd => currmap + (cd._1 -> cd._2)))
    }
  }

  // Just like mapFirst from Lab 4 but uses a callback f that returns a DoWith in the Some case.
  def mapFirstWith[W,A](l: List[A])(f: A => Option[DoWith[W,A]]): DoWith[W,List[A]] = l match {
    case Nil => doreturn(l) //
    case h :: t => f(h) match {
      case None => mapFirstWith(t)(f) map { (ft) => h :: ft}
      case Some(fh) => fh map { (fh) => fh :: t}
    }
  }

  // There are better ways to deal with the combination of data structures like List, Map, and
  // DoWith, but we won't tackle that in this assignment.

  /*** Casting ***/

  def castOk(t1: Typ, t2: Typ): Boolean = (t1, t2) match {
      /***** Make sure to replace the case _ => ???. */
    case (TObj(_), TNull) => true
    case (TNull, TObj(_)) => true
    case (TObj(f1), TObj(f2)) => {
      val f1ok = f1 forall{
        case (stri, typ1) => if (typ1 == None) true else f2.get(stri) match {
          case None => false
          case Some(typ2) => castOk(typ1, typ2)
        }
      }
      val f2ok = f2 forall{
        case (stri, typ1) => if (typ1 == None) true else f1 get(stri) match {
          case None => false
          case Some(typ2) => castOk(typ1, typ2)
        }
      }
      f1ok || f2ok
    }
      /***** Cases for the extra credit. Do not attempt until the rest of the assignment is complete. */
    case (TInterface(tvar, t1p), _) => ??? /* idk */
    case (_, TInterface(tvar, t2p)) => ??? /* idk */
      /***** Otherwise, false. */
    case _ => false
  }

  /*** Type Inference ***/

  // A helper function to check whether a jsy type has a function type in it.
  // While this is completely given, this function is worth studying to see
  // how library functions are used.
  def hasFunctionTyp(t: Typ): Boolean = t match {
    case TFunction(_, _) => true
    case TObj(fields) if (fields exists { case (_, t) => hasFunctionTyp(t) }) => true
    case _ => false
  }

  def isBindex(m: Mode, e: Expr): Boolean = m match {
    /* match the mode if its a var ,name, or ocnst */
      /* from rec & lec */
    case (MVar | MConst | MName) => true
    case MRef => e match {
      case GetField(_, _) => true
      case Var(_) => true
      case _ => false
    }
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
      case Var(x) => {
        val MTyp(m, t) = lookup(env, x)
        t
      }
      case Unary(Neg, e1) => typeof(env, e1) match {
        case TNumber => TNumber
        case someothertype => err(someothertype, e1)
      }
        /***** Cases directly from Lab 4. We will minimize the test of these cases in Lab 5. */
      case Unary(Not, e1) => typeof(env, e1) match {
        case TBool => TBool
        case someothertype => err(someothertype, e1)
      }
      case Binary(Plus, e1, e2) => (typeof(env, e1), typeof(env, e2)) match {
        case (TNumber, TNumber) => TNumber
        case (TString, TString) => TString
        case (TNumber|TString, someothertype) => err(someothertype, e2)
        case (someothertype, TNumber|TString) => err(someothertype, e1)
        case (someothertype, _) => err(someothertype, e1)
      }
      case Binary(Minus|Times|Div, e1, e2) => (typeof(env, e1), typeof(env, e2)) match {
        case (TNumber, TNumber) => TNumber
        case (TNumber, someothertype) => err(someothertype, e2)
        case (someothertype, TNumber) => err(someothertype, e1)
        case (someothertype, _) => err(someothertype, e1)
      }
      case Binary(Eq|Ne, e1, e2) => (typeof(env, e1), typeof(env, e2)) match {
        case(typ1, _) if hasFunctionTyp(typ1) => err(typ1, e1)
        case(_, typ2) if hasFunctionTyp(typ2) => err(typ2, e2)
        case(typ1, typ2) => if(typ1 == typ2) TBool else err(typ2, e2)
      }
      case Binary(Lt|Le|Gt|Ge, e1, e2) =>(typeof(env,e1), typeof(env,e2)) match {
        case (TNumber,TNumber) => TBool
        case (TString, TString) => TBool
        case (TNumber|TString, someothertype) => err(someothertype, e2)
        case (someothertype, TNumber|TString) => err(someothertype,e1)
        case (someothertype, _) => err(someothertype, e1)
      }
      case Binary(And|Or, e1, e2) => (typeof(env,e1), typeof(env,e2)) match {
        case (TBool, TBool) => TBool
        case (TBool, someothertype) => err(someothertype, e2)
        case (someothertype,_) => err(someothertype, e1)
      }
      case Binary(Seq, e1, e2) =>(typeof(env,e1), typeof(env,e2)) match {
        case (_ , t2) => t2
      }
      case If(e1, e2, e3) => (typeof(env,e1), typeof(env,e2), typeof(env,e3)) match {
        case (TBool, t1, t2)  => if(t1 == t2) t1 else err(t2,e2)
        case (someothertype, _, _) => err(someothertype, e1)
      }

      case Obj(fields) => {
        fields foreach ((exp1) => typeof(env, exp1._2))
        TObj(fields mapValues((exp1) => typeof(env, exp1)))
      }
      case GetField(e1, f) => typeof(env,e1) match {
        case TObj(fields) => fields.get(f) match {
          case Some(foo) => foo
          case None => err(TObj(fields), e1)
        }
        case someothertype => err(someothertype, e1)
      }

        /***** Cases from Lab 4 that need a small amount of adapting. */
      case Decl(m, x, e1, e2) =>{
        val typ2 = typeof(extend(env, x, MTyp(m, typeof(env, e1))), e2)
        if (isBindex(m, e1)) typ2 else err(typ2, e1)
      }

      case Function(p, params, tann, e1) => {
        // Bind to env1 an environment that extends env with an appropriate binding if
        // the function is potentially recursive.
        val env1 = (p, tann) match {
          case (Some(f), Some(tret)) =>
            val tprime = TFunction(params, tret)
            extend(env, f, MTyp(MConst, tprime))
          case (None, _) => env
          case _ => err(TUndefined, e1)
        }
        // Bind to env2 an environment that extends env1 with bindings for params.
        val env2 = params.foldLeft(env1)({
          case (currEnv: TEnv, (s: String, mt@MTyp(_, _))) => currEnv + (s -> mt)
        })
        // Match on whether the return type is specified.
        val typ1 = typeof(env2, e1)
        tann match {
          case None => TFunction(params, typ1)
          case Some(tret) => if (TFunction(params, typ1) == TFunction(params, tret)) {
            TFunction(params, typ1)
          } else {
            err(TFunction(params, tret), e1)
          }
        }
      }

      case Call(e1, args) => typeof(env, e1) match {
        case TFunction(params, tret) if params.length == args.length =>
          (params, args).zipped.foreach {
            /* make sure that 1) it is the right case and 2) it is bindable, lec / rec*/
            case ((s, MTyp(m,t)), arg) => if ((t != typeof(env, arg)) || (!isBindex(m, arg))){
              err(typeof(env, arg), arg)
            }
          }
          tret
        case tgot => err(tgot, e1)
      }

        /***** New cases for Lab 5. ***/
      case Assign(Var(x), e1) => env get x match {
        case None => err(typeof(env, e1), e1)
        case Some(MTyp(m, t)) => m match {
          case (MVar | MRef) => typeof(env, e1) match {
            case tsomeothertype => if (tsomeothertype == t){
              t
            } else {
              err(tsomeothertype, e1)
            }
          }
          case _ => err(t, e)
        }
      }

      case Assign(GetField(e1, f), e2) => typeof(env, e1) match {
        case tsomeothertype => err(tsomeothertype, e1)
        case TObj(fieldfoo) => {
          val typ1 = fieldfoo(f)
          if(typ1 == typeof(env, e2)){
            typ1
          } else {
            err(typ1, e1)
          }
        }
      }

      case Assign(_, _) => err(TUndefined, e)

      case Null => TNull

      case Unary(Cast(t), e1) => typeof(env, e1) match {
        case tgot if (castOk(typeof(env, e1), t))  => t
        case tgot => err(tgot, e1)
      }

      /* Should not match: non-source expressions or should have been removed */
      case A(_) | Unary(Deref, _) | InterfaceDecl(_, _, _) => throw new IllegalArgumentException("Gremlins: Encountered unexpected expression %s.".format(e))
    }
  }

  /*** Small-Step Interpreter ***/

  /*
   * Helper function that implements the semantics of inequality
   * operators Lt, Le, Gt, and Ge on values.
   *
   * We suggest a refactoring of code from Lab 2 to be able to
   * use this helper function in eval and step.
   *
   * This should the same code as from Lab 3 and Lab 4.
   */
  def inequalityVal(bop: Bop, v1: Expr, v2: Expr): Boolean = {
    require(isValue(v1), s"inequalityVal: v1 ${v1} is not a value")
    require(isValue(v2), s"inequalityVal: v2 ${v2} is not a value")
    require(bop == Lt || bop == Le || bop == Gt || bop == Ge)
    (v1, v2) match {
      case (S(s1), S(s2)) => {
        bop match {
          case Lt => s1 < s2
          case Gt => s1 > s2
          case Le => s1 <= s2
          case Ge => s1 >= s2
        }
      }
      case (N(n1), N(n2)) => {
        bop match {
          case Lt => n1 < n2
          case Gt => n1 > n2
          case Le => n1 <= n2
          case Ge => n1 >= n2
        }
      }
    }
  }

  /* Capture-avoiding substitution in e replacing variables x with esub. */
  def substitute(e: Expr, esub: Expr, x: String): Expr = {
    def subst(e: Expr): Expr = e match {
      case N(_) | B(_) | Undefined | S(_) | Null | A(_) => e
      case Print(e1) => Print(subst(e1))
        /***** Cases from Lab 3 */
      case Unary(uop, e1) => Unary(uop, subst(e1))
      case Binary(bop, e1, e2) => Binary(bop, subst(e1), subst(e2))
      case If(e1, e2, e3) => If(subst(e1), subst(e2), subst(e3))
      case Var(y) => if(x == y) esub else e
        /***** Cases need a small adaption from Lab 3 */
      case Decl(mut, y, e1, e2) => Decl(mut, y, subst(e1), if (x == y) e2 else subst(e2))
        /***** Cases needing adapting from Lab 4 */
      case Function(p, params, retty, e1) => p match {
        case None => if (params.exists(pa => pa._1 == x)){
          e
        } else {
          Function(p, params, retty, subst(e1))
        }
        case Some(foo) => if(foo == x || params.exists(pa => pa._1 == x)){
          e
        } else {
          Function(p, params, retty, subst(e1))
        }
      }
        /***** Cases directly from Lab 4 */
      case Call(e1, args) => Call(subst(e1), args map subst)
      case Obj(fields) => Obj(fields map { case (f1, exp1) => (f1, subst(exp1))})
      case GetField(e1, f) => GetField(subst(e1), f)
        /***** New case for Lab 5 */
      case Assign(e1, e2) => Assign(subst(e1), subst(e2))

      /* Should not match: should have been removed */
      case InterfaceDecl(_, _, _) => throw new IllegalArgumentException("Gremlins: Encountered unexpected expression %s.".format(e))
    }

    def myrename(e: Expr): Expr = {
      val fvs = freeVars(esub)
      def fresh(x: String): String = if (fvs contains x) fresh(x + "$") else x
      rename[Unit](e)(){ x => doreturn(fresh(x)) }
    }

    subst(myrename(e))
  }

  /* Check whether or not an expression is reduced enough to be applied given a mode. */
  def isRedex(mode: Mode, e: Expr): Boolean = mode match {
    /* match the mode like bindex */
    case (MConst | MVar) if !isValue(e) => true
    case MRef => e match {
      case lval if isLValue(lval) => false
      case _ => true
    }
    case _ => false
  }

  def getBinding(mode: Mode, e: Expr): DoWith[Mem,Expr] = {
    require(!isRedex(mode,e), s"expression ${e} must not reducible under mode ${mode}")
    mode match {
      case (MConst | MName | MRef) => doreturn(e)
      case MVar => {
        memalloc(e) map(addy => Unary(Deref, addy))
      }
    }
  }

  /* A small-step transition. */
  def step(e: Expr): DoWith[Mem, Expr] = {
    require(!isValue(e), "stepping on a value: %s".format(e))
    e match {
      /* Base Cases: Do Rules */
      case Print(v1) if isValue(v1) => doget map { m => println(pretty(m, v1)); Undefined }
        /***** Cases needing adapting from Lab 3. */
      case Unary(Neg, v1) if isValue(v1) => doget map { _ =>
        v1 match {
          case N(n1) => N(-n1)
          case _ => throw StuckError(e)
        }
      } // throw the error if we get stuck. copy from what austin did in rec.
      /***** More cases here */
      case Unary(Not, v1) if isValue(v1) => doget map { _ =>
        v1 match {
          case B(b1) => B(!b1)
          case _ => throw StuckError(e)
        }
      }
        /***** More cases here */
      case Binary(Plus, S(s1), S(s2)) => doget map( w => S(s1 + s2))/* get memory, place it in r pos, call map with it so we get access, did manipulation and applied to it */
      case Binary(Plus, N(n1), N(n2)) =>  doget map( w => N(n1 + n2))
      case Binary(Seq, v1, e2) if isValue(v1) => doget map (w => e2)
      case Binary(bop @ (Lt|Le|Gt|Ge), v1, v2) if isValue(v1) && isValue(v2) => doget map( w => B(inequalityVal(bop, v1, v2)))
      case Binary(Eq, v1, v2) if isValue(v1) && isValue(v2) => doget map( w => B(v1 == v2))
      case Binary(Ne, v1, v2) if isValue(v1) && isValue(v2) => doget map( w => B(v1 != v2))
      case Binary(And, B(b1), e2) => doget map(w => if (b1) e2 else B(false) )
      case Binary(Or, B(b1), e2) => doget map(w => if (b1) B(true) else e2 )
      case Binary(Minus, N(n1), N(n2)) => doget map(w => N(n1 - n2))
      case Binary(Times, N(n1), N(n2)) => doget map(w => N(n1 * n2))
      case Binary(Div, N(n1), N(n2)) => doget map(w => N(n1 / n2))
      case If(B(b1), e2, e3) => doget map(w => if (b1) e2 else e3 )
        /***** Cases needing adapting from Lab 4. */
      case Obj(fields) if fields forall { case (_, vi) => isValue(vi)} => memalloc(e) map(a => a)
      case GetField(a @ A(_), f) => {
        doget map ((memory: Mem) => memory get(a) match {
          case Some(Obj(f1)) => f1 get(f) match {
            case Some(foo) => foo
            case _ => throw StuckError(e)
          }
          case _ => throw StuckError(e)
        })
      }

      case Decl(MConst, x, v1, e2) if isValue(v1) => doreturn(substitute(e2, v1, x))
      case Decl(MVar, x, v1, e2) if isValue(v1) => memalloc(v1) map(addy => substitute(e2, Unary(Deref, addy), x))
      case Decl(mode, x, e1, e2) if !isRedex(mode, e1) => {
        getBinding(mode, e1) map(e1foo => substitute(e2, e1, x))
      }

        /***** New cases for Lab 5. */
      case Unary(Deref, a @ A(_)) =>
        doget map(addy => addy get a match {
          case None => throw StuckError(e)
          case Some(e) => e
        })

      case Assign(Unary(Deref, a @ A(_)), v) if isValue(v) =>
        domodify[Mem] { m => m + (a -> v) } map { _ => v }

      case Assign(GetField(a @ A(_), f), v) if isValue(v) => {
        domodify[Mem]{ m => m(a) match {
            case Obj(foo) => m + (a -> Obj(foo + ((f, v))))
            case _ => throw StuckError(e)
          }
        } map(_ => v)
      }

      case Call(v @ Function(p, params, _, e), args) => {
        val pazip = params zip args
        if (pazip forall{case ((_, MTyp(m, _)), arg) => isRedex(m, arg)}) {
          val dwep = pazip.foldRight( doreturn(e) : DoWith[Mem,Expr] )  {
            case (((xi, MTyp(mi, _)), ei), dwacc) => getBinding(mi, ei) flatMap(eip => dwacc map(ep => substitute(ep, eip, xi)))
          }
          p match {
            case None => dwep
            case Some(x) => dwep map(exp1 => substitute(exp1, v, x))
          }
        }
        else {
          val dwpazipp = mapFirstWith(pazip) {
            /* go through each argument, starting with the first, then step them */
            case (param @ (_ : String, MTyp(m, _)), arg1: Expr) if isRedex(m, arg1) => Some(step(arg1) map(argparam => (param, argparam)))
            case _ => None
          }
          /* call again with teh updated params */
          dwpazipp map(updatedparams => Call(v, updatedparams.unzip._2))
        }
      }

      /* Base Cases: Error Rules */
        /***** Replace the following case with a case to throw NullDeferenceError.  */
      //case _ => throw NullDeferenceError(e)
      case Assign(GetField(Null, _ ), e2) => throw NullDereferenceError(e)
      case GetField(Null, _) => throw NullDereferenceError(e)

      /* Inductive Cases: Search Rules */
        /***** Cases needing adapting from Lab 3. Make sure to replace the case _ => ???. */
      case Print(e1) => step(e1) map { e1p => Print(e1p) }
      case Unary(uop, e1) => step(e1) map(e1stepped => Unary(uop , e1stepped))
        /* bop case, if case,  cant believe i forgot about these */
      case Binary(bop, e1, e2) => step(e1) map(e1stepped => Binary(bop, e1stepped, e2))
      case Binary(bop, v1, e2) if isValue(v1) => step(e2) map(e2stepped => Binary(bop, v1, e2stepped))
      case If(e1, e2, e3) => step(e1) map(e1stepped => If(e1stepped, e2, e3))
        /***** Cases needing adapting from Lab 4 */
      case GetField(e1, f) => step(e1) map(e1stepped => GetField(e1stepped, f))
      case Obj(fields) => {
        fields find {foo => !isValue(foo._2)} match {
          case Some((foox, esome)) => step(esome) map(esomestepped => Obj(fields + (foox -> esomestepped)))
          case None => throw StuckError(e)
        }
      }
      case Decl(mode, x, e1, e2) => step(e1) map(e1stepped => Decl(mode, x, e1stepped, e2))
      case Call(e1, args) => step(e1) map(e1stepped => Call(e1stepped, args))

        /***** New cases for Lab 5.  */
        /* i HATE assign case */
        /* check if e1 is not a value, then we need to step it/
        else we have to step e2, i think that is why there is twoo cases? in that case heh why not make 1 */
      case Assign(e1, e2) if !isLValue(e1)=> step(e1) map(e1stepped => Assign(e1stepped, e2))
        /* after exhausting the steps to e1, check for e2 */
      case Assign(e1, e2) => step(e2) map(e2stepped => Assign(e1, e2stepped))

      /* Everything else is a stuck error. */
      case _ => throw StuckError(e)
    }
  }

  /*** Extra Credit: Lowering: Remove Interface Declarations ***/

  def lower(e: Expr): Expr =
    /* Do nothing by default. Change to attempt extra credit. */
    e

  /*** External Interfaces ***/

  //this.debug = true // comment this out or set to false if you don't want print debugging information
  this.maxSteps = Some(1000) // comment this out or set to None to not bound the number of steps.
  this.keepGoing = true // comment this out if you want to stop at first exception when processing a file
}
