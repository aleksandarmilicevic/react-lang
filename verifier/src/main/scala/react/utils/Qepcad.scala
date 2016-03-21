package react.utils

import dzufferey.smtlib._
import dzufferey.utils._
import dzufferey.utils.LogLevel._

object Qepcad {

  val defaultMemory = 100000000l
  val defaultPrime = 2000
  val defaultTimeout = 120 * 1000 // 2 min
  
  def sanitize(str: String): String = str.replaceAll("_", "").replaceAll("\\.", "")
  
  def sanitizeNames(rename: Map[Variable, Variable], f: Formula): Formula = f match {
    case l @ Literal(_) => l
    case v @ Variable(_) => rename(v)
    case a @ Application(fct, args) => Application(fct, args.map(sanitizeNames(rename, _))).setType(a.tpe)
    case b @ Binding(bt, vs, f) =>
      val vs2 = vs.map(rename)
      val f2 = sanitizeNames(rename, f)
      Binding(bt, vs2, f2).setType(b.tpe)
  }

  def sanitization(vars: Iterable[Variable]): Map[Variable, Variable] = {
    vars.foldLeft(Map[Variable,Variable]())( (acc, v) => acc + (v -> Variable(sanitize(v.name)).setType(v.tpe)) )
  }

  def reverse(map: Map[Variable, Variable]): Map[Variable, Variable] = {
    map.foldLeft(Map[Variable,Variable]())( (acc, v) => {
      assert(!acc.contains(v._2), "variables clash")
      acc + (v._2 -> v._1)
    })
  }

  def query(freeVariables: List[Variable],
            toEliminate: List[Variable],
            formula: Formula,
            assumption: Option[Formula]) = {
    Query(freeVariables, toEliminate, formula, assumption)
  }


  case class Query(freeVariables: List[Variable],
                   toEliminate: List[Variable],
                   formula: Formula,
                   assumption: Option[Formula]) {

    //keep a map that we can use for renaming variables after parsing
    val renaming = sanitization(freeVariables ++ toEliminate)
    val derenaming = reverse(renaming)

    val assumption2 = assumption.map(sanitizeNames(renaming, _))
    val formula2 = sanitizeNames(renaming, Exists(toEliminate, formula))
    val allVars = (freeVariables ++ toEliminate).map(renaming)

    def variables = allVars.mkString("(", ", ", ")")
    def nbrFV = freeVariables.length
    def printFormula = QepcadPrinter.printFormula(formula2) + "."
    def printAssume = assumption2.map(a => "assume " + QepcadPrinter.printFormula(a) )
  
    def print {
      println("variables:")
      println(variables)
      println("free variables:")
      println(nbrFV.toString)
      println("formula:")
      println(printFormula)
      println("assumption:")
      printAssume.foreach(println)
    }

    def execute(mcCallum: Boolean = true,
                memory: Long = defaultMemory,
                primeList: Int = defaultPrime,
                timeout: Long = defaultTimeout): Formula = {
      try {
        import java.io._
        val solver = java.lang.Runtime.getRuntime.exec(Array("qepcad", "-noecho", "+N"+memory, "+L"+primeList), null, null)
        val output = new BufferedWriter(new OutputStreamWriter(solver.getOutputStream()))
        val input = new BufferedReader(new InputStreamReader(solver.getInputStream()))
        val error = new BufferedReader(new InputStreamReader(solver.getErrorStream()))
       
        val txt1 = "Enter an informal description  between '[' and ']':"
        val txt2 = "Enter a variable list:"
        val txt3 = "Enter the number of free variables:"
        val txt4 = "Enter a prenex formula:"
        val txt5 = "Before Normalization >"
        val success = "An equivalent quantifier-free formula:"
        
        val to = System.currentTimeMillis + timeout
        
        def readUntil(txt: String) {
          while (!input.ready && System.currentTimeMillis <= to) {
            Thread.sleep(10)
          }
          if (System.currentTimeMillis > to) {
            solver.destroy
            sys.error("Timeout")
          } else {
            val line = input.readLine.trim
            Logger("Qepcad <- ", Info, line)
            if (line != txt) readUntil(txt)
          }
        }
       
        def write(txt: String) {
          Logger("Qepcad -> ", Info, txt)
          output.write(txt)
          output.newLine()
          output.flush()
        }
        
       
        readUntil(txt1)
        write("[ react robot model simplification ]")
       
        readUntil(txt2)
        write(variables)
        
        readUntil(txt3)
        write(nbrFV.toString)
       
        readUntil(txt4)
        write(printFormula)
       
        readUntil(txt5)
        val a = printAssume
        if (a.isDefined) {
          write(a.get)
          readUntil(txt5)
        }
        
        if (!mcCallum && nbrFV > 3) {
          write("go")
          write("proj-op (m,m," + (3 until (nbrFV+toEliminate.size)).map(_ => "h").mkString(",")  + ")")
        }
       
        write("finish")
       
        readUntil(success)
        var line = input.readLine.trim
        while (line == "") {
          line = input.readLine.trim
        }
       
        solver.waitFor
       
        val formula = QepcadParser(line)

        //dumpQuery(Some(formula))

        sanitizeNames(derenaming, formula)
      } catch {
        case t: Throwable =>
          //dumpQuery(None)
          throw t
      }
    }

    def dumpQuery(result: Option[Formula]) = {
      if (!toEliminate.isEmpty) {
        val fname = Namer("eliminationQuery") + ".txt"
        val query =
          "free variables:\n" + freeVariables.map(renaming).mkString(", ") + "\n" +
          "variables to eliminate:\n  " + toEliminate.map(renaming).mkString(", ") + "\n" +
          "formula:\n  " + printFormula + "\n" +
          printAssume.mkString("assumption:\n  ", "\n  ", "\n") +
          "result:\n  " + result.map(QepcadPrinter.printFormula).getOrElse("???") + "\n"
        IO.writeInFile(fname, query)
      }
    }

  }

  def isSupported(f: Formula): Boolean = {
    try {
      QepcadPrinter.printFormula(f)(Info)
      true
    } catch { case _: Throwable =>
      false
    }
  }


}

import scala.util.parsing.combinator.syntactical.StandardTokenParsers

object QepcadParser extends StandardTokenParsers {

  import scala.util.parsing.combinator._
  import scala.util.parsing.combinator.lexical._
  import scala.util.parsing.combinator.token._
  import scala.util.parsing.combinator.syntactical._

  lexical.delimiters += (
    "(", ")", "[", "]", "/\\", "\\/",
    "=", "/=", "<", ">", ">=", "<=",
    "+", "-", "*", "/", "^"
  )

  lexical.reserved += (
    "TRUE", "FALSE"
  )

  def lit(l: Long) = Literal(l).setType(Real)

  // num ::= [-] numericLit [/ numericLit]
  def coeff: Parser[Formula] = (
    opt("-") ~ numericLit ~ opt("/" ~> numericLit) ^^ { case sign ~ num ~ denom =>
        val n = if (sign.isDefined) -(num.toLong) else num.toLong
        denom match {
          case Some(d) => Divides(lit(n), lit(d.toLong)).setType(Real)
          case None => lit(n)
        }
      }
  )
  
  def posCoeff: Parser[Formula] = (
    numericLit ~ opt("/" ~> numericLit) ^^ { case num ~ denom =>
      denom match {
        case Some(d) => Divides(lit(num.toLong), lit(d.toLong)).setType(Real)
        case None => lit(num.toLong)
      }
    }
  )

  // variable ::= ident [^ num]
  def variable: Parser[Formula] = (
    ident ~ opt("^" ~> coeff) ^^ { case name ~ exponent => 
        val v = Variable(name).setType(Real)
        exponent match {
         case Some(e) => DRealDecl.pow(v, e)
         case None => v
        }
      }
  )

  //monome ::= rep1( num | variable )
  def monomial: Parser[Formula] =
    rep1( posCoeff | variable ) ^^ ( lst =>
      if (lst.size > 1) Times(lst:_*) else lst.head
    )


  def inverse(a: Formula): Formula = a match {
    case Times(Literal(l: Double), tail @ _*) => Times(Literal(-l) +: tail:_*)
    case Times(Literal(l: Long), tail @ _*) => Times(Literal(-l).setType(Real) +: tail:_*)
    case Literal(l: Double) => Literal(-l)
    case Literal(l: Long) => Literal(-l).setType(Real)
    case Times(lst @ _*)=> Times(Literal(-1.0) +: lst :_*)
    case other => Times(Literal(-1.0), other)
  }

  //polynome ::= monome + polynome
  //           | monome - polynome
  //           | monome
  def polynomial: Parser[Formula] = (
    "+" ~> monomial ~ polynomial ^^ { case m ~ Plus(lst @ _*) => Plus(m +: lst:_*) }
  | "-" ~> monomial ~ polynomial ^^ { case m ~ Plus(lst @ _*) => Plus(inverse(m) +: lst:_*) }
  | monomial ~ polynomial ^^ { case m ~ Plus(lst @ _*) => Plus(m +: lst:_*) }
  | success(Plus())
  )

  // equation ::= polynome (= | > | >= | < | <=) num
  def equation: Parser[Formula] = (
    polynomial ~ ("=" ~> coeff) ^^ { case p ~ c => Eq(p, c) }
  | polynomial ~ ("/=" ~> coeff) ^^ { case p ~ c => Not(Eq(p, c)) }
  | polynomial ~ (">" ~> coeff) ^^ { case p ~ c => Gt(p, c) }
  | polynomial ~ ("<" ~> coeff) ^^ { case p ~ c => Lt(p, c) }
  | polynomial ~ (">=" ~> coeff) ^^ { case p ~ c => Geq(p, c) }
  | polynomial ~ ("<=" ~> coeff) ^^ { case p ~ c => Leq(p, c) }
  )

  //system ::= equation
  //         | '[' repsep1(equation, /\) ']'
  //         | '[' repsep1(equation, \/) ']'
  def system: Parser[Formula] = (
    "TRUE" ^^^ True()
  | "FALSE" ^^^ False()
  | "[" ~> system <~ "]"
  | rep1sep(equation, "/\\") ^^ ( lst => if (lst.size == 1) lst.head else And(lst:_*) )
  | rep1sep(equation, "\\/") ^^ ( lst => Or(lst:_*) )
  )

  def apply(str: String): Formula = {
    Logger("QepcadParser", Info, "parsing: " + str)
    val tokens = new lexical.Scanner(str)
    val result = phrase(system)(tokens)
    if (result.successful) {
      val res =result.get
      Logger("QepcadParser", Info, "parsed: " + res)
      res
    } else {
      Logger.logAndThrow("QepcadParser", LogLevel.Error, "parsing error: " + result.toString)
    }
  }
  
}



object QepcadPrinter {

  def printMonomial(f: Formula, first: Boolean)(implicit level: Level = Error): String = {
    val elts = f match {
      case Times(lst @ _*) => lst
      case other => List(other)
    }
    val (lst2, sign) = Misc.mapFold(elts.toList, true, (e: Formula, acc: Boolean) =>  e match {
      case Divides(Literal(l: Long), l2 @ Literal(_)) if l < 0l => (Divides(Literal(-l),l2), !acc)
      case Literal(l: Long) if l < 0l => (Literal(-l), !acc)
      case Literal(d: Double) if d < 0.0 => (Literal(-d), !acc)
      case other => (other, acc)
    })
    val prefix = if (!sign) " - " else if (first) " " else " + "
    prefix + lst2.map(printFormula).mkString(" ")
  }

  def printPolynomial(f: Formula)(implicit level: Level = Error): String = f match {
    case Plus(lst @ _*) =>
      val hd = printMonomial(lst.head, true)
      val tl = lst.tail.map( printMonomial(_, false) ).mkString(" ")
      hd + tl
    case other => printMonomial(other, true)
  }


  def printFormula(formula: Formula)(implicit level: Level = Error): String = formula match {
    case Variable(name) => name
    case Literal(true) => "TRUE"
    case Literal(false) => "FALSE"
    case Literal(i: Int) => i.toString
    case Literal(l: Long) => l.toString
    case Literal(d: Double) if d.isWhole => d.toLong.toString //TODO check for overflow
    case ForAll(vs, f) => vs.map(v => "(A "+v+")").mkString + "[ " + printFormula(f) + " ]"
    case Exists(vs, f) => vs.map(v => "(E "+v+")").mkString + "[ " + printFormula(f) + " ]"
    case And(lst @ _*) => lst.map(printFormula).mkString("[ ", " /\\ ", " ]")
    case Or(lst @ _*) =>  lst.map(printFormula).mkString("[ ", " \\/ ", " ]")
    case Eq(a, b) => printFormula(a) + " = " + printFormula(b)
    case Not(Eq(a, b)) => printFormula(a) + " /= " + printFormula(b)
    case Leq(a, b) => printFormula(a) + " <= " + printFormula(b)
    case Geq(a, b) => printFormula(a) + " >= " + printFormula(b)
    case Lt(a, b) => printFormula(a) + " < " + printFormula(b)
    case Gt(a, b) => printFormula(a) + " > " + printFormula(b)
    case Plus(_*) => printPolynomial(formula)
    case Times(_*) => printMonomial(formula, true)
    case Minus(a, b) => "(" + printFormula(a) + " - " + printFormula(b) + ")" 
    case Divides(a @ Literal(_), b @ Literal(_)) => printFormula(a)+"/"+printFormula(b)
    case Divides(a, b @ Literal(_)) =>  "(" + printFormula(a) + " 1/" + printFormula(b) + ")" 
    case Application(DRealDecl.pow, List(a @ Variable(_), b @ Literal(_))) => "(" + printFormula(a) + "^" + printFormula(b) + ")"
    case other => Logger.logAndThrow("qepcad", level, "not supported: " + other)
  }
  
}
