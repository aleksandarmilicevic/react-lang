package react.rewriting

import react.verification.{Playground, Stateful}

import scala.language.experimental.macros
//import scala.reflect.macros.whitebox.Context
import scala.reflect.macros.blackbox.Context

import java.nio.ByteBuffer


class ExplorableMacros(val c: Context) extends Types
                                       with Fields
{

  import c.universe._

  def wordLength[T: c.WeakTypeTag]: c.Expr[Int] = {
    assert(unsupportedFields.isEmpty, "unsupported fields: " + unsupportedFields)
    val size = permanentFields.map(length).foldLeft(0)( _ + _ )
    c.Expr[Int](q"$size")
  }

  def fieldsSaved[T: c.WeakTypeTag]: c.Expr[String] = {
    val tpe = weakTypeOf[T].toString
    val getters = permanentFields.map(fieldGetter).map(_.name).mkString(", ")
    c.Expr[String](Literal(Constant(tpe + ": " + getters)))
  }
  
  def longDescr[T: c.WeakTypeTag]: c.Expr[String] = {
    val tpe = weakTypeOf[T].toString
    val getters = permanentFields.map(fieldGetter)//.map(_.name).mkString(", ")
    val gettersStr = getters.map(g => {
      val gs = g.name.toString
      q"""$gs + " = " + $g.toString + ", " """
    })
    val t = gettersStr.foldLeft(q""" $tpe + ": " """ )( (acc, t) => q"$acc + $t" )
    c.Expr[String](t)
  }

  def makeStateful[M: c.WeakTypeTag](robot: c.Expr[M], world: c.Expr[Playground]): c.Expr[Stateful] = {

    val caches = for (f <- permanentFields if !isNative(f) ) yield {
      val cid = cacheId(f)
      val t = getCache(f.typeSignature)
      q"val $cid = $t"
    }

    var length = wordLength[M]
    
    val storing = for (f <- permanentFields) yield {
      val getter = fieldGetter(f)
      write(f, c.Expr[ByteBuffer](Ident(TermName("out"))), getter)
    }

    val restored = for (f <- permanentFields) yield {
      val setter = fieldSetter(f)
      read(f, c.Expr[ByteBuffer](Ident(TermName("in"))), setter)
    }

    val havoced = for (f <- transientFields) yield {
      val setter = fieldSetter(f)
      havoc(f, setter)
    }

    val rndFld = permanentFields.filter(f => f.typeSignature =:= definitions.DoubleTpe ||
                                             f.typeSignature =:= definitions.FloatTpe)
    val rnd = rndFld.size

    //Console.err.println("roundable fields are: " + rndFld)
    
    val rounding = for (f <- rndFld) yield {
      def rc(f: TermSymbol) = {
        val set = fieldSetter(f)
        val get = fieldGetter(f)
        val name = get.name.toString
        val min = Select(world.tree, TermName(name+"Min"))
        val max = Select(world.tree, TermName(name+"Max"))
        val step = Select(world.tree, TermName(name+"Discretization"))
        val t1 = q"$set(react.verification.Stateful.round($get, $min, $max, $step))"
        //c.echo(world.tree.pos, "rounding " + name + " in " + f.owner + " with " + t1)
        t1
      }
      def r(f: TermSymbol) = {
        val set = fieldSetter(f)
        val get = fieldGetter(f)
        val name = get.name.toString
        val step = Select(world.tree, TermName("fpDiscretization"))
        val t1 = q"react.verification.Stateful.round($get, Double.MinValue, Double.MaxValue, $step)"
        //c.echo(world.tree.pos, "rounding " + name + " in " + f.owner + " with " + t1)
        if (f.typeSignature =:= definitions.FloatTpe) {
          q"$set($t1.toFloat)"
        } else {
          q"$set($t1)"
        }
      }
      fieldGetter(f).name.toString match {
        case "x" | "y" => rc(f)
        case _ => r(f)
      }
    }

    val rt = robot.tree

    val concretizing = {
      //get the range for each value and 
      val intervals = rndFld.map( f => {
        val get = fieldGetter(f)
        val name = get.name.toString
        val vinterval = TermName(name+"_interval")
        name match {
          case "x" | "y" => 
            val min = Select(world.tree, TermName(name+"Min"))
            val max = Select(world.tree, TermName(name+"Max"))
            val step = Select(world.tree, TermName(name+"Discretization"))
                  //println("concretizing " + $name + " at " + tmp + " in " + $rt)
            q"""private val $vinterval = {
                  val tmp = $get
                  Array( tmp,
                    react.verification.Stateful.lower(tmp, $min, $max, $step),
                    react.verification.Stateful.upper(tmp, $min, $max, $step))
                }"""
          case  _  => 
            val step = Select(world.tree, TermName("fpDiscretization"))
            if (f.typeSignature =:= definitions.FloatTpe) {
                    //println("concretizing " + $name + " at " + tmp + " in " + $rt)
              q"""private val $vinterval = {
                    val tmp = $get
                    Array( tmp.toFloat,
                      react.verification.Stateful.lower(tmp, Double.MinValue, Double.MaxValue, $step).toFloat,
                      react.verification.Stateful.upper(tmp, Double.MinValue, Double.MaxValue, $step).toFloat)
                  }"""
            } else {
                    //println("concretizing " + $name + " at " + tmp + " in " + $rt)
              q"""private val $vinterval = {
                    val tmp = $get
                    Array( tmp,
                      react.verification.Stateful.lower(tmp, Double.MinValue, Double.MaxValue, $step),
                      react.verification.Stateful.upper(tmp, Double.MinValue, Double.MaxValue, $step))
                  }"""
            }
        } 
      })
      val restoring = rndFld.flatMap( f => {
        val set = fieldSetter(f)
        val name = fieldGetter(f).name.toString
        val vinterval = TermName(name+"_interval")
        List(
          q"$set( $vinterval(tmp % 3) )",
          q"tmp = tmp / 3"
        )
      })
      q"""
      new StateModifierIterator {
        private var idx = 0
        private val max = math.pow(3, $rnd).toInt
        ..$intervals
        def hasNext = {
          idx < max
        }
        def next {
          var tmp = idx
          ..$restoring
          idx += 1
        }
        def reset {
          idx = 0
        }
      }
      """
    }
    
    val wa = permanentFields forall worldAgnostic
    val descr = {
      val l = fieldsSaved[M]
      if (wa) {
        l
      } else {
        val internalized = permanentFields filterNot worldAgnostic
        val iStr = Literal(Constant(internalized.map(_.name).mkString(", ")))
        c.Expr[String](q"""$l + " [internalized: " + $iStr + "]" """)
      }
    }
    val lDescr = longDescr[M]

    val tree = q"""
    new Stateful {
      import Stateful._
      val robot = $robot
      ..$caches
      def length: Int = $length
      def worldAgnostic: Boolean = $wa
      def nbrOfRoundedVars: Int = $rnd
      def round: Unit = {
        ..$rounding
      }
      def concretize: StateModifierIterator = $concretizing
      def serialize(out: java.nio.ByteBuffer): Unit = {
        ..$storing
      }
      def deserilize(in: java.nio.ByteBuffer): Unit = {
        ..$restored
        ..$havoced
      }
      def description: String = $descr
      def longDescription: String = $lDescr
    }
    """
    c.Expr[Stateful](tree)
  }

}
