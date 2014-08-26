package react.rewriting

import java.nio.ByteBuffer

trait Types {
  self: ExplorableMacros =>
  import c.universe._

  import definitions._

  trait TypeIO {
    def is(t: Type): Boolean
    val length: Int
    def read(in: c.Expr[ByteBuffer], setter: Tree): Tree
    def write(out: c.Expr[ByteBuffer], getter: Tree): Tree
    def havoc(setter: Tree): Tree
  }

  val supportedType = List[TypeIO](
    new TypeIO {
      def is(t: Type) = t =:= UnitTpe
      val length: Int = 0
      def read(in: c.Expr[ByteBuffer], setter: Tree): Tree = q"()"
      def write(out: c.Expr[ByteBuffer], getter: Tree): Tree = q"()"
      def havoc(setter: Tree): Tree = q"()"
    },
    new TypeIO {
      def is(t: Type) = t =:= BooleanTpe
      val length: Int = 1
      def read(in: c.Expr[ByteBuffer], setter: Tree): Tree = q"$setter($in.getBoolean)"
      def write(out: c.Expr[ByteBuffer], getter: Tree): Tree = q"$out.putBoolean($getter)"
      def havoc(setter: Tree): Tree = q"$setter(scala.util.Random.nextBoolean())"
    },
    new TypeIO {
      def is(t: Type) = t =:= ByteTpe
      val length: Int = 1
      def read(in: c.Expr[ByteBuffer], setter: Tree): Tree = q"$setter($in.getByte)"
      def write(out: c.Expr[ByteBuffer], getter: Tree): Tree = q"$out.putByte($getter)"
      def havoc(setter: Tree): Tree = q"$setter(scala.util.Random.nextByte())"
    },
    new TypeIO {
      def is(t: Type) = t =:= CharTpe
      val length: Int = 2
      def read(in: c.Expr[ByteBuffer], setter: Tree): Tree = q"$setter($in.getChar)"
      def write(out: c.Expr[ByteBuffer], getter: Tree): Tree = q"$out.putChar($getter)"
      def havoc(setter: Tree): Tree = q"$setter(scala.util.Random.nextChar())"
    },
    new TypeIO {
      def is(t: Type) = t =:= ShortTpe
      val length: Int = 2
      def read(in: c.Expr[ByteBuffer], setter: Tree): Tree = q"$setter($in.getShort)"
      def write(out: c.Expr[ByteBuffer], getter: Tree): Tree = q"$out.putShort($getter)"
      def havoc(setter: Tree): Tree = q"$setter(scala.util.Random.nextShort())"
    },
    new TypeIO {
      def is(t: Type) = t =:= IntTpe
      val length: Int = 4
      def read(in: c.Expr[ByteBuffer], setter: Tree): Tree = q"$setter($in.getInt)"
      def write(out: c.Expr[ByteBuffer], getter: Tree): Tree = q"$out.putInt($getter)"
      def havoc(setter: Tree): Tree = q"$setter(scala.util.Random.nextInt())"
    },
    new TypeIO {
      def is(t: Type) = t =:= FloatTpe
      val length: Int = 4
      def read(in: c.Expr[ByteBuffer], setter: Tree): Tree = q"$setter($in.getFloat)"
      def write(out: c.Expr[ByteBuffer], getter: Tree): Tree = q"$out.putFloat($getter)"
      def havoc(setter: Tree): Tree = q"$setter(scala.util.Random.nextFloat())"
    },
    new TypeIO {
      def is(t: Type) = t =:= LongTpe
      val length: Int = 8
      def read(in: c.Expr[ByteBuffer], setter: Tree): Tree = q"$setter($in.getLong)"
      def write(out: c.Expr[ByteBuffer], getter: Tree): Tree = q"$out.putLong($getter)"
      def havoc(setter: Tree): Tree = q"$setter(scala.util.Random.nextLong())"
    },
    new TypeIO {
      def is(t: Type) = t =:= DoubleTpe
      val length: Int = 8
      def read(in: c.Expr[ByteBuffer], setter: Tree): Tree = q"$setter($in.getDouble)"
      def write(out: c.Expr[ByteBuffer], getter: Tree): Tree = q"$out.putDouble($getter)"
      def havoc(setter: Tree): Tree = q"$setter(scala.util.Random.nextDouble())"
    },
    new TypeIO {
      def is(t: Type) = t match {
        case TypeRef(ThisType(_), sym, List()) => sym.toString == "scala.Symbol"
        case _ => false
      }
      val length: Int = 1
      def read(in: c.Expr[ByteBuffer], setter: Tree): Tree = q"$setter(react.verification.SymbolCache.value($in.getByte))"
      def write(out: c.Expr[ByteBuffer], getter: Tree): Tree = q"$out.putByte(react.verification.SymbolCache.idx($getter))"
      def havoc(setter: Tree): Tree = q"$setter(react.verification.SymbolCache.returnSome)"
    },
    new TypeIO {
      def is(t: Type) = t.toString == "react.robot.Orientation"
      val length: Int = 1
      def read(in: c.Expr[ByteBuffer], setter: Tree): Tree = q"$setter(react.verification.OrientationCache.value($in.getByte))"
      def write(out: c.Expr[ByteBuffer], getter: Tree): Tree = q"$out.putByte(react.verification.OrientationCache.idx($getter))"
      def havoc(setter: Tree): Tree = q"$setter(react.verification.OrientationCache.returnSome)"
    }
    //TODO extends to more type (MetaCache ?)
  )
  
  
  /** check whether this is a supported type */
  protected def isSupported(m: TermSymbol) = {
    val t = m.typeSignature
    supportedType.exists(_.is(t))
  }

  protected def length(t: Type): Int =
    supportedType.find(_.is(t)).get.length
  protected def length(s: TermSymbol): Int =
    length(s.typeSignature)
  
  protected def write(t: Type, out: c.Expr[ByteBuffer], getter: Tree): Tree =
    supportedType.find(_.is(t)).get.write(out, getter)
  protected def write(s: TermSymbol, out: c.Expr[ByteBuffer], getter: Tree): Tree =
    write(s.typeSignature, out, getter)

  protected def read(t: Type, in: c.Expr[ByteBuffer], setter: Tree): Tree =
    supportedType.find(_.is(t)).get.read(in, setter)
  protected def read(s: TermSymbol, in: c.Expr[ByteBuffer], setter: Tree): Tree =
    read(s.typeSignature, in, setter)

  protected def havoc(t: Type, setter: Tree): Tree =
    supportedType.find(_.is(t)).get.havoc(setter)
  protected def havoc(s: TermSymbol, setter: Tree): Tree =
    havoc(s.typeSignature, setter)


}
