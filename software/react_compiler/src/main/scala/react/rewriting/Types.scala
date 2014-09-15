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
      def read(in: c.Expr[ByteBuffer], setter: Tree): Tree = q"$setter($in.get != 0)"
      def write(out: c.Expr[ByteBuffer], getter: Tree): Tree = q"if ($getter) $out.put(1: Byte) else $out.put(0: Byte)"
      def havoc(setter: Tree): Tree = q"$setter(scala.util.Random.nextBoolean())"
    },
    new TypeIO {
      def is(t: Type) = t =:= ByteTpe
      val length: Int = 1
      def read(in: c.Expr[ByteBuffer], setter: Tree): Tree = q"$setter($in.get)"
      def write(out: c.Expr[ByteBuffer], getter: Tree): Tree = q"$out.put($getter)"
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
  )

  protected def cacheId(m: TermSymbol): TermName = TermName(m.name.toString + "__cache")
  
  /** check whether this is a supported type */
  protected def isSupported(m: TermSymbol) = true

  /** check whether this is a natively supported type (or if it needs a cache) */
  protected def isNative(m: TermSymbol) = {
    val t = m.typeSignature
    supportedType.exists(_.is(t))
  }

  protected def length(t: Type): Int =
    supportedType.find(_.is(t)).map(_.length).getOrElse(4)
  protected def length(s: TermSymbol): Int =
    length(s.typeSignature)
  
  protected def write(s: TermSymbol, out: c.Expr[ByteBuffer], getter: Tree): Tree = {
    val t = s.typeSignature
    supportedType.find(_.is(t)) match {
      case Some(tio) => tio.write(out, getter)
      case None => 
        val cid = cacheId(s)
        q"$out.putInt($cid.idx($getter))"
    }
  }

  protected def read(s: TermSymbol, in: c.Expr[ByteBuffer], setter: Tree): Tree = {
    val t = s.typeSignature
    supportedType.find(_.is(t)) match {
      case Some(tio) => tio.read(in, setter)
      case None =>
        val cid = cacheId(s)
        q"$setter($cid.value($in.getInt))"
    }
  }

  protected def havoc(t: Type, setter: Tree): Tree =
    supportedType.find(_.is(t)).get.havoc(setter)
  protected def havoc(s: TermSymbol, setter: Tree): Tree =
    havoc(s.typeSignature, setter)


}
