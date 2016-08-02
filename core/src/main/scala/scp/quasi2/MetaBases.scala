package scp
package quasi2

import utils._
import lang2._
import scala.reflect.runtime.{universe => sru}

trait MetaBases {
  val u: scala.reflect.api.Universe
  import u._
  
  def freshName(hint: String): TermName
  
  /** Base that generates the Scala code necessary to construct the DSL program.
    * This class let-binds type symbols, method symbols, type applications and modules: the corresponding definitions
    * are stored in the `symbols` mutable buffer.
    * Note: these are let-bound but not cached – caching is assumed to be performed by the caller (eg: ModularEmbedding). */
  class MirrorBase(Base: Tree) extends Base {
    import scala.collection.mutable
    
    /** For legibility of the gen'd code, calls `mapp` instead of `methodApp`.
      * Note: maybe it will make the gen'd pgrm slower (cf: more Seq/List ction?) */
    val shortAppSyntax = true
    //val shortAppSyntax = false
    
    val symbols = mutable.Buffer[(TermName, Tree)]()
    def mkSymbolDefs = symbols map { case(n, t) => q"val $n = $t" }
    
    
    type Rep = Tree
    type BoundVal = (TermName, TypeRep)
    type TypeRep = Tree
    
    type MtdSymbol = Tree
    type TypSymbol = Tree
    
    
    def loadTypSymbol(fullName: String): TypSymbol = {
      val n = freshName(fullName.drop(fullName.lastIndexOf('.')+1))
      val s = q"$Base.loadTypSymbol($fullName)"
      symbols += n -> s
      q"$n"
    }
    
    def loadMtdSymbol(typ: TypSymbol, symName: String, index: Option[Int], static: Boolean = false): MtdSymbol = {
      val n = freshName(symName)
      val s = q"$Base.loadMtdSymbol($typ, $symName, $index, $static)"
      symbols += n -> s
      q"$n"
    }
    
    def bindVal(name: String, typ: TypeRep): BoundVal =
      TermName(name) -> typ // We assume it's find without a fresh name, since the binding structure should reflect that of the encoded program
    def readVal(v: BoundVal): Rep = q"$Base.readVal(${v._1})"
    
    def const[A: sru.TypeTag](value: A): Rep = q"$Base.const[${typeOf[A]}](${Literal(Constant(value))})"
    
    def lambda(params: List[BoundVal], body: => Rep): Rep = q"""
      ..${params map { case (vn, vt) => q"val $vn = $Base.bindVal(${vn.toString}, $vt)" }}
      $Base.lambda(${mkNeatList(params map (_._1) map Ident.apply)}, $body)
    """
    
    def newObject(tp: TypeRep): Rep = q"$Base.newObject($tp)"
    
    def moduleObject(fullName: String, isPackage: Boolean): Rep = {
      val n = freshName(fullName.drop(fullName.lastIndexOf('.')+1))
      symbols += n -> q"$Base.moduleObject($fullName, $isPackage)"
      q"$n"
    }
    
    def methodApp(self: Rep, mtd: MtdSymbol, targs: List[TypeRep], argss: List[ArgList], tp: TypeRep): Rep = {
      val as = {
        def quote(argl: ArgList): Tree = argl match {
          case Args(reps @ _*) => q"$Base.Args(..$reps)"
          case ArgsVarargs(args, varargs) => q"$Base.ArgsVarargs(${quote(args)}, ${quote(varargs)})"
          case ArgsVarargSpliced(args, vararg) => q"$Base.ArgsVarargSpliced(${quote(args)}, $vararg)"
        }
        argss map quote
      }
      if (shortAppSyntax) q"$Base.mapp($self, $mtd, $tp)(..$targs)(..$as)"
      else q"$Base.methodApp($self, $mtd, ${mkNeatList(targs)}, ${mkNeatList(as)}, $tp)"
    }
    
    def byName(arg: => Rep) = q"$Base.byName($arg)"
    
    def uninterpretedType[A: sru.TypeTag]: TypeRep = q"$Base.uninterpretedType[${typeOf[A]}]"
    
    def typeApp(self: Rep, typ: TypSymbol, targs: List[TypeRep]): TypeRep = {
      val n = freshName("")
      symbols += n -> q"$Base.typeApp($self, $typ, ${mkNeatList(targs)})"
      q"$n"
    }
    
    def recordType(fields: List[(String, TypeRep)]): TypeRep = ???
    
    def repType(r: Rep): TypeRep = ??? // FIXME remove capability
    
    
    def mkNeatList(xs: Seq[Tree]) = xs match {
      case Seq() => q"scala.Nil"
      case _ => q"scala.List(..$xs)"
    }
    
    
  }
  
  
  
  
  
  /** Base that simply outputs the Scala tree representation of the DSL program.
    * It does not add types to the trees, although it could (to some extent). */
  class ScalaReflectionBase extends Base {
    
    type Rep = Tree
    type BoundVal = (TermName, TypeRep)
    type TypeRep = Tree
    
    type MtdSymbol = TermName
    type TypSymbol = () => TypeName // to delay computation, since we won't need most of them! (only needed in typeApp)
    
    def loadTypSymbol(fullName: String): TypSymbol = () =>
      TypeName(ir2.RuntimeSymbols.loadTypSymbol(fullName).name.toString)
    
    def loadMtdSymbol(typ: TypSymbol, symName: String, index: Option[Int], static: Boolean = false): MtdSymbol =
      TermName(symName)
    
    def bindVal(name: String, typ: TypeRep): BoundVal = TermName(name) -> typ
    def readVal(v: BoundVal): Rep = q"${v._1}"
    def const[A: sru.TypeTag](value: A): Rep = Literal(Constant(value))
    def lambda(params: List[BoundVal], body: => Rep): Rep = q"""
      (..${params map { case (vn, vt) => q"val $vn: $vt" }}) => $body
    """
    
    def newObject(tp: TypeRep): Rep = q"new $tp"
    def moduleObject(fullName: String, isPackage: Boolean): Rep = {
      val path = fullName.splitSane('.').toList
      path.tail.foldLeft(q"${TermName(path.head)}":Tree)((acc,n) => q"$acc.${TermName(n)}")
    }
    def methodApp(self: Rep, mtd: MtdSymbol, targs: List[TypeRep], argss: List[ArgList], tp: TypeRep): Rep =
      q"$self.${mtd}[..$targs](...${
        def quote(argl: ArgList): Seq[Tree] = argl match {
          case Args(reps @ _*) => reps
          case ArgsVarargs(args, varargs) => quote(args) ++ quote(varargs)
          case ArgsVarargSpliced(args, vararg) => quote(args) :+ q"$vararg: _*"
        }
        argss map quote})" // Note: could also call  internal.setType ...?
    
    def byName(arg: => Rep): Rep = arg
    
    def uninterpretedType[A: sru.TypeTag]: TypeRep = tq"${typeOf[A]}"
    
    def typeApp(self: TypeRep, typ: TypSymbol, targs: List[TypeRep]): TypeRep = tq"$self.${typ()}[..$targs]"
    
    
    def recordType(fields: List[(String, TypeRep)]): TypeRep = ??? // TODO
    
    def repType(r: Rep): TypeRep = ??? // TODO impl (store types using internal)
    
    
    
  
  }
}










