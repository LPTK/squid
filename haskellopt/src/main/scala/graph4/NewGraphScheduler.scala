package graph4

import scala.collection.mutable
import squid.utils._
import squid.utils.CollectionUtils.MutSetHelper

abstract class NewGraphScheduler { self: GraphIR =>
  
  import ScheduleDebug.{debug=>Sdebug}
  
  class NewScheduler(val mod: GraphModule) extends Scheduler { scheduler =>
    
    val useDummyCids = false
    //val useDummyCids = true
    
    object ScopeAccessMethod extends Enumeration {
      val Case, Selector, Lens = Value
    }
    val scopeAccessMethod: ScopeAccessMethod.Value =
      ScopeAccessMethod.Selector // note that sometimes, I've seen this create type inference errors (where using Case works)
      //ScopeAccessMethod.Case
      //ScopeAccessMethod.Lens
    
    var usesLenses = false
    val mayUseLenses = scopeAccessMethod === ScopeAccessMethod.Lens
    var usesTupleSelectors = false
    
    object AST extends HaskellAST(pp) {
      type Ident = Var
      def printIdent(id: Ident): String = printVar(id)
      def mkIdent(nameHint: String): Ident = bindVal(mkName(nameHint, ""))
      val dropUnusedLets = false // TODO make unused lets an error?
      val inlineOneShotLets = inlineScheduledLets
      val inlineTrivialLets = inlineScheduledLets
      //val inlineCalls = inlineScheduledLets // Causes assertion to fail in HaskellAST.Let.reorder since it may remove usages of some defs
      
      //val inlineCalls = false
      val inlineCalls = true
      
      val commonSubexprElim = false
      //val commonSubexprElim = true
      // ^ Currently cannot handle recursive defs...
      //   Also, can actually grealty increase the number of lines of a program if there are too many shared bindings to introduce...
      
      //override val mergeLets = true
      //override val mergeLets = !debugScheduling
      override val mergeLets = true
      
      val useOnlyIntLits = self.useOnlyIntLits
    }
    
    def refs = mod.modDefs.iterator.flatMap(_._2.iterator)
    
    /** Used as the keys to identify unique let-bound definitions. */
    type IRef = (Instr, Ref)
    
    implicit object ScopeOrd extends Ordering[Scope] {
      def compare(x: Scope, y: Scope) = y.cid.uid - x.cid.uid
    }
    class Scope(val cid: CallId, val payload: Instr, name: Str, lambdaBound: Opt[Var]) {
      val push = Push(cid, payload, Id)
      val ident = AST.mkIdent(name)
      val callIdent = AST.mkIdent("ret")
      
      // To be filled in during processing:
      var params = mutable.Set.empty[IRef]
      var captures = mutable.Set.empty[IRef]
      var returns = mutable.Set.empty[IRef]
      var parents = mutable.SortedSet.empty[Scope]
      var called = mutable.SortedSet.empty[Scope]
      
      val idents: mutable.Map[IRef, AST.Ident] = mutable.Map.empty
      def toIdent(iref: IRef): AST.Ident = idents.getOrElseUpdate(iref, AST.mkIdent(iref._2.name.getOrElse("t")))
      def toVari(ref: IRef): AST.Vari = {
        //Sdebug(s"<$this>! toVari ${ref} --> ${ref|>printRef}")
        AST.Vari(toIdent(ref))
      }
      def printRef(iref: IRef) = printVar(toIdent(iref))
      
      sealed abstract class Scheduled {
        def toExpr: AST.Expr
        def str(indent: Int = 0) = toExpr.stringify
        override def toString = str(0)
      }
      case class Local(iref: IRef) extends Scheduled {
        def toExpr: AST.Expr = AST.Vari(toIdent(iref))
      }
      case class Concrete(nde: ConcreteNode) extends Scheduled {
        private def subExpr(ref: Ref): AST.Expr = AST.Vari(toIdent(Id->ref))
        def toExpr: AST.Expr = nde match {
          case IntBoxing(n) => constantToExpr(IntLit(true, n))
          case App(l, r) => AST.App(subExpr(l), subExpr(r))
          case l @ Lam(_,lbody) => die // we handle lambdas specially
          case v: Var => AST.Vari(v)
          case c: ConstantNode => constantToExpr(c)
          case Case(s,as) =>
            AST.Case(subExpr(s), as.map {
              case (c,a,r) => (c, List.fill(a)(bindVal("ρ") |> AST.Vari), subExpr(r)) })
          case CtorField(s,c,a,i) => AST.CtorField(subExpr(s), c, a, i)
        }
      }
      case class Lambda(v: Var, scp: Scope) extends Scheduled {
        assert(scp.returns.size === 1)
        lazy val call = new Call(scp, scp.returns.head).toExpr
        def toExpr: AST.Expr = {
          assert(scp.returns.size === 1)
          call
        }
        override def str(indent: Int) = s"\\${v|>printVar} @ $v -> $call"
      }
      case class NonLocalBranch(flag: Var, thn: IRef, els: IRef) extends Scheduled {
        def toExpr: AST.Expr =
          AST.Case(AST.Vari(flag), ("True",Nil,thn|>toVari) :: ("False",Nil,els|>toVari) :: Nil)
      }
      case object Undefined extends Scheduled {
        def toExpr: AST.Expr =
          AST.Inline(s"undefined") // TODO make it s"error ${reason}"
      }
      class Call(val scp: Scope, val iref: IRef) extends Scheduled {
        called += scp
        private def mkSelection(scp: Scope, prefix: AST.Expr, idx: Int) = {
          if (scopeAccessMethod === ScopeAccessMethod.Lens) {
            usesLenses = true
            AST.App(AST.App(AST.Inline("(^.)"), prefix), AST.Inline(s"_${idx+1}"))
          }
          else if (scopeAccessMethod === ScopeAccessMethod.Selector) {
            usesTupleSelectors = true
            AST.App(AST.Inline(s"sel${idx+1}"), prefix)
          }
          else AST.CtorField(prefix, scp.returnsTupleCtor, scp.returns.size, idx)
        }
        def toExpr: AST.Expr = {
          val call = AST.Vari(scp.callIdent)
          scp.returns.toList match {
            case Nil => die
            case _ :: Nil => call
            case rets =>
              val idx = rets.indexOf(iref)
              assert(idx >= 0)
              mkSelection(scp, call, idx)
          }
        }
        private def orUndefined(arg: => AST.Expr): AST.Expr =
          try arg catch { case _: BadComparison => Undefined.toExpr }
        override def str(indent: Int) = s"${scp.showName}(${
            scp.params.iterator.map(scp.toArg).map(printRef).mkString(", ")
            //scp.params.iterator.map(p => orUndefined(toVari(scp.toArg(p))).stringify).mkString(", ")
          })(${
            scp.captures.iterator.map(scp.toCaptureArg).map(printRef).mkString(", ")
            //scp.captures.iterator.map(p => orUndefined(toVari(scp.toCaptureArg(p))).stringify).mkString(", ")
          })#${scp.printRef(iref)}"
      }
      def toArg(ref:IRef): IRef = {
        //assert(scheduled.contains(ref), (this,ref))
        val iref = ref._1.asInstanceOf[Drop].rest -> ref._2
        //assert(scheduled.contains(iref), (this,iref))
        //Sdebug(s"?! toArg $ref :: ${ref |> printRef} --> ${iref}")
        iref
      }
      def toCaptureArg(ref: (IRef)): IRef = {
        //assert(scheduled.contains(ref))
        val pop = ref._1.asInstanceOf[Pop]
        val iref = (payload `;` pop.rest) -> ref._2
        //assert(scheduled.contains(iref), (this,iref))
        //Sdebug(s"?! toCaptureArg $ref :: ${ref |> printRef} --> ${iref}")
        iref
      }
      
      val scheduled = mutable.Map.empty[IRef, Scheduled]
      val processed = mutable.Set.empty[IRef]
      
      def processRec(iref: IRef): Unit = processRec(iref._1, iref._2)
      
      def processRec(ictx: Instr, ref: Ref): Unit = {
        val iref = ictx->ref
        processed.setAndIfUnset(iref, Sdebug(s"A $this [$ictx] ${ref.showDef}") thenReturn ScheduleDebug.nestDbg {
          val sch: Opt[Scheduled] = ictx match {
            case Push(pcid, pay, rest) =>
              val scp = scopes.getOrElseUpdate(pcid, new Scope(pcid, pay, "call", None))
              assert(scp.payload === pay)
              scp.returns += rest -> ref
              scp.parents += this
              updateArguments(scp)
              scp.processRec(rest, ref)
              Some(new Call(scp, rest->ref))
            case d @ Drop(rest) =>
              //assert(d.originalCid === cid)
              if (d.originalCid =/= cid) Some(Undefined) else {
                params += iref
                parents.foreach(_.updateArguments(this))
                //Some(Local(rest->ref))
                None
              }
            case p @ Pop(rest) =>
              assert(p.originalVar === cid.v)
              //if (p.originalVar =/= cid.v)
              captures += iref
              parents.foreach(_.updateArguments(this))
              //Some(Local(rest->ref))
              None
            case Id =>
              Some(ref.node match {
                case Control(i, body) =>
                  processRec(i, body)
                  Local(i, body)
                case Branch(cnd, thn, els) =>
                  assert(ictx === Id)
                  checkCondition(cnd) match {
                    case Right(true) =>
                      processRec(ictx, thn)
                      Local(ictx, thn)
                    case Right(false) =>
                      processRec(ictx, els)
                      Local(ictx, els)
                    case Left(None) => Undefined
                    case Left(Some(flag)) =>
                      processRec(ictx, thn)
                      processRec(ictx, els)
                      NonLocalBranch(flag, ictx->thn, ictx->els)
                  }
                case lam: Lam =>
                  var isNew = false
                  val scp = lambdaScopes.getOrElseUpdate(ref, {
                    isNew = true
                    val newCid = if (useDummyCids) DummyCallId else new CallId(lam.param, mod.nextUnique)
                    val scp = new Scope(newCid, Id, "lam", Some(lam.param))
                    scopes += newCid -> scp
                    scp.returns += Id -> lam.body
                    scp
                  })
                  scp.parents += this
                  updateArguments(scp)
                  scp.processRec(Id, lam.body)
                  Lambda(lam.param, scp) // TODO just make a call?
                case v: Var =>
                  if (lambdaBound contains v) Concrete(v) else Undefined
                case c: ConcreteNode =>
                  c.children.foreach(processRec(Id, _))
                  Concrete(c)
              })
          }
          assert(!scheduled.contains(iref))
          sch match {
            case None =>
              Sdebug(s"Don't bind: ${iref|>printRef}")
            case Some(sch) =>
              Sdebug(s"Bind: ${iref|>printRef} = $sch")
              scheduled += iref -> sch
          }
        })
      }
      def updateArguments(calledScope: Scope): Unit = Sdebug(s"Updating arguments of $calledScope in $this") thenReturn ScheduleDebug.nestDbg {
        assert(calledScope.parents(this))
        //try {
        calledScope.params.foreach(p => processRec(calledScope.toArg(p)))
        calledScope.captures.foreach(p => processRec(calledScope.toCaptureArg(p)))
        //} catch { case bc: BadComparison => System.err.println(s"WARNING: ${bc}") }
        calledScope.flagParams.map(kv => addFlagArg(kv._2,kv._1))
      }
      
      val flagParams = mutable.ListMap.empty[Condition, Var]
      val flagArgs = mutable.ListMap.empty[Var, AST.Expr]
      
      def addFlagParam(cnd: Condition): Var = {
        val flag = flagParams.getOrElseUpdate(cnd,
          AST.mkIdent("flag_"+cnd.valuesIterator.collect{case cid:CallId=>cid.v|>printVar}.mkString("_")))
        parents.foreach(_.updateArguments(this))
        flag
      }
      val addedFlagArgs = mutable.Set.empty[Var]
      def addFlagArg(childFlag: Var, cnd: Condition): Unit = addedFlagArgs.setAndIfUnset(childFlag, {
        flagArgs.getOrElseUpdate(childFlag, checkCondition(cnd) match {
          case Right(tr) => AST.Inline(if (tr) "True" else "False")
          case Left(None) => Undefined.toExpr // ...?
          case Left(Some(newFlag)) => AST.Vari(newFlag)
        })
      })
      def checkCondition(cnd: Condition): Either[Opt[Var],Bool] = {
        Condition.test(cnd, push, hasToMakeSense = false) match { // FIXME is this `hasToMakeSense` thing really necessary?
          case Some(truth) => Right(truth)
          case None =>
            Sdebug(s"$this Don't know [$push]  ${Condition.show(cnd)} ?")
            val newCondOpt = Condition.throughControl(push, cnd)
            Sdebug(s"? --> ${newCondOpt.map(Condition.show)}")
            Left(newCondOpt.fold[Opt[Var]](None) { newCond =>
              val flag = addFlagParam(newCond)
              Some(flag)
            })
        }
      }
      def process(): Unit = {
        assert(returns.nonEmpty)
        assert(returns.size === 1)
        returns.foreach(r => processRec(r._1, r._2))
        //captures.foreach(r => processRec(r._1, r._2))
        //params.foreach(r => processRec(r._1, r._2))
      }
      def addArgument(childScope: CallId, ictx: Instr, ref: Ref): Unit = {
        processRec(ictx, ref)
      }
      
      lazy val toDefn: AST.Defn = Sdebug(s"Defn of $this") thenReturn ScheduleDebug.nestDbg {
        val flParams = flagParams.valuesIterator.map(AST.Vari).toList
        new AST.Defn(ident, flParams ++ (params.iterator ++ captures.iterator).map(toVari), Lazy {
          val body = scheduled.iterator.map { case (iref, sch) => Left(toVari(iref),sch.toExpr) }.toList
          val bodyWithCalls = called.foldRight(body) {
            case (scp, body) =>
              val flArgs = scp.flagParams.valuesIterator.map(flagArgs).toList
              val normalArgs = (
                scp.params.iterator.map(scp.toArg).map(toVari) ++
                //scp.params.iterator.map(p => orUndefined(scp.toArg(p)|>toVari) ++
                scp.captures.iterator.map(scp.toCaptureArg).map(toVari)
                //scp.captures.iterator.map(p => orUndefined(scp.toCaptureArg(p)|>toVari))
              ).toList
              val call = AST.Call(Lazy(scp.toDefn), flArgs ::: normalArgs)
              Left(AST.Vari(scp.callIdent), call) :: body
          }
          val ret = returns.iterator.toList match {
            case Nil => die
            case iref :: Nil => toVari(iref)
            case _ => returns.foldLeft[AST.Expr](AST.Inline(returnsTupleCtor)) {
              case (app, iref) => AST.App(app, toVari(iref)) }
          }
          val bodyExpr = AST.Let.reorder(bodyWithCalls, ret)
          lambdaBound.fold[AST.Expr](bodyExpr)(v => AST.Lam(AST.Vari(v), Nil, bodyExpr))
        })
      }
      def returnsTupleCtor: Str = {
        assert(returns.size > 1)
        "(" + "," * (returns.size - 1) + ")"
      }
      def show(indent: Int = 0): Str = {
        val rets = returns.map(c =>
          s"${c|>printRef} ≡ ${c._1.shortString}${c._2}").mkString(", ")
        val pre = "\n" + "  " * (indent+1)
        val capts = if (captures.isEmpty) "" else s"${pre+Console.BOLD}Captures: ${Console.RESET+captures.map(c =>
          s"${c|>printRef} ≡ ${c._1.shortString}${c._2}").mkString(", ")}"
        val aliases = ""
        val outerTests = ""
        val recInfo = ""
        val defs = scheduled.iterator.map{case(iref@(i,r),sch) => s"$pre${printRef(iref)} ≡ ${i.shortString}${r.showName} = ${sch.str(indent+1)}"}.mkString
        s"$toString (${params.map(c =>
          s"${c|>printRef} ≡ ${c._1.shortString}${c._2}").mkString(", ")}) -> ($rets) " +
          s"${Console.BOLD}where${Console.RESET}$aliases$capts$outerTests$recInfo${defs}"
      }
      def showName = s"${Console.BOLD}<${ident|>printVar}>${Console.RESET}"
      override def toString = s"$showName[${Push(cid,payload,Id)}]"
    }
    
    val scopes = mutable.Map.empty[CallId, Scope]
    val lambdaScopes = mutable.Map.empty[Ref, Scope]
    val modDefsWithIdents = mod.modDefs.map { case (nme,df) => (nme,AST.mkIdent(nme),df) }
    
    if (debugScheduling) modDefsWithIdents.foreach { d =>
      printVar(d._2)  // allocate the right 'bare' name for each top-level definition
    }
    
    var delayedExplorations: List[(CallId,Instr,Ref)] = Nil
    val allPops = mutable.Set.empty[(Var, Instr, Ref)]
    
    val defScopes = modDefsWithIdents.map { case (nme,ide,df) =>
      Sdebug(s"Processing $nme")
      
      if (!debugScheduling) resetVarPrintingState()
      printVar(ide) // allocate the right 'bare' name for each top-level definition
      
      val rootScp = ScheduleDebug.nestDbg {
        val cid = if (useDummyCids) DummyCallId else new CallId(ide,mod.nextUnique)
        val scp = new Scope(cid, Id, "def", None) {
          override val ident = ide
        }
        scopes += cid -> scp
        scp.returns += Id -> df
        scp
      }
      
      rootScp
      
    }
    
    Sdebug(s"=== PROGRAM SCOPES ===")
    scopes.valuesIterator.foreach(scp => Sdebug(scp.show()))
    Sdebug(s"=== / ===")
    
    defScopes.foreach(_.process()) // Q: not all scopes?
    
    Sdebug(s"=== ANALYZED SCOPES ===")
    scopes.valuesIterator.foreach(scp => Sdebug(scp.show()))
    Sdebug(s"=== / ===")
    
    val modDefStrings: List[Str] = {
      scopes.valuesIterator
        //.filterNot(lambdaScopes.valuesIterator.toSet)
        .map(_.toDefn.stringify).toList // TODO rm those to be inlined
    }
    
  }
  
}
