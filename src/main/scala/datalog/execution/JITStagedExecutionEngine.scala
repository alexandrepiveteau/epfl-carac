package datalog.execution

import datalog.dsl.{MODE, Term}
import datalog.execution.ast.ASTNode
import datalog.execution.ir.*
import datalog.storage.{CollectionsStorageManager, DB, KNOWLEDGE, StorageManager}
import datalog.tools.Debug.debug

import scala.collection.mutable
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.quoted.staging
import scala.util.{Failure, Success}

class JITStagedExecutionEngine(override val storageManager: CollectionsStorageManager, granularity: OpCode, aot: Boolean, block: Boolean) extends StagedExecutionEngine(storageManager) {
  import storageManager.EDB
  val trees: mutable.Queue[ProgramOp] = mutable.Queue.empty
  override def solve(rId: Int, mode: MODE): Set[Seq[Term]] = super.solve(rId, MODE.Interpret)
  def interpretIRRelOp(irTree: IRRelOp)(using ctx: InterpreterContext): storageManager.EDB = {
//    println(s"IN INTERPRET REL_IR, code=${irTree.code}")
    irTree match {
      case op: ScanOp =>
        op.runRel(storageManager)

      case op: ScanEDBOp =>
        op.runRel(storageManager)

      case op: JoinOp =>
        op.runRel(storageManager, op.ops.map(o => sm => interpretIRRelOp(o)))

      case op: ProjectOp =>
        op.runRel(storageManager, Seq(sm => interpretIRRelOp(op.subOp)))

      case op: UnionOp =>
        op.runRel(storageManager, op.ops.map(o => sm => interpretIRRelOp(o)))

      case op: DiffOp =>
        op.runRel(storageManager, Seq(sm => interpretIRRelOp(op.lhs), sm => interpretIRRelOp(op.rhs)))

      case op: DebugPeek =>
        op.runRel(storageManager, Seq(sm => interpretIRRelOp(op.op)))
      case _ => throw new Exception("Error: interpretRelOp called with unit operation")
    }
  }
  override def interpretIR(irTree: IROp)(using ctx: InterpreterContext): Any = {
//    println(s"IN INTERPRET IR, code=${irTree.code}")
    given CollectionsStorageManager = storageManager
    irTree match {
      case op: ProgramOp =>
        trees.append(op)
        if (aot)
          aotCompile(op)
        // test if need to compile, if so:
        if (op.compiledFn == null) { // don't bother online compile since only 1
          op.run(storageManager, Seq(sm => interpretIR(op.body)))
        } else {
          op.compiledFn.value match {
            case Some(Success(run)) =>
              debug("COMPILED PROGRAM", () => "")
              run(storageManager)
            case Some(Failure(e)) =>
              throw Exception(s"Error compiling PROGRAM with: ${e.getCause}")
            case None =>
              if (block)
                debug("program compilation not ready yet, so blocking", () => "")
                Await.result(op.compiledFn, Duration.Inf)(storageManager)
              else
                debug("program compilation not ready yet, so defaulting", () => "")
                op.run(storageManager, Seq(sm => interpretIR(op.body)))
          }
        }

      case op: DoWhileOp =>
        // test if need to compile, if so:
        if (op.compiledFn == null) { // don't bother online compile since only 1
          op.run(storageManager, Seq(sm => interpretIR(op.body)))
        } else {
          op.compiledFn.value match {
            case Some(Success(run)) =>
              debug("COMPILED DOWHILE", () => "")
              run(storageManager)
            case Some(Failure(e)) =>
              throw Exception(s"Error compiling DOWHILE:${op.code} with: ${e.getCause}")
            case None =>
              if (block)
                debug("dowhile compilation not ready yet, so blocking", () => "")
                Await.result(op.compiledFn, Duration.Inf)(storageManager)
              else
                debug("dowhile compilation not ready yet, so defaulting", () => "")
                op.run(storageManager, Seq(sm => interpretIR(op.body)))
          }
        }

      case op: SequenceOp =>
        op.code match
          case OpCode.EVAL_SN | OpCode.EVAL_NAIVE | OpCode.LOOP_BODY if granularity == op.code => {
//            debug("", () => s"found subtree to compile: ${op.code} and gran=$granularity")
            // test if need to compile, if so:
            if (op.compiledFn == null && !aot) { // need to start compilation
              debug(s"starting online compilation for code ${op.code}", () => "")
              given scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global

              op.compiledFn = Future {
                given staging.Compiler = dedicatedDotty //staging.Compiler.make(getClass.getClassLoader) // TODO: new dotty per thread, maybe concat
                compiler.getCompiled(op, ctx)
              }
            }
            op.compiledFn.value match {
              case Some(Success(run)) =>
                debug(s"COMPILED ${op.code}", () => "")
                run(storageManager)
              case Some(Failure(e)) =>
                throw Exception(s"Error compiling SEQ:${op.code} with: ${e.getCause}")
              case None =>
//                Thread.sleep(1000)
                if (block)
                  debug(s"${op.code} compilation not ready yet, so blocking", () => "")
                  Await.result(op.compiledFn, Duration.Inf)(storageManager)
                else
                  debug(s"${op.code} compilation not ready yet, so defaulting", () => "")
                  op.run(storageManager, op.ops.map(o => sm => interpretIR(o)))
            }
          }
          case _ =>
            op.run(storageManager, op.ops.map(o => sm => interpretIR(o)))

      case op: SwapAndClearOp =>
        op.run(storageManager)

      case op: InsertOp =>
        op.run(storageManager, Seq((sm: CollectionsStorageManager) => interpretIRRelOp(op.subOp)) ++ op.subOp2.map(sop => (sm: CollectionsStorageManager) => interpretIRRelOp(sop)))

      case op: DebugNode =>
        op.run(storageManager)

      case _ =>
        irTree match {
          case op: IRRelOp => interpretIRRelOp(op)
          case _ =>
            throw new Exception(s"Error: unhandled node type $irTree")
        }
    }
  }
  // TODO: this could potentially go as a tree transform phase
  def aotCompile(tree: ProgramOp)(using ctx: InterpreterContext): Unit = {
    val subTree = tree.getSubTree(granularity)
    debug("", () => s"ahead-of-time compiling ${subTree.code}")
    given scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global
    given staging.Compiler = dedicatedDotty

    subTree.compiledFn = Future {
      compiler.getCompiled(subTree, ctx)
    }
  }

  def waitForAll(): Unit = {
    debug(s"awaiting in aot=$aot gran=$granularity block=$block", () => trees.map(t => t.code).mkString("[", ", ", "]"))
    trees.foreach(t =>
      val subTree = t.getSubTree(granularity)
      if (subTree.compiledFn != null)
        try {
          Await.result(subTree.compiledFn, Duration.Inf)
        } catch {
          case e  => throw new Exception(s"Exception cleaning up compiler: ${e.getCause}")
        }
    )
    trees.clear()
  }
}
