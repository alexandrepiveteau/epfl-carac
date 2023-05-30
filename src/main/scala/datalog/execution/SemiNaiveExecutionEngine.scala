package datalog.execution

import datalog.dsl.{Atom, Constant, Term, Variable}
import datalog.storage.{RelationId, StorageManager, EDB}
import datalog.tools.Debug.debug

import scala.collection.mutable

/**
 * Shallow embedding version of the execution engine, i.e. no AST. Used mostly to compare results and step through
 * since much easier to debug than the staged versions.
 *
 * @param storageManager
 */
class SemiNaiveExecutionEngine(override val storageManager: StorageManager) extends NaiveExecutionEngine(storageManager) {
  def evalRuleSN(rId: RelationId): EDB = {
    storageManager.SPJU(rId, getOperatorKeys(rId))
  }

  def evalSN(rId: RelationId, relations: Seq[RelationId]): Unit = {
    debug("evalSN for ", () => storageManager.ns(rId))
    relations.foreach(r => {
      val prev = storageManager.getKnownDerivedDB(r)
      debug(s"\tderived[known][${storageManager.ns(r)}] =", () => storageManager.printer.factToString(prev))
      val res = evalRuleSN(r)
      debug("\tevalRuleSN=", () => storageManager.printer.factToString(res))
      val diff = storageManager.diff(res, prev)
      storageManager.resetNewDelta(r, diff)
      storageManager.resetNewDerived(r, storageManager.getKnownDerivedDB(r), storageManager.getNewDeltaDB(r)) // set derived[new] to derived[known]+delta[new]
      debug(s"\tdiff, i.e. delta[new][${storageManager.ns(r)}] =", () => storageManager.printer.factToString(storageManager.getNewDeltaDB(r)))
      debug(s"\tall, i.e. derived[new][${storageManager.ns(r)}] =", () => storageManager.printer.factToString(storageManager.getNewDerivedDB(r)))
      /* storageManager.resetDelta(r, newDbId, storageManager.getDiff(
        evalRuleSN(r, newDbId, knownDbId),
        storageManager.getDerivedDB(r, knownDbId)
      ))
      storageManager.resetDerived(r, newDbId, storageManager.getDeltaDB(r, newDbId), storageManager.getDerivedDB(r, knownDbId)) // set derived[new] to derived[known]+delta[new] */
//      System.gc()
//      System.gc()
//      val mb = 1024 * 1024
//      val runtime = Runtime.getRuntime
//      println(s"after SPJU for relation ${storageManager.ns(r)}, query=${storageManager.printer.snPlanToString(getOperatorKeys(rId)JoinIndexes])} results in MB")
//      println("** Used Memory:  " + (runtime.totalMemory - runtime.freeMemory) / mb)
//      println("** Free Memory:  " + runtime.freeMemory / mb)
//      println("** Total Memory: " + runtime.totalMemory / mb)
//      println("** Given memory:   " + runtime.maxMemory / mb)
    })
  }

  override def solve(toSolve: RelationId): Set[Seq[Term]] = {
    storageManager.verifyEDBs(idbs.keys.to(mutable.Set))
    if (storageManager.edbContains(toSolve) && !idbs.contains(toSolve)) { // if just an edb predicate then return
      return storageManager.getEDBResult(toSolve)
    }
    if (!idbs.contains(toSolve)) {
      throw new Exception("Solving for rule without body")
    }
    // TODO: if a IDB predicate without vars, then solve all and test contains result?
    //    if (relations.isEmpty)
    //      return Set()
    val strata = precedenceGraph.scc()
    storageManager.initEvaluation() // facts previously derived

    debug(s"solving relation: ${storageManager.ns(toSolve)} order of relations=", strata.toString)

    var scount = 0
    // for each stratum
    strata.foreach(relations =>
      var count = 0
      println(s"\n\n*****STRATA $scount with relations $relations")
      scount += 1

      evalNaive(relations.toSeq, true) // this fills derived[new] and delta[new]
      var setDiff = true
      while (setDiff) {
        storageManager.swapKnowledge()
        storageManager.clearNewDerived()

        debug(s"initial state @ $count", storageManager.printer.toString)
        count += 1
        evalSN(toSolve, relations.toSeq)
        setDiff = storageManager.compareNewDeltaDBs()
      }
      debug(s"final state @$count", storageManager.printer.toString)
      storageManager.updateDiscovered()
    )
    storageManager.getNewIDBResult(toSolve)
  }
}
