package datalog.storage

import datalog.dsl.Atom
import datalog.execution.ast.*
import datalog.execution.JoinIndexes
import datalog.execution.ir.*

import scala.collection.{immutable, mutable}

// Keep pretty print stuff separate bc long and ugly, mb put it in a macro
class Printer[S <: StorageManager](val sm: S) {
  def factToString(r: EDB): String = {
//    r.map(s => s.mkString("(", ", ", ")")).mkString("[", ", ", "]")
    r.factToString
  }

  def atomToString(a: Seq[Atom]): String = {
    s"${sm.ns(a.head.rId)}${a.head.terms.mkString("(", ", ", ")")} :- ${a.drop(1).map(b => sm.ns(b.rId) + b.terms.mkString("(", ", ", ")")).mkString("", ", ", "")}"
  }

  def ruleToString(r: mutable.ArrayBuffer[Seq[Atom]]): String = {
    r.map(s => if (s.isEmpty) "<empty>" else s.head.toString + s.drop(1).mkString(" :- ", ",", ""))
      .mkString("[", "; ", "]")
  }
  def edbToString(db: Database[?]): String = {
    immutable.ListMap(db.toSeq.sortBy(_._1):_*)
      .map((k, v) => (sm.ns(k), factToString(v)))
      .mkString("[\n  ", ",\n  ", "]")
  }
  def naivePlanToString(keys: mutable.ArrayBuffer[JoinIndexes]): String = {
    "Union( " +
      keys.map(k =>
        if (k.edb)
          "SCAN(" + k.deps.map(n => sm.ns(n)).mkString("[", ", ", "]") + ")"
        else
          "Project" + k.projIndexes.map((typ, v) => f"$typ$v").mkString("[", " ", "]") + "( " +
            "JOIN" +
            k.varIndexes.map(v => v.mkString("$", "==$", "")).mkString("[", ",", "]") +
            k.constIndexes.map((k, v) => "$" + k + "==" + (if (v.isInstanceOf[String]) s"\"$v\"" else "v")).mkString("{", "&&", "}") +
            k.deps.map(n =>
              if(k.edb) "edbs-" + sm.ns(n)  else sm.ns(n)
            ).mkString("(", "*", ")") +
            " )"
      ).mkString("", ", ", "") +
      " )"
  }

  def snPlanToString(keys: mutable.ArrayBuffer[JoinIndexes]): String = {
    "UNION( " +
      keys.map(k =>
        if (k.edb)
          "SCAN(" + k.deps.map(n => sm.ns(n)).mkString("[", ", ", "]") + ")"
        else
          var idx = -1
          "UNION(" +
            k.deps.map(d => {
              var found = false
              "PROJECT" + k.projIndexes.map((typ, v) => f"$typ$v").mkString("[", " ", "]") + "( " +
                "JOIN" +
                k.varIndexes.map(v => v.mkString("$", "==$", "")).mkString("[", ",", "]") +
                k.constIndexes.map((k, v) => k + "==" + v).mkString("{", "&&", "}") +
                k.deps.zipWithIndex.map((n, i) => {
                  if (n == d && !found && i > idx)
                    found = true
                    idx = i
                    "delta[known][" + sm.ns(n) + s"($n)" + "]"
                  else
                    if(k.edb)
                      "edbs[" + sm.ns(n) + s"($n)" + "]"
                    else
                      "derived[known][" + sm.ns(n) + s"($n)" + "]"
                }).mkString("(", "*", ")") +
                " )"
            }).mkString("[ ", ", ", " ]") + " )"
      ).mkString("[ ", ", ", " ]") +
      " )"
  }



  /**
   * Print IDBs stored in the regular SN/N Execution Engines
   * @param idbs
   * @return
   */
  def printIDB(idbs: mutable.Map[RelationId, mutable.ArrayBuffer[Seq[Atom]]]): String = {
    immutable.ListMap(idbs.toSeq.sortBy(_._1):_*)
      .map((k, v) => (sm.ns(k), ruleToString(v)))
      .mkString("[\n  ", ",\n  ", "]")
  }

  def printAST(node: ASTNode): String = {
    node match {
      case ProgramNode(allRules) => "PROGRAM\n" + allRules.map((rId, rules) => s"  ${sm.ns(rId)} => ${printAST(rules)}").mkString("", "\n", "")
      case AllRulesNode(rules, rId, edb) => s"${if (edb) "{EDB}"+factToString(sm.getEDB(rId))+"{IDB}" else ""}${rules.map(printAST).mkString("[", "\n\t", "  ]")}"
      case RuleNode(head, body, atoms, hash) =>
        s"\n\t${printAST(head)} :- ${body.map(printAST).mkString("(", ", ", ")")}" +
          s" => idx=${sm.allRulesAllIndexes(atoms.head.rId)(hash).toStringWithNS(sm.ns)}\n"
      case n: AtomNode => n match {
        case NegAtom(expr) => s"!${printAST(expr)}"
        case LogicAtom(relation, terms) => s"${sm.ns(relation)}${terms.map(printAST).mkString("(", ", ", ")")}"
      }
      case n: TermNode => n match {
        case VarTerm(value) => s"${value.toString}"
        case ConstTerm(value) => s"$value"
      }
    }
  }

  def printIR[T](node: IROp[T], ident: Int = 0, seq: Int = 0)(using ctx: InterpreterContext): String = {
    val i = "\t"*ident
    i + (node match {
      case ProgramOp(children:_*) => s"PROGRAM:\n${printIR(children.head, ident+1)}"
      case SwapAndClearOp() => "SWAP & CLEAR"
      case DoWhileOp(toCmp, children:_*) => s"DO {\n${printIR(children.head, ident+1)}}\n${i}WHILE {$toCmp}\n"
      case SequenceOp(fnCode, children:_*) => s"SEQ{${seq+1}${if (fnCode != OpCode.SEQ) "::" + fnCode else "_"}:${children.zipWithIndex.map((o, idx) => s"${seq+1}.$idx" + printIR(o, ident+1, seq+1)).mkString("[\n", ",\n", "]")}"
      case ScanEDBOp(srcRel) => s"SCANEDB(edbs[${ctx.storageManager.ns(srcRel)}])"
      case ScanOp(srcRel, db, knowledge) =>
        s"SCAN[$db.$knowledge](${ctx.storageManager.ns(srcRel)})"
      case ProjectJoinFilterOp(rId, hash, children:_*) =>
        val keys = sm.allRulesAllIndexes(rId)(hash)
        s"JOIN${keys.varToString()}${keys.constToString()}${children.map(s => printIR(s, ident+1)).mkString("(\n", ",\n", ")")}"
      case InsertOp(rId, db, knowledge, children:_*) =>
        s"INSERT INTO $db.$knowledge.${ctx.storageManager.ns(rId)}\n${children.map(s => printIR(s, ident+1)).mkString("", "\n", "")}\n"
      case UnionOp(fnCode, children:_*) => s"UNION${if (fnCode != OpCode.UNION) "::" + fnCode else "_"}${children.map(o => printIR(o, ident+1)).mkString("(\n", ",\n", ")")}"
      case UnionSPJOp(rId, hash, children:_*) => s"UNION_SPJ::${ctx.storageManager.ns(rId)}::${sm.allRulesAllIndexes(rId)(hash)}::${children.map(o => printIR(o, ident+1)).mkString("(\n", ",\n", ")")}"
      case DiffOp(children:_*) => s"DIFF\n${printIR(children.head, ident+1)}\n-${printIR(children(1), ident+1)}"
      case DebugNode(prefix, dbg) => s"DEBUG: $prefix"
      case DebugPeek(prefix, dbg, children:_*) => s"DEBUG PEEK: $prefix into: ${printIR(children.head)}"
    })
  }
}
