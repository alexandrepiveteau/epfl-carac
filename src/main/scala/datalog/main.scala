package datalog

import datalog.dsl.MODE.Interpret
import datalog.execution.{ExecutionEngine, NaiveExecutionEngine, SemiNaiveExecutionEngine, JITStagedExecutionEngine, InterpretedStagedExecutionEngine, CompiledStagedExecutionEngine, ir}
import datalog.dsl.{Constant, Program, __}
import datalog.execution.ast.transform.CopyEliminationPass
import datalog.execution.ir.InterpreterContext
import datalog.storage.{CollectionsStorageManager, NS, RelationalStorageManager}

import scala.util.Random
import scala.collection.mutable

def tc(program: Program): Unit = {
  val edge = program.relation[Constant]("edge")
  val path = program.relation[Constant]("path")
//  val oneHop = program.relation[Constant]("oneHop")
//  val twoHops = program.relation[Constant]("twoHops")
//  val threeHops = program.relation[Constant]("threeHops")
//  val fourHops = program.relation[Constant]("fourHops")
//  val query = program.relation[Constant]("query")

  val x, y, z, w, q = program.variable()

  path(x, y) :- edge(x, y)
  path(x, z) :- (edge(x, y), path(y, z))

//  oneHop(x, y) :- edge(x, y, "red")
//  twoHops(x, z) :- ( oneHop(x, y), oneHop(y, z) )
//  threeHops(x, w) :- ( oneHop(x, y), oneHop(y, z), oneHop(z, w))
//  fourHops(x, q) :- ( oneHop(x, y), oneHop(y, z), oneHop(z, w), oneHop(w, q))


  edge("a", "a", "red") :- ()
  edge("a", "b", "blue") :- ()
  edge("b", "c", "red") :- ()
  edge("c", "d", "blue") :- ()

  println("RES=" + path.solve())
}

def tc_long(program: Program): Unit = {
  val base = program.relation[Constant]("base")

  val tc = program.relation[Constant]("tc")

  val tcl = program.relation[Constant]("tcl")

  val tcr = program.relation[Constant]("tcr")

  val X, Y, Z = program.variable()

  tcl(X, Y) :- (base(X, Y))
  tcl(X, Y) :- (tcl(X, Z), base(Z, Y))

  tcr(X, Y) :- (base(X, Y))
  tcr(X, Y) :- (base(X, Z), tcr(Z, Y))

  tc(X, Y) :- (base(X, Y))
  tc(X, Y) :- (tc(X, Z), tc(Z, Y))

  base("a", "b") :- ()
  base("b", "c") :- ()
  base("c", "d") :- ()
  println("RES=" + tc.solve(mode = Interpret))
}

def reversible(program: Program, engine: ExecutionEngine): Unit = {
  val assign = program.relation[Constant]("assign")
  val assignOp = program.relation[Constant]("assignOp")
  val inverses = program.relation[Constant]("inverses")
  val equiv = program.relation[Constant]("equiv")
  val query = program.relation[Constant]("query")

  val a, b, x, y, cst, f2, f1 = program.variable()

  assignOp("v1", "+", "v0", 1) :- ()
  assignOp("v2", "-", "v1", 1) :- ()
  inverses("+", "-") :- ()

  equiv(a, b) :- assign(a, b)
  equiv(a, b) :- ( equiv(a, x), equiv(x, b) )
//  equiv(a, b) :- ( assignOp(x, f1, a, cst), assignOp(b, f2, y, cst), equiv(x, y), inverses(f1, f2))
  equiv(a, b) :- ( assignOp(x, f1, a, cst), assignOp(b, f2, x, cst), inverses(f1, f2) )

  query(x) :- equiv(x, "v2")

//  println("RES=" + engine.solveCompiled(query.id))
  println("RES=" + query.solve(mode = Interpret))
}

def func(program: Program) = {
  val eq = program.relation[Constant]("eq")
  val succ = program.relation[Constant]("succ")
  val f = program.relation[Constant]("f")
  val arg = program.relation[Constant]("arg")
  val args = program.relation[Constant]("args")
  val a, b, v, w, i, p, k, any1, any2 = program.variable()


  succ("1", "2") :- ()
  succ("2", "3") :- ()
  succ("3", "4") :- ()

  f("x", "g") :- ()
  f("y", "f") :- ()

  arg("x", "1", "A") :- ()
  arg("x", "2", "B") :- ()
  arg("x", "3", "Z") :- ()

  arg("y", "1", "C") :- ()
  arg("y", "2", "D") :- ()
  arg("y", "3", "W") :- ()

  eq(a, b) :- ( f(v, a), f(w, b), args(v, w, "3") )

  args(v, w, i) :- ( succ(p, i), arg(v, i, k), arg(w, i, k), args(v, w, p) )
  args(v, w, "1") :- ( arg(v, "1", any1), arg(w, "1", any2) )
  val res = eq.solve()

  println(s"RES LEN=${res.size}; res=$res")
}

def isEqual(program: Program): Unit = {
  val equal = program.relation[Constant]("equal")

  val isEqual = program.relation[Constant]("isEqual")

  val succ = program.relation[Constant]("succ")

  val m, n, r, pn, pm = program.variable()


  equal("0", "0", "1") :- ()
  equal(m, n, r) :- ( succ(pm, m) , succ(pn, n), equal(pm, pn, r) )

  isEqual(r) :- equal("5", "5", r)

  succ("0", "1") :- ()
  succ("1", "2") :- ()
  succ("2", "3") :- ()
  succ("3", "4") :- ()
  succ("4", "5") :- ()
  succ("5", "6") :- ()
  succ("6", "7") :- ()
  succ("7", "8") :- ()
  succ("8", "9") :- ()
  succ("9", "10") :- ()
  succ("10", "11") :- ()
  succ("11", "12") :- ()
  succ("12", "13") :- ()
  succ("13", "14") :- ()
  succ("14", "15") :- ()
  succ("15", "16") :- ()
  succ("16", "17") :- ()
  succ("17", "18") :- ()
  succ("18", "19") :- ()
  succ("19", "20") :- ()
  val res = isEqual.solve()

  println(s"RES LEN=${res.size}; res=$res")
}

def multiJoin(program: Program): Unit = {
  val edge = program.relation[Constant]("edge")
  val path = program.relation[Constant]("path")
  val hops1 = program.relation[Constant]("hops1")
  val hops2_join = program.relation[Constant]("hops2_join")
  val hops3_join = program.relation[Constant]("hops3_join")
  val hops4_join = program.relation[Constant]("hops4_join")
  val hops5_join = program.relation[Constant]("hops5_join")
  val hops6_join = program.relation[Constant]("hops6_join")
  val hops7_join = program.relation[Constant]("hops7_join")
  val hops8_join = program.relation[Constant]("hops8_join")
//  val hops9_join = program.relation[Constant]("hops9_join")
//  val hops10_join = program.relation[Constant]("hops10_join")

  val x, y, z, w, q = program.variable()
  val a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11 = program.variable()

  path(x, y) :- edge(x, y)
  path(x, z) :- (edge(x, y), path(y, z))

  hops1(x, y) :- edge(x, y)
  hops2_join(a1, a3) :-   (hops1(a1, a2), hops1(a2, a3))
  hops3_join(a1, a4) :-   (hops1(a1, a2), hops1(a2, a3), hops1(a3, a4))
  hops4_join(a1, a5) :-   (hops1(a1, a2), hops1(a2, a3), hops1(a3, a4), hops1(a4, a5))
  hops5_join(a1, a6) :-   (hops1(a1, a2), hops1(a2, a3), hops1(a3, a4), hops1(a4, a5), hops1(a5, a6))
  hops6_join(a1, a7) :-   (hops1(a1, a2), hops1(a2, a3), hops1(a3, a4), hops1(a4, a5), hops1(a5, a6), hops1(a6, a7))
  hops7_join(a1, a8) :-   (hops1(a1, a2), hops1(a2, a3), hops1(a3, a4), hops1(a4, a5), hops1(a5, a6), hops1(a6, a7), hops1(a7, a8))
  hops8_join(a1, a9) :-   (hops1(a1, a2), hops1(a2, a3), hops1(a3, a4), hops1(a4, a5), hops1(a5, a6), hops1(a6, a7), hops1(a7, a8), hops1(a8, a9))
//  hops9_join(a1, a10) :-  (hops1(a1, a2), hops1(a2, a3), hops1(a3, a4), hops1(a4, a5), hops1(a5, a6), hops1(a6, a7), hops1(a7, a8), hops1(a8, a9), hops1(a9, a10))
//  hops10_join(a1, a11) :- (hops1(a1, a2), hops1(a2, a3), hops1(a3, a4), hops1(a4, a5), hops1(a5, a6), hops1(a6, a7), hops1(a7, a8), hops1(a8, a9), hops1(a9, a10), hops1(a10, a11))


  for i <- 0 until 200 do
    edge(
      Random.alphanumeric.dropWhile(_.isDigit).dropWhile(_.isUpper).head.toString,
      Random.alphanumeric.dropWhile(_.isDigit).dropWhile(_.isUpper).head.toString
    ) :- ()
  println("RES=" + hops3_join.solve().size)
}

def cliquer(program: Program): Unit = {
  val edge = program.relation[Constant]("edge")

  val leg = program.relation[Constant]("leg")

  val reachable = program.relation[Constant]("reachable")

  val same_clique = program.relation[Constant]("same_clique")

  val X, Y, Z = program.variable()

  leg(X, Z) :- (edge(X, Y), edge(Y, Z))

  reachable(X, Y) :- edge(X, Y)
  reachable(X, Y) :- (edge(X, Z), reachable(Z, Y))
  same_clique(X, Y) :- (reachable(X, Y), reachable(Y, X))

  edge("a", "b") :- ()
  edge("b", "c") :- ()
  edge("c", "d") :- ()
  edge("d", "a") :- ()

  reachable("e", "f") :- ()

  println("reachable=" + reachable.solve(mode = Interpret))
}

def input_output(program: Program): Unit = {
  // input, i.e. defined in facts+here, i.e. named
  val InputOutputNumberSymbol = program.relation[Constant]("InputOutputNumberSymbol")
  val InputOutputReceiveNumberSymbol = program.relation[Constant]("InputOutputReceiveNumberSymbol")
  val InputOutputReceiveSendNumberSymbol = program.relation[Constant]("InputOutputReceiveSendNumberSymbol")
  val InputOutputReceiveSendSymbolNumber = program.relation[Constant]("InputOutputReceiveSendSymbolNumber")
  val InputOutputReceiveSymbolNumber = program.relation[Constant]("InputOutputReceiveSymbolNumber")
  val InputOutputSendNumberSymbol = program.relation[Constant]("InputOutputSendNumberSymbol")
  val InputOutputSendSymbolNumber = program.relation[Constant]("InputOutputSendSymbolNumber")
  val InputOutputSymbolNumber = program.relation[Constant]("InputOutputSymbolNumber")

  // input, i.e. defined in facts
  val InputNumberSymbol = program.relation[Constant]("InputNumberSymbol")
  val InputReceiveNumberSymbol = program.relation[Constant]("InputReceiveNumberSymbol")
  val InputReceiveSendNumberSymbol = program.relation[Constant]("InputReceiveSendNumberSymbol")
  val InputReceiveSendSymbolNumber = program.relation[Constant]("InputReceiveSendSymbolNumber")
  val InputReceiveSymbolNumber = program.relation[Constant]("InputReceiveSymbolNumber")
  val InputSendNumberSymbol = program.relation[Constant]("InputSendNumberSymbol")
  val InputSendSymbolNumber = program.relation[Constant]("InputSendSymbolNumber")
  val InputSymbolNumber = program.relation[Constant]("InputSymbolNumber")

  // output, i.e. new relations
  val OutputNumberSymbol = program.relation[Constant]("OutputNumberSymbol")
  val OutputReceiveNumberSymbol = program.relation[Constant]("OutputReceiveNumberSymbol")
  val OutputReceiveSendNumberSymbol = program.relation[Constant]("OutputReceiveSendNumberSymbol")
  val OutputReceiveSendSymbolNumber = program.relation[Constant]("OutputReceiveSendSymbolNumber")
  val OutputReceiveSymbolNumber = program.relation[Constant]("OutputReceiveSymbolNumber")
  val OutputSendNumberSymbol = program.relation[Constant]("OutputSendNumberSymbol")
  val OutputSendSymbolNumber = program.relation[Constant]("OutputSendSymbolNumber")
  val OutputSymbolNumber = program.relation[Constant]("OutputSymbolNumber")

  val ReceiveNumberSymbol = program.relation[Constant]("ReceiveNumberSymbol")
  val ReceiveSendNumberSymbol = program.relation[Constant]("ReceiveSendNumberSymbol")
  val ReceiveSendSymbolNumber = program.relation[Constant]("ReceiveSendSymbolNumber")
  val ReceiveSymbolNumber = program.relation[Constant]("ReceiveSymbolNumber")
  val ReceiverNumberSymbol = program.relation[Constant]("ReceiverNumberSymbol")
  val ReceiverSymbolNumber = program.relation[Constant]("ReceiverSymbolNumber")
  val SendNumberSymbol = program.relation[Constant]("SendNumberSymbol")
  val SendSymbolNumber = program.relation[Constant]("SendSymbolNumber")

  val SenderNumberSymbol = program.relation[Constant]("SenderNumberSymbol")
  val SenderSymbolNumber = program.relation[Constant]("SenderSymbolNumber")

  val x, y = program.variable()

  InputNumberSymbol(1, "b") :- ()
  InputOutputNumberSymbol(1, "b") :- ()
  InputOutputReceiveNumberSymbol(1, "b") :- ()
  InputOutputReceiveNumberSymbol(x, y) :- (SenderNumberSymbol(x, y))
  InputOutputReceiveSendNumberSymbol(1, "b") :- ()
  InputOutputReceiveSendNumberSymbol(x, y) :- (SenderNumberSymbol(x, y))
  InputOutputReceiveSendSymbolNumber("b", 1) :- ()
  InputOutputReceiveSendSymbolNumber(x, y) :- (SenderSymbolNumber(x, y))
  InputOutputReceiveSymbolNumber("b", 1) :- ()
  InputOutputReceiveSymbolNumber(x, y) :- (SenderSymbolNumber(x, y))
  InputOutputSendNumberSymbol(1, "b") :- ()
  InputOutputSendSymbolNumber("b", 1) :- ()
  InputOutputSymbolNumber("b", 1) :- ()
  InputReceiveNumberSymbol(1, "b") :- ()
  InputReceiveNumberSymbol(x, y) :- (SenderNumberSymbol(x, y))
  InputReceiveSendNumberSymbol(1, "b") :- ()
  InputReceiveSendNumberSymbol(x, y) :- (SenderNumberSymbol(x, y))
  InputReceiveSendSymbolNumber("b", 1) :- ()
  InputReceiveSendSymbolNumber(x, y) :- (SenderSymbolNumber(x, y))
  InputReceiveSymbolNumber("b", 1) :- ()
  InputReceiveSymbolNumber(x, y) :- (SenderSymbolNumber(x, y))
  InputSendNumberSymbol(1, "b") :- ()
  InputSendSymbolNumber("b", 1) :- ()
  InputSymbolNumber("b", 1) :- ()
  OutputNumberSymbol(1, "b") :- ()
  OutputReceiveNumberSymbol(1, "b") :- ()
  OutputReceiveNumberSymbol(x, y) :- (SenderNumberSymbol(x, y))
  OutputReceiveSendNumberSymbol(1, "b") :- ()
  OutputReceiveSendNumberSymbol(x, y) :- (SenderNumberSymbol(x, y))
  OutputReceiveSendSymbolNumber("b", 1) :- ()
  OutputReceiveSendSymbolNumber(x, y) :- (SenderSymbolNumber(x, y))
  OutputReceiveSymbolNumber("b", 1) :- ()
  OutputReceiveSymbolNumber(x, y) :- (SenderSymbolNumber(x, y))
  OutputSendNumberSymbol(1, "b") :- ()
  OutputSendSymbolNumber("b", 1) :- ()
  OutputSymbolNumber("b", 1) :- ()
  ReceiveNumberSymbol(1, "b") :- ()
  ReceiveNumberSymbol(x, y) :- SenderNumberSymbol(x, y)
  ReceiveSendNumberSymbol(1, "b") :- ()
  ReceiveSendNumberSymbol(x, y) :- (SenderNumberSymbol(x, y))
  ReceiveSendSymbolNumber("b", 1) :- ()
  ReceiveSendSymbolNumber(x, y) :- (SenderSymbolNumber(x, y))
  ReceiveSymbolNumber("b", 1) :- ()
  ReceiveSymbolNumber(x, y) :- (SenderSymbolNumber(x, y))
  ReceiverNumberSymbol(x, y) :- (InputOutputReceiveSendNumberSymbol(x, y))
  ReceiverNumberSymbol(x, y) :- (InputOutputSendNumberSymbol(x, y))
  ReceiverNumberSymbol(x, y) :- (InputReceiveSendNumberSymbol(x, y))
  ReceiverNumberSymbol(x, y) :- (InputSendNumberSymbol(x, y))
  ReceiverNumberSymbol(x, y) :- (OutputReceiveSendNumberSymbol(x, y))
  ReceiverNumberSymbol(x, y) :- (OutputSendNumberSymbol(x, y))
  ReceiverNumberSymbol(x, y) :- (SendNumberSymbol(x, y))
  ReceiverSymbolNumber(x, y) :- (InputOutputReceiveSendSymbolNumber(x, y))
  ReceiverSymbolNumber(x, y) :- (InputOutputSendSymbolNumber(x, y))
  ReceiverSymbolNumber(x, y) :- (InputReceiveSendSymbolNumber(x, y))
  ReceiverSymbolNumber(x, y) :- (InputSendSymbolNumber(x, y))
  ReceiverSymbolNumber(x, y) :- (OutputReceiveSendSymbolNumber(x, y))
  ReceiverSymbolNumber(x, y) :- (OutputSendSymbolNumber(x, y))
  ReceiverSymbolNumber(x, y) :- (SendSymbolNumber(x, y))
  SendNumberSymbol(1, "b") :- ()
  SendSymbolNumber("b", 1) :- ()

  val res = InputOutputSymbolNumber.solve()

  println(s"RES=${res.size}")
}

def manyRelations(program: Program): Int = {
  val edge = program.relation[Constant]("edge")
  val path = program.relation[Constant]("path")
  val hops1 = program.relation[Constant]("hops1")
  val hops2 = program.relation[Constant]("hops2")
  val hops3 = program.relation[Constant]("hops3")
  val hops4 = program.relation[Constant]("hops4")
  val hops5 = program.relation[Constant]("hops5")
  val hops6 = program.relation[Constant]("hops6")
  val hops7 = program.relation[Constant]("hops7")
  val hops8 = program.relation[Constant]("hops8")
  val hops9 = program.relation[Constant]("hops9")
  val hops10 = program.relation[Constant]("hops10")
  val hops11 = program.relation[Constant]("hops11")
  val hops12 = program.relation[Constant]("hops12")
  val hops13 = program.relation[Constant]("hops13")
  val hops14 = program.relation[Constant]("hops14")
  val hops15 = program.relation[Constant]("hops15")
  val hops16 = program.relation[Constant]("hops16")
  val hops17 = program.relation[Constant]("hops17")
  val hops18 = program.relation[Constant]("hops18")
  val hops19 = program.relation[Constant]("hops19")
  val hops20 = program.relation[Constant]("hops20")

  val x, y, z, w, q = program.variable()

  path(x, y) :- edge(x, y)
  path(x, z) :- (edge(x, y), path(y, z))

  hops1(x, y) :- edge(x, y)
  hops2(x, y) :- (hops1(x, z), hops1(z, y))
  hops3(x, y) :- (hops1(x, z), hops2(z, y))
  hops4(x, y) :- (hops1(x, z), hops3(z, y))
  hops5(x, y) :- (hops1(x, z), hops4(z, y))
  hops6(x, y) :- (hops1(x, z), hops5(z, y))
  hops7(x, y) :- (hops1(x, z), hops6(z, y))
  hops8(x, y) :- (hops1(x, z), hops7(z, y))
  hops9(x, y) :- (hops1(x, z), hops8(z, y))
  hops10(x, y) :- (hops1(x, z), hops9(z, y))
  hops11(x, y) :- (hops1(x, z), hops10(z, y))
  hops12(x, y) :- (hops1(x, z), hops11(z, y))
  hops13(x, y) :- (hops1(x, z), hops12(z, y))
  hops14(x, y) :- (hops1(x, z), hops13(z, y))
  hops15(x, y) :- (hops1(x, z), hops14(z, y))
  hops16(x, y) :- (hops1(x, z), hops15(z, y))
  hops17(x, y) :- (hops1(x, z), hops16(z, y))
  hops18(x, y) :- (hops1(x, z), hops17(z, y))
  hops19(x, y) :- (hops1(x, z), hops18(z, y))
  hops20(x, y) :- (hops1(x, z), hops19(z, y))

  for i <- 0 until 20 do
    edge(
      Random.alphanumeric.dropWhile(_.isDigit).dropWhile(_.isUpper).head.toString,
      Random.alphanumeric.dropWhile(_.isDigit).dropWhile(_.isUpper).head.toString
    ) :- ()
  val res = hops20.solve()
  hops20.solve()
  hops20.solve()
  hops20.solve()
  hops20.solve()
  println(s"RES=${res.size}")
  hops20.id
}

def anon_var(program: Program) = {
  val Check = program.relation[Constant]("Check")
  val In = program.relation[Constant]("In")
  val A1 = program.relation[Constant]("A1")

  val a, b, c, d, e, f, i = program.variable()

  A1(1, i) :- (Check(__, b, c, d, e, f), In(__, b, c, d, e, f, i))
  A1(2, i) :- (Check(a, __, c, d, e, f), In(a, __, c, d, e, f, i))
  A1(3, i) :- (Check(a, b, __, d, e, f), In(a, b, __, d, e, f, i))
  A1(4, i) :- (Check(a, b, c, __, e, f), In(a, b, c, __, e, f, i))
  A1(5, i) :- (Check(a, b, c, d, __, f), In(a, b, c, d, __, f, i))
  A1(6, i) :- (Check(a, b, c, d, e, __), In(a, b, c, d, e, __, i))

  A1(7, i) :- (Check(__, __, c, d, e, f), In(__, __, c, d, e, f, i))
  A1(8, i) :- (Check(a, __, __, d, e, f), In(a, __, __, d, e, f, i))
  A1(9, i) :- (Check(a, b, __, __, e, f), In(a, b, __, __, e, f, i))
  A1(10, i) :- (Check(a, b, c, __, __, f), In(a, b, c, __, __, f, i))
  A1(11, i) :- (Check(a, b, c, d, __, __), In(a, b, c, d, __, __, i))

  A1(12, i) :- (Check(__, __, __, d, e, f), In(__, __, __, d, e, f, i))
  A1(13, i) :- (Check(a, __, __, __, e, f), In(a, __, __, __, e, f, i))
  A1(14, i) :- (Check(a, b, __, __, __, f), In(a, b, __, __, __, f, i))
  A1(15, i) :- (Check(a, b, c, __, __, __), In(a, b, c, __, __, __, i))

  A1(16, i) :- (Check(__, __, __, __, e, f), In(__, __, __, __, e, f, i))
  A1(17, i) :- (Check(a, __, __, __, __, f), In(a, __, __, __, __, f, i))
  A1(18, i) :- (Check(a, b, __, __, __, __), In(a, b, __, __, __, __, i))

  A1(19, i) :- (Check(a, b, c, d, e, f), In(a, b, c, d, e, f, i))

  // po2
  val A2 = program.relation[Constant]("A2")
  A2(1, i) :- (Check(__, b, c, d, e, __), In(__, b, c, d, e, __, i))
  A2(2, i) :- (Check(a, __, c, d, e, __), In(a, __, c, d, e, __, i))
  A2(3, i) :- (Check(a, b, __, __, e, f), In(a, b, __, __, e, f, i))
  A2(4, i) :- (Check(__, __, c, d, __, __), In(__, __, c, d, __, __, i))
  A2(5, i) :- (Check(a, __, __, __, __, f), In(a, __, __, __, __, f, i))
  A2(6, i) :- (Check(__, b, c, d, __, f), In(__, b, c, d, __, f, i))
  A2(7, i) :- (Check(__, b, c, __, e, f), In(__, b, c, __, e, f, i))
  A2(8, i) :- (Check(__, b, __, d, e, f), In(__, b, __, d, e, f, i))
  A2(9, i) :- (Check(__, __, c, d, e, f), In(__, __, c, d, e, f, i))
  A2(10, i) :- (Check(__, b, c, d, __, __), In(__, b, c, d, __, __, i))
  A2(11, i) :- (Check(__, b, c, __, __, f), In(__, b, c, __, __, f, i))
  A2(12, i) :- (Check(__, b, __, __, e, f), In(__, b, __, __, e, f, i))
  A2(13, i) :- (Check(__, __, __, d, e, f), In(__, __, __, d, e, f, i))
  A2(14, i) :- (Check(__, b, c, __, __, __), In(__, b, c, __, __, __, i))
  A2(15, i) :- (Check(__, b, __, __, __, f), In(__, b, __, __, __, f, i))
  A2(16, i) :- (Check(__, __, __, __, e, f), In(__, __, __, __, e, f, i))
  A2(17, i) :- (Check(__, b, c, d, e, f), In(__, b, c, d, e, f, i))
  A2(18, i) :- (Check(a, b, c, d, e, f), In(a, b, c, d, e, f, i))

  // po3
  val A3 = program.relation[Constant]("A3")
  A3(1, i) :- (Check(a, b, c, d, e, f), In(a, b, c, d, e, f, i))
  A3(2, i) :- (Check(a, __, c, d, e, f), In(a, __, c, d, e, f, i))
  A3(3, i) :- (Check(a, __, __, d, e, f), In(a, __, __, d, e, f, i))
  A3(4, i) :- (Check(a, __, __, __, e, f), In(a, __, __, __, e, f, i))
  A3(5, i) :- (Check(a, __, __, __, __, f), In(a, __, __, __, __, f, i))
  A3(6, i) :- (Check(a, __, __, __, __, __), In(a, __, __, __, __, __, i))
  A3(7, i) :- (Check(a, b, __, d, e, f), In(a, b, __, d, e, f, i))
  A3(8, i) :- (Check(a, b, __, __, e, f), In(a, b, __, __, e, f, i))
  A3(9, i) :- (Check(a, b, __, __, __, f), In(a, b, __, __, __, f, i))
  A3(10, i) :- (Check(a, b, __, __, __, __), In(a, b, __, __, __, __, i))
  A3(11, i) :- (Check(a, b, c, __, e, f), In(a, b, c, __, e, f, i))
  A3(12, i) :- (Check(a, b, c, __, __, f), In(a, b, c, __, __, f, i))
  A3(13, i) :- (Check(a, b, c, __, __, __), In(a, b, c, __, __, __, i))
  A3(14, i) :- (Check(a, b, c, d, __, f), In(a, b, c, d, __, f, i))
  A3(15, i) :- (Check(a, b, c, d, __, __), In(a, b, c, d, __, __, i))

  // po4
  val A4 = program.relation[Constant]("A4")
  A4(1, i) :- (Check(a, b, c, d, e, f), In(a, b, c, d, e, f, i))
  A4(2, i) :- (Check(a, b, c, d, __, __), In(a, b, c, d, __, __, i))
  A4(3, i) :- (Check(a, __, c, d, __, __), In(a, __, c, d, __, __, i))
  A4(4, i) :- (Check(a, b, __, d, __, __), In(a, b, __, d, __, __, i))
  A4(5, i) :- (Check(a, b, c, __, __, __), In(a, b, c, __, __, __, i))
  A4(6, i) :- (Check(__, __, c, d, __, __), In(__, __, c, d, __, __, i))
  A4(7, i) :- (Check(a, b, __, __, __, __), In(a, b, __, __, __, __, i))
  A4(8, i) :- (Check(a, __, __, __, __, __), In(a, __, __, __, __, __, i))
  A4(9, i) :- (Check(__, b, __, __, __, __), In(__, b, __, __, __, __, i))

  // po5
  val A5 = program.relation[Constant]("A5")
  A5(1, i) :- (Check(a, b, c, d, e, f), In(a, b, c, d, e, f, i))
  A5(2, i) :- (Check(a, __, __, d, e, f), In(a, __, __, d, e, f, i))
  A5(3, i) :- (Check(a, b, __, d, e, f), In(a, b, __, d, e, f, i))
  A5(4, i) :- (Check(a, __, __, __, e, f), In(a, __, __, __, e, f, i))
  A5(5, i) :- (Check(a, b, __, __, __, f), In(a, b, __, __, __, f, i))
  A5(6, i) :- (Check(a, __, __, __, __, __), In(a, __, __, __, __, __, i))
  A5(7, i) :- (Check(a, b, __, d, e, f), In(a, b, __, d, e, f, i))
  A5(8, i) :- (Check(a, b, __, __, e, f), In(a, b, __, __, e, f, i))
  A5(9, i) :- (Check(a, b, __, __, __, f), In(a, b, __, __, __, f, i))
  A5(10, i) :- (Check(a, b, __, __, __, __), In(a, b, __, __, __, __, i))
  A5(11, i) :- (Check(a, b, c, __, e, f), In(a, b, c, __, e, f, i))
  A5(12, i) :- (Check(a, __, c, __, __, f), In(a, __, c, __, __, f, i))
  A5(13, i) :- (Check(a, b, c, __, __, __), In(a, b, c, __, __, __, i))
  A5(14, i) :- (Check(a, b, c, d, __, f), In(a, b, c, d, __, f, i))
  A5(15, i) :- (Check(a, __, c, d, __, __), In(a, __, c, d, __, __, i))

  val res = A5.solve()
  println(s"RES=${res.size}")
}

@main def main = {
  //  val engine = new SemiNaiveStagedExecutionEngine(new CollectionsStorageManager())
  //  val program = Program(engine)
  //  println("staged")
  //  run(program)
  //  reversible(program, engine)
  //  val run = multiJoin

//  println("OLD SN")
//  given engine1: ExecutionEngine = new SemiNaiveExecutionEngine(new CollectionsStorageManager())
//  val program1 = Program(engine1)
//  tc(program1)
//  println("\n\n_______________________\n\n")

//  println("STAGED COMPILE")
//  given engine2: ExecutionEngine = new CompiledStagedExecutionEngine(new CollectionsStorageManager())
//  val program2 = Program(engine2)
//  tc(program2)
//  println("\n\n_______________________\n\n")
//
  println("STAGED INTERP")
  given engine3: ExecutionEngine = new InterpretedStagedExecutionEngine(new CollectionsStorageManager())
  val program3 = Program(engine3)
  tc(program3)
  println("\n\n_______________________\n\n")

  println("JIT STAGED: aot Loop Body")
  val engine4: ExecutionEngine = new JITStagedExecutionEngine(new CollectionsStorageManager(), ir.OpCode.LOOP_BODY, false, false)
  val program4 = Program(engine4)
  tc(program4)
  println("\n\n_______________________\n\n")

//  println("JIT STAGED: aot EvalSN")
//  val engine5: ExecutionEngine = new JITStagedExecutionEngine(new CollectionsStorageManager(), ir.OpCode.EVAL_SN, true, true)
//  val program5 = Program(engine5)
//  manyRelations(program5)
//  println("\n\n_______________________\n\n")
  //  println("JIT STAGED")
//
//  given engine3: ExecutionEngine = new SemiNaiveJITStagedExecutionEngine(new CollectionsStorageManager())
//  val program3 = Program(engine3)
//  benchStaged10x(program3)
//  println("\n\n_______________________\n\n")
}
