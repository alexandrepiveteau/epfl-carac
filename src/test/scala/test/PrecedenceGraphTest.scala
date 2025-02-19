package test
import datalog.execution.{ExecutionEngine, PrecedenceGraph, SemiNaiveExecutionEngine}

import scala.collection.mutable
import datalog.dsl.{Program, Constant}
import datalog.storage.{NS, VolcanoStorageManager}

class PrecedenceGraphTest extends munit.FunSuite {
  test("tarjan ex") {
    given engine: ExecutionEngine = new SemiNaiveExecutionEngine(new VolcanoStorageManager())
    val program = Program(engine)
    val t0 = program.relation[Constant]("t0")
    val t1 = program.relation[Constant]("t1")
    val t2 = program.relation[Constant]("t2")
    val t3 = program.relation[Constant]("t3")
    val t4 = program.relation[Constant]("t4")
    val t5 = program.relation[Constant]("t5")
    val t6 = program.relation[Constant]("t6")
    val t7 = program.relation[Constant]("t7")
    val t8 = program.relation[Constant]("t8")
    val t9 = program.relation[Constant]("t9")
    val t10 = program.relation[Constant]("t10")
    val x = program.variable()

    t0(x) :- t1(x)
    t1(x) :- (t4(x), t6(x), t7(x))
    t2(x) :- (t4(x), t6(x), t7(x))
    t3(x) :- (t4(x), t6(x), t7(x))
    t4(x) :- (t2(x), t3(x))
    t5(x) :- (t2(x), t3(x))
    t6(x) :- (t5(x), t8(x))
    t7(x) :- (t5(x), t8(x))
    t10(x) :- t10(x)

    assertEquals(
      engine.precedenceGraph.scc(),
      Seq(Set(t8.id), Set(t2.id, t3.id, t4.id, t5.id, t6.id, t7.id), Set(t1.id), Set(t0.id), Set(t10.id))
    )
    assertEquals(
      engine.precedenceGraph.topSort(t10.id),
      Seq(t10.id),
    )
    // There's a single component with the 6 nodes
    assertEquals(
      engine.precedenceGraph.topSort(t4.id).toSet,
      Set(t2.id, t3.id, t4.id, t5.id, t6.id, t7.id)
    )
    assertEquals(
      engine.precedenceGraph.topSort(t8.id),
      Seq() // TODO: empty
    )
  }
  test("tc with isolated cycles") {
    given engine: ExecutionEngine = new SemiNaiveExecutionEngine(new VolcanoStorageManager())

    val program = Program(engine)
    val e = program.relation[String]("e")
    val p = program.relation[String]("p")
    val other = program.relation[String]("other")
    val e2 = program.relation[String]("e2")
    val p2 = program.relation[String]("p2")
    val other2 = program.relation[String]("other2")
    val x, y, z = program.variable()

    e("a", "b") :- ()
    e("b", "c") :- ()
    e("c", "d") :- ()
    p(x, y) :- e(x, y)
    p(x, z) :- (e(x, y), p(y, z))
    other(x) :- p("a", x)

    e2("a", "b") :- ()
    e2("b", "c") :- ()
    e2("c", "d") :- ()
    p2(x, y) :- e2(x, y)
    p2(x, z) :- (e2(x, y), p2(y, z))
    other2(x) :- p2("a", x)

    assertEquals(
      engine.precedenceGraph.scc().toSet,
      Set(Set(e.id), Set(p.id), Set(other.id), Set(e2.id), Set(p2.id), Set(other2.id))
    )
    assertEquals(
      engine.precedenceGraph.topSort(other.id),
      Seq(p.id, other.id)
    )
    assertEquals(
      engine.precedenceGraph.topSort(other2.id),
      Seq(p2.id, other2.id)
    )
  }

  test("transitive closure") {
    given engine: ExecutionEngine = new SemiNaiveExecutionEngine(new VolcanoStorageManager())
    val program = Program(engine)
    val e = program.relation[String]("e")
    val p = program.relation[String]("p")
    val other = program.relation[String]("other")
    val x, y, z = program.variable()

    e("a", "b") :- ()
    e("b", "c") :- ()
    e("c", "d") :- ()
    p(x, y) :- e(x, y)
    p(x, z) :- ( e(x, y), p(y, z) )
    other(x) :- p("a", x)

    engine.precedenceGraph.topSort(other.id) // test against sorted to keep groups
    assertEquals(
      engine.precedenceGraph.scc(),
      Seq(Set(e.id), Set(p.id), Set(other.id))
    )
  }

  test("simple cycle") {
    given engine: ExecutionEngine = new SemiNaiveExecutionEngine(new VolcanoStorageManager())
    val program = Program(engine)
    val a = program.relation[String]("a")
    val b = program.relation[String]("b")
    val c = program.relation[String]("c")
    val other = program.relation[String]("other")

    a() :- b()
    b() :- c()
    c() :- a()
    a() :- other()

    engine.precedenceGraph.topSort(a.id)
    assertEquals(
      engine.precedenceGraph.scc(),
      Seq(Set(other.id), Set(a.id, b.id, c.id))
    )
  }

  test("simple cycle with inner loop") {
    given engine: ExecutionEngine = new SemiNaiveExecutionEngine(new VolcanoStorageManager())
    val program = Program(engine)
    val a = program.relation[String]("a")
    val b = program.relation[String]("b")
    val c = program.relation[String]("c")
    val other = program.relation[String]("other")

    a() :- b()
    a() :- (a(), b())
    b() :- c()
    c() :- a()
    a() :- other()

    engine.precedenceGraph.topSort(a.id)
    assertEquals(
      engine.precedenceGraph.scc(),
      Seq(Set(other.id), Set(a.id, b.id, c.id))
    )
  }

  test("souffle top order test") {
    given engine: ExecutionEngine = new SemiNaiveExecutionEngine(new VolcanoStorageManager())
    val program = Program(engine)
    val a = program.relation[String]("a")
    val b = program.relation[String]("b")
    val c = program.relation[String]("c")
    val other = program.relation[String]("other")

    a() :- b()
    a() :- (a(), b())
    b() :- c()
    c() :- a()
    a() :- other()

    engine.precedenceGraph.topSort(a.id)
    assertEquals(
      engine.precedenceGraph.scc(),
      Seq(Set(other.id), Set(a.id, b.id, c.id))
    )
  }

  test("from souffle double_tree".ignore) {
    //
    // double_tree.dl
    //
    // This test is useful in the analysis of the topological order
    // of nodes. Use the debug report to view the scc graph, then
    // see what order the nodes are visited in.

    given engine: ExecutionEngine = new SemiNaiveExecutionEngine(new VolcanoStorageManager())
    val program = Program(engine)

    val xy = program.relation[Constant]("xy")

    val x1 = program.relation[Constant]("x1")
    val x2 = program.relation[Constant]("x2")

    val x11 = program.relation[Constant]("x11")
    val x12 = program.relation[Constant]("x12")
    val x13 = program.relation[Constant]("x13")

    val x21 = program.relation[Constant]("x21")
    val x22 = program.relation[Constant]("x22")
    val x23 = program.relation[Constant]("x23")

    val x111 = program.relation[Constant]("x111")
    val x112 = program.relation[Constant]("x112")
    val x113 = program.relation[Constant]("x113")
    val x114 = program.relation[Constant]("x114")

    val x121 = program.relation[Constant]("x121")
    val x122 = program.relation[Constant]("x122")
    val x123 = program.relation[Constant]("x123")
    val x124 = program.relation[Constant]("x124")

    val x131 = program.relation[Constant]("x131")
    val x132 = program.relation[Constant]("x132")
    val x133 = program.relation[Constant]("x133")
    val x134 = program.relation[Constant]("x134")

    val x211 = program.relation[Constant]("x211")
    val x212 = program.relation[Constant]("x212")
    val x213 = program.relation[Constant]("x213")
    val x214 = program.relation[Constant]("x214")

    val x221 = program.relation[Constant]("x221")
    val x222 = program.relation[Constant]("x222")
    val x223 = program.relation[Constant]("x223")
    val x224 = program.relation[Constant]("x224")

    val x231 = program.relation[Constant]("x231")
    val x232 = program.relation[Constant]("x232")
    val x233 = program.relation[Constant]("x233")
    val x234 = program.relation[Constant]("x234")

    val x1111 = program.relation[Constant]("x1111")
    val x1112 = program.relation[Constant]("x1112")
    val x1113 = program.relation[Constant]("x1113")
    val x1114 = program.relation[Constant]("x1114")
    val x1115 = program.relation[Constant]("x1115")
    val x1121 = program.relation[Constant]("x1121")
    val x1122 = program.relation[Constant]("x1122")
    val x1123 = program.relation[Constant]("x1123")
    val x1124 = program.relation[Constant]("x1124")
    val x1125 = program.relation[Constant]("x1125")
    val x1131 = program.relation[Constant]("x1131")
    val x1132 = program.relation[Constant]("x1132")
    val x1133 = program.relation[Constant]("x1133")
    val x1134 = program.relation[Constant]("x1134")
    val x1135 = program.relation[Constant]("x1135")
    val x1141 = program.relation[Constant]("x1141")
    val x1142 = program.relation[Constant]("x1142")
    val x1143 = program.relation[Constant]("x1143")
    val x1144 = program.relation[Constant]("x1144")
    val x1145 = program.relation[Constant]("x1145")

    val x1211 = program.relation[Constant]("x1211")
    val x1212 = program.relation[Constant]("x1212")
    val x1213 = program.relation[Constant]("x1213")
    val x1214 = program.relation[Constant]("x1214")
    val x1215 = program.relation[Constant]("x1215")
    val x1221 = program.relation[Constant]("x1221")
    val x1222 = program.relation[Constant]("x1222")
    val x1223 = program.relation[Constant]("x1223")
    val x1224 = program.relation[Constant]("x1224")
    val x1225 = program.relation[Constant]("x1225")
    val x1231 = program.relation[Constant]("x1231")
    val x1232 = program.relation[Constant]("x1232")
    val x1233 = program.relation[Constant]("x1233")
    val x1234 = program.relation[Constant]("x1234")
    val x1235 = program.relation[Constant]("x1235")
    val x1241 = program.relation[Constant]("x1241")
    val x1242 = program.relation[Constant]("x1242")
    val x1243 = program.relation[Constant]("x1243")
    val x1244 = program.relation[Constant]("x1244")
    val x1245 = program.relation[Constant]("x1245")

    val x1311 = program.relation[Constant]("x1311")
    val x1312 = program.relation[Constant]("x1312")
    val x1313 = program.relation[Constant]("x1313")
    val x1314 = program.relation[Constant]("x1314")
    val x1315 = program.relation[Constant]("x1315")
    val x1321 = program.relation[Constant]("x1321")
    val x1322 = program.relation[Constant]("x1322")
    val x1323 = program.relation[Constant]("x1323")
    val x1324 = program.relation[Constant]("x1324")
    val x1325 = program.relation[Constant]("x1325")
    val x1331 = program.relation[Constant]("x1331")
    val x1332 = program.relation[Constant]("x1332")
    val x1333 = program.relation[Constant]("x1333")
    val x1334 = program.relation[Constant]("x1334")
    val x1335 = program.relation[Constant]("x1335")
    val x1341 = program.relation[Constant]("x1341")
    val x1342 = program.relation[Constant]("x1342")
    val x1343 = program.relation[Constant]("x1343")
    val x1344 = program.relation[Constant]("x1344")
    val x1345 = program.relation[Constant]("x1345")

    val x2111 = program.relation[Constant]("x2111")
    val x2112 = program.relation[Constant]("x2112")
    val x2113 = program.relation[Constant]("x2113")
    val x2114 = program.relation[Constant]("x2114")
    val x2115 = program.relation[Constant]("x2115")
    val x2121 = program.relation[Constant]("x2121")
    val x2122 = program.relation[Constant]("x2122")
    val x2123 = program.relation[Constant]("x2123")
    val x2124 = program.relation[Constant]("x2124")
    val x2125 = program.relation[Constant]("x2125")
    val x2131 = program.relation[Constant]("x2131")
    val x2132 = program.relation[Constant]("x2132")
    val x2133 = program.relation[Constant]("x2133")
    val x2134 = program.relation[Constant]("x2134")
    val x2135 = program.relation[Constant]("x2135")
    val x2141 = program.relation[Constant]("x2141")
    val x2142 = program.relation[Constant]("x2142")
    val x2143 = program.relation[Constant]("x2143")
    val x2144 = program.relation[Constant]("x2144")
    val x2145 = program.relation[Constant]("x2145")

    val x2211 = program.relation[Constant]("x2211")
    val x2212 = program.relation[Constant]("x2212")
    val x2213 = program.relation[Constant]("x2213")
    val x2214 = program.relation[Constant]("x2214")
    val x2215 = program.relation[Constant]("x2215")
    val x2221 = program.relation[Constant]("x2221")
    val x2222 = program.relation[Constant]("x2222")
    val x2223 = program.relation[Constant]("x2223")
    val x2224 = program.relation[Constant]("x2224")
    val x2225 = program.relation[Constant]("x2225")
    val x2231 = program.relation[Constant]("x2231")
    val x2232 = program.relation[Constant]("x2232")
    val x2233 = program.relation[Constant]("x2233")
    val x2234 = program.relation[Constant]("x2234")
    val x2235 = program.relation[Constant]("x2235")
    val x2241 = program.relation[Constant]("x2241")
    val x2242 = program.relation[Constant]("x2242")
    val x2243 = program.relation[Constant]("x2243")
    val x2244 = program.relation[Constant]("x2244")
    val x2245 = program.relation[Constant]("x2245")

    val x2311 = program.relation[Constant]("x2311")
    val x2312 = program.relation[Constant]("x2312")
    val x2313 = program.relation[Constant]("x2313")
    val x2314 = program.relation[Constant]("x2314")
    val x2315 = program.relation[Constant]("x2315")
    val x2321 = program.relation[Constant]("x2321")
    val x2322 = program.relation[Constant]("x2322")
    val x2323 = program.relation[Constant]("x2323")
    val x2324 = program.relation[Constant]("x2324")
    val x2325 = program.relation[Constant]("x2325")
    val x2331 = program.relation[Constant]("x2331")
    val x2332 = program.relation[Constant]("x2332")
    val x2333 = program.relation[Constant]("x2333")
    val x2334 = program.relation[Constant]("x2334")
    val x2335 = program.relation[Constant]("x2335")
    val x2341 = program.relation[Constant]("x2341")
    val x2342 = program.relation[Constant]("x2342")
    val x2343 = program.relation[Constant]("x2343")
    val x2344 = program.relation[Constant]("x2344")
    val x2345 = program.relation[Constant]("x2345")

    x2345(1) :- ()

    val z = program.variable()

    xy(z) :- (x1(z), x2(z))

    x1(z) :- (x11(z), x12(z), x13(z))
    x2(z) :- (x21(z), x22(z), x23(z))

    x11(z) :- (x111(z), x112(z), x113(z), x114(z))
    x12(z) :- (x121(z), x122(z), x123(z), x124(z))
    x13(z) :- (x131(z), x132(z), x133(z), x134(z))

    x21(z) :- (x211(z), x212(z), x213(z), x214(z))
    x22(z) :- (x221(z), x222(z), x223(z), x224(z))
    x23(z) :- (x231(z), x232(z), x233(z), x234(z))

    x111(z) :- (x1111(z), x1112(z), x1113(z), x1114(z), x1115(z))
    x112(z) :- (x1121(z), x1122(z), x1123(z), x1124(z), x1125(z))
    x113(z) :- (x1131(z), x1132(z), x1133(z), x1134(z), x1135(z))
    x114(z) :- (x1141(z), x1142(z), x1143(z), x1144(z), x1145(z))

    x121(z) :- (x1211(z), x1212(z), x1213(z), x1214(z), x1215(z))
    x122(z) :- (x1221(z), x1222(z), x1223(z), x1224(z), x1225(z))
    x123(z) :- (x1231(z), x1232(z), x1233(z), x1234(z), x1235(z))
    x124(z) :- (x1241(z), x1242(z), x1243(z), x1244(z), x1245(z))

    x131(z) :- (x1311(z), x1312(z), x1313(z), x1314(z), x1315(z))
    x132(z) :- (x1321(z), x1322(z), x1323(z), x1324(z), x1325(z))
    x133(z) :- (x1331(z), x1332(z), x1333(z), x1334(z), x1335(z))
    x134(z) :- (x1341(z), x1342(z), x1343(z), x1344(z), x1345(z))

    x211(z) :- (x2111(z), x2112(z), x2113(z), x2114(z), x2115(z))
    x212(z) :- (x2121(z), x2122(z), x2123(z), x2124(z), x2125(z))
    x213(z) :- (x2131(z), x2132(z), x2133(z), x2134(z), x2135(z))
    x214(z) :- (x2141(z), x2142(z), x2143(z), x2144(z), x2145(z))

    x221(z) :- (x2211(z), x2212(z), x2213(z), x2214(z), x2215(z))
    x222(z) :- (x2221(z), x2222(z), x2223(z), x2224(z), x2225(z))
    x223(z) :- (x2231(z), x2232(z), x2233(z), x2234(z), x2235(z))
    x224(z) :- (x2241(z), x2242(z), x2243(z), x2244(z), x2245(z))

    x231(z) :- (x2311(z), x2312(z), x2313(z), x2314(z), x2315(z))
    x232(z) :- (x2321(z), x2322(z), x2323(z), x2324(z), x2325(z))
    x233(z) :- (x2331(z), x2332(z), x2333(z), x2334(z), x2335(z))
    x234(z) :- (x2341(z), x2342(z), x2343(z), x2344(z), x2345(z))

    x1111(1) :- ()
    x1112(1) :- ()
    x1113(1) :- ()
    x1114(1) :- ()
    x1115(1) :- ()
    x1121(1) :- ()
    x1122(1) :- ()
    x1123(1) :- ()
    x1124(1) :- ()
    x1125(1) :- ()
    x1131(1) :- ()
    x1132(1) :- ()
    x1133(1) :- ()
    x1134(1) :- ()
    x1135(1) :- ()
    x1141(1) :- ()
    x1142(1) :- ()
    x1143(1) :- ()
    x1144(1) :- ()
    x1145(1) :- ()

    x1211(1) :- ()
    x1212(1) :- ()
    x1213(1) :- ()
    x1214(1) :- ()
    x1215(1) :- ()
    x1221(1) :- ()
    x1222(1) :- ()
    x1223(1) :- ()
    x1224(1) :- ()
    x1225(1) :- ()
    x1231(1) :- ()
    x1232(1) :- ()
    x1233(1) :- ()
    x1234(1) :- ()
    x1235(1) :- ()
    x1241(1) :- ()
    x1242(1) :- ()
    x1243(1) :- ()
    x1244(1) :- ()
    x1245(1) :- ()

    x1311(1) :- ()
    x1312(1) :- ()
    x1313(1) :- ()
    x1314(1) :- ()
    x1315(1) :- ()
    x1321(1) :- ()
    x1322(1) :- ()
    x1323(1) :- ()
    x1324(1) :- ()
    x1325(1) :- ()
    x1331(1) :- ()
    x1332(1) :- ()
    x1333(1) :- ()
    x1334(1) :- ()
    x1335(1) :- ()
    x1341(1) :- ()
    x1342(1) :- ()
    x1343(1) :- ()
    x1344(1) :- ()
    x1345(1) :- ()

    x2111(1) :- ()
    x2112(1) :- ()
    x2113(1) :- ()
    x2114(1) :- ()
    x2115(1) :- ()
    x2121(1) :- ()
    x2122(1) :- ()
    x2123(1) :- ()
    x2124(1) :- ()
    x2125(1) :- ()
    x2131(1) :- ()
    x2132(1) :- ()
    x2133(1) :- ()
    x2134(1) :- ()
    x2135(1) :- ()
    x2141(1) :- ()
    x2142(1) :- ()
    x2143(1) :- ()
    x2144(1) :- ()
    x2145(1) :- ()

    x2211(1) :- ()
    x2212(1) :- ()
    x2213(1) :- ()
    x2214(1) :- ()
    x2215(1) :- ()
    x2221(1) :- ()
    x2222(1) :- ()
    x2223(1) :- ()
    x2224(1) :- ()
    x2225(1) :- ()
    x2231(1) :- ()
    x2232(1) :- ()
    x2233(1) :- ()
    x2234(1) :- ()
    x2235(1) :- ()
    x2241(1) :- ()
    x2242(1) :- ()
    x2243(1) :- ()
    x2244(1) :- ()
    x2245(1) :- ()

    x2311(1) :- ()
    x2312(1) :- ()
    x2313(1) :- ()
    x2314(1) :- ()
    x2315(1) :- ()
    x2321(1) :- ()
    x2322(1) :- ()
    x2323(1) :- ()
    x2324(1) :- ()
    x2325(1) :- ()
    x2331(1) :- ()
    x2332(1) :- ()
    x2333(1) :- ()
    x2334(1) :- ()
    x2335(1) :- ()
    x2341(1) :- ()
    x2342(1) :- ()
    x2343(1) :- ()
    x2344(1) :- ()


    val y1 = program.relation[Constant]("y1")
    val y2 = program.relation[Constant]("y2")

    val y11 = program.relation[Constant]("y11")
    val y12 = program.relation[Constant]("y12")
    val y13 = program.relation[Constant]("y13")

    val y21 = program.relation[Constant]("y21")
    val y22 = program.relation[Constant]("y22")
    val y23 = program.relation[Constant]("y23")

    val y111 = program.relation[Constant]("y111")
    val y112 = program.relation[Constant]("y112")
    val y113 = program.relation[Constant]("y113")
    val y114 = program.relation[Constant]("y114")

    val y121 = program.relation[Constant]("y121")
    val y122 = program.relation[Constant]("y122")
    val y123 = program.relation[Constant]("y123")
    val y124 = program.relation[Constant]("y124")

    val y131 = program.relation[Constant]("y131")
    val y132 = program.relation[Constant]("y132")
    val y133 = program.relation[Constant]("y133")
    val y134 = program.relation[Constant]("y134")

    val y211 = program.relation[Constant]("y211")
    val y212 = program.relation[Constant]("y212")
    val y213 = program.relation[Constant]("y213")
    val y214 = program.relation[Constant]("y214")

    val y221 = program.relation[Constant]("y221")
    val y222 = program.relation[Constant]("y222")
    val y223 = program.relation[Constant]("y223")
    val y224 = program.relation[Constant]("y224")

    val y231 = program.relation[Constant]("y231")
    val y232 = program.relation[Constant]("y232")
    val y233 = program.relation[Constant]("y233")
    val y234 = program.relation[Constant]("y234")

    val y1111 = program.relation[Constant]("y1111")
    val y1112 = program.relation[Constant]("y1112")
    val y1113 = program.relation[Constant]("y1113")
    val y1114 = program.relation[Constant]("y1114")
    val y1115 = program.relation[Constant]("y1115")
    val y1121 = program.relation[Constant]("y1121")
    val y1122 = program.relation[Constant]("y1122")
    val y1123 = program.relation[Constant]("y1123")
    val y1124 = program.relation[Constant]("y1124")
    val y1125 = program.relation[Constant]("y1125")
    val y1131 = program.relation[Constant]("y1131")
    val y1132 = program.relation[Constant]("y1132")
    val y1133 = program.relation[Constant]("y1133")
    val y1134 = program.relation[Constant]("y1134")
    val y1135 = program.relation[Constant]("y1135")
    val y1141 = program.relation[Constant]("y1141")
    val y1142 = program.relation[Constant]("y1142")
    val y1143 = program.relation[Constant]("y1143")
    val y1144 = program.relation[Constant]("y1144")
    val y1145 = program.relation[Constant]("y1145")

    val y1211 = program.relation[Constant]("y1211")
    val y1212 = program.relation[Constant]("y1212")
    val y1213 = program.relation[Constant]("y1213")
    val y1214 = program.relation[Constant]("y1214")
    val y1215 = program.relation[Constant]("y1215")
    val y1221 = program.relation[Constant]("y1221")
    val y1222 = program.relation[Constant]("y1222")
    val y1223 = program.relation[Constant]("y1223")
    val y1224 = program.relation[Constant]("y1224")
    val y1225 = program.relation[Constant]("y1225")
    val y1231 = program.relation[Constant]("y1231")
    val y1232 = program.relation[Constant]("y1232")
    val y1233 = program.relation[Constant]("y1233")
    val y1234 = program.relation[Constant]("y1234")
    val y1235 = program.relation[Constant]("y1235")
    val y1241 = program.relation[Constant]("y1241")
    val y1242 = program.relation[Constant]("y1242")
    val y1243 = program.relation[Constant]("y1243")
    val y1244 = program.relation[Constant]("y1244")
    val y1245 = program.relation[Constant]("y1245")

    val y1311 = program.relation[Constant]("y1311")
    val y1312 = program.relation[Constant]("y1312")
    val y1313 = program.relation[Constant]("y1313")
    val y1314 = program.relation[Constant]("y1314")
    val y1315 = program.relation[Constant]("y1315")
    val y1321 = program.relation[Constant]("y1321")
    val y1322 = program.relation[Constant]("y1322")
    val y1323 = program.relation[Constant]("y1323")
    val y1324 = program.relation[Constant]("y1324")
    val y1325 = program.relation[Constant]("y1325")
    val y1331 = program.relation[Constant]("y1331")
    val y1332 = program.relation[Constant]("y1332")
    val y1333 = program.relation[Constant]("y1333")
    val y1334 = program.relation[Constant]("y1334")
    val y1335 = program.relation[Constant]("y1335")
    val y1341 = program.relation[Constant]("y1341")
    val y1342 = program.relation[Constant]("y1342")
    val y1343 = program.relation[Constant]("y1343")
    val y1344 = program.relation[Constant]("y1344")
    val y1345 = program.relation[Constant]("y1345")

    val y2111 = program.relation[Constant]("y2111")
    val y2112 = program.relation[Constant]("y2112")
    val y2113 = program.relation[Constant]("y2113")
    val y2114 = program.relation[Constant]("y2114")
    val y2115 = program.relation[Constant]("y2115")
    val y2121 = program.relation[Constant]("y2121")
    val y2122 = program.relation[Constant]("y2122")
    val y2123 = program.relation[Constant]("y2123")
    val y2124 = program.relation[Constant]("y2124")
    val y2125 = program.relation[Constant]("y2125")
    val y2131 = program.relation[Constant]("y2131")
    val y2132 = program.relation[Constant]("y2132")
    val y2133 = program.relation[Constant]("y2133")
    val y2134 = program.relation[Constant]("y2134")
    val y2135 = program.relation[Constant]("y2135")
    val y2141 = program.relation[Constant]("y2141")
    val y2142 = program.relation[Constant]("y2142")
    val y2143 = program.relation[Constant]("y2143")
    val y2144 = program.relation[Constant]("y2144")
    val y2145 = program.relation[Constant]("y2145")

    val y2211 = program.relation[Constant]("y2211")
    val y2212 = program.relation[Constant]("y2212")
    val y2213 = program.relation[Constant]("y2213")
    val y2214 = program.relation[Constant]("y2214")
    val y2215 = program.relation[Constant]("y2215")
    val y2221 = program.relation[Constant]("y2221")
    val y2222 = program.relation[Constant]("y2222")
    val y2223 = program.relation[Constant]("y2223")
    val y2224 = program.relation[Constant]("y2224")
    val y2225 = program.relation[Constant]("y2225")
    val y2231 = program.relation[Constant]("y2231")
    val y2232 = program.relation[Constant]("y2232")
    val y2233 = program.relation[Constant]("y2233")
    val y2234 = program.relation[Constant]("y2234")
    val y2235 = program.relation[Constant]("y2235")
    val y2241 = program.relation[Constant]("y2241")
    val y2242 = program.relation[Constant]("y2242")
    val y2243 = program.relation[Constant]("y2243")
    val y2244 = program.relation[Constant]("y2244")
    val y2245 = program.relation[Constant]("y2245")

    val y2311 = program.relation[Constant]("y2311")
    val y2312 = program.relation[Constant]("y2312")
    val y2313 = program.relation[Constant]("y2313")
    val y2314 = program.relation[Constant]("y2314")
    val y2315 = program.relation[Constant]("y2315")
    val y2321 = program.relation[Constant]("y2321")
    val y2322 = program.relation[Constant]("y2322")
    val y2323 = program.relation[Constant]("y2323")
    val y2324 = program.relation[Constant]("y2324")
    val y2325 = program.relation[Constant]("y2325")
    val y2331 = program.relation[Constant]("y2331")
    val y2332 = program.relation[Constant]("y2332")
    val y2333 = program.relation[Constant]("y2333")
    val y2334 = program.relation[Constant]("y2334")
    val y2335 = program.relation[Constant]("y2335")
    val y2341 = program.relation[Constant]("y2341")
    val y2342 = program.relation[Constant]("y2342")
    val y2343 = program.relation[Constant]("y2343")
    val y2344 = program.relation[Constant]("y2344")
    val y2345 = program.relation[Constant]("y2345")
    y2345(1) :- ()

    y1(z) :- xy(z)
    y2(z) :- xy(z)

    y11(z) :- y1(z)
    y12(z) :- y1(z)
    y13(z) :- y1(z)
    y21(z) :- y2(z)
    y22(z) :- y2(z)
    y23(z) :- y2(z)

    y111(z) :- y11(z)
    y112(z) :- y11(z)
    y113(z) :- y11(z)
    y114(z) :- y11(z)
    y121(z) :- y12(z)
    y122(z) :- y12(z)
    y123(z) :- y12(z)
    y124(z) :- y12(z)
    y131(z) :- y13(z)
    y132(z) :- y13(z)
    y133(z) :- y13(z)
    y134(z) :- y13(z)

    y211(z) :- y21(z)
    y212(z) :- y21(z)
    y213(z) :- y21(z)
    y214(z) :- y21(z)
    y221(z) :- y22(z)
    y222(z) :- y22(z)
    y223(z) :- y22(z)
    y224(z) :- y22(z)
    y231(z) :- y23(z)
    y232(z) :- y23(z)
    y233(z) :- y23(z)
    y234(z) :- y23(z)

    y1111(z) :- y111(z)
    y1112(z) :- y111(z)
    y1113(z) :- y111(z)
    y1114(z) :- y111(z)
    y1115(z) :- y111(z)
    y1121(z) :- y112(z)
    y1122(z) :- y112(z)
    y1123(z) :- y112(z)
    y1124(z) :- y112(z)
    y1125(z) :- y112(z)
    y1131(z) :- y113(z)
    y1132(z) :- y113(z)
    y1133(z) :- y113(z)
    y1134(z) :- y113(z)
    y1135(z) :- y113(z)
    y1141(z) :- y114(z)
    y1142(z) :- y114(z)
    y1143(z) :- y114(z)
    y1144(z) :- y114(z)
    y1145(z) :- y114(z)

    y1211(z) :- y121(z)
    y1212(z) :- y121(z)
    y1213(z) :- y121(z)
    y1214(z) :- y121(z)
    y1215(z) :- y121(z)
    y1221(z) :- y122(z)
    y1222(z) :- y122(z)
    y1223(z) :- y122(z)
    y1224(z) :- y122(z)
    y1225(z) :- y122(z)
    y1231(z) :- y123(z)
    y1232(z) :- y123(z)
    y1233(z) :- y123(z)
    y1234(z) :- y123(z)
    y1235(z) :- y123(z)
    y1241(z) :- y124(z)
    y1242(z) :- y124(z)
    y1243(z) :- y124(z)
    y1244(z) :- y124(z)
    y1245(z) :- y124(z)

    y1311(z) :- y131(z)
    y1312(z) :- y131(z)
    y1313(z) :- y131(z)
    y1314(z) :- y131(z)
    y1315(z) :- y131(z)
    y1321(z) :- y132(z)
    y1322(z) :- y132(z)
    y1323(z) :- y132(z)
    y1324(z) :- y132(z)
    y1325(z) :- y132(z)
    y1331(z) :- y133(z)
    y1332(z) :- y133(z)
    y1333(z) :- y133(z)
    y1334(z) :- y133(z)
    y1335(z) :- y133(z)
    y1341(z) :- y134(z)
    y1342(z) :- y134(z)
    y1343(z) :- y134(z)
    y1344(z) :- y134(z)
    y1345(z) :- y134(z)
    y2111(z) :- y211(z)

    y2112(z) :- y211(z)
    y2113(z) :- y211(z)
    y2114(z) :- y211(z)
    y2115(z) :- y211(z)
    y2121(z) :- y212(z)
    y2122(z) :- y212(z)
    y2123(z) :- y212(z)
    y2124(z) :- y212(z)
    y2125(z) :- y212(z)
    y2131(z) :- y213(z)
    y2132(z) :- y213(z)
    y2133(z) :- y213(z)
    y2134(z) :- y213(z)
    y2135(z) :- y213(z)
    y2141(z) :- y214(z)
    y2142(z) :- y214(z)
    y2143(z) :- y214(z)
    y2144(z) :- y214(z)
    y2145(z) :- y214(z)

    y2211(z) :- y221(z)
    y2212(z) :- y221(z)
    y2213(z) :- y221(z)
    y2214(z) :- y221(z)
    y2215(z) :- y221(z)
    y2221(z) :- y222(z)
    y2222(z) :- y222(z)
    y2223(z) :- y222(z)
    y2224(z) :- y222(z)
    y2225(z) :- y222(z)
    y2231(z) :- y223(z)
    y2232(z) :- y223(z)
    y2233(z) :- y223(z)
    y2234(z) :- y223(z)
    y2235(z) :- y223(z)
    y2241(z) :- y224(z)
    y2242(z) :- y224(z)
    y2243(z) :- y224(z)
    y2244(z) :- y224(z)
    y2245(z) :- y224(z)

    y2311(z) :- y231(z)
    y2312(z) :- y231(z)
    y2313(z) :- y231(z)
    y2314(z) :- y231(z)
    y2315(z) :- y231(z)
    y2321(z) :- y232(z)
    y2322(z) :- y232(z)
    y2323(z) :- y232(z)
    y2324(z) :- y232(z)
    y2325(z) :- y232(z)
    y2331(z) :- y233(z)
    y2332(z) :- y233(z)
    y2333(z) :- y233(z)
    y2334(z) :- y233(z)
    y2335(z) :- y233(z)
    y2341(z) :- y234(z)
    y2342(z) :- y234(z)
    y2343(z) :- y234(z)
    y2344(z) :- y234(z)
    y2345(z) :- y234(z)
  }

  test("simple alias removal") {
    val adjacency = Map(
      0 -> Seq(),
      1 -> Seq(0),
      2 -> Seq(1),
    )

    val graph = new PrecedenceGraph(using new NS())
    for ((node, deps) <- adjacency) {
      graph.addNode(node, deps)
      graph.idbs.add(node)
    }

    assertEquals(
      graph.topSort(2),
      Seq(0, 1, 2),
    )
    graph.updateNodeAlias(2, mutable.Map(1 -> 0))
    assertEquals(
      graph.topSort(2),
      Seq(0, 2),
    )
  }

  test("consecutive aliases removal") {
    val adjacencyList = Map(
      0 -> Seq(),
      1 -> Seq(0),
      2 -> Seq(1),
      3 -> Seq(2),
    )

    val graph = new PrecedenceGraph(using new NS())
    for ((node, deps) <- adjacencyList) {
      graph.addNode(node, deps)
      graph.idbs.add(node)
    }

    assertEquals(
      graph.topSort(3),
      Seq(0, 1, 2, 3),
    )
    graph.updateNodeAlias(3, mutable.Map(2 -> 1, 1 -> 0))
    assertEquals(
      graph.topSort(3),
      Seq(0, 3),
    )
  }
}
