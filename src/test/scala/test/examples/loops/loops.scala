package test.examples.loops

import datalog.dsl.{Constant, Program}
import test.ExampleTestGenerator

class loops_test extends ExampleTestGenerator("loops") with loops

trait loops {
  val toSolve = "result"

  def pretest(program: Program) = {
    val succ = program.namedRelation[Constant]("succ")
    val start = program.relation[Constant]("start")
    val r1 = program.relation[Constant]("r1")
    val r2 = program.relation[Constant]("r2")
    val r3 = program.relation[Constant]("r3")
    val r4 = program.relation[Constant]("r4")
    val r5 = program.relation[Constant]("r5")
    val result = program.relation[Constant](toSolve)
    val x, y = program.variable()

    val min = "0"
    val max = "399"

    start(min) :- ()

    // r1: 0 --> 399
    r1(x) :- start(x)
    r1(x) :- (r1(y), succ(y, x))

    // r2: 399 -> 0
    r2(max) :- r1(max)
    r2(x) :- (r2(y), succ(x, y))

    // r3: 0 -> 399
    r3(min) :- r2(min)
    r3(x) :- (r3(y), succ(y, x))

    // r4: 399 -> 0
    r4(max) :- r3(max)
    r4(x) :- (r4(y), succ(x, y))

    // r5: 0 -> 399
    r5(min) :- r4(min)
    r5(x) :- (r5(y), succ(y, x))

    // Result will contain only a single tuple.
    result("1") :- r5(max)
  }
}