package test.examples.livevariables

import datalog.dsl.{Constant, Program, __, not}
import test.ExampleTestGenerator

class livevariables_test extends ExampleTestGenerator("livevariables") with livevariables
trait livevariables {
  val toSolve = "i"
  def pretest(program: Program): Unit = {
    val v = program.relation[Constant]("v")
    val o = program.relation[Constant]("o")
    val i = program.relation[Constant]("i")
    val gen = program.relation[Constant]("gen")
    val kill = program.relation[Constant]("kill")
    val s = program.relation[Constant]("dependency")

    val x, n, y = program.variable()

    // A single function which computes factorial and fibonacci in order.
    //  1. n = read-int
    //  2. acc = 1
    //  3. i = 0
    //  4. while i < n:
    //  5.   acc = acc * i
    //  6.   i = i + 1
    //  7. print(acc)
    //  8. res = 0
    //  9. a = 0
    // 10. b = 1
    // 11. i = 0
    // 12. while i < acc:
    // 14.   res = res + a
    // 15.   tmp = a + b
    // 16.   a = b
    // 17.   b = tmp
    // 18.   i = i + 1
    // 13. print(res)

    // Instruction dependencies.
    s("1", "2") :- ()
    s("2", "3") :- ()
    s("3", "4") :- ()
    s("4", "5") :- ()
    s("5", "6") :- ()
    s("6", "4") :- ()
    s("4", "7") :- ()
    s("7", "8") :- ()
    s("8", "9") :- ()
    s("9", "10") :- ()
    s("10", "11") :- ()
    s("11", "12") :- ()
    s("12", "13") :- ()
    s("12", "14") :- ()
    s("14", "15") :- ()
    s("15", "16") :- ()
    s("16", "17") :- ()
    s("17", "18") :- ()
    s("18", "12") :- ()

    // Gen and kill sets.
    gen("4", "i") :- ()
    gen("4", "n") :- ()
    gen("5", "acc") :- ()
    gen("5", "i") :- ()
    gen("6", "i") :- ()
    gen("7", "acc") :- ()
    gen("12", "i") :- ()
    gen("12", "acc") :- ()
    gen("13", "res") :- ()
    gen("14", "res") :- ()
    gen("14", "a") :- ()
    gen("15", "a") :- ()
    gen("15", "b") :- ()
    gen("16", "b") :- ()
    gen("17", "tmp") :- ()
    gen("18", "i") :- ()


    kill("1", "n") :- ()
    kill("2", "acc") :- ()
    kill("3", "i") :- ()
    kill("5", "acc") :- ()
    kill("6", "i") :- ()
    kill("8", "res") :- ()
    kill("9", "a") :- ()
    kill("10", "b") :- ()
    kill("11", "i") :- ()
    kill("14", "res") :- ()
    kill("15", "tmp") :- ()
    kill("16", "a") :- ()
    kill("17", "b") :- ()
    kill("18", "i") :- ()

    // Out and in sets.
    v(x) :- gen(__, x)
    v(x) :- kill(__, x)
    i(x, n) :- gen(x, n)
    i(x, n) :- (o(x, n), not(kill(x, n)))
    o(x, n) :- (s(x, y), i(y, n))
  }
}
