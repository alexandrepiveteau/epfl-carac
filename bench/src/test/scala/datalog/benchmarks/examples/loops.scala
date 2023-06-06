package datalog.benchmarks.examples

import datalog.dsl.{Constant, Program, Term}
import datalog.execution.{NaiveExecutionEngine, SemiNaiveExecutionEngine, StagedExecutionEngine}
import datalog.storage.{DefaultStorageManager, VolcanoStorageManager}
import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole
import test.examples.loops.loops as loops_test

import java.nio.file.{Files, Path, Paths}
import java.util.concurrent.TimeUnit
import scala.collection.{immutable, mutable}
import scala.jdk.StreamConverters.*

@Fork(1)
@State(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
class loops() extends loops_test {

  private val facts = mutable.Map[String, Set[Seq[Term]]]()
  private val expectedFacts = mutable.Map[String, Set[Seq[Term]]]()

  @Setup
  def setup(): Unit = {
    val factdir = Paths.get("..", "src", "test", "scala", "test", "examples", "loops", "facts")
    if (Files.exists(factdir))
      Files.walk(factdir, 1)
        .filter(p => Files.isRegularFile(p))
        .forEach(f => {
          val edbName = f.getFileName.toString.replaceFirst("[.][^.]+$", "")
          val reader = Files.newBufferedReader(f)
          val headers = reader.readLine().split("\t")
          val edbs = reader.lines()
            .map(l => {
              val factInput = l
                .split("\t")
                .zipWithIndex.map((s, i) =>
                (headers(i) match {
                  case "Int" => s.toInt
                  case "String" => s
                  case _ => throw new Exception(s"Unknown type ${headers(i)}")
                }).asInstanceOf[Term]
              ).toSeq
              if (factInput.size != headers.size)
                throw new Exception(s"Input data for fact of length ${factInput.size} but should be ${headers.mkString("[", ", ", "]")}. Line='$l'")
              factInput
            }).toScala(Set)
          reader.close()
          facts(edbName) = edbs
        })
    // Generate expected
    val expDir = Paths.get("..", "src", "test", "scala", "test", "examples", "loops", "expected")
    if (!Files.exists(expDir)) throw new Exception(s"Missing expected directory '$expDir'")
    Files.walk(expDir, 1)
      .filter(p => Files.isRegularFile(p) && p.toString.endsWith(".csv"))
      .forEach(f => {
        val rule = f.getFileName.toString.replaceFirst("[.][^.]+$", "")
        val reader = Files.newBufferedReader(f)
        val headers = reader.readLine().split("\t")
        val expected = reader.lines()
          .map(l => l.split("\t").zipWithIndex.map((s, i) =>
            (headers(i) match {
              case "Int" => s.toInt
              case "String" => s
              case _ => throw new Exception(s"Unknown type ${headers(i)}")
            }).asInstanceOf[Term]
          ).toSeq)
          .toScala(Set)
        expectedFacts(rule) = expected
        reader.close()
      })
  }

  def run(program: Program): Unit = {
    for ((name, facts) <- facts) {
      val r = program.relation[Constant](name)
      for (fact <- facts) {
        r(fact: _*) :- ()
      }
    }

    pretest(program)

    for ((name, facts) <- expectedFacts) {
      val r = program.namedRelation[Constant](name)
      val tuples = program.solve(r.id)
      val expected = facts
      if (tuples != expected) {
        throw new Exception(s"Expected $expected but got $tuples")
      }
    }
  }

  @Benchmark
  def naive_default(x: Blackhole): Unit = {
    val program = Program(NaiveExecutionEngine(DefaultStorageManager()))
    x.consume(run(program))
  }

  @Benchmark
  def naive_volcano(x: Blackhole): Unit = {
    val program = Program(NaiveExecutionEngine(VolcanoStorageManager()))
    x.consume(run(program))
  }

  @Benchmark
  def semiNaive_default(x: Blackhole): Unit = {
    val program = Program(SemiNaiveExecutionEngine(DefaultStorageManager()))
    x.consume(run(program))
  }

  @Benchmark
  def semiNaive_volcano(x: Blackhole): Unit = {
    val program = Program(SemiNaiveExecutionEngine(VolcanoStorageManager()))
    x.consume(run(program))
  }

  @Benchmark
  def staged_default(x: Blackhole): Unit = {
    val program = Program(StagedExecutionEngine(DefaultStorageManager()))
    x.consume(run(program))
  }

  @Benchmark
  def staged_volcano(x: Blackhole): Unit = {
    val program = Program(StagedExecutionEngine(VolcanoStorageManager()))
    x.consume(run(program))
  }
}
