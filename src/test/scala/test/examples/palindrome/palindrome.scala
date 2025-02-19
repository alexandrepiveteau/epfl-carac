package test.examples.palindrome

import datalog.dsl.{Constant, Program}
import test.ExampleTestGenerator

import java.nio.file.Paths
class palindrome_test extends ExampleTestGenerator("palindrome") with palindrome
trait palindrome {
  val toSolve = "palindrome"
  def pretest(program: Program): Unit = {
    val palindrome = program.relation[Constant]("palindrome")

    val p = program.relation[Constant]("p")
    val palin_aux = program.relation[Constant]("palin_aux")
    val str = program.relation[Constant]("str")
    val len = program.relation[Constant]("len")

    val x, s, t, r, a, y, l = program.variable()
    val any1, any2, any3 = program.variable()
    
    p(0) :- ()
    p(x) :- ( str(any1,any2,any3,x) )
    
    palin_aux(s,x,x) :- ( str(s,any1,any2,any3), p(x) )
    palin_aux(s,x,y) :- ( str(s,x,any1,y) )
    palin_aux(s,x,y) :- ( str(s,x,a,t), palin_aux(s,t,r), str(s,r,a,y) )
    
    palindrome(s) :- ( palin_aux(s,0,l), len(s,l) )
    
    str(0,0,"h",1) :- ()
    str(0,1,"e",2) :- ()
    str(0,2,"l",3) :- ()
    str(0,3,"l",4) :- ()
    str(0,4,"o",5) :- ()
    len(0,5) :- ()
    
    str(1,0,"r",1) :- ()
    str(1,1,"a",2) :- ()
    str(1,2,"c",3) :- ()
    str(1,3,"e",4) :- ()
    str(1,4,"c",5) :- ()
    str(1,5,"a",6) :- ()
    str(1,6,"r",7) :- ()
    len(1,7) :- ()
    
    str(2,0,"n",1) :- ()
    str(2,1,"o",2) :- ()
    str(2,2,"o",3) :- ()
    str(2,3,"n",4) :- ()
    len(2,4) :- ()
    
    str(3,0,"n",1) :- ()
    str(3,1,"e",2) :- ()
    str(3,2,"v",3) :- ()
    str(3,3,"e",4) :- ()
    str(3,4,"r",5) :- ()
    str(3,5,"o",6) :- ()
    str(3,6,"d",7) :- ()
    str(3,7,"d",8) :- ()
    str(3,8,"o",9) :- ()
    str(3,9,"r",10) :- ()
    str(3,10,"e",11) :- ()
    str(3,11,"v",12) :- ()
    str(3,12,"e",13) :- ()
    str(3,13,"n",14) :- ()
    len(3,14) :- ()
  }
}
