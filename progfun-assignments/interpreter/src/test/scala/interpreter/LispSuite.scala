package interpreter

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Lisp._

@RunWith(classOf[JUnitRunner])
class LispSuite extends FunSuite {

  val expr1 = "(+ 1 2)"
  test("addition") {
    assert(string2lisp(expr1).toString === "List('+, 1, 2)")
    assert(evaluate(expr1) === 3)
  }
}