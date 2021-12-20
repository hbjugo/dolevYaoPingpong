import org.scalatest.funsuite.AnyFunSuite
import dolevYao.{S, R, verify, Participant}

class DolevYaoTest extends AnyFunSuite {
  test("Example 1") {
    val example1: List[(Participant, String)] = List(
      (S, "Er"),
      (R, "Es Dr")
    )
    assert(verify(example1) == false)
  }
  test("Example 2") {
    val example2: List[(Participant, String)] = List(
      (S, "Er is"),
      (R, "Es ds Dr")
    )
    assert(verify(example2) == true)
  }
  test("Example 3") {
    val example3: List[(Participant, String)] = List(
      (S, "Er is Er"),
      (R, "Es Dr ds Dr")
    )
    assert(verify(example3) == false)
  }
}
