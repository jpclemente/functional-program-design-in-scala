package calculator

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._
import calculator.Calculator._
import calculator.Signal._
import calculator.TweetLength._
import calculator.Polynomial._

@RunWith(classOf[JUnitRunner])
class CalculatorSuite extends FunSuite with ShouldMatchers {

  /******************
   ** TWEET LENGTH **
   ******************/

  def tweetLength(text: String): Int =
    text.codePointCount(0, text.length)

  test("tweetRemainingCharsCount with a constant signal") {
    val result = tweetRemainingCharsCount(Var("hello world"))
    assert(result() == MaxTweetLength - tweetLength("hello world"))

    val tooLong = "foo" * 200
    val result2 = tweetRemainingCharsCount(Var(tooLong))
    assert(result2() == MaxTweetLength - tweetLength(tooLong))
  }

  test("tweetRemainingCharsCount with a supplementary char") {
    val result = tweetRemainingCharsCount(Var("foo blabla \uD83D\uDCA9 bar"))
    assert(result() == MaxTweetLength - tweetLength("foo blabla \uD83D\uDCA9 bar"))
  }

  test("colorForRemainingCharsCount with a constant signal") {
    val resultGreen1 = colorForRemainingCharsCount(Var(52))
    assert(resultGreen1() == "green")
    val resultGreen2 = colorForRemainingCharsCount(Var(15))
    assert(resultGreen2() == "green")

    val resultOrange1 = colorForRemainingCharsCount(Var(12))
    assert(resultOrange1() == "orange")
    val resultOrange2 = colorForRemainingCharsCount(Var(0))
    assert(resultOrange2() == "orange")

    val resultRed1 = colorForRemainingCharsCount(Var(-1))
    assert(resultRed1() == "red")
    val resultRed2 = colorForRemainingCharsCount(Var(-5))
    assert(resultRed2() == "red")
  }

  /******************
   ** CALCULATOR **
   ******************/

  test("computeDelta with different values") {
    val result1 = Polynomial.computeDelta(Var(1), Var(0), Var(1))
    assert(result1() == -4)
    val result2 = Polynomial.computeDelta(Var(1), Var(-2), Var(-3))
    assert(result2() == 16)
  }

  test("computeSolutions with different values") {
    val result = (x: Var[Double], y: Var[Double], z: Var[Double]) =>
      Polynomial.computeSolutions(x, y, z, Polynomial.computeDelta(x, y, z))

    assert(result(Var(1), Var(0), Var(1))() == Set())
    assert(result(Var(1), Var(-2), Var(-3))() == Set(3, -1))
  }

  /******************
   ** CALCULATOR **
   ******************/

  test("computeValues with cyclic reference") {

    val namedRef = Map[String, Signal[Expr]](
      "a" -> Signal(Plus(Ref("b"),Literal(3))),
      "b" -> Signal(Minus(Ref("a"),Literal(5))),
      "c" -> Signal(Plus(Literal(2),Literal(8)))
    )

    val result = computeValues(namedRef).mapValues(x => x())

    assert(result("a").isNaN)
    assert(result("b").isNaN)
    assert(result("c") == 10)
  }

  test("computeValues with reference to non existing variable") {

    val namedRef = Map[String, Signal[Expr]](
      "a" -> Signal(Plus(Ref("b"),Literal(3))),
      "b" -> Signal(Plus(Ref("c"),Literal(9)))
    )

    val result = computeValues(namedRef).mapValues(x => x())

    assert(result("a").isNaN)
    assert(result("b").isNaN)
  }

  test("computeValues with correct variables") {

    val namedRef = Map[String, Signal[Expr]](
      "a" -> Signal(Plus(Ref("b"),Literal(3))),
      "b" -> Signal(Minus(Ref("c"),Literal(9))),
      "c" -> Signal(Literal(10))
    )

    val result = computeValues(namedRef).mapValues(x => x())

    val expected = Map[String, Double](
      "a" -> 4,
      "b" -> 1,
      "c" -> 10
    )

    assert(result == expected)
  }

}
