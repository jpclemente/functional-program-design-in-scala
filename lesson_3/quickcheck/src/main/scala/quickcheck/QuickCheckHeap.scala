package quickcheck

import common._
import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

import scala.util.Random

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  val genMap: Gen[Map[Int, Int]] = oneOf(
    const(Map.empty[Int,Int]),
    for {
      k <- arbitrary[Int]
      v <- arbitrary[Int]
      m <- oneOf(const(Map.empty[Int,Int]), genMap)
    } yield m.updated(k, v)
  )

  lazy val genHeap: Gen[H] = oneOf(
    empty,
    for {
      k <- arbitrary[Int]
      h <- oneOf(empty, genHeap)
    } yield const(insert(k,h))
  )

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

}
