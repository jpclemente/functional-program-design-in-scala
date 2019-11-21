package quickcheck

import org.scalacheck.Arbitrary._
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
      k <- arbitrary[A]
      h <- oneOf(const(empty), genHeap)
    } yield insert(k,h)
  )

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  def toList(h:H):List[Int] = if (isEmpty(h)) Nil else findMin(h) :: toList(deleteMin(h))

  /* adding a single element to an empty heap, and then removing this element, should yield the element in question */
  property("min1") = forAll { a: A =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  /* For any heap, adding the minimal element, and then finding it, should return the element in question */
  property("gen1") = forAll { h: H =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }
  /* If you insert any two elements into an empty heap, finding the minimum of the resulting heap should get the
    smallest of the two elements back. */
  property("insert2") = forAll { (a:A, b: A) =>
    val h = insert(b,insert(a, empty))
    findMin(h) == Seq(a, b).min
  }

  /* If you insert an element into an empty heap, then delete the minimum, the resulting heap should be empty. */
  property("delete1") = forAll { a: A =>
    isEmpty(deleteMin(insert(a, empty)))
  }

  /* Given any heap, you should get a sorted sequence of elements when continually finding and deleting minima */
  property("sorted1") = forAll { h: H =>
    def loop(h: H): List[A] = if (isEmpty(h)) Nil else findMin(h)::loop(deleteMin(h))
    val l = loop(h)
    val ls = if (l.isEmpty) Nil else l.sorted
    (l zip ls).foldLeft(List(0))((count, i) => ord.compare(i._1, i._2) :: count) forall (_ == 0)
  }

  /* Finding a minimum of the melding of any two heaps should return a minimum of one or the other */
  property("min2") = forAll { (h1: H, h2: H) =>
    def getMin(h: H): A = if (isEmpty(h)) 0 else findMin(h)
    val min = getMin(meld(h1, h2))
    min == getMin(h1) || min == getMin(h2)

  }

  /* Order should not make any difference when melding any two heaps */
  property("meld1") = forAll { (h1: H, h2: H) =>
    meld(h1, h2) == meld(h2, h1)
  }

  /* Order should not make any difference when inserting elements in a heap */
  property("insert3") = forAll { (a: A, b: A) =>
    insert(a, insert(b, empty)) == insert(b, insert(a, empty))
  }

  /* Meld operation should be associative */
  property("meld2") = forAll { (h:H, i:H, j:H) =>
    val a = meld(meld(h, i), j)
    val b = meld(h, meld(i, j))
    toList(a) == toList(b)
  }
}
