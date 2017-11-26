package org.hablapps.typeclasses
package exercise3

import org.scalatest._

/* Exercise 3
 *
 * Take the `Order` interface we created in section 1 and transform it
 * into a typeclass. Tasks include:
 *   - Modify the interface to make it a typeclass
 *   - Modify the functions `greatest`, `quicksort` & `greatest2`
 *   - Modify the instances for `Person` & `Int`
 *   - Modify the tests to use the new functions & instances
 */
class OrderSpec extends FunSpec with Matchers {

  // 1. Typeclass
  trait Order[A] {
    def compare(a1: A, a2: A): Int

    def gt(a1: A, a2: A): Boolean = compare(a1, a2) > 0
    def eq(a1: A, a2: A): Boolean = compare(a1, a2) == 0
    def lt(a1: A, a2: A): Boolean = compare(a1, a2) < 0
  }

  object Order {
    def apply[A](implicit O: Order[A]) = O

    object syntax {
      implicit class OrderOperations[A](self: A)(implicit O: Order[A]) {
        def >(other: A) = O.gt(self, other)
        def ===(other: A) = O.eq(self, other)
        def <(other: A) = O.lt(self, other)
      }
    }
  }

  // 2. Generic function
  import Order.syntax._
  def greatest[A: Order](l: List[A]): Option[A] =
    l.foldLeft(Option.empty[A]) {
      case (Some(max), a) if a < max => Option(max)
      case (_, a) => Option(a)
    }

  def quicksort[A: Order](l: List[A]): List[A] =
    l match {
      case h :: t =>
        val (lower, greater) = t.partition(_ < h)
        quicksort(lower) ::: h :: quicksort(greater)
      case Nil => Nil
    }

  def greatest2[A: Order](l: List[A]): Option[A] =
    quicksort(l).reverse.headOption

  // 3. Typeclass instance
  implicit val intOrd: Order[Int] = (a1: Int, a2: Int) => a1 - a2

  case class Person(name: String, age: Int)
  implicit val personOrd: Order[Person] = (a1: Person, a2: Person) => intOrd.compare(a1.age, a2.age)

  // 4. Execution
  describe("greatest") {
    describe("person") {
      it("should work for non-empty lists") {
        greatest(Person("Félix", 5) ::
                 Person("Alberto", 3) ::
                 Person("Alfredo", 7) ::
                 Person("Sergio", 2) :: Nil) shouldBe Option(Person("Alfredo", 7))
      }
      it("should work for empty lists") {
        greatest(List.empty[Person]) shouldBe None
      }
    }
    describe("int") {
      it("should work for non-empty lists") {
        greatest(5 :: 3 :: 7 :: 2 :: Nil) shouldBe Option(7)
      }
      it("should work for empty lists") {
        greatest(List.empty[Int]) shouldBe None
      }
    }
  }

  describe("quicksort") {
    describe("person") {
      it("should work for non-empty lists") {
        quicksort(
          Person("Félix", 5) ::
          Person("Alberto", 3) ::
          Person("Alfredo", 7) ::
          Person("Sergio", 2) :: Nil) shouldBe
            Person("Sergio", 2) ::
            Person("Alberto", 3) ::
            Person("Félix", 5) ::
            Person("Alfredo", 7) :: Nil
      }
      it("should work for empty lists") {
        quicksort(List.empty[Person]) shouldBe List.empty[Person]
      }
    }
    describe("int") {
      it("should work for non-empty lists") {
        quicksort(5 :: 3 :: 7 :: 2 :: Nil) shouldBe 2 :: 3 :: 5 :: 7 :: Nil
      }
      it("should work for empty lists") {
        quicksort(List.empty[Int]) shouldBe List.empty[Int]
      }
    }
  }

  describe("greatest2") {
    describe("person") {
      it("should work for non-empty lists") {
        greatest2(Person("Félix", 5) ::
                 Person("Alberto", 3) ::
                 Person("Alfredo", 7) ::
                 Person("Sergio", 2) :: Nil) shouldBe Option(Person("Alfredo", 7))
      }
      it("should work for empty lists") {
        greatest2(List.empty[Person]) shouldBe None
      }
    }
    describe("int") {
      it("should work for non-empty lists") {
        greatest2(5 :: 3 :: 7 :: 2 :: Nil) shouldBe Option(7)
      }
      it("should work for empty lists") {
        greatest2(List.empty[Int]) shouldBe None
      }
    }
  }

}
