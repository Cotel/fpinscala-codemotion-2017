package org.hablapps.typeclasses
package exercise1

import org.scalatest._

/* Exercise 1
 *
 * Let's put into practice all the concepts we learned in this typeclass section,
 * for this purpose we're going to use another common typeclass, named `Show` that
 * simply gives us the ability to convert a value to a `String`. People with
 * Java knowledge may thing of this as the `toString` method every `Object` has
 * due to inheritance. Using typeclasses we decouple this behavior, as we saw
 * in previous steps.
 */
class ShowSpec extends FunSpec with Matchers {

  // 1. Typeclass
  trait Show[A] {
    def show(a: A): String
  }

  // 2. Generic functions:
  //
  //    Define the following functions that make use of `Show` typeclass.
  //    Using `toString` method in this section is not allowed :)

  // 2.1. This function takes a list of elements of type `A` and returns the
  //      third element in `String` format
  def thirdString[A](l: List[A])(S: Show[A]): Option[String] =
    l.drop(2).headOption.map(S.show)

  // 2.2. This function takes two `Int`s and prints the sum in the following format:
  //      > sum(5, 8) // "5 + 8 = 13"
  def sum(i1: Int, i2: Int)(S: Show[Int]): String = s"$i1 + $i2 = ${S.show(i1 + i2)}"

  // 3. Typeclass instances

  // 3.1. Give an instance for `Int`s
  //      > show(5) // "5"
  lazy val intShow: Show[Int] = (a: Int) => a.toString

  // 3.2. Give an instance for `String`s
  //      > show("hello") // "hello"
  lazy val stringShow: Show[String] = (a: String) => a

  // 3.3. Give an instance for `Person`s
  //      > show(Person("Alberto", 28)) // "Alberto - 28"
  case class Person(name: String, age: Int)

  def personShow(SS: Show[String], IS: Show[Int]): Show[Person] = (a: Person) => s"${a.name} - ${a.age}"

  // 4. Execution
  //
  //    To run the tests, replace `ignore` for `describe` and run them
  //    > testOnly package org.hablapps.typeclasses.exercise1.ShowSpec
  describe("thirdString") {
    describe("int") {
      it("should work for list with 3+ elements") {
        thirdString(List(1, 2, 3))(intShow) shouldBe Option("3")
      }
      it("should work for list with 2- elements") {
        thirdString(List(1, 2))(intShow) shouldBe Option.empty[String]
        thirdString(List.empty[Int])(intShow) shouldBe Option.empty[String]
      }
    }
    describe("string") {
      it("should work for list with 3+ elements") {
        thirdString(List("hello", ", ", "world!"))(stringShow) shouldBe Option("world!")
      }
      it("should work for list with 2- elements") {
        thirdString(List("hello", "world!"))(stringShow) shouldBe Option.empty[String]
        thirdString(List.empty[String])(stringShow) shouldBe Option.empty[String]
      }
    }
    describe("person") {
      it("should work for list with 3+ elements") {
        thirdString(List(
          Person("Ana", 28),
          Person("Berto", 38),
          Person("Carlos", 18)))(personShow(stringShow, intShow)) shouldBe Option("Carlos - 18")
      }
      it("should work for list with 2- elements") {
        thirdString(List(
          Person("Ana", 28),
          Person("Berto", 38)))(personShow(stringShow, intShow)) shouldBe Option.empty[String]
        thirdString(List.empty[Person])(personShow(stringShow, intShow)) shouldBe Option.empty[String]
      }
    }
  }

  describe("sum") {
    it("should work") {
      sum(5, 8)(intShow) shouldBe "5 + 8 = 13"
    }
  }

}
