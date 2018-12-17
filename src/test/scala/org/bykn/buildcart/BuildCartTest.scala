package org.bykn.buildcart

import org.scalatest.FunSuite
import cats.{Applicative, Monad}

import cats.implicits._

class BuildCartTest extends FunSuite {
  import BuildCart._

  object SpreadSheet1 {
    type T = Task[Applicative, String, Int]

    val b1: T = new Task[Applicative, String, Int] {
      def run[F[_]](build: String => F[Int])(implicit ctx: Applicative[F]): F[Int] =
        (build("a1"), build("a2")).mapN(_ + _)
    }

    val b2: T = new Task[Applicative, String, Int] {
      def run[F[_]](build: String => F[Int])(implicit ctx: Applicative[F]): F[Int] =
        build("b1").map(_ * 2)
    }

    val tasks = Tasks("b1" -> b1, "b2" -> b2)
    val tasksM = Tasks[Monad, String, Int]("b1" -> b1, "b2" -> b2)
  }



  test("SpreadSheet1 example build using busy") {
    import SpreadSheet1._

    assert(b1.dependencies == List("a1", "a2"))
    assert(b2.dependencies == List("b1"))

    val store = Store.init[Unit, String, Int](()) {
      case "a1" => 10
      case "a2" => 20
      case _ => 0
    }

    val res = Build.busy[String, Int].update(tasks, "b2", store)
    assert(res.getValue("b1") == 30)
    assert(res.getValue("b2") == 60)
  }

  test("SpreadSheet1 example build using shake") {
    import SpreadSheet1._

    val store1 = Store.init[VT[String, Int], String, Int](VT.empty) {
      case "a1" => 10
      case "a2" => 20
      case _ => 0
    }


    implicit val hashInt: Hashable[Int] =
      new Hashable[Int] {
        private case class IntHash(toInt: Int) extends Hash[Int]
        def hash(i: Int): Hash[Int] = IntHash(i)
      }

    val res1 = Build.shake[String, Int].update(tasksM, "b2", store1)
    assert(res1.getValue("b1") == 30)
    assert(res1.getValue("b2") == 60)
  }
}
