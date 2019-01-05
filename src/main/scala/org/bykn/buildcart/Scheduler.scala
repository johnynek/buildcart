package org.bykn.buildcart

import cats.{Monad, Order}
import cats.data.StateT
import scala.collection.immutable.SortedSet

import cats.implicits._
/**
 * A scheduler computes the order in which to run a build. It is given
 * a rebuilder in order to return a Build
 */
trait Scheduler[Ctx[_[_]], M[_], I, IR, K, V] {
  def schedule(r: Rebuilder[Ctx, M, IR, K, V]): Build[Ctx, M, I, K, V]
}

object Scheduler {
  /**
   * This is the suspending tactic described in the paper
   */
  def suspending[M[_]: Monad, I, K: Order, V]: Scheduler[Monad, M, I, I, K, V] =
    new Scheduler[Monad, M, I, I, K, V] {
      def schedule(r: Rebuilder[Monad, M, I, K, V]): Build[Monad, M, I, K, V] =
        new Build[Monad, M, I, K, V] {
          type S = (Store[I, K, V], SortedSet[K])
          val monadState: MonadState[StateT[M, S, ?], I] =
            new MonadState[StateT[M, S, ?], I] {
              def monad = Monad[StateT[M, S, ?]]
              def update[A](fn: I => StateT[M, S, (I, A)]): StateT[M, S, A] =
                for {
                  sd <- StateT.get[M, S]
                  (store, _) = sd
                  ia <- fn(store.getInfo)
                  (newI, a) = ia
                  _ <- StateT.modify[M, S] { case (store, d) => (store.putInfo(newI), d) }
                } yield a
            }

          def update(tsks: Tasks[Monad, M, K, V], key: K, store: Store[I, K, V]): M[Store[I, K, V]] = {
            def run(t: Task.StateTask[M, I, K, V], fn: K => StateT[M, S, V]): StateT[M, S, V] =
              t.run(fn)(monadState, Absorb.absorbStateT)

            def fetch(key: K): StateT[M, S, V] =
              StateT.get[M, S]
                .flatMap { case (store, done) =>
                  val value = store.getValue(key)
                  tsks.get(key) match {
                    case Some(task) if !done(key) =>
                      // we need to run
                      val newTask = r(key, value, task)
                      for {
                        newValue <- run(newTask, fetch(_))
                        _ <- StateT.modify[M, S] { case (str, set) =>
                          (str.putValue(key, newValue), set + key)
                        }
                      } yield newValue
                    case _ =>
                      StateT.pure(value)
                  }
                }

            fetch(key).runS((store, SortedSet.empty[K](Order[K].toOrdering))).map(_._1)
          }
        }
    }
}
