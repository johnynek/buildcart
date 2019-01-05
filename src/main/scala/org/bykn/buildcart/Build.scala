package org.bykn.buildcart

import cats.{Applicative, Eq, Monad, Order}
import cats.data.StateT

/**
 * This runs a build for a given k by updating the state of the store
 */
trait Build[+Ctx[_[_]], M[_], I, K, V] {
  def update(tsks: Tasks[Ctx, M, K, V], key: K, store: Store[I, K, V]): M[Store[I, K, V]]
}

object Build {
  /**
   * A naive applicative builder that rebuilds each time an item is needed
   */
  def busy[M[_]: Monad, K: Eq, V]: Build[Applicative, M, Unit, K, V] =
    new Build[Applicative, M, Unit, K, V] {
      def update(tsks: Tasks[Applicative, M, K, V], key: K, store: Store[Unit, K, V]): M[Store[Unit, K, V]] = {
        def fetch(k: K): StateT[M, Store[Unit, K, V], V] =
          tsks.get(k) match {
            case None => StateT.get[M, Store[Unit, K, V]].map(_.getValue(k))
            case Some(t) =>
              for {
                v <- t.run(fetch(_))
                _ <- StateT.modify[M, Store[Unit, K, V]](_.putValue(k, v))
              } yield v
          }


        fetch(key).runS(store)
      }
    }

  /**
   * This is a build, as defined in the paper, that implements
   * Shake's approach of suspending scheduling and verifying rebuilding
   */
  def shake[M[_]: Monad, K: Order, V: Hashable]: Build[Monad, M, VT[K, V], K, V] =
    Scheduler.suspending[M, VT[K, V], K, V].schedule(Rebuilder.vtRebuilder[M, K, V])
}

