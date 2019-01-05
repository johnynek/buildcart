package org.bykn.buildcart

import cats.{Applicative, Monad}

import cats.implicits._
/**
 * This is a type that defines how to decide when to rebuild a task from
 * the current value
 */
trait Rebuilder[+Ctx[_[_]], M[_], IR, K, V] {
  def apply(k: K, v: V, task: Task[Ctx, M, K, V]): Task.StateTask[M, IR, K, V]
}

object Rebuilder {
  implicit class ApplicativeRebuilder[M[_], IR, K, V](val rebuilder: Rebuilder[Applicative, M, IR, K, V]) extends AnyVal {
    def shouldRebuild(k: K, v: V, task: Task[Applicative, M, K, V], ir: IR): List[K] = {
      val stateTask = rebuilder(k, v, task)
      ???
    }
  }

  def dirtyBit[M[_], K, V]: Rebuilder[Monad, M, K => Boolean, K, V] =
    new Rebuilder[Monad, M, K => Boolean, K, V] {
      def apply(k: K, v: V, task: Task[Monad, M, K, V]): Task.StateTask[M, K => Boolean, K, V] =
        new Task.StateTask[M, K => Boolean, K, V] {
          def run[F[_]](build: K => F[V])(implicit c: MonadState[F, K => Boolean], a: Absorb[M, F]): F[V] =
            c.monad.flatMap(c.get) { isDirty =>
              if (isDirty(k)) task.run(build)(c.monad, a)
              else c.monad.pure(v)
          }
        }
    }

  /**
   * This is the verifying trace rebuilder
   */
  def vtRebuilder[M[_], K, V: Hashable]: Rebuilder[Monad, M, VT[K, V], K, V] =
    new Rebuilder[Monad, M, VT[K, V], K, V] {
      def apply(k: K, v: V, task: Task[Monad, M, K, V]): Task.StateTask[M, VT[K, V], K, V] =
        new Task.StateTask[M, VT[K, V], K, V] {
          def run[F[_]](build: K => F[V])(implicit c: MonadState[F, VT[K, V]], a: Absorb[M, F]): F[V] = {
            implicit val monad = c.monad

            def rebuild: F[V] =
              for {
                depsV <- task.track(build)
                (deps, newValue) = depsV
                newHash = Hashable.hash(newValue)
                depsHash = deps.iterator.map { case (k, v) => (k, Hashable.hash(v)) }.toList
                _ <- c.modify(_.record(k, newHash, depsHash))
              } yield newValue

            for {
              vt <- c.get
              upToDate <- vt.verify(k, Hashable.hash(v))(build.andThen(_.map(Hashable.hash(_))))
              res <- if (upToDate) monad.pure(v) else rebuild
            } yield res

          }
        }
    }
}

