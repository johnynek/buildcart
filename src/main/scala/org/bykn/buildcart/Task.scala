package org.bykn.buildcart

import cats.{Applicative, Monad, Traverse}
import cats.data.{Chain, Const, WriterT}

import cats.implicits._
/**
 * This is a kind of continuation that is generic
 * on the return value
 *
 * These are the "targets" or "rules" of your build
 * system
 *
 * Tasks are contravariant in the context, this means
 * if you have a Task[Applicative, K, V], you can consider
 * it as a Task[Monad, K, V]: we can always run in a more
 * powerful context.
 */
trait Task[-Ctx[_[_]], M[_], +K, V] { self =>
  def run[F[_]](build: K => F[V])(implicit c: Ctx[F], a: Absorb[M, F]): F[V]

  def map[K1](fn: K => K1): Task[Ctx, M, K1, V] =
    new Task[Ctx, M, K1, V] {
      def run[F[_]](build: K1 => F[V])(implicit c: Ctx[F], a: Absorb[M, F]): F[V] =
        self.run(fn.andThen(build))
    }
}

object Task {
  type StateTask[M[_], IR, K, V] = Task[λ[M1[_] => MonadState[M1, IR]], M, K, V]

  /**
   * This builds a given key, not very useful except for defining a Monad on Task
   */
  def alias[Ctx[_[_]], M[_], K, V](k: K): Task[Ctx, M, K, V] =
    new Task[Ctx, M, K, V] {
      def run[F[_]](build: K => F[V])(implicit c: Ctx[F], a: Absorb[M, F]): F[V] = build(k)
    }

  def value[M[_], V](value: M[V]): Task[Applicative, M, Nothing, V] =
    new Task[Applicative, M, Nothing, V] {
      def run[F[_]](build: Nothing => F[V])(implicit c: Applicative[F], a: Absorb[M, F]): F[V] =
        a.absorb(value)
    }

  // implicit def taskMonad[Ctx[_[_]], V]: Monad[Task[Ctx, ?, V]] =
  //   new Monad[Task[Ctx, ?, V]] {
  //     def pure[A](a: A) = Task.alias(a)

  //     override def map[A, B](fa: Task[Ctx, A, V])(fn: A => B): Task[Ctx, B, V] =
  //       fa.map(fn)

  //     override def product[A, B](fa: Task[Ctx, A, V], fb: Task[Ctx, B, V]): Task[Ctx, (A, B), V] =
  //       fa.product(fb)

  //     override def ap[A, B](fab: Task[Ctx, A => B, V])(fb: Task[Ctx, A, V]): Task[Ctx, B, V] =
  //       product(fab, fb).map { case (f, b) => f(b) }

  //     def flatMap[A, B](fa: Task[Ctx, A, V])(fn: A => Task[Ctx, B, V]): Task[Ctx, B, V] =
  //       fa.flatMap(fn)

  //     def tailRecM[A, B](a: A)(fn: A => Task[Ctx, Either[A, B], V]): Task[Ctx, B, V] =
  //       new Task[Ctx, B, V] {
  //         def run[F[_]](build: B => F[V])(implicit c: Ctx[F]): F[V] = {
  //           // TODO: this is not really stack safe.
  //           // if we had Ctx is Monad or Defer, I think we are
  //           // okay here.
  //           def go(a: A): F[V] =
  //             fn(a).run {
  //               case Left(a) => go(a)
  //               case Right(b) => build(b)
  //             }

  //           go(a)
  //         }
  //       }
  //   }

  implicit class InvariantTask[Ctx[_[_]], M[_], K, V](val task: Task[Ctx, M, K, V]) extends AnyVal {
    def reconstrain[Ctx2[_[_]]](fn: FunctionKK[Ctx2, Ctx]): Task[Ctx2, M, K, V] =
      new Task[Ctx2, M, K, V] {
        def run[F[_]](build: K => F[V])(implicit c: Ctx2[F], a: Absorb[M, F]): F[V] =
          task.run(build)(fn(c), a)
      }

    def flatMap[K1](fn: K => Task[Ctx, M, K1, V]): Task[Ctx, M, K1, V] =
      new Task[Ctx, M, K1, V] {
        def run[F[_]](build: K1 => F[V])(implicit c: Ctx[F], a: Absorb[M, F]): F[V] =
          task.run { k =>
            fn(k).run(build)
          }
      }

    // type MapRes[F[_]] = (F[V], Ctx[F])
    // type IdK[F[_]] = F[V]

    // def mapResult(fn: FunctionKK[MapRes, IdK]): Task[Ctx, M, K, V] =
    //   new Task[Ctx, M, K, V] {
    //     def run[F[_]](build: K => F[V])(implicit c: Ctx[F], a: Absorb[M, F]): F[V] =
    //       task.run { k =>
    //         val fv = build(k)
    //         fn((fv, c))
    //       }
    //   }

    // def product[K1](that: Task[Ctx, K1, V]): Task[Ctx, (K, K1), V] =
    //   new Task[Ctx, (K, K1), V] {
    //     def run[F[_]](build: ((K, K1)) => F[V])(implicit c: Ctx[F]): F[V] =
    //       task.run { a =>
    //         that.run { b =>
    //           build((a, b))
    //         }
    //       }
    //   }
  }

  implicit class ApplicativeTask[M[_], K, V](val task: Task[Applicative, M, K, V]) extends AnyVal {
    // Get the list of dependencies for a given K
    // I think this also works for Alternative
    def dependencies: List[K] =
      task.run[Const[Chain[K], ?]] { k =>
          Const(Chain.one(k))
        }(implicitly, Absorb.absorbConst(Chain.empty))
        .getConst
        .toList

    def absorb[M1[_]](implicit abs: Absorb[M, M1]): Task[Applicative, M1, K, V] =
      new Task[Applicative, M1, K, V] {
        def run[F[_]](build: K => F[V])(implicit c: Applicative[F], a: Absorb[M1, F]): F[V] =
          task.run(build)(c, Absorb.compose(abs, a))
      }

  }

  implicit class MonadTask[G[_], K, V](val task: Task[Monad, G, K, V]) extends AnyVal {
    // // Run without attempting to update any dependencies
    // def compute[I](store: Store[I, K, V])(implicit EK: Eq[K], abs: ): V =
    //   task.run[Id] { k => store.getValue(k) }

    def track[M[_]: Monad](fn: K => M[V])(implicit a: Absorb[G, M]): M[(Chain[(K, V)], V)] = {

      def fetchTrack(k: K): WriterT[M, Chain[(K, V)], V] =
        for {
          v <- WriterT.liftF[M, Chain[(K, V)], V](fn(k))
          _ <- WriterT.tell[M, Chain[(K, V)]](Chain.one((k, v)))
        } yield v

      implicit val absW: Absorb[G, WriterT[M, Chain[(K, V)], ?]] =
        Absorb.compose(a, Absorb.absorbWriter[M, Chain[(K, V)]])

      task.run(fetchTrack).run
    }
  }

  def traverse[T[_]: Traverse, M[_], K, V](tasks: T[K])(fn: T[V] => M[V]): Task[Applicative, M, K, V] =
    new Task[Applicative, M, K, V] {
      def run[F[_]](build: K => F[V])(implicit ctx: Applicative[F], a: Absorb[M, F]): F[V] =
        a.absorb1(tasks.traverse(build).map(fn))
    }
}
