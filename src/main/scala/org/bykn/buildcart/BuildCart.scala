package org.bykn.buildcart

import cats.{Applicative, Id, Eq, Functor, Monad, Monoid, Order, Traverse}
import cats.data.{Chain, Const, State, StateT, WriterT}
import scala.collection.immutable.SortedSet

import cats.implicits._

object BuildCart {
  /**
   * This is (currently) an opaque value that we use the equals method on
   */
  trait Hash[A]

  /**
   * In a real system this will probably be
   * the outer most effect monad, which includes
   * all effects possible while completing a task
   */
  trait MonadState[M[_], S] {
    def monad: Monad[M]
    def update[A](fn: S => M[(S, A)]): M[A]
    def modify(fn: S => S): M[Unit] =
      update { s => monad.pure((fn(s), ())) }
    def get: M[S] =
      update { s => monad.pure((s, s)) }
    def put(s: S): M[Unit] =
      update { _ => monad.pure((s, ())) }
  }

  object MonadState {
    implicit def forCatsState[S]: MonadState[State[S, ?], S] =
      new MonadState[State[S, ?], S] {
        def monad = Monad[State[S, ?]]
        def update[A](fn: S => State[S, (S, A)]): State[S, A] =
          State.get[S].flatMap(fn).map(_._2)
      }
  }

  /**
   * A typeclass for hashing values.
   */
  trait Hashable[A] {
    def hash(a: A): Hash[A]
  }

  object Hashable {
    def hash[A](a: A)(implicit h: Hashable[A]): Hash[A] =
      h.hash(a)
  }

  /**
   * the state of the build system. There is an
   * extended that value I, and also a mapping for
   * all keys to the values that have been built
   */
  trait Store[I, K, V] {
    def getInfo: I
    def putInfo(i: I): Store[I, K, V]
    def getValue(k: K)(implicit eqK: Eq[K]): V
    def putValue(k: K, v: V)(implicit eqK: Eq[K]): Store[I, K, V]
    def getHash(k: K)(implicit HV: Hashable[V], EK: Eq[K]): Hash[V] =
      HV.hash(getValue(k))
  }

  object Store {
    private case class StoreImpl[I, K, V](
      getInfo: I,
      items: Map[K, V],
      fn: K => V) extends Store[I, K, V] {

        def putInfo(i: I) = StoreImpl(i, items, fn)
        def getValue(k: K)(implicit eqK: Eq[K]): V =
          items.getOrElse(k, fn(k))

        def putValue(k: K, v: V)(implicit eqK: Eq[K]) =
          StoreImpl(getInfo, items.updated(k, v), fn)
    }

    def init[I, K, V](i: I)(fn: K => V): Store[I, K, V] =
      StoreImpl(i, Map.empty, fn)
  }

  trait StoreM[M[_], I, K, V] {
    def getInfo: M[I]
    def putInfo(info: I): M[Unit]
    def getValue(k: K)(implicit eqK: Eq[K]): M[V]
    def putValue(k: K, v: V)(implicit eqK: Eq[K]): M[Unit]
  }

  /**
   * this is a doubly higher kinded function for transforming
   * constraints (e.g. MonadState to Monad or Monad to Applicative,
   * ApplicativeError to Applicative, etc...
   */
  trait FunctionKK[A[_[_]], B[_[_]]] {
    def apply[F[_]](a: A[F]): B[F]
  }

  trait Absorb[F[_], G[_]] {
    def absorb[A](f: F[A]): G[A]
    def absorb1[A](gf: G[F[A]]): G[A]
  }

  object Absorb {
    implicit def absorbId[G[_]: Applicative]: Absorb[Id, G] =
      new Absorb[Id, G] {
        def absorb[A](a: A): G[A] = a.pure[G]
        def absorb1[A](gf: G[A]): G[A] = gf
      }

    implicit def absorbStateT[M[_]: Monad, S]: Absorb[M, StateT[M, S, ?]] =
      new Absorb[M, StateT[M, S, ?]] {
        def absorb[A](a: M[A]): StateT[M, S, A] =
          StateT.liftF(a)
        def absorb1[A](gf: StateT[M, S, M[A]]): StateT[M, S, A] =
          gf.flatMap(absorb(_))
      }

    implicit def absorbWriter[M[_]: Monad, L: Monoid]: Absorb[M, WriterT[M, L, ?]] =
      new Absorb[M, WriterT[M, L, ?]] {
        def absorb[A](a: M[A]): WriterT[M, L, A] =
          WriterT.liftF(a)
        def absorb1[A](gf: WriterT[M, L, M[A]]): WriterT[M, L, A] =
          gf.flatMap(absorb(_))
      }

    def absorbConst[F[_], B](b: B): Absorb[F, Const[B, ?]] =
      new Absorb[F, Const[B, ?]] {
        val c = Const[B, Nothing](b)
        def absorb[A](a: F[A]): Const[B, A] = c.retag
        def absorb1[A](gf: Const[B, F[A]]): Const[B, A] = gf.retag[A]
      }

    def compose[F[_], G[_], H[_]: Functor](first: Absorb[F, G], second: Absorb[G, H]): Absorb[F, H] =
      new Absorb[F, H] {
        def absorb[A](a: F[A]): H[A] =
          second.absorb(first.absorb(a))
        def absorb1[A](gf: H[F[A]]): H[A] =
          second.absorb1(gf.map { fa => first.absorb(fa) })
      }
  }

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
    type StateTask[IR, K, V] = Task[λ[M[_] => MonadState[M, IR]], K, V]

    /**
     * This builds a given key, not very useful except for defining a Monad on Task
     */
    def key[Ctx[_[_]], M[_], K, V](k: K): Task[Ctx, M, K, V] =
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
    //     def pure[A](a: A) = Task.key(a)

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

  /**
   * State for a verifying trace
   */
  trait VT[K, V] {
    def record(k: K, hash: Hash[V], deps: List[(K, Hash[V])]): VT[K, V]
    def verify[M[_]: Monad](k: K, hash: Hash[V])(buildHash: K => M[Hash[V]]): M[Boolean]
  }

  object VT {
    private case class VTImpl[K, V](toMap: Map[K, (Hash[V], List[(K, Hash[V])])]) extends VT[K, V] {
      def record(k: K, hash: Hash[V], deps: List[(K, Hash[V])]): VT[K, V] =
        VTImpl(toMap.updated(k, (hash, deps)))
      // we verify if the hash match, and all the deps match
      def verify[M[_]: Monad](k: K, hash: Hash[V])(buildHash: K => M[Hash[V]]): M[Boolean] =
        toMap.get(k) match {
          case None => Monad[M].pure(false)
          case Some((hv, deps)) =>
            val mfalse = Monad[M].pure(false)
            if (hv !=  hash) mfalse
            else deps.foldM(true) {
              case (false, _) => mfalse
              case (true, (k, hv)) => buildHash(k).map(_ == hv)
            }
        }

    }

    def empty[K, V]: VT[K, V] = VTImpl(Map.empty)
  }

  /**
   * This is just an alias for a map of Tasks
   */
  sealed trait Tasks[-Ctx[_[_]], M[_], K, V] {
    def get(k: K): Option[Task[Ctx, M, K, V]]
  }

  object Tasks {
    implicit class InvariantTasks[Ctx[_[_]], M[_], K, V](val tasks: Tasks[Ctx, M, K, V]) extends AnyVal {
      def ++(that: Tasks[Ctx, M, K, V]): Tasks[Ctx, M, K, V] =
        (tasks, that) match {
          case (TaskMap(m1), TaskMap(m2)) => TaskMap(m1 ++ m2)
        }
    }

    implicit class ApplicativeTasks[M[_], K, V](val tasks: Tasks[Applicative, M, K, V]) extends AnyVal {
      def absorb[M1[_]](implicit a: Absorb[M, M1]): Tasks[Applicative, M1, K, V] =
        tasks match {
          case TaskMap(m) => TaskMap(m.map { case (k, t) => (k, t.absorb[M1]) })
        }
    }

    private case class TaskMap[Ctx[_[_]], M[_], K, V](
      toMap: Map[K, Task[Ctx, M, K, V]]) extends Tasks[Ctx, M, K, V] {
        def get(k: K) = toMap.get(k)
      }
    def one[Ctx[_[_]], M[_], K, V](k: K, t: Task[Ctx, M, K, V]): Tasks[Ctx, M, K, V] =
      TaskMap(Map((k, t)))

    def apply[Ctx[_[_]], M[_], K, V](tasks: (K, Task[Ctx, M, K, V])*): Tasks[Ctx, M, K, V] =
      TaskMap(Map(tasks: _*))
  }

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
}
