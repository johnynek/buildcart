package org.bykn.buildcart

import cats.{Applicative, Id, Eq, Monad, Order, Traverse}
import cats.data.{Chain, Const, State, WriterT}
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

  /**
   * this is a doubly higher kinded function for transforming
   * constraints (e.g. MonadState to Monad or Monad to Applicative,
   * ApplicativeError to Applicative, etc...
   */
  trait FunctionKK[A[_[_]], B[_[_]]] {
    def apply[F[_]](a: A[F]): B[F]
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
  trait Task[-Ctx[_[_]], +K, V] { self =>
    def run[F[_]](build: K => F[V])(implicit c: Ctx[F]): F[V]

    def map[K1](fn: K => K1): Task[Ctx, K1, V] =
      new Task[Ctx, K1, V] {
        def run[F[_]](build: K1 => F[V])(implicit c: Ctx[F]): F[V] =
          self.run(fn.andThen(build))
      }
  }

  object Task {
    type StateTask[IR, K, V] = Task[({type MS[M[_]] = MonadState[M, IR]})#MS, K, V]

    /**
     * This builds a given key, not very useful except for defining a Monad on Task
     */
    def key[Ctx[_[_]], K, V](k: K): Task[Ctx, K, V] =
      new Task[Ctx, K, V] {
        def run[F[_]](build: K => F[V])(implicit c: Ctx[F]): F[V] = build(k)
      }

    def value[V](value: => V): Task[Applicative, Nothing, V] =
      new Task[Applicative, Nothing, V] {
        def run[F[_]](build: Nothing => F[V])(implicit c: Applicative[F]): F[V] =
          c.pure(value)
      }

    implicit def taskMonad[Ctx[_[_]], V]: Monad[Task[Ctx, ?, V]] =
      new Monad[Task[Ctx, ?, V]] {
        def pure[A](a: A) = Task.key(a)

        override def map[A, B](fa: Task[Ctx, A, V])(fn: A => B): Task[Ctx, B, V] =
          fa.map(fn)

        override def product[A, B](fa: Task[Ctx, A, V], fb: Task[Ctx, B, V]): Task[Ctx, (A, B), V] =
          fa.product(fb)

        override def ap[A, B](fab: Task[Ctx, A => B, V])(fb: Task[Ctx, A, V]): Task[Ctx, B, V] =
          product(fab, fb).map { case (f, b) => f(b) }

        def flatMap[A, B](fa: Task[Ctx, A, V])(fn: A => Task[Ctx, B, V]): Task[Ctx, B, V] =
          fa.flatMap(fn)

        def tailRecM[A, B](a: A)(fn: A => Task[Ctx, Either[A, B], V]): Task[Ctx, B, V] =
          new Task[Ctx, B, V] {
            def run[F[_]](build: B => F[V])(implicit c: Ctx[F]): F[V] = {
              // TODO: this is not really stack safe.
              // if we had Ctx is Monad or Defer, I think we are
              // okay here.
              def go(a: A): F[V] =
                fn(a).run {
                  case Left(a) => go(a)
                  case Right(b) => build(b)
                }

              go(a)
            }
          }
      }

    implicit class InvariantTask[Ctx[_[_]], K, V](val task: Task[Ctx, K, V]) extends AnyVal {
      def reconstrain[Ctx2[_[_]]](fn: FunctionKK[Ctx2, Ctx]): Task[Ctx2, K, V] =
        new Task[Ctx2, K, V] {
          def run[F[_]](build: K => F[V])(implicit c: Ctx2[F]): F[V] =
            task.run(build)(fn(c))
        }

      def flatMap[K1](fn: K => Task[Ctx, K1, V]): Task[Ctx, K1, V] =
        new Task[Ctx, K1, V] {
          def run[F[_]](build: K1 => F[V])(implicit c: Ctx[F]): F[V] =
            task.run { k =>
              fn(k).run(build)
            }
        }

      type MapRes[F[_]] = (F[V], Ctx[F])
      type IdK[F[_]] = F[V]

      def mapResult(fn: FunctionKK[MapRes, IdK]): Task[Ctx, K, V] =
        new Task[Ctx, K, V] {
          def run[F[_]](build: K => F[V])(implicit c: Ctx[F]): F[V] =
            task.run { k =>
              val fv = build(k)
              fn((fv, c))
            }
        }

      def product[K1](that: Task[Ctx, K1, V]): Task[Ctx, (K, K1), V] =
        new Task[Ctx, (K, K1), V] {
          def run[F[_]](build: ((K, K1)) => F[V])(implicit c: Ctx[F]): F[V] =
            task.run { a =>
              that.run { b =>
                build((a, b))
              }
            }
        }
    }

    implicit class ApplicativeTask[K, V](val task: Task[Applicative, K, V]) extends AnyVal {
      // Get the list of dependencies for a given K
      // I think this also works for Alternative
      def dependencies: List[K] =
        task.run[Const[Chain[K], ?]] { k => Const(Chain.one(k)) }
          .getConst
          .toList
    }

    implicit class MonadTask[K, V](val task: Task[Monad, K, V]) extends AnyVal {
      // Run without attempting to update any dependencies
      def compute[I](store: Store[I, K, V])(implicit EK: Eq[K]): V =
        task.run[Id] { k => store.getValue(k) }

      def track[M[_]: Monad](fn: K => M[V]): M[(Chain[(K, V)], V)] = {

        def fetchTrack(k: K): WriterT[M, Chain[(K, V)], V] =
          for {
            v <- WriterT.liftF[M, Chain[(K, V)], V](fn(k))
            _ <- WriterT.tell[M, Chain[(K, V)]](Chain.one((k, v)))
          } yield v

        task.run(fetchTrack).run
      }
    }

    def traverse[T[_]: Traverse, K, V](tasks: T[K])(fn: T[V] => V): Task[Applicative, K, V] =
      new Task[Applicative, K, V] {
        def run[F[_]](build: K => F[V])(implicit ctx: Applicative[F]): F[V] =
          tasks.traverse(build).map(fn)
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
  sealed trait Tasks[-Ctx[_[_]], K, V] {
    def get(k: K): Option[Task[Ctx, K, V]]
  }

  object Tasks {
    implicit class InvariantTasks[Ctx[_[_]], K, V](val tasks: Tasks[Ctx, K, V]) extends AnyVal {
      def ++(that: Tasks[Ctx, K, V]): Tasks[Ctx, K, V] =
        (tasks, that) match {
          case (TaskMap(m1), TaskMap(m2)) => TaskMap(m1 ++ m2)
        }
    }

    private case class TaskMap[Ctx[_[_]], K, V](toMap: Map[K, Task[Ctx, K, V]]) extends Tasks[Ctx, K, V] {
      def get(k: K) = toMap.get(k)
    }
    def one[Ctx[_[_]], K, V](k: K, t: Task[Ctx, K, V]): Tasks[Ctx, K, V] =
      TaskMap(Map((k, t)))

    def apply[Ctx[_[_]], K, V](tasks: (K, Task[Ctx, K, V])*): Tasks[Ctx, K, V] =
      TaskMap(Map(tasks: _*))
  }

  /**
   * This runs a build for a given k by updating the state of the store
   */
  trait Build[+Ctx[_[_]], I, K, V] {
    def update(tsks: Tasks[Ctx, K, V], key: K, store: Store[I, K, V]): Store[I, K, V]
  }

  object Build {
    /**
     * A naive applicative builder that rebuilds each time an item is needed
     */
    def busy[K: Eq, V]: Build[Applicative, Unit, K, V] =
      new Build[Applicative, Unit, K, V] {
        def update(tsks: Tasks[Applicative, K, V], key: K, store: Store[Unit, K, V]): Store[Unit, K, V] = {

          def fetch(k: K): State[Store[Unit, K, V], V] =
            tsks.get(k) match {
              case None => State.get[Store[Unit, K, V]].map(_.getValue(k))
              case Some(t) =>
                for {
                  v <- t.run(fetch(_))
                  _ <- State.modify[Store[Unit, K, V]](_.putValue(k, v))
                } yield v
            }

          fetch(key).run(store).value._1
        }
      }

    /**
     * This is a build, as defined in the paper, that implements
     * Shake's approach of suspending scheduling and verifying rebuilding
     */
    def shake[K: Order, V: Hashable]: Build[Monad, VT[K, V], K, V] =
      Scheduler.suspending[VT[K, V], K, V].schedule(Rebuilder.vtRebuilder[K, V])
  }

  /**
   * This is a type that defines how to decide when to rebuild a task from
   * the current value
   */
  trait Rebuilder[+Ctx[_[_]], IR, K, V] {
    def apply(k: K, v: V, task: Task[Ctx, K, V]): Task.StateTask[IR, K, V]
  }

  object Rebuilder {
    def dirtyBit[K, V]: Rebuilder[Monad, K => Boolean, K, V] =
      new Rebuilder[Monad, K => Boolean, K, V] {
        def apply(k: K, v: V, task: Task[Monad, K, V]): Task.StateTask[K => Boolean, K, V] =
          new Task.StateTask[K => Boolean, K, V] {
            def run[F[_]](build: K => F[V])(implicit c: MonadState[F, K => Boolean]): F[V] =
              c.monad.flatMap(c.get) { isDirty =>
                if (isDirty(k)) task.run(build)(c.monad)
                else c.monad.pure(v)
            }
          }
      }

    /**
     * This is the verifying trace rebuilder
     */
    def vtRebuilder[K, V: Hashable]: Rebuilder[Monad, VT[K, V], K, V] =
      new Rebuilder[Monad, VT[K, V], K, V] {
        def apply(k: K, v: V, task: Task[Monad, K, V]): Task.StateTask[VT[K, V], K, V] =
          new Task.StateTask[VT[K, V], K, V] {
            def run[F[_]](build: K => F[V])(implicit c: MonadState[F, VT[K, V]]): F[V] = {
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
  trait Scheduler[Ctx[_[_]], I, IR, K, V] {
    def schedule(r: Rebuilder[Ctx, IR, K, V]): Build[Ctx, I, K, V]
  }

  object Scheduler {
    /**
     * This is the suspending tactic described in the paper
     */
    def suspending[I, K: Order, V]: Scheduler[Monad, I, I, K, V] =
      new Scheduler[Monad, I, I, K, V] {
        def schedule(r: Rebuilder[Monad, I, K, V]): Build[Monad, I, K, V] =
          new Build[Monad, I, K, V] {
            type S = (Store[I, K, V], SortedSet[K])
            val monadState: MonadState[State[S, ?], I] =
              new MonadState[State[S, ?], I] {
                def monad = Monad[State[S, ?]]
                def update[A](fn: I => State[S, (I, A)]): State[S, A] =
                  for {
                    sd <- State.get[S]
                    (store, _) = sd
                    ia <- fn(store.getInfo)
                    (newI, a) = ia
                    _ <- State.modify[S] { case (store, d) => (store.putInfo(newI), d) }
                  } yield a
              }

            def update(tsks: Tasks[Monad, K, V], key: K, store: Store[I, K, V]): Store[I, K, V] = {
              def run(t: Task.StateTask[I, K, V], fn: K => State[S, V]): State[S, V] =
                t.run(fn)(monadState)

              def fetch(key: K): State[S, V] =
                State.get[S]
                  .flatMap { case (store, done) =>
                    val value = store.getValue(key)
                    tsks.get(key) match {
                      case Some(task) if !done(key) =>
                        // we need to run
                        val newTask = r(key, value, task)
                        for {
                          newValue <- run(newTask, fetch(_))
                          _ <- State.modify[S] { case (str, set) =>
                            (str.putValue(key, newValue), set + key)
                          }
                        } yield newValue
                      case _ =>
                        State.pure(value)
                    }
                  }

              fetch(key).run((store, SortedSet.empty[K](Order[K].toOrdering))).value._1._1
            }
          }
      }
  }
}
