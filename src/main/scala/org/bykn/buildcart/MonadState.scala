package org.bykn.buildcart

import cats.Monad
import cats.data.State

/**
 * This is MTL type approach for encoding that a given M has a state S variable
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
