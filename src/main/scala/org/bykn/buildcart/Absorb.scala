package org.bykn.buildcart

import cats.{Applicative, Functor, Id, Monad, Monoid}
import cats.data.{Const, StateT, WriterT}

import cats.implicits._

/**
 * This is proof that we can lift F into G and G[F[A]] into G[A]
 */
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

