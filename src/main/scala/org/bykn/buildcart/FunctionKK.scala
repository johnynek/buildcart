package org.bykn.buildcart

/**
 * this is a doubly higher kinded function for transforming
 * constraints (e.g. MonadState to Monad or Monad to Applicative,
 * ApplicativeError to Applicative, etc...
 */
trait FunctionKK[A[_[_]], B[_[_]]] {
  def apply[F[_]](a: A[F]): B[F]
}
