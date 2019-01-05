package org.bykn.buildcart

import cats.Monad

import cats.implicits._

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
