package org.bykn.buildcart

import cats.Eq

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
