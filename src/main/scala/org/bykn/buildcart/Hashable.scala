package org.bykn.buildcart

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
