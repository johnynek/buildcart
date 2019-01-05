package org.bykn.buildcart

import cats.Applicative

/**
 * This is just a partial function from K to Task
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

