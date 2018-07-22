package com.omd.fp.pearls

import scala.annotation.tailrec

protected[pearls] trait SmallestFree {
  final def minfree(xs: List[Int]): Int = minfrom(0, xs.toSet.size, xs.toSet.toList)

  @tailrec
  private def minfrom(a: Int, n: Int, xs: List[Int]) :Int = (a, n, xs) match {
    case (_, 0, _)       ⇒ a
    case (min, l, other) ⇒
      val b  = min + 1 + l / 2
      val (us, vs) = other.partition(_ < b)
      if (b - min == us.length) minfrom(b, l - b + min, vs)
      else minfrom(a, us.length, us)
  }
}
