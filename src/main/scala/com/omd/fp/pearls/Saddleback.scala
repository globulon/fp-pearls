package com.omd.fp.pearls

import scala.annotation.tailrec

protected[pearls] trait Saddleback {
  //Anne Solution - step 1 : dividing in optimized stripped squares
  final def find1(f: (Int, Int) ⇒ Int)(z: Int): List[(Int, Int)] = find1(List.empty)(0, z)(f)(z)

  @tailrec
  private def find1(acc: List[(Int, Int)])(u: Int, v: Int)(f: (Int, Int) ⇒ Int)(z: Int): List[(Int, Int)] = f(u, v) match {
    case _ if u > z ⇒ acc
    case _ if v < 0 ⇒ acc
    case n if n < z ⇒ find1(acc)(u + 1, v)(f)(z)
    case n if n > z ⇒ find1(acc)(u, v - 1)(f)(z)
    case _          ⇒ find1((u, v)::acc)(u + 1, v - 1)(f)(z)
  }
}
