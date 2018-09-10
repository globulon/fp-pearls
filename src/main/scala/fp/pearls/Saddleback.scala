package fp.pearls

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


  //Dubbed saddle back search
  final def find2(f: (Int, Int) ⇒ Int)(z: Int): List[(Int, Int)] = find2(List.empty)(0, z)(f)(z)

  @tailrec
  private def find2(acc: List[(Int, Int)])(u: Int, v: Int)(f: (Int, Int) ⇒ Int)(z: Int): List[(Int, Int)] =
    (bsearch(f(0,_))(z)(-1, v + 1), bsearch(f(_,0))(z)(u - 1, z + 1)) match {
      case (_, n) if u > n       ⇒ acc
      case _      if v < 0       ⇒ acc
      case (m, _) if f(u,m) < z  ⇒ find2(acc)(u + 1, m)(f)(z)
      case (m, _) if f(u, m) > z ⇒ find2(acc)(u, m - 1)(f)(z)
      case (m, _)                ⇒ find2((u, v)::acc)(u + 1, m - 1)(f)(z)
    }

  @tailrec
  private def bsearch(g: Int ⇒ Int)(z: Int)(min: Int, max: Int) : Int = (min, max) match {
    case (a, b) if b == a + 1           ⇒ a
    case (a, b) if g((a + b) / 2) <= z  ⇒ bsearch(g)(z)((a + b) / 2, b)
    case (a, b)                         ⇒ bsearch(g)(z)(a, (a + b) / 2)
  }
}
