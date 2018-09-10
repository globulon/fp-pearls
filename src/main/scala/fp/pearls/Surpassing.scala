package fp.pearls

protected[pearls] trait Surpassing {
  protected type Count[A]  = (A, Int)
  protected type Counts[A] = List[Count[A]]

  final def table[A : Ordering](xs: List[A]): Counts[A] =   xs match {
    case Nil      ⇒ Nil
    case x :: Nil ⇒ List((x, 0))
    case other     ⇒
      val m = other.length
      val n = m / 2
      val (zs, ys) = xs.splitAt(n)
      join(m - n, table(zs), table(ys))
  }

  private def join[A : Ordering](length: Int, xs: Counts[A], ys: Counts[A]): Counts[A] = (length, xs , ys) match {
    case (0, `xs`, Nil)  ⇒ `xs`
    case (_, Nil, `ys`)  ⇒ `ys`
    case (n, (x, c)::txs , `ys` @ (y, _) :: _) if implicitly[Ordering[A]].lt(x, y) ⇒
      (x, c + n) :: join[A](n , txs, `ys`)
    case (n, `xs`  , (y, d) :: tys) ⇒
      (y, d) :: join[A](n - 1, `xs`, tys)
  }
}
