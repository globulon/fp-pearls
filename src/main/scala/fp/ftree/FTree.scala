package fp.ftree

sealed trait FTree[@specialized(Short, Int) +A, @specialized(Int, Long) +B] extends Immutable
case object FLeaf extends FTree[Nothing, Nothing]
case class FNode[@specialized(Short, Int) +A, @specialized(Int, Long) +B](l: FTree[A, B], k: A, v: B, r: FTree[A, B]) extends FTree[A, B]

protected[ftree] trait FTreeConstructors {
  @inline
  final def leaf[A, B]: FTree[A, B] = FLeaf

  @inline
  final def node[@specialized(Short, Int) A, @specialized(Int, Long) B](k: A, v: B): FTree[A, B] = FNode(FLeaf, k, v, FLeaf)
  @inline
  final def node[@specialized(Short, Int) A, @specialized(Int, Long) B](k: A, v: B, r: FTree[A, B]): FTree[A, B] = FNode(FLeaf, k, v, r)
  @inline
  final def node[@specialized(Short, Int) A, @specialized(Int, Long) B](l: FTree[A, B], k: A, v: B): FTree[A, B] = FNode(l, k, v, FLeaf)
  @inline
  final def node[@specialized(Short, Int) A, @specialized(Int, Long) B](l: FTree[A, B], k: A, v: B, r: FTree[A, B]): FTree[A, B] = FNode(l, k, v, r)
}