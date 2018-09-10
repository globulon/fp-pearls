package fp.ftree

import scala.annotation.tailrec
import scala.reflect.ClassTag

private[fp] sealed trait Crumb[@specialized(Short, Int) +A, @specialized(Int, Long) +B] extends Immutable
private[fp] final case class LeftCrumb[@specialized(Short, Int) +A, @specialized(Int, Long) +B](k: A, v: B, right: FTree[A, B]) extends Crumb[A, B]
private[fp] final case class RightCrumb[@specialized(Short, Int) +A, @specialized(Int, Long) +B](k: A, v: B, left: FTree[A, B]) extends Crumb[A, B]

private[fp] final case class Zipper[@specialized(Short, Int) +A, @specialized(Int, Long) +B](tree: FTree[A, B], crumbs: List[Crumb[A, B]]) extends Immutable

protected[fp] trait FTreeOps { self: FTreeConstructors with FTreeNavigation with FTreeCatamorphism ⇒
  @inline
  final def sum[@specialized(Short, Int) A, @specialized(Int, Long) B: Numeric](t: FTree[A, B]): B =
    fold[B, A, B](implicitly[Numeric[B]].zero) { case (acc, (_, b)) ⇒ implicitly[Numeric[B]].plus(acc, b) }.apply(t)

  @inline
  final def size[@specialized(Short, Int) A, @specialized(Int, Long) B](t: FTree[A, B]): Int =
    fold[Int, A, B](0){ (n, _) ⇒ n + 1 } (t)

  @inline
  final def map[@specialized(Short, Int) A: Ordering: ClassTag, @specialized(Int, Long) B: ClassTag, @specialized(Int, Long) C: ClassTag]
  (f: B ⇒ C): FTree[A, B] ⇒ FTree[A, C] = fold[FTree[A, C], A, B](leaf[A, C]) { case (acc, (k, v)) ⇒  add(k, f(v), acc) }

  @inline
  final def add[@specialized(Short, Int) A: Ordering: ClassTag, @specialized(Int, Long) B : ClassTag](k: A, v: B, to: FTree[A, B]): FTree[A, B] =
    update(k, append(k, v), to)

  @inline
  private def append[@specialized(Short, Int) A: Ordering: ClassTag, @specialized(Int, Long) B : ClassTag](k:A, v: B): FTree[A, B] ⇒ FTree[A, B] = {
    case FLeaf  ⇒ node[A, B](k, v)
    case other ⇒ other
  }

  @inline
  final def update[@specialized(Short, Int) A: Ordering: ClassTag, @specialized(Int, Long) B : ClassTag]
  (k: A, f: FTree[A, B] ⇒ FTree[A, B], t: FTree[A, B]): FTree[A, B] = updateTree(k, f, Zipper(t, Nil))

  @inline @tailrec
  private def updateTree[@specialized(Short, Int) A: Ordering: ClassTag, @specialized(Int, Long) B : ClassTag]
  (k: A, f: FTree[A, B] ⇒ FTree[A, B], zipper: Zipper[A, B]): FTree[A, B] = zipper match {
    case Zipper(FLeaf, bs)                                                  ⇒ gotTop(Zipper(f(FLeaf), bs))
    case Zipper(n@FNode(_, a, _, _), bs) if k == a                          ⇒ gotTop(Zipper(f(n), bs))
    case z@Zipper(FNode(_, a, _, _), _) if implicitly[Ordering[A]].gt(k, a) ⇒ updateTree(k, f, goRight[A, B](z))
    case other                                                             ⇒ updateTree[A, B](k, f, goLeft[A, B](other))
  }
}

protected[fp] trait FTreeNavigation {
  @inline @tailrec
  final protected def gotTop[@specialized(Short, Int) A, @specialized(Int, Long) B](z: Zipper[A, B]): FTree[A, B] = z match {
    case Zipper(tree, Nil) ⇒ tree
    case zipper            ⇒ gotTop(goUp(zipper))
  }

  @inline
  final protected def goLeft[@specialized(Short, Int) A, @specialized(Int, Long) B]: Zipper[A, B] ⇒ Zipper[A, B] = {
    case Zipper(FNode(l, k, v, r), tail)  ⇒ Zipper(l , LeftCrumb(k, v, r)::tail)
    case other                           ⇒ other
  }

  @inline
  final protected def goRight[@specialized(Short, Int) A, @specialized(Int, Long) B]: Zipper[A, B] ⇒ Zipper[A, B] = {
    case Zipper(FNode(l, k, v, r), tail)  ⇒ Zipper(r , RightCrumb(k, v, l)::tail)
    case other                           ⇒ other
  }

  @inline
  final protected def goUp[A, B]: Zipper[A, B] ⇒ Zipper[A, B] = {
    case Zipper(left, LeftCrumb(k, v, right)::tail)  ⇒ Zipper(FNode(left, k, v, right), tail)
    case Zipper(right, RightCrumb(k, v, left)::tail) ⇒ Zipper(FNode(left, k, v, right), tail)
    case other                                       ⇒ other
  }
}

protected[fp] trait FTreeCatamorphism { self: FTreeConstructors with FTreeNavigation ⇒
  private[this] trait Direction[@specialized(Short, Int) +A, @specialized(Int, Long) +B]
  private[this] final case class Unzip[@specialized(Short, Int) +A, @specialized(Int, Long) +B](z: Zipper[A, B]) extends Direction[A, B]
  private[this] final case class Zip[@specialized(Short, Int) +A, @specialized(Int, Long) +B](crumb: Crumb[A, B], z: Zipper[A, B]) extends Direction[A, B]

  @inline
  final def fold[M, @specialized(Short, Int) A, @specialized(Int, Long) B](z: M)(f: (M, (A, B)) ⇒ M): FTree[A, B] ⇒ M =
    t ⇒ fold(f, Unzip(Zipper(t, Nil)), z)

  @inline @tailrec
  private def fold[M, A, B](f: (M, (A, B)) ⇒ M, d: Direction[A, B], acc: M): M = d match {
    case Unzip(Zipper(FLeaf, Nil)) ⇒ acc
    case Unzip(Zipper(FNode(FLeaf, k, v, FLeaf), Nil)) ⇒ f(acc, (k, v))
    case Zip(LeftCrumb(_, _, _), Zipper(FNode(_, _, _, FLeaf), Nil)) ⇒ acc
    case Zip(RightCrumb(_, _, _), Zipper(_, Nil)) ⇒ acc
    case Unzip(z@Zipper(FNode(FLeaf, k, v, FLeaf), c :: _)) ⇒ fold(f, Zip(c, goUp(z)), f(acc, (k, v)))
    case Unzip(z@Zipper(FNode(FLeaf, k, v, _), _)) ⇒ fold(f, Unzip(goRight(z)), f(acc, (k, v)))
    case Unzip(z@Zipper(FNode(_, k, v, _), _)) ⇒ fold(f, Unzip(goLeft(z)), f(acc, (k, v)))
    case Zip(LeftCrumb(_, _, _), z@Zipper(FNode(_, _, _, FLeaf), c :: _)) ⇒ fold(f, Zip(c, goUp(z)), acc)
    case Zip(LeftCrumb(_, _, _), z@Zipper(FNode(_, _, _, _), _)) ⇒ fold(f, Unzip(goRight(z)), acc)
    case Zip(RightCrumb(_, _, _), z@Zipper(FNode(_, _, _, _), c :: _)) ⇒ fold(f, Zip(c, goUp(z)), acc)
  }
}