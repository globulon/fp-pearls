package fp.ftree

import fp.ftree
import org.scalatest.{MustMatchers, WordSpecLike}

final class FTreeTest extends WordSpecLike with MustMatchers {
  "update tree" should {
    "update leaf" in {
      update[Short, Long](7, addValue(7, 11L), leaf) must be (node(7, 11L))
    }

    "update single node" in {
      update[Short, Long](7, addValue(7, 12L), node(7, 11L)) must be (node(7, 23L))
    }

    "append right node" in {
      update[Short, Long](11, addValue(11, 17L), node(7, 11L)) must be (node(7, 11L, node(11, 17L)))
    }

    "append left node" in {
      update[Short, Long](7, addValue(7, 11L), node(11, 17L)) must be (node(node(7, 11L), 11, 17L))
    }

    "append node to existing tree" in {
      update(15, addValue(15, 15L), sample) must be (node(
        node(node(3, 2L), 5, 11L, node(7, 19L)),
        11, 17L,
        node(node(15, 15L), 17, 23L, node(37, 7L))
      ))
    }
  }

  "fold" should {
    "count zero node" in {
      ftree.size(FLeaf) must be (0)
    }

    "count one node" in {
      ftree.size(node(3, 2L)) must be (1)
    }

    "count 2 nodes" in {
      ftree.size(sample2) must be (2)
    }

    "count again 2 nodes" in {
      ftree.size(sample4) must be (2)
    }

    "count 3 nodes" in {
      ftree.size(sample3) must be (3)
    }

    "count nodes " in {
      ftree.size(sample) must be (6)
    }
  }

  "sum" should {
    "sum zero node" in {
      sum(leaf[Short, Long]) must be (0L)
    }

    "sum one node" in {
      sum(node(3, 2L)) must be (2L)
    }

    "sum 2 nodes" in {
      sum(sample2) must be (19L)
    }

    "sum again 2 nodes" in {
      sum(sample4) must be (40L)
    }

    "sum 3 nodes" in {
      sum(sample3) must be (42L)
    }

    "sum nodes " in {
      sum(sample) must be (79L)
    }
  }

  "map" should {
    "map empty tree to empty tree" in {
      map[Short, Int, String](_.toString).apply(leaf[Short, Int]) must be (leaf[Short, Int])
    }

    "map singleton tree to singleton tree" in {
      map[Short, Int, String](_.toString).apply(node(7, 11)) must be (node(7, "11"))
    }

    "map 2 node tree to 2 node tree" in {
      map[Int, Long, String](_.toString).apply(sample2) must be (node(node(3, "2"), 11, "17", FLeaf))
    }

    "map another 2 node tree to 2 node tree" in {
      map[Int, Long, String](_.toString).apply(sample4) must be (node(FLeaf, 11, "17", node(17, "23")))
    }

    "map a 3 node tree to a 3 node tree" in {
      map[Int, Long, String](_.toString).apply(sample3) must be (node(node(3, "2"), 11, "17", node(17, "23")))
    }

    "map sample tree to new tree" in {
      map[Int, Long, String](_.toString).apply(sample) must be (node(
        node(node(3, "2"), 5, "11", node(7, "19")),
        11, "17",
        node(17, "23", node(37, "7"))
      ))
    }
  }

  private def addValue[@specialized(Short, Int) A : Numeric](k: A, v: Long) : FTree[A, Long] ⇒ FTree[A, Long] = {
    case FLeaf             ⇒ FNode(FLeaf, k, v, FLeaf)
    case FNode(l, _, b, r) ⇒ FNode(l, k, b + v, r)
  }

  private def sample: FTree[Int, Long]= node(
    node(node(3, 2L), 5, 11L, node(7, 19L)),
    11, 17L,
    node(17, 23L, node(37, 7L))
  )

  private def sample2: FTree[Int, Long]= node(
    node(3, 2L),
    11, 17L,
    FLeaf)

  private def sample3: FTree[Int, Long]= node(
    node(3, 2L),
    11, 17L,
    node(17, 23L))

  private def sample4: FTree[Int, Long]= node(
    FLeaf,
    11, 17L,
    node(17, 23L))
}