package fp.pearls

import org.scalatest.{MustMatchers, WordSpecLike}

final class SaddlebackTest extends WordSpecLike with MustMatchers {
  "find 1" should {
    "find all combinations for 10" in {
      find1(_ * _)(10).toSet must be (Set((10,1), (5,2), (2,5), (1,10)))
    }

    "find all combinations for 17" in {
      find1(_ * _)(17).toSet must be (Set((1,17), (17, 1)))
    }

    "find all combinations for  36" in {
      find1(_ * _)(36).toSet must be (Set((6,6), (1,36), (3,12), (4,9), (12,3), (18,2), (9,4), (36,1), (2,18)))
    }

    "find all power combinators for 17" in {
      find1((x, y) ⇒ math.pow(2, x).toInt + math.pow(3, y).toInt)(17).toSet must be (Set((4,0), (3,2)))
    }
  }

  "find 2" should {
    "find all combinations for 10" in {
      find2(_ * _)(10).toSet must be (Set((10,1), (5,2), (2,5), (1,10)))
    }

    "find all combinations for 17" in {
      find2(_ * _)(17).toSet must be (Set((1,17), (17, 1)))
    }

    "find all combinations for  36" in {
      find2(_ * _)(36).toSet must be (Set((6,6), (1,36), (3,12), (4,9), (12,3), (18,2), (9,4), (36,1), (2,18)))
    }

    "find all power combinators for 17" in {
      find2((x, y) ⇒ math.pow(2, x).toInt + math.pow(3, y).toInt)(17).toSet must be (Set((4,0), (3,2)))
    }
  }
}
