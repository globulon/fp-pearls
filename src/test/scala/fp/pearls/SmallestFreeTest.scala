package fp.pearls

import org.scalatest.{MustMatchers, WordSpecLike}

final class SmallestFreeTest extends WordSpecLike with MustMatchers {
  "minfree" should {
    "find smallest element" in {
      minfree(List(8, 23, 9, 0, 12, 11, 1, 10, 13, 7, 41, 4, 14, 21, 5, 17, 3, 19, 2, 6)) must be (15)
    }
  }
}
