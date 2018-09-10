package fp.pearls

import org.scalatest.{MustMatchers, WordSpecLike}

final class SurpassingTest extends WordSpecLike with MustMatchers {
  "counting" should {
    "delivered classified info" in {
      table("GENERATING".toList) must be (List(('A',4), ('E',5), ('E',6), ('G',0), ('G',5), ('I',1), ('N',0), ('N',2), ('R',1), ('T',0)))
    }
  }
}
