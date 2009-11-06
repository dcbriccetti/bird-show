package birdshow.snippet

import org.junit.Test
import org.junit.Assert._

class PicturesTest {
  @Test def grouping {
    assertEquals(List((Some(1), Some(2), Some(3)),(Some(4), None, None)), 
      new Pictures().group(1 to 4))
  }
}