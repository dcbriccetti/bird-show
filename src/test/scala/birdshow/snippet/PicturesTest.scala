package birdshow.snippet

import org.junit.Test
import org.junit.Assert._
import birdshow.util.Group

class PicturesTest {
  @Test def grouping {
    assertEquals(List((Some(1), Some(2), Some(3)),(Some(4), None, None)), 
      Group.group(1 to 4))
  }
}