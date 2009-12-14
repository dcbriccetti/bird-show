package birdshow.util

import org.junit.Test
import org.junit.Assert._
import org.hamcrest.CoreMatchers._
import org.apache.log4j.Logger

class ParallelizerTest {
  val log = Logger.getLogger("Test")
  var result1: List[Int] = _
  var result2: List[Int] = _
  
  @Test def concurrentUse {
    val range1 = List.range(0, 100)
    val range2 = List.range(100, 200)

    val pl = new Parallelizer(20)

    val t = new Thread(new Runnable(){
      def run = {
        result2 = pl.run(range2, doubleIt)
      }
    })
    t.start
    result1 = pl.run(range1, doubleIt)
    t.join
    assertThat(result1.sort(_ < _), is(range1.map(_ * 2)))
    assertThat(result2.sort(_ < _), is(range2.map(_ * 2)))
    pl.shutDown
  }
  
  private def doubleIt(num: Int): Int = {
    Thread.sleep(10)
    log.info("Doubling " + num)
    num * 2
  }
}