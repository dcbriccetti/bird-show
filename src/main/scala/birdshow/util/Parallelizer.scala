package birdshow.util

import java.util.concurrent.{ExecutorCompletionService, Callable, Executors}
import net.liftweb.util.Log

class Parallelizer(numThreads: Int) {
  private var pool = Executors.newFixedThreadPool(numThreads)
  
  /**
   * Runs the function f in multiple threads, giving it each A of args, returning a List[T]
   */
  def run[T,A](args: Seq[A], f: (A) => T): List[T] = {
    val completionService = new ExecutorCompletionService[T](pool)
    args.foreach(arg => 
      completionService.submit(new Callable[T] {
        def call = f(arg)
      }))
    (args map (_ => completionService.take.get)).toList
  }
  
  def shutDown() = {
    Log.debug("Parallelizer shutting down")
    pool.shutdown
  }
}
