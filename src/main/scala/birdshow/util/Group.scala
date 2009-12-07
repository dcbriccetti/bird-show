package birdshow.util

object Group {
  type Row[T] = Tuple3[Option[T], Option[T], Option[T]]

  /**
   * Groups items into three columns.
   */
  def group[T](items: Seq[T]): Seq[Row[T]] = {
    
    def nextOrNone(it: Iterator[T]) = if (it.hasNext) Some(it.next) else None
    
    val it = items.elements
    var result = List[Row[T]]()
    
    while(it.hasNext) {
      result = result ::: List((nextOrNone(it), nextOrNone(it), nextOrNone(it)))
    }
    result
  }
}