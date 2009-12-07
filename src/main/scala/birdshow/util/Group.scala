package birdshow.util

object Group {
  type Row[T] = Tuple3[Option[T], Option[T], Option[T]]
  
  def group[T](items: Seq[T]): Seq[Row[T]] = {
    def hss(it: Iterator[T]) = if (it.hasNext) Some(it.next) else None
    val it = items.elements
    var result = List[Row[T]]()
    while(it.hasNext) {
      result = result ::: List((hss(it), hss(it), hss(it)))
    }
    result
  }
}