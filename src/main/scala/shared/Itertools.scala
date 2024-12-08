package shared

object Itertools {
  
  //cartesian product
  def cartesianProduct[A](l: List[A], repeat: Int): List[List[A]] =
    (for {
      list <- (1 to repeat).foldLeft(Seq(List.empty[A])) { (acc, _) =>
        acc.flatMap(list => l.map(list :+ _))
      }
    } yield list).toList

}
