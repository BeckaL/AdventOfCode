package shared

object UpdaterHelpers {
  implicit class StringUpdater(str: String) {
    def updateChar(i: Int, replacement: Char) =         {
      val (before, after) = str.splitAt(i)
      (before :+ replacement) ++ after.tail
    }
  }

  implicit class ListStringUpdater(l: List[String]) {
    def update(x: Int, y: Int, char: Char): List[String] = {
      val (before, after) = l.splitAt(y)
      val updatedStr = after.head.updateChar(x, char)
      (before :+ updatedStr) ++ after.tail
    }
  }

  def removeAtIndex[A](list: List[A], index: Int): List[A] =
    val (before, after) = list.splitAt(index)
    before ++ after.tail
    
  def updateAtIndex[A](i: Int, replacement: A, list: List[A]): List[A] =
    val (before, after) = list.splitAt(i)
    (before :+ replacement) ++ after.tail
}
