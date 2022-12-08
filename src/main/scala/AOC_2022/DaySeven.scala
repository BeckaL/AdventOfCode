package AOC_2022

import shared.{DayChallenge, TestData}

object DaySeven extends DayChallenge[Int, Int] {
  override def partOne(l: List[String]): Int =
    getFileNamesToSizes(l).values.filter(_ <= 100000).sum

  override def partTwo(l: List[String]): Int = {
    val dirNamesToSizes = getFileNamesToSizes(l)
    val spaceRequired = 30000000 - (70000000 - dirNamesToSizes("/"))
    dirNamesToSizes.values.filter(_ >= spaceRequired).min
  }

  private def dirSize(dir: Dir, m: Map[String, Dir]): Int =
    dir.files.sum + dir.children.map(c => dirSize(m(c), m)).sum

  private def getFileNamesToSizes(l: List[String]) = {
    val m = getMapOfDirNamesToValues(Map("/" -> Dir(None, List(), List())), l)
    m.map { case (name, dir) => name -> dirSize(dir, m)}
  }

  private def getMapOfDirNamesToValues(startMap: Map[String, Dir], l: List[String]): Map[String, Dir] =
    l.drop(1).foldLeft(("/", startMap)) { case ((currentDirName, m), instruction) =>
        instruction.split(" ").toList match {
          case "$" :: "ls" :: Nil => (currentDirName, m)
          case "$" :: "cd" :: ".." :: Nil => (m.get(currentDirName).flatMap(_.parent).get, m)
          case "$" :: "cd" :: newDirName :: Nil => (currentDirName + "/" + newDirName, m)
          case "dir" :: newDirName :: Nil =>
            val currentDir = m(currentDirName)
            (currentDirName, m
              .updated(currentDirName, currentDir.updateChildren(currentDirName + "/" + newDirName))
              .updated(currentDirName + "/" + newDirName, Dir(Some(currentDirName), List(), List()))
            )
          case fileSize :: _ :: Nil =>
            val currentDir = m(currentDirName)
            (currentDirName, m.updated(currentDirName, currentDir.updateFiles(fileSize.toInt)))
        }
    }._2


  case class File(name: String, size: Int)
  case class Dir(parent: Option[String], children: List[String], files: List[Int]) {
    def updateChildren(child: String) = this.copy(children = child +: children)
    def updateFiles(file: Int) = this.copy(files = file +: files)
  }
}

object DaySevenData extends TestData[Int, Int] {
  override val testData: List[String] = List(
    "$ cd /",
      "$ ls",
      "dir a",
      "14848514 b.txt",
      "8504156 c.dat",
      "dir d",
      "$ cd a",
      "$ ls",
      "dir e",
      "29116 f",
      "2557 g",
      "62596 h.lst",
      "$ cd e",
      "$ ls",
      "584 i",
      "$ cd ..",
      "$ cd ..",
      "$ cd d",
      "$ ls",
      "4060174 j",
      "8033020 d.log",
      "5626152 d.ext",
      "7214296 k",
  )
  override val expectedPartOne: Option[Int] = Some(95437)
  override val expectedPartTwo: Option[Int] = Some(24933642)
}