import scala.io.Source

object Day1 {
  val filename = "inputs/day1.txt"
  def data =
    for (line <- Source.fromFile(filename).getLines)
      yield line.toInt

  def partOne =
    data
      .sliding(2, 1)
      .count(s => s(0) < s(1))

  def partTwo =
    data
      .sliding(3, 1)
      .map(_.sum)
      .sliding(2, 1)
      .count(s => s(0) < s(1))

  def apply(): Unit = {
    println(s"""Day1:
                |- part 1: $partOne
                |- part 2: $partTwo""".stripMargin)
  }
}
