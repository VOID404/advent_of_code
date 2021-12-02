import scala.io.Source

object Day1 extends Day {
  val inputFile = "inputs/day1.txt"

  implicit def parse(d: Iterable[String]): Iterable[Int] = d.map(_.toInt)

  def partOne(d: Iterable[Int] = data) =
    d.iterator
      .sliding(2, 1)
      .count(s => s(0) < s(1))

  def partTwo(d: Iterable[Int] = data) =
    d
      .sliding(3, 1)
      .map(_.sum)
      .sliding(2, 1)
      .count(s => s(0) < s(1))

  def apply(): Unit = {
    println(s"""Day1:
                |- part 1: ${partOne()}
                |- part 2: ${partTwo()}""".stripMargin)
  }
}
