import scala.io.Source

abstract class Day {
  val inputFile: String
  def data =
    Source.fromFile(inputFile).getLines.toIterable
}
