import scala.io.Source

abstract class Day {
  /** File with challange input */
  val inputFile: String

  /** Returns the input file as iterable lines */
  def data =
    Source.fromFile(inputFile).getLines.toIterable
}
