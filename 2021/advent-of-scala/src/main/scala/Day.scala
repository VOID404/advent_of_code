import scala.io.Source
import java.nio.file.Paths

abstract class Day {
  var basePath = Paths.get("inputs")

  /** File with challange input */
  val inputFile: String

  /** Returns the input file as iterable lines */
  def data =
    Source
      .fromFile(basePath.resolve(inputFile).toString)
      .getLines
      .to(Iterable)
}
