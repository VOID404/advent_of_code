import scala.io.Source
import scala.runtime.stdLibPatches.language.unsafeNulls
import scala.annotation.targetName
import scala.annotation.tailrec

object Day3 extends Day {
  val inputFile = "day3.txt"

  class BinList(val l: Seq[Int]) {
    def toNum = l.reverse.zipWithIndex
      .map(x => x._1 * math.pow(2, x._2).toInt)
      .sum
  }

  implicit def toBinList(l: Seq[Int]): BinList = BinList(l)

  implicit def parseInputs(d: Iterable[String]): Iterable[Seq[Int]] =
    d.map(_.map(_.asDigit))

  def partOne(d: Iterable[Seq[Int]] = data) =
    val gamma = d.transpose
      .map(_.groupBy(identity).maxBy(_._2.size))
      .map(_._1)
      .toList
    val epsilon = gamma.map(x => if (x == 0) 1 else 0)

    epsilon.toNum * gamma.toNum

  def oxygenOrd: Ordering[Tuple2[Int, Iterable[Seq[Int]]]] =
    new Ordering[Tuple2[Int, Iterable[Seq[Int]]]] {
      def compare(
          x: Tuple2[Int, Iterable[Seq[Int]]],
          y: Tuple2[Int, Iterable[Seq[Int]]]
      ): Int =
        val cmp = x._2.size compare y._2.size
        if (cmp == 0)
          x._1 compare y._1
        else cmp
    }

  @tailrec
  def oxy(d: Iterable[Seq[Int]], n: Int = 0): Int =
    if (d.size == 1)
      d.head.toNum
    else
      oxy(d.groupBy(_(n)).max(oxygenOrd)._2, n + 1)
  
  @tailrec
  def co2(d: Iterable[Seq[Int]], n: Int = 0): Int =
    if (d.size == 1)
      d.head.toNum
    else
      co2(d.groupBy(_(n)).min(oxygenOrd)._2, n + 1)

  def partTwo(d: Iterable[Seq[Int]] = data) =
    oxy(d) * co2(d)
}
