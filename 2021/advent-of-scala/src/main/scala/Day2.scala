import scala.io.Source
import scala.runtime.stdLibPatches.language.unsafeNulls

object Day2 extends Day {
  override val inputFile = "inputs/day2.txt"

  case class Position(val x: Int = 0, val y: Int = 0)
  case class Command(val cmd: Cmd, val arg: Int) {
    def apply(pos: Position): Position =
      cmd match
        case Cmd.Forward => Position(pos.x + arg, pos.y)
        case Cmd.Up      => Position(pos.x, pos.y - arg)
        case Cmd.Down    => Position(pos.x, pos.y + arg)
  }
  enum Cmd:
    case Forward, Down, Up

  implicit def parse(d: Iterable[String]): Iterable[Command] =
    d.map { l =>
      val w = l.split(" ")
      val cmd = w(0) match
        case "forward" => Cmd.Forward
        case "up"      => Cmd.Up
        case "down"    => Cmd.Down
        case _         => Cmd.Forward

      Command(cmd, w(1).toInt)
    }

  def partOne(cmds: Iterable[Command] = data): Int =
    val pos = cmds
      .foldLeft(Position())((p, c) => c(p))
    pos.x * pos.y

//   def partOne =
}
