import scala.io.Source
import scala.runtime.stdLibPatches.language.unsafeNulls
import scala.annotation.targetName

object Day2 extends Day {
  override val inputFile = "inputs/day2.txt"

  case class Position(val x: Int = 0, val y: Int = 0)
  case class Command(val cmd: Cmd, val arg: Int) {
    def apply(pos: Position): Position =
      cmd match
        case Cmd.Forward => pos.copy(x = pos.x + arg)
        case Cmd.Up      => pos.copy(y = pos.y - arg)
        case Cmd.Down    => pos.copy(y = pos.y + arg)
  }

  case class Position2(val x: Int = 0, val y: Int = 0, val aim: Int = 0)
  case class Command2(val cmd: Cmd, val arg: Int) {
    def apply(pos: Position2): Position2 =
      cmd match
        case Cmd.Up   => pos.copy(aim = pos.aim - arg)
        case Cmd.Down => pos.copy(aim = pos.aim + arg)
        case Cmd.Forward =>
          pos.copy(
            x = pos.x + arg,
            y = pos.y + pos.aim * arg
          )
  }
  enum Cmd:
    case Forward, Down, Up

  implicit def parseV2(cmd: Command): Command2 = Command2(cmd.cmd, cmd.arg)
  implicit def parseV2(d: Iterable[Command]): Iterable[Command2] =
    d.map(parseV2(_))

  implicit def parse(cmd: String): Cmd =
    cmd match
      case "forward" => Cmd.Forward
      case "up"      => Cmd.Up
      case "down"    => Cmd.Down

  implicit def parse(d: Iterable[String]): Iterable[Command] =
    d.map { l =>
      val w = l.split(" ")
      val cmd = w(0)

      Command(cmd, w(1).toInt)
    }

  def partOne(cmds: Iterable[Command] = data): Int =
    val pos = cmds
      .foldLeft(Position())((p, c) => c(p))
    pos.x * pos.y

  def partTwo(cmds: Iterable[Command] = data): Int =
    val pos = cmds.foldLeft(Position2())((p, c) => c(p))
    pos.x * pos.y

  def apply(): Unit = 
    println(s"""Day2:
                |- part 1: ${partOne()}
                |- part 2: ${partTwo()}""".stripMargin)
}
