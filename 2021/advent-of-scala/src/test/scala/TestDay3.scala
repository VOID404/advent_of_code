import org.junit.Test
import org.junit.Assert.*

class TestDay3:
  val cmds = Day3.parseInputs(
      List(
        "00100",
        "11110",
        "10110",
        "10111",
        "10101",
        "01111",
        "00111",
        "11100",
        "10000",
        "11001",
        "00010",
        "01010"
      )
    )

  @Test def example(): Unit =
    assertEquals(198, Day3.partOne(cmds))

  @Test def partOne(): Unit =
    assertEquals(3320834, Day3.partOne())

  @Test def oxygen(): Unit =
    assertEquals(23, Day3.oxy(cmds))
  
  @Test def co2(): Unit =
    assertEquals(10, Day3.co2(cmds))
  
  @Test def lifeSupport(): Unit =
    assertEquals(230, Day3.partTwo(cmds))

  @Test def partTwo(): Unit =
    assertEquals(4481199, Day3.partTwo())
