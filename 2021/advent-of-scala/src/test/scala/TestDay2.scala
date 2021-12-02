import org.junit.Test
import org.junit.Assert.*

class TestDay2:
  @Test def example(): Unit =
    val cmds = List(
      "forward 5",
      "down 5",
      "forward 8",
      "up 3",
      "down 8",
      "forward 2"
    )
    assertEquals(150, Day2.partOne(cmds))

  @Test def partOne(): Unit =
    assertEquals(1561344, Day2.partOne())
  
  @Test def exampleV2(): Unit =
    val cmds = List(
      "forward 5",
      "down 5",
      "forward 8",
      "up 3",
      "down 8",
      "forward 2"
    )
    assertEquals(900, Day2.partTwo(cmds))
  
  @Test def partTwo(): Unit =
    assertEquals(1848454425, Day2.partTwo())
