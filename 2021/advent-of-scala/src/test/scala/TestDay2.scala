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
    val res = Day2.partOne(cmds)
    assertEquals(res, 150)

  @Test def partOne(): Unit =
    val res = Day2.partOne()
    assertEquals(res, 1561344)
