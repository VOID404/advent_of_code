import org.junit.Test
import org.junit.Assert.*

class TestDay1:
  @Test def partOne(): Unit =
    assertEquals(1139, Day1.partOne())

  @Test def partTwo(): Unit =
    assertEquals(1103, Day1.partTwo())
