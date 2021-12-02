import org.junit.Test
import org.junit.Assert.*

class TestDay1:
  @Test def partOne(): Unit =
    assertEquals(Day1.partOne(), 1139)

  @Test def partTwo(): Unit =
    assertEquals(Day1.partTwo(), 1103)
