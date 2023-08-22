import cats.effect._
import fs2._
import munit._

class LineSplitterSpec extends CatsEffectSuite {

  private val CHAR_LIMIT = 40

  test("lineSplitter handles an empty stream") {
    val input: Stream[IO, Char] = Stream.empty

    val result: List[String] =
      Main.lineSplitter(input, CHAR_LIMIT).compile.toList.unsafeRunSync()

    assertEquals(result, List.empty)
  }

  test("lineSplitter handles a large word") {
    val input: Stream[IO, Char] = Stream.emits("also processes averyverylargeWordWithMoreThan40Characters".toCharArray)

    val result: List[String] =
      Main.lineSplitter(input, CHAR_LIMIT).compile.toList.unsafeRunSync()

    assertEquals(result, List("also processes", "averyverylargeWordWithMoreThan40Characters"))
  }
}