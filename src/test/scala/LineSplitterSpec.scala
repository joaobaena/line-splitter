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

  test("lineSplitter handles large words") {
    val aLongWord = "averyverylargeWordWithMoreThan40Characters1"
    val input: Stream[IO, Char] = Stream.emits(s"also processes $aLongWord".toCharArray)

    val result: List[String] =
      Main.lineSplitter(input, CHAR_LIMIT).compile.toList.unsafeRunSync()

    assertEquals(result, List("also processes", aLongWord))
  }

  test("lineSplitter works on the proposed string") {
    val sampleText =
      "In 1991, while studying computer science at University of Helsinki, Linus Torvalds began a project that later became the Linux kernel. He wrote the program specifically for the hardware he was using and independent of an operating system because he wanted to use the functions of his new PC with an 80386 processor. Development was done on MINIX using the GNU C Compiler."

    val input: Stream[IO, Char] = Stream.emits(sampleText.toCharArray)

    val result: List[String] =
      Main.lineSplitter(input, CHAR_LIMIT).compile.toList.unsafeRunSync()

    val expected = List(
      "In 1991, while studying computer science",
      "at University of Helsinki, Linus",
      "Torvalds began a project that later",
      "became the Linux kernel. He wrote the",
      "program specifically for the hardware he",
      "was using and independent of an",
      "operating system because he wanted to",
      "use the functions of his new PC with an",
      "80386 processor. Development was done on",
      "MINIX using the GNU C Compiler."
    )
    assertEquals(result, expected)
  }
}
