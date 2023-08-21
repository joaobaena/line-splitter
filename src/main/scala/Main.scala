import cats.effect._
import fs2._

object Main extends IOApp.Simple {
  private val EMPTY_STRING = ""
  private val CHAR_LIMIT = 40


  def run: IO[Unit] = {
    val sampleText =
      "In 1991, while studying computer science at University of Helsinki, Linus Torvalds began a project that later became the Linux kernel. He wrote the program specifically for the hardware he was using and independent of an operating system because he wanted to use the functions of his new PC with an 80386 processor. Development was done on MINIX using the GNU C Compiler."

    val input: Stream[IO, Char] = Stream.emits(sampleText.toCharArray)

    val output: Stream[IO, Unit] =
      lineSplitter(input, CHAR_LIMIT).evalMap(line => IO(println(line)))

    output.compile.drain
  }

  def lineSplitter(input: Stream[IO, Char], charLimit: Int): Stream[IO, String] =
    breakWords(input).through(breakLines(charLimit))

  private def breakWords(input: Stream[IO, Char]): Stream[IO, String] =
    input.chunks
      .fold(EMPTY_STRING)((acc, chunk) => acc + chunk.toList.mkString)
      .flatMap { sentence =>
        val words = sentence.split("\\s+")
        Stream.emits(words)
      }

  private def breakLines(charLimit: Int): Pipe[IO, String, String] = {
    def go(wordStream: Stream[IO, String], currentLine: String): Pull[IO, String, Unit] =
      wordStream.pull.uncons1.flatMap {
        case Some((nextWord, stream)) =>
          val currentLineSize = currentLine.length
          val nextWordSize    = nextWord.length
          if (nextWordSize > charLimit)
            if (currentLineSize > 0) Pull.output1(currentLine) >> Pull.output1(nextWord) >> go(stream, EMPTY_STRING)
            else Pull.output1(nextWord) >> go(stream, EMPTY_STRING)
          else if (currentLineSize + nextWordSize >= charLimit)
            Pull.output1(currentLine) >> go(stream, EMPTY_STRING)
          else go(stream, if (currentLineSize > 0) s"$currentLine $nextWord" else nextWord)
        case None                     =>
          Pull.done
      }
    in => go(in, EMPTY_STRING).stream
  }

}
