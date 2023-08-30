import cats.effect._
import fs2._

object Main extends IOApp.Simple {
  private val EMPTY_STRING = ""
  private val CHAR_LIMIT   = 40

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

  private def breakWords(input: Stream[IO, Char]): Stream[IO, String] = {
    def go(charStream: Stream[IO, Char], currentWord: String): Pull[IO, String, Unit] =
      charStream.pull.uncons1.flatMap {
        case Some((' ', nextStream))  =>
          Pull.output1(currentWord) >> go(nextStream, EMPTY_STRING)
        case Some((char, nextStream)) =>
          go(nextStream, currentWord + char)
        case None                     =>
          Pull.output1(currentWord) >> Pull.done
      }
    go(input, EMPTY_STRING).stream

  }

  private def breakLines(charLimit: Int): Pipe[IO, String, String] = {
    def go(wordStream: Stream[IO, String], currentLine: String): Pull[IO, String, Unit] =
      wordStream.pull.uncons1.flatMap {
        case Some((nextWord, stream)) =>
          val currentLineSize                  = currentLine.length
          val nextWordSize                     = nextWord.length
          val nextWordExceedsCharLimit         = nextWordSize > charLimit
          val linePlusNextWordExceedsCharLimit = currentLineSize + nextWordSize >= charLimit
          (nextWordExceedsCharLimit, linePlusNextWordExceedsCharLimit, currentLine.isEmpty) match {
            case (true, _, false)      => Pull.output1(currentLine) >> Pull.output1(nextWord) >> go(stream, EMPTY_STRING)
            case (true, _, true)       => Pull.output1(nextWord) >> go(stream, EMPTY_STRING)
            case (false, true, false)  => Pull.output1(currentLine) >> go(stream, nextWord)
            case (false, true, true)   => Pull.output1(nextWord) >> go(stream, EMPTY_STRING)
            case (false, false, true)  => go(stream, nextWord)
            case (false, false, false) => go(stream, s"$currentLine $nextWord")
          }
        case None                     =>
          if (currentLine.isEmpty) Pull.done
          else Pull.output1(currentLine) >> Pull.done
      }
    in => go(in, EMPTY_STRING).stream
  }
}
