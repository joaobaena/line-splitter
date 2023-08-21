import cats.Eval
import cats.effect._
import fs2._

object Main extends IOApp.Simple {
  def run: IO[Unit] = {
    val text =
      "In 1991, while studying computer science at University of Helsinki, Linus Torvalds began a project that later became the Linux kernel. He wrote the program specifically for the hardware he was using and independent of an operating system because he wanted to use the functions of his new PC with an 80386 processor. Development was done on MINIX using the GNU C Compiler."

    val charLimit = 40
    val input: Stream[IO, Char] = Stream.emits(text.toCharArray)

    val output: Stream[IO, Unit] =
      breakLines(input, charLimit).evalMap(line => IO(println(line)))

    output.compile.drain
  }

  def breakLines(input: Stream[IO, Char], charLimit: Int): Stream[IO, String] =
    input
      .chunks
      .fold("")((acc, chunk) => acc + chunk.toList.mkString)
      .flatMap { sentence =>
        val words = sentence.split("\\s+")
        Stream.emits(words)
      }


}
