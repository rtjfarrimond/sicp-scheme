package com.rtjfarrimond.sicpscheme

import cats.effect.IO
import cats.effect.IOApp
import cats.effect.std.Console
import fs2.Stream
import fs2.io.stdout
import scala.concurrent.duration._

object Main extends IOApp.Simple {

  val run =
    Stream
      .repeatEval(IO(print("> ")) *> Console[IO].readLine)
      .map(Interpreter.interpret)
      .through(fs2.text.utf8.encode)
      .through(stdout[IO])
      .compile
      .drain

}
