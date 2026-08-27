/*
 * Copyright 2020-2025 Typelevel
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package cats.effect

import cats.effect.implicits._
import cats.syntax.all._

import scala.concurrent.duration._

class FoldProbeSuite extends BaseSuite {

  private def selfCancelingRes =
    Resource
      .make(IO.pure(42))(_ => IO.unit)
      .flatMap(_ => Resource.eval(IO.uncancelable { _ => IO.canceled }))

  private def countContinuation(allocate: IO[?]): IO[String] =
    for {
      ctr <- IO.ref(0)
      fib <- IO.uncancelable(poll => poll(allocate).flatMap(_ => ctr.update(_ + 1))).start
      oc <- fib.join
      c <- ctr.get
    } yield s"outcome=$oc ctr=$c"

  real("probe - fold paths") {
    val a = Resource
      .eval(IO.uncancelable(_ => IO.sleep(100.millis)))
      .timeout(10.millis)
      .use_
      .attempt
      .map(r => s"A use_ timeout: $r")

    val b = countContinuation(selfCancelingRes.use_).map(s => s"B use_ selfcancel: $s")
    val c = countContinuation(selfCancelingRes.allocatedCase).map(s =>
      s"C allocatedCase selfcancel: $s")
    val d =
      countContinuation(selfCancelingRes.use(_ => IO.unit)).map(s => s"D use(f) selfcancel: $s")

    (a, b, c, d).tupled.flatMap {
      case (x, y, z, w) => IO(println(List(x, y, z, w).map("PROBE " + _).mkString("\n")))
    }
  }
}
