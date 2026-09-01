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

import cats.data.OptionT
import cats.effect.kernel.Concurrent
import cats.syntax.all._

class LawProbeSuite extends BaseSuite {

  private def pureIdentity[F[_]](implicit F: Concurrent[F]): F[Int] =
    F.onCancelRequested(F.pure(42), F.unit)

  private def errorIdentity[F[_]](implicit F: Concurrent[F]): F[Either[Throwable, Int]] =
    F.attempt(F.onCancelRequested(F.raiseError[Int](new RuntimeException("boom")), F.unit))

  private def ackRuns[F[_]](implicit F: Concurrent[F]): F[Unit] =
    (F.deferred[Unit], F.deferred[Unit]).flatMapN { (started, d) =>
      val body = F.onCancelRequested(
        F.productR(F.void(started.complete(())))(F.never[Unit]),
        F.void(d.complete(())))

      F.flatMap(F.start(body))(f => F.productR(started.get)(F.productR(f.cancel)(d.get)))
    }

  private def lossless[F[_]](implicit F: Concurrent[F]): F[Int] =
    (F.deferred[Unit], F.deferred[Unit], F.deferred[Int]).flatMapN { (started, d, out) =>
      val body = F.uncancelable { poll =>
        F.productR(F.void(started.complete(())))(
          F.flatMap(
            F.onCancelRequested(F.map(poll(d.get))(_ => 42), F.void(d.complete(())))
          )(r => F.void(out.complete(r))))
      }

      F.flatMap(F.start(body))(f => F.productR(started.get)(F.productR(f.cancel)(out.get)))
    }

  ticked("probe - candidate laws") { implicit ticker =>
    type OT[A] = OptionT[IO, A]

    println(s"LAW io      pureIdentity  = ${unsafeRun(pureIdentity[IO])}")
    println(s"LAW optiont pureIdentity  = ${unsafeRun(pureIdentity[OT].value)}")
    println(s"LAW io      errorIdentity = ${unsafeRun(errorIdentity[IO]).toString.take(50)}")
    println(
      s"LAW optiont errorIdentity = ${unsafeRun(errorIdentity[OT].value).toString.take(50)}")
    println(s"LAW io      ackRuns       = ${unsafeRun(ackRuns[IO])}")
    println(s"LAW optiont ackRuns       = ${unsafeRun(ackRuns[OT].value)}")
    println(s"LAW io      lossless      = ${unsafeRun(lossless[IO])}")
    println(s"LAW optiont lossless      = ${unsafeRun(lossless[OT].value)}")
  }
}
