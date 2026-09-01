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

import scala.concurrent.duration._

class PollProbeSuite extends BaseSuite {

  type F[A] = OptionT[IO, A]

  ticked("probe - default onCancelRequested via OptionT") { implicit ticker =>
    val G = Concurrent[F]

    val cancelableRan = for {
      ref <- IO.ref(0)
      fib <- G.cancelable(G.never[Unit], OptionT.liftF(ref.update(_ + 1))).value.start
      _ <- IO.sleep(1.second)
      _ <- fib.cancel
      v <- ref.get
    } yield v

    val racePairAck = for {
      ref <- IO.ref(0)
      fib <- G.racePair(G.never[Unit], G.never[Unit]).value.guarantee(ref.update(_ + 1)).start
      _ <- IO.sleep(1.second)
      _ <- fib.cancel
      v <- ref.get
    } yield v

    val ioCancelable = for {
      ref <- IO.ref(0)
      fib <- IO.never[Unit].cancelable(ref.update(_ + 1)).start
      _ <- IO.sleep(1.second)
      _ <- fib.cancel
      v <- ref.get
    } yield v

    println(s"POLLPROBE optiont cancelable fin ran = ${unsafeRun(cancelableRan)}")
    println(s"POLLPROBE io      cancelable fin ran = ${unsafeRun(ioCancelable)}")
    println(s"POLLPROBE racePair finished  = ${unsafeRun(racePairAck)}")
  }
}
