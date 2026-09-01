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

import cats.syntax.all._

import scala.concurrent.duration._

class AckHangSuite extends BaseSuite {

  ticked("probe - cancel a never with onCancelRequested") { implicit ticker =>
    val t1 = for {
      d <- IO.deferred[Unit]
      f <- IO.never[Unit].onCancelRequested(d.complete(()).void).start
      _ <- IO.sleep(1.second)
      cf <- f.cancel.start
      _ <- IO.sleep(1.second)
      cancelDone <- cf.join.map(_.isSuccess).timeoutTo(1.second, IO.pure(false))
      ackRan <- d.tryGet.map(_.isDefined)
    } yield s"cancelReturned=$cancelDone ackRan=$ackRan"

    val t2 = for {
      d <- IO.deferred[Unit]
      f <- IO.never[Unit].onCancel(d.complete(()).void).start
      _ <- IO.sleep(1.second)
      cf <- f.cancel.start
      _ <- IO.sleep(1.second)
      cancelDone <- cf.join.map(_.isSuccess).timeoutTo(1.second, IO.pure(false))
      ackRan <- d.tryGet.map(_.isDefined)
    } yield s"cancelReturned=$cancelDone ackRan=$ackRan"

    def withFa(name: String, fa: IO[Unit]) = {
      val t = for {
        d <- IO.deferred[Unit]
        f <- fa.onCancelRequested(d.complete(()).void).start
        _ <- IO.sleep(1.second)
        cf <- f.cancel.start
        done <- cf.join.as(true).timeoutTo(60.seconds, IO.pure(false))
        ackRan <- d.tryGet.map(_.isDefined)
      } yield s"$name cancelReturnedWithin60s=$done ackRan=$ackRan"
      println(s"ACKHANG ${unsafeRun(t)}")
    }

    withFa("never       ", IO.never[Unit])
    withFa("sleep(10s)  ", IO.sleep(10.seconds))
    withFa("sleep(120s) ", IO.sleep(120.seconds))

    println(s"ACKHANG onCancelRequested: ${unsafeRun(t1)}")
    println(s"ACKHANG onCancel        : ${unsafeRun(t2)}")
  }
}
