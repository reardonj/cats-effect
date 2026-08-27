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
import cats.laws.discipline.arbitrary._

import org.scalacheck.Prop.forAll

class ComboProbeSuite extends BaseScalaCheckSuite {

  override def scalaCheckInitialSeed = "gEuK2d5wMaG6hAem1iPvdbemcyKX2vZY0Qp6nVhrACO="

  override def scalaCheckTestParameters =
    super.scalaCheckTestParameters.withMinSuccessfulTests(2000)

  tickedProperty("probe - combineK vs orElse") { implicit ticker =>
    forAll { (r1: Resource[IO, Int], r2: Resource[IO, Int]) =>
      val lhs = unsafeRun(r1.orElse(r2).use(IO.pure))
      val rhs = unsafeRun((r1 <+> r2).use(IO.pure))

      if (lhs.toString != rhs.toString)
        println(s"MISMATCH orElse=$lhs combineK=$rhs")

      assertEquals(lhs.toString, rhs.toString)
    }
  }
}
