package org.scalajos.precise.testerrors

import org.scalajos.precise.errors.either.{Either2Error, Either3Error, Either4Error, Either5Error}
import org.scalajos.precise.errors.{Expected, FaultOr, Outcome, SingleDomainError}
import org.scalajos.precise.errors.syntax._
import org.scalajos.precise.testerrors.services.A.{ErrorA1, ErrorA2, ErrorA3, ErrorA4, ErrorA5, ExceptionA}
import org.scalajos.precise.testerrors.services.B.{ErrorB1, ErrorB2, ExceptionB}
import org.scalajos.precise.testerrors.services.C.{ErrorC1, ErrorC1C2, ErrorC2, ErrorC2C3, ErrorC3, ExceptionC}
import org.scalajos.precise.testerrors.services.D.{ErrorD1, ErrorD1D2D3, ErrorD2, ErrorD3, ExceptionD}
import org.scalajos.precise.testerrors.services.E.{ErrorE1E2, ErrorE1E2E3, ErrorE1E2E3E4, ErrorE1E2E3E4E5, ExceptionE}
import org.scalajos.precise.testerrors.services.F.{ExceptionX, ExceptionY}
import org.scalajos.precise.testerrors.services.{A, B, C, D, E, F, ZioServicePattern}
import org.scalatest.flatspec.AnyFlatSpec
import zio.{Has, IO, Task, ZIO}

class TestPreciseError extends AnyFlatSpec {
  val runtime:zio.Runtime[zio.ZEnv] = zio.Runtime.default

  "multiple faults" should "compose in covariant way (using the Least Upper Bound LUB type)" in {
    {
      val action1: ZIO[Any,FaultOr[ExceptionX,Any],Int] = F.resultOrExceptionX("42").toFaultOr
      val action2: ZIO[Any,FaultOr[Throwable,Any],Int] = F.resultOrThrowable("fault").toFaultOr

      // the fault LUB is Throwable
      val p: ZIO[Any,FaultOr[Throwable,Any],Int] = for {
        _ <- action1
        _ <- action2
      } yield 42

      val res = runtime.unsafeRun(p.flip)
      assert(res.cause.fold(_ => true, _ => false))
    }

    {
      val action1: ZIO[Any,FaultOr[ExceptionX,Any],Int] = F.resultOrExceptionX("42").toFaultOr
      val action2: ZIO[Any,FaultOr[ExceptionY,Any],Int] = F.resultOrExceptionY("fault").toFaultOr

      // the fault LUB is Exception
      val p: ZIO[Any,FaultOr[Exception,Any],Int] = for {
        _ <- action1
        _ <- action2
      } yield 42

      val res = runtime.unsafeRun(p.flip)
      assert(res.cause.fold(_ => true, _ => false))
    }
  }

  "multiple single errors from same domain error ADT" should "compose" in {
    def testTemplate(a1: IO[ExceptionA, Outcome[ErrorA1, Int]],
                     a2: IO[ExceptionA, Outcome[ErrorA2, Int]]): ZIO[Any, FaultOr[ExceptionA, _ >: ErrorA1 with ErrorA2],Int] = {
      // this action can produce a result, a domain error ErrorA1 or a system fault ExceptionA
      val action1: ZIO[Any,FaultOr[ExceptionA,ErrorA1],Int] = a1.toFaultOr
      // this action can produce a result, a domain error ErrorA2 or a system fault ExceptionA
      val action2: ZIO[Any,FaultOr[ExceptionA,ErrorA2],Int] = a2.toFaultOr

      // to compose domain errors, I need to use their type bound
      val action1ComposableError: ZIO[Any,FaultOr[ExceptionA, _ >: ErrorA1],Int] = action1.composableDomainError
      val action2ComposableError: ZIO[Any,FaultOr[ExceptionA, _ >: ErrorA2],Int] = action2.composableDomainError

      // note that errors do compose nicely in for comprehension
      // beware that intellij type engine is quickly lost, use Metals to get the right (but verbose) type
      // ZIO[Any,errors.FaultOr[services.A.ExceptionA, _ >: org.scalajos.precise.testerrors.services.A.ErrorA1 with org.scalajos.precise.testerrors.services.A.ErrorA2],Int]
      val p: ZIO[Any,FaultOr[ExceptionA, _ >: ErrorA1 with ErrorA2],Int] = for {
        _ <- action1ComposableError
        _ <- action2ComposableError
      } yield 42

      p
    }

    assert(runtime.unsafeRun(testTemplate(A.outcomeA1("42"), A.outcomeA2("42"))) == 42)

    {
      val res: FaultOr[ExceptionA, _ >: ErrorA1 with ErrorA2] = runtime.unsafeRun(testTemplate(A.outcomeA1("ErrorA1"), A.outcomeA2("42")).flip)
      assert(res.cause.fold(_ => false, e => e.isInstanceOf[ErrorA1]))
    }

    {
      val res: FaultOr[ExceptionA, _ >: ErrorA1 with ErrorA2] = runtime.unsafeRun(testTemplate(A.outcomeA1("42"), A.outcomeA2("ErrorA2")).flip)
      assert(res.cause.fold(_ => false, e => e.isInstanceOf[ErrorA2]))
    }

    {
      val res: FaultOr[ExceptionA, _ >: ErrorA1 with ErrorA2] = runtime.unsafeRun(testTemplate(A.outcomeA1("42"), A.outcomeA2("fault")).flip)
      assert(res.cause.fold(_ => true, _ => false))
    }

    {
      val action1: ZIO[Any,FaultOr[ExceptionA, _ >: ErrorA1],Int] = A.outcomeA1("42").toFaultOr.composableDomainError

      // try composition with a function which has no domain error, only a result or a fault
      // as there is no domain error , it's already composable
      val action2: ZIO[Any,FaultOr[Throwable,Any],Int] = A.resultOrFault("42").toFaultOr

      // notice the produced domain error type which is type bound based
      val p: ZIO[Any,FaultOr[Throwable, _ >: ErrorA1],Int] = for {
        _ <- action1
        _ <- action2
      } yield 42

      val res  = runtime.unsafeRun(p)
      assert(res == 42)
    }

    {
      val action1: ZIO[Any, FaultOr[ExceptionA, _ >: ErrorA1],Int] = A.outcomeA1("42").toFaultOr.composableDomainError
      val action2: ZIO[Any, FaultOr[Throwable, Any],Int] = A.resultOrFault("fault").toFaultOr

      val p: ZIO[Any, FaultOr[Throwable, _ >: ErrorA1],Int] = for {
        _ <- action1
        _ <- action2
      } yield 42

      val res = runtime.unsafeRun(p.flip)
      assert(res.cause.fold(_ => true, _ => false))
    }

    {
      val action1: ZIO[Any, FaultOr[ExceptionA, _ >: ErrorA1], Int] = A.outcomeA1("42").toFaultOr.composableDomainError
      val action2: ZIO[Any, FaultOr[ExceptionA, _ >: ErrorA2], Int] = A.outcomeA2("fault").toFaultOr.composableDomainError
      val action3: ZIO[Any, FaultOr[Throwable, Any], Int] = A.resultOrFault("42").toFaultOr

      // type obtained from metals
      // ZIO[Any,errors.FaultOr[Throwable, _ >: org.scalajos.precise.testerrors.services.A.ErrorA1 with org.scalajos.precise.testerrors.services.A.ErrorA2],Int]
      val p: ZIO[Any,FaultOr[Throwable, _ >: ErrorA1 with ErrorA2],Int] = for {
        _ <- action1
        _ <- action2
        _ <- action3
      } yield 42

      val res: FaultOr[Throwable, _ >: ErrorA1 with ErrorA2] = runtime.unsafeRun(p.flip)
      assert(res.cause.fold(_ => true, _ => false))
    }
  }

  "multiple single errors from 2 different domain error ADTs" should "compose" in {
    def testTemplate(a1: IO[ExceptionA, Outcome[ErrorA1, Int]],
                     a2: IO[ExceptionB, Outcome[ErrorB1, Int]]): ZIO[Any, FaultOr[Exception, _ >: ErrorB1 with ErrorA1], Int] = {
      val action1: ZIO[Any, FaultOr[ExceptionA, _ >: ErrorA1], Int] = a1.toFaultOr.composableDomainError
      val action2: ZIO[Any, FaultOr[ExceptionB, _ >: ErrorB1], Int] = a2.toFaultOr.composableDomainError

      // beware that intellij type engine is quickly lost, use Metals to get the right type
      val p: ZIO[Any,FaultOr[Exception, _ >: ErrorA1 with ErrorB1],Int] = for {
        _ <- action1
        _ <- action2
      } yield 42

      p
    }

    {
      val res = runtime.unsafeRun(testTemplate(A.outcomeA1("42"), B.outcomeB1("42")))
      assert(res == 42)
    }

    {
      val res = runtime.unsafeRun(testTemplate(A.outcomeA1("42"), B.outcomeB1("ErrorB1")).flip)
      assert(res.cause.fold(_ => false, e => e.isInstanceOf[ErrorB1]))
    }

    // something more complex...
    {
      val action1: ZIO[Any, FaultOr[ExceptionA, _ >: ErrorA1],Int] = A.outcomeA1("42").toFaultOr.composableDomainError
      val action2: ZIO[Any, FaultOr[ExceptionB, _ >: ErrorB1],Int] = B.outcomeB1("42").toFaultOr.composableDomainError
      val action3: ZIO[Any, FaultOr[ExceptionA, _ >: ErrorA2],Int] = A.outcomeA2("42").toFaultOr.composableDomainError
      val action4: ZIO[Any, FaultOr[ExceptionB, _ >: ErrorB2],Int] = B.outcomeB2("ErrorB2").toFaultOr.composableDomainError

      // metals hint gives
      // ZIO[Any,errors.FaultOr[Exception, _ >: org.scalajos.precise.testerrors.services.A.ErrorA1 with org.scalajos.precise.testerrors.services.B.ErrorB1 with org.scalajos.precise.testerrors.services.A.ErrorA2 with org.scalajos.precise.testerrors.services.B.ErrorB2],Int]
      // do some 'compaction'...
      val p: ZIO[Any, FaultOr[Exception, _ >: ErrorA1 with ErrorB1 with ErrorA2 with ErrorB2],Int] = for {
        _ <- action1
        _ <- action2
        _ <- action3
        _ <- action4
      } yield 42

      val res: FaultOr[Exception, _ >: ErrorA1 with ErrorB1 with ErrorA2 with ErrorB2] = runtime.unsafeRun(p.flip)
      assert(res.cause.fold(_ => false, e => e.isInstanceOf[ErrorB2]))
    }

    {
      val action1: ZIO[Any, FaultOr[ExceptionA, _ >: ErrorA1],Int] = A.outcomeA1("42").toFaultOr.composableDomainError
      val action2: ZIO[Any, FaultOr[ExceptionB, _ >: ErrorB1],Int] = B.outcomeB1("42").toFaultOr.composableDomainError
      val action3: ZIO[Any, FaultOr[Throwable, Any],Int] = A.resultOrFault("42").toFaultOr

      val p: ZIO[Any, FaultOr[Throwable, _ >: ErrorA1 with ErrorB1],Int] = for {
        _ <- action1
        _ <- action2
        _ <- action3
      } yield 42

      val res = runtime.unsafeRun(p)
      assert(res == 42)
    }

    {
      val action1: ZIO[Any, FaultOr[ExceptionA, _ >: ErrorA1],Int] = A.outcomeA1("42").toFaultOr.composableDomainError
      val action2: ZIO[Any, FaultOr[ExceptionB, _ >: ErrorB1],Int] = B.outcomeB1("42").toFaultOr.composableDomainError
      val action3: ZIO[Any, FaultOr[Throwable, Any],Int] = A.resultOrFault("fault").toFaultOr

      val p: ZIO[Any, FaultOr[Throwable, _ >: ErrorA1 with ErrorB1],Int] = for {
        _ <- action1
        _ <- action2
        _ <- action3
      } yield 42

      val res: FaultOr[Throwable, _ >: ErrorA1 with ErrorB1] = runtime.unsafeRun(p.flip)
      assert(res.cause.fold(_ => true, _ => false))
    }
  }

  "zio fromOption" should "map to Outcome" in {
    def withOption(a: Int): Option[Float] = {
      println("withOption")
      if (a == 0) None else Some(1F/a)
    }

    // Option[Nothing] -> the only possible value is None
    val io: IO[Option[Nothing], Float] = ZIO.fromOption(withOption(1))

    final case class EmptyResult(msg: String = "") extends SingleDomainError

    val outcome: ZIO[Any,Nothing,Outcome[EmptyResult,Float]] = io.toOutcome(EmptyResult("result is empty"))

    val faultor: ZIO[Any,FaultOr[Nothing,EmptyResult],Float] = outcome.toFaultOr

    // composition
    val action1: ZIO[Any,FaultOr[ExceptionA,ErrorA1],Int] = A.outcomeA1("42").toFaultOr

    // to compose domain errors, I need to use their type bound
    val action1ComposableError: ZIO[Any,FaultOr[ExceptionA, _ >: ErrorA1],Int] = action1.composableDomainError
    val action2ComposableError: ZIO[Any,FaultOr[Nothing, _ >: EmptyResult],Float] = faultor.composableDomainError

    val p: ZIO[Any,FaultOr[ExceptionA, _ >: ErrorA1 with EmptyResult],Int] = for {
      _ <- action1ComposableError
      _ <- action2ComposableError
    } yield 42

    assert(runtime.unsafeRun(p) == 42)
  }

  "domain errors from multiple program fragments" should "compose" in {
    val action1: ZIO[Any, FaultOr[ExceptionA, _ >: ErrorA1], Int] = A.outcomeA1("42").toFaultOr.composableDomainError
    val action2: ZIO[Any, FaultOr[ExceptionB, _ >: ErrorB1], Int] = B.outcomeB1("42").toFaultOr.composableDomainError

    // beware that intellij type engine is quickly lost, use Metals to get the right type
    val p1: ZIO[Any,FaultOr[Exception, _ >: ErrorA1 with ErrorB1],Int] = for {
      _ <- action1
      _ <- action2
    } yield 42

    val action3: ZIO[Any, FaultOr[ExceptionA, _ >: ErrorA2], Int] = A.outcomeA2("42").toFaultOr.composableDomainError
    val action4: ZIO[Any, FaultOr[ExceptionB, _ >: ErrorB2], Int] = B.outcomeB2("42").toFaultOr.composableDomainError

    val p2: ZIO[Any,FaultOr[Exception, _ >: ErrorA2 with ErrorB2],Int] = for {
      _ <- action3
      _ <- action4
    } yield 42
    
    val program: ZIO[Any,FaultOr[Exception, _ >: ErrorA1 with ErrorB1 with ErrorA2 with ErrorB2],Int] = for {
      _ <- p1
      _ <- p2
    } yield 42

    val res = runtime.unsafeRun(program)
    assert(res == 42)
  }

  "multiple errors (types-bound from Either2Error) from same domain error ADT" should "compose" in {
    // ErrorC1C2 is an alias for Either2Error[ErrorC1, ErrorC2]
    def testTemplate(a1: IO[ExceptionC, Outcome[ErrorC3, Int]],
                     a2: IO[ExceptionC, Outcome[ErrorC1C2, Int]]): ZIO[Any, FaultOr[Throwable, _ >: ErrorC3 with ErrorC1 with ErrorC2],Int] = {
      val action1: ZIO[Any,FaultOr[ExceptionC,ErrorC3],Int] = a1.toFaultOr
      val action2: ZIO[Any,FaultOr[ExceptionC,Either2Error[ErrorC1,ErrorC2]],Int] = a2.toFaultOr

      val action1ComposableError: ZIO[Any,FaultOr[ExceptionC, _ >: ErrorC3],Int] = action1.composableDomainError
      // Either2Error is converted into its type bound based version so it can be composed
      val action2ComposableError: ZIO[Any,FaultOr[ExceptionC, _ >: ErrorC1 with ErrorC2],Int] = action2.composableDomainError

      // Metals infers type
      // ZIO[Any,errors.FaultOr[services.C.ExceptionC, _ >: org.scalajos.precise.testerrors.services.C.ErrorC3 with org.scalajos.precise.testerrors.services.C.ErrorC1 with org.scalajos.precise.testerrors.services.C.ErrorC2],Int]
      // 'compact' the given type manually
      val p: ZIO[Any,FaultOr[ExceptionC, _ >: ErrorC3 with ErrorC1 with ErrorC2],Int] = for {
        _ <- action1ComposableError
        _ <- action2ComposableError
      } yield 42

      p
    }

    assert(runtime.unsafeRun(testTemplate(C.outcomeC3("42"), C.outcomeC1C2("42"))) == 42)

    {
      val res: FaultOr[Throwable, _ >: ErrorC3 with ErrorC1 with ErrorC2] = runtime.unsafeRun(testTemplate(C.outcomeC3("42"), C.outcomeC1C2("ErrorC1")).flip)
      assert(res.cause.fold(_ => false, e => e.isInstanceOf[ErrorC1]))
    }

    {
      val res: FaultOr[Throwable, _ >: ErrorC3 with ErrorC1 with ErrorC2] = runtime.unsafeRun(testTemplate(C.outcomeC3("42"), C.outcomeC1C2("ErrorC2")).flip)
      assert(res.cause.fold(_ => false, e => e.isInstanceOf[ErrorC2]))
    }

    {
      val res: FaultOr[Throwable, _ >: ErrorC3 with ErrorC1 with ErrorC2] = runtime.unsafeRun(testTemplate(C.outcomeC3("42"), C.outcomeC1C2("fault")).flip)
      assert(res.cause.fold(_ => true, _ => false))
    }
  }

  "multiple overlapping errors (linearized from Either2Error and Either3Error) from same domain error ADT" should "compose" in {
    def testTemplate(a1: IO[ExceptionC, Outcome[ErrorC3, Int]],
                     a2: IO[ExceptionC, Outcome[ErrorC1C2, Int]],
                     a3: IO[ExceptionC, Outcome[ErrorC2C3, Int]]) = {
      val action1: ZIO[Any, FaultOr[ExceptionC, _ >: ErrorC3],Int] = a1.toFaultOr.composableDomainError
      val action2: ZIO[Any, FaultOr[ExceptionC, _ >: ErrorC1 with ErrorC2],Int] = a2.toFaultOr.composableDomainError // FaultOr[Either2Error[ErrorC1, ErrorC2]] -> FaultOr[_ >: ErrorC1 with ErrorC2]
      val action3: ZIO[Any, FaultOr[ExceptionC, _ >: ErrorC2 with ErrorC3],Int] = a3.toFaultOr.composableDomainError // FaultOr[Either2Error[ErrorC2, ErrorC3]] -> FaultOr[_ >: ErrorC2 with ErrorC3]

      // Metals infers type
      // ZIO[Any,errors.FaultOr[services.C.ExceptionC, _ >: org.scalajos.precise.testerrors.services.C.ErrorC1 with org.scalajos.precise.testerrors.services.C.ErrorC2 with org.scalajos.precise.testerrors.services.C.ErrorC2 with org.scalajos.precise.testerrors.services.C.ErrorC3],Int]
      // Notice the redundant ErrorC2 type ! The inferred upper type bound is still valid and compiles OK but it's not 'clean'
      // Manually remove redundant type
      val p: ZIO[Any,FaultOr[ExceptionC, _ >: ErrorC1 with ErrorC2 with ErrorC2 with ErrorC3],Int] = for {
        _ <- action1
        _ <- action2
        _ <- action3
      } yield 42

      p
    }

    assert(runtime.unsafeRun(testTemplate(C.outcomeC3("42"), C.outcomeC1C2("42"), C.outcomeC2C3("42"))) == 42)

    {
      val res: FaultOr[ExceptionC, _ >: ErrorC1 with ErrorC2 with ErrorC3] = runtime.unsafeRun(testTemplate(C.outcomeC3("42"), C.outcomeC1C2("ErrorC1"), C.outcomeC2C3("42")).flip)
      assert(res.cause.fold(_ => false, e => e.isInstanceOf[ErrorC1]))
    }

    {
      val res: FaultOr[ExceptionC, _ >: ErrorC3 with ErrorC1 with ErrorC2] = runtime.unsafeRun(testTemplate(C.outcomeC3("42"), C.outcomeC1C2("ErrorC2"), C.outcomeC2C3("42")).flip)
      assert(res.cause.fold(_ => false, e => e.isInstanceOf[ErrorC2]))
    }

    {
      val res: FaultOr[ExceptionC, _ >: ErrorC3 with ErrorC1 with ErrorC2] = runtime.unsafeRun(testTemplate(C.outcomeC3("42"), C.outcomeC1C2("42"), C.outcomeC2C3("ErrorC2")).flip)
      assert(res.cause.fold(_ => false, e => e.isInstanceOf[ErrorC2]))
    }

    {
      val res: FaultOr[ExceptionC, _ >: ErrorC3 with ErrorC1 with ErrorC2 with ErrorC3] = runtime.unsafeRun(testTemplate(C.outcomeC3("42"), C.outcomeC1C2("42"), C.outcomeC2C3("ErrorC3")).flip)
      assert(res.cause.fold(_ => false, e => e.isInstanceOf[ErrorC3]))
    }

    {
      val res: FaultOr[ExceptionC, _ >: ErrorC1 with ErrorC2 with ErrorC3] = runtime.unsafeRun(testTemplate(C.outcomeC3("42"), C.outcomeC1C2("42"), C.outcomeC2C3("fault")).flip)
      assert(res.cause.fold(_ => true, _ => false))
    }
  }

  "multiple errors ('type bounded'ify from Either2Error and Either3Error) from 2 different domain error ADTs" should "compose" in {
    def testTemplate(a1: IO[ExceptionC, Outcome[ErrorC3, Int]],
                     a2: IO[ExceptionC, Outcome[ErrorC1C2, Int]],
                     a3: IO[ExceptionD, Outcome[ErrorD1D2D3, Int]]) = {
      val action1: ZIO[Any, FaultOr[ExceptionC, _ >: ErrorC3],Int] = a1.toFaultOr.composableDomainError
      val action2: ZIO[Any, FaultOr[ExceptionC, _ >: ErrorC1 with ErrorC2],Int] = a2.toFaultOr.composableDomainError
      val action3: ZIO[Any, FaultOr[ExceptionD, _ >: ErrorD1 with ErrorD2 with ErrorD3],Int] = a3.toFaultOr.composableDomainError

      // Metals infers type
      // ZIO[Any,errors.FaultOr[Exception, _ >: org.scalajos.precise.testerrors.services.C.ErrorC3 with org.scalajos.precise.testerrors.services.C.ErrorC1 with org.scalajos.precise.testerrors.services.C.ErrorC2 with org.scalajos.precise.testerrors.services.D.ErrorD1 with org.scalajos.precise.testerrors.services.D.ErrorD2 with org.scalajos.precise.testerrors.services.D.ErrorD3],Int]
      val p: ZIO[Any,FaultOr[Exception, _ >: ErrorC3 with ErrorC1 with ErrorC2 with ErrorD1 with ErrorD2 with ErrorD3],Int] = for {
        _ <- action1
        _ <- action2
        _ <- action3
      } yield 42

      p
    }

    assert(runtime.unsafeRun(testTemplate(C.outcomeC3("42"), C.outcomeC1C2("42"), D.outcomeD1D2D3("42"))) == 42)

    {
      val res: FaultOr[Exception, _ >: ErrorC3 with ErrorC1 with ErrorC2 with ErrorD1 with ErrorD2 with ErrorD3] = runtime.unsafeRun(testTemplate(C.outcomeC3("42"), C.outcomeC1C2("ErrorC1"), D.outcomeD1D2D3("42")).flip)
      assert(res.cause.fold(_ => false, e => e.isInstanceOf[ErrorC1]))
    }

    {
      val res: FaultOr[Exception, _ >: ErrorC3 with ErrorC1 with ErrorC2 with ErrorD1 with ErrorD2 with ErrorD3] = runtime.unsafeRun(testTemplate(C.outcomeC3("42"), C.outcomeC1C2("ErrorC2"), D.outcomeD1D2D3("42")).flip)
      assert(res.cause.fold(_ => false, e => e.isInstanceOf[ErrorC2]))
    }

    {
      val res: FaultOr[Exception, _ >: ErrorC3 with ErrorC1 with ErrorC2 with ErrorD1 with ErrorD2 with ErrorD3] = runtime.unsafeRun(testTemplate(C.outcomeC3("42"), C.outcomeC1C2("42"), D.outcomeD1D2D3("ErrorD1")).flip)
      assert(res.cause.fold(_ => false, e => e.isInstanceOf[ErrorD1]))
    }

    {
      val res: FaultOr[Exception, _ >: ErrorC3 with ErrorC1 with ErrorC2 with ErrorD1 with ErrorD2 with ErrorD3] = runtime.unsafeRun(testTemplate(C.outcomeC3("42"), C.outcomeC1C2("42"), D.outcomeD1D2D3("ErrorD2")).flip)
      assert(res.cause.fold(_ => false, e => e.isInstanceOf[ErrorD2]))
    }

    {
      val res: FaultOr[Exception, _ >: ErrorC3 with ErrorC1 with ErrorC2 with ErrorD1 with ErrorD2 with ErrorD3] = runtime.unsafeRun(testTemplate(C.outcomeC3("42"), C.outcomeC1C2("42"), D.outcomeD1D2D3("ErrorD3")).flip)
      assert(res.cause.fold(_ => false, e => e.isInstanceOf[ErrorD3]))
    }

    {
      val res: FaultOr[Exception, _ >: ErrorC3 with ErrorC1 with ErrorC2 with ErrorD1 with ErrorD2 with ErrorD3] = runtime.unsafeRun(testTemplate(C.outcomeC3("42"), C.outcomeC1C2("fault"), D.outcomeD1D2D3("42")).flip)
      assert(res.cause.fold(_ => true, _ => false))
    }

    {
      val res: FaultOr[Exception, _ >: ErrorC3 with ErrorC1 with ErrorC2 with ErrorD1 with ErrorD2 with ErrorD3] = runtime.unsafeRun(testTemplate(C.outcomeC3("42"), C.outcomeC1C2("42"), D.outcomeD1D2D3("fault")).flip)
      assert(res.cause.fold(_ => true, _ => false))
    }
  }

  "a single error" should "map to 1-type-bound error" in {
    def testTemplate(a1: IO[ExceptionA, Outcome[ErrorA1, Int]],
                     a2: IO[ExceptionA, Outcome[ErrorA2, Int]]) = {
      val action1: ZIO[Any, FaultOr[ExceptionA, ErrorA1],Int] = a1.toFaultOr

      val adapted: ZIO[Any,FaultOr[ExceptionA,ErrorB1],Int] = action1.mapDomainErrorsTo1( (from: ErrorA1) => ErrorB1(from.id, from) )

      val composableAdapted: ZIO[Any,FaultOr[ExceptionA, _ >: ErrorB1],Int] = adapted.composableDomainError

      // error composition is still possible as 'adapted' error is a linear error
      val action2: ZIO[Any, FaultOr[ExceptionA, _ >: ErrorA2],Int] = a2.toFaultOr.composableDomainError
      val p2: ZIO[Any, FaultOr[ExceptionA, _ >: ErrorB1 with ErrorA2],Int] = for {
        _ <- composableAdapted
        _ <- action2
      } yield 42

      p2
    }

    {
      val testCase1 = testTemplate(A.outcomeA1("42"), A.outcomeA2("42"))
      val res1 = runtime.unsafeRun(testCase1)
      //println(res1)
      assert(res1 == 42)
    }

    {
      val testCase1 = testTemplate(A.outcomeA1("ErrorA1"), A.outcomeA2("42"))
      val res1 = runtime.unsafeRun(testCase1.flip)
      //println(res1)
      assert(res1.cause.fold(_ => false, e => e.isInstanceOf[ErrorB1]))
    }
  }

  "a 1-type-bound error" should "map to 1-type-bound error" in {
    def testTemplate(a1: IO[ExceptionA, Outcome[ErrorA1, Int]],
                     a2: IO[ExceptionA, Outcome[ErrorA2, Int]]) = {
      val action1: ZIO[Any, FaultOr[ExceptionA, _ >: ErrorA1],Int] = a1.toFaultOr.composableDomainError

      val p1: ZIO[Any, FaultOr[ExceptionA, _ >: ErrorA1],Int] = for {
        _ <- action1
      } yield 42

      // remember that 'from' type represents a a type bound error (eg: _ >: E1 with E2 with ...) that requires match/case to map errors
      // This is the case with only one error but one still needs a match/case
      // notice that the fault type (ExceptionA) is unchanged, mapping is only applied on the domain error part
      val adapted: ZIO[Any, FaultOr[ExceptionA, _ >: ErrorB1],Int] = p1.mapDomainErrorsTo1{ from: Expected[_ >: ErrorA1] =>
        from.cause match {case ea1@ErrorA1(id, _) => ErrorB1(id, ea1)} // chain original error with the new one
      }

      // error composition is still possible as 'adapted' error is a linear error
      val action2: ZIO[Any, FaultOr[ExceptionA, _ >: ErrorA2],Int] = a2.toFaultOr.composableDomainError
      val p2: ZIO[Any, FaultOr[ExceptionA, _ >: ErrorB1 with ErrorA2],Int] = for {
        _ <- adapted
        _ <- action2
      } yield 42

      p2
    }

    {
      val testCase1 = testTemplate(A.outcomeA1("42"), A.outcomeA2("42"))
      val res1 = runtime.unsafeRun(testCase1)
      //println(res1)
      assert(res1 == 42)
    }

    {
      val testCase1 = testTemplate(A.outcomeA1("ErrorA1"), A.outcomeA2("42"))
      val res1 = runtime.unsafeRun(testCase1.flip)
      //println(res1)
      assert(res1.cause.fold(_ => false, e => e.isInstanceOf[ErrorB1]))
    }
  }

  "an Either2Error error" should "map to 1-type-bound error" in {
    def testTemplate(a1: IO[ExceptionC, Outcome[ErrorC1C2, Int]],
                     a2: IO[ExceptionA, Outcome[ErrorA2, Int]]) = {
      val action1: ZIO[Any, FaultOr[ExceptionC, ErrorC1C2],Int] = a1.toFaultOr  // ErrorC1C2 is an alias for Either2Error[ErrorC1, ErrorC2]

      val adapted: ZIO[Any, FaultOr[ExceptionC, ErrorB1], Int] = action1.mapDomainErrorsTo1((first: ErrorC1) => ErrorB1(first.id, first),
                                                                                            (second: ErrorC2) => ErrorB1(second.id, second))

      val composableAdapted: ZIO[Any,FaultOr[ExceptionC, _ >: ErrorB1],Int] = adapted.composableDomainError

      // error composition is still possible
      val s3: ZIO[Any, FaultOr[ExceptionA, _ >: ErrorA2],Int] = a2.toFaultOr
      val p2: ZIO[Any, FaultOr[Exception, _ >: ErrorB1 with ErrorA2],Int] = for {
        _ <- composableAdapted
        _ <- s3
      } yield 42

      p2
    }

    {
      val testCase = testTemplate(C.outcomeC1C2("42"), A.outcomeA2("42"))
      val res = runtime.unsafeRun(testCase)
      //println(res1)
      assert(res == 42)
    }

    {
      val testCase = testTemplate(C.outcomeC1C2("ErrorC1"), A.outcomeA2("42"))
      val res1 = runtime.unsafeRun(testCase.flip)
      //println(res1)
      assert(res1.cause.fold(_ => false, e => e.isInstanceOf[ErrorB1]))
    }

    {
      val testCase = testTemplate(C.outcomeC1C2("ErrorC2"), A.outcomeA2("42"))
      val res1 = runtime.unsafeRun(testCase.flip)
      //println(res1)
      assert(res1.cause.fold(_ => false, e => e.isInstanceOf[ErrorB1]))
    }

    {
      val testCase = testTemplate(C.outcomeC1C2("42"), A.outcomeA2("ErrorA2"))
      val res1 = runtime.unsafeRun(testCase.flip)
      //println(res1)
      assert(res1.cause.fold(_ => false, e => e.isInstanceOf[ErrorA2]))
    }

    {
      val testCase = testTemplate(C.outcomeC1C2("fault"), A.outcomeA2("42"))
      val res1 = runtime.unsafeRun(testCase.flip)
      //println(res1)
      assert(res1.cause.fold(_ => true, _ => false))
    }

    {
      val testCase = testTemplate(C.outcomeC1C2("fault"), A.outcomeA2("ErrorA2"))
      val res1 = runtime.unsafeRun(testCase.flip)
      //println(res1)
      assert(res1.cause.fold(_ => true, _ => false))
    }

    {
      val testCase = testTemplate(C.outcomeC1C2("42"), A.outcomeA2("fault"))
      val res1 = runtime.unsafeRun(testCase.flip)
      //println(res1)
      assert(res1.cause.fold(_ => true, _ => false))
    }

    {
      val testCase = testTemplate(C.outcomeC1C2("ErrorC1"), A.outcomeA2("fault"))
      val res1 = runtime.unsafeRun(testCase.flip)
      //println(res1)
      assert(res1.cause.fold(_ => false, e => e.isInstanceOf[ErrorB1]))
    }

    {
      val testCase = testTemplate(C.outcomeC1C2("ErrorC2"), A.outcomeA2("fault"))
      val res1 = runtime.unsafeRun(testCase.flip)
      //println(res1)
      assert(res1.cause.fold(_ => false, e => e.isInstanceOf[ErrorB1]))
    }
  }

  "a 2-type-bound error" should "map to 1-type-bound error" in {
    def testTemplate1(a1: IO[ExceptionA, Outcome[ErrorA1, Int]],
                      a2: IO[ExceptionA, Outcome[ErrorA2, Int]],
                      a3: IO[ExceptionA, Outcome[ErrorA3, Int]]) = {
      val action1: ZIO[Any, FaultOr[ExceptionA, _ >: ErrorA1],Int] = a1.toFaultOr.composableDomainError
      val action2: ZIO[Any, FaultOr[ExceptionA, _ >: ErrorA2],Int] = a2.toFaultOr.composableDomainError

      val p: ZIO[Any, FaultOr[ExceptionA, _ >: ErrorA1 with ErrorA2],Int] = for {
        _ <- action1
        _ <- action2
      } yield 42

      def toErrorB1(from: Expected[_ >: ErrorA1 with ErrorA2]): ErrorB1 = from.cause match {
        case a1@ErrorA1(id, _) => ErrorB1(id, a1)
        case a2@ErrorA2(id, _) => ErrorB1(id, a2)
      }
      val adapted: ZIO[Any, FaultOr[ExceptionA, _ >: ErrorB1],Int] = p.mapDomainErrorsTo1(toErrorB1)

      // error composition is still possible
      val action3: ZIO[Any, FaultOr[ExceptionA, _ >: ErrorA3],Int] = a3.toFaultOr.composableDomainError
      val p2: ZIO[Any, FaultOr[ExceptionA, _ >: ErrorB1 with ErrorA3],Int] = for {
        _ <- adapted
        _ <- action3
      } yield 42

      p2
    }

    def testTemplate2(a1: IO[ExceptionA, Outcome[ErrorA1, Int]],
                      a2: IO[ExceptionA, Outcome[ErrorA2, Int]],
                      a3: IO[ExceptionA, Outcome[ErrorA3, Int]]) = {
      val action1: ZIO[Any, FaultOr[ExceptionA, _ >: ErrorA1],Int] = a1.toFaultOr.composableDomainError
      val action2: ZIO[Any, FaultOr[ExceptionA, _ >: ErrorA2],Int] = a2.toFaultOr.composableDomainError

      val p: ZIO[Any, FaultOr[ExceptionA, _ >: ErrorA1 with ErrorA2],Int] = for {
        _ <- action1
        _ <- action2
      } yield 42

      val adapted: ZIO[Any, FaultOr[ExceptionA, _ >: ErrorB1],Int] = p.mapDomainErrorsTo1 { from: Expected[_ >: ErrorA1 with ErrorA2] =>from.cause match {
        case a1@ErrorA1(id, _) => ErrorB1(id, a1)
        case a2@ErrorA2(id, _) => ErrorB1(id, a2)
      }}

      // error composition is still possible
      val action3: ZIO[Any, FaultOr[ExceptionA, _ >: ErrorA3],Int] = a3.toFaultOr.composableDomainError
      val p2: ZIO[Any, FaultOr[ExceptionA, _ >: ErrorB1 with ErrorA3],Int] = for {
        _ <- adapted
        _ <- action3
      } yield 42

      p2
    }

    {
      val testCase1 = testTemplate1(A.outcomeA1("42"), A.outcomeA2("42"), A.outcomeA3("42"))
      val res1 = runtime.unsafeRun(testCase1)
      //println(res1)
      assert(res1 == 42)

      val testCase2 = testTemplate2(A.outcomeA1("42"), A.outcomeA2("42"), A.outcomeA3("42"))
      val res2 = runtime.unsafeRun(testCase2)
      //println(res2)
      assert(res2 == 42)
    }

    {
      val testCase1 = testTemplate1(A.outcomeA1("ErrorA1"), A.outcomeA2("42"), A.outcomeA3("42"))
      val res1 = runtime.unsafeRun(testCase1.flip)
      //println(res1)
      assert(res1.cause.fold(_ => false, e => e.isInstanceOf[ErrorB1]))

      val testCase2 = testTemplate2(A.outcomeA1("ErrorA1"), A.outcomeA2("42"), A.outcomeA3("42"))
      val res2 = runtime.unsafeRun(testCase2.flip)
      //println(res2)
      assert(res2.cause.fold(_ => false, e => e.isInstanceOf[ErrorB1]))
    }

    {
      val testCase1 = testTemplate1(A.outcomeA1("42"), A.outcomeA2("ErrorA2"), A.outcomeA3("42"))
      val res1 = runtime.unsafeRun(testCase1.flip)
      //println(res1)
      assert(res1.cause.fold(_ => false, e => e.isInstanceOf[ErrorB1]))

      val testCase2 = testTemplate2(A.outcomeA1("42"), A.outcomeA2("ErrorA2"), A.outcomeA3("42"))
      val res2 = runtime.unsafeRun(testCase2.flip)
      //println(res2)
      assert(res2.cause.fold(_ => false, e => e.isInstanceOf[ErrorB1]))
    }

    {
      val testCase1 = testTemplate1(A.outcomeA1("42"), A.outcomeA2("42"), A.outcomeA3("ErrorA3"))
      val res1 = runtime.unsafeRun(testCase1.flip)
      //println(res1)
      assert(res1.cause.fold(_ => false, e => e.isInstanceOf[ErrorA3]))

      val testCase2 = testTemplate2(A.outcomeA1("42"), A.outcomeA2("42"), A.outcomeA3("ErrorA3"))
      val res2 = runtime.unsafeRun(testCase2.flip)
      //println(res2)
      assert(res2.cause.fold(_ => false, e => e.isInstanceOf[ErrorA3]))
    }

    {
      val testCase1 = testTemplate1(A.outcomeA1("fault"), A.outcomeA2("42"), A.outcomeA3("42"))
      val res1 = runtime.unsafeRun(testCase1.flip)
      //println(res1)
      assert(res1.cause.fold(_ => true, _ => false))

      val testCase2 = testTemplate2(A.outcomeA1("fault"), A.outcomeA2("42"), A.outcomeA3("42"))
      val res2 = runtime.unsafeRun(testCase2.flip)
      //println(res2)
      assert(res2.cause.fold(_ => true, _ => false))
    }

    {
      val testCase1 = testTemplate1(A.outcomeA1("42"), A.outcomeA2("fault"), A.outcomeA3("42"))
      val res1 = runtime.unsafeRun(testCase1.flip)
      //println(res1)
      assert(res1.cause.fold(_ => true, _ => false))

      val testCase2 = testTemplate2(A.outcomeA1("42"), A.outcomeA2("fault"), A.outcomeA3("42"))
      val res2 = runtime.unsafeRun(testCase2.flip)
      //println(res2)
      assert(res2.cause.fold(_ => true, _ => false))
    }

    {
      val testCase1 = testTemplate1(A.outcomeA1("42"), A.outcomeA2("42"), A.outcomeA3("fault"))
      val res1 = runtime.unsafeRun(testCase1.flip)
      //println(res1)
      assert(res1.cause.fold(_ => true, _ => false))

      val testCase2 = testTemplate2(A.outcomeA1("42"), A.outcomeA2("42"), A.outcomeA3("fault"))
      val res2 = runtime.unsafeRun(testCase2.flip)
      //println(res2)
      assert(res2.cause.fold(_ => true, _ => false))
    }
  }

  "an Either2Error error" should "map to 2-type-bound error" in {
    // ErrorA1 is mapped to ErrorB1
    // ErrorA2 is mapped to ErrorB2
    def testTemplate(a1: IO[ExceptionC, Outcome[ErrorC1C2, Int]],
                     a2: IO[ExceptionA, Outcome[ErrorA2, Int]]) = {
      val action1: ZIO[Any, FaultOr[ExceptionC, Either2Error[ErrorC1, ErrorC2]], Int] = a1.toFaultOr  // ErrorC1C2 is an alias for Either2Error[ErrorC1, ErrorC2]

      val adapted: ZIO[Any, FaultOr[ExceptionC, Either2Error[ErrorB1, ErrorB2]], Int] = action1.mapDomainErrorsTo2((first: ErrorC1) => ErrorB1(first.id, first),
                                                                                                                   (second: ErrorC2) => ErrorB2(second.id, second))

      val composableAdapted: ZIO[Any,FaultOr[ExceptionC, _ >: ErrorB1 with ErrorB2],Int] = adapted.composableDomainError

      // error composition is still possible
      val action3: ZIO[Any,FaultOr[ExceptionA, _ >: ErrorA2],Int] = a2.toFaultOr.composableDomainError
      val p2: ZIO[Any,FaultOr[Exception, _ >: ErrorB1 with ErrorB2 with ErrorA2],Int] = for {
        _ <- composableAdapted
        _ <- action3
      } yield 42

      p2
    }

    {
      val testCase = testTemplate(C.outcomeC1C2("42"), A.outcomeA2("42"))
      val res = runtime.unsafeRun(testCase)
      //println(res)
      assert(res == 42)
    }

    {
      val testCase = testTemplate(C.outcomeC1C2("ErrorC1"), A.outcomeA2("42"))
      val res = runtime.unsafeRun(testCase.flip)
      //println(res)
      assert(res.cause.fold(_ => false, e => e.isInstanceOf[ErrorB1]))
    }

    {
      val testCase = testTemplate(C.outcomeC1C2("ErrorC2"), A.outcomeA2("42"))
      val res = runtime.unsafeRun(testCase.flip)
      //println(res)
      assert(res.cause.fold(_ => false, e => e.isInstanceOf[ErrorB2]))
    }

    {
      val testCase = testTemplate(C.outcomeC1C2("42"), A.outcomeA2("ErrorA2"))
      val res = runtime.unsafeRun(testCase.flip)
      //println(res)
      assert(res.cause.fold(_ => false, e => e.isInstanceOf[ErrorA2]))
    }
  }

  "a 2-type-bound error" should "map to 2-type-bound error" in {
    def testTemplate(a1: IO[ExceptionA, Outcome[ErrorA1, Int]],
                     a2: IO[ExceptionA, Outcome[ErrorA2, Int]],
                     a3: IO[ExceptionA, Outcome[ErrorA3, Int]]) = {
      val action1: ZIO[Any, FaultOr[ExceptionA, _ >: ErrorA1],Int] = a1.toFaultOr.composableDomainError
      val action2: ZIO[Any, FaultOr[ExceptionA, _ >: ErrorA2],Int] = a2.toFaultOr.composableDomainError

      val p: ZIO[Any, FaultOr[ExceptionA, _ >: ErrorA1 with ErrorA2],Int] = for {
        _ <- action1
        _ <- action2
      } yield 42

      def adapterErrorB1B2(from: Expected[_ >: ErrorA1 with ErrorA2]): Either2Error[ErrorB1, ErrorB2] = from.cause match {
        case a1@ErrorA1(id, _) => Either2Error.first[ErrorB1, ErrorB2](ErrorB1(id, a1))
        case a2@ErrorA2(id, _) => Either2Error.second[ErrorB1, ErrorB2](ErrorB2(id, a2))
      }
      val adapted: ZIO[Any, FaultOr[ExceptionA, _ >: ErrorB1 with ErrorB2],Int] = p.mapDomainErrorsTo2(adapterErrorB1B2)

      // error composition is still possible
      val action3: ZIO[Any, FaultOr[ExceptionA, _ >: ErrorA3],Int] = a3.toFaultOr
      val p2: ZIO[Any, FaultOr[ExceptionA, _ >: ErrorB1 with ErrorB2 with ErrorA3],Int] = for {
        _ <- adapted
        _ <- action3
      } yield 42

      p2
    }

    {
      val testCase = testTemplate(A.outcomeA1("42"), A.outcomeA2("42"), A.outcomeA3("42"))
      val res = runtime.unsafeRun(testCase)
      //println(res)
      assert(res == 42)
    }

    {
      val testCase = testTemplate(A.outcomeA1("ErrorA1"), A.outcomeA2("42"), A.outcomeA3("42"))
      val res = runtime.unsafeRun(testCase.flip)
      //println(res)
      assert(res.cause.fold(_ => false, e => e.isInstanceOf[ErrorB1]))
    }

    {
      val testCase = testTemplate(A.outcomeA1("42"), A.outcomeA2("ErrorA2"), A.outcomeA3("42"))
      val res = runtime.unsafeRun(testCase.flip)
      //println(res)
      assert(res.cause.fold(_ => false, e => e.isInstanceOf[ErrorB2]))
    }

    {
      val testCase = testTemplate(A.outcomeA1("42"), A.outcomeA2("42"), A.outcomeA3("ErrorA3"))
      val res = runtime.unsafeRun(testCase.flip)
      //println(res)
      assert(res.cause.fold(_ => false, e => e.isInstanceOf[ErrorA3]))
    }

    {
      val testCase = testTemplate(A.outcomeA1("fault"), A.outcomeA2("42"), A.outcomeA3("42"))
      val res = runtime.unsafeRun(testCase.flip)
      //println(res)
      assert(res.cause.fold(_ => true, _ => false))
    }

    {
      val testCase = testTemplate(A.outcomeA1("42"), A.outcomeA2("fault"), A.outcomeA3("42"))
      val res = runtime.unsafeRun(testCase.flip)
      //println(res)
      assert(res.cause.fold(_ => true, _ => false))
    }

    {
      val testCase = testTemplate(A.outcomeA1("42"), A.outcomeA2("42"), A.outcomeA3("fault"))
      val res = runtime.unsafeRun(testCase.flip)
      //println(res)
      assert(res.cause.fold(_ => true, _ => false))
    }
  }

  "Either2Error" should "allow bijection" in {
    def testTemplate(a: IO[ExceptionE, Outcome[ErrorE1E2, Int]]) = {
      val either2Source: ZIO[Any, FaultOr[ExceptionE, ErrorE1E2], Int] = a.toFaultOr
      val bijection: ZIO[Any, FaultOr[ExceptionE, Either2Error[ErrorA1, ErrorA2]], Int] = either2Source.mapDomainErrorsTo2(
        errorE1 => ErrorA1(errorE1.id),
        errorE2 => ErrorA2(errorE2.id))
      bijection.composableDomainError
    }

    assert(runtime.unsafeRun(testTemplate(E.outcomeE1E2("42"))) == 42)
    assert(runtime.unsafeRun(testTemplate(E.outcomeE1E2("ErrorE1")).flip).cause.fold(_ => false, e => e.isInstanceOf[ErrorA1]))
    assert(runtime.unsafeRun(testTemplate(E.outcomeE1E2("ErrorE2")).flip).cause.fold(_ => false, e => e.isInstanceOf[ErrorA2]))
    assert(runtime.unsafeRun(testTemplate(E.outcomeE1E2("fault")).flip).cause.fold(_ => true, _ => false))
  }

  "Either2Error" should "allow overjection" in {
    def testTemplateMono(a: IO[ExceptionE, Outcome[ErrorE1E2, Int]]) = {
      val either2Source: ZIO[Any, FaultOr[ExceptionE, ErrorE1E2], Int] = a.toFaultOr
      val mono: ZIO[Any, FaultOr[ExceptionE, ErrorA1], Int] = either2Source.mapDomainErrorsTo1(
        errorE1 => ErrorA1(errorE1.id),
        errorE2 => ErrorA1(errorE2.id))
      mono.composableDomainError
    }

    assert(runtime.unsafeRun(testTemplateMono(E.outcomeE1E2("42"))) == 42)
    assert(runtime.unsafeRun(testTemplateMono(E.outcomeE1E2("ErrorE1")).flip).cause.fold(_ => false, e => e.isInstanceOf[ErrorA1]))
    assert(runtime.unsafeRun(testTemplateMono(E.outcomeE1E2("ErrorE2")).flip).cause.fold(_ => false, e => e.isInstanceOf[ErrorA1]))
    assert(runtime.unsafeRun(testTemplateMono(E.outcomeE1E2("fault")).flip).cause.fold(_ => true, _ => false))
  }

  "Either3Error" should "allow bijection" in {
    def testTemplate(a: IO[ExceptionE, Outcome[ErrorE1E2E3, Int]]) = {
      val either3Source: ZIO[Any, FaultOr[ExceptionE, ErrorE1E2E3], Int] = a.toFaultOr
      val bijection: ZIO[Any, FaultOr[ExceptionE, Either3Error[ErrorA1, ErrorA2, ErrorA3]], Int] = either3Source.mapDomainErrorsTo3(
        errorE1 => ErrorA1(errorE1.id),
        errorE2 => ErrorA2(errorE2.id),
        errorE3 => ErrorA3(errorE3.id))
      bijection.composableDomainError
    }

    assert(runtime.unsafeRun(testTemplate(E.outcomeE1E2E3("42"))) == 42)
    assert(runtime.unsafeRun(testTemplate(E.outcomeE1E2E3("ErrorE1")).flip).cause.fold(_ => false, e => e.isInstanceOf[ErrorA1]))
    assert(runtime.unsafeRun(testTemplate(E.outcomeE1E2E3("ErrorE2")).flip).cause.fold(_ => false, e => e.isInstanceOf[ErrorA2]))
    assert(runtime.unsafeRun(testTemplate(E.outcomeE1E2E3("ErrorE3")).flip).cause.fold(_ => false, e => e.isInstanceOf[ErrorA3]))
    assert(runtime.unsafeRun(testTemplate(E.outcomeE1E2E3("fault")).flip).cause.fold(_ => true, _ => false))
  }

  "Either3Error" should "allow overjection" in {
    def testTemplateMono(a: IO[ExceptionE, Outcome[ErrorE1E2E3, Int]]) = {
      val either3Source: ZIO[Any, FaultOr[ExceptionE, ErrorE1E2E3], Int] = a.toFaultOr
      val mono: ZIO[Any, FaultOr[ExceptionE, ErrorA1], Int] = either3Source.mapDomainErrorsTo1(
        errorE1 => ErrorA1(errorE1.id),
        errorE2 => ErrorA1(errorE2.id),
        errorE3 => ErrorA1(errorE3.id))
      mono.composableDomainError
    }

    assert(runtime.unsafeRun(testTemplateMono(E.outcomeE1E2E3("42"))) == 42)
    assert(runtime.unsafeRun(testTemplateMono(E.outcomeE1E2E3("ErrorE1")).flip).cause.fold(_ => false, e => e.isInstanceOf[ErrorA1]))
    assert(runtime.unsafeRun(testTemplateMono(E.outcomeE1E2E3("ErrorE2")).flip).cause.fold(_ => false, e => e.isInstanceOf[ErrorA1]))
    assert(runtime.unsafeRun(testTemplateMono(E.outcomeE1E2E3("ErrorE3")).flip).cause.fold(_ => false, e => e.isInstanceOf[ErrorA1]))
    assert(runtime.unsafeRun(testTemplateMono(E.outcomeE1E2E3("fault")).flip).cause.fold(_ => true, _ => false))

    def testTemplateEither2(a: IO[ExceptionE, Outcome[ErrorE1E2E3, Int]]) = {
      val either3Source: ZIO[Any, FaultOr[ExceptionE, ErrorE1E2E3], Int] = a.toFaultOr
      val overjection: ZIO[Any, FaultOr[ExceptionE, Either2Error[ErrorA1, ErrorA2]], Int] = either3Source.mapDomainErrorsTo2(
        errorE1 => Either2Error.first(ErrorA1(errorE1.id)),
        errorE2 => Either2Error.first(ErrorA1(errorE2.id)),
        errorE3 => Either2Error.second(ErrorA2(errorE3.id)))
      overjection.composableDomainError
    }

    assert(runtime.unsafeRun(testTemplateEither2(E.outcomeE1E2E3("42"))) == 42)
    assert(runtime.unsafeRun(testTemplateEither2(E.outcomeE1E2E3("ErrorE1")).flip).cause.fold(_ => false, e => e.isInstanceOf[ErrorA1]))
    assert(runtime.unsafeRun(testTemplateEither2(E.outcomeE1E2E3("ErrorE2")).flip).cause.fold(_ => false, e => e.isInstanceOf[ErrorA1]))
    assert(runtime.unsafeRun(testTemplateEither2(E.outcomeE1E2E3("ErrorE3")).flip).cause.fold(_ => false, e => e.isInstanceOf[ErrorA2]))
    assert(runtime.unsafeRun(testTemplateEither2(E.outcomeE1E2E3("fault")).flip).cause.fold(_ => true, _ => false))
  }

  "Either4Error" should "allow bijection" in {
    def testTemplate(a: IO[ExceptionE, Outcome[ErrorE1E2E3E4, Int]]) = {
      val either4Source: ZIO[Any, FaultOr[ExceptionE, ErrorE1E2E3E4], Int] = a.toFaultOr
      val bijection: ZIO[Any, FaultOr[ExceptionE, Either4Error[ErrorA1, ErrorA2, ErrorA3, ErrorA4]], Int] = either4Source.mapDomainErrorsTo4(
        errorE1 => ErrorA1(errorE1.id),
        errorE2 => ErrorA2(errorE2.id),
        errorE3 => ErrorA3(errorE3.id),
        errorE4 => ErrorA4(errorE4.id))
      bijection.composableDomainError
    }

    assert(runtime.unsafeRun(testTemplate(E.outcomeE1E2E3E4("42"))) == 42)
    assert(runtime.unsafeRun(testTemplate(E.outcomeE1E2E3E4("ErrorE1")).flip).cause.fold(_ => false, e => e.isInstanceOf[ErrorA1]))
    assert(runtime.unsafeRun(testTemplate(E.outcomeE1E2E3E4("ErrorE2")).flip).cause.fold(_ => false, e => e.isInstanceOf[ErrorA2]))
    assert(runtime.unsafeRun(testTemplate(E.outcomeE1E2E3E4("ErrorE3")).flip).cause.fold(_ => false, e => e.isInstanceOf[ErrorA3]))
    assert(runtime.unsafeRun(testTemplate(E.outcomeE1E2E3E4("ErrorE4")).flip).cause.fold(_ => false, e => e.isInstanceOf[ErrorA4]))
    assert(runtime.unsafeRun(testTemplate(E.outcomeE1E2E3E4("fault")).flip).cause.fold(_ => true, _ => false))
  }

  "Either4Error" should "allow overjection" in {
    def testTemplateMono(a: IO[ExceptionE, Outcome[ErrorE1E2E3E4, Int]]) = {
      val either4Source: ZIO[Any, FaultOr[ExceptionE, ErrorE1E2E3E4], Int] = a.toFaultOr
      val mono: ZIO[Any, FaultOr[ExceptionE, ErrorA1], Int] = either4Source.mapDomainErrorsTo1(
        errorE1 => ErrorA1(errorE1.id),
        errorE2 => ErrorA1(errorE2.id),
        errorE3 => ErrorA1(errorE3.id),
        errorE4 => ErrorA1(errorE4.id))
      mono.composableDomainError
    }

    assert(runtime.unsafeRun(testTemplateMono(E.outcomeE1E2E3E4("42"))) == 42)
    assert(runtime.unsafeRun(testTemplateMono(E.outcomeE1E2E3E4("ErrorE1")).flip).cause.fold(_ => false, e => e.isInstanceOf[ErrorA1]))
    assert(runtime.unsafeRun(testTemplateMono(E.outcomeE1E2E3E4("ErrorE2")).flip).cause.fold(_ => false, e => e.isInstanceOf[ErrorA1]))
    assert(runtime.unsafeRun(testTemplateMono(E.outcomeE1E2E3E4("ErrorE3")).flip).cause.fold(_ => false, e => e.isInstanceOf[ErrorA1]))
    assert(runtime.unsafeRun(testTemplateMono(E.outcomeE1E2E3E4("ErrorE4")).flip).cause.fold(_ => false, e => e.isInstanceOf[ErrorA1]))
    assert(runtime.unsafeRun(testTemplateMono(E.outcomeE1E2E3E4("fault")).flip).cause.fold(_ => true, _ => false))

    def testTemplateEither2(a: IO[ExceptionE, Outcome[ErrorE1E2E3E4, Int]]) = {
      val either4Source: ZIO[Any, FaultOr[ExceptionE, ErrorE1E2E3E4], Int] = a.toFaultOr
      val overjection: ZIO[Any, FaultOr[ExceptionE, Either2Error[ErrorA1, ErrorA2]], Int] = either4Source.mapDomainErrorsTo2(
        errorE1 => Either2Error.first(ErrorA1(errorE1.id)),
        errorE2 => Either2Error.first(ErrorA1(errorE2.id)),
        errorE3 => Either2Error.second(ErrorA2(errorE3.id)),
        errorE4 => Either2Error.second(ErrorA2(errorE4.id)))
      overjection.composableDomainError
    }

    assert(runtime.unsafeRun(testTemplateEither2(E.outcomeE1E2E3E4("42"))) == 42)
    assert(runtime.unsafeRun(testTemplateEither2(E.outcomeE1E2E3E4("ErrorE1")).flip).cause.fold(_ => false, e => e.isInstanceOf[ErrorA1]))
    assert(runtime.unsafeRun(testTemplateEither2(E.outcomeE1E2E3E4("ErrorE2")).flip).cause.fold(_ => false, e => e.isInstanceOf[ErrorA1]))
    assert(runtime.unsafeRun(testTemplateEither2(E.outcomeE1E2E3E4("ErrorE3")).flip).cause.fold(_ => false, e => e.isInstanceOf[ErrorA2]))
    assert(runtime.unsafeRun(testTemplateEither2(E.outcomeE1E2E3E4("ErrorE4")).flip).cause.fold(_ => false, e => e.isInstanceOf[ErrorA2]))
    assert(runtime.unsafeRun(testTemplateEither2(E.outcomeE1E2E3E4("fault")).flip).cause.fold(_ => true, _ => false))

    def testTemplateEither3(a: IO[ExceptionE, Outcome[ErrorE1E2E3E4, Int]]) = {
      val either4Source: ZIO[Any, FaultOr[ExceptionE, ErrorE1E2E3E4], Int] = a.toFaultOr
      val overjection: ZIO[Any, FaultOr[ExceptionE, Either3Error[ErrorA1, ErrorA2, ErrorA3]], Int] = either4Source.mapDomainErrorsTo3(
        errorE1 => Either3Error.first(ErrorA1(errorE1.id)),
        errorE2 => Either3Error.first(ErrorA1(errorE2.id)),
        errorE3 => Either3Error.second(ErrorA2(errorE3.id)),
        errorE4 => Either3Error.third(ErrorA3(errorE4.id)))
      overjection.composableDomainError
    }

    assert(runtime.unsafeRun(testTemplateEither3(E.outcomeE1E2E3E4("42"))) == 42)
    assert(runtime.unsafeRun(testTemplateEither3(E.outcomeE1E2E3E4("ErrorE1")).flip).cause.fold(_ => false, e => e.isInstanceOf[ErrorA1]))
    assert(runtime.unsafeRun(testTemplateEither3(E.outcomeE1E2E3E4("ErrorE2")).flip).cause.fold(_ => false, e => e.isInstanceOf[ErrorA1]))
    assert(runtime.unsafeRun(testTemplateEither3(E.outcomeE1E2E3E4("ErrorE3")).flip).cause.fold(_ => false, e => e.isInstanceOf[ErrorA2]))
    assert(runtime.unsafeRun(testTemplateEither3(E.outcomeE1E2E3E4("ErrorE4")).flip).cause.fold(_ => false, e => e.isInstanceOf[ErrorA3]))
    assert(runtime.unsafeRun(testTemplateEither3(E.outcomeE1E2E3E4("fault")).flip).cause.fold(_ => true, _ => false))
  }

  "Either5Error" should "allow bijection" in {
    def testTemplate(a: IO[ExceptionE, Outcome[ErrorE1E2E3E4E5, Int]]) = {
      val either5Source: ZIO[Any, FaultOr[ExceptionE, ErrorE1E2E3E4E5], Int] = a.toFaultOr
      val bijection: ZIO[Any, FaultOr[ExceptionE, Either5Error[ErrorA1, ErrorA2, ErrorA3, ErrorA4, ErrorA5]], Int] = either5Source.mapDomainErrorsTo5(
        errorE1 => ErrorA1(errorE1.id),
        errorE2 => ErrorA2(errorE2.id),
        errorE3 => ErrorA3(errorE3.id),
        errorE4 => ErrorA4(errorE4.id),
        errorE5 => ErrorA5(errorE5.id))
      bijection.composableDomainError
    }

    assert(runtime.unsafeRun(testTemplate(E.outcomeE1E2E3E4E5("42"))) == 42)
    assert(runtime.unsafeRun(testTemplate(E.outcomeE1E2E3E4E5("ErrorE1")).flip).cause.fold(_ => false, e => e.isInstanceOf[ErrorA1]))
    assert(runtime.unsafeRun(testTemplate(E.outcomeE1E2E3E4E5("ErrorE2")).flip).cause.fold(_ => false, e => e.isInstanceOf[ErrorA2]))
    assert(runtime.unsafeRun(testTemplate(E.outcomeE1E2E3E4E5("ErrorE3")).flip).cause.fold(_ => false, e => e.isInstanceOf[ErrorA3]))
    assert(runtime.unsafeRun(testTemplate(E.outcomeE1E2E3E4E5("ErrorE4")).flip).cause.fold(_ => false, e => e.isInstanceOf[ErrorA4]))
    assert(runtime.unsafeRun(testTemplate(E.outcomeE1E2E3E4E5("ErrorE5")).flip).cause.fold(_ => false, e => e.isInstanceOf[ErrorA5]))
    assert(runtime.unsafeRun(testTemplate(E.outcomeE1E2E3E4E5("fault")).flip).cause.fold(_ => true, _ => false))
  }

  "Either5Error" should "allow overjection" in {
    def testTemplateMono(a: IO[ExceptionE, Outcome[ErrorE1E2E3E4E5, Int]]) = {
      val either4Source: ZIO[Any, FaultOr[ExceptionE, ErrorE1E2E3E4E5], Int] = a.toFaultOr
      val mono: ZIO[Any, FaultOr[ExceptionE, ErrorA1], Int] = either4Source.mapDomainErrorsTo1(
        errorE1 => ErrorA1(errorE1.id),
        errorE2 => ErrorA1(errorE2.id),
        errorE3 => ErrorA1(errorE3.id),
        errorE4 => ErrorA1(errorE4.id),
        errorE5 => ErrorA1(errorE5.id))
      mono.composableDomainError
    }

    assert(runtime.unsafeRun(testTemplateMono(E.outcomeE1E2E3E4E5("42"))) == 42)
    assert(runtime.unsafeRun(testTemplateMono(E.outcomeE1E2E3E4E5("ErrorE1")).flip).cause.fold(_ => false, e => e.isInstanceOf[ErrorA1]))
    assert(runtime.unsafeRun(testTemplateMono(E.outcomeE1E2E3E4E5("ErrorE2")).flip).cause.fold(_ => false, e => e.isInstanceOf[ErrorA1]))
    assert(runtime.unsafeRun(testTemplateMono(E.outcomeE1E2E3E4E5("ErrorE3")).flip).cause.fold(_ => false, e => e.isInstanceOf[ErrorA1]))
    assert(runtime.unsafeRun(testTemplateMono(E.outcomeE1E2E3E4E5("ErrorE4")).flip).cause.fold(_ => false, e => e.isInstanceOf[ErrorA1]))
    assert(runtime.unsafeRun(testTemplateMono(E.outcomeE1E2E3E4E5("ErrorE5")).flip).cause.fold(_ => false, e => e.isInstanceOf[ErrorA1]))
    assert(runtime.unsafeRun(testTemplateMono(E.outcomeE1E2E3E4E5("fault")).flip).cause.fold(_ => true, _ => false))

    def testTemplateEither2(a: IO[ExceptionE, Outcome[ErrorE1E2E3E4E5, Int]]) = {
      val either5Source: ZIO[Any, FaultOr[ExceptionE, ErrorE1E2E3E4E5], Int] = a.toFaultOr
      val overjection: ZIO[Any, FaultOr[ExceptionE, Either2Error[ErrorA1, ErrorA2]], Int] = either5Source.mapDomainErrorsTo2(
        errorE1 => Either2Error.first(ErrorA1(errorE1.id)),
        errorE2 => Either2Error.first(ErrorA1(errorE2.id)),
        errorE3 => Either2Error.second(ErrorA2(errorE3.id)),
        errorE4 => Either2Error.second(ErrorA2(errorE4.id)),
        errorE5 => Either2Error.second(ErrorA2(errorE5.id)))
      overjection.composableDomainError
    }

    assert(runtime.unsafeRun(testTemplateEither2(E.outcomeE1E2E3E4E5("42"))) == 42)
    assert(runtime.unsafeRun(testTemplateEither2(E.outcomeE1E2E3E4E5("ErrorE1")).flip).cause.fold(_ => false, e => e.isInstanceOf[ErrorA1]))
    assert(runtime.unsafeRun(testTemplateEither2(E.outcomeE1E2E3E4E5("ErrorE2")).flip).cause.fold(_ => false, e => e.isInstanceOf[ErrorA1]))
    assert(runtime.unsafeRun(testTemplateEither2(E.outcomeE1E2E3E4E5("ErrorE3")).flip).cause.fold(_ => false, e => e.isInstanceOf[ErrorA2]))
    assert(runtime.unsafeRun(testTemplateEither2(E.outcomeE1E2E3E4E5("ErrorE4")).flip).cause.fold(_ => false, e => e.isInstanceOf[ErrorA2]))
    assert(runtime.unsafeRun(testTemplateEither2(E.outcomeE1E2E3E4E5("ErrorE5")).flip).cause.fold(_ => false, e => e.isInstanceOf[ErrorA2]))
    assert(runtime.unsafeRun(testTemplateEither2(E.outcomeE1E2E3E4E5("fault")).flip).cause.fold(_ => true, _ => false))

    def testTemplateEither3(a: IO[ExceptionE, Outcome[ErrorE1E2E3E4E5, Int]]) = {
      val either5Source: ZIO[Any, FaultOr[ExceptionE, ErrorE1E2E3E4E5], Int] = a.toFaultOr
      val overjection: ZIO[Any, FaultOr[ExceptionE, Either3Error[ErrorA1, ErrorA2, ErrorA3]], Int] = either5Source.mapDomainErrorsTo3(
        errorE1 => Either3Error.first(ErrorA1(errorE1.id)),
        errorE2 => Either3Error.first(ErrorA1(errorE2.id)),
        errorE3 => Either3Error.second(ErrorA2(errorE3.id)),
        errorE4 => Either3Error.third(ErrorA3(errorE4.id)),
        errorE5 => Either3Error.third(ErrorA3(errorE5.id)))
      overjection.composableDomainError
    }

    assert(runtime.unsafeRun(testTemplateEither3(E.outcomeE1E2E3E4E5("42"))) == 42)
    assert(runtime.unsafeRun(testTemplateEither3(E.outcomeE1E2E3E4E5("ErrorE1")).flip).cause.fold(_ => false, e => e.isInstanceOf[ErrorA1]))
    assert(runtime.unsafeRun(testTemplateEither3(E.outcomeE1E2E3E4E5("ErrorE2")).flip).cause.fold(_ => false, e => e.isInstanceOf[ErrorA1]))
    assert(runtime.unsafeRun(testTemplateEither3(E.outcomeE1E2E3E4E5("ErrorE3")).flip).cause.fold(_ => false, e => e.isInstanceOf[ErrorA2]))
    assert(runtime.unsafeRun(testTemplateEither3(E.outcomeE1E2E3E4E5("ErrorE4")).flip).cause.fold(_ => false, e => e.isInstanceOf[ErrorA3]))
    assert(runtime.unsafeRun(testTemplateEither3(E.outcomeE1E2E3E4E5("ErrorE5")).flip).cause.fold(_ => false, e => e.isInstanceOf[ErrorA3]))
    assert(runtime.unsafeRun(testTemplateEither3(E.outcomeE1E2E3E4E5("fault")).flip).cause.fold(_ => true, _ => false))

    def testTemplateEither4(a: IO[ExceptionE, Outcome[ErrorE1E2E3E4E5, Int]]) = {
      val either5Source: ZIO[Any, FaultOr[ExceptionE, ErrorE1E2E3E4E5], Int] = a.toFaultOr
      val overjection: ZIO[Any, FaultOr[ExceptionE, Either4Error[ErrorA1, ErrorA2, ErrorA3, ErrorA4]], Int] = either5Source.mapDomainErrorsTo4(
        errorE1 => Either4Error.first(ErrorA1(errorE1.id)),
        errorE2 => Either4Error.first(ErrorA1(errorE2.id)),
        errorE3 => Either4Error.second(ErrorA2(errorE3.id)),
        errorE4 => Either4Error.third(ErrorA3(errorE4.id)),
        errorE5 => Either4Error.fourth(ErrorA4(errorE5.id)))
      overjection.composableDomainError
    }

    assert(runtime.unsafeRun(testTemplateEither4(E.outcomeE1E2E3E4E5("42"))) == 42)
    assert(runtime.unsafeRun(testTemplateEither4(E.outcomeE1E2E3E4E5("ErrorE1")).flip).cause.fold(_ => false, e => e.isInstanceOf[ErrorA1]))
    assert(runtime.unsafeRun(testTemplateEither4(E.outcomeE1E2E3E4E5("ErrorE2")).flip).cause.fold(_ => false, e => e.isInstanceOf[ErrorA1]))
    assert(runtime.unsafeRun(testTemplateEither4(E.outcomeE1E2E3E4E5("ErrorE3")).flip).cause.fold(_ => false, e => e.isInstanceOf[ErrorA2]))
    assert(runtime.unsafeRun(testTemplateEither4(E.outcomeE1E2E3E4E5("ErrorE4")).flip).cause.fold(_ => false, e => e.isInstanceOf[ErrorA3]))
    assert(runtime.unsafeRun(testTemplateEither4(E.outcomeE1E2E3E4E5("ErrorE5")).flip).cause.fold(_ => false, e => e.isInstanceOf[ErrorA4]))
    assert(runtime.unsafeRun(testTemplateEither4(E.outcomeE1E2E3E4E5("fault")).flip).cause.fold(_ => true, _ => false))
  }

  // ADAPTING SHOULD NOT INCREASE THE INITIAL ERROR SET

  "catching all type-bound domain errors" should "work " in {
    // ErrorC1 is caught and trigger D.outcomeD2("42").outcomeToFaultOr
    // ErrorC2 is caught and trigger D.outcomeD2("42").outcomeToFaultOr
    // ErrorC3 is caught and trigger D.outcomeD1("42").outcomeToFaultOr
    def testTemplate1(a1: IO[ExceptionC, Outcome[ErrorC3, Int]],
                      a2: IO[ExceptionC, Outcome[ErrorC1C2, Int]],
                      a3: IO[ExceptionA, Outcome[ErrorA3, Int]]) = {
      val action1: ZIO[Any, FaultOr[ExceptionC, _ >: ErrorC3], Int] = a1.toFaultOr.composableDomainError
      val action2: ZIO[Any, FaultOr[ExceptionC, Either2Error[ErrorC1, ErrorC2]], Int] = a2.toFaultOr

      val p: ZIO[Any, FaultOr[ExceptionC, _ >: ErrorC3 with ErrorC1 with ErrorC2], Int] = for {
        _ <- action1
        _ <- action2.composableDomainError
      } yield 42

      def onErrorC1C2C3_42(error: Expected[_ >: ErrorC1 with ErrorC2 with ErrorC3]): ZIO[Any, FaultOr[ExceptionD, _ >: ErrorD2 with ErrorD1], Int] = {
        error.cause match {
          case ErrorC1(_, _) => D.outcomeD2("42").toFaultOr
          case ErrorC2(_, _) => D.outcomeD2("42").toFaultOr
          case ErrorC3(_, _) => D.outcomeD1("42").toFaultOr
        }
      }

      val p2: ZIO[Any, FaultOr[Throwable, _ >: ErrorD2 with ErrorD1], Int] = p.catchDomainErrors(onErrorC1C2C3_42)
      // ideally I should infer the least common type between ExceptionC and ExceptionD which is Exception but didn't yet find a way so the most common type (Throwable) is used for now
      // if throwable is too vague for you, you can explicitly map the fault
      val p2a: ZIO[Any, FaultOr[Exception, _ >: ErrorD2 with ErrorD1],Int] = p2.mapFault(throwable => throwable.asInstanceOf[Exception])

      // error composition is still possible but notice that I failed to keep a precise fault type : Throwable is used
      val action3: ZIO[Any, FaultOr[ExceptionA, _ >: ErrorA3], Int] = a3.toFaultOr
      val p3: ZIO[Any, FaultOr[Exception, _ >: ErrorD2 with ErrorD1 with ErrorA3],Int] = for {
        _ <- p2a
        _ <- action3
      } yield 42

      p3
    }

    def testTemplate2(a1: IO[ExceptionC, Outcome[ErrorC3, Int]],
                      a2: IO[ExceptionC, Outcome[ErrorC1C2, Int]],
                      a3: IO[ExceptionA, Outcome[ErrorA3, Int]]) = {
      val action1: ZIO[Any, FaultOr[ExceptionC, _ >: ErrorC3],Int] = a1.toFaultOr.composableDomainError
      val action2: ZIO[Any, FaultOr[ExceptionC, Either2Error[ErrorC1, ErrorC2]], Int] = a2.toFaultOr

      val p = for {
        _ <- action1
        _ <- action2.composableDomainError
      } yield 42

      val p2 = p.catchDomainErrors((error: Expected[_ >: ErrorC1 with ErrorC2 with ErrorC3]) => {
        error.cause match {
          case ErrorC1(_, _) => D.outcomeD2("42").toFaultOr
          case ErrorC2(_, _) => D.outcomeD2("42").toFaultOr
          case ErrorC3(_, _) => D.outcomeD1("42").toFaultOr
        }
      })

      // error composition is still possible
      val action3: ZIO[Any, FaultOr[ExceptionA, _ >: ErrorA3],Int] = a3.toFaultOr
      val p3 = for {
        _ <- p2
        _ <- action3
      } yield 42

      p3
    }

    {
      val testCase1 = testTemplate1(C.outcomeC3("42"), C.outcomeC1C2("42"), A.outcomeA3("42"))
      val res1 = runtime.unsafeRun(testCase1)
      //println(res)
      assert(res1 == 42)

      val testCase2 = testTemplate2(C.outcomeC3("42"), C.outcomeC1C2("42"), A.outcomeA3("42"))
      val res2 = runtime.unsafeRun(testCase2)
      //println(res)
      assert(res2 == 42)
    }

    {
      val testCase1 = testTemplate1(C.outcomeC3("ErrorC3"), C.outcomeC1C2("42"), A.outcomeA3("42"))
      val res1 = runtime.unsafeRun(testCase1)
      println(res1)
      assert(res1 == 42)

      val testCase2 = testTemplate2(C.outcomeC3("ErrorC3"), C.outcomeC1C2("42"), A.outcomeA3("42"))
      val res2 = runtime.unsafeRun(testCase2)
      println(res2)
      assert(res2 == 42)
    }

    {
      val testCase1 = testTemplate1(C.outcomeC3("fault"), C.outcomeC1C2("42"), A.outcomeA3("42"))
      val res1 = runtime.unsafeRun(testCase1.flip)
      println(res1)
      assert(res1.cause.fold(_ => true, _ => false))

      val testCase2 = testTemplate2(C.outcomeC3("fault"), C.outcomeC1C2("42"), A.outcomeA3("42"))
      val res2 = runtime.unsafeRun(testCase2.flip)
      println(res2)
      assert(res2.cause.fold(_ => true, _ => false))
    }

    {
      val testCase1 = testTemplate1(C.outcomeC3("42"), C.outcomeC1C2("fault"), A.outcomeA3("42"))
      val res1 = runtime.unsafeRun(testCase1.flip)
      println(res1)
      assert(res1.cause.fold(_ => true, _ => false))

      val testCase2 = testTemplate2(C.outcomeC3("42"), C.outcomeC1C2("fault"), A.outcomeA3("42"))
      val res2 = runtime.unsafeRun(testCase2.flip)
      println(res2)
      assert(res2.cause.fold(_ => true, _ => false))
    }

    {
      val testCase1 = testTemplate1(C.outcomeC3("42"), C.outcomeC1C2("42"), A.outcomeA3("fault"))
      val res1 = runtime.unsafeRun(testCase1.flip)
      println(res1)
      assert(res1.cause.fold(_ => true, _ => false))

      val testCase2 = testTemplate2(C.outcomeC3("42"), C.outcomeC1C2("42"), A.outcomeA3("fault"))
      val res2 = runtime.unsafeRun(testCase2.flip)
      println(res2)
      assert(res2.cause.fold(_ => true, _ => false))
    }
  }

  "reacting on any type-bound domain error" should "work" in {
    def testTemplate(a1: IO[ExceptionA, Outcome[ErrorA1, Int]],
                     a2: IO[ExceptionA, Outcome[ErrorA2, Int]],
                     a3: IO[ExceptionA, Outcome[ErrorA3, Int]]) = {
      val p: ZIO[Any, FaultOr[ExceptionA, _ >: ErrorA1 with ErrorA2],Int] = for {
        _ <- a1.toFaultOr.composableDomainError
        _ <- a2.toFaultOr.composableDomainError
      } yield 42

      val p1: ZIO[Any, FaultOr[Throwable, _ >: ErrorD1],Int] = p.onDomainErrors(D.outcomeD1("84").toFaultOr)

      // Error composition is still possible but LUB on fault is lost and one gets Throwable
      // One can still use mapFault to refine the fault type
      val p2: ZIO[Any, FaultOr[Throwable, _ >: ErrorD1 with ErrorA3],Int] = for {
        r1 <- p1
        _ <- a3.toFaultOr.composableDomainError
      } yield r1

      val p2a = p2.mapFault(fault => fault.asInstanceOf[Exception])
      p2a
    }

    {
      val testCase = testTemplate(A.outcomeA1("42"), A.outcomeA2("42"), A.outcomeA3("42"))
      val res = runtime.unsafeRun(testCase)
      //println(res)
      assert(res == 42)
    }

    {
      val testCase = testTemplate(A.outcomeA1("ErrorA1"), A.outcomeA2("42"), A.outcomeA3("42"))
      val res = runtime.unsafeRun(testCase)
      //println(res)
      assert(res == 84)
    }

    {
      val testCase = testTemplate(A.outcomeA1("fault"), A.outcomeA2("42"), A.outcomeA3("42"))
      val res = runtime.unsafeRun(testCase.flip)
      //println(res)
      assert(res.cause.fold(_ => true, _ => false))
    }
  }

  "fault" should "stay unchanged through domain error adaptation" in {
    val a1: IO[ExceptionA, Outcome[ErrorA1, Int]] = A.outcomeA1("42")
    val a2: IO[ExceptionA, Outcome[ErrorA2, Int]] = A.outcomeA2("42")
    val a3: Task[Int] = A.resultOrFault("fault")

    val action1: ZIO[Any, FaultOr[ExceptionA, _ >: ErrorA1],Int] = a1.toFaultOr.composableDomainError
    val action2: ZIO[Any, FaultOr[ExceptionA, _ >: ErrorA2],Int] = a2.toFaultOr.composableDomainError
    val action3: ZIO[Any, FaultOr[Throwable,Any],Int] = a3.toFaultOr

    val p: ZIO[Any, FaultOr[Throwable, _ >: ErrorA1 with ErrorA2],Int] = for {
      _ <- action1
      _ <- action2
      _ <- action3
    } yield 42

    // adapt error from service A error domain to service B error domain using 'mapError1' in this example
    val adapted: ZIO[Any, FaultOr[Throwable, _ >: ErrorB1],Int] = p.mapDomainErrorsTo1 { from: Expected[_ >: ErrorA1 with ErrorA2] =>
      from.cause match {
        case a1@ErrorA1(id, _) => ErrorB1(id, a1)
        case a2@ErrorA2(id, _) => ErrorB1(id, a2)
      }
    }

    // whatever transformation of domain error, the root fault will be propagated
    val res: FaultOr[Throwable, _ >: ErrorB1] = runtime.unsafeRun(adapted.flip)
    //println(res)
    assert(res.cause.fold(throwable => throwable.getMessage == "A", _ => false))
  }

  "FaultOr[type-bound-error]" should "fold" in {
    // intellij type inference is lost, it gives ZIO[Any, Expected[_], Int]
    // use Metals type inference which give the right type ZIO[Any,errors.Expected[_ >: org.scalajos.precise.testerrors.services.A.ErrorA1 with org.scalajos.precise.testerrors.services.A.ErrorA2],Int]
    def testTemplate(a1: IO[ExceptionA, Outcome[ErrorA1, Int]],
                     a2: IO[ExceptionA, Outcome[ErrorA2, Int]]) = {
      val action1: ZIO[Any, FaultOr[ExceptionA, _ >: ErrorA1],Int] = a1.toFaultOr.composableDomainError
      val action2: ZIO[Any, FaultOr[ExceptionA, _ >: ErrorA2],Int] = a2.toFaultOr.composableDomainError

      val p: ZIO[Any, FaultOr[ExceptionA, _ >: ErrorA1 with ErrorA2],Int] = for {
        _ <- action1
        _ <- action2
      } yield 42

      p.fold((faultOr: FaultOr[ExceptionA, _ >: ErrorA1 with ErrorA2]) =>
        faultOr.fold(
          _ => 0, // throwable
          (expected: Expected[_ >: ErrorA1 with ErrorA2]) => expected.cause match {
            case ErrorA1(_, _) => -1
            case ErrorA2(_, _) => -2
          }),
        result => result)
    }

    assert(runtime.unsafeRun(testTemplate(A.outcomeA1("42"), A.outcomeA2("42"))) == 42)
    assert(runtime.unsafeRun(testTemplate(A.outcomeA1("ErrorA1"), A.outcomeA2("42"))) == -1)
    assert(runtime.unsafeRun(testTemplate(A.outcomeA1("42"), A.outcomeA2("ErrorA2"))) == -2)
    assert(runtime.unsafeRun(testTemplate(A.outcomeA1("fault"), A.outcomeA2("42"))) == 0)
    assert(runtime.unsafeRun(testTemplate(A.outcomeA1("42"), A.outcomeA2("fault"))) == 0)
  }

  "FaultOr[Either2Error]" should "fold" in {
    def testTemplate(a1: IO[ExceptionE, Outcome[ErrorE1E2, Int]]) = {
      val action1: ZIO[Any, FaultOr[ExceptionE, ErrorE1E2],Int] = a1.toFaultOr

      action1.fold((faultOr: FaultOr[ExceptionE, ErrorE1E2]) =>
        faultOr.fold(
          _ => 0, // throwable
          (either2: ErrorE1E2) => either2.fold(_ => -1, _ => -2)),
        result => result)
    }

    assert(runtime.unsafeRun(testTemplate(E.outcomeE1E2("42"))) == 42)
    assert(runtime.unsafeRun(testTemplate(E.outcomeE1E2("ErrorE1"))) == -1)
    assert(runtime.unsafeRun(testTemplate(E.outcomeE1E2("ErrorE2"))) == -2)
    assert(runtime.unsafeRun(testTemplate(E.outcomeE1E2("fault"))) == 0)
  }

  "FaultOr[Either3Error]" should "fold" in {
    def testTemplate(a1: IO[ExceptionE, Outcome[ErrorE1E2E3, Int]]) = {
      val action1: ZIO[Any, FaultOr[ExceptionE, ErrorE1E2E3],Int] = a1.toFaultOr

      action1.fold((faultOr: FaultOr[ExceptionE, ErrorE1E2E3]) =>
        faultOr.fold(
          _ => 0, // throwable
          (either3: ErrorE1E2E3) => either3.fold(_ => -1, _ => -2, _ => -3)),
        result => result)
    }

    assert(runtime.unsafeRun(testTemplate(E.outcomeE1E2E3("42"))) == 42)
    assert(runtime.unsafeRun(testTemplate(E.outcomeE1E2E3("ErrorE1"))) == -1)
    assert(runtime.unsafeRun(testTemplate(E.outcomeE1E2E3("ErrorE2"))) == -2)
    assert(runtime.unsafeRun(testTemplate(E.outcomeE1E2E3("ErrorE3"))) == -3)
    assert(runtime.unsafeRun(testTemplate(E.outcomeE1E2E3("fault"))) == 0)
  }

  "FaultOr[Either4Error]" should "fold" in {
    def testTemplate(a1: IO[ExceptionE, Outcome[ErrorE1E2E3E4, Int]]) = {
      val action1: ZIO[Any, FaultOr[ExceptionE, ErrorE1E2E3E4],Int] = a1.toFaultOr

      action1.fold((faultOr: FaultOr[ExceptionE, ErrorE1E2E3E4]) =>
        faultOr.fold(
          _ => 0, // throwable
          (either4: ErrorE1E2E3E4) => either4.fold(_ => -1, _ => -2, _ => -3, _ => -4)),
        result => result)
    }

    assert(runtime.unsafeRun(testTemplate(E.outcomeE1E2E3E4("42"))) == 42)
    assert(runtime.unsafeRun(testTemplate(E.outcomeE1E2E3E4("ErrorE1"))) == -1)
    assert(runtime.unsafeRun(testTemplate(E.outcomeE1E2E3E4("ErrorE2"))) == -2)
    assert(runtime.unsafeRun(testTemplate(E.outcomeE1E2E3E4("ErrorE3"))) == -3)
    assert(runtime.unsafeRun(testTemplate(E.outcomeE1E2E3E4("ErrorE4"))) == -4)
    assert(runtime.unsafeRun(testTemplate(E.outcomeE1E2E3E4("fault"))) == 0)
  }

  "FaultOr[Either5Error]" should "fold" in {
    def testTemplate(a1: IO[ExceptionE, Outcome[ErrorE1E2E3E4E5, Int]]) = {
      val action1: ZIO[Any, FaultOr[ExceptionE, ErrorE1E2E3E4E5],Int] = a1.toFaultOr

      action1.fold((faultOr: FaultOr[ExceptionE, ErrorE1E2E3E4E5]) =>
        faultOr.fold(
          _ => 0, // throwable
          (either5: ErrorE1E2E3E4E5) => either5.fold(_ => -1, _ => -2, _ => -3, _ => -4, _ => -5)),
        result => result)
    }

    assert(runtime.unsafeRun(testTemplate(E.outcomeE1E2E3E4E5("42"))) == 42)
    assert(runtime.unsafeRun(testTemplate(E.outcomeE1E2E3E4E5("ErrorE1"))) == -1)
    assert(runtime.unsafeRun(testTemplate(E.outcomeE1E2E3E4E5("ErrorE2"))) == -2)
    assert(runtime.unsafeRun(testTemplate(E.outcomeE1E2E3E4E5("ErrorE3"))) == -3)
    assert(runtime.unsafeRun(testTemplate(E.outcomeE1E2E3E4E5("ErrorE4"))) == -4)
    assert(runtime.unsafeRun(testTemplate(E.outcomeE1E2E3E4E5("ErrorE5"))) == -5)
    assert(runtime.unsafeRun(testTemplate(E.outcomeE1E2E3E4E5("fault"))) == 0)
  }

  "multiple single errors from services obtained through ZLayer" should "compose" in {
    /*
    {
      // a service with a dependency should be error composable : OK

      val serviceWithDependency:        ZIO[zio.Has[ZioServicePattern.Service], ExceptionA, Outcome[ErrorA1, Int]]  = ???
      val serviceFaultOrWithDependency: ZIO[Has[ZioServicePattern.Service], FaultOr[ExceptionA, ErrorA1], Int]      = serviceWithDependency.toFaultOr
      val serviceComposableError:       ZIO[Has[ZioServicePattern.Service], FaultOr[ExceptionA, _ >: ErrorA1], Int] = serviceFaultOrWithDependency.composableDomainError
    }
    */

    def testTemplate(a1: ZIO[zio.Has[ZioServicePattern.Service], ExceptionA, Outcome[ErrorA1, Int]],
                     a2: ZIO[zio.Has[ZioServicePattern.Service], ExceptionA, Outcome[ErrorA2, Int]]): ZIO[Has[ZioServicePattern.Service], FaultOr[ExceptionA, _ >: ErrorA1 with ErrorA2], Int] = {
      // this action can produce a result, a domain error ErrorA1 or a system fault ExceptionA
      val action1: ZIO[zio.Has[ZioServicePattern.Service],FaultOr[ExceptionA,ErrorA1],Int] = a1.toFaultOr
      // this action can produce a result, a domain error ErrorA2 or a system fault ExceptionA
      val action2: ZIO[zio.Has[ZioServicePattern.Service],FaultOr[ExceptionA,ErrorA2],Int] = a2.toFaultOr

      // to compose domain errors, I need to use their type bound
      val action1ComposableError: ZIO[zio.Has[ZioServicePattern.Service],FaultOr[ExceptionA, _ >: ErrorA1],Int] = action1.composableDomainError
      val action2ComposableError: ZIO[zio.Has[ZioServicePattern.Service],FaultOr[ExceptionA, _ >: ErrorA2],Int] = action2.composableDomainError

      // note that errors do compose nicely in for comprehension
      // beware that intellij type engine is quickly lost, use Metals to get the right (but verbose) type
      // ZIO[Any,errors.FaultOr[services.A.ExceptionA, _ >: org.scalajos.precise.testerrors.services.A.ErrorA1 with org.scalajos.precise.testerrors.services.A.ErrorA2],Int]
      val p: ZIO[zio.Has[ZioServicePattern.Service],FaultOr[ExceptionA, _ >: ErrorA1 with ErrorA2],Int] = for {
        _ <- action1ComposableError
        _ <- action2ComposableError
      } yield 42

      p
    }

    {
      val program: ZIO[Any, FaultOr[ExceptionA, _ >: ErrorA1 with ErrorA2], Int] = testTemplate(A.outcomeA1("42"), A.outcomeA2("42")).provideLayer(ZioServicePattern.serviceAProvider)
      assert(runtime.unsafeRun(program) == 42)
    }

    {
      val program: ZIO[Any, FaultOr[ExceptionA, _ >: ErrorA1 with ErrorA2], Int] = testTemplate(A.outcomeA1("ErrorA1"), A.outcomeA2("42")).provideLayer(ZioServicePattern.serviceAProvider)
      val res: FaultOr[ExceptionA, _ >: ErrorA1 with ErrorA2] = runtime.unsafeRun(program.flip)
      assert(res.cause.fold(_ => false, e => e.isInstanceOf[ErrorA1]))
    }

    {
      val program: ZIO[Any, FaultOr[ExceptionA, _ >: ErrorA1 with ErrorA2], Int] = testTemplate(A.outcomeA1("42"), A.outcomeA2("ErrorA2")).provideLayer(ZioServicePattern.serviceAProvider)
      val res: FaultOr[ExceptionA, _ >: ErrorA1 with ErrorA2] = runtime.unsafeRun(program.flip)
      assert(res.cause.fold(_ => false, e => e.isInstanceOf[ErrorA2]))
    }

    {
      val program: ZIO[Any, FaultOr[ExceptionA, _ >: ErrorA1 with ErrorA2], Int] = testTemplate(A.outcomeA1("42"), A.outcomeA2("fault")).provideLayer(ZioServicePattern.serviceAProvider)
      val res: FaultOr[ExceptionA, _ >: ErrorA1 with ErrorA2] = runtime.unsafeRun(program.flip)
      assert(res.cause.fold(_ => true, _ => false))
    }
  }
}
