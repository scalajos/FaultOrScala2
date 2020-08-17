package org.scalajos.precise.testerrors

import org.scalajos.precise.errors.{ChainableDomainError, Outcome, SingleDomainError}
import org.scalajos.precise.errors.either.{Either2Error, Either3Error, Either4Error, Either5Error}
import zio.{Has, Task, ULayer, ZIO}

object services {
  object RootDomainError {
    sealed trait ErrorRoot extends SingleDomainError
    final case class ErrorRoot1(id: String) extends ErrorRoot
    final case class ErrorRoot2(id: String) extends ErrorRoot
    final case class ErrorRoot3(id: String) extends ErrorRoot
  }

  object A {
    sealed trait ErrorA extends SingleDomainError
    sealed abstract class ChainableErrorA(id: String, chainedWith: SingleDomainError) extends ChainableDomainError(s"ErrorA1 with $id", chainedWith) with ErrorA
    final case class ErrorA1(id: String, chainedWith: SingleDomainError = null) extends ChainableErrorA(id, chainedWith)
    final case class ErrorA2(id: String, chainedWith: SingleDomainError = null) extends ChainableErrorA(id, chainedWith)
    final case class ErrorA3(id: String, chainedWith: SingleDomainError = null) extends ChainableErrorA(id, chainedWith)
    final case class ErrorA4(id: String, chainedWith: SingleDomainError = null) extends ChainableErrorA(id, chainedWith)
    final case class ErrorA5(id: String, chainedWith: SingleDomainError = null) extends ChainableErrorA(id, chainedWith)

    final class ExceptionA(msg: String) extends Exception(msg)

    def outcomeA1(s: String): ZIO[Any, ExceptionA, Outcome[ErrorA1, Int]] = Task {
      import Outcome._
      s match {
        case "42" => giveResult[ErrorA1, Int](42)
        case "ErrorA1" => giveDomainError[ErrorA1, Int](ErrorA1("1"))
        case "ErrorA1id2" => giveDomainError[ErrorA1, Int](ErrorA1("2"))
        case _ => throw new ExceptionA("outcomeA1")
      }
    }.refineToOrDie[ExceptionA]

    def outcomeA2(s: String): ZIO[Any, ExceptionA, Outcome[ErrorA2, Int]] = Task {
      import Outcome._
      s match {
        case "42" => giveResult[ErrorA2, Int](42)
        case "ErrorA2" => giveDomainError[ErrorA2, Int](ErrorA2("2"))
        case _ => throw new ExceptionA("outcomeA2")
      }
    }.refineToOrDie[ExceptionA]

    def outcomeA3(s: String): ZIO[Any, ExceptionA, Outcome[ErrorA3, Int]] = Task {
      import Outcome._
      s match {
        case "42" => giveResult[ErrorA3, Int](42)
        case "ErrorA3" => giveDomainError[ErrorA3, Int](ErrorA3("3"))
        case _ => throw new ExceptionA("outcomeA3")
      }
    }.refineToOrDie[ExceptionA]

    def resultOrFault(s: String): Task[Int] = Task {
      s match {
        case "42" => 42
        case _ => throw new Exception("A")
      }
    }
  }

  object B {
    sealed trait ErrorB extends SingleDomainError
    sealed abstract class ChainableErrorB(id: String, chainedWith: SingleDomainError) extends ChainableDomainError(s"ErrorA1 with $id", chainedWith) with ErrorB
    final case class ErrorB1(id: String, chainedWith: SingleDomainError = null) extends ChainableErrorB(id, chainedWith)
    final case class ErrorB2(id: String, chainedWith: SingleDomainError = null) extends ChainableErrorB(id, chainedWith)
    final case class ErrorB3(id: String, chainedWith: SingleDomainError = null) extends ChainableErrorB(id, chainedWith)

    final class ExceptionB(msg: String) extends Exception(msg)

    def outcomeB1(s: String): ZIO[Any, ExceptionB, Outcome[ErrorB1, Int]] = Task {
      import Outcome._
      s match {
        case "42" => giveResult[ErrorB1, Int](42)
        case "ErrorB1" => giveDomainError[ErrorB1, Int](ErrorB1("1"))
        case _ => throw new ExceptionB("outcomeB1")
      }
    }.refineToOrDie[ExceptionB]

    def outcomeB2(s: String): ZIO[Any, ExceptionB, Outcome[ErrorB2, Int]] = Task {
      import Outcome._
      s match {
        case "42" => giveResult[ErrorB2, Int](42)
        case "ErrorB2" => giveDomainError[ErrorB2, Int](ErrorB2("2"))
        case _ => throw new ExceptionB("outcomeB2")
      }
    }.refineToOrDie[ExceptionB]

    def outcomeB3(s: String): ZIO[Any, ExceptionB, Outcome[ErrorB3, Int]] = Task {
      import Outcome._
      s match {
        case "42" => giveResult[ErrorB3, Int](42)
        case "ErrorB3" => giveDomainError[ErrorB3, Int](ErrorB3("3"))
        case _ => throw new ExceptionB("outcomeB3")
      }
    }.refineToOrDie[ExceptionB]

    def resultOrFault(s: String): Task[Int] = Task {
      s match {
        case "42" => 42
        case _ => throw new Exception("B")
      }
    }
  }

  object C {
    sealed trait ErrorC extends SingleDomainError
    sealed abstract class ChainableErrorC(id: String, chainedWith: SingleDomainError) extends ChainableDomainError(s"ErrorA1 with $id", chainedWith) with ErrorC
    final case class ErrorC1(id: String, chainedWith: SingleDomainError = null) extends ChainableErrorC(id, chainedWith)
    final case class ErrorC2(id: String, chainedWith: SingleDomainError = null) extends ChainableErrorC(id, chainedWith)
    final case class ErrorC3(id: String, chainedWith: SingleDomainError = null) extends ChainableErrorC(id, chainedWith)

    final class ExceptionC(msg: String) extends Exception(msg)

    def outcomeC1(s: String): ZIO[Any, ExceptionC, Outcome[ErrorC1, Int]] = Task {
      import Outcome._
      s match {
        case "42" => giveResult[ErrorC1, Int](42)
        case "ErrorC1" => giveDomainError[ErrorC1, Int](ErrorC1("1"))
        case _ => throw new ExceptionC("outcomeC1")
      }
    }.refineToOrDie[ExceptionC]

    def outcomeC2(s: String): ZIO[Any, ExceptionC, Outcome[ErrorC2, Int]] = Task {
      import Outcome._
      s match {
        case "42" => giveResult[ErrorC2, Int](42)
        case "ErrorC2" => giveDomainError[ErrorC2, Int](ErrorC2("2"))
        case _ => throw new ExceptionC("outcomeC2")
      }
    }.refineToOrDie[ExceptionC]

    def outcomeC3(s: String): ZIO[Any, ExceptionC, Outcome[ErrorC3, Int]] = Task {
      import Outcome._
      s match {
        case "42" => giveResult[ErrorC3, Int](42)
        case "ErrorC3" => giveDomainError[ErrorC3, Int](ErrorC3("3"))
        case _ => throw new ExceptionC("outcomeC3")
      }
    }.refineToOrDie[ExceptionC]

    type ErrorC1C2 = Either2Error[ErrorC1, ErrorC2]
    def outcomeC1C2(s: String): ZIO[Any, ExceptionC, Outcome[ErrorC1C2, Int]] = Task {
      import Outcome._
      s match {
        case "ErrorC1" => giveDomainError[ErrorC1C2, Int](Either2Error.first(ErrorC1("1")))
        case "ErrorC2" => giveDomainError[ErrorC1C2, Int](Either2Error.second(ErrorC2("2")))
        case "42" => giveResult[ErrorC1C2, Int](42)
        case _ => throw new ExceptionC("outcomeC1C2")
      }
    }.refineToOrDie[ExceptionC]

    type ErrorC2C3 = Either2Error[ErrorC2, ErrorC3]
    def outcomeC2C3(s: String): ZIO[Any, ExceptionC, Outcome[ErrorC2C3, Int]] = Task {
      import Outcome._
      s match {
        case "ErrorC2" => giveDomainError[ErrorC2C3, Int](Either2Error.first(ErrorC2("2")))
        case "ErrorC3" => giveDomainError[ErrorC2C3, Int](Either2Error.second(ErrorC3("3")))
        case "42" => giveResult[ErrorC2C3, Int](42)
        case _ => throw new ExceptionC("outcomeC2C3")
      }
    }.refineToOrDie[ExceptionC]

    def resultOrFault(s: String): Task[Int] = Task {
      s match {
        case "42" => 42
        case _ => throw new Exception("C")
      }
    }
  }

  object D {
    sealed trait ErrorD extends SingleDomainError
    sealed abstract class ChainableErrorD(id: String, chainedWith: SingleDomainError) extends ChainableDomainError(s"ErrorA1 with $id", chainedWith) with ErrorD
    final case class ErrorD1(id: String, chainedWith: SingleDomainError = null) extends ChainableErrorD(id, chainedWith)
    final case class ErrorD2(id: String, chainedWith: SingleDomainError = null) extends ChainableErrorD(id, chainedWith)
    final case class ErrorD3(id: String, chainedWith: SingleDomainError = null) extends ChainableErrorD(id, chainedWith)

    final class ExceptionD(msg: String) extends Exception(msg)

    def outcomeD1(s: String): ZIO[Any, ExceptionD, Outcome[ErrorD1, Int]] = Task {
      import Outcome._
      s match {
        case "42" => giveResult[ErrorD1, Int](42)
        case "84" => giveResult[ErrorD1, Int](84)
        case "ErrorD1" => giveDomainError[ErrorD1, Int](ErrorD1("1"))
        case _ => throw new ExceptionD("outcomeD1")
      }
    }.refineToOrDie[ExceptionD]

    def outcomeD2(s: String): ZIO[Any, ExceptionD, Outcome[ErrorD2, Int]] = Task {
      import Outcome._
      s match {
        case "42" => giveResult[ErrorD2, Int](42)
        case "ErrorD2" => giveDomainError[ErrorD2, Int](ErrorD2("2"))
        case _ => throw new ExceptionD("outcomeD2")
      }
    }.refineToOrDie[ExceptionD]

    def outcomeD3(s: String): ZIO[Any, ExceptionD, Outcome[ErrorD3, Int]] = Task {
      import Outcome._
      s match {
        case "42" => giveResult[ErrorD3, Int](42)
        case "ErrorD3" => giveDomainError[ErrorD3, Int](ErrorD3("3"))
        case _ => throw new ExceptionD("outcomeD3")
      }
    }.refineToOrDie[ExceptionD]

    type ErrorD1D2D3 = Either3Error[ErrorD1, ErrorD2, ErrorD3]
    def outcomeD1D2D3(s: String): ZIO[Any, ExceptionD, Outcome[ErrorD1D2D3, Int]] = Task {
      import Outcome._
      s match {
        case "ErrorD1" => giveDomainError[ErrorD1D2D3, Int](Either3Error.first(ErrorD1("1")))
        case "ErrorD2" => giveDomainError[ErrorD1D2D3, Int](Either3Error.second(ErrorD2("2")))
        case "ErrorD3" => giveDomainError[ErrorD1D2D3, Int](Either3Error.third(ErrorD3("3")))
        case "42" => giveResult[ErrorD1D2D3, Int](42)
        case _ => throw new ExceptionD("outcomeD1D2D3")
      }
    }.refineToOrDie[ExceptionD]

    def resultOrFault(s: String): Task[Int] = Task {
      s match {
        case "42" => 42
        case _ => throw new Exception("D")
      }
    }
  }

  object E {
    sealed trait ErrorE extends SingleDomainError
    sealed abstract class ChainableErrorE(id: String, chainedWith: SingleDomainError) extends ChainableDomainError(s"ErrorA1 with $id", chainedWith) with ErrorE
    final case class ErrorE1(id: String, chainedWith: SingleDomainError = null) extends ChainableErrorE(id, chainedWith)
    final case class ErrorE2(id: String, chainedWith: SingleDomainError = null) extends ChainableErrorE(id, chainedWith)
    final case class ErrorE3(id: String, chainedWith: SingleDomainError = null) extends ChainableErrorE(id, chainedWith)
    final case class ErrorE4(id: String, chainedWith: SingleDomainError = null) extends ChainableErrorE(id, chainedWith)
    final case class ErrorE5(id: String, chainedWith: SingleDomainError = null) extends ChainableErrorE(id, chainedWith)

    final class ExceptionE(msg: String) extends Exception(msg)

    type ErrorE1E2 = Either2Error[ErrorE1, ErrorE2]
    def outcomeE1E2(s: String): ZIO[Any, ExceptionE, Outcome[ErrorE1E2, Int]] = Task {
      import Outcome._
      s match {
        case "ErrorE1" => giveDomainError[ErrorE1E2, Int](Either2Error.first(ErrorE1("1")))
        case "ErrorE2" => giveDomainError[ErrorE1E2, Int](Either2Error.second(ErrorE2("2")))
        case "42" => giveResult[ErrorE1E2, Int](42)
        case _ => throw new ExceptionE("outcomeE1E2")
      }
    }.refineToOrDie[ExceptionE]

    type ErrorE1E2E3 = Either3Error[ErrorE1, ErrorE2, ErrorE3]
    def outcomeE1E2E3(s: String): ZIO[Any, ExceptionE, Outcome[ErrorE1E2E3, Int]] = Task {
      import Outcome._
      s match {
        case "ErrorE1" => giveDomainError[ErrorE1E2E3, Int](Either3Error.first(ErrorE1("1")))
        case "ErrorE2" => giveDomainError[ErrorE1E2E3, Int](Either3Error.second(ErrorE2("2")))
        case "ErrorE3" => giveDomainError[ErrorE1E2E3, Int](Either3Error.third(ErrorE3("3")))
        case "42" => giveResult[ErrorE1E2E3, Int](42)
        case _ => throw new ExceptionE("outcomeE1E2E3")
      }
    }.refineToOrDie[ExceptionE]

    type ErrorE1E2E3E4 = Either4Error[ErrorE1, ErrorE2, ErrorE3, ErrorE4]
    def outcomeE1E2E3E4(s: String): ZIO[Any, ExceptionE, Outcome[ErrorE1E2E3E4, Int]] = Task {
      import Outcome._
      s match {
        case "ErrorE1" => giveDomainError[ErrorE1E2E3E4, Int](Either4Error.first(ErrorE1("1")))
        case "ErrorE2" => giveDomainError[ErrorE1E2E3E4, Int](Either4Error.second(ErrorE2("2")))
        case "ErrorE3" => giveDomainError[ErrorE1E2E3E4, Int](Either4Error.third(ErrorE3("3")))
        case "ErrorE4" => giveDomainError[ErrorE1E2E3E4, Int](Either4Error.fourth(ErrorE4("3")))
        case "42" => giveResult[ErrorE1E2E3E4, Int](42)
        case _ => throw new ExceptionE("ErrorE1E2E3E4")
      }
    }.refineToOrDie[ExceptionE]

    type ErrorE1E2E3E4E5 = Either5Error[ErrorE1, ErrorE2, ErrorE3, ErrorE4, ErrorE5]
    def outcomeE1E2E3E4E5(s: String): ZIO[Any, ExceptionE, Outcome[ErrorE1E2E3E4E5, Int]] = Task {
      import Outcome._
      s match {
        case "ErrorE1" => giveDomainError[ErrorE1E2E3E4E5, Int](Either5Error.first(ErrorE1("1")))
        case "ErrorE2" => giveDomainError[ErrorE1E2E3E4E5, Int](Either5Error.second(ErrorE2("2")))
        case "ErrorE3" => giveDomainError[ErrorE1E2E3E4E5, Int](Either5Error.third(ErrorE3("3")))
        case "ErrorE4" => giveDomainError[ErrorE1E2E3E4E5, Int](Either5Error.fourth(ErrorE4("3")))
        case "ErrorE5" => giveDomainError[ErrorE1E2E3E4E5, Int](Either5Error.fifth(ErrorE5("3")))
        case "42" => giveResult[ErrorE1E2E3E4E5, Int](42)
        case _ => throw new ExceptionE("outcomeE1E2E3E4E5")
      }
    }.refineToOrDie[ExceptionE]
  }

  object F {
    final class ExceptionX(msg: String) extends Exception(msg)
    final class ExceptionY(msg: String) extends Exception(msg)

    def resultOrThrowable(s: String): Task[Int] = Task {
      s match {
        case "42" => 42
        case _ => throw new ExceptionX("F")
      }
    }

    def resultOrExceptionX(s: String): ZIO[Any, ExceptionX, Int] = Task {
      s match {
        case "42" => 42
        case _ => throw new ExceptionX("F")
      }
    }.refineToOrDie[ExceptionX]

    def resultOrExceptionY(s: String): ZIO[Any, ExceptionY, Int] = Task {
      s match {
        case "42" => 42
        case _ => throw new ExceptionY("F")
      }
    }.refineToOrDie[ExceptionY]
  }

  object ZioServicePattern {
    import A._

    trait Service {
      def serviceA1(s: String): ZIO[Any, ExceptionA, Outcome[ErrorA1, Int]]
      def serviceA2(s: String): ZIO[Any, ExceptionA, Outcome[ErrorA2, Int]]
    }

    def serviceA1(s: String): ZIO[zio.Has[ZioServicePattern.Service], ExceptionA, Outcome[ErrorA1, Int]] =
      ZIO.accessM[zio.Has[ZioServicePattern.Service]](_.get.serviceA1(s))

    def serviceA2(s: String): ZIO[zio.Has[ZioServicePattern.Service], ExceptionA, Outcome[ErrorA2, Int]] =
      ZIO.accessM[zio.Has[ZioServicePattern.Service]](_.get.serviceA2(s))

    // ZLayer[Any, Nothing, ROut]
    val serviceAProvider: ULayer[Has[Service]] = zio.ZLayer.succeed(new ZioServicePattern.Service {
      override def serviceA1(s: String): ZIO[Any, ExceptionA, Outcome[ErrorA1, Int]] = Task {
        import Outcome._
        s match {
          case "42" => giveResult[ErrorA1, Int](42)
          case "ErrorA1" => giveDomainError[ErrorA1, Int](ErrorA1("1"))
          case "ErrorA1id2" => giveDomainError[ErrorA1, Int](ErrorA1("2"))
          case _ => throw new ExceptionA("outcomeA1")
        }
      }.refineToOrDie[ExceptionA]

      override def serviceA2(s: String): ZIO[Any, ExceptionA, Outcome[ErrorA2, Int]] = Task {
        import Outcome._
        s match {
          case "42" => giveResult[ErrorA2, Int](42)
          case "ErrorA2" => giveDomainError[ErrorA2, Int](ErrorA2("2"))
          case _ => throw new ExceptionA("outcomeA2")
        }
      }.refineToOrDie[ExceptionA]
    })
  }
}
