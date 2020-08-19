# FaultOr : expressive error management for ZIO

# Goal
Good error management is crucial for software robustness and quality.
 
This library tries to provide an expressive error management library to the already impressive ZIO library.

The goal is twofold :
- make a strong distinction between 2 types of application errors : system faults and domain errors
- overcoming the current lack of union type in scala 2 and still have error types which are as **expressive** and developer friendly as possible

In order to have expressive error types, I will use type bound and let the compiler infers error composition type with a LUB type (which hopefully should be developer friendly).

Definition : 'LUB' is the acronym for Least Upper Bounds

# Sell me the stuff

## Warning and caveats
IntelliJ internal scala type inference engine does not always infer the proper type bound.
Using Metals (with VSCode in my case) gives the proper type hint and so far it works as expected (but is a bit too verbose as it uses fully package qualified types).

Unfortunately, exhaustivity checking is lost because an upper type bound doesn't give a finite set of types (but Union type in Scala 3 should !).

## A quick overview
``` scala
sealed trait BizzError extends org.scalajos.precise.errors.SingleDomainError
final case class BizzError1(id: String) extends BizzError
final case class BizzError2(id: String) extends BizzError
final case class BizzError3(id: String) extends BizzError

// A FUNCTION WHICH CAN RETURN A RESULT (42), A SYSTEM FAULT (ExceptionA) OR A DOMAIN/BUSINESS ERROR (BizzError1)
def myBusinessFunctionA(s: String): ZIO[Any, ExceptionA, Outcome[BizzError1, Int]] = Task {
  import Outcome._
  s match {
    case "42"         => giveResult[BizzError1, Int](42)
    case "ErrorA1"    => giveDomainError[BizzError1, Int](BizzError1("1"))
    case "ErrorA1bis" => giveDomainError[BizzError1, Int](BizzError1("2"))
    case _            => throw new ExceptionA("ExceptionA")
  }
}.refineToOrDie[ExceptionA]

// ANOTHER FUNCTION WHICH CAN RETURN A RESULT, A SYSTEM FAULT (ExceptionB) OR A DOMAIN/BUSINESS ERROR (BizzError2)
def myBusinessFunctionB(s: String): ZIO[Any, ExceptionB, Outcome[BizzError2, Int]] = ???

// A BUSINESS PROCESS ALREADY MADE OF OTHER COMPOSED BUSINESS FUNCTIONS
// Domain error(s) are already in their composable form  (eg: _ >: BizzError3) and differentiated from fault (eg: Exception)
def otherBusinessProcess(s: String): ZIO[Any, FaultOr[Exception, _ >: BizzError3],Int] = ???

// OUTCOME ARE CONVERTED INTO (not yet composable) BASE FaultOr ERROR TYPE
val s1: ZIO[Any, FaultOr[ExceptionA, BizzError1],Int] = myBusinessFunctionA("42").toFaultOr
val s2: ZIO[Any, FaultOr[ExceptionB, BizzError2],Int] = myBusinessFunctionB("42").toFaultOr

// CONVERT BASE FaultOr INTO COMPOSABLE FaultOr (using type bounds)
val s1ComposableDomainError: ZIO[Any, FaultOr[ExceptionA, _ >: BizzError1],Int] = s1.composableDomainError
val s2ComposableDomainError: ZIO[Any, FaultOr[ExceptionB, _ >: BizzError2],Int] = s2.composableDomainError

// **HERE IS THE INTERESTING STUFF** : ERROR TYPE STAYS EXPRESSIVE
// Notice that :
// - the type ZIO[Any, FaultOr[Exception, _ >: BizzError1 with BizzError2 with BizzError3],Int] is the type inferred by Metals
// - errors do compose nicely in a 'for comprehension' !
// - I can keep track of all errors I have to deal with
// - FaultOr makes a clear distinction between system fault and domain error
// - the system fault part is covariant
// Beware that intellij type engine is quickly lost, Metals always gives us the right (but verbose) type

val program: ZIO[Any, FaultOr[Exception, _ >: BizzError1 with BizzError2 with BizzError3],Int] = for {
  _ <- s1ComposableDomainError
  _ <- s2ComposableDomainError
  _ <- otherBusinessProcess("42")
} yield 42
```

Take a look at the test cases for a more complete coverage of all the features.

## Installation and usage
To publish the project locally, go to the project directory and type ```sbt publishLocal```

To import the project in sbt, add : ```"ch.scalajos" %% "faultor" % "1.0-SNAPSHOT"``` in your sbt project dependencies.

The library has only one transitive dependency on (core) ZIO 1.0.0.

# 2 categories of errors : Faults and Domain Errors
A **Domain Error** (or business error) is a normal and expected outcome of a business action.
It simply means that the business action cannot complete because some business conditions are not met.
A Domain Error is a normal part of the business process and it's often used to express alternatives business scenario in business Use Cases.

eg:
performing a debit operation on an overdraft account should result in a 'refused' error.
The error could then be handled to propose the client to credit the account. It's an expected business scenario.

On the opposite, a **System Fault** is used to signal a system error, usually a technical error which cannot be resolved.
A **System Fault** does not crash the program (unless it is a fatal system error), but the only way for the program to handle it is to signal in the cleanest way that the business action cannot be performed.
A **System Fault** does not trigger any alternative business scenario.

eg:
the program tries to make some database operation on a down database : a ```SQLException``` is thrown by the driver.
One can retry the action but if the database is still down, the only thing that can be done is to signal the user that the request cannot be completed and maybe to send a monitoring alert.

**Domain Errors** belongs to the business domain, **System Faults** are purely technical and should not be mixed with business domain concerns.


**Domain Errors** usually carry a business context to interpret the error, they don't need a stacktrace ! They should be based on simple Algebraic Data Type.

**System Faults** normally don't have a business context need, they must have a stacktrace: they are based on Java exceptions. 

These 2 categories of errors are strictly exclusive : code can only produce a system fault **OR [EXCLUSIVE]** a domain error.

This is the '*raison d'etre*' of the FaultOr data type.


# Does it replace the ZIO error channel ?
Absolutely not ! The library reuses and extends the ZIO error channel : the default error channel has only the notion of *errors*.

With this library it can now be used to carry on the notions of both system faults and domain errors with distinctive behaviour.

## Is using the ZIO error channel the right approach ?
I don't know yet. It feels so far OK for me.
My personal experience tends to show that :
- I prefer to deal with normal result and exceptional domain errors in distinct code path and using ZIO error channel makes it clearly distinct
- exceptional scenario (and their associated domain errors) tend to evolve more often than nominal scenario, a clear separation make them easier to manage and reduce or isolate complexity pollution

Another way to handle domain error would be to not use the ZIO error channel at all :

- the ZIO error channel would **only** be used for **System faults** and a yet to be defined composite result type (likely to be based on Either or a kind of Outcome type already used in this library) would handle normal result **and** expected domain errors.

I will try to investigate this approach too in another library attempt and see if it makes code simpler than the error channel.
 
What I believe to be true in any case is that (technical) System Faults and (business) Domain Errors should not be based on the same error type (Exception) as their use and behaviour is completely different.
Java is wrong on this subject and Scala has no standard best practice yet.

Feel free to contribute and tell me what you think.

Some crazy/stupid though :

ZIO is based on TriFunctor R, E, A. All errors are managed through the same E channel.
A quadrifunctor (!!) ```[R (environment), F (fault), E (domain error), A (result)]``` might express a clean separation between system faults and domain errors but it's likely to be a bit excessive and overcomplicated ;-)

# Semantic of System Fault and Domain Error regarding Layers

## Layers
A program is usually made of several layers whatever the underlying architecture (n-tiers, hexagonal or onion).

For instance, a REST Web Service layer calling the Business Service layer calling the Persistence Layer through repositories.

## Fault semantic
A **System Fault** would typically occur at the edge layers of the program dealing with Input/Output, for instance the Persistence Layer.

When a **System Fault** occurs in an edge layer, it should cross all the caller layers and be propagated unchanged up to the initial caller which can signal the failure appropriately (sending an HTTP 502 response for instance).

As System Faults are propagated through the layers, they don't need to be chained to other faults.

```
[edge layer]                  [core layer]                  [edge layer]
Persistence Layer  ----- Business Service Layer ----- Web Services Layer
      <----------------------------  <------------------------- call
  System Fault
(technical error)
      ---------------------------->...-------------------------> Fault Handler
                            fault propagation (unchanged)
```

## Domain Error semantic
On the contrary **Domain Error** semantic is only valid **within** its layer.
A 'read' operation on the repository could fail with an EntityNotFound which has a clear meaning in the Persistence Layer.

The business service which called the repository should not expose an EntityNotFound domain error but a meaningful (for its layer) business error, DebitRejectedError for instance.

A layer will need to 'adapt' incoming **Domain Errors** from other layers to **Domain Errors semantically sound** in the domain layer.
```
Persistence Layer  ----- Business Service Layer ----- Web Services Layer
      <----------------------------  <------------------------- call
    EntityNotFound 
    (domain error)
      ---------------------------->
                             DebitRejectedError ----------------> [Domain Error Handler] (return code 40x)
                                     |           (or)
                                     v (or)
                           [Domain Error Handler]
                         handle alternative scenario
```
As **Domain Errors** are translated into other Domain Errors when they cross layers, it's important to not lose the initial error cause :
usually one needs to keep track of the initial domain error (if there is one).

**Domain Errors could be chained** to other Domain Errors.

eg:
DebitRefusedError(cause = entityNotFoundError)

Note : when **Domain Errors** 'are 'adapted', the cardinality of new domain errors should be less than or at worst identical to the incoming domain error cardinality.
It's not a good practice to increase errors cardinality when crossing layers.
```
Layer N-2                         Layer N-1                    Layer N

ErrorA-------+
ErrorB       |                |-> ErrorU ---+    
ErrorC       |-> adapt error -+   ErrorV    |-> adapt error -> ErrorX
ErrorD       |                |-> ErrorW ---+
ErrorE-------+
```

# Library core abstractions

## Domain Error
A **Domain Error** represents a business error and is one of the expected and normal outcome.

It's encoded as an ADT and it **must** extend ```SingleDomainError``` or  ```ChainableDomainError``` :
``` scala
sealed trait MyErrorA extends SingleDomainError
final case class MyErrorA1 extends MyErrorA
...
```
```
sealed trait MyErrorB extends SingleDomainError
final case class MyErrorB1(..., chainedWith: SingleDomainError = null) extends ChainableDomainError("some message", chainedWith) with MyErrorB
...
```

A Domain Error is not based on Java Exception and therefore does not have any stacktrace : it's a much lighter data object; performance should be pretty good regarding memory or cpu when used in 'high' frequency.

## System Fault
A **System Fault** represents a system error which cannot be finally recovered (eg: when a database is down for a long time)

Transient System Error could be recovered through a 'retry' behaviour but persistent failure must be signalled by a System Fault.

Any exception inheriting from ```Throwable``` will represent a System Fault :
- exceptions from middleware libraries such as ```SQLException```, ```JMSException```, ```IOException```, ... 
- any custom exceptions you could create :

```final class MyFault(msg: String) extends Exception(msg)```

A System Fault will 'bubble up' through the callers stack, up to the initial root call which should be in charge of the fault handling (usually through a zio fold or foldM).

System faults should be infrequent : the stacktrace creation cost should not be a burden in this case.

## Expected Domain Errors
it can be :
- a single domain error ```ErrorA1```
- a type bound domain error instance eg: ```Expected[_ >: ErrorA1 with ErrorA2]```
- an EitherNError domain error instance eg: ```Either2Error[ErrorB1, ErrorB2]```

## Either System Fault Or Domain Error
**FaultOr** type represents either a System Fault *or* an expected Domain Error (based on upper type bound or on EitherNError).

It's the most frequent type one will use.

``` scala
final case class FaultOr[+FAULT, E](cause: Either[FAULT, E])(implicit ev1: FAULT <:< Throwable) extends EffectError
```

Notice that fault is covariant.

## Outcome
Use Outcome data type within a Task to return a Domain Error, or a result value.

The Task error channel (Throwable or refined exception) is used to signal a system fault.

``` scala
sealed trait Outcome[E <: DomainError, A]
// and 2 private children : ExpectedError and Result
```

use ```giveDomainError(domainError)``` or ```giveResult(value)``` function to return the right outcome :

``` scala
def outcomeA1(s: String): Task[Outcome[ErrorA1, Int]] = Task {
  import Outcome._
  s match {
    case "42"      => giveResult[ErrorA1, Int](42)                    // a result
    case "ErrorA1" => giveDomainError[ErrorA1, Int](ErrorA1("1"))     // a domain error
    case _         => throw new Exception("outcomeA1")                // a system fault
  }
}
```

## EitherNError (N from 2 to 5)
A service often needs to produce more than one domain error.

Use the appropriate EitherNError :

``` scala
type ErrorC1C2 = Either2Error[ErrorC1, ErrorC2]
def outcomeC1C2(s: String): Task[Outcome[ErrorC1C2, Int]] = Task {
  import Outcome._
  s match {
    case "42"      => giveResult[ErrorC1C2, Int](42)
    case "ErrorC1" => giveDomainError[ErrorC1C2, Int](Either2Error.first(ErrorC1("1")))
    case "ErrorC2" => giveDomainError[ErrorC1C2, Int](Either2Error.second(ErrorC2("2")))    
    case _         => throw new Exception("outcomeC1C2")
  }
}
```

To get the typed bound version of an EitherNError, use the '.composableDomainError' operator :

``` scala
import errors.syntax._

type ErrorC1C2 = Either2Error[ErrorC1, ErrorC2]

val task:        Task[Outcome[ErrorC1C2, Int]]                = ???
val taskFaultOr: ZIO[Any, FaultOr[Throwable, ErrorC1C2], Int] = task.toFaultOr
 
val s: ZIO[Any, FaultOr[Throwable, _ >: ErrorC1 with ErrorC2],Int] = taskFaultOr.composableDomainError
```

# Examples

## Error definition
It's based on an ADT type.

Here is a domain error ADT ErrorA which can be chained with other domain errors :
``` scala
sealed trait ErrorA extends SingleDomainError
final case class ErrorA1(id: String, chainedWith: SingleDomainError = null) extends ChainableDomainError(s"ErrorA1 with $id", chainedWith) with ErrorA
final case class ErrorA2(id: String, chainedWith: SingleDomainError = null) extends ChainableDomainError(s"ErrorA2 with $id", chainedWith) with ErrorA
final case class ErrorA3(id: String, chainedWith: SingleDomainError = null) extends ChainableDomainError(s"ErrorA3 with $id", chainedWith) with ErrorA

sealed trait ErrorB extends SingleDomainError
...

sealed trait ErrorC extends SingleDomainError
...
```

Now one could also define a domain error which can't be chained to other domain errors :
``` scala
sealed trait ErrorA extends SingleDomainError
final case class ErrorA1(id: String) extends ErrorA
final case class ErrorA2(id: String) extends ErrorA
final case class ErrorA3(id: String) extends ErrorA
```

## Composition of errors with FaultOr
``` scala
// can return a result, one domain error or a fault
val t1: String => Task[Outcome[ErrorA1, Int]] = (s: String) => Task {
  import Outcome._
  s match {
    case "42"      => giveResult[Int, ErrorA1](42)                 // outcome is a result value
    case "ErrorA1" => giveDomainError[Int, ErrorA1](ErrorA1("1"))  // outcome is an expected domain error
    case _         => throw new Exception("outcomeA1")             // an unexpected error, a system fault
  }
}

// can return a result, two domain errors or a fault
type ErrorC1C2 = Either2Error[ErrorC1, ErrorC2]

val t2: String => Task[Outcome[ErrorC1C2, Int]] = (s: String) => Task {
  import Outcome._
  s match {
    case "42"      => giveResult[Int, ErrorC1C2](42)
    case "ErrorC1" => giveDomainError[Int, ErrorC1C2](Either2Error.first(ErrorC1("1")))
    case "ErrorC2" => giveDomainError[Int, ErrorC1C2](Either2Error.second(ErrorC2("2")))    
    case _         => throw new Exception("outcomeC1C2")
  }
}

// can return a result or a throwable aka a system fault
def t3: String => Task[Int] = (s: String) => Task {
  s match {
    case "42" => 42
    case _    => throw new Exception("B")
  }
}

import org.scalajos.precise.errors.syntax._

// keep in mind that these FaultOr are not yet composable
val s1: ZIO[Any, FaultOr[Throwable, ErrorA1],Int]                        = t1("42").toFaultOr
val s2: ZIO[Any, FaultOr[Throwable, Either2Error[ErrorC1, ErrorC2]],Int] = t2("42").toFaultOr

// 'Any' means no domain error. As there is no domain error, this FaultOr is already composable
val s3: ZIO[Any, FaultOr[Throwable, Any],Int]                            = t3("42").toFaultOr

// here is the way to get composable FaultOr
val s1ComposableError: ZIO[Any, FaultOr[Throwable, _ >: ErrorA1],Int]              = s1.composableDomainError                     
val s2ComposableError: ZIO[Any, FaultOr[Throwable, _ >: ErrorC1 with ErrorC2],Int] = s2.composableDomainError
                      
// metals gives the type hint
// ZIO[Any,errors.FaultOr[Throwable, _ >: org.scalajos.precise.testerrors.services.A.ErrorA1 with org.scalajos.precise.testerrors.services.C.ErrorC1 with org.scalajos.precise.testerrors.services.C.ErrorC2],Int]
// which is right but too verbose, after a quick cleaning
val p: ZIO[Any, FaultOr[Throwable, _ >: ErrorA1 with ErrorC1 with ErrorC2],Int] = for {
  _ <- s1ComposableError
  _ <- s2ComposableError
  _ <- s3
} yield 42
```

## Using EitherNError with FaultOr
A core business service could return 0, 1 or more than 1 domain error.

To return more than 1 domain error use the EitherNError where N is [2..5].

EitherNError are not directly composable error as they are not based on type bound.

To get the associated type bound, one must use the 'linearizeDomainError' operator.
```
val res: ZIO[Any, FaultOr[Throwable, Either2Error[ErrorC1, ErrorC2]],Int]      = ...
val resComposable: ZIO[Any, FaultOr[Throwable, _ >: ErrorC1 with ErrorC2],Int] = res.composableDomainError
```
Core business services should not throw more than 5 domain errors. Refactor to several smaller services if required.

Note that error composition itself is **not limited** and can accumulate as many errors as necessary.

## Adapting errors
when crossing a layer, the Domain Error semantic changes and must be 'adapted' to the semantic of the new layer.

**Warning :** one can only 'adapt' incoming domain errors to produce up to 5 different Domain Errors as internally an EitherNError is used (N up to 5).

Here is an example on how to 'adapt' a composable (type bound based) domain error :
``` scala
// IN LAYER 0
val s1: ZIO[Any, FaultOr[Throwable, _ >: ErrorA1],Int] = ???
val s2: ZIO[Any, FaultOr[Throwable, _ >: ErrorA2],Int] = ???

val p: ZIO[Any, FaultOr[Throwable, _ >: ErrorA1 with ErrorA2],Int] = for {
  _ <- s1
  _ <- s2
} yield 42

// IN LAYER 1
def toErrorB1(from: Expected[_ >: ErrorA1 with ErrorA2]): ErrorB1 = from.cause match {
  case a1@ErrorA1(id, _) => ErrorB1(id, a1)
  case a2@ErrorA2(id, _) => ErrorB1(id, a2)
}

val adapted: ZIO[Any, FaultOr[Throwable, _ >: ErrorB1],Int] = p.mapDomainErrorsTo1(toErrorB1)

// error composition is still possible
val s3: ZIO[Any, FaultOr[_ >: ErrorA3],Int] = ???
val p2: ZIO[Any, FaultOr[Throwable, _ >: ErrorB1 with ErrorA3],Int] = for {
  _ <- adapted
  _ <- s3
} yield 42
```

and another one to show how to adapt EitherNError domain error :
``` scala
val action1: ZIO[Any, FaultOr[ExceptionC, Either2Error[ErrorC1, ErrorC2]],Int] = ???  

val adapted: ZIO[Any, FaultOr[ExceptionC, ErrorB1], Int] = action1.mapDomainErrorsTo1((first: ErrorC1)  => ErrorB1(first.id, first),
                                                                                      (second: ErrorC2) => ErrorB1(second.id, second))

val composableAdapted: ZIO[Any,FaultOr[ExceptionC, _ >: ErrorB1],Int] = adapted.composableDomainError

// error composition is still possible
val s3: ZIO[Any, FaultOr[ExceptionA, _ >: ErrorA2],Int] = ???
val p2: ZIO[Any, FaultOr[Exception, _ >: ErrorB1 with ErrorA2],Int] = for {
  _ <- composableAdapted
  _ <- s3
} yield 42
```
   
## Folding System Faults And Domain Errors
Edge services (such as a REST service) will need to convert System Faults or Domain Errors to some useful value (HTTP 40x or 50x for instance).

Folding FaultOr can be used for such purpose :
``` scala
val p: ZIO[Any, FaultOr[Throwable, _ >: ErrorA1 with ErrorA2],Int] = ???

// can use ZIO fold or foldM depending on the type of produced value
val v = p.fold((faultOr: FaultOr[Throwable, _ >: ErrorA1 with ErrorA2]) => faultOr.fold(
                    // HANDLE FAULT :
                    _ => 0, // fault: throwable
                    // HANDLE DOMAIN ERRORS :
                    (expected: Expected[_ >: ErrorA1 with ErrorA2]) => expected.cause match { // domain errors
                      case ErrorA1(_, _) => -1
                      case ErrorA2(_, _) => -2
                    }),
                // HANDLE NORMAL RESULT :
                result => result)
```

## ZLayer
The whole library is fully usable with ZIO effect having ```Has``` dependencies :

``` scala
val action1WithDependency: ZIO[zio.Has[ZioServicePattern.Service], ExceptionA, Outcome[ErrorA1, Int]] = ???
val action2WithDependency: ZIO[zio.Has[ZioServicePattern.Service], ExceptionA, Outcome[ErrorA2, Int]] = ???

val action1WithDependencyWithComposableError: ZIO[zio.Has[ZioServicePattern.Service],FaultOr[ExceptionA, _ >: ErrorA1],Int] = action1WithDependency.toFaultOr.composableDomainError
val action2WithDependencyWithComposableError: ZIO[zio.Has[ZioServicePattern.Service],FaultOr[ExceptionA, _ >: ErrorA2],Int] = action2WithDependency.toFaultOr.composableDomainError

val p: ZIO[zio.Has[ZioServicePattern.Service],FaultOr[ExceptionA, _ >: ErrorA1 with ErrorA2],Int] = for {
  _ <- action1WithDependencyWithComposableError
  _ <- action2WithDependencyWithComposableError
} yield 42


val resolvedProgram: ZIO[Any, FaultOr[ExceptionA, _ >: ErrorA1 with ErrorA2], Int] = p.provideLayer(ZioServicePattern.liveServiceProvider)

runtime.unsafeRun(resolvedProgram)
```

# References and thanks
I'd like to thank :
- the ZIO team and the ZIO  community for making functional programming in scala fun and productive
- François Armand for his talk and slides on Error Management (https://www.youtube.com/watch?v=q0PlcgR5M1Q)

He expressed many important ideas that resonated well with my own experience and that I tried to apply for my reference project on hexagonal architecture with ZIO.

This ultimately lead me to write this library.

I hope this library will help developers to better express errors in scala 2.x nevertheless this is the very first version, and it likely will need to be more battlefield tested.

Any help or advice is very welcome.

# Future work

## On the library itself

If the library is popular enough I'll try to publish it to Maven Central once it is hardened.
I'm very bad at finding libraries name, the final artifact name could change if I find or is suggested a better one.

I will also try to work on a Scala 3 port as the Union type will immensely simplify the implementation (no more EitherNError required and type bound could be entirely replaced by union type which will regain exhaustivity checking)

## Other libraries and reference implementation
I'm also currently working :
- on a library to support XA transaction and XA resources within ZIO (currently with Atomikos XA library) which is a bit tricky due to the thread stickiness of JTA specification (Java Transaction API)
- on a reference implementation of a hexagonal architecture with ZIO, Doobie (with a ZLayer approach), HTTP4S, this error library and the ZIO XA library (with JMS/ActiveMq)

# Quote
Jacques Brel said once that we "gain humility by doing; the more we do, the more we see that it's hard, and the more we become humble" 

# Legal

Copyright 2020 José Romero. All rights reserved.