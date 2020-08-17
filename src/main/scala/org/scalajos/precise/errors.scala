package org.scalajos.precise

import zio.{UIO, ZIO}


/**
 * This object contains all the data types and syntax extensions required for the library
 */
object errors {
  // -----------------------------------------------------------------------------
  // BASIC ADT STRUCTURE FOR DOMAIN ERRORS
  // -----------------------------------------------------------------------------
  /**
   * Base ADT to represent a domain error (flat type bound or explicit EitherNError)
   */
  sealed trait DomainError

  /**
   * The type used to describe domain errors. this trait must be extended by application defined domain errors
   */
  trait SingleDomainError extends DomainError

  /**
   * This type is only used to represent explicit EitherNError instances
   */
  sealed trait EitherDomainErrors extends DomainError

  /**
   * Base class one can extends if error chaining behaviour is required
   * @param message error message
   * @param chained chained domain error, null if no domain error is chained
   */
  abstract class ChainableDomainError(val message: String, val chained: DomainError) extends SingleDomainError

  // -----------------------------------------------------------------------------
  // EXPRESSING A DOMAIN ERROR SET (1 to 5)
  // WHEN ONE WANTS TO EXPLICITLY THROW A SET OF ERRORS
  // core business services should not throw more than 5 domain errors, refactor to several smaller services if required
  // note that error composition itself is not limited and can accumulate as many errors as necessary
  // -----------------------------------------------------------------------------

  /**
   * Define a set of EitherNError which represent explicit 'either' domain error set (up to 5)
   * They can be folded to a unique value
   * They can be mapped to any other EitherNError
   */
  object either {
    sealed trait Either2Error[A <: SingleDomainError, B <: SingleDomainError] extends EitherDomainErrors {
      def fold[T](foldFirst: A => T, foldSecond: B => T): T
      def overjectionTo1[A1 <: SingleDomainError](mapFirst: A => A1, mapSecond: B => A1): A1
      def bijection[A1 <: SingleDomainError, B1 <: SingleDomainError](mapFirst: A => A1, mapSecond: B => B1): Either2Error[A1, B1]
    }

    object Either2Error {
      private[precise] final case class one[A <: SingleDomainError, B <: SingleDomainError](value: A) extends Either2Error[A, B] {
        def fold[T](foldFirst: A => T, foldSecond: B => T): T = foldFirst(value)
        def overjectionTo1[A1 <: SingleDomainError](mapFirst: A => A1, mapSecond: B => A1): A1 = mapFirst(value)
        def bijection[A1 <: SingleDomainError, B1 <: SingleDomainError](mapFirst: A => A1, mapSecond: B => B1): Either2Error[A1, B1] = one[A1, B1](mapFirst(value))
      }
      private[precise] final case class two[A <: SingleDomainError, B <: SingleDomainError](value: B) extends Either2Error[A, B] {
        def fold[T](foldFirst: A => T, foldSecond: B => T): T = foldSecond(value)
        def overjectionTo1[A1 <: SingleDomainError](mapFirst: A => A1, mapSecond: B => A1): A1 = mapSecond(value)
        def bijection[A1 <: SingleDomainError, B1 <: SingleDomainError](mapFirst: A => A1, mapSecond: B => B1): Either2Error[A1, B1] = two(mapSecond(value))
      }

      def first[A <: SingleDomainError, B <: SingleDomainError](value: A): Either2Error[A, B] = one[A, B](value)
      def second[A <: SingleDomainError, B <: SingleDomainError](value: B): Either2Error[A, B] = two[A, B](value)
    }

    sealed trait Either3Error[A <: SingleDomainError, B <: SingleDomainError, C <: SingleDomainError] extends EitherDomainErrors {
      def fold[T](foldFirst: A => T, foldSecond: B => T, foldThird: C => T): T
      def overjectionTo1[A1 <: SingleDomainError](mapFirst: A => A1, mapSecond: B => A1, mapThird: C => A1): A1
      def overjectionTo2[A1 <: SingleDomainError, B1 <: SingleDomainError](mapFirst: A => Either2Error[A1, B1], mapSecond: B => Either2Error[A1, B1], mapThird: C => Either2Error[A1, B1]): Either2Error[A1, B1]
      def bijection[A1 <: SingleDomainError, B1 <: SingleDomainError, C1 <: SingleDomainError](mapFirst: A => A1, mapSecond: B => B1, mapThird: C => C1): Either3Error[A1, B1, C1]
    }

    object Either3Error {
      private[precise] final case class one[A <: SingleDomainError, B <: SingleDomainError, C <: SingleDomainError](value: A) extends Either3Error[A, B, C] {
        def fold[T](foldFirst: A => T, foldSecond: B => T, foldThird: C => T): T = foldFirst(value)
        def overjectionTo1[A1 <: SingleDomainError](mapFirst: A => A1, mapSecond: B => A1, mapThird: C => A1): A1 = mapFirst(value)
        def overjectionTo2[A1 <: SingleDomainError, B1 <: SingleDomainError](mapFirst: A => Either2Error[A1, B1], mapSecond: B => Either2Error[A1, B1], mapThird: C => Either2Error[A1, B1]): Either2Error[A1, B1] = mapFirst(value)
        def bijection[A1 <: SingleDomainError, B1 <: SingleDomainError, C1 <: SingleDomainError](mapFirst: A => A1, mapSecond: B => B1, mapThird: C => C1): Either3Error[A1, B1, C1] = one(mapFirst(value))
      }
      private[precise] final case class two[A <: SingleDomainError, B <: SingleDomainError, C <: SingleDomainError](value: B) extends Either3Error[A, B, C] {
        def fold[T](foldFirst: A => T, foldSecond: B => T, foldThird: C => T): T = foldSecond(value)
        def overjectionTo1[A1 <: SingleDomainError](mapFirst: A => A1, mapSecond: B => A1, mapThird: C => A1): A1 = mapSecond(value)
        def overjectionTo2[A1 <: SingleDomainError, B1 <: SingleDomainError](mapFirst: A => Either2Error[A1, B1], mapSecond: B => Either2Error[A1, B1], mapThird: C => Either2Error[A1, B1]): Either2Error[A1, B1] = mapSecond(value)
        def bijection[A1 <: SingleDomainError, B1 <: SingleDomainError, C1 <: SingleDomainError](mapFirst: A => A1, mapSecond: B => B1, mapThird: C => C1): Either3Error[A1, B1, C1] = two(mapSecond(value))
      }
      private[precise] final case class three[A <: SingleDomainError, B <: SingleDomainError, C <: SingleDomainError](value: C) extends Either3Error[A, B, C] {
        def fold[T](foldFirst: A => T, foldSecond: B => T, foldThird: C => T): T = foldThird(value)
        def overjectionTo1[A1 <: SingleDomainError](mapFirst: A => A1, mapSecond: B => A1, mapThird: C => A1): A1 = mapThird(value)
        def overjectionTo2[A1 <: SingleDomainError, B1 <: SingleDomainError](mapFirst: A => Either2Error[A1, B1], mapSecond: B => Either2Error[A1, B1], mapThird: C => Either2Error[A1, B1]): Either2Error[A1, B1] = mapThird(value)
        def bijection[A1 <: SingleDomainError, B1 <: SingleDomainError, C1 <: SingleDomainError](mapFirst: A => A1, mapSecond: B => B1, mapThird: C => C1): Either3Error[A1, B1, C1] = three(mapThird(value))
      }

      def first[A <: SingleDomainError, B <: SingleDomainError, C <: SingleDomainError](value: A): Either3Error[A, B, C] = one[A, B, C](value)
      def second[A <: SingleDomainError, B <: SingleDomainError, C <: SingleDomainError](value: B): Either3Error[A, B, C] = two[A, B, C](value)
      def third[A <: SingleDomainError, B <: SingleDomainError, C <: SingleDomainError](value: C): Either3Error[A, B, C] = three[A, B, C](value)
    }

    sealed trait Either4Error[A <: SingleDomainError, B <: SingleDomainError, C <: SingleDomainError, D <: SingleDomainError] extends EitherDomainErrors {
      def fold[T](foldFirst: A => T, foldSecond: B => T, foldThird: C => T, foldFourth: D => T): T
      def overjectionTo1[A1 <: SingleDomainError](mapFirst: A => A1, mapSecond: B => A1, mapThird: C => A1, mapFourth: D => A1): A1
      def overjectionTo2[A1 <: SingleDomainError, B1 <: SingleDomainError](mapFirst: A => Either2Error[A1, B1], mapSecond: B => Either2Error[A1, B1], mapThird: C => Either2Error[A1, B1], mapFourth: D => Either2Error[A1, B1]): Either2Error[A1, B1]
      def overjectionTo3[A1 <: SingleDomainError, B1 <: SingleDomainError, C1 <: SingleDomainError](mapFirst: A => Either3Error[A1, B1, C1], mapSecond: B => Either3Error[A1, B1, C1], mapThird: C => Either3Error[A1, B1, C1], mapFourth: D => Either3Error[A1, B1, C1]): Either3Error[A1, B1, C1]
      def bijection[A1 <: SingleDomainError, B1 <: SingleDomainError, C1 <: SingleDomainError, D1 <: SingleDomainError](mapFirst: A => A1, mapSecond: B => B1, mapThird: C => C1, mapFourth: D => D1): Either4Error[A1, B1, C1, D1]
    }

    object Either4Error {
      private[precise] final case class one[A <: SingleDomainError, B <: SingleDomainError, C <: SingleDomainError, D <: SingleDomainError](value: A) extends Either4Error[A, B, C, D] {
        def fold[T](foldFirst: A => T, foldSecond: B => T, foldThird: C => T, foldFourth: D => T): T = foldFirst(value)
        def overjectionTo1[A1 <: SingleDomainError](mapFirst: A => A1, mapSecond: B => A1, mapThird: C => A1, mapFourth: D => A1): A1 = mapFirst(value)
        def overjectionTo2[A1 <: SingleDomainError, B1 <: SingleDomainError](mapFirst: A => Either2Error[A1, B1], mapSecond: B => Either2Error[A1, B1], mapThird: C => Either2Error[A1, B1], mapFourth: D => Either2Error[A1, B1]): Either2Error[A1, B1] = mapFirst(value)
        def overjectionTo3[A1 <: SingleDomainError, B1 <: SingleDomainError, C1 <: SingleDomainError](mapFirst: A => Either3Error[A1, B1, C1], mapSecond: B => Either3Error[A1, B1, C1], mapThird: C => Either3Error[A1, B1, C1], mapFourth: D => Either3Error[A1, B1, C1]): Either3Error[A1, B1, C1] = mapFirst(value)
        def bijection[A1 <: SingleDomainError, B1 <: SingleDomainError, C1 <: SingleDomainError, D1 <: SingleDomainError](mapFirst: A => A1, mapSecond: B => B1, mapThird: C => C1, mapFourth: D => D1): Either4Error[A1, B1, C1, D1] = one(mapFirst(value))
      }
      private[precise] final case class two[A <: SingleDomainError, B <: SingleDomainError, C <: SingleDomainError, D <: SingleDomainError](value: B) extends Either4Error[A, B, C, D] {
        def fold[T](foldFirst: A => T, foldSecond: B => T, foldThird: C => T, foldFourth: D => T): T = foldSecond(value)
        def overjectionTo1[A1 <: SingleDomainError](mapFirst: A => A1, mapSecond: B => A1, mapThird: C => A1, mapFourth: D => A1): A1 = mapSecond(value)
        def overjectionTo2[A1 <: SingleDomainError, B1 <: SingleDomainError](mapFirst: A => Either2Error[A1, B1], mapSecond: B => Either2Error[A1, B1], mapThird: C => Either2Error[A1, B1], mapFourth: D => Either2Error[A1, B1]): Either2Error[A1, B1] = mapSecond(value)
        def overjectionTo3[A1 <: SingleDomainError, B1 <: SingleDomainError, C1 <: SingleDomainError](mapFirst: A => Either3Error[A1, B1, C1], mapSecond: B => Either3Error[A1, B1, C1], mapThird: C => Either3Error[A1, B1, C1], mapFourth: D => Either3Error[A1, B1, C1]): Either3Error[A1, B1, C1] = mapSecond(value)
        def bijection[A1 <: SingleDomainError, B1 <: SingleDomainError, C1 <: SingleDomainError, D1 <: SingleDomainError](mapFirst: A => A1, mapSecond: B => B1, mapThird: C => C1, mapFourth: D => D1): Either4Error[A1, B1, C1, D1] = two(mapSecond(value))
      }
      private[precise] final case class three[A <: SingleDomainError, B <: SingleDomainError, C <: SingleDomainError, D <: SingleDomainError](value: C) extends Either4Error[A, B, C, D] {
        def fold[T](foldFirst: A => T, foldSecond: B => T, foldThird: C => T, foldFourth: D => T): T = foldThird(value)
        def overjectionTo1[A1 <: SingleDomainError](mapFirst: A => A1, mapSecond: B => A1, mapThird: C => A1, mapFourth: D => A1): A1 = mapThird(value)
        def overjectionTo2[A1 <: SingleDomainError, B1 <: SingleDomainError](mapFirst: A => Either2Error[A1, B1], mapSecond: B => Either2Error[A1, B1], mapThird: C => Either2Error[A1, B1], mapFourth: D => Either2Error[A1, B1]): Either2Error[A1, B1] = mapThird(value)
        def overjectionTo3[A1 <: SingleDomainError, B1 <: SingleDomainError, C1 <: SingleDomainError](mapFirst: A => Either3Error[A1, B1, C1], mapSecond: B => Either3Error[A1, B1, C1], mapThird: C => Either3Error[A1, B1, C1], mapFourth: D => Either3Error[A1, B1, C1]): Either3Error[A1, B1, C1] = mapThird(value)
        def bijection[A1 <: SingleDomainError, B1 <: SingleDomainError, C1 <: SingleDomainError, D1 <: SingleDomainError](mapFirst: A => A1, mapSecond: B => B1, mapThird: C => C1, mapFourth: D => D1): Either4Error[A1, B1, C1, D1] = three(mapThird(value))
      }
      private[precise] final case class four[A <: SingleDomainError, B <: SingleDomainError, C <: SingleDomainError, D <: SingleDomainError](value: D) extends Either4Error[A, B, C, D] {
        def fold[T](foldFirst: A => T, foldSecond: B => T, foldThird: C => T, foldFourth: D => T): T = foldFourth(value)
        def overjectionTo1[A1 <: SingleDomainError](mapFirst: A => A1, mapSecond: B => A1, mapThird: C => A1, mapFourth: D => A1): A1 = mapFourth(value)
        def overjectionTo2[A1 <: SingleDomainError, B1 <: SingleDomainError](mapFirst: A => Either2Error[A1, B1], mapSecond: B => Either2Error[A1, B1], mapThird: C => Either2Error[A1, B1], mapFourth: D => Either2Error[A1, B1]): Either2Error[A1, B1] = mapFourth(value)
        def overjectionTo3[A1 <: SingleDomainError, B1 <: SingleDomainError, C1 <: SingleDomainError](mapFirst: A => Either3Error[A1, B1, C1], mapSecond: B => Either3Error[A1, B1, C1], mapThird: C => Either3Error[A1, B1, C1], mapFourth: D => Either3Error[A1, B1, C1]): Either3Error[A1, B1, C1] = mapFourth(value)
        def bijection[A1 <: SingleDomainError, B1 <: SingleDomainError, C1 <: SingleDomainError, D1 <: SingleDomainError](mapFirst: A => A1, mapSecond: B => B1, mapThird: C => C1, mapFourth: D => D1): Either4Error[A1, B1, C1, D1] = four(mapFourth(value))
      }

      def first[A <: SingleDomainError, B <: SingleDomainError, C <: SingleDomainError, D <: SingleDomainError](value: A): Either4Error[A, B, C, D] = one[A, B, C, D](value)
      def second[A <: SingleDomainError, B <: SingleDomainError, C <: SingleDomainError, D <: SingleDomainError](value: B): Either4Error[A, B, C, D] = two[A, B, C, D](value)
      def third[A <: SingleDomainError, B <: SingleDomainError, C <: SingleDomainError, D <: SingleDomainError](value: C): Either4Error[A, B, C, D] = three[A, B, C, D](value)
      def fourth[A <: SingleDomainError, B <: SingleDomainError, C <: SingleDomainError, D <: SingleDomainError](value: D): Either4Error[A, B, C, D] = four[A, B, C, D](value)
    }

    sealed trait Either5Error[A <: SingleDomainError, B <: SingleDomainError, C <: SingleDomainError, D <: SingleDomainError, E <: SingleDomainError] extends EitherDomainErrors {
      def fold[T](foldFirst: A => T, foldSecond: B => T, foldThird: C => T, foldFourth: D => T, foldFifth: E => T): T
      def overjectionTo1[A1 <: SingleDomainError](mapFirst: A => A1, mapSecond: B => A1, mapThird: C => A1, mapFourth: D => A1, mapFifth: E => A1): A1
      def overjectionTo2[A1 <: SingleDomainError, B1 <: SingleDomainError](mapFirst: A => Either2Error[A1, B1], mapSecond: B => Either2Error[A1, B1], mapThird: C => Either2Error[A1, B1], mapFourth: D => Either2Error[A1, B1], mapFifth: E => Either2Error[A1, B1]): Either2Error[A1, B1]
      def overjectionTo3[A1 <: SingleDomainError, B1 <: SingleDomainError, C1 <: SingleDomainError](mapFirst: A => Either3Error[A1, B1, C1], mapSecond: B => Either3Error[A1, B1, C1], mapThird: C => Either3Error[A1, B1, C1], mapFourth: D => Either3Error[A1, B1, C1], mapFifth: E => Either3Error[A1, B1, C1]): Either3Error[A1, B1, C1]
      def overjectionTo4[A1 <: SingleDomainError, B1 <: SingleDomainError, C1 <: SingleDomainError, D1 <: SingleDomainError](mapFirst: A => Either4Error[A1, B1, C1, D1], mapSecond: B => Either4Error[A1, B1, C1, D1], mapThird: C => Either4Error[A1, B1, C1, D1], mapFourth: D => Either4Error[A1, B1, C1, D1], mapFifth: E => Either4Error[A1, B1, C1, D1]): Either4Error[A1, B1, C1, D1]
      def bijection[A1 <: SingleDomainError, B1 <: SingleDomainError, C1 <: SingleDomainError, D1 <: SingleDomainError, E1 <: SingleDomainError](mapFirst: A => A1, mapSecond: B => B1, mapThird: C => C1, mapFourth: D => D1, mapFifth: E => E1): Either5Error[A1, B1, C1, D1, E1]
    }

    object Either5Error {
      private[precise] final case class one[A <: SingleDomainError, B <: SingleDomainError, C <: SingleDomainError, D <: SingleDomainError, E <: SingleDomainError](value: A) extends Either5Error[A, B, C, D, E] {
        def fold[T](foldFirst: A => T, foldSecond: B => T, foldThird: C => T, foldFourth: D => T, foldFifth: E => T): T = foldFirst(value)
        def overjectionTo1[A1 <: SingleDomainError](mapFirst: A => A1, mapSecond: B => A1, mapThird: C => A1, mapFourth: D => A1, mapFifth: E => A1): A1 = mapFirst(value)
        def overjectionTo2[A1 <: SingleDomainError, B1 <: SingleDomainError](mapFirst: A => Either2Error[A1, B1], mapSecond: B => Either2Error[A1, B1], mapThird: C => Either2Error[A1, B1], mapFourth: D => Either2Error[A1, B1], mapFifth: E => Either2Error[A1, B1]): Either2Error[A1, B1] = mapFirst(value)
        def overjectionTo3[A1 <: SingleDomainError, B1 <: SingleDomainError, C1 <: SingleDomainError](mapFirst: A => Either3Error[A1, B1, C1], mapSecond: B => Either3Error[A1, B1, C1], mapThird: C => Either3Error[A1, B1, C1], mapFourth: D => Either3Error[A1, B1, C1], mapFifth: E => Either3Error[A1, B1, C1]): Either3Error[A1, B1, C1] = mapFirst(value)
        def overjectionTo4[A1 <: SingleDomainError, B1 <: SingleDomainError, C1 <: SingleDomainError, D1 <: SingleDomainError](mapFirst: A => Either4Error[A1, B1, C1, D1], mapSecond: B => Either4Error[A1, B1, C1, D1], mapThird: C => Either4Error[A1, B1, C1, D1], mapFourth: D => Either4Error[A1, B1, C1, D1], mapFifth: E => Either4Error[A1, B1, C1, D1]): Either4Error[A1, B1, C1, D1] = mapFirst(value)
        def bijection[A1 <: SingleDomainError, B1 <: SingleDomainError, C1 <: SingleDomainError, D1 <: SingleDomainError, E1 <: SingleDomainError](mapFirst: A => A1, mapSecond: B => B1, mapThird: C => C1, mapFourth: D => D1, mapFifth: E => E1): Either5Error[A1, B1, C1, D1, E1] = one(mapFirst(value))
      }
      private[precise] final case class two[A <: SingleDomainError, B <: SingleDomainError, C <: SingleDomainError, D <: SingleDomainError, E <: SingleDomainError](value: B) extends Either5Error[A, B, C, D, E] {
        def fold[T](foldFirst: A => T, foldSecond: B => T, foldThird: C => T, foldFourth: D => T, foldFifth: E => T): T = foldSecond(value)
        def overjectionTo1[A1 <: SingleDomainError](mapFirst: A => A1, mapSecond: B => A1, mapThird: C => A1, mapFourth: D => A1, mapFifth: E => A1): A1 = mapSecond(value)
        def overjectionTo2[A1 <: SingleDomainError, B1 <: SingleDomainError](mapFirst: A => Either2Error[A1, B1], mapSecond: B => Either2Error[A1, B1], mapThird: C => Either2Error[A1, B1], mapFourth: D => Either2Error[A1, B1], mapFifth: E => Either2Error[A1, B1]): Either2Error[A1, B1] = mapSecond(value)
        def overjectionTo3[A1 <: SingleDomainError, B1 <: SingleDomainError, C1 <: SingleDomainError](mapFirst: A => Either3Error[A1, B1, C1], mapSecond: B => Either3Error[A1, B1, C1], mapThird: C => Either3Error[A1, B1, C1], mapFourth: D => Either3Error[A1, B1, C1], mapFifth: E => Either3Error[A1, B1, C1]): Either3Error[A1, B1, C1] = mapSecond(value)
        def overjectionTo4[A1 <: SingleDomainError, B1 <: SingleDomainError, C1 <: SingleDomainError, D1 <: SingleDomainError](mapFirst: A => Either4Error[A1, B1, C1, D1], mapSecond: B => Either4Error[A1, B1, C1, D1], mapThird: C => Either4Error[A1, B1, C1, D1], mapFourth: D => Either4Error[A1, B1, C1, D1], mapFifth: E => Either4Error[A1, B1, C1, D1]): Either4Error[A1, B1, C1, D1] = mapSecond(value)
        def bijection[A1 <: SingleDomainError, B1 <: SingleDomainError, C1 <: SingleDomainError, D1 <: SingleDomainError, E1 <: SingleDomainError](mapFirst: A => A1, mapSecond: B => B1, mapThird: C => C1, mapFourth: D => D1, mapFifth: E => E1): Either5Error[A1, B1, C1, D1, E1] = two(mapSecond(value))
      }
      private[precise] final case class three[A <: SingleDomainError, B <: SingleDomainError, C <: SingleDomainError, D <: SingleDomainError, E <: SingleDomainError](value: C) extends Either5Error[A, B, C, D, E] {
        def fold[T](foldFirst: A => T, foldSecond: B => T, foldThird: C => T, foldFourth: D => T, foldFifth: E => T): T = foldThird(value)
        def overjectionTo1[A1 <: SingleDomainError](mapFirst: A => A1, mapSecond: B => A1, mapThird: C => A1, mapFourth: D => A1, mapFifth: E => A1): A1 = mapThird(value)
        def overjectionTo2[A1 <: SingleDomainError, B1 <: SingleDomainError](mapFirst: A => Either2Error[A1, B1], mapSecond: B => Either2Error[A1, B1], mapThird: C => Either2Error[A1, B1], mapFourth: D => Either2Error[A1, B1], mapFifth: E => Either2Error[A1, B1]): Either2Error[A1, B1] = mapThird(value)
        def overjectionTo3[A1 <: SingleDomainError, B1 <: SingleDomainError, C1 <: SingleDomainError](mapFirst: A => Either3Error[A1, B1, C1], mapSecond: B => Either3Error[A1, B1, C1], mapThird: C => Either3Error[A1, B1, C1], mapFourth: D => Either3Error[A1, B1, C1], mapFifth: E => Either3Error[A1, B1, C1]): Either3Error[A1, B1, C1] = mapThird(value)
        def overjectionTo4[A1 <: SingleDomainError, B1 <: SingleDomainError, C1 <: SingleDomainError, D1 <: SingleDomainError](mapFirst: A => Either4Error[A1, B1, C1, D1], mapSecond: B => Either4Error[A1, B1, C1, D1], mapThird: C => Either4Error[A1, B1, C1, D1], mapFourth: D => Either4Error[A1, B1, C1, D1], mapFifth: E => Either4Error[A1, B1, C1, D1]): Either4Error[A1, B1, C1, D1] = mapThird(value)
        def bijection[A1 <: SingleDomainError, B1 <: SingleDomainError, C1 <: SingleDomainError, D1 <: SingleDomainError, E1 <: SingleDomainError](mapFirst: A => A1, mapSecond: B => B1, mapThird: C => C1, mapFourth: D => D1, mapFifth: E => E1): Either5Error[A1, B1, C1, D1, E1] = three(mapThird(value))
      }
      private[precise] final case class four[A <: SingleDomainError, B <: SingleDomainError, C <: SingleDomainError, D <: SingleDomainError, E <: SingleDomainError](value: D) extends Either5Error[A, B, C, D, E] {
        def fold[T](foldFirst: A => T, foldSecond: B => T, foldThird: C => T, foldFourth: D => T, foldFifth: E => T): T = foldFourth(value)
        def overjectionTo1[A1 <: SingleDomainError](mapFirst: A => A1, mapSecond: B => A1, mapThird: C => A1, mapFourth: D => A1, mapFifth: E => A1): A1 = mapFourth(value)
        def overjectionTo2[A1 <: SingleDomainError, B1 <: SingleDomainError](mapFirst: A => Either2Error[A1, B1], mapSecond: B => Either2Error[A1, B1], mapThird: C => Either2Error[A1, B1], mapFourth: D => Either2Error[A1, B1], mapFifth: E => Either2Error[A1, B1]): Either2Error[A1, B1] = mapFourth(value)
        def overjectionTo3[A1 <: SingleDomainError, B1 <: SingleDomainError, C1 <: SingleDomainError](mapFirst: A => Either3Error[A1, B1, C1], mapSecond: B => Either3Error[A1, B1, C1], mapThird: C => Either3Error[A1, B1, C1], mapFourth: D => Either3Error[A1, B1, C1], mapFifth: E => Either3Error[A1, B1, C1]): Either3Error[A1, B1, C1] = mapFourth(value)
        def overjectionTo4[A1 <: SingleDomainError, B1 <: SingleDomainError, C1 <: SingleDomainError, D1 <: SingleDomainError](mapFirst: A => Either4Error[A1, B1, C1, D1], mapSecond: B => Either4Error[A1, B1, C1, D1], mapThird: C => Either4Error[A1, B1, C1, D1], mapFourth: D => Either4Error[A1, B1, C1, D1], mapFifth: E => Either4Error[A1, B1, C1, D1]): Either4Error[A1, B1, C1, D1] = mapFourth(value)
        def bijection[A1 <: SingleDomainError, B1 <: SingleDomainError, C1 <: SingleDomainError, D1 <: SingleDomainError, E1 <: SingleDomainError](mapFirst: A => A1, mapSecond: B => B1, mapThird: C => C1, mapFourth: D => D1, mapFifth: E => E1): Either5Error[A1, B1, C1, D1, E1] = four(mapFourth(value))
      }
      private[precise] final case class five[A <: SingleDomainError, B <: SingleDomainError, C <: SingleDomainError, D <: SingleDomainError, E <: SingleDomainError](value: E) extends Either5Error[A, B, C, D, E] {
        def fold[T](foldFirst: A => T, foldSecond: B => T, foldThird: C => T, foldFourth: D => T, foldFifth: E => T): T = foldFifth(value)
        def overjectionTo1[A1 <: SingleDomainError](mapFirst: A => A1, mapSecond: B => A1, mapThird: C => A1, mapFourth: D => A1, mapFifth: E => A1): A1 = mapFifth(value)
        def overjectionTo2[A1 <: SingleDomainError, B1 <: SingleDomainError](mapFirst: A => Either2Error[A1, B1], mapSecond: B => Either2Error[A1, B1], mapThird: C => Either2Error[A1, B1], mapFourth: D => Either2Error[A1, B1], mapFifth: E => Either2Error[A1, B1]): Either2Error[A1, B1] = mapFifth(value)
        def overjectionTo3[A1 <: SingleDomainError, B1 <: SingleDomainError, C1 <: SingleDomainError](mapFirst: A => Either3Error[A1, B1, C1], mapSecond: B => Either3Error[A1, B1, C1], mapThird: C => Either3Error[A1, B1, C1], mapFourth: D => Either3Error[A1, B1, C1], mapFifth: E => Either3Error[A1, B1, C1]): Either3Error[A1, B1, C1] = mapFifth(value)
        def overjectionTo4[A1 <: SingleDomainError, B1 <: SingleDomainError, C1 <: SingleDomainError, D1 <: SingleDomainError](mapFirst: A => Either4Error[A1, B1, C1, D1], mapSecond: B => Either4Error[A1, B1, C1, D1], mapThird: C => Either4Error[A1, B1, C1, D1], mapFourth: D => Either4Error[A1, B1, C1, D1], mapFifth: E => Either4Error[A1, B1, C1, D1]): Either4Error[A1, B1, C1, D1] = mapFifth(value)
        def bijection[A1 <: SingleDomainError, B1 <: SingleDomainError, C1 <: SingleDomainError, D1 <: SingleDomainError, E1 <: SingleDomainError](mapFirst: A => A1, mapSecond: B => B1, mapThird: C => C1, mapFourth: D => D1, mapFifth: E => E1): Either5Error[A1, B1, C1, D1, E1] = five(mapFifth(value))
      }

      def first[A <: SingleDomainError, B <: SingleDomainError, C <: SingleDomainError, D <: SingleDomainError, E <: SingleDomainError](value: A): Either5Error[A, B, C, D, E] = one[A, B, C, D, E](value)
      def second[A <: SingleDomainError, B <: SingleDomainError, C <: SingleDomainError, D <: SingleDomainError, E <: SingleDomainError](value: B): Either5Error[A, B, C, D, E] = two[A, B, C, D, E](value)
      def third[A <: SingleDomainError, B <: SingleDomainError, C <: SingleDomainError, D <: SingleDomainError, E <: SingleDomainError](value: C): Either5Error[A, B, C, D, E] = three[A, B, C, D, E](value)
      def fourth[A <: SingleDomainError, B <: SingleDomainError, C <: SingleDomainError, D <: SingleDomainError, E <: SingleDomainError](value: D): Either5Error[A, B, C, D, E] = four[A, B, C, D, E](value)
      def fifth[A <: SingleDomainError, B <: SingleDomainError, C <: SingleDomainError, D <: SingleDomainError, E <: SingleDomainError](value: E): Either5Error[A, B, C, D, E] = five[A, B, C, D, E](value)
    }
  }

  // -----------------------------------------------------------------------------
  // CORE ERROR ABSTRACTION
  // Any exception inheriting from Throwable is a fault.
  // An 'Expected' is a wrapper around a normal domain error which is likely associated with an alternative business scenario.
  // A 'FaultOr' is a data type to represent a fault or a damain error. It's the most commonly used type.
  // -----------------------------------------------------------------------------

  /**
   * The ADT to define an effect error which can represent a domain error or a system fault
   */
  sealed trait EffectError

  // only expected (business only) error
  /**
   * 'Expected' represents an expected domain error
   * @param cause a domain error
   * @tparam E the type of the domain error (it could be a type bound domain error or an EitherNError instance)
   */
  final case class Expected[E](cause: E) extends EffectError {
    /**
     * Take this expected error and lift it to a FaultOr type
     * @return a FaultOr type with no fault
     */
    def liftToFaultOr: FaultOr[Nothing, E] = FaultOr[Nothing, E](Right(cause).asInstanceOf[Either[Nothing, E]])

    override def toString = s"EXPECTED ERROR\n${cause.toString}"
  }

  // FAULT is explicitly covariant as I want it to keep the minimum common type and not the type bound
  /**
   * 'FaultOr' represents either a system fault (an exception) or an expected domain error
   * It will be the most common error type in any non trivial application
   * @param cause either a system fault or an expected domain error
   * @param ev1 the system fault must be a subtype of Throwable
   * @tparam FAULT a system fault, any exception inheriting from Throwable
   * @tparam E the type of the domain error (could be a type bound domain error or an EitherNError instance)
   */
  final case class FaultOr[+FAULT, E](cause: Either[FAULT, E])(implicit ev1: FAULT <:< Throwable) extends EffectError {
    /**
     * Convert any EitherNError domain error to a 'flat' type bound domain error T, fault is unchanged
     * @tparam T type of the the type bound based domain error
     * @return a FaultOr with a domain error based on a type bound
     */
    private[errors] def flatten[T <: SingleDomainError]: Either[FAULT, T] = cause map {
      case either.Either2Error.one(v) => v.asInstanceOf[T]
      case either.Either2Error.two(v) => v.asInstanceOf[T]

      case either.Either3Error.one(v) => v.asInstanceOf[T]
      case either.Either3Error.two(v) => v.asInstanceOf[T]
      case either.Either3Error.three(v) => v.asInstanceOf[T]

      case either.Either4Error.one(v) => v.asInstanceOf[T]
      case either.Either4Error.two(v) => v.asInstanceOf[T]
      case either.Either4Error.three(v) => v.asInstanceOf[T]
      case either.Either4Error.four(v) => v.asInstanceOf[T]

      case either.Either5Error.one(v) => v.asInstanceOf[T]
      case either.Either5Error.two(v) => v.asInstanceOf[T]
      case either.Either5Error.three(v) => v.asInstanceOf[T]
      case either.Either5Error.four(v) => v.asInstanceOf[T]
      case either.Either5Error.five(v) => v.asInstanceOf[T]

      case single: SingleDomainError => single.asInstanceOf[T]
    }

    override def toString: String = cause match {
      case Left(throwable) =>
        val sw = new java.io.StringWriter()
        val pw = new java.io.PrintWriter(sw)
        throwable.printStackTrace(pw)
        s"EITHER EXPECTED OR UNEXPECTED ERROR : UNEXPECTED\n${sw.toString}"
      case Right(expectedCause) => s"EITHER EXPECTED OR UNEXPECTED ERROR : EXPECTED\n$expectedCause"
    }

    /**
     * To map a system fault (an exception) to another system fault (still an exception)
     * @param f the mapping function
     * @param ev1 the target fault must be a subtype of Throwable
     * @tparam FAULT1 the target system fault type
     * @return a FaultOr instance with the new system fault. Domain errors are unchanged
     */
    def mapFault[FAULT1](f: FAULT => FAULT1)(implicit ev1: FAULT1 <:< Throwable): FaultOr[FAULT1, E] = FaultOr(cause.left.map(f))
  }

  type ThrowableOr[E] = FaultOr[Throwable, E]

  /**
   * Some FaultOr utility functions
   */
  object FaultOr {
    import either._

    /**
     * Make a FaultOr from a domain error
     * @param expectedError the expected domain error to make a FaultOr from
     * @param ev1 fault must inherit from Throwable
     * @tparam FAULT the fault type used in FaultOr
     * @tparam E the domain error type
     * @return a FaultOr containing a domain error
     */
    def makeError[FAULT, E <: SingleDomainError](expectedError: E)(implicit ev1: FAULT <:< Throwable): FaultOr[FAULT, E] = FaultOr(Right(expectedError).asInstanceOf[Either[FAULT, E]])

    /**
     * Make a FaultOr from a set of 2 exclusive domain errors
     * @param expectedError the expected domain errors (Either2Error) to make a FaultOr from
     * @param ev1 fault must inherit from Throwable
     * @tparam FAULT the fault type used in FaultOr
     * @tparam E1 the first possible domain error
     * @tparam E2 the second possible domain error
     * @return a FaultOr containing an Either2Error domain error
     */
    def makeEither2Error[FAULT, E1 <: SingleDomainError, E2 <: SingleDomainError](expectedError: Either2Error[E1, E2])(implicit ev1: FAULT <:< Throwable): FaultOr[FAULT, Either2Error[E1, E2]] = FaultOr(Right(expectedError).asInstanceOf[Either[FAULT, Either2Error[E1, E2]]])

    /**
     * Make a FaultOr from a set of 3 exclusive domain errors
     * @param expectedError the expected domain errors (Either3Error) to make a FaultOr from
     * @param ev1 fault must inherit from Throwable
     * @tparam FAULT the fault type used in FaultOr
     * @tparam E1 the first possible domain error
     * @tparam E2 the second possible domain error
     * @tparam E3 the third possible domain error
     * @return a FaultOr containing an Either3Error domain error
     */
    def makeEither3Error[FAULT, E1 <: SingleDomainError, E2 <: SingleDomainError, E3 <: SingleDomainError](expectedError: Either3Error[E1, E2, E3])(implicit ev1: FAULT <:< Throwable): FaultOr[FAULT, Either3Error[E1, E2, E3]] = FaultOr(Right(expectedError).asInstanceOf[Either[FAULT, Either3Error[E1, E2, E3]]])

    /**
     * Make a FaultOr from a set of 4 exclusive domain errors
     * @param expectedError the expected domain errors (Either4Error) to make a FaultOr from
     * @param ev1 fault must inherit from Throwable
     * @tparam FAULT the fault type used in FaultOr
     * @tparam E1 the first possible domain error
     * @tparam E2 the second possible domain error
     * @tparam E3 the third possible domain error
     * @tparam E4 the fourth possible domain error
     * @return a FaultOr containing an Either4Error domain error
     */
    def makeEither4Error[FAULT, E1 <: SingleDomainError, E2 <: SingleDomainError, E3 <: SingleDomainError, E4 <: SingleDomainError](expectedError: Either4Error[E1, E2, E3, E4])(implicit ev1: FAULT <:< Throwable): FaultOr[FAULT, Either4Error[E1, E2, E3, E4]] = FaultOr(Right(expectedError).asInstanceOf[Either[FAULT, Either4Error[E1, E2, E3, E4]]])

    /**
     * Make a FaultOr from a set of 5 exclusive domain errors
     * @param expectedError the expected domain errors (Either5Error) to make a FaultOr from
     * @param ev1 fault must inherit from Throwable
     * @tparam FAULT the fault type used in FaultOr
     * @tparam E1 the first possible domain error
     * @tparam E2 the second possible domain error
     * @tparam E3 the third possible domain error
     * @tparam E4 the fourth possible domain error
     * @tparam E5 the fifth possible domain error
     * @return a FaultOr containing an Either5Error domain error
     */
    def makeEither5Error[FAULT, E1 <: SingleDomainError, E2 <: SingleDomainError, E3 <: SingleDomainError, E4 <: SingleDomainError, E5 <: SingleDomainError](expectedError: Either5Error[E1, E2, E3, E4, E5])(implicit ev1: FAULT <:< Throwable): FaultOr[FAULT, Either5Error[E1, E2, E3, E4, E5]] = FaultOr(Right(expectedError).asInstanceOf[Either[FAULT, Either5Error[E1, E2, E3, E4, E5]]])

    /**
     * Make a FaultOr from a system fault
     * @param fault the system fault to make a FaultOr from
     * @param ev1 fault must inherit from Throwable
     * @tparam FAULT the fault type used in FaultOr
     * @tparam T the dommain error type used in FaultOr
     * @return a FaultOr containing a system fault
     */
    def makeFault[FAULT, T](fault: FAULT)(implicit ev1: FAULT <:< Throwable): FaultOr[FAULT, T] = FaultOr(Left(fault).asInstanceOf[Either[FAULT, T]])
  }

  // -----------------------------------------------------------------------------

  /**
   * Outcome represent a result which can be a value or a domain error. Mostly used in Task.
   * @tparam A the type of the result
   * @tparam E the type of the domain error
   */
  sealed trait Outcome[E <: DomainError, A]

  // -----------------------------------------------------------------------------
  // OUTCOME IS A WAY TO EXPRESS THAT A [ZIO TASK] CAN RESULT IN A VALUE OR ONE OR MANY DOMAIN ERRORS (USING EitherNError types)
  // BOTH ARE NORMAL OUTCOME, SO THE TASK ERROR CHANNEL IS NOT APPROPRIATE FOR DOMAIN ERRORS
  // -----------------------------------------------------------------------------

  object Outcome {
    /**
     * To return an outcome which is a result
     * @param a the result
     * @tparam A the outcome result type
     * @tparam E the outcome domain error type
     * @return an Outcome instance
     */
    def giveResult[E <: DomainError, A](a: A): Outcome[E, A] = Result[E, A](a)

    /**
     * To return an outcome which is a domain error
     * @param e the domain error (can be a single error or one of the EitherNError)
     * @tparam A the outcome result type
     * @tparam E the outcome domain error type
     * @return
     */
    def giveDomainError[E <: DomainError, A](e: E): Outcome[E, A] = ExpectedError[E, A](e)
  }

  // Concrete Outcome subtypes are not meant to be used directly

  private final case class Result[E <: DomainError, A](value: A) extends Outcome[E, A]
  private final case class ExpectedError[E <: DomainError, A](error: E) extends Outcome[E, A]

  // -----------------------------------------------------------------------------
  // ADD USEFUL ERROR OPERATIONS TO THE DIFFERENT ZIO ONE CAN GET WHEN EXPRESSING THE PROGRAM
  // -----------------------------------------------------------------------------

  /**
   * Add some extension methods on FaultOr and ZIO classes
   */
  object syntax {
    import either._

    /**
     * Fold a FaultOr instance based on a type bound domain error into a value
     * @param faultOr the FaultOr instance to fold
     * @param ev1 fault must be a subtype of Throwable
     * @tparam FAULT the fault type
     * @tparam E the type bound domain error
     */
    implicit final class FaultOrLinearSyntax[FAULT, E <: SingleDomainError](faultOr : FaultOr[FAULT, _ >: E])(implicit ev1: FAULT <:< Throwable) {
      def fold[U](onFault: FAULT => U, onErrors: Expected[_ >: E] => U): U = faultOr.cause
                                                                                    .map(error => Expected(error))
                                                                                    .fold(throwable => onFault(throwable),
                                                                                          error => onErrors(error))
    }

    /**
     * Fold a FaultOr instance based on a Either2Error domain error into a value
     * @param faultOr the FaultOr instance to fold
     * @param ev1 fault must be a subtype of Throwable
     * @tparam FAULT the fault type
     * @tparam E1 the Either2Error first domain error type
     * @tparam E2 the Either2Error second domain error type
     */
    implicit final class FaultOrEither2Syntax[FAULT, E1 <: SingleDomainError, E2 <: SingleDomainError](faultOr : FaultOr[FAULT, Either2Error[E1, E2]])(implicit ev1: FAULT <:< Throwable) {
      def fold[U](onFault: FAULT => U, onErrors: Either2Error[E1, E2] => U): U = faultOr.cause
                                                                                        .fold(throwable => onFault(throwable),
                                                                                              error => onErrors(error))
    }

    /**
     * Fold a FaultOr instance based on a Either3Error domain error into a value
     * @param faultOr the FaultOr instance to fold
     * @param ev1 fault must be a subtype of Throwable
     * @tparam FAULT the fault type
     * @tparam E1 the Either3Error first domain error type
     * @tparam E2 the Either3Error second domain error type
     * @tparam E3 the Either3Error third domain error type
     */
    implicit final class FaultOrEither3Syntax[FAULT, E1 <: SingleDomainError, E2 <: SingleDomainError, E3 <: SingleDomainError](faultOr : FaultOr[FAULT, Either3Error[E1, E2, E3]])(implicit ev1: FAULT <:< Throwable) {
      def fold[U](onFault: FAULT => U, onErrors: Either3Error[E1, E2, E3] => U): U = faultOr.cause
                                                                                            .fold(throwable => onFault(throwable),
                                                                                                  error => onErrors(error))
    }

    /**
     * Fold a FaultOr instance based on a Either4Error domain error into a value
     * @param faultOr the FaultOr instance to fold
     * @param ev1 fault must be a subtype of Throwable
     * @tparam FAULT the fault type
     * @tparam E1 the Either4Error first domain error type
     * @tparam E2 the Either4Error second domain error type
     * @tparam E3 the Either4Error third domain error type
     * @tparam E4 the Either4Error fourth domain error type
     */
    implicit final class FaultOrEither4Syntax[FAULT, E1 <: SingleDomainError, E2 <: SingleDomainError, E3 <: SingleDomainError, E4 <: SingleDomainError](faultOr : FaultOr[FAULT, Either4Error[E1, E2, E3, E4]])(implicit ev1: FAULT <:< Throwable) {
      def fold[U](onFault: FAULT => U, onErrors: Either4Error[E1, E2, E3, E4] => U): U = faultOr.cause
                                                                                                .fold(throwable => onFault(throwable),
                                                                                                      error => onErrors(error))
    }

    /**
     * Fold a FaultOr instance based on a Either5Error domain error into a value
     * @param faultOr the FaultOr instance to fold
     * @param ev1 fault must be a subtype of Throwable
     * @tparam FAULT the fault type
     * @tparam E1 the Either5Error first domain error type
     * @tparam E2 the Either5Error second domain error type
     * @tparam E3 the Either5Error third domain error type
     * @tparam E4 the Either5Error fourth domain error type
     * @tparam E5 the Either5Error fifth domain error type
     */
    implicit final class FaultOrEither5Syntax[FAULT, E1 <: SingleDomainError, E2 <: SingleDomainError, E3 <: SingleDomainError, E4 <: SingleDomainError, E5 <: SingleDomainError](faultOr : FaultOr[FAULT, Either5Error[E1, E2, E3, E4, E5]])(implicit ev1: FAULT <:< Throwable) {
      def fold[U](onFault: FAULT => U, onErrors: Either5Error[E1, E2, E3, E4, E5] => U): U = faultOr.cause
                                                                                                    .fold(throwable => onFault(throwable),
                                                                                                          error => onErrors(error))
    }

    // -------------------------------------

    /**
     * Convert a ZIO effect based on Throwable error into a ZIO effect based on FaultOr error
     * @param effect ZIO effect based on Throwable
     * @param ev1 fault must be a subtype of Throwable
     * @tparam FAULT the system fault type
     * @tparam R effect environmental type
     * @tparam A effect result type
     */
    implicit final class ZioThrowableSyntax[R, FAULT, A](effect: zio.ZIO[R, FAULT, A])(implicit ev1: FAULT <:< Throwable) {
      def toFaultOr: ZIO[R,FaultOr[FAULT, Any],A] =
        effect.mapError(FaultOr.makeFault[FAULT, Any])

      def toFaultOr1[E1 <: SingleDomainError]: ZIO[R,FaultOr[FAULT, E1],A] =
        effect.mapError(FaultOr.makeFault[FAULT, E1])
      def toFaultOr2[E1 <: SingleDomainError, E2 <: SingleDomainError]: ZIO[R,FaultOr[FAULT, Either2Error[E1, E2]],A] =
        effect.mapError(FaultOr.makeFault[FAULT, Either2Error[E1, E2]])
      def toFaultOr3[E1 <: SingleDomainError, E2 <: SingleDomainError, E3 <: SingleDomainError]: ZIO[R,FaultOr[FAULT, Either3Error[E1, E2, E3]],A] =
        effect.mapError(FaultOr.makeFault[FAULT, Either3Error[E1, E2, E3]])
      def toFaultOr4[E1 <: SingleDomainError, E2 <: SingleDomainError, E3 <: SingleDomainError, E4 <: SingleDomainError]: ZIO[R,FaultOr[FAULT, Either4Error[E1, E2, E3, E4]],A] =
        effect.mapError(FaultOr.makeFault[FAULT, Either4Error[E1, E2, E3, E4]])
      def toFaultOr5[E1 <: SingleDomainError, E2 <: SingleDomainError, E3 <: SingleDomainError, E4 <: SingleDomainError, E5 <: SingleDomainError]: ZIO[R,FaultOr[FAULT, Either5Error[E1, E2, E3, E4, E5]],A] =
        effect.mapError(FaultOr.makeFault[FAULT, Either5Error[E1, E2, E3, E4, E5]])
    }

    // -------------------------------------

    /**
     * Convert a ZIO effect based on ZIO.fromOption into a ZIO effect based on Outcome
     * @param effect ZIO effect resulting from ZIO.fromOption
     * @tparam R effect environmental type
     * @tparam E effect domain error type
     * @tparam A effect result type
     */
    implicit final class ZioFromOptionSyntax[R, E <: SingleDomainError, A](effect: zio.ZIO[R, Option[Nothing], A]) {
      def toOutcome(domainError: E): ZIO[R,Nothing,Outcome[E,A]] = effect.foldM(_ => zio.ZIO.succeed(Outcome.giveDomainError[E, A](domainError)),
                                                                                a => zio.ZIO.succeed(Outcome.giveResult[E, A](a)))
    }

    /**
     * Convert a ZIO effect, which can fail with a system fault, based on an Outcome value into a ZIO effect based on FaultOr with domain error using type bound
     * @param effect ZIO effect, which can fail with a system fault, based on an Outcome value
     * @param ev1 fault must be a subtype of Throwable
     * @tparam R effect environmental type
     * @tparam A effect result type
     * @tparam FAULT FaultOr fault type
     * @tparam E FaultOr domain error type based on type bound
     */
    implicit final class ZioOutcomeSyntaxWithFault[R, FAULT, E <: SingleDomainError, A](effect: zio.ZIO[R, FAULT, Outcome[E, A]])(implicit ev1: FAULT <:< Throwable) {
      def toFaultOr: ZIO[R,FaultOr[FAULT, E], A] = effect.mapError(FaultOr.makeFault[FAULT, Any])
                                                         .flatMap {
                                                           case ExpectedError(expected) => ZIO.fail(FaultOr.makeError[FAULT, E](expected))
                                                           case Result(value) => UIO(value)
                                                         }.asInstanceOf[ZIO[R,FaultOr[FAULT, E], A]]
    }

    /**
     * Convert a ZIO effect, which can never fail with a system fault, based on an Outcome value into a ZIO effect based on FaultOr with domain error using type bound
     * @param effect ZIO effect, which can never fail with a system fault, based on an Outcome value
     * @tparam R effect environmental type
     * @tparam E domain error type
     * @tparam A effect result type
     */
    implicit final class ZioOutcomeSyntaxWithoutFault[R, E <: SingleDomainError, A](effect: zio.ZIO[R, Nothing, Outcome[E, A]]) {
      def toFaultOr: ZIO[R,FaultOr[Nothing, E], A] = effect.flatMap {
        case ExpectedError(expected) => ZIO.fail(FaultOr.makeError[Nothing, E](expected))
        case Result(value) => UIO(value)
      }
    }

    /**
     * Convert a ZIO effect based on an Outcome value into a ZIO effect based on FaultOr error with domain error using Either2Error
     * @param effect ZIO effect based on an Outcome value
     * @param ev1 fault must be a subtype of Throwable
     * @tparam R effect environmental type
     * @tparam A effect result type
     * @tparam FAULT effect fault type
     * @tparam E1 the Either2Error first domain error type
     * @tparam E2 the Either2Error second domain error type
     */
    implicit final class ZioOutcomeEither2Syntax[R, FAULT, E1 <: SingleDomainError, E2 <: SingleDomainError, A](effect: zio.ZIO[R, FAULT, Outcome[Either2Error[E1, E2],A]])(implicit ev1: FAULT <:< Throwable) {
      def toFaultOr: ZIO[R,FaultOr[FAULT, Either2Error[E1, E2]],A] = effect.mapError(FaultOr.makeFault[FAULT, Any])
                                                                           .flatMap {
                                                                             case ExpectedError(expected) => ZIO.fail(FaultOr.makeEither2Error(expected))
                                                                             case Result(value) => UIO(value)
                                                                           }
                                                                           .asInstanceOf[ZIO[R,FaultOr[FAULT, Either2Error[E1, E2]],A]]
    }

    /**
     * Convert a ZIO effect based on an Outcome value into a ZIO effect based on FaultOr error with domain error using Either3Error
     * @param effect ZIO effect based on an Outcome value
     * @param ev1 fault must be a subtype of Throwable
     * @tparam R effect environmental type
     * @tparam A effect result type
     * @tparam FAULT effect fault type
     * @tparam E1 the Either3Error first domain error type
     * @tparam E2 the Either3Error second domain error type
     * @tparam E3 the Either3Error third domain error type
     */
    implicit final class ZioOutcomeEither3Syntax[R, FAULT, E1 <: SingleDomainError, E2 <: SingleDomainError, E3 <: SingleDomainError, A](effect: zio.ZIO[R, FAULT, Outcome[Either3Error[E1, E2, E3], A]])(implicit ev1: FAULT <:< Throwable) {
      def toFaultOr: ZIO[R,FaultOr[FAULT, Either3Error[E1, E2, E3]],A] = effect.mapError(FaultOr.makeFault[FAULT, Any])
                                                                               .flatMap {
                                                                                 case ExpectedError(expected) => ZIO.fail(FaultOr.makeEither3Error(expected))
                                                                                 case Result(value) => UIO(value)
                                                                               }
                                                                               .asInstanceOf[ZIO[R,FaultOr[FAULT, Either3Error[E1, E2, E3]],A]]
    }

    /**
     * Convert a ZIO effect based on an Outcome value into a ZIO effect based on FaultOr error with domain error using Either4Error
     * @param effect ZIO effect based on an Outcome value
     * @param ev1 fault must be a subtype of Throwable
     * @tparam R effect environmental type
     * @tparam A effect result type
     * @tparam FAULT effect fault type
     * @tparam E1 the Either4Error first domain error type
     * @tparam E2 the Either4Error second domain error type
     * @tparam E3 the Either4Error third domain error type
     * @tparam E4 the Either4Error fourth domain error type
     */
    implicit final class ZioOutcomeEither4Syntax[R, FAULT, E1 <: SingleDomainError, E2 <: SingleDomainError, E3 <: SingleDomainError, E4 <: SingleDomainError, A](effect: zio.ZIO[R, FAULT, Outcome[Either4Error[E1, E2, E3, E4], A]])(implicit ev1: FAULT <:< Throwable) {
      def toFaultOr: ZIO[R,FaultOr[FAULT, Either4Error[E1, E2, E3, E4]],A] = effect.mapError(FaultOr.makeFault[FAULT, Any])
                                                                                   .flatMap {
                                                                                     case ExpectedError(expected) => ZIO.fail(FaultOr.makeEither4Error(expected))
                                                                                     case Result(value) => UIO(value)
                                                                                   }
                                                                                   .asInstanceOf[ZIO[R,FaultOr[FAULT, Either4Error[E1, E2, E3, E4]],A]]
    }

    /**
     * Convert a ZIO effect based on an Outcome value into a ZIO effect based on FaultOr error with domain error using Either5Error
     * @param effect ZIO effect based on an Outcome value
     * @param ev1 fault must be a subtype of Throwable
     * @tparam R effect environmental type
     * @tparam A effect result type
     * @tparam FAULT effect fault type
     * @tparam E1 Either5Error first domain error type
     * @tparam E2 Either5Error second domain error type
     * @tparam E3 Either5Error third domain error type
     * @tparam E4 Either5Error fourth domain error type
     * @tparam E5 Either5Error fifth domain error type
     */
    implicit final class ZioOutcomeEither5Syntax[R, FAULT, E1 <: SingleDomainError, E2 <: SingleDomainError, E3 <: SingleDomainError, E4 <: SingleDomainError, E5 <: SingleDomainError, A](effect: zio.ZIO[R, FAULT, Outcome[Either5Error[E1, E2, E3, E4, E5], A]])(implicit ev1: FAULT <:< Throwable) {
      def toFaultOr: ZIO[R,FaultOr[FAULT, Either5Error[E1, E2, E3, E4, E5]],A] = effect.mapError(FaultOr.makeFault[FAULT, Any])
                                                                                       .flatMap {
                                                                                         case ExpectedError(expected) => ZIO.fail(FaultOr.makeEither5Error(expected))
                                                                                         case Result(value) => UIO(value)
                                                                                       }
                                                                                       .asInstanceOf[ZIO[R,FaultOr[FAULT, Either5Error[E1, E2, E3, E4, E5]],A]]
    }

    // -------------------------------------

    /**
     * Syntax extension for ZIO effect based on FaultOr where fault has Throwable type
     * @param effect the ZIO effect based on FaultOr with type bound domain errors
     * @tparam R effect environmental type
     * @tparam E FaultOr domain error type (using type bound)
     * @tparam A effect result type
     */
    implicit final class ZioFaultOrThrowableSyntax[R, E <: SingleDomainError, A](effect : ZIO[R, FaultOr[Throwable, _ >: E], A]) {
      /**
       * Map the fault part of a ZIO effect based on FaultOr
       * @param f fault mapping function
       * @param ev1 fault must be a subtype of Throwable
       * @tparam FAULT1 target fault type
       * @return a ZIO effect based on FaultOr with the mapped fault
       */
      def mapFault[FAULT1](f: Throwable => FAULT1)(implicit ev1: FAULT1 <:< Throwable): ZIO[R,FaultOr[FAULT1, _ >: E],A] = effect.mapError(faultOr => faultOr.mapFault(f))
    }

    /**
     * Syntax extension for ZIO effect based on FaultOr where domain error is based on composable type bound
     * @param effect the ZIO effect based on FaultOr with type bound domain errors
     * @param ev1 fault must inherit from Throwable
     * @tparam R effect environmental type
     * @tparam FAULT FaultOr fault type
     * @tparam E FaultOr domain error based on type bound
     * @tparam A effect result type
     *
     * mapDomainErrorsToN (1..5) are used to map type bound based domain errors to 1..5 new domain errors
     *
     * ignoreDomainErrors will ignore all domain errors only keeping fault
     *
     * catchDomainErrors is used to catch domain error and use it to trigger another ZIO effect
     *
     * onDomainErrors will trigger another ZIO effect which will produce a type bound domain error on any existing domain errors
     *
     * onDomainErrorsMakeN (2..5) will trigger another ZIO effect which will produce an EitherNError domain error on any existing domain errors
     */
    implicit final class ZioFaultOrTypeBoundSyntax[R, FAULT, E <: SingleDomainError, A](effect: ZIO[R, FaultOr[FAULT, _ >: E], A])(implicit ev1: FAULT <:< Throwable) {
      def mapDomainErrorsTo1[E1 <: SingleDomainError](f: Expected[_ >: E] => E1): ZIO[R, FaultOr[FAULT, _ >: E1], A] =
        effect.mapError(faultOr => FaultOr(faultOr.cause.map(error => f(Expected(error)))).asInstanceOf[FaultOr[FAULT, _ >: E1]])
      def mapDomainErrorsTo2[E1 <: SingleDomainError, E2 <: SingleDomainError](f: Expected[_ >: E] => Either2Error[E1, E2]): ZIO[R, FaultOr[FAULT, _ >: E1 with E2], A] =
        effect.mapError(faultOr => FaultOr(faultOr.cause.map(error => Expected(error)).map(expected => f(expected)))).composableDomainError
      def mapDomainErrorsTo3[E1 <: SingleDomainError, E2 <: SingleDomainError, E3 <: SingleDomainError](f: Expected[_ >: E] => Either3Error[E1, E2, E3]): ZIO[R, FaultOr[FAULT, _ >: E1 with E2 with E3], A] =
        effect.mapError(faultOr => FaultOr(faultOr.cause.map(error => Expected(error)).map(expected => f(expected)))).composableDomainError
      def mapDomainErrorsTo4[E1 <: SingleDomainError, E2 <: SingleDomainError, E3 <: SingleDomainError, E4 <: SingleDomainError](f: Expected[_ >: E] => Either4Error[E1, E2, E3, E4]): ZIO[R, FaultOr[FAULT, _ >: E1 with E2 with E3 with E4], A] =
        effect.mapError(faultOr => FaultOr(faultOr.cause.map(error => Expected(error)).map(expected => f(expected)))).composableDomainError
      def mapDomainErrorsTo5[E1 <: SingleDomainError, E2 <: SingleDomainError, E3 <: SingleDomainError, E4 <: SingleDomainError, E5 <: SingleDomainError](f: Expected[_ >: E] => Either5Error[E1, E2, E3, E4, E5]): ZIO[R, FaultOr[FAULT, _ >: E1 with E2 with E3 with E4 with E5], A] =
        effect.mapError(faultOr => FaultOr(faultOr.cause.map(error => Expected(error)).map(expected => f(expected)))).composableDomainError

      def ignoreDomainErrors[B >: A](f: Expected[_ >: E] => ZIO[R, FAULT, B]): ZIO[R, FAULT, B] =
        effect.catchAll((faultOr: FaultOr[FAULT, _ >: E]) =>
                          faultOr.cause
                                 .map(error => Expected(error))
                                 .fold(throwable => ZIO.fail(throwable),
                                       error => f(error))
                        )

      // don't know how to force scala to infer the proper LUB between FAULT and FAULT1, so I forced Throwable
      // HELP WELCOME
      def catchDomainErrors[FAULT1 <: Throwable, B >: A, E1 <: SingleDomainError](f: Expected[_ >: E] => ZIO[R, FaultOr[FAULT1, _ >: E1], B]): ZIO[R,FaultOr[Throwable, _ >: E1],B] =
        effect.catchAll((faultOr: FaultOr[FAULT, _ >: E]) =>
                          faultOr.cause
                                 .map(error => Expected(error))
                                 .fold(throwable => ZIO.fail(FaultOr.makeFault(throwable)),
                                       error => f(error))
                        )

      // don't know how to force scala to infer the proper LUB between FAULT and FAULT1, so I forced Throwable
      // HELP WELCOME
      def onDomainErrors[FAULT1 <: Throwable, E1 <: SingleDomainError](alternative: ZIO[R, FaultOr[FAULT1, _ >: E1], A]): ZIO[R, FaultOr[Throwable, _ >: E1], A] =
        effect.catchAll((f: FaultOr[FAULT, _ >: E]) =>
                          if (f.cause.isLeft) effect.asInstanceOf[zio.ZIO[R, FaultOr[Throwable, _ >: E1], A]]
                          else alternative
                        )

      def onDomainErrorsMake2[FAULT1 <: Throwable, E1 <: SingleDomainError, E2 <: SingleDomainError](alternative: ZIO[R, FaultOr[FAULT1, Either2Error[E1, E2]], A]): ZIO[R, FaultOr[Throwable, Either2Error[E1, E2]], A] =
        effect.catchAll((f: FaultOr[FAULT, _ >: E]) =>
                          if (f.cause.isLeft) effect.asInstanceOf[zio.ZIO[R, FaultOr[Throwable, Either2Error[E1, E2]], A]]
                          else alternative
                        )

      def onDomainErrorsMake3[FAULT1 <: Throwable, E1 <: SingleDomainError, E2 <: SingleDomainError, E3 <: SingleDomainError](alternative: ZIO[R, FaultOr[FAULT1, Either3Error[E1, E2, E3]], A]): ZIO[R, FaultOr[Throwable, Either3Error[E1, E2, E3]], A] =
        effect.catchAll((f: FaultOr[FAULT, _ >: E]) =>
                          if (f.cause.isLeft) effect.asInstanceOf[zio.ZIO[R, FaultOr[Throwable, Either3Error[E1, E2, E3]], A]]
                          else alternative
                        )

      def onDomainErrorsMake4[FAULT1 <: Throwable, E1 <: SingleDomainError, E2 <: SingleDomainError, E3 <: SingleDomainError, E4 <: SingleDomainError](alternative: ZIO[R, FaultOr[FAULT1, Either4Error[E1, E2, E3, E4]], A]): ZIO[R, FaultOr[Throwable, Either4Error[E1, E2, E3, E4]], A] =
        effect.catchAll((f: FaultOr[FAULT, _ >: E]) =>
                          if (f.cause.isLeft) effect.asInstanceOf[zio.ZIO[R, FaultOr[Throwable, Either4Error[E1, E2, E3, E4]], A]]
                          else alternative
                        )

      def onDomainErrorsMake5[FAULT1 <: Throwable, E1 <: SingleDomainError, E2 <: SingleDomainError, E3 <: SingleDomainError, E4 <: SingleDomainError, E5 <: SingleDomainError](alternative: ZIO[R, FaultOr[FAULT1, Either5Error[E1, E2, E3, E4, E5]], A]): ZIO[R, FaultOr[Throwable, Either5Error[E1, E2, E3, E4, E5]], A] =
        effect.catchAll((f: FaultOr[FAULT, _ >: E]) =>
                          if (f.cause.isLeft) effect.asInstanceOf[zio.ZIO[R, FaultOr[Throwable, Either5Error[E1, E2, E3, E4, E5]], A]]
                          else alternative
                        )
    }

    // -------------------------------------

    /**
     * Convert an single domain error effect which can never failed with a system fault into the composable (domain error based on type bound) effect version
     * @param effect the ZIO effect based on FaultOr
     * @tparam R effect environmental type
     * @tparam E FaultOr domain error type
     * @tparam A effect result type
     */
    implicit final class ZioFaultOrEmptyResult[R, E <: SingleDomainError, A](effect: zio.ZIO[R, FaultOr[Nothing, E], A]) {
      def composableDomainError: ZIO[R, FaultOr[Nothing, _ >: E], A] =
        effect.mapError(faultOr => FaultOr(faultOr.flatten)).asInstanceOf[zio.ZIO[R, FaultOr[Nothing, _ >: E], A]]
    }

    /**
     * Syntax extensions for ZIO effect based on FaultOr where domain error is based on a single domain error
     * @param effect the ZIO effect based on FaultOr
     * @param ev1 system fault must inherit from Throwable
     * @tparam R effect environmental type
     * @tparam FAULT FaultOr system fault type
     * @tparam E1 FaultOr domain error type
     * @tparam A effect result type
     *
     * composableDomainError convert the effect into the composable domain error version (based on type bound)
     *
     * mapDomainErrorsTo1 is used to map a single domain error to a new domain errors
     */
    implicit final class ZioFaultOrSingleErrorSyntax[R, FAULT, E1 <: SingleDomainError, A](effect: zio.ZIO[R, FaultOr[FAULT, E1], A])(implicit ev1: FAULT <:< Throwable) {
      /**
       * Convert an single domain error effect into the composable (domain error based on type bound) effect version
       * @return
       */
      def composableDomainError: ZIO[R, FaultOr[FAULT, _ >: E1], A] =
        effect.mapError(faultOr => FaultOr(faultOr.flatten)).asInstanceOf[zio.ZIO[R, FaultOr[FAULT, _ >: E1], A]]

      def mapDomainErrorsTo1[TE1 <: SingleDomainError](mapSingle: E1 => TE1): ZIO[R, FaultOr[FAULT, TE1], A] =
        effect.mapError(faultError => FaultOr(faultError.cause.map(single => mapSingle(single))))
    }

    /**
     * Syntax extensions for ZIO effect based on FaultOr where domain error is based on Either2Error
     * @param effect the ZIO effect based on FaultOr
     * @param ev1 system fault must inherit from Throwable
     * @tparam R effect environmental type
     * @tparam FAULT FaultOr system fault type
     * @tparam E1 Either2Error first domain error type
     * @tparam E2 Either2Error second domain error type
     * @tparam A effect result type
     *
     * composableDomainError convert the effect into the composable domain error version (based on type bound)
     *
     * mapDomainErrorsToN is used to map domain error to new domain errors
     *
     * ignoreDomainErrors will ignore all domain errors only keeping fault
     *
     * catchDomainErrors is used to catch domain error and use it to trigger another ZIO effect
     *
     * catchDomainErrorsMakeN (1..5) is used to catch domain error and use it to trigger another ZIO effect with up to 1..5 domain errors using EitherNError
     *
     * onDomainErrors will trigger another ZIO effect which will produce a type bound domain error on any existing domain errors
     *
     * onDomainErrorsMakeN (2..5) will trigger another ZIO effect which will produce an EitherNError domain error on any existing domain errors
     */
    implicit final class ZioFaultOrEither2Syntax[R, FAULT, E1 <: SingleDomainError, E2 <: SingleDomainError, A](effect: zio.ZIO[R, FaultOr[FAULT, Either2Error[E1, E2]], A])(implicit ev1: FAULT <:< Throwable) {
      def composableDomainError: ZIO[R, FaultOr[FAULT, _ >: E1 with E2], A] =
        effect.mapError(faultOr => FaultOr(faultOr.flatten)).asInstanceOf[zio.ZIO[R, FaultOr[FAULT, _ >: E1 with E2], A]]

      // --- single or EitherNError

      def mapDomainErrorsTo1[TE1 <: SingleDomainError](mapFirst: E1 => TE1,
                                                       mapSecond: E2 => TE1): ZIO[R, FaultOr[FAULT, TE1], A] =
        effect.mapError(faultError => FaultOr(faultError.cause.map(either2 => either2.overjectionTo1(mapFirst, mapSecond))))

      def mapDomainErrorsTo2[TE1 <: SingleDomainError, TE2 <: SingleDomainError](mapFirst: E1 => TE1,
                                                                                 mapSecond: E2 => TE2): ZIO[R, FaultOr[FAULT, Either2Error[TE1, TE2]], A] =
        effect.mapError(faultError => FaultOr(faultError.cause.map(either2 => either2.bijection(mapFirst, mapSecond))))

      // --- type bound

      // map can reduce or keep the same nb of errors
      def mapDomainErrorsTo1[E3 <: SingleDomainError](f: Expected[_ >: E1 with E2] => E3): ZIO[R, FaultOr[FAULT, _ >: E3], A] =
        effect.composableDomainError.mapDomainErrorsTo1(f)

      def mapDomainErrorsTo2[E3 <: SingleDomainError, E4 <: SingleDomainError](f: Expected[_ >: E1 with E2] => Either2Error[E3, E4]): ZIO[R, FaultOr[FAULT, _ >: E3 with E4], A] =
        effect.composableDomainError.mapDomainErrorsTo2(f)

      // ---

      def ignoreDomainErrors[B >: A](f: Either2Error[E1, E2] => ZIO[R, FAULT, B]): ZIO[R, FAULT, B] =
        effect.catchAll((failure: FaultOr[FAULT, Either2Error[E1, E2]]) => failure.cause.
                                                                                  fold(throwable => ZIO.fail(throwable), // Fault must be propagated
                                                                                       (errors: Either2Error[E1, E2]) => f(errors)))

      // --- catch allows to get error and produce new ones

      def catchDomainErrors[B >: A, E3 <: SingleDomainError](f: Either2Error[E1, E2] => ZIO[R, FaultOr[FAULT, _ >: E3], B]): ZIO[R, FaultOr[FAULT, _ >: E3], B] =
        effect.catchAll((failure: FaultOr[FAULT, Either2Error[E1, E2]]) => failure.cause
                                                                                  .fold(throwable => ZIO.fail(FaultOr.makeFault[FAULT, E3](throwable)), // Fault must be propagated
                                                                                        (errors: Either2Error[E1, E2]) => f(errors)))

      def catchDomainErrorsMake1[B >: A, E3 <: SingleDomainError](f: Either2Error[E1, E2] => ZIO[R, FaultOr[FAULT, E3], B]): ZIO[R, FaultOr[FAULT, E3], B] =
        effect.catchAll((failure: FaultOr[FAULT, Either2Error[E1, E2]]) => failure.cause
                                                                                  .fold(throwable => ZIO.fail(FaultOr.makeFault[FAULT, E3](throwable)), // Fault must be propagated
                                                                                        (errors: Either2Error[E1, E2]) => f(errors)))

      def catchDomainErrorsMake2[B >: A, E3 <: SingleDomainError, E4 <: SingleDomainError](f: Either2Error[E1, E2] => ZIO[R, FaultOr[FAULT, Either2Error[E3, E4]], B]): ZIO[R, FaultOr[FAULT, Either2Error[E3, E4]], B] =
        effect.catchAll((failure: FaultOr[FAULT, Either2Error[E1, E2]]) => failure.cause
                                                                                  .fold(throwable => ZIO.fail(FaultOr.makeFault[FAULT, Either2Error[E3, E4]](throwable)), // Fault must be propagated
                                                                                        (errors: Either2Error[E1, E2]) => f(errors)))

      def catchDomainErrorsMake3[B >: A, E3 <: SingleDomainError, E4 <: SingleDomainError, E5 <: SingleDomainError](f: Either2Error[E1, E2] => ZIO[R, FaultOr[FAULT, Either3Error[E3, E4, E5]], B]): ZIO[R, FaultOr[FAULT, Either3Error[E3, E4, E5]], B] =
        effect.catchAll((failure: FaultOr[FAULT, Either2Error[E1, E2]]) => failure.cause
                                                                                  .fold(throwable => ZIO.fail(FaultOr.makeFault[FAULT, Either3Error[E3, E4, E5]](throwable)), // Fault must be propagated
                                                                                        (errors: Either2Error[E1, E2]) => f(errors)))

      def catchDomainErrorsMake4[B >: A, E3 <: SingleDomainError, E4 <: SingleDomainError, E5 <: SingleDomainError, E6 <: SingleDomainError](f: Either2Error[E1, E2] => ZIO[R, FaultOr[FAULT, Either4Error[E3, E4, E5, E6]], B]): ZIO[R, FaultOr[FAULT, Either4Error[E3, E4, E5, E6]], B] =
        effect.catchAll((failure: FaultOr[FAULT, Either2Error[E1, E2]]) => failure.cause
                                                                                  .fold(throwable => ZIO.fail(FaultOr.makeFault[FAULT, Either4Error[E3, E4, E5, E6]](throwable)), // Fault must be propagated
                                                                                        (errors: Either2Error[E1, E2]) => f(errors)))

      def catchDomainErrorsMake5[B >: A, E3 <: SingleDomainError, E4 <: SingleDomainError, E5 <: SingleDomainError, E6 <: SingleDomainError, E7 <: SingleDomainError](f: Either2Error[E1, E2] => ZIO[R, FaultOr[FAULT, Either5Error[E3, E4, E5, E6, E7]], B]): ZIO[R, FaultOr[FAULT, Either5Error[E3, E4, E5, E6, E7]], B] =
        effect.catchAll((failure: FaultOr[FAULT, Either2Error[E1, E2]]) => failure.cause
                                                                                  .fold(throwable => ZIO.fail(FaultOr.makeFault[FAULT, Either5Error[E3, E4, E5, E6, E7]](throwable)), // Fault must be propagated
                                                                                        (errors: Either2Error[E1, E2]) => f(errors)))

      // --- 'on' only react to error (but you don't get it) and produces nes ones

      def onDomainErrors[EE1 <: SingleDomainError](alternative: ZIO[R, FaultOr[FAULT, _ >: EE1], A]): ZIO[R, FaultOr[FAULT, _ >: EE1], A] =
        effect.catchAll((f: FaultOr[FAULT, Either2Error[E1, E2]]) =>
                          if (f.cause.isLeft) effect.asInstanceOf[zio.ZIO[R, FaultOr[FAULT, _ >: EE1], A]]
                          else alternative
                        )

      def onDomainErrorsMake1[EE1 <: SingleDomainError](alternative: ZIO[R, FaultOr[FAULT, EE1], A]): ZIO[R, FaultOr[FAULT, EE1], A] =
        effect.catchAll((f: FaultOr[FAULT, Either2Error[E1, E2]]) =>
                          if (f.cause.isLeft) effect.asInstanceOf[zio.ZIO[R, FaultOr[FAULT, EE1], A]]
                          else alternative
                        )

      def onDomainErrorsMake2[EE1 <: SingleDomainError, EE2 <: SingleDomainError](alternative: ZIO[R, FaultOr[FAULT, Either2Error[EE1, EE2]], A]): ZIO[R, FaultOr[FAULT, Either2Error[EE1, EE2]], A] =
        effect.catchAll((f: FaultOr[FAULT, Either2Error[E1, E2]]) =>
                          if (f.cause.isLeft) effect.asInstanceOf[zio.ZIO[R, FaultOr[FAULT, Either2Error[EE1, EE2]], A]]
                          else alternative
                        )

      def onDomainErrorsMake3[EE1 <: SingleDomainError, EE2 <: SingleDomainError, EE3 <: SingleDomainError](alternative: ZIO[R, FaultOr[FAULT, Either3Error[EE1, EE2, EE3]], A]): ZIO[R, FaultOr[FAULT, Either3Error[EE1, EE2, EE3]], A] =
        effect.catchAll((f: FaultOr[FAULT, Either2Error[E1, E2]]) =>
                          if (f.cause.isLeft) effect.asInstanceOf[zio.ZIO[R, FaultOr[FAULT, Either3Error[EE1, EE2, EE3]], A]]
                          else alternative
                        )

      def onDomainErrorsMake4[EE1 <: SingleDomainError, EE2 <: SingleDomainError, EE3 <: SingleDomainError, EE4 <: SingleDomainError](alternative: ZIO[R, FaultOr[FAULT, Either4Error[EE1, EE2, EE3, EE4]], A]): ZIO[R, FaultOr[FAULT, Either4Error[EE1, EE2, EE3, EE4]], A] =
        effect.catchAll((f: FaultOr[FAULT, Either2Error[E1, E2]]) =>
                          if (f.cause.isLeft) effect.asInstanceOf[zio.ZIO[R, FaultOr[FAULT, Either4Error[EE1, EE2, EE3, EE4]], A]]
                          else alternative
                        )

      def onDomainErrorsMake5[EE1 <: SingleDomainError, EE2 <: SingleDomainError, EE3 <: SingleDomainError, EE4 <: SingleDomainError, EE5 <: SingleDomainError](alternative: ZIO[R, FaultOr[FAULT, Either5Error[EE1, EE2, EE3, EE4, EE5]], A]): ZIO[R, FaultOr[FAULT, Either5Error[EE1, EE2, EE3, EE4, EE5]], A] =
        effect.catchAll((f: FaultOr[FAULT, Either2Error[E1, E2]]) =>
                          if (f.cause.isLeft) effect.asInstanceOf[zio.ZIO[R, FaultOr[FAULT, Either5Error[EE1, EE2, EE3, EE4, EE5]], A]]
                          else alternative
                        )
    }

    /**
     * Syntax extensions for ZIO effect based on FaultOr where domain error is based on Either3Error
     * @param effect the ZIO effect based on FaultOr
     * @param ev1 system fault must inherit from Throwable
     * @tparam R effect environmental type
     * @tparam FAULT FaultOr system fault type
     * @tparam E1 Either3Error first domain error type
     * @tparam E2 Either3Error second domain error type
     * @tparam E3 Either3Error third domain error type
     * @tparam A effect result type
     *
     * composableDomainError convert the effect into the composable domain error version (based on type bound)
     *
     * mapDomainErrorsToN is used to map domain error to new domain errors
     *
     * ignoreDomainErrors will ignore all domain errors only keeping fault
     *
     * catchDomainErrors is used to catch domain error and use it to trigger another ZIO effect
     *
     * catchDomainErrorsMakeN (1..5) is used to catch domain error and use it to trigger another ZIO effect with up to 1..5 domain errors using EitherNError
     *
     * onDomainErrors will trigger another ZIO effect which will produce a type bound domain error on any existing domain errors
     *
     * onDomainErrorsMakeN (2..5) will trigger another ZIO effect which will produce an EitherNError domain error on any existing domain errors
     */
    implicit final class ZioFaultOrEither3Syntax[R, FAULT, E1 <: SingleDomainError, E2 <: SingleDomainError, E3 <: SingleDomainError, A](effect: zio.ZIO[R, FaultOr[FAULT, Either3Error[E1, E2, E3]], A])(implicit ev1: FAULT <:< Throwable) {
      def composableDomainError: ZIO[R, FaultOr[FAULT, _ >: E1 with E2 with E3], A] =
        effect.mapError(faultOr => FaultOr(faultOr.flatten)).asInstanceOf[zio.ZIO[R, FaultOr[FAULT, _ >: E1 with E2 with E3], A]]

      def mapDomainErrorsTo1[E4 <: SingleDomainError](f: Expected[_ >: E1 with E2 with E3] => E4): ZIO[R, FaultOr[FAULT, _ >: E4], A] =
        effect.composableDomainError.mapDomainErrorsTo1(f)
      def mapDomainErrorsTo2[E4 <: SingleDomainError, E5 <: SingleDomainError](f: Expected[_ >: E1 with E2 with E3] => Either2Error[E4, E5]): ZIO[R, FaultOr[FAULT, _ >: E4 with E5], A] =
        effect.composableDomainError.mapDomainErrorsTo2(f)
      def mapDomainErrorsTo3[E4 <: SingleDomainError, E5 <: SingleDomainError, E6 <: SingleDomainError](f: Expected[_ >: E1 with E2 with E3] => Either3Error[E4, E5, E6]): ZIO[R, FaultOr[FAULT, _ >: E4 with E5 with E6], A] =
        effect.composableDomainError.mapDomainErrorsTo3(f)

      def mapDomainErrorsTo1[TE1 <: SingleDomainError](mapFirst : E1 => TE1,
                                                       mapSecond: E2 => TE1,
                                                       mapThird : E3 => TE1): ZIO[R, FaultOr[FAULT, TE1], A] =
        effect.mapError(faultError => FaultOr(faultError.cause.map(either3 => either3.overjectionTo1(mapFirst, mapSecond, mapThird))))

      def mapDomainErrorsTo2[TE1 <: SingleDomainError, TE2 <: SingleDomainError](mapFirst : E1 => Either2Error[TE1, TE2],
                                                                                 mapSecond: E2 => Either2Error[TE1, TE2],
                                                                                 mapThird : E3 => Either2Error[TE1, TE2]): ZIO[R, FaultOr[FAULT, Either2Error[TE1, TE2]], A] =
        effect.mapError(faultError => FaultOr(faultError.cause.map(either3 => either3.overjectionTo2(mapFirst, mapSecond, mapThird))))

      def mapDomainErrorsTo3[TE1 <: SingleDomainError, TE2 <: SingleDomainError, TE3 <: SingleDomainError](mapFirst : E1 => TE1,
                                                                                                           mapSecond: E2 => TE2,
                                                                                                           mapThird : E3 => TE3): ZIO[R, FaultOr[FAULT, Either3Error[TE1, TE2, TE3]], A] =
        effect.mapError(faultError => FaultOr(faultError.cause.map(either3 => either3.bijection(mapFirst, mapSecond, mapThird))))

      // won't produce any domain error
      def ignoreDomainErrors[B >: A](f: Either3Error[E1, E2, E3] => ZIO[R, FAULT, B]): ZIO[R, FAULT, B] =
        effect.catchAll((failure: FaultOr[FAULT, Either3Error[E1, E2, E3]]) => failure.cause
                                                                                      .fold(throwable => ZIO.fail(throwable), // Fault must be propagated
                                                                                            (errors: Either3Error[E1, E2, E3]) => f(errors)))

      def catchDomainErrors[B >: A, E4 <: SingleDomainError](f: Either3Error[E1, E2, E3] => ZIO[R, FaultOr[FAULT, _ >: E4], B]): ZIO[R, FaultOr[FAULT, _ >: E4], B] =
        effect.catchAll((failure: FaultOr[FAULT, Either3Error[E1, E2, E3]]) => failure.cause
                                                                                      .fold(throwable => ZIO.fail(FaultOr.makeFault[FAULT, E4](throwable)), // Fault must be propagated
                                                                                            (errors: Either3Error[E1, E2, E3]) => f(errors)))

      def catchDomainErrorsMake1[B >: A, E4 <: SingleDomainError](f: Either3Error[E1, E2, E3] => ZIO[R, FaultOr[FAULT, E4], B]): ZIO[R, FaultOr[FAULT, E4], B] =
        effect.catchAll((failure: FaultOr[FAULT, Either3Error[E1, E2, E3]]) => failure.cause
                                                                                      .fold(throwable => ZIO.fail(FaultOr.makeFault[FAULT, E4](throwable)), // Fault must be propagated
                                                                                            (errors: Either3Error[E1, E2, E3]) => f(errors)))

      def catchDomainErrorsMake2[B >: A, E4 <: SingleDomainError, E5 <: SingleDomainError](f: Either3Error[E1, E2, E3] => ZIO[R, FaultOr[FAULT, Either2Error[E4, E5]], B]): ZIO[R, FaultOr[FAULT, Either2Error[E4, E5]], B] =
        effect.catchAll((failure: FaultOr[FAULT, Either3Error[E1, E2, E3]]) => failure.cause
                                                                                      .fold(throwable => ZIO.fail(FaultOr.makeFault[FAULT, Either2Error[E4, E5]](throwable)), // Fault must be propagated
                                                                                            (errors: Either3Error[E1, E2, E3]) => f(errors)))

      def catchDomainErrorsMake3[B >: A, E4 <: SingleDomainError, E5 <: SingleDomainError, E6 <: SingleDomainError](f: Either3Error[E1, E2, E3] => ZIO[R, FaultOr[FAULT, Either3Error[E3, E4, E5]], B]): ZIO[R, FaultOr[FAULT, Either3Error[E3, E4, E5]], B] =
        effect.catchAll((failure: FaultOr[FAULT, Either3Error[E1, E2, E3]]) => failure.cause
                                                                                      .fold(throwable => ZIO.fail(FaultOr.makeFault[FAULT, Either3Error[E3, E4, E5]](throwable)), // Fault must be propagated
                                                                                            (errors: Either3Error[E1, E2, E3]) => f(errors)))

      def catchDomainErrorsMake4[B >: A, E4 <: SingleDomainError, E5 <: SingleDomainError, E6 <: SingleDomainError, E7 <: SingleDomainError](f: Either3Error[E1, E2, E3] => ZIO[R, FaultOr[FAULT, Either4Error[E4, E5, E6, E7]], B]): ZIO[R, FaultOr[FAULT, Either4Error[E4, E5, E6, E7]], B] =
        effect.catchAll((failure: FaultOr[FAULT, Either3Error[E1, E2, E3]]) => failure.cause
                                                                                      .fold(throwable => ZIO.fail(FaultOr.makeFault[FAULT, Either4Error[E4, E5, E6, E7]](throwable)), // Fault must be propagated
                                                                                            (errors: Either3Error[E1, E2, E3]) => f(errors)))

      def catchDomainErrorsMake5[B >: A, E4 <: SingleDomainError, E5 <: SingleDomainError, E6 <: SingleDomainError, E7 <: SingleDomainError, E8 <: SingleDomainError](f: Either3Error[E1, E2, E3] => ZIO[R, FaultOr[FAULT, Either5Error[E4, E5, E6, E7, E8]], B]): ZIO[R, FaultOr[FAULT, Either5Error[E4, E5, E6, E7, E8]], B] =
        effect.catchAll((failure: FaultOr[FAULT, Either3Error[E1, E2, E3]]) => failure.cause
                                                                                      .fold(throwable => ZIO.fail(FaultOr.makeFault[FAULT, Either5Error[E4, E5, E6, E7, E8]](throwable)), // Fault must be propagated
                                                                                            (errors: Either3Error[E1, E2, E3]) => f(errors)))

      def onDomainErrors[EE1 <: SingleDomainError](alternative: ZIO[R, FaultOr[FAULT, _ >: EE1], A]): ZIO[R, FaultOr[FAULT, _ >: EE1], A] =
        effect.catchAll((f: FaultOr[FAULT, Either3Error[E1, E2, E3]]) =>
                          if (f.cause.isLeft) effect.asInstanceOf[zio.ZIO[R, FaultOr[FAULT, _ >: EE1], A]]
                          else alternative
                        )

      def onDomainErrorsMake1[EE1 <: SingleDomainError](alternative: ZIO[R, FaultOr[FAULT, EE1], A]): ZIO[R, FaultOr[FAULT, EE1], A] =
        effect.catchAll((f: FaultOr[FAULT, Either3Error[E1, E2, E3]]) =>
                          if (f.cause.isLeft) effect.asInstanceOf[zio.ZIO[R, FaultOr[FAULT, EE1], A]]
                          else alternative
                        )

      def onDomainErrorsMake2[EE1 <: SingleDomainError, EE2 <: SingleDomainError](alternative: ZIO[R, FaultOr[FAULT, Either2Error[EE1, EE2]], A]): ZIO[R, FaultOr[FAULT, Either2Error[EE1, EE2]], A] =
        effect.catchAll((f: FaultOr[FAULT, Either3Error[E1, E2, E3]]) =>
                          if (f.cause.isLeft) effect.asInstanceOf[zio.ZIO[R, FaultOr[FAULT, Either2Error[EE1, EE2]], A]]
                          else alternative
                        )

      def onDomainErrorsMake3[EE1 <: SingleDomainError, EE2 <: SingleDomainError, EE3 <: SingleDomainError](alternative: ZIO[R, FaultOr[FAULT, Either3Error[EE1, EE2, EE3]], A]): ZIO[R, FaultOr[FAULT, Either3Error[EE1, EE2, EE3]], A] =
        effect.catchAll((f: FaultOr[FAULT, Either3Error[E1, E2, E3]]) =>
                          if (f.cause.isLeft) effect.asInstanceOf[zio.ZIO[R, FaultOr[FAULT, Either3Error[EE1, EE2, EE3]], A]]
                          else alternative
                        )

      def onDomainErrorsMake4[EE1 <: SingleDomainError, EE2 <: SingleDomainError, EE3 <: SingleDomainError, EE4 <: SingleDomainError](alternative: ZIO[R, FaultOr[FAULT, Either4Error[EE1, EE2, EE3, EE4]], A]): ZIO[R, FaultOr[FAULT, Either4Error[EE1, EE2, EE3, EE4]], A] =
        effect.catchAll((f: FaultOr[FAULT, Either3Error[E1, E2, E3]]) =>
                          if (f.cause.isLeft) effect.asInstanceOf[zio.ZIO[R, FaultOr[FAULT, Either4Error[EE1, EE2, EE3, EE4]], A]]
                          else alternative
                        )

      def onDomainErrorsMake5[EE1 <: SingleDomainError, EE2 <: SingleDomainError, EE3 <: SingleDomainError, EE4 <: SingleDomainError, EE5 <: SingleDomainError](alternative: ZIO[R, FaultOr[FAULT, Either5Error[EE1, EE2, EE3, EE4, EE5]], A]): ZIO[R, FaultOr[FAULT, Either5Error[EE1, EE2, EE3, EE4, EE5]], A] =
        effect.catchAll((f: FaultOr[FAULT, Either3Error[E1, E2, E3]]) =>
                          if (f.cause.isLeft) effect.asInstanceOf[zio.ZIO[R, FaultOr[FAULT, Either5Error[EE1, EE2, EE3, EE4, EE5]], A]]
                          else alternative
                        )
    }

    /**
     * Syntax extensions for ZIO effect based on FaultOr where domain error is based on Either4Error
     * @param effect the ZIO effect based on FaultOr
     * @param ev1 system fault must inherit from Throwable
     * @tparam R effect environmental type
     * @tparam FAULT FaultOr system fault type
     * @tparam E1 Either4Error first domain error type
     * @tparam E2 Either4Error second domain error type
     * @tparam E3 Either4Error third domain error type
     * @tparam E4 Either4Error fourth domain error type
     * @tparam A effect result type
     *
     * composableDomainError convert the effect into the composable domain error version (based on type bound)
     *
     * mapDomainErrorsToN is used to map domain error to new domain errors
     *
     * ignoreDomainErrors will ignore all domain errors only keeping fault
     *
     * catchDomainErrors is used to catch domain error and use it to trigger another ZIO effect
     *
     * catchDomainErrorsMakeN (1..5) is used to catch domain error and use it to trigger another ZIO effect with up to 1..5 domain errors using EitherNError
     *
     * onDomainErrors will trigger another ZIO effect which will produce a type bound domain error on any existing domain errors
     *
     * onDomainErrorsMakeN (2..5) will trigger another ZIO effect which will produce an EitherNError domain error on any existing domain errors
     */
    implicit final class ZioFaultOrEither4Syntax[R, FAULT, E1 <: SingleDomainError, E2 <: SingleDomainError, E3 <: SingleDomainError, E4 <: SingleDomainError, A](effect: zio.ZIO[R, FaultOr[FAULT, Either4Error[E1, E2, E3, E4]], A])(implicit ev1: FAULT <:< Throwable) {
      def composableDomainError: ZIO[R, FaultOr[FAULT, _ >: E1 with E2 with E3 with E4], A] = effect.mapError(faultOr => FaultOr(faultOr.flatten)).asInstanceOf[zio.ZIO[R, FaultOr[FAULT, _ >: E1 with E2 with E3 with E4], A]]

      def mapDomainErrorsTo1[E5 <: SingleDomainError](f: Expected[_ >: E1 with E2 with E3 with E4] => E5): ZIO[R, FaultOr[FAULT, _ >: E5], A] =
        effect.composableDomainError.mapDomainErrorsTo1(f)
      def mapDomainErrorsTo2[E5 <: SingleDomainError, E6 <: SingleDomainError](f: Expected[_ >: E1 with E2 with E3 with E4] => Either2Error[E5, E6]): ZIO[R, FaultOr[FAULT, _ >: E5 with E6], A] =
        effect.composableDomainError.mapDomainErrorsTo2(f)
      def mapDomainErrorsTo3[E5 <: SingleDomainError, E6 <: SingleDomainError, E7 <: SingleDomainError](f: Expected[_ >: E1 with E2 with E3 with E4] => Either3Error[E5, E6, E7]): ZIO[R, FaultOr[FAULT, _ >: E5 with E6 with E7], A] =
        effect.composableDomainError.mapDomainErrorsTo3(f)
      def mapDomainErrorsTo4[E5 <: SingleDomainError, E6 <: SingleDomainError, E7 <: SingleDomainError, E8 <: SingleDomainError](f: Expected[_ >: E1 with E2 with E3 with E4] => Either4Error[E5, E6, E7, E8]): ZIO[R, FaultOr[FAULT, _ >: E5 with E6 with E7 with E8], A] =
        effect.composableDomainError.mapDomainErrorsTo4(f)

      def mapDomainErrorsTo1[TE1 <: SingleDomainError](mapFirst : E1 => TE1,
                                                       mapSecond: E2 => TE1,
                                                       mapThird : E3 => TE1,
                                                       mapFourth: E4 => TE1): ZIO[R, FaultOr[FAULT, TE1], A] =
        effect.mapError(faultError => FaultOr(faultError.cause.map(either4 => either4.overjectionTo1(mapFirst, mapSecond, mapThird, mapFourth))))

      def mapDomainErrorsTo2[TE1 <: SingleDomainError, TE2 <: SingleDomainError](mapFirst : E1 => Either2Error[TE1, TE2],
                                                                                 mapSecond: E2 => Either2Error[TE1, TE2],
                                                                                 mapThird : E3 => Either2Error[TE1, TE2],
                                                                                 mapFourth: E4 => Either2Error[TE1, TE2]): ZIO[R, FaultOr[FAULT, Either2Error[TE1, TE2]], A] =
        effect.mapError(faultError => FaultOr(faultError.cause.map(either4 => either4.overjectionTo2(mapFirst, mapSecond, mapThird, mapFourth))))

      def mapDomainErrorsTo3[TE1 <: SingleDomainError, TE2 <: SingleDomainError, TE3 <: SingleDomainError](mapFirst : E1 => Either3Error[TE1, TE2, TE3],
                                                                                                           mapSecond: E2 => Either3Error[TE1, TE2, TE3],
                                                                                                           mapThird : E3 => Either3Error[TE1, TE2, TE3],
                                                                                                           mapFourth: E4 => Either3Error[TE1, TE2, TE3]): ZIO[R, FaultOr[FAULT, Either3Error[TE1, TE2, TE3]], A] =
        effect.mapError(faultError => FaultOr(faultError.cause.map(either4 => either4.overjectionTo3(mapFirst, mapSecond, mapThird, mapFourth))))

      def mapDomainErrorsTo4[TE1 <: SingleDomainError, TE2 <: SingleDomainError, TE3 <: SingleDomainError, TE4 <: SingleDomainError](mapFirst : E1 => TE1,
                                                                                                                                     mapSecond: E2 => TE2,
                                                                                                                                     mapThird : E3 => TE3,
                                                                                                                                     mapFourth: E4 => TE4): ZIO[R, FaultOr[FAULT, Either4Error[TE1, TE2, TE3, TE4]], A] =
        effect.mapError(faultError => FaultOr(faultError.cause.map(either4 => either4.bijection(mapFirst, mapSecond, mapThird, mapFourth))))

      // won't produce any domain error
      def ignoreDomainErrors[B >: A](f: Either4Error[E1, E2, E3, E4] => ZIO[R, FAULT, B]): ZIO[R, FAULT, B] =
        effect.catchAll((failure: FaultOr[FAULT, Either4Error[E1, E2, E3, E4]]) => failure.cause
                                                                                          .fold(throwable => ZIO.fail(throwable), // Fault must be propagated
                                                                                                (errors: Either4Error[E1, E2, E3, E4]) => f(errors)))

      def catchDomainErrors[B >: A, E5 <: SingleDomainError](f: Either4Error[E1, E2, E3, E4] => ZIO[R, FaultOr[FAULT, _ >: E5], B]): ZIO[R, FaultOr[FAULT, _ >: E5], B] =
        effect.catchAll((failure: FaultOr[FAULT, Either4Error[E1, E2, E3, E4]]) => failure.cause
                                                                                          .fold(throwable => ZIO.fail(FaultOr.makeFault[FAULT, E5](throwable)), // Fault must be propagated
                                                                                                (errors: Either4Error[E1, E2, E3, E4]) => f(errors)))

      def catchDomainErrorsMake1[B >: A, E5 <: SingleDomainError](f: Either4Error[E1, E2, E3, E4] => ZIO[R, FaultOr[FAULT, E5], B]): ZIO[R, FaultOr[FAULT, E5], B] =
        effect.catchAll((failure: FaultOr[FAULT, Either4Error[E1, E2, E3, E4]]) => failure.cause
                                                                                          .fold(throwable => ZIO.fail(FaultOr.makeFault[FAULT, E5](throwable)), // Fault must be propagated
                                                                                                (errors: Either4Error[E1, E2, E3, E4]) => f(errors)))

      def catchDomainErrorsMake2[B >: A, E5 <: SingleDomainError, E6 <: SingleDomainError](f: Either4Error[E1, E2, E3, E4] => ZIO[R, FaultOr[FAULT, Either2Error[E5, E6]], B]): ZIO[R, FaultOr[FAULT, Either2Error[E5, E6]], B] =
        effect.catchAll((failure: FaultOr[FAULT, Either4Error[E1, E2, E3, E4]]) => failure.cause
                                                                                          .fold(throwable => ZIO.fail(FaultOr.makeFault[FAULT, Either2Error[E5, E6]](throwable)), // Fault must be propagated
                                                                                                (errors: Either4Error[E1, E2, E3, E4]) => f(errors)))

      def catchDomainErrorsMake3[B >: A, E5 <: SingleDomainError, E6 <: SingleDomainError, E7 <: SingleDomainError](f: Either4Error[E1, E2, E3, E4] => ZIO[R, FaultOr[FAULT, Either3Error[E5, E6, E7]], B]): ZIO[R, FaultOr[FAULT, Either3Error[E5, E6, E7]], B] =
        effect.catchAll((failure: FaultOr[FAULT, Either4Error[E1, E2, E3, E4]]) => failure.cause
                                                                                          .fold(throwable => ZIO.fail(FaultOr.makeFault[FAULT, Either3Error[E5, E6, E7]](throwable)), // Fault must be propagated
                                                                                                (errors: Either4Error[E1, E2, E3, E4]) => f(errors)))

      def catchDomainErrorsMake4[B >: A, E5 <: SingleDomainError, E6 <: SingleDomainError, E7 <: SingleDomainError, E8 <: SingleDomainError](f: Either4Error[E1, E2, E3, E4] => ZIO[R, FaultOr[FAULT, Either4Error[E5, E6, E7, E8]], B]): ZIO[R, FaultOr[FAULT, Either4Error[E5, E6, E7, E8]], B] =
        effect.catchAll((failure: FaultOr[FAULT, Either4Error[E1, E2, E3, E4]]) => failure.cause
                                                                                          .fold(throwable => ZIO.fail(FaultOr.makeFault[FAULT, Either4Error[E5, E6, E7, E8]](throwable)), // Fault must be propagated
                                                                                                (errors: Either4Error[E1, E2, E3, E4]) => f(errors)))

      def catchDomainErrorsMake5[B >: A, E5 <: SingleDomainError, E6 <: SingleDomainError, E7 <: SingleDomainError, E8 <: SingleDomainError, E9 <: SingleDomainError](f: Either4Error[E1, E2, E3, E4] => ZIO[R, FaultOr[FAULT, Either5Error[E5, E6, E7, E8, E9]], B]): ZIO[R, FaultOr[FAULT, Either5Error[E5, E6, E7, E8, E9]], B] =
        effect.catchAll((failure: FaultOr[FAULT, Either4Error[E1, E2, E3, E4]]) => failure.cause
                                                                                          .fold(throwable => ZIO.fail(FaultOr.makeFault[FAULT, Either5Error[E5, E6, E7, E8, E9]](throwable)), // Fault must be propagated
                                                                                                (errors: Either4Error[E1, E2, E3, E4]) => f(errors)))

      def onDomainErrors[EE1 <: SingleDomainError](alternative: ZIO[R, FaultOr[FAULT, _ >: EE1], A]): ZIO[R, FaultOr[FAULT, _ >: EE1], A] =
        effect.catchAll((f: FaultOr[FAULT, Either4Error[E1, E2, E3, E4]]) =>
                          if (f.cause.isLeft) effect.asInstanceOf[zio.ZIO[R, FaultOr[FAULT, _ >: EE1], A]]
                          else alternative
                        )

      def onDomainErrorsMake1[EE1 <: SingleDomainError](alternative: ZIO[R, FaultOr[FAULT, EE1], A]): ZIO[R, FaultOr[FAULT, EE1], A] =
        effect.catchAll((f: FaultOr[FAULT, Either4Error[E1, E2, E3, E4]]) =>
                          if (f.cause.isLeft) effect.asInstanceOf[zio.ZIO[R, FaultOr[FAULT, EE1], A]]
                          else alternative
                        )

      def onDomainErrorsMake2[EE1 <: SingleDomainError, EE2 <: SingleDomainError](alternative: ZIO[R, FaultOr[FAULT, Either2Error[EE1, EE2]], A]): ZIO[R, FaultOr[FAULT, Either2Error[EE1, EE2]], A] =
        effect.catchAll((f: FaultOr[FAULT, Either4Error[E1, E2, E3, E4]]) =>
                          if (f.cause.isLeft) effect.asInstanceOf[zio.ZIO[R, FaultOr[FAULT, Either2Error[EE1, EE2]], A]]
                          else alternative
                        )

      def onDomainErrorsMake3[EE1 <: SingleDomainError, EE2 <: SingleDomainError, EE3 <: SingleDomainError](alternative: ZIO[R, FaultOr[FAULT, Either3Error[EE1, EE2, EE3]], A]): ZIO[R, FaultOr[FAULT, Either3Error[EE1, EE2, EE3]], A] =
        effect.catchAll((f: FaultOr[FAULT, Either4Error[E1, E2, E3, E4]]) =>
                          if (f.cause.isLeft) effect.asInstanceOf[zio.ZIO[R, FaultOr[FAULT, Either3Error[EE1, EE2, EE3]], A]]
                          else alternative
                        )

      def onDomainErrorsMake4[EE1 <: SingleDomainError, EE2 <: SingleDomainError, EE3 <: SingleDomainError, EE4 <: SingleDomainError](alternative: ZIO[R, FaultOr[FAULT, Either4Error[EE1, EE2, EE3, EE4]], A]): ZIO[R, FaultOr[FAULT, Either4Error[EE1, EE2, EE3, EE4]], A] =
        effect.catchAll((f: FaultOr[FAULT, Either4Error[E1, E2, E3, E4]]) =>
                          if (f.cause.isLeft) effect.asInstanceOf[zio.ZIO[R, FaultOr[FAULT, Either4Error[EE1, EE2, EE3, EE4]], A]]
                          else alternative
                        )

      def onDomainErrorsMake5[EE1 <: SingleDomainError, EE2 <: SingleDomainError, EE3 <: SingleDomainError, EE4 <: SingleDomainError, EE5 <: SingleDomainError](alternative: ZIO[R, FaultOr[FAULT, Either5Error[EE1, EE2, EE3, EE4, EE5]], A]): ZIO[R, FaultOr[FAULT, Either5Error[EE1, EE2, EE3, EE4, EE5]], A] =
        effect.catchAll((f: FaultOr[FAULT, Either4Error[E1, E2, E3, E4]]) =>
                          if (f.cause.isLeft) effect.asInstanceOf[zio.ZIO[R, FaultOr[FAULT, Either5Error[EE1, EE2, EE3, EE4, EE5]], A]]
                          else alternative
                        )
    }

    /**
     * Syntax extensions for ZIO effect based on FaultOr where domain error is based on Either5Error
     * @param effect the ZIO effect based on FaultOr
     * @param ev1 system fault must inherit from Throwable
     * @tparam R effect environmental type
     * @tparam FAULT FaultOr system fault type
     * @tparam E1 Either5Error first domain error type
     * @tparam E2 Either5Error second domain error type
     * @tparam E3 Either5Error third domain error type
     * @tparam E4 Either5Error fourth domain error type
     * @tparam E5 Either5Error fifth domain error type
     * @tparam A effect result type
     *
     * composableDomainError convert the effect into the composable domain error version (based on type bound)
     *
     * mapDomainErrorsToN is used to map domain error to new domain errors
     *
     * ignoreDomainErrors will ignore all domain errors only keeping fault
     *
     * catchDomainErrors is used to catch domain error and use it to trigger another ZIO effect
     *
     * catchDomainErrorsMakeN (1..5) is used to catch domain error and use it to trigger another ZIO effect with up to 1..5 domain errors using EitherNError
     *
     * onDomainErrors will trigger another ZIO effect which will produce a type bound domain error on any existing domain errors
     *
     * onDomainErrorsMakeN (2..5) will trigger another ZIO effect which will produce an EitherNError domain error on any existing domain errors
     */
    implicit final class ZioFaultOrEither5Syntax[R, FAULT, E1 <: SingleDomainError, E2 <: SingleDomainError, E3 <: SingleDomainError, E4 <: SingleDomainError, E5 <: SingleDomainError, A](effect: zio.ZIO[R, FaultOr[FAULT, Either5Error[E1, E2, E3, E4, E5]], A])(implicit ev1: FAULT <:< Throwable) {
      def composableDomainError: ZIO[R, FaultOr[FAULT, _ >: E1 with E2 with E3 with E4 with E5], A] = effect.mapError(faultOr => FaultOr(faultOr.flatten)).asInstanceOf[zio.ZIO[R, FaultOr[FAULT, _ >: E1 with E2 with E3 with E4 with E5], A]]

      def mapDomainErrorsTo1[E6 <: SingleDomainError](f: Expected[_ >: E1 with E2 with E3 with E4 with E5] => E5): ZIO[R, FaultOr[FAULT, _ >: E6], A] =
        effect.composableDomainError.mapDomainErrorsTo1(f)
      def mapDomainErrorsTo2[E6 <: SingleDomainError, E7 <: SingleDomainError](f: Expected[_ >: E1 with E2 with E3 with E4 with E5] => Either2Error[E6, E7]): ZIO[R, FaultOr[FAULT, _ >: E6 with E7], A] =
        effect.composableDomainError.mapDomainErrorsTo2(f)
      def mapDomainErrorsTo3[E6 <: SingleDomainError, E7 <: SingleDomainError, E8 <: SingleDomainError](f: Expected[_ >: E1 with E2 with E3 with E4 with E5] => Either3Error[E6, E7, E8]): ZIO[R, FaultOr[FAULT, _ >: E6 with E7 with E8], A] =
        effect.composableDomainError.mapDomainErrorsTo3(f)
      def mapDomainErrorsTo4[E6 <: SingleDomainError, E7 <: SingleDomainError, E8 <: SingleDomainError, E9 <: SingleDomainError](f: Expected[_ >: E1 with E2 with E3 with E4 with E5] => Either4Error[E6, E7, E8, E9]): ZIO[R, FaultOr[FAULT, _ >: E6 with E7 with E8 with E9], A] =
        effect.composableDomainError.mapDomainErrorsTo4(f)
      def mapDomainErrorsTo5[E6 <: SingleDomainError, E7 <: SingleDomainError, E8 <: SingleDomainError, E9 <: SingleDomainError, E10 <: SingleDomainError](f: Expected[_ >: E1 with E2 with E3 with E4 with E5] => Either5Error[E6, E7, E8, E9, E10]): ZIO[R, FaultOr[FAULT, _ >: E6 with E7 with E8 with E9 with E10], A] =
        effect.composableDomainError.mapDomainErrorsTo5(f)

      def mapDomainErrorsTo1[TE1 <: SingleDomainError](mapFirst : E1 => TE1,
                                                       mapSecond: E2 => TE1,
                                                       mapThird : E3 => TE1,
                                                       mapFourth: E4 => TE1,
                                                       mapFifth : E5 => TE1): ZIO[R, FaultOr[FAULT, TE1], A] =
        effect.mapError(faultError => FaultOr(faultError.cause.map(either5 => either5.overjectionTo1(mapFirst, mapSecond, mapThird, mapFourth, mapFifth))))

      def mapDomainErrorsTo2[TE1 <: SingleDomainError, TE2 <: SingleDomainError](mapFirst : E1 => Either2Error[TE1, TE2],
                                                                                 mapSecond: E2 => Either2Error[TE1, TE2],
                                                                                 mapThird : E3 => Either2Error[TE1, TE2],
                                                                                 mapFourth: E4 => Either2Error[TE1, TE2],
                                                                                 mapFifth : E5 => Either2Error[TE1, TE2]): ZIO[R, FaultOr[FAULT, Either2Error[TE1, TE2]], A] =
        effect.mapError(faultError => FaultOr(faultError.cause.map(either5 => either5.overjectionTo2(mapFirst, mapSecond, mapThird, mapFourth, mapFifth))))

      def mapDomainErrorsTo3[TE1 <: SingleDomainError, TE2 <: SingleDomainError, TE3 <: SingleDomainError](mapFirst : E1 => Either3Error[TE1, TE2, TE3],
                                                                                                           mapSecond: E2 => Either3Error[TE1, TE2, TE3],
                                                                                                           mapThird : E3 => Either3Error[TE1, TE2, TE3],
                                                                                                           mapFourth: E4 => Either3Error[TE1, TE2, TE3],
                                                                                                           mapFifth : E5 => Either3Error[TE1, TE2, TE3]): ZIO[R, FaultOr[FAULT, Either3Error[TE1, TE2, TE3]], A] =
        effect.mapError(faultError => FaultOr(faultError.cause.map(either4 => either4.overjectionTo3(mapFirst, mapSecond, mapThird, mapFourth, mapFifth))))

      def mapDomainErrorsTo4[TE1 <: SingleDomainError, TE2 <: SingleDomainError, TE3 <: SingleDomainError, TE4 <: SingleDomainError](mapFirst : E1 => Either4Error[TE1, TE2, TE3, TE4],
                                                                                                                                     mapSecond: E2 => Either4Error[TE1, TE2, TE3, TE4],
                                                                                                                                     mapThird : E3 => Either4Error[TE1, TE2, TE3, TE4],
                                                                                                                                     mapFourth: E4 => Either4Error[TE1, TE2, TE3, TE4],
                                                                                                                                     mapFifth : E5 => Either4Error[TE1, TE2, TE3, TE4]): ZIO[R, FaultOr[FAULT, Either4Error[TE1, TE2, TE3, TE4]], A] =
        effect.mapError(faultError => FaultOr(faultError.cause.map(either4 => either4.overjectionTo4(mapFirst, mapSecond, mapThird, mapFourth, mapFifth))))

      def mapDomainErrorsTo5[TE1 <: SingleDomainError, TE2 <: SingleDomainError, TE3 <: SingleDomainError, TE4 <: SingleDomainError, TE5 <: SingleDomainError](mapFirst : E1 => TE1,
                                                                                                                                                               mapSecond: E2 => TE2,
                                                                                                                                                               mapThird : E3 => TE3,
                                                                                                                                                               mapFourth: E4 => TE4,
                                                                                                                                                               mapFifth : E5 => TE5): ZIO[R, FaultOr[FAULT, Either5Error[TE1, TE2, TE3, TE4, TE5]], A] =
        effect.mapError(faultError => FaultOr(faultError.cause.map(either5 => either5.bijection(mapFirst, mapSecond, mapThird, mapFourth, mapFifth))))


      // won't produce any domain error
      def ignoreDomainErrors[B >: A](f: Either5Error[E1, E2, E3, E4, E5] => ZIO[R, FAULT, B]): ZIO[R, FAULT, B] =
        effect.catchAll((failure: FaultOr[FAULT, Either5Error[E1, E2, E3, E4, E5]]) => failure.cause
                                                                                              .fold(throwable => ZIO.fail(throwable), // Fault must be propagated
                                                                                                    (errors: Either5Error[E1, E2, E3, E4, E5]) => f(errors)))

      def catchDomainErrors[B >: A, E6 <: SingleDomainError](f: Either5Error[E1, E2, E3, E4, E5] => ZIO[R, FaultOr[FAULT, _ >: E6], B]): ZIO[R, FaultOr[FAULT, _ >: E6], B] =
        effect.catchAll((failure: FaultOr[FAULT, Either5Error[E1, E2, E3, E4, E5]]) => failure.cause
                                                                                              .fold(throwable => ZIO.fail(FaultOr.makeFault[FAULT, E6](throwable)), // Fault must be propagated
                                                                                                    (errors: Either5Error[E1, E2, E3, E4, E5]) => f(errors)))

      def catchDomainErrorsMake1[B >: A, E6 <: SingleDomainError](f: Either5Error[E1, E2, E3, E4, E5] => ZIO[R, FaultOr[FAULT, E6], B]): ZIO[R, FaultOr[FAULT, E6], B] =
        effect.catchAll((failure: FaultOr[FAULT, Either5Error[E1, E2, E3, E4, E5]]) => failure.cause
                                                                                              .fold(throwable => ZIO.fail(FaultOr.makeFault[FAULT, E6](throwable)), // Fault must be propagated
                                                                                                    (errors: Either5Error[E1, E2, E3, E4, E5]) => f(errors)))

      def catchDomainErrorsMake2[B >: A, E6 <: SingleDomainError, E7 <: SingleDomainError](f: Either5Error[E1, E2, E3, E4, E5] => ZIO[R, FaultOr[FAULT, Either2Error[E6, E7]], B]): ZIO[R, FaultOr[FAULT, Either2Error[E6, E7]], B] =
        effect.catchAll((failure: FaultOr[FAULT, Either5Error[E1, E2, E3, E4, E5]]) => failure.cause
                                                                                              .fold(throwable => ZIO.fail(FaultOr.makeFault[FAULT, Either2Error[E6, E7]](throwable)), // Fault must be propagated
                                                                                                    (errors: Either5Error[E1, E2, E3, E4, E5]) => f(errors)))

      def catchDomainErrorsMake3[B >: A, E6 <: SingleDomainError, E7 <: SingleDomainError, E8 <: SingleDomainError](f: Either5Error[E1, E2, E3, E4, E5] => ZIO[R, FaultOr[FAULT, Either3Error[E6, E7, E8]], B]): ZIO[R, FaultOr[FAULT, Either3Error[E6, E7, E8]], B] =
        effect.catchAll((failure: FaultOr[FAULT, Either5Error[E1, E2, E3, E4, E5]]) => failure.cause
                                                                                              .fold(throwable => ZIO.fail(FaultOr.makeFault[FAULT, Either3Error[E6, E7, E8]](throwable)), // Fault must be propagated
                                                                                                    (errors: Either5Error[E1, E2, E3, E4, E5]) => f(errors)))

      def catchDomainErrorsMake4[B >: A, E6 <: SingleDomainError, E7 <: SingleDomainError, E8 <: SingleDomainError, E9 <: SingleDomainError](f: Either5Error[E1, E2, E3, E4, E5] => ZIO[R, FaultOr[FAULT, Either4Error[E6, E7, E8, E9]], B]): ZIO[R, FaultOr[FAULT, Either4Error[E6, E7, E8, E9]], B] =
        effect.catchAll((failure: FaultOr[FAULT, Either5Error[E1, E2, E3, E4, E5]]) => failure.cause
                                                                                              .fold(throwable => ZIO.fail(FaultOr.makeFault[FAULT, Either4Error[E6, E7, E8, E9]](throwable)), // Fault must be propagated
                                                                                                    (errors: Either5Error[E1, E2, E3, E4, E5]) => f(errors)))

      def catchDomainErrorsMake5[B >: A, E6 <: SingleDomainError, E7 <: SingleDomainError, E8 <: SingleDomainError, E9 <: SingleDomainError, E10 <: SingleDomainError](f: Either5Error[E1, E2, E3, E4, E5] => ZIO[R, FaultOr[FAULT, Either5Error[E6, E7, E8, E9, E10]], B]): ZIO[R, FaultOr[FAULT, Either5Error[E6, E7, E8, E9, E10]], B] =
        effect.catchAll((failure: FaultOr[FAULT, Either5Error[E1, E2, E3, E4, E5]]) => failure.cause
                                                                                              .fold(throwable => ZIO.fail(FaultOr.makeFault[FAULT, Either5Error[E6, E7, E8, E9, E10]](throwable)), // Fault must be propagated
                                                                                                    (errors: Either5Error[E1, E2, E3, E4, E5]) => f(errors)))

      def onDomainErrors[EE1 <: SingleDomainError](alternative: ZIO[R, FaultOr[FAULT, _ >: EE1], A]): ZIO[R, FaultOr[FAULT, _ >: EE1], A] =
        effect.catchAll((f: FaultOr[FAULT, Either5Error[E1, E2, E3, E4, E5]]) =>
                          if (f.cause.isLeft) effect.asInstanceOf[zio.ZIO[R, FaultOr[FAULT, _ >: EE1], A]]
                          else alternative
                        )

      def onDomainErrorsMake1[EE1 <: SingleDomainError](alternative: ZIO[R, FaultOr[FAULT, EE1], A]): ZIO[R, FaultOr[FAULT, EE1], A] =
        effect.catchAll((f: FaultOr[FAULT, Either5Error[E1, E2, E3, E4, E5]]) =>
                          if (f.cause.isLeft) effect.asInstanceOf[zio.ZIO[R, FaultOr[FAULT, EE1], A]]
                          else alternative
                        )

      def onDomainErrorsMake2[EE1 <: SingleDomainError, EE2 <: SingleDomainError](alternative: ZIO[R, FaultOr[FAULT, Either2Error[EE1, EE2]], A]): ZIO[R, FaultOr[FAULT, Either2Error[EE1, EE2]], A] =
        effect.catchAll((f: FaultOr[FAULT, Either5Error[E1, E2, E3, E4, E5]]) =>
                          if (f.cause.isLeft) effect.asInstanceOf[zio.ZIO[R, FaultOr[FAULT, Either2Error[EE1, EE2]], A]]
                          else alternative
                        )

      def onDomainErrorsMake3[EE1 <: SingleDomainError, EE2 <: SingleDomainError, EE3 <: SingleDomainError](alternative: ZIO[R, FaultOr[FAULT, Either3Error[EE1, EE2, EE3]], A]): ZIO[R, FaultOr[FAULT, Either3Error[EE1, EE2, EE3]], A] =
        effect.catchAll((f: FaultOr[FAULT, Either5Error[E1, E2, E3, E4, E5]]) =>
                          if (f.cause.isLeft) effect.asInstanceOf[zio.ZIO[R, FaultOr[FAULT, Either3Error[EE1, EE2, EE3]], A]]
                          else alternative
                        )

      def onDomainErrorsMake4[EE1 <: SingleDomainError, EE2 <: SingleDomainError, EE3 <: SingleDomainError, EE4 <: SingleDomainError](alternative: ZIO[R, FaultOr[FAULT, Either4Error[EE1, EE2, EE3, EE4]], A]): ZIO[R, FaultOr[FAULT, Either4Error[EE1, EE2, EE3, EE4]], A] =
        effect.catchAll((f: FaultOr[FAULT, Either5Error[E1, E2, E3, E4, E5]]) =>
                          if (f.cause.isLeft) effect.asInstanceOf[zio.ZIO[R, FaultOr[FAULT, Either4Error[EE1, EE2, EE3, EE4]], A]]
                          else alternative
                        )

      def onDomainErrorsMake5[EE1 <: SingleDomainError, EE2 <: SingleDomainError, EE3 <: SingleDomainError, EE4 <: SingleDomainError, EE5 <: SingleDomainError](alternative: ZIO[R, FaultOr[FAULT, Either5Error[EE1, EE2, EE3, EE4, EE5]], A]): ZIO[R, FaultOr[FAULT, Either5Error[EE1, EE2, EE3, EE4, EE5]], A] =
        effect.catchAll((f: FaultOr[FAULT, Either5Error[E1, E2, E3, E4, E5]]) =>
                          if (f.cause.isLeft) effect.asInstanceOf[zio.ZIO[R, FaultOr[FAULT, Either5Error[EE1, EE2, EE3, EE4, EE5]], A]]
                          else alternative
                        )
    }
  }
}