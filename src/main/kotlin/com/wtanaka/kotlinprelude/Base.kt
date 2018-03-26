@file:Suppress("TooManyFunctions")

package com.wtanaka.kotlinprelude

/**
 * The Maybe type encapsulates an optional value. A value of type Maybe a
 * either contains a value of type a (represented as Just a), or it is empty
 * (represented as Nothing). Using Maybe is a good way to deal with errors
 * or exceptional cases without resorting to drastic measures such as error.
 *
 * The Maybe type is also a monad. It is a simple kind of error monad,
 * where all errors are represented by Nothing. A richer error monad can
 * be built using the Either type.
 *
 * TODO: deriving Eq, Ord
 */
sealed class Maybe<T> {
    /**
     * Represents an empty Maybe.
     */
    class Nothing<T> : Maybe<T>()

    /**
     * Represents a value of type T.
     */
    data class Just<T>(val t: T) : Maybe<T>()
}

/**
 * The class of semigroups (types with an associative binary operation).
 *
 * Instances should satisfy the associativity law:
 * @x '{}' (y '{}' z) = (x '{}' y) '{}' z@
 */
interface Semigroup<A> {
    /**
     * An associative operation.
     */
    infix fun `{}`(@Suppress("UnusedPrivateMember") a: Semigroup<A>):
        Semigroup<A>
}
//abstract class SemigroupImpl<A> : Semigroup<A> {
//    fun sconcat
//
//}
