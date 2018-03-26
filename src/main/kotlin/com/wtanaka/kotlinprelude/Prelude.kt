@file:Suppress("TooManyFunctions")

package com.wtanaka.kotlinprelude

/**
 * Boolean type.
 */
sealed class Bool {
    /**
     * False.
     */
    object False : Bool()

    /**
     * True.
     */
    object True : Bool()
}

/**
 * Ordering.
 */
sealed class Ordering {
    /**
     * Less than.
     */
    object LT : Ordering()

    /**
     * Equal.
     */
    object EQ : Ordering()

    /**
     * Greater than.
     */
    object GT : Ordering()
}

/**
 * Boolean "and".
 */
infix fun Bool.`&&`(other: Bool) = this == Bool.True && other == Bool.True

/**
 * Boolean "or".
 */
infix fun Boolean.`||`(other: Boolean) = this || other

/**
 * Boolean "not".
 */
fun not(a: Boolean): Boolean = !a

@Suppress("TopLevelPropertyNaming")
const val otherwise: Boolean = true

/**
 * The maybe function takes a default value, a function, and a Maybe value.
 * If the Maybe value is Nothing, the function returns the default value.
 * Otherwise, it applies the function to the value inside the Just and
 * returns the result.
 */
fun <A, B> maybe(default: B): (((A) -> B) -> ((Maybe<A>) -> B)) = { function ->
    { maybeA ->
        when (maybeA) {
            is Maybe.Nothing -> default
            is Maybe.Just -> function(maybeA.t)
        }
    }
}

/**
 * The Either type represents values with two possibilities: a value of type
 * Either a b is either Left a or Right b.
 *
 * The Either type is sometimes used to represent a value which is either
 * correct or an error; by convention, the Left constructor is used to hold
 * an error value and the Right constructor is used to hold a correct value
 * (mnemonic: "right" also means "correct").
 */
sealed class Either<A, B> {
    /**
     * Usually used to represent an error value.
     */
    data class Left<A, B>(val l: A) : Either<A, B>()

    /**
     * Usually used to represent a correct ("right") value.
     */
    data class Right<A, B>(val r: B) : Either<A, B>()
}

/**
 * Case analysis for the Either type. If the value is Left a, apply the first
 * function to a; if it is Right b, apply the second function to b.
 */
fun <A, B, C> either(leftFn: ((A) -> C)):
    (((B) -> C) -> ((Either<A, B>) -> C)) = { rightFn ->
    { input ->
        when (input) {
            is Either.Left -> leftFn(input.l)
            is Either.Right -> rightFn(input.r)
        }
    }
}

/**
 * Extract the first component of a pair.
 */
fun <A, B> fst(pair: Pair<A, B>): A = pair.first

/**
 * Extract the second component of a pair.
 */
fun <A, B> snd(pair: Pair<A, B>): B = pair.second

/**
 * curry converts an uncurried function to a curried function.
 */
fun <A, B, C> curry(func: ((Pair<A, B>) -> C)): ((A) -> ((B) -> C)) = { a ->
    { b -> func(Pair(a, b)) }
}

/**
 * uncurry converts a curried function to a function on pairs.
 */
fun <A, B, C> uncurry(func: ((A) -> ((B) -> C))):
    ((Pair<A, B>) -> C) = { pair ->
    func(pair.first)(pair.second)
}

/**
 * The Eq class defines equality (==) and inequality (/=). All the basic
 * datatypes exported by the Prelude are instances of Eq, and Eq may be
 * derived for any datatype whose constituents are also instances of Eq.
 *
 * Minimal complete definition: either == or /=.
 */
interface Eq<A> {
    /**
     * Equality.
     */
    fun equalequal(@Suppress("UnusedPrivateMember") a: Eq<A>): Bool
}

/**
 * The Ord class is used for totally ordered datatypes.
 *
 * Instances of Ord can be derived for any user-defined datatype whose
 * constituent types are in Ord. The declared order of the constructors in
 * the data declaration determines the ordering in derived Ord instances. The
 * Ordering datatype allows a single comparison to determine the precise
 * ordering of two objects.
 *
 * Minimal complete definition: either compare or <=. Using compare can be
 * more efficient for complex types.
 */
abstract class Ord<A : Eq<A>> {
    /**
     * compare from Ord.
     */
    abstract fun compare(a: A): Ordering

    /**
     * compareTo.
     */
    fun compareTo(a: A): Int = when (this.compare(a)) {
        Ordering.GT -> +1
        Ordering.EQ -> 0
        Ordering.LT -> -1
    }
}

/**
 * Class Enum defines operations on sequentially ordered types.
 *
 * The enumFrom... methods are used in Haskell's translation of arithmetic
 * sequences.
 *
 * Instances of Enum may be derived for any enumeration type (types whose
 * constructors have no fields). The nullary constructors are assumed to be
 * numbered left-to-right by fromEnum from 0 through n-1. See Chapter 10 of
 * the Haskell Report for more details.
 *
 * For any type that is an instance of class Bounded as well as Enum, the
 * following should hold:
 *
 * The calls succ maxBound and pred minBound should result in a runtime error.
 * fromEnum and toEnum should give a runtime error if the result value is not
 * representable in the result type. For example, toEnum 7 :: Bool is an error.
 * enumFrom and enumFromThen should be defined with an implicit bound, thus:
 * enumFrom     x   = enumFromTo     x maxBound
 * enumFromThen x y = enumFromThenTo x y bound
 * where
 * bound | fromEnum y >= fromEnum x = maxBound
 * | otherwise                = minBound
 */
interface Enum<A> {
    /**
     * Convert from an Int.
     */
    fun toEnum(@Suppress("UnusedPrivateMember") int: Int): Enum<A>

    /**
     * Convert to an Int. It is implementation-dependent what fromEnum
     * returns when applied to a value that is too large to fit in an Int.
     */
    fun fromEnum(): Int

    /**
     * the successor of a value. For numeric types, succ adds 1.
     */
    fun succ(): Enum<A> = ({ x: Int -> toEnum(x) }
        _ { x: Int -> x + 1 }
        _ { x: Enum<A> -> x.fromEnum() }).invoke(this)

    /**
     * the predecessor of a value. For numeric types, pred subtracts 1.
     */
    fun pred(): Enum<A> = ({ x: Int -> toEnum(x) }
        _ { x: Int -> x - 1 }
        _ { x: Enum<A> -> x.fromEnum() }).invoke(this)
}

/**
 * The Bounded class is used to name the upper and lower limits of a type.
 * Ord is not a superclass of Bounded since types that are not totally
 * ordered may also have upper and lower bounds.
 *
 * The Bounded class may be derived for any enumeration type; minBound is the
 * first constructor listed in the data declaration and maxBound is the last.
 * Bounded may also be derived for single-constructor datatypes whose
 * constituent types are in Bounded.
 */
interface Bounded<A> {
    @Suppress("UndocumentedPublicFunction")
    fun minBound(): A

    @Suppress("UndocumentedPublicFunction")
    fun maxBound(): A
}

/**
 * A fixed-precision integer type with at least the range [-2^29 .. 2^29-1].
 * The exact range for a given implementation can be determined by using
 * minBound and maxBound from the Bounded class.
 */
class PInt(val i: Int) : Enum<PInt>, Bounded<PInt>, Eq<PInt> {
    override fun equalequal(a: Eq<PInt>): Bool = if (a is PInt) {
        when (this.i == a.i) {
            true -> Bool.True
            false -> Bool.False
        }
    } else {
        Bool.False
    }

    override fun toEnum(int: Int): Enum<PInt> = PInt(int)

    override fun minBound(): PInt = PInt(-0x20000000)

    @Suppress("MagicNumber")
    override fun maxBound(): PInt = PInt(0x1FFFFFFF)

    override fun fromEnum(): Int = i
}

/**
 * Basic numeric class.
 */
interface Num<A> {
    /**
     * Plus.
     */
    operator fun plus(a: Num<A>): Num<A>

    /**
     * Minus.
     */
    operator fun minus(y: Num<A>): Num<A> = this + negate(y)

    /**
     * Times.
     */
    operator fun times(a: Num<A>): Num<A>

    /**
     * Unary.
     */
    operator fun unaryMinus(): Num<A>

    /**
     * Absolute value.
     */
    fun abs(): Num<A>

    /**
     * Sign of a number. The functions abs and signum should satisfy the law:
     *
     * abs x * signum x == x
     * For real numbers, the signum is either -1 (negative), 0 (zero) or 1
     * (positive).
     */
    fun signum(): Num<A>

    /**
     * Conversion from an Integer. An integer literal represents the
     * application of the function fromInteger to the appropriate value of
     * type Integer, so such literals have type (Num a) => a.
     */
    fun fromInteger(@Suppress("UnusedPrivateMember") a: PInt): Num<A>

    /**
     * Unary negation.
     */
    fun negate() = this.fromInteger(PInt(0)) - this
}

/**
 * Unary negation.
 */
fun <A> negate(a: Num<A>) = a.negate()

/**
 * Sign of a number. The functions abs and signum should satisfy the law:
 *
 * abs x * signum x == x
 * For real numbers, the signum is either -1 (negative), 0 (zero) or 1
 * (positive).
 */
fun <A> signum(a: Num<A>) = a.signum()

/**
 * Real value.
 */
@Suppress("EmptyClassBlock")
interface Real<A> : Num<A> { //, Ord<A>() {
//    fun toRational(): Rational
}

/**
 * Integral numbers, supporting integer division.
 */
interface Integral<A> : Real<A>, Enum<A> {
    /**
     * integer division truncated toward zero.
     */
//    infix fun quot(d: Integral<A>): Integral<A> = this.quotRem(d).first

    /**
     * integer remainder, satisfying
     *
     * "(x `quot` y)*y + (x `rem` y) == x".
     */
//    infix fun rem(d: Integral<A>): Integral<A> = this.quotRem(d).second

//    /**
//     * integer division truncated toward negative infinity.
//     */
//    infix fun div(d: Integral<A>): Integral<A> = this.divMod(d).first
//
//    /**
//     * integer modulus, satisfying
//     *
//     * "(x `div` y)*y + (x `mod` y) == x".
//     */
//    infix fun mod(d: Integral<A>): Integral<A> = this.divMod(d).second
//
//    /**
//     * simultaneous quot and rem.
//     */
//    fun quotRem(d: Integral<A>): Pair<Integral<A>, Integral<A>>
//
//    /**
//     * simultaneous div and mod.
//     */
//    fun divMod(d: Integral<A>): Pair<Integral<A>, Integral<A>> =
//        com.wtanaka.kotlinprelude.quotRem(this)(d).let { qr ->
//            val r = qr.second
//            @Suppress("UnsafeCast")
//            return if (signum(r) == negate(signum(d))) Pair(
//                qr.first - fromInteger(PInt(1)), r + d) as
//                Pair<Integral<A>, Integral<A>> else qr
//        }

    /**
     * conversion to Integer.
     */
    fun toInteger(): PInt
}

/**
 * simultaneous quot and rem.
 */
//fun <A> quotRem(n: Integral<A>) = n::quotRem

/**
 * Fractional numbers, supporting real division.
 */
interface Fractional<A> : Num<A> {
    /**
     * fractional division.
     */
    @Suppress("UnsafeCast")
    operator fun div(y: Fractional<A>): Fractional<A> =
        (this * recip(y)) as Fractional<A>

    /**
     * reciprocal fraction.
     */
    fun recip(): Fractional<A> {
        val temp: Num<A> = this.fromInteger(PInt(1))
        @Suppress("UnsafeCast")
        val frac: Fractional<A> = temp as Fractional<A>
        return frac / this
    }
}

/**
 * reciprocal fraction.
 */
fun <A> recip(a: Fractional<A>): Fractional<A> = a.recip()

/**
 * result of a - b.
 */
fun subtract(a: Int): ((Int) -> Int) = { b -> a - b }

/**
 * result of a - b.
 */
fun subtract(a: Long): ((Long) -> Long) = { b -> a - b }

/**
 * result of a - b.
 */
fun subtract(a: Double): ((Double) -> Double) = { b -> a - b }

/**
 * check if i is even.
 */
fun even(i: Int) = i % 2 == 0

/**
 * check if i is odd.
 */
fun odd(i: Int) = i % 2 != 0

/**
 * gcd x y is the non-negative factor of both x and y of which every common
 * factor of x and y is also a factor; for example gcd 4 2 = 2, gcd (-4) 6 =
 * 2, gcd 0 4 = 4. gcd 0 0 = 0. (That is, the common divisor that is
 * "greatest" in the divisibility preordering.)
 */
fun gcd(i: Int): ((Int) -> Int) = { j ->
    var a = i
    var b = j
    while (b != 0) {
        var t = b
        b = a % b
        a = t
    }
    a
}

/**
 * lcm x y is the smallest positive integer that both x and y divide.
 */
fun lcm(i: Int): ((Int) -> Int) = { j -> i * j / gcd(i)(j) }

/**
 * Identity function.
 */
@Suppress("FunctionMinLength")
fun <A> id(a: A): A = a

/**
 * const x is a unary function which evaluates to x for all inputs.
 */
fun <A, B> const(a: A): ((B) -> A) = { a }

/**
 * Function composition.
 */
infix fun <A, B, C> ((B) -> C).`_`(inner: ((A) -> B)): (A) -> C = {
    this.invoke(inner(it))
}

/**
 * flip f takes its (first) two arguments in the reverse order of f.
 */
fun <A, B, C> flip(func: ((A) -> ((B) -> C))): ((B) -> ((A) -> C)) = { b ->
    { a ->
        func(a)(b)
    }
}

/**
 * until p f yields the result of applying f until p holds.
 */
fun <A> until(predicate: ((A) -> Boolean)):
    (((A) -> A) -> ((A) -> A)) = { func ->
    { input ->
        when (predicate(input)) {
            true -> input
            false -> until(predicate)(func)(func(input))
        }
    }
}

/**
 * asTypeOf is a type-restricted version of const. It is usually used as an
 * infix operator, and its typing forces its first argument (which is usually
 * overloaded) to have the same type as the second.
 */
fun <A> asTypeOf(first: A): ((A) -> A) = { first }

/**
 * map(f)(xs) is the list obtained by applying f to each element of xs, i.e.,
 *
 * map f [x1, x2, ..., xn] == [f x1, f x2, ..., f xn]
 * map f [x1, x2, ...] == [f x1, f x2, ...]
 * .
 */
fun <A, B> map(func: ((A) -> B)): ((Iterable<A>) -> Iterable<B>) = { input ->
    input.map(func)
}

/**
 * filter, applied to a predicate and a list, returns the list of those
 * elements that satisfy the predicate; i.e.,
 *
 * filter p xs = [ x | x <- xs, p x]
 * .
 */
fun <A> filter(predicate: ((A) -> Boolean)): ((Iterable<A>) -> Iterable<A>) = {
    it.filter(predicate)
}

/**
 * Extract the first element of a list, which must be non-empty.
 */
fun <A> head(list: Iterable<A>): A = list.first()

/**
 * Extract the first element of a list, which must be non-empty.
 */
fun <A> head(list: Sequence<A>): A = list.first()

/**
 * Extract the last element of a list, which must be finite and non-empty.
 */
fun <A> last(list: Iterable<A>): A = list.last()

/**
 * Extract the elements after the head of a list, which must be non-empty.
 */
fun <A> tail(list: Iterable<A>): Iterable<A> = list.drop(1)

/**
 * Extract the elements after the head of a list, which must be non-empty.
 */
fun <A> tail(list: Sequence<A>): Sequence<A> = list.drop(1)
//fun <A> init(list: Iterable<A>): Iterable<A> = TODO()
/**
 * Returns the size/length of a finite structure as an Int.
 */
fun <A> length(list: Iterable<A>): Int = 1 + length(tail(list))

/**
 * List index (subscript) operator, starting from 0. It is an instance of the
 * more general genericIndex, which takes an index of any integral type.
 */
infix fun <A> Iterable<A>.`!!`(index: Int): A = this.elementAt(index)

/**
 * List index (subscript) operator, starting from 0. It is an instance of the
 * more general genericIndex, which takes an index of any integral type.
 */
infix fun <A> Sequence<A>.`!!`(index: Int): A = this.elementAt(index)

/**
 * reverse(xs) returns the elements of xs in reverse order. xs must be finite.
 */
fun <A> reverse(list: Iterable<A>):
    Iterable<A> = reverse(tail(list)) + head(list)

/**
 * reverse(xs) returns the elements of xs in reverse order. xs must be finite.
 */
fun <A> reverse(list: Sequence<A>):
    Sequence<A> = reverse(tail(list)) + head(list)

/**
 * and returns the conjunction of a container of Booleans. For the result to
 * be True, the container must be finite; False, however, results from a
 * False value finitely far from the left end.
 */
fun and(container: Iterable<Boolean>): Boolean = container.all { it }

/**
 * or returns the disjunction of a container of Bools. For the result to be
 * False, the container must be finite; True, however, results from a True
 * value finitely far from the left end.
 */
@Suppress("FunctionMinLength")
fun or(container: Iterable<Boolean>): Boolean = container.any { it }

/**
 * Determines whether any element of the structure satisfies the predicate.
 */
fun <A> any(predicate: ((A) -> Boolean)): ((Iterable<A>) -> Boolean) = {
    it.any(predicate)
}

/**
 * Determines whether all elements of the structure satisfy the predicate.
 */
fun <A> all(predicate: ((A) -> Boolean)): ((Iterable<A>) -> Boolean) = {
    it.all(predicate)
}

/**
 * The concatenation of all the elements of a container of lists.
 */
fun <A> concat(containerOfLists: Iterable<Iterable<A>>):
    Iterable<A> = containerOfLists.fold(
    listOf(), { acc, elem ->
    acc + elem
})

/**
 * Map a function over all the elements of a container and concatenate the
 * resulting lists.
 */
fun <A, B> concatMap(mapFn: ((A) -> Iterable<B>)):
    ((Iterable<A>) -> Iterable<B>) = { container ->
    concat(container.map(mapFn))
}

/**
 * scanl is similar to foldl, but returns a list of successive reduced values
 * from the left.
 */
fun <A, B> scanl(reducer: (B) -> ((A) -> B)):
    (B) -> ((Iterable<A>) -> Iterable<B>) = { accum ->
    { container ->
        listOf(accum) +
            scanl(reducer)(
                reducer(accum)(head(container)))(
                tail(container))
    }
}
//fun <A> scanl1(reducer: ((A) -> ((A) -> A))):
// ((Iterable<A>) -> Iterable<A>) = TODO()
//fun <A, B> scanr(reducer: ((A) -> ((B) -> B))): Nothing = TODO()
//fun <A> scanr1(reducer: ((A) -> ((A) -> A))): Nothing = TODO()
/**
 * iterate f x returns an infinite list of repeated applications of f to x:
 *
 * iterate f x == [x, f x, f (f x), ...]
 * .
 */
fun <A : Any> iterate(f: ((A) -> A)): ((A) -> Sequence<A>) =
    { x -> generateSequence(x, f) }

/**
 * repeat x is an infinite list, with x the value of every element.
 */
fun <A : Any> repeat(x: A): Sequence<A> = generateSequence { x }

/**
 * replicate(n)(x) is a list of length n with x the value of every element.
 * It is an instance of the more general genericReplicate, in which n may be
 * of any integral type.
 */
fun <A> replicate(n: Int): ((A) -> Sequence<A>) = { x ->
    sequenceOf(x) + replicate<A>(n - 1)(x)
}

private class LazyPlus<T>(
    var first: Sequence<T>,
    var second: () -> Sequence<T>
) :
    Sequence<T> {
    @Suppress("ComplexMethod")
    override fun iterator(): Iterator<T> = object : Iterator<T> {
        var isBeforeStart = true
        var thisIterator: Iterator<T>? = null
        var otherIterator: Iterator<T>? = null

        @Suppress("UnusedPrivateMember")
        private fun preCall() {
            if (isBeforeStart) {
                isBeforeStart = false
                thisIterator = first.iterator()
            }
            val it1 = thisIterator
            if (it1 != null && !it1.hasNext()) {
                thisIterator = null
                otherIterator = second().iterator()
            }
            val it2 = otherIterator
            if (it2 != null && !it2.hasNext()) {
                otherIterator = null
            }

            assert(!isBeforeStart)
            assert(!(thisIterator != null && otherIterator != null))
        }

        @Suppress("UnusedPrivateMember")
        private fun activeIterator(): Iterator<T>? = when {
            thisIterator != null -> thisIterator
            otherIterator != null -> otherIterator
            else -> null
        }

        override fun next(): T {
            preCall()
            val active = activeIterator()
            return when {
                active != null -> active.next()
                else -> throw NoSuchElementException()
            }
        }

        override fun hasNext(): Boolean {
            preCall()
            val active = activeIterator()
            return when {
                active != null -> active.hasNext()
                else -> false
            }
        }
    }
}

private infix fun <T> Sequence<T>.lazyPlus(otherSeq: () -> Sequence<T>) =
    LazyPlus<T>(this, otherSeq)

/**
 * cycle ties a finite list into a circular one, or equivalently, the
 * infinite repetition of the original list. It is the identity on infinite
 * lists.
 */
fun <A> cycle(seq: Sequence<A>): Sequence<A> = seq lazyPlus { cycle(seq) }

/**
 * take(n), applied to a list xs, returns the prefix of xs of length n, or xs
 * itself if n > length xs:
 *
 * take 5 "Hello World!" == "Hello"
 * take 3 [1,2,3,4,5] == [1,2,3]
 * take 3 [1,2] == [1,2]
 * take 3 [] == []
 * take (-1) [1,2] == []
 * take 0 [1,2] == []
 * .
 */
fun <A> take(n: Int): ((Sequence<A>) -> Sequence<A>) = { xs ->
    when {
        n <= 0 -> emptySequence()
        else -> sequenceOf(head(xs)) + take<A>(n - 1)(tail(xs))
    }
}
