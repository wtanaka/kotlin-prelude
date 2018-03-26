package com.wtanaka.kotlinhaskell.data

import com.wtanaka.kotlinprelude.Eq

/**
 * Rational numbers, with numerator and denominator of some Integral type.
 */
@Suppress("EmptyClassBlock")
interface Ratio<A> : Eq<A>
