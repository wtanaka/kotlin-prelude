package com.wtanaka.kotlinprelude

import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class PreludeTest {
    @Test
    fun testMaybe() {
        assertEquals(true, maybe<Int, Boolean>(false)(::odd)(Maybe.Just(3)))
        assertEquals(false, maybe<Int, Boolean>(false)(::odd)(Maybe.Nothing()))
    }

    @Test
    fun testEither() {
        val s: Either<String, Int> = Either.Left("foo")
        val n: Either<String, Int> = Either.Right(3)
        assertEquals(3, either<String, Int, Int>(String::length)({ it * 2 })(s))
        assertEquals(6, either<String, Int, Int>(String::length)({ it * 2 })(n))
    }

    @Test
    fun testCurry() {
        assertEquals(1, curry<Int, Int, Int>(::fst)(1)(2))
    }

    @Test
    fun testUncurry() {
        assertEquals(3,
                uncurry<Int, Int, Int>({ a ->
                    { b -> a + b }
                })(Pair(1, 2)))
    }

    @Test
    fun testOdd() {
        assertTrue(odd(3))
        assertFalse(odd(2))
        assertTrue(odd(1))
        assertFalse(odd(0))
        assertTrue(odd(-1))
        assertFalse(odd(-2))
    }

    @Test
    fun testIterate() {
        assertEquals("b", iterate<String>({ "o" })("b") `!!` 0)
        assertEquals("o", iterate<String>({ "o" })("b") `!!` 1)
        assertEquals("o", iterate<String>({ "o" })("b") `!!` 2)
    }

    @Test
    fun testRepeat() {
        assertEquals(7, repeat(7) `!!` 0)
        assertEquals(7, repeat(7) `!!` 1)
        assertEquals(7, repeat(7) `!!` 2)
        assertEquals(7, repeat(7) `!!` 999)
    }

    @Test
    fun testCycle() {
        assertEquals("1,2,3,1,2,3",
                cycle(sequenceOf(1, 2, 3)).take(6).joinToString(","))
    }
}