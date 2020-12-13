package io.github.ephemient.aoc2020

import java.math.BigInteger
import java.math.BigInteger.ONE
import java.math.BigInteger.ZERO

fun egcd(a: BigInteger, b: BigInteger): Triple<BigInteger, BigInteger, BigInteger> {
    if (b == ZERO) {
        return Triple(ONE, ZERO, a)
    }
    val (q, r) = a.divideAndRemainder(b)
    val (s, t, g) = egcd(b, r)
    return Triple(t, s - q * t, g)
}

fun crt(
    r1: BigInteger,
    q1: BigInteger,
    r2: BigInteger,
    q2: BigInteger
): Pair<BigInteger, BigInteger> {
    val q3 = q1 / q1.gcd(q2) * q2
    val (t, _, g) = egcd(q1 + q2, q3)
    var (r3, zero) = ((r1 * q2 + r2 * q1) * t).divideAndRemainder(g)
    check(zero == ZERO)
    r3 %= q3
    if (r3.signum() * q3.signum() < 0) r3 += q3
    return Pair(r3, q3)
}
