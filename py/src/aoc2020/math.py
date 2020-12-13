import math


def egcd(a, b):
    if not b:
        return 1, 0, a
    q, r = a // b, a % b
    s, t, g = egcd(b, r)
    return t, s - q * t, g


def crt(r1, q1, r2, q2):
    q3 = math.lcm(q1, q2)
    t, _, g = egcd(q1 + q2, q3)
    r3 = (r1 * q2 + r2 * q1) * t
    assert not r3 % g
    r3 = r3 // g % q3
    if (r3 < 0) != (q3 < 0):
        r3 += q3
    return r3, q3
