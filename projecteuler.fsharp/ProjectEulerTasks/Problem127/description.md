## abc-hits
### Problem 127

The radical of $n$, $rad(n)$, is the product of distinct prime factors of $n$. For example, $504 = 2^3 \times 3^2 \times 7$, so $rad(504) = 2 \times 3 \times 7$.

We shall define the triplet of positive integers $(a, b, c)$ to be an abc-hit if:
1. $gcd(a, b) = gcd(a, c) = gcd(b, c) = 1$
1. $a < b$
1. $a + b = c$
1. $rad(abc) < c$

For example, $(5, 27, 32)$ is an abc-hit, because:
1. $gcd(5, 27) = gcd(5, 32) = gcd(27, 32) = 1$
1. $5 < 27$
1. $5 + 27 = 32$
1. $rad(5 \times 27 \times 32) = rad(4320) = 30 < 32$

It turns out that abc-hits are quite rare and there are only thirty-one abc-hits for $c < 1000$, with $\sum c = 12523$.

Find $\sum c$ for $c < 120000$.