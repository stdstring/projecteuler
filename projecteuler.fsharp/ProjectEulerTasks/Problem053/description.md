## Combinatoric selections
### Problem 53

There are exactly ten ways of selecting three from five, 12345:
<p align="center">
123, 124, 125, 134, 135, 145, 234, 235, 245, and 345
</p>

In combinatorics, we use the notation, $\binom{5}{3} = 10$.

In general, $\binom{n}{k} = \frac{n!}{k!(n-k)!}$, where $r \leq n, n! = n \times (n - 1) \times ... \times 2 \times 1$, and $0! = 1$.

It is not until $n = 23$, that a value exceeds one-million: $\binom{23}{10} = 1144066$.

How many, not necessarily distinct, values of $\binom{n}{r}$ for $1 \leq n \leq 100$?