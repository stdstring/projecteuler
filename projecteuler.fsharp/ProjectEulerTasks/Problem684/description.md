﻿## Inverse Digit Sum
### Problem 684

Define $s(n)$ to be the smallest number that has a digit sum of n. For example $s(10) = 19$.

Let $S(k) = \displaystyle\sum_{n=1}^k s(n)$. You are given $S(20) = 1074$.

Further let $f_i$ be the Fibonacci sequence defined by $f_0 = 0$, $f_1 = 1$ and $f_i = f_{i-1} + f_{i-2}$ for all $i \geq 2$.

Find $\displaystyle\sum_{i=2}^{90} S(f_i)$. Give your answer modulo **1 000 000 007**.
