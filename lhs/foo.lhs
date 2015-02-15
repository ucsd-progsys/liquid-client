---
title: This is my thing
---

<div class="hidden">
\begin{code}
fib     :: Int -> Int
\end{code}
</div>

Here is the world famous **factorial** function. 

\begin{code}
{-@ fib :: Nat -> Nat @-}
fib 0   = 1
fib 1   = 1
fib n   = fib (n-1) + fib (n-2)
\end{code}


In the context of foo, I must report.

Bar
---

Please say it isn't so!

+ hello
+ you *must*
+ not **eat** a `burrito`

Baz
---

Please say it isn't so!


\begin{code}
{-@ fac :: Nat -> Nat @-}
fac   :: Int -> Int
fac 0 = 1
fac n = n * fac (n-1)
\end{code}



