---
title: This is my thing
root: ".."
author: Ranjit Jhala
date: Sep 5th, 2014
liquidserver: "http://goto.ucsd.edu:8090/"
---

<div class="hidden">
\begin{code}
{-@ LIQUID "--short-names" @-}
module Foo where

fib  :: Int -> Int
incr :: Int -> Int
\end{code}
</div>

My personal favorite:

\begin{code}
{-@ incr :: Nat -> Nat @-}
incr x = x - 1
\end{code}




Here is the world famous **factorial** function. 

\begin{code}
{-@ fib :: Nat -> Nat @-}
fib 0   = 1
fib 1   = 1
fib n   = fib (n-1) + fib (n-2)
\end{code}


In the context of foo, I must report.

\begin{code}
append :: [a] -> [a] -> [a]
append []     ys = ys 
append (x:xs) ys = x : append xs ys
\end{code}


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



