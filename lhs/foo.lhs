---
title: This is my thing
---

<div class="hidden">
\begin{code}
fib  :: Int -> Int
incr :: Int -> Int
\end{code}
</div>

OK

<button class="btn btn-xs btn-link" type="button" style="color:green; z-index:1">
<span class="glyphicon glyphicon-ok"></span>
</button>

ERROR 

<button class="btn btn-xs btn-link" type="button" style="color:red; z-index:1">
<span class="glyphicon glyphicon-remove"></span>
</button>
 
CRASH

<button class="btn btn-xs btn-link" type="button" style="color:orange; z-index:1">
<span class="glyphicon glyphicon-fire"></span>
</button>
 

<button class="btn btn-link btn-success" type="button" style="color:blue">
<span class="glyphicon glyphicon-search"></span>
</button>


This is an <i class="icon-play"></i> icon.

And now, this is an <i class="glyphicon glyphicon-search"></i> icon.

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



