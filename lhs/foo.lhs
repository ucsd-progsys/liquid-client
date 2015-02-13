---
title: This is my thing
---


<button class="btn btn-info" type="button>Check</button>


<div class="codeblock">
{-@ fib :: Nat -> Nat @-}
fib     :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)
</div>



<div id="program-pane" class="welleditor" style="background:#fff">
  <div class="form-horizontal">
    <button class="btn btn-info" type="button" ng-show="isUnknown" ng-click="verifySource()">Check</button>
    <button class="btn"             type="button" ng-show="isChecking" ng-click="verifySource()">Verifying...</button>
    <button class="btn btn-success" type="button" ng-show="isSafe">Safe</button>
    <button class="btn btn-danger"  type="button" ng-show="isUnsafe">Unsafe</button>
    <button class="btn btn-warning" type="button" ng-show="isError">Error!</button>
    <button class="btn btn-warning" type="button" ng-show="isCrash">Crash!</button>

    <div style="float:right" class="input-prepend input-append">
       <button class="btn" type="button" ng-click="makePermalink()">Permalink: {{sourceFileName}}</button>
       <!-- <button class="btn" type="button" ng-click="toggleFullScreen()">{{embiggen}}</i></button> -->
    </div>
  </div> 
  <br>
  <div id="program" style="height: 85%"> 
  -- Wait for it ...
  </div>
</div>

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



