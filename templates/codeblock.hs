{-# LANGUAGE ViewPatterns #-}
{-@ LIQUID "--no-termination" @-}

module CodeBlock where

import Text.Pandoc.JSON
import Text.Pandoc
import Data.Char (isSpace)
import Data.List
import Data.Monoid (mempty)
import Debug.Trace
import Text.Printf (printf)

main :: IO ()
main = toJSONFilter txBlock

txBlock :: Block -> IO Block
txBlock (CodeBlock (id, classes, namevals) contents)
  = return (CodeBlock (id, classes, namevals) contents')
    where
      contents' = trace msg contents
      msg       = ("CodeBlock:" ++ "id = " ++ show id ++ ", classes=" ++ show classes)

--  EXAMPLE 
doInclude :: Block -> IO Block
doInclude cb@(CodeBlock (id, classes, namevals) contents) =
  case lookup "include" namevals of
       Just f     -> return . (CodeBlock (id, classes, namevals)) =<< readFile f
       Nothing    -> return cb
doInclude x = return x

