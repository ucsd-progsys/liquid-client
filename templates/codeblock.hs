{-# LANGUAGE OverloadedStrings #-}
{-@ LIQUID "--no-termination" @-}

module CodeBlock where

import System.Posix.Env
import Data.IORef
import Text.Pandoc.JSON
import Text.Pandoc
-- import Data.Char (isSpace)
import Data.List (isPrefixOf)
-- import Data.Monoid (mempty)
import Debug.Trace
import Text.Printf (printf)

import qualified Data.ByteString.Lazy as S
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.IO as TIO

import Data.Text.Template

main :: IO ()
main = do r     <- newIORef 0
          tpltF <- templateFile
          tplt  <- TIO.readFile tpltF
          toJSONFilter (txBlock tplt r)

templateFile :: IO FilePath
templateFile = do
  fo <- getEnv "PANDOC_CODETEMPLATE"
  case fo of
    Nothing -> return "templates/code.template"
    Just f  -> return f

txBlock :: T.Text -> IORef Int -> Block -> IO Block
txBlock t r (CodeBlock (id, classes, namevals) contents)
  | isCode classes
  = makeHtml t r False contents

txBlock t r (RawBlock (Format "latex") str)
  | Just contents <- isCommentCode str
  = makeHtml t r True contents

txBlock _ _ z
  = return z -- $ trace ("block:" ++ show z) z

isCode  = ("haskell" `elem`)

isCommentCode str
  | 4 <= length ls  = Just $ unlines ls''
  | otherwise       = Nothing
  where
    ls              = lines str
    (c1, ls' , c1') = snip ls
    (c2, ls'', c2') = snip ls'
    ok              = isCommentBlock c1 c1' && isCodeBlock c2 c2'

isCommentBlock c c' = isPrefixOf "\\begin{comment}" c && isPrefixOf "\\end{comment}" c'
isCodeBlock    c c' = isPrefixOf "\\begin{code}" c    && isPrefixOf "\\end{code}"    c'

-- "\\begin{code}","module Logic where","main :: IO ()","main = return ()","","","{-@ type TRUE  = {v:Bool | Prop v} @-}","{-@ type FALSE = {v:Bool | not (Prop v)} @-}","","","","{-@ invariant {v:[a] | size v >= 0} @-}","{-@ f :: x:Int -> {v:Int | v = f x} @-}","f :: Int -> Int","f = undefined","\\end{code}","\\end{comment}"]

{-@ snip :: xs:{[a] | 2 <= len xs} -> (a, {v:[a] | len v = len xs - 2}, a) @-}
snip (x:xs)  = (x, reverse xs', x')
  where
   (x',xs')  = fs (reverse xs)
   fs (x:xs) = (x, xs)

makeHtml t r hide contents
  = do n <- getCount r
       let contents'  = pad t n contents
       let contents'' = (if hide then wrapHide else id) contents'
       return $ RawBlock (Format "html") contents''

wrapHide :: String -> String
wrapHide s = unlines ["<div class=\"hidden\">", s, "</div>"]



getCount r = do n <- readIORef r
                writeIORef r (n+1)
                return n

pad :: T.Text -> Int -> String -> String
pad tplt n s   = L.unpack $ substitute tplt ctx
  where
    tn         = T.pack $ show n
    ts         = T.pack $ prefix ++ s
    prefix     = "" -- """-- block: " ++ show n ++ "\n"

    ctx        :: T.Text -> T.Text
    ctx "code" = ts
    ctx "id"   = tn
    ctx z      = z



--  EXAMPLE
doInclude :: Block -> IO Block
doInclude cb@(CodeBlock (id, classes, namevals) contents) =
  case lookup "include" namevals of
       Just f     -> return . (CodeBlock (id, classes, namevals)) =<< readFile f
       Nothing    -> return cb
doInclude x = return x
