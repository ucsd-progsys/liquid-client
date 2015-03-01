{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-@ LIQUID "--no-termination" @-}

module CodeBlock where

import Data.IORef
import Text.Pandoc.JSON
import Text.Pandoc
-- import Data.Char (isSpace)
-- import Data.List
-- import Data.Monoid (mempty)
import Debug.Trace
import Text.Printf (printf)
import System.Directory
import System.IO
  
import qualified Data.ByteString.Lazy as S
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.IO as TIO

import Data.Text.Template

main :: IO ()
main = do r    <- newIORef 0
          tplt <- TIO.readFile "web/templates/code.template"
          toJSONFilter (txBlock tplt r)
          
txBlock :: T.Text -> IORef Int -> Block -> IO Block
txBlock t r (CodeBlock (id, classes, namevals) contents)
  | isCode classes
  = do n <- getCount r
       let contents' = pad t n contents 
       return $ RawBlock (Format "html") contents'
       -- CodeBlock (id, classes, namevals) contents'
       
    where

txBlock _ _ z
  = return z -- $ trace ("block:" ++ show z) z

isCode  = ("haskell" `elem`)


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

