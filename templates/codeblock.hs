{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-@ LIQUID "--no-termination" @-}

module CodeBlock where

import Data.IORef
import Text.Pandoc.JSON
import Text.Pandoc
-- import Data.Char (isSpace)
import Data.List (isSuffixOf, isPrefixOf)
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
  = makeCodeBlock t r True contents 
  -- = do n <- getCount r
  --      let contents' = pad t n contents False 
  --      return $ RawBlock (Format "html") contents'

txBlock t r b@(RawBlock (Format "latex") str)
  = maybe (return b) (makeCodeBlock t r False) (isCommentCode str)
        
  -- = do n <- getCount r
  --      let contents  = trimLines 2 str  
  --      let contents' = pad t n contents True
  --      return $ RawBlock (Format "html") contents'
       
txBlock _ _ z
  = return z -- $ trace ("block:" ++ show z) z

isCode             = ("haskell" `elem`)

isCommentCode str
  = do str'  <- stripPrefix commentPrefix str
       str'' <- stripSuffix commentSuffix str'
       return str''
    where
       commentPrefix = "\\begin{comment}\n\\begin{code}\n"
       commentSuffix = "\\end{code}\n\\end{comment}"

stripSuffix p s
  | isSuffixOf p s = Just $ take (length s - length p) s
  | otherwise      = Nothing
  
stripPrefix p s
  | isPrefixOf p s = Just $ drop (length p) s
  | otherwise      = Nothing
                                   
       
getCount r = do n <- readIORef r
                writeIORef r (n+1)
                return n

makeCodeBlock :: T.Text -> IORef Int -> Bool -> String -> IO Block 
makeCodeBlock t r hide contents 
  = do n <- getCount r 
       let contents' = pad t n hide contents 
       return $ RawBlock (Format "html") contents'

pad :: T.Text -> Int -> Bool -> String -> String                
pad tplt n h s = L.unpack $ substitute tplt ctx 
  where
    tn         = T.pack $ show n 
    ts         = T.pack $ prefix ++ s
    tb True    = T.pack "hidden"
    tb False   = T.pack ""
    prefix     = ""

    ctx        :: T.Text -> T.Text 
    ctx "code" = ts 
    ctx "id"   = tn 
    ctx "hide" = tb h
    ctx z      = z


    
--  EXAMPLE 
doInclude :: Block -> IO Block
doInclude cb@(CodeBlock (id, classes, namevals) contents) =
  case lookup "include" namevals of
       Just f     -> return . (CodeBlock (id, classes, namevals)) =<< readFile f
       Nothing    -> return cb
doInclude x = return x

