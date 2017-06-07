{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc
    ) where

import qualified Data.Text as T

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data HTMLPage = HTMLPage { htmlPageContent :: T.Text
                         }
