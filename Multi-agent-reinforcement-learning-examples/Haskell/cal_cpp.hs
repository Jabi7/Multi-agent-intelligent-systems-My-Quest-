{-# LANGUAGE ForeignFunctionInterface #-}

module Main where

import Foreign.C.Types

foreign import ccall "add" c_add :: CInt -> CInt -> CInt

add :: Int -> Int -> Int
add x y = fromIntegral $ c_add (fromIntegral x) (fromIntegral y)

main :: IO ()
main = putStrLn $ show $ add 1 2
