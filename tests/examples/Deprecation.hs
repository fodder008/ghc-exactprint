
module Deprecation where

{-# DEPRECATED foo
         ["This is a multi-line",
          "deprecation message",
          "for foo"] #-}
foo :: Int
foo = 4

