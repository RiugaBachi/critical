{- |
Copyright: (c) 2020 John 'Ski
SPDX-License-Identifier: BSD-3-Clause
Maintainer: John 'Ski <riuga@tuta.io>

A restricted ResourceT where released objects must be immediately reallocated, preventing 'dead' references.
-}

module Realloc
       ( someFunc
       ) where


someFunc :: IO ()
someFunc = putStrLn ("someFunc" :: String)
