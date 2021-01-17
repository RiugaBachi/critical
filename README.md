# critical

[![GitHub CI](https://img.shields.io/github/workflow/status/RiugaBachi/critical/CI?logo=github&style=flat-square)](https://github.com/RiugaBachi/critical/actions)
[![Hackage](https://img.shields.io/hackage/v/critical.svg?logo=haskell&style=flat-square)](https://hackage.haskell.org/package/critical)
[![BSD-3-Clause license](https://img.shields.io/badge/license-BSD--3--Clause-blue.svg?style=flat-square)](LICENSE)

A resource management monad that prevents usage of dead resources while allowing for fine-tuned reallocation semantics.

## Name & Purpose

The name `critical` is primarily derived from the problem imposed by multiple threads demanding concurrent access to `Cell`s. Additionally, this monad is most useful for resources that are somewhat 'critical' in nature, such as those that are allocated from limited external pools that necessitate both precise reallocation semantics and the prevention of dead references. Monads such as `ResourceT` solve the problem of flexible deallocation semantics that cannot be idiomatically achieved by `ConT`-esque monads, however it assumes that all references to the data prior to releasing the resource remain valid afterwards. This is true for common cases such as pointers to plain old data structures, however these semantics are insufficient for, say, ensuring externally-managed resources tracked in the form of handles/IDs remain valid after being released.

`critical` solves this problem by providing a `Cell` data type that cannot be unwrapped and cannot be used outside the monad it was constructed in. Furthermore, `critical` ensures that `Cell`s must be remain valid references at all times within the scope of the monad; that is to say, all deallocations must be accompanied by an immediate reallocation---the precise semantics of which are governed by the particular `InterchangeStrategy` the `Cell` in question was constructed with. This functionality is provided by the `interchange` monadic action.

Potential use cases of `critical` include libraries built on top of Vulkan and OpenGL, which both use a state machine internally and expose resources in the form of (invalidatable) handles and IDs.

## Example

```hs
import Data.IORef
import Control.Monad.Critical
import Control.Monad.Managed

data SomeCell i a = forall t. SCell (Cell t i a)

main = runCritical $ do
  res1 <- cellularize myAlloc myRelease
  res2Ref <- liftIO $ newIORef (Nothing :: Maybe (SomeCell Conservative String))
  -- ^ Let's try to obtain res2 with this IORef hack since 
  --   runCritical specializes to () only
  liftIO $ runCritical $ do
    res2 <- cellularize @Conservative myAlloc myRelease
    borrow res2 putStrLn
    interchange res2 myAlloc 
    borrow res2 putStrLn
    {- borrow res1 putStrLn -} -- Bad! s0 ~ s1 cannot be proved
    liftIO $ writeIORef res2Ref $ Just $ SCell res2
    pure ()
  Just (SCell res2) <- liftIO $ readIORef res2Ref
  -- ^ Oh no! s0 ~ s1 cannot be proven due to existential quantification
  --   of runCritical. No cellular monadic actions can be performed on
  --   this cell from this outer monad. It is useless from this scope.
  pure ()
  where
    myAlloc = pure "critical"
    myRelease = const $ pure ()
```

## Building

This library utilizes the Cabal build system. Please consult the [official Cabal documentation](https://cabal.readthedocs.io) and the project cabal file for more information on building the library, tests, and benchmarks.

## Notes

Unit tests and benchmarks are for now stubs, but are on the agenda. Additionally, the library's API will remain static for the forseeable future, however internal implementations and data representations may change in pursuit of optimization.
