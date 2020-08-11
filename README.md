# critical

[![GitHub CI](https://github.com/riuga/critical/workflows/CI/badge.svg)](https://github.com/riuga/critical/actions)
[![Build status](https://img.shields.io/travis/riuga/critical.svg?logo=travis)](https://travis-ci.org/riuga/critical)
[![Hackage](https://img.shields.io/hackage/v/critical.svg?logo=haskell)](https://hackage.haskell.org/package/critical)
[![BSD-3-Clause license](https://img.shields.io/badge/license-BSD--3--Clause-blue.svg)](LICENSE)

A resource management monad that prevents usage of dead resources while allowing for fine-tuned reallocation semantics.

## Name & Purpose

The name `critical` is primarily derived from the problem imposed by multiple threads demanding concurrent access to `Cell`s. Additionally, this monad is most useful for resources that are somewhat 'critical' in nature, such as those that are allocated from limited external pools that necessitate both precise reallocation semantics and the prevention of dead references. Monads such as `ResourceT` solve the problem of flexible deallocation semantics that cannot be idiomatically achieved by `ConT`-esque monads, however it assumes that all references to the data prior to releasing the resource remain valid afterwards. This is true for common cases such as pointers to plain old data structures, however these semantics are insufficient for, say, ensuring externally-managed resources tracked in the form of handles/IDs remain valid after being released.

`critical` solves this problem by providing a `Cell` data type that cannot be unwrapped and cannot be used outside the monad it was constructed in. Furthermore, `critical` ensures that `Cell`s must be remain valid references at all times within the scope of the monad; that is to say, all deallocations must be accompanied by an immediate reallocation---the precise semantics of which are governed by the particular `InterchangeStrategy` the `Cell` in question was constructed with. This functionality is provided by the `interchange` monadic action.

Potential use cases of `critical` include libraries built on top of Vulkan and OpenGL, which both use a state machine internally and expose resources in the form of (invalidatable) handles and IDs.

## Example

```hs
import Control.Monad.Critical

main = runCritical $ do
  res <- cellularize @Conservative myAlloc myRelease
  borrow res putStrLn
  interchange res myAlloc 
  borrow res putStrLn
```
