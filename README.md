# realloc

[![GitHub CI](https://github.com/riuga/realloc/workflows/CI/badge.svg)](https://github.com/riuga/realloc/actions)
[![Build status](https://img.shields.io/travis/riuga/realloc.svg?logo=travis)](https://travis-ci.org/riuga/realloc)
[![Hackage](https://img.shields.io/hackage/v/realloc.svg?logo=haskell)](https://hackage.haskell.org/package/realloc)
[![BSD-3-Clause license](https://img.shields.io/badge/license-BSD--3--Clause-blue.svg)](LICENSE)

A restricted ResourceT where released objects must be immediately reallocated, preventing 'dead' references.
