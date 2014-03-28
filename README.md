
[![Build Status](https://travis-ci.org/music-suite/abcnotation.svg?branch=master)](https://travis-ci.org/music-suite/abcnotation)

# abcnotation

This package contains a Haskell representation and parser for ABC notation. 

ABC notation is a text-based music notation system designed to be comprehensible by both people and 
computers. For more information see <http://abcnotation.com>.

Based on the 2.1 standard.

## Limitations

  * Limited support for *volatile* features
  * Limited support for text strings (§8.2)
    * No mnemonics
    * No entities
    * Unicode escapes are supported
  * No support for macros (§9)
  * No support for outdated syntax (§10)
  * Stylesheet directives are ignored (§11)
  * Typeset text is ignored (§2.2.3)
  * Strict interpretation assumed (§12)

## Requirements

* [Haskell Platform](http://www.haskell.org/platform)

## Installation

    cabal configure
    cabal install
