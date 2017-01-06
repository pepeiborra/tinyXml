# tinyXml [![Hackage version](https://img.shields.io/hackage/v/tinyXml.svg?label=Hackage)](https://hackage.haskell.org/package/tinyXml) [![Stackage version](https://www.stackage.org/package/tinyXml/badge/lts?label=Stackage)](https://www.stackage.org/package/tinyXml) [![Linux Build Status](https://img.shields.io/travis/pepeiborra/tinyXml.svg?label=Linux%20build)](https://travis-ci.org/pepeiborra/tinyXml) 

TinyXml is a DOM style parsing library for a subset of XML. It is intended to be fast and memory efficient, but it currently has no support for: 
- entities, 
- CDATA sections, nor 
- namespaces. 

TinyXml is a reimplementation of [Hexml] (http://neilmitchell.blogspot.com/2016/12/fuzz-testing-hexml-with-afl.html) without the cbits, attaining mostly comparable performance in pure (but hardly idiomatic) Haskell.  

TinyXml is alpha quality software and not currently used in production anywhere.
