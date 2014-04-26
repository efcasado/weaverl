# weaverl

Erlang meets [Aspect-oriented Programming (AOP)][1]


## Overview

Weaverl aims at bringing to the Erlang community similar AOP features than those 
offered by major AOP frameworks to mainstream languages (e.g. [AspectJ][2]/Java). 
More concretely, this project's goal is to allow Erlang/OTP developers to keep
their applications clean and understandable by placing cross-cutting concerns 
(e.g. logging) into separate files (i.e. modules) a.k.a. aspects.

At the time of this writting, weaverl supports only [compile-time weaving][3]. So
far, no effort has been done to bring the weaving process at load or run-time.

Compile-time aspect weaving has been implemented by means of a parse transformation.
This parse transformation reads all advice definitions from an external file and 
injects their associated proxy functions at the specified join-points, if applicable.

## Basic Concepts

TBD

## Getting Started

TBD

*__DISCLAIMER: This project was started as an exercise to learn more about 
Erlang/OTP's parse transformations. Do not expect a full-fledged, bug-free
tool ready to be integrated in production software, but rather an experimental
piece of software you can use to start experimenting with AOP principles in your 
Erlang applications.__*

[1]: http://en.wikipedia.org/wiki/Aspect-oriented_programming
[2]: http://eclipse.org/aspectj/doc/released/progguide/index.html
[3]: http://www.eclipse.org/aspectj/doc/next/devguide/ltw.html


