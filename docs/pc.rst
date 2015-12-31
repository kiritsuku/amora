THIS DOCUMENT IS A DRAFT

New Presentation Compiler for Scala
===================================

This document is a proposal for a redesign of Scalas Presentation Compiler (PC). The PC is an API that can run all phases of the Scala compiler (scalac) from scanning over parsing up to typechecking. Tools can use the PC to retrieve semantic information from Scala code. A more in depth explanation of the PC can be found `here <http://scala-ide.org/docs/dev/architecture/presentation-compiler.html>`_.

The reason for this document is that I argue, that the current PC is broken by design. It follows a problematic architecture, which led to lots of nasty bugs that make the live for all users of the PC unnecessarily difficult. The most problematic design issues are:

- The Abstract Syntax Tree (AST) doesn't retain all information of a Scala source file. Comments, annotations, formatting and some other things are completely removed from the AST and simple expressions like `1+1` are already optimized in the parser, which lead to ASTs that no longer represent the source file.
- TODO

Lately, the Scala ecosystem got the chance to fix these issues. There exists the new `scalameta <https://github.com/scalameta/scalameta>`_ project, whose goal is it to find a better API for syntactic and semantic operations on the data structures of a Scala compiler. Also, there exists `dotty <https://github.com/lampepfl/dotty>`_, which is a compiler for a research programming language called dotty that may one day result in a new language version of Scala. Both have important improvements over the previous compiler scalac and did progress enough to make a redesign of a PC viable.

A new PC should follow the following design points:

- Provide not only an AST but also a token stream
- The AST and token stream must not delete any information
- Follow the specification of a `Reactive Stream <https://github.com/reactive-streams/reactive-streams-jvm/tree/v1.0.0>`_
- TODO
