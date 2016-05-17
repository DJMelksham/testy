# Testy v 1.0.0
_A Testing Framework for Common Lisp SBCL

# Philosophy and Design
Testy was designed with specific goals for use on my own projects:

* Powerful, simple and explicit
* Efficieny is paramount: tests must run extremely fast by default. Instant feedback should be the goal.
* Optional recompilation of tests
* Compilation while loading tests is multi-threaded by default
* Running tests is multi-threaded by default
* Tests should be definable as quickly as possible
* Tests are defined via interactive development at the REPL.  No flow-breaking.  No (manually managed) test file writing.
* Tests accompany and support systems, but do not clutter the system's namespace
* Tests should be invisible to the main source code of the program, allowing a focus on style and optimal code
* Tests should be serialisable and human readable
* Everything important about a test should be documented and version controlled with your program repository
* Tests are architectured for future inclusion in continuous development and real-time testing: tests store their results after each run but do not trigger the debugger on error/failure.
* Definable before and after test functions with their own status when tests are run
* Tests are basic and atomic: there are no fixtures, no factories, no dependencies between tests: Embrace it.  Lispers have functions and macros anyway.
* All tests can be categorised and grouped via multiple tags
* Maintain high levels of convenience by basing many functions on sets of tests.  Functions return sets of tests.  Functions can operate on sets of tests.
