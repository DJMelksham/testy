# Testy v 1.0.0
_A Testing Framework for Common Lisp SBCL & a Triple Entendre in One!_

# Philosophy and Design
Testy was designed with specific goals for use on my own projects:

* Powerful and simple and explicit
* Efficieny is PARAMOUNT: tests must run OBSCENELY fast by default. Instant feedback should be the goal.
* Optional recompilation of tests
* Loading tests is multi-threaded by default.
* Running tests is multi-threaded by default
* Tests should be definable as quickly as possible
* Tests are defined during interactive development at the REPL.  No flow-breaking.  No explicit test file authoring.
* Tests accompany and support systems, but do not clutter the same namespace
* Tests should be invisible to the main source code of the program
* Tests should be serialisable automatically with one command and human readable  
* Everything important about a test can be documented and version controlled with your main program
* Tests are architectured for future inclusion in continuous real-time testing: tests store their results after each run 
but do not trigger the debugger on error/failure.
* Definable before and after test functions with their own status monitoring when tests are run
* Tests are basic and atomic: there are no fixtures, no factories, no dependencies between tests: Embrace it.
* All tests can be given multiple tags
* Maintain high levels of convenience/automation by basing the majority of 
functions on arbitrary sets of tests.  Many functions return sets of tests.
Many functions can operate on sets of tests.
