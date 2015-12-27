
# Mockosaur
Simple mocking for Scala

## Not Ready For General Usage
This library is/was a brief exploration of how feasible it is to create a fully featured mocking framework specifically Scala that:
  * has a simple intuative user API
  * doesn't use macros
  * doesn't use dynamic

It turns out not very. The further down the road the project went, the more nasty JVM/reflection hacks had to be added to cover corner cases.

Work stopped when it became clear that it would actually be simpler and cleaner to use macros.

See [the tests](src/test/scala/mockosaur/MockInvocationTest.scala) for the implemented features.
