# SICP Scheme

The intention of this project is to implement a Scheme interpreter in Scala
that will provide just enough functionality to be able to evaluate exercises
and examples in Structure and Interpretation of Computer Programs (SICP).

## Resources

- [An online Scheme interpreter from UC Berkley][berkley-scheme]
- [PDF copy of SICP from MIT][sicp-pdf]


[berkley-scheme]: https://inst.eecs.berkeley.edu/~cs61a/fa14/assets/interpreter/scheme.html
[sicp-pdf]: https://web.mit.edu/6.001/6.037/sicp.pdf

## TODO

- Bring behaviour in line with Berkley interpreter
  - Consistent error model
  - Distinguish between syntax error and scheme error
  - Evaluation stack
- Check whether the Berkley interpreter is actually worth copying
  or if I should find something more official
- Extend prefix notation calculations to support real numbers
- Variables in the global environment

## Done

- Evaluate integer literals without parens
- Prefix notation calculations for + - * / on integers