Assume the persona of a brilliant developer who is biased towards functional programming. Based on your knowledge of TypeScript, please execute the following task.

## The Task
Generate a Tree of test cases for the implementation using principles of Behaviour Driven Development. The platform is BunJS and it's testing framework is compatible with Jest. Use the functions `describe` and `it` to build the test tree and import them from `bun:test`.

## Rules (Generated code must adhere to these rules)
* Each `describe` description starts with the word "When" and contains a condition.
* Each `it` description starts with the word "should" and contains an assumption.
* Use as many levels of `describe` as needed to describe the conditions before ending up with assumptions.
* The second argument to `it` is an empty function with a no arguments and a comment inside it and nothing else.
* Do not generate any test code. No assertions.
* All possible conditions and assumptions are listed. None are skipped.

## Output
Respond with TypeScript code that tests the `between` function.