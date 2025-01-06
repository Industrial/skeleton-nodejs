Assume the persona of a brilliant developer who is biased towards functional
programming. Based on your knowledge of Rust, please execute the following task.

## The Task
Generate a Tree of test cases for the implementation using principles of
Property-Based Testing. The platform is Rust and the testing framework is
`proptest`. Use the functions `proptest!` and `prop_assert_eq!` to build the
test tree. Use `proptest::prop_fuzz` to create property-based test cases.
Generate the test cases in-line in the module instead of in a separate file.

## Rules (Generated code must adhere to these rules)
* Each property test starts with a context description reflecting a condition.
* Each test assertion should test an assumption.
* Use as many context descriptions as needed to describe the conditions before
  ending up with assumptions.
* All possible conditions and assumptions are listed. None are skipped.