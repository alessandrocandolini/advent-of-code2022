[![CI](https://github.com/alessandrocandolini/advent-of-code2022/actions/workflows/ci.yml/badge.svg)](https://github.com/alessandrocandolini/advent-of-code2022/actions/workflows/ci.yml) [![codecov](https://codecov.io/gh/alessandrocandolini/advent-of-code2022/branch/main/graph/badge.svg?token=P1OXMBYk3O)](https://codecov.io/gh/alessandrocandolini/advent-of-code2022)

# advent-of-code2022

https://adventofcode.com/2022

## Calendar
- Day 1: [problem](https://adventofcode.com/2022/day/1) | [solution](src/Day1.hs) | [test](test/Day1Spec.hs) 
- Day 2: [problem](https://adventofcode.com/2022/day/2) | [solution](src/Day2.hs) | [test](test/Day2Spec.hs) 
- Day 3: [problem](https://adventofcode.com/2022/day/3) | [solution](src/Day3.hs) | [test](test/Day3Spec.hs)
- Day 4: [problem](https://adventofcode.com/2022/day/3) | [solution](src/Day3.hs) | [test](test/Day3Spec.hs) NOT DONE YET
- Day 5: TODO

## How to build and run locally

The project uses the [Haskell tool stack](https://docs.haskellstack.org/en/stable/README/).

Assuming `stack` is installed in the system, the project can be build by running
```
stack build
```
To build and also run the tests, run
```
stack test
```
which is equivalent to
```
stack build --test
```
To run with test coverage
```
stack test --coverage
```
which generates a textual and HTML report.

To run the executable,
```
stack exec advent-of-code2022-exe -- -d <day> -f <filename> 
```
for example
```
stack exec advent-of-code2022-exe -- -d 1 -f resources/input1
stack exec advent-of-code2022-exe -- -d 2 -f resources/input2
stack exec advent-of-code2022-exe -- -d 3 -f resources/input3
stack exec advent-of-code2022-exe -- -d 4 -f resources/input4
```
For faster feedback loop,
```
stack test --fast --file-watch
```
To run `ghci` (with a version compatible with the resolver) run
```
stack ghci
```
For more information, refer to the `stack` official docs.
