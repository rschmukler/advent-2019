# Advent of Code 2019

My adventures in completing the advent of code in 2019.

This year I decided to use Clojure.


## Running

Install tools.deps + clojure. Then just run `clojure` inside the root directory of the project.


## Strucutre

Standard clojure project. `src/` has code, organizaed into namespaces for each day. `test/` has test modules
mirroring the `src/` namespaces. `resources/` holds static files.


## Other Notes
I sometimes create static variables for things like input file contents. I wouldn't do this in a real application (usually) but
for solving something quick in the editor, it's great.
