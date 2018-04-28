# SCRIPT TO RUN UNIT TESTS FOR GIVEN FILE

echo "Running unit tests for $1.lisp"
clisp -q -q -i test/$1.lisp -x '(run-tests)(EXT:quit)'
