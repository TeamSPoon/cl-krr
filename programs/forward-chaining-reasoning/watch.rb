#!/usr/bin/env watchr

# Watcher runs unit tests when any source/test file changes
# @requires http://github.com/mynyml/watchr

watch( '(test|src)\/(.*)\.lisp' ) { |md|
  puts "==============================================="
  puts "#{md[0]} updated - running #{md[2]} test"
  puts "==============================================="
  system("sh run-test.sh #{md[2]}")
}
