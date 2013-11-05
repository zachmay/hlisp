run-tests:
	cat test/test.lisp | runhaskell HLisp test/test-definitions.lisp test/eval.lisp > test/output.actual
	diff test/output.actual test/output.expected

test-interactive:
	runhaskell HLisp test/test-definitions.lisp test/eval.lisp 

clean:
	rm test/output.actual
