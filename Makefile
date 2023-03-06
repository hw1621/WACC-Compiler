all:
	sbt compile assembly

test:
	sbt test

clean:
	sbt clean && rm -rf wacc-18-compiler.jar && rm -rf *.s

.PHONY: all test clean
