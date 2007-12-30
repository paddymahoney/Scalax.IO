PROJECT = scalax
VERSION = 0.0
SCALACOMMAND ?= scalac
SCALAC = $(SCALACOMMAND) -target:jvm-1.5 -unchecked -deprecation -encoding UTF-8

all: jar docs

classes: tmp/built

jar: dist/$(PROJECT)-$(VERSION).jar

docs: tmp/docs-built

test: tmp/tests-built
	scala -Djava.awt.headless=true -Djava.net.preferIPv4Stack=true -classpath build:tmp/tests ScalaxTests

dist/$(PROJECT)-$(VERSION).jar: tmp/built
	mkdir -p dist
	jar cf dist/$(PROJECT)-$(VERSION).jar -C build .

tmp/built: tmp/exists src/scalax/*/*.scala
	mkdir -p build
	$(SCALAC) -d build src/scalax/*/*.scala
	touch tmp/built

tmp/docs-built: tmp/exists src/scalax/*/*.scala
	mkdir -p dist/api
	scaladoc -d dist/api -doctitle Scalax -windowtitle Scalax -linksource -footer "Copyright (c) 2007 The Scalax Project. All Rights Reserved." src/scalax/*/*.scala
	touch tmp/docs-built

tmp/tests-built: tmp/built tests/scalax/*.scala tests/scalax/*/*.scala
	mkdir -p tmp/tests
	$(SCALAC) -d tmp/tests -classpath build tests/scalax/*.scala tests/scalax/*/*.scala
	touch tmp/tests-built

clean:
	rm -rf build
	rm -rf dist
	rm -rf tmp

tmp/exists:
	mkdir -p tmp
	touch tmp/exists

int:
	scala -Djava.awt.headless=true -Djava.net.preferIPv4Stack=true -classpath .:build -target:jvm-1.5 -unchecked -deprecation

.PHONY: all classes jar clean int docs test
