PROJECT = scalax
VERSION = 0.0
SCALA_HOME ?= $(dir $(shell which scala))..
SCALA ?= $(SCALA_HOME)/bin/scala
SCALAC ?= $(SCALA_HOME)/bin/scalac
SCALADOC ?= $(SCALA_HOME)/bin/scaladoc
SCALAC_OPTS = -target:jvm-1.5 -unchecked -deprecation -encoding UTF-8
SLF4J = slf4j-api-1.5.0.jar

all: message jar docs

message:
	: =================================================
	: This build method exists on sufferance.
	: You are strongly encouraged to use Maven instead.
	: =================================================
	:
	: SCALA_HOME=$(SCALA_HOME)
	:

tmp/$(SLF4J):
	wget -O tmp/$(SLF4J) http://mirrors.ibiblio.org/pub/mirrors/maven2/org/slf4j/slf4j-api/1.5.0/slf4j-api-1.5.0.jar

classes: message tmp/built

jar: message dist/$(PROJECT)-$(VERSION).jar

docs: message tmp/docs-built

test: message tmp/tests-built
	$(SCALA) -Djava.awt.headless=true -Djava.net.preferIPv4Stack=true -classpath build:tmp/tests:tmp/$(SLF4J) ScalaxTests

dist/$(PROJECT)-$(VERSION).jar: tmp/built
	mkdir -p dist
	jar cf dist/$(PROJECT)-$(VERSION).jar -C build .

tmp/built: tmp/exists tmp/$(SLF4J) src/scalax/*/*.scala
	mkdir -p build
	$(SCALAC) $(SCALAC_OPTS) -classpath tmp/$(SLF4J) -d build src/scalax/*/*.scala
	touch tmp/built

tmp/docs-built: tmp/exists src/scalax/*/*.scala
	mkdir -p dist/api
	$(SCALADOC) -d dist/api -classpath tmp/$(SLF4J) -doctitle Scalax -windowtitle Scalax -linksource -footer "Copyright (c) 2005-8 The Scalax Project. All Rights Reserved." src/scalax/*/*.scala
	touch tmp/docs-built

tmp/tests-built: tmp/built tests/scalax/*.scala tests/scalax/*/*.scala
	mkdir -p tmp/tests
	$(SCALAC) -d tmp/tests -classpath build:tmp/$(SLF4J) tests/scalax/*.scala tests/scalax/*/*.scala
	touch tmp/tests-built

clean: message
	rm -rf build
	rm -rf dist
	rm -rf tmp

tmp/exists:
	mkdir -p tmp
	touch tmp/exists

int:
	$(SCALA) -Djava.awt.headless=true -Djava.net.preferIPv4Stack=true -classpath .:build:tmp/$(SLF4J) -target:jvm-1.5 -unchecked -deprecation

.PHONY: all classes jar clean int docs test message
