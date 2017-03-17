
# BUILD_FLAGS=--pedantic --library-profiling --executable-profiling
BUILD_FLAGS=--pedantic

RUN=stack exec -- wombat-garden

run: build
	$(RUN) --help

init: stack.yaml

stack.yaml:
	stack init

docs:
	stack haddock
	open `stack path --local-doc-root`/index.html

# package:
# build a release tarball or executable
#
# dev:
# start dev server or process. `vagrant up`, `yesod devel`, etc.
#
# deploy:
# prep and push

configure:
	cabal configure \
		--package-db=clear \
		--package-db=global \
		--package-db=`stack path --snapshot-pkg-db` \
		--package-db=`stack path --local-pkg-db`

install:
	stack install

tags: $(SRC)
	codex update

hlint:
	hlint *.hs src specs

clean:
	stack clean
	codex cache clean

distclean: clean
	rm stack.yaml

build:
	stack build $(BUILD_FLAGS)

test:
	stack test $(BUILD_FLAGS)

bench:
	stack bench $(BUILD_FLAGS)

watch:
	stack build --fast --file-watch --pedantic

watch-test:
	stack test --fast --file-watch --pedantic # --test-arguments "-m TODO"

restart: distclean init build

rebuild: clean build

.PHONY: init run docs configure install hlint clean distclean build test
.PHONY: bench watch watch-test restart rebuild