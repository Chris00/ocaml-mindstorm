PKGVERSION = $(shell git describe --always --dirty)

build:
	dune build @install @tests @examples

tests: build
	dune runtest --force

install uninstall clean:
	dune $@

doc: build
	dune build @doc
	sed -e 's/%%VERSION%%/$(PKGVERSION)/' --in-place \
	  _build/default/_doc/_html/mindstorm/Mindstorm/index.html

website:
	@ if [ -d $(WEB_DIR)/ ] ; then \
	  scp $(wildcard $(WEB_DIR)/*.html $(WEB_DIR)/*.css) LICENSE $(WEB) \
	  && echo "--- Published web site (in $(WEB_DIR)/) on OCamlForge." ; \
	fi

website-img:
	@ if [ -d $(WEB_DIR)/ ] ; then \
	  scp $(wildcard $(WEB_DIR)/*.png $(WEB_DIR)/*.jpg) $(WEB) \
	  && echo "--- Published images (in $(WEB_DIR)/) on OCamlForge." ; \
	fi

.PHONY: build tests install uninstall clean doc submit \
  website website-img clean
