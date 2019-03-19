PKGVERSION = $(shell git describe --always --dirty)

build:
	jbuilder build @install @tests @examples -j 4 #--dev

tests: build
	jbuilder runtest

install uninstall clean:
	jbuilder $@

doc: build
	sed -e 's/%%VERSION%%/$(PKGVERSION)/' src/mindstorm.mli \
	  > _build/default/src/mindstorm.mli
	jbuilder build @doc
	echo '.def { background: #f0f0f0; }' >> _build/default/_doc/odoc.css

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
