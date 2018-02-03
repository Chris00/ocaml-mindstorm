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

submit:
	topkg distrib
	topkg publish distrib
	topkg opam pkg -n mindstorm
	topkg opam pkg -n mindstorm-lwt
# 	Perform the subtitution that topkkg does not
#	(until opam2, https://discuss.ocaml.org/t/sync-versions-of-several-packages-coming-from-a-single-repo/808/5)
	sed -e 's/\(^ *"mindstorm"\) */\1 {= "$(PKGVERSION)"}/' --in-place \
	  _build/mindstorm-lwt.$(PKGVERSION)/opam
# until we have https://github.com/ocaml/opam-publish/issues/38
	[ -d packages ] ||(echo "ERROR: Make a symbolic link packages → \
		opam-repo/packages"; exit 1)
	cp -r _build/mindstorm.* packages/mindstorm
	cp -r _build/mindstorm-lwt.* packages/mindstorm-lwt/
	cd packages && git add mindstorm mindstorm-lwt
#	topkg opam submit -n mindstorm-lwt -n mindstorm-lwt-lwt

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
