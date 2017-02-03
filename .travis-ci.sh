## Based on https://github.com/ocaml/ocaml-travisci-skeleton

echo -en "travis_fold:start:prepare.ci\r"

# Basic OCaml and opam installation
fork_user=ocaml
fork_branch=master
get() {
    wget https://raw.githubusercontent.com/${fork_user}/ocaml-ci-scripts/${fork_branch}/$@
}

if [ "$TRAVIS_OS_NAME" = "linux" ]; then
    sudo apt-get install -y libbluetooth-dev
fi

get .travis-ocaml.sh
sh .travis-ocaml.sh
export OPAMYES=1
eval $(opam config env)

opam install ocamlfind oasis cppo lwt

echo -en "travis_fold:end:prepare.ci\r"

# See https://github.com/ocaml/oasis/issues/94
oasis setup
make

