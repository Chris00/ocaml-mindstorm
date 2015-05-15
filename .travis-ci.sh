## Based on https://github.com/ocaml/ocaml-travisci-skeleton

# Basic OCaml and opam installation
fork_user=ocaml
fork_branch=master
get() {
    wget https://raw.githubusercontent.com/${fork_user}/ocaml-travisci-skeleton/${fork_branch}/$@
}

get .travis-ocaml.sh
sh .travis-ocaml.sh
export OPAMYES=1
eval $(opam config env)

if [ "$TRAVIS_OS_NAME" = "linux" ]; then
    sudo apt-get install -y libbluetooth-dev
fi
opam install ocamlfind oasis

make

