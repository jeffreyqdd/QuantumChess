#!/usr/bin/env bash

packages=(
  utop 
  odoc
  ounit2 
  qcheck
  bisect_ppx 
  menhir 
  ocaml-lsp-server 
  ocamlformat 
  ocamlformat-rpc
)

opam install -y "${packages[@]}"