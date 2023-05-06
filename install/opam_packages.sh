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
  ANSITerminal
  curses
)

opam update -y
opam upgrade -y
opam install -y "${packages[@]}"