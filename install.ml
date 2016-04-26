#use "topfind";;
#require "js-build-tools.oasis2opam_install";;

open Oasis2opam_install;;

generate ~package:"async_smtp"
  [ oasis_lib "async_smtp"
  ; oasis_lib "async_smtp_command"
  ; oasis_lib "async_smtp_tools"
  ; file "META" ~section:"lib"
  ; file "_build/namespace_wrappers/crypto.cmi" ~section:"lib"
  ]
