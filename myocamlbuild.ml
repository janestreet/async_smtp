(* OASIS_START *)
(* OASIS_STOP *)
# 4 "myocamlbuild.ml"

let dispatch = function
  | Before_options ->
    Options.make_links := false
  | After_rules ->
    let hack = "ugly_hack_to_workaround_ocamlbuild_nightmare" in
    mark_tag_used hack;
    dep [hack] [hack];

    let lib_mods_to_delete =
      [ "common"
      ; "spool"
      ]
    in

    let add_exts l exts =
      List.concat (List.map (fun fn ->
        let fn = "lib/" ^ fn in
        List.map (fun ext -> fn ^ ext)  exts)
        l)
    in

    rule hack
      ~prod:hack
      ~deps:(add_exts lib_mods_to_delete [".cmx"; ".cmi"; ".cmo"])
      (fun _ _ ->
         let to_remove =
           add_exts lib_mods_to_delete [ ".cmx"
                                       ; ".cmi"
                                       ; ".cmo"
                                       ; ".ml"
                                       ; ".mli"
                                       ; ".ml.depends"
                                       ; ".mli.depends"
                                       ; ".o"
                                       ]
         in
         Seq
           [ Seq (List.map rm_f to_remove)
           ; Echo ([], hack) ])

  | _ ->
    ()

let () = Ocamlbuild_plugin.dispatch (fun hook -> dispatch hook; dispatch_default hook)
