let rec emit = function
  | Ast.External args ->
      args
        |> List.rev_map (fun arg -> Inst.Push arg)
        |> List.cons Inst.Exec
        |> List.rev