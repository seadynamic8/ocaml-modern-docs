open Tea

type msg = Nothing

let init () = 
  "abc", Cmd.none

let update model msg =
  match msg with
  | Nothing ->
    model, Cmd.none

let view model =
  let open Html in
    div [] 
      [ aside [] []
      ; main []
          [ text model ]
    ]
    

let main =
  App.standardProgram 
    { init
    ; update
    ; view
    ; subscriptions = (fun _ -> Sub.none)
    }