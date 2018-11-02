open Tea
open Html
open Decoder
open Home_page
open License

module Task = Tea_task

type doc
type dom_element = Dom.element Js.Nullable.t
external document : doc = "" [@@bs.val]
external querySelector : doc -> string -> dom_element = "" [@@bs.send]
external scrollIntoView : dom_element -> unit = "" [@@bs.send]
external scrollTo : int -> int -> unit = "" [@@bs.val][@@bs.scope "window"]

type module_name = string
type page_location = string
type function_name = string

type msg 
  = UrlChange of Web.Location.location
  | ClickedSidebarLink of module_name * bool
  | Scroll
  [@@bs.deriving accessors]

type page =
  { name : module_name
  ; position : page_location
  }

type sidebar_link =
  { name : module_name
  ; selected : bool
  ; functions : function_name list
  ; functions_selected : bool
  }

type model =
  { history : Web.Location.location list
  ; module_list : Decoder.modules
  ; sidebar_links : sidebar_link list
  ; page : page
  }

(* --------------- Init ------------------- *)

let get_module_list =
  let json = 
    Node.Fs.readFileAsUtf8Sync "parser/modules.json"
    |> Js.Json.parseExn
  in
    json
    |> Decode.decode_modules

let get_function_names elements functions =
  elements
  |> List.fold_left (fun functions e ->
    match e with
    | Function (name, _annotation, _info) ->
      name :: functions
    | _ -> functions
  ) functions
  |> List.rev

let functions_in_sub_sections sub_sections functions =
  sub_sections
  |> List.fold_left (fun functions ss ->
      get_function_names ss.elements functions
    ) functions
  |> List.rev

let functions_in_section section =
  [] 
  |> functions_in_sub_sections section.sub_sections
  |> get_function_names section.elements    

let get_function_list sections =
  sections
  |> List.fold_left (fun functions s ->
    (functions_in_section s) :: functions
  ) []
  |> List.rev
  |> List.flatten

let create_sidebar_link_state module_list =
  module_list
  |> List.map (fun m -> 
    { name = m.module_name
    ; selected = false
    ; functions = get_function_list m.sections
    ; functions_selected = false
    }) 

let init () location = 
  let module_list = get_module_list in
  { history = [ location ]
  ; module_list
  ; sidebar_links = create_sidebar_link_state module_list
  ; page = 
    { name = "docs_home"
    ; position = ""
    }
  }, Cmd.none

(* ------------ UPDATE -------------- *)

let parse_hash_value hash =
  match Js.String.split "-" hash with
  | [| name; position |] -> 
    { name = Js.String.substr ~from: 1 name; position }
  | [| name |] -> 
    { name = Js.String.substr ~from: 1 name; position = "" }
  | _ -> { name = ""; position = "" }

let scrollToPosition position =
  let element = document |. querySelector("#" ^ position) in
    if element <> Js.Nullable.null then
      element |. scrollIntoView

let setScrollPosition position =
  if position = "" then 
    scrollTo 0 0 
  else 
    scrollToPosition position

let update model msg =
  match msg with
  | UrlChange location ->
    { model with 
      history = location :: model.history 
    ; page = parse_hash_value location.Web.Location.hash
    }, 
    Task.perform (fun _ -> Scroll) (Task.succeed ())

  | ClickedSidebarLink (sidebar_link_name, is_functions) ->
    let 
      flipSelected s =
        if s.name = sidebar_link_name then
          begin match is_functions with
          | true ->
            { s with functions_selected = not s.functions_selected }
          | false ->
            { s with selected = not s.selected }
          end
        else
          s
    in
    { model with 
      sidebar_links = List.map flipSelected model.sidebar_links
    }, Cmd.none

  | Scroll ->
    let _ = setScrollPosition model.page.position in
      model, Cmd.none


(* ------------ ViEW -------------- *)

let innerHTML html =
  Vdom.prop "innerHTML" html

(* Sidebar *)

let function_links sidebar_link =
  sidebar_link.functions
  |> List.map (fun func_name ->
    li 
      [] 
      [ a [ href ("#" ^ sidebar_link.name ^ "-" ^ func_name) ] [ text func_name ] ]
  ) 

let sidebar_functions sidebar_link =
  li 
    [] 
    [ a 
        [onClick (ClickedSidebarLink (sidebar_link.name, true))
        ] 
        [ text "Functions" ]
    ; ul
        [ classList 
            [ "function-links", true
            ; "selected", sidebar_link.functions_selected
            ]
        ]
        (function_links sidebar_link)
    ]

let sublist_sidebar sidebar_link =
  let add_sidebar_functions list = 
    if List.length sidebar_link.functions > 0 then
      sidebar_functions sidebar_link :: list
    else
      list
  in
    ul
      [ classList 
          [ "sublist", true
          ; "selected", sidebar_link.selected ]
      ]
      (li [] [ a [ href ("#" ^ sidebar_link.name) ] [ text "Top" ] ]
        :: add_sidebar_functions [])

let sidebar_link sidebar_link =
  li 
    [] 
    [ a 
        [ onClick (ClickedSidebarLink (sidebar_link.name, false))
        ] 
        [ text sidebar_link.name ]
    ; sublist_sidebar sidebar_link
    ]

let module_sidebar_links sidebar_links =
  List.map (fun s -> sidebar_link s) sidebar_links

let view_sidebar sidebar_links =
  aside [ class' "sidebar" ]
    [ a
      [ href "#docs_home" ]
      [ img [ src "http://ocaml.org/img/colour-logo-white.svg"; id "logo" ] []
      ]
    ; h5 [ id "version" ] [ text "v4.07 (Unofficial)" ]
    ; h6 
        [] 
        [ a 
            [ href "https://caml.inria.fr/pub/docs/manual-ocaml-4.07/" ] 
            [ text "Official Docs" ]
        ]
    ; h6 [] [ a [ href "https://ocaml.org/" ] [ text "Official Website" ] ]
    ; h6 [] [ a [ href "https://www.streamingspring.com" ] [ text "Back to Blog" ] ]
    ; h3 [ id "modules-title" ] [ text "Modules" ]
    ; ul 
        [ class' "module-links" ] 
        (module_sidebar_links sidebar_links)
    ]

(* Elements *)

let view_element_header name category content element_html =
  h5 
    [ id name; class' "element-item" ] 
    [ span [ class' "category" ] [ text category ]
    ; text (name ^ content )
    ] :: element_html

let view_element_info info element_html =
  match info with
  | Some info -> div [ class' "info"; innerHTML info ] [] :: element_html
  | None -> element_html

let view_element_type_table type_table element_html =
  match type_table with
  | Some type_table -> table [ innerHTML type_table ] [] :: element_html
  | None -> element_html

let parse_exception exec_parameter =
  match exec_parameter with
  | Some exec_parameter -> " of " ^ exec_parameter
  | None -> ""

let view_element element =
  match element with
  | Type (name, type_type, info) ->
      []
      |> view_element_info info
      |> view_element_header name "type " (" = " ^ type_type)
  | Typevariant (name, type_table, info) ->
      []
      |> view_element_info info
      |> view_element_type_table type_table
      |> view_element_header name "type " (" =")
  | Function (name, func_annotation, info) ->
      []
      |> view_element_info info
      |> view_element_header name "val " (" : " ^ func_annotation)
  | Exception (name, exec_parameter, info) ->
      []
      |> view_element_info info
      |> view_element_header name "exception " (parse_exception exec_parameter)
  | Module (name, info) ->
      []
      |> view_element_info info
      |> view_element_header name "module " (": sig .. end")
  | Moduletype (name, info) ->
      []
      |> view_element_info info
      |> view_element_header name "module type " (": sig .. end")

let view_elements elements =
  List.map (fun e -> li [ class' "element" ] (view_element e)) elements

(* Sections *)

let view_section_name section_name =
  match section_name with
  | Some name -> h2 [ class' "subtitle" ] [ text name ]
  | None -> span [] []

let view_section_info section_info =
  match section_info with
  | Some info -> div [ class' "info"; innerHTML info ] []
  | None -> span [] []

let rec view_section section =
  li [ class' "section" ]
    [ view_section_name section.section_name
    ; view_section_info section.section_info
    ; ul 
        [] 
        (view_elements section.elements)
    ; ul 
        [ class' "sub-section" ] 
        (List.map (fun ss -> view_section ss) section.sub_sections)
    ]

let view_sections module_item =
  List.map (fun s -> view_section s) module_item.sections

(* Main Content Page *)

let view_main model =
  match model.page with
  | { name = "docs_home" } ->
    home_page ()
  | { name = "license" } ->
    [ main [ id "license"; class' "content" ] (license ())
    ]
  | { name } ->
    try
      let module_item = 
          model.module_list
          |> List.find (fun m -> m.module_name = name)
      in
        [ main 
            [ id "module-content"; class' "content" ] 
            [ h1 [ class' "title" ] [ text module_item.module_name ] 
            ; div [ class' "info"; innerHTML module_item.module_info ] []
            ; hr [] []
            ; section 
              [ id "elements" ]
              [ ul [] (view_sections module_item) ]
            ]
        ]
    with
      | Not_found ->
        [ main 
            [ id "not-found"; class' "content" ] 
            [ h1 [ class' "title" ] [ text "Not Found" ] ]
        ]

let view model =
  div [ id "site-container" ] 
    [ view_sidebar model.sidebar_links
    ; div [ id "main-container" ] (view_main model)
    ]


let main =
  Navigation.navigationProgram urlChange
    { init
    ; update
    ; view
    ; subscriptions = (fun _ -> Sub.none)
    ; shutdown = (fun _ -> Cmd.none)
    }
