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
external add : dom_element -> string -> unit = "" [@@bs.send][@@bs.scope "classList"]
external remove : dom_element -> string -> unit = "" [@@bs.send][@@bs.scope "classList"]
external setTimeout : (unit -> unit) -> int -> unit = "" [@@bs.val]

type module_name = string
type element_name = string
type page_location = string
type function_name = string
type search_term = string

type msg
  = UrlChange of Web.Location.location
  | ClickedSidebarLink of module_name * bool
  | Scroll
  | UpdateSearchTerm of search_term
  | Search
  | Clear
  [@@bs.deriving accessors]

type page =
  { name : module_name
  ; position : page_location
  }

type search_result =
  { module_group : module_name
  ; element_results : element_name list Belt.Map.String.t
  }

type sidebar_link =
  { name : module_name
  ; selected : bool
  ; functions : function_name list
  ; functions_selected : bool
  }

type sidebar =
  { search_term : search_term
  ; search_results : search_result list
  ; sidebar_links : sidebar_link list
  }

type model =
  { history : Web.Location.location list
  ; module_list : Decoder.modules
  ; sidebar : sidebar
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
  ; sidebar =
    { search_term = ""
    ; search_results = []
    ; sidebar_links = create_sidebar_link_state module_list
    }
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

let flipSelected sidebar_link_name is_functions sidebar_link =
  if sidebar_link.name = sidebar_link_name then
    begin match is_functions with
    | true ->
      { sidebar_link with functions_selected = not sidebar_link.functions_selected }
    | false ->
      { sidebar_link with selected = not sidebar_link.selected }
    end
  else
    sidebar_link

let scrollToPosition position =
  let element = document |. querySelector("#" ^ position) in
    if element <> Js.Nullable.null then
      begin
        element |. scrollIntoView;
        element |. add "blink";
        setTimeout (fun () -> element |. remove "blink") 1000
      end

let setScrollPosition position =
  if position = "" then
    scrollTo 0 0
  else
    scrollToPosition position

let update_element_results category name element_results =
  Belt.Map.String.update element_results category (fun r ->
    match r with
    | Some r -> Some (name :: r)
    | None -> Some [name]
  )

let elements_search_results element_results search_term section_elements =
  let
    search_term_matches_name search_term name =
      Js.String.includes search_term (String.lowercase name)
  in
  section_elements
  |> List.fold_left (fun element_results element ->
      match element with
      | Type (name, _, _) when search_term_matches_name search_term name ->
          update_element_results "Type" name element_results

      | Typevariant (name, _, _) when search_term_matches_name search_term name ->
          update_element_results "Type" name element_results

      | Function (name, _, _) when search_term_matches_name search_term name ->
          update_element_results "Function" name element_results

      | Exception (name, _, _) when search_term_matches_name search_term name->
          update_element_results "Exception" name element_results

      | Module (name, _) when search_term_matches_name search_term name ->
          update_element_results "Module" name element_results

      | Moduletype (name, _) when search_term_matches_name search_term name ->
          update_element_results "Module" name element_results

      | _ ->
          element_results
  ) element_results

let sections_search_results search_result search_term sections =
  sections
  |> List.fold_left (fun search_result section ->
    { search_result with element_results =
      elements_search_results search_result.element_results search_term section.elements
    }
  ) search_result

let create_search_result module_name =
  { module_group = module_name
  ; element_results = Belt.Map.String.empty
  }

let get_search_results search_term module_list =
  module_list
  |> List.fold_left (fun results m ->
    let new_search_result = create_search_result m.module_name in
    let section_result =
      sections_search_results new_search_result search_term m.sections
    in
      if Belt.Map.String.isEmpty section_result.element_results then
        results
      else
        section_result :: results
  ) []
  |> List.rev

let onEnter msg =
  let isEnter code =
    if code = 13 then
      Json.Decoder.succeed msg
    else
      Json.Decoder.fail "not ENTER"
  in
    on "keydown" (Json.Decoder.andThen isEnter keyCode)

let update model msg =
  match msg with
  | UrlChange location ->
      { model with
        history = location :: model.history
      ; page = parse_hash_value location.Web.Location.hash
      },
      Task.perform (fun _ -> Scroll) (Task.succeed ())

  | ClickedSidebarLink (sidebar_link_name, is_functions) ->
      let sidebar_links =
        List.map (flipSelected sidebar_link_name is_functions) model.sidebar.sidebar_links
      in
      { model with sidebar = { model.sidebar with sidebar_links }
      }, Cmd.none

  | Scroll ->
      let _ = setScrollPosition model.page.position in
        model, Cmd.none

  | UpdateSearchTerm search_term ->
      { model with sidebar = { model.sidebar with search_term } }, Cmd.none

  | Search ->
      if String.length model.sidebar.search_term > 1 then
        let search_term    = String.lowercase model.sidebar.search_term in
        let search_results = get_search_results search_term model.module_list in

          { model with sidebar = { model.sidebar with search_results }
          }, Cmd.none
      else
        model, Cmd.none

  | Clear ->
      { model with sidebar =
        { model.sidebar with
          search_term = ""
        ; search_results = []
        }
      }, Cmd.none


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

let sidebar_element_results search_results =
  List.map (fun (category, elements) ->
    ul
      [ class' "search-result-category-list" ]
      [ h5 [ class' "search-result-category" ] [ text category ]
      ; ul
          [ class' "search-result-elements-list" ]
          (List.map (fun name ->
            li
              [ class' "search-result-element" ]
              [ a
                [ href ("#" ^ search_results.module_group ^ "-" ^ name) ]
                [ text name ]
              ]
          ) elements)
      ]
  ) (Belt.Map.String.toList search_results.element_results |> List.rev)


let sidebar_search_results search_results =
  List.map (fun sr ->
    ul [ class' "search-result-module-list" ]
      ( a [ href ("#" ^ sr.module_group) ] [ text sr.module_group ] ::
        sidebar_element_results sr)
  ) search_results

let view_sidebar sidebar =
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
    ; h6 [ id "back-link" ] [ a [ href "https://www.streamingspring.com" ] [ text "<- Back to Blog" ] ]
    ; input'
        [ type' "text"
        ; id "search-bar"
        ; value sidebar.search_term
        ; onInput updateSearchTerm
        ; onEnter Search ]
        []
    ; div
        [ id "search-results"
        ; classList
            [ "show-results", List.length sidebar.search_results > 0 ]
        ]
        ([ div
            [ id "search-results-header" ]
            [ h3 [ id "search-results-title" ] [ text "Results" ]
            ; button [ id "clear-button"; onClick Clear ] [ text "Clear" ]
            ]
         ] @
          (sidebar_search_results sidebar.search_results))
    ; h3 [ id "modules-title" ] [ text "Modules" ]
    ; ul
        [ class' "module-links" ]
        (module_sidebar_links sidebar.sidebar_links)
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
    [ view_sidebar model.sidebar
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
