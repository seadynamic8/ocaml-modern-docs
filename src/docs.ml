open Tea
open Html

open Types
open Decoder
open Sidebar
open Content
open Home_page
open License

module Task = Tea_task

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
    ; icon_selected = false
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

let flipSidebarLinkSelected sidebar_link_name is_functions sidebar_link =
  if sidebar_link.name = sidebar_link_name then
    begin match is_functions with
    | true ->
      { sidebar_link with functions_selected = not sidebar_link.functions_selected }
    | false ->
      { sidebar_link with selected = not sidebar_link.selected }
    end
  else
    sidebar_link

let flipSidebarIconSelected condition sidebar =
  if condition then
    { sidebar with icon_selected = not sidebar.icon_selected }
  else
    sidebar

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

      | Typevariant (name, _, _, _) when search_term_matches_name search_term name ->
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

let update model msg =
  match msg with
  | UrlChange location ->
      let icon_selected =
        if model.sidebar.icon_selected then
          not model.sidebar.icon_selected
        else
          model.sidebar.icon_selected
      in
      { model with
        history = location :: model.history
      ; page = parse_hash_value location.Web.Location.hash
      ; sidebar = { model.sidebar with icon_selected }
      },
      Task.perform (fun _ -> Scroll) (Task.succeed ())

  | ClickedSidebarLink (sidebar_link_name, is_functions) ->
      let sidebar_links =
        List.map
          (flipSidebarLinkSelected sidebar_link_name is_functions)
          model.sidebar.sidebar_links
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

  | ClickedSidebarIcon ->
      { model with
        sidebar =
          { model.sidebar with
            icon_selected = not model.sidebar.icon_selected }
      }, Cmd.none


(* ------------ ViEW -------------- *)


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
      model.module_list
      |> List.find (fun m -> m.module_name = name)
      |> view_content
    with
      | Not_found ->
        [ main
            [ id "not-found"; class' "content" ]
            [ h1 [ class' "title" ] [ text "Not Found" ] ]
        ]

let view model =
  div [ id "site-container" ]
    [ div
        [ id "backdrop"
        ; classList [ "selected", model.sidebar.icon_selected ]
        ; onClick ClickedSidebarIcon
        ]
        []
    ; view_sidebar model.sidebar
    ; i
        [ classList
            [ "fas fa-bars fa-lg", true
            ; "selected", model.sidebar.icon_selected
            ]
        ; id "sidebar-icon"
        ; onClick ClickedSidebarIcon
        ] []
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
