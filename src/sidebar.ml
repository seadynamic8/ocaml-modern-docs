open Tea
open Html

open Types

let onEnter msg =
  let isEnter code =
    if code = 13 then
      Json.Decoder.succeed msg
    else
      Json.Decoder.fail "not ENTER"
  in
    on "keydown" (Json.Decoder.andThen isEnter keyCode)

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

let view_sidebar_link sidebar_link =
  li
    []
    [ a
        [ onClick (ClickedSidebarLink (sidebar_link.name, false))
        ]
        [ text sidebar_link.name ]
    ; sublist_sidebar sidebar_link
    ]

let module_sidebar_links sidebar =
  if sidebar.switch_selected then
    List.map (fun s -> view_sidebar_link s) sidebar.sidebar_links
  else
    sidebar.sidebar_links
    |> List.filter (fun s -> s.is_mod_standard)
    |> List.map (fun s -> view_sidebar_link s)

let module_links_header =
  div
    [ class' "module-links-header" ]
    [ h3 [ id "modules-title" ] [ text "Modules" ]
    ; div
        [ class' "switch" ]
        [ label [] [ text "Standard" ]
        ; label
            [ class' "switch-selector" ]
            [ input'
                [ type' "checkbox"
                ; onClick ToggleModuleSwitch
                ]
                []
            ; span [ class' "slider round" ] []
            ]
        ; label [] [ text "All" ]
        ]
    ]

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


let sidebar_search_results_list search_results =
  List.map (fun sr ->
    ul [ class' "search-result-module-list" ]
      ( a [ href ("#" ^ sr.module_group) ] [ text sr.module_group ] ::
        sidebar_element_results sr)
  ) search_results

let sidebar_search_results search_results =
  [ div
      [ id "search-results-header" ]
      [ h3 [ id "search-results-title" ] [ text "Results" ]
      ; button [ id "clear-button"; onClick Clear ] [ text "Clear" ]
      ]
   ] @ (sidebar_search_results_list search_results)

let sidebar_content sidebar =
  [ div
      [ id "search-results"
      ; classList [ "show-results", List.length sidebar.search_results > 0 ]
      ]
      (sidebar_search_results sidebar.search_results)
  ; module_links_header
  ; ul
      [ class' "module-links" ]
      (module_sidebar_links sidebar)
  ]

let sidebar_header sidebar =
  [ a
      [ href "#docs_home" ]
      [ img [ src "https://ocaml.org/img/colour-logo-white.svg"; id "logo" ] []
      ]
  ; h5 [ id "version" ] [ text "v4.07 (Unofficial)" ]
  ; h6
      []
      [ a
          [ href "https://caml.inria.fr/pub/docs/manual-ocaml-4.07/" ]
          [ text "Official Docs" ]
      ]
  ; h6 [] [ a [ href "https://ocaml.org/" ] [ text "Official Website" ] ]
  ; h6
      [ id "back-link" ]
      [ a [ href "https://www.streamingspring.com" ]
          [ i [ class' "fas fa-angle-double-left fa-lg" ] []
          ; text " Back to Blog" ]
      ]
  ; input'
      [ type' "text"
      ; id "search-bar"
      ; value sidebar.search_term
      ; onInput updateSearchTerm
      ; onEnter Search
      ; placeholder "Search" ]
      []
  ]

let view_sidebar sidebar =
  aside
    [ classList
        [ "sidebar", true
        ; "selected", sidebar.icon_selected
        ]
    ]
    [ div
        [ id "sidebar-header" ]
        (sidebar_header sidebar)
    ; div
        [ id "sidebar-content" ]
        (sidebar_content sidebar)
    ]
