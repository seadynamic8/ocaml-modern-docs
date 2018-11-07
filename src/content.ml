open Tea
open Html

open Decoder

let innerHTML html =
  Vdom.prop "innerHTML" html

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

let parse_type_extra type_extra =
  match type_extra with
  | Some extra -> extra
  | None -> ""

let view_element element_html element =
  match element with
  | Type (name, type_type, info) ->
      element_html
      |> view_element_info info
      |> view_element_header name "type " (" = " ^ type_type)
  | Typevariant (name, type_extra, type_table, info) ->
      element_html
      |> view_element_info info
      |> view_element_type_table type_table
      |> view_element_header name "type " (parse_type_extra type_extra)
  | Function (name, func_annotation, info) ->
      element_html
      |> view_element_info info
      |> view_element_header name "val " (" : " ^ func_annotation)
  | Exception (name, exec_parameter, info) ->
      element_html
      |> view_element_info info
      |> view_element_header name "exception " (parse_exception exec_parameter)
  | Module (name, info) ->
      element_html
      |> view_element_info info
      |> view_element_header name "module " (": sig .. end")
  | Moduletype (name, info) ->
      element_html
      |> view_element_info info
      |> view_element_header name "module type " (": sig .. end")
  | Include name ->
      view_element_header name "include " "" element_html

let view_elements elements =
  List.map (fun e -> li [ class' "element" ] (view_element [] e)) elements

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


let parse_module_name module_item =
  let is_functor =
    match module_item.functor_info with
    | Some _f -> true
    | None -> false
  in
  let module_identifier =
    if module_item.is_module_type then
      "Module Type: "
    else if is_functor then
      "Functor: "
    else (* Module *)
      ""
  in
    module_identifier ^ module_item.module_name


let view_content module_item =
  [ main
      [ id "module-content"; class' "content" ]
      [ h1 [ class' "title" ] [ text (parse_module_name module_item) ]
      ; div [ class' "info"; innerHTML module_item.module_info ] []
      ; hr [] []
      ; section
        [ id "elements" ]
        [ ul [] (view_sections module_item) ]
      ]
  ]
