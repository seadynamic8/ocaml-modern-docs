open Tea
open Html

open Decoder

let innerHTML html =
  Vdom.prop "innerHTML" html

(* Elements *)

let element_header_content module_name category name content =
  let trimmed_category = category |> String.trim in
  match trimmed_category with
  | "module" | "module type" ->
      [ span []
          [ a
              [ href ("#" ^ module_name ^ "." ^ name)
              ; class' "name" ]
              [ text name ]
          ; text content
          ]
      ]
  | _ ->
      [ text (name ^ content) ]

let view_element_header module_name category name content element_html =
  (h5
    [ id name; class' "element-item" ]

    (span [ class' "category" ] [ text category ]
      :: element_header_content module_name category name content)
  )
  :: element_html

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

let view_element module_name element_html element =
  match element with
  | Type (name, type_type, info) ->
      element_html
      |> view_element_info info
      |> view_element_header module_name "type " name (" = " ^ type_type)
  | Typevariant (name, type_extra, type_table, info) ->
      element_html
      |> view_element_info info
      |> view_element_type_table type_table
      |> view_element_header module_name "type " name (parse_type_extra type_extra)
  | Function (name, func_annotation, info) ->
      element_html
      |> view_element_info info
      |> view_element_header module_name "val " name (" : " ^ func_annotation)
  | Exception (name, exec_parameter, info) ->
      element_html
      |> view_element_info info
      |> view_element_header module_name "exception " name (parse_exception exec_parameter)
  | Module (name, info) ->
      element_html
      |> view_element_info info
      |> view_element_header module_name "module " name (": sig .. end")
  | Moduletype (name, info) ->
      element_html
      |> view_element_info info
      |> view_element_header module_name "module type " name (": sig .. end")
  | Include name ->
      view_element_header module_name "include " name "" element_html

let view_elements module_name elements =
  List.map (fun e ->
    li [ class' "element" ] (view_element module_name [] e)
  ) elements

(* Sections *)

let view_section_name section_name =
  match section_name with
  | Some name -> h2 [ class' "subtitle" ] [ text name ]
  | None -> span [] []

let view_section_info section_info =
  match section_info with
  | Some info -> div [ class' "info"; innerHTML info ] []
  | None -> span [] []

let rec view_section module_name section =
  li [ class' "section" ]
    [ view_section_name section.section_name
    ; view_section_info section.section_info
    ; ul
        []
        (view_elements module_name section.elements)
    ; ul
        [ class' "sub-section" ]
        (List.map (fun ss -> view_section module_name ss) section.sub_sections)
    ]

let view_sections module_item =
  List.map (fun s -> view_section module_item.module_name s) module_item.sections

let view_functor_table functor_info top_html =
  if String.length functor_info.table > 0 then
    top_html @ [ table [ innerHTML functor_info.table ] [] ]
  else
    top_html

let view_module_info module_info top_html =
  if String.length module_info > 0 then
    top_html @ [ div [ class' "info"; innerHTML module_info ] [] ]
  else
    top_html

let view_functor_sig module_name functor_info top_html =
  [ div [ id "functor-begin-sig"; innerHTML functor_info.begin_sig ] []
  ; ul [ id "functor-elements" ] (view_elements module_name functor_info.functor_elements)
  ; div [ id "functor-begin-sig"; innerHTML functor_info.end_sig ] []
  ] @ top_html

let view_module_top module_item =
  let module_info = module_item.module_info in
  match module_item.functor_info with
  | Some functor_info ->
    []
    |> view_functor_sig module_item.module_name functor_info
    |> view_module_info module_info
    |> view_functor_table functor_info
  | None ->
    view_module_info module_info []

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
      ; div [ id "module-top" ] (view_module_top module_item)
      ; hr [] []
      ; section
        [ id "elements" ]
        [ ul [] (view_sections module_item) ]
      ]
  ]
