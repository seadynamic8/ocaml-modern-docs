(* #require "lambdasoup" *)

open Soup
open Parser_t

(* type name = string
type info = string option

type type_type       = string
type type_table      = string option
type func_annotation = string
type exec_parameter  = string option

type element_type
  = Type of        name * type_type * info
  | Typevariant of name * type_table * info
  | Function of    name * func_annotation * info
  | Exception of   name * exec_parameter * info
  | Module of      name * info
  | Moduletype of  name * info

type section =
  { section_name : string option
  ; section_info : string option
  ; elements     : element_type list
  ; sub_sections : section list
  }

type module_parts =
  { module_name        : string
  ; module_info : string
  ; sections    : section list
  }

type sec_element_type
  = Element
  | MainSection
  | SubSection
  | End

type 'a section_type
  = Section of 'a Soup.node
  | End *)


let blank_section =
  { section_name = None
  ; section_info = None
  ; elements     = []
  ; sub_sections = []
  }

let opt_default default option =
  match option with
  | Some v -> v
  | None -> default

let index_dir = "./htmlman/"

let ref_dir = "libref/"

let output_filename = "modules.json"

let parse_html_info node =
  node
  |> children
  |> to_list
  |> List.map (fun elem -> to_string elem)
  |> String.concat ""

let node_class node =
  let node_classes = node |> classes in
  match List.length node_classes with
  | 1 -> List.hd node_classes
  | _ -> ""

let rec parse_info_exception next_node info =
  match name next_node with
  | "p" | "ul" ->
      let next_next_node = next_node |> next_element in

      begin match next_next_node with
      | Some next_next_node ->
          parse_info_exception next_next_node (info ^ (to_string next_node))
      | None ->
          (Some info, None, Some next_node)
      end
  | _ ->
      (Some info, None, Some next_node)

(* Since there is no really grouping of nodes, we need to see what follows
  the primary "pre" node.
  There are a few variations that we need to see if they belong to the same
  node:

  - info is next node
    - param_info is next next node  (param_info gets appended onto info)
    - no next next node
  - typetable section only
    - info is next next node
    - no next next node
  - p / ul is next node (exception)
  - No extra nodes

  * No extra nodes -> meaning either the end or new section
*)
let parse_next_nodes node =
  match node |> next_element with
  | Some next_node ->

    begin match node_class next_node with
    | "info" ->
      let next_next_node = next_node |> next_element in
      let div_info = parse_html_info next_node in

      begin match next_next_node with
      | Some next_next_node ->

        begin match node_class next_next_node with
        (* Exception for Arg - has a param-info section *)
        | "param_info" ->
          let param_info = next_next_node |> to_string in
          let div_info = div_info ^ param_info in
          (Some div_info, None, next_next_node |> next_element)

        | _ ->
          (Some div_info, None, Some next_next_node)
        end

      | None ->
        (Some div_info, None, Some next_node)
      end

    | "typetable" ->
      let next_next_node = next_node |> next_element in
      let type_table = parse_html_info next_node in

      begin match next_next_node with
      | Some next_next_node ->

        begin match node_class next_next_node with
        | "info" ->
          let div_info = parse_html_info next_next_node in
          (Some div_info, Some type_table, next_next_node |> next_element)
        | _ ->
          (None, Some type_table, Some next_next_node)
        end

      | None ->
        (None, Some type_table, Some next_node)
      end

    (* Exception for Format -> Formatted pretty-printing (function info not in div) *)
    | _ when (name next_node = "p") || (name next_node = "ul") ->
      parse_info_exception next_node ""

    | _ -> (None, None, Some next_node)
    end

  | None ->
    (None, None, None)

let parse_element node =
  let (div_info, type_table, next_element) = parse_next_nodes node in

  let parsed_element =
    match trimmed_texts node with
    | "type" :: name :: _sep :: type_type :: _tl ->
        Type (name, type_type, div_info)
    | "type" :: name :: _tl ->
        Typevariant (name, type_table, div_info)
    | "val" :: name :: _sep :: tl ->
        let annotation = String.concat " " tl in
        Function (name, annotation, div_info)
    | "exception" :: name :: _sep :: parameter :: _tl ->
        Exception (name, Some parameter, div_info)
    | "exception" :: name :: _tl ->
        Exception (name, None, div_info)
    | "module" :: name :: _tl ->
        Module (name, div_info)
    | "module type" :: name :: _tl ->
        Moduletype (name, div_info)
    | _ ->
        let _ = print_endline ("prev node: " ^ (node |> R.previous_element |> name)) in
        raise (Failure ("Can't parse element: " ^ (node |> name) ^
          "\n trimmed_texts: " ^ (node |> trimmed_texts |> String.concat "") ))
  in
    (next_element, parsed_element)


let is_sect_exception node =
  let prev_node = node |> R.previous_element |> name in
    prev_node = "div"

let is_section_element node =
  match node with
  | Some node ->
      let section =
        begin match name node with
        | "h2" | "h4" | "h7" ->
            MainSection
        | "p" when is_sect_exception node ->
            MainSection
        | "h3" ->
            SubSection
        | "div" when node_class node = "h8" ->
            SubSection
        | "p" | "ul" | "pre" ->
            Element
        | _ ->
            End
        end
      in
        (section, node)

  | None ->
      (End, create_element "empty")

let parse_section_header (node, section) =
  match name node with
  | "h2" | "h3" | "h4" | "h7" | "div" ->
      (node |> next_element,
      { section with section_name = Some (node |> R.leaf_text) })

  | _ ->
      (Some node, section)

let rec parse_section_info (node, section) =
  match node with
  | Some node ->
    begin match name node with
    | "p" | "ul" ->
        parse_section_info
          (node |> next_element,
          { section with
            section_info =
              Some ((opt_default "" section.section_info) ^ (to_string node)) })

    | _ ->
        (Some node, section)
    end

  | None ->
    (None, section)

let process_section_top (node, section) =
  (node, section)
  |> parse_section_header
  |> parse_section_info

(* in_sub_section flag is used for only one level deep *)
let rec process_section_elements in_sub_section (node, section) =
  match is_section_element node with
  | End, _ ->
      (None,
      { section with
        elements     = List.rev section.elements
      ; sub_sections = List.rev section.sub_sections })

  | MainSection, node ->
      (Some node,
      { section with
        elements     = List.rev section.elements
      ; sub_sections = List.rev section.sub_sections })

  | SubSection, node ->
      if in_sub_section = true then
        (Some node,
        { section with
          elements     = List.rev section.elements
        ; sub_sections = List.rev section.sub_sections })
      else
        let (next_element, sub_section) =
          (node, blank_section)
          |> process_section_top
          |> process_section_elements true
        in

        process_section_elements
          false
          (next_element,
          { section with sub_sections = sub_section :: section.sub_sections })

  | Element, node ->
      let (next_element, element) = parse_element node in
      process_section_elements
        in_sub_section
        (next_element,
        { section with elements = element :: section.elements })

let process_section (node, section) =
  (node, section)
  |> process_section_top
  |> process_section_elements false

let is_new_section node =
  match node with
  | Some node ->
      begin match name node with
      | "h2" | "h3" | "h4" | "h7" | "p" | "ul" | "pre" -> Some node
      | _ -> None
      end
  | None -> None

(* Sections are either the default one with no heading or has a heading *)
let rec process_sections (node, m) =
  match is_new_section node with
  | None ->
      { m with sections = List.rev m.sections }

  | Some node ->
      let (next_section_node, new_section) = process_section (node, blank_section) in
      process_sections
        (next_section_node, { m with sections = new_section :: m.sections })

let process_after_top (node, m) =
  let node = node $ "hr" |> next_element in
    process_sections (node, m)

let parse_top (node, m) =
  let module_name = node $ "h1 a" |> R.leaf_text in
  let module_info = node $ ".module.top .info-desc" in
    (node,
    { m with
      module_name;
      module_info = parse_html_info module_info })

let blank_module =
  { module_name = ""
  ; module_info = ""
  ; sections    = []
  }

let parse_file file =
  let top_node = read_file file |> parse in

  (top_node, blank_module)
  |> parse_top
  |> process_after_top


let index_node = read_file (index_dir ^ "stdlib.html") |> parse

let extra_files =
  [ "Pervasives.html"
  ; "Map.Make.html"
  ; "Str.html"
  ; "Unix.html"
  ; "UnixLabels.html"
  ; "Thread.html"
  ; "Mutex.html"
  ; "Condition.html"
  ; "Event.html"
  ; "ThreadUnix.html"
  ; "Graphics.html"
  ; "Dynlink.html"
  ]

let standard_files =
  index_node $$ ".li-links a"
  |> to_list
  |> List.map (fun link -> R.attribute "href" link)

let all_sorted_files =
  extra_files
  |> List.fold_left (fun standard_files file ->
      (ref_dir ^ file) :: standard_files
    ) standard_files
  |> List.sort compare

let _ =
  let ch = open_out output_filename in

    all_sorted_files
    |> List.map (fun file ->
        try
          parse_file (index_dir ^ file)
        with
          | Failure msg ->
            print_endline ("error file: " ^ file ^ "\n error msg: " ^ msg);
            blank_module
      )
    |> Parser_j.string_of_modules
    |> Yojson.Safe.prettify
    |> output_string ch;

    close_out ch
