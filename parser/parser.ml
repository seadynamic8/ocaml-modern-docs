open Soup
open Parser_t

type sec_element_type =
  | Element
  | MainSection
  | SubSection
  | End

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
  let html_info =
    node
    |> children
    |> to_list
    |> List.map (fun elem -> to_string elem)
    |> String.concat ""
  in
    (node |> next_element, html_info)

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
      let (next_next_node, div_info) = parse_html_info next_node in

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
        (* let _ = print_endline ("name: " ^ (next_node |> R.parent |> R.next_element |> to_string)) in *)
        (Some div_info, None, None)
      end

    | "typetable" ->
      let (next_next_node, type_table) = parse_html_info next_node in

      begin match next_next_node with
      | Some next_next_node ->

        (* Exception for Location.html *)
        if name next_next_node = "p" then
          let (next_next_next_node, div_info) = parse_html_info next_next_node in
          (* let _ = print_endline ("type_table: " ^ type_table) in *)

          (Some div_info, Some type_table, next_next_next_node)
        else
          begin match node_class next_next_node with
          | "info" ->
            let (next_next_next_node, div_info) = parse_html_info next_next_node in

            (Some div_info, Some type_table, next_next_next_node)
          | _ ->
            (None, Some type_table, Some next_next_node)
          end

      | None ->
        (None, Some type_table, None)
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
    | "type" :: name :: ":" :: type_type :: _tl ->
        Type (name, type_type, div_info)
    | "type" :: name :: type_extra ->
        let type_extra =
          if List.length type_extra > 0 then
            Some (String.concat " " type_extra)
          else
            None
        in
        Typevariant (name, type_extra, type_table, div_info)
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
    | "include" :: name :: _tl ->
        Include name
    | _ ->
        let _ = print_endline ("prev node: " ^ (node |> R.previous_element |> name)) in
        raise (Failure ("Can't parse element: " ^ (node |> name) ^
          "\n trimmed_texts: " ^ (node |> trimmed_texts |> String.concat "|") ))
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
  | None -> { m with sections = List.rev m.sections }

  | Some node ->
      let (next_section_node, new_section) = process_section (node, blank_section) in
      process_sections
        (next_section_node, { m with sections = new_section :: m.sections })

let process_after_top (node, m) =
  let node =
    if name node = "hr" then
      node |> next_element
    else
      raise (Failure "this should be 'hr' element")
  in
    process_sections (node, m)


let blank_functor =
  { begin_sig        = ""
  ; functor_elements = []
  ; end_sig          = ""
  ; table            = ""
  }

let parse_module_name (node, m) =
  let node = node $ "h1" in
  let m =
    match node |> trimmed_texts with
    | "Module" :: [ module_name ] -> { m with module_name }
    | "Module type" :: [ module_name ] ->
        { m with module_name; is_module_type = true }
    | "Functor" :: [ module_name ] ->
        { m with module_name; functor_info = Some blank_functor }
    | mod_name ->
        raise (Failure ("can't parse module name: " ^ (mod_name |> String.concat " ")))
  in
    (node |> R.next_element, m)

let rec parse_functor_element_nodes ?(is_finished=false) node functor_elements =
  match name node with
  | _ when is_finished ->
    (node |> R.parent |> R.next_element, List.rev functor_elements)
  | "pre" ->
    let (next_node, parsed_element) = parse_element node in

    begin match next_node with
    | Some n ->
      parse_functor_element_nodes n (parsed_element :: functor_elements)
    | None ->
      parse_functor_element_nodes ~is_finished:true node (parsed_element :: functor_elements)
    end
  | _ ->
    raise (Failure ("Error: parse_funtor_element_nodes: " ^ (name node)
                                         ^ " to_string: " ^ (to_string node)))

let parse_functor_elements node functor_elements =
  if (name node = "div") && (node |> classes |> List.hd = "sig_block") then
    parse_functor_element_nodes (node |> R.child_element) functor_elements
  else
    (node, functor_elements)

let parse_module_info (node, m) =
  let node = node |> R.next_element in (* ignore top module sig *)
  match node |> classes with
  | ["info"; _module; "top"] ->
      let (_next_node, module_info) = parse_html_info (node |> R.child_element) in
      (node |> R.next_element, { m with module_info })
  | _ -> (node, { m with module_info = "" })

let parse_end_sig node =
  if name node = "pre" then
    parse_html_info node
  else
    (Some node, "")

let parse_functor_module_info node =
  if name node = "div" then
    parse_html_info node
  else
    (Some node, "")

let parse_functor_table node =
  if name node = "table" then
    parse_html_info node
  else
    (Some node, "")

let parse_functor_info (node, m) =
  match m.functor_info with
  | Some _functor ->
      let (next_node, begin_sig)        = parse_html_info node in
      let (next_node, functor_elements) = parse_functor_elements (require next_node) [] in
      let (next_node, end_sig)          = parse_end_sig next_node in
      let (next_node, module_info)      = parse_functor_module_info (require next_node) in
      let (next_node, table)            = parse_functor_table (require next_node) in

      let functor_info = Some { begin_sig; functor_elements; end_sig; table } in
      (require next_node, { m with module_info; functor_info })
  | None ->
      parse_module_info (node, m)

let parse_top (node, m) =
  (node, m)
  |> parse_module_name
  |> parse_functor_info

let mark_standard_file standard_files file (node, m) =
  if List.mem file standard_files then
    (node, { m with is_standard = true })
  else
    (node, m)

let blank_module =
  { module_name    = ""
  ; module_info    = ""
  ; sections       = []
  ; is_standard    = false
  ; is_module_type = false
  ; functor_info   = None
  }

let parse_file standard_files file =
  let top_node = read_file (index_dir ^ ref_dir ^ file) |> parse in

  (top_node, blank_module)
  |> mark_standard_file standard_files file
  |> parse_top
  |> process_after_top

let get_standard_files =
  let std_files =
    (index_dir ^ "stdlib.html")
    |> read_file
    |> parse
    |> select ".li-links a"
    |> to_list
    |> List.map (fun link ->
      let href = R.attribute "href" link in
      match Str.split (Str.regexp "/") href with
      | _hd :: [ filename ] -> filename
      | _ -> raise (Failure "Can't split filename")
      )
  in
    List.sort compare ("Pervasives.html" :: std_files)

let all_files =
  (index_dir ^ ref_dir)
  |> Sys.readdir
  |> Array.to_list
  |> List.filter (fun file ->
    try
      if (Str.search_forward (Str.regexp "index\\|type\\|style") file 0) == 0 then
         false
      else
         true
    with
      Not_found -> true
    )

let _ =
  let ch = open_out output_filename in
  let standard_files = get_standard_files in

    all_files
    |> List.map (fun file ->
        try
          parse_file standard_files file
        with
          | Failure msg ->
            print_endline ("error file: " ^ file ^ "\n error msg: " ^ msg);
            blank_module
      )
    |> Parser_j.string_of_modules
    |> Yojson.Safe.prettify
    |> output_string ch;

    close_out ch
