type name = string
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
  { module_name : string
  ; module_info : string
  ; sections    : section list
  }

type sec_element_type
  = Element
  | MainSection
  | SubSection
  | End

type modules = module_parts list

module Decode = struct
  open Json.Decode

  let variant parameter_decoder json =
    if Js.Array.isArray json then begin
      let source = (Obj.magic (json : Js.Json.t) : Js.Json.t array) in

      try
        let first = Array.unsafe_get source 0 in
        let decoded_type = string first in

        parameter_decoder (Array.unsafe_get source 1) decoded_type
      with
        DecodeError msg -> raise @@ DecodeError (msg ^ "\n\tin variant")
    end
    else
      raise @@ DecodeError ("Variant: Expected array, got " ^ Json.stringify json)

  let name = string
  let info = optional string

  let type_type       = string
  let type_table      = optional string
  let func_annotation = string
  let exec_parameter  = optional string

  let element_type_map parameters type_name =
    match type_name with
      | "Type" ->
          let (x, y, z) = parameters |> tuple3 name type_type info in
          Type (x, y, z)
      | "Typevariant" ->
          let (x, y, z) = parameters |> tuple3 name type_table info in
          Typevariant (x, y, z)
      | "Function" ->
          let (x, y, z) = parameters |> tuple3 name func_annotation info in
          Function (x, y, z)
      | "Exception" ->
          let (x, y, z) = parameters |> tuple3 name exec_parameter info in
          Exception (x, y, z)
      | "Module" ->
          let (x, y) = parameters |> tuple2 name info in
          Module (x, y)
      | "Moduletype" ->
          let (x, y)= parameters |> tuple2 name info in
          Moduletype (x, y)
      | _ -> failwith "Unknown element type"

  let element_type =
    variant element_type_map

  let rec section json =
    { section_name = json |> optional (field "section_name" string)
    ; section_info = json |> optional (field "section_info" string)
    ; elements     = json |> field "elements" (list element_type)
    ; sub_sections = json |> field "sub_sections" (list section)
    }

  let module_parts json =
    { module_name = json |> field "module_name" string
    ; module_info = json |> field "module_info" string
    ; sections    = json |> field "sections" (list section)
    }

  let decode_modules =
    list module_parts

end
