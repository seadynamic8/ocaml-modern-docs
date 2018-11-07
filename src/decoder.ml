type name = string
type info = string option

type type_type       = string
type type_extra      = string option
type type_table      = string option
type func_annotation = string
type exec_parameter  = string option

type element_type
  = Type of        name * type_type * info
  | Typevariant of name * type_extra * type_table * info
  | Function of    name * func_annotation * info
  | Exception of   name * exec_parameter * info
  | Module of      name * info
  | Moduletype of  name * info
  | Include of     name

type section =
  { section_name : string option
  ; section_info : string option
  ; elements     : element_type list
  ; sub_sections : section list
  }

type functor_parts =
  { begin_sig         : string
  ; functor_elements  : element_type list
  ; end_sig           : string
  ; table             : string
  }

type module_parts =
  { module_name    : string
  ; module_info    : string
  ; sections       : section list
  ; is_standard    : bool
  ; is_module_type : bool
  ; functor_info   : functor_parts option
  }

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
  let type_extra      = optional string
  let type_table      = optional string
  let func_annotation = string
  let exec_parameter  = optional string

  let element_type_map parameters type_name =
    match type_name with
      | "Type" ->
          let (x, y, z) = parameters |> tuple3 name type_type info in
          Type (x, y, z)
      | "Typevariant" ->
          let (a, b, c, d) = parameters |> tuple4 name type_extra type_table info in
          Typevariant (a, b, c, d)
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
          let (x, y) = parameters |> tuple2 name info in
          Moduletype (x, y)
      | "Include" ->
          let x = parameters |> name in
          Include x
      | _ -> failwith "Unknown element type"

  let element_type =
    variant element_type_map

  let rec section json =
    { section_name = json |> optional (field "section_name" string)
    ; section_info = json |> optional (field "section_info" string)
    ; elements     = json |> field "elements" (list element_type)
    ; sub_sections = json |> field "sub_sections" (list section)
    }

  let functor_parts json =
    { begin_sig        = json |> field "begin_sig" string
    ; functor_elements = json |> field "functor_elements" (list element_type)
    ; end_sig          = json |> field "end_sig" string
    ; table            = json |> field "table" string
    }

  let module_parts json =
    { module_name = json |> field "module_name" string
    ; module_info = json |> field "module_info" string
    ; sections    = json |> field "sections" (list section)
    ; is_standard = json |> field "is_standard" bool
    ; is_module_type = json |> field "is_module_type" bool
    ; functor_info = json |> optional (field "functor_info" functor_parts)
    }

  let decode_modules =
    list module_parts

end
