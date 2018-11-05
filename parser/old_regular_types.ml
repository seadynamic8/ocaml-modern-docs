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
