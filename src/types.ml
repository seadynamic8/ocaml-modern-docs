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
  | ClickedSidebarIcon
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
  ; icon_selected : bool
  }

type model =
  { history : Web.Location.location list
  ; module_list : Decoder.modules
  ; sidebar : sidebar
  ; page : page
  }
