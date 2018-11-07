// Generated by BUCKLESCRIPT VERSION 4.0.5, PLEASE EDIT WITH CARE
'use strict';

var List = require("bs-platform/lib/js/list.js");
var Vdom = require("bucklescript-tea/src-ocaml/vdom.js");
var Tea_html = require("bucklescript-tea/src-ocaml/tea_html.js");
var Pervasives = require("bs-platform/lib/js/pervasives.js");

function innerHTML(html) {
  return Vdom.prop("innerHTML", html);
}

function view_element_header(name, category, content, element_html) {
  return /* :: */[
          Tea_html.h5(undefined, undefined, /* :: */[
                Tea_html.id(name),
                /* :: */[
                  Tea_html.class$prime("element-item"),
                  /* [] */0
                ]
              ], /* :: */[
                Tea_html.span(undefined, undefined, /* :: */[
                      Tea_html.class$prime("category"),
                      /* [] */0
                    ], /* :: */[
                      Tea_html.text(category),
                      /* [] */0
                    ]),
                /* :: */[
                  Tea_html.text(name + content),
                  /* [] */0
                ]
              ]),
          element_html
        ];
}

function view_element_info(info, element_html) {
  if (info !== undefined) {
    return /* :: */[
            Tea_html.div(undefined, undefined, /* :: */[
                  Tea_html.class$prime("info"),
                  /* :: */[
                    Vdom.prop("innerHTML", info),
                    /* [] */0
                  ]
                ], /* [] */0),
            element_html
          ];
  } else {
    return element_html;
  }
}

function view_element_type_table(type_table, element_html) {
  if (type_table !== undefined) {
    return /* :: */[
            Tea_html.table(undefined, undefined, /* :: */[
                  Vdom.prop("innerHTML", type_table),
                  /* [] */0
                ], /* [] */0),
            element_html
          ];
  } else {
    return element_html;
  }
}

function parse_exception(exec_parameter) {
  if (exec_parameter !== undefined) {
    return " of " + exec_parameter;
  } else {
    return "";
  }
}

function parse_type_extra(type_extra) {
  if (type_extra !== undefined) {
    return type_extra;
  } else {
    return "";
  }
}

function view_element(element_html, element) {
  switch (element.tag | 0) {
    case 0 : 
        return view_element_header(element[0], "type ", " = " + element[1], view_element_info(element[2], element_html));
    case 1 : 
        return view_element_header(element[0], "type ", parse_type_extra(element[1]), view_element_type_table(element[2], view_element_info(element[3], element_html)));
    case 2 : 
        return view_element_header(element[0], "val ", " : " + element[1], view_element_info(element[2], element_html));
    case 3 : 
        return view_element_header(element[0], "exception ", parse_exception(element[1]), view_element_info(element[2], element_html));
    case 4 : 
        return view_element_header(element[0], "module ", ": sig .. end", view_element_info(element[1], element_html));
    case 5 : 
        return view_element_header(element[0], "module type ", ": sig .. end", view_element_info(element[1], element_html));
    case 6 : 
        return view_element_header(element[0], "include ", "", element_html);
    
  }
}

function view_elements(elements) {
  return List.map((function (e) {
                return Tea_html.li(undefined, undefined, /* :: */[
                            Tea_html.class$prime("element"),
                            /* [] */0
                          ], view_element(/* [] */0, e));
              }), elements);
}

function view_section_name(section_name) {
  if (section_name !== undefined) {
    return Tea_html.h2(undefined, undefined, /* :: */[
                Tea_html.class$prime("subtitle"),
                /* [] */0
              ], /* :: */[
                Tea_html.text(section_name),
                /* [] */0
              ]);
  } else {
    return Tea_html.span(undefined, undefined, /* [] */0, /* [] */0);
  }
}

function view_section_info(section_info) {
  if (section_info !== undefined) {
    return Tea_html.div(undefined, undefined, /* :: */[
                Tea_html.class$prime("info"),
                /* :: */[
                  Vdom.prop("innerHTML", section_info),
                  /* [] */0
                ]
              ], /* [] */0);
  } else {
    return Tea_html.span(undefined, undefined, /* [] */0, /* [] */0);
  }
}

function view_section(section) {
  return Tea_html.li(undefined, undefined, /* :: */[
              Tea_html.class$prime("section"),
              /* [] */0
            ], /* :: */[
              view_section_name(section[/* section_name */0]),
              /* :: */[
                view_section_info(section[/* section_info */1]),
                /* :: */[
                  Tea_html.ul(undefined, undefined, /* [] */0, view_elements(section[/* elements */2])),
                  /* :: */[
                    Tea_html.ul(undefined, undefined, /* :: */[
                          Tea_html.class$prime("sub-section"),
                          /* [] */0
                        ], List.map(view_section, section[/* sub_sections */3])),
                    /* [] */0
                  ]
                ]
              ]
            ]);
}

function view_sections(module_item) {
  return List.map(view_section, module_item[/* sections */2]);
}

function view_functor_table(functor_info, top_html) {
  if (functor_info[/* table */3].length !== 0) {
    return Pervasives.$at(top_html, /* :: */[
                Tea_html.table(undefined, undefined, /* :: */[
                      Vdom.prop("innerHTML", functor_info[/* table */3]),
                      /* [] */0
                    ], /* [] */0),
                /* [] */0
              ]);
  } else {
    return top_html;
  }
}

function view_module_info(module_info, top_html) {
  if (module_info.length !== 0) {
    return Pervasives.$at(top_html, /* :: */[
                Tea_html.div(undefined, undefined, /* :: */[
                      Tea_html.class$prime("info"),
                      /* :: */[
                        Vdom.prop("innerHTML", module_info),
                        /* [] */0
                      ]
                    ], /* [] */0),
                /* [] */0
              ]);
  } else {
    return top_html;
  }
}

function view_functor_sig(functor_info, top_html) {
  return Pervasives.$at(/* :: */[
              Tea_html.div(undefined, undefined, /* :: */[
                    Vdom.prop("innerHTML", functor_info[/* begin_sig */0]),
                    /* [] */0
                  ], /* [] */0),
              /* :: */[
                Tea_html.ul(undefined, undefined, /* [] */0, view_elements(functor_info[/* functor_elements */1])),
                /* :: */[
                  Tea_html.div(undefined, undefined, /* :: */[
                        Vdom.prop("innerHTML", functor_info[/* end_sig */2]),
                        /* [] */0
                      ], /* [] */0),
                  /* [] */0
                ]
              ]
            ], top_html);
}

function view_module_top(module_item) {
  var module_info = module_item[/* module_info */1];
  var match = module_item[/* functor_info */5];
  if (match !== undefined) {
    var functor_info = match;
    return view_functor_table(functor_info, view_module_info(module_info, view_functor_sig(functor_info, /* [] */0)));
  } else {
    return view_module_info(module_info, /* [] */0);
  }
}

function parse_module_name(module_item) {
  var match = module_item[/* functor_info */5];
  var is_functor = match !== undefined;
  var module_identifier = module_item[/* is_module_type */4] ? "Module Type: " : (
      is_functor ? "Functor: " : ""
    );
  return module_identifier + module_item[/* module_name */0];
}

function view_content(module_item) {
  return /* :: */[
          Tea_html.main(undefined, undefined, /* :: */[
                Tea_html.id("module-content"),
                /* :: */[
                  Tea_html.class$prime("content"),
                  /* [] */0
                ]
              ], /* :: */[
                Tea_html.h1(undefined, undefined, /* :: */[
                      Tea_html.class$prime("title"),
                      /* [] */0
                    ], /* :: */[
                      Tea_html.text(parse_module_name(module_item)),
                      /* [] */0
                    ]),
                /* :: */[
                  Tea_html.div(undefined, undefined, /* :: */[
                        Tea_html.class$prime("module_top"),
                        /* [] */0
                      ], view_module_top(module_item)),
                  /* :: */[
                    Tea_html.hr(undefined, undefined, /* [] */0, /* [] */0),
                    /* :: */[
                      Tea_html.section(undefined, undefined, /* :: */[
                            Tea_html.id("elements"),
                            /* [] */0
                          ], /* :: */[
                            Tea_html.ul(undefined, undefined, /* [] */0, view_sections(module_item)),
                            /* [] */0
                          ]),
                      /* [] */0
                    ]
                  ]
                ]
              ]),
          /* [] */0
        ];
}

exports.innerHTML = innerHTML;
exports.view_element_header = view_element_header;
exports.view_element_info = view_element_info;
exports.view_element_type_table = view_element_type_table;
exports.parse_exception = parse_exception;
exports.parse_type_extra = parse_type_extra;
exports.view_element = view_element;
exports.view_elements = view_elements;
exports.view_section_name = view_section_name;
exports.view_section_info = view_section_info;
exports.view_section = view_section;
exports.view_sections = view_sections;
exports.view_functor_table = view_functor_table;
exports.view_module_info = view_module_info;
exports.view_functor_sig = view_functor_sig;
exports.view_module_top = view_module_top;
exports.parse_module_name = parse_module_name;
exports.view_content = view_content;
/* Tea_html Not a pure module */
