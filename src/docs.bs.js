// Generated by BUCKLESCRIPT VERSION 4.0.5, PLEASE EDIT WITH CARE
'use strict';

var Fs = require("fs");
var List = require("bs-platform/lib/js/list.js");
var Vdom = require("bucklescript-tea/src-ocaml/vdom.js");
var Block = require("bs-platform/lib/js/block.js");
var Decoder = require("./decoder.bs.js");
var License = require("./license.bs.js");
var Tea_cmd = require("bucklescript-tea/src-ocaml/tea_cmd.js");
var Tea_sub = require("bucklescript-tea/src-ocaml/tea_sub.js");
var Tea_html = require("bucklescript-tea/src-ocaml/tea_html.js");
var Tea_task = require("bucklescript-tea/src-ocaml/tea_task.js");
var Home_page = require("./home_page.bs.js");
var Tea_navigation = require("bucklescript-tea/src-ocaml/tea_navigation.js");
var Caml_builtin_exceptions = require("bs-platform/lib/js/caml_builtin_exceptions.js");

function urlChange(param_0) {
  return /* UrlChange */Block.__(0, [param_0]);
}

function clickedSidebarLink(param_0, param_1) {
  return /* ClickedSidebarLink */Block.__(1, [
            param_0,
            param_1
          ]);
}

var json = JSON.parse(Fs.readFileSync("parser/modules.json", "utf8"));

var get_module_list = Decoder.Decode[/* decode_modules */11](json);

function get_function_names(elements, functions) {
  return List.rev(List.fold_left((function (functions, e) {
                    if (e.tag === 2) {
                      return /* :: */[
                              e[0],
                              functions
                            ];
                    } else {
                      return functions;
                    }
                  }), functions, elements));
}

function functions_in_sub_sections(sub_sections, functions) {
  return List.rev(List.fold_left((function (functions, ss) {
                    return get_function_names(ss[/* elements */2], functions);
                  }), functions, sub_sections));
}

function functions_in_section(section) {
  return get_function_names(section[/* elements */2], functions_in_sub_sections(section[/* sub_sections */3], /* [] */0));
}

function get_function_list(sections) {
  return List.flatten(List.rev(List.fold_left((function (functions, s) {
                        return /* :: */[
                                functions_in_section(s),
                                functions
                              ];
                      }), /* [] */0, sections)));
}

function create_sidebar_link_state(module_list) {
  return List.map((function (m) {
                return /* record */[
                        /* name */m[/* module_name */0],
                        /* selected */false,
                        /* functions */get_function_list(m[/* sections */2]),
                        /* functions_selected */false
                      ];
              }), module_list);
}

function init(_, $$location) {
  return /* tuple */[
          /* record */[
            /* history : :: */[
              $$location,
              /* [] */0
            ],
            /* module_list */get_module_list,
            /* sidebar_links */create_sidebar_link_state(get_module_list),
            /* page : record */[
              /* name */"home",
              /* position */""
            ]
          ],
          Tea_cmd.none
        ];
}

function parse_hash_value(hash) {
  var match = hash.split("-");
  var len = match.length;
  if (len >= 3) {
    return /* record */[
            /* name */"",
            /* position */""
          ];
  } else {
    switch (len) {
      case 0 : 
          return /* record */[
                  /* name */"",
                  /* position */""
                ];
      case 1 : 
          var name = match[0];
          return /* record */[
                  /* name */name.substr(1),
                  /* position */""
                ];
      case 2 : 
          var name$1 = match[0];
          var position = match[1];
          return /* record */[
                  /* name */name$1.substr(1),
                  /* position */position
                ];
      
    }
  }
}

function scrollToPosition(position) {
  var element = document.querySelector("#" + position);
  if (element !== null) {
    element.scrollIntoView();
    return /* () */0;
  } else {
    return 0;
  }
}

function setScrollPosition(position) {
  if (position === "") {
    window.scrollTo(0, 0);
    return /* () */0;
  } else {
    return scrollToPosition(position);
  }
}

function update(model, msg) {
  if (typeof msg === "number") {
    setScrollPosition(model[/* page */3][/* position */1]);
    return /* tuple */[
            model,
            Tea_cmd.none
          ];
  } else if (msg.tag) {
    var is_functions = msg[1];
    var sidebar_link_name = msg[0];
    var flipSelected = function (s) {
      if (s[/* name */0] === sidebar_link_name) {
        if (is_functions) {
          return /* record */[
                  /* name */s[/* name */0],
                  /* selected */s[/* selected */1],
                  /* functions */s[/* functions */2],
                  /* functions_selected */!s[/* functions_selected */3]
                ];
        } else {
          return /* record */[
                  /* name */s[/* name */0],
                  /* selected */!s[/* selected */1],
                  /* functions */s[/* functions */2],
                  /* functions_selected */s[/* functions_selected */3]
                ];
        }
      } else {
        return s;
      }
    };
    return /* tuple */[
            /* record */[
              /* history */model[/* history */0],
              /* module_list */model[/* module_list */1],
              /* sidebar_links */List.map(flipSelected, model[/* sidebar_links */2]),
              /* page */model[/* page */3]
            ],
            Tea_cmd.none
          ];
  } else {
    var $$location = msg[0];
    return /* tuple */[
            /* record */[
              /* history : :: */[
                $$location,
                model[/* history */0]
              ],
              /* module_list */model[/* module_list */1],
              /* sidebar_links */model[/* sidebar_links */2],
              /* page */parse_hash_value($$location[/* hash */7])
            ],
            Tea_task.perform((function () {
                    return /* Scroll */0;
                  }), Tea_task.succeed(/* () */0))
          ];
  }
}

function innerHTML(html) {
  return Vdom.prop("innerHTML", html);
}

function function_links(sidebar_link) {
  return List.map((function (func_name) {
                return Tea_html.li(undefined, undefined, /* [] */0, /* :: */[
                            Tea_html.a(undefined, undefined, /* :: */[
                                  Tea_html.href("#" + (sidebar_link[/* name */0] + ("-" + func_name))),
                                  /* [] */0
                                ], /* :: */[
                                  Tea_html.text(func_name),
                                  /* [] */0
                                ]),
                            /* [] */0
                          ]);
              }), sidebar_link[/* functions */2]);
}

function sidebar_functions(sidebar_link) {
  return Tea_html.li(undefined, undefined, /* [] */0, /* :: */[
              Tea_html.a(undefined, undefined, /* :: */[
                    Tea_html.onClick(/* ClickedSidebarLink */Block.__(1, [
                            sidebar_link[/* name */0],
                            true
                          ])),
                    /* [] */0
                  ], /* :: */[
                    Tea_html.text("Functions"),
                    /* [] */0
                  ]),
              /* :: */[
                Tea_html.ul(undefined, undefined, /* :: */[
                      Tea_html.classList(/* :: */[
                            /* tuple */[
                              "function-links",
                              true
                            ],
                            /* :: */[
                              /* tuple */[
                                "selected",
                                sidebar_link[/* functions_selected */3]
                              ],
                              /* [] */0
                            ]
                          ]),
                      /* [] */0
                    ], function_links(sidebar_link)),
                /* [] */0
              ]
            ]);
}

function sublist_sidebar(sidebar_link) {
  var add_sidebar_functions = function (list) {
    if (List.length(sidebar_link[/* functions */2]) > 0) {
      return /* :: */[
              sidebar_functions(sidebar_link),
              list
            ];
    } else {
      return list;
    }
  };
  return Tea_html.ul(undefined, undefined, /* :: */[
              Tea_html.classList(/* :: */[
                    /* tuple */[
                      "sublist",
                      true
                    ],
                    /* :: */[
                      /* tuple */[
                        "selected",
                        sidebar_link[/* selected */1]
                      ],
                      /* [] */0
                    ]
                  ]),
              /* [] */0
            ], /* :: */[
              Tea_html.li(undefined, undefined, /* [] */0, /* :: */[
                    Tea_html.a(undefined, undefined, /* :: */[
                          Tea_html.href("#" + sidebar_link[/* name */0]),
                          /* [] */0
                        ], /* :: */[
                          Tea_html.text("Top"),
                          /* [] */0
                        ]),
                    /* [] */0
                  ]),
              add_sidebar_functions(/* [] */0)
            ]);
}

function sidebar_link(sidebar_link$1) {
  return Tea_html.li(undefined, undefined, /* [] */0, /* :: */[
              Tea_html.a(undefined, undefined, /* :: */[
                    Tea_html.onClick(/* ClickedSidebarLink */Block.__(1, [
                            sidebar_link$1[/* name */0],
                            false
                          ])),
                    /* [] */0
                  ], /* :: */[
                    Tea_html.text(sidebar_link$1[/* name */0]),
                    /* [] */0
                  ]),
              /* :: */[
                sublist_sidebar(sidebar_link$1),
                /* [] */0
              ]
            ]);
}

function module_sidebar_links(sidebar_links) {
  return List.map(sidebar_link, sidebar_links);
}

function view_sidebar(sidebar_links) {
  return Tea_html.aside(undefined, undefined, /* :: */[
              Tea_html.class$prime("sidebar"),
              /* [] */0
            ], /* :: */[
              Tea_html.a(undefined, undefined, /* :: */[
                    Tea_html.href("#docs_home"),
                    /* [] */0
                  ], /* :: */[
                    Tea_html.img(undefined, undefined, /* :: */[
                          Tea_html.src("http://ocaml.org/img/colour-logo-white.svg"),
                          /* :: */[
                            Tea_html.id("logo"),
                            /* [] */0
                          ]
                        ], /* [] */0),
                    /* [] */0
                  ]),
              /* :: */[
                Tea_html.h5(undefined, undefined, /* :: */[
                      Tea_html.id("version"),
                      /* [] */0
                    ], /* :: */[
                      Tea_html.text("v4.07 (Unofficial)"),
                      /* [] */0
                    ]),
                /* :: */[
                  Tea_html.h6(undefined, undefined, /* [] */0, /* :: */[
                        Tea_html.a(undefined, undefined, /* :: */[
                              Tea_html.href("https://caml.inria.fr/pub/docs/manual-ocaml-4.07/"),
                              /* [] */0
                            ], /* :: */[
                              Tea_html.text("Official Docs"),
                              /* [] */0
                            ]),
                        /* [] */0
                      ]),
                  /* :: */[
                    Tea_html.h6(undefined, undefined, /* [] */0, /* :: */[
                          Tea_html.a(undefined, undefined, /* :: */[
                                Tea_html.href("https://ocaml.org/"),
                                /* [] */0
                              ], /* :: */[
                                Tea_html.text("Official Website"),
                                /* [] */0
                              ]),
                          /* [] */0
                        ]),
                    /* :: */[
                      Tea_html.h6(undefined, undefined, /* [] */0, /* :: */[
                            Tea_html.a(undefined, undefined, /* :: */[
                                  Tea_html.href("https://www.streamingspring.com"),
                                  /* [] */0
                                ], /* :: */[
                                  Tea_html.text("Back to Blog"),
                                  /* [] */0
                                ]),
                            /* [] */0
                          ]),
                      /* :: */[
                        Tea_html.h3(undefined, undefined, /* :: */[
                              Tea_html.id("modules-title"),
                              /* [] */0
                            ], /* :: */[
                              Tea_html.text("Modules"),
                              /* [] */0
                            ]),
                        /* :: */[
                          Tea_html.ul(undefined, undefined, /* :: */[
                                Tea_html.class$prime("module-links"),
                                /* [] */0
                              ], List.map(sidebar_link, sidebar_links)),
                          /* [] */0
                        ]
                      ]
                    ]
                  ]
                ]
              ]
            ]);
}

function view_element_header(name, category, content, element_html) {
  return /* :: */[
          Tea_html.h5(undefined, undefined, /* :: */[
                Tea_html.id(name),
                /* :: */[
                  Tea_html.class$prime("element"),
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

function view_element(element) {
  switch (element.tag | 0) {
    case 0 : 
        return view_element_header(element[0], "type ", " = " + element[1], view_element_info(element[2], /* [] */0));
    case 1 : 
        return view_element_header(element[0], "type ", " =", view_element_type_table(element[1], view_element_info(element[2], /* [] */0)));
    case 2 : 
        return view_element_header(element[0], "val ", " : " + element[1], view_element_info(element[2], /* [] */0));
    case 3 : 
        return view_element_header(element[0], "exception ", parse_exception(element[1]), view_element_info(element[2], /* [] */0));
    case 4 : 
        return view_element_header(element[0], "module ", ": sig .. end", view_element_info(element[1], /* [] */0));
    case 5 : 
        return view_element_header(element[0], "module type ", ": sig .. end", view_element_info(element[1], /* [] */0));
    
  }
}

function view_elements(elements) {
  return List.map((function (e) {
                return Tea_html.li(undefined, undefined, /* [] */0, view_element(e));
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

function view_main(model) {
  var match = model[/* page */3];
  var name = match[/* name */0];
  switch (name) {
    case "docs_home" : 
        return Tea_html.main(undefined, undefined, /* :: */[
                    Tea_html.id("home"),
                    /* :: */[
                      Tea_html.class$prime("content"),
                      /* [] */0
                    ]
                  ], Home_page.home_page(/* () */0));
    case "license" : 
        return Tea_html.main(undefined, undefined, /* :: */[
                    Tea_html.id("license"),
                    /* :: */[
                      Tea_html.class$prime("content"),
                      /* [] */0
                    ]
                  ], License.license(/* () */0));
    default:
      try {
        var module_item = List.find((function (m) {
                return m[/* module_name */0] === name;
              }), model[/* module_list */1]);
        return Tea_html.main(undefined, undefined, /* :: */[
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
                          Tea_html.text(module_item[/* module_name */0]),
                          /* [] */0
                        ]),
                    /* :: */[
                      Tea_html.div(undefined, undefined, /* :: */[
                            Tea_html.class$prime("info"),
                            /* :: */[
                              Vdom.prop("innerHTML", module_item[/* module_info */1]),
                              /* [] */0
                            ]
                          ], /* [] */0),
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
                  ]);
      }
      catch (exn){
        if (exn === Caml_builtin_exceptions.not_found) {
          return Tea_html.main(undefined, undefined, /* :: */[
                      Tea_html.id("not-found"),
                      /* :: */[
                        Tea_html.class$prime("content"),
                        /* [] */0
                      ]
                    ], /* :: */[
                      Tea_html.h1(undefined, undefined, /* :: */[
                            Tea_html.class$prime("title"),
                            /* [] */0
                          ], /* :: */[
                            Tea_html.text("Not Found"),
                            /* [] */0
                          ]),
                      /* [] */0
                    ]);
        } else {
          throw exn;
        }
      }
  }
}

function view(model) {
  return Tea_html.div(undefined, undefined, /* [] */0, /* :: */[
              view_sidebar(model[/* sidebar_links */2]),
              /* :: */[
                view_main(model),
                /* [] */0
              ]
            ]);
}

var main = Tea_navigation.navigationProgram(urlChange, /* record */[
      /* init */init,
      /* update */update,
      /* view */view,
      /* subscriptions */(function () {
          return Tea_sub.none;
        }),
      /* shutdown */(function () {
          return Tea_cmd.none;
        })
    ]);

var Task = 0;

var scroll = /* Scroll */0;

exports.Task = Task;
exports.urlChange = urlChange;
exports.clickedSidebarLink = clickedSidebarLink;
exports.scroll = scroll;
exports.get_module_list = get_module_list;
exports.get_function_names = get_function_names;
exports.functions_in_sub_sections = functions_in_sub_sections;
exports.functions_in_section = functions_in_section;
exports.get_function_list = get_function_list;
exports.create_sidebar_link_state = create_sidebar_link_state;
exports.init = init;
exports.parse_hash_value = parse_hash_value;
exports.scrollToPosition = scrollToPosition;
exports.setScrollPosition = setScrollPosition;
exports.update = update;
exports.innerHTML = innerHTML;
exports.function_links = function_links;
exports.sidebar_functions = sidebar_functions;
exports.sublist_sidebar = sublist_sidebar;
exports.sidebar_link = sidebar_link;
exports.module_sidebar_links = module_sidebar_links;
exports.view_sidebar = view_sidebar;
exports.view_element_header = view_element_header;
exports.view_element_info = view_element_info;
exports.view_element_type_table = view_element_type_table;
exports.parse_exception = parse_exception;
exports.view_element = view_element;
exports.view_elements = view_elements;
exports.view_section_name = view_section_name;
exports.view_section_info = view_section_info;
exports.view_section = view_section;
exports.view_sections = view_sections;
exports.view_main = view_main;
exports.view = view;
exports.main = main;
/* json Not a pure module */
