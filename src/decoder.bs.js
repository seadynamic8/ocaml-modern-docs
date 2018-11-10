// Generated by BUCKLESCRIPT VERSION 4.0.5, PLEASE EDIT WITH CARE
'use strict';

var Json = require("@glennsl/bs-json/src/Json.bs.js");
var Block = require("bs-platform/lib/js/block.js");
var Curry = require("bs-platform/lib/js/curry.js");
var Js_exn = require("bs-platform/lib/js/js_exn.js");
var Pervasives = require("bs-platform/lib/js/pervasives.js");
var Json_decode = require("@glennsl/bs-json/src/Json_decode.bs.js");

function variant(parameter_decoder, json) {
  if (Array.isArray(json)) {
    try {
      var first = json[0];
      var decoded_type = Json_decode.string(first);
      return Curry._2(parameter_decoder, json[1], decoded_type);
    }
    catch (raw_exn){
      var exn = Js_exn.internalToOCamlException(raw_exn);
      if (exn[0] === Json_decode.DecodeError) {
        throw [
              Json_decode.DecodeError,
              exn[1] + "\n\tin variant"
            ];
      } else {
        throw exn;
      }
    }
  } else {
    throw [
          Json_decode.DecodeError,
          "Variant: Expected array, got " + Json.stringify(json)
        ];
  }
}

function info(param) {
  return Json_decode.optional(Json_decode.string, param);
}

function type_extra(param) {
  return Json_decode.optional(Json_decode.string, param);
}

function type_table(param) {
  return Json_decode.optional(Json_decode.string, param);
}

function exec_parameter(param) {
  return Json_decode.optional(Json_decode.string, param);
}

function element_type_map(parameters, type_name) {
  switch (type_name) {
    case "Exception" : 
        var match = Json_decode.tuple3(Json_decode.string, exec_parameter, info, parameters);
        return /* Exception */Block.__(3, [
                  match[0],
                  match[1],
                  match[2]
                ]);
    case "Function" : 
        var match$1 = Json_decode.tuple3(Json_decode.string, Json_decode.string, info, parameters);
        return /* Function */Block.__(2, [
                  match$1[0],
                  match$1[1],
                  match$1[2]
                ]);
    case "Include" : 
        var x = Json_decode.string(parameters);
        return /* Include */Block.__(6, [x]);
    case "Module" : 
        var match$2 = Json_decode.tuple2(Json_decode.string, info, parameters);
        return /* Module */Block.__(4, [
                  match$2[0],
                  match$2[1]
                ]);
    case "Moduletype" : 
        var match$3 = Json_decode.tuple2(Json_decode.string, info, parameters);
        return /* Moduletype */Block.__(5, [
                  match$3[0],
                  match$3[1]
                ]);
    case "Typepoly" : 
        var match$4 = Json_decode.tuple3(Json_decode.string, Json_decode.string, info, parameters);
        return /* Typepoly */Block.__(0, [
                  match$4[0],
                  match$4[1],
                  match$4[2]
                ]);
    case "Typevariant" : 
        var match$5 = Json_decode.tuple4(Json_decode.string, type_extra, type_table, info, parameters);
        return /* Typevariant */Block.__(1, [
                  match$5[0],
                  match$5[1],
                  match$5[2],
                  match$5[3]
                ]);
    default:
      return Pervasives.failwith("Unknown element type");
  }
}

function element_type(param) {
  return variant(element_type_map, param);
}

function section(json) {
  return /* record */[
          /* section_name */Json_decode.optional((function (param) {
                  return Json_decode.field("section_name", Json_decode.string, param);
                }), json),
          /* section_info */Json_decode.optional((function (param) {
                  return Json_decode.field("section_info", Json_decode.string, param);
                }), json),
          /* elements */Json_decode.field("elements", (function (param) {
                  return Json_decode.list(element_type, param);
                }), json),
          /* sub_sections */Json_decode.field("sub_sections", (function (param) {
                  return Json_decode.list(section, param);
                }), json)
        ];
}

function functor_parts(json) {
  return /* record */[
          /* begin_sig */Json_decode.field("begin_sig", Json_decode.string, json),
          /* functor_elements */Json_decode.field("functor_elements", (function (param) {
                  return Json_decode.list(element_type, param);
                }), json),
          /* end_sig */Json_decode.field("end_sig", Json_decode.string, json),
          /* table */Json_decode.field("table", Json_decode.string, json)
        ];
}

function module_parts(json) {
  return /* record */[
          /* module_name */Json_decode.field("module_name", Json_decode.string, json),
          /* module_info */Json_decode.field("module_info", Json_decode.string, json),
          /* sections */Json_decode.field("sections", (function (param) {
                  return Json_decode.list(section, param);
                }), json),
          /* is_standard */Json_decode.field("is_standard", Json_decode.bool, json),
          /* is_module_type */Json_decode.field("is_module_type", Json_decode.bool, json),
          /* functor_info */Json_decode.optional((function (param) {
                  return Json_decode.field("functor_info", functor_parts, param);
                }), json)
        ];
}

function decode_modules(param) {
  return Json_decode.list(module_parts, param);
}

var Decode = /* module */[
  /* variant */variant,
  /* name */Json_decode.string,
  /* info */info,
  /* type_type */Json_decode.string,
  /* type_extra */type_extra,
  /* type_table */type_table,
  /* func_annotation */Json_decode.string,
  /* exec_parameter */exec_parameter,
  /* element_type_map */element_type_map,
  /* element_type */element_type,
  /* section */section,
  /* functor_parts */functor_parts,
  /* module_parts */module_parts,
  /* decode_modules */decode_modules
];

exports.Decode = Decode;
/* No side effect */
