import * as $dynamic from "../../gleam_stdlib/gleam/dynamic.mjs";
import * as $function from "../../gleam_stdlib/gleam/function.mjs";
import * as $int from "../../gleam_stdlib/gleam/int.mjs";
import * as $list from "../../gleam_stdlib/gleam/list.mjs";
import * as $result from "../../gleam_stdlib/gleam/result.mjs";
import * as $string from "../../gleam_stdlib/gleam/string.mjs";
import * as $string_builder from "../../gleam_stdlib/gleam/string_builder.mjs";
import { Ok, Error, toList, CustomType as $CustomType, makeError } from "../gleam.mjs";

class Attribute extends $CustomType {
  constructor(x0, x1, as_property) {
    super();
    this[0] = x0;
    this[1] = x1;
    this.as_property = as_property;
  }
}

class Event extends $CustomType {
  constructor(x0, x1) {
    super();
    this[0] = x0;
    this[1] = x1;
  }
}

export function attribute(name, value) {
  return new Attribute(name, $dynamic.from(value), false);
}

export function property(name, value) {
  return new Attribute(name, $dynamic.from(value), true);
}

export function on(name, handler) {
  return new Event(
    "on" + name,
    $function.compose(
      handler,
      (_capture) => { return $result.replace_error(_capture, undefined); },
    ),
  );
}

export function map(attr, f) {
  if (attr instanceof Attribute) {
    let name$1 = attr[0];
    let value$1 = attr[1];
    let as_property = attr.as_property;
    return new Attribute(name$1, value$1, as_property);
  } else if (attr instanceof Event) {
    let on$1 = attr[0];
    let handler = attr[1];
    return new Event(on$1, (e) => { return $result.map(handler(e), f); });
  } else {
    throw makeError(
      "case_no_match",
      "lustre/attribute",
      46,
      "map",
      "No case clause matched",
      { values: [attr] }
    )
  }
}

export function to_string_parts(attr) {
  if (attr instanceof Attribute) {
    let name$1 = attr[0];
    let value$1 = attr[1];
    let as_property = attr.as_property;
    let $ = $dynamic.classify(value$1);
    if ($ === "String") {
      return new Ok([name$1, $dynamic.unsafe_coerce(value$1)]);
    } else if ($ === "Boolean") {
      let $1 = $dynamic.unsafe_coerce(value$1);
      if ($1) {
        return new Ok([name$1, name$1]);
      } else if (!$1) {
        return new Error(undefined);
      } else {
        throw makeError(
          "case_no_match",
          "lustre/attribute",
          75,
          "to_string_parts",
          "No case clause matched",
          { values: [$1] }
        )
      }
    } else if (as_property) {
      return new Error(undefined);
    } else {
      return new Ok([name$1, $string.inspect(value$1)]);
    }
  } else if (attr instanceof Event) {
    let on$1 = attr[0];
    return new Ok(["data-lustre-on", on$1]);
  } else {
    throw makeError(
      "case_no_match",
      "lustre/attribute",
      66,
      "to_string_parts",
      "No case clause matched",
      { values: [attr] }
    )
  }
}

export function to_string(attr) {
  let $ = to_string_parts(attr);
  if ($.isOk()) {
    let key = $[0][0];
    let val = $[0][1];
    return ((key + "=\"") + val) + "\"";
  } else if (!$.isOk()) {
    return "";
  } else {
    throw makeError(
      "case_no_match",
      "lustre/attribute",
      57,
      "to_string",
      "No case clause matched",
      { values: [$] }
    )
  }
}

export function to_string_builder(attr) {
  if (attr instanceof Attribute) {
    let name$1 = attr[0];
    let value$1 = attr[1];
    let as_property = attr.as_property;
    let $ = $dynamic.classify(value$1);
    if ($ === "String") {
      let _pipe = toList([name$1, "=\"", $dynamic.unsafe_coerce(value$1), "\""]);
      return $string_builder.from_strings(_pipe);
    } else if ($ === "Boolean") {
      let $1 = $dynamic.unsafe_coerce(value$1);
      if ($1) {
        return $string_builder.from_string(name$1);
      } else if (!$1) {
        return $string_builder.new$();
      } else {
        throw makeError(
          "case_no_match",
          "lustre/attribute",
          104,
          "to_string_builder",
          "No case clause matched",
          { values: [$1] }
        )
      }
    } else if (as_property) {
      return $string_builder.new$();
    } else {
      let _pipe = toList([name$1, "=\"", $string.inspect(value$1), "\""]);
      return $string_builder.from_strings(_pipe);
    }
  } else if (attr instanceof Event) {
    let on$1 = attr[0];
    let _pipe = toList(["data-lustre-on:", on$1]);
    return $string_builder.from_strings(_pipe);
  } else {
    throw makeError(
      "case_no_match",
      "lustre/attribute",
      93,
      "to_string_builder",
      "No case clause matched",
      { values: [attr] }
    )
  }
}

export function style(properties) {
  return attribute(
    "style",
    $list.fold(
      properties,
      "",
      (styles, _use1) => {
        let name$1 = _use1[0];
        let value$1 = _use1[1];
        return (((styles + name$1) + ":") + value$1) + ";";
      },
    ),
  );
}

export function class$(name) {
  return attribute("class", name);
}

export function classes(names) {
  return attribute(
    "class",
    (() => {
      let _pipe = names;
      let _pipe$1 = $list.filter_map(
        _pipe,
        (class$) => {
          let $ = class$[1];
          if ($) {
            return new Ok(class$[0]);
          } else if (!$) {
            return new Error(undefined);
          } else {
            throw makeError(
              "case_no_match",
              "lustre/attribute",
              145,
              "",
              "No case clause matched",
              { values: [$] }
            )
          }
        },
      );
      return $string.join(_pipe$1, " ");
    })(),
  );
}

export function id(name) {
  return attribute("id", name);
}

export function type_(name) {
  return attribute("type", name);
}

export function value(val) {
  return property("value", val);
}

export function checked(is_checked) {
  return property("checked", is_checked);
}

export function placeholder(text) {
  return attribute("placeholder", text);
}

export function selected(is_selected) {
  return property("selected", is_selected);
}

export function accept(types) {
  return attribute("accept", $string.join(types, " "));
}

export function accept_charset(types) {
  return attribute("acceptCharset", $string.join(types, " "));
}

export function msg(uri) {
  return attribute("msg", uri);
}

export function autocomplete(name) {
  return attribute("autocomplete", name);
}

export function autofocus(should_autofocus) {
  return property("autoFocus", should_autofocus);
}

export function disabled(is_disabled) {
  return property("disabled", is_disabled);
}

export function name(name) {
  return attribute("name", name);
}

export function pattern(regex) {
  return attribute("pattern", regex);
}

export function readonly(is_readonly) {
  return property("readonly", is_readonly);
}

export function required(is_required) {
  return property("required", is_required);
}

export function for$(id) {
  return attribute("for", id);
}

export function max(val) {
  return attribute("max", val);
}

export function min(val) {
  return attribute("min", val);
}

export function step(val) {
  return attribute("step", val);
}

export function cols(val) {
  return attribute("cols", $int.to_string(val));
}

export function rows(val) {
  return attribute("rows", $int.to_string(val));
}

export function wrap(mode) {
  return attribute("wrap", mode);
}

export function href(uri) {
  return attribute("href", uri);
}

export function target(target) {
  return attribute("target", target);
}

export function download(filename) {
  return attribute("download", filename);
}

export function rel(relationship) {
  return attribute("rel", relationship);
}

export function src(uri) {
  return attribute("src", uri);
}

export function height(val) {
  return property("height", $int.to_string(val));
}

export function width(val) {
  return property("width", $int.to_string(val));
}

export function alt(text) {
  return attribute("alt", text);
}

export function autoplay(should_autoplay) {
  return property("autoplay", should_autoplay);
}

export function controls(visible) {
  return property("controls", visible);
}

export function loop(should_loop) {
  return property("loop", should_loop);
}
