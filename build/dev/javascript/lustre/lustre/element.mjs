import * as $list from "../../gleam_stdlib/gleam/list.mjs";
import * as $string from "../../gleam_stdlib/gleam/string.mjs";
import * as $string_builder from "../../gleam_stdlib/gleam/string_builder.mjs";
import { CustomType as $CustomType, makeError } from "../gleam.mjs";
import * as $attribute from "../lustre/attribute.mjs";

class Text extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

class Element extends $CustomType {
  constructor(x0, x1, x2) {
    super();
    this[0] = x0;
    this[1] = x1;
    this[2] = x2;
  }
}

class ElementNs extends $CustomType {
  constructor(x0, x1, x2, x3) {
    super();
    this[0] = x0;
    this[1] = x1;
    this[2] = x2;
    this[3] = x3;
  }
}

export function element(tag, attrs, children) {
  return new Element(tag, attrs, children);
}

export function namespaced(namespace, tag, attrs, children) {
  return new ElementNs(tag, attrs, children, namespace);
}

export function text(content) {
  return new Text(content);
}

function escape(loop$escaped, loop$content) {
  while (true) {
    let escaped = loop$escaped;
    let content = loop$content;
    if (content.startsWith("<")) {
      let rest = content.slice(1);
      loop$escaped = escaped + "&lt;";
      loop$content = rest;
    } else if (content.startsWith(">")) {
      let rest = content.slice(1);
      loop$escaped = escaped + "&gt;";
      loop$content = rest;
    } else if (content.startsWith("&")) {
      let rest = content.slice(1);
      loop$escaped = escaped + "&amp;";
      loop$content = rest;
    } else if (content.startsWith("\"")) {
      let rest = content.slice(1);
      loop$escaped = escaped + "&quot;";
      loop$content = rest;
    } else if (content.startsWith("'")) {
      let rest = content.slice(1);
      loop$escaped = escaped + "&#39;";
      loop$content = rest;
    } else {
      let $ = $string.pop_grapheme(content);
      if ($.isOk()) {
        let x = $[0][0];
        let xs = $[0][1];
        loop$escaped = escaped + x;
        loop$content = xs;
      } else if (!$.isOk()) {
        return escaped;
      } else {
        throw makeError(
          "case_no_match",
          "lustre/element",
          54,
          "escape",
          "No case clause matched",
          { values: [$] }
        )
      }
    }
  }
}

export function map(element, f) {
  if (element instanceof Text) {
    let content = element[0];
    return new Text(content);
  } else if (element instanceof Element) {
    let tag = element[0];
    let attrs = element[1];
    let children = element[2];
    return new Element(
      tag,
      $list.map(attrs, (_capture) => { return $attribute.map(_capture, f); }),
      $list.map(children, (_capture) => { return map(_capture, f); }),
    );
  } else if (element instanceof ElementNs) {
    let tag = element[0];
    let attrs = element[1];
    let children = element[2];
    let namespace = element[3];
    return new ElementNs(
      tag,
      $list.map(attrs, (_capture) => { return $attribute.map(_capture, f); }),
      $list.map(children, (_capture) => { return map(_capture, f); }),
      namespace,
    );
  } else {
    throw makeError(
      "case_no_match",
      "lustre/element",
      65,
      "map",
      "No case clause matched",
      { values: [element] }
    )
  }
}

function attrs_to_string_builder(html, attrs) {
  let $ = $list.fold(
    attrs,
    [html, "", ""],
    (_use0, attr) => {
      let html$1 = _use0[0];
      let class$ = _use0[1];
      let style = _use0[2];
      let $1 = $attribute.to_string_parts(attr);
      if ($1.isOk() && $1[0][0] === "class" && class$ === "") {
        let val = $1[0][1];
        return [html$1, val, style];
      } else if ($1.isOk() && $1[0][0] === "class") {
        let val = $1[0][1];
        return [html$1, (class$ + " ") + val, style];
      } else if ($1.isOk() && $1[0][0] === "style" && style === "") {
        let val = $1[0][1];
        return [html$1, class$, val];
      } else if ($1.isOk() && $1[0][0] === "style") {
        let val = $1[0][1];
        return [html$1, class$, (style + " ") + val];
      } else if ($1.isOk()) {
        let key = $1[0][0];
        let val = $1[0][1];
        return [
          $string_builder.append(html$1, (((" " + key) + "=\"") + val) + "\""),
          class$,
          style,
        ];
      } else if (!$1.isOk()) {
        return [html$1, class$, style];
      } else {
        throw makeError(
          "case_no_match",
          "lustre/element",
          154,
          "",
          "No case clause matched",
          { values: [$1] }
        )
      }
    },
  );
  let html$1 = $[0];
  let class$ = $[1];
  let style = $[2];
  if (class$ === "" && style === "") {
    return html$1;
  } else if (style === "") {
    return $string_builder.append(html$1, (" class=\"" + class$) + "\"");
  } else if (class$ === "") {
    return $string_builder.append(html$1, (" style=\"" + style) + "\"");
  } else {
    return $string_builder.append(
      html$1,
      (((" class=\"" + class$) + "\" style=\"") + style) + "\"",
    );
  }
}

function children_to_string_builder(html, children, raw_text) {
  return $list.fold(
    children,
    html,
    (html, child) => {
      return $string_builder.append_builder(
        html,
        to_string_builder_helper(child, raw_text),
      );
    },
  );
}

export function to_string_builder_helper(element, raw_text) {
  if (element instanceof Text && raw_text) {
    let content = element[0];
    return $string_builder.from_string(content);
  } else if (element instanceof Text) {
    let content = element[0];
    return $string_builder.from_string(escape("", content));
  } else if (element instanceof Element && element[0] === "area") {
    let tag = element[0];
    let attrs = element[1];
    let _pipe = $string_builder.from_string("<" + tag);
    let _pipe$1 = attrs_to_string_builder(_pipe, attrs);
    return $string_builder.append(_pipe$1, ">");
  } else if (element instanceof Element && element[0] === "base") {
    let tag = element[0];
    let attrs = element[1];
    let _pipe = $string_builder.from_string("<" + tag);
    let _pipe$1 = attrs_to_string_builder(_pipe, attrs);
    return $string_builder.append(_pipe$1, ">");
  } else if (element instanceof Element && element[0] === "br") {
    let tag = element[0];
    let attrs = element[1];
    let _pipe = $string_builder.from_string("<" + tag);
    let _pipe$1 = attrs_to_string_builder(_pipe, attrs);
    return $string_builder.append(_pipe$1, ">");
  } else if (element instanceof Element && element[0] === "col") {
    let tag = element[0];
    let attrs = element[1];
    let _pipe = $string_builder.from_string("<" + tag);
    let _pipe$1 = attrs_to_string_builder(_pipe, attrs);
    return $string_builder.append(_pipe$1, ">");
  } else if (element instanceof Element && element[0] === "embed") {
    let tag = element[0];
    let attrs = element[1];
    let _pipe = $string_builder.from_string("<" + tag);
    let _pipe$1 = attrs_to_string_builder(_pipe, attrs);
    return $string_builder.append(_pipe$1, ">");
  } else if (element instanceof Element && element[0] === "hr") {
    let tag = element[0];
    let attrs = element[1];
    let _pipe = $string_builder.from_string("<" + tag);
    let _pipe$1 = attrs_to_string_builder(_pipe, attrs);
    return $string_builder.append(_pipe$1, ">");
  } else if (element instanceof Element && element[0] === "img") {
    let tag = element[0];
    let attrs = element[1];
    let _pipe = $string_builder.from_string("<" + tag);
    let _pipe$1 = attrs_to_string_builder(_pipe, attrs);
    return $string_builder.append(_pipe$1, ">");
  } else if (element instanceof Element && element[0] === "input") {
    let tag = element[0];
    let attrs = element[1];
    let _pipe = $string_builder.from_string("<" + tag);
    let _pipe$1 = attrs_to_string_builder(_pipe, attrs);
    return $string_builder.append(_pipe$1, ">");
  } else if (element instanceof Element && element[0] === "link") {
    let tag = element[0];
    let attrs = element[1];
    let _pipe = $string_builder.from_string("<" + tag);
    let _pipe$1 = attrs_to_string_builder(_pipe, attrs);
    return $string_builder.append(_pipe$1, ">");
  } else if (element instanceof Element && element[0] === "meta") {
    let tag = element[0];
    let attrs = element[1];
    let _pipe = $string_builder.from_string("<" + tag);
    let _pipe$1 = attrs_to_string_builder(_pipe, attrs);
    return $string_builder.append(_pipe$1, ">");
  } else if (element instanceof Element && element[0] === "param") {
    let tag = element[0];
    let attrs = element[1];
    let _pipe = $string_builder.from_string("<" + tag);
    let _pipe$1 = attrs_to_string_builder(_pipe, attrs);
    return $string_builder.append(_pipe$1, ">");
  } else if (element instanceof Element && element[0] === "source") {
    let tag = element[0];
    let attrs = element[1];
    let _pipe = $string_builder.from_string("<" + tag);
    let _pipe$1 = attrs_to_string_builder(_pipe, attrs);
    return $string_builder.append(_pipe$1, ">");
  } else if (element instanceof Element && element[0] === "track") {
    let tag = element[0];
    let attrs = element[1];
    let _pipe = $string_builder.from_string("<" + tag);
    let _pipe$1 = attrs_to_string_builder(_pipe, attrs);
    return $string_builder.append(_pipe$1, ">");
  } else if (element instanceof Element && element[0] === "wbr") {
    let tag = element[0];
    let attrs = element[1];
    let _pipe = $string_builder.from_string("<" + tag);
    let _pipe$1 = attrs_to_string_builder(_pipe, attrs);
    return $string_builder.append(_pipe$1, ">");
  } else if (element instanceof Element && element[0] === "style") {
    let tag = element[0];
    let attrs = element[1];
    let children = element[2];
    let _pipe = $string_builder.from_string("<" + tag);
    let _pipe$1 = attrs_to_string_builder(_pipe, attrs);
    let _pipe$2 = $string_builder.append(_pipe$1, ">");
    let _pipe$3 = children_to_string_builder(_pipe$2, children, true);
    return $string_builder.append(_pipe$3, ("</" + tag) + ">");
  } else if (element instanceof Element && element[0] === "script") {
    let tag = element[0];
    let attrs = element[1];
    let children = element[2];
    let _pipe = $string_builder.from_string("<" + tag);
    let _pipe$1 = attrs_to_string_builder(_pipe, attrs);
    let _pipe$2 = $string_builder.append(_pipe$1, ">");
    let _pipe$3 = children_to_string_builder(_pipe$2, children, true);
    return $string_builder.append(_pipe$3, ("</" + tag) + ">");
  } else if (element instanceof Element) {
    let tag = element[0];
    let attrs = element[1];
    let children = element[2];
    let _pipe = $string_builder.from_string("<" + tag);
    let _pipe$1 = attrs_to_string_builder(_pipe, attrs);
    let _pipe$2 = $string_builder.append(_pipe$1, ">");
    let _pipe$3 = children_to_string_builder(_pipe$2, children, raw_text);
    return $string_builder.append(_pipe$3, ("</" + tag) + ">");
  } else if (element instanceof ElementNs) {
    let tag = element[0];
    let attrs = element[1];
    let children = element[2];
    let namespace = element[3];
    let _pipe = $string_builder.from_string("<" + tag);
    let _pipe$1 = attrs_to_string_builder(_pipe, attrs);
    let _pipe$2 = $string_builder.append(
      _pipe$1,
      (" xmlns=\"" + namespace) + "\"",
    );
    let _pipe$3 = $string_builder.append(_pipe$2, ">");
    let _pipe$4 = children_to_string_builder(_pipe$3, children, raw_text);
    return $string_builder.append(_pipe$4, ("</" + tag) + ">");
  } else {
    throw makeError(
      "case_no_match",
      "lustre/element",
      100,
      "to_string_builder_helper",
      "No case clause matched",
      { values: [element] }
    )
  }
}

export function to_string(element) {
  let _pipe = to_string_builder_helper(element, false);
  return $string_builder.to_string(_pipe);
}

export function to_string_builder(element) {
  return to_string_builder_helper(element, false);
}
