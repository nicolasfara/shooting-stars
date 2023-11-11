import { CustomType as $CustomType, makeError } from "../gleam.mjs";
import * as $list from "../gleam/list.mjs";
import * as $map from "../gleam/map.mjs";
import * as $result from "../gleam/result.mjs";

class Set extends $CustomType {
  constructor(map) {
    super();
    this.map = map;
  }
}

const token = undefined;

export function new$() {
  return new Set($map.new$());
}

export function size(set) {
  return $map.size(set.map);
}

export function insert(set, member) {
  return new Set($map.insert(set.map, member, token));
}

export function contains(set, member) {
  let _pipe = set.map;
  let _pipe$1 = $map.get(_pipe, member);
  return $result.is_ok(_pipe$1);
}

export function delete$(set, member) {
  return new Set($map.delete$(set.map, member));
}

export function to_list(set) {
  return $map.keys(set.map);
}

export function from_list(members) {
  let map = $list.fold(
    members,
    $map.new$(),
    (m, k) => { return $map.insert(m, k, token); },
  );
  return new Set(map);
}

export function fold(set, initial, reducer) {
  return $map.fold(set.map, initial, (a, k, _) => { return reducer(a, k); });
}

export function filter(set, predicate) {
  return new Set($map.filter(set.map, (m, _) => { return predicate(m); }));
}

export function drop(set, disallowed) {
  return $list.fold(disallowed, set, delete$);
}

export function take(set, desired) {
  return new Set($map.take(set.map, desired));
}

function order(first, second) {
  let $ = $map.size(first.map) > $map.size(second.map);
  if ($) {
    return [first, second];
  } else if (!$) {
    return [second, first];
  } else {
    throw makeError(
      "case_no_match",
      "gleam/set",
      225,
      "order",
      "No case clause matched",
      { values: [$] }
    )
  }
}

export function union(first, second) {
  let $ = order(first, second);
  let larger = $[0];
  let smaller = $[1];
  return fold(smaller, larger, insert);
}

export function intersection(first, second) {
  let $ = order(first, second);
  let larger = $[0];
  let smaller = $[1];
  return take(larger, to_list(smaller));
}
