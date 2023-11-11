import { Error, toList, makeError, isEqual } from "../gleam.mjs";
import * as $option from "../gleam/option.mjs";
import {
  map_size as do_size,
  map_to_list as do_to_list,
  new_map as do_new,
  map_get as do_get,
  map_insert as do_insert,
  map_remove as do_delete,
} from "../gleam_stdlib.mjs";

export function size(map) {
  return do_size(map);
}

export function to_list(map) {
  return do_to_list(map);
}

export function new$() {
  return do_new();
}

export function get(from, get) {
  return do_get(from, get);
}

function do_has_key(key, map) {
  return !isEqual(get(map, key), new Error(undefined));
}

export function has_key(map, key) {
  return do_has_key(key, map);
}

export function insert(map, key, value) {
  return do_insert(key, value, map);
}

function fold_list_of_pair(loop$list, loop$initial) {
  while (true) {
    let list = loop$list;
    let initial = loop$initial;
    if (list.hasLength(0)) {
      return initial;
    } else if (list.atLeastLength(1)) {
      let x = list.head;
      let rest = list.tail;
      loop$list = rest;
      loop$initial = insert(initial, x[0], x[1]);
    } else {
      throw makeError(
        "case_no_match",
        "gleam/map",
        85,
        "fold_list_of_pair",
        "No case clause matched",
        { values: [list] }
      )
    }
  }
}

function do_from_list(list) {
  return fold_list_of_pair(list, new$());
}

export function from_list(list) {
  return do_from_list(list);
}

function reverse_and_concat(loop$remaining, loop$accumulator) {
  while (true) {
    let remaining = loop$remaining;
    let accumulator = loop$accumulator;
    if (remaining.hasLength(0)) {
      return accumulator;
    } else if (remaining.atLeastLength(1)) {
      let item = remaining.head;
      let rest = remaining.tail;
      loop$remaining = rest;
      loop$accumulator = toList([item], accumulator);
    } else {
      throw makeError(
        "case_no_match",
        "gleam/map",
        233,
        "reverse_and_concat",
        "No case clause matched",
        { values: [remaining] }
      )
    }
  }
}

function do_keys_acc(loop$list, loop$acc) {
  while (true) {
    let list = loop$list;
    let acc = loop$acc;
    if (list.hasLength(0)) {
      return reverse_and_concat(acc, toList([]));
    } else if (list.atLeastLength(1)) {
      let x = list.head;
      let xs = list.tail;
      loop$list = xs;
      loop$acc = toList([x[0]], acc);
    } else {
      throw makeError(
        "case_no_match",
        "gleam/map",
        241,
        "do_keys_acc",
        "No case clause matched",
        { values: [list] }
      )
    }
  }
}

function do_keys(map) {
  let list_of_pairs = (() => {
    let _pipe = map;
    return to_list(_pipe);
  })();
  return do_keys_acc(list_of_pairs, toList([]));
}

export function keys(map) {
  return do_keys(map);
}

function do_values_acc(loop$list, loop$acc) {
  while (true) {
    let list = loop$list;
    let acc = loop$acc;
    if (list.hasLength(0)) {
      return reverse_and_concat(acc, toList([]));
    } else if (list.atLeastLength(1)) {
      let x = list.head;
      let xs = list.tail;
      loop$list = xs;
      loop$acc = toList([x[1]], acc);
    } else {
      throw makeError(
        "case_no_match",
        "gleam/map",
        278,
        "do_values_acc",
        "No case clause matched",
        { values: [list] }
      )
    }
  }
}

function do_values(map) {
  let list_of_pairs = (() => {
    let _pipe = map;
    return to_list(_pipe);
  })();
  return do_values_acc(list_of_pairs, toList([]));
}

export function values(map) {
  return do_values(map);
}

function insert_taken(loop$map, loop$desired_keys, loop$acc) {
  while (true) {
    let map = loop$map;
    let desired_keys = loop$desired_keys;
    let acc = loop$acc;
    let insert$1 = (taken, key) => {
      let $ = get(map, key);
      if ($.isOk()) {
        let value = $[0];
        return insert(taken, key, value);
      } else {
        return taken;
      }
    };
    if (desired_keys.hasLength(0)) {
      return acc;
    } else if (desired_keys.atLeastLength(1)) {
      let x = desired_keys.head;
      let xs = desired_keys.tail;
      loop$map = map;
      loop$desired_keys = xs;
      loop$acc = insert$1(acc, x);
    } else {
      throw makeError(
        "case_no_match",
        "gleam/map",
        369,
        "insert_taken",
        "No case clause matched",
        { values: [desired_keys] }
      )
    }
  }
}

function do_take(desired_keys, map) {
  return insert_taken(map, desired_keys, new$());
}

export function take(map, desired_keys) {
  return do_take(desired_keys, map);
}

function insert_pair(map, pair) {
  return insert(map, pair[0], pair[1]);
}

function fold_inserts(loop$new_entries, loop$map) {
  while (true) {
    let new_entries = loop$new_entries;
    let map = loop$map;
    if (new_entries.hasLength(0)) {
      return map;
    } else if (new_entries.atLeastLength(1)) {
      let x = new_entries.head;
      let xs = new_entries.tail;
      loop$new_entries = xs;
      loop$map = insert_pair(map, x);
    } else {
      throw makeError(
        "case_no_match",
        "gleam/map",
        409,
        "fold_inserts",
        "No case clause matched",
        { values: [new_entries] }
      )
    }
  }
}

function do_merge(map, new_entries) {
  let _pipe = new_entries;
  let _pipe$1 = to_list(_pipe);
  return fold_inserts(_pipe$1, map);
}

export function merge(map, new_entries) {
  return do_merge(map, new_entries);
}

export function delete$(map, key) {
  return do_delete(key, map);
}

export function drop(loop$map, loop$disallowed_keys) {
  while (true) {
    let map = loop$map;
    let disallowed_keys = loop$disallowed_keys;
    if (disallowed_keys.hasLength(0)) {
      return map;
    } else if (disallowed_keys.atLeastLength(1)) {
      let x = disallowed_keys.head;
      let xs = disallowed_keys.tail;
      loop$map = delete$(map, x);
      loop$disallowed_keys = xs;
    } else {
      throw makeError(
        "case_no_match",
        "gleam/map",
        466,
        "drop",
        "No case clause matched",
        { values: [disallowed_keys] }
      )
    }
  }
}

export function update(map, key, fun) {
  let _pipe = map;
  let _pipe$1 = get(_pipe, key);
  let _pipe$2 = $option.from_result(_pipe$1);
  let _pipe$3 = fun(_pipe$2);
  return ((_capture) => { return insert(map, key, _capture); })(_pipe$3);
}

function do_fold(loop$list, loop$initial, loop$fun) {
  while (true) {
    let list = loop$list;
    let initial = loop$initial;
    let fun = loop$fun;
    if (list.hasLength(0)) {
      return initial;
    } else if (list.atLeastLength(1)) {
      let k = list.head[0];
      let v = list.head[1];
      let rest = list.tail;
      loop$list = rest;
      loop$initial = fun(initial, k, v);
      loop$fun = fun;
    } else {
      throw makeError(
        "case_no_match",
        "gleam/map",
        510,
        "do_fold",
        "No case clause matched",
        { values: [list] }
      )
    }
  }
}

export function fold(map, initial, fun) {
  let _pipe = map;
  let _pipe$1 = to_list(_pipe);
  return do_fold(_pipe$1, initial, fun);
}

function do_map_values(f, map) {
  let f$1 = (map, k, v) => { return insert(map, k, f(k, v)); };
  let _pipe = map;
  return fold(_pipe, new$(), f$1);
}

export function map_values(map, fun) {
  return do_map_values(fun, map);
}

function do_filter(f, map) {
  let insert$1 = (map, k, v) => {
    let $ = f(k, v);
    if ($) {
      return insert(map, k, v);
    } else {
      return map;
    }
  };
  let _pipe = map;
  return fold(_pipe, new$(), insert$1);
}

export function filter(map, predicate) {
  return do_filter(predicate, map);
}
