import * as $int from "../../gleam_stdlib/gleam/int.mjs";
import * as $list from "../../gleam_stdlib/gleam/list.mjs";
import * as $map from "../../gleam_stdlib/gleam/map.mjs";
import * as $order from "../../gleam_stdlib/gleam/order.mjs";
import * as $pair from "../../gleam_stdlib/gleam/pair.mjs";
import * as $result from "../../gleam_stdlib/gleam/result.mjs";
import { Ok, Error, CustomType as $CustomType, makeError } from "../gleam.mjs";
import * as $list_extra from "../internals/list_extra.mjs";
import * as $order_extra from "../internals/order_extra.mjs";

class Grid extends $CustomType {
  constructor(values, rows, cols) {
    super();
    this.values = values;
    this.rows = rows;
    this.cols = cols;
  }
}

function all_pairs(rows, cols) {
  return $list.flat_map(
    $list.range(0, rows - 1),
    (row) => {
      return $list.map(
        $list.range(0, cols - 1),
        (col) => { return [row, col]; },
      );
    },
  );
}

export function new$(rows, cols, generator) {
  let _pipe = all_pairs(rows, cols);
  let _pipe$1 = $list.map(
    _pipe,
    (pair) => { return [pair, generator(pair[0], pair[1])]; },
  );
  let _pipe$2 = $map.from_list(_pipe$1);
  return new Grid(_pipe$2, rows, cols);
}

export function set(grid, row, col, value) {
  let $ = $map.has_key(grid.values, [row, col]);
  if ($) {
    return new Ok(
      grid.withFields({ values: $map.insert(grid.values, [row, col], value) }),
    );
  } else if (!$) {
    return new Error(undefined);
  } else {
    throw makeError(
      "case_no_match",
      "internals/grid",
      37,
      "set",
      "No case clause matched",
      { values: [$] }
    )
  }
}

export function get(grid, row, col) {
  return $map.get(grid.values, [row, col]);
}

export function update(grid, row, col, mapper) {
  return $result.try$(
    get(grid, row, col),
    (elem) => { return set(grid, row, col, mapper(elem)); },
  );
}

function compare_row_and_col(one, other) {
  let one_row = one[0];
  let one_col = one[1];
  let other_row = other[0];
  let other_col = other[1];
  return $order_extra.break_tie(
    $int.compare(one_row, other_row),
    () => { return $int.compare(one_col, other_col); },
  );
}

export function rows(grid) {
  let _pipe = $map.to_list(grid.values);
  let _pipe$1 = $list_extra.sort_on(_pipe, $pair.first, compare_row_and_col);
  let _pipe$2 = $list.map(_pipe$1, $pair.second);
  return $list.sized_chunk(_pipe$2, grid.cols);
}
