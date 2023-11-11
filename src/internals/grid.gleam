import gleam/map.{type Map}
import gleam/list
import gleam/result
import gleam/int
import gleam/pair
import gleam/order.{type Order}
import internals/order_extra
import internals/list_extra

pub opaque type Grid(a) {
  Grid(values: Map(#(Int, Int), a), rows: Int, cols: Int)
}

pub fn new(
  rows rows: Int,
  cols cols: Int,
  with generator: fn(Int, Int) -> a,
) -> Grid(a) {
  all_pairs(rows, cols)
  |> list.map(fn(pair) { #(pair, generator(pair.0, pair.1)) })
  |> map.from_list
  |> Grid(rows, cols)
}

fn all_pairs(rows: Int, cols: Int) -> List(#(Int, Int)) {
  use row <- list.flat_map(list.range(0, rows - 1))
  use col <- list.map(list.range(0, cols - 1))
  #(row, col)
}

pub fn set(
  grid: Grid(a),
  row row: Int,
  col col: Int,
  value value: a,
) -> Result(Grid(a), Nil) {
  case map.has_key(grid.values, #(row, col)) {
    True ->
      Ok(Grid(..grid, values: map.insert(grid.values, #(row, col), value)))
    False -> Error(Nil)
  }
}

pub fn update(
  grid: Grid(a),
  row row: Int,
  col col: Int,
  with mapper: fn(a) -> a,
) -> Result(Grid(a), Nil) {
  use elem <- result.try(get(grid, row, col))
  set(grid, row, col, mapper(elem))
}

pub fn get(grid: Grid(a), row row: Int, col col: Int) -> Result(a, Nil) {
  map.get(grid.values, #(row, col))
}

pub fn rows(grid: Grid(a)) -> List(List(a)) {
  map.to_list(grid.values)
  |> list_extra.sort_on(on: pair.first, with: compare_row_and_col)
  |> list.map(pair.second)
  |> list.sized_chunk(into: grid.cols)
}

fn compare_row_and_col(one: #(Int, Int), other: #(Int, Int)) -> Order {
  let #(one_row, one_col) = one
  let #(other_row, other_col) = other
  use <- order_extra.break_tie(int.compare(one_row, other_row))
  int.compare(one_col, other_col)
}
