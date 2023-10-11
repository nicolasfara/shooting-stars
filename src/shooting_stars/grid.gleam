import gleam/map.{Map}
import gleam/list
import gleam/result
import gleam/int
import gleam/pair

pub opaque type Grid(a) {
  Grid(values: Map(#(Int, Int), a))
}

pub fn new(
  rows rows: Int,
  cols cols: Int,
  with generator: fn(Int, Int) -> a,
) -> Grid(a) {
  all_pairs(rows, cols)
  |> list.map(fn(pair) { #(pair, generator(pair.0, pair.1)) })
  |> map.from_list
  |> Grid
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
    True -> Ok(Grid(map.insert(grid.values, #(row, col), value)))
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
  |> list.map(fn(pair) {
    let #(#(row, _), value) = pair
    #(row, value)
  })
  |> list.group(pair.first)
  |> map.map_values(fn(_, pairs) { list.map(pairs, pair.second) })
  |> map.to_list
  |> list.sort(fn(row1, row2) { int.compare(row1.0, row2.0) })
  |> list.map(pair.second)
}
