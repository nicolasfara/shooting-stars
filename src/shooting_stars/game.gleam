import internals/grid.{Grid}
import gleam/bool
import gleam/result
import gleam/list
import gleam/string

pub opaque type Game {
  Game(grid: Grid(Cell))
}

pub type GameError {
  NotAStar
  OutOfRange
}

pub type Cell {
  Star
  BlackHole
}

pub type Outcome {
  Win
  Lose
  Continue
}

pub fn new() -> Game {
  Game(grid.new(
    rows: 3,
    cols: 3,
    with: fn(row, col) {
      case row, col {
        1, 1 -> Star
        _, _ -> BlackHole
      }
    },
  ))
}

pub fn get(
  from game: Game,
  row row: Int,
  col col: Int,
) -> Result(Cell, GameError) {
  grid.get(game.grid, row, col)
  |> result.replace_error(OutOfRange)
}

pub fn explode(
  game: Game,
  row row: Int,
  col col: Int,
) -> Result(#(Outcome, Game), GameError) {
  use new_game <- result.map(explode_star(game, row, col))
  #(get_outcome(new_game), new_game)
}

fn explode_star(game: Game, row: Int, col: Int) -> Result(Game, GameError) {
  use cell <- result.try(get(game, row, col))
  use <- bool.guard(when: cell != Star, return: Error(NotAStar))
  case get_neighbours(row, col) {
    Ok(neighbours) -> Ok(flip_all(game, [#(row, col), ..neighbours]))
    Error(Nil) -> Error(OutOfRange)
  }
}

fn flip(game: Game, coord: #(Int, Int)) -> Game {
  let #(row, col) = coord
  grid.update(game.grid, row, col, flip_cell)
  |> result.unwrap(game.grid)
  |> Game
}

fn flip_all(game: Game, coord: List(#(Int, Int))) -> Game {
  list.fold(over: coord, from: game, with: flip)
}

fn flip_cell(cell: Cell) -> Cell {
  case cell {
    Star -> BlackHole
    BlackHole -> Star
  }
}

fn get_neighbours(row: Int, col: Int) -> Result(List(#(Int, Int)), Nil) {
  case row, col {
    0, 0 -> Ok([#(0, 1), #(1, 0), #(1, 1)])
    0, 1 -> Ok([#(0, 0), #(0, 2)])
    0, 2 -> Ok([#(0, 1), #(1, 1), #(1, 2)])
    1, 0 -> Ok([#(0, 0), #(2, 0)])
    1, 1 -> Ok([#(0, 1), #(1, 0), #(1, 2), #(2, 1)])
    1, 2 -> Ok([#(0, 2), #(2, 2)])
    2, 0 -> Ok([#(1, 0), #(1, 1), #(2, 1)])
    2, 1 -> Ok([#(2, 0), #(2, 2)])
    2, 2 -> Ok([#(1, 1), #(1, 2), #(2, 1)])
    _, _ -> Error(Nil)
  }
}

fn get_outcome(game: Game) -> Outcome {
  use <- bool.guard(when: game.grid == winning_grid(), return: Win)
  use <- bool.guard(when: game.grid == losing_grid(), return: Lose)
  Continue
}

fn winning_grid() -> Grid(Cell) {
  use row, col <- grid.new(3, 3)
  case row, col {
    1, 1 -> BlackHole
    _, _ -> Star
  }
}

fn losing_grid() -> Grid(Cell) {
  grid.new(3, 3, fn(_, _) { BlackHole })
}

pub fn to_string(game: Game) -> String {
  grid.rows(game.grid)
  |> list.map(row_to_string)
  |> string.join(with: "\n")
}

fn row_to_string(cells: List(Cell)) -> String {
  list.map(cells, cell_to_string)
  |> string.join(with: " ")
}

fn cell_to_string(cell: Cell) -> String {
  case cell {
    Star -> "*"
    BlackHole -> "."
  }
}
