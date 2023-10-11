import shooting_stars/grid.{Grid}
import gleam/bool
import gleam/result
import gleam/list
import gleam/string

pub opaque type Game {
  Game(grid: Grid(Cell))
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

pub fn get(from game: Game, row row: Int, col col: Int) -> Result(Cell, Nil) {
  grid.get(game.grid, row, col)
}

pub fn explode(
  game: Game,
  row row: Int,
  col col: Int,
) -> Result(#(Outcome, Game), Nil) {
  use new_game <- result.map(explode_star(game, row, col))
  #(get_outcome(new_game), new_game)
}

fn explode_star(game: Game, row: Int, col: Int) -> Result(Game, Nil) {
  use <- bool.guard(when: !is_star(game, row, col), return: Error(Nil))
  use coords <- result.map(get_neighbours(row, col))
  let initial_state = flip(game, #(row, col))
  list.fold(over: coords, from: initial_state, with: flip)
}

fn flip(game: Game, coord: #(Int, Int)) -> Game {
  let #(row, col) = coord
  grid.update(game.grid, row, col, flip_cell)
  |> result.unwrap(game.grid)
  |> Game
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

fn is_star(game: Game, row: Int, col: Int) -> Bool {
  case get(game, row, col) {
    Ok(Star) -> True
    _ -> False
  }
}

fn get_outcome(game: Game) -> Outcome {
  use <- bool.guard(when: has_won(game), return: Win)
  use <- bool.guard(when: has_lost(game), return: Lose)
  Continue
}

fn has_won(game: Game) -> Bool {
  let winning_grid =
    grid.new(
      3,
      3,
      fn(row, col) {
        case row, col {
          1, 1 -> BlackHole
          _, _ -> Star
        }
      },
    )
  game.grid == winning_grid
}

fn has_lost(game: Game) -> Bool {
  let losing_grid = grid.new(3, 3, fn(_, _) { BlackHole })
  game.grid == losing_grid
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
