import gleam/io
import shooting_stars/game.{
  Continue, Game, GameError, Lose, NotAStar, OutOfRange, Win,
}
import gleam/erlang
import gleam/int
import gleam/string

pub fn main() {
  io.println("Hello from shooting_stars!")
  main_loop(game.new())
}

fn game_error_to_string(error: GameError) -> String {
  case error {
    OutOfRange -> "Move out of range"
    NotAStar -> "Not a star"
  }
}

fn main_loop(game: Game) {
  io.println(game.to_string(game))

  let row = ask_int("Insert row: ")
  let col = ask_int("Insert col: ")

  case game.explode(game, row, col) {
    Error(err) -> {
      io.println(game_error_to_string(err))
      main_loop(game)
    }
    Ok(#(outcome, new_game)) ->
      case outcome {
        Win -> io.println("Win!\n" <> game.to_string(new_game))
        Lose -> io.println("Lose!\n" <> game.to_string(new_game))
        Continue -> main_loop(new_game)
      }
  }
}

fn ask_int(prompt: String) -> Int {
  case erlang.get_line(prompt) {
    Error(_) -> panic as "Fatal error"
    Ok(line) ->
      case int.parse(string.trim(line)) {
        Error(_) -> ask_int(prompt)
        Ok(value) -> value
      }
  }
}
