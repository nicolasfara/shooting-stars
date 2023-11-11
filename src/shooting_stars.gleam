import lustre
import lustre/attribute.{class}
import lustre/event.{on_click}
import lustre/element/html.{button, div}
import lustre/element.{type Element, text}
import gleam/option.{type Option, None, Some}
import shooting_stars/game.{
  type Game, type GameError, type Outcome, BlackHole, Continue, Lose, Star, Win,
}

pub fn main() {
  let app = lustre.simple(init, update, view)
  let assert Ok(_) = lustre.start(app, "[data-lustre-app]", Nil)

  Nil
}

pub type Model {
  Model(game: Game, error: Option(GameError), status: Outcome)
}

fn init(_) -> Model {
  Model(game: game.new(), error: None, status: Continue)
}

pub type Message {
  CellClicked(row: Int, col: Int)
  Reset
}

fn update(model: Model, message: Message) -> Model {
  case message, model.status {
    Reset, _ -> init(Nil)
    CellClicked(_, _), Win -> model
    CellClicked(row, col), _ ->
      case game.explode(model.game, row, col) {
        Ok(#(outcome, game)) -> Model(game, None, outcome)
        Error(err) -> Model(..model, error: Some(err))
      }
  }
}

fn view(model: Model) -> Element(Message) {
  html.main([], [message_view(model), grid_view(model), reset_button_view()])
}

fn reset_button_view() -> Element(Message) {
  button([on_click(Reset)], [text("Reset")])
}

fn message_view(model: Model) -> Element(nothing) {
  case model.status {
    Continue -> text("Keep going!")
    Win -> text("You win!")
    Lose -> text("You lose!")
  }
}

fn grid_view(model: Model) -> Element(Message) {
  div(
    [class("grid")],
    [
      div([on_click(CellClicked(0, 0))], [model_cell_view(model, 0, 0)]),
      div([on_click(CellClicked(0, 1))], [model_cell_view(model, 0, 1)]),
      div([on_click(CellClicked(0, 2))], [model_cell_view(model, 0, 2)]),
      div([on_click(CellClicked(1, 0))], [model_cell_view(model, 1, 0)]),
      div([on_click(CellClicked(1, 1))], [model_cell_view(model, 1, 1)]),
      div([on_click(CellClicked(1, 2))], [model_cell_view(model, 1, 2)]),
      div([on_click(CellClicked(2, 0))], [model_cell_view(model, 2, 0)]),
      div([on_click(CellClicked(2, 1))], [model_cell_view(model, 2, 1)]),
      div([on_click(CellClicked(2, 2))], [model_cell_view(model, 2, 2)]),
    ],
  )
}

fn model_cell_view(model: Model, row: Int, col: Int) -> Element(a) {
  case game.get(model.game, row, col) {
    Ok(Star) -> text("⭐")
    Ok(BlackHole) -> text("⚫")
    Error(_) -> text("")
  }
}
