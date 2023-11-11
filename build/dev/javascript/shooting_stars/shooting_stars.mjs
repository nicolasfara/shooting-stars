import * as $option from "../gleam_stdlib/gleam/option.mjs";
import { None, Some } from "../gleam_stdlib/gleam/option.mjs";
import * as $lustre from "../lustre/lustre.mjs";
import * as $attribute from "../lustre/lustre/attribute.mjs";
import { class$ } from "../lustre/lustre/attribute.mjs";
import * as $element from "../lustre/lustre/element.mjs";
import { text } from "../lustre/lustre/element.mjs";
import * as $html from "../lustre/lustre/element/html.mjs";
import { button, div } from "../lustre/lustre/element/html.mjs";
import * as $event from "../lustre/lustre/event.mjs";
import { on_click } from "../lustre/lustre/event.mjs";
import { toList, CustomType as $CustomType, makeError } from "./gleam.mjs";
import * as $game from "./shooting_stars/game.mjs";
import { BlackHole, Continue, Lose, Star, Win } from "./shooting_stars/game.mjs";

export class Model extends $CustomType {
  constructor(game, error, status) {
    super();
    this.game = game;
    this.error = error;
    this.status = status;
  }
}

export class CellClicked extends $CustomType {
  constructor(row, col) {
    super();
    this.row = row;
    this.col = col;
  }
}

export class Reset extends $CustomType {}

function init(_) {
  return new Model($game.new$(), new None(), new Continue());
}

function update(model, message) {
  let $ = model.status;
  if (message instanceof Reset) {
    return init(undefined);
  } else if (message instanceof CellClicked && $ instanceof Win) {
    return model;
  } else if (message instanceof CellClicked) {
    let row = message.row;
    let col = message.col;
    let $1 = $game.explode(model.game, row, col);
    if ($1.isOk()) {
      let outcome = $1[0][0];
      let game = $1[0][1];
      return new Model(game, new None(), outcome);
    } else if (!$1.isOk()) {
      let err = $1[0];
      return model.withFields({ error: new Some(err) });
    } else {
      throw makeError(
        "case_no_match",
        "shooting_stars",
        36,
        "update",
        "No case clause matched",
        { values: [$1] }
      )
    }
  } else {
    throw makeError(
      "case_no_match",
      "shooting_stars",
      32,
      "update",
      "No case clause matched",
      { values: [message, $] }
    )
  }
}

function reset_button_view() {
  return button(toList([on_click(new Reset())]), toList([text("Reset")]));
}

function message_view(model) {
  let $ = model.status;
  if ($ instanceof Continue) {
    return text("Keep going!");
  } else if ($ instanceof Win) {
    return text("You win!");
  } else if ($ instanceof Lose) {
    return text("You lose!");
  } else {
    throw makeError(
      "case_no_match",
      "shooting_stars",
      52,
      "message_view",
      "No case clause matched",
      { values: [$] }
    )
  }
}

function model_cell_view(model, row, col) {
  let $ = $game.get(model.game, row, col);
  if ($.isOk() && $[0] instanceof Star) {
    return text("⭐");
  } else if ($.isOk() && $[0] instanceof BlackHole) {
    return text("⚫");
  } else if (!$.isOk()) {
    return text("");
  } else {
    throw makeError(
      "case_no_match",
      "shooting_stars",
      77,
      "model_cell_view",
      "No case clause matched",
      { values: [$] }
    )
  }
}

function grid_view(model) {
  return div(
    toList([class$("grid")]),
    toList([
      div(
        toList([on_click(new CellClicked(0, 0))]),
        toList([model_cell_view(model, 0, 0)]),
      ),
      div(
        toList([on_click(new CellClicked(0, 1))]),
        toList([model_cell_view(model, 0, 1)]),
      ),
      div(
        toList([on_click(new CellClicked(0, 2))]),
        toList([model_cell_view(model, 0, 2)]),
      ),
      div(
        toList([on_click(new CellClicked(1, 0))]),
        toList([model_cell_view(model, 1, 0)]),
      ),
      div(
        toList([on_click(new CellClicked(1, 1))]),
        toList([model_cell_view(model, 1, 1)]),
      ),
      div(
        toList([on_click(new CellClicked(1, 2))]),
        toList([model_cell_view(model, 1, 2)]),
      ),
      div(
        toList([on_click(new CellClicked(2, 0))]),
        toList([model_cell_view(model, 2, 0)]),
      ),
      div(
        toList([on_click(new CellClicked(2, 1))]),
        toList([model_cell_view(model, 2, 1)]),
      ),
      div(
        toList([on_click(new CellClicked(2, 2))]),
        toList([model_cell_view(model, 2, 2)]),
      ),
    ]),
  );
}

function view(model) {
  return $html.main(
    toList([]),
    toList([message_view(model), grid_view(model), reset_button_view()]),
  );
}

export function main() {
  let app = $lustre.simple(init, update, view);
  let $ = $lustre.start(app, "[data-lustre-app]", undefined);
  if (!$.isOk()) {
    throw makeError(
      "assignment_no_match",
      "shooting_stars",
      13,
      "main",
      "Assignment pattern did not match",
      { value: $ }
    )
  }
  return undefined;
}
