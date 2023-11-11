import * as $bool from "../../gleam_stdlib/gleam/bool.mjs";
import * as $list from "../../gleam_stdlib/gleam/list.mjs";
import * as $result from "../../gleam_stdlib/gleam/result.mjs";
import * as $string from "../../gleam_stdlib/gleam/string.mjs";
import { Ok, Error, toList, CustomType as $CustomType, makeError, isEqual } from "../gleam.mjs";
import * as $grid from "../internals/grid.mjs";

class Game extends $CustomType {
  constructor(grid) {
    super();
    this.grid = grid;
  }
}

export class NotAStar extends $CustomType {}

export class OutOfRange extends $CustomType {}

export class Star extends $CustomType {}

export class BlackHole extends $CustomType {}

export class Win extends $CustomType {}

export class Lose extends $CustomType {}

export class Continue extends $CustomType {}

export function new$() {
  return new Game(
    $grid.new$(
      3,
      3,
      (row, col) => {
        if (row === 1 && col === 1) {
          return new Star();
        } else {
          return new BlackHole();
        }
      },
    ),
  );
}

export function get(game, row, col) {
  let _pipe = $grid.get(game.grid, row, col);
  return $result.replace_error(_pipe, new OutOfRange());
}

function flip_cell(cell) {
  if (cell instanceof Star) {
    return new BlackHole();
  } else if (cell instanceof BlackHole) {
    return new Star();
  } else {
    throw makeError(
      "case_no_match",
      "shooting_stars/game",
      79,
      "flip_cell",
      "No case clause matched",
      { values: [cell] }
    )
  }
}

function flip(game, coord) {
  let row = coord[0];
  let col = coord[1];
  let _pipe = $grid.update(game.grid, row, col, flip_cell);
  let _pipe$1 = $result.unwrap(_pipe, game.grid);
  return new Game(_pipe$1);
}

function flip_all(game, coord) {
  return $list.fold(coord, game, flip);
}

function get_neighbours(row, col) {
  if (row === 0 && col === 0) {
    return new Ok(toList([[0, 1], [1, 0], [1, 1]]));
  } else if (row === 0 && col === 1) {
    return new Ok(toList([[0, 0], [0, 2]]));
  } else if (row === 0 && col === 2) {
    return new Ok(toList([[0, 1], [1, 1], [1, 2]]));
  } else if (row === 1 && col === 0) {
    return new Ok(toList([[0, 0], [2, 0]]));
  } else if (row === 1 && col === 1) {
    return new Ok(toList([[0, 1], [1, 0], [1, 2], [2, 1]]));
  } else if (row === 1 && col === 2) {
    return new Ok(toList([[0, 2], [2, 2]]));
  } else if (row === 2 && col === 0) {
    return new Ok(toList([[1, 0], [1, 1], [2, 1]]));
  } else if (row === 2 && col === 1) {
    return new Ok(toList([[2, 0], [2, 2]]));
  } else if (row === 2 && col === 2) {
    return new Ok(toList([[1, 1], [1, 2], [2, 1]]));
  } else {
    return new Error(undefined);
  }
}

function explode_star(game, row, col) {
  return $result.try$(
    get(game, row, col),
    (cell) => {
      return $bool.guard(
        !isEqual(cell, new Star()),
        new Error(new NotAStar()),
        () => {
          let $ = get_neighbours(row, col);
          if ($.isOk()) {
            let neighbours = $[0];
            return new Ok(flip_all(game, toList([[row, col]], neighbours)));
          } else if (!$.isOk() && !$[0]) {
            return new Error(new OutOfRange());
          } else {
            throw makeError(
              "case_no_match",
              "shooting_stars/game",
              61,
              "",
              "No case clause matched",
              { values: [$] }
            )
          }
        },
      );
    },
  );
}

function winning_grid() {
  return $grid.new$(
    3,
    3,
    (row, col) => {
      if (row === 1 && col === 1) {
        return new BlackHole();
      } else {
        return new Star();
      }
    },
  );
}

function losing_grid() {
  return $grid.new$(3, 3, (_, _1) => { return new BlackHole(); });
}

function get_outcome(game) {
  return $bool.guard(
    isEqual(game.grid, winning_grid()),
    new Win(),
    () => {
      return $bool.guard(
        isEqual(game.grid, losing_grid()),
        new Lose(),
        () => { return new Continue(); },
      );
    },
  );
}

export function explode(game, row, col) {
  return $result.map(
    explode_star(game, row, col),
    (new_game) => { return [get_outcome(new_game), new_game]; },
  );
}

function cell_to_string(cell) {
  if (cell instanceof Star) {
    return "*";
  } else if (cell instanceof BlackHole) {
    return ".";
  } else {
    throw makeError(
      "case_no_match",
      "shooting_stars/game",
      130,
      "cell_to_string",
      "No case clause matched",
      { values: [cell] }
    )
  }
}

function row_to_string(cells) {
  let _pipe = $list.map(cells, cell_to_string);
  return $string.join(_pipe, " ");
}

export function to_string(game) {
  let _pipe = $grid.rows(game.grid);
  let _pipe$1 = $list.map(_pipe, row_to_string);
  return $string.join(_pipe$1, "\n");
}
