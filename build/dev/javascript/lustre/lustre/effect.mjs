import * as $dynamic from "../../gleam_stdlib/gleam/dynamic.mjs";
import * as $function from "../../gleam_stdlib/gleam/function.mjs";
import * as $list from "../../gleam_stdlib/gleam/list.mjs";
import { toList, CustomType as $CustomType, makeError } from "../gleam.mjs";

class Effect extends $CustomType {
  constructor(all) {
    super();
    this.all = all;
  }
}

export function from(effect) {
  return new Effect(toList([(dispatch, _) => { return effect(dispatch); }]));
}

export function event(name, data) {
  return new Effect(
    toList([(_, emit) => { return emit(name, $dynamic.from(data)); }]),
  );
}

export function none() {
  return new Effect(toList([]));
}

export function batch(effects) {
  return new Effect(
    $list.fold(
      effects,
      toList([]),
      (b, _use1) => {
        if (!(_use1 instanceof Effect)) {
          throw makeError(
            "assignment_no_match",
            "lustre/effect",
            51,
            "",
            "Assignment pattern did not match",
            { value: _use1 }
          )
        }
        let a = _use1.all;
        return $list.append(b, a);
      },
    ),
  );
}

export function map(effect, f) {
  return new Effect(
    $list.map(
      effect.all,
      (effect) => {
        return (dispatch, emit) => {
          let dispatch$1 = $function.compose(f, dispatch);
          return effect(dispatch$1, emit);
        };
      },
    ),
  );
}
