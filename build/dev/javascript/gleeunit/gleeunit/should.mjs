import * as $string from "../../gleam_stdlib/gleam/string.mjs";
import { inspect as stringify, toList, isEqual } from "../gleam.mjs";
import { crash } from "../gleeunit_ffi.mjs";

export function equal(a, b) {
  let $ = isEqual(a, b);
  if ($) {
    return undefined;
  } else {
    return crash(
      $string.concat(
        toList(["\n\t", stringify(a), "\n\tshould equal \n\t", stringify(b)]),
      ),
    );
  }
}

export function not_equal(a, b) {
  let $ = !isEqual(a, b);
  if ($) {
    return undefined;
  } else {
    return crash(
      $string.concat(
        toList(["\n", stringify(a), "\nshould not equal \n", stringify(b)]),
      ),
    );
  }
}

export function be_ok(a) {
  if (a.isOk()) {
    let value = a[0];
    return value;
  } else {
    return crash($string.concat(toList(["\n", stringify(a), "\nshould be ok"])));
  }
}

export function be_error(a) {
  if (!a.isOk()) {
    let error = a[0];
    return error;
  } else {
    return crash(
      $string.concat(toList(["\n", stringify(a), "\nshould be error"])),
    );
  }
}

export function be_true(actual) {
  let _pipe = actual;
  return equal(_pipe, true);
}

export function be_false(actual) {
  let _pipe = actual;
  return equal(_pipe, false);
}

export function fail() {
  return be_true(false);
}
