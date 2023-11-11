import * as $order from "../../gleam_stdlib/gleam/order.mjs";
import { Eq, Gt, Lt } from "../../gleam_stdlib/gleam/order.mjs";
import { makeError } from "../gleam.mjs";

export function break_tie(order, other) {
  if (order instanceof Lt) {
    return order;
  } else if (order instanceof Gt) {
    return order;
  } else if (order instanceof Eq) {
    return other();
  } else {
    throw makeError(
      "case_no_match",
      "internals/order_extra",
      4,
      "break_tie",
      "No case clause matched",
      { values: [order] }
    )
  }
}
