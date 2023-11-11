import * as $list from "../../gleam_stdlib/gleam/list.mjs";
import * as $order from "../../gleam_stdlib/gleam/order.mjs";

export function sort_on(list, projection, comparator) {
  return $list.sort(
    list,
    (one, other) => {
      let first = projection(one);
      let second = projection(other);
      return comparator(first, second);
    },
  );
}
