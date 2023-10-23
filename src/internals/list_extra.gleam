import gleam/order.{Order}
import gleam/list

pub fn sort_on(
  list: List(a),
  on projection: fn(a) -> b,
  with comparator: fn(b, b) -> Order,
) -> List(a) {
  list.sort(
    list,
    fn(one, other) {
      let first = projection(one)
      let second = projection(other)
      comparator(first, second)
    },
  )
}
