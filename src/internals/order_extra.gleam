import gleam/order.{type Order, Eq, Gt, Lt}

pub fn break_tie(order: Order, other: fn() -> Order) -> Order {
  case order {
    Lt | Gt -> order
    Eq -> other()
  }
}
