import gleam/order.{Eq, Gt, Lt, Order}

pub fn break_tie(order: Order, other: fn() -> Order) -> Order {
  case order {
    Lt | Gt -> order
    Eq -> other()
  }
}
