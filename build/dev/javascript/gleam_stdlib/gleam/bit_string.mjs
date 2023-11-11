import * as $bit_array from "../gleam/bit_array.mjs";

export function from_string(x) {
  return $bit_array.from_string(x);
}

export function byte_size(x) {
  return $bit_array.byte_size(x);
}

export function append(first, second) {
  return $bit_array.append(first, second);
}

export function slice(string, position, length) {
  return $bit_array.slice(string, position, length);
}

export function is_utf8(bits) {
  return $bit_array.is_utf8(bits);
}

export function to_string(bits) {
  return $bit_array.to_string(bits);
}

export function concat(bit_strings) {
  return $bit_array.concat(bit_strings);
}
