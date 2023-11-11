import * as $bytes_builder from "../gleam/bytes_builder.mjs";
import * as $string_builder from "../gleam/string_builder.mjs";

export function new$() {
  return $bytes_builder.new$();
}

export function prepend(to, prefix) {
  return $bytes_builder.prepend(to, prefix);
}

export function append(to, suffix) {
  return $bytes_builder.append(to, suffix);
}

export function prepend_builder(to, prefix) {
  return $bytes_builder.prepend_builder(to, prefix);
}

export function append_builder(first, second) {
  return $bytes_builder.append_builder(first, second);
}

export function prepend_string(to, prefix) {
  return $bytes_builder.prepend_string(to, prefix);
}

export function append_string(to, suffix) {
  return $bytes_builder.append_string(to, suffix);
}

export function concat(builders) {
  return $bytes_builder.concat(builders);
}

export function concat_bit_strings(bits) {
  return $bytes_builder.concat_bit_arrays(bits);
}

export function from_string(string) {
  return $bytes_builder.from_string(string);
}

export function from_string_builder(builder) {
  return $bytes_builder.from_string_builder(builder);
}

export function from_bit_string(bits) {
  return $bytes_builder.from_bit_array(bits);
}

export function to_bit_string(builder) {
  return $bytes_builder.to_bit_array(builder);
}

export function byte_size(builder) {
  return $bytes_builder.byte_size(builder);
}
