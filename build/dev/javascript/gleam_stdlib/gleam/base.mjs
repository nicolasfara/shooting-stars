import * as $bit_array from "../gleam/bit_array.mjs";

export function encode64(input, padding) {
  return $bit_array.base64_encode(input, padding);
}

export function decode64(encoded) {
  return $bit_array.base64_decode(encoded);
}

export function url_encode64(input, padding) {
  return $bit_array.base64_url_encode(input, padding);
}

export function url_decode64(encoded) {
  return $bit_array.base64_url_decode(encoded);
}
