import * as $dynamic from "../gleam_stdlib/gleam/dynamic.mjs";
import * as $map from "../gleam_stdlib/gleam/map.mjs";
import { CustomType as $CustomType } from "./gleam.mjs";
import {
  setup as application,
  setup_component as component,
  start,
  destroy,
  is_browser,
  is_registered,
} from "./lustre.ffi.mjs";
import * as $effect from "./lustre/effect.mjs";
import * as $element from "./lustre/element.mjs";

export { application, component, destroy, is_browser, is_registered, start };

export class AppAlreadyStarted extends $CustomType {}

export class AppNotYetStarted extends $CustomType {}

export class BadComponentName extends $CustomType {}

export class ComponentAlreadyRegistered extends $CustomType {}

export class ElementNotFound extends $CustomType {}

export class NotABrowser extends $CustomType {}

export function element(element) {
  let init = (_) => { return [undefined, $effect.none()]; };
  let update = (_, _1) => { return [undefined, $effect.none()]; };
  let view = (_) => { return element; };
  return application(init, update, view);
}

export function simple(init, update, view) {
  let init$1 = (flags) => { return [init(flags), $effect.none()]; };
  let update$1 = (model, msg) => { return [update(model, msg), $effect.none()]; };
  return application(init$1, update$1, view);
}
