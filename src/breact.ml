module RR = ReasonReact

let unsafe_set dict key value =
  Js.Dict.set dict key @@ Obj.magic value

let create_dom_element tag props children =
  RR.createDomElement tag ~props:(Obj.magic props) children

let div ?class_name children =
  let props = Js.Dict.empty () in
  Option.may' class_name @@ unsafe_set props "className";
  create_dom_element "div" props children

let span ?class_name children =
  let props = Js.Dict.empty () in
  Option.may' class_name @@ unsafe_set props "className";
  create_dom_element "span" props children

let button ?class_name ?disabled ?on_click children =
  let props = Js.Dict.empty () in
  Option.may' class_name @@ unsafe_set props "className";
  Option.may' disabled @@ unsafe_set props "disabled";
  Option.may' on_click @@ unsafe_set props "onClick";
  create_dom_element "button" props children

let s = RR.string