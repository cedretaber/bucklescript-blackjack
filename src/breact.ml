module RR = ReasonReact

let get_or_else = function
    Some props -> props
  | None -> Js.Obj.empty ()

let div ?props children =
  RR.createDomElement "div" ~props:(get_or_else props) children

let span ?props children =
  RR.createDomElement "span" ~props:(get_or_else props) children

let button ?props children =
  RR.createDomElement "button" ~props:(get_or_else props) children

let img ?props children =
  RR.createDomElement "img" ~props:(get_or_else props) children

let h2 ?props children =
  RR.createDomElement "h2" ~props:(get_or_else props) children

let code ?props children =
  RR.createDomElement "code" ~props:(get_or_else props) children

let p ?props children =
  RR.createDomElement "p" ~props:(get_or_else props) children

let s str = RR.string str