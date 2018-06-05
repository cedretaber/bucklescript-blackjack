module RR = ReasonReact

module RDRe = ReactDOMRe

let props = ReactDOMRe.props

let create_element tag props children =
  RR.createDomElement tag ~props:(Obj.magic props) children

let div props children =
  create_element "div" props children

let span props children =
  create_element "span" props children

let button props children =
  create_element "button" props children

let s = RR.string