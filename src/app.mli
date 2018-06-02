open Breact

type action

val make : 'a -> (Game.result * Game.state, RR.noRetainedProps, action) ReasonReact.component