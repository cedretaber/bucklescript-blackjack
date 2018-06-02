type cards = (int * Mark.t) list

type action
  = NoAction
  | Hit
  | Stand

type state

val split_state : state -> cards * cards * action

type result
  = Playing
  | Win
  | Lose

val init : unit -> state

val next : action -> state -> result * state