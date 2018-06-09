[%bs.raw {|require('./app.css')|}];

open Breact
open Mark

let mark_to_str = function
    Spade -> {js|♠|js}
  | Club -> {js|♣|js}
  | Heart -> {js|♥|js}
  | Diam -> {js|♦|js}

let is_red = function
    Heart
  | Diam -> true
  | _ -> false

let render_card number mark =
  let str = mark_to_str mark in
  let colour =
    if is_red mark then "red" else "" in
  div (props ~className:"card" ()) [|
    div (props ~className:"front" ()) [|
      span (props ~className:{j|top-left $colour|j} ()) [| s {j|$number$str|j} |];
      span (props ~className:{j|middle $colour|j} ()) [| s str |];
      span (props ~className:{j|bottom-right $colour|j} ()) [| s {j|$number$str|j} |]
    |]
  |]

let show_cards cards =
  cards
  |> List.rev
  |> List.map begin function
         1, mark -> render_card "A" mark
       | 11, mark -> render_card "J" mark
       | 12, mark -> render_card "Q" mark
       | 13, mark -> render_card "K" mark
       | n, mark -> render_card (string_of_int n) mark
     end
  |> Array.of_list

let render_cards_table cards =
  div (props ~className:"play-table" ()) cards

type state = Game.result * Game.state

let initialState () = Game.Playing, Game.init ()

type action
  = Hit
  | Stand
  | Retry

let reducer action state =
  match action, state with
    Hit, (Game.Playing, game_state) ->
    RR.Update (Game.next Game.Hit game_state)
  | Stand, (Game.Playing, game_state) ->
    RR.Update (Game.next Game.Stand game_state)
  | Retry, _ -> RR.Update (Game.Playing, Game.init ())
  | _ -> RR.NoUpdate

type self = (state, RR.noRetainedProps, action) RR.self

let render (self : self) =
  let _, state = self.state in
  let player, dealer, last_action = Game.split_state state in
  let player_cards = show_cards player in
  let dealer_cards =
    match last_action with
    | Game.NoAction ->
      let cards = show_cards dealer in
      let hided_cards =
        div (props ~className:"card" ()) [|
          div (props ~className:"back" ()) [||]
        |] in
      cards.(0) <- hided_cards;
      cards
    | _ -> show_cards dealer in
  let panel =
    match self.state with
    | Game.Playing, _ when last_action = Game.Stand ->
      div (props ~className:"button-table" ()) [|
        button (props ~className:"disabled" ~disabled:true ()) [| s "Hit" |];
        button (props ~onClick:(fun _ -> self.send Stand) ()) [| s "Stand" |]
      |]
    | Game.Playing, _ ->
      div (props ~className:"button-table" ()) [|
        button (props ~onClick:(fun _ -> self.send Hit) ()) [| s "Hit" |];
        button (props ~onClick:(fun _ -> self.send Stand) ()) [| s "Stand" |]
      |]
    | Game.Win, _ ->
      div (props ~className:"result-table" ()) [|
        div' [|
          span (props ~className:"result win" ()) [| s "You win!" |]
        |];
        div' [|
          button (props ~onClick:(fun _ -> self.send Retry) ()) [| s "Retry" |]
        |]
      |]
    | Game.Lose, _ ->
      div (props ~className:"result-table" ()) [|
        div' [|
          span (props ~className:"result lose" ()) [| s "You lose!" |]
        |];
        div' [|
          button (props ~onClick:(fun _ -> self.send Retry) ()) [| s "Retry" |]
        |]
      |] in
  div' [|
    div (props ~className:"table-wrapper" ()) [|
      render_cards_table dealer_cards;
      span (props ~className:"cast-label" ()) [| s "DEALER" |]
    |];
    div (props ~className:"table-wrapper" ()) [|
      span (props ~className:"cast-label" ()) [| s "PLAYER" |];
      render_cards_table player_cards
    |];
    panel
  |]

let component = RR.reducerComponent "App"

let make _children = {
  component with
  initialState;
  reducer;
  render
}