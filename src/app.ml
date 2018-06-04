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
  div ~class_name:"card" [|
    div ~class_name:"front" [|
      span ~class_name:{j|top-left $colour|j} [| s {j|$number$str|j} |];
      span ~class_name:{j|middle $colour|j} [| s str |];
      span ~class_name:{j|bottom-right $colour|j} [| s {j|$number$str|j} |]
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
  div ~class_name:"play-table" cards

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
  | _ -> RR.Update state

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
        div ~class_name:"card" [|
          div ~class_name:"back" [||]
        |] in
      cards.(0) <- hided_cards;
      cards
    | _ -> show_cards dealer in
  let panel =
    match self.state with
    | Game.Playing, _ when last_action = Game.Stand ->
      div ~class_name:"button-table" [|
        button ~class_name:"disabled" ~disabled:true [| s "Hit" |];
        button ~on_click:(fun _ -> self.send Stand) [| s "Stand" |]
      |]
    | Game.Playing, _ ->
      div ~class_name:"button-table" [|
        button ~on_click:(fun _ -> self.send Hit) [| s "Hit" |];
        button ~on_click:(fun _ -> self.send Stand) [| s "Stand" |]
      |]
    | Game.Win, _ ->
      div ~class_name:"result-table" [|
        div [|
          span ~class_name:"result win" [| s "You win!" |]
        |];
        div [|
          button ~on_click:(fun _ -> self.send Retry) [| s "Retry" |]
        |]
      |]
    | Game.Lose, _ ->
      div ~class_name:"result-table" [|
        div [|
          span ~class_name:"result lose" [| s "You lose!" |]
        |];
        div [|
          button ~on_click:(fun _ -> self.send Retry) [| s "Retry" |]
        |]
      |] in
  div [|
    div ~class_name:"table-wrapper" [|
      render_cards_table dealer_cards;
      span ~class_name:"cast-label" [| s "DEALER" |]
    |];
    div ~class_name:"table-wrapper" [|
      span ~class_name:"cast-label" [| s "PLAYER" |];
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