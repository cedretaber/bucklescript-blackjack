
type cards = (int * Mark.t) list

type action
  = NoAction
  | Hit
  | Stand

type state = {
  player : cards;
  dealer : cards;
  last_action : action
}

let split_state {player; dealer; last_action} = player, dealer, last_action

type result
  = Playing
  | Win
  | Lose

exception UnexpectedRandomNumber of int

let draw_card () =
  let number =
    match Random.int 13 with
      0 -> 13
    | n -> n in
  let mark =
    let open Mark in
    match Random.int 4 with
      0 -> Spade
    | 1 -> Club
    | 2 -> Heart
    | 3 -> Diam
    | n -> raise @@ UnexpectedRandomNumber n in
  number, mark

let sum_cards (cards : cards) =
  let sum, ac =
    cards
    |> List.map (fun (n, _) -> n)
    |> List.fold_left begin fun (s, a) -> function
          1 -> (s + 1), (a + 1)
        | n when n = 11 || n = 12 || n = 13 -> (s + 10), a
        | n -> (s + n), a
      end
      (0, 0) in

  let rec ret = fun sum -> function
      0 -> sum
    | _ when sum + 10 > 21 -> sum
    | _ -> ret (sum + 10) (ac - 1) in

  ret sum ac

let is_bust cards = sum_cards cards > 21

let will_dealer_draw cards = sum_cards cards < 17

let init () =
  Random.self_init ();
  {
    player = [draw_card (); draw_card ()];
    dealer = [draw_card (); draw_card ()];
    last_action = NoAction
  }

let rec next action {player; dealer; last_action} =
  let player, last_action =
    match last_action, action with
      _, Stand
    | _, NoAction
    | Stand, Hit -> player, Stand
    | _, Hit -> draw_card () :: player, Hit in

  if is_bust player then
    Lose, {player; dealer; last_action}
  else
    let dealer =
      if will_dealer_draw dealer then
        draw_card () :: dealer
      else dealer in
    let state = {player; dealer; last_action} in

    match is_bust dealer, will_dealer_draw dealer, last_action with
      true, _, _ -> Win, state
    | _, _, Hit -> Playing, state
    | _, true, Stand -> next Stand state
    | _, false, Stand when sum_cards player > sum_cards dealer ->
      Win, state
    | _, false, Stand -> Lose, state
    | _ -> Playing, state