
let may f = function
    Some x -> f x
  | None -> ()

let may' opt f = may f opt