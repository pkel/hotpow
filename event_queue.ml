type time = float
type 'a t = Empty | Node of time * 'a * 'a t * 'a t

let empty = Empty

let rec schedule queue time evt =
  match queue with
  | Empty -> Node (time, evt, Empty, Empty)
  | Node (time', evt', left, right) ->
      if time <= time' then Node (time, evt, schedule right time' evt', left)
      else Node (time', evt', schedule right time evt, left)

let rec remove_top = function
  | Empty -> raise Not_found
  | Node (_time, _evt, left, Empty) -> left
  | Node (_time, _evt, Empty, right) -> right
  | Node
      ( _time
      , _evt
      , (Node (ltime, levt, _, _) as left)
      , (Node (rtime, revt, _, _) as right) ) ->
      if ltime <= rtime then Node (ltime, levt, remove_top left, right)
      else Node (rtime, revt, left, remove_top right)

let next = function
  | Empty -> raise Not_found
  | Node (time, evt, _, _) as queue -> (time, evt, remove_top queue)
