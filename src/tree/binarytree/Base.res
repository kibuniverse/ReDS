// define tree type
type rec t =
  | Empty
  | Node(t, int, t, int)

let height = x =>
  switch x {
  | Empty => 0
  | Node(_, _, _, h) => h
  }

// print tree
let rec printTreeRec = t =>
  switch t {
  | Empty => ()
  | Node(left, v, right, _) => {
      printTreeRec(left)
      Js.log(v)
      printTreeRec(right)
    }
  }
