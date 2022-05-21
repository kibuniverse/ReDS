// define tree type
type rec t =
  | Empty
  | Node(t, int, t, int)

let height = x =>
  switch x {
  | Empty => 0
  | Node(_, _, _, h) => h
  }

let create = (l, v, r) => {
  let hl = height(l)
  let hr = height(r)
  Node(
    l,
    v,
    r,
    if hl > hr {
      hl + 1
    } else {
      hr + 1
    },
  )
}

let bal = (l, v, r) => {
  let hl = height(l)
  let hr = height(r)
  if hl > hr + 2 {
    switch l {
    | Node(ll, lv, lr, _) if height(ll) >= height(lr) => create(ll, lv, create(lr, v, r))
    | Node(ll, lv, Node(lrl, lrv, lrr, _), _) => create(create(ll, lv, lrl), lrv, create(lrr, v, r))
    | _ => assert false
    }
  } else if hr > hl + 2 {
    switch r {
    | Node(rl, rv, rr, _) if height(rr) >= height(rl) => create(create(l, v, rl), rv, rr)
    | Node(Node(rll, rlv, rlr, _), rv, rr, _) => create(create(l, v, rll), rlv, create(rlr, rv, rr))
    | _ => assert false
    }
  } else {
    create(l, v, r)
  }
}

let rec add = (x, tree) =>
  switch tree {
  | Empty => Node(Empty, x, Empty, 1)
  | Node(l, v, r, _) as t =>
    if x == v {
      t
    } else if x < v {
      bal(add(x, l), v, r)
    } else {
      bal(l, v, add(x, r))
    }
  }
