let create = (l, v, r) => {
  let hl = Base.height(l)
  let hr = Base.height(r)
  Base.Node(
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
  let hl = Base.height(l)
  let hr = Base.height(r)
  if hl > hr + 2 {
    switch l {
    | Node(ll, lv, lr, _) if Base.height(ll) >= Base.height(lr) => create(ll, lv, create(lr, v, r))
    | Node(ll, lv, Node(lrl, lrv, lrr, _), _) => create(create(ll, lv, lrl), lrv, create(lrr, v, r))
    | _ => assert false
    }
  } else if hr > hl + 2 {
    switch r {
    | Node(rl, rv, rr, _) if Base.height(rr) >= Base.height(rl) => create(create(l, v, rl), rv, rr)
    | Node(Node(rll, rlv, rlr, _), rv, rr, _) => create(create(l, v, rll), rlv, create(rlr, rv, rr))
    | _ => assert false
    }
  } else {
    create(l, v, r)
  }
}

let rec add = (x, tree) => {
  switch tree {
  | Base.Empty => Base.Node(Empty, x, Empty, 1)
  | Base.Node(l, v, r, _) as t =>
    if x == v {
      t
    } else if x < v {
      bal(add(x, l), v, r)
    } else {
      bal(l, v, add(x, r))
    }
  }
}
