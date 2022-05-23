let create = (left, val, right) => {
  let hl = Base.height(left)
  let hr = Base.height(right)
  Base.Node(
    left,
    val,
    right,
    if hl > hr {
      hl + 1
    } else {
      hr + 1
    },
  )
}
let rec add = (val, tree) =>
  switch tree {
  | Base.Empty => Base.Node(Empty, val, Empty, 1)
  | Node(left, v, right, _) =>
    if val < v {
      create(add(val, left), v, right)
    } else if val > v {
      create(left, v, add(val, right))
    } else {
      tree
    }
  }
let root = Base.Node(Base.Empty, 1, Base.Empty, 0)
let newRoot = add(1, root)
