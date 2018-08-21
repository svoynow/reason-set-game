type t = list(Card.t);

let make =
  Belt.List.map(Card.shapes, shape =>
    Belt.List.map(Card.colors, color =>
      Belt.List.map(Card.numbers, num =>
        Belt.List.map(Card.shadings, shading =>
          {Card.shape, Card.color, Card.num, Card.shading}
        )
      )
    )
  )
  |> Belt.List.flatten
  |> Belt.List.flatten
  |> Belt.List.flatten;