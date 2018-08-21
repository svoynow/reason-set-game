type shape =
  | Pill
  | Squiggle
  | Diamond;

type color =
  | Red
  | Green
  | Purple;

type num =
  | One
  | Two
  | Three;

type shading =
  | Solid
  | Hatched
  | Open;

let shapes = [Pill, Squiggle, Diamond];
let colors = [Red, Green, Purple];
let numbers = [One, Two, Three];
let shadings = [Solid, Hatched, Open];

let shapeAbbrev =
  fun
  | Pill => "P"
  | Squiggle => "S"
  | Diamond => "D";

let colorAbbrev =
  fun
  | Red => "r"
  | Green => "g"
  | Purple => "p";

let numAbbrev =
  fun
  | One => "1"
  | Two => "2"
  | Three => "3";

let shadingAbbrev =
  fun
  | Solid => "S"
  | Hatched => "H"
  | Open => "O";

type t = {
  shape,
  color,
  num,
  shading,
};

let imageUrl = ({shape, color, num, shading}) =>
  numAbbrev(num)
  ++ shadingAbbrev(shading)
  ++ colorAbbrev(color)
  ++ shapeAbbrev(shape)
  ++ ".svg";

let rec unique =
  fun
  | [] => []
  | [x, ...xs] when Helpers.contains(xs, x) => unique(xs)
  | [x, ...xs] => [x, ...unique(xs)];

let testAttribute = (cards, attribute) =>
  cards
  |. Belt.List.map(x => attribute(x))
  |> unique
  |> Belt.List.length
  |> (x => x == 1 || x == 3);

let isMatch = cards =>
  testAttribute(cards, x => x.shape)
  && testAttribute(cards, x => x.color)
  && testAttribute(cards, x => x.num)
  && testAttribute(cards, x => x.shading);