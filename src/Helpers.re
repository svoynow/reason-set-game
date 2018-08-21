let contains = (xs, x) => Belt.List.some(xs, y => y == x);

let rangeList = (start, finish) =>
  Belt.List.makeBy(finish - start + 1, x => x + start);

let rangeArray = (start, finish) =>
  Belt.Array.makeBy(finish - start + 1, x => x + start);

let str = x => ReasonReact.string(x);

let noOp = _ => ();

let string_of_intlist = ints =>
  Belt.List.map(ints, string_of_int) |> String.concat(", ");