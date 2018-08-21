module M = Belt.Map.Int;

let maxSize = 21;
let minSize = 3;

let set = M.set;
let get = M.get;
let has = M.has;
let size = M.size;
let remove = M.remove;
let removeMany = M.removeMany;

type position = int;
type contents = Card.t;

type t = M.t(contents);

let initialize = cards =>
  cards
  |. Belt.List.mapWithIndex((ix, x) => (ix, x))
  |> Belt.List.toArray
  |> M.fromArray;

let full = map => size(map) >= maxSize;

let isReady = map => size(map) >= minSize;

let lastIndex = map => map |> M.maxKey |. Belt.Option.getWithDefault(0);

let nextIndex = map =>
  map |> M.maxKey |. Belt.Option.mapWithDefault(0, x => x + 1);

let gaps = map => {
  let keySet = M.keysToArray(map) |> Belt.Set.Int.fromArray;
  let rangeSet =
    Helpers.rangeArray(0, lastIndex(map)) |> Belt.Set.Int.fromArray;
  Belt.Set.Int.diff(keySet, rangeSet) |> Belt.Set.Int.toList;
};

let nextPosition = map =>
  switch (gaps(map)) {
  | [] => lastIndex(map) + 1
  | [x, ..._xs] => x
  };

let select = (table, a, b, c) =>
  get(table, a)
  |. Belt.Option.flatMap(ac =>
       get(table, b)
       |. Belt.Option.flatMap(bc =>
            get(table, c) |. Belt.Option.flatMap(cc => Some([ac, bc, cc]))
          )
     );

let add = (table, card) => set(table, nextPosition(table), card);