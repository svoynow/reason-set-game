open Belt.Result;

type gameState =
  | Dealing
  | WaitingForSelection
  | OneCardSelected(Table.position)
  | TwoCardsSelected(Table.position, Table.position)
  | Match(Table.position, Table.position, Table.position)
  | NoMatch(Table.position, Table.position, Table.position)
  | Done;

type t = {
  deck: Deck.t,
  table: Table.t,
  gameState,
};

type action =
  | Tick
  | Claim
  | Choose(Table.position)
  | Continue
  | Restart;

let initialize = () => {
  let [x, y, z, ...xs] = Deck.make |> Belt.List.shuffle;
  {deck: xs, table: Table.initialize([x, y, z]), gameState: Dealing};
};

let match = (table, x, y, z) =>
  Table.select(table, x, y, z)
  |. Belt.Option.mapWithDefault(false, Card.isMatch);

let doAction = (action, {gameState, deck, table} as game) => {
  let nothing = Ok({deck, table, gameState});
  let error = Error("illegal transition");

  switch (action, gameState, deck) {
  | (_, _, []) => Ok({...game, gameState: Done})

  | (Tick, Dealing, [x, ...xs]) when ! Table.full(table) =>
    Ok({deck: xs, table: Table.add(table, x), gameState: Dealing})

  | (Tick, Dealing, _) when Table.full(table) =>
    Ok({...game, gameState: WaitingForSelection})

  | (Tick, _, _) => nothing

  | (Claim, Dealing, _) when Table.size(table) > 2 =>
    Ok({...game, gameState: WaitingForSelection})

  | (Choose(pos), WaitingForSelection, _) when Table.has(table, pos) =>
    Ok({...game, gameState: OneCardSelected(pos)})

  | (Choose(pos), OneCardSelected(x), _) when Table.has(table, pos) =>
    Ok({...game, gameState: TwoCardsSelected(x, pos)})

  | (Choose(pos), TwoCardsSelected(x, y), _)
      when Table.has(table, pos) && match(table, x, y, pos) =>
    Ok({...game, gameState: Match(x, y, pos)})

  | (Choose(pos), TwoCardsSelected(x, y), _)
      when Table.has(table, pos) && ! match(table, x, y, pos) =>
    Ok({...game, gameState: NoMatch(x, y, pos)})

  | (Choose(pos), _, _) when ! Table.has(table, pos) =>
    Error("invalid card index")

  | (Continue, Match(a, b, c), _) =>
    Ok({
      ...game,
      table: Table.removeMany(table, [|a, b, c|]),
      gameState: Dealing,
    })

  | (Continue, NoMatch(_, _, _), _) => Ok({...game, gameState: Dealing})

  | _ => error
  };
};