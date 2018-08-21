open Helpers;

let dealingInterval = 5 * 1000;

let component = ReasonReact.reducerComponent("App");

type action = Game.action;

type state = Game.t;

let reducer = (action, state) => {
  Belt.Result.mapWithDefault(
    Game.doAction(action, state), ReasonReact.NoUpdate, a =>
    ReasonReact.Update(a)
  );
};

let initialState = () => Game.initialize();

let buttonClick = ({ ReasonReact.state: { Game.gameState, Game.table } } as self) => (_event) => {
  switch(gameState) {
  | Match(_, _, _) | NoMatch(_, _, _) => self.ReasonReact.send(Game.Continue)
  | Done => self.ReasonReact.send(Game.Restart)
  | Dealing | WaitingForSelection when Table.isReady(table) => self.send(Game.Claim)
  | _ => noOp()
  }
}

let cardClick = ({ ReasonReact.state: { Game.gameState } } as self, pos) => (_event) => {
  switch(gameState) {
  | Dealing | WaitingForSelection => self.ReasonReact.send(Game.Choose(pos))
  | OneCardSelected(x) when x !== pos => self.ReasonReact.send(Game.Choose(pos))
  | TwoCardsSelected(x, y) when x !== pos && y !== pos => self.ReasonReact.send(Game.Choose(pos))
  | _ => noOp()
  };
}

let renderButton = ({ ReasonReact.state: { Game.gameState, Game.table } } as self) => {
  let onClick = buttonClick(self);  
  switch(gameState) {
  | Match(_, _, _) | NoMatch(_, _, _) => 
      <button onClick>(str("Continue"))</button>

  | Done => 
      <button onClick>(str("New Game"))</button>

  | Dealing | WaitingForSelection when Table.isReady(table) => 
      <button onClick>(str("Set!"))</button>
      
  | _ => ReasonReact.null
  };
};

let renderCard = ({ ReasonReact.state: { Game.gameState } } as self, card, pos) => {
  let selected = switch(gameState) {
  | OneCardSelected(x) => x == pos
  | TwoCardsSelected(x, y) => x == pos || y == pos
  | _ => false
  };
  <CardView card selected key=(string_of_int(pos)) onClick=(cardClick(self, pos))/>
};

let renderMessage = ({ Game.gameState }) => {
  switch (gameState) {
  | Match(_, _, _) => str("Set!")
  | NoMatch(_, _, _) => str("Sorry, but that's not a set.")
  | OneCardSelected(_) | TwoCardsSelected(_, _) => str("Select another card")
  | Done => str("Game Over")
  | _ => ReasonReact.null
  };
};

let make = _children => {
  ...component,
  didMount: self => {
    let intervalId = Js.Global.setInterval(() => self.send(Game.Tick), dealingInterval);
    self.onUnmount(() => Js.Global.clearInterval(intervalId));
  },  
  initialState,
  reducer,
  render: ({ state: { Game.table } as state } as self) => {
    <div className="app">
      <div className="button">(renderButton(self))</div>
        <div className="grid">
          (Belt.Map.Int.reduce(table, [||], (acc, k, v) => Array.append(acc, [|renderCard(self, v, k)|])) |> ReasonReact.array)
        </div>
      <div className="message">(renderMessage(state))</div>
    </div>
  },
};