let component = ReasonReact.statelessComponent("CardView");

let make = (~card, ~selected=false, ~onClick, _children) => {
  ...component,
  render: _self =>
    <img
      className=("card-image" ++ (selected ? " selected" : ""))
      src=(Card.imageUrl(card))
      onClick
    />,
};