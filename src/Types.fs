module App.Types

open Global

type Msg =
    | PlayerMsg of Player.Types.Msg
    | GameMsg of Game.Types.Msg

type Model =
    | PlayerBuilderModel of Player.Types.Model
    | GameModel of Game.Types.Model
