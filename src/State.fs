module App.State

open Elmish
open Elmish.Navigation
open Elmish.UrlParser
open Browser
open Global
open Types

let pageParser: Parser<Page->Page,Page> =
    oneOf [
        map About (s "about")
        map Start (s "start")
        map Game (s "game")
    ]

let init result =
    let (model, playerCmd) = Player.State.init ()
    PlayerBuilderModel model, Cmd.map PlayerMsg playerCmd

let update (msg:Msg) (model:Model) =
    console.log (message = "ölkölklö")
    match msg, model with
    | PlayerMsg msg, PlayerBuilderModel model ->
        match msg with
            | Player.Types.StartGame builder ->
                let (gameModel, gameCmd) =  Game.State.Player.fromBuilder builder |> Game.State.init
                GameModel gameModel, Cmd.map GameMsg gameCmd
            | _ ->
                let (playerModel, playerCmd) = Player.State.update msg model
                PlayerBuilderModel playerModel, Cmd.map PlayerMsg playerCmd
    | GameMsg msg, GameModel model ->
        let (gameModel, gameCmd) = Game.State.update msg model
        GameModel gameModel, Cmd.map GameMsg gameCmd
    | _ -> failwith "Message and model don't match"
