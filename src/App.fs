module App.View

open Elmish
open Elmish.Navigation
open Elmish.UrlParser
open Fable.Core
open Fable.Core.JsInterop
open Types
open App.State
open Global
open Game

importAll "../sass/main.sass"

open Fable.React
open Fable.React.Props

let menuItem label page currentPage =
    li
      [ ]
      [ a
          [ classList [ "is-active", page = currentPage ]
            Href (toHash page) ]
          [ str label ] ]

let menu currentPage =
  aside
    [ ClassName "menu" ]
    [ p
        [ ClassName "menu-label" ]
        [ str "General" ]
      ul
        [ ClassName "menu-list" ] [
          menuItem "Game" Game currentPage
          menuItem "About" Page.About currentPage ] ]

let root model dispatch =

    let pageHtml = function
        | PlayerBuilderModel model -> Player.View.root model (PlayerMsg >> dispatch)
        | GameModel model -> Game.View.root model (GameMsg >> dispatch)

    section [Class "hero is-fullheight"; Style [BackgroundImage "url('/img/fJii66q-castle-wall-paper.jpg')"; BackgroundSize "cover"]] [
        div [Class "hero-head"][
        ]
        div [Class "hero-body"] [
            div [ ClassName "content" ] [
                pageHtml model
            ]
        ]
        div [Class "hero-foot"][
        ]
    ]

open Elmish.React
open Elmish.Debug
open Elmish.HMR

// App
Program.mkProgram init update root
//|> Program.toNavigable (parseHash pageParser) urlUpdate
#if DEBUG
|> Program.withDebugger
#endif
|> Program.withReactBatched "elmish-app"
|> Program.run
