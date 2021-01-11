module Player.View
open Fable.Core
open Fable.React
open Fable.React.Props
open Types
open Global.Colors

let root (model: Model) dispatch =
    let builder = model.Builder
    let statTile =
         (match model.ShowStats with
            | false -> None
            | true ->
                (div [Class "tile is-block"; Style[BackgroundColor tileBackground]] [
                    p [] [
                        model.Builder.Name |> sprintf "%s mötte en god fe som sa \"Här är sex magiska kristaller. Var och en kan ge dig styrka, hälsa eller skicklighet. Använd dem klokt.\"" |> str
                    ]
                    table [] [
                        tr [] [
                            td [][str "Kristaller kvar"]
                            td [][str (builder.UnusedPoints.ToString())]
                            td [][]
                            td [][]
                        ]
                        tr [] [
                            td [][str "Din styrka"]
                            td [][str (builder.STR.ToString())]
                            td [][a [OnClick (fun _e -> ChangeStat (STR,Increase) |> dispatch) ] [str "+"]]
                            td [][a [OnClick (fun _e -> ChangeStat (STR,Decrease) |> dispatch) ] [str "-"]]
                        ]
                        tr [] [
                            td [][str "Din skicklighet"]
                            td [][str (builder.DEX.ToString())]
                            td [][a [OnClick (fun _e -> ChangeStat (DEX,Increase) |> dispatch) ] [str "+"]]
                            td [][a [OnClick (fun _e -> ChangeStat (DEX,Decrease) |> dispatch) ] [str "-"]]
                        ]
                        tr [] [
                            td [][str "Din hälsa"]
                            td [][str (builder.CON.ToString())]
                            td [][a [OnClick (fun _e -> ChangeStat (CON,Increase) |> dispatch) ] [str "+"]]
                            td [][a [OnClick (fun _e -> ChangeStat (CON,Decrease) |> dispatch) ] [str "-"]]
                        ]
                    ]
                ]) |> Some
                ) |> ofOption
    div [Class "tile is-ancestor is-vertical"] [
        div [Class "tile"; Style[BackgroundColor tileBackground]] [
            p [] [
                str "I sagornas tid, i ett land långt borta fanns en tapper äventyrare vid namn "
                input [Value builder.Name;OnChange (fun e -> (ChangeName e.Value |> dispatch ))]
                button [Disabled (builder.Name = ""); OnClick (fun _e -> ShowStats |> dispatch )] [str "Vidare..."]
            ]
        ]
        (statTile)
        (if builder.UnusedPoints = 0 then
            (div [Class "tile"; Style[BackgroundColor tileBackground]] [
                button [OnClick (fun _e -> StartGame builder |> dispatch )] [str "Starta äventyret"]]) |> Some
        else
            None
        ) |> ofOption
    ]
