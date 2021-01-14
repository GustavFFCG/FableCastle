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
                let crystalRow = function
                    | n when n < 1 -> []
                    | n ->
                        {1..n}
                        |> Seq.map (fun _ -> img [Src "/img/Crystal.png"])
                        |> List.ofSeq
                (div [Class "tile is-block"; Style[BackgroundColor tileBackground]] [
                    p [] [
                        model.Builder.Name |> sprintf "%s mötte en god fe som sa \"Här är sex magiska kristaller. Var och en kan förbättra din styrka, hälsa eller skicklighet. Använd dem klokt.\"" |> str
                    ]
                    table [] [
                        tr [] [
                            td [][str "Kristaller kvar"]
                            td [] (crystalRow builder.UnusedPoints)
                        ]
                        tr [] [
                            td [][str "Din styrka"]
                            td []
                                [
                                    a [OnClick (fun _e -> ChangeStat (STR,Increase) |> dispatch) ] [str "+"]
                                    a [OnClick (fun _e -> ChangeStat (STR,Decrease) |> dispatch) ] [str "-"]
                                    span [] (crystalRow builder.STR)
                                ]
                        ]
                        tr [] [
                            td [][str "Din skicklighet"]
                            td []
                                [
                                    a [OnClick (fun _e -> ChangeStat (DEX,Increase) |> dispatch) ] [str "+"]
                                    a [OnClick (fun _e -> ChangeStat (DEX,Decrease) |> dispatch) ] [str "-"]
                                    span [] (crystalRow builder.DEX)
                                ]
                        ]
                        tr [] [
                            td [][str "Din hälsa"]
                            td []
                                [
                                    a [OnClick (fun _e -> ChangeStat (CON,Increase) |> dispatch) ] [str "+"]
                                    a [OnClick (fun _e -> ChangeStat (CON,Decrease) |> dispatch) ] [str "-"]
                                    span [] (crystalRow builder.CON)
                                ]
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
