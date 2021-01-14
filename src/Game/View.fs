module Game.View

open Fable.Core
open Fable.React
open Fable.React.Props
open Types
open Global.Colors

let root (model: Model) dispatch =
    let squares =
        let legalMoves =
            model.Squares
            |> Map.find model.Player.AtSquare
            |> fun s -> s.LegalMoves
        model.Squares
        |> Map.toList
        |> List.map (fun (k, s) ->
            let onCLick, cursor =
                if legalMoves |> List.exists (fun m -> m = k) then
                    (fun _e -> dispatch (Move k)), "pointer"
                else (fun _e -> ()), "default"
            let style, radius =
                s.Type |> function
                    | Normal -> [Stroke "black"; StrokeWidth 1; Fill sandcolor;Cursor cursor], 7
                    | Special -> [Stroke "black"; StrokeWidth 2; Fill sandcolor;Cursor cursor], 12
            circle [Cx s.X ; Cy s.Y ; R radius; Style style; OnClick onCLick] [])
    let players =
        [model.Player]
        |> List.map (fun p -> [
                let currentSquare =
                    model.Squares
                    |> Map.tryFind p.AtSquare
                    |> Option.defaultWith (fun() -> failwith "Illegal square")
                let x, y = (currentSquare.X,currentSquare.Y)

                circle [Cx x; Cy y; R 4; Style [Stroke "red"; StrokeWidth 1; Fill sandcolor]] []
                text [X (x+5); Y (y-5); Style [Fill "black";PointerEvents "none"]] [str p.Name]
            ])
        |> List.concat
    let cards =
        model.Cards
        |> Map.toList
        |> List.map (fun (key, c) ->
            let x, y =
                model.Squares
                |> Map.tryFind key
                |> Option.defaultWith (fun() -> failwith "Illegal square")
                |> fun square -> square.X, square.Y
            rect [X (x+4); Y (y+2); Rx 1; Ry 1 ;
                    Style [Height 20; Width 13; Fill "linen"; Stroke "black"; StrokeWidth 1;Cursor "pointer"] ;
                    OnClick (fun _e -> dispatch (ShowCard c))
                ][]
            )
    let parkstyle = Style [Fill grasscolor; Stroke bushcolor; StrokeWidth 3; StrokeLinecap "round"]    
    let castlestyle = Style [Fill "slategray"; Stroke "black"; StrokeWidth 1]
    let castle x y =
        let points =
            [
                x, y
                x+12, y
                x+12, y-8
                x+18, y-8
                x+18, y
                x+30, y
                x+30, y-20
                x+32, y-22
                x+32, y-25
                x+24, y-25
                x+24, y-22
                x+6,  y-22
                x+6,  y-25
                x-2,  y-25
                x-2,  y-22
                x, y-20
            ]
            |> List.map (fun (x, y) -> sprintf "%i,%i " x y)
            |> List.fold (+) ""
        polygon [Points points; castlestyle] []
    let drawActiveCard (maybeCard: Card option) =
        let cardImg (c:Card) =
            match c.Type with
                | Item ``Kungens kalsonger`` -> Some "/img/Kalsong.png"
                | Monster (``Mördarsnigel``, _) -> Some "/img/Mordarsnigel.png"
                | Monster (``Marodörmaskros``, _) -> Some "/img/Marodormaskros.jpg"
                | Monster (``Zombiepudel``, _) -> Some "/img/Zombiepudel.png"
                | _ -> None
        maybeCard
        |> Option.map (fun c ->
            div [ClassName "modal is-active"] [
                div [ClassName "modal-background"] []
                div [ClassName "modal-content is-block"] [
                    div [   ClassName "tile is-vertical has-text-centered"
                            //Style [Width "70"; Height "200"; Border "1px solid black"; BackgroundColor tileBackground]
                            OnClick (fun _e -> dispatch (HideCard))
                        ] 
                        [
                            div [ClassName "tile has-text-centered"] [
                                h2 [ClassName "subtitle"] [str (if c.DisplayMode = Unknown then "???" else Card.title c)]
                                (match c.Type with
                                    | Monster (_, Some _d) -> p [] [str "(skadad)"] |> Some
                                    | _ -> None
                                ) |> ofOption
                            ]
                            div [ClassName "tile"] [
                                (cardImg c |> Option.map (fun url -> img [Src url; Style [Height 300; Width 300]]))
                                |> ofOption
                            ]
                            div [ClassName "tile"] [
                                (match c.Type, c.DisplayMode with
                                    | Item _, Available ->
                                        [
                                            button [Class "button"; OnClick (fun _e -> (PickUp c |> dispatch))] [str "Plocka upp"]
                                            button [Class "button"] [str "Lämna"]
                                        ]
                                    | Monster _, Available ->
                                        [
                                            button [Class "button"; OnClick (fun _e -> (AttackMonster c |> dispatch))] [str "Kämpa"]
                                            button [Class "button"; OnClick (fun _e -> (dispatch Flee))] [str "Fly"]
                                        ]
                                    | _ -> []
                                        ) |> ofList
                            ]
                        ]
                ]
            ])
        |> ofOption
    let drawActivePlayer dispatch (player: Player) =
        [
            div [Class "tile is-parent is-vertical"; Style[BackgroundColor tileBackground]] [
                div [Class "tile content"] [
                    h2 [Class "subtitle is-2"] [str player.Name]
                    ul [] [
                        li [] [sprintf "Erfarenhet %i" player.XP |> str]
                        li [] [sprintf "Hälsa %i/%i" player.Health player.Stats.CON|> str]
                        li [] [sprintf "Styrka %i" player.Stats.STR |> str]
                        li [] [sprintf "Skicklighet %i" player.Stats.DEX |> str]
                    ]
                ]
                div [Class "tile content"] [
                    h4 [Class "subtitle is-4"] [str "Kort"]
                    ul [] 
                        (player.Cards
                        |> List.map (fun c->
                            li [] [
                                span [] [str (Card.title c)]
                                a [OnClick (fun _e -> (Drop c |> dispatch))] [str "lämna"]
                            ])
                        |> Seq.ofList)
                ]
            ]
        ]
    let drawMessages =
        List.truncate 6
        >> List.map (fun message ->
            p [][str message]
        )
    div [ClassName "tile is-ancestor"] [
        div [ClassName "tile is-8"] [
            div[Style[BackgroundColor tileBackground]][
                [
                    polygon [Points "70,90 70,510 80,510 270,320 270,280 80,90" ; parkstyle] []
                    polygon [Points "90,70 510,70 510,80 320,270 280,270 90,80" ; parkstyle] []
                    polygon [Points "530,90 530,510 520,510 330,320 330,280 520,90" ; parkstyle] []
                    polygon [Points "90,530 510,530 510,520 320,330 280,330 90,520" ; parkstyle] []
                    line [X1 50; Y1 510; X2 50; Y2 50; Style[Stroke bushcolor; StrokeWidth 2]] []
                    line [X1 90; Y1 550; X2 550; Y2 550; Style[Stroke bushcolor; StrokeWidth 2]] []
                    line [X1 550; Y1 550; X2 550; Y2 90; Style[Stroke bushcolor; StrokeWidth 2]] []
                    line [X1 50; Y1 50; X2 510; Y2 50; Style[Stroke bushcolor; StrokeWidth 2]] []
                    castle 540 50
                ] @ squares @ cards @ players 
                |> svg [Style [Height 600; Width 600; Border "1px solid black"; Fill sandcolor] ]
            ]
        ]
        div [ClassName "tile is-vertical"] [
            div [ClassName "tile is-block"] 
                (drawActivePlayer dispatch model.Player)
            div [ClassName "tile is-block"; Style[BackgroundColor tileBackground]] 
                (drawMessages model.Messages)
            drawActiveCard model.ActiveCard
            div [ClassName "tile"] [
                button [OnClick (fun _e -> (Restart |> dispatch))] [str "Börja om"]
            ]
        ]
    ]
