module Game.State

open Elmish
open Types
open Types

let keyInline s i = (sprintf "%s%i" s i)
let legalmovesInline s i = [keyInline s (i-1);keyInline s (i+1)]
let squareSeq (sq: int seq) s (posFun: (int->int*int)) =
    sq |> Seq.map (fun i ->
            let x, y = posFun i
            (keyInline s i),
            {LegalMoves = legalmovesInline s i ; X=x ; Y=y; Type = Normal})
let squaresOnPath =
    (squareSeq {2..10} "A" (fun i ->60, (540-i*40))|> List.ofSeq)
    @ (squareSeq {13..21} "A" (fun i ->(90+(i-12)*40),60) |> List.ofSeq)
    @ (squareSeq {2..10} "B" (fun i ->(60+i*40), 540) |> List.ofSeq)
    @ (squareSeq {13..21} "B" (fun i ->540, (500-(i-12)*40)) |> List.ofSeq)
    @ (squareSeq {2..9} "C" (fun i ->(70+i*20), (530-i*20)) |> List.ofSeq)
    @ (squareSeq {12..19} "C" (fun i ->(330+(i-11)*20), (270-(i-11)*20)) |> List.ofSeq)
    @ (squareSeq {2..9} "D" (fun i ->(70+i*20), (70+i*20)) |> List.ofSeq)
    @ (squareSeq {12..19} "D" (fun i ->(330+(i-11)*20), (330+(i-11)*20)) |> List.ofSeq)
    
let squares = 
    [
        "START", {LegalMoves = ["A1";"B1";"C1"]     ; X=70;   Y=530; Type = Special}
        "A1", {LegalMoves = ["START";"A2"]          ; X=60;   Y=500; Type = Normal}
        "A11", {LegalMoves = ["ACORNER";"A10"]      ; X=60;   Y=100; Type = Normal}
        "A12", {LegalMoves = ["ACORNER";"A13"]      ; X=100;  Y=60;  Type = Normal}
        "A22", {LegalMoves = ["ENTRANCE";"A21"]     ; X=500;  Y=60;  Type = Normal}
        "B1", {LegalMoves = ["START";"B2"]          ; X=100;  Y=540; Type = Normal}
        "B11", {LegalMoves = ["BCORNER";"B10"]      ; X=500;  Y=540; Type = Normal}
        "B12", {LegalMoves = ["BCORNER";"B13"]      ; X=540;  Y=500; Type = Normal}
        "B22", {LegalMoves = ["ENTRANCE";"B21"]     ; X=540;  Y=100; Type = Normal}
        "C1", {LegalMoves = ["START";"C2"]          ; X=90 ;  Y=510; Type = Normal}
        "C10", {LegalMoves = ["PARKCENTER";"C9"]    ; X=270 ; Y=330; Type = Normal}
        "C11", {LegalMoves = ["PARKCENTER";"C12"]   ; X=330 ; Y=270; Type = Normal}
        "C20", {LegalMoves = ["ENTRANCE";"C19"]     ; X=510 ; Y=90;  Type = Normal}
        "D1", {LegalMoves = ["ACORNER";"D2"]        ; X=90  ; Y=90;  Type = Normal}
        "D10", {LegalMoves = ["PARKCENTER";"D9"]    ; X=270 ; Y=270; Type = Normal}
        "D11", {LegalMoves = ["PARKCENTER";"D12"]   ; X=330 ; Y=330; Type = Normal}
        "D20", {LegalMoves = ["BCORNER";"D92"]      ; X=510 ; Y=510; Type = Normal}
        "PARKCENTER", {LegalMoves = ["C10";"C11";"D10";"D11"]; X=300; Y=300; Type = Special}
        "ACORNER", {LegalMoves = ["A11";"A12"]; X=70; Y=70; Type = Special}
        "BCORNER", {LegalMoves = ["B11";"B12"]; X=530; Y=530; Type = Special}
        "ENTRANCE", {LegalMoves = ["A22";"B22";"C20"]; X=530; Y=70; Type = Special}
    ] @ squaresOnPath|> Map.ofList

module Player =
    let fromBuilder (b:Player.Types.PlayerBuilder) = {
        Name = b.Name
        XP = 0
        Stats = {
            STR = b.STR
            DEX = b.DEX
            CON = b.CON
            AC = 0
        }
        Health = b.CON
        AtSquare = "START"
        LastSquare ="START"
        Cards = []
    }


let freshGame player = {
    Player = player
    Squares = squares
    Cards = [
        "B4", Card.newItem ``Äppelskrutt``
        "A5", Card.newItem ``Kungens kalsonger``
        "C3", Card.newItem ``Brev från Kungen``
        "A7", Card.newMonster ``Zombiepudel``
        "A15", Card.newMonster ``Knivbladsträd``
        "PARKCENTER", Card.newMonster ``Jättemaskros``
        "B6", Card.newMonster ``Mördarsnigel``
        "ENTRANCE", Card.newMonster ``Vaktzombie``
        ] |> Map.ofList
    ActiveCard = None
    Messages = ["Du står vid ingången till slottsträdgården"]
}

let init player : Model * Cmd<Msg> =
  freshGame player, []

let flee model =
    let players = {
        model.Player with
            AtSquare = model.Player.LastSquare
            LastSquare=model.Player.AtSquare
        }
    {model with
        ActiveCard = None
        Player=players
        }, []
let fightMonster (card: Card) (model: Model) = 
    match card.Type with
        | Item _ -> model, []
        | Monster (m, d) ->
            let damageDone = model.Player.Stats.STR + (Option.defaultValue 0 d)
            let monsterStats = MonsterType.stats m
            if damageDone >= monsterStats.CON
            then
                let messages = (Card.title card |> sprintf "Du har besegrat %s!") :: model.Messages
                let player = {model.Player with XP = model.Player.XP + (MonsterType.xpAward m)}
                let cards = model.Cards |> Map.remove model.Player.AtSquare
                {model with
                    Player = player
                    Cards=cards
                    ActiveCard= None
                    Messages = messages
                    }, []
            else
                let messages = (Card.title card |> sprintf "Du har skadat %s!") :: model.Messages
                let card = {card with Type = Monster (m, Some damageDone)}
                let cards = model.Cards |> Map.add model.Player.AtSquare card
                {model with
                    Cards=cards
                    ActiveCard= Some card
                    Messages = messages
                    }, Cmd.ofMsg (MonsterAttacks card)

let monsterFightsYou (card: Card) (model: Model) = 
    match card.Type with
        | Item _ -> model, []
        | Monster (m, d) ->
            let monsterStats = MonsterType.stats m
            let damageDone = monsterStats.STR
            let playerAfterFight = {model.Player with Health = model.Player.Health - damageDone}
            if playerAfterFight.Health <= 0
            then
                let messages = (Card.title card |> sprintf "%s besegrade dig!") :: model.Messages
                let player = {playerAfterFight with AtSquare="START"}
                {model with
                    Player = player
                    Messages = messages
                    }, []
            else
                let messages = (Card.title card |> sprintf "%s skadade dig!") :: model.Messages
                let player = {playerAfterFight with AtSquare = model.Player.LastSquare}
                {model with
                    Player = player
                    Messages = messages
                    }, []


let pickUpCard card model =
    let players =
        {model.Player with Cards = card :: model.Player.Cards}
    let cards = model.Cards.Remove model.Player.AtSquare
    let messages = (Card.title card |> sprintf "Du plockar upp %s") :: model.Messages
    ({model with
        ActiveCard = Some card
        Cards = cards
        Player=players
        Messages= messages
        }, [])

let dropCard card model =
    let players =
        {model.Player with Cards = List.except[card] model.Player.Cards}
    let cards = model.Cards.Add (model.Player.AtSquare, card)
    ({model with
        ActiveCard = Some card
        Cards = cards
        Player=players}, [])

let update msg (model:Model) : Model * Cmd<Msg> =
    match msg with
    | NewGame player -> init player
    | Move key ->
        let currentSquare =
            model.Squares
            |> Map.find model.Player.AtSquare
        if List.exists (fun k -> k = key) currentSquare.LegalMoves
        then ({model with Player= {model.Player with AtSquare = key; LastSquare=model.Player.AtSquare}}, Cmd.ofMsg (RevealCard key))
        else (model, [])
    | RevealCard key ->
        model.Cards
        |> Map.tryFind key
        |> function
            | Some card ->
                let revealedCard = {card with DisplayMode=Available}
                ({model with ActiveCard = Some revealedCard; Cards=model.Cards |> Map.add key revealedCard }, [])
            | None -> ({model with ActiveCard = None}, [])
    | ShowCard c -> ({model with ActiveCard = Some c}, [])
    | HideCard -> ({model with ActiveCard = None}, [])
    | AttackMonster c -> fightMonster c model
    | MonsterAttacks c -> monsterFightsYou c model
    | Flee -> flee model
    | PickUp c -> pickUpCard c model
    | Drop c -> dropCard c model
    | Restart -> init {model.Player with Cards = []; AtSquare="START"; LastSquare="START";Health=model.Player.Stats.CON}
