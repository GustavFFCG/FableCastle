module Player.State
open Elmish
open Types

let newPlayer = {
    Builder = {
        Name = ""
        STR = 1
        DEX = 1
        CON = 1
        UnusedPoints = 6
    }
    ShowStats = false
}

let init () : Model * Cmd<Msg> =
  newPlayer, []

let update msg (model:Model) : Model * Cmd<Msg> =
    let updateStat s c m =
        match s,c,m.UnusedPoints with
            | _, Increase, 0 -> m 
            | STR, Decrease, _  when m.STR > 1 -> {m with STR = m.STR - 1; UnusedPoints = (m.UnusedPoints+1)}
            | DEX, Decrease, _  when m.DEX > 1 -> {m with DEX = m.DEX - 1; UnusedPoints = (m.UnusedPoints+1)}
            | CON, Decrease, _  when m.CON > 1 -> {m with CON = m.CON - 1; UnusedPoints = (m.UnusedPoints+1)}
            | STR, Increase, _  -> {m with STR = m.STR + 1; UnusedPoints = (m.UnusedPoints-1)}
            | DEX, Increase, _  -> {m with DEX = m.DEX + 1; UnusedPoints = (m.UnusedPoints-1)}
            | CON, Increase, _  -> {m with CON = m.CON + 1; UnusedPoints = (m.UnusedPoints-1)}
            | _ -> m
    match msg with
    | ChangeName s -> {model with Builder = {model.Builder with Name = s.Replace(" ", "") }}, []
    | ShowStats -> {model with ShowStats= true}, []
    | ChangeStat (s,c) -> {model with Builder = updateStat s c model.Builder }, []
    | StartGame p -> model, []

