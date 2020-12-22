module Player.Types

type Stat =
    | STR
    | DEX
    | CON
type Change =
    | Increase
    | Decrease

type PlayerBuilder = {
    Name : string
    STR: int
    DEX: int
    CON: int
    UnusedPoints: int
}

type Model = {
    Builder:PlayerBuilder
    ShowStats: bool
}

type Msg =
    | ChangeName of string
    | ChangeStat of Stat*Change
    | ShowStats
    | StartGame of PlayerBuilder
