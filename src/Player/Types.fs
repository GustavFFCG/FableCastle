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

type Model = PlayerBuilder

type Msg =
    | ChangeName of string
    | ChangeStat of Stat*Change
    | StartGame of PlayerBuilder
