module Game.Types

type SquareKey = string

type SquareType =
    |Normal
    |Special

type Square = {
    LegalMoves : SquareKey list
    X: int
    Y: int
    Type: SquareType
}

type MonsterType =
    | ``Zombiepudel``
    | ``Mördarsnigel``
    | ``Jättemaskros``
    | ``Knivbladsträd``
    | ``Vaktzombie``

type ItemType =
    | ``Brev från Kungen``
    | ``Kungens kalsonger``
    | ``Äppelskrutt``

type Damage = int
type CardType =
    | Monster of MonsterType*Damage option
    | Item of ItemType

type Stats = {
    STR: int
    DEX: int
    CON: int
    AC: int
}

type Card = {
    Type: CardType
    FaceUp: bool
}
module Card =
    let newItem itemType = {
        Type = Item itemType
        FaceUp = false
    }
    let newMonster monsterType = {
        Type = Monster (monsterType, None)
        FaceUp = false
    }
        
    let title (c: Card) =
        c.Type |> function
            | Monster (``Zombiepudel``, _) -> "Zombiepudel"
            | Monster (``Mördarsnigel``, _) -> "Mördarsnigel"
            | Monster (``Jättemaskros``, _) -> "Jättemaskros"
            | Monster (``Knivbladsträd``, _) -> "Knivbladsträd"
            | Monster (``Vaktzombie``, _) -> "Vaktzombie"
            | Item ``Brev från Kungen`` -> "Brev från kungen"
            | Item ``Kungens kalsonger`` -> "Kungens kalsonger"
            | Item ``Äppelskrutt`` -> "Ett äppelskrutt"


module MonsterType =
    let xpAward  =  function
        | ``Zombiepudel``   -> 2
        | ``Mördarsnigel``  -> 1
        | ``Jättemaskros``  -> 2
        | ``Knivbladsträd`` -> 2
        | ``Vaktzombie``    -> 3

    let stats = function
        | ``Zombiepudel``   -> {STR = 2; DEX = 3; CON = 3; AC = 2}
        | ``Mördarsnigel``  -> {STR = 1; DEX = 3; CON = 3; AC = 1}
        | ``Jättemaskros``  -> {STR = 1; DEX = 3; CON = 8; AC = 1}
        | ``Knivbladsträd`` -> {STR = 3; DEX = 3; CON = 3; AC = 2}
        | ``Vaktzombie``    -> {STR = 3; DEX = 3; CON = 6; AC = 3}


type Player = {
    Name : string
    XP: int
    Stats: Stats
    Health: int
    AtSquare: SquareKey
    LastSquare: SquareKey
    Cards: Card list
}

type Model = {
    Player : Player// list
    Squares : Map<SquareKey, Square>
    Cards: Map<SquareKey,Card>
    ActiveCard: Card option
    Messages: string list
}

type Msg =
    | NewGame of Player
    | Restart
    | Move of SquareKey
    | RevealCard of SquareKey
    | ShowCard of Card
    | HideCard
    | AttackMonster of Card
    | MonsterAttacks of Card
    | Flee
    | PickUp of Card
    | Drop of Card
