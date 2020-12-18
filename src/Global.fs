module Global

type Page =
    | Start
    | Game
    | Finish
    | About

let toHash page =
    match page with
    | About -> "#about"
    | Start -> "#start"
    | Game -> "#game"
    | Finish -> "#finish"

module Colors =
    let grasscolor = "lime"
    let bushcolor = "darkgreen"
    let sandcolor = "linen"
    let tileBackground = "rgba(255,255,255,0.5)"

