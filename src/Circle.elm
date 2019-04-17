module Circle exposing (Circle, handlePotentialStrikes, incrementCirclePosition, radius, updateCircleWithMouse, updateCircleWithSpeed)

import Screen


type alias Circle =
    { x : Float
    , y : Float
    , xSpeed : Float
    , ySpeed : Float
    }


radius =
    20


updateCircleWithMouse { clientX, clientY } { x, y, xSpeed, ySpeed } =
    Circle (toFloat clientX) (toFloat -clientY |> (+) Screen.width) xSpeed ySpeed


updateCircleWithSpeed oldCircle newCircle =
    { newCircle | xSpeed = newCircle.x - oldCircle.x, ySpeed = newCircle.y - oldCircle.y }


incrementCirclePosition { x, y, xSpeed, ySpeed } =
    Circle (x + xSpeed) (y + ySpeed) xSpeed ySpeed


circlesCollide : Circle -> Circle -> Bool
circlesCollide c1 c2 =
    let
        xDiff =
            abs c1.x - c2.x

        yDiff =
            abs c1.y - c2.y

        hypot =
            sqrt (xDiff ^ 2 + yDiff ^ 2)
    in
    (hypot - radius - radius) <= 0


handlePotentialStrikes striker puck =
    if circlesCollide striker puck then
        strikePuck puck striker

    else
        puck


strikePuck puck striker =
    let
        angle =
            atan2 (striker.y - puck.y) (striker.x - puck.x)
    in
    { puck
        | xSpeed =
            (cos angle * -(striker.xSpeed + abs puck.xSpeed))
                - (cos angle * abs puck.ySpeed)
        , ySpeed =
            (sin angle * -(striker.ySpeed + abs puck.ySpeed))
                - (sin angle * abs puck.xSpeed)
    }
