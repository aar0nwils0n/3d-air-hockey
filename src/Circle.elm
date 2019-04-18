module Circle exposing (Circle, handlePotentialStrikes, incrementCirclePosition, radius, updateCircleWithMouse, updateCircleWithSpeed)

import Board
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


slowDownButDontReverse speed =
    if speed <= 0.5 && speed >= 0 || speed >= -0.5 && speed <= 0 then
        0

    else if speed > 0 then
        speed - 0.5

    else if speed < 0 then
        speed + 0.5

    else
        speed


incrementCirclePosition { x, y, xSpeed, ySpeed } =
    let
        newXspeed =
            (if x >= Board.right - radius || x <= Board.left + radius then
                -xSpeed

             else
                xSpeed
            )
                |> slowDownButDontReverse

        newYSpeed =
            (if y >= Board.top - radius || y <= Board.bottom + radius then
                -ySpeed

             else
                ySpeed
            )
                |> slowDownButDontReverse
    in
    Circle (x + newXspeed)
        (y + newYSpeed)
        newXspeed
        newYSpeed


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
            (cos angle * -(abs striker.xSpeed + abs puck.xSpeed))
                - (cos angle * abs puck.ySpeed)
        , ySpeed =
            (sin angle * -(abs striker.ySpeed + abs puck.ySpeed))
                - (sin angle * abs puck.xSpeed)
    }
