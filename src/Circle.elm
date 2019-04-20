module Circle exposing (Circle, handlePotentialStrikes, incrementCirclePosition, radius, slowDown, updateCircleWithMouse, updateCircleWithSpeed)

import Board
import Screen


type alias Circle =
    { x : Float
    , y : Float
    , xSpeed : Float
    , ySpeed : Float
    }


radius =
    15


updateCircleWithMouse { clientX, clientY } { x, y, xSpeed, ySpeed } =
    let
        clientXFloat =
            toFloat clientX

        newX =
            if clientXFloat < Board.left + radius then
                Board.left + radius

            else if clientXFloat > Board.right - radius then
                Board.right - radius

            else
                clientXFloat

        clientYFloat =
            toFloat -clientY |> (+) Screen.width

        newY =
            if clientYFloat < Board.bottom + radius then
                Board.bottom + radius

            else if clientYFloat > Board.top - radius then
                Board.top - radius

            else
                clientYFloat
    in
    Circle newX newY xSpeed ySpeed


updateCircleWithSpeed oldCircle newCircle =
    { newCircle | xSpeed = newCircle.x - oldCircle.x, ySpeed = newCircle.y - oldCircle.y }


slowDownButDontReverse speed =
    if speed <= 0.25 && speed >= 0 || speed >= -0.25 && speed <= 0 then
        0

    else if speed > 0 then
        speed - 0.25

    else if speed < 0 then
        speed + 0.25

    else
        speed


pythag a b =
    sqrt (abs a ^ 2 + abs b ^ 2)


nanTo0 x =
    if isNaN x then
        0

    else
        x


slowDown xSpeed ySpeed =
    let
        hypot =
            pythag xSpeed ySpeed

        hypot2 =
            slowDownButDontReverse hypot

        angle =
            atan (xSpeed / ySpeed)

        newXSpeed =
            (abs <| sin angle)
                * hypot2
                * (if xSpeed < 0 then
                    -1

                   else
                    1
                  )

        newYSpeed =
            (abs <| cos angle)
                * hypot2
                * (if ySpeed < 0 then
                    -1

                   else
                    1
                  )
    in
    ( nanTo0 newXSpeed, nanTo0 newYSpeed )


incrementCirclePosition { x, y, xSpeed, ySpeed } =
    let
        ( newXSpeed, newYSpeed ) =
            slowDown xSpeed ySpeed

        incrementedX =
            x + xSpeed

        incrementedY =
            y + ySpeed

        newXSpeed2 =
            if incrementedX >= Board.right - radius || incrementedX <= Board.left + radius then
                -newXSpeed

            else
                newXSpeed

        newYSpeed2 =
            if incrementedY >= Board.top - radius || incrementedY <= Board.bottom + radius then
                -newYSpeed

            else
                newYSpeed

        increasedX =
            x + newXSpeed

        newX =
            if increasedX > Board.right - radius then
                Board.right - radius

            else if increasedX < Board.left + radius then
                Board.left + radius

            else
                increasedX

        increasedY =
            y + newYSpeed

        newY =
            if increasedY > Board.top - radius then
                Board.top - radius

            else if increasedY < Board.bottom + radius then
                Board.bottom + radius

            else
                increasedY
    in
    Circle (newX + newXSpeed2)
        (newY + newYSpeed2)
        newXSpeed2
        newYSpeed2


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

        totalStrikerSpeed =
            sqrt (abs striker.xSpeed ^ 2 + abs striker.ySpeed ^ 2)

        -- todo get total speed on the vecor rather than splitting x and y
    in
    { puck
        | xSpeed =
            clamp (Screen.width * -0.025)
                (Screen.width * 0.025)
            <|
                (cos angle * -(totalStrikerSpeed + abs puck.xSpeed))
                    - (cos angle * abs puck.ySpeed)
        , ySpeed =
            clamp (Screen.width * -0.025)
                (Screen.width * 0.025)
            <|
                (sin angle * -(totalStrikerSpeed + abs puck.ySpeed))
                    - (sin angle * abs puck.xSpeed)
    }
