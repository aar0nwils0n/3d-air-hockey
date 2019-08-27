module Main exposing (main)

import Browser
import Browser.Events as Events exposing (onAnimationFrameDelta)
import Circle exposing (Circle)
import Html exposing (Html, div, text)
import Html.Attributes exposing (height, style, width)
import Json.Decode as Decode exposing (Value)
import Mouse
import Screen
import Time
import Vertex
import WebGL exposing (Mesh)


type Msg
    = Move Mouse.MouseMove
    | Tick Time.Posix


initPuck =
    Circle (Screen.width / 2 - Circle.radius) (Screen.height / 2 - Circle.radius) 0 0


main : Program Value Model Msg
main =
    Browser.element
        { init =
            \_ ->
                ( Model
                    (Circle 0 0 0 0)
                    initPuck
                    (Circle (Screen.width / 2 - Circle.radius) (Screen.height / 1.5 - Circle.radius) 0 0)
                    (Mouse.MouseMove 0 0)
                    0
                    0
                , Cmd.none
                )
        , view = view
        , subscriptions =
            \_ ->
                Sub.batch
                    [ Events.onMouseMove <| Decode.map Move Mouse.decoder
                    , Time.every (1000 / 30) Tick
                    ]
        , update = update
        }


type alias Model =
    { striker : Circle
    , puck : Circle
    , bot : Circle
    , mouse : Mouse.MouseMove
    , botScore : Int
    , playerScore : Int
    }


update msg model =
    case msg of
        Move mouse ->
            ( { model | mouse = mouse }, Cmd.none )

        Tick _ ->
            let
                newStriker =
                    Circle.updateCircleWithSpeed model.striker (Circle.updateCircleWithMouse model.mouse model.striker)

                newBot =
                    Circle.updateCircleWithSpeed model.bot (Circle.updateBot model.bot model.puck)

                newPuck =
                    model.puck
                        |> Circle.handlePotentialStrikes newStriker
                        |> Circle.handlePotentialStrikes newBot
                        |> Circle.incrementCirclePosition

                ( playerIncrease, botIncrease ) =
                    Circle.checkForPuckAndScoreIncrease model.puck
            in
            ( { model
                | striker = newStriker
                , puck =
                    if playerIncrease || botIncrease then
                        initPuck

                    else
                        newPuck
                , bot = newBot
                , botScore =
                    if botIncrease then
                        model.botScore + 1

                    else
                        model.botScore
                , playerScore =
                    if playerIncrease then
                        model.playerScore + 1

                    else
                        model.playerScore
              }
            , Cmd.none
            )


view : Model -> Html msg
view { striker, puck, bot, botScore, playerScore } =
    div
        [ style "background" "black"
        , style "margin" "0"
        , style "padding" "10px"
        , style "color" "white"
        , style "position" "fixed"
        , style "top" "0"
        , style "left" "0"
        , style "right" "0"
        , style "bottom" "0"
        , style "font-family" "sans-serif"
        , style "text-align" "center"
        , style "display" "flex"
        , style "justify-content" "space-around"
        , style "cursor" "none"
        ]
        [ div []
            [ text <| "You: " ++ String.fromInt botScore ++ " Bot: " ++ String.fromInt playerScore
            , WebGL.toHtml
                [ width Screen.width
                , height Screen.height
                , style "display" "block"
                ]
                [ WebGL.entity
                    Vertex.vertexShader
                    Vertex.fragmentShader
                    (Vertex.mesh striker puck bot)
                    { perspective = Vertex.perspective }
                ]
            ]
        ]
