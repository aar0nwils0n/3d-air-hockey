module Main exposing (main)

import Browser
import Browser.Events as Events exposing (onAnimationFrameDelta)
import Circle exposing (Circle)
import Html exposing (Html)
import Html.Attributes exposing (height, style, width)
import Json.Decode as Decode exposing (Value)
import Mouse
import Time
import Vertex
import WebGL exposing (Mesh)


type Msg
    = Move Mouse.MouseMove
    | Tick Time.Posix


main : Program Value Model Msg
main =
    Browser.element
        { init =
            \_ ->
                ( Model (Circle 0 0 0 0) (Circle 100 200 0 0) (Mouse.MouseMove 0 0)
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
    , mouse : Mouse.MouseMove
    }


update msg model =
    let
        _ =
            Debug.log "" model
    in
    case msg of
        Move mouse ->
            ( { model | mouse = mouse }, Cmd.none )

        Tick _ ->
            let
                newStriker =
                    Circle.updateCircleWithSpeed model.striker (Circle.updateCircleWithMouse model.mouse model.striker)

                newPuck =
                    Circle.handlePotentialStrikes newStriker model.puck |> Circle.incrementCirclePosition
            in
            ( { model
                | striker = newStriker
                , puck = newPuck
              }
            , Cmd.none
            )


view : Model -> Html msg
view { striker, puck } =
    WebGL.toHtml
        [ width 600
        , height 600
        , style "display" "block"
        ]
        [ WebGL.entity
            Vertex.vertexShader
            Vertex.fragmentShader
            (Vertex.mesh (Circle.transformToMagicRatio striker) (Circle.transformToMagicRatio puck))
            { perspective = Vertex.perspective }
        ]
