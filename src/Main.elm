module Main exposing (main)

import Browser
import Browser.Events as Events exposing (onAnimationFrameDelta)
import Html exposing (Html)
import Html.Attributes exposing (height, style, width)
import Json.Decode as Decode exposing (Value)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Mouse
import WebGL exposing (Mesh, Shader)


type Msg
    = Move Mouse.MouseMove


main : Program Value Mouse.MouseMove Msg
main =
    Browser.element
        { init =
            \_ ->
                ( { clientX = 200
                  , clientY = 200
                  }
                , Cmd.none
                )
        , view = view
        , subscriptions =
            \_ ->
                Sub.batch
                    [ Events.onMouseMove <| Decode.map Move Mouse.decoder
                    ]
        , update = update
        }


update msg model =
    case msg of
        Move mouse ->
            ( mouse, Cmd.none )


view : Mouse.MouseMove -> Html msg
view model =
    WebGL.toHtml
        [ width 600
        , height 600
        , style "display" "block"
        ]
        [ WebGL.entity
            vertexShader
            fragmentShader
            (mesh (toFloat model.clientX / 50 |> (+) -6) (toFloat model.clientY / -50 |> (+) 6))
            { perspective = perspective }
        ]


perspective : Mat4
perspective =
    Mat4.mul
        (Mat4.makePerspective 100 1 0.01 100)
        (Mat4.makeLookAt (vec3 0 0 6) (vec3 0 0 0) (vec3 0 1 0))


type alias Vertex =
    { position : Vec3
    , color : Vec3
    }


mesh : Float -> Float -> Mesh Vertex
mesh x y =
    circle x y 0 1 100 (vec3 0.1 0.1 0.1)
        |> (++) (circle x y 0.2 1 100 <| vec3 0.3 0.3 0.3)
        |> (++) (tube x y 0 0.2 1 100 <| vec3 0.15 0.15 0.15)
        |> (++) (circle 1 1 0.2 1 100 <| vec3 1 0 0)
        |> (++) (circle 1 1 0 1 100 <| vec3 1 0 0)
        |> (++) (tube 1 1 0 0.2 1 100 <| vec3 0.5 0 0)
        |> WebGL.triangles


rect x y w h =
    WebGL.triangles
        [ ( Vertex (vec3 x (y + h) 0) (vec3 1 0 0)
          , Vertex (vec3 (x + w) (y + h) 0) (vec3 0 1 0)
          , Vertex (vec3 (x + w) y 0) (vec3 0 0 1)
          )
        , ( Vertex (vec3 x (y + h) 0) (vec3 1 0 0)
          , Vertex (vec3 x y 0) (vec3 0 1 0)
          , Vertex (vec3 (x + w) y 0) (vec3 0 0 1)
          )
        ]


getCircleVertex sides side radius x y =
    let
        rads =
            (turns 1 / toFloat sides) * toFloat side
    in
    ( cos rads |> (*) radius |> (+) x
    , sin rads |> (*) radius |> (+) y
    )


tube x y z length radius sides color =
    List.range 1 sides
        |> List.map
            (\side ->
                let
                    ( x2, y2 ) =
                        getCircleVertex sides side radius x y

                    ( x3, y3 ) =
                        getCircleVertex sides (side + 1) radius x y
                in
                [ ( Vertex (vec3 x2 y2 z) color
                  , Vertex (vec3 x3 y3 z) color
                  , Vertex (vec3 x2 y2 <| z + length) color
                  )
                , ( Vertex (vec3 x2 y2 <| z + length) color
                  , Vertex (vec3 x3 y3 <| z + length) color
                  , Vertex (vec3 x3 y3 z) color
                  )
                ]
            )
        |> List.concat


circle x y z radius sides color =
    List.range 1 sides
        |> List.map
            (\side ->
                ( let
                    ( x2, y2 ) =
                        getCircleVertex sides side radius x y
                  in
                  Vertex (vec3 x2 y2 z) color
                , Vertex (vec3 x y z) color
                , let
                    ( x2, y2 ) =
                        getCircleVertex sides (side + 1) radius x y
                  in
                  Vertex (vec3 x2 y2 z) color
                )
            )



-- Shaders


type alias Uniforms =
    { perspective : Mat4 }


vertexShader : Shader Vertex Uniforms { vcolor : Vec3 }
vertexShader =
    [glsl|

        attribute vec3 position;
        attribute vec3 color;
        uniform mat4 perspective;
        varying vec3 vcolor;

        void main () {
            gl_Position = perspective * vec4(position, 1.0);
            vcolor = color;
        }

    |]


fragmentShader : Shader {} Uniforms { vcolor : Vec3 }
fragmentShader =
    [glsl|

        precision mediump float;
        varying vec3 vcolor;

        void main () {
            gl_FragColor = vec4(vcolor, 1.0);
        }

    |]
