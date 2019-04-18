module Vertex exposing (Vertex, fragmentShader, mesh, perspective, vertexShader)

import Board
import Circle exposing (Circle)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Screen
import WebGL exposing (Mesh, Shader)


perspective : Mat4
perspective =
    Mat4.mul
        (Mat4.makePerspective Screen.width 0.75 0.01 (Screen.height * 2))
        (Mat4.makeLookAt (vec3 (Screen.width / 2) (Screen.height / 2) (Screen.width / 3.3)) (vec3 (Screen.width / 2) (Screen.height / 2) 0) (vec3 (Screen.width / 2) -9999 9999))


type alias Vertex =
    { position : Vec3
    , color : Vec3
    }


cylinder x y color =
    let
        darkerColor =
            color / 2

        height =
            Circle.radius / 5
    in
    circle x y 0 Circle.radius 100 (vec3 0.1 0.1 0.1)
        |> (++) (circle x y height Circle.radius 100 <| vec3 color color color)
        |> (++) (tube x y 0 height Circle.radius 100 <| vec3 darkerColor darkerColor darkerColor)


mesh : Circle -> Circle -> Mesh Vertex
mesh striker puck =
    cylinder striker.x striker.y 0.3
        |> (++) (cylinder puck.x puck.y 0.5)
        |> (++) board
        |> WebGL.triangles


board =
    let
        bottomColor =
            vec3 0.95 0.95 0.95

        sideColor =
            vec3 0.9 0.9 0.9
    in
    [ ( Vertex (vec3 Board.left Board.top 0) bottomColor
      , Vertex (vec3 Board.left Board.bottom 0) bottomColor
      , Vertex (vec3 Board.right Board.bottom 0) bottomColor
      )
    , ( Vertex (vec3 Board.right Board.bottom 0) bottomColor
      , Vertex (vec3 Board.left Board.top 0) bottomColor
      , Vertex (vec3 Board.right Board.top 0) bottomColor
      )
    ]


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
