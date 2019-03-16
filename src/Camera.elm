module Camera exposing (perspective)

import Math.Matrix4 as Mat4 exposing (Mat4)


perspective : Mat4
perspective =
    Mat4.mul
        (Mat4.makePerspective 100 1 0.01 100)
        (Mat4.makeLookAt (vec3 0 0 6) (vec3 0 0 0) (vec3 0 1 0))
