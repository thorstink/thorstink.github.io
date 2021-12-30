module RandomColor exposing (..)


hsvToRGB : Float -> Float -> Float -> ( Int, Int, Int )
hsvToRGB h s v =
    let
        h_i =
            floor
                (h * 6)

        f =
            h * 6 - toFloat h_i

        p =
            v * (1 - s)

        q =
            v * (1 - f * s)

        t =
            v * (1 - (1 - f) * s)

        ( r, g, b ) =
            case h_i of
                0 ->
                    ( v, t, p )

                1 ->
                    ( q, v, p )

                2 ->
                    ( p, v, t )

                3 ->
                    ( p, q, v )

                4 ->
                    ( t, p, v )

                _ ->
                    ( v, p, q )
    in
    ( round (256 * r), round (256 * g), round (256 * b) )


rgbString : ( Int, Int, Int ) -> String
rgbString ( r, g, b ) =
    "rgb(" ++ String.fromInt r ++ "," ++ String.fromInt g ++ "," ++ String.fromInt b ++ ")"
