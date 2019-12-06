import Browser
import Html exposing (..)
import Html.Attributes as A
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Square exposing (squaref)

-- MAIN

main =
  Browser.sandbox { init = init, update = update, view = view }

-- MODEL

type alias Model = 
                    { x: Int
                    , y: Int
                    , z: Int
                    , w: Int
                    }
init : Model
init =
  {x=0,y=0,z=120,w=120}

-- UPDATE
-- re-apply noise

type Msg = Increment | Decrement

update : Msg -> Model -> Model
update msg model = 
  {x=0,y=0,z=120,w=120}

-- VIEW

view : Model -> Html Msg
view model =
    Html.div [A.class "quad", A.style "background-image" "url('data:image/svg+xml,<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 920 200\"><polygon fill=\"rgb(234, 158, 141)\" stroke=\"rgb(234, 158, 141)\" stroke-width=\"2\" points=\"0,200 920,200 920,0 0,0\"/></svg>')", A.style "background-repeat" "no-repeat"] 
    [ 
      Html.h1 [] [Html.text "Thomas Horstink's webpage"]
      , Html.h4 [] [Html.text "random doodles I think about and try to work on"] 
    ]