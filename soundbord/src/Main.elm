port module Main exposing (Msg(..), main, subscriptions, view)

import Basics.Extra exposing (uncurry)
import Browser
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (class, controls, href, id, selected, src, style, value)
import Html.Events exposing (on, onClick, onMouseEnter)
import Http
import IntSqrt
import Json.Decode as D exposing (field, string)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as E
import List
import List.Extra exposing (unique)
import RandomColor



-- MAIN


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type State
    = Settings
    | Soundboard
    | Loading


type alias Sound =
    { url : String
    , text : String
    , category : String
    }


type alias Model =
    { soundcolors : List ( Float, Sound )
    , categories : List String
    , state : State
    , active_categories : Dict String (Maybe String)
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model [] [] Loading (Dict.fromList [ ( "Top", Nothing ), ( "Bottom", Nothing ) ])
    , Http.get
        { url = "assets/sounds/39sounds.json"
        , expect = Http.expectJson GotList soundlistDecoder
        }
    )



-- UPDATE


port play : E.Value -> Cmd msg


type Msg
    = Play String
    | GoTo State
    | GotList (Result Http.Error (List Sound))
    | ChangeCategory String String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Play id ->
            ( model, play (E.string id) )

        GotList result ->
            case result of
                Ok soundlist ->
                    let
                        sounds_and_colors =
                            List.map2 Tuple.pair (createList (List.length soundlist) []) soundlist

                        unique_categories =
                            List.Extra.unique <| List.map (\s -> s.category) soundlist
                    in
                    ( { model | soundcolors = sounds_and_colors, categories = unique_categories }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        GoTo navigation ->
            case navigation of
                Settings ->
                    ( { model | state = Settings }, Cmd.none )

                Soundboard ->
                    ( { model | state = Soundboard }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ChangeCategory panel new_category ->
            ( { model | active_categories = model.active_categories |> Dict.update panel (Maybe.map (\_ -> Just new_category)) }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch [ Sub.none ]



-- VIEW


viewSoundBoard : Model -> Browser.Document Msg
viewSoundBoard model =
    let
        column_count =
            columnCount <| List.length model.soundcolors
    in
    { title = "The Soundboard"
    , body =
        [ createMenu
        , div []
            (List.map (createCategoryGrid model column_count) (Dict.toList model.active_categories))
        ]
    }


createCategoryGrid : Model -> Int -> ( String, Maybe String ) -> Html Msg
createCategoryGrid model column_count ( panel, category ) =
    case category of
        Just cat ->
            div [ class "grid-container", id cat, Basics.Extra.uncurry style <| constructGrid column_count ]
                (List.map (toCell panel) (List.filter (\( _, c ) -> String.contains c.category cat) model.soundcolors))

        Nothing ->
            div [ class "grid-container", Basics.Extra.uncurry style <| constructGrid column_count ]
                []


viewSettings : Model -> Browser.Document Msg
viewSettings model =
    { title = "The Soundboard"
    , body =
        [ createMenu
        , div []
            (List.map (createCategoryDropdown model) (Dict.toList model.active_categories))
        ]
    }


createCategoryDropdown : Model -> ( String, Maybe String ) -> Html Msg
createCategoryDropdown model ( panel, maybe_string ) =
    let
        ( category, list ) =
            case maybe_string of
                Just cat ->
                    ( cat, [] )

                Nothing ->
                    ( "Kies een categorie", [ option [ selected True ] [ text "Kies een categorie" ] ] )
    in
    div [ class "grid-container" ]
        [ select [ onChange (ChangeCategory panel) ]
            (List.append
                (List.map
                    (\cat ->
                        option
                            [ value cat
                            , selected (String.contains cat category)
                            ]
                            [ text cat ]
                    )
                    model.categories
                )
                list
            )
        ]


onChange : (String -> msg) -> Html.Attribute msg
onChange tagger =
    on "change" (D.map tagger Html.Events.targetValue)


view : Model -> Browser.Document Msg
view model =
    case model.state of
        Loading ->
            viewSoundBoard model

        Soundboard ->
            viewSoundBoard model

        Settings ->
            viewSettings model


createMenu : Html Msg
createMenu =
    nav [ id "menu" ]
        [ li [ onClick (GoTo Settings), id "settings" ] []
        , li [ onClick (GoTo Soundboard), id "soundboard" ] []
        , li [ onClick (GoTo Soundboard), id "mute" ] []
        ]


columnCount : Int -> Int
columnCount tile_count =
    case IntSqrt.squareRoot tile_count of
        Just column_count ->
            column_count

        Nothing ->
            1


constructGrid : Int -> ( String, String )
constructGrid cellcount =
    ( "grid-template-columns", List.repeat cellcount "auto " |> List.foldl (++) "" )


toCell : String -> ( Float, Sound ) -> Html Msg
toCell category_id ( random, sound ) =
    let
        audio_id =
            category_id ++ sound.text
    in
    div [ class "grid-item", onClick (Play audio_id), style "background-color" (RandomColor.rgbString (RandomColor.hsvToRGB random 0.3 0.99)) ]
        [ audio
            [ id audio_id, src sound.url, controls False ]
            []
        , text sound.category
        ]


soundDecoder : D.Decoder Sound
soundDecoder =
    D.succeed Sound
        |> required "file" string
        |> required "name" string
        |> optional "category" string "no-cat"


soundlistDecoder : D.Decoder (List Sound)
soundlistDecoder =
    field "sounds" (D.list soundDecoder)


createList : Int -> List Float -> List Float
createList remaining list =
    case remaining of
        0 ->
            list

        _ ->
            case List.head list of
                Just r ->
                    let
                        magicnr =
                            r + 0.618033988749895
                    in
                    createList (remaining - 1)
                        (List.append
                            [ if magicnr > 1.0 then
                                magicnr - 1.0

                              else
                                magicnr
                            ]
                            list
                        )

                Nothing ->
                    createList (remaining - 1)
                        [ 0.5 ]
