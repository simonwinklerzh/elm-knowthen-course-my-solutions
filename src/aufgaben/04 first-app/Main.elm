module Main exposing (main)

import Browser
import Html
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)


type alias Model =
    { calories : Int
    , input : String
    , parsedInput : Int
    , error : Maybe String
    }


initModel : Model
initModel =
    { calories = 0
    , input = ""
    , parsedInput = 0
    , error = Nothing
    }


type Msg
    = AddCalories Int
    | Input String
    | Clear


update : Msg -> Model -> Model
update msg model =
    case msg of
        AddCalories n ->
            { model | calories = model.calories + n }

        Input val ->
            case String.toInt val of
                Just parsedInput ->
                    { model
                        | parsedInput = parsedInput
                        , input = val
                        , error = Nothing
                    }

                Nothing ->
                    if String.isEmpty val then
                        { model
                            | input = ""
                            , parsedInput = 0
                            , error = Nothing
                        }

                    else
                        { model
                            | error = Just "Bitte geben Sie eine ganze Zahl ein."
                            , input = val
                            , parsedInput = 0
                        }

        Clear ->
            initModel


view : Model -> Html.Html Msg
view model =
    Html.div []
        [ Html.h1
            []
            [ Html.text ("Wert: " ++ String.fromInt model.calories) ]
        , Html.button
            [ type_ "button"
            , onClick (AddCalories 1)
            ]
            [ Html.text "Wert erhöhen" ]
        , Html.button
            [ type_ "button"
            , onClick (AddCalories 5)
            ]
            [ Html.text "Wert um 5 erhöhen" ]
        , Html.button
            [ type_ "button"
            , onClick Clear
            ]
            [ Html.text "Wert zurücksetzen" ]
        , Html.br
            []
            []
        , Html.div
            []
            [ Html.input
                [ placeholder "Inputfeld"
                , value model.input
                , onInput Input
                ]
                []
            , Html.button
                [ type_ "button"
                , onClick (AddCalories model.parsedInput)
                , disabled (model.error /= Nothing)
                ]
                [ Html.text "Um Wert erhöhen" ]
            , Html.p
                []
                [ Html.text (Maybe.withDefault "" model.error) ]
            ]
        ]


main =
    Browser.sandbox { init = initModel, update = update, view = view }
