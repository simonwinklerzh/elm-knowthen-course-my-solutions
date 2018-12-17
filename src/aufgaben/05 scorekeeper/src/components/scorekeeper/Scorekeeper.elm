module Scorekeeper exposing ( scorekeeper )

import Browser
import Html
import Html.Attributes exposing ( class, type_, placeholder, value )
import Html.Events exposing ( onClick, onSubmit, onInput )

-- Model

type alias Model =
    { players : List Player
    , playerName : String
    , playerId : Maybe Int
    , plays : List Play
    }

type alias Player =
    { id : Int
    , name : String
    , points : Int
    }

type alias Play = 
    { id : Int
    , playerId : Int
    , name : String
    , points : Int
    }

init_model : Model
init_model =
    { players = []
    , playerName = ""
    , playerId = Nothing
    , plays = []
    }


-- Update

type Msg 
    = Edit Player
    | Score Player Int
    | Input String
    | Save
    | Cancel
    | DeletePlay Play

update : Msg -> Model -> Model
update msg model =
    case msg of 
        Edit player ->
            { model | playerName = player.name, playerId = Just player.id }
        Score player points ->
            score model player points
        Input name ->
            { model | playerName = name }
        Save ->
            if String.isEmpty model.playerName then
                model
            else 
                save model
        Cancel ->
            { model | playerName = "", playerId = Nothing }
        DeletePlay play_to_delete ->
            { model 
                | plays  =
                    model.plays |> List.filter (\play -> play_to_delete.id /= play.id)
            }

save : Model -> Model
save model =
    case model.playerId of
        Just id ->
            edit model id
        Nothing ->
            add model


edit : Model -> Int -> Model
edit model id =
    let 
        new_players =
            List.map
                (\player -> 
                    if player.id == id then
                        { player | name = model.playerName }
                    else
                        player
                )
                model.players

        new_plays =
            List.map
                (\play ->
                    if play.playerId == id then
                        { play | name = model.playerName }
                    else
                        play
                )
                model.plays
    in
        { model
            | players = new_players
            , plays = new_plays
            , playerId = Nothing
            , playerName = ""
        }



add : Model -> Model
add model =
    let player =
            Player (List.length model.players) model.playerName 0

        new_players = 
            player :: model.players
    in
        { model 
            | players = new_players
            , playerName = ""
        }

score : Model -> Player -> Int -> Model
score model scorer points =
    let new_players =
            List.map
                (\player ->
                    if player.id == scorer.id then
                        { player | points = player.points + points  }
                    else
                        player
                )
                model.players

        new_plays = 
            Play (List.length model.plays) scorer.id scorer.name points :: model.plays
    in
        { model 
            | players = new_players
            , plays = new_plays 
        }

-- View

player_list_header : Html.Html Msg
player_list_header =
    Html.header
        []
        [ Html.div [] [ Html.text "Name" ]
        , Html.div [] [ Html.text "Points" ]
        ]

player_entry : Player -> Html.Html Msg
player_entry player =
    Html.li
        []
        [ Html.i
            [ class "edit"
            , onClick ( Edit player ) 
            ]
            []
        , Html.div
            []
            [ Html.text player.name ]
        , Html.button
            [ type_ "button" 
            , onClick ( Score player 2 )
            ]
            [ Html.text "2 pt" ]
        , Html.button
            [ type_ "button"
            , onClick ( Score player 3 )
            ]
            [ Html.text "3pt" ]
        , Html.div
            []
            [ Html.text ( String.fromInt player.points ) ]
        ]

player_list : Model -> Html.Html Msg
player_list model = 
    model.players
        |> List.sortBy .name
        |> List.map player_entry
        |> Html.ul []

points_total : Model -> Html.Html Msg
points_total model =
    let
        total = List.map .points model.players
            |> List.sum
    in
        Html.footer
            []
            [ Html.div
                []
                [ Html.text "Total:" ]
            , Html.div
                []
                [ Html.text ( String.fromInt total ) ]
            ]

player_section : Model -> Html.Html Msg
player_section model =
    Html.div
        []
        [ player_list_header
        , player_list model
        , points_total model
        ]

player_form : Model -> Html.Html Msg
player_form model =
    Html.form
        [ onSubmit Save ]
        [ Html.input 
            [ onInput Input
            , value model.playerName
            , type_ "Text"
            , placeholder "Add/Edit Player"
            ]
            []
        , Html.button
            [ type_ "submit" ]
            [ Html.text "Save" ]
        , Html.button
            [ type_ "submit"
            , onClick Cancel 
            ]
            [ Html.text "Cancel" ]
        ]

play_list_header : Html.Html Msg
play_list_header = 
    Html.header
        []
        [ Html.div [] [ Html.text "Plays" ]
        , Html.div [] [ Html.text "Points" ]
        ]

play_list_entry : Play -> Html.Html Msg
play_list_entry play =
    Html.li
        []
        [ Html.i 
            [ class "remove"
            , onClick (DeletePlay play)
            ]
            []
        , Html.div
            []
            [ Html.text play.name ]
        , Html.div
            []
            [ Html.text (String.fromInt play.points) ]
        ]

play_list : Model -> Html.Html Msg
play_list model =
    model.plays
        |> List.map play_list_entry
        |> Html.ul []


play_section : Model -> Html.Html Msg
play_section model =
    Html.div
        []
        [ play_list_header
        , play_list model
        ]



view : Model -> Html.Html Msg
view model =
    Html.div
        [ class "scoreboard" ]
        [ Html.h1
            [ ]
            [ Html.text "Scoreboard" ]
        , player_section model
        , player_form model
        , play_section model
        ]

scorekeeper =
    Browser.sandbox { init = init_model, update = update, view = view }
