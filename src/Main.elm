module Main exposing (Msg(..))

import Browser
import Data
    exposing
        ( Color(..)
        , Field
        , Figure
        , Kind(..)
        , Move(..)
        , Position
        , allowedMoves2
        , colorToString
        , fieldColor
        , init
        , isCheck
        , isCheckmate
        , opposite
        , performMove
        , toSymbol
        )
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (disabled, style)
import Html.Events exposing (onClick)


type Msg
    = Select Position
    | Move Position
    | Reset


type alias Model =
    { field : Field
    , selected : Maybe Position
    , player : Color
    }


init : Model
init =
    { field = Data.init, selected = Nothing, player = White }


update : Msg -> Model -> Model
update msg model =
    case msg of
        Reset ->
            init

        Select pos ->
            { model | selected = Just pos }

        Move to ->
            case model.selected of
                Nothing ->
                    model

                Just from ->
                    { model
                        | selected = Nothing
                        , player = opposite model.player
                        , field = performMove (Single ( from, to )) model.field
                    }


isSelected : Model -> Position -> Bool
isSelected model pos =
    Maybe.withDefault False (Maybe.map ((==) pos) model.selected)


fieldStyle : Position -> List (Attribute msg)
fieldStyle position =
    let
        col =
            case fieldColor position of
                Black ->
                    "grey"

                White ->
                    "white"
    in
    [ style "height" "50px"
    , style "width" "50px"
    , style "background-color" col
    , style "text-align" "center"
    , style
        "vertical-align"
        "middle"
    , style "font-size" "40px"
    ]


cellContent : Model -> Position -> Html Msg
cellContent model pos =
    let
        css =
            case isSelected model pos of
                True ->
                    [ style "font-weight" "bold" ]

                False ->
                    []

        allowed =
            Maybe.map (allowedMoves2 model.field) model.selected |> Maybe.withDefault []
    in
    case Dict.get pos model.field of
        Just fig ->
            button
                (css
                    ++ [ onClick
                            (if fig.color == model.player then
                                Select pos

                             else
                                Move pos
                            )
                       , style "text-align" "center"
                       , style "font-size" "40px"
                       , style "background-color" "Transparent"
                       , style "outline" "none"
                       , style "border" "none"
                       , disabled (not (List.member pos allowed) && fig.color /= model.player)
                       , style "text-decoration"
                            (if List.member pos allowed && fig.color /= model.player then
                                "line-through"

                             else
                                ""
                            )
                       ]
                )
                [ text (toSymbol fig) ]

        Nothing ->
            if List.member pos allowed then
                button
                    [ onClick (Move pos)
                    , style "text-align" "center"
                    , style "font-size" "40px"
                    , style "background-color" "Transparent"
                    , style "outline" "none"
                    , style "border" "none"
                    ]
                    [ text "+"
                    ]

            else
                text ""


row : Model -> Int -> List (Html Msg)
row model x =
    let
        ys =
            List.range 1 8
    in
    List.map (\pos -> td (fieldStyle pos) [ cellContent model pos ]) (List.map (\y -> ( x, y )) ys)


view : Model -> Html Msg
view model =
    let
        rows =
            List.map (\r -> tr [] (row model r)) (List.reverse (List.range 1 8))
    in
    div []
        [ table [] rows
        , div [] [ text ("Player: " ++ colorToString model.player) ]
        , br [] []
        , div []
            [ text
                ("Black Check: "
                    ++ (if isCheck Black model.field then
                            "true"

                        else
                            "false"
                       )
                )
            ]
        , br [] []
        , div []
            [ text
                ("White Check: "
                    ++ (if isCheck White model.field then
                            "true"

                        else
                            "false"
                       )
                )
            ]
        , br [] []
        , div []
            [ text
                ("Checkmate: "
                    ++ (if isCheckmate White model.field then
                            "White"

                        else if isCheckmate Black model.field then
                            "Black"

                        else
                            "None"
                       )
                )
            ]
        , br [] []
        , button [ onClick Reset ] [ text "Reset" ]
        ]


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }
