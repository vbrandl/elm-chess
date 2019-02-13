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
        , single
        , toSymbol
        )
import Dialog
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (disabled, style)
import Html.Events exposing (onClick)


type Msg
    = Select Position
    | Move Position
    | Reset
    | Trade Figure


type alias Model =
    { field : Field
    , selected : Maybe Position
    , player : Color
    , tradePawn : Maybe Position
    }


init : Model
init =
    { field = Data.init, selected = Nothing, player = White, tradePawn = Nothing }


update : Msg -> Model -> Model
update msg model =
    case msg of
        Reset ->
            init

        Select pos ->
            { model | selected = Just pos }

        Trade fig ->
            case model.tradePawn of
                Nothing ->
                    model

                Just pos ->
                    { model
                        | field = Dict.insert pos fig model.field
                        , tradePawn = Nothing
                        , player = opposite model.player
                    }

        Move to ->
            case model.selected of
                Nothing ->
                    model

                Just from ->
                    let
                        ( tx, _ ) =
                            to

                        colorLastRow =
                            case model.player of
                                White ->
                                    8

                                Black ->
                                    1

                        pawnReachedLastRow =
                            tx
                                == colorLastRow
                                && (Dict.get from model.field
                                        |> Maybe.map .kind
                                        |> Maybe.map ((==) Pawn)
                                        |> Maybe.withDefault False
                                   )
                    in
                    if pawnReachedLastRow then
                        { model
                            | selected = Nothing
                            , field = performMove (single from to) model.field
                            , tradePawn = Just to
                        }

                    else
                        { model
                            | selected = Nothing
                            , player = opposite model.player
                            , field = performMove (single from to) model.field
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
                    "#8B4513"

                White ->
                    "#DEB887"
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


viewTradeDialog : Model -> Position -> Dialog.Config Msg
viewTradeDialog model pos =
    let
        figs =
            List.map (\k -> { kind = k, color = model.player }) [ Queen, Rook True, Bishop, Knight ]
    in
    { closeMessage = Nothing
    , containerClass = Nothing
    , footer = []
    , header = Just (text "Trade Pawn")
    , body =
        Just
            (div []
                (figs
                    |> List.map
                        (\fig ->
                            button
                                [ onClick (Trade fig)
                                , style "text-align" "center"
                                , style "font-weight" "bold"
                                , style "font-size" "40px"
                                , style "background-color" "Transparent"
                                , style "outline" "none"
                                , style "border" "none"
                                ]
                                [ text (toSymbol fig) ]
                        )
                )
            )
    }


view : Model -> Html Msg
view model =
    let
        rows =
            List.map (\r -> tr [] (row model r)) (List.reverse (List.range 1 8))
    in
    div []
        [ table [ style "border" "none", style "border-collapse" "collapse" ] rows
        , br [] []
        , Dialog.view
            (Maybe.map (viewTradeDialog model) model.tradePawn)
        , br [] []
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
