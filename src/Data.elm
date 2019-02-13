module Data exposing
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
    , isPawn
    , opposite
    , performMove
    , single
    , toFirstPos
    , toSymbol
    )

import Dict exposing (Dict)
import List.Extra exposing (zip)


even : Int -> Bool
even n =
    modBy 2 n == 0


fieldColor : Position -> Color
fieldColor ( x, y ) =
    case even (x + y) of
        True ->
            Black

        False ->
            White


type alias Position =
    ( Int, Int )


validPosition : Position -> Bool
validPosition ( x, y ) =
    x >= 1 && x <= 8 && y >= 1 && y <= 8


type Color
    = Black
    | White


colorToString : Color -> String
colorToString col =
    case col of
        Black ->
            "Black"

        White ->
            "White"


opposite : Color -> Color
opposite col =
    case col of
        Black ->
            White

        White ->
            Black


type Kind
    = King Bool
    | Queen
    | Bishop
    | Knight
    | Rook Bool
    | Pawn Bool


isPawn : Kind -> Bool
isPawn kind =
    case kind of
        Pawn _ ->
            True

        _ ->
            False


type alias Figure =
    { color : Color
    , kind : Kind
    }


hasMoved : Figure -> Bool
hasMoved { color, kind } =
    case kind of
        King False ->
            False

        Rook False ->
            False

        Pawn False ->
            False

        _ ->
            True


moveFigure : Figure -> Figure
moveFigure fig =
    case fig.kind of
        Rook _ ->
            { fig | kind = Rook True }

        King _ ->
            { fig | kind = King True }

        Pawn _ ->
            { fig | kind = Pawn True }

        _ ->
            fig


type alias Field =
    Dict Position Figure


performMove : Move -> Field -> Field
performMove move field =
    case move of
        Single ( from, to ) ->
            Dict.get from field |> Maybe.map (\f -> Dict.remove from field |> Dict.insert to (moveFigure f)) |> Maybe.withDefault field

        Double ( f1, t1 ) ( f2, t2 ) ->
            performMove (single f1 t1) field |> performMove (single f2 t2)


type Move
    = Single ( Position, Position )
    | Double ( Position, Position ) ( Position, Position )


toFirstPos : Move -> Position
toFirstPos move =
    case move of
        Single ( _, to ) ->
            to

        Double ( _, to ) _ ->
            to


isSingle : Move -> Bool
isSingle move =
    case move of
        Single _ ->
            True

        _ ->
            False


single : Position -> Position -> Move
single from to =
    Single ( from, to )


init : Field
init =
    let
        figures =
            pawnRow Black ++ pawnRow White ++ figureRow Black ++ figureRow White
    in
    Dict.fromList figures


pawnRow : Color -> List ( Position, Figure )
pawnRow col =
    let
        x =
            case col of
                Black ->
                    7

                White ->
                    2
    in
    List.map (\y -> ( ( x, y ), Figure col (Pawn False) )) (List.range 1 8)


xPosByColor : Color -> Int
xPosByColor col =
    case col of
        Black ->
            8

        White ->
            1


figureRow : Color -> List ( Position, Figure )
figureRow col =
    let
        x =
            xPosByColor col

        figures =
            zip (List.range 1 8) [ Rook False, Knight, Bishop, Queen, King False, Bishop, Knight, Rook False ]
    in
    List.map (\( y, k ) -> ( ( x, y ), Figure col k )) figures


posToSym : Field -> Position -> Maybe String
posToSym field pos =
    Dict.get pos field |> Maybe.map toSymbol


isInitialPos : Figure -> Position -> Bool
isInitialPos fig pos =
    Dict.get pos init |> Maybe.map ((==) fig) |> Maybe.withDefault False


allowedMoves2 : Field -> Position -> List Move
allowedMoves2 field pos =
    let
        color =
            Dict.get pos field |> Maybe.map .color |> Maybe.withDefault Black
    in
    allowedMoves field pos
        |> List.filter
            (\move ->
                performMove move field
                    |> isCheck color
                    |> not
            )


allowedMoves : Field -> Position -> List Move
allowedMoves field (( x, y ) as pos) =
    case Dict.get pos field of
        Nothing ->
            []

        Just fig ->
            case ( fig.color, fig.kind ) of
                ( col, Pawn moved ) ->
                    let
                        f =
                            pawnMoveFn col

                        opLeft =
                            ( f x 1, y - 1 )

                        opRight =
                            ( f x 1, y + 1 )

                        moveLeft =
                            if isOpponent opLeft field col then
                                [ opLeft ]

                            else
                                []

                        moveRight =
                            if isOpponent opRight field col then
                                [ opRight ]

                            else
                                []
                    in
                    ((if not moved && not (isOpponent ( f x 2, y ) field col) then
                        [ ( f x 2, y ) ]

                      else
                        []
                     )
                        ++ moveLeft
                        ++ moveRight
                        ++ (if not (Dict.member ( f x 1, y ) field) then
                                [ ( f x 1, y ) ]

                            else
                                []
                           )
                    )
                        |> List.map (\to -> single pos to)

                ( col, Knight ) ->
                    List.filter
                        (\p ->
                            Dict.get p field
                                |> Maybe.map .color
                                -- has figure at pos
                                |> Maybe.map ((/=) col)
                                -- with different color
                                |> Maybe.withDefault True
                         -- or empty field
                        )
                        [ ( x + 2, y + 1 ), ( x + 2, y - 1 ), ( x - 2, y + 1 ), ( x - 2, y - 1 ), ( x + 1, y + 2 ), ( x + 1, y - 2 ), ( x - 1, y + 2 ), ( x - 1, y - 2 ) ]
                        |> List.map (\to -> single pos to)

                ( col, Bishop ) ->
                    diagonalMovement 8 col pos field

                ( col, Rook _ ) ->
                    straightMovement 8 col pos field

                ( col, Queen ) ->
                    diagonalMovement 8 col pos field
                        ++ straightMovement 8 col pos field

                ( col, King moved ) ->
                    let
                        figRow =
                            xPosByColor col

                        leftRookNotMoved =
                            Dict.get ( figRow, 1 ) field
                                |> Maybe.map hasMoved
                                |> Maybe.withDefault False

                        rightRookNotMoved =
                            Dict.get ( figRow, 8 ) field
                                |> Maybe.map hasMoved
                                |> Maybe.withDefault False

                        leftFree =
                            [ ( figRow, 2 ), ( figRow, 3 ), ( figRow, 4 ) ]
                                |> List.map (\p -> not (Dict.member p field))
                                |> List.foldl (\t acc -> t && acc) True

                        rightFree =
                            [ ( figRow, 6 ), ( figRow, 7 ) ]
                                |> List.map (\p -> not (Dict.member p field))
                                |> List.foldl (\t acc -> t && acc) True

                        leftCastle =
                            if not moved && leftRookNotMoved && leftFree then
                                [ Double ( ( figRow, 5 ), ( figRow, 2 ) ) ( ( figRow, 1 ), ( figRow, 3 ) ) ]

                            else
                                []

                        rightCastle =
                            if not moved && rightRookNotMoved && rightFree then
                                [ Double ( ( figRow, 5 ), ( figRow, 7 ) ) ( ( figRow, 8 ), ( figRow, 6 ) ) ]

                            else
                                []
                    in
                    diagonalMovement 1 col pos field
                        ++ straightMovement 1 col pos field
                        ++ leftCastle
                        ++ rightCastle


diagonalMovement : Int -> Color -> Position -> Field -> List Move
diagonalMovement max col pos field =
    let
        moveFn =
            moveUntilFigure max col pos field
    in
    moveFn add1 add1 ++ moveFn add1 sub1 ++ moveFn sub1 add1 ++ moveFn sub1 sub1


straightMovement : Int -> Color -> Position -> Field -> List Move
straightMovement max col pos field =
    let
        moveFn =
            moveUntilFigure max col pos field
    in
    moveFn add1 id ++ moveFn sub1 id ++ moveFn id add1 ++ moveFn id sub1


add1 : Int -> Int
add1 =
    (+) 1


sub1 : Int -> Int
sub1 x =
    x - 1


id : a -> a
id a =
    a


isOpponent : Position -> Field -> Color -> Bool
isOpponent pos field col =
    Dict.get pos field |> Maybe.map .color |> Maybe.map ((/=) col) |> Maybe.withDefault False


pawnMoveFn : Color -> (Int -> Int -> Int)
pawnMoveFn col =
    case col of
        White ->
            (+)

        Black ->
            (-)


moveUntilFigure : Int -> Color -> Position -> Field -> (Int -> Int) -> (Int -> Int) -> List Move
moveUntilFigure max col (( x, y ) as pos) field fx fy =
    let
        newPos =
            ( fx x, fy y )
    in
    if not (validPosition newPos) || max == 0 then
        []

    else
        Dict.get newPos field
            |> Maybe.map
                (\other ->
                    if other.color /= col then
                        [ single pos newPos ]

                    else
                        []
                )
            |> Maybe.withDefault
                ([ single pos newPos ]
                    ++ moveUntilFigure (max - 1) col newPos field fx fy
                )


isCheck : Color -> Field -> Bool
isCheck col field =
    let
        kingPos =
            Dict.toList field
                |> List.filter (\( _, { kind, color } ) -> isKing kind && color == col)
                |> List.head
                |> Maybe.map (\( pos, _ ) -> pos)

        opMoves =
            Dict.toList field
                |> List.filter (\( _, { kind, color } ) -> color /= col)
                |> List.map (\( pos, _ ) -> pos)
                |> List.map (\pos -> allowedMoves field pos)
                |> List.foldl (\moves acc -> acc ++ moves) []
                |> List.map toFirstPos
    in
    kingPos |> Maybe.map (\pos -> List.member pos opMoves) |> Maybe.withDefault True


isKing : Kind -> Bool
isKing kind =
    case kind of
        King _ ->
            True

        _ ->
            False


isCheckmate : Color -> Field -> Bool
isCheckmate col field =
    if not (isCheck col field) then
        False

    else
        let
            ownFig =
                Dict.toList field
                    |> List.filter (\( _, { kind, color } ) -> color == col)
                    |> List.map (\( pos, _ ) -> pos)
                    |> List.map (allowedMoves2 field)
                    |> List.foldl (\moves acc -> acc ++ moves) []
        in
        List.isEmpty ownFig


toSymbol : Figure -> String
toSymbol { color, kind } =
    case ( color, kind ) of
        ( Black, King _ ) ->
            "♚"

        ( Black, Queen ) ->
            "♛"

        ( Black, Rook _ ) ->
            "♜"

        ( Black, Bishop ) ->
            "♝"

        ( Black, Knight ) ->
            "♞"

        ( Black, Pawn _ ) ->
            "♟"

        ( White, King _ ) ->
            "♔"

        ( White, Queen ) ->
            "♕"

        ( White, Rook _ ) ->
            "♖"

        ( White, Bishop ) ->
            "♗"

        ( White, Knight ) ->
            "♘"

        ( White, Pawn _ ) ->
            "♙"
