module Board exposing (..)

import Html
import Html exposing (Html, div, table, tr, td, text, node)
import Html.Attributes exposing (style, id, class, rel, href)
import List
import Random
import Cell
import Window
import Task

main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }

-- MODEL

type alias GameSurface =
    { vsize: Int
    , hsize: Int
    }

type alias Coords =
    ( Int, Int )


type alias Model =
    { aliveCells: List Coords
    , gameSurface: GameSurface
    }



init : ( Model, Cmd Msg )
init =
    let
        gameSurface = { vsize = 0, hsize = 0 }
        model = { aliveCells = [(4,4), (4, 5), (5, 5), (6, 6), (7, 7), (8, 10), (20,2), (22,3), (22,15)], gameSurface = gameSurface }
    in
        (model, Task.perform ResizeBoard Window.size)


-- FIXME
neighbors : Coords -> List Coords
neighbors ( i, j ) =
    [ ( i, j ), ( i - 1, j ), ( i + 1, j ), ( i, j - 1 ), ( i, j + 1 ) ]


-- might be needed prob not
indexedMap : (Coords -> a -> b) -> List (List a) -> List (List b)
indexedMap f board =
    board
        |> List.indexedMap (\i row -> row |> List.indexedMap (\j cellModel -> f ( i, j ) cellModel))


cellSize = 50


-- Update

type Msg = ResizeBoard Window.Size

update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        ResizeBoard { width, height } ->
            { model | gameSurface = { hsize = width // cellSize, vsize = height // cellSize } } ! []


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Window.resizes ResizeBoard

-- VIEW


view : Model -> Html Msg
view {aliveCells, gameSurface} =
    let
        cellSizeStr =
            toString cellSize

        cellStyle =
            style
                [ ( "width", cellSizeStr ++ "px" )
                , ( "height", cellSizeStr ++ "px" )
                ]

        renderCellAt = renderCell aliveCells

        rows = List.range 0 gameSurface.vsize
            |> List.map (\_ -> List.range 0 gameSurface.hsize)
            |> List.indexedMap
                (\y row -> tr [] (row |> List.indexedMap (\x col -> td [ cellStyle ] [ (renderCellAt (x, y)) ])))

        lightsTable =
            table [] rows
    in
        div [ id "main" ]
            [ css "style.css"
            , div [ class "board" ] [ table [] rows ]
            ]

renderCell : List Coords -> Coords -> Html Msg
renderCell aliveCells coords =
    let
        state = if List.member coords aliveCells then Cell.Alive else Cell.Dead
    in
       Cell.render state


css : String -> Html a
css path =
    node "link" [ rel "stylesheet", href path ] []

