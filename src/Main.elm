module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Canvas exposing (PathSegment, Point, Renderable, lineTo, moveTo, path, rect, shapes)
import Canvas.Settings exposing (fill, stroke)
import Canvas.Settings.Line exposing (lineWidth)
import Color
import Html exposing (Html, div, p, text, textarea)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import List exposing (map)
import Parser exposing ((|.), (|=), DeadEnd, Parser, Step(..), deadEndsToString, float, int, keyword, lazy, loop, oneOf, spaces, succeed, symbol, token)


main =
    Browser.sandbox { init = init, update = update, view = view }


type TurtleCommand
    = Forward Int
    | Backward Int
    | Right Float
    | Left Float
    | PenUp
    | PenDown
    | Repeat Int (List TurtleCommand)


parseCommand : Parser TurtleCommand
parseCommand =
    oneOf
        [ succeed Forward |. token "fd" |. spaces |= int
        , succeed Backward |. token "bk" |. spaces |= int
        , succeed Right |. token "rt" |. spaces |= float
        , succeed Left |. token "lt" |. spaces |= float
        , succeed PenUp |. token "pu"
        , succeed PenDown |. token "pd"
        , succeed Repeat
            |. token "repeat"
            |. spaces
            |= int
            |. spaces
            |. symbol "["
            |. spaces
            |= parseInput
            |. spaces
            |. symbol "]"
        ]
        |. spaces


parseCommandStep : List TurtleCommand -> Parser (Step (List TurtleCommand) (List TurtleCommand))
parseCommandStep revStmts =
    oneOf
        [ succeed (\stmt -> Loop (stmt :: revStmts)) |= parseCommand
        , succeed () |> Parser.map (\_ -> Done (List.reverse revStmts))
        ]


parseInput : Parser (List TurtleCommand)
parseInput =
    loop [] parseCommandStep


runParseInput : String -> Result (List DeadEnd) (List TurtleCommand)
runParseInput inputString =
    inputString
        |> String.toLower
        |> String.trim
        |> Parser.run parseInput


parseResultToCommands : Result (List DeadEnd) (List TurtleCommand) -> List TurtleCommand
parseResultToCommands result =
    case result of
        Ok commands ->
            commands

        Err _ ->
            []



-- model


type alias Model =
    { inputValue : String
    , parseResult : Result (List DeadEnd) (List TurtleCommand)
    , commands : List TurtleCommand
    }


initInput : String
initInput =
    "repeat 20 [ repeat 180 [fd 5 rt 2 ] rt 18 ]"


init : Model
init =
    let
        initialResult =
            runParseInput initInput

        initialCommands =
            parseResultToCommands initialResult
    in
    Model initInput initialResult initialCommands



-- update


type Msg
    = ChangeInput String


update : Msg -> Model -> Model
update msg model =
    case msg of
        ChangeInput newInput ->
            let
                result =
                    runParseInput newInput

                commands =
                    parseResultToCommands result
            in
            { model | inputValue = newInput, parseResult = result, commands = commands }



--view


view : Model -> Html Msg
view model =
    div [ style "display" "flex", style "flex-direction" "column" ]
        [ showTextArea model
        , showDrawing model.commands
        , showParseResults model.parseResult
        ]


showTextArea : Model -> Html Msg
showTextArea model =
    textarea
        [ placeholder "Give the turtle some commands"
        , rows 4
        , value model.inputValue
        , onInput ChangeInput
        , style "max-width" "750px"
        ]
        []


showDrawing : List TurtleCommand -> Html Msg
showDrawing commands =
    let
        width =
            750

        height =
            750

        startPoint =
            ( width / 2, height / 2 )
    in
    Canvas.toHtml ( width, height )
        [ style "border" "1px solid black"
        , style "width" <| String.fromInt width ++ "px"
        , style "height" <| String.fromInt height ++ "px"
        ]
        [ clearCanvas width height
        , showCanvasContent startPoint commands
        ]


clearCanvas : Float -> Float -> Renderable
clearCanvas width height =
    shapes [ fill (Color.rgb 0.85 0 0) ] [ rect ( 0, 0 ) width height ]


showCanvasContent : Point -> List TurtleCommand -> Renderable
showCanvasContent startPoint commands =
    shapes
        [ stroke (Color.rgba 0.85 0.85 0 1), lineWidth 5 ]
        [ path startPoint (generatePathSegments startPoint commands) ]


expandRepeats : List TurtleCommand -> List TurtleCommand
expandRepeats commands =
    commands
        |> List.map
            (\c ->
                case c of
                    Repeat count repeatCommands ->
                        List.repeat count repeatCommands
                            |> List.map expandRepeats
                            |> List.concat

                    _ ->
                        List.singleton c
            )
        |> List.concat


generatePathSegments : Point -> List TurtleCommand -> List PathSegment
generatePathSegments startPoint commands =
    commands
        |> expandRepeats
        |> List.foldl generateStep (initSegmentsState startPoint)
        |> .segments
        |> List.reverse


generateStep : TurtleCommand -> SegementsState -> SegementsState
generateStep command state =
    let
        newLocation =
            nextLocation state.location command

        lineFn =
            if state.location.down then
                lineTo

            else
                moveTo
    in
    { state
        | location = newLocation
        , segments = lineFn newLocation.point :: state.segments
    }


type alias Facing =
    Float


type alias Location =
    { point : Point, facing : Facing, down : Bool }


type alias SegementsState =
    { location : Location, segments : List PathSegment }


initSegmentsState : Point -> SegementsState
initSegmentsState startPoint =
    { location =
        { point = startPoint
        , facing = 0
        , down = True
        }
    , segments = []
    }


nextLocation : Location -> TurtleCommand -> Location
nextLocation location command =
    let
        ( x, y ) =
            location.point

        radFacing =
            degrees location.facing
    in
    case command of
        Forward amount ->
            let
                newX =
                    x + cos radFacing * toFloat amount

                newY =
                    y + sin radFacing * toFloat amount
            in
            { location | point = ( newX, newY ) }

        Backward amount ->
            let
                newX =
                    x + cos (radFacing + pi) * toFloat amount

                newY =
                    y + sin (radFacing + pi) * toFloat amount
            in
            { location | point = ( newX, newY ) }

        Right amount ->
            { location | facing = location.facing + amount }

        Left amount ->
            { location | facing = location.facing - amount }

        PenUp ->
            { location | down = False }

        PenDown ->
            { location | down = True }

        -- Repeat should never get here because it is already expanded
        -- Try to enforce or build commands here instead?
        Repeat _ _ ->
            location


showParseResults : Result (List DeadEnd) (List TurtleCommand) -> Html Msg
showParseResults result =
    case result of
        Ok commands ->
            showParsedCommands commands

        Err deadEnds ->
            p [] [ text "could not build all commands" ]


showParsedCommands : List TurtleCommand -> Html Msg
showParsedCommands commands =
    div [] (map showCommand commands)


showCommand : TurtleCommand -> Html Msg
showCommand command =
    case command of
        Repeat count commands ->
            div []
                [ commandToHtml command
                , div [ style "margin-left" "30px" ] (List.map showCommand commands)
                ]

        c ->
            commandToHtml c


commandToHtml : TurtleCommand -> Html Msg
commandToHtml command =
    p [] [ text <| commandToString command ]


commandToString : TurtleCommand -> String
commandToString command =
    case command of
        Forward amount ->
            "forward " ++ String.fromInt amount

        Backward amount ->
            "backward " ++ String.fromInt amount

        Right amount ->
            "turn right " ++ String.fromFloat amount

        Left amount ->
            "turn left " ++ String.fromFloat amount

        PenUp ->
            "pen up"

        PenDown ->
            "pen down"

        Repeat count _ ->
            "repeat " ++ String.fromInt count ++ ":"
