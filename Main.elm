module Main exposing (..)

import Browser
import Browser.Events exposing (onKeyDown)
import Json.Decode as D exposing (Decoder, field, string)
import Array exposing (Array)
import Html exposing (Html, div, text, br, input, span)
import Html.Attributes exposing (id, class, style)
import Html.Events exposing (onClick, onInput, targetValue)
import Time exposing (Posix, every, posixToMillis, millisToPosix)
import Task exposing (perform)
import Random exposing (Generator)

import Debug exposing (log)

type alias Array2d = Array (Array Int)

type alias Model =
  { next : Array2d
  , hold : Array2d
  , currentTick : Int
  , startTime : Int
  , score : Int
  , level : Int
  , holdUse : Bool
  , current :
    { x : Int
    , y : Int
    , shape : Array2d
    }
  , field : Array2d
  , canGoDown : Bool
  , isGameOver : Bool
  }

type Shape
  = T | S | Z | O | I | J | L

shapeToArray s =
  let
    ( strList, color ) =
      case s of
        Just O ->
          ( [ "⬜⬜⬜⬜"
            , "⬜⬛⬛⬜"
            , "⬜⬛⬛⬜"
            , "⬜⬜⬜⬜"
            ], 1 )
        Just T ->
          ( [ "⬜⬛⬜"
            , "⬜⬛⬛"
            , "⬜⬛⬜"
            ], 2 )
        Just S ->
          ( [ "⬛⬜⬜"
            , "⬛⬛⬜"
            , "⬜⬛⬜"
            ], 3 )
        Just Z ->
          ( [ "⬜⬜⬛"
            , "⬜⬛⬛"
            , "⬜⬛⬜"
            ], 4 )
        Just I ->
          ( [ "⬜⬛⬜⬜"
            , "⬜⬛⬜⬜"
            , "⬜⬛⬜⬜"
            , "⬜⬛⬜⬜"
            ], 5 )
        Just J ->
          ( [ "⬜⬛⬜"
            , "⬜⬛⬜"
            , "⬛⬛⬜"
            ], 6 )
        Just L ->
          ( [ "⬜⬛⬜"
            , "⬜⬛⬜"
            , "⬜⬛⬛"
            ], 7 )
        _ ->
          ( [ "⬜⬜⬜⬜"
            , "⬜⬜⬜⬜"
            , "⬜⬜⬜⬜"
            , "⬜⬜⬜⬜"
            ], 0 )
  in
    strList
      |> List.map
        (\str ->
          String.toList str
            |> List.map
              (\c ->
                if c=='⬜'
                then 0
                else color
              )
            |> Array.fromList
        )
      |> Array.fromList

initialModel =
  { next = Array.empty
  , hold = Array.empty
  , currentTick = 0
  , startTime = 0
  , score = 0
  , level = 0
  , holdUse = False
  , current =
    { x = 4
    , y = 0
    , shape = Array.empty
    }
  , field = emptyField
  , canGoDown = False
  , isGameOver = False
  }

emptyField =
  Array.repeat 17 (Array.repeat 10 0)

-- "⬛⬜"

type Msg
  = Tick Posix
  | Pressed (Maybe Option)
  | Start Posix
  | FirstShape Shape
  | Next Shape

type Option
  = Rotate Direction
  | Move Direction
  | Hold
  | SpeedUp
  | Drop

type Direction = Left | Right | Down

nextGen : Generator Shape
nextGen =
  Random.uniform O [T,S,Z,I,L,J]

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  let
    noChange = ( checkDestroy model, Cmd.none )
  in
    case msg of
      Start posix ->
        ( { model |
            startTime = posixToMillis posix
          , isGameOver = False
          }
        , Random.generate FirstShape nextGen
        )

      FirstShape shape ->
        ( { model |
            current =
              { x = model.current.x
              , y = model.current.y
              , shape = Just shape |> shapeToArray
              }
          }
        , Random.generate Next nextGen
        )

      Tick posix ->
        if model.isGameOver
        then
          ( { initialModel |
              isGameOver = True
            }
          , Cmd.none
          )
        else
          let
            newPosModel =
              if modBy ((808-model.level*8)//16) model.currentTick == 0
              then
                if checkCollision model.field model.current Down
                then
                  (
                    { model |
                      current =
                        { x = model.current.x
                        , y = model.current.y + 1
                        , shape = model.current.shape
                        }
                    }
                    |> checkDestroy
                  , Cmd.none
                  )
                else
                  if model.canGoDown
                  then
                    ( { model |
                        canGoDown = False
                      }
                      |> checkDestroy
                    , Cmd.none
                    )
                  else
                    let
                      newField =
                        verifyFalling
                          model.current
                          model.field
                    in
                      if newField == model.field
                      then
                        ( { model |
                            isGameOver = True
                          }
                          |> checkDestroy
                        , Cmd.none
                        )
                      else
                        ( { model |
                            field = newField
                          , canGoDown = True
                          , current =
                            { x = 4
                            , y = 0
                            , shape = model.next
                            }
                          , holdUse = False
                          }
                          |> checkDestroy
                        , Random.generate Next nextGen
                        )
              else
                noChange
        in
          newPosModel
      Next shape ->
        ( { model |
            next = Just shape |> shapeToArray
          , canGoDown = True
          }
        , Cmd.none
        )

      Pressed mOpt ->
        if model.startTime == 0
        then
          ( model, perform Start Time.now)
        else
          case mOpt of
            Just (Rotate d) ->
              ( rotate model d
              , Cmd.none
              )
            Just (Move d) ->
              if checkCollision model.field model.current d
              then
                let
                  movement =
                    if d == Left then -1 else 1
                in
                  ( { model |
                    current =
                      { x = model.current.x + movement
                      , y = model.current.y
                      , shape = model.current.shape
                      }
                    }
                  , Cmd.none
                  )
              else
                noChange
            Just Hold ->
              if model.holdUse
              then noChange
              else
                if model.hold == Array.empty
                then
                  ( { model |
                      holdUse = True
                    , current =
                      { x = 4
                      , y = 0
                      , shape = model.next
                      }
                    , hold = model.current.shape
                    }
                  , Random.generate Next nextGen
                  )
                else
                  ( { model |
                      holdUse = True
                    , current =
                        { x = 4
                        , y = 0
                        , shape = model.hold
                        }
                    , hold = model.current.shape
                    }
                  , Cmd.none
                  )
            Just SpeedUp ->
              if checkCollision model.field model.current Down
                then
                  (
                    { model |
                      current =
                        { x = model.current.x
                        , y = model.current.y + 1
                        , shape = model.current.shape
                        }
                    }
                    |> checkDestroy
                  , Cmd.none
                  )
                else
                  noChange
            Just Drop -> noChange
            _ -> noChange

checkDestroy model =
  let
    width =
      ( Maybe.withDefault
        Array.empty
        (Array.get 0 model.field)
      )
        |> Array.length

    length = Array.length model.field
    lineDestr0yer =
      Array.filter
        ( Array.foldl
            (\a b ->(||) (a==0) b)
            False
        )
        model.field
    newField =
      Array.append
        ( Array.repeat
          (length - (Array.length lineDestr0yer))
          (Array.repeat width 0)
        )
        lineDestr0yer
  in
    { model |
      score = model.score + (length - (Array.length lineDestr0yer))
    , level = min (model.score // 10) 100
    , field = newField
    , currentTick = (model.currentTick + 1) |> log "tick"
    }

rotate model d =
  let
    target = model.current.shape
    size = Array.length target
    newArray = Array.repeat size (Array.repeat size 0)
    rotatedX y = if d == Left then size-y-1 else y
    rotatedY x = if d == Left then x else size-x-1
    setHelper x y arr =
      let
        helper =
          ( Maybe.withDefault 0
            (array2dGet (rotatedX y) (rotatedY x) target)
          )
      in
        if helper /= 0
        then
          array2dSet x y helper arr
        else
          arr

    rotateHelper =
      List.range 0 (size - 1)
        |> List.concatMap
          (\x ->
            List.range 0 (size - 1)
              |> List.map (\y -> ( x, y ))
          )
        |> List.foldl
          (\( x, y ) arr ->
            setHelper x y arr
          )
          newArray
    newCurrent x_ y_ =
      { shape = rotateHelper
      , x = model.current.x + x_
      , y = model.current.y + y_
      }

  in
    if
      checkCollision
        model.field
        (newCurrent 0 -1)
        Down
    then
      { model | current = newCurrent 0 0 }
    else
      model


verifyFalling current field =
  let
    helper : (Int ,Int) -> Array2d -> Int
    helper (x,y) target =
      array2dGet x y target
        |> Maybe.withDefault 0

    loopList =
      List.range 0 3
        |> List.concatMap
          (\n ->
            List.range 0 3
              |> List.map (\m -> (n,m))
          )
  in
    loopList
      |> List.foldl
        (\(x,y) f ->
          if helper (current.x+x,current.y+y) field == 0
          then
            array2dSet
              (current.x+x)
              (current.y+y)
              (helper (x,y) current.shape)
              f
          else
            f
        )
        field
    |> log "field"


checkCollision field current d = -- True => No Obstruction
  let
    helper : (Int ,Int) -> Array2d -> Int
    helper (x,y) target =
      case array2dGet x y target of
        Just n -> n
        _ -> -1

    loopList =
      List.range 0 3
        |> List.concatMap
          (\n ->
            List.range 0 3
              |> List.map (\m -> (n,m))
          )

    changeCood (x,y) =
      case d of
        Right -> ( 1+current.x+x, 0+current.y+y )
        Left -> ( -1+current.x+x, 0+current.y+y )
        Down ->  ( 0+current.x+x, 1+current.y+y )

  in
    loopList
      |> List.foldl
        (\ (x,y) bool ->
            (&&)
              bool
              ( (||)
                ( helper (x,y) current.shape <= 0 )
                ( helper (changeCood (x,y)) field == 0 )
              )
        )
        True

array2dGet : Int -> Int -> Array2d -> Maybe Int
array2dGet x y arr =
  case Array.get y arr of
    Just a -> Array.get x a
    _ -> Nothing

array2dSet : Int -> Int -> Int -> Array2d -> Array2d
array2dSet x y after arr =
  Array.set
    y
    ((Maybe.withDefault
        Array.empty
        (Array.get y arr)
      )
      |> Array.set x after
    )
    arr

view : Model -> Html Msg
view model =
  if model.startTime == 0
  then
    if model.isGameOver then
      div[]
        [ text "GameOver"
        , br[][]
        , text "score : "
        , text <| String.fromInt model.score
        , br[][]
        , text "Press any key to continue"
        ]
    else
      text "Press any key to start"
  else
    let
      field =
        verifyFalling
          model.current
          model.field

      blockView f =
        div[style "line-height" "10px"]
          (
            Array.toList f
              |> List.map
                (\arr ->
                  Array.toList arr
                    |> List.map
                      (\n ->
                        let
                          color =
                            case n of
                              1 -> "yellow"
                              2 -> "purple"
                              3 -> "green"
                              4 -> "red"
                              5 -> "cyan"
                              6 -> "blue"
                              7 -> "orange"
                              _ -> "black"
                        in
                          span
                            [ style "color" color
                            , style "letter-spacing" "-6px"
                            ]
                            [ text "■" ]
                      )
                )
              |> List.intersperse ([text "  ",br[][]])
              |> List.concat
          )

      fieldView =
        div[]
          <| List.append
            [blockView field]
            [ br[][]
            , text "score : ", text <| String.fromInt model.score
            , br[][]
            , text "next :"
            , br[][]
            , blockView model.next
            , br[][]
            , text "hold :"
            , br[][]
            , blockView model.hold
            ]
    in
      fieldView

keyToOption : String -> Maybe Option
keyToOption str =
  case str of
    "a" -> Just (Move Left)
    "d" -> Just (Move Right)
    "s" -> Just SpeedUp
    "w" -> Just Drop
    "q" -> Just (Rotate Left)
    "e" -> Just (Rotate Right)
    "r" -> Just Hold
    _ -> Nothing

keyDownDecoder =
  (field "key" string)
    |> D.map keyToOption
    |> D.map Pressed

subscriptions : Model -> Sub Msg
subscriptions model =
  if model.startTime == 0
  then
    onKeyDown keyDownDecoder
  else
    Sub.batch
      [ Time.every 16 Tick
      , onKeyDown keyDownDecoder
      ]

main : Program () Model Msg
main =
  Browser.element
    { init = \_ -> ( initialModel, Cmd.none )
    , view = view
    , update = update
    , subscriptions = subscriptions
    }
