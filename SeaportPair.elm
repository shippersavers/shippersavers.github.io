module SeaportPair where

import Seaport
import Effects exposing (Effects)
import Html exposing (..)
import Html.Attributes exposing (..)
--  Model

type alias Model =
  { left : Seaport.Model
  , right : Seaport.Model }

init : String -> String -> (Model, Effects Action)
init leftPort rightPort =
  let
    (left, leftFx)   = Seaport.init leftPort
    (right, rightFx) = Seaport.init rightPort
  in
    ( Model left right
    , Effects.batch
        [ Effects.map Left leftFx
        , Effects.map Right rightFx
        ]
    )

-- UPDATE

type Action
  = Left Seaport.Action
  | Right Seaport.Action

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    Left act ->
      let
        (left, fx) = Seaport.update act model.left
      in
        (Model left model.right
        , Effects.map Left fx
        )

    Right act ->
      let
        (right, fx) = Seaport.update act model.right
      in
        (Model model.left right
        , Effects.map Right fx
        )

-- VIEW
view : Signal.Address Action -> Model -> Html
view address model =
  div [ ]
    [ Seaport.view (Signal.forwardTo address Left) model.left
    , Seaport.view (Signal.forwardTo address Right) model.right
    ]

