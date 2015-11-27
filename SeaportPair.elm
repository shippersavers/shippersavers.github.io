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
  div
  [ id "layout" ]
  [ a [ href "#menu"
      , id "menuLink"
      , class "menu-link"
      ]
    [ span [] [] ]
  , div [ id "menu"]
    [ div [ class "pure-menu" ]
      [ a [ class "pure-menu-heading" ]
        [ text "Shipper Savers"]
      , ul [ class "pure-menu-list"]
        [ li [ class "pure-menu-item"]
          [ a
            [ href "#"
            , class "pure-menu-link" ]
            [ text "Home"]
          ]
        ]
      ]
    ]
  , Html.main' [ ]
    [ header [ ]
      [ div [ class "hero-titles" ]
        [ h1 [] [ text "Shipper Savers" ]
        , h3 [] [ text "We compare sea freight from shipping lines, helping you save ."]
        ]
      , div [ class "hero-form" ]
        [ Html.form [ class "pure-form" ]
          [ fieldset []
            [ Seaport.view (Signal.forwardTo address Left) model.left
            , Seaport.view (Signal.forwardTo address Right) model.right
            , button [
               classList
               [ ("pure-button", True)
               , ("pure-button-primary", True)
               ]
              ]
              [ text "Search"]
            ]
          ]
        ]
      ]
    ]
  ]
