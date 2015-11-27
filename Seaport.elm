module Seaport (Model, Seaport, init, Action, update, view) where

import Effects exposing (Effects, Never)
import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Json exposing ((:=))
import String
import Task
import Char
import Task exposing (..)

-- MODEL

type alias Model =
    { id : String
    , seaportCode : String
    , seaport : Maybe Seaport
    , ports : List Seaport
    , hideList : Bool
    , counter : Int
    }

type alias Seaport =
  { code: String,
    name: String,
    country: String
  }

newSeaport : String -> String -> String -> Seaport
newSeaport code city country =
  { code = code ,
    name = city,
    country = country
  }


init : String -> (Model, Effects Action)
init id =
  ( Model id "" Nothing [] True 0
  , Effects.none
  )


-- UPDATE

type Action
    = RequestMore String
    | NewList (Maybe (List Seaport))
    | PortUpdate String
    | Pickup Seaport
    | NextPort String
    | PickupEnter

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    RequestMore query->
      (model, getListPort query)

    NewList maybeSeaport ->
      ( Model model.id model.seaportCode model.seaport (Maybe.withDefault model.ports maybeSeaport) False 0
      , Effects.none
      )

    PortUpdate code ->
      ( Model model.id code model.seaport model.ports False 0
      , getListPort code
      )

    Pickup seaport ->
      ( Model model.id seaport.code (Just seaport) model.ports True 0
      , Effects.none
      )

    NextPort direction ->
      ( Model model.id model.seaportCode model.seaport model.ports False (addCounter direction model.counter (List.length model.ports))
      , Effects.none
      )

    PickupEnter ->
      let
        seaport = selectedPort model.counter model.ports
        s = Maybe.withDefault (newSeaport "" "" "")  seaport
        code    =  s.code
      in
        ( Model model.id code seaport model.ports True 0
        , Effects.none
        )


-- VIEW

(=>) = (,)

selectedPort : Int -> List Seaport -> Maybe Seaport
selectedPort n seaports = List.head (List.drop (n - 1) seaports)

addCounter : String -> Int -> Int -> Int
addCounter s x l =
  case s of
    "up"   -> if x - 1 > 0 then x - 1  else (l + x) - 1
    "down" -> if x + 1 > l then (x - l) + 1 else x + 1 
              

handler : Int -> Action
handler x =
  case x of
    38 -> NextPort "up"
    40 -> NextPort "down"
    13 -> PickupEnter

view : Signal.Address Action -> Model -> Html
view address model =
  div  [ class "inline" ]
    [ input
      [ class "autocomplete"
      , on "input" targetValue (Signal.message address << PortUpdate)
      , on "keydown" keyCode (\code -> Signal.message address (handler code))
      , value model.seaportCode
      ] [ ]
    , div
      [ class "autocomplete" ]
      [ p [ hidden (not <| seaportExist model.seaport) ] [ text (seaportStr model.seaport) ]      
      , ul
        [ hidden model.hideList
        , classList [
           ("select", True),
           ("waiting", List.isEmpty model.ports),
           ("loaded", not (List.isEmpty model.ports))
          ]
        ]
        (seaportList address model.ports model.counter)
      ]
    ]

seaportExist : Maybe Seaport -> Bool
seaportExist maybe =
  case maybe of
    Just value -> True
    Nothing -> False
      
    

seaportStr : Maybe Seaport -> String
seaportStr seaport =
  case seaport of
    Just p -> String.concat [p.code, ", ", p.name, ", ", p.country]
    Nothing -> ""

seaportList : Signal.Address Action -> List Seaport -> Int -> List Html
seaportList address seaports counter =
  List.map (\s ->
              li
              [ onClick address (Pickup (fst s))
              , classList
                [ ("selected", (snd s))]
              ]
            [ text (seaportStr (Just (fst s)))]
           ) (List.map2 (,) seaports (counterList (List.length seaports) counter))

counterList : Int -> Int -> List Bool
counterList l n =
  List.map (\x -> if x == n then True else False ) [1..l]
      

headerStyle : Attribute
headerStyle =
  style
    [ "width" => "500px"
    , "text-align" => "left"
    ]


-- EFFECTS

getListPort : String -> Effects Action
getListPort query = 
  Http.get decodePorts (portUrl query)
    |> Task.toMaybe 
    |> Task.map NewList
    |> Effects.task
    

portUrl : String -> String
portUrl query=
  Http.url ("http://seaports.herokuapp.com/seaports.json?q=" ++ query) [ ]
         

decodePorts : Json.Decoder (List Seaport)
decodePorts =
  let place =
        Json.object3 Seaport
          ("code" := Json.string)
          ("name" := Json.string)
          ("country" := Json.string)          
  in
      "seaports" := Json.list place


lookupSeaport : String -> Task String (List Seaport)
lookupSeaport inquiry =
  let
    toUrl =
      if String.length inquiry >= 1
        then succeed ("http://seaports.herokuapp.com/seaports.json?q=" ++ inquiry)
        else fail "Please input some character"
  in
    toUrl `andThen` (mapError (always "Not found :(") << Http.get places)


places : Json.Decoder (List Seaport)
places =
  let place =
        Json.object3 Seaport
          ("code" := Json.string)
          ("name" := Json.string)
          ("country" := Json.string)          
  in
      "seaports" := Json.list place
