module Tariff (Model, Tariff, Filter, emptyFilter, init, Action, update, view, view', tariffList, countTariffs) where

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
import Set exposing (..)

-- MODEL

type alias Model =
  { pol     : String
  , pod     : String
  , tariffs : List Tariff
  , filter  : Filter
  , setFilter : Filter
  , filterTariffs : List Tariff
  }

type alias Tariff =
  { company  :  String,
    pol      :  String,
    pod      :  String,
    container:  String,
    status   :  String,
    owner    :  String,
    freight  :  String,
    baf      :  String
  }

type alias Filter =
  { owners     : Set String
  , containers : Set String
  , status     : Set String
  }

emptyFilter = Filter empty empty empty

init : (Model, Effects Action)
init =
  -- ( Model "" "" []
  -- , Effects.none
  -- )
  ( Model "RUVVO" "HKHKG"  []  emptyFilter emptyFilter []
  , getListTariff "RUVVO" "HKHKG"
  )


priceTariff : Tariff -> String
priceTariff t =
  let freight =
        (String.toFloat t.freight)
      baf =
        (String.toFloat t.baf)
      result = Result.map2 (+) freight baf
  in Result.toMaybe result |> Maybe.withDefault 0.0 |> toString 


-- UPDATE
type Action
    = RequestMore String String
    | NewList (Maybe (List Tariff))
    | FilterByOwners String  Bool 
    | FilterByContainers String Bool
    | FilterByStatus String Bool

update : Action -> Model -> (Model, Effects Action)
update a m =
  case a of
    RequestMore pol pod ->
      ( m
      , getListTariff m.pol m.pod)
      
    NewList maybeTariff ->
      let
        tariffs = Maybe.withDefault m.tariffs maybeTariff
        listContainers   = List.map (.container) tariffs
        filterContainers = fromList listContainers

        listOwners       = List.map (.owner) tariffs
        filterOwners     = fromList listOwners

        listStatus       = List.map (.status) tariffs
        filterStatus     = fromList listStatus

        filter : Filter
        filter  = { owners = filterOwners , containers = filterContainers, status = filterStatus }
      in
        ( Model m.pol m.pod tariffs filter filter tariffs
        , Effects.none
        )

    FilterByOwners string bool -> 
      let
        insertSet =
          case bool of
            True -> Set.insert string m.setFilter.owners
            False -> Set.remove string m.setFilter.owners
        setFilter = Filter insertSet m.setFilter.containers m.setFilter.status

        filterTariffs = filterTariff setFilter m.tariffs
      in
        ( Model m.pol m.pod m.tariffs m.filter setFilter filterTariffs
        , Effects.none
        )

    FilterByContainers string bool -> 
      let
        set =
          case bool of
            True -> Set.insert string m.setFilter.containers
            False -> Set.remove string m.setFilter.containers
        setFilter = Filter m.setFilter.owners set m.setFilter.status
        filterTariffs = filterTariff setFilter m.tariffs
      in
        ( Model m.pol m.pod m.tariffs m.filter setFilter filterTariffs
        , Effects.none
        )

    FilterByStatus string bool -> 
      let
        set =
          case bool of
            True -> Set.insert string m.setFilter.status
            False -> Set.remove string m.setFilter.status
        setFilter = Filter m.setFilter.owners m.setFilter.containers set
        filterTariffs = filterTariff setFilter m.tariffs
      in
        ( Model m.pol m.pod m.tariffs m.filter setFilter filterTariffs
        , Effects.none
        )

-- VIEW
filterTariff : Filter -> List Tariff -> List Tariff
filterTariff filter tariffs
  = List.filter (\x -> byContainers x filter.containers)
    (List.filter (\x -> byStatus x filter.status)
     (List.filter (\x -> byOwners x filter.owners) tariffs))
    
byOwners : Tariff ->  Set String -> Bool
byOwners { owner } list =
  member owner list

byContainers : Tariff ->  Set String -> Bool
byContainers { container } list =
  member container list

byStatus : Tariff ->  Set String -> Bool
byStatus { status } list =
  member status list


checkbox : Signal.Address Action -> Model -> Set String -> (String -> Bool -> Action) -> String -> Html
checkbox address model filter tag title=
  case isEmpty filter of
    True ->
      div [class "hidde"] [ ]
    False ->
      div
      [ class "owners pure-form"]
      [ p [] [ text title]
      , ul [ class "filter owners" ] (listFilterUniversal address model filter tag)
      ]

listFilterUniversal : Signal.Address Action -> Model-> Set String -> (String -> Bool -> Action) -> List Html
listFilterUniversal address model filter tag
  = List.map (\x -> radioFilterUniversal address model.setFilter filter tag x) (Set.toList filter)

radioFilterUniversal : Signal.Address Action -> Filter -> Set String -> (String -> Bool -> Action) -> String -> Html
radioFilterUniversal address setFilter filter' tag style =
  let
    isChecked = member style filter'
    -- f' = FilterByOwners style filter'
  in
    li
    [ ]
    [ label
      [ ]
      [ input
        [ type' "checkbox"
        , checked isChecked
        , on "change" targetChecked (Signal.message address << (tag style))
        ]
        []
      , span [] [text style]
      ]
    ]


view' : Signal.Address Action -> Model -> Html
view' address model =
    div
    [ class "filters"]
    [ checkbox address model model.filter.containers FilterByContainers "Containers"
    , checkbox address model model.filter.owners FilterByOwners "Owners"
    , checkbox address model model.filter.status FilterByStatus "Status"
    -- , p [ ] [ text (setToStr model.setFilter.owners)]
    -- , p [ ] [ text (setToStr model.setFilter.containers)]
    , h3
      [ class "" ]
      [ text "Status"]
    , h3
      [ class "" ]
      [ text "Freight"]
    , h3
      [ class "" ]
      [ text "BAF"]
    , h3
      [ class "" ]
      [ text "Companies"]
    ]


view : Signal.Address Action -> Model -> Html
view address model =
  
  div [ class "requestTariff" ]
    [ button
      [ classList
        [ ("pure-button", True)
        , ("pure-button-primary", True)
        , ("button-large ", True)
        ]
      , onClick address (RequestMore model.pol model.pod)
      ]
      [ text "Search"]
    ]

tariffStr : Tariff -> String
tariffStr t =
  String.concat [ "Company: ",   t.company
                , " POL: ",       t.pol
                , " POD: ",       t.pod
                , " Container: ", t.container
                , " Status: ",    t.status
                , " Owner: ",     t.owner
                , " Freight: ",   t.freight
                , " BAF: ",       t.baf
                ]

tariffList : List Tariff -> List Html
tariffList tariffs =
  List.map (\t ->
              div
              [ class "callout" ]
              [ div
                [ class "pure-g" ]
                [ div [ class "pure-u-1-5" ]
                  [ div [ class "logo" ]
                    [ h3 [ ] [ text t.company ] ]
                  ]
                , div [ class "pure-u-1-5" ]
                  [ div [ class "pol" ] 
                    [ h3 [ ] [ text t.pol ] ]
                  ]
                , div [ class "pure-u-1-5" ]
                  [ div [ class "container" ]
                    [ h3 [ ] [ text t.container ] ]
                  ]
                , div [ class "pure-u-1-5" ]
                  [ div [ class "pod" ]
                    [ h3 [ ] [ text t.pod ] ]
                  ]
                , div [ class "pure-u-1-5" ]
                  [ div [ class "price" ]
                    [ h3 [ ] [ text ("$ " ++ (priceTariff t)) ] ]
                  ]
                ]
              , div
                [ class "pure-g" ]
                [ div [ class "pure-u-1-5" ] []
                , div [ class "pure-u-3-5" ]
                  [ div [ class "pure-g arrow" ]
                    [ div [ class "pure-u-2-24" ] []
                    , div [ class "pure-u-20-24" ]
                      [ div [class "line"] []
                      ]
                    , div [ class "pure-u-2-24" ]
                      [ div [class "point"] [] ]
                    ]
                  ]
                , div [ class "pure-u-1-5" ] []
                ]
              , div
                [ class "pure-g" ]
                [ div [ class "pure-u-1-24" ] []
                , div [ class "pure-u-22-24" ]
                  [ hr [] []
                  , p [ ] [ text "Details" ]
                  ] 
                , div [ class "pure-u-1-24" ] []
                ]
              , div
                [ class "pure-g details" ]
                [ div [ class "pure-u-6-24" ]
                  [ p [ ]
                    [ span [ class "status" ] [ text "Status: " ]
                    , span [ ] [ text t.status ]
                    ]
                  ]
                , div [ class "pure-u-6-24" ]
                  [ p [ ]
                    [ span [ class "owners" ] [ text "Owners: " ]
                    , span [ ] [ text t.owner ]
                    ]
                  ]
                , div [ class "pure-u-6-24" ]
                  [ p [ ]
                    [ span [ class "freight" ] [ text "Freight: " ]
                    , span [ ] [ text t.freight ]
                    ]
                  ]
                , div [ class "pure-u-6-24" ]
                  [ p [ ]
                    [ span [ class "baf" ] [ text "BAF: " ]
                    , span [ ] [ text t.baf ]
                    ]
                  ]
                ]
              ]
           ) tariffs


setToStr : Set String -> String
setToStr set =
  let 
    list = toList set
  in
    String.concat list

countTariffs : Model -> String
countTariffs model =
  let
    tariffs = toString (List.length model.filterTariffs)
  in
    tariffs ++ " results"

-- EFFECTS

getListTariff : String -> String -> Effects Action
getListTariff pol pod = 
  Http.get decodePorts (portUrl pol pod)
    |> Task.toMaybe 
    |> Task.map NewList
    |> Effects.task
    

portUrl : String -> String -> String
portUrl pol pod =
  Http.url ("http://seaports.herokuapp.com/api/v1/tariffs/pols/"
            ++ pol
            ++ "/pods/"
            ++ pod
           ) [ ]
         

decodePorts : Json.Decoder (List Tariff)
decodePorts =
  let tariff =
        Json.object8 Tariff
          ("company"   := Json.string)
          ("pol"       := Json.string)
          ("pod"       := Json.string)
          ("container" := Json.string)
          ("status"    := Json.string)
          ("owners"    := Json.string)
          ("freight"   := Json.string)
          ("baf"       := Json.string)
  in
      "tariffs" := Json.list tariff
