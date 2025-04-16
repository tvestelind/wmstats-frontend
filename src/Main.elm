module Main exposing (..)

import Browser
import FormatNumber as FN
import FormatNumber.Locales as FNL
import Html exposing (Html, table, tbody, td, text, th, thead, tr)
import Http
import Json.Decode as D
import RemoteData as RD


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Stat =
    { faction : String
    , wins : Int
    , losses : Int
    , minElo : Int
    , avgElo : Int
    , maxElo : Int
    }


type alias Model =
    { stats : RD.WebData (List Stat) }


type Msg
    = StatsResponse (RD.WebData (List Stat))


init : () -> ( Model, Cmd Msg )
init _ =
    ( { stats = RD.Loading }, getStats )


getStats : Cmd Msg
getStats =
    Http.get { url = "http://localhost:8080/stats", expect = Http.expectJson (RD.fromResult >> StatsResponse) decodeStats }


decodeStats : D.Decoder (List Stat)
decodeStats =
    D.list decodeStat


decodeStat : D.Decoder Stat
decodeStat =
    D.map6 Stat
        (D.field "faction" D.string)
        (D.field "wins" D.int)
        (D.field "losses" D.int)
        (D.field "minElo" D.int)
        (D.field "averageElo" D.int)
        (D.field "maxElo" D.int)


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        StatsResponse response ->
            ( { model | stats = response }
            , Cmd.none
            )


view : Model -> Html msg
view model =
    case model.stats of
        RD.NotAsked ->
            text "Initialising."

        RD.Loading ->
            text "Loading."

        RD.Failure err ->
            text "Error"

        RD.Success stats ->
            viewStats stats


viewStats : List Stat -> Html msg
viewStats stats =
    table []
        [ thead []
            [ th [] [ text "Faction" ]
            , th [] [ text "Wins" ]
            , th [] [ text "Losses" ]
            , th [] [ text "Total" ]
            , th [] [ text "Win%" ]
            , th [] [ text "Play%" ]
            , th [] [ text "Min ELO" ]
            , th [] [ text "Avg ELO" ]
            , th [] [ text "Max ELO" ]
            , th [] [ text "Max - Min ELO" ]
            ]
        , tbody [] (List.map (viewStat (List.sum (List.map (\s -> s.wins) stats))) stats)
        ]


viewStat : Int -> Stat -> Html msg
viewStat totalPlayed stat =
    let
        total =
            stat.wins + stat.losses

        winPercentage =
            (toFloat stat.wins / toFloat total) * 100

        playPercentage =
            (toFloat total / toFloat totalPlayed) * 100

        eloDifference =
            stat.maxElo - stat.minElo
    in
    tr []
        [ td [] [ text stat.faction ]
        , td [] [ text (String.fromInt stat.wins) ]
        , td [] [ text (String.fromInt stat.losses) ]
        , td [] [ text (String.fromInt total) ]
        , td [] [ text (FN.format FNL.usLocale winPercentage) ]
        , td [] [ text (FN.format FNL.usLocale playPercentage) ]
        , td [] [ text (String.fromInt stat.minElo) ]
        , td [] [ text (String.fromInt stat.avgElo) ]
        , td [] [ text (String.fromInt stat.maxElo) ]
        , td [] [ text (String.fromInt eloDifference) ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
