module Main exposing (..)

import Browser exposing (Document)
import Element as E
import Element.Background as EBG
import Element.Border as EB
import Element.Font as EF
import FormatNumber as FN
import FormatNumber.Locales as FNL
import Html exposing (Html)
import Http
import Json.Decode as D
import RemoteData as RD


type alias Flags =
    {}


type alias Model =
    { stats : RD.WebData (List Stat) }


type alias Stat =
    { faction : String
    , wins : Int
    , losses : Int
    , minElo : Int
    , avgElo : Int
    , maxElo : Int
    }


type Msg
    = StatsResponse (RD.WebData (List Stat))


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : Flags -> ( Model, Cmd Msg )
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


view : Model -> Document msg
view model =
    { title = "Warmaster Stats"
    , body = [ viewBody model ]
    }


viewBody : Model -> Html msg
viewBody model =
    E.layout [ E.width E.fill, E.height E.fill, E.padding 50 ]
        (case model.stats of
            RD.NotAsked ->
                E.text "Initialising."

            RD.Loading ->
                E.text "Loading."

            RD.Failure err ->
                E.text "Error"

            RD.Success stats ->
                viewStats stats
        )


viewStats : List Stat -> E.Element msg
viewStats stats =
    let
        totalPlayed =
            List.sum (List.map (\s -> s.wins) stats)

        total =
            \stat -> stat.wins + stat.losses

        winPercentage =
            \stat -> toFloat stat.wins / toFloat (total stat) * 100

        playPercentage =
            \stat -> toFloat (total stat) / toFloat totalPlayed * 100

        headerAttrs =
            [ EF.bold
            , EF.color colors.blue
            , EB.widthEach { bottom = 1, top = 0, left = 0, right = 0 }
            , EB.color colors.lightGrey
            ]

        colors =
            { blue = E.rgb255 0x72 0x9F 0xCF
            , lightGrey = E.rgb255 0xE0 0xE0 0xE0
            }
    in
    E.table [ E.width E.shrink, E.spacing 10 ]
        { data = stats
        , columns =
            [ { header = E.el headerAttrs <| E.text "Faction"
              , width = E.fill
              , view = \stat -> E.text stat.faction
              }
            , { header = E.el headerAttrs <| E.text "Wins"
              , width = E.fill
              , view = \stat -> E.text (String.fromInt stat.wins)
              }
            , { header = E.el headerAttrs <| E.text "Losses"
              , width = E.fill
              , view = \stat -> E.text (String.fromInt stat.losses)
              }
            , { header = E.el headerAttrs <| E.text "Total"
              , width = E.fill
              , view = \stat -> E.text (String.fromInt (total stat))
              }
            , { header = E.el headerAttrs <| E.text "Win%"
              , width = E.fill
              , view = \stat -> E.text (FN.format FNL.usLocale (winPercentage stat))
              }
            , { header = E.el headerAttrs <| E.text "Play%"
              , width = E.fill
              , view = \stat -> E.text (FN.format FNL.usLocale (playPercentage stat))
              }
            , { header = E.el headerAttrs <| E.text "Min ELO"
              , width = E.fill
              , view = \stat -> E.text (String.fromInt stat.minElo)
              }
            , { header = E.el headerAttrs <| E.text "Avg ELO"
              , width = E.fill
              , view = \stat -> E.text (String.fromInt stat.avgElo)
              }
            , { header = E.el headerAttrs <| E.text "Max ELO"
              , width = E.fill
              , view = \stat -> E.text (String.fromInt stat.maxElo)
              }
            , { header = E.el headerAttrs <| E.text "Max - Min ELO"
              , width = E.fill
              , view = \stat -> E.text (String.fromInt (stat.maxElo - stat.minElo))
              }
            ]
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
