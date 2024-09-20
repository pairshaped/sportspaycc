module Sportspay exposing (init)

import Browser
import CreditCard
import CreditCard.Config
import Html exposing (Html, button, div, input, label, p, text)
import Html.Attributes exposing (class, placeholder, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Regex exposing (Regex)
import Task
import Time



-- import Json.Decode as Decode exposing (Decoder, nullable, string)
-- import RemoteData exposing (RemoteData(..), WebData)
-- import RemoteData.Http
-- MODEL


type alias Model =
    { flags : Flags
    , cardData : CreditCard.CardData {}
    , currentDate : Maybe DateParts
    , error : Maybe String
    }


type alias Flags =
    { host : String
    , apiKey : String
    }


type alias DateParts =
    { year : Int
    , month : Int
    , day : Int
    }


type alias CardType =
    { name : String
    , bins : Regex
    , codeLength : Int
    }



-- HELPERS


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { flags = flags, cardData = CreditCard.emptyCardData, currentDate = Nothing, error = Nothing }, getCurrentTime )


getCurrentTime : Cmd Msg
getCurrentTime =
    Task.perform UpdateCurrentTime Time.now


supportedCardTypes : List CardType
supportedCardTypes =
    [ { name = "Mastercard"
      , bins = Maybe.withDefault Regex.never <| Regex.fromString "/^(603136|603689|608619|606200|603326|605919|608783|607998|603690|604891|603600|603134|608718|603680|608710|604998)|(5[1-5][0-9]{14}|2221[0-9]{12}|222[2-9][0-9]{12}|22[3-9][0-9]{13}|2[3-6][0-9]{14}|27[01][0-9]{13}|2720[0-9]{12})$/"
      , codeLength = 3
      }
    , { name = "Visa"
      , bins = Maybe.withDefault Regex.never <| Regex.fromString "/^4[0-9]{12}(?:[0-9]{3})?$/"
      , codeLength = 3
      }
    ]


validate : Model -> Bool
validate { flags, cardData, currentDate } =
    -- validate
    let
        cardName : String
        cardName =
            -- getCreditCardNameByNumber
            ""

        isNumValid : Bool
        isNumValid =
            -- isValid
            case cardData.number of
                Just num ->
                    False

                Nothing ->
                    False

        isExpirationDateValid : Bool
        isExpirationDateValid =
            case ( currentDate, cardData.month, cardData.year ) of
                ( Just currentDate_, Just month, Just year ) ->
                    case ( String.toInt month, String.toInt year ) of
                        ( Just m, Just y ) ->
                            (y >= currentDate_.year)
                                && (y <= (currentDate_.year + 15))

                        _ ->
                            False

                _ ->
                    False

        isSecurityCodeValid : Bool
        isSecurityCodeValid =
            let
                regex : String -> Regex.Regex
                regex cvv =
                    if String.length cvv >= 3 then
                        ("^[0-9]{1," ++ String.fromInt (String.length cvv) ++ "}$")
                            |> Regex.fromString
                            |> Maybe.withDefault Regex.never

                    else
                        Regex.never
            in
            case cardData.cvv of
                Just cvv ->
                    Regex.contains (regex cvv) cvv

                Nothing ->
                    False
    in
    False


tokenize : Model -> String
tokenize { flags, cardData } =
    -- tokenize
    ""



-- UPDATE


type Msg
    = NoOp
    | UpdateCurrentTime Time.Posix
    | UpdateCardNumber String
    | CancelPayment
    | SubmitPayment


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        UpdateCurrentTime time ->
            let
                timeToDateParts : DateParts
                timeToDateParts =
                    let
                        monthToInt : Time.Month -> Int
                        monthToInt month =
                            case month of
                                Time.Jan ->
                                    1

                                Time.Feb ->
                                    2

                                Time.Mar ->
                                    3

                                Time.Apr ->
                                    4

                                Time.May ->
                                    5

                                Time.Jun ->
                                    6

                                Time.Jul ->
                                    7

                                Time.Aug ->
                                    8

                                Time.Sep ->
                                    9

                                Time.Oct ->
                                    10

                                Time.Nov ->
                                    11

                                Time.Dec ->
                                    12
                    in
                    { year = Time.toYear Time.utc time
                    , month = monthToInt (Time.toMonth Time.utc time)
                    , day = Time.toDay Time.utc time
                    }
            in
            ( { model | currentDate = Just timeToDateParts }, Cmd.none )

        UpdateCardNumber value ->
            let
                updatedCardData cardData =
                    { cardData
                        | number =
                            case value of
                                "" ->
                                    Nothing

                                _ ->
                                    if String.length value > 19 then
                                        cardData.number

                                    else
                                        case String.toInt value of
                                            Just int ->
                                                Just (String.fromInt int)

                                            Nothing ->
                                                cardData.number
                    }
            in
            ( { model | cardData = updatedCardData model.cardData }, Cmd.none )

        CancelPayment ->
            ( model, Cmd.none )

        SubmitPayment ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view { cardData } =
    let
        viewHeader =
            div [ class "my-3" ]
                [ CreditCard.card CreditCard.Config.defaultConfig cardData ]

        viewBody =
            div [ class "my-3" ]
                [ input
                    [ class "w-100 p-2"
                    , onInput UpdateCardNumber
                    , value (Maybe.withDefault "" cardData.number)
                    , placeholder "Number"
                    ]
                    []
                ]

        viewFooter =
            div [ class "d-flex justify-content-end my-3" ]
                [ button
                    [ class "mr-2 btn btn-secondary"
                    , onClick CancelPayment
                    ]
                    [ text "Cancel" ]
                , button
                    [ class "btn btn-primary"
                    , onClick SubmitPayment
                    ]
                    [ text "Submit" ]
                ]
    in
    div [ class "m-4", style "width" "350px" ]
        [ viewHeader
        , viewBody
        , viewFooter
        ]



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }
