module Sportspay exposing (init)

import Browser
import CreditCard
import CreditCard.Config
import Html exposing (Html, button, div, input, label, p, text)
import Html.Attributes exposing (class, placeholder, style, type_, value)
import Html.Events exposing (onBlur, onClick, onInput)
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
    , cardErrors : CardErrors
    , currentDate : Maybe DateParts
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
    { name : CardName
    , bins : Regex
    , codeLength : Int
    }


type CardName
    = Visa
    | Mastercard


type alias CardErrors =
    { number : Maybe String
    , name : Maybe String
    , month : Maybe String
    , year : Maybe String
    , cvv : Maybe String
    }



-- HELPERS


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        emptyCardErrors =
            { number = Nothing, name = Nothing, month = Nothing, year = Nothing, cvv = Nothing }
    in
    ( { flags = flags, cardData = CreditCard.emptyCardData, cardErrors = emptyCardErrors, currentDate = Nothing }, getCurrentTime )


getCurrentTime : Cmd Msg
getCurrentTime =
    Task.perform UpdateCurrentTime Time.now


supportedCardTypes : List CardType
supportedCardTypes =
    [ { name = Mastercard
      , bins = Maybe.withDefault Regex.never <| Regex.fromString "/^(603136|603689|608619|606200|603326|605919|608783|607998|603690|604891|603600|603134|608718|603680|608710|604998)|(5[1-5][0-9]{14}|2221[0-9]{12}|222[2-9][0-9]{12}|22[3-9][0-9]{13}|2[3-6][0-9]{14}|27[01][0-9]{13}|2720[0-9]{12})$/"
      , codeLength = 3
      }
    , { name = Visa
      , bins = Maybe.withDefault Regex.never <| Regex.fromString "/^4[0-9]{12}(?:[0-9]{3})?$/"
      , codeLength = 3
      }
    ]


validate : Model -> CardErrors
validate { flags, cardData, currentDate } =
    -- validate
    let
        validNumber : Maybe String
        validNumber =
            -- isValid
            case cardData.number of
                Just num ->
                    let
                        validCardType =
                            let
                                match =
                                    supportedCardTypes
                                        |> List.filter (\ct -> not (List.isEmpty (Regex.findAtMost 1 ct.bins num)))
                                        |> List.head
                            in
                            match /= Nothing
                    in
                    if validCardType then
                        Nothing

                    else
                        Just "Invalid Card Type"

                Nothing ->
                    Just "Blank number"

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

        validCvv : Maybe String
        validCvv =
            case cardData.cvv of
                Just cvv ->
                    if String.length cvv /= 3 then
                        Just "Too short"

                    else if String.toInt cvv == Nothing then
                        Just "Not a number"

                    else
                        Nothing

                Nothing ->
                    Nothing
    in
    { number = validNumber
    , name = Nothing
    , month = Nothing
    , year = Nothing
    , cvv = validCvv
    }


tokenize : Model -> String
tokenize { flags, cardData } =
    -- tokenize
    ""



-- UPDATE


type Msg
    = NoOp
    | UpdateCurrentTime Time.Posix
    | UpdateCardNumber String
    | UpdateCardName String
    | UpdateCardMonth String
    | UpdateCardYear String
    | UpdateCardCvv String
    | Validate
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
                                                Just value

                                            Nothing ->
                                                cardData.number
                    }
            in
            ( { model | cardData = updatedCardData model.cardData }, Cmd.none )

        UpdateCardName value ->
            let
                updatedCardData cardData =
                    { cardData
                        | name =
                            case value of
                                "" ->
                                    Nothing

                                _ ->
                                    if String.length value < 128 then
                                        Just value

                                    else
                                        cardData.name
                    }
            in
            ( { model | cardData = updatedCardData model.cardData }, Cmd.none )

        UpdateCardMonth value ->
            let
                updatedCardData cardData =
                    { cardData
                        | month =
                            case value of
                                "" ->
                                    Nothing

                                _ ->
                                    case String.toInt value of
                                        Just int ->
                                            if int >= 0 && int <= 12 then
                                                Just value

                                            else
                                                cardData.month

                                        Nothing ->
                                            cardData.month
                    }
            in
            ( { model | cardData = updatedCardData model.cardData }, Cmd.none )

        UpdateCardYear value ->
            let
                updatedCardData cardData =
                    { cardData
                        | year =
                            case value of
                                "" ->
                                    Nothing

                                _ ->
                                    if String.length value > 4 then
                                        cardData.year

                                    else
                                        case String.toInt value of
                                            Just int ->
                                                Just value

                                            Nothing ->
                                                cardData.year
                    }
            in
            ( { model | cardData = updatedCardData model.cardData }, Cmd.none )

        UpdateCardCvv value ->
            let
                updatedCardData cardData =
                    { cardData
                        | cvv =
                            case value of
                                "" ->
                                    Nothing

                                _ ->
                                    if String.length value > 4 then
                                        cardData.cvv

                                    else
                                        case String.toInt value of
                                            Just int ->
                                                Just value

                                            Nothing ->
                                                cardData.cvv
                    }
            in
            ( { model | cardData = updatedCardData model.cardData }, Cmd.none )

        Validate ->
            ( { model | cardErrors = validate model }, Cmd.none )

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
            div []
                [ div [ class "my-3" ]
                    [ input
                        [ class "w-100 p-2"
                        , onInput UpdateCardNumber
                        , value (Maybe.withDefault "" cardData.number)
                        , onBlur Validate
                        , placeholder "Number"
                        ]
                        []
                    ]
                , div [ class "my-3" ]
                    [ input
                        [ class "w-100 p-2"
                        , onInput UpdateCardName
                        , value (Maybe.withDefault "" cardData.name)
                        , onBlur Validate
                        , placeholder "Cardholder Name"
                        ]
                        []
                    ]
                , div [ class "my-3 d-flex" ]
                    [ input
                        [ class "p-2 mr-2"
                        , style "width" "60px"
                        , onInput UpdateCardMonth
                        , onBlur Validate
                        , value (Maybe.withDefault "" cardData.month)
                        , placeholder "MM"
                        ]
                        []
                    , input
                        [ class "p-2 mr-2"
                        , style "width" "80px"
                        , onInput UpdateCardYear
                        , onBlur Validate
                        , value (Maybe.withDefault "" cardData.year)
                        , placeholder "YYYY"
                        ]
                        []
                    , input
                        [ class "p-2"
                        , style "width" "80px"
                        , onInput UpdateCardCvv
                        , onBlur Validate
                        , value (Maybe.withDefault "" cardData.cvv)
                        , placeholder "CVV"
                        ]
                        []
                    ]
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
