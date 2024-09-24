module SportspayCC exposing (init)

import Browser
import Browser.Navigation as Navigation
import CreditCard
import CreditCard.Config
import Html exposing (Html, a, button, div, em, form, h4, img, input, label, p, text)
import Html.Attributes exposing (alt, class, disabled, href, placeholder, src, style, type_, value)
import Html.Events exposing (onBlur, onInput, onSubmit)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (custom, optional, optionalAt, required)
import Regex exposing (Regex)
import Task
import Time



-- MODEL


type alias Model =
    { flags : Flags
    , cardData : CreditCard.CardData {}
    , cardErrors : CardErrors
    , currentDate : Maybe DateParts
    , ott : Maybe String
    }


type alias Flags =
    { sportspayHost : String
    , sportspayApiKey : String
    , transactionAmount : String
    , transactionUrl : String
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
    , expiry : Maybe String
    , cvv : Maybe String
    , ott : Maybe String
    }


type alias TokenizeResult =
    { message : String
    , ott : Maybe String
    , apiKeyError : Maybe String
    , numberError : Maybe String
    , expiryError : Maybe String
    , cvvError : Maybe String
    }


type alias PaymentResult =
    { message : String }



-- HELPERS


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { flags = flags
      , cardData = CreditCard.emptyCardData
      , cardErrors = emptyCardErrors
      , currentDate = Nothing
      , ott = Nothing
      }
    , getCurrentTime
    )


getCurrentTime : Cmd Msg
getCurrentTime =
    Task.perform UpdateCurrentTime Time.now


supportedCardTypes : List CardType
supportedCardTypes =
    [ { name = Mastercard
      , bins = Maybe.withDefault Regex.never <| Regex.fromString "^(603136|603689|608619|606200|603326|605919|608783|607998|603690|604891|603600|603134|608718|603680|608710|604998)|(5[1-5][0-9]{14}|2221[0-9]{12}|222[2-9][0-9]{12}|22[3-9][0-9]{13}|2[3-6][0-9]{14}|27[01][0-9]{13}|2720[0-9]{12})$"
      , codeLength = 3
      }
    , { name = Visa
      , bins = Maybe.withDefault Regex.never <| Regex.fromString "^4[0-9]{12}(?:[0-9]{3})?$"
      , codeLength = 3
      }
    ]


validate : Model -> CardErrors
validate { flags, cardData, currentDate } =
    -- validate
    let
        validNumber : Maybe String
        validNumber =
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
                    Nothing

        validExpiry : Maybe String
        validExpiry =
            case ( currentDate, cardData.month, cardData.year ) of
                ( Just currentDate_, Just month, Just year ) ->
                    case ( String.toInt month, String.toInt year ) of
                        ( Just m, Just y ) ->
                            if (y < currentDate_.year) || (y == currentDate_.year && m < currentDate_.month) then
                                Just "Already expired"

                            else if y > (currentDate_.year + 15) then
                                Just "Too far in the future"

                            else
                                Nothing

                        _ ->
                            Nothing

                _ ->
                    Nothing

        validCvv : Maybe String
        validCvv =
            case cardData.cvv of
                Just cvv ->
                    if String.length cvv < 3 then
                        Just "Too short"

                    else if String.length cvv > 3 then
                        Just "Too long"

                    else if String.toInt cvv == Nothing then
                        Just "Not a number"

                    else
                        Nothing

                Nothing ->
                    Nothing
    in
    { number = validNumber
    , name = Nothing
    , expiry = validExpiry
    , cvv = validCvv
    , ott = Nothing
    }


hasCardErrors : CardErrors -> Bool
hasCardErrors { number, expiry, cvv, ott } =
    (number /= Nothing)
        -- || (name /= Nothing)
        || (expiry /= Nothing)
        || (cvv /= Nothing)
        || (ott /= Nothing)


tokenizeResultDecoder : Decoder TokenizeResult
tokenizeResultDecoder =
    -- , "TEXT": "TOKEN OK"
    -- , "OTT": "4200HDETSHGT0042"
    -- , "APIKEY":
    --     { "isvalid": true
    --     , "message": ""
    --     }
    -- , "CARDNUM":
    --     { "isvalid": true
    --     , "message": ""
    --     }
    -- , "CARDEXP":
    --     { "isvalid": true
    --     , "message": ""
    --     }
    -- , "CARDCVV":
    --     { "isvalid": true
    --     ,"message": ""
    --     }
    -- }
    let
        decodeMessage =
            Decode.string
                |> Decode.andThen
                    (\str ->
                        case str of
                            "" ->
                                Decode.succeed Nothing

                            _ ->
                                Decode.succeed (Just str)
                    )
    in
    Decode.succeed TokenizeResult
        |> required "TEXT" Decode.string
        |> optional "OTT" (Decode.nullable Decode.string) Nothing
        |> optionalAt [ "apikey", "message" ] decodeMessage Nothing
        |> optionalAt [ "cardnum", "message" ] decodeMessage Nothing
        |> optionalAt [ "cardexp", "message" ] decodeMessage Nothing
        |> optionalAt [ "cardcvv", "message" ] decodeMessage Nothing


paymentResultDecoder : Decoder PaymentResult
paymentResultDecoder =
    Decode.succeed PaymentResult
        |> required "message" Decode.string


getOneTimeToken : Model -> Cmd Msg
getOneTimeToken { flags, cardData } =
    case [ cardData.number, cardData.month, cardData.year, cardData.cvv ] of
        [ Just number, Just month, Just year, Just cvv ] ->
            let
                url =
                    let
                        cardDataToParams =
                            let
                                month_ =
                                    if String.length month == 1 then
                                        "0" ++ month

                                    else
                                        month
                            in
                            "&CARDNUM=" ++ number ++ "&CARDEXP=" ++ (month_ ++ String.right 2 year) ++ "&CARDCVV=" ++ cvv
                    in
                    flags.sportspayHost ++ "/api/HOSTPYMT/ott?APIKEY=" ++ flags.sportspayApiKey ++ cardDataToParams
            in
            Http.get
                { url = url
                , expect = Http.expectJson GotTokenizeResponse tokenizeResultDecoder
                }

        _ ->
            Cmd.none


completePayment : Flags -> String -> Cmd Msg
completePayment flags ott =
    let
        url =
            flags.transactionUrl ++ "?ott=" ++ ott
    in
    Navigation.load url


emptyCardErrors =
    { number = Nothing, name = Nothing, expiry = Nothing, cvv = Nothing, ott = Nothing }



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
    | SubmitCard
    | GotTokenizeResponse (Result Http.Error TokenizeResult)


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
                                    if String.length value > 2 then
                                        cardData.month

                                    else
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
                                    if String.length value > 3 then
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
            let
                cardErrors =
                    validate model
            in
            ( { model | cardErrors = cardErrors }, Cmd.none )

        SubmitCard ->
            let
                updatedModel =
                    { model | cardErrors = validate model }

                isValid : Bool
                isValid =
                    let
                        hasBlanks { number, month, year, cvv } =
                            (number == Nothing)
                                -- && (name == Nothing)
                                && (month == Nothing)
                                && (year == Nothing)
                                && (cvv == Nothing)
                    in
                    not (hasBlanks model.cardData || hasCardErrors updatedModel.cardErrors)
            in
            ( updatedModel
            , if isValid then
                getOneTimeToken updatedModel

              else
                Cmd.none
            )

        GotTokenizeResponse response ->
            case response of
                Ok result ->
                    case result.ott of
                        Just ott_ ->
                            ( model, completePayment model.flags ott_ )

                        Nothing ->
                            let
                                updatedCardErrors =
                                    { number = result.numberError
                                    , name = Nothing
                                    , expiry = result.expiryError
                                    , cvv = result.cvvError
                                    , ott = Just result.message
                                    }
                            in
                            ( { model | cardErrors = updatedCardErrors }, Cmd.none )

                Err error ->
                    let
                        updatedCardErrors cardErrors =
                            { cardErrors | ott = Just "Server error" }
                    in
                    ( { model | cardErrors = updatedCardErrors model.cardErrors }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view { flags, cardData, cardErrors, ott } =
    case ott of
        Just ott_ ->
            div [] [ text ("Have OTT: " ++ ott_) ]

        Nothing ->
            div [ class "d-flex flex-column" ]
                [ viewCardData flags cardData cardErrors
                , if hasCardErrors cardErrors then
                    viewErrors cardErrors

                  else
                    text ""
                ]


viewErrors : CardErrors -> Html Msg
viewErrors cardErrors =
    let
        viewError label errMsg =
            case errMsg of
                Just msg ->
                    div [ class "d-flex mt-2" ]
                        [ div [ class "mr-2" ] [ text label ]
                        , div [] [ text msg ]
                        ]

                Nothing ->
                    text ""
    in
    div [ class "d-flex flex-column mt-2 text-danger" ]
        [ h4 [ class "mt-2" ] [ text "Errors" ]
        , viewError "Number: " cardErrors.number
        , viewError "Expiry: " cardErrors.expiry
        , viewError "CVV: " cardErrors.cvv
        , viewError "OTT: " cardErrors.ott
        ]


viewCardData : Flags -> CreditCard.CardData {} -> CardErrors -> Html Msg
viewCardData { transactionAmount } cardData cardErrors =
    let
        cardNumberInput =
            input
                [ class "w-100 p-2"
                , onInput UpdateCardNumber
                , value (Maybe.withDefault "" cardData.number)
                , onBlur Validate
                , placeholder "Card Number"
                ]
                []

        cardNameInput =
            input
                [ class "w-100 p-2"
                , onInput UpdateCardName
                , value (Maybe.withDefault "" cardData.name)
                , onBlur Validate
                , placeholder "Cardholder Name"
                ]
                []

        cardMonthInput =
            input
                [ class "p-2 mr-2"
                , style "width" "40px"
                , onInput UpdateCardMonth
                , onBlur Validate
                , value (Maybe.withDefault "" cardData.month)
                , placeholder "M"
                ]
                []

        cardYearInput =
            input
                [ class "p-2 mr-2"
                , style "width" "60px"
                , onInput UpdateCardYear
                , onBlur Validate
                , value (Maybe.withDefault "" cardData.year)
                , placeholder "YYYY"
                ]
                []

        cardCvvInput =
            input
                [ class "p-2 mr-2"
                , style "width" "52px"
                , onInput UpdateCardCvv
                , onBlur Validate
                , value (Maybe.withDefault "" cardData.cvv)
                , placeholder "CVV"
                ]
                []

        cardButtons =
            div [ class "ml-1 d-flex flex-fill justify-content-end" ]
                [ input
                    [ type_ "submit"
                    , class "btn btn-primary"
                    , value ("Pay " ++ transactionAmount)
                    , disabled (hasCardErrors cardErrors)
                    ]
                    []
                ]
    in
    div [ class "text-left", style "width" "350px" ]
        [ form [ onSubmit SubmitCard ]
            [ div []
                [ CreditCard.card CreditCard.Config.defaultConfig cardData ]
            , div []
                [ div [ class "my-3" ] [ cardNumberInput ]

                -- , div [ class "my-3" ] [ cardNameInput ]
                , div [ class "my-3 d-flex" ]
                    [ cardMonthInput
                    , cardYearInput
                    , cardCvvInput
                    , cardButtons
                    ]
                ]
            ]
        , img [ class "w-50 mt-2", src "https://raw.githubusercontent.com/pairshaped/sportspaycc/refs/heads/master/sports-pay-logo.svg", alt "Powered by SportsPay" ] []
        ]



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }
