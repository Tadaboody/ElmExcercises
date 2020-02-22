module Trivia exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as D exposing (Decoder, field, index, map2, string)



-- MAIN

main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias AnsweredQuestion =
    { question : BooleanQuestion
    , answer : Bool
    }


type Model
    = Failure String
    | Loading
    | Success BooleanQuestion
    | Answered AnsweredQuestion


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading, getRandomCatGif )



-- UPDATE


type Msg
    = NextQuestion
    | GotQuestion (Result Http.Error BooleanQuestion)
    | Answer Bool


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NextQuestion ->
            ( Loading, getRandomCatGif )

        GotQuestion result ->
            case result of
                Ok url ->
                    ( Success url, Cmd.none )

                Err reason ->
                    ( Failure (errorToString reason), Cmd.none )

        Answer answer ->
            case model of
                Success question ->
                    ( Answered { question = question, answer = answer }, Cmd.none )

                _ ->
                    ( Failure "Answered without a question", Cmd.none )


errorToString : Http.Error -> String
errorToString err =
    case err of
        Http.Timeout ->
            "Timeout exceeded"

        Http.NetworkError ->
            "Network error"

        Http.BadStatus resp ->
            "Bad status" ++ String.fromInt resp

        Http.BadUrl url ->
            "Malformed url: " ++ url

        Http.BadBody reason ->
            reason



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text "Trivia!" ]
        , viewGif model
        ]


viewGif : Model -> Html Msg
viewGif model =
    case model of
        Failure reason ->
            div []
                [ text ("I could not load the question because of " ++ reason)
                , button [ onClick NextQuestion ] [ text "Try Again!" ]
                ]

        Loading ->
            text "Loading..."

        Success question ->
            div []
                [ button [ onClick NextQuestion, style "display" "block" ] [ text "More Please!" ]
                , p [] [ text question.question ]
                , button [ onClick (Answer True) ] [ text "True" ]
                , button [ onClick (Answer False) ] [ text "False" ]
                ]

        Answered answered ->
            div []
                [ button [ onClick NextQuestion, style "display" "block" ] [ text "More Please!" ]
                , p [] [ text answered.question.question ]
                , p [] [ text (correctNessString answered) ]
                ]


correctNessString : AnsweredQuestion -> String
correctNessString model =
    if correctNess model then
        "Correct!"

    else
        "Wrong :("


correctNess : AnsweredQuestion -> Bool
correctNess model =
    model.question.answer == model.answer



-- HTTP


getRandomCatGif : Cmd Msg
getRandomCatGif =
    Http.get
        { url = "https://opentdb.com/api.php?amount=1&type=boolean"
        , expect = Http.expectJson GotQuestion questionDecoder
        }


type alias BooleanQuestion =
    { question : String
    , answer : Bool
    }


questionDecoder : Decoder BooleanQuestion
questionDecoder =
    map2 BooleanQuestion
        (field "results" (index 0 (field "question" string)))
        (field "results" (index 0 (field "correct_answer" string)) |> D.andThen booleanStringDecoder)


booleanStringDecoder : String -> Decoder Bool
booleanStringDecoder string =
    case string of
        "True" ->
            D.succeed True

        "False" ->
            D.succeed False

        _ ->
            D.fail ("Could not parse " ++ string ++ " as a boolean")
