module Trivia exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as JsDecode
import Random
import Random.List



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }



-- MODEL


type Model
    = Failure String
    | Loading
    | Success Question
    | Answered Question


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading, getQuestion )



-- UPDATE


type Msg
    = NextQuestion
    | GotQuestion (Result Http.Error Question)
    | Answer String
    | Shuffle (List String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NextQuestion ->
            ( Loading, getQuestion )

        GotQuestion result ->
            case result of
                Ok question ->
                    ( Success question, Random.generate Shuffle (Random.List.shuffle (question.answers.correctAnswer :: question.answers.wrongAnswers)) )

                Err reason ->
                    ( Failure (errorToString reason), Cmd.none )

        Answer answer ->
            case model of
                Success question ->
                    ( Answered { question | chosenAnswer = Just answer }, Cmd.none )

                _ ->
                    ( Failure "Answered without a question", Cmd.none )

        Shuffle shuffledAnswers ->
            case model of
                Success question ->
                    let
                        answers =
                            question.answers
                    in
                    ( Success { question | answers = { answers | displayAnswers = Just shuffledAnswers } }, Cmd.none )

                _ ->
                    ( Failure "Tried to shuffle a question without a question", Cmd.none )


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



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text "Trivia!" ]
        , viewQuestion model
        ]


viewQuestion : Model -> Html Msg
viewQuestion model =
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
                , div [] (displayAnswers question)
                ]

        Answered answered ->
            div []
                [ button [ onClick NextQuestion, style "display" "block" ] [ text "More Please!" ]
                , p [] [ text answered.question ]
                , p [] [ text (correctNessString answered) ]
                ]


displayAnswers : Question -> List (Html Msg)
displayAnswers question =
    case question.answers.displayAnswers of
        Just answers ->
            List.map answerButton answers

        Nothing ->
            [ text "No answers to display :*(" ]


answerButton : String -> Html Msg
answerButton answer =
    button [ onClick <| Answer answer ] [ text answer ]


correctNessString : Question -> String
correctNessString question =
    case correctNess question of
        Just True ->
            "Correct!"

        Just False ->
            "Wrong :("

        Nothing ->
            "Didn't get an answer to check"


correctNess : Question -> Maybe Bool
correctNess question =
    Maybe.map (\answer -> question.answers.correctAnswer == answer) question.chosenAnswer



-- HTTP


getQuestion : Cmd Msg
getQuestion =
    Http.get
        { url = "https://opentdb.com/api.php?amount=1"
        , expect = Http.expectJson GotQuestion questionDecoder
        }


type alias Answers =
    { correctAnswer : String
    , wrongAnswers : List String
    , displayAnswers : Maybe (List String)
    }


type alias Question =
    { question : String
    , answers : Answers
    , chosenAnswer : Maybe String
    }


firstQuestion : JsDecode.Decoder a -> JsDecode.Decoder a
firstQuestion =
    JsDecode.field "results" << JsDecode.index 0


questionDecoder : JsDecode.Decoder Question
questionDecoder =
    firstQuestion
        (JsDecode.map3 Question
            (JsDecode.field "question" JsDecode.string)
            (JsDecode.map3 Answers
                (JsDecode.field "correct_answer" JsDecode.string)
                (JsDecode.field "incorrect_answers" (JsDecode.list JsDecode.string))
                (JsDecode.succeed Nothing)
            )
            (JsDecode.succeed Nothing)
        )
