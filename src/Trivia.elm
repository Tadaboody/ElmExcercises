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


type Model
    = Failure String
    | Loading
    | Success Question
    | Answered Question


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading, getRandomCatGif )



-- UPDATE


type Msg
    = NextQuestion
    | GotQuestion (Result Http.Error Question)
    | Answer String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NextQuestion ->
            ( Loading, getRandomCatGif )

        GotQuestion result ->
            case result of
                Ok question ->
                    ( Success question, Cmd.none )

                Err reason ->
                    ( Failure (errorToString reason), Cmd.none )

        Answer answer ->
            case model of
                Success question ->
                    ( Answered { question | chosenAnswer = Just answer }, Cmd.none )

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
                , div [] (List.map answerButton (question.answers.correctAnswer :: question.answers.wrongAnswers))
                ]

        Answered answered ->
            div []
                [ button [ onClick NextQuestion, style "display" "block" ] [ text "More Please!" ]
                , p [] [ text answered.question ]
                , p [] [ text (correctNessString answered) ]
                ]


answerButton : String -> Html Msg
answerButton answer =
    button [ onClick <| Answer answer ] [ text answer ]


correctNessString : Question -> String
correctNessString model =
    case correctNess model of
        Just True ->
            "Correct!"

        Just False ->
            "Wrong :("

        Nothing ->
            "Huh?"


correctNess : Question -> Maybe Bool
correctNess question =
    case question.chosenAnswer of
        Maybe.Just answer ->
            Just (question.answers.correctAnswer == answer)

        Nothing ->
            Nothing



-- HTTP


getRandomCatGif : Cmd Msg
getRandomCatGif =
    Http.get
        { url = "https://opentdb.com/api.php?amount=1"
        , expect = Http.expectJson GotQuestion questionDecoder
        }


type alias Answers =
    { correctAnswer : String
    , wrongAnswers : List String
    }


type alias Question =
    { question : String
    , answers : Answers
    , chosenAnswer : Maybe String
    }


firstQuestion : Decoder a -> Decoder a
firstQuestion =
    field "results" << index 0


questionDecoder : Decoder Question
questionDecoder =
    D.map3 Question
        (firstQuestion (field "question" D.string))
        (D.map2 Answers
            (firstQuestion (field "correct_answer" D.string))
            (firstQuestion (field "incorrect_answers" (D.list D.string)))
        )
        (D.succeed Nothing)
