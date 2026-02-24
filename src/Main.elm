module Main exposing (..)

import Browser
import Html exposing (Html, button, div, h2, text, img)
import Html.Events exposing (onClick)
import Html.Attributes exposing (style, src) -- för css

-- Model (minne, data)
type alias Model =
    { currentQuestion : Int
    , score : Int
    , showResult : Bool
    , selectedAnswer : Maybe String
    }

-- Vad Roboten ska minnas vid start
init : Model
init =
    { currentQuestion = 0
    , score = 0
    , showResult = False
    , selectedAnswer = Nothing
    }

type alias Question =
    { text : String
    , options : List String
    , correctAnswer : String
    , imageUrl : String
    }

questions : List Question
questions =
    [{ text = "In which country did I do my first marathon?"
    , options = ["Czech Republic", "Germany", "Turkey", "South Africa"]
    , correctAnswer = "Czech Republic"
    , imageUrl = "photos/IMG_17291.jpeg"
    }
   , { text = "Who is my all time favourite hockey player"
    , options = ["Nathan McKinnon", "Peter Forsberg", "Mats Sundin", "Wayne Gretzky"]
    , correctAnswer = "Peter Forsberg"
    , imageUrl = "photos/IMG_5222.JPG"
    }
   , { text = "Which team won when i watched Formula 1 live in Italy last year"
    , options = ["Ferrari", "McLaren", "Mercedes", "Red Bull"]
    , correctAnswer = "Red Bull"
    , imageUrl = "photos/IMG_8792.jpeg"
    }
   , { text = "Which backend language am I currently learning alongside Elm?"
    , options = ["Python", "C#", "Java", "PHP"]
    , correctAnswer = "Java"
    , imageUrl = "photos/IMG_8932.jpeg"
    }
   , { text = "What profession did i have before i chose to start studying again?"
    , options = ["Construction worker", "Professional golfer", "Nurse assistant", "Comedian"]
    , correctAnswer = "Nurse assistant"
    , imageUrl = "photos/geraltme.png"
    }
   , { text = "Why is my background in healthcare an asset in web development?"
    , options = ["High stress tolerance and problem solving", "I know medicine names", "I like wearing scrubs", "I know HLR"]
    , correctAnswer = "High stress tolerance and problem solving"
    , imageUrl = "photos/IMG_5223.JPG"
    }
   , { text = "Training for a marathon requires the same skill as coding, which one?"
    , options = ["Sprinting", "Luck", "Goal intended", "Eating pasta"]
    , correctAnswer = "Goal intended"
    , imageUrl = "photos/IMG_17211.jpg"
    }
   , { text = "What is my top priority during my LIA at Webbhuset?"
    , options = ["To learn and contribute to real projects", "To hide in the corner", "To rewrite everything in Python", "To play video games"]
    , correctAnswer = "To learn and contribute to real projects"
    , imageUrl = "photos/IMG_1699.jpeg"
    }
   , { text = "How do I plan to contribute to the team at Webbhuset?"
    , options = ["By being a proactive and curious learner", "By working only on my own", "By avoiding difficult tasks", "By just observing others"]
    , correctAnswer = "By being a proactive and curious learner"
    , imageUrl = "photos/IMG_9397.JPG"
    }
   , { text = "Are you ready to choose me for an internship at Webbhuset?"
    , options = ["Yes!", "Absolutely!", "Definitely!", "100%!"]
    , correctAnswer = "Yes!"
    , imageUrl = "photos/IMG_8077.jpeg"
    }
    ]

-- Update (händelser som uppdaterar state)
type Msg
    = SelectAnswer String
    | NextQuestion
    | Reset

update : Msg -> Model -> Model
update msg model =
    case msg of
        SelectAnswer choice ->
            -- Vi tillåter bara ett val per fråga
            if model.selectedAnswer == Nothing then
                let
                    currentQ = List.head (List.drop model.currentQuestion questions)
                    isCorrect = case currentQ of
                        Just q -> q.correctAnswer == choice
                        Nothing -> False

                    newScore = if isCorrect then model.score + 1 else model.score
                in
                { model | selectedAnswer = Just choice, score = newScore }
            else
                model

        NextQuestion ->
            let next = model.currentQuestion + 1 in
            if next >= 10 then
                { model | showResult = True, selectedAnswer = Nothing }
            else
                { model | currentQuestion = next, selectedAnswer = Nothing }

        Reset -> init


-- View (Vad som visas)
view : Model -> Html Msg
view model =
    div
        [ style "display" "flex", style "justify-content" "center", style "align-items" "center", style "height" "100vh", style "background-color" "#f0f2f5", style "font-family" "sans-serif" ]
        [ div
            [ style "background" "white", style "padding" "40px", style "border-radius" "12px", style "box-shadow" "0 4px 6px rgba(0,0,0,0.1)", style "text-align" "center", style "max-width" "500px", style "width" "100%" ]
            (if model.showResult then
                [ h2 [] [ text "Quiz Complete!" ]
                , div [ style "font-size" "1.2em", style "margin-bottom" "20px" ]
                    [ text ("Good job! Your score was: " ++ String.fromInt model.score ++ " out of 10") ]
                , button [ onClick Reset, btnStyle "#4A90E2" ] [ text "Play again" ]
                ]
             else
                case List.head (List.drop model.currentQuestion questions) of
                    Just currentQ ->
                        [ -- Progress Bar
                          div [ style "width" "100%", style "background-color" "#eee", style "height" "8px", style "border-radius" "4px", style "margin-bottom" "20px" ]
                            [ div [ style "width" (String.fromInt (model.currentQuestion * 10) ++ "%"), style "background-color" "#4A90E2", style "height" "100%", style "transition" "width 0.3s" ] [] ]

                        -- Layout: Text till vänster, bild till höger
                        , div [ style "display" "flex", style "align-items" "center", style "justify-content" "space-between", style "margin-bottom" "20px", style "text-align" "left" ]
                            [ div [ style "flex" "1", style "padding-right" "15px" ]
                                [ h2 [ style "color" "#333", style "margin" "0" ] [ text ("Question " ++ String.fromInt (model.currentQuestion + 1)) ]
                                , div [ style "margin-top" "10px", style "font-weight" "bold" ] [ text currentQ.text ]
                                ]
                            , if currentQ.imageUrl /= "" then
                                img [ src currentQ.imageUrl
                                , style "width" "200px"
                                , style "height" "200px"
                                , style "border-radius" "50%"
                                , style "object-fit" "cover"
                                , style "image-orientation" "from-image"
                                ] []
                              else text ""
                            ]

                        , div [] (List.map (viewOption currentQ.correctAnswer model.selectedAnswer) currentQ.options)

                        -- Nästa-knapp som bara syns när man svarat
                        , if model.selectedAnswer /= Nothing then
                            button [ onClick NextQuestion, btnStyle "#4A90E2" ] [ text "Next Question →" ]
                          else text ""
                        ]
                    Nothing -> [ text "Loading question.." ]
            )
        ]

viewOption : String -> Maybe String -> String -> Html Msg
viewOption correct selected choice =
    let
        bgColor =
            case selected of
                Nothing -> "white"
                Just sel ->
                    if choice == sel then
                        if choice == correct then "#d4edda" else "#f8d7da" -- Grön om rätt, röd om fel
                    else if choice == correct then "#d4edda" -- Visa rätt svar även om man valde fel
                    else "white"
    in
    button
        [ onClick (SelectAnswer choice)
        , style "display" "block", style "width" "100%", style "padding" "12px", style "margin" "8px 0"
        , style "border" "1px solid #ddd", style "border-radius" "6px"
        , style "background-color" bgColor
        , style "cursor" (if selected == Nothing then "pointer" else "default")
        ]
        [ text choice ]

btnStyle : String -> Html.Attribute Msg
btnStyle color =
    style "display" "block"
    |> (\_ -> style "width" "100%")
    |> (\_ -> style "padding" "12px")
    |> (\_ -> style "margin-top" "20px")
    |> (\_ -> style "border" "none")
    |> (\_ -> style "border-radius" "6px")
    |> (\_ -> style "background-color" color)
    |> (\_ -> style "color" "white")
    |> (\_ -> style "cursor" "pointer")
    |> (\_ -> style "font-weight" "bold")

main =
    Browser.sandbox { init = init, update = update, view = view}