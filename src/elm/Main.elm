module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (placeholder, type_, value, class)
import Html.Events exposing (..)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : ( Model, Cmd Msg )
init =
    ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- MODEL


type alias Person =
    { id : Int
    , name : String
    , count : Int
    }


type alias Persons =
    List Person


type alias PersonForm =
    { name : String }


type alias Model =
    { persons : Persons
    , personForm : PersonForm
    }


model : Model
model =
    { persons = []
    , personForm =
        { name = "" }
    }



-- UPDATE


type Msg
    = NewName String
    | Increment Person
    | AddPerson


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewName name ->
            let
                oldPersonForm =
                    model.personForm

                newPersonForm =
                    { oldPersonForm | name = name }
            in
                ( { model
                    | personForm = newPersonForm
                  }
                , Cmd.none
                )

        AddPerson ->
            case model.personForm.name of
                "" ->
                    ( model, Cmd.none )

                _ ->
                    let
                        maxId =
                            model.persons
                                |> List.map (\p -> p.id)
                                |> List.maximum
                                |> Maybe.withDefault 0

                        newPerson =
                            { id = maxId + 1
                            , name = model.personForm.name
                            , count = 0
                            }
                    in
                        ( { model
                            | persons = newPerson :: model.persons
                            , personForm = { name = "" }
                          }
                        , Cmd.none
                        )

        Increment person ->
            let
                updateCounterForPerson : Person -> Person -> Person
                updateCounterForPerson personToUpdate person =
                    if person == personToUpdate then
                        { person | count = person.count + 1 }
                    else
                        person

                updatedPersons =
                    List.map (updateCounterForPerson person) model.persons
            in
                ( { model | persons = updatedPersons }
                , Cmd.none
                )



-- VIEW


personToHtml : Person -> Html Msg
personToHtml person =
    div
        [ class "person" ]
        [ div [ class "person__add" ] [ button [ class "btn", onClick <| Increment person ] [ text "+" ] ]
        , div [ class "person__name" ] [ text <| " Name: " ++ person.name ]
        , div [ class "person__count" ] [ text <| " Count: " ++ (toString person.count) ]
        ]


personListing : List Person -> Html Msg
personListing persons =
    div [ class "persons" ] <| List.map personToHtml persons


personForm : String -> Html Msg
personForm name =
    div [ class "person-input" ]
        [ input [ class "person-input__name", type_ "text", placeholder "Name", onInput NewName, value name ] []
        , button [ class "btn", onClick AddPerson ] [ text "Add" ]
        ]


totalView : List Person -> Html Msg
totalView persons =
    div [ class "total" ]
        [ text "Total: "
        , persons
            |> List.map (\p -> p.count)
            |> List.sum
            |> toString
            |> text
        ]


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ header [ class "header" ] [ h1 [] [ text "🍹 beverage-counter" ] ]
        , main_ [ class "main" ]
            [ totalView model.persons
            , personListing model.persons
            , personForm model.personForm.name
            ]
        ]
