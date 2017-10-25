module Main exposing (..)
import Html exposing(..)
import Html.Attributes exposing(..)
import Html.Events exposing(..)
import String
import Http


-- MODEL

type alias Model =
    {
      courses: List Course,
      name: String,
      id: Maybe Int
    }

type alias Course =
    {
      id: Int,
      name: String
    }

init: (Model, Cmd Msg)
init =
    (
    {
      courses = [],
      name = "",
      id = Nothing
    }
    , Cmd.none
    )


-- UPDATE

type Msg = 
    Input String |
    EditCourse Course |
    DeleteCourse Course |
    Save |
    Cancel

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Input name ->
            ( {model | name = name}, Cmd.none)
        Cancel ->
            ({model | name = "", id = Nothing}, Cmd.none)
        Save ->
            if (String.isEmpty model.name) then
                (model, Cmd.none)
            else
                save model
        EditCourse course ->
            ({model | name = course.name, id = Just course.id}, Cmd.none)
        DeleteCourse course ->
            deleteCourse model course


deleteCourse: Model -> Course -> (Model, Cmd Msg)
deleteCourse model course =
    let
      filteredCourses =
        List.filter (\c -> c.id /= course.id) model.courses
    in
      ({model | courses = filteredCourses}, Cmd.none)



save: Model -> (Model , Cmd Msg)
save model =
    case model.id of
        Nothing -> 
            add model
        Just id ->
            edit model id


add : Model -> (Model, Cmd Msg)
add model =
    let
        newCourse = Course (List.length model.courses) model.name
        newCourses = newCourse :: model.courses

    in
        ({model | courses = newCourses, name = ""}, Cmd.none)

            
edit : Model -> Int -> (Model, Cmd Msg)
edit model id =
    let
        newCourses =
            List.map (\course ->
                        if (course.id == id) then
                            {course | name = model.name}
                        else
                            course
                     )
            model.courses

    in
        ({model | courses = newCourses, name ="", id = Nothing}, Cmd.none)



--VIEW

view: Model -> Html Msg
view model =
  div[]
  [
    viewHeader "Instructor Dashboard",
    courseList model,
    viewFooter
  ]

viewHeader: String -> Html Msg
viewHeader title =
  header[] 
  [ h1[][text title] 
  ]
  

viewFooter: Html Msg
viewFooter =
  footer[]
  [
    a[href "http://elm-lang.org"][text "Powered by Elm"]
  ]

courseList: Model -> Html Msg
courseList model =
    div[]
    [
      courseListHeader model,
      courseForm model,
      courseListBody model
    ]


courseListHeader: Model -> Html Msg
courseListHeader model =
    header []
    [ div [] [text "Course Name"]
    ]

courseForm: Model -> Html Msg
courseForm model =
    Html.form [onSubmit Save]
    [
      input 
      [
        type_ "text",
        value model.name,
        placeholder "Enter Course name",
        onInput Input
      ] [],
      button [type_ "submit" ][text "Save"],
      button [type_ "button", onClick Cancel][text "Cancel"]
    ]

courseListBody: Model -> Html Msg
courseListBody model =
   model.courses
     |> List.sortBy .name
     |> List.map displayCourse
     |> ul []


displayCourse course =
    li []
    [ 
      div[]
      [ 
       a [href "www.google.com"] [text course.name]
      ]
     --, div [class "button-group"]
     --[ button [type_ "button", onClick (EditCourse course)][text "Edit"],
     --  button [type_ "button", onClick (DeleteCourse course)][text "Delete"]
     --]
    ]



main: Program Never Model Msg
main =
    Html.program
        {init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        } 
            



