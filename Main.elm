import Effects exposing (Never)
import SeaportPair exposing (init, update, view)
import StartApp
import Task


app =
  StartApp.start
    { init = init "POL" "POD"
    , update = update
    , view = view
    , inputs = []
    }


main =
  app.html


port tasks : Signal (Task.Task Never ())
port tasks =
  app.tasks
