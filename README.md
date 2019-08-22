# Elm-Debouncer

Use continutations for deboucing!

```elm
import Debounce
import Task exposing (Task)
import Time exposing (millisToPosix)

type Msg
  = PingFoo
  | PungFoo
  | DebouncerMsg (Debounce.Msg Msg)

type alias Model =
  { something   : Foo
  , myDebouncer : Debounce.Model Msg
  }

init : Model
init =
  { something   = initFoo
  , myDebouncer = Debounce.init
  }

mkCmd : a -> Cmd a
mkCmd = Task.perform (Debug.crash << toString) identity << Task.succeed


update : Msg
      -> Model
      -> (Model, Cmd Msg)
update action model =
  case action of
    PingFoo -> model ! [mkCmd <| DebouncerMsg <| Debounce.Bounce <| mkCmd PungFoo]
    PungFoo -> pang model ! [] -- the past tense of ping or something
    DebouncerMsg a ->
      let (newDebouncer, eff) = updateDebouncer
                                  (millisToPosix 500)
                                  a
                                  model.myDebouncer
      in  { model | myDebouncer = newDebouncer }
        ! [ Cmd.map (\r -> case r of
                             Err a' -> DebouncerMsg a'
                             Ok  a' -> a') eff ]
```
