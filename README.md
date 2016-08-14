# Elm-Debouncer

Use continutations for deboucing!

```elm
import Debouncer
import Task exposing (Task)

type Msg
  = PingFoo
  | PungFoo
  | DebouncerMsg (Debouncer.Msg Msg)

type alias Model =
  { something   : Foo
  , myDebouncer : Debouncer.Model Msg
  }

init : Model
init =
  { something   = initFoo
  , myDebouncer = Debouncer.init
  }

mkCmd : a -> Cmd a
mkCmd = Task.perform (Debug.crash << toString) identity << Task.succeed


update : Msg
      -> Model
      -> (Model, Cmd Msg)
update action model =
  case action of
    PingFoo -> model ! [mkCmd <| DebouncerMsg <| Debouncer.Bounce <| mkCmd PungFoo]
    PungFoo -> pang model ! [] -- the past tense of ping or something
    DebouncerMsg a ->
      let (newDebouncer, eff) = updateDebouncer
                                  (500 * millisecond)
                                  a
                                  model.myDebouncer
      in  { model | myDebouncer = newDebouncer }
        ! [ Cmd.map (\r -> case r of
                             Err a' -> DebouncerMsg a'
                             Ok  a' -> a') eff ]
```
