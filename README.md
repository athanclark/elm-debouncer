# Elm-Debouncer

HEED ALL YEE WHO HATH WANT TO DEBOUNCE STUFF, because it's chill and we
all know what happens to chill people >_>

Basically the idea is simple, a `Debouncer` is a stateful component
that manages _when_ an arbitrary action of your own should be issued.
The `Deboucer` itself is basically the model for the delay, so you
should have a separate one for different isolated miniscule components
of your applicaiton - like one for each text box.

To let the debouncer actually issue your actions for you, you need to
delegate that work to the updating function - `updateDebouncer`. Note
that it needs the delay and action to issue as read-only data - we won't
be changing the delay or action to issue dynamically here.

When viewing this system correctly, we should consider the debouncer somewhat
like a _service_ - when we issue the `Start` action / message, we're actually
asking the debouncer to issue the main action. from this perspective,
`DebouncerMsg` (with `Start` as the only exposed constructor) as our input interface -
the interface we can solicit with queries, while `DebouncerResults` is our output
interface - representing either more debouncing or an issuance of the main
action we wish to debounce. Luckily this output channel is very easy to handle,
using `handleDebouncerResults` all we need is something that manages our inputs,
so to say that we understand how to handle a `DebouncerMsg` means we can handle
a `DebouncerResults`. Sorry :\ I'll explain more in a second.

Okay, so here's an example:

```elm
type alias MyModel =
  { somethingDank : Dank
  , myDebouncer   : Debouncer String
  }

-- setting up a debouncer which stagnates String
-- data changes

initMyModel : MyModel
initMyModel =
  { somethingDank = ayyLmao
  , myDebouncer   = initDebouncer
  }
  
type MyAction
  = DankAction DankAction
  | DebouncerMsg (DebouncerMsg String)
  
updateMyModel : MyAction
             -> MyModel
             -> (MyModel, Cmd MyAction)
updateMyModel action model =
  case action of
    DankAction a ->
      let (awwYiss, eff) = updateDank a model.somethingDank
      in  ( { model | somethingDank = awwYiss }
          , Cmd.map DankAction eff
          )
    DebouncerMsg a ->
      let (newDebouncer, eff) = updateDebouncer
                                  (500 * millisecond)
                                  (\s -> DankAction <| SomeDankAction s)
                                  -- this action _needs_ a string
                                  a
                                  model.myDebouncer
      in  ( { model | myDebouncer = newDebouncer }
          , Cmd.map (handleDebouncerResults DebouncerMsg) eff
          ) -- tie up the system
      -- note that `eff : DebouncerResults String MyAction`
          
viewMyModel : MyModel
           -> Html MyAction
viewMyModel model =
  div []
    [ h2 [] [text "Dank, right?"]
    , input [ type' "text"
            , on "input" ( targetValue `Json.Decode.andThen` \s ->
                           Json.Decode.succeed (Bounce s) -- plumbing the string
                         )
            , value model.somethingDank.dankness
            ] []
    ]
```
