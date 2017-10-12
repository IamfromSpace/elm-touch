effect module Touch
    where { subscription = MySub }
    exposing
        ( Touch
        , touches
        , moves
        , starts
        , ends
        , cancels
        )

{-| This library lets you listen to global touch events. This is useful
for a couple tricky scenarios including:

  - Detecting a touch outside the current component.
  - Supporting drag-and-drop interactions.

# Touch Touch
@docs Touch, touches

# Subscriptions
@docs moves, starts, ends, cancels

-}

import Dict
import Dom.LowLevel as Dom
import Json.Decode as Json
import Process
import Task exposing (Task)


-- POSITIONS


{-| The position of the touch point relative to the whole document. So if you are
scrolled down a bunch, you are still getting a coordinate relative to the
very top left corner of the *whole* document.
-}
type alias Touch =
    { x : Float
    , y : Float
    , id : Int
    }


changedTouchesKey : String
changedTouchesKey =
    "changedTouches"


{-| The decoder used to extract a list of `Touch`s from a JavaScript touch event.
-}
touches : Json.Decoder (List Touch)
touches =
    -- TODO: This is pretty silly, and only works with 5 or less touches.
    -- `Json.list` fails to decode the array of touches, because it is not
    -- an `instanceof Array`, it is a `TouchList`.
    -- note that Array.isArray(touchList) also returns false.
    Json.at [ changedTouchesKey, "length" ] Json.int
        |> Json.andThen
            (\length ->
                case length of
                    1 ->
                        Json.map (\x -> [ x ]) (touch 0)

                    2 ->
                        Json.map2 (\a b -> [ a, b ]) (touch 0) (touch 1)

                    3 ->
                        Json.map3 (\a b c -> [ a, b, c ]) (touch 0) (touch 1) (touch 2)

                    4 ->
                        Json.map4 (\a b c d -> [ a, b, c, d ]) (touch 0) (touch 1) (touch 2) (touch 3)

                    _ ->
                        Json.map5 (\a b c d e -> [ a, b, c, d, e ]) (touch 0) (touch 1) (touch 2) (touch 3) (touch 4)
            )


touch : Int -> Json.Decoder Touch
touch i =
    Json.map3 Touch
        (Json.at [ changedTouchesKey, toString i, "pageX" ] Json.float)
        (Json.at [ changedTouchesKey, toString i, "pageY" ] Json.float)
        (Json.at [ changedTouchesKey, toString i, "identifier" ] Json.int)


{-| Subscribe to touch moves anywhere on screen. It is best to unsubscribe if
you do not need these events. Otherwise you will handle a bunch of events for
no benefit.
-}
moves : (List Touch -> msg) -> Sub msg
moves tagger =
    subscription (MySub "touchmove" tagger)


{-| Get a position whenever the user touches the page.
-}
starts : (List Touch -> msg) -> Sub msg
starts tagger =
    subscription (MySub "touchstart" tagger)


{-| Get a position whenever the user lifts his/her finger.
-}
ends : (List Touch -> msg) -> Sub msg
ends tagger =
    subscription (MySub "touchend" tagger)


{-| Get a position whenever a touch event is canceled.
-}
cancels : (List Touch -> msg) -> Sub msg
cancels tagger =
    subscription (MySub "touchcancel" tagger)



-- SUBSCRIPTIONS


type MySub msg
    = MySub String (List Touch -> msg)


subMap : (a -> b) -> MySub a -> MySub b
subMap func (MySub category tagger) =
    MySub category (tagger >> func)



-- EFFECT MANAGER STATE


type alias State msg =
    Dict.Dict String (Watcher msg)


type alias Watcher msg =
    { taggers : List (List Touch -> msg)
    , pid : Process.Id
    }



-- CATEGORIZE SUBSCRIPTIONS


type alias SubDict msg =
    Dict.Dict String (List (List Touch -> msg))


categorize : List (MySub msg) -> SubDict msg
categorize subs =
    categorizeHelp subs Dict.empty


categorizeHelp : List (MySub msg) -> SubDict msg -> SubDict msg
categorizeHelp subs subDict =
    case subs of
        [] ->
            subDict

        (MySub category tagger) :: rest ->
            categorizeHelp rest <|
                Dict.update category (categorizeHelpHelp tagger) subDict


categorizeHelpHelp : a -> Maybe (List a) -> Maybe (List a)
categorizeHelpHelp value maybeValues =
    case maybeValues of
        Nothing ->
            Just [ value ]

        Just values ->
            Just (value :: values)



-- EFFECT MANAGER


init : Task Never (State msg)
init =
    Task.succeed Dict.empty


type alias Msg =
    { category : String
    , touches : List Touch
    }


(&>) : Task a b -> Task a c -> Task a c
(&>) t1 t2 =
    Task.andThen (\_ -> t2) t1


onEffects : Platform.Router msg Msg -> List (MySub msg) -> State msg -> Task Never (State msg)
onEffects router newSubs oldState =
    let
        leftStep category { pid } task =
            Process.kill pid &> task

        bothStep category { pid } taggers task =
            task
                |> Task.andThen (\state -> Task.succeed (Dict.insert category (Watcher taggers pid) state))

        rightStep category taggers task =
            let
                tracker =
                    Dom.onDocument category touches (Platform.sendToSelf router << Msg category)
            in
                task
                    |> Task.andThen
                        (\state ->
                            Process.spawn tracker
                                |> Task.andThen (\pid -> Task.succeed (Dict.insert category (Watcher taggers pid) state))
                        )
    in
        Dict.merge
            leftStep
            bothStep
            rightStep
            oldState
            (categorize newSubs)
            (Task.succeed Dict.empty)


onSelfMsg : Platform.Router msg Msg -> Msg -> State msg -> Task Never (State msg)
onSelfMsg router { category, touches } state =
    case Dict.get category state of
        Nothing ->
            Task.succeed state

        Just { taggers } ->
            let
                send tagger =
                    Platform.sendToApp router (tagger touches)
            in
                Task.sequence (List.map send taggers)
                    &> Task.succeed state
