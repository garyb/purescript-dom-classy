module DOM.Classy.Event where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Except (runExcept)

import Data.Either (either)
import Data.Foreign (toForeign)
import Data.Maybe (Maybe(..))

import DOM (DOM)
import DOM.Event.Event as EE
import DOM.Event.EventPhase (EventPhase)
import DOM.Event.Types as E
import DOM.HTML.Event.Types as HE
import DOM.Node.Types (Node)
import DOM.Websocket.Event.Types as WS

-- | A class for subtypes of `Event`.
class IsEvent e where
  toEvent :: e -> E.Event
  fromEvent :: E.Event -> Maybe e

-- | The event type.
type_ :: forall e. IsEvent e => e -> E.EventType
type_ = EE.type_ <<< toEvent

-- | The element that was the source of the event.
target :: forall e. IsEvent e => e -> Node
target = EE.target <<< toEvent

-- | The element that the event listener was added to.
currentTarget :: forall e. IsEvent e => e -> Node
currentTarget = EE.currentTarget <<< toEvent

-- | Indicates which phase of the event flow that is currently being processed
-- | for the event.
eventPhase :: forall e. (IsEvent e, Partial) => e -> EventPhase
eventPhase = EE.eventPhase <<< toEvent

-- | The integer value for the current event phase.
eventPhaseIndex :: forall e. IsEvent e => e -> Int
eventPhaseIndex = EE.eventPhaseIndex <<< toEvent

-- | Prevents the event from bubbling up to futher event listeners. Other event
-- | listeners on the current target will still fire.
stopPropagation :: forall e eff. IsEvent e => e -> Eff (dom :: DOM | eff) Unit
stopPropagation = EE.stopPropagation <<< toEvent

-- | Prevents all other listeners for the event from being called. This includes
-- | event listeners added to the current target after the current listener.
stopImmediatePropagation :: forall e eff. IsEvent e => e -> Eff (dom :: DOM | eff) Unit
stopImmediatePropagation = EE.stopImmediatePropagation <<< toEvent

-- | Indicates whether the event will bubble up through the DOM or not.
bubbles :: forall e. IsEvent e => e -> Boolean
bubbles = EE.bubbles <<< toEvent

-- | Indicates whether the event can be cancelled.
cancelable :: forall e. IsEvent e => e -> Boolean
cancelable = EE.cancelable <<< toEvent

-- | Cancels the event if it can be cancelled.
preventDefault :: forall e eff. IsEvent e => e -> Eff (dom :: DOM | eff) Unit
preventDefault = EE.preventDefault <<< toEvent

-- | Indicates whether `preventDefault` was called on the event.
defaultPrevented :: forall e. IsEvent e => e -> Boolean
defaultPrevented = EE.defaultPrevented <<< toEvent

-- | The time in milliseconds between 01/01/1970 and when the event was
-- | dispatched.
timeStamp :: forall e. IsEvent e => e -> Number
timeStamp = EE.timeStamp <<< toEvent

instance isEventEvent :: IsEvent E.Event where
  toEvent = id
  fromEvent = Just

instance isEventCustomEvent :: IsEvent E.CustomEvent where
  toEvent = E.customEventToEvent
  fromEvent = either (const Nothing) Just <<< runExcept <<< E.readCustomEvent <<< toForeign

instance isEventUIEvent :: IsEvent E.UIEvent where
  toEvent = E.uiEventToEvent
  fromEvent = either (const Nothing) Just <<< runExcept <<< E.readUIEvent <<< toForeign

instance isEventFocusEvent :: IsEvent E.FocusEvent where
  toEvent = E.focusEventToEvent
  fromEvent = either (const Nothing) Just <<< runExcept <<< E.readFocusEvent <<< toForeign

instance isEventMouseEvent :: IsEvent E.MouseEvent where
  toEvent = E.mouseEventToEvent
  fromEvent = either (const Nothing) Just <<< runExcept <<< E.readMouseEvent <<< toForeign

instance isEventWheelEvent :: IsEvent E.WheelEvent where
  toEvent = E.wheelEventToEvent
  fromEvent = either (const Nothing) Just <<< runExcept <<< E.readWheelEvent <<< toForeign

instance isEventTouchEvent :: IsEvent E.TouchEvent where
  toEvent = E.touchEventToEvent
  fromEvent = either (const Nothing) Just <<< runExcept <<< E.readTouchEvent <<< toForeign

instance isEventInputEvent :: IsEvent E.InputEvent where
  toEvent = E.inputEventToEvent
  fromEvent = either (const Nothing) Just <<< runExcept <<< E.readInputEvent <<< toForeign

instance isEventKeyboardEvent :: IsEvent E.KeyboardEvent where
  toEvent = E.keyboardEventToEvent
  fromEvent = either (const Nothing) Just <<< runExcept <<< E.readKeyboardEvent <<< toForeign

instance isEventCompositionEvent :: IsEvent E.CompositionEvent where
  toEvent = E.compositionEventToEvent
  fromEvent = either (const Nothing) Just <<< runExcept <<< E.readCompositionEvent <<< toForeign

instance isEventProgressEvent :: IsEvent E.ProgressEvent where
  toEvent = E.progressEventToEvent
  fromEvent = either (const Nothing) Just <<< runExcept <<< E.readProgressEvent <<< toForeign

instance isEventDragEvent :: IsEvent HE.DragEvent where
	toEvent = HE.dragEventToEvent
	fromEvent = either (const Nothing) Just <<< runExcept <<< HE.readDragEvent <<< toForeign

instance isEventErrorEvent :: IsEvent HE.ErrorEvent where
	toEvent = HE.errorEventToEvent
	fromEvent = either (const Nothing) Just <<< runExcept <<< HE.readErrorEvent <<< toForeign

instance isEventHashChangeEvent :: IsEvent HE.HashChangeEvent where
	toEvent = HE.hashChangeEventToEvent
	fromEvent = either (const Nothing) Just <<< runExcept <<< HE.readHashChangeEvent <<< toForeign

instance isEventCloseEvent :: IsEvent WS.CloseEvent where
	toEvent = WS.closeEventToEvent
	fromEvent = either (const Nothing) Just <<< runExcept <<< WS.readCloseEvent <<< toForeign

instance isEventMessageEvent :: IsEvent WS.MessageEvent where
	toEvent = WS.messageEventToEvent
	fromEvent = either (const Nothing) Just <<< runExcept <<< WS.readMessageEvent <<< toForeign
