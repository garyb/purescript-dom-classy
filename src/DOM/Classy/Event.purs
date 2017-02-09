module DOM.Classy.Event where

import Prelude

import Control.Monad.Eff (Eff)

import Data.Maybe (Maybe(..))

import DOM (DOM)
import DOM.Classy.Util (fromAny)
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
  fromEvent = fromAny E.readCustomEvent

instance isEventUIEvent :: IsEvent E.UIEvent where
  toEvent = E.uiEventToEvent
  fromEvent = fromAny E.readUIEvent

instance isEventFocusEvent :: IsEvent E.FocusEvent where
  toEvent = E.focusEventToEvent
  fromEvent = fromAny E.readFocusEvent

instance isEventMouseEvent :: IsEvent E.MouseEvent where
  toEvent = E.mouseEventToEvent
  fromEvent = fromAny E.readMouseEvent

instance isEventWheelEvent :: IsEvent E.WheelEvent where
  toEvent = E.wheelEventToEvent
  fromEvent = fromAny E.readWheelEvent

instance isEventTouchEvent :: IsEvent E.TouchEvent where
  toEvent = E.touchEventToEvent
  fromEvent = fromAny E.readTouchEvent

instance isEventInputEvent :: IsEvent E.InputEvent where
  toEvent = E.inputEventToEvent
  fromEvent = fromAny E.readInputEvent

instance isEventKeyboardEvent :: IsEvent E.KeyboardEvent where
  toEvent = E.keyboardEventToEvent
  fromEvent = fromAny E.readKeyboardEvent

instance isEventCompositionEvent :: IsEvent E.CompositionEvent where
  toEvent = E.compositionEventToEvent
  fromEvent = fromAny E.readCompositionEvent

instance isEventProgressEvent :: IsEvent E.ProgressEvent where
  toEvent = E.progressEventToEvent
  fromEvent = fromAny E.readProgressEvent

instance isEventDragEvent :: IsEvent HE.DragEvent where
	toEvent = HE.dragEventToEvent
	fromEvent = fromAny HE.readDragEvent

instance isEventErrorEvent :: IsEvent HE.ErrorEvent where
	toEvent = HE.errorEventToEvent
	fromEvent = fromAny HE.readErrorEvent

instance isEventHashChangeEvent :: IsEvent HE.HashChangeEvent where
	toEvent = HE.hashChangeEventToEvent
	fromEvent = fromAny HE.readHashChangeEvent

instance isEventCloseEvent :: IsEvent WS.CloseEvent where
	toEvent = WS.closeEventToEvent
	fromEvent = fromAny WS.readCloseEvent

instance isEventMessageEvent :: IsEvent WS.MessageEvent where
	toEvent = WS.messageEventToEvent
	fromEvent = fromAny WS.readMessageEvent
