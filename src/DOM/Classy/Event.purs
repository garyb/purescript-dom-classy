module DOM.Classy.Event where

import Prelude
import Control.Monad.Except (runExcept)
import Data.Either (either)
import Data.Foreign (toForeign)
import Data.Maybe (Maybe(..))
import DOM.Event.Types as E
import DOM.HTML.Event.Types as HE
import DOM.Websocket.Event.Types as WS

-- | A class for subtypes of `Event`.
class IsEvent e where
  toEvent :: e -> E.Event
  fromEvent :: E.Event -> Maybe e

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
