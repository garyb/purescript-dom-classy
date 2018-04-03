module DOM.Classy.Event.EventTarget
  ( class EventTarget
  , toEventTarget
  , eventListener
  , addEventListener
  , removeEventListener
  , dispatchEvent
  , module Exports
  ) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import DOM (DOM)
import DOM.Classy.Event (class IsEvent, fromEvent, toEvent)
import DOM.Event.EventTarget (EventListener) as Exports
import DOM.Event.EventTarget as E
import DOM.Event.Types (EventTarget, EventType)
import DOM.HTML.Types as H
import DOM.Node.Types as N
import Data.Maybe (maybe)
import Data.Monoid (mempty)
import Unsafe.Coerce as U

-- | A class for subtypes of `EventTarget`.
class EventTarget t where
  toEventTarget :: t -> EventTarget

-- | Creates an event listener from a normal `Eff`-based callback function,
-- | automatically converting to an event subtype. If the conversion fails due
-- | to a received event not matching the expected type it will be ignored.
eventListener
  :: forall event eff a
   . IsEvent event
  => (event -> Eff eff a)
  -> E.EventListener eff
eventListener handler =
  E.eventListener (maybe mempty (void <<< handler) <<< fromEvent)

-- | Adds a listener to an event target. The boolean argument indicates whether
-- | the listener should be added for the "capture" phase.
addEventListener
  :: forall t eff
   . EventTarget t
  => EventType
  -> E.EventListener (dom :: DOM | eff)
  -> Boolean
  -> t
  -> Eff (dom :: DOM | eff) Unit
addEventListener eventType listener capture =
  E.addEventListener eventType listener capture <<< toEventTarget

-- | Removes a listener to an event target. The boolean argument indicates
-- | whether the listener should be removed for the "capture" phase.
removeEventListener
  :: forall t eff
   . EventTarget t
  => EventType
  -> E.EventListener (dom :: DOM | eff)
  -> Boolean
  -> t
  -> Eff (dom :: DOM | eff) Unit
removeEventListener eventType listener capture =
  E.removeEventListener eventType listener capture <<< toEventTarget

-- | Dispatches an event from an event target.
dispatchEvent
  :: forall t event eff
   . EventTarget t
  => IsEvent event
  => event
  -> t
  -> Eff (dom :: DOM, err :: EXCEPTION | eff) Boolean
dispatchEvent event =
  E.dispatchEvent (toEvent event) <<< toEventTarget

instance eventTargetNode :: EventTarget N.Node where
  toEventTarget = U.unsafeCoerce

instance eventTargetDocument :: EventTarget N.Document where
  toEventTarget = U.unsafeCoerce

instance eventTargetElement :: EventTarget N.Element where
  toEventTarget = U.unsafeCoerce

instance eventTargetCharacterData :: EventTarget N.CharacterData where
  toEventTarget = U.unsafeCoerce

instance eventTargetText :: EventTarget N.Text where
  toEventTarget = U.unsafeCoerce

instance eventTargetComment :: EventTarget N.Comment where
  toEventTarget = U.unsafeCoerce

instance eventTargetProcessingInstruction :: EventTarget N.ProcessingInstruction where
  toEventTarget = U.unsafeCoerce

instance eventTargetDocumentFragment :: EventTarget N.DocumentFragment where
  toEventTarget = U.unsafeCoerce

instance eventTargetDocumentType :: EventTarget N.DocumentType where
  toEventTarget = U.unsafeCoerce

instance eventTargetWindow :: EventTarget H.Window where
  toEventTarget = U.unsafeCoerce

instance eventTargetHTMLDocument :: EventTarget H.HTMLDocument where
  toEventTarget = U.unsafeCoerce

instance eventTargetHTMLElement :: EventTarget H.HTMLElement where
  toEventTarget = U.unsafeCoerce

instance eventTargetHTMLHtmlElement :: EventTarget H.HTMLHtmlElement where
  toEventTarget = U.unsafeCoerce

instance eventTargetHTMLHeadElement :: EventTarget H.HTMLHeadElement where
  toEventTarget = U.unsafeCoerce

instance eventTargetHTMLTitleElement :: EventTarget H.HTMLTitleElement where
  toEventTarget = U.unsafeCoerce

instance eventTargetHTMLBaseElement :: EventTarget H.HTMLBaseElement where
  toEventTarget = U.unsafeCoerce

instance eventTargetHTMLLinkElement :: EventTarget H.HTMLLinkElement where
  toEventTarget = U.unsafeCoerce

instance eventTargetHTMLMetaElement :: EventTarget H.HTMLMetaElement where
  toEventTarget = U.unsafeCoerce

instance eventTargetHTMLStyleElement :: EventTarget H.HTMLStyleElement where
  toEventTarget = U.unsafeCoerce

instance eventTargetHTMLBodyElement :: EventTarget H.HTMLBodyElement where
  toEventTarget = U.unsafeCoerce

instance eventTargetHTMLHeadingElement :: EventTarget H.HTMLHeadingElement where
  toEventTarget = U.unsafeCoerce

instance eventTargetHTMLParagraphElement :: EventTarget H.HTMLParagraphElement where
  toEventTarget = U.unsafeCoerce

instance eventTargetHTMLHRElement :: EventTarget H.HTMLHRElement where
  toEventTarget = U.unsafeCoerce

instance eventTargetHTMLPreElement :: EventTarget H.HTMLPreElement where
  toEventTarget = U.unsafeCoerce

instance eventTargetHTMLQuoteElement :: EventTarget H.HTMLQuoteElement where
  toEventTarget = U.unsafeCoerce

instance eventTargetHTMLOListElement :: EventTarget H.HTMLOListElement where
  toEventTarget = U.unsafeCoerce

instance eventTargetHTMLUListElement :: EventTarget H.HTMLUListElement where
  toEventTarget = U.unsafeCoerce

instance eventTargetHTMLLIElement :: EventTarget H.HTMLLIElement where
  toEventTarget = U.unsafeCoerce

instance eventTargetHTMLDListElement :: EventTarget H.HTMLDListElement where
  toEventTarget = U.unsafeCoerce

instance eventTargetHTMLDivElement :: EventTarget H.HTMLDivElement where
  toEventTarget = U.unsafeCoerce

instance eventTargetHTMLAnchorElement :: EventTarget H.HTMLAnchorElement where
  toEventTarget = U.unsafeCoerce

instance eventTargetHTMLDataElement :: EventTarget H.HTMLDataElement where
  toEventTarget = U.unsafeCoerce

instance eventTargetHTMLTimeElement :: EventTarget H.HTMLTimeElement where
  toEventTarget = U.unsafeCoerce

instance eventTargetHTMLSpanElement :: EventTarget H.HTMLSpanElement where
  toEventTarget = U.unsafeCoerce

instance eventTargetHTMLBRElement :: EventTarget H.HTMLBRElement where
  toEventTarget = U.unsafeCoerce

instance eventTargetHTMLModElement :: EventTarget H.HTMLModElement where
  toEventTarget = U.unsafeCoerce

instance eventTargetHTMLImageElement :: EventTarget H.HTMLImageElement where
  toEventTarget = U.unsafeCoerce

instance eventTargetHTMLIFrameElement :: EventTarget H.HTMLIFrameElement where
  toEventTarget = U.unsafeCoerce

instance eventTargetHTMLEmbedElement :: EventTarget H.HTMLEmbedElement where
  toEventTarget = U.unsafeCoerce

instance eventTargetHTMLObjectElement :: EventTarget H.HTMLObjectElement where
  toEventTarget = U.unsafeCoerce

instance eventTargetHTMLParamElement :: EventTarget H.HTMLParamElement where
  toEventTarget = U.unsafeCoerce

instance eventTargetHTMLMediaElement :: EventTarget H.HTMLMediaElement where
  toEventTarget = U.unsafeCoerce

instance eventTargetHTMLAudioElement :: EventTarget H.HTMLAudioElement where
  toEventTarget = U.unsafeCoerce

instance eventTargetHTMLVideoElement :: EventTarget H.HTMLVideoElement where
  toEventTarget = U.unsafeCoerce

instance eventTargetHTMLSourceElement :: EventTarget H.HTMLSourceElement where
  toEventTarget = U.unsafeCoerce

instance eventTargetHTMLTrackElement :: EventTarget H.HTMLTrackElement where
  toEventTarget = U.unsafeCoerce

instance eventTargetHTMLMapElement :: EventTarget H.HTMLMapElement where
  toEventTarget = U.unsafeCoerce

instance eventTargetHTMLAreaElement :: EventTarget H.HTMLAreaElement where
  toEventTarget = U.unsafeCoerce

instance eventTargetHTMLTableElement :: EventTarget H.HTMLTableElement where
  toEventTarget = U.unsafeCoerce

instance eventTargetHTMLTableCaptionElement :: EventTarget H.HTMLTableCaptionElement where
  toEventTarget = U.unsafeCoerce

instance eventTargetHTMLTableColElement :: EventTarget H.HTMLTableColElement where
  toEventTarget = U.unsafeCoerce

instance eventTargetHTMLTableSectionElement :: EventTarget H.HTMLTableSectionElement where
  toEventTarget = U.unsafeCoerce

instance eventTargetHTMLTableRowElement :: EventTarget H.HTMLTableRowElement where
  toEventTarget = U.unsafeCoerce

instance eventTargetHTMLTableCellElement :: EventTarget H.HTMLTableCellElement where
  toEventTarget = U.unsafeCoerce

instance eventTargetHTMLTableDataCellElement :: EventTarget H.HTMLTableDataCellElement where
  toEventTarget = U.unsafeCoerce

instance eventTargetHTMLTableHeaderCellElement :: EventTarget H.HTMLTableHeaderCellElement where
  toEventTarget = U.unsafeCoerce

instance eventTargetHTMLFormElement :: EventTarget H.HTMLFormElement where
  toEventTarget = U.unsafeCoerce

instance eventTargetHTMLLabelElement :: EventTarget H.HTMLLabelElement where
  toEventTarget = U.unsafeCoerce

instance eventTargetHTMLInputElement :: EventTarget H.HTMLInputElement where
  toEventTarget = U.unsafeCoerce

instance eventTargetHTMLButtonElement :: EventTarget H.HTMLButtonElement where
  toEventTarget = U.unsafeCoerce

instance eventTargetHTMLSelectElement :: EventTarget H.HTMLSelectElement where
  toEventTarget = U.unsafeCoerce

instance eventTargetHTMLDataListElement :: EventTarget H.HTMLDataListElement where
  toEventTarget = U.unsafeCoerce

instance eventTargetHTMLOptGroupElement :: EventTarget H.HTMLOptGroupElement where
  toEventTarget = U.unsafeCoerce

instance eventTargetHTMLOptionElement :: EventTarget H.HTMLOptionElement where
  toEventTarget = U.unsafeCoerce

instance eventTargetHTMLTextAreaElement :: EventTarget H.HTMLTextAreaElement where
  toEventTarget = U.unsafeCoerce

instance eventTargetHTMLKeygenElement :: EventTarget H.HTMLKeygenElement where
  toEventTarget = U.unsafeCoerce

instance eventTargetHTMLOutputElement :: EventTarget H.HTMLOutputElement where
  toEventTarget = U.unsafeCoerce

instance eventTargetHTMLProgressElement :: EventTarget H.HTMLProgressElement where
  toEventTarget = U.unsafeCoerce

instance eventTargetHTMLMeterElement :: EventTarget H.HTMLMeterElement where
  toEventTarget = U.unsafeCoerce

instance eventTargetHTMLFieldSetElement :: EventTarget H.HTMLFieldSetElement where
  toEventTarget = U.unsafeCoerce

instance eventTargetHTMLLegendElement :: EventTarget H.HTMLLegendElement where
  toEventTarget = U.unsafeCoerce

instance eventTargetHTMLScriptElement :: EventTarget H.HTMLScriptElement where
  toEventTarget = U.unsafeCoerce

instance eventTargetHTMLTemplateElement :: EventTarget H.HTMLTemplateElement where
  toEventTarget = U.unsafeCoerce

instance eventTargetHTMLCanvasElement :: EventTarget H.HTMLCanvasElement where
  toEventTarget = U.unsafeCoerce
