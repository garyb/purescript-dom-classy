module DOM.Classy.EventTarget
  ( class EventTarget
  , addEventListener
  , removeEventListener
  , dispatchEvent
  ) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import DOM (DOM)
import DOM.Classy.Event (class IsEvent, toEvent)
import DOM.Event.EventTarget as E
import DOM.Event.Types (EventType)
import DOM.HTML.Types as H
import DOM.Node.Types as N
import Unsafe.Coerce (unsafeCoerce)


class EventTarget t where
  addEventListener :: forall eff. EventType -> E.EventListener (dom :: DOM | eff) -> Boolean -> t -> Eff (dom :: DOM | eff) Unit
  removeEventListener :: forall eff. EventType -> E.EventListener (dom :: DOM | eff) -> Boolean -> t -> Eff (dom :: DOM | eff) Unit
  dispatchEvent :: forall event eff. IsEvent event => event -> t -> Eff (dom :: DOM, err :: EXCEPTION | eff) Boolean


_addEventListener
  :: forall eff a
   . EventType
  -> E.EventListener (dom :: DOM | eff)
  -> Boolean
  -> a
  -> Eff (dom :: DOM | eff) Unit
_addEventListener eventType listener capture target =
  E.addEventListener eventType listener capture (unsafeCoerce target)

_removeEventListener
  :: forall eff a
   . EventType
  -> E.EventListener (dom :: DOM | eff)
  -> Boolean
  -> a
  -> Eff (dom :: DOM | eff) Unit
_removeEventListener eventType listener capture target =
  E.removeEventListener eventType listener capture (unsafeCoerce target)

_dispatchEvent
  :: forall eff e a
   . IsEvent e
  => e
  -> a
  -> Eff (dom :: DOM, err :: EXCEPTION | eff) Boolean
_dispatchEvent event element = E.dispatchEvent (toEvent event) (unsafeCoerce element)


instance eventTargetElement :: EventTarget N.Element where
  addEventListener = _addEventListener
  removeEventListener = _removeEventListener
  dispatchEvent = _dispatchEvent

instance eventTargetHTMLElement :: EventTarget H.HTMLElement where
  addEventListener = _addEventListener
  removeEventListener = _removeEventListener
  dispatchEvent = _dispatchEvent

instance eventTargetHTMLHtmlElement :: EventTarget H.HTMLHtmlElement where
  addEventListener = _addEventListener
  removeEventListener = _removeEventListener
  dispatchEvent = _dispatchEvent

instance eventTargetHTMLHeadElement :: EventTarget H.HTMLHeadElement where
  addEventListener = _addEventListener
  removeEventListener = _removeEventListener
  dispatchEvent = _dispatchEvent

instance eventTargetHTMLTitleElement :: EventTarget H.HTMLTitleElement where
  addEventListener = _addEventListener
  removeEventListener = _removeEventListener
  dispatchEvent = _dispatchEvent

instance eventTargetHTMLBaseElement :: EventTarget H.HTMLBaseElement where
  addEventListener = _addEventListener
  removeEventListener = _removeEventListener
  dispatchEvent = _dispatchEvent

instance eventTargetHTMLLinkElement :: EventTarget H.HTMLLinkElement where
  addEventListener = _addEventListener
  removeEventListener = _removeEventListener
  dispatchEvent = _dispatchEvent

instance eventTargetHTMLMetaElement :: EventTarget H.HTMLMetaElement where
  addEventListener = _addEventListener
  removeEventListener = _removeEventListener
  dispatchEvent = _dispatchEvent

instance eventTargetHTMLStyleElement :: EventTarget H.HTMLStyleElement where
  addEventListener = _addEventListener
  removeEventListener = _removeEventListener
  dispatchEvent = _dispatchEvent

instance eventTargetHTMLBodyElement :: EventTarget H.HTMLBodyElement where
  addEventListener = _addEventListener
  removeEventListener = _removeEventListener
  dispatchEvent = _dispatchEvent

instance eventTargetHTMLHeadingElement :: EventTarget H.HTMLHeadingElement where
  addEventListener = _addEventListener
  removeEventListener = _removeEventListener
  dispatchEvent = _dispatchEvent

instance eventTargetHTMLParagraphElement :: EventTarget H.HTMLParagraphElement where
  addEventListener = _addEventListener
  removeEventListener = _removeEventListener
  dispatchEvent = _dispatchEvent

instance eventTargetHTMLHRElement :: EventTarget H.HTMLHRElement where
  addEventListener = _addEventListener
  removeEventListener = _removeEventListener
  dispatchEvent = _dispatchEvent

instance eventTargetHTMLPreElement :: EventTarget H.HTMLPreElement where
  addEventListener = _addEventListener
  removeEventListener = _removeEventListener
  dispatchEvent = _dispatchEvent

instance eventTargetHTMLQuoteElement :: EventTarget H.HTMLQuoteElement where
  addEventListener = _addEventListener
  removeEventListener = _removeEventListener
  dispatchEvent = _dispatchEvent

instance eventTargetHTMLOListElement :: EventTarget H.HTMLOListElement where
  addEventListener = _addEventListener
  removeEventListener = _removeEventListener
  dispatchEvent = _dispatchEvent

instance eventTargetHTMLUListElement :: EventTarget H.HTMLUListElement where
  addEventListener = _addEventListener
  removeEventListener = _removeEventListener
  dispatchEvent = _dispatchEvent

instance eventTargetHTMLLIElement :: EventTarget H.HTMLLIElement where
  addEventListener = _addEventListener
  removeEventListener = _removeEventListener
  dispatchEvent = _dispatchEvent

instance eventTargetHTMLDListElement :: EventTarget H.HTMLDListElement where
  addEventListener = _addEventListener
  removeEventListener = _removeEventListener
  dispatchEvent = _dispatchEvent

instance eventTargetHTMLDivElement :: EventTarget H.HTMLDivElement where
  addEventListener = _addEventListener
  removeEventListener = _removeEventListener
  dispatchEvent = _dispatchEvent

instance eventTargetHTMLAnchorElement :: EventTarget H.HTMLAnchorElement where
  addEventListener = _addEventListener
  removeEventListener = _removeEventListener
  dispatchEvent = _dispatchEvent

instance eventTargetHTMLDataElement :: EventTarget H.HTMLDataElement where
  addEventListener = _addEventListener
  removeEventListener = _removeEventListener
  dispatchEvent = _dispatchEvent

instance eventTargetHTMLTimeElement :: EventTarget H.HTMLTimeElement where
  addEventListener = _addEventListener
  removeEventListener = _removeEventListener
  dispatchEvent = _dispatchEvent

instance eventTargetHTMLSpanElement :: EventTarget H.HTMLSpanElement where
  addEventListener = _addEventListener
  removeEventListener = _removeEventListener
  dispatchEvent = _dispatchEvent

instance eventTargetHTMLBRElement :: EventTarget H.HTMLBRElement where
  addEventListener = _addEventListener
  removeEventListener = _removeEventListener
  dispatchEvent = _dispatchEvent

instance eventTargetHTMLModElement :: EventTarget H.HTMLModElement where
  addEventListener = _addEventListener
  removeEventListener = _removeEventListener
  dispatchEvent = _dispatchEvent

instance eventTargetHTMLImageElement :: EventTarget H.HTMLImageElement where
  addEventListener = _addEventListener
  removeEventListener = _removeEventListener
  dispatchEvent = _dispatchEvent

instance eventTargetHTMLIFrameElement :: EventTarget H.HTMLIFrameElement where
  addEventListener = _addEventListener
  removeEventListener = _removeEventListener
  dispatchEvent = _dispatchEvent

instance eventTargetHTMLEmbedElement :: EventTarget H.HTMLEmbedElement where
  addEventListener = _addEventListener
  removeEventListener = _removeEventListener
  dispatchEvent = _dispatchEvent

instance eventTargetHTMLObjectElement :: EventTarget H.HTMLObjectElement where
  addEventListener = _addEventListener
  removeEventListener = _removeEventListener
  dispatchEvent = _dispatchEvent

instance eventTargetHTMLParamElement :: EventTarget H.HTMLParamElement where
  addEventListener = _addEventListener
  removeEventListener = _removeEventListener
  dispatchEvent = _dispatchEvent

instance eventTargetHTMLMediaElement :: EventTarget H.HTMLMediaElement where
  addEventListener = _addEventListener
  removeEventListener = _removeEventListener
  dispatchEvent = _dispatchEvent

instance eventTargetHTMLAudioElement :: EventTarget H.HTMLAudioElement where
  addEventListener = _addEventListener
  removeEventListener = _removeEventListener
  dispatchEvent = _dispatchEvent

instance eventTargetHTMLVideoElement :: EventTarget H.HTMLVideoElement where
  addEventListener = _addEventListener
  removeEventListener = _removeEventListener
  dispatchEvent = _dispatchEvent

instance eventTargetHTMLSourceElement :: EventTarget H.HTMLSourceElement where
  addEventListener = _addEventListener
  removeEventListener = _removeEventListener
  dispatchEvent = _dispatchEvent

instance eventTargetHTMLTrackElement :: EventTarget H.HTMLTrackElement where
  addEventListener = _addEventListener
  removeEventListener = _removeEventListener
  dispatchEvent = _dispatchEvent

instance eventTargetHTMLMapElement :: EventTarget H.HTMLMapElement where
  addEventListener = _addEventListener
  removeEventListener = _removeEventListener
  dispatchEvent = _dispatchEvent

instance eventTargetHTMLAreaElement :: EventTarget H.HTMLAreaElement where
  addEventListener = _addEventListener
  removeEventListener = _removeEventListener
  dispatchEvent = _dispatchEvent

instance eventTargetHTMLTableElement :: EventTarget H.HTMLTableElement where
  addEventListener = _addEventListener
  removeEventListener = _removeEventListener
  dispatchEvent = _dispatchEvent

instance eventTargetHTMLTableColElement :: EventTarget H.HTMLTableColElement where
  addEventListener = _addEventListener
  removeEventListener = _removeEventListener
  dispatchEvent = _dispatchEvent

instance eventTargetHTMLTableSectionElement :: EventTarget H.HTMLTableSectionElement where
  addEventListener = _addEventListener
  removeEventListener = _removeEventListener
  dispatchEvent = _dispatchEvent

instance eventTargetHTMLTableRowElement :: EventTarget H.HTMLTableRowElement where
  addEventListener = _addEventListener
  removeEventListener = _removeEventListener
  dispatchEvent = _dispatchEvent

instance eventTargetHTMLTableCellElement :: EventTarget H.HTMLTableCellElement where
  addEventListener = _addEventListener
  removeEventListener = _removeEventListener
  dispatchEvent = _dispatchEvent

instance eventTargetHTMLTableDataCellElement :: EventTarget H.HTMLTableDataCellElement where
  addEventListener = _addEventListener
  removeEventListener = _removeEventListener
  dispatchEvent = _dispatchEvent

instance eventTargetHTMLTableHeaderCellElement :: EventTarget H.HTMLTableHeaderCellElement where
  addEventListener = _addEventListener
  removeEventListener = _removeEventListener
  dispatchEvent = _dispatchEvent

instance eventTargetHTMLFormElement :: EventTarget H.HTMLFormElement where
  addEventListener = _addEventListener
  removeEventListener = _removeEventListener
  dispatchEvent = _dispatchEvent

instance eventTargetHTMLLabelElement :: EventTarget H.HTMLLabelElement where
  addEventListener = _addEventListener
  removeEventListener = _removeEventListener
  dispatchEvent = _dispatchEvent

instance eventTargetHTMLInputElement :: EventTarget H.HTMLInputElement where
  addEventListener = _addEventListener
  removeEventListener = _removeEventListener
  dispatchEvent = _dispatchEvent

instance eventTargetHTMLButtonElement :: EventTarget H.HTMLButtonElement where
  addEventListener = _addEventListener
  removeEventListener = _removeEventListener
  dispatchEvent = _dispatchEvent

instance eventTargetHTMLSelectElement :: EventTarget H.HTMLSelectElement where
  addEventListener = _addEventListener
  removeEventListener = _removeEventListener
  dispatchEvent = _dispatchEvent

instance eventTargetHTMLDataListElement :: EventTarget H.HTMLDataListElement where
  addEventListener = _addEventListener
  removeEventListener = _removeEventListener
  dispatchEvent = _dispatchEvent

instance eventTargetHTMLOptGroupElement :: EventTarget H.HTMLOptGroupElement where
  addEventListener = _addEventListener
  removeEventListener = _removeEventListener
  dispatchEvent = _dispatchEvent

instance eventTargetHTMLOptionElement :: EventTarget H.HTMLOptionElement where
  addEventListener = _addEventListener
  removeEventListener = _removeEventListener
  dispatchEvent = _dispatchEvent

instance eventTargetHTMLTextAreaElement :: EventTarget H.HTMLTextAreaElement where
  addEventListener = _addEventListener
  removeEventListener = _removeEventListener
  dispatchEvent = _dispatchEvent

instance eventTargetHTMLKeygenElement :: EventTarget H.HTMLKeygenElement where
  addEventListener = _addEventListener
  removeEventListener = _removeEventListener
  dispatchEvent = _dispatchEvent

instance eventTargetHTMLOutputElement :: EventTarget H.HTMLOutputElement where
  addEventListener = _addEventListener
  removeEventListener = _removeEventListener
  dispatchEvent = _dispatchEvent

instance eventTargetHTMLProgressElement :: EventTarget H.HTMLProgressElement where
  addEventListener = _addEventListener
  removeEventListener = _removeEventListener
  dispatchEvent = _dispatchEvent

instance eventTargetHTMLMeterElement :: EventTarget H.HTMLMeterElement where
  addEventListener = _addEventListener
  removeEventListener = _removeEventListener
  dispatchEvent = _dispatchEvent

instance eventTargetHTMLFieldSetElement :: EventTarget H.HTMLFieldSetElement where
  addEventListener = _addEventListener
  removeEventListener = _removeEventListener
  dispatchEvent = _dispatchEvent

instance eventTargetHTMLLegendElement :: EventTarget H.HTMLLegendElement where
  addEventListener = _addEventListener
  removeEventListener = _removeEventListener
  dispatchEvent = _dispatchEvent

instance eventTargetHTMLScriptElement :: EventTarget H.HTMLScriptElement where
  addEventListener = _addEventListener
  removeEventListener = _removeEventListener
  dispatchEvent = _dispatchEvent

instance eventTargetHTMLTemplateElement :: EventTarget H.HTMLTemplateElement where
  addEventListener = _addEventListener
  removeEventListener = _removeEventListener
  dispatchEvent = _dispatchEvent

instance eventTargetHTMLCanvasElement :: EventTarget H.HTMLCanvasElement where
  addEventListener = _addEventListener
  removeEventListener = _removeEventListener
  dispatchEvent = _dispatchEvent
