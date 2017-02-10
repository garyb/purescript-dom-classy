module DOM.Classy.Element
  ( module DOM.Classy.Element
  , module Exports
  ) where

import Prelude hiding (id)
import Prelude as P

import Control.Monad.Eff (Eff)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable)

import DOM.Classy.Node (class IsNode)
import DOM.Classy.Util (fromAny)
import DOM.HTML.Types as H

import DOM (DOM)
import DOM.Node.Types as N
import DOM.Node.Element as NE

import Unsafe.Coerce as U

import DOM.Classy.Node (appendChild, baseURI, childNodes, compareDocumentPositionBits, contains, firstChild, hasChildNodes, insertBefore, isDefaultNamespace, isEqualNode, lastChild, lookupNamespaceURI, lookupPrefix, nextSibling, nodeName, nodeType, nodeTypeIndex, nodeValue, normalize, ownerDocument, parentElement, parentNode, previousSibling, removeChild, replaceChild, setNodeValue, setTextContent, textContent) as Exports

-- | A class for subtypes of `Element`.
class IsNode e <= IsElement e where
  toElement :: e -> N.Element
  fromElement :: N.Element -> Maybe e

namespaceURI :: forall el. IsElement el => el -> Nullable String
namespaceURI = NE.namespaceURI <<< toElement

prefix :: forall el. IsElement el => el -> Nullable String
prefix = NE.prefix <<< toElement

localName :: forall el. IsElement el => el -> String
localName = NE.localName <<< toElement

tagName :: forall el. IsElement el => el -> String
tagName = NE.tagName <<< toElement

id :: forall eff el. IsElement el => el -> Eff (dom :: DOM | eff) N.ElementId
id = NE.id <<< toElement

setId :: forall eff el. IsElement el => N.ElementId -> el -> Eff (dom :: DOM | eff) Unit
setId ei = NE.setId ei <<< toElement

className :: forall eff el. IsElement el => el -> Eff (dom :: DOM | eff) String
className = NE.className <<< toElement

setClassName :: forall eff el. IsElement el => String -> el -> Eff (dom :: DOM | eff) Unit
setClassName x = NE.setClassName x <<< toElement

getElementsByTagName :: forall eff el. IsElement el => String -> el -> Eff (dom :: DOM | eff) N.HTMLCollection
getElementsByTagName x = NE.getElementsByTagName x <<< toElement

getElementsByTagNameNS :: forall eff el. IsElement el => Nullable String -> String -> el -> Eff (dom :: DOM | eff) N.HTMLCollection
getElementsByTagNameNS x y = NE.getElementsByTagNameNS x y <<< toElement

getElementsByClassName :: forall eff el. IsElement el => String -> el -> Eff (dom :: DOM | eff) N.HTMLCollection
getElementsByClassName x = NE.getElementsByClassName x <<< toElement

setAttribute :: forall eff el. IsElement el => String -> String -> el -> Eff (dom :: DOM | eff) Unit
setAttribute x y = NE.setAttribute x y <<< toElement

getAttribute :: forall eff el. IsElement el => String -> el -> Eff (dom :: DOM | eff) (Nullable String)
getAttribute x = NE.getAttribute x <<< toElement

removeAttribute :: forall eff el. IsElement el => String -> el -> Eff (dom :: DOM | eff) Unit
removeAttribute x = NE.removeAttribute x <<< toElement

scrollTop :: forall eff el. IsElement el => el -> Eff (dom :: DOM | eff) Number
scrollTop = NE.scrollTop <<< toElement

setScrollTop :: forall el eff. IsElement el => Number -> el -> Eff (dom :: DOM | eff) Unit
setScrollTop st = NE.setScrollTop st <<< toElement

scrollLeft :: forall eff el. IsElement el => el -> Eff (dom :: DOM | eff) Number
scrollLeft = NE.scrollLeft <<< toElement

setScrollLeft :: forall el eff. IsElement el => Number -> el -> Eff (dom :: DOM | eff) Unit
setScrollLeft sl = NE.setScrollLeft sl <<< toElement

scrollWidth :: forall eff el. IsElement el => el -> Eff (dom :: DOM | eff) Number
scrollWidth = NE.scrollWidth <<< toElement

scrollHeight :: forall eff el. IsElement el => el -> Eff (dom :: DOM | eff) Number
scrollHeight = NE.scrollHeight <<< toElement

clientTop :: forall eff el. IsElement el => el -> Eff (dom :: DOM | eff) Number
clientTop = NE.clientTop <<< toElement

clientLeft :: forall eff el. IsElement el => el -> Eff (dom :: DOM | eff) Number
clientLeft = NE.clientLeft <<< toElement

clientWidth :: forall eff el. IsElement el => el -> Eff (dom :: DOM | eff) Number
clientWidth = NE.clientWidth <<< toElement

clientHeight :: forall eff el. IsElement el => el -> Eff (dom :: DOM | eff) Number
clientHeight = NE.clientHeight <<< toElement

instance isElementElement :: IsElement N.Element where
  toElement = P.id
  fromElement = Just

instance isElementHTMLElement :: IsElement H.HTMLElement where
  toElement = U.unsafeCoerce
  fromElement = fromAny H.readHTMLElement

instance isElementHTMLHtmlElement :: IsElement H.HTMLHtmlElement where
  toElement = U.unsafeCoerce
  fromElement = fromAny H.readHTMLHtmlElement

instance isElementHTMLHeadElement :: IsElement H.HTMLHeadElement where
  toElement = U.unsafeCoerce
  fromElement = fromAny H.readHTMLHeadElement

instance isElementHTMLTitleElement :: IsElement H.HTMLTitleElement where
  toElement = U.unsafeCoerce
  fromElement = fromAny H.readHTMLTitleElement

instance isElementHTMLBaseElement :: IsElement H.HTMLBaseElement where
  toElement = U.unsafeCoerce
  fromElement = fromAny H.readHTMLBaseElement

instance isElementHTMLLinkElement :: IsElement H.HTMLLinkElement where
  toElement = U.unsafeCoerce
  fromElement = fromAny H.readHTMLLinkElement

instance isElementHTMLMetaElement :: IsElement H.HTMLMetaElement where
  toElement = U.unsafeCoerce
  fromElement = fromAny H.readHTMLMetaElement

instance isElementHTMLStyleElement :: IsElement H.HTMLStyleElement where
  toElement = U.unsafeCoerce
  fromElement = fromAny H.readHTMLStyleElement

instance isElementHTMLBodyElement :: IsElement H.HTMLBodyElement where
  toElement = U.unsafeCoerce
  fromElement = fromAny H.readHTMLBodyElement

instance isElementHTMLHeadingElement :: IsElement H.HTMLHeadingElement where
  toElement = U.unsafeCoerce
  fromElement = fromAny H.readHTMLHeadingElement

instance isElementHTMLParagraphElement :: IsElement H.HTMLParagraphElement where
  toElement = U.unsafeCoerce
  fromElement = fromAny H.readHTMLParagraphElement

instance isElementHTMLHRElement :: IsElement H.HTMLHRElement where
  toElement = U.unsafeCoerce
  fromElement = fromAny H.readHTMLHRElement

instance isElementHTMLPreElement :: IsElement H.HTMLPreElement where
  toElement = U.unsafeCoerce
  fromElement = fromAny H.readHTMLPreElement

instance isElementHTMLQuoteElement :: IsElement H.HTMLQuoteElement where
  toElement = U.unsafeCoerce
  fromElement = fromAny H.readHTMLQuoteElement

instance isElementHTMLOListElement :: IsElement H.HTMLOListElement where
  toElement = U.unsafeCoerce
  fromElement = fromAny H.readHTMLOListElement

instance isElementHTMLUListElement :: IsElement H.HTMLUListElement where
  toElement = U.unsafeCoerce
  fromElement = fromAny H.readHTMLUListElement

instance isElementHTMLLIElement :: IsElement H.HTMLLIElement where
  toElement = U.unsafeCoerce
  fromElement = fromAny H.readHTMLLIElement

instance isElementHTMLDListElement :: IsElement H.HTMLDListElement where
  toElement = U.unsafeCoerce
  fromElement = fromAny H.readHTMLDListElement

instance isElementHTMLDivElement :: IsElement H.HTMLDivElement where
  toElement = U.unsafeCoerce
  fromElement = fromAny H.readHTMLDivElement

instance isElementHTMLAnchorElement :: IsElement H.HTMLAnchorElement where
  toElement = U.unsafeCoerce
  fromElement = fromAny H.readHTMLAnchorElement

instance isElementHTMLDataElement :: IsElement H.HTMLDataElement where
  toElement = U.unsafeCoerce
  fromElement = fromAny H.readHTMLDataElement

instance isElementHTMLTimeElement :: IsElement H.HTMLTimeElement where
  toElement = U.unsafeCoerce
  fromElement = fromAny H.readHTMLTimeElement

instance isElementHTMLSpanElement :: IsElement H.HTMLSpanElement where
  toElement = U.unsafeCoerce
  fromElement = fromAny H.readHTMLSpanElement

instance isElementHTMLBRElement :: IsElement H.HTMLBRElement where
  toElement = U.unsafeCoerce
  fromElement = fromAny H.readHTMLBRElement

instance isElementHTMLModElement :: IsElement H.HTMLModElement where
  toElement = U.unsafeCoerce
  fromElement = fromAny H.readHTMLModElement

instance isElementHTMLImageElement :: IsElement H.HTMLImageElement where
  toElement = U.unsafeCoerce
  fromElement = fromAny H.readHTMLImageElement

instance isElementHTMLIFrameElement :: IsElement H.HTMLIFrameElement where
  toElement = U.unsafeCoerce
  fromElement = fromAny H.readHTMLIFrameElement

instance isElementHTMLEmbedElement :: IsElement H.HTMLEmbedElement where
  toElement = U.unsafeCoerce
  fromElement = fromAny H.readHTMLEmbedElement

instance isElementHTMLObjectElement :: IsElement H.HTMLObjectElement where
  toElement = U.unsafeCoerce
  fromElement = fromAny H.readHTMLObjectElement

instance isElementHTMLParamElement :: IsElement H.HTMLParamElement where
  toElement = U.unsafeCoerce
  fromElement = fromAny H.readHTMLParamElement

instance isElementHTMLMediaElement :: IsElement H.HTMLMediaElement where
  toElement = U.unsafeCoerce
  fromElement = fromAny H.readHTMLMediaElement

instance isElementHTMLAudioElement :: IsElement H.HTMLAudioElement where
  toElement = U.unsafeCoerce
  fromElement = fromAny H.readHTMLAudioElement

instance isElementHTMLVideoElement :: IsElement H.HTMLVideoElement where
  toElement = U.unsafeCoerce
  fromElement = fromAny H.readHTMLVideoElement

instance isElementHTMLSourceElement :: IsElement H.HTMLSourceElement where
  toElement = U.unsafeCoerce
  fromElement = fromAny H.readHTMLSourceElement

instance isElementHTMLTrackElement :: IsElement H.HTMLTrackElement where
  toElement = U.unsafeCoerce
  fromElement = fromAny H.readHTMLTrackElement

instance isElementHTMLMapElement :: IsElement H.HTMLMapElement where
  toElement = U.unsafeCoerce
  fromElement = fromAny H.readHTMLMapElement

instance isElementHTMLAreaElement :: IsElement H.HTMLAreaElement where
  toElement = U.unsafeCoerce
  fromElement = fromAny H.readHTMLAreaElement

instance isElementHTMLTableElement :: IsElement H.HTMLTableElement where
  toElement = U.unsafeCoerce
  fromElement = fromAny H.readHTMLTableElement

instance isElementHTMLTableCaptionElement :: IsElement H.HTMLTableCaptionElement where
  toElement = U.unsafeCoerce
  fromElement = fromAny H.readHTMLTableCaptionElement

instance isElementHTMLTableColElement :: IsElement H.HTMLTableColElement where
  toElement = U.unsafeCoerce
  fromElement = fromAny H.readHTMLTableColElement

instance isElementHTMLTableSectionElement :: IsElement H.HTMLTableSectionElement where
  toElement = U.unsafeCoerce
  fromElement = fromAny H.readHTMLTableSectionElement

instance isElementHTMLTableRowElement :: IsElement H.HTMLTableRowElement where
  toElement = U.unsafeCoerce
  fromElement = fromAny H.readHTMLTableRowElement

instance isElementHTMLTableCellElement :: IsElement H.HTMLTableCellElement where
  toElement = U.unsafeCoerce
  fromElement = fromAny H.readHTMLTableCellElement

instance isElementHTMLTableDataCellElement :: IsElement H.HTMLTableDataCellElement where
  toElement = U.unsafeCoerce
  fromElement = fromAny H.readHTMLTableDataCellElement

instance isElementHTMLTableHeaderCellElement :: IsElement H.HTMLTableHeaderCellElement where
  toElement = U.unsafeCoerce
  fromElement = fromAny H.readHTMLTableHeaderCellElement

instance isElementHTMLFormElement :: IsElement H.HTMLFormElement where
  toElement = U.unsafeCoerce
  fromElement = fromAny H.readHTMLFormElement

instance isElementHTMLLabelElement :: IsElement H.HTMLLabelElement where
  toElement = U.unsafeCoerce
  fromElement = fromAny H.readHTMLLabelElement

instance isElementHTMLInputElement :: IsElement H.HTMLInputElement where
  toElement = U.unsafeCoerce
  fromElement = fromAny H.readHTMLInputElement

instance isElementHTMLButtonElement :: IsElement H.HTMLButtonElement where
  toElement = U.unsafeCoerce
  fromElement = fromAny H.readHTMLButtonElement

instance isElementHTMLSelectElement :: IsElement H.HTMLSelectElement where
  toElement = U.unsafeCoerce
  fromElement = fromAny H.readHTMLSelectElement

instance isElementHTMLDataListElement :: IsElement H.HTMLDataListElement where
  toElement = U.unsafeCoerce
  fromElement = fromAny H.readHTMLDataListElement

instance isElementHTMLOptGroupElement :: IsElement H.HTMLOptGroupElement where
  toElement = U.unsafeCoerce
  fromElement = fromAny H.readHTMLOptGroupElement

instance isElementHTMLOptionElement :: IsElement H.HTMLOptionElement where
  toElement = U.unsafeCoerce
  fromElement = fromAny H.readHTMLOptionElement

instance isElementHTMLTextAreaElement :: IsElement H.HTMLTextAreaElement where
  toElement = U.unsafeCoerce
  fromElement = fromAny H.readHTMLTextAreaElement

instance isElementHTMLKeygenElement :: IsElement H.HTMLKeygenElement where
  toElement = U.unsafeCoerce
  fromElement = fromAny H.readHTMLKeygenElement

instance isElementHTMLOutputElement :: IsElement H.HTMLOutputElement where
  toElement = U.unsafeCoerce
  fromElement = fromAny H.readHTMLOutputElement

instance isElementHTMLProgressElement :: IsElement H.HTMLProgressElement where
  toElement = U.unsafeCoerce
  fromElement = fromAny H.readHTMLProgressElement

instance isElementHTMLMeterElement :: IsElement H.HTMLMeterElement where
  toElement = U.unsafeCoerce
  fromElement = fromAny H.readHTMLMeterElement

instance isElementHTMLFieldSetElement :: IsElement H.HTMLFieldSetElement where
  toElement = U.unsafeCoerce
  fromElement = fromAny H.readHTMLFieldSetElement

instance isElementHTMLLegendElement :: IsElement H.HTMLLegendElement where
  toElement = U.unsafeCoerce
  fromElement = fromAny H.readHTMLLegendElement

instance isElementHTMLScriptElement :: IsElement H.HTMLScriptElement where
  toElement = U.unsafeCoerce
  fromElement = fromAny H.readHTMLScriptElement

instance isElementHTMLTemplateElement :: IsElement H.HTMLTemplateElement where
  toElement = U.unsafeCoerce
  fromElement = fromAny H.readHTMLTemplateElement

instance isElementHTMLCanvasElement :: IsElement H.HTMLCanvasElement where
  toElement = U.unsafeCoerce
  fromElement = fromAny H.readHTMLCanvasElement
