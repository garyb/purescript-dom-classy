module DOM.Classy.HTMLElement
  ( module DOM.Classy.HTMLElement
  , module Exports
  ) where

import Prelude

import Control.Monad.Eff (Eff)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable)

import DOM (DOM)
import DOM.Classy.Element (class IsElement)
import DOM.Classy.Util (fromAny)
import DOM.HTML.HTMLElement as HE
import DOM.HTML.Types as H
import DOM.Node.Types (Element)

import Unsafe.Coerce as U

import DOM.Classy.Element (appendChild, baseURI, childNodes, className, clientHeight, clientLeft, clientTop, clientWidth, compareDocumentPositionBits, contains, firstChild, getAttribute, getElementsByClassName, getElementsByTagName, getElementsByTagNameNS, hasChildNodes, id, insertBefore, isDefaultNamespace, isEqualNode, lastChild, localName, lookupNamespaceURI, lookupPrefix, namespaceURI, nextSibling, nodeName, nodeType, nodeTypeIndex, nodeValue, normalize, ownerDocument, parentElement, parentNode, prefix, previousSibling, removeAttribute, removeChild, replaceChild, scrollHeight, scrollLeft, scrollTop, scrollWidth, setAttribute, setClassName, setId, setNodeValue, setScrollLeft, setScrollTop, setTextContent, tagName, textContent) as Exports

-- | A class for subtypes of `HTMLElement`.
class IsElement e <= IsHTMLElement e where
  toHTMLElement :: e -> H.HTMLElement
  fromHTMLElement :: H.HTMLElement -> Maybe e

title :: forall el eff. IsHTMLElement el => el -> Eff (dom :: DOM | eff) String
title = HE.title <<< toHTMLElement

setTitle :: forall el eff. IsHTMLElement el => String -> el -> Eff (dom :: DOM | eff) Unit
setTitle t = HE.setTitle t <<< toHTMLElement

lang :: forall el eff. IsHTMLElement el => el -> Eff (dom :: DOM | eff) String
lang = HE.lang <<< toHTMLElement

setLang :: forall el eff. IsHTMLElement el => String -> el -> Eff (dom :: DOM | eff) Unit
setLang l = HE.setLang l <<< toHTMLElement

dir :: forall el eff. IsHTMLElement el => el -> Eff (dom :: DOM | eff) String
dir = HE.dir <<< toHTMLElement

setDir :: forall el eff. IsHTMLElement el => String -> el -> Eff (dom :: DOM | eff) Unit
setDir d = HE.setDir d <<< toHTMLElement

hidden :: forall el eff. IsHTMLElement el => el -> Eff (dom :: DOM | eff) Boolean
hidden = HE.hidden <<< toHTMLElement

setHidden :: forall el eff. IsHTMLElement el => Boolean -> el -> Eff (dom :: DOM | eff) Unit
setHidden h = HE.setHidden h <<< toHTMLElement

tabIndex :: forall el eff. IsHTMLElement el => el -> Eff (dom :: DOM | eff) Int
tabIndex = HE.tabIndex <<< toHTMLElement

setTabIndex :: forall el eff. IsHTMLElement el => Int -> el -> Eff (dom :: DOM | eff) Unit
setTabIndex ti = HE.setTabIndex ti <<< toHTMLElement

draggable :: forall el eff. IsHTMLElement el => el -> Eff (dom :: DOM | eff) Boolean
draggable = HE.draggable <<< toHTMLElement

setDraggable :: forall el eff. IsHTMLElement el => Boolean -> el -> Eff (dom :: DOM | eff) Unit
setDraggable d = HE.setDraggable d <<< toHTMLElement

contentEditable :: forall el eff. IsHTMLElement el => el -> Eff (dom :: DOM | eff) String
contentEditable = HE.contentEditable <<< toHTMLElement

setContentEditable :: forall el eff. IsHTMLElement el => String -> el -> Eff (dom :: DOM | eff) Unit
setContentEditable ce = HE.setContentEditable ce <<< toHTMLElement

isContentEditable :: forall el eff. IsHTMLElement el => el -> Eff (dom :: DOM | eff) Boolean
isContentEditable = HE.isContentEditable <<< toHTMLElement

spellcheck :: forall el eff. IsHTMLElement el => el -> Eff (dom :: DOM | eff) Boolean
spellcheck = HE.spellcheck <<< toHTMLElement

setSpellcheck :: forall el eff. IsHTMLElement el => Boolean -> el -> Eff (dom :: DOM | eff) Unit
setSpellcheck s = HE.setSpellcheck s <<< toHTMLElement

click :: forall el eff. IsHTMLElement el => el -> Eff (dom :: DOM | eff) Unit
click = HE.click <<< toHTMLElement

focus :: forall el eff. IsHTMLElement el => el -> Eff (dom :: DOM | eff) Unit
focus = HE.focus <<< toHTMLElement

blur :: forall el eff. IsHTMLElement el => el -> Eff (dom :: DOM | eff) Unit
blur = HE.blur <<< toHTMLElement

getBoundingClientRect
  :: forall el eff
   . IsHTMLElement el
  => el
  -> Eff
      (dom :: DOM | eff)
      { left :: Number
      , top :: Number
      , right :: Number
      , bottom :: Number
      , width :: Number
      , height :: Number
      }
getBoundingClientRect = HE.getBoundingClientRect <<< toHTMLElement

offsetParent :: forall el eff. IsHTMLElement el => el -> Eff (dom :: DOM | eff) (Nullable Element)
offsetParent = HE.offsetParent <<< toHTMLElement

offsetTop :: forall el eff. IsHTMLElement el => el -> Eff (dom :: DOM | eff) Number
offsetTop = HE.offsetTop <<< toHTMLElement

offsetLeft :: forall el eff. IsHTMLElement el => el -> Eff (dom :: DOM | eff) Number
offsetLeft = HE.offsetLeft <<< toHTMLElement

offsetWidth :: forall el eff. IsHTMLElement el => el -> Eff (dom :: DOM | eff) Number
offsetWidth = HE.offsetWidth <<< toHTMLElement

offsetHeight :: forall el eff. IsHTMLElement el => el -> Eff (dom :: DOM | eff) Number
offsetHeight = HE.offsetHeight <<< toHTMLElement

instance isHTMLElementHTMLElement :: IsHTMLElement H.HTMLElement where
  toHTMLElement = id
  fromHTMLElement = Just

instance isHTMLElementHTMLHtmlElement :: IsHTMLElement H.HTMLHtmlElement where
  toHTMLElement = U.unsafeCoerce
  fromHTMLElement = fromAny H.readHTMLHtmlElement

instance isHTMLElementHTMLHeadElement :: IsHTMLElement H.HTMLHeadElement where
  toHTMLElement = U.unsafeCoerce
  fromHTMLElement = fromAny H.readHTMLHeadElement

instance isHTMLElementHTMLTitleElement :: IsHTMLElement H.HTMLTitleElement where
  toHTMLElement = U.unsafeCoerce
  fromHTMLElement = fromAny H.readHTMLTitleElement

instance isHTMLElementHTMLBaseElement :: IsHTMLElement H.HTMLBaseElement where
  toHTMLElement = U.unsafeCoerce
  fromHTMLElement = fromAny H.readHTMLBaseElement

instance isHTMLElementHTMLLinkElement :: IsHTMLElement H.HTMLLinkElement where
  toHTMLElement = U.unsafeCoerce
  fromHTMLElement = fromAny H.readHTMLLinkElement

instance isHTMLElementHTMLMetaElement :: IsHTMLElement H.HTMLMetaElement where
  toHTMLElement = U.unsafeCoerce
  fromHTMLElement = fromAny H.readHTMLMetaElement

instance isHTMLElementHTMLStyleElement :: IsHTMLElement H.HTMLStyleElement where
  toHTMLElement = U.unsafeCoerce
  fromHTMLElement = fromAny H.readHTMLStyleElement

instance isHTMLElementHTMLBodyElement :: IsHTMLElement H.HTMLBodyElement where
  toHTMLElement = U.unsafeCoerce
  fromHTMLElement = fromAny H.readHTMLBodyElement

instance isHTMLElementHTMLHeadingElement :: IsHTMLElement H.HTMLHeadingElement where
  toHTMLElement = U.unsafeCoerce
  fromHTMLElement = fromAny H.readHTMLHeadingElement

instance isHTMLElementHTMLParagraphElement :: IsHTMLElement H.HTMLParagraphElement where
  toHTMLElement = U.unsafeCoerce
  fromHTMLElement = fromAny H.readHTMLParagraphElement

instance isHTMLElementHTMLHRElement :: IsHTMLElement H.HTMLHRElement where
  toHTMLElement = U.unsafeCoerce
  fromHTMLElement = fromAny H.readHTMLHRElement

instance isHTMLElementHTMLPreElement :: IsHTMLElement H.HTMLPreElement where
  toHTMLElement = U.unsafeCoerce
  fromHTMLElement = fromAny H.readHTMLPreElement

instance isHTMLElementHTMLQuoteElement :: IsHTMLElement H.HTMLQuoteElement where
  toHTMLElement = U.unsafeCoerce
  fromHTMLElement = fromAny H.readHTMLQuoteElement

instance isHTMLElementHTMLOListElement :: IsHTMLElement H.HTMLOListElement where
  toHTMLElement = U.unsafeCoerce
  fromHTMLElement = fromAny H.readHTMLOListElement

instance isHTMLElementHTMLUListElement :: IsHTMLElement H.HTMLUListElement where
  toHTMLElement = U.unsafeCoerce
  fromHTMLElement = fromAny H.readHTMLUListElement

instance isHTMLElementHTMLLIElement :: IsHTMLElement H.HTMLLIElement where
  toHTMLElement = U.unsafeCoerce
  fromHTMLElement = fromAny H.readHTMLLIElement

instance isHTMLElementHTMLDListElement :: IsHTMLElement H.HTMLDListElement where
  toHTMLElement = U.unsafeCoerce
  fromHTMLElement = fromAny H.readHTMLDListElement

instance isHTMLElementHTMLDivElement :: IsHTMLElement H.HTMLDivElement where
  toHTMLElement = U.unsafeCoerce
  fromHTMLElement = fromAny H.readHTMLDivElement

instance isHTMLElementHTMLAnchorElement :: IsHTMLElement H.HTMLAnchorElement where
  toHTMLElement = U.unsafeCoerce
  fromHTMLElement = fromAny H.readHTMLAnchorElement

instance isHTMLElementHTMLDataElement :: IsHTMLElement H.HTMLDataElement where
  toHTMLElement = U.unsafeCoerce
  fromHTMLElement = fromAny H.readHTMLDataElement

instance isHTMLElementHTMLTimeElement :: IsHTMLElement H.HTMLTimeElement where
  toHTMLElement = U.unsafeCoerce
  fromHTMLElement = fromAny H.readHTMLTimeElement

instance isHTMLElementHTMLSpanElement :: IsHTMLElement H.HTMLSpanElement where
  toHTMLElement = U.unsafeCoerce
  fromHTMLElement = fromAny H.readHTMLSpanElement

instance isHTMLElementHTMLBRElement :: IsHTMLElement H.HTMLBRElement where
  toHTMLElement = U.unsafeCoerce
  fromHTMLElement = fromAny H.readHTMLBRElement

instance isHTMLElementHTMLModElement :: IsHTMLElement H.HTMLModElement where
  toHTMLElement = U.unsafeCoerce
  fromHTMLElement = fromAny H.readHTMLModElement

instance isHTMLElementHTMLImageElement :: IsHTMLElement H.HTMLImageElement where
  toHTMLElement = U.unsafeCoerce
  fromHTMLElement = fromAny H.readHTMLImageElement

instance isHTMLElementHTMLIFrameElement :: IsHTMLElement H.HTMLIFrameElement where
  toHTMLElement = U.unsafeCoerce
  fromHTMLElement = fromAny H.readHTMLIFrameElement

instance isHTMLElementHTMLEmbedElement :: IsHTMLElement H.HTMLEmbedElement where
  toHTMLElement = U.unsafeCoerce
  fromHTMLElement = fromAny H.readHTMLEmbedElement

instance isHTMLElementHTMLObjectElement :: IsHTMLElement H.HTMLObjectElement where
  toHTMLElement = U.unsafeCoerce
  fromHTMLElement = fromAny H.readHTMLObjectElement

instance isHTMLElementHTMLParamElement :: IsHTMLElement H.HTMLParamElement where
  toHTMLElement = U.unsafeCoerce
  fromHTMLElement = fromAny H.readHTMLParamElement

instance isHTMLElementHTMLMediaElement :: IsHTMLElement H.HTMLMediaElement where
  toHTMLElement = U.unsafeCoerce
  fromHTMLElement = fromAny H.readHTMLMediaElement

instance isHTMLElementHTMLAudioElement :: IsHTMLElement H.HTMLAudioElement where
  toHTMLElement = U.unsafeCoerce
  fromHTMLElement = fromAny H.readHTMLAudioElement

instance isHTMLElementHTMLVideoElement :: IsHTMLElement H.HTMLVideoElement where
  toHTMLElement = U.unsafeCoerce
  fromHTMLElement = fromAny H.readHTMLVideoElement

instance isHTMLElementHTMLSourceElement :: IsHTMLElement H.HTMLSourceElement where
  toHTMLElement = U.unsafeCoerce
  fromHTMLElement = fromAny H.readHTMLSourceElement

instance isHTMLElementHTMLTrackElement :: IsHTMLElement H.HTMLTrackElement where
  toHTMLElement = U.unsafeCoerce
  fromHTMLElement = fromAny H.readHTMLTrackElement

instance isHTMLElementHTMLMapElement :: IsHTMLElement H.HTMLMapElement where
  toHTMLElement = U.unsafeCoerce
  fromHTMLElement = fromAny H.readHTMLMapElement

instance isHTMLElementHTMLAreaElement :: IsHTMLElement H.HTMLAreaElement where
  toHTMLElement = U.unsafeCoerce
  fromHTMLElement = fromAny H.readHTMLAreaElement

instance isHTMLElementHTMLTableElement :: IsHTMLElement H.HTMLTableElement where
  toHTMLElement = U.unsafeCoerce
  fromHTMLElement = fromAny H.readHTMLTableElement

instance isHTMLElementHTMLTableCaptionElement :: IsHTMLElement H.HTMLTableCaptionElement where
  toHTMLElement = U.unsafeCoerce
  fromHTMLElement = fromAny H.readHTMLTableCaptionElement

instance isHTMLElementHTMLTableColElement :: IsHTMLElement H.HTMLTableColElement where
  toHTMLElement = U.unsafeCoerce
  fromHTMLElement = fromAny H.readHTMLTableColElement

instance isHTMLElementHTMLTableSectionElement :: IsHTMLElement H.HTMLTableSectionElement where
  toHTMLElement = U.unsafeCoerce
  fromHTMLElement = fromAny H.readHTMLTableSectionElement

instance isHTMLElementHTMLTableRowElement :: IsHTMLElement H.HTMLTableRowElement where
  toHTMLElement = U.unsafeCoerce
  fromHTMLElement = fromAny H.readHTMLTableRowElement

instance isHTMLElementHTMLTableCellElement :: IsHTMLElement H.HTMLTableCellElement where
  toHTMLElement = U.unsafeCoerce
  fromHTMLElement = fromAny H.readHTMLTableCellElement

instance isHTMLElementHTMLTableDataCellElement :: IsHTMLElement H.HTMLTableDataCellElement where
  toHTMLElement = U.unsafeCoerce
  fromHTMLElement = fromAny H.readHTMLTableDataCellElement

instance isHTMLElementHTMLTableHeaderCellElement :: IsHTMLElement H.HTMLTableHeaderCellElement where
  toHTMLElement = U.unsafeCoerce
  fromHTMLElement = fromAny H.readHTMLTableHeaderCellElement

instance isHTMLElementHTMLFormElement :: IsHTMLElement H.HTMLFormElement where
  toHTMLElement = U.unsafeCoerce
  fromHTMLElement = fromAny H.readHTMLFormElement

instance isHTMLElementHTMLLabelElement :: IsHTMLElement H.HTMLLabelElement where
  toHTMLElement = U.unsafeCoerce
  fromHTMLElement = fromAny H.readHTMLLabelElement

instance isHTMLElementHTMLInputElement :: IsHTMLElement H.HTMLInputElement where
  toHTMLElement = U.unsafeCoerce
  fromHTMLElement = fromAny H.readHTMLInputElement

instance isHTMLElementHTMLButtonElement :: IsHTMLElement H.HTMLButtonElement where
  toHTMLElement = U.unsafeCoerce
  fromHTMLElement = fromAny H.readHTMLButtonElement

instance isHTMLElementHTMLSelectElement :: IsHTMLElement H.HTMLSelectElement where
  toHTMLElement = U.unsafeCoerce
  fromHTMLElement = fromAny H.readHTMLSelectElement

instance isHTMLElementHTMLDataListElement :: IsHTMLElement H.HTMLDataListElement where
  toHTMLElement = U.unsafeCoerce
  fromHTMLElement = fromAny H.readHTMLDataListElement

instance isHTMLElementHTMLOptGroupElement :: IsHTMLElement H.HTMLOptGroupElement where
  toHTMLElement = U.unsafeCoerce
  fromHTMLElement = fromAny H.readHTMLOptGroupElement

instance isHTMLElementHTMLOptionElement :: IsHTMLElement H.HTMLOptionElement where
  toHTMLElement = U.unsafeCoerce
  fromHTMLElement = fromAny H.readHTMLOptionElement

instance isHTMLElementHTMLTextAreaElement :: IsHTMLElement H.HTMLTextAreaElement where
  toHTMLElement = U.unsafeCoerce
  fromHTMLElement = fromAny H.readHTMLTextAreaElement

instance isHTMLElementHTMLKeygenElement :: IsHTMLElement H.HTMLKeygenElement where
  toHTMLElement = U.unsafeCoerce
  fromHTMLElement = fromAny H.readHTMLKeygenElement

instance isHTMLElementHTMLOutputElement :: IsHTMLElement H.HTMLOutputElement where
  toHTMLElement = U.unsafeCoerce
  fromHTMLElement = fromAny H.readHTMLOutputElement

instance isHTMLElementHTMLProgressElement :: IsHTMLElement H.HTMLProgressElement where
  toHTMLElement = U.unsafeCoerce
  fromHTMLElement = fromAny H.readHTMLProgressElement

instance isHTMLElementHTMLMeterElement :: IsHTMLElement H.HTMLMeterElement where
  toHTMLElement = U.unsafeCoerce
  fromHTMLElement = fromAny H.readHTMLMeterElement

instance isHTMLElementHTMLFieldSetElement :: IsHTMLElement H.HTMLFieldSetElement where
  toHTMLElement = U.unsafeCoerce
  fromHTMLElement = fromAny H.readHTMLFieldSetElement

instance isHTMLElementHTMLLegendElement :: IsHTMLElement H.HTMLLegendElement where
  toHTMLElement = U.unsafeCoerce
  fromHTMLElement = fromAny H.readHTMLLegendElement

instance isHTMLElementHTMLScriptElement :: IsHTMLElement H.HTMLScriptElement where
  toHTMLElement = U.unsafeCoerce
  fromHTMLElement = fromAny H.readHTMLScriptElement

instance isHTMLElementHTMLTemplateElement :: IsHTMLElement H.HTMLTemplateElement where
  toHTMLElement = U.unsafeCoerce
  fromHTMLElement = fromAny H.readHTMLTemplateElement

instance isHTMLElementHTMLCanvasElement :: IsHTMLElement H.HTMLCanvasElement where
  toHTMLElement = U.unsafeCoerce
  fromHTMLElement = fromAny H.readHTMLCanvasElement
