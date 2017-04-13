module DOM.Classy.ParentNode where

import Prelude

import Control.Monad.Eff (Eff)

import Data.Maybe (Maybe)

import DOM (DOM)
import DOM.Classy.Util (fromAny)
import DOM.HTML.Types as H
import DOM.Node.ParentNode as PN
import DOM.Node.Types as N

import Unsafe.Coerce as U

-- | A class for subtypes of `ParentNode`.
class IsParentNode n where
  toParentNode :: n -> N.ParentNode
  fromParentNode :: N.ParentNode -> Maybe n

-- | The child elements for the node.
children :: forall n eff. IsParentNode n => n -> Eff (dom :: DOM | eff) N.HTMLCollection
children = PN.children <<< toParentNode

-- | The first child that is an element, or null if no such element exists.
firstElementChild :: forall n eff. IsParentNode n => n -> Eff (dom :: DOM | eff) (Maybe N.Element)
firstElementChild = PN.firstElementChild <<< toParentNode

-- | The last child that is an element, or null if no such element exists.
lastElementChild :: forall n eff. IsParentNode n => n -> Eff (dom :: DOM | eff) (Maybe N.Element)
lastElementChild = PN.lastElementChild <<< toParentNode

-- | The number of child elements.
childElementCount :: forall n eff. IsParentNode n => n -> Eff (dom :: DOM | eff) Int
childElementCount = PN.childElementCount <<< toParentNode

-- | Finds the first child that is an element that matches the selector(s), or
-- | null if no such element exists.
querySelector :: forall n eff. IsParentNode n => PN.QuerySelector -> n -> Eff (dom :: DOM | eff) (Maybe N.Element)
querySelector selector = PN.querySelector selector <<< toParentNode

-- | Finds all the child elements that matches the selector(s).
querySelectorAll :: forall n eff. IsParentNode n => PN.QuerySelector -> n -> Eff (dom :: DOM | eff) N.NodeList
querySelectorAll selector = PN.querySelectorAll selector <<< toParentNode

instance isParentNodeDocument :: IsParentNode N.Document where
  toParentNode = U.unsafeCoerce
  fromParentNode = fromAny N.readDocument

instance isParentNodeElement :: IsParentNode N.Element where
  toParentNode = U.unsafeCoerce
  fromParentNode = fromAny N.readElement

instance isParentNodeHTMLElement :: IsParentNode H.HTMLElement where
  toParentNode = U.unsafeCoerce
  fromParentNode = fromAny H.readHTMLElement

instance isParentNodeHTMLHtmlElement :: IsParentNode H.HTMLHtmlElement where
  toParentNode = U.unsafeCoerce
  fromParentNode = fromAny H.readHTMLHtmlElement

instance isParentNodeHTMLHeadElement :: IsParentNode H.HTMLHeadElement where
  toParentNode = U.unsafeCoerce
  fromParentNode = fromAny H.readHTMLHeadElement

instance isParentNodeHTMLTitleElement :: IsParentNode H.HTMLTitleElement where
  toParentNode = U.unsafeCoerce
  fromParentNode = fromAny H.readHTMLTitleElement

instance isParentNodeHTMLBaseElement :: IsParentNode H.HTMLBaseElement where
  toParentNode = U.unsafeCoerce
  fromParentNode = fromAny H.readHTMLBaseElement

instance isParentNodeHTMLLinkElement :: IsParentNode H.HTMLLinkElement where
  toParentNode = U.unsafeCoerce
  fromParentNode = fromAny H.readHTMLLinkElement

instance isParentNodeHTMLMetaElement :: IsParentNode H.HTMLMetaElement where
  toParentNode = U.unsafeCoerce
  fromParentNode = fromAny H.readHTMLMetaElement

instance isParentNodeHTMLStyleElement :: IsParentNode H.HTMLStyleElement where
  toParentNode = U.unsafeCoerce
  fromParentNode = fromAny H.readHTMLStyleElement

instance isParentNodeHTMLBodyElement :: IsParentNode H.HTMLBodyElement where
  toParentNode = U.unsafeCoerce
  fromParentNode = fromAny H.readHTMLBodyElement

instance isParentNodeHTMLHeadingElement :: IsParentNode H.HTMLHeadingElement where
  toParentNode = U.unsafeCoerce
  fromParentNode = fromAny H.readHTMLHeadingElement

instance isParentNodeHTMLParagraphElement :: IsParentNode H.HTMLParagraphElement where
  toParentNode = U.unsafeCoerce
  fromParentNode = fromAny H.readHTMLParagraphElement

instance isParentNodeHTMLHRElement :: IsParentNode H.HTMLHRElement where
  toParentNode = U.unsafeCoerce
  fromParentNode = fromAny H.readHTMLHRElement

instance isParentNodeHTMLPreElement :: IsParentNode H.HTMLPreElement where
  toParentNode = U.unsafeCoerce
  fromParentNode = fromAny H.readHTMLPreElement

instance isParentNodeHTMLQuoteElement :: IsParentNode H.HTMLQuoteElement where
  toParentNode = U.unsafeCoerce
  fromParentNode = fromAny H.readHTMLQuoteElement

instance isParentNodeHTMLOListElement :: IsParentNode H.HTMLOListElement where
  toParentNode = U.unsafeCoerce
  fromParentNode = fromAny H.readHTMLOListElement

instance isParentNodeHTMLUListElement :: IsParentNode H.HTMLUListElement where
  toParentNode = U.unsafeCoerce
  fromParentNode = fromAny H.readHTMLUListElement

instance isParentNodeHTMLLIElement :: IsParentNode H.HTMLLIElement where
  toParentNode = U.unsafeCoerce
  fromParentNode = fromAny H.readHTMLLIElement

instance isParentNodeHTMLDListElement :: IsParentNode H.HTMLDListElement where
  toParentNode = U.unsafeCoerce
  fromParentNode = fromAny H.readHTMLDListElement

instance isParentNodeHTMLDivElement :: IsParentNode H.HTMLDivElement where
  toParentNode = U.unsafeCoerce
  fromParentNode = fromAny H.readHTMLDivElement

instance isParentNodeHTMLAnchorElement :: IsParentNode H.HTMLAnchorElement where
  toParentNode = U.unsafeCoerce
  fromParentNode = fromAny H.readHTMLAnchorElement

instance isParentNodeHTMLDataElement :: IsParentNode H.HTMLDataElement where
  toParentNode = U.unsafeCoerce
  fromParentNode = fromAny H.readHTMLDataElement

instance isParentNodeHTMLTimeElement :: IsParentNode H.HTMLTimeElement where
  toParentNode = U.unsafeCoerce
  fromParentNode = fromAny H.readHTMLTimeElement

instance isParentNodeHTMLSpanElement :: IsParentNode H.HTMLSpanElement where
  toParentNode = U.unsafeCoerce
  fromParentNode = fromAny H.readHTMLSpanElement

instance isParentNodeHTMLBRElement :: IsParentNode H.HTMLBRElement where
  toParentNode = U.unsafeCoerce
  fromParentNode = fromAny H.readHTMLBRElement

instance isParentNodeHTMLModElement :: IsParentNode H.HTMLModElement where
  toParentNode = U.unsafeCoerce
  fromParentNode = fromAny H.readHTMLModElement

instance isParentNodeHTMLImageElement :: IsParentNode H.HTMLImageElement where
  toParentNode = U.unsafeCoerce
  fromParentNode = fromAny H.readHTMLImageElement

instance isParentNodeHTMLIFrameElement :: IsParentNode H.HTMLIFrameElement where
  toParentNode = U.unsafeCoerce
  fromParentNode = fromAny H.readHTMLIFrameElement

instance isParentNodeHTMLEmbedElement :: IsParentNode H.HTMLEmbedElement where
  toParentNode = U.unsafeCoerce
  fromParentNode = fromAny H.readHTMLEmbedElement

instance isParentNodeHTMLObjectElement :: IsParentNode H.HTMLObjectElement where
  toParentNode = U.unsafeCoerce
  fromParentNode = fromAny H.readHTMLObjectElement

instance isParentNodeHTMLParamElement :: IsParentNode H.HTMLParamElement where
  toParentNode = U.unsafeCoerce
  fromParentNode = fromAny H.readHTMLParamElement

instance isParentNodeHTMLMediaElement :: IsParentNode H.HTMLMediaElement where
  toParentNode = U.unsafeCoerce
  fromParentNode = fromAny H.readHTMLMediaElement

instance isParentNodeHTMLAudioElement :: IsParentNode H.HTMLAudioElement where
  toParentNode = U.unsafeCoerce
  fromParentNode = fromAny H.readHTMLAudioElement

instance isParentNodeHTMLVideoElement :: IsParentNode H.HTMLVideoElement where
  toParentNode = U.unsafeCoerce
  fromParentNode = fromAny H.readHTMLVideoElement

instance isParentNodeHTMLSourceElement :: IsParentNode H.HTMLSourceElement where
  toParentNode = U.unsafeCoerce
  fromParentNode = fromAny H.readHTMLSourceElement

instance isParentNodeHTMLTrackElement :: IsParentNode H.HTMLTrackElement where
  toParentNode = U.unsafeCoerce
  fromParentNode = fromAny H.readHTMLTrackElement

instance isParentNodeHTMLMapElement :: IsParentNode H.HTMLMapElement where
  toParentNode = U.unsafeCoerce
  fromParentNode = fromAny H.readHTMLMapElement

instance isParentNodeHTMLAreaElement :: IsParentNode H.HTMLAreaElement where
  toParentNode = U.unsafeCoerce
  fromParentNode = fromAny H.readHTMLAreaElement

instance isParentNodeHTMLTableElement :: IsParentNode H.HTMLTableElement where
  toParentNode = U.unsafeCoerce
  fromParentNode = fromAny H.readHTMLTableElement

instance isParentNodeHTMLTableCaptionElement :: IsParentNode H.HTMLTableCaptionElement where
  toParentNode = U.unsafeCoerce
  fromParentNode = fromAny H.readHTMLTableCaptionElement

instance isParentNodeHTMLTableColElement :: IsParentNode H.HTMLTableColElement where
  toParentNode = U.unsafeCoerce
  fromParentNode = fromAny H.readHTMLTableColElement

instance isParentNodeHTMLTableSectionElement :: IsParentNode H.HTMLTableSectionElement where
  toParentNode = U.unsafeCoerce
  fromParentNode = fromAny H.readHTMLTableSectionElement

instance isParentNodeHTMLTableRowElement :: IsParentNode H.HTMLTableRowElement where
  toParentNode = U.unsafeCoerce
  fromParentNode = fromAny H.readHTMLTableRowElement

instance isParentNodeHTMLTableCellElement :: IsParentNode H.HTMLTableCellElement where
  toParentNode = U.unsafeCoerce
  fromParentNode = fromAny H.readHTMLTableCellElement

instance isParentNodeHTMLTableDataCellElement :: IsParentNode H.HTMLTableDataCellElement where
  toParentNode = U.unsafeCoerce
  fromParentNode = fromAny H.readHTMLTableDataCellElement

instance isParentNodeHTMLTableHeaderCellElement :: IsParentNode H.HTMLTableHeaderCellElement where
  toParentNode = U.unsafeCoerce
  fromParentNode = fromAny H.readHTMLTableHeaderCellElement

instance isParentNodeHTMLFormElement :: IsParentNode H.HTMLFormElement where
  toParentNode = U.unsafeCoerce
  fromParentNode = fromAny H.readHTMLFormElement

instance isParentNodeHTMLLabelElement :: IsParentNode H.HTMLLabelElement where
  toParentNode = U.unsafeCoerce
  fromParentNode = fromAny H.readHTMLLabelElement

instance isParentNodeHTMLInputElement :: IsParentNode H.HTMLInputElement where
  toParentNode = U.unsafeCoerce
  fromParentNode = fromAny H.readHTMLInputElement

instance isParentNodeHTMLButtonElement :: IsParentNode H.HTMLButtonElement where
  toParentNode = U.unsafeCoerce
  fromParentNode = fromAny H.readHTMLButtonElement

instance isParentNodeHTMLSelectElement :: IsParentNode H.HTMLSelectElement where
  toParentNode = U.unsafeCoerce
  fromParentNode = fromAny H.readHTMLSelectElement

instance isParentNodeHTMLDataListElement :: IsParentNode H.HTMLDataListElement where
  toParentNode = U.unsafeCoerce
  fromParentNode = fromAny H.readHTMLDataListElement

instance isParentNodeHTMLOptGroupElement :: IsParentNode H.HTMLOptGroupElement where
  toParentNode = U.unsafeCoerce
  fromParentNode = fromAny H.readHTMLOptGroupElement

instance isParentNodeHTMLOptionElement :: IsParentNode H.HTMLOptionElement where
  toParentNode = U.unsafeCoerce
  fromParentNode = fromAny H.readHTMLOptionElement

instance isParentNodeHTMLTextAreaElement :: IsParentNode H.HTMLTextAreaElement where
  toParentNode = U.unsafeCoerce
  fromParentNode = fromAny H.readHTMLTextAreaElement

instance isParentNodeHTMLKeygenElement :: IsParentNode H.HTMLKeygenElement where
  toParentNode = U.unsafeCoerce
  fromParentNode = fromAny H.readHTMLKeygenElement

instance isParentNodeHTMLOutputElement :: IsParentNode H.HTMLOutputElement where
  toParentNode = U.unsafeCoerce
  fromParentNode = fromAny H.readHTMLOutputElement

instance isParentNodeHTMLProgressElement :: IsParentNode H.HTMLProgressElement where
  toParentNode = U.unsafeCoerce
  fromParentNode = fromAny H.readHTMLProgressElement

instance isParentNodeHTMLMeterElement :: IsParentNode H.HTMLMeterElement where
  toParentNode = U.unsafeCoerce
  fromParentNode = fromAny H.readHTMLMeterElement

instance isParentNodeHTMLFieldSetElement :: IsParentNode H.HTMLFieldSetElement where
  toParentNode = U.unsafeCoerce
  fromParentNode = fromAny H.readHTMLFieldSetElement

instance isParentNodeHTMLLegendElement :: IsParentNode H.HTMLLegendElement where
  toParentNode = U.unsafeCoerce
  fromParentNode = fromAny H.readHTMLLegendElement

instance isParentNodeHTMLScriptElement :: IsParentNode H.HTMLScriptElement where
  toParentNode = U.unsafeCoerce
  fromParentNode = fromAny H.readHTMLScriptElement

instance isParentNodeHTMLTemplateElement :: IsParentNode H.HTMLTemplateElement where
  toParentNode = U.unsafeCoerce
  fromParentNode = fromAny H.readHTMLTemplateElement

instance isParentNodeHTMLCanvasElement :: IsParentNode H.HTMLCanvasElement where
  toParentNode = U.unsafeCoerce
  fromParentNode = fromAny H.readHTMLCanvasElement
