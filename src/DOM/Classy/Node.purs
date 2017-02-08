module DOM.Classy.Node where

import Prelude

import Control.Monad.Eff (Eff)

import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable)

import DOM (DOM)
import DOM.Classy.Util (fromAny)
import DOM.HTML.Types as H
import DOM.Node.Node as NN
import DOM.Node.NodeType (NodeType)
import DOM.Node.Types as N

import Unsafe.Coerce as U

-- | A class for subtypes of `Node`.
class IsNode n where
  toNode :: n -> N.Node
  fromNode :: N.Node -> Maybe n

-- | The type of a node.
nodeType :: forall n. (Partial, IsNode n) => n -> NodeType
nodeType = NN.nodeType <<< toNode

-- | The numeric value for the type of a node.
nodeTypeIndex :: forall n. IsNode n => n -> Int
nodeTypeIndex = NN.nodeTypeIndex <<< toNode

-- | For elements this is the tag name, for document types this is the doctype
-- | name, for processing instructions this is the target, for all other nodes
-- | it is a string like `"#text"`, `"#comment", etc. depending on the node
-- | type.
nodeName :: forall n. IsNode n => n -> String
nodeName = NN.nodeName <<< toNode

-- | The node's base URL.
baseURI :: forall n eff. IsNode n => n -> Eff (dom :: DOM | eff) String
baseURI = NN.baseURI <<< toNode

-- | The document the node belongs to, unless the node is a document in which
-- | case the value is null.
ownerDocument :: forall n eff. IsNode n => n -> Eff (dom :: DOM | eff) (Nullable N.Document)
ownerDocument = NN.ownerDocument <<< toNode

-- | The parent node of the node.
parentNode :: forall n eff. IsNode n => n -> Eff (dom :: DOM | eff) (Nullable N.Node)
parentNode = NN.parentNode <<< toNode

-- | The parent element of the node.
parentElement :: forall n eff. IsNode n => n -> Eff (dom :: DOM | eff) (Nullable N.Element)
parentElement = NN.parentElement <<< toNode

-- | Indicates whether the node has any child nodes.
hasChildNodes :: forall n eff. IsNode n => n -> Eff (dom :: DOM | eff) Boolean
hasChildNodes = NN.hasChildNodes <<< toNode

-- | The children of the node.
childNodes :: forall n eff. IsNode n => n -> Eff (dom :: DOM | eff) N.NodeList
childNodes = NN.childNodes <<< toNode

-- | The first child of the node, or null if the node has no children.
firstChild :: forall n eff. IsNode n => n -> Eff (dom :: DOM | eff) (Nullable N.Node)
firstChild = NN.firstChild <<< toNode

-- | The last child of the node, or null if the node has no children.
lastChild :: forall n eff. IsNode n => n -> Eff (dom :: DOM | eff) (Nullable N.Node)
lastChild = NN.lastChild <<< toNode

-- | The previous sibling node, or null if there is no previous sibling.
previousSibling :: forall n eff. IsNode n => n -> Eff (dom :: DOM | eff) (Nullable N.Node)
previousSibling = NN.previousSibling <<< toNode

-- | The next sibling node, or null if there is no next sibling.
nextSibling :: forall n eff. IsNode n => n -> Eff (dom :: DOM | eff) (Nullable N.Node)
nextSibling = NN.nextSibling <<< toNode

-- | If the node type is text, comment, or processing instruction this is the
-- | node's data, or null in all other cases.
nodeValue :: forall n eff. IsNode n => n -> Eff (dom :: DOM | eff) String
nodeValue = NN.nodeValue <<< toNode

-- | If the node type is text, comment, or processing instruction this allows
-- | the node's data to be changed, or has no effect in all other cases.
setNodeValue :: forall n eff. IsNode n => String -> n -> Eff (dom :: DOM | eff) Unit
setNodeValue v = NN.setNodeValue v <<< toNode

-- | If the node type is document fragment, element, text, processing
-- | instruction, or comment this is the node's data, or null in all other
-- | cases.
textContent :: forall n eff. IsNode n => n -> Eff (dom :: DOM | eff) String
textContent = NN.textContent <<< toNode

-- | If the node type is document fragment, element, text, processing
-- | instruction, or comment this allows the node's data to be changed, or has
-- | no effect in all other cases.
setTextContent :: forall n eff. IsNode n => String -> n -> Eff (dom :: DOM | eff) Unit
setTextContent tc = NN.setTextContent tc <<< toNode

-- | Removes empty text nodes and then combines any remaining text nodes that
-- | are contiguous.
normalize :: forall n eff. IsNode n => n -> Eff (dom :: DOM | eff) Unit
normalize = NN.normalize <<< toNode

-- | Checks whether two nodes are equivalent.
isEqualNode :: forall n1 n2 eff. (IsNode n1, IsNode n2) => n1 -> n2 -> Eff (dom :: DOM | eff) Boolean
isEqualNode n1 n2 = NN.isEqualNode (toNode n1) (toNode n2)

-- | Compares the position of two nodes in the document.
compareDocumentPositionBits :: forall n1 n2 eff. (IsNode n1, IsNode n2) => n1 -> n2 -> Eff (dom :: DOM | eff) Int
compareDocumentPositionBits n1 n2 = NN.compareDocumentPositionBits (toNode n1) (toNode n2)

-- | Checks whether the second node is contained within the first
contains :: forall n1 n2 eff. (IsNode n1, IsNode n2) => n1 -> n2 -> Eff (dom :: DOM | eff) Boolean
contains n1 n2 = NN.contains (toNode n1) (toNode n2)

lookupPrefix :: forall n eff. IsNode n => String -> n -> Eff (dom :: DOM | eff) (Nullable String)
lookupPrefix s = NN.lookupPrefix s <<< toNode

lookupNamespaceURI :: forall n eff. IsNode n => String -> n -> Eff (dom :: DOM | eff) (Nullable String)
lookupNamespaceURI s = NN.lookupNamespaceURI s <<< toNode

isDefaultNamespace :: forall n eff. IsNode n => String -> n -> Eff (dom :: DOM | eff) Boolean
isDefaultNamespace s = NN.isDefaultNamespace s <<< toNode

-- | Inserts the first node before the second as a child of the third node.
insertBefore :: forall n1 n2 n3 eff. (IsNode n1, IsNode n2, IsNode n3) => n1 -> n2 -> n3 -> Eff (dom :: DOM | eff) N.Node
insertBefore n1 n2 n3 = NN.insertBefore (toNode n1) (toNode n2) (toNode n3)

-- | Appends the first node to the child node list of the second node.
appendChild :: forall n1 n2 eff. (IsNode n1, IsNode n2) => n1 -> n2 -> Eff (dom :: DOM | eff) N.Node
appendChild n1 n2 = NN.appendChild (toNode n1) (toNode n2)

-- | Uses the first node as a replacement for the second node in the children
-- | of the third node.
replaceChild :: forall n1 n2 n3 eff. (IsNode n1, IsNode n2, IsNode n3) => n1 -> n2 -> n3 -> Eff (dom :: DOM | eff) N.Node
replaceChild n1 n2 n3 = NN.replaceChild (toNode n1) (toNode n2) (toNode n3)

-- | Removes the first node from the children of the second node.
removeChild :: forall n1 n2 eff. (IsNode n1, IsNode n2) => n1 -> n2 -> Eff (dom :: DOM | eff) N.Node
removeChild n1 n2 = NN.removeChild (toNode n1) (toNode n2)

instance isNodeNode :: IsNode N.Node where
  toNode = U.unsafeCoerce
  fromNode = Just

instance isNodeDocument :: IsNode N.Document where
  toNode = U.unsafeCoerce
  fromNode = fromAny N.readDocument

instance isNodeElement :: IsNode N.Element where
  toNode = U.unsafeCoerce
  fromNode = fromAny N.readElement

instance isNodeHTMLElement :: IsNode H.HTMLElement where
  toNode = U.unsafeCoerce
  fromNode = fromAny H.readHTMLElement

instance isNodeHTMLHtmlElement :: IsNode H.HTMLHtmlElement where
  toNode = U.unsafeCoerce
  fromNode = fromAny H.readHTMLHtmlElement

instance isNodeHTMLHeadElement :: IsNode H.HTMLHeadElement where
  toNode = U.unsafeCoerce
  fromNode = fromAny H.readHTMLHeadElement

instance isNodeHTMLTitleElement :: IsNode H.HTMLTitleElement where
  toNode = U.unsafeCoerce
  fromNode = fromAny H.readHTMLTitleElement

instance isNodeHTMLBaseElement :: IsNode H.HTMLBaseElement where
  toNode = U.unsafeCoerce
  fromNode = fromAny H.readHTMLBaseElement

instance isNodeHTMLLinkElement :: IsNode H.HTMLLinkElement where
  toNode = U.unsafeCoerce
  fromNode = fromAny H.readHTMLLinkElement

instance isNodeHTMLMetaElement :: IsNode H.HTMLMetaElement where
  toNode = U.unsafeCoerce
  fromNode = fromAny H.readHTMLMetaElement

instance isNodeHTMLStyleElement :: IsNode H.HTMLStyleElement where
  toNode = U.unsafeCoerce
  fromNode = fromAny H.readHTMLStyleElement

instance isNodeHTMLBodyElement :: IsNode H.HTMLBodyElement where
  toNode = U.unsafeCoerce
  fromNode = fromAny H.readHTMLBodyElement

instance isNodeHTMLHeadingElement :: IsNode H.HTMLHeadingElement where
  toNode = U.unsafeCoerce
  fromNode = fromAny H.readHTMLHeadingElement

instance isNodeHTMLParagraphElement :: IsNode H.HTMLParagraphElement where
  toNode = U.unsafeCoerce
  fromNode = fromAny H.readHTMLParagraphElement

instance isNodeHTMLHRElement :: IsNode H.HTMLHRElement where
  toNode = U.unsafeCoerce
  fromNode = fromAny H.readHTMLHRElement

instance isNodeHTMLPreElement :: IsNode H.HTMLPreElement where
  toNode = U.unsafeCoerce
  fromNode = fromAny H.readHTMLPreElement

instance isNodeHTMLQuoteElement :: IsNode H.HTMLQuoteElement where
  toNode = U.unsafeCoerce
  fromNode = fromAny H.readHTMLQuoteElement

instance isNodeHTMLOListElement :: IsNode H.HTMLOListElement where
  toNode = U.unsafeCoerce
  fromNode = fromAny H.readHTMLOListElement

instance isNodeHTMLUListElement :: IsNode H.HTMLUListElement where
  toNode = U.unsafeCoerce
  fromNode = fromAny H.readHTMLUListElement

instance isNodeHTMLLIElement :: IsNode H.HTMLLIElement where
  toNode = U.unsafeCoerce
  fromNode = fromAny H.readHTMLLIElement

instance isNodeHTMLDListElement :: IsNode H.HTMLDListElement where
  toNode = U.unsafeCoerce
  fromNode = fromAny H.readHTMLDListElement

instance isNodeHTMLDivElement :: IsNode H.HTMLDivElement where
  toNode = U.unsafeCoerce
  fromNode = fromAny H.readHTMLDivElement

instance isNodeHTMLAnchorElement :: IsNode H.HTMLAnchorElement where
  toNode = U.unsafeCoerce
  fromNode = fromAny H.readHTMLAnchorElement

instance isNodeHTMLDataElement :: IsNode H.HTMLDataElement where
  toNode = U.unsafeCoerce
  fromNode = fromAny H.readHTMLDataElement

instance isNodeHTMLTimeElement :: IsNode H.HTMLTimeElement where
  toNode = U.unsafeCoerce
  fromNode = fromAny H.readHTMLTimeElement

instance isNodeHTMLSpanElement :: IsNode H.HTMLSpanElement where
  toNode = U.unsafeCoerce
  fromNode = fromAny H.readHTMLSpanElement

instance isNodeHTMLBRElement :: IsNode H.HTMLBRElement where
  toNode = U.unsafeCoerce
  fromNode = fromAny H.readHTMLBRElement

instance isNodeHTMLModElement :: IsNode H.HTMLModElement where
  toNode = U.unsafeCoerce
  fromNode = fromAny H.readHTMLModElement

instance isNodeHTMLImageElement :: IsNode H.HTMLImageElement where
  toNode = U.unsafeCoerce
  fromNode = fromAny H.readHTMLImageElement

instance isNodeHTMLIFrameElement :: IsNode H.HTMLIFrameElement where
  toNode = U.unsafeCoerce
  fromNode = fromAny H.readHTMLIFrameElement

instance isNodeHTMLEmbedElement :: IsNode H.HTMLEmbedElement where
  toNode = U.unsafeCoerce
  fromNode = fromAny H.readHTMLEmbedElement

instance isNodeHTMLObjectElement :: IsNode H.HTMLObjectElement where
  toNode = U.unsafeCoerce
  fromNode = fromAny H.readHTMLObjectElement

instance isNodeHTMLParamElement :: IsNode H.HTMLParamElement where
  toNode = U.unsafeCoerce
  fromNode = fromAny H.readHTMLParamElement

instance isNodeHTMLMediaElement :: IsNode H.HTMLMediaElement where
  toNode = U.unsafeCoerce
  fromNode = fromAny H.readHTMLMediaElement

instance isNodeHTMLAudioElement :: IsNode H.HTMLAudioElement where
  toNode = U.unsafeCoerce
  fromNode = fromAny H.readHTMLAudioElement

instance isNodeHTMLVideoElement :: IsNode H.HTMLVideoElement where
  toNode = U.unsafeCoerce
  fromNode = fromAny H.readHTMLVideoElement

instance isNodeHTMLSourceElement :: IsNode H.HTMLSourceElement where
  toNode = U.unsafeCoerce
  fromNode = fromAny H.readHTMLSourceElement

instance isNodeHTMLTrackElement :: IsNode H.HTMLTrackElement where
  toNode = U.unsafeCoerce
  fromNode = fromAny H.readHTMLTrackElement

instance isNodeHTMLMapElement :: IsNode H.HTMLMapElement where
  toNode = U.unsafeCoerce
  fromNode = fromAny H.readHTMLMapElement

instance isNodeHTMLAreaElement :: IsNode H.HTMLAreaElement where
  toNode = U.unsafeCoerce
  fromNode = fromAny H.readHTMLAreaElement

instance isNodeHTMLTableElement :: IsNode H.HTMLTableElement where
  toNode = U.unsafeCoerce
  fromNode = fromAny H.readHTMLTableElement

instance isNodeHTMLTableCaptionElement :: IsNode H.HTMLTableCaptionElement where
  toNode = U.unsafeCoerce
  fromNode = fromAny H.readHTMLTableCaptionElement

instance isNodeHTMLTableColElement :: IsNode H.HTMLTableColElement where
  toNode = U.unsafeCoerce
  fromNode = fromAny H.readHTMLTableColElement

instance isNodeHTMLTableSectionElement :: IsNode H.HTMLTableSectionElement where
  toNode = U.unsafeCoerce
  fromNode = fromAny H.readHTMLTableSectionElement

instance isNodeHTMLTableRowElement :: IsNode H.HTMLTableRowElement where
  toNode = U.unsafeCoerce
  fromNode = fromAny H.readHTMLTableRowElement

instance isNodeHTMLTableCellElement :: IsNode H.HTMLTableCellElement where
  toNode = U.unsafeCoerce
  fromNode = fromAny H.readHTMLTableCellElement

instance isNodeHTMLTableDataCellElement :: IsNode H.HTMLTableDataCellElement where
  toNode = U.unsafeCoerce
  fromNode = fromAny H.readHTMLTableDataCellElement

instance isNodeHTMLTableHeaderCellElement :: IsNode H.HTMLTableHeaderCellElement where
  toNode = U.unsafeCoerce
  fromNode = fromAny H.readHTMLTableHeaderCellElement

instance isNodeHTMLFormElement :: IsNode H.HTMLFormElement where
  toNode = U.unsafeCoerce
  fromNode = fromAny H.readHTMLFormElement

instance isNodeHTMLLabelElement :: IsNode H.HTMLLabelElement where
  toNode = U.unsafeCoerce
  fromNode = fromAny H.readHTMLLabelElement

instance isNodeHTMLInputElement :: IsNode H.HTMLInputElement where
  toNode = U.unsafeCoerce
  fromNode = fromAny H.readHTMLInputElement

instance isNodeHTMLButtonElement :: IsNode H.HTMLButtonElement where
  toNode = U.unsafeCoerce
  fromNode = fromAny H.readHTMLButtonElement

instance isNodeHTMLSelectElement :: IsNode H.HTMLSelectElement where
  toNode = U.unsafeCoerce
  fromNode = fromAny H.readHTMLSelectElement

instance isNodeHTMLDataListElement :: IsNode H.HTMLDataListElement where
  toNode = U.unsafeCoerce
  fromNode = fromAny H.readHTMLDataListElement

instance isNodeHTMLOptGroupElement :: IsNode H.HTMLOptGroupElement where
  toNode = U.unsafeCoerce
  fromNode = fromAny H.readHTMLOptGroupElement

instance isNodeHTMLOptionElement :: IsNode H.HTMLOptionElement where
  toNode = U.unsafeCoerce
  fromNode = fromAny H.readHTMLOptionElement

instance isNodeHTMLTextAreaElement :: IsNode H.HTMLTextAreaElement where
  toNode = U.unsafeCoerce
  fromNode = fromAny H.readHTMLTextAreaElement

instance isNodeHTMLKeygenElement :: IsNode H.HTMLKeygenElement where
  toNode = U.unsafeCoerce
  fromNode = fromAny H.readHTMLKeygenElement

instance isNodeHTMLOutputElement :: IsNode H.HTMLOutputElement where
  toNode = U.unsafeCoerce
  fromNode = fromAny H.readHTMLOutputElement

instance isNodeHTMLProgressElement :: IsNode H.HTMLProgressElement where
  toNode = U.unsafeCoerce
  fromNode = fromAny H.readHTMLProgressElement

instance isNodeHTMLMeterElement :: IsNode H.HTMLMeterElement where
  toNode = U.unsafeCoerce
  fromNode = fromAny H.readHTMLMeterElement

instance isNodeHTMLFieldSetElement :: IsNode H.HTMLFieldSetElement where
  toNode = U.unsafeCoerce
  fromNode = fromAny H.readHTMLFieldSetElement

instance isNodeHTMLLegendElement :: IsNode H.HTMLLegendElement where
  toNode = U.unsafeCoerce
  fromNode = fromAny H.readHTMLLegendElement

instance isNodeHTMLScriptElement :: IsNode H.HTMLScriptElement where
  toNode = U.unsafeCoerce
  fromNode = fromAny H.readHTMLScriptElement

instance isNodeHTMLTemplateElement :: IsNode H.HTMLTemplateElement where
  toNode = U.unsafeCoerce
  fromNode = fromAny H.readHTMLTemplateElement

instance isNodeHTMLCanvasElement :: IsNode H.HTMLCanvasElement where
  toNode = U.unsafeCoerce
  fromNode = fromAny H.readHTMLCanvasElement
