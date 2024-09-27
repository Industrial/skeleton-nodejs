# Streaming MVC Framework
Assume the role of an expert developer inclined towards functional programming. Apply your PureScript proficiency to perform the following task.

## The Task
Formulate a proposal for a technology intended to be a successor of React. The name should relate to streams or pipes. This technology will adopt the Model-View-Controller (MVC) architecture to render websites. While traditionally object-oriented, express the MVC pattern through functional programming practices using PureScript syntax when possible.

### Models
Models in this framework operate like JavaScript signals, accepting simple or complex data structures and indicating changes in values. Views register with models to listen for state changes, triggering a re-render upon each update. These updates should be asynchronous.

### Views
The views system employs a streaming architecture using the Web Streams API, specifically `ReadableStream` and `WritableStream`, to seamlessly manage rendering in both client and server environments. This system ensures views can render both to the DOM in browsers and to the HTTP response body on servers.

Views are defined as functions that accept a variable number of arguments, where the first argument is for attributes and subsequent ones are for children. During the rendering process, a `ReadableStream` is generated to represent the HTML structure, applied asynchronously. In browser environments, this stream updates the DOM by replacing existing elements with new components. To achieve precise rendering, each top-level element must have a unique identifier.

```purescript
module Example.View where

import Prelude
import Data.Foldable (sequence_)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (launchAff_, delay)
import Effect.Promise (Promise, resolve)
import Web.Streams (ReadableStream, WritableStream, newReadableStream, newWritableStream, write)
import Web.DOM (Element, querySelector, setInnerHTML)
import Control.Monad.Random (random)

-- Model can have any key with any value
type Model = Map String String

type Attribute = Tuple String String

type View = String -> Array Attribute -> Array ReadableStream -> Effect ReadableStream

generateUniqueIdentifier :: Effect String
generateUniqueIdentifier = do
  random <- random
  pure $ "id-" <> show random

renderAttribute :: Attribute -> String
renderAttribute (Tuple key value) = key <> "=\"" <> value <> "\""

renderAttributes :: Array Attribute -> String
renderAttributes attrs = " " <> String.join " " (map renderAttribute attrs)

renderView :: View
renderView tagName attrs children = do
  uniqueId <- generateUniqueIdentifier
  let
    idAttribute = Tuple "id" (show uniqueId)
    attributesWithId = idAttribute : attrs

  stream <- newReadableStream $ \controller -> do
    write controller ("<" <> tagName <> renderAttributes attributesWithId <> ">")
    for_ children \child -> do
      pipeTo child controller
    write controller ("</" <> tagName <> ">")

  pure stream
```

### Controllers
Controllers serve as straightforward functions. They interact with external systems and may induce changes in Models, completing the unidirectional MVC framework. These controller functions should operate asynchronously, ensuring they have adequate time to finish.

### Client and Server Side Rendering
#### Client
When a view is rendered in the browser, it should update a specific portion of the DOM tree, replacing the existing elements with the new component. Each top-level element of every component should carry a unique identifier attribute to help the rendering system locate and replace the element swiftly.

#### Server
Server rendering should create a ReadableStream. Parts of the view ready for immediate serialization will stream first, while others will follow as they become ready. The stream starts from the deepest level, working its way to the top. Each level is a composite of streams that operate and stream in parallel, maintained in order by unique identifiers.

## Rules
* Generate a proposal for a technology that is a successor of React.
* The name of the project should have something to do with streams or pipes.
* Write the proposal in Markdown.
  * The complete markdown project will be written as one code block, so that it will not be markdown formatted and can be copied as a whole.
* The proposal should be a single file.
* Write it in such a way that it can be used as a template for a proposal.
* Include these rules at the end of the proposal so that I can input it into an LLM to ask me to solve the problem.
* Start the proposal with a few sentences telling the LLM that it should behave in a certain way. That way should be conductive to produce the optimal solution.
