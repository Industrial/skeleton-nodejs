module Lib.HTTPResponse where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Class (liftEffect)
import Lib (Handler)
import Lib.Stream (endAff, writeStringAff)
import Node.Encoding (Encoding(..))
import Node.HTTP as HTTP

locationHeader :: forall a. String -> Handler a
locationHeader location ctx = do
  liftEffect $ HTTP.setHeader ctx.res "Location" location
  pure ctx

contentTypeHeader :: forall a. String -> Handler a
contentTypeHeader contentType ctx = do
  liftEffect $ HTTP.setHeader ctx.res "Content-Type" contentType
  pure ctx

jsonContentTypeHeader :: forall a. Handler a
jsonContentTypeHeader = do
  ctx <- contentTypeHeader "application/json"
  pure ctx

statusCode :: forall a. Int -> Handler a
statusCode code ctx = do
  liftEffect $ HTTP.setStatusCode ctx.res code
  pure ctx

bodyHandlerOf :: forall a. Int -> Maybe String -> Handler a
bodyHandlerOf code maybeBody request response = do
  let responseStream = HTTP.responseAsStream response
  statusCode code request response
  case maybeBody of
    Just body -> do
      writeStringAff responseStream UTF8 body
    Nothing -> pure unit
  endAff responseStream

redirectHandlerOf :: forall a. Int -> String -> Handler a
redirectHandlerOf code redirectURL request response = do
  let responseStream = HTTP.responseAsStream response
  locationHeader redirectURL request response
  statusCode code request response
  endAff responseStream

-- 2xx
-- 200 OK
-- Standard response for successful HTTP requests. The actual response will
-- depend on the request method used. In a GET request, the response will
-- contain an entity corresponding to the requested resource. In a POST request,
-- the response will contain an entity describing or containing the result of
-- the action.
ok :: Maybe String -> Handler
ok maybeBody = bodyHandlerOf 200 maybeBody

-- 201 Created
-- The request has been fulfilled, resulting in the creation of a new resource.
created :: Maybe String -> Handler
created maybeBody = bodyHandlerOf 201 maybeBody

-- 202 Accepted
-- The request has been accepted for processing, but the processing has not been
-- completed. The request might or might not be eventually acted upon, and may
-- be disallowed when processing occurs.
accepted :: Maybe String -> Handler
accepted maybeBody = bodyHandlerOf 202 maybeBody

-- 203 Non-Authorative Information
-- The server is a transforming proxy (e.g. a Web accelerator) that received a
-- 200 OK from its origin, but is returning a modified version of the origin's
-- response.
nonAuthorativeInformation :: Maybe String -> Handler
nonAuthorativeInformation maybeBody = bodyHandlerOf 203 maybeBody

-- 204 No Content
-- The server successfully processed the request, and is not returning any
-- content.
noContent :: Maybe String -> Handler
noContent maybeBody = bodyHandlerOf 202 maybeBody

-- 205 Reset Content
-- The server successfully processed the request, asks that the requester reset
-- its document view, and is not returning any content.
resetContent :: Maybe String -> Handler
resetContent maybeBody = bodyHandlerOf 205 maybeBody

-- 206 Partial Content
-- The server is delivering only part of the resource (byte serving) due to a
-- range header sent by the client. The range header is used by HTTP clients to
-- enable resuming of interrupted downloads, or split a download into multiple
-- simultaneous streams.
partialContent :: Maybe String -> Handler
partialContent maybeBody = bodyHandlerOf 206 maybeBody

-- 207 Multi-Status (WebDAV; RFC 4918)
-- The message body that follows is by default an XML message and can contain a
-- number of separate response codes, depending on how many sub-requests were
-- made.
multiStatus :: Maybe String -> Handler
multiStatus maybeBody = bodyHandlerOf 207 maybeBody

-- 208 Already Reported (WebDAV; RFC 5842)
-- The members of a DAV binding have already been enumerated in a preceding part
-- of the (multistatus) response, and are not being included again.
alreadyReported :: Maybe String -> Handler
alreadyReported maybeBody = bodyHandlerOf 208 maybeBody

-- 226 IM Used (RFC 3229)
-- The server has fulfilled a request for the resource, and the response is a
-- representation of the result of one or more instance-manipulations applied to
-- the current instance.
imUsed :: Maybe String -> Handler
imUsed maybeBody = bodyHandlerOf 226 maybeBody

-- 3xx
-- 300
-- Indicates multiple options for the resource from which the client may choose
-- (via agent-driven content negotiation). For example, this code could be used
-- to present multiple video format options, to list files with different
-- filename extensions, or to suggest word-sense disambiguation.
multipleChoices :: Maybe String -> Handler
multipleChoices maybeBody = bodyHandlerOf 300 maybeBody

-- 300
-- This and all future requests should be directed to the given URI.
movedPermanently :: String -> Handler
movedPermanently redirectURL = redirectHandlerOf 301 redirectURL

-- 302
-- Tells the client to look at (browse to) another URL. The HTTP/1.0
-- specification required the client to perform a temporary redirect with the
-- same method (the original describing phrase was "Moved Temporarily"), but
-- popular browsers implemented 302 redirects by changing the method to GET.
-- Therefore, HTTP/1.1 added status codes 303 and 307 to distinguish between the
-- two behaviours.
found :: String -> Handler
found redirectURL = redirectHandlerOf 302 redirectURL

-- 303
-- The response to the request can be found under another URI using the GET
-- method.  When received in response to a POST (or PUT/DELETE), the client
-- should presume that the server has received the data and should issue a new
-- GET request to the given URI.
seeOther :: String -> Handler
seeOther redirectURL = redirectHandlerOf 303 redirectURL

-- 304
-- Indicates that the resource has not been modified since the version specified
-- by the request headers If-Modified-Since or If-None-Match. In such case,
-- there is no need to retransmit the resource since the client still has a
-- previously-downloaded copy.
notModified :: Handler
notModified = bodyHandlerOf 304 Nothing

-- 305
-- The requested resource is available only through a proxy, the address for
-- which is provided in the response. For security reasons, many HTTP clients
-- (such as Mozilla Firefox and Internet Explorer) do not obey this status code.
useProxy :: Handler
useProxy = bodyHandlerOf 304 Nothing

-- 306
-- No longer used. Originally meant "Subsequent requests should use the
-- specified proxy.
switchProxy :: Handler
switchProxy = bodyHandlerOf 304 Nothing

-- 308
-- This and all future requests should be directed to the given URI.  308
-- parallel the behaviour of 301, but does not allow the HTTP method to change.
-- So, for example, submitting a form to a permanently redirected resource may
-- continue smoothly.
permanentRedirect :: String -> Handler
permanentRedirect redirectURL = redirectHandlerOf 308 redirectURL

-- 4xx
-- 400
-- The server cannot or will not process the request due to an apparent client
-- error (e.g., malformed request syntax, size too large, invalid request
-- message framing, or deceptive request routing).
badRequest :: Maybe String -> Handler
badRequest maybeBody = bodyHandlerOf 400 maybeBody

-- 401
-- Similar to 403 Forbidden, but specifically for use when authentication is
-- required and has failed or has not yet been provided. The response must
-- include a WWW-Authenticate header field containing a challenge applicable to
-- the requested resource.
unauthorized :: Handler
unauthorized = bodyHandlerOf 401 Nothing

-- 402
-- Reserved for future use. The original intention was that this code might be
-- used as part of some form of digital cash or micropayment scheme, as
-- proposed, for example, by GNU Taler, but that has not yet happened, and this
-- code is not widely used.
paymentRequired :: Handler
paymentRequired = bodyHandlerOf 402 Nothing

-- 404
-- The requested resource could not be found but may be available in the future.
-- Subsequent requests by the client are permissible.
notFound :: Handler
notFound = bodyHandlerOf 404 Nothing

-- 405
-- A request method is not supported for the requested resource; for example, a
-- GET request on a form that requires data to be presented via POST, or a PUT
-- request on a read-only resource.
methodNotAllowed :: Handler
methodNotAllowed = bodyHandlerOf 405 Nothing

-- 406
-- The requested resource is capable of generating only content not acceptable
-- according to the Accept headers sent in the request. See Content negotiation.
notAcceptable :: Handler
notAcceptable = bodyHandlerOf 406 Nothing

-- 407
-- The client must first authenticate itself with the proxy.
proxyAuthenticationRequired :: Handler
proxyAuthenticationRequired = bodyHandlerOf 407 Nothing

-- 408
-- The server timed out waiting for the request. According to HTTP
-- specifications: "The client did not produce a request within the time that
-- the server was prepared to wait.  The client MAY repeat the request without
-- modifications at any later time."
requestTimeout :: Handler
requestTimeout = bodyHandlerOf 408 Nothing

-- 409
-- Indicates that the request could not be processed because of conflict in the
-- current state of the resource, such as an edit conflict between multiple
-- simultaneous updates.
conflict :: Handler
conflict = bodyHandlerOf 409 Nothing

-- 410
-- Indicates that the resource requested was previously in use but is no longer
-- available and will not be available again. This should be used when a
-- resource has been intentionally removed and the resource should be purged.
-- Upon receiving a 410 status code, the client should not request the resource
-- in the future. Clients such as search engines should remove the resource from
-- their indices. Most use cases do not require clients and search engines to
-- purge the resource, and a "404 Not Found" may be used instead.
gone :: Handler
gone = bodyHandlerOf 410 Nothing

-- 411
-- The request did not specify the length of its content, which is required by
-- the requested resource.
lengthRequired :: Handler
lengthRequired = bodyHandlerOf 411 Nothing

-- 412 Precondition Failed
-- The server does not meet one of the preconditions that the requester put on
-- the request header fields.
preconditionFailed :: Handler
preconditionFailed = bodyHandlerOf 412 Nothing

-- 413 Payload Too Large.
-- The request is larger than the server is willing or able to process.
-- Previously called "Request Entity Too Large" in RFC 2616.
payloadTooLarge :: Handler
payloadTooLarge = bodyHandlerOf 413 Nothing

-- 414 URI Too Long
-- The URI provided was too long for the server to process. Often the result of
-- too much data being encoded as a query-string of a GET request, in which case
-- it should be converted to a POST request.
uriTooLong :: Handler
uriTooLong = bodyHandlerOf 414 Nothing

-- 415 Unsupported Media Type
-- The request entity has a media type which the server or resource does not
-- support.  For example, the client uploads an image as image/svg+xml, but the
-- server requires that images use a different format.
unsupportedMediaType :: Handler
unsupportedMediaType = bodyHandlerOf 415 Nothing

-- 416 Range Not Satisfiable
-- The client has asked for a portion of the file (byte serving), but the server
-- cannot supply that portion.  For example, if the client asked for a part of
-- the file that lies beyond the end of the file.
rangeNotSatisfiable :: Handler
rangeNotSatisfiable = bodyHandlerOf 416 Nothing

-- 417 Expectation Failed
-- The server cannot meet the requirements of the Expect request-header field.
expectationFailed :: Handler
expectationFailed = bodyHandlerOf 417 Nothing

-- 418 I'm a teapot
-- This code was defined in 1998 as one of the traditional IETF April Fools'
-- jokes, in RFC 2324, Hyper Text Coffee Pot Control Protocol, and is not
-- expected to be implemented by actual HTTP servers. The RFC specifies this
-- code should be returned by teapots requested to brew coffee. This HTTP status
-- is used as an Easter egg in some websites, such as Google.com's "I'm a
-- teapot" easter egg. Sometimes, this status code is also used as a response to
-- a blocked request, instead of the more appropriate 403 Forbidden.
imATeapot :: Handler
imATeapot = bodyHandlerOf 418 Nothing

-- 421 Misdirected Request
-- The request was directed at a server that is not able to produce a response
-- (for example because of connection reuse).
misdirectedRequest :: Handler
misdirectedRequest = bodyHandlerOf 421 Nothing

-- 422 Unprocessable Entity
-- The request was well-formed but was unable to be followed due to semantic
-- errors.
unprocessableEntity :: Handler
unprocessableEntity = bodyHandlerOf 422 Nothing

-- 423 Locked
-- The resource that is being accessed is locked.
locked :: Handler
locked = bodyHandlerOf 423 Nothing

-- 424 Failed Dependency
-- The request failed because it depended on another request and that request
-- failed.
failedDependency :: Handler
failedDependency = bodyHandlerOf 424 Nothing

-- 425 Too Early
-- Indicates that the server is unwilling to risk processing a request that
-- might be replayed.
tooEarly :: Handler
tooEarly = bodyHandlerOf 425 Nothing

-- 426 Upgrade Required
-- The client should switch to a different protocol such as TLS/1.3, given in
-- the Upgrade header field.
upgradeRequired :: Handler
upgradeRequired = bodyHandlerOf 426 Nothing

-- 428 Precondition Required
-- The origin server requires the request to be conditional. Intended to prevent
-- the 'lost update' problem, where a client GETs a resource's state, modifies
-- it, and PUTs it back to the server, when meanwhile a third party has modified
-- the state on the server, leading to a conflict.
preconditionRequired :: Handler
preconditionRequired = bodyHandlerOf 428 Nothing

-- 429 Too Many Requests
-- The user has sent too many requests in a given amount of time. Intended for
-- use with rate-limiting schemes.
tooManyRequests :: Handler
tooManyRequests = bodyHandlerOf 429 Nothing

-- 431 Request Header Fields Too Large
-- The server is unwilling to process the request because either an individual
-- header field, or all the header fields collectively, are too large.
requestHeaderFieldsTooLarge :: Handler
requestHeaderFieldsTooLarge = bodyHandlerOf 431 Nothing

-- 451 Unavailable For Legal Reasons
-- A server operator has received a legal demand to deny access to a resource or
-- to a set of resources that includes the requested resource. The code 451 was
-- chosen as a reference to the novel Fahrenheit 451.
unavailableForLegalReasons :: Handler
unavailableForLegalReasons = bodyHandlerOf 451 Nothing

-- 5xx
-- 500 Internal Server Error
-- A generic error message, given when an unexpected condition was encountered
-- and no more specific message is suitable.
internalServerError :: Handler
internalServerError = bodyHandlerOf 500 Nothing

-- 501 Not Implemented
-- The server either does not recognize the request method, or it lacks the
-- ability to fulfil the request. Usually this implies future availability
-- (e.g., a new feature of a web-service API).
notImplemented :: Handler
notImplemented = bodyHandlerOf 501 Nothing

-- 502 Bad Gateway
-- The server was acting as a gateway or proxy and received an invalid response
-- from the upstream server.
badGateway :: Handler
badGateway = bodyHandlerOf 502 Nothing

-- 503 Service Unavailable
-- The server cannot handle the request (because it is overloaded or down for
-- maintenance).  Generally, this is a temporary state.
serviceUnavailable :: Handler
serviceUnavailable = bodyHandlerOf 503 Nothing

-- 504 Gateway Timeout
-- The server was acting as a gateway or proxy and did not receive a timely
-- response from the upstream server.
gatewayTimeout :: Handler
gatewayTimeout = bodyHandlerOf 504 Nothing

-- 505 HTTP Version Not Supported
-- The server does not support the HTTP version used in the request.
httpVersionNotSupported :: Handler
httpVersionNotSupported = bodyHandlerOf 505 Nothing

-- 506 Variant Also Negotiates
-- Transparent content negotiation for the request results in a circular
-- reference.
variantAlsoNegotiates :: Handler
variantAlsoNegotiates = bodyHandlerOf 506 Nothing

-- 507 Insufficient Storage
-- The server is unable to store the representation needed to complete the
-- request.
insufficientStorage :: Handler
insufficientStorage = bodyHandlerOf 507 Nothing

-- 508 Loop Detected
-- The server detected an infinite loop while processing the request (sent
-- instead of 208 Already Reported).
loopDetected :: Handler
loopDetected = bodyHandlerOf 508 Nothing

-- 510 Not Extended
-- Further extensions to the request are required for the server to fulfil it.
notExtended :: Handler
notExtended = bodyHandlerOf 510 Nothing

-- 511 Network Authentication Required
-- The client needs to authenticate to gain network access. Intended for use by
-- intercepting proxies used to control access to the network (e.g., "captive
-- portals" used to require agreement to Terms of Service before granting full
-- Internet access via a Wi-Fi hotspot).
networkAuthenticationRequired :: Handler
networkAuthenticationRequired = bodyHandlerOf 511 Nothing
