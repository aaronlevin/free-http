Free Your Client... and Your Requests Will Follow
=================================================

`free-http` is an http-client based on Free Monads. `free-http` exposes a Free Monad to express standard http verbs as well as several backends to interpet programs written in the free monad using various http clients (currently: a pure client, an `http-client`-backed client, and a random client).

See [here](https://github.com/aaronlevin/free-http/blob/master/src/Network/HTTP/Client/Free/Examples.hs#L152) for an example.

To use free-http, simply:

1. Import Network.HTTP.Client.Free to use the library.
2. Choose your base request type by defining your own instance of the `RequestType` type family or importing one from an interpreter. E.g.

  ```
  data MyClient
  type instance RequestType MyClient = Request
  ```

  or

  ```
  import Network.HTTP.Free.Client.HttpClient (HttpClient)
  ```

3. Choose your base response type by defining your own instance of the `ResponseTYpe` type family or importing one from an interpreter. E.g.  

  ```
  type instance ResponseType MyClient = Response ByteString
  ```

  (or)

  ```
  import Network.HTTP.Free.Client.HttpClient (HttpClient)
  ```

4. Write a program in the 'FreeHttp MyClient m a' free monad.
5. Import an interpreter, such as 'HttpClient'

  ```
  import Network.HTTP.Free.Client.HttpClient
  ```

6. Run your program against the interpreter:

  ```
  runHttp (myProgram :: FreeHttp MyClient IO String)
  ```

## Design Choices

### `RequestType` and `ResponseType`

Haskell is fortunate to have several very well-designed http clients: [http-client](https://hackage.haskell.org/package/http-client-0.4.16/docs/Network-HTTP-Client.html), [wreq](http://www.serpentine.com/wreq/), [http-conduit](https://hackage.haskell.org/package/http-conduit), [pipes-http](https://hackage.haskell.org/package/pipes-http), etc. Unfortunately, a few of those clients support several different *Request* and *Response* types. To keep `free-http` flexible, we use two type families defined as:

```
type family RequestType client  :: *
type family ResponseType client :: *
```

Our `HttpF` functor is thus defined as:

```
data HttpF client a = HttpF StdMethod (RequestType client) (ResponseType client -> a)
                    deriving Functor
```

This allows our `HttpF` functor to be agnostic of the foundational request and response type, while allowing interpreter authors to specify the concrete types they need for their http client libraries (e.g. `Request` in the case of `http-client`). A consequence of this is that `free-http` clients (you) need to specify, at some point, the foundation you're using. This can be done in two ways:

1. You can define your own foundation (see above).
2. You can import one from an interpreter.

To specify your request and response foundation, use replace the `client` type in `HttpF client a` or `FreeHttp client m a` to the type signalling your foundation. For example, the http-client, pure, and arbitrary interpreters use `HttpClient`, `PureClient`, and `ArbitraryClient` respectively.
