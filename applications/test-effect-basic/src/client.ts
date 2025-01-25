// client.ts
import {
  FetchHttpClient,
  HttpClient,
  HttpClientRequest,
} from '@effect/platform'
import { RpcResolver } from '@effect/rpc'
import { HttpRpcResolver } from '@effect/rpc-http'
import { Effect } from 'effect'
import { UserCreate, UserList } from './request.js'
import type { AppRouter } from './router.js'

// Define an effect which creates the client
const makeClient = Effect.gen(function* () {
  const baseClient = yield* HttpClient.HttpClient
  const client = baseClient.pipe(
    HttpClient.filterStatusOk,
    HttpClient.mapRequest(
      HttpClientRequest.prependUrl('http://localhost:3000/rpc'),
    ),
  )
  return RpcResolver.toClient(HttpRpcResolver.make<AppRouter>(client))
})

// Use the client
const program = Effect.gen(function* () {
  const client = yield* makeClient
  let users = yield* client(new UserList())
  if (!users.find((user) => user.id === '3')) {
    console.log(`Creating user "Charlie"`)
    yield* client(new UserCreate({ name: 'Charlie' }))
    users = yield* client(new UserList())
  } else {
    console.log(`User "Charlie" already exists`)
  }
  return users
})

program
  .pipe(Effect.provide(FetchHttpClient.layer), Effect.runPromise)
  .then(console.log)
