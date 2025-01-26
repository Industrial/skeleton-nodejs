import {
  FetchHttpClient,
  HttpClient,
  HttpClientRequest,
} from '@effect/platform'
import { RpcResolver } from '@effect/rpc'
import { HttpRpcResolver } from '@effect/rpc-http'
import { Effect } from 'effect'
import { StrictMode, useEffect } from 'react'
import React from 'react'
import { UserCreate, UserList } from '../request.js'
import type { AppRouter } from '../router.js'
import { Home } from './Home.js'

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

export const App = () => {
  useEffect(() => {
    program
      .pipe(Effect.provide(FetchHttpClient.layer), Effect.runPromise)
      .then(console.log)
  })

  return (
    <StrictMode>
      <Home />
    </StrictMode>
  )
}
