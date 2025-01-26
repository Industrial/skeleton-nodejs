import { toHttpApp } from '@effect/rpc-http/HttpRpcRouter'
import { appRouter } from '../router.js'

export const rpc = () => toHttpApp(appRouter)
