// router.ts
import { Rpc, RpcRouter } from '@effect/rpc'
import { Effect, Ref } from 'effect'
import { User, UserById, UserCreate, UserList } from './request.js'

// ---------------------------------------------
// Imaginary Database
// ---------------------------------------------

const ref = Ref.unsafeMake<Array<User>>([
  new User({ id: '1', name: 'Alice' }),
  new User({ id: '2', name: 'Bob' }),
])

const db = {
  user: {
    findMany: () => ref.get,
    findById: (id: string) =>
      Ref.get(ref).pipe(
        Effect.andThen((users) => {
          const user = users.find((user) => user.id === id)
          return user
            ? Effect.succeed(user)
            : Effect.fail(`User not found: ${id}`)
        }),
      ),
    create: (name: string) =>
      Ref.updateAndGet(ref, (users) => [
        ...users,
        new User({ id: String(users.length + 1), name }),
      ]).pipe(Effect.andThen((users) => users[users.length - 1])),
  },
}

// ---------------------------------------------
// Router
// ---------------------------------------------

export const appRouter = RpcRouter.make(
  Rpc.effect(UserList, () => db.user.findMany()),
  Rpc.effect(UserById, ({ id }) => db.user.findById(id)),
  Rpc.effect(UserCreate, ({ name }) => db.user.create(name)),
)

export type AppRouter = typeof appRouter
