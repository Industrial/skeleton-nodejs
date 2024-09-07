import { directoryExists } from '@code9/fs'
import { Effect as Fx, pipe } from 'effect'

const main1 = async () => {
  const directoryPath = process.argv[2]
  console.log('directoryPath', directoryPath)

  const x = pipe(
    directoryExists(directoryPath),
  )

  const y = Fx.runFork(x)
}

const main = () =>
  pipe(
    Fx.fromNullable(process.argv[2]),
    Fx.flatMap((directoryPath) => {
      console.log('Directory path:', directoryPath)
    }),
    // Fx.flatMap(directoryExists),
    // Fx.flatMap((doesExist) => {
    //   // doesExist
    //   //   ? pipe(
    //   //     readdir(process.argv[2]),
    //   //     Fx.map((dirContent) => {
    //   //       console.log('Directory contents:', dirContent)
    //   //       return dirContent
    //   //     }),
    //   //     Fx.mapError((error) => {
    //   //       console.error('Error reading directory:', error)
    //   //       throw error
    //   //     }))
    //   //   : Fx.succeed(null)
    //   console.log('Does exist:', doesExist)
    // }),
    Fx.log,
  )

// await Fx.runPromise(main())
await main1()
