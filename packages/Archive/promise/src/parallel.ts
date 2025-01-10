import { isUndefined } from '@susu/undefined'

export type ParallelJob<A> = {
  index: number
  promise: Promise<[number, A]>
  resolved: boolean
  result: A | undefined
}

const jobIsNotResolved = <A>(a: ParallelJob<A>) => a.resolved === false
const getJobPromise = <A>(a: ParallelJob<A>) => a.promise
const getJobResult = <A>(a: ParallelJob<A>) => a.result as A

const getResults = <A>(jobs: Array<ParallelJob<A>>): Array<A> => {
  return jobs.map(getJobResult)
}

const waitForNextJob = async <A>(jobs: Array<ParallelJob<A>>) => {
  const waitingJobs = jobs.filter(jobIsNotResolved)
  const jobPromises = waitingJobs.map(getJobPromise)
  const [finishedIndex, finishedResult] = await Promise.race(jobPromises)
  jobs[finishedIndex].resolved = true
  jobs[finishedIndex].result = finishedResult
}

const createJobRunner = async <A extends unknown[]>(
  index: number,
  deferreds: {
    [K in keyof A]: () => Promise<A[K]>
  },
): Promise<[number, A]> => {
  const result = (await deferreds[index]()) as A
  return [index, result]
}

// export async function test<A extends unknown[]>(
//   deferreds: {
//     [K in keyof A]: () => Promise<A[K]>
//   },
// ): Promise<A> {
//   const promises = deferreds.map((deferred) => deferred())
//   return Promise.all(promises) as Promise<A>
// }
// const abc = await test([
//   async () => 'a',
//   async () => 123,
//   async () => new Date(),
//   async () => /asdf/,
// ])

/**
 * Runs an array of deferreds in parallel, with an optional concurrency limit.
 *
 * @template A - The type of the resolved promise values.
 * @param deferreds - The array of deferreds to execute.
 * @param limit - The maximum number of concurrent promises.
 * @returns A wrapper promise that resolves with an array of results.
 */
export const parallel = async <A extends unknown[]>(
  deferreds: {
    [K in keyof A]: () => Promise<A[K]>
  },
  limit: number = deferreds.length,
): Promise<A> => {
  if (
    isUndefined(limit) ||
    limit >= deferreds.length ||
    deferreds.length === 0
  ) {
    const mapped = deferreds.map((fn) => fn())
    const result = Promise.all(mapped) as Promise<A>
    return result
  }

  type JobAElement = A[number]
  const jobs: Array<ParallelJob<JobAElement>> = []

  const executeConcurrent = async (index: number): Promise<void> => {
    if (index >= deferreds.length) {
      await waitForNextJob(jobs)
      return
    }

    jobs.push({
      index,
      promise: createJobRunner(index, deferreds),
      resolved: false,
      result: undefined,
    })

    const waiting = jobs.filter(jobIsNotResolved)

    if (waiting.length >= limit) {
      await waitForNextJob(jobs)
    }

    await executeConcurrent(index + 1)
  }

  await executeConcurrent(0)

  const results = getResults(jobs)

  return results as A
}
