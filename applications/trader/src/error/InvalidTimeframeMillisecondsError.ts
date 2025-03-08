import { Data } from 'effect'

export class InvalidTimeframeMillisecondsError extends Data.TaggedError(
  'InvalidTimeframeMillisecondsError',
)<{
  readonly value: number
}> {}
