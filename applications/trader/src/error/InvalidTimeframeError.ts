import { Data } from 'effect'

export class InvalidTimeframeError extends Data.TaggedError(
  'InvalidTimeframeError',
)<{
  readonly value: string
}> {}
