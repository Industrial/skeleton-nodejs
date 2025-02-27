import { Schema } from 'effect'

export const RawOHLCVSchema = Schema.Array(
  Schema.Tuple(
    Schema.Number, // timestamp
    Schema.Number, // open
    Schema.Number, // high
    Schema.Number, // low
    Schema.Number, // close
    Schema.Number, // volume
  ),
)
