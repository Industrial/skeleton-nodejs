import { Schema } from 'effect'
import { RawOHLCVSchema } from './raw/RawOHLCVSchema'

export const OHLCVSchema = Schema.transform(
  RawOHLCVSchema,
  Schema.Array(
    Schema.Struct({
      timestamp: Schema.DateFromSelf,
      open: Schema.Number,
      high: Schema.Number,
      low: Schema.Number,
      close: Schema.Number,
      volume: Schema.Number,
    }),
  ),
  {
    decode: (raw) =>
      raw.map(([timestamp, open, high, low, close, volume]) => ({
        timestamp: new Date(timestamp),
        open,
        high,
        low,
        close,
        volume,
      })),
    encode: (transformed) =>
      transformed.map(({ timestamp, open, high, low, close, volume }) => [
        timestamp.getTime(),
        open,
        high,
        low,
        close,
        volume,
      ]),
  },
)
