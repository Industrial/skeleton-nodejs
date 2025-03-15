import { Schema } from 'effect'
import { ExchangeIdSchema } from '../market-data/ExchangeId'
import { TimeframeSchema } from '../market-data/Timeframe'

export const AppConfigSchema = Schema.Struct({
  exchange: ExchangeIdSchema,
  pair: Schema.String,
  timeframe: TimeframeSchema,
})

export type AppConfig = Schema.Schema.Type<typeof AppConfigSchema>
