import { OHLCV } from './ohlcv.ts'
import { Position } from './position.ts'

export type StrategyOptionsIntegerBounds = [number, number]

export type StrategyOptions = Record<string, number>
export type StrategyOptionsBounds<T> = {
  [K in keyof T]: StrategyOptionsIntegerBounds
}

export type Strategy<T extends StrategyOptions> = (options: T) => (bars: Array<OHLCV>) => Array<Position>
