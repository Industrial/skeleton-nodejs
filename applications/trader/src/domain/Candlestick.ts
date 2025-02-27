import type { Close } from './Close'
import type { High } from './High'
import type { Low } from './Low'
import type { Open } from './Open'
import type { Volume } from './Volume'

export type Candlestick = {
  open: Open
  high: High
  low: Low
  close: Close
  volume: Volume
}
