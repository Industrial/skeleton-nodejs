import { Market } from '../market.ts'

export const createMarket = (
  base: string,
  quote: string,
  active: boolean,
  type: 'future' | 'margin' | 'option' | 'spot' | 'swap',
): Market => {
  return {
    id: `${base}/${quote}`,
    base,
    baseId: base,
    quote,
    quoteId: quote,
    symbol: `${base}/${quote}`,
    active,
    type,
    spot: true,
    margin: false,
    swap: false,
    future: false,
    option: false,
    contract: false,
    settle: '',
    settleId: '',
    contractSize: 1,
    linear: false,
    inverse: false,
    expiry: 1,
    expiryDatetime: 'abc',
    strike: 1,
    optionType: 'abc',
    precision: {
      amount: 1,
      price: 1,
    },
    limits: {},
    info: 'abc',
  }
}
