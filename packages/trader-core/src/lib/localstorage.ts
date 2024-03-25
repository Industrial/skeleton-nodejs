import { Maybe } from './maybe.ts'

export const createKey = (key: string): string => `browser_trader_${key}`

export const get = (key: string): Maybe<string> => {
  const result = localStorage.getItem(createKey(key))
  if (result === null) {
    return undefined
  }
  return result
}

export const set = (key: string, value: string): void => {
  localStorage.setItem(createKey(key), value)
}

export const del = (key: string): void => {
  localStorage.removeItem(createKey(key))
}
