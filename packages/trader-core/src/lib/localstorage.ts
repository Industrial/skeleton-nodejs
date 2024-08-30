export const createKey = (key: string): string =>
  `browser_trader_${key}`

export const get = (key: string): string | undefined => {
  const result = window.localStorage.getItem(createKey(key))
  if (result === null) {
    return undefined
  }
  return result
}

export const set = (key: string, value: string): void => {
  window.localStorage.setItem(createKey(key), value)
}

export const del = (key: string): void => {
  window.localStorage.removeItem(createKey(key))
}
