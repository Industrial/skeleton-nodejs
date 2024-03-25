export const entriesValues = <T>(entries: Array<[string, T]>): Array<T> => {
  return entries.map(([, value]) => {
    return value
  })
}

export const sortedEntries = <T>(object: Record<string, T>): Array<[string, T]> => {
  return Object.entries(object).sort((a, b) => {
    return a[0].localeCompare(b[0])
  })
}

export const bag = <T>(size: number, object: Record<string, T>, v?: T, k?: string): Record<string, T> => {
  const entries = sortedEntries(k && v ? { ...object, [k]: v } : object)

  // We should slice from 0, because when size > entries.length, it becomes
  // negative and removes entries from the end of the array, which is not what
  // we want.
  const sliceLength = entries.length - Math.min(size, entries.length)

  return Object.fromEntries(entries.slice(sliceLength))
}
