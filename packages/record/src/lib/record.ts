export const entries = <K extends string, V>(object: Record<K, V>): Array<[K, V]> => {
  return Object.entries(object) as Array<[K, V]>
}
