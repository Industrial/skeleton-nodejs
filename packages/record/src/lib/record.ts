export const entries = <K extends string, V>(object: Record<K, V>): Array<[K, V]> =>
Object.entries(object) as Array<[K, V]>
