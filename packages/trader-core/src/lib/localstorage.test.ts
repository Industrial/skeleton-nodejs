
// TODO: Bun doesn't have window or localStorage.
// describe('localstorage', () => {
//   describe('createKey', () => {
//     describe('when given a key', () => {
//       it('should return a string', () => {
//         const expected = 'browser_trader_test'
//         const actual = createKey('test')
//         expect(actual).toBe(expected)
//       })
//     })
//   })

//   describe('get', () => {
//     describe('when the key does not exist', () => {
//       it('should return undefined', () => {
//         const getSpy = spyOn(window.localStorage, 'get')
//         const actual = get('test')
//         expect(actual).toBeUndefined()
//         expect(getSpy).toHaveBeenCalledTimes(1)
//       })
//     })
//   })

//   describe('set', () => {
//     describe('when setting a key', () => {
//       it('should return the value through get', () => {
//         const expected = 'derp'
//         set('herp', 'derp')
//         const actual = get('herp')
//         expect(actual).toEqual(expected)
//       })
//     })
//   })

//   describe('del', () => {
//     describe('when deleting a key', () => {
//       it('should delete the value in the localStorage', () => {
//         set('herp', 'derp')
//         del('herp')
//         const actual = get('herp')
//         expect(actual).toBeUndefined()
//       })
//     })
//   })
// })

