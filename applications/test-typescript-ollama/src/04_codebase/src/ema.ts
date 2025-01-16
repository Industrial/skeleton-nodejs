import { Array as A, Effect as E, pipe } from 'effect'

/**
 * Calculates the Exponential Moving Average (EMA) for an array-effect of numbers.
 *
 * The Exponential Moving Average gives more weight to recent prices, making it more responsive to new information.
 *
 * @param length - The number of data points to consider for each EMA calculation.
 * @param alpha - A function that returns the smoothing factor alpha based on the length.
 * @returns A function that takes an array-effect of numbers and returns an array-effect of the computed EMA values.
 *
 * @example
 *
 * const ema5 = ema(5);
 * const values = E.succeed([1, 2, 3, 4, 5, 6, 7, 8, 9, 10]);
 * const result = await E.runPromise(ema5(values));
 * // result is [1, 1.333, 1.888, 2.593, 3.396, 4.265, 5.176, 6.117, 7.078, 8.053]
 */
export type Alpha = (length: number) => number

export const emaAlpha = (length: number): number => 2 / (length + 1)

export const ema =
  (length: number, alpha: Alpha = emaAlpha) =>
  (values: E.Effect<Array<number>>) =>
    pipe(
      values,
      E.flatMap((as) => {
        const [firstValue] = as

        if (as.length === 0 || firstValue === undefined) {
          return E.succeed([])
        }

        return pipe(
          A.range(1, as.length),
          A.reduce(
            E.succeed([firstValue]),
            (previousE: E.Effect<Array<number>>, i: number) =>
              pipe(
                previousE,
                E.flatMap((previousArray) => {
                  const lastPreviousValue =
                    previousArray[previousArray.length - 1]
                  if (lastPreviousValue === undefined) {
                    return E.succeed(previousArray)
                  }
                  const currentValue = as[i]
                  if (currentValue === undefined) {
                    return E.succeed(previousArray)
                  }
                  const computedEMA =
                    alpha(length) * currentValue +
                    (1 - alpha(length)) * lastPreviousValue
                  return E.succeed(previousArray.concat(computedEMA))
                }),
              ),
          ),
        )
      }),
    )
