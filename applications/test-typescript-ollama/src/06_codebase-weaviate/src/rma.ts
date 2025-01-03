import type { Alpha } from "./ema.ts";
import { ema } from "./ema.ts";

/**
 * Calculates the Running Moving Average (RMA) using the Exponential Moving Average (EMA) implementation.
 *
 * The Running Moving Average uses the formula alpha = 1 / length for smoothing.
 *
 * @param length - The number of data points to consider for each RMA calculation.
 * @returns A function that takes an array-effect of numbers and returns an array-effect of the computed RMA values.
 *
 * @example
 *
 * const rma5 = rma(5);
 * const values = E.succeed([1, 2, 3, 4, 5, 6, 7, 8, 9, 10]);
 * const result = await E.runPromise(rma5(values));
 * // result would be equivalent to using EMA with alpha = 1 / length
 */
export const rmaAlpha: Alpha = (length: number) => 1 / length;

export const rma = (length: number) => ema(length, rmaAlpha);
