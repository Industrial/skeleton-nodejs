import { average, sliceE } from "@code9/array";
import { Array as A, Effect as E, Either, pipe } from "effect";

/**
 * Calculates the Simple Moving Average (SMA) over a specified length of data points.
 *
 * The Simple Moving Average is the unweighted mean of the previous n data points.
 *
 * @param length - The number of data points to consider for each SMA calculation.
 * @returns A function that takes an array-effect of numbers and returns an array-effect of the computed SMA values.
 *
 * @example
 *
 * const sma5 = sma(5);
 * const values = E.succeed([1, 2, 3, 4, 5, 6, 7, 8, 9, 10]);
 * const result = await E.runPromise(sma5(values));
 * // result is [3, 4, 5, 6, 7, 8]
 */
export const sma = (length: number) => (values: E.Effect<Array<number>>) =>
	pipe(
		values,
		E.flatMap((as) =>
			pipe(
				A.range(0, as.length - length + 1),
				A.map((i) =>
					pipe(
						as,
						sliceE(i, i + length),
						E.flatMap((b) =>
							pipe(
								average(b),
								Either.fromOption(() => new Error("Calculation failed")),
							),
						),
					),
				),
				E.all,
			),
		),
	);
