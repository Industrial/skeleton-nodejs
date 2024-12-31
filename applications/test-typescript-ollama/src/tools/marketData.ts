import { tool } from "@langchain/core/tools";
import { z } from "zod";

export const name = "MarketData";

export const description = "Fetch market data from CoinGecko";

export const MarketDataSchema = z.object({
	currency: z.string(),
	order: z.enum(["ascending", "descending"]),
	perPage: z.number(),
	page: z.number(),
	sparkline: z.boolean(),
});

export const marketData = tool(
	async ({
		currency = "usd",
		order = "descending",
		perPage = 100,
		page = 1,
		sparkline = false,
	}: z.infer<typeof MarketDataSchema>) => {
		const hostname = "api.coingecko.com";
		const port = 80;
		const path = "/api/v3/coins/markets";
		const querySting = new URLSearchParams({
			vs_currency: currency,
			order: order === "descending" ? "market_cap_desc" : "market_cap_asc",
			per_page: String(perPage),
			page: String(page),
			sparkline: String(sparkline),
		});
		const url = new URL(`http://${hostname}:${port}${path}?${querySting}`);

		const marketDataResponse = await fetch(url);
		const marketDataJSON = await marketDataResponse.json();

		return marketDataJSON;
	},
	{
		name,
		description,
		schema: MarketDataSchema,
	},
);
