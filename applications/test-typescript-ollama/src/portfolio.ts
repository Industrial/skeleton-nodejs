import { HumanMessage } from "@langchain/core/messages";
import { Ollama } from "@langchain/ollama";
import { marketData } from "./tools/marketData";

const hostname = "0.0.0.0";
const port = 11434;

const marketDataJSON = await marketData.invoke({
	currency: "usd",
	order: "descending",
	perPage: 100,
	page: 1,
	sparkline: false,
});

const model = new Ollama({
	baseUrl: `http://${hostname}:${port}`,
	model: "0xroyce/plutus:latest",
	format: "json",
});

const stream = await model.stream([
	new HumanMessage(`
    This is Crypto Market Data:

    \`\`\`json
    ${marketDataJSON}
    \`\`\`

    You are a financial advisor AI specializing in creating cryptocurrency
    portfolio distributions. Generate a diversified investment portfolio based on
    the following parameters:

    1. Risk Tolerance: Medium.
    2. Investment Horizon: 5 years.
    3. Initial Investment Amount: 10000.

    - Ensure the output is formatted as a JSON object as follows:
      {
        "<ticker>": <percentage>"
      }
    - Make sure the ticker is a valid crypto ticker.
    - Make sure the percentages are whole numbers that sum to 100.
    - Ensure between 10 and 20 tickers are included in the output.
  `),
]);

const chunks = [];
for await (const chunk of stream) {
	chunks.push(chunk);
}

console.log(chunks.join(""));
