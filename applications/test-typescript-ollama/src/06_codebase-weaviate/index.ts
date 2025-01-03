import fs from "node:fs/promises";
import path from "node:path";
import * as lancedb from "@langchain/community/vectorstores/lancedb";
import { ChatPromptTemplate } from "@langchain/core/prompts";
import { Ollama, OllamaEmbeddings } from "@langchain/ollama";
import { createRetrievalChain } from "langchain/chains/retrieval";
import { importDocuments } from "./importer";

const ollamaHost = "0.0.0.0";
const ollamaPort = 11434;
const ollamaEmbeddingsModel = "nomic-embed-text:latest";

const collectionName = "06_codebase-weaviate";

const embeddings = new OllamaEmbeddings({
	baseUrl: `http://${ollamaHost}:${ollamaPort}`,
	model: ollamaEmbeddingsModel,
});

// Initialize vector store
const vectorStore = new lancedb.LanceDB(embeddings, {
	mode: "overwrite",
	tableName: collectionName,
});

// Import and process documents from source directory
const sourceDirectory = path.join(__dirname, ".");
console.log("Starting document import from:", sourceDirectory);

const documents = await importDocuments(sourceDirectory);
console.log(`Imported ${documents.length} documents`);

console.log("Adding documents to vector store...");
await vectorStore.addDocuments(documents);
console.log("Documents added successfully");

// Get total document count for retriever configuration
const totalDocuments = (await vectorStore.similaritySearch("", 9999)).length;

// Configure retriever
const retriever = vectorStore.asRetriever({
	k: totalDocuments,
});
console.log(
	`Retriever configured successfully using ${totalDocuments} total documents`,
);

// Define the retrieval model using the codegemma model
const retrievalModel = new Ollama({
	baseUrl: "http://0.0.0.0:11434",
	model: "codegemma:latest",
});

const prompt = ChatPromptTemplate.fromTemplate(`
	Based on the following information from retrieved documents: {context}\n
  Answer the following question or inquiry: {input}
`);

const chain = prompt.pipe(retrievalModel);

const retrievalChain = await createRetrievalChain({
	combineDocsChain: chain,
	retriever,
});

const response = await retrievalChain.invoke({
	input:
		"How would you refactor the importer.ts file into smaller, unit testable functions? Can we use the Effect.ts library? Please only refactor out one part and use Effect.ts for the implementation.",
});

await fs.writeFile("output.md", response.answer);

console.log(response);
