import path from "node:path";
import { ChatPromptTemplate } from "@langchain/core/prompts";
import { Ollama, OllamaEmbeddings } from "@langchain/ollama";
import { QdrantVectorStore } from "@langchain/qdrant";
import { createRetrievalChain } from "langchain/chains/retrieval";
import { importDocuments } from "./importer";

const ollamaHost = "0.0.0.0";
const ollamaPort = 11434;
const ollamaEmbeddingsModel = "nomic-embed-text:latest";

const qdrantHost = "0.0.0.0";
const qdrantPort = 6333;
const collectionName = "06_codebase-weaviate";

const embeddings = new OllamaEmbeddings({
	baseUrl: `http://${ollamaHost}:${ollamaPort}`,
	model: ollamaEmbeddingsModel,
});

// Initialize vector store
const vectorStore = await QdrantVectorStore.fromExistingCollection(embeddings, {
	url: `http://${qdrantHost}:${qdrantPort}`,
	collectionName,
});

// Import and process documents from source directory
const sourceDirectory = path.join(__dirname, "src");
console.log("Starting document import from:", sourceDirectory);

const documents = await importDocuments(
	sourceDirectory,
	async (filePath, currentHash, previousHash) => {
		if (previousHash) {
			// Delete vectors for previous version of the file
			console.log(`Removing vectors for changed file: ${filePath}`);
			await vectorStore.delete({
				filter: {
					fileHash: previousHash,
				},
			});
		}
	},
);

if (documents.length > 0) {
	// Add new/changed documents to vector store
	console.log(
		`Adding ${documents.length} changed documents to vector store...`,
	);
	await vectorStore.addDocuments(documents);
	console.log("Documents added successfully");
}

// Configure retriever
const retriever = vectorStore.asRetriever({
	k: documents.length,
});
console.log(
	`Retriever configured successfully using ${documents.length} documents`,
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
	input: "List refactor opportunities in the src directory",
});

console.log(response);
