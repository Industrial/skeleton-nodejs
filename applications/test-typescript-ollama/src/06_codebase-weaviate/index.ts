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

// Metadata file path
const metadataFilePath = path.join(__dirname, ".import-metadata.json");

const result = await importDocuments(sourceDirectory, metadataFilePath);

// Remove old documents if any files changed
if (result.oldDocuments.length > 0) {
	console.log(
		`Removing vectors for ${result.oldDocuments.length} changed documents...`,
	);
	await vectorStore.delete({
		filter: {
			$or: result.oldDocuments.map((doc) => ({
				fileHash: doc.metadata.fileHash,
			})),
		},
	});
	console.log("Old vectors removed successfully");
}

// Add new documents if any files changed
if (result.newDocuments.length > 0) {
	console.log(
		`Adding ${result.newDocuments.length} new documents to vector store...`,
	);
	await vectorStore.addDocuments(result.newDocuments);
	console.log("New documents added successfully");
}

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
	input: "List refactor opportunities in the src directory",
});

console.log(response);
