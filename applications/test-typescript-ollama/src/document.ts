import path from "node:path";
import { ChatPromptTemplate } from "@langchain/core/prompts";
import { Ollama, OllamaEmbeddings } from "@langchain/ollama";
import { createRetrievalChain } from "langchain/chains/retrieval";
import { TextLoader } from "langchain/document_loaders/fs/text";
import { RecursiveCharacterTextSplitter } from "langchain/text_splitter";
import { MemoryVectorStore } from "langchain/vectorstores/memory";

const baseUrl = "http://0.0.0.0:11434";

const embeddingModel = new OllamaEmbeddings({
	baseUrl,
	model: "granite-embedding:30m",
});
console.log("Created embedding model");

const vectorstore = new MemoryVectorStore(embeddingModel);
console.log("Created vectorstore");

const documentsPath = path.join(process.cwd(), "src/document.txt");
console.log("documentsPath", documentsPath);
const textLoader = new TextLoader(documentsPath);
console.log("Created textLoader");

const loadedTexts = await textLoader.load();
console.log("Loaded texts");

const textSplitter = new RecursiveCharacterTextSplitter({
	chunkSize: 500,
	chunkOverlap: 100,
});

const chunks = await textSplitter.splitDocuments(loadedTexts);
console.log(`Split document into ${chunks.length} chunks`);

await vectorstore.addDocuments(chunks);
console.log("Added documents to vectorstore");

const retriever = vectorstore.asRetriever({
	k: 3,
});

const retrievalModel = new Ollama({
	baseUrl: "http://0.0.0.0:11434",
	model: "llama3.2:latest",
});

const prompt = ChatPromptTemplate.fromTemplate(`
  Answer the following question or inquiry: {input}
`);

const chain = prompt.pipe(retrievalModel);

const retrievalChain = await createRetrievalChain({
	combineDocsChain: chain,
	retriever,
});

const response = await retrievalChain.invoke({
	input:
		"List the observations made about habits of living in differnet countries",
});

console.log(response);
