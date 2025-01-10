import fs from "node:fs/promises";
import path from "node:path";
import { Effect as E, pipe } from "effect";
import { type Answer, type Question, importDocuments } from "./importer";
import {
	createOllamaEmbeddingsModel,
	createOllamaRetrievalModel,
} from "./model";
import { askQuestionToRetrievalChain, createRetrievalChain } from "./retriever";
import {
	addDocumentsToVectorStore,
	createLanceDBVectorStore,
	createVectorStoreRetriever,
} from "./store";

const ollamaHost = "0.0.0.0";
const ollamaPort = 11434;
const ollamaEmbeddingsModel = "nomic-embed-text:latest";
const ollamaRetrievalModel = "codegemma:latest";
const collectionName = "06_codebase-weaviate";

const sourceDirectory = path.join(__dirname, "..", "src");

const writeResponseToFile = (response: Answer): Promise<void> =>
	fs.writeFile("output.md", response.answer);

const logAnswer = (response: Answer) => console.log(response);

const askQuestion = (question: Question, targetFilePath?: string) => {
	const embeddingsModel = createOllamaEmbeddingsModel(
		ollamaHost,
		ollamaPort,
		ollamaEmbeddingsModel,
	);
	const retrievalModel = createOllamaRetrievalModel(
		ollamaHost,
		ollamaPort,
		ollamaRetrievalModel,
	);
	const vectorStore = createLanceDBVectorStore(embeddingsModel, collectionName);
	const retriever = createVectorStoreRetriever(9999, vectorStore);

	const chatPromptTemplate = `
		You are a helpful coding assistant. Use the following source code context to answer the question.
		Each piece of context has a source file path in its metadata - only use context from relevant files.
		
		Context from source files:
		{context}
		
		Question or task to address:
		{input}
		
		When providing code examples or suggestions, ensure they are consistent with the specific file's context and patterns.
	`;

	return pipe(
		importDocuments(sourceDirectory),
		E.flatMap((documents) =>
			E.tryPromise(() => addDocumentsToVectorStore(vectorStore, documents)),
		),
		E.flatMap(() =>
			E.tryPromise(() =>
				createRetrievalChain(chatPromptTemplate, retrievalModel, retriever),
			),
		),
		E.flatMap((retrievalChain) =>
			pipe(
				E.tryPromise(() =>
					askQuestionToRetrievalChain(question, targetFilePath)(retrievalChain),
				),
				E.tap((answer) => E.tryPromise(() => writeResponseToFile(answer))),
				E.tap((answer) => E.succeed(logAnswer(answer))),
			),
		),
	);
};

const question: Question =
	"Given the documentation of all of Effect.ts, give me the best possible `modelfile` to fine-tune the CodeGemma model for Effect.ts-specific tasks.";

await E.runPromise(askQuestion(question));
