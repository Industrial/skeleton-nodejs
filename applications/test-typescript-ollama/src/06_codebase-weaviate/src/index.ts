// TODO: import the complete documentation of Effect.ts with a scraper and save
// it to a file for caching, then import the file to the vector store and use
// that as knowledge for refactoring code.

import fs from "node:fs/promises";
import path from "node:path";
import * as lancedb from "@langchain/community/vectorstores/lancedb";
import type { Document } from "@langchain/core/documents";
import { ChatPromptTemplate } from "@langchain/core/prompts";
import type { VectorStoreRetriever } from "@langchain/core/vectorstores";
import { Ollama, OllamaEmbeddings } from "@langchain/ollama";
import { Effect as E, pipe } from "effect";
import { createRetrievalChain } from "langchain/chains/retrieval";
import { importDocuments } from "./importer";

const ollamaHost = "0.0.0.0";
const ollamaPort = 11434;
const ollamaEmbeddingsModel = "nomic-embed-text:latest";
const collectionName = "06_codebase-weaviate";

const createVectorStore = () =>
	new lancedb.LanceDB(
		new OllamaEmbeddings({
			baseUrl: `http://${ollamaHost}:${ollamaPort}`,
			model: ollamaEmbeddingsModel,
		}),
		{
			mode: "overwrite",
			tableName: collectionName,
		},
	);

const addDocumentsToVectorStore =
	(vectorStore: lancedb.LanceDB) =>
	(documents: Document<Record<string, unknown>>[]) =>
		E.tryPromise(() => vectorStore.addDocuments(documents));

const configureRetriever = (vectorStore: lancedb.LanceDB) => () =>
	pipe(
		E.tryPromise(() => vectorStore.similaritySearch("", 9999)),
		E.map((docs) => docs.length),
		E.map((totalDocuments) => vectorStore.asRetriever({ k: totalDocuments })),
	);

const createRetrievalChainEffect = (
	retriever: VectorStoreRetriever<lancedb.LanceDB>,
) =>
	E.tryPromise(() =>
		createRetrievalChain({
			combineDocsChain: ChatPromptTemplate.fromTemplate(`
				Based on the following information from retrieved documents: {context}\n
				Answer the following question or inquiry: {input}
			`).pipe(
				new Ollama({
					baseUrl: "http://0.0.0.0:11434",
					model: "codegemma:latest",
				}),
			),
			retriever,
		}),
	);

const sourceDirectory = path.join(__dirname, "..", "src");
console.log("Starting document import from:", sourceDirectory);

const writeResponseToFile = (response: { answer: string }) =>
	E.tryPromise(() => fs.writeFile("output.md", response.answer));

const logAnswer = (response: { answer: string }) =>
	E.succeed(console.log(response));

const askQuestion = (question: string) =>
	pipe(
		E.succeed(createVectorStore()),
		E.flatMap((vectorStore) =>
			pipe(
				importDocuments(sourceDirectory),
				E.flatMap(addDocumentsToVectorStore(vectorStore)),
				E.flatMap(configureRetriever(vectorStore)),
				E.flatMap(createRetrievalChainEffect),
				E.flatMap((retrievalChain) =>
					pipe(
						E.tryPromise(() =>
							retrievalChain.invoke({
								input: question,
							}),
						),
						E.tap(writeResponseToFile),
						E.tap(logAnswer),
					),
				),
			),
		),
	);

const question = `
	\`\`\`
	const initializeVectorStore = () => {
		const embeddings = new OllamaEmbeddings({
			baseUrl: \`http://${ollamaHost}:${ollamaPort}\`,
			model: ollamaEmbeddingsModel,
		});

		const vectorStore = new lancedb.LanceDB(embeddings, {
			mode: "overwrite",
			tableName: collectionName,
		});

		return E.succeed(vectorStore);
	};
	\`\`\`

	Rewrite the \`initializeVectorStore\` function using a \`pipe()\` function.
`;

await E.runPromise(askQuestion(question));
