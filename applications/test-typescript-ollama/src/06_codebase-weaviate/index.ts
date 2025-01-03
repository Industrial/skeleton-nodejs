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

const initializeVectorStore = () => {
	const embeddings = new OllamaEmbeddings({
		baseUrl: `http://${ollamaHost}:${ollamaPort}`,
		model: ollamaEmbeddingsModel,
	});

	const vectorStore = new lancedb.LanceDB(embeddings, {
		mode: "overwrite",
		tableName: collectionName,
	});

	return E.succeed(vectorStore);
};

const importAndProcessDocuments = (directory: string) =>
	pipe(
		importDocuments(directory),
		E.tap((documents) =>
			E.succeed(console.log(`Imported ${documents.length} documents`)),
		),
	);

const addDocumentsToVectorStore =
	(vectorStore: lancedb.LanceDB) =>
	(documents: Document<Record<string, unknown>>[]) =>
		pipe(
			E.tryPromise(() => vectorStore.addDocuments(documents)),
			E.tap(() => E.succeed(console.log("Documents added successfully"))),
		);

const configureRetriever = (vectorStore: lancedb.LanceDB) => () =>
	pipe(
		E.tryPromise(() => vectorStore.similaritySearch("", 9999)),
		E.map((docs) => docs.length),
		E.tap((totalDocuments) =>
			E.succeed(
				console.log(
					`Retriever configured successfully using ${totalDocuments} total documents`,
				),
			),
		),
		E.map((totalDocuments) => vectorStore.asRetriever({ k: totalDocuments })),
	);

const createRetrievalChainEffect = (
	retriever: VectorStoreRetriever<lancedb.LanceDB>,
) => {
	const retrievalModel = new Ollama({
		baseUrl: "http://0.0.0.0:11434",
		model: "codegemma:latest",
	});

	const prompt = ChatPromptTemplate.fromTemplate(`
        Based on the following information from retrieved documents: {context}\n
        Answer the following question or inquiry: {input}
    `);

	const chain = prompt.pipe(retrievalModel);

	return E.tryPromise(() =>
		createRetrievalChain({
			combineDocsChain: chain,
			retriever,
		}),
	);
};

const sourceDirectory = path.join(__dirname, ".");
console.log("Starting document import from:", sourceDirectory);

const program = pipe(
	initializeVectorStore(),
	E.flatMap((vectorStore) =>
		pipe(
			importAndProcessDocuments(sourceDirectory),
			E.flatMap(addDocumentsToVectorStore(vectorStore)),
			E.flatMap(configureRetriever(vectorStore)),
			E.flatMap(createRetrievalChainEffect),
			E.flatMap((retrievalChain) =>
				pipe(
					E.tryPromise(() =>
						retrievalChain.invoke({
							input:
								"Rewrite the processFile function in the importer.ts file to use the `pipe()` function instead of `E.gen()`.",
						}),
					),
					E.tap((response) =>
						E.tryPromise(() => fs.writeFile("output.md", response.answer)),
					),
					E.tap((response) => E.succeed(console.log(response))),
				),
			),
		),
	),
);

await E.runPromise(program);
