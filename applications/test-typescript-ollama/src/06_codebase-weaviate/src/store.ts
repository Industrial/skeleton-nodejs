import { LanceDB } from "@langchain/community/vectorstores/lancedb";
import type { VectorStoreRetriever } from "@langchain/core/vectorstores";
import type { OllamaEmbeddings } from "@langchain/ollama";
import {
	type DocumentWithMetadata,
	filterDocumentByFilePath,
} from "./importer";

export const createLanceDBVectorStore = (
	embeddingsModel: OllamaEmbeddings,
	collectionName: string,
): LanceDB =>
	new LanceDB(embeddingsModel, {
		mode: "overwrite",
		tableName: collectionName,
	});

export const addDocumentsToVectorStore = (
	vectorStore: LanceDB,
	documents: DocumentWithMetadata[],
): Promise<void> => vectorStore.addDocuments(documents);

export const createVectorStoreRetriever = (
	k: number,
	vectorStore: LanceDB,
): VectorStoreRetriever<LanceDB> =>
	vectorStore.asRetriever({
		k,
		filter: filterDocumentByFilePath,
		searchType: "similarity",
	});
