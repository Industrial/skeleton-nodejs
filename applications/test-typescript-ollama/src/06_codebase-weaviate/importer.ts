import fs from "node:fs/promises";
import path from "node:path";
import type { Document } from "@langchain/core/documents";
import { Effect as E, pipe } from "effect";
import type { UnknownException } from "effect/Cause";
import { TextLoader } from "langchain/document_loaders/fs/text";
import { RecursiveCharacterTextSplitter } from "langchain/text_splitter";

interface FileMetadata {
	filePath: string;
	lastProcessed: number;
}

const CHUNK_SIZE = 500;
const CHUNK_OVERLAP = 100;

async function processFile(
	filePath: string,
): Promise<{ documents: Document[] }> {
	const textLoader = new TextLoader(filePath);
	const loadedTexts = await textLoader.load();

	const textSplitter = new RecursiveCharacterTextSplitter({
		chunkSize: CHUNK_SIZE,
		chunkOverlap: CHUNK_OVERLAP,
	});

	const documents = await textSplitter.splitDocuments(loadedTexts);

	for (const doc of documents) {
		doc.metadata = {
			...doc.metadata,
			source: filePath,
		};
	}

	return { documents };
}

const listFilesRecursively = async (directory: string): Promise<string[]> => {
	const entries = await fs.readdir(directory, { withFileTypes: true });

	const filePromises = entries.map(async (entry) => {
		const fullPath = path.join(directory, entry.name);
		return entry.isDirectory() ? listFilesRecursively(fullPath) : fullPath;
	});

	const files = await Promise.all(filePromises);
	return files.flat();
};

export const importDocuments = async (
	directory: string,
): Promise<Document[]> => {
	const filePaths = await listFilesRecursively(directory);
	console.log(`Found ${filePaths.length} files to process`);

	if (filePaths.length === 0) {
		return [];
	}

	let allDocuments: Document[] = [];

	for (const filePath of filePaths) {
		const { documents } = await processFile(filePath);
		allDocuments = allDocuments.concat(documents);
	}

	return allDocuments;
};
