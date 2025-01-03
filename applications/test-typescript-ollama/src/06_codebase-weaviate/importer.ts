import crypto from "node:crypto";
import fs from "node:fs/promises";
import path from "node:path";
import type { Document } from "@langchain/core/documents";
import { TextLoader } from "langchain/document_loaders/fs/text";
import { RecursiveCharacterTextSplitter } from "langchain/text_splitter";

// Types
interface FileMetadata {
	hash: string;
	filePath: string;
	lastProcessed: number;
}

// Constants
const METADATA_FILE = ".import-metadata.json";
const CHUNK_SIZE = 500;
const CHUNK_OVERLAP = 100;

// Helper functions
async function calculateFileHash(filePath: string): Promise<string> {
	const content = await fs.readFile(filePath);
	return crypto.createHash("sha256").update(content.toString()).digest("hex");
}

async function loadMetadata(
	directory: string,
): Promise<Map<string, FileMetadata>> {
	const metadataPath = path.join(directory, METADATA_FILE);
	try {
		const content = await fs.readFile(metadataPath, "utf-8");
		const data = JSON.parse(content) as FileMetadata[];
		return new Map(data.map((item) => [item.filePath, item]));
	} catch {
		return new Map();
	}
}

async function saveMetadata(
	directory: string,
	metadata: Map<string, FileMetadata>,
): Promise<void> {
	const metadataPath = path.join(directory, METADATA_FILE);
	const data = Array.from(metadata.values());
	await fs.writeFile(metadataPath, JSON.stringify(data, null, 2));
}

// Helper function to process a single file
async function processFile(
	filePath: string,
	currentHash: string,
	previousMetadata?: FileMetadata,
): Promise<{ documents: Document[]; hasChanged: boolean }> {
	// If hash hasn't changed and we have previous metadata, skip processing
	if (previousMetadata && previousMetadata.hash === currentHash) {
		return { documents: [], hasChanged: false };
	}

	const textLoader = new TextLoader(filePath);
	const loadedTexts = await textLoader.load();

	const textSplitter = new RecursiveCharacterTextSplitter({
		chunkSize: CHUNK_SIZE,
		chunkOverlap: CHUNK_OVERLAP,
	});

	const documents = await textSplitter.splitDocuments(loadedTexts);

	// Add source and hash metadata to each document
	for (const doc of documents) {
		doc.metadata = {
			...doc.metadata,
			source: filePath,
			fileHash: currentHash,
		};
	}

	return { documents, hasChanged: true };
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

// Main function to import documents
export const importDocuments = async (
	directory: string,
	onFileChanged?: (
		filePath: string,
		currentHash: string,
		previousHash?: string,
	) => Promise<void>,
): Promise<Document[]> => {
	const filePaths = await listFilesRecursively(directory);
	console.log(`Found ${filePaths.length} files to process`);

	if (filePaths.length === 0) {
		return [];
	}

	// Load previous metadata
	const metadata = await loadMetadata(directory);
	const newMetadata = new Map<string, FileMetadata>();
	const allDocuments: Document[] = [];

	// Process each file
	for (const filePath of filePaths) {
		const currentHash = await calculateFileHash(filePath);
		const previousMetadata = metadata.get(filePath);

		const { documents, hasChanged } = await processFile(
			filePath,
			currentHash,
			previousMetadata,
		);

		if (hasChanged) {
			allDocuments.push(...documents);
			console.log(`Processed file (changed): ${filePath}`);

			// Notify about the change if callback provided
			if (onFileChanged) {
				await onFileChanged(filePath, currentHash, previousMetadata?.hash);
			}
		} else {
			console.log(`Skipped file (unchanged): ${filePath}`);
		}

		// Update metadata
		newMetadata.set(filePath, {
			hash: currentHash,
			filePath,
			lastProcessed: Date.now(),
		});
	}

	// Save updated metadata
	await saveMetadata(directory, newMetadata);

	console.log(
		`Processed ${allDocuments.length} changed files out of ${filePaths.length} total files`,
	);
	return allDocuments;
};

// Export functions for testing
export const _test = {
	processFile,
	listFilesRecursively,
	calculateFileHash,
	loadMetadata,
	saveMetadata,
};
