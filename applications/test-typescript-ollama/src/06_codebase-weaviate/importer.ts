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
const CHUNK_SIZE = 500;
const CHUNK_OVERLAP = 100;

// Helper functions
async function calculateFileHash(filePath: string): Promise<string> {
	const content = await fs.readFile(filePath);
	return crypto.createHash("sha256").update(content.toString()).digest("hex");
}

async function loadMetadata(
	metadataFilePath: string,
): Promise<Map<string, FileMetadata>> {
	try {
		const content = await fs.readFile(metadataFilePath, "utf-8");
		const data = JSON.parse(content) as FileMetadata[];
		return new Map(data.map((item) => [item.filePath, item]));
	} catch {
		return new Map();
	}
}

async function saveMetadata(
	metadataFilePath: string,
	metadata: Map<string, FileMetadata>,
): Promise<void> {
	const data = Array.from(metadata.values());
	await fs.writeFile(metadataFilePath, JSON.stringify(data, null, 2));
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

interface ImportResult {
	// Documents from changed files that need to be added
	newDocuments: Document[];
	// Documents from previous version of changed files that need to be removed
	oldDocuments: Document[];
	// Stats about the import
	stats: {
		totalFiles: number;
		changedFiles: number;
		unchangedFiles: number;
	};
}

// Main function to import documents
export const importDocuments = async (
	directory: string,
	metadataFilePath: string,
): Promise<ImportResult> => {
	const filePaths = await listFilesRecursively(directory);
	console.log(`Found ${filePaths.length} files to process`);

	if (filePaths.length === 0) {
		return {
			newDocuments: [],
			oldDocuments: [],
			stats: {
				totalFiles: 0,
				changedFiles: 0,
				unchangedFiles: 0,
			},
		};
	}

	// Load previous metadata
	const metadata = await loadMetadata(metadataFilePath);
	const newMetadata = new Map<string, FileMetadata>();
	const newDocuments: Document[] = [];
	const oldDocuments: Document[] = [];
	let changedFiles = 0;
	let unchangedFiles = 0;

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
			changedFiles++;
			newDocuments.push(...documents);
			console.log(`Processed file (changed): ${filePath}`);

			// If we have previous metadata, we need to remove old documents
			if (previousMetadata) {
				// Create placeholder documents with the old hash for removal
				const oldDocumentCount = documents.length; // Assume same number of chunks
				for (let i = 0; i < oldDocumentCount; i++) {
					oldDocuments.push({
						pageContent: "", // Content not needed for removal
						metadata: {
							source: filePath,
							fileHash: previousMetadata.hash,
						},
					});
				}
			}
		} else {
			unchangedFiles++;
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
	await saveMetadata(metadataFilePath, newMetadata);

	console.log(
		`Processed ${changedFiles} changed files out of ${filePaths.length} total files`,
	);

	return {
		newDocuments,
		oldDocuments,
		stats: {
			totalFiles: filePaths.length,
			changedFiles,
			unchangedFiles,
		},
	};
};
