import type { Dirent } from "node:fs";
import fs from "node:fs/promises";
import path from "node:path";
import type { Document } from "@langchain/core/documents";
import { Array as A, Effect as E, pipe } from "effect";
import type { UnknownException } from "effect/Cause";
import { TextLoader } from "langchain/document_loaders/fs/text";
import { RecursiveCharacterTextSplitter } from "langchain/text_splitter";

const CHUNK_SIZE = 500;
const CHUNK_OVERLAP = 100;

export const addMetadata =
	(filePath: string) =>
	(documents: Document[]): Document[] =>
		pipe(
			documents,
			A.map((doc) => ({
				...doc,
				metadata: {
					...doc.metadata,
					filePath: filePath.replace(__dirname, ""),
				},
			})),
		);

export const processFile = (
	filePath: string,
): E.Effect<Document[], UnknownException, never> => {
	const textLoader = new TextLoader(filePath);
	const textSplitter = new RecursiveCharacterTextSplitter({
		chunkSize: CHUNK_SIZE,
		chunkOverlap: CHUNK_OVERLAP,
	});

	return pipe(
		E.tryPromise(() => textLoader.load()),
		E.flatMap((loadedTexts) =>
			E.tryPromise(() => textSplitter.splitDocuments(loadedTexts)),
		),
		E.map(addMetadata(filePath)),
		E.tap((documents) => E.succeed(console.log(documents))),
	);
};

export const listFilesRecursively = (
	directoryPath: string,
): E.Effect<string[], UnknownException, never> =>
	pipe(
		E.tryPromise(() => fs.readdir(directoryPath, { withFileTypes: true })),
		E.flatMap((entries: Dirent[]) =>
			pipe(
				entries,
				A.map((entry) =>
					entry.isDirectory()
						? listFilesRecursively(path.join(directoryPath, entry.name))
						: E.succeed([path.join(directoryPath, entry.name)]),
				),
				E.all,
				E.map((x) => x.flat()),
			),
		),
	);

export const importDocuments = (
	directory: string,
): E.Effect<Document[], UnknownException, never> =>
	pipe(
		listFilesRecursively(directory),
		E.tap((filePaths) => E.succeed(console.log(filePaths))),
		E.map((filePaths) => {
			console.log(`Found ${filePaths.length} files to process`);
			return filePaths;
		}),
		E.flatMap((filePaths) =>
			pipe(
				filePaths,
				A.map(processFile),
				E.all,
				E.map((documentArrays) => documentArrays.flat()),
			),
		),
	);
