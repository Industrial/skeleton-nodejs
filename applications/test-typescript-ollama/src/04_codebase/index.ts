import fs from 'node:fs/promises'
import path from 'node:path'
import { ChatPromptTemplate } from '@langchain/core/prompts'
import { Ollama, OllamaEmbeddings } from '@langchain/ollama'
import { createRetrievalChain } from 'langchain/chains/retrieval'
import { TextLoader } from 'langchain/document_loaders/fs/text'
import { RecursiveCharacterTextSplitter } from 'langchain/text_splitter'
import { MemoryVectorStore } from 'langchain/vectorstores/memory'

// Define base URL and model
const baseUrl = 'http://0.0.0.0:11434'
const embeddingModel = new OllamaEmbeddings({
  baseUrl,
  model: 'codegemma:latest',
})
console.log('Created embedding model')

// Initialize vector store
const vectorstore = new MemoryVectorStore(embeddingModel)
console.log('Created vectorstore')

const sourceDirectory = path.join(process.cwd(), 'src')
console.log('sourceDirectory', sourceDirectory)

// Function to recursively load files from a directory
async function loadFilesFromDirectory(directory: string): Promise<string[]> {
  const entries = await fs.readdir(directory, { withFileTypes: true })
  const files = entries.map((entry) => {
    const fullPath = path.join(directory, entry.name)
    if (entry.isDirectory()) {
      return loadFilesFromDirectory(fullPath)
    }
    return fullPath
  })
  return (await Promise.all(files)).flat()
}

// Load all files from the src directory.
const filePaths = await loadFilesFromDirectory(sourceDirectory)
console.log(`Loaded ${filePaths.length} files from source directory.`)

// Load each file into text loader
for (const filePath of filePaths) {
  const textLoader = new TextLoader(filePath)
  const loadedTexts = await textLoader.load()
  console.log(`Loaded texts from ${filePath}`)

  const textSplitter = new RecursiveCharacterTextSplitter({
    chunkSize: 500,
    chunkOverlap: 100,
  })

  const chunks = await textSplitter.splitDocuments(loadedTexts)
  console.log(`Split ${filePath} into ${chunks.length} chunks`)

  await vectorstore.addDocuments(chunks)
  console.log(`Added documents from ${filePath} to vectorstore`)
}

// Configure retriever
const retriever = vectorstore.asRetriever({
  k: 3,
})

// Define the retrieval model using the codegemma model
const retrievalModel = new Ollama({
  baseUrl: 'http://0.0.0.0:11434',
  model: 'codegemma:latest',
})

const prompt = ChatPromptTemplate.fromTemplate(`
	Based on the following information from retrieved documents: {context}\n
  Answer the following question or inquiry: {input}
`)

const chain = prompt.pipe(retrievalModel)

const retrievalChain = await createRetrievalChain({
  combineDocsChain: chain,
  retriever,
})

const response = await retrievalChain.invoke({
  input: 'Describe the main logic used in the src directory.',
})

console.log(response)
