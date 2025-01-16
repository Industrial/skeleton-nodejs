import type { Document } from '@langchain/core/documents'
import { OllamaEmbeddings } from '@langchain/ollama'
import { QdrantVectorStore } from '@langchain/qdrant'

const qdrantHost = '0.0.0.0'
const qdrantPort = 6333

const embeddings = new OllamaEmbeddings({
  model: 'nomic-embed-text:latest',
})

// const vectorStore = new RedisVectorStore(embeddings, {
// 	redisClient,
// 	indexName: "Langchainjs_test",
// });

const vectorStore = await QdrantVectorStore.fromExistingCollection(embeddings, {
  url: `http://${qdrantHost}:${qdrantPort}`,
  collectionName: 'langchainjs-testing',
})

const document1: Document = {
  pageContent: 'The powerhouse of the cell is the mitochondria',
  metadata: { source: 'https://example.com' },
}

const document2: Document = {
  pageContent: 'Buildings are made out of brick',
  metadata: { source: 'https://example.com' },
}

const document3: Document = {
  pageContent: 'Mitochondria are made out of lipids',
  metadata: { source: 'https://example.com' },
}

const document4: Document = {
  pageContent: 'The 2024 Olympics are in Paris',
  metadata: { source: 'https://example.com' },
}

const documents = [document1, document2, document3, document4]

await vectorStore.addDocuments(documents)

// await vectorStore.delete({ ids: [uuids[3]] });

const filter = {
  must: [{ key: 'metadata.source', match: { value: 'https://example.com' } }],
}

const similaritySearchResults = await vectorStore.similaritySearch(
  'biology',
  2,
  filter,
)

for (const doc of similaritySearchResults) {
  console.log(`* ${doc.pageContent} [${JSON.stringify(doc.metadata, null)}]`)
}
